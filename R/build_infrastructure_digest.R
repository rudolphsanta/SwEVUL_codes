#------------------------------------------------------------------------------
# Script: Static EV charging infrastructure analysis
#         (SFOE registry "ch.bfe.ladestellen-elektromobilitaet")
#
#   Per month, over Dec-2022 - Dec-2025:
#     - number of sites (physical locations lat/lon of charging stations)
#     - number of charging points (identified via EvseID; SFOE's own
#       feed documentation calls EVSEData/EVSEStatus "data about the
#       charging points" - github.com/SFOE/ichtankestrom_Documentation)
#     - shares of "connection types"
#     - shares of "charging capacity"
#
#   Category labels match the official SFOE dashboard
#   ("Ladeinfrastruktur Elektromobilitaet",
#   https://www.uvek-gis.admin.ch/BFE/storymaps/MO_Kennzahlen_Fahrzeuge/
#   Ladeinfrastruktur_Elektromobilitaet/?lang=en):
#       Site and charging-point counts are validated month by month
#       against SFOE's published figures (see validate_counts.R
#       and validation_table_monthly.csv).
#     - Connection type: one count per entry in each EVSE's Plugs list
#       (an EVSE may list several types, so this counts plug-type
#       listings, not physical connectors). Same eight categories and
#       labels as the SFOE dashboard (Socket type 2, CCS, CHAdeMO, Tesla,
#       Cable type 2, Cable type 1, Household plug CH, Other); CCS also
#       includes the rare CCS Combo 1.
#     - Charging capacity: per site, only the highest power across the
#       site's EVSEs counts (so a multi-EVSE site is counted once),
#       binned into SFOE's power bands. NOTE: this is SwEVUL's own unit.
#       SFOE's published band series counts ChargingFacilities entries
#       and uses upper-inclusive band edges; the published site-level and
#       facility-level series are therefore produced by
#       compute_capacity_bands.R, which supersedes this script's
#       charging_capacity_shares.csv.
#
#   Country filtering
#   ------------------
#   Address.Country cannot be used to restrict the feed to Switzerland:
#   in the most recent months it is uniformly "CHE", including for
#   records with clearly non-Swiss or invalid coordinates, and in earlier
#   months it carried other country codes for one operator's foreign
#   stations. Instead, every record's
#   GeoCoordinates is checked with a
#   point-in-polygon test against the official Swiss national boundary
#   (swissBOUNDARIES3D, layer TLM_LANDESGEBIET, feature ICC == "CH" -
#   external/swissBOUNDARIES3D_1_5_LV95_LN02.gdb). This runs before any
#   counting, so all counts and shares are Switzerland-only.
#
#   Snapshot selection
#   ------------------
#   Each archive has twice-daily snapshots; only the first one per day is
#   used. A snapshot can be incomplete when some operators fail to
#   respond to the upstream feed. Each month's representative snapshot is 
#   the one with the most charging points that month - an incomplete pull always
#   under-counts relative to a complete one. Every snapshot's counts and 
#   the chosen one are logged to 'snapshot_quality_log.csv' and visualized 
#   in 'snapshot_completeness.pdf'.
#
#   Only the chosen snapshot per month is then fully parsed (Pass 2) for
#   plug types and charging power.
#
# Input : Infrastructure-Digest/static_archives/data_static_YYYY-MM.tar.gz (twice-daily JSON
#         snapshots per month; only the first snapshot of each day is used)
# Output: Infrastructure-Digest/monthly_summary/operator_evse_timeseries.csv,
#         Infrastructure-Digest/monthly_summary/connection_type_shares.csv,
#         Infrastructure-Digest/monthly_summary/charging_capacity_shares.csv,
#         Infrastructure-Digest/monthly_summary/snapshot_quality_log.csv, figures/digest_diagnostics/*.pdf (diagnostic only)
#------------------------------------------------------------------------------

#------------------------------------------------------------
# Packages
#------------------------------------------------------------

library(jsonlite)
library(parallel)
library(sf)

#------------------------------------------------------------
# Define paths and parameters
#------------------------------------------------------------

source('config.R')   # d.path.static, d.path.summ, d.path.gdb, path.fig
d.path.out    <- d.path.summ
d.path.plots  <- file.path(path.fig, 'digest_diagnostics')   # diagnostic plots only
d.path.tmp    <- file.path(tempdir(), 'evse_extract')

dir.create(d.path.out,   showWarnings = FALSE, recursive = TRUE)
dir.create(d.path.plots, showWarnings = FALSE, recursive = TRUE)

# Official Swiss national boundary (swissBOUNDARIES3D) - see Country
# filtering note above. Reprojected to WGS84 (EPSG:4326) to match the
# feed's lat/lon GeoCoordinates.
d.ch.polygon <- sf::st_read(d.path.gdb, layer = 'TLM_LANDESGEBIET',
                             quiet = TRUE)
d.ch.polygon <- d.ch.polygon[d.ch.polygon$ICC == 'CH', ]
d.ch.polygon <- sf::st_zm(d.ch.polygon)
d.ch.polygon <- sf::st_transform(d.ch.polygon, 4326)

# TRUE for each geo_str ("lat lon") whose point falls inside Switzerland.
is_in_switzerland <- function(geo_str) {
  parts <- strsplit(geo_str, ' ')
  lat <- suppressWarnings(as.numeric(vapply(parts, function(x) {
    if (length(x) >= 1) x[1] else NA_character_
  }, character(1))))
  lon <- suppressWarnings(as.numeric(vapply(parts, function(x) {
    if (length(x) >= 2) x[2] else NA_character_
  }, character(1))))
  ok <- !is.na(lat) & !is.na(lon)
  result <- rep(FALSE, length(geo_str))
  if (any(ok)) {
    pts <- sf::st_as_sf(data.frame(lon = lon[ok], lat = lat[ok]),
                         coords = c('lon', 'lat'), crs = 4326)
    result[ok] <- lengths(sf::st_intersects(pts, d.ch.polygon)) > 0
  }
  result
}

# Charging-capacity bands, matching the official SFOE dashboard.
d.power.band.levels <- c('<10 kW', '10kW - 21 kW', '21kW - 42 kW',
                          '42kW - 100 kW', '>100 kW', 'Unknown')

d.classify_power_band <- function(power) {
  band <- rep('Unknown', length(power))
  ok <- !is.na(power) & power > 0
  # Upper-inclusive edges, as on the SFOE dashboard.
  band[ok & power <= 10]                 <- '<10 kW'
  band[ok & power > 10  & power <= 21]   <- '10kW - 21 kW'
  band[ok & power > 21  & power <= 42]   <- '21kW - 42 kW'
  band[ok & power > 42  & power <= 100]  <- '42kW - 100 kW'
  band[ok & power > 100]                 <- '>100 kW'
  band
}

# Connection-type categories, matching the official SFOE dashboard.
# Plug strings not covered by the map below fall into 'Other'.
d.connection.type.levels <- c('Socket type 2', 'CCS', 'CHAdeMO', 'Tesla',
                               'Cable type 2', 'Cable type 1',
                               'Household plug CH', 'Other')

d.connection.type.map <- c(
  'Type 2 Outlet'                     = 'Socket type 2',
  'CCS Combo 1 Plug (Cable Attached)' = 'CCS',
  'CCS Combo 2 Plug (Cable Attached)' = 'CCS',
  'CHAdeMO'                           = 'CHAdeMO',
  'Tesla Connector'                   = 'Tesla',
  'Type 2 Connector (Cable Attached)' = 'Cable type 2',
  'Type 1 Connector (Cable Attached)' = 'Cable type 1',
  'Type J Swiss Standard'             = 'Household plug CH'
)

d.classify_connection_type <- function(plug) {
  cat <- unname(d.connection.type.map[plug])
  cat[is.na(cat)] <- 'Other'
  cat
}

# Number of workers for Pass 1. 1 = run in-process (no cluster); values
# > 1 parallelise Pass 1 with a PSOCK cluster. Each worker loads sf and
# parses ~25 MB JSON snapshots (roughly 1 GB of RAM per worker), so keep
# this at 1 unless plenty of memory is free. OPENBLAS_NUM_THREADS = 1
# stops each process from spawning BLAS threads on top of that.
Sys.setenv(OPENBLAS_NUM_THREADS = '1')
d.n.cores <- 1
d.use.cluster <- d.n.cores > 1

#------------------------------------------------------------
# Helper: restrict one operator-group's EVSEDataRecord data.frame to
# records inside Switzerland
#------------------------------------------------------------

# The feed delivers GeoCoordinates$Google in three shapes over the study
# period: a plain string "lat lon"; the same string wrapped in a
# one-element list (from March 2023, for some operators); and an object
# {"Coordinates": "lat lon"} (early July 2023). jsonlite turns these into a
# character vector, a list, or a data.frame with a Coordinates column.
# This normalizes all three to a character vector of "lat lon".
safe_geo_vector <- function(google) {
  if (is.character(google)) return(google)
  if (is.data.frame(google) && 'Coordinates' %in% names(google)) {
    return(safe_geo_vector(google$Coordinates))
  }
  vapply(google, function(x) {
    if (is.list(x) && !is.null(x$Coordinates)) x <- x$Coordinates
    if (is.character(x) && length(x) == 1) x else NA_character_
  }, character(1))
}

ch_filter_group <- function(g) {
  if (is.null(g) || nrow(g) == 0 || is.null(g$GeoCoordinates)) return(NULL)
  geo <- safe_geo_vector(g$GeoCoordinates$Google)
  in_ch <- is_in_switzerland(geo)
  if (!any(in_ch)) return(NULL)
  g <- g[in_ch, , drop = FALSE]
  g$GeoCoordinates$Google <- geo[in_ch]
  g
}

#------------------------------------------------------------
# Helper: Pass 1 - lightweight per-snapshot counts (Switzerland-only)
#------------------------------------------------------------

quick_summarize_snapshot <- function(path) {
  d.raw <- tryCatch(fromJSON(path, simplifyVector = TRUE),
                     error = function(e) NULL)
  if (is.null(d.raw) || is.null(d.raw$EVSEData)) {
    return(data.frame(n_operators = NA_integer_, n_evse = NA_integer_,
                       n_sites = NA_integer_))
  }
  d.groups <- d.raw$EVSEData
  op_present <- character(0)
  evse_ids   <- character(0)
  sites      <- character(0)
  for (i in seq_len(nrow(d.groups))) {
    g <- ch_filter_group(d.groups$EVSEDataRecord[[i]])
    if (is.null(g)) next
    op_present <- c(op_present, d.groups$OperatorID[i])
    evse_ids   <- c(evse_ids, g$EvseID)
    sites      <- c(sites, g$GeoCoordinates$Google)
  }
  data.frame(
    n_operators = length(unique(op_present)),
    n_evse      = length(unique(evse_ids)),
    n_sites     = length(unique(sites))
  )
}

#------------------------------------------------------------
# Helper: Pass 2 - detailed parse (plugs + charging power) of one snapshot
# (Switzerland-only)
#------------------------------------------------------------

extract_cf <- function(x) {
  if (!is.data.frame(x) || nrow(x) == 0) return(NULL)
  data.frame(
    power = if ('power' %in% colnames(x)) {
      as.numeric(x$power)
    } else {
      NA_real_
    },
    powertype = if ('powertype' %in% colnames(x)) {
      as.character(x$powertype)
    } else {
      NA_character_
    },
    stringsAsFactors = FALSE
  )
}

detailed_parse_snapshot <- function(path) {
  d.raw <- fromJSON(path, simplifyVector = TRUE)
  d.groups <- d.raw$EVSEData

  plug_list <- character(0)
  d.evse.power <- list()

  for (i in seq_len(nrow(d.groups))) {
    g <- ch_filter_group(d.groups$EVSEDataRecord[[i]])
    if (is.null(g)) next

    # Connection types: one count per entry in the Plugs list
    # (plug-type listings; an EVSE may list more than one).
    if (!is.null(g$Plugs)) plug_list <- c(plug_list, unlist(g$Plugs))

    # Charging capacity: highest known power per site (max over the
    # site's EVSEs), not per EVSE or per ChargingFacilities entry.
    if (!is.null(g$ChargingFacilities)) {
      evse_max <- vapply(seq_len(nrow(g)), function(j) {
        cf <- extract_cf(g$ChargingFacilities[[j]])
        if (is.null(cf) || all(is.na(cf$power))) return(NA_real_)
        max(cf$power, na.rm = TRUE)
      }, numeric(1))
      d.evse.power[[length(d.evse.power) + 1]] <- data.frame(
        site = g$GeoCoordinates$Google, evse_max_power = evse_max,
        stringsAsFactors = FALSE
      )
    }
  }

  d.evse.power <- do.call(rbind, d.evse.power)
  site_max_power <- NULL
  if (!is.null(d.evse.power)) {
    site_max_power <- suppressWarnings(tapply(
      d.evse.power$evse_max_power, d.evse.power$site, max, na.rm = TRUE
    ))
    site_max_power[!is.finite(site_max_power)] <- NA_real_
  }

  list(plugs = plug_list, site_max_power = site_max_power)
}

#------------------------------------------------------------
# Main loop: process one monthly archive at a time
#------------------------------------------------------------

d.archives <- sort(list.files(
  d.path.static, pattern = '^data_static_\\d{4}-\\d{2}\\.tar\\.gz$',
  full.names = TRUE
))
cat('Found', length(d.archives), 'monthly archives.\n')

if (d.use.cluster) {
  d.cl <- makeCluster(d.n.cores)
  invisible(clusterEvalQ(d.cl, { library(jsonlite); library(sf) }))
  clusterExport(d.cl, c('quick_summarize_snapshot', 'ch_filter_group',
                         'is_in_switzerland', 'safe_geo_vector',
                         'd.ch.polygon'))
}

d.summary.all     <- list()  # per-snapshot quality log (all snapshots)
d.monthly         <- list()  # operator/charging point/site counts for the chosen
                              # representative snapshot per month
d.conn.shares.all <- list()  # connection type shares per month
d.cap.shares.all  <- list()  # charging capacity (per site) shares per
                              # month

for (d.archive in d.archives) {

  d.month <- sub('^data_static_(\\d{4}-\\d{2})\\.tar\\.gz$', '\\1',
                  basename(d.archive))
  cat('Processing', d.month, '...\n')

  unlink(d.path.tmp, recursive = TRUE, force = TRUE)
  dir.create(d.path.tmp, showWarnings = FALSE, recursive = TRUE)

  # ---- Extract only the first snapshot of each calendar day (archives
  #      have two per day, ~08:00 and ~20:00) ----
  d.members <- untar(d.archive, list = TRUE)
  d.members <- d.members[grepl('\\.json$', d.members)]
  if (length(d.members) == 0) {
    cat('  WARNING: no snapshot files found in', d.archive,
        '- skipping.\n')
    next
  }

  d.ts <- sub('^.*ladestellen-elektromobilitaet_(.*)\\.json$', '\\1',
               basename(d.members))
  d.mdatetime <- as.POSIXct(d.ts, format = '%Y-%m-%d-%H-%M-%S', tz = 'UTC')
  d.ord <- order(d.mdatetime)
  d.members <- d.members[d.ord]
  d.mdatetime <- d.mdatetime[d.ord]
  d.first.of.day <- !duplicated(as.Date(d.mdatetime))
  d.members <- d.members[d.first.of.day]
  d.datetime <- d.mdatetime[d.first.of.day]

  untar(d.archive, files = d.members, exdir = d.path.tmp)
  d.files <- file.path(d.path.tmp, d.members)

  # ---- Pass 1: lightweight counts for every (daily) snapshot in this
  #      month ----
  d.counts <- if (d.use.cluster) parLapply(d.cl, d.files, quick_summarize_snapshot) else lapply(d.files, quick_summarize_snapshot)
  d.counts <- do.call(rbind, d.counts)

  d.snap <- data.frame(
    year_month  = d.month,
    path        = d.files,
    datetime    = d.datetime,
    n_operators = d.counts$n_operators,
    n_evse      = d.counts$n_evse,
    n_sites     = d.counts$n_sites,
    stringsAsFactors = FALSE
  )
  d.snap <- d.snap[order(d.snap$datetime), ]

  # ---- Pick the snapshot with the most charging points; ties broken by operator
  #      count, then by the most recent date ----
  d.valid <- d.snap[!is.na(d.snap$n_evse), ]
  if (nrow(d.valid) == 0) {
    warning(sprintf(
      'Month %s: every snapshot failed to parse - skipping month.',
      d.month
    ))
    d.snap$chosen <- FALSE
    d.summary.all[[d.month]] <- d.snap
    next
  }
  d.ord <- order(-d.valid$n_evse, -d.valid$n_operators,
                 -as.numeric(d.valid$datetime))
  d.pick <- d.valid[d.ord[1], ]
  d.snap$chosen <- d.snap$path == d.pick$path

  d.summary.all[[d.month]] <- d.snap
  d.monthly[[d.month]] <- data.frame(
    year_month  = d.month,
    snapshot    = basename(d.pick$path),
    datetime    = d.pick$datetime,
    n_operators = d.pick$n_operators,
    n_evse      = d.pick$n_evse,
    n_sites     = d.pick$n_sites,
    stringsAsFactors = FALSE
  )

  # ---- Pass 2: detailed parse of the chosen snapshot only ----
  d.detail <- detailed_parse_snapshot(d.pick$path)

  if (length(d.detail$plugs) > 0) {
    d.conn <- factor(d.classify_connection_type(d.detail$plugs),
                      levels = d.connection.type.levels)
    d.ct <- as.data.frame(table(d.conn), stringsAsFactors = FALSE)
    names(d.ct) <- c('connection_type', 'n')
    d.ct$share_pct <- round(100 * d.ct$n / sum(d.ct$n), 2)
    d.ct$year_month <- d.month
    d.conn.shares.all[[d.month]] <- d.ct
  }

  if (!is.null(d.detail$site_max_power) &&
        length(d.detail$site_max_power) > 0) {
    d.band <- factor(d.classify_power_band(as.numeric(
      d.detail$site_max_power
    )), levels = d.power.band.levels)
    d.cb <- as.data.frame(table(d.band), stringsAsFactors = FALSE)
    names(d.cb) <- c('capacity_band', 'n')
    d.cb$share_pct <- round(100 * d.cb$n / sum(d.cb$n), 2)
    d.cb$year_month <- d.month
    d.cap.shares.all[[d.month]] <- d.cb
  }

  unlink(d.path.tmp, recursive = TRUE, force = TRUE)

  # ---- Free memory before the next month ----
  rm(d.detail, d.counts, d.snap, d.pick, d.files, d.members, d.datetime,
     d.mdatetime)
  if (d.use.cluster) invisible(clusterEvalQ(d.cl, gc(verbose = FALSE)))
  invisible(gc(verbose = FALSE))
}

if (d.use.cluster) stopCluster(d.cl)

#------------------------------------------------------------
# Assemble output tables
#------------------------------------------------------------

d.quality.log <- do.call(rbind, d.summary.all)
d.monthly.ts  <- do.call(rbind, d.monthly)
d.conn.shares <- do.call(rbind, d.conn.shares.all)
d.cap.shares  <- do.call(rbind, d.cap.shares.all)

rownames(d.quality.log) <- NULL
rownames(d.monthly.ts)  <- NULL
rownames(d.conn.shares) <- NULL
rownames(d.cap.shares)  <- NULL

d.monthly.ts <- d.monthly.ts[order(d.monthly.ts$year_month), ]

d.conn.shares$connection_type <- factor(
  d.conn.shares$connection_type, levels = d.connection.type.levels
)
d.conn.shares <- d.conn.shares[order(d.conn.shares$year_month,
                                      d.conn.shares$connection_type), ]
d.conn.shares$connection_type <- as.character(d.conn.shares$connection_type)

d.cap.shares$capacity_band <- factor(d.cap.shares$capacity_band,
                                      levels = d.power.band.levels)
d.cap.shares <- d.cap.shares[order(d.cap.shares$year_month,
                                    d.cap.shares$capacity_band), ]
d.cap.shares$capacity_band <- as.character(d.cap.shares$capacity_band)

cat('\nMonthly representative snapshots chosen',
    '(highest charging point count per month):\n')
print(d.monthly.ts[, c('year_month', 'snapshot', 'n_operators', 'n_evse',
                        'n_sites')])

#------------------------------------------------------------
# Write CSV outputs
#------------------------------------------------------------

# Store the snapshot file name, not the machine-specific extraction path.
d.quality.log$path <- basename(d.quality.log$path)
write.csv(d.quality.log,
          file.path(d.path.out, 'snapshot_quality_log.csv'),
          row.names = FALSE)
write.csv(d.monthly.ts,
          file.path(d.path.out, 'operator_evse_timeseries.csv'),
          row.names = FALSE)
write.csv(d.conn.shares,
          file.path(d.path.out, 'connection_type_shares.csv'),
          row.names = FALSE)
write.csv(d.cap.shares,
          file.path(d.path.out, 'charging_capacity_shares.csv'),
          row.names = FALSE)

cat('\nCSV outputs written to', d.path.out, '\n')

#------------------------------------------------------------
# Plot 1: sites and charging points over time
#------------------------------------------------------------

pdf(file.path(d.path.plots, 'evse_sites_over_time.pdf'), paper = 'special',
    width = 11.7, height = 5.3, pointsize = 11)
par(mar = c(5, 5, 2, 2) + 0.1, family = 'Times')
d.x <- seq_len(nrow(d.monthly.ts))
d.y.max <- max(d.monthly.ts$n_evse, d.monthly.ts$n_sites, na.rm = TRUE)
plot(d.x, d.monthly.ts$n_evse, type = 'l', lwd = 1.5, col = 'black',
     ylim = c(0, d.y.max), frame = FALSE, axes = FALSE, ann = FALSE)
lines(d.x, d.monthly.ts$n_sites, lwd = 1.5, lty = 2, col = 'blue')
axis(1, at = d.x, labels = d.monthly.ts$year_month, las = 2, cex.axis = 0.6)
axis(2, cex.axis = 1.0)
title(xlab = 'month', cex.lab = 1.2, line = 3.5)
title(ylab = 'count', cex.lab = 1.2, line = 3)
legend('topleft', bty = 'n', legend = c('charging points', 'sites'),
       col = c('black', 'blue'), lty = c(1, 2), lwd = 1.5)
dev.off()

#------------------------------------------------------------
# Plot 2: connection type shares over time
#------------------------------------------------------------

d.ct.wide <- reshape(
  d.conn.shares[, c('year_month', 'connection_type', 'share_pct')],
  idvar = 'year_month', timevar = 'connection_type', direction = 'wide'
)
names(d.ct.wide) <- sub('^share_pct\\.', '', names(d.ct.wide))
d.ct.wide <- d.ct.wide[order(d.ct.wide$year_month), ]
d.ct.wide[is.na(d.ct.wide)] <- 0

# Order categories as on the SFOE dashboard rather than alphabetically
d.conn.cols <- intersect(d.connection.type.levels, names(d.ct.wide))
d.col <- c('green3', 'black', 'gray50', 'orange', 'purple', 'red3', 'blue',
           'brown', 'pink')

pdf(file.path(d.path.plots, 'connection_type_shares_over_time.pdf'),
    paper = 'special', width = 11.7, height = 5.3, pointsize = 11)
par(mar = c(5, 5, 2, 2) + 0.1, family = 'Times')
d.x <- seq_len(nrow(d.ct.wide))
plot(d.x, d.ct.wide[[d.conn.cols[1]]], type = 'l', lwd = 1.5, col = d.col[1],
     ylim = c(0, 100), frame = FALSE, axes = FALSE, ann = FALSE)
for (i in seq_along(d.conn.cols)[-1]) {
  lines(d.x, d.ct.wide[[d.conn.cols[i]]], lwd = 1.5,
        col = d.col[((i - 1) %% length(d.col)) + 1])
}
axis(1, at = d.x, labels = d.ct.wide$year_month, las = 2, cex.axis = 0.6)
axis(2, at = seq(0, 100, 20), cex.axis = 1.0)
title(xlab = 'month', cex.lab = 1.2, line = 3.5)
title(ylab = 'share of connection types (%)', cex.lab = 1.2, line = 3)
legend('topright', bty = 'n', legend = d.conn.cols,
       col = d.col[((seq_along(d.conn.cols) - 1) %% length(d.col)) + 1],
       lty = 1, lwd = 1.5, cex = 0.7)
dev.off()

#------------------------------------------------------------
# Plot 3: charging capacity shares over time
#         (sites, by highest-power connector)
#------------------------------------------------------------

d.cb.wide <- reshape(
  d.cap.shares[, c('year_month', 'capacity_band', 'share_pct')],
  idvar = 'year_month', timevar = 'capacity_band', direction = 'wide'
)
names(d.cb.wide) <- sub('^share_pct\\.', '', names(d.cb.wide))
d.cb.wide <- d.cb.wide[order(d.cb.wide$year_month), ]
d.cb.wide[is.na(d.cb.wide)] <- 0

# Order bands low-to-high (as on the SFOE dashboard) rather than
# alphabetically
d.band.cols <- intersect(d.power.band.levels, names(d.cb.wide))

pdf(file.path(d.path.plots, 'charging_capacity_shares_over_time.pdf'),
    paper = 'special', width = 11.7, height = 5.3, pointsize = 11)
par(mar = c(5, 5, 2, 2) + 0.1, family = 'Times')
d.x <- seq_len(nrow(d.cb.wide))
plot(d.x, d.cb.wide[[d.band.cols[1]]], type = 'l', lwd = 1.5, col = d.col[1],
     ylim = c(0, 100), frame = FALSE, axes = FALSE, ann = FALSE)
for (i in seq_along(d.band.cols)[-1]) {
  lines(d.x, d.cb.wide[[d.band.cols[i]]], lwd = 1.5,
        col = d.col[((i - 1) %% length(d.col)) + 1])
}
axis(1, at = d.x, labels = d.cb.wide$year_month, las = 2, cex.axis = 0.6)
axis(2, at = seq(0, 100, 20), cex.axis = 1.0)
title(xlab = 'month', cex.lab = 1.2, line = 3.5)
title(ylab = 'share of sites by charging capacity (%)', cex.lab = 1.2,
      line = 3)
legend('topright', bty = 'n', legend = d.band.cols,
       col = d.col[((seq_along(d.band.cols) - 1) %% length(d.col)) + 1],
       lty = 1, lwd = 1.5, cex = 0.7)
dev.off()

#------------------------------------------------------------
# Plot 4: snapshot selection diagnostic
#         (every snapshot, chosen ones highlighted)
#------------------------------------------------------------

pdf(file.path(d.path.plots, 'snapshot_completeness.pdf'),
    paper = 'special', width = 11.7, height = 5.3, pointsize = 11)
par(mar = c(5, 5, 2, 2) + 0.1, family = 'Times')
plot(d.quality.log$datetime, d.quality.log$n_evse, type = 'l', lwd = 1,
     col = 'gray60', frame = FALSE, xlab = '',
     ylab = 'number of charging points (per snapshot)', cex.lab = 1.2)
points(d.quality.log$datetime[d.quality.log$chosen],
       d.quality.log$n_evse[d.quality.log$chosen],
       pch = 19, col = 'blue', cex = 0.6)
legend('bottomright', bty = 'n',
       legend = 'chosen as monthly representative (most charging points)',
       pch = 19, col = 'blue', cex = 0.8)
dev.off()

cat('\nPlots written to', d.path.plots, '\n')
cat('Done.\n')
