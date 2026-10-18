#------------------------------------------------------------------------------
# Script: Compute charging-capacity band shares
#
#   Two series are computed from the 37 representative snapshots selected
#   by build_infrastructure_digest.R (snapshot_quality_log.csv,
#   chosen == TRUE), Switzerland-filtered with the same swissBOUNDARIES3D
#   point-in-polygon test:
#
#   1. Site level (charging_capacity_shares.csv):
#      one count per site (unique coordinate), assigned the highest power
#      reported across the site's EVSEs. SwEVUL's own metric.
#
#   2. Facility level (charging_capacity_shares_facility.csv):
#      one count per entry in each EVSE's ChargingFacilities list. This is
#      the unit SFOE's published monthly band series uses (its band totals
#      exceed its own charging-point count and match the number of
#      ChargingFacilities entries), so this series is the one to validate
#      against Infrastructure-Digest/reference_SFOE/ich_tanke_strom_Kennzahlen_montlich_*.csv.
#
#   Conventions (both series):
#     - Band edges are upper-inclusive, matching SFOE:
#         <=10 | (10,21] | (21,42] | (42,100] | >100 kW
#       (with lower-inclusive edges only entries at exactly 42 kW or
#       100 kW would change band; see boundary_clustering_check.csv.)
#     - Entries with missing or 0 kW power are reported as 'Unknown' in
#       the digest files (share_pct includes them in the denominator;
#       share_pct_known excludes them). The validation against SFOE
#       excludes them completely, following SFOE's method note ("only
#       charging points for which the charging power is known are taken
#       into account"): shares on both sides are over the five bands.
#
#   Usage (Rscript):
#     compute_capacity_bands.R compute <from_ym> <to_ym>   # per-month RDS
#     compute_capacity_bands.R assemble                    # all CSVs + figure
#     compute_capacity_bands.R validate                    # validation CSVs +
#                                  figure from charging_capacity_shares_facility.csv
#
# Input : Infrastructure-Digest/static_archives/data_static_YYYY-MM.tar.gz,
#         Infrastructure-Digest/monthly_summary/snapshot_quality_log.csv,
#         external/swissBOUNDARIES3D_1_5_LV95_LN02.gdb,
#         Infrastructure-Digest/reference_SFOE/ich_tanke_strom_Kennzahlen_montlich_2026_08_23.csv
# Output: Infrastructure-Digest/monthly_summary/charging_capacity_shares.csv
#         Infrastructure-Digest/monthly_summary/charging_capacity_shares_facility.csv
#         Infrastructure-Digest/validation/capacity_validation_monthly.csv,
#         Infrastructure-Digest/validation/capacity_validation_summary.csv,
#         Infrastructure-Digest/validation/boundary_clustering_check.csv (sites whose
#           maximum power lies exactly on a band edge)
#         figures/capacity_validation_monthly.png
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(jsonlite)
  library(sf)
})

source('config.R')   # d.path.static, d.path.gdb, d.path.bfe, d.path.summ, d.path.val, path.fig
d.path.log   <- file.path(d.path.summ, 'snapshot_quality_log.csv')
d.path.tmp   <- file.path(path.out, 'capacity_bands_tmp')   # per-month .rds results, read by 'assemble'
d.path.xtr   <- file.path(tempdir(), 'evse_extract')
for (p in c(d.path.summ, d.path.val)) {
  dir.create(p, showWarnings = FALSE, recursive = TRUE)
}

d.band.levels <- c('<10 kW', '10kW - 21 kW', '21kW - 42 kW',
                   '42kW - 100 kW', '>100 kW', 'Unknown')

# Upper-inclusive edges (SFOE convention). NA / <= 0 -> Unknown.
d.classify_power_band <- function(power) {
  band <- rep('Unknown', length(power))
  ok <- !is.na(power) & power > 0
  band[ok & power <= 10]                 <- '<10 kW'
  band[ok & power > 10  & power <= 21]   <- '10kW - 21 kW'
  band[ok & power > 21  & power <= 42]   <- '21kW - 42 kW'
  band[ok & power > 42  & power <= 100]  <- '42kW - 100 kW'
  band[ok & power > 100]                 <- '>100 kW'
  band
}

#------------------------------------------------------------
# Switzerland filter (identical to build_infrastructure_digest.R)
#------------------------------------------------------------

load_ch_polygon <- function() {
  p <- sf::st_read(d.path.gdb, layer = 'TLM_LANDESGEBIET', quiet = TRUE)
  p <- p[p$ICC == 'CH', ]
  p <- sf::st_zm(p)
  sf::st_transform(p, 4326)
}

is_in_switzerland <- function(geo_str, ch_polygon) {
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
    result[ok] <- lengths(sf::st_intersects(pts, ch_polygon)) > 0
  }
  result
}

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

extract_cf_power <- function(x) {
  if (is.data.frame(x) && nrow(x) > 0 && 'power' %in% colnames(x)) {
    return(suppressWarnings(as.numeric(x$power)))
  }
  if (is.list(x) && length(x) > 0) {
    return(suppressWarnings(as.numeric(unlist(lapply(x, function(e) {
      if (is.list(e) && !is.null(e$power)) e$power else NA
    })))))
  }
  numeric(0)
}

#------------------------------------------------------------
# Per-snapshot computation
#------------------------------------------------------------

compute_snapshot <- function(path, ch_polygon) {
  d.raw <- fromJSON(path, simplifyVector = TRUE)
  d.groups <- d.raw$EVSEData

  facility_power <- numeric(0)
  site_rows <- list()
  n_evse <- 0L

  for (i in seq_len(nrow(d.groups))) {
    g <- d.groups$EVSEDataRecord[[i]]
    if (is.null(g) || nrow(g) == 0 || is.null(g$GeoCoordinates)) next
    geo <- safe_geo_vector(g$GeoCoordinates$Google)
    in_ch <- is_in_switzerland(geo, ch_polygon)
    if (!any(in_ch)) next
    g <- g[in_ch, , drop = FALSE]
    geo <- geo[in_ch]
    n_evse <- n_evse + nrow(g)

    evse_max <- rep(NA_real_, nrow(g))
    has_num  <- rep(FALSE, nrow(g))   # any numeric power entry, incl. 0
    if (!is.null(g$ChargingFacilities)) {
      for (j in seq_len(nrow(g))) {
        pw <- extract_cf_power(g$ChargingFacilities[[j]])
        if (length(pw) == 0) next
        facility_power <- c(facility_power, pw)
        has_num[j] <- any(!is.na(pw))
        known <- pw[!is.na(pw) & pw > 0]
        if (length(known) > 0) evse_max[j] <- max(known)
      }
    }
    site_rows[[length(site_rows) + 1]] <- data.frame(
      site = geo, evse_max = evse_max, has_num = has_num,
      stringsAsFactors = FALSE
    )
  }

  site_df <- do.call(rbind, site_rows)
  site_max <- suppressWarnings(tapply(site_df$evse_max, site_df$site, max, na.rm = TRUE))
  site_max[!is.finite(site_max)] <- NA_real_

  # Boundary check (boundary_clustering_check.csv): sites whose
  # maximum power lies exactly on a band edge. n_sites = sites with at
  # least one numeric power entry (a value of 0 counts as numeric).
  site_has_num <- tapply(site_df$has_num, site_df$site, any)
  sm <- as.numeric(site_max)
  boundary <- data.frame(
    n_sites = sum(site_has_num),
    at_10  = sum(sm == 10,  na.rm = TRUE),
    at_21  = sum(sm == 21,  na.rm = TRUE),
    at_42  = sum(sm == 42,  na.rm = TRUE),
    at_100 = sum(sm == 100, na.rm = TRUE)
  )

  tab <- function(power) {
    b <- factor(d.classify_power_band(power), levels = d.band.levels)
    n <- as.integer(table(b))
    data.frame(capacity_band = d.band.levels, n = n,
               share_pct = round(100 * n / sum(n), 2),
               stringsAsFactors = FALSE)
  }
  site_tab <- tab(as.numeric(site_max))
  fac_tab  <- tab(facility_power)
  known <- fac_tab$capacity_band != 'Unknown'
  fac_tab$share_pct_known <- NA_real_
  fac_tab$share_pct_known[known] <- round(100 * fac_tab$n[known] / sum(fac_tab$n[known]), 2)

  list(site = site_tab, facility = fac_tab, boundary = boundary,
       n_evse = n_evse, n_sites = length(site_max),
       n_facility_entries = length(facility_power))
}

#------------------------------------------------------------
# Mode: compute
#------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) args[1] else 'assemble'

if (mode == 'compute') {
  from_ym <- args[2]; to_ym <- args[3]
  for (p in c(d.path.tmp, d.path.xtr)) dir.create(p, showWarnings = FALSE, recursive = TRUE)
  ch_polygon <- load_ch_polygon()
  d.log <- read.csv(d.path.log, stringsAsFactors = FALSE)
  d.chosen <- d.log[d.log$chosen == TRUE, ]
  d.chosen <- d.chosen[d.chosen$year_month >= from_ym & d.chosen$year_month <= to_ym, ]
  d.chosen <- d.chosen[order(d.chosen$year_month), ]

  for (k in seq_len(nrow(d.chosen))) {
    ym <- d.chosen$year_month[k]
    member <- basename(gsub('\\\\', '/', d.chosen$path[k]))
    out_rds <- file.path(d.path.tmp, paste0('bands_', ym, '.rds'))
    if (file.exists(out_rds)) { cat(ym, 'exists, skipping\n'); next }
    archive <- file.path(d.path.static, paste0('data_static_', ym, '.tar.gz'))
    t0 <- Sys.time()
    untar(archive, files = member, exdir = d.path.xtr)
    snap <- file.path(d.path.xtr, member)
    res <- compute_snapshot(snap, ch_polygon)
    res$year_month <- ym
    res$snapshot <- member
    saveRDS(res, out_rds)
    unlink(snap)
    cat(sprintf('%s  %s  evse=%d sites=%d facility_entries=%d  (%.0f s)\n',
                ym, member, res$n_evse, res$n_sites, res$n_facility_entries,
                as.numeric(difftime(Sys.time(), t0, units = 'secs'))))
  }
  quit(save = 'no')
}

#------------------------------------------------------------
# Validation against SFOE (facility level, known power only)
#------------------------------------------------------------

# Compare SwEVUL's facility-level counts with SFOE's published band
# counts. Entries without a known power ('Unknown') are excluded
# completely, following SFOE's method note ("only charging points for
# which the charging power is known are taken into account"); shares on
# both sides are therefore over the five bands only.
build_validation <- function(d.fac) {
  d.bfe <- read.csv(d.path.bfe, stringsAsFactors = FALSE, check.names = FALSE)
  d.bfe$year_month <- sprintf('%04d-%02d', d.bfe$year, d.bfe$month)
  d.bfe.cols <- c('chargingPower_10kW_count', 'chargingPower_21kW_count',
                  'chargingPower_42kW_count', 'chargingPower_100kW_count',
                  'chargingPower_100pluskW_count')
  d.bfe <- d.bfe[, c('year_month', d.bfe.cols)]
  names(d.bfe) <- c('year_month', d.band.levels[1:5])

  bands <- d.band.levels[1:5]
  months <- sort(unique(d.fac$year_month))
  d.val <- do.call(rbind, lapply(months, function(ym) {
    f <- d.fac[d.fac$year_month == ym & d.fac$capacity_band %in% bands, ]
    b <- d.bfe[d.bfe$year_month == ym, ]
    n_f <- setNames(f$n, f$capacity_band)[bands]
    n_b <- if (nrow(b) == 1) unlist(b[1, bands]) else setNames(rep(NA, 5), bands)
    rows <- data.frame(year_month = ym, band = bands,
                       swevul_n = as.integer(n_f), bfe_n = as.integer(n_b),
                       stringsAsFactors = FALSE)
    rows$swevul_share_pct <- round(100 * rows$swevul_n / sum(rows$swevul_n), 2)
    rows$bfe_share_pct <- round(100 * rows$bfe_n / sum(rows$bfe_n), 2)
    rows$diff_pp <- round(rows$swevul_share_pct - rows$bfe_share_pct, 2)
    rows
  }))
  rownames(d.val) <- NULL
  d.val
}

draw_validation_figure <- function(d.val, out_png) {
  suppressPackageStartupMessages(library(ggplot2))
  bands <- d.band.levels[1:5]
  labs.fig <- c('<10 kW', '10-21 kW', '21-42 kW', '42-100 kW', '>100 kW')
  d.plot <- rbind(
    data.frame(year_month = d.val$year_month, band = d.val$band,
               source = 'SFOE',   share = d.val$bfe_share_pct),
    data.frame(year_month = d.val$year_month, band = d.val$band,
               source = 'SwEVUL', share = d.val$swevul_share_pct)
  )
  d.plot$date   <- as.Date(paste0(d.plot$year_month, '-01'))
  d.plot$band   <- factor(d.plot$band, levels = bands, labels = labs.fig)
  d.plot$source <- factor(d.plot$source, levels = c('SFOE', 'SwEVUL'))

  # mean difference (SwEVUL minus SFOE) per panel
  d.ann <- aggregate(diff_pp ~ band, data = d.val, FUN = mean)
  d.ann$label <- sprintf('mean diff. %+.1f pp', d.ann$diff_pp)
  d.ann$band  <- factor(d.ann$band, levels = bands, labels = labs.fig)

  p <- ggplot(d.plot, aes(x = date, y = share, color = source, linetype = source)) +
    geom_line(linewidth = 1) +
    geom_text(data = d.ann, aes(x = as.Date(Inf), y = Inf, label = label),
              inherit.aes = FALSE, hjust = 1.05, vjust = 1.6, size = 3) +
    facet_wrap(~ band, ncol = 3, scales = 'free_y') +
    scale_x_date(date_breaks = '6 months', date_labels = '%b %Y',
                 expand = c(0.01, 0.01)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.15))) +
    scale_color_manual(values = c('SFOE' = 'gray50', 'SwEVUL' = '#1B9E77')) +
    scale_linetype_manual(values = c('SFOE' = 'solid', 'SwEVUL' = 'dashed')) +
    labs(x = NULL, y = 'Share of charging facilities (%)',
         title = 'Charging Capacity Validation by Power Band',
         subtitle = 'SwEVUL (facility level, known power only) vs. SFOE published series; pp = percentage points') +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          strip.background = element_rect(fill = 'gray95'),
          plot.title = element_text(face = 'bold', size = 12),
          legend.position = c(0.84, 0.22),
          legend.title = element_blank())
  ggsave(out_png, plot = p, width = 11.7, height = 7, dpi = 300)
  cat('Figure written to', out_png, '\n')
}

write_validation <- function(d.val) {
  write.csv(d.val, file.path(d.path.val, 'capacity_validation_monthly.csv'), row.names = FALSE)
  cat('capacity_validation_monthly.csv written.\n')
  d.summary <- aggregate(diff_pp ~ band, data = d.val[!is.na(d.val$diff_pp), ],
                         FUN = function(x) c(mean = mean(x), mean_abs = mean(abs(x)), max_abs = max(abs(x))))
  d.summary <- data.frame(band = d.summary$band, round(d.summary$diff_pp, 2))
  d.summary <- d.summary[match(d.band.levels[1:5], d.summary$band), ]
  write.csv(d.summary, file.path(d.path.val, 'capacity_validation_summary.csv'), row.names = FALSE)
  cat('\nSwEVUL (facility level, known power only) minus SFOE, percentage points, over',
      length(unique(d.val$year_month)), 'months:\n')
  print(d.summary, row.names = FALSE)
  draw_validation_figure(d.val, file.path(path.fig, 'capacity_validation_monthly.png'))
}

if (mode %in% c('validate', 'figure')) {
  d.fac <- read.csv(file.path(d.path.summ, 'charging_capacity_shares_facility.csv'),
                    stringsAsFactors = FALSE)
  write_validation(build_validation(d.fac))
  quit(save = 'no')
}

#------------------------------------------------------------
# Mode: assemble
#------------------------------------------------------------

d.files <- sort(list.files(d.path.tmp, pattern = '^bands_\\d{4}-\\d{2}\\.rds$',
                           full.names = TRUE))
stopifnot(length(d.files) > 0)
d.res <- lapply(d.files, readRDS)
cat('Assembling', length(d.res), 'months.\n')

bind_series <- function(key) {
  do.call(rbind, lapply(d.res, function(r) {
    x <- r[[key]]; x$year_month <- r$year_month; x
  }))
}
d.site <- bind_series('site')
d.fac  <- bind_series('facility')
d.site <- d.site[, c('capacity_band', 'n', 'share_pct', 'year_month')]
d.fac  <- d.fac[, c('capacity_band', 'n', 'share_pct', 'share_pct_known', 'year_month')]

write.csv(d.site, file.path(d.path.summ, 'charging_capacity_shares.csv'), row.names = FALSE)
write.csv(d.fac,  file.path(d.path.summ, 'charging_capacity_shares_facility.csv'), row.names = FALSE)

d.bound <- do.call(rbind, lapply(d.res, function(r) {
  cbind(data.frame(year_month = r$year_month, stringsAsFactors = FALSE), r$boundary)
}))
write.csv(d.bound, file.path(d.path.val, 'boundary_clustering_check.csv'), row.names = FALSE)
cat('Digest CSVs written.\n')

write_validation(build_validation(d.fac))
