#------------------------------------------------------------------------------
# Script: Build the count-validation table and figure comparing SwEVUL's
#         computed infrastructure digest against SFOE's own published figures
#        .
#
#   - Counts (sites, charging points), Dec-2025: SFOE-published figures
#     (locations_CH_count / stations_CH_count in the reference data) vs.
#     SwEVUL's Dec-2025 values. SFOE's charging-point figure sits very
#     close to SwEVUL's EVSE-level count (EvseID) and well above a raw
#     unique-ChargingStationId count of the same snapshot, so it is not a
#     literal ChargingStationId count. Consistent with SFOE's own feed
#     documentation, which calls EVSEData/EVSEStatus "data about the
#     charging points" (github.com/SFOE/ichtankestrom_Documentation).
#   - Full monthly validation (Dec-2022 - Dec-2025, 37 months): SFOE's
#     reference data (Infrastructure-Digest/reference_SFOE/ich_tanke_strom_Kennzahlen_montlich_
#     2026_08_23.csv, a manually captured snapshot of SFOE's underlying
#     dashboard data, accessed 2026-08-23 - see the dataset README)
#     provide stations_CH_count / locations_CH_count series for the whole
#     study period.
#
#   The charging-capacity band validation is NOT built here: SFOE's band
#   series counts ChargingFacilities entries, a different unit from the
#   site-level shares. The unit-consistent validation is produced by
#   compute_capacity_bands.R (capacity_validation_monthly.csv,
#   capacity_validation_monthly.png).
#
# Input : Infrastructure-Digest/monthly_summary/operator_evse_timeseries.csv,
#         Infrastructure-Digest/reference_SFOE/ich_tanke_strom_Kennzahlen_montlich_2026_08_23.csv
# Output: Infrastructure-Digest/validation/validation_table.csv,
#         Infrastructure-Digest/validation/validation_table_monthly.csv,
#         figures/validation_figure_monthly.png
#------------------------------------------------------------------------------

source('config.R')   # d.path.summ, d.path.val, d.path.bfe, path.fig
dir.create(d.path.val, showWarnings = FALSE, recursive = TRUE)

#------------------------------------------------------------
# 1. Counts validation table (Dec-2025 vs Dec-2025)
#------------------------------------------------------------

d.ts <- read.csv(file.path(d.path.summ, 'operator_evse_timeseries.csv'),
                 stringsAsFactors = FALSE)
d.dec25 <- d.ts[d.ts$year_month == '2025-12', ]

# SFOE reference values for Dec-2025, taken from the reference data rather
# than hard-coded.
d.bfe.ref <- read.csv(d.path.bfe, stringsAsFactors = FALSE, check.names = FALSE)
d.bfe.ref$year_month <- sprintf('%04d-%02d', d.bfe.ref$year, d.bfe.ref$month)
d.bfe.dec25 <- d.bfe.ref[d.bfe.ref$year_month == '2025-12', ]

d.counts <- data.frame(
  metric = c('Sites', 'Charging points (EVSE, EvseID)'),
  bfe_dec2025    = c(d.bfe.dec25$locations_CH_count, d.bfe.dec25$stations_CH_count),
  swevul_dec2025 = c(d.dec25$n_sites, d.dec25$n_evse),
  stringsAsFactors = FALSE
)
d.counts$diff     <- d.counts$swevul_dec2025 - d.counts$bfe_dec2025
d.counts$pct_diff <- round(100 * d.counts$diff / d.counts$bfe_dec2025, 2)

write.csv(d.counts, file.path(d.path.val, 'validation_table.csv'),
          row.names = FALSE)
cat('validation_table.csv written.\n')
print(d.counts)

#------------------------------------------------------------
# 2. Full monthly validation (Dec-2022 - Dec-2025, all 37 months)
#------------------------------------------------------------

d.bfe.monthly <- read.csv(d.path.bfe, stringsAsFactors = FALSE, check.names = FALSE)
d.bfe.monthly$year_month <- sprintf('%04d-%02d', d.bfe.monthly$year, d.bfe.monthly$month)
d.bfe.monthly <- d.bfe.monthly[, c('year_month', 'locations_CH_count', 'stations_CH_count')]
names(d.bfe.monthly) <- c('year_month', 'bfe_sites', 'bfe_charging_points')

d.merged <- merge(d.ts[, c('year_month', 'n_sites', 'n_evse')], d.bfe.monthly,
                  by = 'year_month')
d.merged <- d.merged[order(d.merged$year_month), ]

d.merged$sites_pct_diff <- round(
  100 * (d.merged$n_sites - d.merged$bfe_sites) / d.merged$bfe_sites, 2
)
d.merged$charging_points_pct_diff <- round(
  100 * (d.merged$n_evse - d.merged$bfe_charging_points) / d.merged$bfe_charging_points, 2
)

write.csv(d.merged, file.path(d.path.val, 'validation_table_monthly.csv'),
          row.names = FALSE)
cat('validation_table_monthly.csv written (', nrow(d.merged), 'months ).\n')

cat('\nSites pct_diff        - mean:', round(mean(d.merged$sites_pct_diff), 2),
    ' max abs:', round(max(abs(d.merged$sites_pct_diff)), 2), '\n')
cat('Charging pts pct_diff - mean:', round(mean(d.merged$charging_points_pct_diff), 2),
    ' max abs:', round(max(abs(d.merged$charging_points_pct_diff)), 2), '\n')

#------------------------------------------------------------
# 3. monthly sites and charging points, SFOE vs. SwEVUL
#------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

d.long <- rbind(
  data.frame(year_month = d.merged$year_month, metric = 'sites',
             source = 'SFOE',   value = d.merged$bfe_sites),
  data.frame(year_month = d.merged$year_month, metric = 'sites',
             source = 'SwEVUL', value = d.merged$n_sites),
  data.frame(year_month = d.merged$year_month, metric = 'charging_points',
             source = 'SFOE',   value = d.merged$bfe_charging_points),
  data.frame(year_month = d.merged$year_month, metric = 'charging_points',
             source = 'SwEVUL', value = d.merged$n_evse)
)
d.long$date    <- as.Date(paste0(d.long$year_month, '-01'))
d.long$value_k <- d.long$value / 1000
d.long$source  <- factor(d.long$source, levels = c('SFOE', 'SwEVUL'))
stopifnot(!anyNA(d.long$value))

# shared styling
d.cols  <- c('SFOE' = 'gray50', 'SwEVUL' = '#1B9E77')
d.ltys  <- c('SFOE' = 'solid',  'SwEVUL' = 'dashed')
base_theme <- theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = 'none')

panel <- function(d, ylab, ymax) {
  ggplot(d, aes(x = date, y = value_k, color = source, linetype = source)) +
    geom_line(linewidth = 1) +
    scale_x_date(date_breaks = '3 months', date_labels = '%b %Y',
                 expand = c(0.01, 0.01)) +
    scale_y_continuous(limits = c(0, ymax), expand = c(0, 0)) +
    scale_color_manual(values = d.cols) +
    scale_linetype_manual(values = d.ltys) +
    labs(x = NULL, y = ylab) +
    base_theme
}

p.sites  <- panel(d.long[d.long$metric == 'sites', ],
                  'Thousands of Sites', 10)
p.points <- panel(d.long[d.long$metric == 'charging_points', ],
                  'Thousands of Charging Points', 20) +
  theme(legend.position = 'bottom', legend.title = element_blank())

p.monthly <- (p.sites / p.points) +
  plot_annotation(
    title = 'Monthly Infrastructure Digest Validation',
    subtitle = 'Sites and charging points, December 2022 - December 2025',
    theme = theme(plot.title = element_text(face = 'bold', size = 12))
  )

d.fig <- file.path(path.fig, 'validation_figure_monthly.png')
ggsave(d.fig, plot = p.monthly, width = 11.7, height = 7, dpi = 300)
cat('\nFigure written to', d.fig, '\n')
