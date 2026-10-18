#------------------------------------------------------------------------------
# charging_load.R - rated-power-based charging load, one .csv per month
#
# Every 'Occupied' EVSE draws its rated power; the load of a snapshot is the sum.
# Input:  monthly .parquet; rated power from monthly_evse_details/ or path.evse
#         (details.source, config.R)
# Output: Swiss-EVLoad-Data_CSV/YYYY-MM_charging_load.csv, load_match_rate.csv,
#         output/load_worst_case_imputation.csv, figures/charging_load.png
#------------------------------------------------------------------------------

source('config.R')
source('R/functions.R')
source('R/evse_details_cleaning.R')
suppressPackageStartupMessages({library(dplyr); library(ggplot2)})

dir.create(path.load, showWarnings = FALSE, recursive = TRUE)
chg_avail_files <- list_status_files()

# keep only the highest power level if an EvseID has several facilities (one row per EvseID)
rated_power_table <- function(details) {
  details %>%
    group_by(EvseID) %>%
    slice_max(order_by = ChargingFacility_power, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(EvseID, ChargingFacility_power)
}

stopifnot(details.source %in% c('static_single', 'static_monthly'))
if (details.source == 'static_single') chg_power_details <- rated_power_table(CH_EVSE_details)

#--- 1. monthly load files ------------------------------------------------------
global_worst <- data.table()
match_rate   <- list()

for (month_parquet in chg_avail_files) {
  csv_filepath <- file.path(path.load, paste0(tools::file_path_sans_ext(basename(month_parquet)), '_load.csv'))
  if (file.exists(csv_filepath) && !overwrite.load) {
    message('File already exists, skipping (overwrite.load = FALSE): ', csv_filepath)
    next
  }

  ym <- substr(basename(month_parquet), 1, 7)
  details_file <- path.evse
  if (details.source == 'static_monthly') {
    details_file      <- monthly_details_file(ym)
    chg_power_details <- rated_power_table(read_clean_evse_details(details_file))
  }

  month_dt_full <- load_filter_charging_dt_parallel(month_parquet)
  if (is.null(month_dt_full)) {
    message('Empty file, skipping: ', month_parquet)
    next
  }

  chg_dt_occ  <- filter(month_dt_full, EVSEStatus == 'Occupied')
  n_occ       <- nrow(chg_dt_occ)
  n_matched   <- sum(chg_dt_occ$EvseID %in% chg_power_details$EvseID)
  chg_dt_load <- merge(chg_dt_occ, chg_power_details, by = 'EvseID')   # inner join: unmatched EVSEs are dropped

  chg_dt_load[, `:=`(date   = as.Date(datetime),
                     hour   = as.integer(format(datetime, '%H')),
                     minute = as.integer(format(datetime, '%M')))]

  # load of each snapshot
  load_minute_dt <- chg_dt_load[, {
    total_load_val   <- sum(ChargingFacility_power, na.rm = TRUE)
    count_occ_val    <- .N
    NA_sum_val       <- sum(is.na(ChargingFacility_power))
    mean_EVSE_val    <- mean(ChargingFacility_power, na.rm = TRUE)
    mean_imputed_val <- NA_sum_val * mean_EVSE_val
    .(total_load    = total_load_val,
      count_occ     = count_occ_val,
      NA_sum        = NA_sum_val,
      pct_NA_points = NA_sum_val / count_occ_val,   # count_occ includes NA_sum
      mean_EVSE     = mean_EVSE_val,
      mean_imputed  = mean_imputed_val,
      pct_imputed   = mean_imputed_val / total_load_val)
  }, by = .(datetime, date, hour, minute)]   # per snapshot

  # hourly maxima, added to every row of the hour
  load_hour_dt <- load_minute_dt[, `:=`(max_load        = max(total_load, na.rm = TRUE),
                                        max_occ         = max(count_occ, na.rm = TRUE),
                                        max_pct_NA_pts  = max(pct_NA_points, na.rm = TRUE),
                                        NA_max          = max(NA_sum, na.rm = TRUE),
                                        mean_EVSE       = max(mean_EVSE),
                                        max_imputed     = max(mean_imputed, na.rm = TRUE),
                                        max_pct_imputed = max(pct_imputed, na.rm = TRUE)),
                                 by = .(date, hour)]
  setorder(load_hour_dt, datetime)   # merge() sorts by EvseID, so order explicitly
  write.csv(load_hour_dt[, !'datetime'], csv_filepath)

  match_rate[[ym]] <- data.frame(year_month         = ym,
                                 details_source     = details.source,
                                 details_file       = basename(details_file),
                                 n_occupied_obs     = n_occ,
                                 n_matched_obs      = n_matched,
                                 match_rate         = n_matched / n_occ,
                                 n_matched_no_power = sum(load_minute_dt$NA_sum),
                                 mean_total_load_kW = mean(load_minute_dt$total_load),
                                 peak_total_load_kW = max(load_minute_dt$total_load))

  # snapshots with the most missing rated-power values in this month
  global_worst <- rbind(global_worst,
                        rbindlist(list(load_minute_dt[which.max(NA_sum)],
                                       load_minute_dt[which.max(pct_NA_points)],
                                       load_minute_dt[which.max(mean_imputed)],
                                       load_minute_dt[which.max(pct_imputed)]))[, source_file := basename(month_parquet)],
                        fill = TRUE)
}

if (length(match_rate) > 0) {
  write.csv(do.call(rbind, match_rate), file.path(path.load, 'load_match_rate.csv'), row.names = FALSE)
}

if (nrow(global_worst) > 0) {
  final_global_worst <- unique(rbindlist(list(global_worst[which.max(NA_max)],
                                              global_worst[which.max(max_pct_NA_pts)],
                                              global_worst[which.max(max_imputed)],
                                              global_worst[which.max(max_pct_imputed)])))
  write.csv(final_global_worst, file.path(path.out, 'load_worst_case_imputation.csv'), row.names = FALSE)
}

#--- 2. figure: load at the full hour, one line per day -------------------------
load_files <- list.files(path.load, pattern = '_charging_load\\.csv$', full.names = TRUE)
if (length(load_files) == 0) stop('No load files found in ', path.load)
all_month_load <- rbindlist(lapply(load_files, function(f) read.csv(f)[, -1]))   # drop the row-index column

chg_load_dt <- all_month_load[minute == 0]
chg_load_dt[, `:=`(month = as.factor(month(as.Date(date))),
                   Year  = as.factor(year(as.Date(date))))]

ggplot(chg_load_dt) +
  geom_line(aes(x = hour, y = total_load / 1000, color = Year, group = interaction(month, Year, date)), alpha = 0.5) +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20), limits = c(0, 23), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 125), expand = c(0, 0)) +
  scale_color_brewer(palette = 'Set1') +
  labs(title = '', x = 'Hour', y = 'Charging Load (MW)') +
  theme_minimal() +
  theme(axis.line        = element_line(colour = 'black'),
        axis.text        = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text      = element_text(size = 12))
ggsave(file.path(path.fig, 'charging_load.png'), width = 8, height = 6, dpi = 300)
