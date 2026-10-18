#------------------------------------------------------------------------------
# validation_coverage.R - monthly coverage of the status data
#
# Output: output/monthly_coverage.csv, figures/monthly_coverage.png
# Delete output/monthly_coverage.csv to recompute the monthly statistics.
#------------------------------------------------------------------------------

source('config.R')
source('R/functions.R')
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(patchwork); library(scales)})

coverage_file <- file.path(path.out, 'monthly_coverage.csv')

#--- 1. monthly statistics ------------------------------------------------------
if (!file.exists(coverage_file)) {
  monthly_coverage_list <- list()
  for (f in list_status_files()) {
    cat('Processing', basename(f), '\n')
    dt_month <- load_filter_charging_dt_parallel(f, status = NULL, exclude_foreign = exclude.foreign, n_cores = 1)   # all statuses
    if (is.null(dt_month)) next
    snap_util <- snapshot_utilization(dt_month)
    monthly_coverage_list[[f]] <- data.table(
      month_date      = as.Date(paste0(substr(basename(f), 1, 7), '-01')),
      n_snapshots     = uniqueN(snap_util$datetime),
      n_evse_all      = uniqueN(dt_month$EvseID),
      n_evse_total    = dt_month[EVSEStatus %in% c('Available', 'Occupied'), uniqueN(EvseID)],
      n_evse_occupied = dt_month[EVSEStatus == 'Occupied', uniqueN(EvseID)],
      mean_util       = mean(snap_util$prop_occupied, na.rm = TRUE),
      median_util     = median(snap_util$prop_occupied, na.rm = TRUE),
      p25_util        = quantile(snap_util$prop_occupied, 0.25, na.rm = TRUE),
      p75_util        = quantile(snap_util$prop_occupied, 0.75, na.rm = TRUE))
    rm(dt_month, snap_util); gc()
  }
  write.csv(rbindlist(monthly_coverage_list), coverage_file, row.names = FALSE)
}

#--- 2. share of possible snapshots ---------------------------------------------
all_months <- data.frame(month_date = seq(as.Date(paste0(first.month, '-01')), as.Date(paste0(last.month, '-01')), by = 'month'))

monthly_fig <- all_months %>%
  left_join(read.csv(coverage_file) %>% mutate(month_date = as.Date(month_date)), by = 'month_date') %>%
  mutate(n_snapshots       = ifelse(is.na(n_snapshots), 0L, n_snapshots),
         expected_snaps    = as.numeric(lubridate::days_in_month(month_date)) * 24 * 12,
         pct_snaps         = 100 * n_snapshots / expected_snaps,
         pct_snaps_missing = 100 - pct_snaps,
         poor_coverage     = pct_snaps < 50,          # more than 50 % of snapshots missing
         n_evse_total_plot    = if_else(poor_coverage, NA_real_, as.numeric(n_evse_total)),
         n_evse_occupied_plot = if_else(poor_coverage, NA_real_, as.numeric(n_evse_occupied)))

cat(sprintf('Snapshots: %s observed of %s possible (%.1f %%).\n',
            comma(sum(monthly_fig$n_snapshots)), comma(sum(monthly_fig$expected_snaps)),
            100 * sum(monthly_fig$n_snapshots) / sum(monthly_fig$expected_snaps)))

#--- 3. figure ---------------------------------------------------------------
x_scale   <- scale_x_date(date_breaks = '6 months', date_labels = '%b %Y',
                          limits = as.Date(c('2022-12-01', '2025-12-31')), expand = expansion(mult = c(0, 0)))
poor_rect <- geom_rect(data = monthly_fig %>% filter(poor_coverage),
                       aes(xmin = month_date - 15, xmax = month_date + 15, ymin = -Inf, ymax = Inf),
                       fill = '#E69F00', alpha = 0.2, inherit.aes = FALSE)
legend_theme <- theme(legend.position      = c(0.98, 0.02),
                      legend.justification = c('right', 'bottom'),
                      legend.background    = element_rect(fill = alpha('white', 0.8), colour = 'grey80', linewidth = 0.3),
                      legend.margin        = margin(3, 6, 3, 6),
                      legend.key.size      = unit(0.5, 'cm'),
                      legend.text          = element_text(size = 9))

p0 <- ggplot(monthly_fig, aes(x = month_date)) +
  geom_col(aes(y = pct_snaps_missing), fill = '#CC3333', alpha = 0.7, width = 25) +
  geom_hline(yintercept = 50, linetype = 'dotted', colour = 'grey40', linewidth = 0.5) +
  x_scale +
  scale_y_continuous(name = 'Missing\nSnapshots', limits = c(0, 100), breaks = seq(0, 100, by = 50),
                     expand = expansion(mult = c(0, 0)), labels = function(x) paste0(x, '%')) +
  labs(x = NULL, title = 'Dataset Coverage, EV Charging Load Share, and Utilization Rate') +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank())

pA <- ggplot(monthly_fig, aes(x = month_date)) +
  poor_rect +
  geom_area(aes(y = n_evse_total_plot), fill = 'grey70', alpha = 0.5) +
  geom_line(aes(y = n_evse_total_plot, colour = 'Total active EVSEs'), linewidth = 0.8) +
  geom_point(aes(y = n_evse_total_plot, colour = 'Total active EVSEs'), size = 1.8) +
  geom_line(aes(y = n_evse_occupied_plot, colour = 'EVSEs occupied at least once in the month'), linewidth = 0.6, linetype = 'dashed') +
  geom_point(aes(y = n_evse_occupied_plot, colour = 'EVSEs occupied at least once in the month'), size = 1.8) +
  scale_colour_manual(values = c('EVSEs occupied at least once in the month' = '#D55E00', 'Total active EVSEs' = 'grey40')) +
  x_scale +
  scale_y_continuous(labels = comma, name = 'Number of EVSEs', breaks = seq(0, 20000, by = 5000),
                     limits = c(0, 20000), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, colour = NULL) +
  theme_bw(base_size = 11) + legend_theme

pB <- ggplot(monthly_fig, aes(x = month_date)) +
  poor_rect +
  geom_ribbon(aes(ymin = p25_util, ymax = p75_util), fill = '#009E73', alpha = 0.2) +
  geom_line(aes(y = mean_util, colour = 'Mean'), linewidth = 0.9) +
  geom_line(aes(y = median_util, colour = 'Median'), linewidth = 0.6, linetype = 'dashed') +
  scale_colour_manual(values = c('Mean' = '#009E73', 'Median' = '#005a3c')) +
  x_scale +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02)), limits = c(0, 0.2)) +
  labs(x = NULL, y = 'EVSE Utilization Rate', colour = NULL) +
  theme_bw(base_size = 11) + legend_theme

p0 / pA / pB + plot_layout(heights = c(2, 4, 4))
ggsave(file.path(path.fig, 'monthly_coverage.png'), width = 691 / 96, height = 419 / 96, dpi = 300)   # size of the original export (96 dpi)
