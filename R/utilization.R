#------------------------------------------------------------------------------
# utilization.R - hourly utilization rates
#
# Utilization rate of a snapshot = Occupied / (Available + Occupied).
# Output: output/utilization_hourly_week{day,end}.csv, figures/*.png
#------------------------------------------------------------------------------

source('config.R')
source('R/functions.R')
suppressPackageStartupMessages({library(ggplot2)})

chg_avail_files <- list_status_files()

#--- hourly summaries for a subset of days --------------------------------------
# Dataset is large so it must be done by hour. wday: 1:5 weekdays, 6:7 weekends
hourly_utilization <- function(files, wday = NULL) {
  res <- vector('list', 24)
  for (h in 0:23) {
    cat('Processing hour', h, '...\n')
    dt_hour <- load_filter_charging_dt_parallel(files, wday = wday, hour = h, exclude_foreign = exclude.foreign)
    if (is.null(dt_hour) || nrow(dt_hour) == 0) next
    s <- snapshot_utilization(dt_hour)
    s[, hour := as.integer(format(datetime, '%H'))]
    res[[h + 1]] <- s[, compute_stats(prop_occupied), by = hour]
    rm(dt_hour, s); gc()
  }
  rbindlist(res, use.names = TRUE, fill = TRUE)
}

# boxplot from summary statistics; whiskers at 1.5 x IQR, bounded below by the minimum
box_aes <- aes(ymin   = pmax(min_prop, q1_prop - 1.5 * (q3_prop - q1_prop)),
               lower  = q1_prop, middle = median_prop, upper = q3_prop,
               ymax   = q3_prop + 1.5 * (q3_prop - q1_prop))
box_theme <- theme_classic() +
  theme(axis.line = element_line(color = 'black'),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = 'bottom', plot.margin = margin(10, 20, 10, 20))

hourly_boxplot <- function(dt, ylab) {
  ggplot(dt, aes(x = factor(hour))) +
    geom_boxplot(box_aes, stat = 'identity', alpha = 0.7, fill = 'steelblue') +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
    coord_cartesian(ylim = c(0, 0.25)) +
    labs(x = 'Hour', y = ylab) +
    box_theme
}

#--- 1. weekdays and weekends ------------------------------------------------
res_wkday <- hourly_utilization(chg_avail_files, wday = 1:5)
res_wkend <- hourly_utilization(chg_avail_files, wday = 6:7)
write.csv(res_wkday, file.path(path.out, 'utilization_hourly_weekday.csv'), row.names = FALSE)
write.csv(res_wkend, file.path(path.out, 'utilization_hourly_weekend.csv'), row.names = FALSE)

hourly_boxplot(res_wkday, 'Weekday Utilization Rate (%)')
ggsave(file.path(path.fig, 'weekday_utilization_crosssection.png'), width = 8, height = 6, dpi = 300)
hourly_boxplot(res_wkend, 'Weekend Utilization Rate (%)')
ggsave(file.path(path.fig, 'weekend_utilization_crosssection.png'), width = 8, height = 6, dpi = 300)

#--- 2. all days in September, October and December --------------------------
# utilization at the full hour, one line per day; months as in Gellrich et al. (2022)
chg_dt_sample <- load_filter_charging_dt_parallel(chg_avail_files, month = c(9, 10, 12), minute = 0,
                                                  exclude_foreign = exclude.foreign)
chg_dt_sample <- snapshot_utilization(chg_dt_sample)
setnames(chg_dt_sample, 'prop_occupied', 'utilization')
chg_dt_sample[, `:=`(month        = lubridate::month(datetime, label = TRUE, abbr = FALSE),
                     hour_index   = data.table::hour(datetime),
                     date         = as.Date(datetime),
                     weekday_type = ifelse(lubridate::wday(datetime, week_start = 1) >= 6, 'Weekend', 'Weekday'))]
# repeat the 00:00 value at hour 24 so that each daily line spans the full axis
plot_sample <- rbind(chg_dt_sample, copy(chg_dt_sample[hour_index == 0])[, hour_index := 24L])

ggplot(plot_sample) +
  geom_line(aes(x = hour_index, y = utilization, group = interaction(date, weekday_type), color = weekday_type),
            alpha = 0.5, linewidth = 0.4) +
  facet_wrap(~ month, ncol = 1, scales = 'free_x') +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, by = 4),
                     labels = function(x) sprintf('%02d:00', x), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_color_manual(values = c('Weekday' = '#1B9E77', 'Weekend' = '#D95F02')) +
  labs(title = 'Hourly Utilization Rates - Weekdays vs Weekends',
       subtitle = 'All days in September, October, and December',
       x = 'Hour of Day', y = 'Utilization (%)', color = 'Day Type') +
  coord_cartesian(ylim = c(0, 0.25), xlim = c(0, 24)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(size = 12),
        plot.title = element_text(face = 'bold'))
ggsave(file.path(path.fig, 'Sep_Oct_Dec-Utilization.png'), width = 8, height = 8, dpi = 300)
