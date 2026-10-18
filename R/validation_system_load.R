#------------------------------------------------------------------------------
# validation_system_load.R - charging load vs. Swiss system load
#
# Hourly share = (max 5-minute charging load [kW] / 1000) / Swissgrid end-user
# consumption [MWh/h]. Swissgrid workbooks: see README.
#------------------------------------------------------------------------------

source('config.R')
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(lubridate); library(ggplot2); library(scales)})

#--- 1. Swissgrid system load, hourly -------------------------------------------
read_swissgrid <- function(file) {
  readxl::read_excel(file, sheet = 'Zeitreihen0h15')[-1, ] %>%      # first row holds the units
    select(1, 2) %>%
    rename(timestamp = 1, total_energy_cons_kWh = 2) %>%
    # labels are local time and repeat in the October DST hour, so the time is taken from the row position
    mutate(label     = timestamp,
           timestamp = as.POSIXct(label[1], tz = 'Europe/Zurich', format = '%d.%m.%Y %H:%M') + 900 * (row_number() - 1)) %>%
    { stopifnot(all(format(.$timestamp, '%d.%m.%Y %H:%M') == .$label)); . } %>%
    group_by(timestamp = timestamp - as.numeric(timestamp) %% 3600) %>%
    summarise(hr_cons_MWh = sum(as.numeric(total_energy_cons_kWh) / 1000, na.rm = TRUE), .groups = 'drop')
}

swissgrid_files <- file.path(path.swissgrid, sprintf('EnergieUebersichtCH-%s.xlsx',
                                                     substr(first.month, 1, 4):substr(last.month, 1, 4)))
Swiss_ELC_dt <- bind_rows(lapply(swissgrid_files, read_swissgrid)) %>%
  mutate(date = as.character(as.Date(timestamp, tz = 'Europe/Zurich')),
         hour = as.integer(format(timestamp, '%H'))) %>%
  group_by(date, hour) %>%
  summarise(hr_cons_MWh = max(hr_cons_MWh), .groups = 'drop')   # duplicate hours for daylight savings

#--- 2. join with the charging load ---------------------------------------------
load_files <- list.files(path.load, pattern = '_charging_load\\.csv$', full.names = TRUE)
all_month_load <- bind_rows(lapply(load_files, function(f) read.csv(f)[, -1]))    # drop the row-index column

hr_load_all <- all_month_load %>%
  group_by(date, hour) %>%
  slice(1) %>%                                   # the 'max_' columns are identical within an hour
  ungroup() %>%
  rename(max_EVload_kWh = max_load) %>%
  right_join(Swiss_ELC_dt, by = c('date', 'hour')) %>%   # keep every system-load hour
  mutate(max_chg_pct_kWh = (max_EVload_kWh / 1000) / hr_cons_MWh) %>%
  select(date, hour, max_EVload_kWh, hr_cons_MWh, max_chg_pct_kWh, mean_EVSE, max_occ, max_imputed, max_pct_imputed, NA_max) %>%
  arrange(date, hour)
write.csv(hr_load_all, file.path(path.out, 'hourly_load_vs_system.csv'), row.names = FALSE)

df <- hr_load_all %>%
  mutate(date     = as.Date(date),
         season   = case_when(month(date) %in% c(11, 12, 1) ~ 'Winter',
                              month(date) %in% c(2, 3, 4)   ~ 'Spring',
                              month(date) %in% c(5, 6, 7)   ~ 'Summer',
                              month(date) %in% c(8, 9, 10)  ~ 'Autumn'),
         season   = factor(season, levels = c('Winter', 'Spring', 'Summer', 'Autumn')),
         day_type = if_else(wday(date, week_start = 1) <= 5, 'Weekday', 'Weekend'))

max_or_na <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
daily <- df %>%
  group_by(date) %>%
  summarise(ev_load_mw  = max_or_na(max_EVload_kWh) / 1000,
            sys_load_mw = max_or_na(hr_cons_MWh),
            max_pct     = max_or_na(max_chg_pct_kWh) * 100, .groups = 'drop')

x_scale <- scale_x_date(date_breaks = '6 months', date_labels = '%b %Y',
                        limits = as.Date(c('2022-12-01', '2025-12-31')), expand = c(0, 0))

#--- 3. daily peaks, two axes ------------------------------------------------
scale_factor <- 10000 / 125          # left axis to 10,000 MW, right axis to 125 MW
ggplot(daily, aes(x = date)) +
  geom_line(aes(y = sys_load_mw, colour = 'Total System Load'), linewidth = 0.4, alpha = 0.7) +
  geom_line(aes(y = ev_load_mw * scale_factor, colour = 'EV Charging Load'), linewidth = 0.5, alpha = 0.9) +
  scale_y_continuous(name = 'Total Swiss System Load (GW)', labels = label_number(scale = 1e-3),
                     breaks = seq(0, 10000, by = 2000), limits = c(0, 10000), expand = expansion(mult = c(0, 0.02)),
                     sec.axis = sec_axis(~ . / scale_factor, name = 'EV Charging Load (MW)', breaks = seq(0, 125, by = 25))) +
  scale_colour_manual(values = c('Total System Load' = 'grey50', 'EV Charging Load' = '#D55E00')) +
  x_scale +
  labs(x = NULL, colour = NULL, title = 'Daily Peak EV Charging Load vs. Total Swiss System Load') +
  theme_bw(base_size = 11) +
  theme(legend.position = 'bottom', axis.title.y.right = element_text(colour = '#D55E00'),
        axis.text.y.right = element_text(colour = '#D55E00'), panel.grid.minor = element_blank())
ggsave(file.path(path.fig, 'load_vs_system_daily_peak.png'), width = 695 / 96, height = 425 / 96, dpi = 300)   # size of the original export (96 dpi)

#--- 4. daily maximum share with loess trend ---------------------------------
max_val  <- max(daily$max_pct, na.rm = TRUE)
max_date <- daily$date[which.max(daily$max_pct)]
ggplot(daily, aes(x = date)) +
  geom_line(aes(y = max_pct), colour = 'grey75', linewidth = 0.35, alpha = 0.6) +
  geom_smooth(data = daily %>% filter(!is.na(max_pct)), aes(y = max_pct), method = 'loess', formula = y ~ x, span = 0.15,
              se = TRUE, colour = '#0072B2', fill = '#0072B2', alpha = 0.15, linewidth = 0.9) +
  annotate('text', x = max_date, y = max_val, label = sprintf('Max: %.2f%%  ', max_val),
           hjust = 1, vjust = 0, size = 4, colour = '#D55E00') +
  annotate('point', x = max_date, y = max_val, colour = '#D55E00', size = 2.5) +
  scale_x_date(date_breaks = '6 months', date_labels = '%b %Y',
               limits = as.Date(c('2022-12-01', '2025-12-31')), expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(limits = c(0, 2), labels = function(x) paste0(x, '%'), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = 'EV Charging Load as % of Swiss System Load',
       title = 'EV Charging Load Share of Total Swiss Electricity System Load') +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank())
ggsave(file.path(path.fig, 'load_share_daily.png'), width = 695 / 96, height = 427 / 96, dpi = 300)

#--- 5. diurnal share by season and day type ---------------------------------
diurnal <- df %>%
  filter(date >= as.Date('2022-12-01'), !is.na(max_chg_pct_kWh)) %>%
  group_by(season, day_type, hour) %>%
  summarise(mean_pct = mean(max_chg_pct_kWh * 100),
            p25      = quantile(max_chg_pct_kWh * 100, 0.25),
            p75      = quantile(max_chg_pct_kWh * 100, 0.75), .groups = 'drop')

ggplot(diurnal, aes(x = hour, colour = day_type, fill = day_type)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.15, colour = NA) +
  geom_line(aes(y = mean_pct), linewidth = 0.8) +
  facet_wrap(~ season, nrow = 2) +
  scale_x_continuous(breaks = seq(0, 23, by = 4), labels = paste0(seq(0, 23, by = 4), ':00'),
                     limits = c(0, 23), expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(labels = function(x) paste0(x, '%'), limits = c(0, 1.5), breaks = seq(0, 1.5, by = 0.3),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_colour_manual(values = c('Weekday' = '#0072B2', 'Weekend' = '#E69F00')) +
  scale_fill_manual(values = c('Weekday' = '#0072B2', 'Weekend' = '#E69F00')) +
  labs(x = 'Hour of Day', y = 'EV Load as % of Swiss System Load', colour = 'Day Type', fill = 'Day Type',
       title = 'Diurnal Profile of EV Charging Load Share by Season') +
  theme_bw(base_size = 11) +
  theme(legend.position = 'bottom', panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'grey92'), panel.spacing = unit(1.2, 'lines'))
ggsave(file.path(path.fig, 'load_share_diurnal.png'), width = 695 / 96, height = 424 / 96, dpi = 300)

#--- 6. summary statistics of the hourly share ----------------------------------
pct <- df$max_chg_pct_kWh * 100
summary_tbl <- data.frame(
  Statistic = c('Mean (%)', 'Median (%)', 'Std. Dev. (%)', '95th pct. (%)', '99th pct. (%)', 'Max (%)',
                'N hours > 1%', 'N hours > 2%', 'Total hours'),
  Value     = c(round(c(mean(pct, na.rm = TRUE), median(pct, na.rm = TRUE), sd(pct, na.rm = TRUE),
                        quantile(pct, c(0.95, 0.99), na.rm = TRUE), max(pct, na.rm = TRUE)), 3),
                sum(pct > 1, na.rm = TRUE), sum(pct > 2, na.rm = TRUE), sum(!is.na(pct))))
write.csv(summary_tbl, file.path(path.out, 'ev_load_share_summary.csv'), row.names = FALSE)
print(summary_tbl)
