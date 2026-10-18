#------------------------------------------------------------------------------
# evse_details_cleaning.R - load and clean an EVSE details file
#
# Input:  ChargingStationDetails_19-02-2026.csv
# Output: CH_EVSE_details (in session), output/evse_details_missing_values.csv
#------------------------------------------------------------------------------

if (!exists('path.evse')) source('config.R')
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(lubridate)})

#--- cleaning function; row counts attached as attribute 'n' --------------------
read_clean_evse_details <- function(file) {
  d <- read.csv(file,
                colClasses = c(IsOpen24Hours          = 'logical',
                               LastUpdate             = 'character',
                               MaxCapacity            = 'numeric',
                               ChargingFacility_power = 'character'),
                stringsAsFactors = TRUE) %>%
    separate(GeoCoordinates, into = c('latitude', 'longitude'), sep = ' ', convert = TRUE)
  n.raw <- nrow(d)

  # rated power that is not a number (e.g. '2x22' in the 2023 snapshots) -> NA
  d$ChargingFacility_power <- suppressWarnings(as.numeric(d$ChargingFacility_power))

  d$LastUpdate <- ymd_hms(d$LastUpdate, tz = 'Europe/Zurich', quiet = TRUE)

  # convert coordinates outside the country extents (config.R) to NA and drop
  d <- d %>%
    mutate(longitude = ifelse(longitude >= lon.range[1] & longitude <= lon.range[2], longitude, NA),
           latitude  = ifelse(latitude  >= lat.range[1] & latitude  <= lat.range[2], latitude,  NA)) %>%
    filter(!is.na(longitude) & !is.na(latitude))
  n.coord <- nrow(d)

  # remove rows that are EXACT duplicates (duplicated across all columns)
  d <- d[!duplicated(d), ]
  attr(d, 'n') <- c(raw = n.raw, coord = n.coord, clean = nrow(d), evse = n_distinct(d$EvseID))
  d
}

CH_EVSE_details <- read_clean_evse_details(path.evse)
n.raw   <- attr(CH_EVSE_details, 'n')[['raw']]
n.coord <- attr(CH_EVSE_details, 'n')[['coord']]
n.clean <- attr(CH_EVSE_details, 'n')[['clean']]

cat(sprintf('EVSE details: %d rows read; %d removed (coordinates); %d removed (exact duplicates); %d rows, %d unique EvseIDs kept.\n',
            n.raw, n.raw - n.coord, n.coord - n.clean, n.clean, n_distinct(CH_EVSE_details$EvseID)))

# missing values per column: NA, zero (numeric columns) or empty string (text columns)
summary_missing <- sapply(CH_EVSE_details, function(x) {
  n <- sum(is.na(x))
  if (is.numeric(x))                   n <- n + sum(x == 0, na.rm = TRUE)
  if (is.character(x) || is.factor(x)) n <- n + sum(trimws(as.character(x)) == '', na.rm = TRUE)
  n
})
summary_missing <- sort(summary_missing, decreasing = TRUE)
write.csv(data.frame(column = names(summary_missing), n_missing = as.integer(summary_missing),
                     pct_missing = round(100 * as.integer(summary_missing) / n.clean, 2)),
          file.path(path.out, 'evse_details_missing_values.csv'), row.names = FALSE)
