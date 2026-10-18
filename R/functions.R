#------------------------------------------------------------------------------
# functions.R - functions shared by utilization.R, charging_load.R and
# validation_coverage.R
#
# 'datetime' holds Swiss local time; read_status_parquet() sets the zone to UTC and the clock values are used as stored.
#------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(lubridate)
  library(parallel)
})

#--- list of monthly status files within the study period ----------------------
list_status_files <- function(folder = path.status, min_month = first.month,
                              max_month = last.month) {
  files  <- list.files(folder, pattern = '\\.parquet$', full.names = TRUE)
  months <- sub('^(\\d{4}-\\d{2}).*$', '\\1', basename(files))
  keep   <- months >= min_month & months <= max_month
  files[keep][order(months[keep])]
}

#--- details table of a month (nearest month if there is none) -----------------
monthly_details_file <- function(ym, folder = d.path.details) {
  files  <- list.files(folder, pattern = '^evse_details_\\d{4}-\\d{2}\\.csv$', full.names = TRUE)
  if (length(files) == 0) stop('No monthly details files in ', folder, '; run R/monthly_evse_details.R first.')
  months <- sub('^evse_details_(\\d{4}-\\d{2})\\.csv$', '\\1', basename(files))
  dist   <- abs(as.numeric(difftime(as.Date(paste0(months, '-01')), as.Date(paste0(ym, '-01')), units = 'days')))
  files[which.min(dist)]
}

#--- EVSEs located outside Switzerland in a month -------------------------------
# listed with coordinates and no row inside the bounding box (config.R);
# EVSEs without coordinates are not returned
foreign_evse_ids <- function(ym, folder = d.path.details) {
  d <- data.table::fread(monthly_details_file(ym, folder), select = c('EvseID', 'GeoCoordinates'),
                         colClasses = 'character', na.strings = '')
  xy <- data.table::tstrsplit(d$GeoCoordinates, ' ', fixed = TRUE, keep = 1:2)
  d[, `:=`(lat = suppressWarnings(as.numeric(xy[[1]])), lon = suppressWarnings(as.numeric(xy[[2]])))]
  d[, inside := !is.na(lat) & !is.na(lon) &
       lat >= lat.range[1] & lat <= lat.range[2] & lon >= lon.range[1] & lon <= lon.range[2]]
  by_evse <- d[, .(inside = any(inside), located = any(!is.na(lat) & !is.na(lon))), by = EvseID]
  by_evse[located & !inside, EvseID]
}

#--- read one monthly file ------------------------------------------------------
read_status_parquet <- function(file) {
  d <- as.data.table(arrow::read_parquet(file, col_select = c('datetime', 'EvseID', 'EVSEStatus')))
  d[, datetime := lubridate::with_tz(datetime, 'UTC')]   # Swiss local time; arrow shows untagged times in the system time zone
  d
}

#--- read and filter one monthly file -------------------------------------------
# wday: 1 = Monday ... 7 = Sunday. exclude_foreign drops foreign_evse_ids().
process_file <- function(file, IDs = NULL, status = NULL, year = NULL, month = NULL,
                         day = NULL, wday = NULL, hour = NULL, minute = NULL,
                         exclude_foreign = FALSE) {
  finfo <- file.info(file)
  if (is.na(finfo$size) || finfo$size == 0) {
    message('Skipping missing or zero-byte file: ', file)
    return(NULL)
  }
  dt <- tryCatch(read_status_parquet(file), error = function(e) {
    message('Skipping unreadable file: ', file, ' (', conditionMessage(e), ')')
    NULL
  })
  if (is.null(dt) || nrow(dt) == 0 || !'datetime' %in% names(dt)) return(NULL)

  dt[, `:=`(EvseID = as.factor(EvseID), EVSEStatus = as.factor(EVSEStatus))]
  rows_start <- nrow(dt)

  if (exclude_foreign)  dt <- dt[!EvseID %in% foreign_evse_ids(substr(basename(file), 1, 7))]
  if (!is.null(IDs))    dt <- dt[EvseID %in% IDs]
  if (!is.null(status)) dt <- dt[EVSEStatus %in% status]
  if (!is.null(year))   dt <- dt[data.table::year(datetime)  %in% year]
  if (!is.null(month))  dt <- dt[data.table::month(datetime) %in% month]
  if (!is.null(day))    dt <- dt[data.table::mday(datetime)  %in% day]
  if (!is.null(wday))   dt <- dt[lubridate::wday(datetime, week_start = 1) %in% wday]
  if (!is.null(hour))   dt <- dt[data.table::hour(datetime)   %in% hour]
  if (!is.null(minute)) dt <- dt[data.table::minute(datetime) %in% minute]

  list(dt = dt, rows_elim = rows_start - nrow(dt))
}

#--- read and filter several monthly files in parallel --------------------------
# Only 'Available' and 'Occupied' rows by default; status = NULL for all rows.
load_filter_charging_dt_parallel <- function(files_to_load, IDs = NULL,
                                             status = c('Available', 'Occupied'),
                                             year = NULL, month = NULL, day = NULL,
                                             wday = NULL, hour = NULL, minute = NULL,
                                             exclude_foreign = FALSE, n_cores = n.cores) {
  # skip files that cannot contain the requested year or month (file names are 'YYYY-MM_charging.parquet')
  if (!is.null(year))  files_to_load <- files_to_load[substr(basename(files_to_load), 1, 4) %in% as.character(year)]
  if (!is.null(month)) files_to_load <- files_to_load[substr(basename(files_to_load), 6, 7) %in% sprintf('%02d', month)]
  if (length(files_to_load) == 0) return(NULL)

  n_cores <- ifelse(n_cores <= 0, detectCores() + n_cores, n_cores)
  n_cores <- max(1, min(n_cores, length(files_to_load)))

  if (n_cores == 1) {
    dt_list <- lapply(files_to_load, process_file, IDs = IDs, status = status, year = year,
                      month = month, day = day, wday = wday, hour = hour, minute = minute,
                      exclude_foreign = exclude_foreign)
  } else {
    cl <- makeCluster(n_cores)
    on.exit(stopCluster(cl), add = TRUE)
    clusterEvalQ(cl, {library(data.table); library(arrow); library(lubridate)})
    clusterExport(cl, varlist = c('process_file', 'read_status_parquet', 'foreign_evse_ids', 'monthly_details_file',
                                  'd.path.details', 'lon.range', 'lat.range'))
    dt_list <- parLapply(cl, files_to_load, process_file, IDs = IDs, status = status, year = year,
                         month = month, day = day, wday = wday, hour = hour, minute = minute,
                         exclude_foreign = exclude_foreign)
  }

  dt_list <- Filter(Negate(is.null), dt_list)
  if (length(dt_list) == 0) return(NULL)
  cat('Rows eliminated by filters:', sum(sapply(dt_list, `[[`, 'rows_elim')), '\n')
  rbindlist(lapply(dt_list, `[[`, 'dt'), use.names = TRUE, fill = TRUE)
}

#--- utilization rate of each snapshot ------------------------------------------
# share of 'Occupied' among 'Available' + 'Occupied' EVSEs, per timestamp
snapshot_utilization <- function(dt) {
  dt[EVSEStatus %in% c('Available', 'Occupied'),
     .(prop_occupied = mean(EVSEStatus == 'Occupied')), by = datetime]
}

#--- summary statistics of a vector of utilization rates ------------------------
compute_stats <- function(x) {
  x <- as.numeric(x)
  n <- sum(!is.na(x))
  mean_prop <- mean(x, na.rm = TRUE)
  sd_prop   <- sd(x, na.rm = TRUE)
  list(N           = n,
       min_prop    = min(x, na.rm = TRUE),
       q1_prop     = as.numeric(quantile(x, 0.25, na.rm = TRUE)),
       median_prop = median(x, na.rm = TRUE),
       mean_prop   = mean_prop,
       q3_prop     = as.numeric(quantile(x, 0.75, na.rm = TRUE)),
       max_prop    = max(x, na.rm = TRUE),
       sd_prop     = sd_prop,
       ci95_low    = mean_prop - 1.96 * sd_prop / sqrt(n),
       ci95_high   = mean_prop + 1.96 * sd_prop / sqrt(n))
}
