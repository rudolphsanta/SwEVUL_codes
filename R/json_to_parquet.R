#------------------------------------------------------------------------------
# json_to_parquet.R - raw status snapshots (.json) -> monthly .csv -> .parquet
#
# Input:  <SWEVUL_RAW>/YYYY-MM/data-YYYY-MM-DD-HH-MM-SS.json
# Output: Swiss-EVChg-Data_Parquet/YYYY-MM_charging.parquet,
#         output/snapshot_counts.csv. Existing months are skipped.
#------------------------------------------------------------------------------

source('config.R')
suppressPackageStartupMessages({
  library(dplyr); library(jsonlite); library(stringr); library(lubridate)
  library(parallel); library(data.table); library(arrow)
})

#--- functions ------------------------------------------------------------------
safe_fromJSON <- function(path) {
  tryCatch(jsonlite::fromJSON(path), error = function(e) {
    warning('Failed to parse file: ', path, ' - ', conditionMessage(e))
    NULL
  })
}

# one snapshot -> data frame with one row per EVSE
json_to_df <- function(json_file_path) {
  json_data <- safe_fromJSON(json_file_path)
  if (is.null(json_data)) return(NULL)

  timestamp_rounded <- basename(json_file_path) %>%
    str_remove('data-') %>%
    str_remove('.json') %>%
    as.POSIXct(format = '%Y-%m-%d-%H-%M-%S', tz = 'Europe/Zurich') %>%
    lubridate::round_date(unit = '5 minutes')

  # one block of EVSE status records per operator
  records <- json_data$EVSEStatuses$EVSEStatusRecord
  all_op_statuses_df <- data.frame()
  for (n in seq_along(records)) {
    this_op <- records[[n]]
    if (is.null(this_op) || nrow(this_op) == 0) next
    this_op$OperatorID   <- json_data$EVSEStatuses$OperatorID[[n]]
    this_op$OperatorName <- json_data$EVSEStatuses$OperatorName[[n]]
    all_op_statuses_df <- rbind(all_op_statuses_df, this_op)
  }

  all_op_statuses_df %>%
    transmute(datetime     = timestamp_rounded,
              EvseID       = as.factor(EvseID),
              EVSEStatus   = as.factor(EVSEStatus),
              OperatorID   = as.factor(OperatorID),
              OperatorName = as.factor(OperatorName))
}

# all snapshots of one month -> one .csv
raw_data_merge_parallel <- function(root_folder, sub_folder, output_file_path) {
  start <- Sys.time()
  files <- list.files(file.path(root_folder, sub_folder), full.names = TRUE, pattern = '\\.json$')
  # filter only those files with minutes ending in 0 or 5
  files <- files[grepl('-[0-5][05]-\\d{2}\\.json$', files)]
  if (length(files) == 0) {
    message('No .json files for ', sub_folder, ', skipping.')
    return(invisible(NULL))
  }

  cl <- makeCluster(max(1, ifelse(n.cores <= 0, detectCores() + n.cores, n.cores)))
  on.exit(stopCluster(cl), add = TRUE)
  clusterExport(cl, varlist = c('json_to_df', 'safe_fromJSON'))
  clusterEvalQ(cl, {library(jsonlite); library(dplyr); library(stringr); library(lubridate)})
  month_df <- bind_rows(parLapply(cl, files, json_to_df))

  # write the time explicitly; R >= 4.3 otherwise drops '00:00:00' at midnight
  month_df$datetime <- format(month_df$datetime, '%Y-%m-%d %H:%M:%S')
  write.csv(month_df, file = output_file_path, row.names = FALSE)
  cat(sub_folder, ':', length(files), 'files,', nrow(month_df), 'rows,',
      round(difftime(Sys.time(), start, units = 'mins'), 1), 'min\n')
  invisible(NULL)
}

# observed and possible 5-minute snapshots of one month, from the file names
count_5min_snapshots <- function(root_folder, sub_folder) {
  files     <- list.files(file.path(root_folder, sub_folder), pattern = '\\.json$')
  files     <- files[grepl('-[0-5][05]-\\d{2}\\.json$', files)]
  datetimes <- as.POSIXct(str_extract(files, '\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}-\\d{2}'),
                          format = '%Y-%m-%d-%H-%M-%S', tz = 'Europe/Zurich')
  datetimes <- datetimes[!is.na(datetimes)]
  month_start <- ymd_hms(paste0(sub_folder, '-01 00:00:00'), tz = 'Europe/Zurich')
  possible    <- seq(from = month_start, to = month_start + months(1) - minutes(5), by = '5 min')
  data.frame(month              = sub_folder,
             json_files         = length(files),
             snapshots_observed = length(unique(round_date(datetimes, '5 minutes'))),
             snapshots_possible = length(possible))
}

csv_to_parquet <- function(csv_path, dest_folder) {
  parquet_path <- file.path(dest_folder, paste0(tools::file_path_sans_ext(basename(csv_path)), '.parquet'))
  if (file.exists(parquet_path)) {
    message('Skipping (already exists): ', parquet_path)
    return(invisible(NULL))
  }
  d <- arrow::arrow_table(fread(csv_path, fill = TRUE))
  d$datetime <- d$datetime$cast(arrow::timestamp('us'))   # Swiss local time, no time-zone label
  arrow::write_parquet(d, parquet_path)
  message('Converted: ', csv_path, ' -> ', parquet_path)
}

#--- run ------------------------------------------------------------------------
dir.create(path.status.csv, showWarnings = FALSE, recursive = TRUE)
dir.create(path.status,     showWarnings = FALSE, recursive = TRUE)

month_vec <- format(seq(as.Date(paste0(first.month, '-01')), as.Date(paste0(last.month, '-01')), by = 'month'), '%Y-%m')

# 1. .json -> monthly .csv
for (month in month_vec) {
  output_path <- file.path(path.status.csv, paste0(month, '_charging.csv'))
  if (file.exists(output_path)) {
    cat('Skipping', month, '- .csv exists.\n')
    next
  }
  raw_data_merge_parallel(path.raw, month, output_path)
}

# 2. monthly .csv -> .parquet
for (csv_path in list.files(path.status.csv, pattern = '\\.csv$', full.names = TRUE)) csv_to_parquet(csv_path, path.status)

# 3. observed vs. possible snapshots over the study period
snapshot_counts <- bind_rows(lapply(month_vec, function(m) count_5min_snapshots(path.raw, m)))
write.csv(snapshot_counts, file.path(path.out, 'snapshot_counts.csv'), row.names = FALSE)
cat('Snapshots observed:', sum(snapshot_counts$snapshots_observed), 'of', sum(snapshot_counts$snapshots_possible), 'possible.\n')
