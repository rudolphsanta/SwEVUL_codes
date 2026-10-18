#------------------------------------------------------------------------------
# monthly_evse_details.R - one EVSE details table per month, from the snapshot
# listed for that month in operator_evse_timeseries.csv
#
# Same columns as EVSEdetails.py. Existing months are skipped.
# Output: Infrastructure-Digest/monthly_evse_details/evse_details_YYYY-MM.csv
#------------------------------------------------------------------------------

source('config.R')
suppressPackageStartupMessages({library(jsonlite); library(data.table)})

nz <- function(x) if (is.null(x)) NA else x     # NULL -> NA
# [['name']], not '$': fac$power would partially match 'powertype'

flatten_static_json <- function(path) {
  d <- fromJSON(path, simplifyVector = FALSE)
  rows <- list()
  for (container in d[['EVSEData']]) {
    for (item in container[['EVSEDataRecord']]) {
      a <- item[['Address']]
      base <- list(EvseID               = nz(item[['EvseID']]),
                   ChargingStationId    = nz(item[['ChargingStationId']]),
                   Accessibility        = nz(item[['Accessibility']]),
                   GeoCoordinates       = nz(item[['GeoCoordinates']][['Google']]),
                   City                 = nz(a[['City']]),
                   Country              = nz(a[['Country']]),
                   PostalCode           = nz(a[['PostalCode']]),
                   Street               = nz(a[['Street']]),
                   ParkingSpot          = nz(a[['ParkingSpot']]),
                   ParkingFacility      = nz(a[['ParkingFacility']]),
                   DynamicInfoAvailable = nz(item[['DynamicInfoAvailable']]),
                   IsOpen24Hours        = nz(item[['IsOpen24Hours']]),
                   LastUpdate           = nz(item[['lastUpdate']]),
                   MaxCapacity          = nz(item[['MaxCapacity']]),
                   DynamicPowerLevel    = nz(item[['DynamicPowerLevel']]),
                   EnergySource         = nz(item[['EnergySource']]))
      facilities <- item[['ChargingFacilities']]
      if (length(facilities) == 0) facilities <- list(list())
      for (fac in facilities) {
        rows[[length(rows) + 1]] <- c(base, list(ChargingFacility_power     = nz(fac[['power']]),
                                                 ChargingFacility_powertype = nz(fac[['powertype']])))
      }
    }
  }
  rbindlist(lapply(rows, function(r) lapply(r, as.character)))
}

snapshots <- read.csv(d.path.snapshots, stringsAsFactors = FALSE)
dir.create(d.path.details, showWarnings = FALSE, recursive = TRUE)
tmp <- file.path(tempdir(), 'swevul_static'); dir.create(tmp, showWarnings = FALSE)

summary_rows <- list()
for (i in seq_len(nrow(snapshots))) {
  ym  <- snapshots$year_month[i]
  out <- file.path(d.path.details, paste0('evse_details_', ym, '.csv'))
  if (!file.exists(out)) {
    archive <- file.path(d.path.static, paste0('data_static_', ym, '.tar.gz'))
    members <- untar(archive, list = TRUE)
    member  <- members[basename(members) == snapshots$snapshot[i]]
    if (length(member) != 1) stop('Snapshot not found (or not unique) in ', archive, ': ', snapshots$snapshot[i])
    untar(archive, files = member, exdir = tmp)
    details <- flatten_static_json(file.path(tmp, member))
    fwrite(details, out)
    unlink(file.path(tmp, member))
  }
  details <- fread(out, colClasses = 'character', na.strings = '')
  summary_rows[[ym]] <- data.frame(year_month = ym, snapshot = snapshots$snapshot[i],
                                   n_rows = nrow(details), n_evse = uniqueN(details$EvseID))
  message(ym, ': ', nrow(details), ' rows, ', uniqueN(details$EvseID), ' EvseIDs')
}
write.csv(do.call(rbind, summary_rows), file.path(path.out, 'monthly_evse_details_summary.csv'), row.names = FALSE)
