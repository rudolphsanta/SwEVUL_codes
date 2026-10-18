#------------------------------------------------------------------------------
# config.R - paths and settings. Run the scripts from the repository root,
# e.g. Rscript R/charging_load.R
#
# SWEVUL_DATA:     the SwEVUL dataset folder
# SWEVUL_EXTERNAL: folder with the swisstopo and Swissgrid files (see README)
# SWEVUL_RAW:      collected raw .json files (json_to_parquet.R only)
#------------------------------------------------------------------------------

path.data <- Sys.getenv('SWEVUL_DATA', unset = file.path('..', 'SwEVUL'))

# --- dataset folders ----------------------------------------------------------
path.status     <- file.path(path.data, 'Swiss-EVChg-Data_Parquet')
path.load       <- file.path(path.data, 'Swiss-EVLoad-Data_CSV')
path.evse       <- file.path(path.data, 'ChargingStationDetails_19-02-2026.csv')
path.digest     <- file.path(path.data, 'Infrastructure-Digest')

# --- rated power for the charging load ----------------------------------------
# 'static_monthly': each month from its own details table (monthly_evse_details/)
# 'static_single' : all months from path.evse
details.source  <- 'static_monthly'
overwrite.load  <- TRUE      # FALSE: skip months whose load file exists

# --- EVSEs outside Switzerland ------------------------------------------------
# bounding box (decimal degrees, WGS84)
lon.range <- c(5.5, 10.5)
lat.range <- c(45.5, 47.9)
# TRUE: drop EVSEs located outside the box from the utilization and coverage statistics
exclude.foreign <- TRUE

# --- raw collection (json_to_parquet.R) ---------------------------------------
path.raw        <- Sys.getenv('SWEVUL_RAW', unset = file.path('..', 'SwEVUL_raw_json'))
path.status.csv <- file.path(path.raw, '_monthly_csv')          # intermediate monthly .csv

# --- study period and parallel settings ---------------------------------------
first.month <- '2022-12'
last.month  <- '2025-12'
n.cores     <- 4          # <= 0 means "all cores minus this many"

# --- infrastructure digest ----------------------------------------------------
d.path.static <- file.path(path.digest, 'static_archives')    # data_static_YYYY-MM.tar.gz
d.path.details <- file.path(path.digest, 'monthly_evse_details')
d.path.summ   <- file.path(path.digest, 'monthly_summary')
d.path.val    <- file.path(path.digest, 'validation')
d.path.snapshots <- file.path(d.path.summ, 'operator_evse_timeseries.csv')   # snapshot used per month
d.path.bfe    <- file.path(path.digest, 'reference_SFOE',
                           'ich_tanke_strom_Kennzahlen_montlich_2026_08_23.csv')

# --- external inputs (not part of the dataset) --------------------------------
path.external  <- Sys.getenv('SWEVUL_EXTERNAL', unset = 'external')
d.path.gdb     <- file.path(path.external, 'swissBOUNDARIES3D_1_5_LV95_LN02.gdb')
path.swissgrid <- path.external      # EnergieUebersichtCH-YYYY.xlsx

# --- outputs ------------------------------------------------------------------
path.fig      <- 'figures'
path.out      <- 'output'
dir.create(path.fig, showWarnings = FALSE, recursive = TRUE)
dir.create(path.out, showWarnings = FALSE, recursive = TRUE)
