# SwEVUL code

Code for the Swiss Public EV Charging Utilization and Load Dataset (SwEVUL), described in the data descriptor "Long-term, high-resolution dataset of public electric vehicle charging in Switzerland". The dataset is archived on Zenodo (see the data descriptor for the record).

The scripts are provided as a documented starting point for working with the dataset, not as a turnkey pipeline, and without any warranty. This code is not maintained. Expect to adapt paths, memory settings, and analysis choices to your own setup. Paths and settings are in `config.R`.

## Contents

```
config.R                          paths and settings
R/
  functions.R                     shared functions
  json_to_parquet.R               raw status snapshots (.json) -> monthly .parquet
  evse_details_cleaning.R         cleaning of an EVSE details file
  monthly_evse_details.R          one EVSE details table per month from the static archive
  utilization.R                   utilization rates
  charging_load.R                 rated-power-based charging load
  validation_coverage.R           monthly data coverage
  validation_system_load.R        charging load vs. Swiss system load
  build_infrastructure_digest.R   monthly infrastructure digest
  compute_capacity_bands.R        charging-capacity bands and their validation
  validate_counts.R               site and charging-point counts vs. SFOE
collect/
  evse_status_downloader.py       status collector, five-minute interval
  ladestationen_scheduled.py      status and static-file collector started by a scheduler
  EVSEdetails.py                  downloads and flattens the EVSE description file
```

Some scripts depend on the output of others. Run order: `json_to_parquet.R` (only for newly collected raw files); `build_infrastructure_digest.R`, then `compute_capacity_bands.R compute <from YYYY-MM> <to YYYY-MM>`, then `compute_capacity_bands.R assemble`, then `validate_counts.R`; `monthly_evse_details.R`; `charging_load.R`; `utilization.R` and `validation_coverage.R`; `validation_system_load.R`.

## Requirements

R 4.4.2 with the packages `arrow`, `data.table`, `dplyr`, `tidyr`, `lubridate`, `stringr`, `jsonlite`, `ggplot2`, `scales`, `patchwork`, `readxl`, and `sf`. Python 3 with `requests` and `pandas` (collectors only).

Set the environment variable `SWEVUL_DATA` to the extracted dataset folder, and run the scripts from the repository root, e.g. `Rscript R/charging_load.R`.

Two inputs belong to other parties and are not redistributed with the code or the dataset. Place them in one folder and point `SWEVUL_EXTERNAL` at it (default: `external/` beside the scripts):

- Swissgrid `EnergieUebersichtCH-YYYY.xlsx`, 2022-2025: https://www.swissgrid.ch/en/home/operation/grid-data/energy-data.html
- swisstopo swissBOUNDARIES3D, file geodatabase `swissBOUNDARIES3D_1_5_LV95_LN02.gdb`: https://www.swisstopo.admin.ch/en/landscape-model-swissboundaries3d

## Citation

If you use this code or the dataset, please cite the data descriptor: Santarromana R, Gellrich M, Panos E, McKenna R, Kannan R. Long-term, high-resolution dataset of public electric vehicle charging in Switzerland.

## License

MIT License. Copyright (c) 2026 The SwEVUL authors.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
