# INFUZE-project
R code for STOCK-Flow-Stock-Flow-Cohort-Model
# UK Vehicle Fleet Modelling Pipeline

This repository contains the R pipeline used to build, project, and analyse the UK car fleet
from 2024 through 2037, covering fuel-mix composition, vehicle age, manufacturing
origin, mileage, and CO₂ emissions under multiple policy scenarios.

The work underpins a paper examining how ZEV mandate rowback and car-sharing assumptions
affect the UK's ability to meet its Climate Change Committee (CCC) surface-transport carbon
budget.

---

## Repository layout

| File | Purpose |
|------|---------|
| `00_setup.R`                   | Environment, helper functions, `base_dir`, and shared constants |
| `01_data_prep.R`               | Load and clean raw DVLA/DfT vehicle licensing data; BEV kerb-weight matching |
| `02_fleet_classification.R`    | Engine-size classification by fuel type; fleet age distribution and cut-off |
| `03_origin_analysis.R`         | Manufacturing-origin concordance, region mapping, fleet origin assignment |
| `04_survival_retirement.R`     | Survival curves, retirement-rate estimation, and the four evidence-based adjustments |
| `05_scenarios.R`               | NATCOP targets, fuel-share forecasts, and the four policy scenarios |
| `06_emissions_model.R`         | Real-world-anchored emissions model with reconciliation checks |
| `07_mileage_scenarios.R`       | VKT-conserving mileage reallocation scenarios |
| `08_workbook.R`                | Excel workbook export of all key outputs |
| `09_figures.R`                 | All figures used in the paper |

---

## Data inputs

The pipeline reads the following inputs (paths configured via `base_dir` in `00_setup.R`):

### Public data (freely available)

- **`VEH0120`** — DfT / DVLA "Vehicles at the end of the quarter by licence status, body
  type, make, generic model and mode: Great Britain."
  <https://www.gov.uk/government/statistical-data-sets/vehicle-licensing-statistics-data-files>

- **`VEH0220`** — DfT / DVLA "Vehicles registered for the first time by body type, make,
  generic model, model, engine size, and licence status." Same source as above.

- **`VEH0124`** — DfT / DVLA "Vehicles registered for the first time by body type, make,
  model, year of first registration and licence status: Great Britain." Same source.

  The repository refers to two derived splits of VEH0124:

  - `df_VEH0124_NZ (updated).xlsx`
  - `df_VEH0124_AM (updated).xlsx`

  **Note:** the `_NZ` and `_AM` suffixes are internal labels used in this project to
  distinguish two pre-processed partitions of the published VEH0124 file. They are not
  part of the public dataset naming.

- **DfT road traffic estimates** — total GB vehicle mileage by year, used for calibration
  validation. <https://roadtraffic.dft.gov.uk/>

- **NAEI 2019 cumulative survival curve** — reference survival probabilities by vehicle
  age and fuel type. <https://naei.beis.gov.uk/>

- **NTEM 8.0 Core / NATCOP** — National Trip End Model household car-ownership probability
  files for 2021, 2026, 2031, 2036, 2041, 2046. supplied via DfT (Roger Witte)

### Third-party data provided for this project

- **`BEVrevenueWeight2024.csv`** — DVLA revenue weight for Battery Electric Vehicles,
  supplied via RAC Foundation (Tim Chatterton).

- **`uk_fleet_enriched_v2.zip`** — enriched vehicle dimension file (kerb weight, length,
  width, height, wheelbase) by Make / Model / Year, supplied via RAC Foundation.

- **`Mileage_CO2_data.csv`** — MOT-derived average annual mileage and CO₂ emissions by
  vehicle age, engine size band, and fuel type. Supplied via RAC Foundation.

### Policy / reference inputs (embedded in code)

- **CCC Sixth Carbon Budget** — surface-transport carbon budget trajectory (2024–2035)
  and the 2035 car allocation.
- **SMMT** new-car sales forecast for 2025–2027.
- **T&E (2025)** and **ICCT (2024)** real-world emissions factors for PHEV and HEV.
- **Nguyen-Tien et al. (2024, CEP/LSE)** — BEV longevity analysis (>300 M MOT records).

Full citations appear inline in the relevant scripts.

---

## Setup

```r
# 1. Open 00_setup.R and set base_dir to the folder holding your data files
#    and to which outputs will be written.
# 2. Set data_dir if input data live in a different folder from outputs.
# 3. Source scripts in numeric order:

source("00_setup.R")
source("01_data_prep.R")
source("02_fleet_classification.R")
source("03_origin_analysis.R")
source("04_survival_retirement.R")
source("05_scenarios.R")
source("06_emissions_model.R")
source("07_mileage_scenarios.R")
source("08_workbook.R")        # requires openxlsx
source("09_figures.R")
