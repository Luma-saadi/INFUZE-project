
---

# 📄 File 2 of 11: `00_setup.R`

```r
# ============================================================================
# 00_setup.R
# ----------------------------------------------------------------------------
# Environment, helper functions, working directories, shared constants.
# Must be sourced first — every other script depends on objects created here.
#
# Depends on: nothing
# Produces  : run_start_time, cp_report(), write_csv_zipped(),
#             save_checkpoint(), load_checkpoint(),
#             base_dir, data_dir, out_dir, local_dir
# ============================================================================

rm(list = ls())
gc()

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(patchwork)
library(lubridate)
library(stringr)
library(data.table)
library(readr)

if (!require(zip)) install.packages("zip")
library(zip)

run_start_time <- Sys.time()
cat("\n\n########## FULL SCRIPT RUN STARTED:", format(run_start_time), "##########\n\n")

# ============================================
# WORKING DIRECTORIES
# ============================================
# base_dir  : top-level project folder — the only path you must set manually.
# data_dir  : folder holding input data files (defaults to base_dir).
# out_dir   : folder for outputs — checkpoints, workbooks, figures, CSVs.
# local_dir : scratch folder for checkpoint .rds files.

base_dir  <- "C:/RWork/UK_Fleet_Model"        # <-- EDIT THIS
data_dir  <- base_dir                          # <-- EDIT if inputs live elsewhere
out_dir   <- file.path(base_dir, "outputs")
local_dir <- file.path(base_dir, "checkpoints")

for (d in c(base_dir, out_dir, local_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ============================================
# CHECKPOINT HELPERS
# ============================================

# ---
# Simple 2019 vs 2023 fleet total report used to verify fleet preservation
# at intermediate pipeline stages.
# ---
cp_report <- function(df, count_col, label, years = c(2019, 2023)) {
  totals <- df %>%
    group_by(Data_Year) %>%
    summarise(Total = sum(.data[[count_col]], na.rm = TRUE), .groups = "drop") %>%
    filter(Data_Year %in% years)
  cat("\n>>> CHECKPOINT:", label, "\n")
  print(totals)
  invisible(totals)
}

# ---
# Write a data frame to CSV, zip it, delete the uncompressed CSV.
# Kept as a thin wrapper so callers can switch to gzip/fwrite cleanly.
# ---
write_csv_zipped <- function(df, filename, row.names = FALSE) {
  csv_name <- filename
  zip_name <- paste0(tools::file_path_sans_ext(filename), ".zip")

  write.csv(df, csv_name, row.names = row.names)
  zip::zip(zipfile = zip_name, files = csv_name)
  file.remove(csv_name)

  cat("Saved:", zip_name, "(", round(file.size(zip_name)/1e6, 1), "MB )\n")
}

# ---
# Save an object to the checkpoint folder as .rds
# ---
save_checkpoint <- function(obj, name, dir = local_dir) {
  path <- file.path(dir, paste0(name, ".rds"))
  saveRDS(obj, path)
  cat("Checkpoint saved:", path, "(", round(file.size(path)/1e6, 1), "MB )\n")
}

# ---
# Load a checkpoint .rds back into the session
# ---
load_checkpoint <- function(name, dir = local_dir) {
  path <- file.path(dir, paste0(name, ".rds"))
  if (!file.exists(path)) stop("Checkpoint not found: ", path)
  readRDS(path)
}

# ============================================
# SHARED CONSTANTS
# ============================================

# Engine-size break points used throughout the classification pipeline (cc)
engine_breaks <- c(0, 1000, 1400, 2000, 3000, Inf)

# CCC surface-transport carbon budget (MtCO2), 2024–2035
ccc_surface_residual <- data.frame(
  Year = 2024:2035,
  CCC_Surface_CO2_Mt = c(98, 92, 87, 81, 76, 69, 62, 55, 49, 43, 38, 32)
)

# Car share of the surface-transport budget (CCC Balanced Net Zero Pathway)
car_share <- 0.602
#source [Climate Change Committee(CCC), “Progress in reducing emissions – 2025 report to Parliament,” UK, Jun. 2025. Accessed: Apr. 15, 2026. [Online]. Available: https://www.theccc.org.uk/publication/progress-in-reducing-emissions-2025-report-to-parliament/]
