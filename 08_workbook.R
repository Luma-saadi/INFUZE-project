# ============================================================================
# 08_workbook.R
# ----------------------------------------------------------------------------
# Excel workbook export of all key outputs, plus a compressed CSV export of
# the full fleet composition.
#
# Design: this script is standalone-runnable provided the checkpoints from
# 04-07 exist. All required objects are read back via readRDS().
#
# Depends on: 00_setup.R (for out_dir) and the saved checkpoints
# Produces  : UK_Fleet_Model_Outputs.xlsx (20 sheets)
#             Fleet_Composition_Full.csv.gz
# ============================================================================

rm(list = ls())
gc()

library(tidyverse)
library(openxlsx)
library(data.table)
library(arrow)

out_dir <- "C:/RWork/UK_Fleet_Model/outputs"  # <-- EDIT to match 00_setup.R

# ----------------------------------------------------------------------------
# Read back checkpoints
# ----------------------------------------------------------------------------
retirement_avg        <- readRDS(file.path(out_dir, "retirement_avg.rds"))
fleet_2024_complete   <- readRDS(file.path(out_dir, "fleet_2024_complete.rds"))
fleet_complete        <- readRDS(file.path(out_dir, "fleet_complete.rds"))
fuel_shares_complete  <- readRDS(file.path(out_dir, "fuel_shares_complete.rds"))
natcop_targets        <- readRDS(file.path(out_dir, "natcop_targets.rds"))
inflow_unique         <- readRDS(file.path(out_dir, "inflow_unique.rds"))
engine_sales_distribution <- readRDS(file.path(out_dir, "engine_sales_distribution.rds"))
smmt_sales_forecast   <- readRDS(file.path(out_dir, "smmt_sales_forecast.rds"))
sales_by_fuel_smmt    <- readRDS(file.path(out_dir, "sales_by_fuel_smmt.rds"))
fleet_all             <- readRDS(file.path(out_dir, "fleet_all.rds"))
origin_by_fuel        <- readRDS(file.path(out_dir, "origin_by_fuel.rds"))
base_fleet_2024       <- readRDS(file.path(out_dir, "base_fleet_2024.rds"))
growth_rates          <- readRDS(file.path(out_dir, "growth_rates.rds"))
weight_model          <- readRDS(file.path(out_dir, "weight_model.rds"))
bev_country_2024      <- readRDS(file.path(out_dir, "bev_country_2024.rds"))
bev_new_region        <- readRDS(file.path(out_dir, "bev_new_region.rds"))
origin_scenarios_year <- readRDS(file.path(out_dir, "origin_scenarios_year.rds"))

# Results objects
results <- list()
results$fleet           <- readRDS(file.path(out_dir, "results_baseline_fleet.rds"))
results$accounting      <- readRDS(file.path(out_dir, "accounting_baseline.rds"))
results$retired_by_fuel <- readRDS(file.path(out_dir, "retired_by_fuel_baseline.rds"))
results$sales_by_fuel   <- readRDS(file.path(out_dir, "sales_by_fuel_baseline.rds"))

# ============================================
# Create workbook
# ============================================
wb <- createWorkbook()

# ----------------------------------------------------------------------------
# SHEET 1: Fuel Groups and Sale Percentages
# ----------------------------------------------------------------------------
fuel_shares_export <- fuel_shares_complete %>%
  rename(
    Sale_Percentage = sale_percentage,
    Fuel_Type = Fuel_Group
  ) %>%
  arrange(Year, Fuel_Type)

addWorksheet(wb, "Fuel_Shares")
writeData(wb, "Fuel_Shares", fuel_shares_export)
addStyle(wb, "Fuel_Shares",
         style = createStyle(numFmt = "0.00%"),
         rows = 2:(nrow(fuel_shares_export)+1),
         cols = 3, gridExpand = TRUE)

# ----------------------------------------------------------------------------
# SHEET 2: NATCOP Targets by Year
# ----------------------------------------------------------------------------
natcop_export <- natcop_targets %>%
  rename(
    NATCOP_Target = Target_Fleet,
    Growth_Rate = Annual_Growth_Rate,
    Core_Forecast = Core_Model
  ) %>%
  mutate(
    NATCOP_Target_Millions = NATCOP_Target / 1e6,
    Core_Forecast_Millions = Core_Forecast / 1e6
  )

addWorksheet(wb, "NATCOP_Targets")
writeData(wb, "NATCOP_Targets", natcop_export)
addStyle(wb, "NATCOP_Targets",
         style = createStyle(numFmt = "#,##0"),
         rows = 2:(nrow(natcop_export)+1),
         cols = c(2,4), gridExpand = TRUE)
addStyle(wb, "NATCOP_Targets",
         style = createStyle(numFmt = "0.00%"),
         rows = 2:(nrow(natcop_export)+1),
         cols = 3, gridExpand = TRUE)

# ----------------------------------------------------------------------------
# SHEET 3: Retirement Rates by Age and Fuel Type
# ----------------------------------------------------------------------------
retirement_wide <- retirement_avg %>%
  select(Fuel_Group, Vehicle_Age, Retirement_Rate) %>%
  pivot_wider(
    names_from = Fuel_Group,
    values_from = Retirement_Rate,
    values_fill = 0
  ) %>%
  arrange(Vehicle_Age) %>%
  rename(Age = Vehicle_Age)

addWorksheet(wb, "Retirement_Rates")
writeData(wb, "Retirement_Rates", retirement_wide)
for(col in 2:ncol(retirement_wide)) {
  addStyle(wb, "Retirement_Rates",
           style = createStyle(numFmt = "0.00%"),
           rows = 2:(nrow(retirement_wide)+1),
           cols = col, gridExpand = TRUE)
}

# ----------------------------------------------------------------------------
# SHEET 4: Survival Rates by Age and Fuel Type
# ----------------------------------------------------------------------------
survival_wide <- retirement_avg %>%
  select(Fuel_Group, Vehicle_Age, Survival_Rate) %>%
  pivot_wider(
    names_from = Fuel_Group,
    values_from = Survival_Rate,
    values_fill = 0
  ) %>%
  arrange(Vehicle_Age) %>%
  rename(Age = Vehicle_Age)

addWorksheet(wb, "Survival_Rates")
writeData(wb, "Survival_Rates", survival_wide)
for(col in 2:ncol(survival_wide)) {
  addStyle(wb, "Survival_Rates",
           style = createStyle(numFmt = "0.00%"),
           rows = 2:(nrow(survival_wide)+1),
           cols = col, gridExpand = TRUE)
}

# ----------------------------------------------------------------------------
# SHEET 5: Annual New Sales by Fuel Type
# ----------------------------------------------------------------------------
sales_by_fuel <- bind_rows(results$fleet) %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Year, Fuel_Group) %>%
  summarise(New_Sales = sum(Fleet), .groups = "drop") %>%
  pivot_wider(
    names_from = Fuel_Group,
    values_from = New_Sales,
    values_fill = 0
  ) %>%
  arrange(Year) %>%
  mutate(
    Total_Sales = rowSums(select(., -Year)),
    Total_Sales_Millions = Total_Sales / 1e6
  )

addWorksheet(wb, "New_Sales_by_Fuel")
writeData(wb, "New_Sales_by_Fuel", sales_by_fuel)
for(col in 2:ncol(sales_by_fuel)) {
  if(col <= ncol(sales_by_fuel)-1) {
    addStyle(wb, "New_Sales_by_Fuel",
             style = createStyle(numFmt = "#,##0"),
             rows = 2:(nrow(sales_by_fuel)+1),
             cols = col, gridExpand = TRUE)
  }
}

# ----------------------------------------------------------------------------
# SHEET 6: Annual Retired Vehicles by Fuel Type
# ----------------------------------------------------------------------------
addWorksheet(wb, "Retired_by_Fuel")
writeData(wb, "Retired_by_Fuel", results$retired_by_fuel)

addStyle(wb, "Retired_by_Fuel",
         style = createStyle(numFmt = "#,##0"),
         rows = 2:(nrow(results$retired_by_fuel)+1),
         cols = 6, gridExpand = TRUE)

# ----------------------------------------------------------------------------
# SHEET 7: Complete Fleet Composition (Age x Fuel x Year) WITH ENGINE SIZE and MILEAGE
# ----------------------------------------------------------------------------
addWorksheet(wb, "Fleet_Composition_Full")
writeData(wb, "Fleet_Composition_Full", fleet_all)

addStyle(wb, "Fleet_Composition_Full",
         style = createStyle(numFmt = "#,##0"),
         rows = 2:(nrow(fleet_all)+1),
         cols = 4, gridExpand = TRUE)

addStyle(wb, "Fleet_Composition_Full",
         style = createStyle(numFmt = "#,##0"),
         rows = 2:(nrow(fleet_all)+1),
         cols = 7:8, gridExpand = TRUE)

addStyle(wb, "Fleet_Composition_Full",
         style = createStyle(textDecoration = "bold", fontSize = 12),
         rows = 1, cols = 1:8, gridExpand = TRUE)

# ----------------------------------------------------------------------------
# SHEET 8: Summary Statistics
# ----------------------------------------------------------------------------
summary_stats <- data.frame(
  Metric = c(
    "2024 Base Fleet",
    "2024 New Sales",
    "2025 New Sales",
    "2037 New Sales",
    "Total New Sales (2025-2037)",
    "Total Retirements (2025-2037)",
    "2037 Target Fleet",
    "2037 Actual Fleet",
    "Difference from Target",
    "Average Annual Sales (2025-2037)"
  ),
  Value = c(
    sum(base_fleet_2024$Fleet_Count),
    sales_by_fuel$Total_Sales[sales_by_fuel$Year == 2024],
    sales_by_fuel$Total_Sales[sales_by_fuel$Year == 2025],
    sales_by_fuel$Total_Sales[sales_by_fuel$Year == 2037],
    sum(sales_by_fuel$Total_Sales[sales_by_fuel$Year >= 2025]),
    sum(
      results$retired_by_fuel$Retired[
        results$retired_by_fuel$Year >= 2025 &
          results$retired_by_fuel$Year <= 2037
      ],
      na.rm = TRUE
    ),
    natcop_targets$Target_Fleet[natcop_targets$Year == 2037],
    results$accounting$Final_Fleet[results$accounting$Year == 2037],
    results$accounting$Final_Fleet[results$accounting$Year == 2037] -
      natcop_targets$Target_Fleet[natcop_targets$Year == 2037],
    mean(sales_by_fuel$Total_Sales[sales_by_fuel$Year >= 2025])
  )
) %>%
  mutate(
    Value_Millions = Value / 1e6,
    Value_Formatted = ifelse(
      abs(Value) > 1e6,
      paste0(format(round(Value/1e6, 2), nsmall = 2), "M"),
      format(round(Value, 0), big.mark = ",")
    )
  )

addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_stats)

# ----------------------------------------------------------------------------
# SHEET 9: Accounting Summary
# ----------------------------------------------------------------------------
accounting_export <- results$accounting %>%
  mutate(
    Target_Fleet_Millions = Target_Fleet / 1e6,
    Starting_Fleet_Millions = Starting_Fleet / 1e6,
    Predefined_Sales_Millions = Predefined_Sales / 1e6,
    Natural_Retirements_Millions = Natural_Retirements / 1e6,
    Final_Fleet_Millions = Final_Fleet / 1e6,
    Error_Millions = Error / 1e6
  )

addWorksheet(wb, "Accounting")
writeData(wb, "Accounting", accounting_export)

# ----------------------------------------------------------------------------
# SHEET 10: Retirement Rates Summary
# ----------------------------------------------------------------------------
retirement_summary <- retirement_avg %>%
  group_by(Fuel_Group) %>%
  summarise(
    Avg_Retirement_Rate = mean(Retirement_Rate),
    Min_Retirement_Rate = min(Retirement_Rate),
    Max_Retirement_Rate = max(Retirement_Rate),
    Rate_at_Age_10 = Retirement_Rate[Vehicle_Age == 10],
    Rate_at_Age_15 = Retirement_Rate[Vehicle_Age == 15],
    Rate_at_Age_20 = Retirement_Rate[Vehicle_Age == 20],
    Rate_at_Age_25 = Retirement_Rate[Vehicle_Age == 25],
    .groups = "drop"
  )

addWorksheet(wb, "Retirement_Summary")
writeData(wb, "Retirement_Summary", retirement_summary)
for(col in 2:ncol(retirement_summary)) {
  addStyle(wb, "Retirement_Summary",
           style = createStyle(numFmt = "0.00%"),
           rows = 2:(nrow(retirement_summary)+1),
           cols = col, gridExpand = TRUE)
}

# ----------------------------------------------------------------------------
# SHEET 11: Origin Scenarios by Fuel Type
# ----------------------------------------------------------------------------
origin_scenarios <- origin_by_fuel %>%
  filter(Data_Year %in% 2019:2024) %>%
  group_by(Region, Fuel_Group) %>%
  summarise(
    Avg_Share = mean(Share_Within_Fuel, na.rm = TRUE),
    Std_Share = sd(Share_Within_Fuel, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Std_Share = ifelse(is.na(Std_Share), 0, Std_Share),
         Base = round(Avg_Share, 1),
         Plus = round(Avg_Share + Std_Share, 1),
         Minus = round(Avg_Share - Std_Share, 1),
         Minus = ifelse(Minus < 0, 0, Minus)
  ) %>%
  group_by(Fuel_Group) %>%
  mutate(
    Base = round(100 * Base / sum(Base), 1),
    Plus = round(100 * Plus / sum(Plus), 1),
    Minus = round(100 * Minus / sum(Minus), 1)
  ) %>%
  ungroup() %>%
  arrange(Fuel_Group, desc(Base)) %>%
  select(Region, Fuel_Group, Base, Plus, Minus)
print(origin_scenarios, n = Inf)

addWorksheet(wb, "Origin_Scenarios")
writeData(wb, "Origin_Scenarios", origin_scenarios)
for(col in c("Base", "Plus", "Minus")) {
  addStyle(wb, "Origin_Scenarios",
           style = createStyle(numFmt = "0.0"),
           rows = 2:(nrow(origin_scenarios)+1),
           cols = which(names(origin_scenarios) == col),
           gridExpand = TRUE)
}

# ----------------------------------------------------------------------------
# SHEET 12: Origin Distribution by Year (Base)
# ----------------------------------------------------------------------------
apply_origin_scenario <- function(results, origin_scenarios, scenario_name = "Base") {
  sales_by_fuel <- results$sales_by_fuel

  region_shares <- origin_scenarios %>%
    group_by(Fuel_Group, Region) %>%
    summarise(Share = sum(!!sym(scenario_name), na.rm = TRUE), .groups = "drop")

  sales_with_origin <- sales_by_fuel %>%
    left_join(region_shares, by = "Fuel_Group", relationship = "many-to-many") %>%
    mutate(
      Vehicles_By_Origin = Required_Sales * Share / 100,
      Scenario = scenario_name)

  return(sales_with_origin)
}

sales_base  <- apply_origin_scenario(results, origin_scenarios, "Base")
sales_plus  <- apply_origin_scenario(results, origin_scenarios, "Plus")
sales_minus <- apply_origin_scenario(results, origin_scenarios, "Minus")
all_origin_scenarios <- bind_rows(sales_base, sales_plus, sales_minus)

origin_summary <- all_origin_scenarios %>%
  group_by(Scenario, Year, Region) %>%
  summarise(Vehicles = sum(Vehicles_By_Origin, na.rm = TRUE), .groups = "drop") %>%
  group_by(Scenario, Year) %>%
  mutate(Share = round(100 * Vehicles / sum(Vehicles), 1)) %>%
  ungroup()

origin_wide_base <- origin_summary %>%
  filter(Scenario == "Base") %>%
  select(Year, Region, Share) %>%
  pivot_wider(names_from = Region, values_from = Share, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric) & !c(Year)), na.rm = TRUE))
print(origin_wide_base)

addWorksheet(wb, "Origin_By_Year_Base_newsales")
writeData(wb, "Origin_By_Year_Base_newsales", origin_wide_base)
for(col in 2:ncol(origin_wide_base)) {
  addStyle(wb, "Origin_By_Year_Base_newsales",
           style = createStyle(numFmt = "0.0"),
           rows = 2:(nrow(origin_wide_base)+1),
           cols = col, gridExpand = TRUE)
}

# ----------------------------------------------------------------------------
# SHEET 13: Origin Sales Detail (All Scenarios)
# ----------------------------------------------------------------------------
addWorksheet(wb, "Origin_Sales_Detail")
writeData(wb, "Origin_Sales_Detail", all_origin_scenarios)
addStyle(wb, "Origin_Sales_Detail",
         style = createStyle(numFmt = "#,##0"),
         rows = 2:(nrow(all_origin_scenarios)+1),
         cols = which(names(all_origin_scenarios) %in% c("Required_Sales", "Vehicles_By_Origin")),
         gridExpand = TRUE)

# ----------------------------------------------------------------------------
# SHEET 14: Fleet Composition by Region
# ----------------------------------------------------------------------------
fleet_by_origin <- fleet_all %>%
  group_by(Year, Region) %>%
  summarise(Fleet = sum(Fleet, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Region, values_from = Fleet, values_fill = 0) %>%
  mutate(
    Total = rowSums(across(where(is.numeric) & !matches("Year")), na.rm = TRUE),
    Total_Millions = Total / 1e6
  )

cat("\n=== FLEET BY ORIGIN ===\n")
print(fleet_by_origin, n = Inf)

addWorksheet(wb, "fleet_by_Region")
writeData(wb, "fleet_by_Region", fleet_by_origin)

for(col in 2:ncol(fleet_by_origin)) {
  if(col < ncol(fleet_by_origin)) {
    addStyle(wb, "fleet_by_Region",
             style = createStyle(numFmt = "#,##0"),
             rows = 2:(nrow(fleet_by_origin)+1),
             cols = col, gridExpand = TRUE)
  }
}

# ----------------------------------------------------------------------------
# SHEET 15: Region Share by Year (%)
# ----------------------------------------------------------------------------
region_share_by_year <- fleet_all %>%
  group_by(Year, Region) %>%
  summarise(
    Fleet = sum(Fleet, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Year) %>%
  mutate(
    Share = round(100 * Fleet / sum(Fleet), 1)
  ) %>%
  ungroup() %>%
  select(Year, Region, Share) %>%
  pivot_wider(
    names_from = Region,
    values_from = Share,
    values_fill = 0
  ) %>%
  mutate(
    Total = rowSums(across(where(is.numeric) & !matches("Year")), na.rm = TRUE)
  )

addWorksheet(wb, "Region_Share_By_Year")
writeData(wb, "Region_Share_By_Year", region_share_by_year)

for(col in 2:ncol(region_share_by_year)) {
  addStyle(wb, "Region_Share_By_Year",
           style = createStyle(numFmt = "0.0"),
           rows = 2:(nrow(region_share_by_year)+1),
           cols = col, gridExpand = TRUE)
}

# ----------------------------------------------------------------------------
# SHEET 16: Origin Sales Distribution by Year (from projection)
# ----------------------------------------------------------------------------
origin_distribution_export <- origin_scenarios_year %>%
  filter(Scenario == "Base") %>%
  select(Year, Fuel_Group, Region, Origin, Origin_Share = Share_exact) %>%
  pivot_wider(
    names_from = c(Region, Origin),
    values_from = Origin_Share,
    values_fill = 0,
    names_sep = "_"
  ) %>%
  arrange(Year, Fuel_Group)
print(origin_distribution_export)

addWorksheet(wb, "Origin_Distribution_Used")
writeData(wb, "Origin_Distribution_Used", origin_distribution_export)

for(col in 3:ncol(origin_distribution_export)) {
  addStyle(wb, "Origin_Distribution_Used",
           style = createStyle(numFmt = "0.0"),
           rows = 2:(nrow(origin_distribution_export)+1),
           cols = col, gridExpand = TRUE)
}

# ----------------------------------------------------------------------------
# Column widths
# ----------------------------------------------------------------------------
for(sheet in seq_along(wb$worksheets)) {
  setColWidths(wb, sheet = sheet, cols = 1:16, widths = 15)
}

# ============================================
# SAVE WORKBOOK
# ============================================
output_dir <- out_dir

xlsx_path <- file.path(output_dir, "UK_Fleet_Model_Outputs.xlsx")
saveWorkbook(wb, xlsx_path, overwrite = TRUE)
cat("\n=== WORKBOOK SAVED:", xlsx_path, "===\n")

# ============================================
# SAVE FLEET DATA AS GZIPPED CSV
# ============================================
csv_gz_path <- file.path(output_dir, "Fleet_Composition_Full.csv.gz")

fwrite(fleet_all,
       csv_gz_path,
       row.names = FALSE,
       sep = ",",
       dec = ".",
       nThread = 4,
       verbose = FALSE,
       showProgress = FALSE,
       scipen = 999,
       compress = "gzip")

cat("File created: Fleet_Composition_Full.csv.gz\n")

fleet_data <- fread(csv_gz_path)

cat("Rows in saved file:", nrow(fleet_data), "\n")
cat("Columns in saved file:", ncol(fleet_data), "\n")
head(fleet_data)

cat("\n", strrep("=", 60), "\n")
cat("FILE SAVED SUCCESSFULLY")
cat("\n", strrep("=", 60), "\n")
cat("Path:", csv_gz_path, "\n")
cat("File size (MB):", round(file.size(csv_gz_path) / 1e6, 1), "\n")
cat("\nData summary:\n")
cat("  - Rows:", format(nrow(fleet_data), big.mark = ","), "\n")
cat("  - Columns:", ncol(fleet_data), "\n")
cat("  - Years:", paste(unique(fleet_data$Year), collapse = ", "), "\n")
cat(strrep("=", 60), "\n")

cat("\n=== 08_workbook.R complete ===\n")
