# ============================================================================
# 03_origin_analysis.R
# ----------------------------------------------------------------------------
# Vehicle dimensions join (kerb weight, length, width, height, wheelbase) and
# the fleet-projection mileage integration. Establishes base_fleet_2024 and
# the mileage-calibrated Stock_model used by every downstream scenario.
#
# Depends on: 00_setup.R, 01_data_prep.R, 02_fleet_classification.R
# Produces  : dimension_lookup, data_1, Fleet_complete,
#             lookup_level1..lookup_level6, final_complete (fleet_level),
#             fleet_2024_complete, fleet_complete, calibration_factors,
#             Stock_model, Stock_prepared, full_stock_model
# ============================================================================

# ============================================================================
# INTEGRATE VEHICLE MILEAGE
# ============================================================================

library(readxl)
library(arrow)

# Read the mileage / CO2 file
DVLA_1 <- read.csv(file.path(data_dir, "Mileage_CO2_data.csv"))

# Load the classified dataset
join_final_complete_classified <- readRDS(
  file.path(data_dir, "join_final_complete_classified.rds")
)

veh_origin <- read_parquet(file.path(data_dir, "veh_origin.parquet"))

dimension_lookup <- read.csv(
  file.path(data_dir, "dimension_lookup.csv")
)

Vcar_classified <- fread(
  file.path(data_dir, "Vcar_classified.zip")
)

join_final_complete <- read_parquet(file.path(data_dir, "join_final.parquet"))

# -----------------------------------
# Update Fuel in all files (unified)
# -----------------------------------
DVLA <- DVLA_1 %>%
  mutate(fuel_harmonised = case_when(
    fuel == "Petrol" ~ "Petrol",
    fuel == "Diesel" ~ "Diesel",
    fuel == "BEV" ~ "BEV",
    fuel == "HEV" ~ "HEV",
    fuel == "FCEV" ~ "Other",
    fuel == "Other" ~ "Other",
    fuel == "FUEL CELLS" ~ "Other",
    fuel == "Fuel Cell Electric" ~ "Other",
    TRUE ~ "Unknown"
  ))

fuel_table <- table(DVLA$fuel, DVLA$fuel_harmonised, useNA = "always")
print(fuel_table)

DVLA <- DVLA %>%
  filter(Age >= 3 & Age <= 23) %>%
  arrange(EndDate, fuel_harmonised, Age) %>%
  select(EndDate, size, Age, fuel_harmonised, Total_Vehicles_stats, Avg_CO2_g_km, Avg_Mileage) %>%
  rename(
    Avg_CO2 = Avg_CO2_g_km,
    engine_size = size,
    Fuel_Group = fuel_harmonised,
    Data_Year = EndDate,
    `Total_Vehicles(mileage_data)` = Total_Vehicles_stats
  ) %>%
  mutate(
    Data_Year = as.numeric(str_sub(Data_Year, 1, 4)),
    Vehicle_Age = Age
  )

cat("=== DATA CHECK ===\n")
cat("Years in data:", paste(sort(unique(DVLA$Data_Year)), collapse = ", "), "\n")
cat("Age range:", min(DVLA$Age), "-", max(DVLA$Age), "\n")
cat("Fuel groups:", paste(unique(DVLA$Fuel_Group), collapse = ", "), "\n")

# ============================================================================
# MILEAGE ANALYSIS PIPELINE - CORRECTED
# ============================================================================

if(!exists("join_final_complete_classified")) {
  cat("\nERROR: 'join_final_complete_classified' dataset not found!\n")
  cat("Please load this dataset before running this code.\n")
  stop("Missing required dataset: join_final_complete_classified")
}

cat("\nAvailable columns in join_final_complete_classified:\n")
print(names(join_final_complete_classified))

# ---------------------------------------------------------------------------
# Prepare DVLA / MOT mileage data
# ---------------------------------------------------------------------------
DVLA_prepared <- DVLA %>%
  mutate(
    engine_size = as.numeric(engine_size),
    engine_band = cut(engine_size,
                      breaks = engine_breaks,
                      right = FALSE)
  )

DVLA_collapsed <- DVLA_prepared %>%
  filter(Avg_Mileage >= 1000, Avg_Mileage <= 30000) %>%
  group_by(Fuel_Group, Data_Year, Vehicle_Age) %>%
  summarise(
    Avg_Mileage = weighted.mean(Avg_Mileage, `Total_Vehicles(mileage_data)`, na.rm = TRUE),
    Avg_CO2 = weighted.mean(Avg_CO2, `Total_Vehicles(mileage_data)`, na.rm = TRUE),
    MOT_Sample_Size = sum(`Total_Vehicles(mileage_data)`, na.rm = TRUE),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# Prepare GB stock data
# ---------------------------------------------------------------------------
vehicle_count_col <- ifelse("Fleet_Count" %in% names(join_final_complete_classified),
                            "Fleet_Count",
                            names(join_final_complete_classified)[grepl("count|vehicle|stock|fleet",
                                                                        names(join_final_complete_classified),
                                                                        ignore.case = TRUE)][1])

# Method 3: trend extrapolation, fit ages 3-8, project to 0-2
extrapolate_young_ages <- function(df, max_fit_age = 8, target_ages = c(0, 1, 2)) {
  df %>%
    filter(Vehicle_Age >= 3, Vehicle_Age <= max_fit_age) %>%
    group_by(Fuel_Group, Data_Year) %>%
    group_modify(~ {
      if (nrow(.x) < 3) return(tibble())
      fit <- lm(Avg_Mileage ~ Vehicle_Age, data = .x)
      tibble(
        Vehicle_Age = target_ages,
        Avg_Mileage = pmax(predict(fit, newdata = tibble(Vehicle_Age = target_ages)), 0),
        Avg_CO2     = mean(.x$Avg_CO2, na.rm = TRUE)
      )
    }) %>%
    ungroup()
}

# Trend extrapolation for old vehicles, fit ages 16-23, project to 24-30
extrapolate_old_ages <- function(df, min_fit_age = 16, max_fit_age = 23,
                                 target_ages = 24:30, min_mileage_floor = 500) {
  df %>%
    filter(Vehicle_Age >= min_fit_age, Vehicle_Age <= max_fit_age) %>%
    group_by(Fuel_Group, Data_Year) %>%
    group_modify(~ {
      if (nrow(.x) < 3) return(tibble())
      fit <- lm(Avg_Mileage ~ Vehicle_Age, data = .x)
      tibble(
        Vehicle_Age = target_ages,
        Avg_Mileage = pmax(predict(fit, newdata = tibble(Vehicle_Age = target_ages)), min_mileage_floor),
        Avg_CO2     = mean(.x$Avg_CO2, na.rm = TRUE)
      )
    }) %>%
    ungroup()
}

cat("\nUsing column '", vehicle_count_col, "' for vehicle counts\n", sep="")

# ---------------------------------------------------------------------------
# Prepare Stock_prepared (as data.table for speed)
# ---------------------------------------------------------------------------
setDT(join_final_complete_classified)
setDT(dimension_lookup)

Stock_prepared <- copy(join_final_complete_classified)
Stock_prepared[, engine_size := as.numeric(engine_size)]
Stock_prepared[, engine_band := cut(engine_size, breaks = engine_breaks, right = FALSE)]
setnames(Stock_prepared, vehicle_count_col, "Vehicle_Stock_GB")
Stock_prepared[, kerb_weight_kg := as.numeric(kerb_weight_kg)]

needed_keys <- unique(Stock_prepared[, .(Make, Model, GenModel, Fuel_Group)])

dimension_lookup_small <- dimension_lookup[
  needed_keys, on = .(Make, Model, GenModel, Fuel_Group), nomatch = NULL
]

dimension_lookup_collapsed <- dimension_lookup_small[
  , .(
    rdw_length_mm      = median(rdw_length_mm, na.rm = TRUE),
    rdw_width_mm       = median(rdw_width_mm, na.rm = TRUE),
    rdw_height_mm      = median(rdw_height_mm, na.rm = TRUE),
    rdw_kerb_weight_kg = median(rdw_kerb_weight_kg, na.rm = TRUE),
    rdw_wheelbase_mm   = median(rdw_wheelbase_mm, na.rm = TRUE)
  ),
  by = .(Make, Model, GenModel, Fuel_Group)
]

setkey(dimension_lookup_collapsed, Make, Model, GenModel, Fuel_Group)
setkey(Stock_prepared, Make, Model, GenModel, Fuel_Group)

n_before <- nrow(Stock_prepared)
Stock_prepared <- dimension_lookup_collapsed[Stock_prepared, on = .(Make, Model, GenModel, Fuel_Group)]

Stock_prepared_check <- Stock_prepared %>%
  group_by(Data_Year) %>%
  summarise(Total = sum(Vehicle_Stock_GB, na.rm = TRUE), .groups = "drop") %>%
  filter(Data_Year %in% c(2019, 2023))
cat("\n>>> CHECKPOINT: CP9: Stock_prepared final\n")
print(Stock_prepared_check)

cat("Rows before:", n_before, " after:", nrow(Stock_prepared), "\n")
stopifnot(nrow(Stock_prepared) == n_before)

save_checkpoint(Stock_prepared, "01_stock_prepared")

cat("\nDimensions added to Stock_prepared:\n")
dimension_cols <- c("rdw_length_mm", "rdw_width_mm", "rdw_height_mm",
                    "rdw_kerb_weight_kg", "rdw_wheelbase_mm")
existing_dims <- dimension_cols[dimension_cols %in% names(Stock_prepared)]
cat(paste("  -", existing_dims, collapse = "\n"))

vehicel_numer <- Stock_prepared %>%
  group_by(Data_Year) %>%
  summarise(fleet = sum(Vehicle_Stock_GB, na.rm = TRUE), .groups = "drop") %>%
  print

cat("=== Stock_prepared just rebuilt at", Sys.time(), "===\n")
cat("BEV age 29 total:", Stock_prepared %>% filter(Fuel_Group == "BEV", Vehicle_Age == 29) %>% summarise(x = sum(Vehicle_Stock_GB, na.rm=TRUE)) %>% pull(x), "\n")

# ---------------------------------------------------------------------------
# Mileage join (data.table)
# ---------------------------------------------------------------------------
fleet_complete <- Stock_prepared %>%
  filter(Vehicle_Age >= 3 & Vehicle_Age <= 23)

fleet_complete_dt   <- as.data.table(fleet_complete)
DVLA_collapsed_dt   <- as.data.table(DVLA_collapsed)

setkey(fleet_complete_dt, Fuel_Group, Data_Year, Vehicle_Age)
setkey(DVLA_collapsed_dt, Fuel_Group, Data_Year, Vehicle_Age)

Stock_model_dt <- DVLA_collapsed_dt[fleet_complete_dt, on = .(Fuel_Group, Data_Year, Vehicle_Age)]

Stock_model_dt[, Avg_Mileage := ifelse(
  is.na(Avg_Mileage),
  weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
  Avg_Mileage
), by = Fuel_Group]

Stock_model_dt[, Avg_CO2 := ifelse(
  is.na(Avg_CO2),
  weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
  Avg_CO2
), by = Fuel_Group]

Stock_model <- as_tibble(Stock_model_dt)
print(Stock_model)

# Young / old vehicle mileage extrapolation
young_vehicle_mileage_extrap <- extrapolate_young_ages(DVLA_collapsed)
old_vehicle_mileage_extrap   <- extrapolate_old_ages(DVLA_collapsed)

DVLA_collapsed_v3 <- bind_rows(
  DVLA_collapsed,
  young_vehicle_mileage_extrap,
  old_vehicle_mileage_extrap
) %>%
  arrange(Fuel_Group, Data_Year, Vehicle_Age)

cat("DVLA_collapsed_v2:", nrow(DVLA_collapsed), "rows\n")
cat("DVLA_collapsed_v3:", nrow(DVLA_collapsed_v3), "rows\n\n")

national_trend_no_age_uncalibrated <- Stock_prepared %>%
  left_join(DVLA_collapsed_v3, by = c("Fuel_Group", "Data_Year", "Vehicle_Age")) %>%
  group_by(Fuel_Group) %>%
  mutate(
    Avg_Mileage = ifelse(
      is.na(Avg_Mileage),
      weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
      Avg_Mileage
    )
  ) %>%
  ungroup() %>%
  group_by(Data_Year) %>%
  summarise(
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Vehicles_with_Data = sum(Vehicle_Stock_GB[!is.na(Avg_Mileage)], na.rm = TRUE),
    GB_Total_Mileage = sum(Avg_Mileage * Vehicle_Stock_GB, na.rm = TRUE),
    Adjusted_Mileage = GB_Total_Mileage * (Total_Vehicles / Vehicles_with_Data),
    Avg_Mileage_Per_Vehicle = Adjusted_Mileage / Total_Vehicles,
    .groups = "drop"
  )
print(national_trend_no_age_uncalibrated)

# ---------------------------------------------------------------------------
# Mileage validation vs DfT
# ---------------------------------------------------------------------------
dft_estimates <- data.frame(
  Data_Year = c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024),
  DfT_Estimate = c(250.3, 254.8, 257.9, 259.1, 262.9, 197.3, 221.4, 244, 251.3, 256.1)
)

comparison_table <- national_trend_no_age_uncalibrated %>%
  select(Data_Year, GB_Total_Mileage) %>%
  mutate(
    my_Data_Billions = GB_Total_Mileage / 1e9,
    DfT_Estimate_Billions = dft_estimates$DfT_Estimate[match(Data_Year, dft_estimates$Data_Year)],
    Difference = my_Data_Billions - DfT_Estimate_Billions,
    Percent_Difference = round(Difference / DfT_Estimate_Billions * 100, 1)
  ) %>%
  filter(!is.na(DfT_Estimate_Billions)) %>%
  select(-GB_Total_Mileage)

print("=== COMPARISON TABLE (Billions) ===")
print(comparison_table)

# ============================================================================
# SIMPLIFIED CALIBRATION - 50/50 SPLIT
# ============================================================================
current_trend <- data.frame(
  Data_Year = 2015:2024,
  Your_Mileage = comparison_table$my_Data_Billions,
  DfT_Target = comparison_table$DfT_Estimate_Billions
) %>%
  mutate(
    Gap = DfT_Target - Your_Mileage,
    Gap_Pct = round(Gap / DfT_Target * 100, 1)
  )

print(current_trend)

decomposition <- national_trend_no_age_uncalibrated %>%
  filter(Data_Year >= 2015) %>%
  left_join(dft_estimates, by = "Data_Year") %>%
  mutate(
    Implied_DfT_Cars = (DfT_Estimate * 1e9) / Avg_Mileage_Per_Vehicle,
    Implied_DfT_AvgMileage = (DfT_Estimate * 1e9) / Total_Vehicles,
    Car_Count_Ratio = Total_Vehicles / Implied_DfT_Cars,
    Mileage_Rate_Ratio = Avg_Mileage_Per_Vehicle / Implied_DfT_AvgMileage
  )

print("=== DECOMPOSITION RESULTS ===")
decomposition %>% select(Data_Year, Car_Count_Ratio, Mileage_Rate_Ratio)

cal_factors <- decomposition %>%
  group_by(Data_Year) %>%
  mutate(
    cal_factor = 1 / Car_Count_Ratio
  ) %>%
  ungroup() %>%
  select(Data_Year, cal_factor)

print(cal_factors)

calibrated_trend <- current_trend %>%
  left_join(cal_factors, by = "Data_Year") %>%
  mutate(
    Calibrated_Mileage = Your_Mileage * cal_factor,
    Error_Before = round((Your_Mileage - DfT_Target) / DfT_Target * 100, 1),
    Error_After = round((Calibrated_Mileage - DfT_Target) / DfT_Target * 100, 1),
    Improvement = abs(Error_Before) - abs(Error_After)
  )

print(calibrated_trend %>%
        select(Data_Year, Your_Mileage, Calibrated_Mileage, DfT_Target,
               cal_factor, Error_Before, Error_After, Improvement))

summary_stats <- calibrated_trend %>%
  filter(Data_Year != 2020) %>%
  summarise(
    Avg_Error_Before = round(mean(abs(Error_Before)), 1),
    Avg_Error_After = round(mean(abs(Error_After)), 1),
    Avg_Improvement = round(mean(Improvement), 1),
    Years_Within_2pct = sum(abs(Error_After) <= 2)
  )

print(summary_stats)

calibration_factors <- decomposition %>%
  filter(Data_Year >= 2015) %>%
  mutate(
    Calibration_Multiplier = 1 / Car_Count_Ratio
  ) %>%
  select(Data_Year, Calibration_Multiplier)

print("=== CALIBRATION FACTORS ===")
print(calibration_factors)

# ---------------------------------------------------------------------------
# Compare three methods
# ---------------------------------------------------------------------------
stopifnot(
  "DVLA_collapsed does not exist" = exists("DVLA_collapsed"),
  "Stock_prepared does not exist" = exists("Stock_prepared"),
  "dft_estimates does not exist" = exists("dft_estimates")
)

Stock_prepared_agg <- Stock_prepared %>%
  group_by(Fuel_Group, Data_Year, Vehicle_Age) %>%
  summarise(
    Vehicle_Stock_GB = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  )

compare_three_methods <- function(dvla_variant, label) {

  dvla_dt  <- as.data.table(dvla_variant)
  stock_dt <- as.data.table(Stock_prepared_agg)

  setkey(dvla_dt, Fuel_Group, Data_Year, Vehicle_Age)
  setkey(stock_dt, Fuel_Group, Data_Year, Vehicle_Age)

  dt <- dvla_dt[stock_dt, on = .(Fuel_Group, Data_Year, Vehicle_Age)]

  dt[, Avg_Mileage := ifelse(
    is.na(Avg_Mileage),
    weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage
  ), by = Fuel_Group]

  result <- dt[, .(GB_Total_Mileage = sum(Avg_Mileage * Vehicle_Stock_GB, na.rm = TRUE)),
               by = Data_Year]
  result[, Method := label]
  as_tibble(result[])
}

all_methods_final <- bind_rows(
  compare_three_methods(DVLA_collapsed,    "Current (fleet-wide avg fill)"),
  compare_three_methods(DVLA_collapsed_v2, "Flat: Age 1-2 = Age 3"),
  compare_three_methods(DVLA_collapsed_v3, "Trend extrapolation")
) %>%
  left_join(dft_estimates, by = "Data_Year") %>%
  mutate(
    Study_Bn = GB_Total_Mileage / 1e9,
    Pct_Diff_vs_DfT = round((Study_Bn - DfT_Estimate) / DfT_Estimate * 100, 1)
  )

print(all_methods_final, n = Inf)

cat("\n=== Average absolute % gap by method (excluding 2020) ===\n")
summary_final <- all_methods_final %>%
  filter(Data_Year != 2020) %>%
  group_by(Method) %>%
  summarise(Avg_Abs_Pct_Diff = round(mean(abs(Pct_Diff_vs_DfT), na.rm = TRUE), 1), .groups = "drop")

print(summary_final)

# ---------------------------------------------------------------------------
# National trend with no age filter, single factor calibration
# ---------------------------------------------------------------------------
national_trend_no_age <- Stock_prepared %>%
  left_join(DVLA_collapsed_v3, by = c("Fuel_Group", "Data_Year", "Vehicle_Age")) %>%
  filter(Data_Year >= 2015) %>%
  group_by(Fuel_Group) %>%
  mutate(
    Avg_Mileage = ifelse(
      is.na(Avg_Mileage),
      weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
      Avg_Mileage
    )
  ) %>%
  ungroup() %>%
  left_join(calibration_factors, by = "Data_Year") %>%
  mutate(
    Avg_Mileage_Calibrated = ifelse(
      !is.na(Calibration_Multiplier),
      Avg_Mileage * Calibration_Multiplier,
      Avg_Mileage
    ),
    Total_Mileage_GB_Calibrated = Avg_Mileage_Calibrated * Vehicle_Stock_GB
  ) %>%
  select(-Calibration_Multiplier) %>%
  group_by(Data_Year) %>%
  summarise(
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Vehicles_with_Data = sum(Vehicle_Stock_GB[!is.na(Avg_Mileage_Calibrated)], na.rm = TRUE),
    GB_Total_Mileage = sum(Total_Mileage_GB_Calibrated, na.rm = TRUE),
    Adjusted_Mileage = GB_Total_Mileage * (Total_Vehicles / Vehicles_with_Data),
    Avg_Mileage_Per_Vehicle = Adjusted_Mileage / Total_Vehicles,
    .groups = "drop"
  )

print(national_trend_no_age)

comparison_table_cal <- national_trend_no_age %>%
  select(Data_Year, GB_Total_Mileage) %>%
  mutate(
    my_Data_Billions = GB_Total_Mileage / 1e9,
    DfT_Estimate_Billions = dft_estimates$DfT_Estimate[match(Data_Year, dft_estimates$Data_Year)],
    Difference = my_Data_Billions - DfT_Estimate_Billions,
    Percent_Difference = round(Difference / DfT_Estimate_Billions * 100, 1)
  ) %>%
  filter(!is.na(DfT_Estimate_Billions)) %>%
  select(-GB_Total_Mileage)

print("=== COMPARISON TABLE (Billions) ===")
print(comparison_table_cal)

decomposition_cal <- national_trend_no_age %>%
  filter(Data_Year >= 2015) %>%
  left_join(dft_estimates, by = "Data_Year") %>%
  mutate(
    Implied_DfT_Cars = (DfT_Estimate * 1e9) / Avg_Mileage_Per_Vehicle,
    Implied_DfT_AvgMileage = (DfT_Estimate * 1e9) / Total_Vehicles,
    Car_Count_Ratio = Total_Vehicles / Implied_DfT_Cars,
    Mileage_Rate_Ratio = Avg_Mileage_Per_Vehicle / Implied_DfT_AvgMileage
  )

decomposition_cal %>% select(Data_Year, Car_Count_Ratio, Mileage_Rate_Ratio)

# ============================================================================
# FULL STOCK MODEL - WITHOUT SUMMARIZING BY YEAR
# ============================================================================

full_stock_model <- Stock_prepared %>%
  select(-any_of(c("Origin", "Region", "Make_key", "GenModel_key", "Build_Year"))) %>%
  left_join(DVLA_collapsed_v3, by = c("Fuel_Group", "Data_Year", "Vehicle_Age")) %>%
  group_by(Fuel_Group) %>%
  mutate(
    Avg_Mileage = ifelse(is.na(Avg_Mileage),
                         weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
                         Avg_Mileage)
  ) %>%
  ungroup() %>%
  left_join(calibration_factors, by = "Data_Year") %>%
  mutate(
    Avg_Mileage_Calibrated = ifelse(!is.na(Calibration_Multiplier),
                                    Avg_Mileage * Calibration_Multiplier,
                                    Avg_Mileage),
    Total_Mileage_GB_Calibrated = Avg_Mileage_Calibrated * Vehicle_Stock_GB,
    Build_Year = Data_Year - Vehicle_Age
  ) %>%
  select(-Calibration_Multiplier)

n_before <- nrow(full_stock_model)

origin_lookup <- veh_origin %>%
  distinct(Make, GenModel, Fuel_Group, Build_Year, Origin, Region)

dup_check <- origin_lookup %>%
  count(Make, GenModel, Fuel_Group, Build_Year) %>%
  filter(n > 1)
cat("Remaining ambiguous keys:", nrow(dup_check), "\n")
if (nrow(dup_check) > 0) print(head(dup_check, 20))

origin_make_fallback <- veh_origin %>%
  count(Make, Origin, Region) %>%
  group_by(Make) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Make, Origin_mk = Origin, Region_mk = Region)

full_stock_model <- full_stock_model %>%
  left_join(origin_lookup, by = c("Make", "GenModel", "Fuel_Group", "Build_Year")) %>%
  left_join(origin_make_fallback, by = "Make") %>%
  mutate(
    Origin = coalesce(Origin, Origin_mk, "Other (unmapped)"),
    Region = coalesce(Region, Region_mk, "Other")
  ) %>%
  select(-Origin_mk, -Region_mk)

stopifnot(nrow(full_stock_model) == n_before)

cat("Row count preserved:", nrow(full_stock_model) == n_before, "\n")
save_checkpoint(full_stock_model, "02_full_stock_model")

cat("\n=== FINAL ORIGIN COVERAGE ===\n")
cat("Total rows:", nrow(full_stock_model), "\n")
cat("NA Origin count:", sum(is.na(full_stock_model$Origin)), "\n")
cat("NA Origin percentage:",
    round(100 * sum(is.na(full_stock_model$Origin)) / nrow(full_stock_model), 2), "%\n")

cat("\n=== CHECK ABARTH AND AIXAM ===\n")
full_stock_model %>%
  filter(Make %in% c("ABARTH", "AIXAM")) %>%
  select(Make, Origin, Region) %>%
  distinct() %>%
  print()

cat("Stock_prepared total vehicles:", sum(Stock_prepared$Vehicle_Stock_GB, na.rm = TRUE), "\n")
cat("full_stock_model total vehicles:", sum(full_stock_model$Vehicle_Stock_GB, na.rm = TRUE), "\n")

cat("\n=== ORIGIN DISTRIBUTION (2024 BEV) ===\n")
bev_origin_2024 <- full_stock_model %>%
  filter(Fuel_Group == "BEV", Data_Year == 2024) %>%
  group_by(Origin) %>%
  summarise(
    Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Share = round(100 * Vehicles / sum(Vehicles), 1)
  ) %>%
  arrange(desc(Share))

print(bev_origin_2024, n = Inf)

cat("Origin coverage:",
    round(100 * sum(!is.na(full_stock_model$Origin)) / nrow(full_stock_model), 1), "%\n")

full_stock_model_export <- full_stock_model %>%
  mutate(
    Avg_Mileage = round(Avg_Mileage, 1),
    Avg_Mileage_Calibrated = round(Avg_Mileage_Calibrated, 1),
    Total_Mileage_GB_Calibrated = round(Total_Mileage_GB_Calibrated, 0),
    Avg_CO2 = round(Avg_CO2, 2),
    across(starts_with("rdw_"), ~ round(.x, 1))
  )

cat("Unrounded total, 2019:",
    full_stock_model %>% filter(Data_Year == 2019) %>% summarise(x = sum(Vehicle_Stock_GB, na.rm = TRUE)) %>% pull(x), "\n")
cat("Rounded total, 2019:",
    full_stock_model_export %>% filter(Data_Year == 2019) %>% summarise(x = sum(Vehicle_Stock_GB, na.rm = TRUE)) %>% pull(x), "\n")

vehicel_numer <- full_stock_model_export %>%
  group_by(Data_Year) %>%
  summarise(fleet = sum(Vehicle_Stock_GB, na.rm = TRUE), .groups = "drop") %>%
  print

write_parquet(
  full_stock_model_export,
  file.path(data_dir, "join_final_complete.parquet"),
  compression = "zstd",
  compression_level = 22
)

# ============================================================================
# STOCK_MODEL CALIBRATION (mileage)
# ============================================================================
Stock_model_calibrated <- Stock_model %>%
  left_join(calibration_factors, by = "Data_Year") %>%
  mutate(
    Avg_Mileage_Calibrated = ifelse(
      !is.na(Calibration_Multiplier),
      Avg_Mileage * Calibration_Multiplier,
      Avg_Mileage
    ),
    Total_Mileage_GB_Calibrated = Avg_Mileage_Calibrated * Vehicle_Stock_GB,
    Total_CO2_GB_Calibrated = ifelse(
      !is.na(Avg_CO2),
      Avg_CO2 * Vehicle_Stock_GB,
      NA
    ),
    Has_Calibrated_Mileage = !is.na(Avg_Mileage_Calibrated)
  ) %>%
  select(-Calibration_Multiplier)

Stock_model <- Stock_model_calibrated %>%
  mutate(
    Avg_Mileage = Avg_Mileage_Calibrated,
    Total_Mileage_GB = ifelse(is.na(Avg_Mileage), NA, Avg_Mileage * Vehicle_Stock_GB),
    Total_CO2_GB = ifelse(is.na(Avg_CO2), NA, Avg_CO2 * Vehicle_Stock_GB),
    Has_Mileage_Data = !is.na(Avg_Mileage)
  )

# Validation checks
cat("\n=== VALIDATION CHECKS ===\n")

cat("\nChecking if mileage values are realistic (should be ~7,000 miles/year average):\n")
summary_stats <- Stock_model %>%
  filter(Has_Mileage_Data == TRUE) %>%
  summarise(
    min_mileage = min(Avg_Mileage, na.rm = TRUE),
    max_mileage = max(Avg_Mileage, na.rm = TRUE),
    mean_mileage = mean(Avg_Mileage, na.rm = TRUE),
    median_mileage = median(Avg_Mileage, na.rm = TRUE)
  )
print(summary_stats)

mileage_2024 <- Stock_model %>%
  filter(Data_Year == 2024) %>%
  summarise(
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Vehicles_with_Data = sum(Vehicle_Stock_GB[!is.na(Avg_Mileage)], na.rm = TRUE),
    Total_Mileage = sum(Total_Mileage_GB, na.rm = TRUE),
    Adjusted_Total_Mileage = Total_Mileage * (Total_Vehicles / Vehicles_with_Data),
    avg_mileage_per_vehicle = Adjusted_Total_Mileage / Total_Vehicles
  )

cat("\n=== 2024 ANNUAL MILEAGE SUMMARY ===\n")
print(mileage_2024)

cat("\n=== NATIONAL MILEAGE TREND (2014-2024) ===\n")
national_trend <- Stock_model %>%
  filter(Data_Year >= 2015) %>%
  group_by(Data_Year) %>%
  summarise(
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Vehicles_with_Data = sum(Vehicle_Stock_GB[!is.na(Avg_Mileage)], na.rm = TRUE),
    GB_Total_Mileage = sum(Total_Mileage_GB, na.rm = TRUE),
    Adjusted_Mileage = ifelse(
      Vehicles_with_Data > 0,
      GB_Total_Mileage * (Total_Vehicles / Vehicles_with_Data),
      NA
    ),
    Avg_Mileage_Per_Vehicle = Adjusted_Mileage / Total_Vehicles,
    .groups = "drop"
  )

simple_calibration <- current_trend %>%
  left_join(cal_factors, by = "Data_Year") %>%
  mutate(
    Calibrated_Mileage = Your_Mileage * cal_factor,
    Error_Before = round((Your_Mileage - DfT_Target) / DfT_Target * 100, 1),
    Error_After = round((Calibrated_Mileage - DfT_Target) / DfT_Target * 100, 1)
  )

print(simple_calibration %>%
        select(Data_Year, Your_Mileage, Calibrated_Mileage, DfT_Target, Error_Before, Error_After))

# Plot: actual vs calibrated vs DfT
p3 <- ggplot() +
  geom_line(data = simple_calibration,
            aes(x = Data_Year, y = Your_Mileage, color = "This Study"),
            size = 1.2) +
  geom_point(data = simple_calibration,
             aes(x = Data_Year, y = Your_Mileage, color = "This Study"),
             size = 3) +
  geom_line(data = simple_calibration,
            aes(x = Data_Year, y = Calibrated_Mileage, color = "Calibrated"),
            size = 1.2) +
  geom_point(data = simple_calibration,
             aes(x = Data_Year, y = Calibrated_Mileage, color = "Calibrated"),
             size = 3) +
  geom_line(data = dft_estimates,
            aes(x = Data_Year, y = DfT_Estimate, color = "DfT Estimates"),
            size = 1.2, linetype = "dashed") +
  geom_point(data = dft_estimates,
             aes(x = Data_Year, y = DfT_Estimate, color = "DfT Estimates"),
             size = 3, shape = 17) +
  labs(
    x = "Year",
    y = "Mileage (Billions)",
    color = "Source"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c(
    "Calibrated" = "blue",
    "DfT Estimates" = "red",
    "This Study" = "pink"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  )
print(p3)

# ============================================================================
# VEHICLE MILEAGE BY FUEL / AGE
# ============================================================================
DVLA_calibrated <- DVLA %>%
  left_join(calibration_factors, by = "Data_Year") %>%
  mutate(
    Avg_Mileage_Calibrated = ifelse(
      !is.na(Calibration_Multiplier),
      Avg_Mileage * Calibration_Multiplier,
      Avg_Mileage
    ),
    Avg_Mileage_Original = Avg_Mileage
  ) %>%
  select(-Calibration_Multiplier)

dff <- DVLA_calibrated %>%
  filter(Fuel_Group != "Other") %>%
  group_by(Fuel_Group, engine_size, Age, Data_Year) %>%
  summarise(
    Avg_Mileage = weighted.mean(Avg_Mileage_Calibrated, `Total_Vehicles(mileage_data)`, na.rm = TRUE),
    Avg_CO2 = weighted.mean(Avg_CO2, `Total_Vehicles(mileage_data)`, na.rm = TRUE),
    Total_Vehicles = sum(`Total_Vehicles(mileage_data)`, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nSummary of dff:\n")
cat("Years:", paste(sort(unique(dff$Data_Year)), collapse = ", "), "\n")
cat("Ages:", paste(sort(unique(dff$Age)), collapse = ", "), "\n")
cat("Fuel groups:", paste(unique(dff$Fuel_Group), collapse = ", "), "\n")
cat("Engine size range:", min(dff$engine_size, na.rm = TRUE), "-", max(dff$engine_size, na.rm = TRUE), "\n")
cat("Total vehicles in dff:", format(sum(dff$Total_Vehicles, na.rm = TRUE), big.mark = ","), "\n")

filtered_df <- dff %>%
  filter(engine_size > 500 & engine_size < 5000,
         Age <= 8,
         Data_Year == 2024,
         Avg_Mileage >= 1000 & Avg_Mileage <= 20000,
         Total_Vehicles >= 0)

filtered_df <- filtered_df %>%
  mutate(
    size_category = cut(engine_size,
                        breaks = c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000),
                        labels = c("500-1000", "1001-1500", "1501-2000", "2001-2500",
                                   "2501-3000", "3001-3500", "3501-4000", "4001-4500", "4501-5000"))
  ) %>%
  group_by(size_category, Age, Fuel_Group, Data_Year, engine_size) %>%
  summarise(
    Avg_Mileage = weighted.mean(Avg_Mileage, Total_Vehicles, na.rm = TRUE),
    Total_Vehicles = sum(Total_Vehicles, na.rm = TRUE),
    .groups = "drop"
  )

simple_table <- filtered_df %>%
  group_by(Age, Fuel_Group) %>%
  summarise(
    Avg_Mileage = round(weighted.mean(Avg_Mileage, Total_Vehicles, na.rm = TRUE), 1),
    Total_Vehicles = sum(Total_Vehicles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = Age,
    names_from = Fuel_Group,
    values_from = c(Avg_Mileage, Total_Vehicles),
    values_fill = 0
  )

print(simple_table, width = Inf)

enhanced_table <- simple_table %>%
  mutate(
    Diesel_vs_Petrol = Avg_Mileage_Diesel - Avg_Mileage_Petrol,
    BEV_vs_Petrol = Avg_Mileage_BEV - Avg_Mileage_Petrol,
    HEV_vs_Petrol = Avg_Mileage_HEV - Avg_Mileage_Petrol,
    Diesel_pct_higher = round((Avg_Mileage_Diesel / Avg_Mileage_Petrol - 1) * 100, 1),
    BEV_pct_higher = round((Avg_Mileage_BEV / Avg_Mileage_Petrol - 1) * 100, 1),
    HEV_pct_higher = round((Avg_Mileage_HEV / Avg_Mileage_Petrol - 1) * 100, 1),
    Total_All = Total_Vehicles_BEV + Total_Vehicles_Diesel +
      Total_Vehicles_HEV + Total_Vehicles_Petrol,
    BEV_share = round(Total_Vehicles_BEV / Total_All * 100, 1),
    Diesel_share = round(Total_Vehicles_Diesel / Total_All * 100, 1),
    HEV_share = round(Total_Vehicles_HEV / Total_All * 100, 1),
    Petrol_share = round(Total_Vehicles_Petrol / Total_All * 100, 1)
  ) %>%
  select(Age,
         starts_with("Avg_Mileage"),
         Diesel_vs_Petrol, Diesel_pct_higher,
         BEV_vs_Petrol, BEV_pct_higher,
         HEV_vs_Petrol, HEV_pct_higher,
         starts_with("Total_Vehicles"),
         BEV_share, Diesel_share, HEV_share, Petrol_share)

print(enhanced_table, width = Inf)

mileage_trends <- enhanced_table %>%
  select(Age, ends_with("_pct_higher")) %>%
  rename_with(~str_remove(., "_pct_higher"), ends_with("_pct_higher"))

print(mileage_trends, width = Inf)

market_share <- enhanced_table %>%
  select(Age, ends_with("_share")) %>%
  rename_with(~str_remove(., "_share"), ends_with("_share"))

print(market_share, width = Inf)

# ---------------------------------------------------------------------------
# Bar plot: mileage by size / age / fuel
# ---------------------------------------------------------------------------
df <- filtered_df

ggplot(df, aes(x = size_category, y = Avg_Mileage, fill = Fuel_Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Age ~ Fuel_Group, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic() +
  labs(
    title = "Mileage by Size Category, Age, and Fuel Type (2024)",
    subtitle = "Filtered: 1000-20000 miles/year, min 10 vehicles per category",
    y = "Average Annual Mileage",
    x = "Vehicle Size (cc)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# ---------------------------------------------------------------------------
# Extreme value check
# ---------------------------------------------------------------------------
extreme_counts <- dff %>%
  filter(Data_Year == 2024, Age <= 8) %>%
  filter(Avg_Mileage < 1000 | Avg_Mileage > 20000) %>%
  group_by(Fuel_Group, engine_size) %>%
  summarise(
    extreme_cases = n(),
    total_vehicles = sum(Total_Vehicles, na.rm = TRUE),
    avg_mileage = mean(Avg_Mileage, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_mileage))

cat("\nExtreme values with vehicle counts:\n")
print(extreme_counts)

weighted_correct <- dff %>%
  filter(Data_Year == 2024, Age <= 8) %>%
  group_by(Age, Fuel_Group) %>%
  summarise(
    total_miles = sum(Avg_Mileage * Total_Vehicles, na.rm = TRUE),
    total_vehicles = sum(Total_Vehicles, na.rm = TRUE),
    weighted_avg = total_miles / total_vehicles,
    .groups = 'drop'
  )

cat("\nPROPER weighted averages (using Total_Vehicles):\n")

comparison_weighted <- df %>%
  filter(Data_Year == 2024, Age <= 8) %>%
  group_by(Age, Fuel_Group) %>%
  summarise(
    simple_avg = weighted.mean(Avg_Mileage, Total_Vehicles, na.rm = TRUE),
    weighted_avg = sum(Avg_Mileage * Total_Vehicles, na.rm = TRUE) / sum(Total_Vehicles, na.rm = TRUE),
    total_vehicles = sum(Total_Vehicles, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\nComparison: Simple Average vs Weighted Average:\n")

impact <- comparison_weighted %>%
  mutate(
    difference = weighted_avg - simple_avg,
    pct_difference = difference / simple_avg * 100
  )

cat("\nImpact of using weighted average:\n")
print(impact)

# Weighted averages plot
ggplot(weighted_correct, aes(x = Age, y = weighted_avg, color = Fuel_Group)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_y_continuous(
    labels = comma,
    limits = c(5500, 12000),
    breaks = seq(5500, 12000, 1000)
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    y = "Average Annual Mileage (miles)",
    x = "Vehicle Age (years)",
    color = "Fuel Type"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray50")
  ) +
  geom_text(aes(label = paste0(round(weighted_avg/1000, 1), "k")),
            vjust = -0.8, size = 3, fontface = "bold", show.legend = FALSE, check_overlap = TRUE)

# Distribution of vehicles per row
vehicle_distribution <- df %>%
  filter(Data_Year == 2024, Age <= 8) %>%
  group_by(Fuel_Group) %>%
  summarise(
    total_rows = n(),
    total_vehicles = sum(Total_Vehicles, na.rm = TRUE),
    avg_vehicles_per_row = mean(Total_Vehicles, na.rm = TRUE),
    median_vehicles_per_row = median(Total_Vehicles, na.rm = TRUE),
    rows_with_few_vehicles = sum(Total_Vehicles < 10, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\nVehicle distribution analysis:\n")
print(vehicle_distribution)

final_correct <- df %>%
  filter(Data_Year == 2024, Age <= 8) %>%
  filter(Total_Vehicles >= 10) %>%
  filter(Avg_Mileage >= 1000 & Avg_Mileage <= 20000) %>%
  group_by(Age, Fuel_Group) %>%
  summarise(
    total_miles = sum(Avg_Mileage * Total_Vehicles, na.rm = TRUE),
    total_vehicles = sum(Total_Vehicles, na.rm = TRUE),
    weighted_avg = total_miles / total_vehicles,
    contributing_rows = n(),
    .groups = 'drop'
  )

cat("\n", strrep("=", 70), "\n", sep="")
cat("FINAL ROBUST ESTIMATES (Weighted avg, min 10 vehicles, 1000-20000 miles range)\n")
cat(strrep("=", 70), "\n")
print(final_correct)

final_table <- final_correct %>%
  pivot_wider(
    id_cols = Age,
    names_from = Fuel_Group,
    values_from = weighted_avg,
    names_glue = "{Fuel_Group} Average Mileage (miles)"
  ) %>%
  rename(`Vehicle Age (years)` = Age) %>%
  arrange(`Vehicle Age (years)`)

cat("\nFinal corrected table:\n")
print(final_table, width = Inf)

# ---------------------------------------------------------------------------
# National mileage trend plot
# ---------------------------------------------------------------------------
p1_main <- ggplot(national_trend %>% filter(Data_Year >= 2015),
                  aes(x = Data_Year, y = Avg_Mileage_Per_Vehicle)) +
  geom_line(color = "steelblue", size = 1.5) +
  geom_point(color = "darkblue", size = 3) +
  scale_y_continuous(
    labels = comma,
    breaks = seq(6000, 11500, by = 1000)) +
  labs(
    title = "Average Annual Mileage per Vehicle (2015-2024)",
    x = "Year",
    y = "Average Mileage(miles)"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(color = "gray50")
  ) +
  geom_text(aes(label = paste0(round(Avg_Mileage_Per_Vehicle / 1e3, 2), "k")),
            vjust = -1, size = 3, color = "darkblue") +
  scale_x_continuous(
    limits = c(2015, 2024),
    breaks = seq(2015, 2024, by = 3)
  )
print(p1_main)
print(national_trend, n = Inf)

# Mileage by fuel type (2024)
mileage_by_fuel_2024 <- Stock_model %>%
  filter(Data_Year == 2024, !is.na(Total_Mileage_GB)) %>%
  group_by(Fuel_Group) %>%
  summarise(
    Total_Mileage = sum(Total_Mileage_GB, na.rm = TRUE),
    Vehicle_Count = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage = Total_Mileage / Vehicle_Count,
    .groups = "drop"
  )

mileage_by_fuel_2024 <- mileage_by_fuel_2024 %>%
  mutate(
    Pct_of_Total = round(100 * Total_Mileage / sum(Total_Mileage), 1),
    Fuel_Group = factor(Fuel_Group,
                        levels = mileage_by_fuel_2024$Fuel_Group[order(-mileage_by_fuel_2024$Total_Mileage)])
  )
print(mileage_by_fuel_2024, n = Inf)

ggplot(mileage_by_fuel_2024, aes(x = reorder(Fuel_Group, -Total_Mileage),
                                 y = Total_Mileage / 1e9, fill = Fuel_Group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Total_Mileage / 1e9, 1), "B\n(", Pct_of_Total, "%)")),
            vjust = -0.5, size = 3) +
  scale_y_continuous(labels = label_number(suffix = "B", scale = 1e-9)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "2024 Annual Mileage by Fuel Type",
    x = "Fuel Type",
    y = "Total Annual Mileage (Billion miles)",
    fill = "Fuel Type"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(mileage_by_fuel_2024, aes(x = reorder(Fuel_Group, -Avg_Mileage),
                                 y = Avg_Mileage, fill = Fuel_Group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Avg_Mileage), " miles")),
            vjust = -0.5, size = 3) +
  geom_hline(yintercept = mileage_2024$avg_mileage_per_vehicle,
             linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 5.5, y = mileage_2024$avg_mileage_per_vehicle * 1.05,
           label = paste("Fleet Average:", round(mileage_2024$avg_mileage_per_vehicle), "miles"),
           color = "red", size = 3) +
  labs(
    title = "2024 Average Annual Mileage by Fuel Type",
    x = "Fuel Type",
    y = "Average Annual Mileage (miles)",
    caption = "Dashed line shows overall fleet average"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

mileage_by_age_2024 <- Stock_model %>%
  filter(Data_Year == 2024, !is.na(Avg_Mileage)) %>%
  group_by(Vehicle_Age) %>%
  summarise(
    Total_Mileage = sum(Total_Mileage_GB, na.rm = TRUE),
    Vehicle_Count = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage = Total_Mileage / Vehicle_Count,
    .groups = "drop"
  )
print(mileage_by_age_2024, n = Inf)

p3_age <- ggplot(mileage_by_age_2024 %>% filter(Vehicle_Age <= 25),
                 aes(x = Vehicle_Age, y = Avg_Mileage)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "loess", se = TRUE, color = "orange", alpha = 0.2) +
  labs(
    title = " Mileage by Vehicle Age (2024)",
    x = "Vehicle Age (years)",
    y = "Average Mileage (miles)"
  ) +
  theme_minimal()
print(p3_age)

p2 <- ggplot(mileage_by_fuel_2024,
             aes(x = reorder(Fuel_Group, -Total_Mileage),
                 y = Total_Mileage / 1e9, fill = Fuel_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Mileage by Fuel Type", x = "", y = "Billion miles") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

(p1_main) / p3_age +
  plot_annotation(
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

# Publication-ready summary table
library(kableExtra)

summary_table <- mileage_by_fuel_2024 %>%
  select(Fuel_Group,
         `Total Mileage (B miles)` = Total_Mileage,
         `Vehicle Count` = Vehicle_Count,
         `Avg Mileage` = Avg_Mileage,
         `Share (%)` = Pct_of_Total) %>%
  mutate(
    `Total Mileage (B miles)` = round(`Total Mileage (B miles)` / 1e9, 1),
    `Vehicle Count` = round(`Vehicle Count` / 1e6, 2),
    `Avg Mileage` = round(`Avg Mileage`),
    `Share (%)` = round(`Share (%)`, 1)
  ) %>%
  add_row(Fuel_Group = "TOTAL",
          `Total Mileage (B miles)` = round(mileage_2024$Total_Mileage / 1e9, 1),
          `Vehicle Count` = round(mileage_2024$Total_Vehicles / 1e6, 2),
          `Avg Mileage` = round(mileage_2024$avg_mileage_per_vehicle),
          `Share (%)` = 100)

kable(summary_table, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  row_spec(nrow(summary_table), bold = TRUE, color = "white", background = "steelblue") %>%
  column_spec(1, bold = TRUE) %>%
  add_header_above(c(" " = 1, "2024 UK Vehicle Mileage Summary" = 4))

# ============================================================================
# CO2 VISUALISATIONS - LINKED WITH MILEAGE
# ============================================================================
Stock_model <- Stock_model %>%
  mutate(
    Avg_Mileage_km = Avg_Mileage * 1.60934,
    Annual_CO2_kg = (Avg_CO2 * Avg_Mileage_km) / 1000,
    Total_CO2_GB_corrected = (Annual_CO2_kg * Vehicle_Stock_GB) / 1000
  )

CO2_trend <- Stock_model %>%
  filter(!is.na(Avg_CO2), Vehicle_Stock_GB > 0) %>%
  group_by(Data_Year) %>%
  summarise(
    GB_Total_CO2 = sum(Total_CO2_GB_corrected, na.rm = TRUE),
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Avge_CO2 = weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage_miles = weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage_km = weighted.mean(Avg_Mileage_km, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Annual_CO2_kg = weighted.mean(Annual_CO2_kg, Vehicle_Stock_GB, na.rm = TRUE),
    Rows_Used = n(),
    .groups = "drop"
  )

print("=== UPDATED CO2 TREND WITH CORRECT UNITS ===")
print(CO2_trend)

co2_trend_by_fuel <- Stock_model %>%
  filter(!is.na(Avg_CO2), Vehicle_Stock_GB > 0) %>%
  group_by(Data_Year, Fuel_Group) %>%
  summarise(
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Avg_CO2 = weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage_miles = weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage_km = weighted.mean(Avg_Mileage_km, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Annual_CO2_kg = weighted.mean(Annual_CO2_kg, Vehicle_Stock_GB, na.rm = TRUE),
    Total_Annual_CO2_tonnes = sum(Total_CO2_GB_corrected, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== 2024 DATA COVERAGE ANALYSIS ===\n")
coverage_2024 <- Stock_model %>%
  filter(Data_Year == 2024) %>%
  summarise(
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Vehicles_with_Data = sum(Vehicle_Stock_GB * Has_Mileage_Data, na.rm = TRUE),
    Coverage_Pct = round(100 * Vehicles_with_Data / Total_Vehicles, 1),
    .groups = "drop"
  )

validation_data <- data.frame(
  Data_Year = c(2019, 2020, 2021, 2022, 2023, 2024),
  Greg_CO2 = c(175.79, 174.69, 173.44, 172.43, 171.29, 170.13)
)

co2_validation <- CO2_trend %>%
  filter(Data_Year >= 2019) %>%
  left_join(validation_data, by = "Data_Year") %>%
  mutate(
    Difference = Avge_CO2 - Greg_CO2,
    Pct_Difference = round(Difference / Greg_CO2 * 100, 1),
    Within_2pct = abs(Pct_Difference) <= 2
  )

cat("\n=== CO2 VALIDATION vs GREG'S DATA ===\n")

scatter_data <- Stock_model %>%
  filter(!is.na(Avg_CO2), !is.na(Avg_Mileage), Vehicle_Stock_GB > 0, Data_Year == 2024) %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(
    Avg_CO2 = weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage = weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  )

age3_data <- scatter_data %>%
  filter(Vehicle_Age == 3) %>%
  mutate(
    Annual_CO2_kg = (Avg_CO2 * Avg_Mileage) / 1000,
    CO2_label = paste0(round(Annual_CO2_kg), " kg"),
    Mileage_label = paste0(round(Avg_Mileage/1000, 1), "k km"),
    CO2_rate_label = paste0(round(Avg_CO2), " g/km")
  )

age3_table <- age3_data %>%
  select(Fuel_Group, Avg_Mileage, Avg_CO2, Total_Vehicles, Annual_CO2_kg) %>%
  mutate(
    Avg_Mileage = round(Avg_Mileage),
    Avg_CO2 = round(Avg_CO2, 1),
    Annual_CO2_kg = round(Annual_CO2_kg),
    `vs Petrol` = ifelse(Fuel_Group == "Petrol", "Baseline",
                         paste0(round((Annual_CO2_kg /
                                         age3_data$Annual_CO2_kg[age3_data$Fuel_Group == "Petrol"] - 1) * 100,
                                      1), "%"))
  ) %>%
  arrange(desc(Annual_CO2_kg))

print(age3_table)

Stock_model_2024 <- Stock_model %>%
  filter(Data_Year == 2024) %>%
  select(-c(Has_Mileage_Data, MOT_Sample_Size))

write_csv_zipped(Stock_model_2024, file.path(out_dir, "Stock_model(all).csv"), row.names = FALSE)

# Age 3 CO2 comparison plot
ggplot(age3_data, aes(x = reorder(Fuel_Group, -Annual_CO2_kg), y = Annual_CO2_kg, fill = Fuel_Group)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = CO2_label), vjust = -0.5, size = 4, fontface = "bold") +
  geom_text(aes(label = paste0("Mileage: ", Mileage_label, "\nRate: ", CO2_rate_label)),
            vjust = 1.5, color = "white", size = 3.5, fontface = "bold") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = comma_format(suffix = " kg"),
                     limits = c(0, max(age3_data$Annual_CO2_kg) * 1.2)) +
  labs(
    title = "Annual CO2 Emissions per Vehicle at Age 3",
    x = "Fuel Type",
    y = "Annual CO2 Emissions (kg per vehicle)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray30", size = 12),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank()
  )

# Decomposition of what drives CO2 differences at age 3
decomp_data <- age3_data %>%
  filter(Fuel_Group != "Other") %>%
  mutate(
    Emission_effect = Avg_CO2 / age3_data$Avg_CO2[age3_data$Fuel_Group == "Petrol"] - 1,
    Mileage_effect = Avg_Mileage / age3_data$Avg_Mileage[age3_data$Fuel_Group == "Petrol"] - 1,
    Combined_effect = (Emission_effect + 1) * (Mileage_effect + 1) - 1
  )

decomp_long <- decomp_data %>%
  select(Fuel_Group, Emission_effect, Mileage_effect, Combined_effect) %>%
  pivot_longer(cols = c(Emission_effect, Mileage_effect, Combined_effect),
               names_to = "Effect_Type",
               values_to = "Percent_Difference") %>%
  mutate(
    Effect_Type = factor(Effect_Type,
                         levels = c("Emission_effect", "Mileage_effect", "Combined_effect"),
                         labels = c("Emission Rate Effect", "Mileage Effect", "Combined Impact"))
  )

ggplot(decomp_long, aes(x = Fuel_Group, y = Percent_Difference * 100, fill = Effect_Type)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_text(aes(label = paste0(round(Percent_Difference * 100, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "What Drives CO2 Differences vs Petrol? (Age 3 Vehicles)",
    subtitle = "Diesel: Higher mileage (41%) + higher emission rate (13%) = 59% more CO2",
    x = "Fuel Type",
    y = "Percent Difference from Petrol",
    fill = "Effect Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray30", size = 11),
    legend.position = "bottom"
  )

# Scatter: CO2 vs Mileage
library(ggpubr)
ggplot(scatter_data, aes(x = Avg_Mileage, y = Avg_CO2, color = Fuel_Group, size = Total_Vehicles)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Fuel_Group), size = 0.5) +
  facet_wrap(~ Fuel_Group, scales = "free") +
  scale_size_continuous(range = c(1, 10),
                        breaks = c(10000, 100000, 1000000),
                        labels = scales::label_number(scale = 1e-3, suffix = "k")) +
  labs(
    title = "Relationship Between Annual Mileage and CO2 Emissions by Fuel Type (2024)",
    subtitle = "Point size represents number of vehicles. BEVs show 0 CO2 regardless of mileage.",
    x = "Average Annual Mileage (miles)",
    y = "Average CO2 Emissions (g/km)",
    size = "Number of\nVehicles",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left", label.y.npc = "top", size = 3)

co2_intensity_data <- Stock_model %>%
  filter(!is.na(Avg_CO2), !is.na(Avg_Mileage), Vehicle_Stock_GB > 0, Data_Year == 2024) %>%
  mutate(
    CO2_per_mile = Avg_CO2 / 0.621371,
    Annual_CO2_per_vehicle = CO2_per_mile * Avg_Mileage
  ) %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(
    CO2_per_mile = weighted.mean(CO2_per_mile, Vehicle_Stock_GB, na.rm = TRUE),
    Annual_CO2_per_vehicle = weighted.mean(Annual_CO2_per_vehicle, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage = weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(co2_intensity_data, aes(x = Vehicle_Age, y = CO2_per_mile, color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::label_number(suffix = " g/mile")) +
  labs(
    title = "CO2 Emission Intensity by Vehicle Age and Fuel Type (2024)",
    subtitle = "CO2 per mile driven - BEVs show 0 emissions",
    x = "Vehicle Age (years)",
    y = "CO2 Emissions per Mile (g/mile)",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  facet_wrap(~ Fuel_Group, scales = "free_y")

ggplot(co2_intensity_data, aes(x = Avg_Mileage, y = Annual_CO2_per_vehicle, color = Fuel_Group)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Fuel_Group)) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = " kg")) +
  labs(
    title = "Annual CO2 Emissions vs Annual Mileage by Fuel Type (2024)",
    subtitle = "Higher mileage leads to higher total annual CO2 emissions",
    x = "Average Annual Mileage (miles)",
    y = "Annual CO2 per Vehicle (kg)",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  annotate("text", x = max(co2_intensity_data$Avg_Mileage) * 0.8, y = 0,
           label = "BEVs: Zero tailpipe emissions\nregardless of mileage",
           color = "green", size = 3.5, fontface = "bold")

p_co2_trend <- ggplot(CO2_trend %>% filter(Data_Year >= 2015),
                      aes(x = Data_Year, y = Avge_CO2)) +
  geom_line(color = "darkred", size = 1.5) +
  geom_point(color = "darkred", size = 3) +
  labs(
    title = "Average CO2 Emissions Trend",
    x = "Year",
    y = "Average CO2 (g/km)"
  ) +
  scale_x_continuous(
    limits = c(2015, 2024),
    breaks = seq(2015, 2024, by = 3),
    expand = c(0.05, 0.05)
  ) +
  theme_minimal()
print(p_co2_trend)

co2_by_fuel_2024 <- Stock_model %>%
  filter(Data_Year == 2024, !is.na(Avg_CO2), Vehicle_Stock_GB > 0) %>%
  group_by(Fuel_Group) %>%
  summarise(
    Avg_CO2 = weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  )

print(co2_by_fuel_2024, n = Inf)

p_co2_fuel <- ggplot(co2_by_fuel_2024,
                     aes(x = reorder(Fuel_Group, -Avg_CO2), y = Avg_CO2, fill = Fuel_Group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Avg_CO2), " g/km")),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(
    title = "Average CO2 by Fuel Type(2024)",
    x = "Fuel Type",
    y = "Average CO2 (g/km)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(p_co2_fuel)

co2_age_2024 <- Stock_model %>%
  filter(Data_Year == 2024, !is.na(Avg_CO2), Vehicle_Stock_GB > 0) %>%
  group_by(Vehicle_Age) %>%
  summarise(
    Avg_CO2 = weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  )
print(co2_age_2024, n = Inf)
p_co2_age <- ggplot(co2_age_2024 %>% filter(Vehicle_Age <= 25),
                    aes(x = Vehicle_Age, y = Avg_CO2)) +
  geom_line(color = "purple", size = 1.2) +
  geom_smooth(method = "loess", se = FALSE, color = "orange", linetype = "dashed") +
  labs(
    title = "CO2 Emissions by Vehicle Age(2024)",
    x = "Vehicle Age (years)",
    y = "Average CO2 (g/km)"
  ) +
  theme_minimal()
print(p_co2_age)

p_correlation <- ggplot(scatter_data,
                        aes(x = Avg_Mileage, y = Avg_CO2, color = Fuel_Group)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(level = 0.68) +
  labs(
    title = "Mileage-CO2 Relationship(2024)",
    x = "Average Mileage (miles)",
    y = "Average CO2 (g/km)",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_correlation)

co2_dashboard <- (p_co2_trend | p_co2_fuel) / (p_co2_age | p_correlation)
print(co2_dashboard)

# CO2 by engine size and fuel
co2_engine_data <- Stock_model %>%
  filter(Data_Year == 2024, !is.na(Avg_Mileage), Vehicle_Stock_GB > 0) %>%
  mutate(
    engine_size_category = cut(engine_size,
                               breaks = c(0, 1000, 1500, 2000, 2500, 3000, 4000, 5000, Inf),
                               labels = c("<1L", "1-1.5L", "1.5-2L", "2-2.5L",
                                          "2.5-3L", "3-4L", "4-5L", ">5L"))
  ) %>%
  filter(!is.na(engine_size_category)) %>%
  group_by(Fuel_Group, engine_size_category) %>%
  summarise(
    Avg_CO2_gkm = if(all(is.na(Avg_CO2))) NA_real_ else
      weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage_miles = weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_CO2_gmile = Avg_CO2_gkm / 0.621371,
    Vehicle_Count = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  )

co2_engine_data <- Stock_model %>%
  filter(Data_Year == 2024, !is.na(Avg_Mileage), Vehicle_Stock_GB > 0) %>%
  mutate(
    engine_size_category = cut(engine_size,
                               breaks = c(0, 1000, 1500, 2000, 2500, 3000, 4000, 5000, Inf),
                               labels = c("<1L", "1-1.5L", "1.5-2L", "2-2.5L",
                                          "2.5-3L", "3-4L", "4-5L", ">5L"))
  ) %>%
  filter(!is.na(engine_size_category)) %>%
  group_by(Fuel_Group, engine_size_category) %>%
  summarise(
    Avg_CO2 = weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage = weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
    Vehicle_Count = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    CO2_per_mile = ifelse(Fuel_Group == "BEV", 0, Avg_CO2 / 0.621371),
    Avg_CO2 = ifelse(Fuel_Group == "BEV", 0, Avg_CO2)
  )

total_co2_impact <- Stock_model %>%
  filter(Data_Year == 2024, !is.na(Avg_CO2), !is.na(Avg_Mileage), Vehicle_Stock_GB > 0) %>%
  mutate(
    Annual_CO2_per_vehicle = (Avg_CO2 / 0.621371) * Avg_Mileage,
    Total_Annual_CO2 = Annual_CO2_per_vehicle * Vehicle_Stock_GB
  ) %>%
  group_by(Fuel_Group) %>%
  summarise(
    Total_CO2_tonnes = sum(Total_Annual_CO2, na.rm = TRUE) / 1e6,
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Avg_CO2_per_vehicle_tonnes = Total_CO2_tonnes / Total_Vehicles,
    .groups = "drop"
  )

ggplot(total_co2_impact, aes(x = reorder(Fuel_Group, -Total_CO2_tonnes),
                             y = Total_CO2_tonnes / 1e6, fill = Fuel_Group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Total_CO2_tonnes / 1e6, 1), "M tonnes\n",
                               round(Avg_CO2_per_vehicle_tonnes, 1), " t/veh")),
            vjust = -0.3, size = 3) +
  scale_y_continuous(labels = scales::label_number(suffix = "M tonnes")) +
  labs(
    title = "Total Annual CO2 Emissions by Fuel Type (2024)",
    subtitle = "Includes both emission rate and mileage impact",
    x = "Fuel Type",
    y = "Total Annual CO2 Emissions (Million tonnes)",
    caption = "BEVs contribute 0 tailpipe emissions despite higher average mileage"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ============================================================================
# VEHICLE CLASS ANALYSIS: MILEAGE & CO2 PATTERNS
# ============================================================================
vehicle_classes <- unique(Stock_model$vehicle_class)
cat("Number of vehicle classes:", length(vehicle_classes), "\n")
print(vehicle_classes)

Stock_model_plot <- Stock_model %>%
  mutate(
    size_category = case_when(
      grepl("Small", vehicle_class) ~ "Small",
      grepl("Medium", vehicle_class) ~ "Medium",
      grepl("Large", vehicle_class) & !grepl("Very Large", vehicle_class) ~ "Large",
      grepl("Very Large", vehicle_class) ~ "Very Large",
      grepl("Unsized", vehicle_class) ~ "Unsized",
      grepl("Unclassified", vehicle_class) ~ "Unclassified",
      TRUE ~ "Other"
    ),
    fuel_type = case_when(
      grepl("Petrol", vehicle_class) ~ "Petrol",
      grepl("Diesel", vehicle_class) ~ "Diesel",
      grepl("BEV", vehicle_class) ~ "BEV",
      grepl("HEV", vehicle_class) ~ "HEV",
      grepl("PHEV", vehicle_class) ~ "PHEV",
      TRUE ~ "Other"
    ),
    size_category = factor(size_category,
                           levels = c("Small", "Medium", "Large", "Very Large", "Unsized", "Unclassified"))
  ) %>%
  filter(size_category != "Unclassified" & !str_detect(size_category, "Unsized"), fuel_type != "Other")

class_summary_2024 <- Stock_model_plot %>%
  filter(Data_Year == 2024, !is.na(Avg_Mileage), Vehicle_Stock_GB > 0,
         vehicle_class != "Unclassified" & !grepl("Unsized", vehicle_class)) %>%
  group_by(vehicle_class, size_category, Fuel_Group) %>%
  summarise(
    Avg_CO2 = if(all(is.na(Avg_CO2))) 0 else
      weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage = weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Total_Mileage = sum(Total_Mileage_GB, na.rm = TRUE),
    Avg_Engine_Size = weighted.mean(engine_size, Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    CO2_per_mile = ifelse(Fuel_Group == "BEV", 0, Avg_CO2 / 0.621371),
    Annual_CO2_per_vehicle = CO2_per_mile * Avg_Mileage,
    Share_of_Vehicles = Total_Vehicles / sum(Total_Vehicles) * 100,
    Share_of_Mileage = Total_Mileage / sum(Total_Mileage) * 100
  )

paper_emissions_table <- class_summary_2024 %>%
  filter(Fuel_Group %in% c("Petrol","Diesel","HEV","PHEV","BEV")) %>%
  group_by(Fuel_Group, size_category) %>%
  summarise(
    Avg_CO2_gkm       = weighted.mean(Avg_CO2, Total_Vehicles, na.rm = TRUE),
    Avg_Mileage_miles = weighted.mean(Avg_Mileage, Total_Vehicles, na.rm = TRUE),
    Annual_CO2_tonnes = weighted.mean(Annual_CO2_per_vehicle, Total_Vehicles, na.rm = TRUE) / 10^6,
    N_Vehicles        = sum(Total_Vehicles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(size_category = factor(size_category, levels = c("Small","Medium","Large","Very Large"))) %>%
  arrange(Fuel_Group, size_category)

paper_emissions_table %>% filter(N_Vehicles < 1000) %>% print()

paper_age_table <- scatter_data %>%
  mutate(age_band = cut(Vehicle_Age, breaks = c(-1,5,10,15,23),
                        labels = c("0-5","6-10","11-15","16-23"))) %>%
  group_by(Fuel_Group, age_band) %>%
  summarise(
    Avg_CO2_gkm = weighted.mean(Avg_CO2, Total_Vehicles, na.rm = TRUE),
    Avg_Mileage = weighted.mean(Avg_Mileage, Total_Vehicles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Fuel_Group, age_band)

paper_age_size_table <- Stock_model_plot %>%
  filter(Data_Year == 2024, Fuel_Group %in% c("Petrol","Diesel","HEV","PHEV")) %>%
  mutate(age_band = cut(Vehicle_Age, breaks = c(-1,5,10,15,23),
                        labels = c("0-5","6-10","11-15","16-23"))) %>%
  group_by(Fuel_Group, age_band, size_category) %>%
  summarise(
    Avg_CO2 = weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
    N = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(N >= 1000) %>%
  arrange(Fuel_Group, age_band, size_category)

size_mix_by_age <- Stock_model_plot %>%
  filter(Data_Year == 2024, Fuel_Group %in% c("Petrol","Diesel")) %>%
  mutate(age_band = cut(Vehicle_Age, breaks = c(-1,5,10,15,23),
                        labels = c("0-5","6-10","11-15","16-23"))) %>%
  group_by(Fuel_Group, age_band, size_category) %>%
  summarise(N = sum(Vehicle_Stock_GB, na.rm = TRUE), .groups = "drop") %>%
  group_by(Fuel_Group, age_band) %>%
  mutate(Share = round(100 * N / sum(N), 1)) %>%
  ungroup() %>%
  select(Fuel_Group, age_band, size_category, Share) %>%
  pivot_wider(names_from = size_category, values_from = Share, values_fill = 0)

ggplot(class_summary_2024 %>% filter(!size_category %in% c("Unsized", "Unclassified")),
       aes(x = reorder(vehicle_class, Avg_Mileage), y = Avg_Mileage, fill = Fuel_Group)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Avg_Mileage), " mi\n",
                               round(Total_Vehicles/1000, 0), "k veh")),
            size = 2.8, hjust = 0.5, vjust = -0.3) +
  facet_wrap(~ size_category, scales = "free", nrow = 1) +
  scale_fill_brewer(palette = "Set2", name = "Fuel Type") +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(
    title = "Annual Mileage by Vehicle Class and Size (2024)",
    subtitle = "Numbers show: Average mileage and vehicle count (thousands)",
    x = "Vehicle Class",
    y = "Average Annual Mileage (miles)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

vehicle_distribution <- Stock_model_plot %>%
  filter(Data_Year == 2024, Vehicle_Stock_GB > 0) %>%
  mutate(
    simple_class = case_when(
      grepl("Petrol", vehicle_class) ~ "Petrol",
      grepl("Diesel", vehicle_class) ~ "Diesel",
      grepl("BEV", vehicle_class) ~ "BEV",
      grepl("HEV", vehicle_class) ~ "HEV",
      grepl("PHEV", vehicle_class) ~ "PHEV",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(size_category, simple_class) %>%
  summarise(
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  )

size_summary <- class_summary_2024 %>%
  filter(!size_category %in% c("Unsized", "Unclassified")) %>%
  group_by(size_category) %>%
  summarise(
    Weighted_Avg_CO2 = weighted.mean(Avg_CO2, Total_Vehicles, na.rm = TRUE),
    Weighted_Avg_Mileage = weighted.mean(Avg_Mileage, Total_Vehicles, na.rm = TRUE),
    Total_Vehicles = sum(Total_Vehicles, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(size_summary) +
  geom_bar(aes(x = size_category, y = Weighted_Avg_Mileage),
           stat = "identity", fill = "steelblue", alpha = 0.7, width = 0.6) +
  geom_line(aes(x = size_category, y = Weighted_Avg_CO2 * 50, group = 1),
            color = "darkred", size = 2) +
  geom_point(aes(x = size_category, y = Weighted_Avg_CO2 * 50),
             color = "darkred", size = 4) +
  scale_y_continuous(
    name = "Average Annual Mileage (miles)",
    sec.axis = sec_axis(~./50, name = "Average CO2 (g/km)")
  ) +
  labs(
    x = "Vehicle Size Category"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "steelblue", size = 12, face = "bold"),
    axis.title.y.right = element_text(color = "darkred", size = 12, face = "bold"),
    axis.text.y = element_text(color = "steelblue"),
    axis.text.y.right = element_text(color = "darkred"),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  geom_text(aes(x = size_category, y = Weighted_Avg_Mileage,
                label = paste0(round(Weighted_Avg_Mileage))),
            vjust = -0.5, color = "steelblue", fontface = "bold") +
  geom_text(aes(x = size_category, y = Weighted_Avg_CO2 * 50,
                label = paste0(round(Weighted_Avg_CO2))),
            vjust = -1.5, color = "darkred", fontface = "bold")

# Radar chart prep
if (!require(fmsb)) install.packages("fmsb")
library(fmsb)

top_classes <- class_summary_2024 %>%
  arrange(desc(Total_Vehicles)) %>%
  head(10) %>%
  select(vehicle_class, Avg_Mileage, Avg_CO2, Total_Vehicles) %>%
  mutate(
    Mileage_Score = (Avg_Mileage - min(Avg_Mileage)) /
      (max(Avg_Mileage) - min(Avg_Mileage)) * 100,
    CO2_Score = (Avg_CO2 - min(Avg_CO2)) /
      (max(Avg_CO2) - min(Avg_CO2)) * 100,
    Vehicle_Score = (Total_Vehicles - min(Total_Vehicles)) /
      (max(Total_Vehicles) - min(Total_Vehicles)) * 100
  )

cat("\n=== TOP 10 VEHICLE CLASSES BY COUNT (2024) ===\n")
print(top_classes %>%
        select(vehicle_class, Total_Vehicles, Avg_Mileage, Avg_CO2) %>%
        arrange(desc(Total_Vehicles)))

# Density plot sample
set.seed(123)
density_data <- Stock_model_plot %>%
  filter(Data_Year == 2024, !is.na(Avg_Mileage), Vehicle_Stock_GB > 0) %>%
  group_by(vehicle_class) %>%
  filter(sum(Vehicle_Stock_GB, na.rm = TRUE) > 100000) %>%
  ungroup() %>%
  sample_n(min(10000, nrow(.)), weight = Vehicle_Stock_GB, replace = TRUE)

mileage_table <- density_data %>%
  mutate(
    age_group = case_when(
      Vehicle_Age >= 0 & Vehicle_Age <= 5 ~ "0-5 years",
      Vehicle_Age >= 6 & Vehicle_Age <= 10 ~ "6-10 years",
      Vehicle_Age >= 11 & Vehicle_Age <= 15 ~ "11-15 years",
      Vehicle_Age >= 16 & Vehicle_Age <= 23 ~ "16-23 years",
      TRUE ~ "Other"
    )
  ) %>%
  filter(age_group != "Other", !is.na(fuel_type)) %>%
  group_by(age_group, size_category, fuel_type) %>%
  summarise(
    avg_mileage = round(mean(Avg_Mileage, na.rm = TRUE), 0),
    vehicle_count = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(age_group, size_category),
    names_from = fuel_type,
    values_from = avg_mileage,
    values_fill = 0
  ) %>%
  arrange(
    factor(age_group, levels = c("0-5 years", "6-10 years", "11-15 years", "16-23 years")),
    factor(size_category, levels = c("Small", "Medium", "Large", "Very Large"))
  )

cat("\n=== AVERAGE MILEAGE BY AGE GROUP, SIZE CATEGORY, AND FUEL TYPE ===\n")

baseline_data <- density_data %>%
  mutate(
    age_group = case_when(
      Vehicle_Age >= 0 & Vehicle_Age <= 5 ~ "0-5 years",
      Vehicle_Age >= 6 & Vehicle_Age <= 10 ~ "6-10 years",
      Vehicle_Age >= 11 & Vehicle_Age <= 15 ~ "11-15 years",
      Vehicle_Age >= 16 & Vehicle_Age <= 23 ~ "16-23 years",
      TRUE ~ "Other"
    )
  ) %>%
  filter(age_group != "Other", fuel_type == "Petrol", size_category == "Small") %>%
  group_by(age_group) %>%
  summarise(baseline_mileage = mean(Avg_Mileage, na.rm = TRUE), .groups = "drop")

ratio_table <- density_data %>%
  mutate(
    age_group = case_when(
      Vehicle_Age >= 0 & Vehicle_Age <= 5 ~ "0-5 years",
      Vehicle_Age >= 6 & Vehicle_Age <= 10 ~ "6-10 years",
      Vehicle_Age >= 11 & Vehicle_Age <= 15 ~ "11-15 years",
      Vehicle_Age >= 16 & Vehicle_Age <= 23 ~ "16-23 years",
      TRUE ~ "Other"
    )
  ) %>%
  filter(age_group != "Other", !is.na(fuel_type)) %>%
  group_by(age_group, size_category, fuel_type) %>%
  summarise(
    avg_mileage = mean(Avg_Mileage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(baseline_data, by = "age_group") %>%
  mutate(
    mileage_ratio = round(avg_mileage / baseline_mileage, 2),
    display = paste0(round(avg_mileage, 0), " (", mileage_ratio, "x)")
  ) %>%
  select(age_group, size_category, fuel_type, display) %>%
  pivot_wider(
    id_cols = c(age_group, size_category),
    names_from = fuel_type,
    values_from = display,
    values_fill = "0 (0x)"
  ) %>%
  arrange(
    factor(age_group, levels = c("0-5 years", "6-10 years", "11-15 years", "16-23 years")),
    factor(size_category, levels = c("Small", "Medium", "Large", "Very Large"))
  )

ggplot(density_data, aes(x = Avg_Mileage, fill = fuel_type)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ size_category, scales = "free_y") +
  scale_fill_brewer(palette = "Set2", name = "Fuel Type") +
  labs(
    x = "Annual Mileage (miles)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

ggplot(density_data, aes(x = Avg_Mileage, color = fuel_type, weight = Vehicle_Stock_GB)) +
  stat_ecdf(size = 1) +
  facet_wrap(~ size_category) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set2", name = "Fuel Type") +
  labs(
    x = "Annual Mileage (miles)",
    y = "Cumulative % of Vehicles"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

density_data_peak <- density_data %>%
  filter(Vehicle_Age >= 4, Vehicle_Age <= 8)

ggplot(density_data_peak, aes(x = Avg_Mileage, fill = fuel_type)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ size_category, scales = "free_y") +
  scale_fill_brewer(palette = "Set2", name = "Fuel Type") +
  labs(
    x = "Annual Mileage (miles)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14))

# Summary table with kableExtra
summary_table_formatted <- class_summary_2024 %>%
  mutate(
    `Vehicles (k)` = round(Total_Vehicles / 1000, 0),
    `Avg Mileage` = round(Avg_Mileage, 0),
    `Avg CO2 (g/km)` = round(Avg_CO2, 0),
    `Share of Fleet %` = round(Share_of_Vehicles, 1),
    `Share of Mileage %` = round(Share_of_Mileage, 1)
  ) %>%
  select(
    `Vehicle Class` = vehicle_class,
    Size = size_category,
    Fuel = Fuel_Group,
    `Vehicles (k)`,
    `Avg Mileage`,
    `Avg CO2 (g/km)`,
    `Share of Fleet %`,
    `Share of Mileage %`
  ) %>%
  arrange(desc(`Vehicles (k)`))

kable(summary_table_formatted, align = c("l", "l", "l", "c", "c", "c", "c", "c")) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) %>%
  row_spec(0, bold = TRUE, background = "#2c3e50", color = "white") %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(4:8, background = "#ecf0f1") %>%
  add_header_above(c(" " = 3, "Key Metrics" = 5)) %>%
  footnote(
    general = "BEVs show 0 g/km CO2 emissions. Vehicle count in thousands.",
    general_title = "Note:"
  )

# ============================================================================
# DIMENSION LOOKUP + KERB WEIGHT ADDITION
# ============================================================================
zip_path <- file.path(data_dir, "uk_fleet_enriched_v2.zip")

if (!file.exists(zip_path)) {
  stop("Zip file not found at: ", zip_path)
}

contents <- tryCatch({
  unzip(zip_path, list = TRUE)
}, error = function(e) {
  print(paste("Error listing zip contents:", e$message))
  NULL
})

if (!is.null(contents)) {
  print("Contents of zip file:")
  print(contents)
}

extract_dir <- file.path(data_dir, "extracted")
if (!dir.exists(extract_dir)) {
  dir.create(extract_dir)
}

tryCatch({
  unzip(zip_path, exdir = extract_dir, overwrite = TRUE)
  print("Extraction successful!")
}, error = function(e) {
  print(paste("Extraction error:", e$message))
})

parquet_files <- list.files(extract_dir, pattern = "\\.parquet$",
                            full.names = TRUE, recursive = TRUE)

if (length(parquet_files) > 0) {
  print(paste("Found parquet file:", parquet_files[1]))
  data_1 <- read_parquet(parquet_files[1])
  print(head(data_1))
  print(dim(data_1))
} else {
  print("No parquet file found in extraction directory")
}

parquet_data <- read_parquet(parquet_files[1])

# Build dimension lookup
dimension_lookup <- parquet_data %>%
  select(Make, Model, GenModel, YearManufacture, fuel_simple,
         rdw_length_mm, rdw_width_mm, rdw_height_mm,
         rdw_kerb_weight_kg, rdw_wheelbase_mm) %>%
  filter(!is.na(rdw_length_mm) | !is.na(rdw_width_mm) | !is.na(rdw_height_mm)) %>%
  distinct() %>%
  group_by(Make, Model, GenModel, YearManufacture, fuel_simple) %>%
  summarise(
    rdw_length_mm = median(rdw_length_mm, na.rm = TRUE),
    rdw_width_mm = median(rdw_width_mm, na.rm = TRUE),
    rdw_height_mm = median(rdw_height_mm, na.rm = TRUE),
    rdw_kerb_weight_kg = median(rdw_kerb_weight_kg, na.rm = TRUE),
    rdw_wheelbase_mm = median(rdw_wheelbase_mm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Fuel_Group = case_when(
      fuel_simple %in% c("PETROL") ~ "Petrol",
      fuel_simple %in% c("BEV") ~ "BEV",
      fuel_simple %in% c("DIESEL") ~ "Diesel",
      fuel_simple %in% c("HEV", "HYBRID") ~ "HEV",
      fuel_simple %in% c("PHEV") ~ "PHEV",
      TRUE ~ "Other"
    )
  ) %>%
  select(-fuel_simple)

print(paste("Dimension lookup created with", nrow(dimension_lookup), "unique vehicles"))

write.csv(dimension_lookup, file.path(out_dir, "dimension_lookup.csv"), row.names = FALSE)

# Impute missing kerb weights
data_imputed <- data_1 %>%
  group_by(Make, Model) %>%
  mutate(
    rdw_kerb_weight_kg = ifelse(
      is.na(rdw_kerb_weight_kg),
      median(rdw_kerb_weight_kg, na.rm = TRUE),
      rdw_kerb_weight_kg
    )
  ) %>%
  ungroup()

data_imputed <- data_1 %>%
  group_by(fuel_simple) %>%
  mutate(
    rdw_kerb_weight_kg = ifelse(
      is.na(rdw_kerb_weight_kg),
      mean(rdw_kerb_weight_kg, na.rm = TRUE),
      rdw_kerb_weight_kg
    )
  ) %>%
  ungroup()

sum(is.na(data_imputed$rdw_kerb_weight_kg))

data_1 <- data_1 %>%
  filter(!is.na(rdw_kerb_weight_kg), LicenceStatus == "LICENSED")

# Build 6-level kerb-weight lookup
dt <- as.data.table(data_1)

lookup_level1 <- dt[
  !is.na(rdw_kerb_weight_kg) &
    rdw_kerb_weight_kg >= 500 &
    rdw_kerb_weight_kg <= 3000 &
    LicenceStatus == "LICENSED"
] %>%
  group_by(Make, Model, GenModel, YearManufacture, fuel_simple) %>%
  summarise(
    kerb_level1 = median(rdw_kerb_weight_kg, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Fuel_Group = case_when(
      fuel_simple %in% c("PETROL") ~ "Petrol",
      fuel_simple %in% c("BEV") ~ "BEV",
      fuel_simple %in% c("DIESEL") ~ "Diesel",
      fuel_simple %in% c("HEV", "HYBRID") ~ "HEV",
      fuel_simple %in% c("PHEV") ~ "PHEV",
      fuel_simple %in% c("OTHER", "FCEV") ~ "Other",
      TRUE ~ "Other"
    )
  ) %>%
  select(-fuel_simple)

lookup_level1 <- lookup_level1 %>%
  group_by(Make, Model, GenModel, YearManufacture, Fuel_Group) %>%
  summarise(kerb_level1 = median(kerb_level1, na.rm = TRUE), .groups = "drop")

lookup_level2 <- dt[
  !is.na(rdw_kerb_weight_kg) &
    rdw_kerb_weight_kg >= 500 &
    rdw_kerb_weight_kg <= 3000 &
    LicenceStatus == "LICENSED"
] %>%
  group_by(Make, Model, GenModel, YearManufacture) %>%
  summarise(
    kerb_level2 = median(rdw_kerb_weight_kg, na.rm = TRUE),
    .groups = 'drop'
  )

lookup_level3 <- dt[
  !is.na(rdw_kerb_weight_kg) &
    rdw_kerb_weight_kg >= 500 &
    rdw_kerb_weight_kg <= 3000 &
    LicenceStatus == "LICENSED"
] %>%
  group_by(Make, Model, GenModel) %>%
  summarise(
    kerb_level3 = median(rdw_kerb_weight_kg, na.rm = TRUE),
    .groups = 'drop'
  )

lookup_level4 <- dt[
  !is.na(rdw_kerb_weight_kg) &
    rdw_kerb_weight_kg >= 500 &
    rdw_kerb_weight_kg <= 3000 &
    LicenceStatus == "LICENSED"
] %>%
  group_by(Make, Model) %>%
  summarise(
    kerb_level4 = median(rdw_kerb_weight_kg, na.rm = TRUE),
    .groups = 'drop'
  )

lookup_level5 <- dt[
  !is.na(rdw_kerb_weight_kg) &
    rdw_kerb_weight_kg >= 500 &
    rdw_kerb_weight_kg <= 3000 &
    LicenceStatus == "LICENSED"
] %>%
  group_by(Make) %>%
  summarise(
    kerb_level5 = median(rdw_kerb_weight_kg, na.rm = TRUE),
    .groups = 'drop'
  )

lookup_level6 <- dt[
  !is.na(rdw_kerb_weight_kg) &
    rdw_kerb_weight_kg >= 500 &
    rdw_kerb_weight_kg <= 3000 &
    LicenceStatus == "LICENSED"
] %>%
  mutate(
    Fuel_Group = case_when(
      fuel_simple %in% c("PETROL") ~ "Petrol",
      fuel_simple %in% c("BEV") ~ "BEV",
      fuel_simple %in% c("DIESEL") ~ "Diesel",
      fuel_simple %in% c("HEV", "HYBRID") ~ "HEV",
      fuel_simple %in% c("PHEV") ~ "PHEV",
      fuel_simple %in% c("OTHER", "FCEV") ~ "Other",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(Fuel_Group) %>%
  summarise(
    kerb_level6 = median(rdw_kerb_weight_kg, na.rm = TRUE),
    .groups = 'drop'
  )

Fleet_complete <- Fleet_complete %>%
  select(-any_of(c("kerb_weight_kg", "kerb_weight_kg.x", "kerb_weight_kg.y")))

Fleet_complete <- Fleet_complete %>%
  left_join(lookup_level1, by = c("Make", "Model", "GenModel", "YearManufacture", "Fuel_Group")) %>%
  left_join(lookup_level2, by = c("Make", "Model", "GenModel", "YearManufacture")) %>%
  left_join(lookup_level3, by = c("Make", "Model", "GenModel")) %>%
  left_join(lookup_level4, by = c("Make", "Model")) %>%
  left_join(lookup_level5, by = "Make") %>%
  left_join(lookup_level6, by = "Fuel_Group") %>%
  mutate(
    kerb_weight_kg = coalesce(kerb_level1, kerb_level2, kerb_level3, kerb_level4, kerb_level5, kerb_level6),
    match_level = case_when(
      !is.na(kerb_level1) ~ "Level 1: Exact (Make+Model+GenModel+Year+Fuel)",
      !is.na(kerb_level2) & is.na(kerb_level1) ~ "Level 2: Make+Model+GenModel+Year (No Fuel)",
      !is.na(kerb_level3) & is.na(kerb_level2) & is.na(kerb_level1) ~ "Level 3: Make+Model+GenModel (No Year/Fuel)",
      !is.na(kerb_level4) & is.na(kerb_level3) & is.na(kerb_level2) & is.na(kerb_level1) ~ "Level 4: Make+Model Only",
      !is.na(kerb_level5) & is.na(kerb_level4) & is.na(kerb_level3) & is.na(kerb_level2) & is.na(kerb_level1) ~ "Level 5: Make Only",
      !is.na(kerb_level6) & is.na(kerb_level5) & is.na(kerb_level4) & is.na(kerb_level3) & is.na(kerb_level2) & is.na(kerb_level1) ~ "Level 6: Fuel Group Only",
      TRUE ~ "No Match"
    )
  ) %>%
  select(-kerb_level1, -kerb_level2, -kerb_level3, -kerb_level4, -kerb_level5, -kerb_level6)

cp_report(Fleet_complete, "Fleet_Count", "CP3: Fleet_complete after kerb-weight join")

names(Fleet_complete)

total_rows <- nrow(Fleet_complete)
matched_rows <- sum(!is.na(Fleet_complete$kerb_weight_kg))
match_rate <- matched_rows / total_rows * 100

cat(sprintf("Total rows: %d\n", total_rows))
cat(sprintf("Rows with kerb weight: %d\n", matched_rows))
cat(sprintf("Overall match rate: %.1f%%\n", match_rate))

match_level_summary <- Fleet_complete %>%
  count(match_level) %>%
  mutate(
    percentage = n / sum(n) * 100,
    cumulative = cumsum(percentage)
  ) %>%
  arrange(desc(percentage))

print("=== MATCH LEVEL SUMMARY ===")
print(match_level_summary)

kerb_by_fuel <- Fleet_complete %>%
  group_by(Fuel_Group) %>%
  summarise(
    n = n(),
    matched = sum(!is.na(kerb_weight_kg)),
    match_rate = matched / n * 100,
    mean_kerb = mean(kerb_weight_kg, na.rm = TRUE),
    median_kerb = median(kerb_weight_kg, na.rm = TRUE)
  )

print("=== KERB WEIGHT BY FUEL GROUP ===")
print(kerb_by_fuel)

total_rows <- nrow(Fleet_complete)
matched_rows <- sum(!is.na(Fleet_complete$kerb_weight_kg))
match_rate <- matched_rows / total_rows * 100

cat(sprintf("Total rows: %d\n", total_rows))
cat(sprintf("Rows with kerb weight: %d\n", matched_rows))
cat(sprintf("Match rate: %.1f%%\n", match_rate))

match_level_summary <- Fleet_complete %>%
  count(match_level) %>%
  mutate(
    percentage = n / sum(n) * 100,
    cumulative = cumsum(percentage)
  ) %>%
  arrange(desc(percentage))

print("=== MATCH LEVEL SUMMARY ===")
print(match_level_summary)

# ============================================================================
# FULL FLEET AGE / FUEL / KERB-WEIGHT IMPUTATION
# ============================================================================

fleet_known <- Fleet_complete %>%
  filter(!is.na(Vehicle_Age), Fuel_Group != "Other") %>%
  mutate(
    engine_size = as.character(engine_size),
    vehicle_class = as.character(vehicle_class)
  ) %>%
  group_by(
    Make, Model, GenModel,
    Vehicle_Age,
    Fuel,
    Fuel_Group,
    vehicle_class,
    engine_size,
    kerb_weight_kg,
    Data_Year
  ) %>%
  summarise(
    Fleet_Count = sum(Fleet_Count),
    .groups = "drop"
  )

age_shares_by_fuel <- fleet_known %>%
  filter(Fuel_Group %in% c("BEV", "Diesel", "HEV", "PHEV", "Petrol")) %>%
  group_by(Fuel_Group, Vehicle_Age, Data_Year) %>%
  summarise(n = sum(Fleet_Count), .groups = "drop") %>%
  group_by(Fuel_Group, Data_Year) %>%
  mutate(age_share = n / sum(n))

na_age_fuel_totals <- Fleet_complete %>%
  filter(is.na(Vehicle_Age), Fuel_Group %in% c("BEV", "Diesel", "HEV", "PHEV", "Petrol")) %>%
  group_by(Fuel_Group, Data_Year) %>%
  summarise(total_na = sum(Fleet_Count, na.rm = TRUE), .groups = "drop")

class_engine_profile_fuel <- fleet_known %>%
  filter(Fuel_Group %in% c("BEV", "Diesel", "HEV", "PHEV", "Petrol")) %>%
  group_by(Fuel_Group, Make, Model, GenModel, vehicle_class, engine_size, kerb_weight_kg, Data_Year) %>%
  summarise(
    profile_n = sum(Fleet_Count),
    .groups = "drop"
  ) %>%
  group_by(Fuel_Group, Data_Year) %>%
  mutate(profile_share = profile_n / sum(profile_n))

step1_base <- age_shares_by_fuel %>%
  left_join(na_age_fuel_totals, by = c("Fuel_Group", "Data_Year")) %>%
  filter(!is.na(total_na) & total_na > 0) %>%
  mutate(base_count = age_share * total_na) %>%
  filter(base_count > 0)

step1_allocated <- step1_base %>%
  left_join(class_engine_profile_fuel, by = c("Fuel_Group", "Data_Year")) %>%
  mutate(
    Fleet_Count = base_count * profile_share,
    Fuel = NA_character_
  ) %>%
  select(Make, Model, GenModel, Vehicle_Age, Fuel, Fuel_Group, vehicle_class, engine_size, kerb_weight_kg, Fleet_Count, Data_Year) %>%
  filter(!is.na(Fleet_Count) & Fleet_Count > 0)

fuel_shares_by_age <- fleet_known %>%
  group_by(Vehicle_Age, Fuel_Group, Data_Year) %>%
  summarise(n = sum(Fleet_Count), .groups = "drop") %>%
  group_by(Vehicle_Age, Data_Year) %>%
  mutate(fuel_share = n / sum(n))

other_fuel_by_age <- Fleet_complete %>%
  filter(Fuel_Group == "Other", !is.na(Vehicle_Age)) %>%
  group_by(Vehicle_Age, Data_Year) %>%
  summarise(total_other = sum(Fleet_Count), .groups = "drop")

class_engine_profile_age_fuel <- fleet_known %>%
  group_by(Vehicle_Age, Fuel_Group, Make, Model, GenModel, vehicle_class, engine_size, kerb_weight_kg, Data_Year) %>%
  summarise(
    profile_n = sum(Fleet_Count),
    .groups = "drop"
  ) %>%
  group_by(Vehicle_Age, Fuel_Group, Data_Year) %>%
  mutate(profile_share = profile_n / sum(profile_n))

step2_base <- fuel_shares_by_age %>%
  left_join(other_fuel_by_age, by = c("Vehicle_Age", "Data_Year")) %>%
  filter(!is.na(total_other) & total_other > 0) %>%
  mutate(base_count = fuel_share * total_other) %>%
  filter(base_count > 0)

step2_allocated <- step2_base %>%
  left_join(class_engine_profile_age_fuel, by = c("Vehicle_Age", "Fuel_Group", "Data_Year")) %>%
  mutate(
    Fleet_Count = base_count * profile_share,
    Fuel = NA_character_
  ) %>%
  select(Make, Model, GenModel, Vehicle_Age, Fuel, Fuel_Group, vehicle_class, engine_size, kerb_weight_kg, Fleet_Count, Data_Year) %>%
  filter(!is.na(Fleet_Count) & Fleet_Count > 0)

class_engine_profile_overall <- fleet_known %>%
  group_by(Fuel_Group, Make, Model, GenModel, vehicle_class, engine_size, kerb_weight_kg, Data_Year) %>%
  summarise(
    profile_n = sum(Fleet_Count),
    .groups = "drop"
  ) %>%
  group_by(Fuel_Group, Data_Year) %>%
  mutate(profile_share = profile_n / sum(profile_n))

total_unknown_by_year <- Fleet_complete %>%
  filter(is.na(Vehicle_Age), Fuel_Group == "Other") %>%
  group_by(Data_Year) %>%
  summarise(total_unknown = sum(Fleet_Count, na.rm = TRUE), .groups = "drop")

age_profile <- fleet_known %>%
  group_by(Vehicle_Age, Data_Year) %>%
  summarise(n = sum(Fleet_Count), .groups = "drop") %>%
  group_by(Data_Year) %>%
  mutate(age_share = n / sum(n))

fuel_profile <- fleet_known %>%
  group_by(Vehicle_Age, Fuel_Group, Data_Year) %>%
  summarise(n = sum(Fleet_Count), .groups = "drop") %>%
  group_by(Vehicle_Age, Data_Year) %>%
  mutate(fuel_share = n / sum(n))

if(sum(total_unknown_by_year$total_unknown) > 0) {
  step3_allocated_list <- list()

  for(year in unique(total_unknown_by_year$Data_Year)) {

    year_age_profile <- age_profile %>% filter(Data_Year == year)
    year_fuel_profile <- fuel_profile %>% filter(Data_Year == year)
    year_unknown <- total_unknown_by_year %>% filter(Data_Year == year)

    if(nrow(year_unknown) > 0 && year_unknown$total_unknown > 0) {

      step3_base_year <- expand.grid(
        Vehicle_Age = unique(year_age_profile$Vehicle_Age),
        Fuel_Group = unique(year_fuel_profile$Fuel_Group),
        stringsAsFactors = FALSE
      ) %>%
        left_join(year_age_profile %>% select(Vehicle_Age, age_share), by = "Vehicle_Age") %>%
        left_join(year_fuel_profile %>% select(Vehicle_Age, Fuel_Group, fuel_share),
                  by = c("Vehicle_Age", "Fuel_Group")) %>%
        mutate(
          total_unknown = year_unknown$total_unknown,
          base_count = age_share * fuel_share * total_unknown,
          Data_Year = year
        ) %>%
        filter(!is.na(base_count) & base_count > 0)

      year_class_profile <- class_engine_profile_overall %>% filter(Data_Year == year)

      step3_allocated_year <- step3_base_year %>%
        left_join(year_class_profile, by = c("Fuel_Group", "Data_Year")) %>%
        mutate(
          Fleet_Count = base_count * profile_share,
          Fuel = NA_character_
        ) %>%
        select(Make, Model, GenModel, Vehicle_Age, Fuel, Fuel_Group, vehicle_class, engine_size, kerb_weight_kg, Fleet_Count, Data_Year) %>%
        filter(!is.na(Fleet_Count) & Fleet_Count > 0)

      step3_allocated_list[[as.character(year)]] <- step3_allocated_year
    }
  }

  step3_allocated <- bind_rows(step3_allocated_list)
} else {
  step3_allocated <- data.frame(
    Make = character(),
    Model = character(),
    GenModel = character(),
    Vehicle_Age = integer(),
    Fuel = character(),
    Fuel_Group = character(),
    vehicle_class = character(),
    engine_size = character(),
    kerb_weight_kg = character(),
    Fleet_Count = numeric(),
    Data_Year = integer()
  )
}

final_complete <- bind_rows(
  fleet_known,
  step1_allocated,
  step2_allocated,
  step3_allocated
) %>%
  group_by(
    Make, Model, GenModel,
    Vehicle_Age,
    Fuel,
    Fuel_Group,
    vehicle_class,
    engine_size,
    kerb_weight_kg,
    Data_Year
  ) %>%
  summarise(
    Fleet_Count = sum(Fleet_Count),
    .groups = "drop"
  ) %>%
  mutate(
    Fleet_Count = ifelse(is.na(Fleet_Count) | is.nan(Fleet_Count), 0, Fleet_Count),
    engine_size = as.character(engine_size),
    kerb_weight_kg = as.character(kerb_weight_kg),
    vehicle_class = as.character(vehicle_class)
  )

cp_report(final_complete, "Fleet_Count", "CP4: final_complete after age/fuel imputation")

cat("\n=== FINAL_COMPLETE NOW HAS MAKE, MODEL, GENMODEL ===\n")
cat("Columns:", paste(names(final_complete), collapse = ", "), "\n")
cat("Unique Makes:", paste(head(unique(final_complete$Make), 10), collapse = ", "), "\n")

cat("\n--- FINAL VERIFICATION ---\n")
cat("Final total:", sum(final_complete$Fleet_Count), "\n")
cat("Original total:", sum(Fleet_complete$Fleet_Count, na.rm = TRUE), "\n\n")

sal_after <- final_complete %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year) %>%
  summarise(fleet = sum(Fleet_Count, na.rm = TRUE), .groups = "drop")

print("Sales AFTER imputation:")
print(sal_after)

# 2023 vs 2024 comparison
comparison_base <- bind_rows(
  final_complete %>%
    filter(Data_Year == 2023, Vehicle_Age == 0) %>%
    summarise(
      category = "2023 New Sales",
      vehicles = sum(Fleet_Count, na.rm = TRUE),
      .groups = "drop"
    ),
  final_complete %>%
    filter(Data_Year == 2024, Vehicle_Age == 1) %>%
    summarise(
      category = "2024 Age 1 Vehicles",
      vehicles = sum(Fleet_Count, na.rm = TRUE),
      .groups = "drop"
    )
)

comparison <- comparison_base %>%
  bind_rows(
    data.frame(
      category = "Difference (2024 Age 1 Vehicles - 2023 New Sales)",
      vehicles = comparison_base$vehicles[2] - comparison_base$vehicles[1]
    )
  )

cat("\n=== 2023 NEW VS 2024 AGE 1 ===\n")
print(comparison)

comparison_all_years <- bind_rows(
  map_dfr(2014:2023, function(year) {
    final_complete %>%
      filter(Data_Year == year, Vehicle_Age == 0) %>%
      summarise(
        category = paste0(year, " New Sales (Age 0)"),
        vehicles = sum(Fleet_Count, na.rm = TRUE),
        .groups = "drop"
      )
  }),

  map_dfr(2015:2024, function(year) {
    final_complete %>%
      filter(Data_Year == year, Vehicle_Age == 1) %>%
      summarise(
        category = paste0(year, " Age 1 Vehicles"),
        vehicles = sum(Fleet_Count, na.rm = TRUE),
        .groups = "drop"
      )
  })
) %>%
  arrange(category)

print(comparison_all_years)

paired_comparisons <- map_dfr(2014:2023, function(year) {

  age0 <- final_complete %>%
    filter(Data_Year == year, Vehicle_Age == 0) %>%
    summarise(vehicles = sum(Fleet_Count, na.rm = TRUE)) %>%
    pull(vehicles)

  age1_next <- final_complete %>%
    filter(Data_Year == year + 1, Vehicle_Age == 1) %>%
    summarise(vehicles = sum(Fleet_Count, na.rm = TRUE)) %>%
    pull(vehicles)

  data.frame(
    category = paste0(year, "-", year+1, " Comparison"),
    age0 = age0,
    age1_next = age1_next,
    difference = age1_next - age0,
    pct_change = round((age1_next - age0) / age0 * 100, 1)
  )
})

print(paired_comparisons)

# Detailed 2024 analysis
cat("\n=== DETAILED 2024 SALES ANALYSIS ===\n")

before_2024 <- sal_before %>% filter(Data_Year == 2024) %>% pull(fleet)
after_2024 <- sal_after %>% filter(Data_Year == 2024) %>% pull(fleet)
direct_increase <- after_2024 - before_2024

step1_2024 <- step1_allocated %>%
  filter(Data_Year == 2024, Vehicle_Age == 0) %>%
  pull(Fleet_Count) %>% sum()

step2_2024 <- step2_allocated %>%
  filter(Data_Year == 2024, Vehicle_Age == 0) %>%
  pull(Fleet_Count) %>% sum()

step3_2024 <- step3_allocated %>%
  filter(Data_Year == 2024, Vehicle_Age == 0) %>%
  pull(Fleet_Count) %>% sum()

step2_by_fuel <- step2_allocated %>%
  filter(Data_Year == 2024, Vehicle_Age == 0) %>%
  group_by(Fuel_Group) %>%
  summarise(vehicles = sum(Fleet_Count), .groups = "drop")

other_before <- Fleet_complete %>%
  filter(Data_Year == 2024, Fuel_Group == "Other", Vehicle_Age == 0) %>%
  summarise(total = sum(Fleet_Count)) %>% pull(total)

cat("\n2024 Sales Comparison:\n")
cat("  Before imputation:", before_2024, "\n")
cat("  After imputation: ", after_2024, "\n")
cat("  Direct increase:  ", direct_increase, "\n\n")

cat("Step Contributions (NEW vehicles only):\n")
cat("  Step 1 (NA age, known fuel):", step1_2024, "\n")
cat("  Step 3 (completely unknown):", step3_2024, "\n")
cat("  TOTAL NEW VEHICLES:         ", step1_2024 + step3_2024, "\n\n")

cat("Step 2 (Reclassification of 'Other' fuel):\n")
cat("  'Other' fuel before imputation:", other_before, "\n")
cat("  Step 2 total:                  ", step2_2024, "\n")
cat("  Step 2 distribution:\n")
print(step2_by_fuel)

cat("\nVerification:\n")
cat("  Before + Step1 + Step3 =", before_2024 + step1_2024 + step3_2024, "\n")
cat("  After imputation       =", after_2024, "\n")
cat("  Match?                 =",
    abs((before_2024 + step1_2024 + step3_2024) - after_2024) < 0.1, "\n")

# ---------------------------------------------------------------------------
# Final checks and clean-up
# ---------------------------------------------------------------------------
yearly_check <- final_complete %>%
  group_by(Data_Year) %>%
  summarise(imputed = sum(Fleet_Count)) %>%
  left_join(
    Fleet_complete %>%
      group_by(Data_Year) %>%
      summarise(original = sum(Fleet_Count, na.rm = TRUE)),
    by = "Data_Year"
  ) %>%
  mutate(difference = original - imputed)

print(yearly_check)

cat("\n--- NA CHECK ---\n")
na_summary <- final_complete %>%
  summarise(across(everything(), ~sum(is.na(.))))
print(na_summary)

cat("\n--- 2024 DISTRIBUTION BY FUEL GROUP ---\n")
final_complete %>%
  filter(Data_Year == 2024) %>%
  group_by(Fuel_Group) %>%
  summarise(total = sum(Fleet_Count)) %>%
  mutate(share = total / sum(total) * 100) %>%
  arrange(desc(total)) %>%
  print()

cat("\n--- SAMPLE OF FINAL DATA (2024) ---\n")
final_complete %>%
  filter(Data_Year == 2024) %>%
  sample_n(10) %>%
  print()

cat("\n=== 2024 FLEET COMPOSITION ===\n")
cat("Total vehicles:", format(sum(final_complete$Fleet_Count), big.mark = ","), "\n")
cat("Fuel types:", paste(unique(final_complete$Fuel_Group), collapse = ", "), "\n")
cat("Max age:", max(final_complete$Vehicle_Age), "\n")

sum(is.na(final_complete$Fleet_Count))
sum(final_complete$Fuel_Group == "Other", na.rm = TRUE)
sum(is.na(final_complete$Fuel_Group))
sum(is.na(final_complete$Vehicle_Age))
sum(is.na(final_complete$Data_Year))
sum(is.na(final_complete$engine_size))

# Handle NA engine sizes
cat("=== MISSING ENGINE SIZES BEFORE IMPUTATION ===\n")
missing_before <- final_complete %>%
  group_by(Fuel_Group) %>%
  summarise(
    total_rows = n(),
    na_count = sum(is.na(engine_size)),
    na_pct = round(mean(is.na(engine_size)) * 100, 2)
  ) %>%
  arrange(desc(na_count))
print(missing_before)

set.seed(123)

final_complete_imputed <- final_complete %>%
  group_by(Fuel_Group) %>%
  mutate(
    engine_size_numeric = as.numeric(as.character(engine_size)),
    engine_size_imputed = ifelse(
      is.na(engine_size_numeric) & n() > sum(is.na(engine_size_numeric)),
      sample(engine_size_numeric[!is.na(engine_size_numeric)],
             size = n(),
             replace = TRUE),
      engine_size_numeric
    )
  ) %>%
  ungroup() %>%
  mutate(
    engine_size = engine_size_imputed
  ) %>%
  select(-engine_size_numeric, -engine_size_imputed)

cat("\n=== MISSING ENGINE SIZES AFTER IMPUTATION ===\n")
missing_after <- final_complete_imputed %>%
  group_by(Fuel_Group) %>%
  summarise(
    total_rows = n(),
    na_count = sum(is.na(engine_size)),
    na_pct = round(mean(is.na(engine_size)) * 100, 2)
  ) %>%
  arrange(desc(na_count))
print(missing_after)

cat("\n=== IMPROVEMENT SUMMARY ===\n")
improvement <- missing_before %>%
  left_join(missing_after, by = "Fuel_Group", suffix = c("_before", "_after")) %>%
  mutate(
    nas_removed = na_count_before - na_count_after,
    pct_improvement = na_pct_before - na_pct_after
  ) %>%
  select(Fuel_Group, na_count_before, na_count_after, nas_removed, na_pct_before, na_pct_after)
print(improvement)

final_complete <- final_complete_imputed
join_final_complete_clean <- final_complete
cp_report(join_final_complete_clean, "Fleet_Count", "CP5: join_final_complete_clean")

cat("\n=== FINAL CHECK ===\n")
cat("Total NAs remaining:", sum(is.na(final_complete$engine_size)), "\n")

sum(is.na(join_final_complete_clean$engine_size))
sum(is.na(join_final_complete_clean$kerb_weight_kg))
unique(join_final_complete_clean$engine_size)

join_final_complete_clean <- join_final_complete_clean %>%
  mutate(
    kerb_weight_kg = as.numeric(as.character(kerb_weight_kg))
  )

cat("Rows with NA after conversion:", sum(is.na(join_final_complete_clean$kerb_weight_kg)), "\n")

hev_median <- join_final_complete_clean %>%
  filter(Fuel_Group == "HEV", !is.na(kerb_weight_kg)) %>%
  summarise(median_kerb = median(kerb_weight_kg, na.rm = TRUE)) %>%
  pull(median_kerb)

cat(sprintf("HEV median kerb weight: %.0f kg\n", hev_median))

join_final_complete_clean <- join_final_complete_clean %>%
  mutate(
    kerb_weight_kg = ifelse(
      Make == "COLEMAN MILNE" & is.na(kerb_weight_kg),
      hev_median,
      kerb_weight_kg
    )
  )

sum(is.na(join_final_complete_clean$kerb_weight_kg))

vehicel_numer <- join_final_complete_clean %>%
  group_by(Data_Year) %>%
  summarise(fleet = sum(Fleet_Count, na.rm = TRUE), .groups = "drop") %>%
  print

cat("Missing values BEFORE imputation:\n")
join_final_complete_clean %>%
  group_by(Fuel_Group) %>%
  summarise(
    total = n(),
    na_count = sum(is.na(engine_size)),
    na_pct = round(mean(is.na(engine_size)) * 100, 2)
  ) %>%
  print()

bev_known_values <- join_final_complete_clean %>%
  filter(Fuel_Group == "BEV", !is.na(engine_size)) %>%
  pull(engine_size)

cat("\nBEV known values for imputation:", length(bev_known_values), "\n")
cat("BEV missing values to impute:",
    sum(is.na(join_final_complete_clean$engine_size[join_final_complete_clean$Fuel_Group == "BEV"])), "\n")

set.seed(123)

Vcar_for_plotss <- join_final_complete_clean %>%
  mutate(
    engine_size = ifelse(
      Fuel_Group == "BEV" & is.na(engine_size),
      sample(bev_known_values, size = n(), replace = TRUE)[row_number()],
      engine_size
    ),
    engine_size_liters = engine_size / 1000
  )
cp_report(Vcar_for_plotss, "Fleet_Count", "CP6: after BEV engine-size imputation")

cat("\nMissing values AFTER imputation (BEV only):\n")
Vcar_for_plotss %>%
  group_by(Fuel_Group) %>%
  summarise(
    total = n(),
    na_count = sum(is.na(engine_size)),
    na_pct = round(mean(is.na(engine_size)) * 100, 2)
  ) %>%
  print()

cat("\nRange Extended Electric vehicles sample:\n")
Vcar_for_plotss %>%
  filter(Fuel == "Range Extended Electric") %>%
  head(10) %>%
  print()

sal <- Vcar_for_plotss %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year) %>%
  summarise(fleet = sum(Fleet_Count, na.rm = TRUE), .groups = "drop")

print(sal)

join_final_complete_clean <- Vcar_for_plotss

join_final_complete_clean <- join_final_complete_clean %>%
  mutate(
    engine_size_clean = as.numeric(
      ifelse(engine_size %in% c("[x]", "[z]", "N/A", "NA", "", NA),
             NA,
             engine_size)
    ),
    engine_size_liters = engine_size_clean / 1000
  )

join_final_complete_clean_BEV <- join_final_complete_clean %>%
  filter(Fuel_Group == "BEV") %>%
  summarise(
    na_count = sum(is.na(engine_size)),
    x_code_count = sum(engine_size == "[x]", na.rm = TRUE),
    total_missing = sum(is.na(engine_size) | engine_size == "[x]", na.rm = TRUE)
  )

print(join_final_complete_clean_BEV)

# ============================================================================
# FENCE 3 — Save retirement, fuel shares, NATCOP, base fleet
# ============================================================================
saveRDS(retirement_avg,      file.path(out_dir, "retirement_avg.rds"))
saveRDS(fleet_2024_complete, file.path(out_dir, "fleet_2024_complete.rds"))
saveRDS(fleet_complete,      file.path(out_dir, "fleet_complete.rds"))
saveRDS(fuel_shares_complete, file.path(out_dir, "fuel_shares_complete.rds"))
saveRDS(weight_model,        file.path(out_dir, "weight_model.rds"))
saveRDS(growth_rates,        file.path(out_dir, "growth_rates.rds"))
saveRDS(bev_country_2024,    file.path(out_dir, "bev_country_2024.rds"))

cat("\n=== FENCE 3: retirement/fuel/base objects saved ===\n")
