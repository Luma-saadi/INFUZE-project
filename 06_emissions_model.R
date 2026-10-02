# ============================================================================
# 06_emissions_model.R
# ----------------------------------------------------------------------------
# Real-data-anchored emissions model with reconciliation checks, size
# multipliers, and T&E / ICCT real-world adjustments for HEV and PHEV.
# Reconciles the sales-weighted average to Greg's original 130 g/km
# blended new-car target so no favourable bias is introduced.
#
# Depends on: 00_setup.R, 01–05
# Produces  : real_new_car_anchor, historical_co2_by_buildyear,
#             size_multiplier_from_data, fuel_2030_target,
#             forward_co2_by_buildyear, base_gkm_by_buildyear,
#             full_lookup, full_lookup_avg, annual_km_lookup,
#             calculate_emissions_size_fuel_fast(),
#             all_car_emissions_revised, all_car_emissions_original,
#             all_car_emissions_revised (paper table)
# ============================================================================

library(dplyr); library(tidyr); library(zoo); library(ggplot2)

# ============================================================================
# CONSTANTS — every non-measured number here has a citation
# ============================================================================
HEV_REALWORLD_UPLIFT <- 1.15   # ICCT (2024): 14-20% real-world gap for ICE+HEV, midpoint used
PHEV_REALWORLD_GKM   <- 139    # T&E (2025) "Smoke Screen" / EC (2024) OBFCM data
                               # PHEV average; held constant (no age/size trajectory: no data exists)

# CCC surface-transport carbon budget (MtCO2), 2024-2035
ccc_surface_residual <- data.frame(
  Year = 2024:2035,
  CCC_Surface_CO2_Mt = c(98, 92, 87, 81, 76, 69, 62, 55, 49, 43, 38, 32)
)

# Greg's original blended new-car target (unchanged) — used ONLY as the
# reconciliation anchor for the 2030 endpoint below, not applied per-fuel directly
blended_target_2030 <- 130

# Relative multipliers at the 2030 endpoint — small, literature-anchored differences
relative_multiplier <- tribble(
  ~Fuel_Group, ~multiplier,
  "Diesel", 1.03,   # EC (2024): diesel real-world runs slightly above petrol
  "Petrol", 0.98,
  "HEV",    0.92    # modest assumed benefit vs ICE — explicitly a judgement call, flagged in text
)

# ----------------------------------------------------------------------------
# HELPER — single source of truth for size-class extraction
# ----------------------------------------------------------------------------
extract_size_class <- function(vehicle_class_vec) {
  case_when(
    grepl("^Small", vehicle_class_vec)      ~ "Small",
    grepl("^Medium", vehicle_class_vec)     ~ "Medium",
    grepl("^Very Large", vehicle_class_vec) ~ "Very Large",
    grepl("^Large", vehicle_class_vec)      ~ "Large",
    TRUE ~ NA_character_
  )
}

# ============================================================================
# STEP 1: REAL 2024 NEW-CAR ANCHOR (Build_Year == 2024)
# ============================================================================
real_new_car_anchor <- results$fleet[["2024"]] %>%
  filter(Fuel_Group %in% c("Petrol","Diesel","HEV"), !is.na(Avg_CO2), Vehicle_Age <= 10) %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(CO2 = weighted.mean(Avg_CO2, Fleet, na.rm = TRUE),
            N   = sum(Fleet, na.rm = TRUE), .groups = "drop") %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age, .by_group = TRUE)

cat("\n=== CO2 BY AGE, 3-10 YEARS (MOT-measured window) ===\n")
print(real_new_car_anchor, n = Inf)

# Fit a simple linear trend on the youngest available ages (3-6) and
# extrapolate back to age 0 — same style of extrapolation used elsewhere
extrapolate_to_age0 <- function(df) {
  fit_data <- df %>% filter(Vehicle_Age <= 6)
  if (nrow(fit_data) < 2) return(df$CO2[which.min(df$Vehicle_Age)])
  model <- lm(CO2 ~ Vehicle_Age, data = fit_data, weights = N)
  predict(model, newdata = data.frame(Vehicle_Age = 0))
}

real_2024_anchor <- real_new_car_anchor %>%
  group_by(Fuel_Group) %>%
  group_modify(~ tibble(CO2_2024_raw = extrapolate_to_age0(.x))) %>%
  ungroup() %>%
  mutate(
    Anchor_Method = "Extrapolated to age 0 from ages 3-6 (no MOT data below age 3)",
    CO2_2024 = ifelse(Fuel_Group == "HEV", CO2_2024_raw * HEV_REALWORLD_UPLIFT, CO2_2024_raw)
  )

cat("\n=== REAL 2024 NEW-CAR ANCHOR (extrapolated to age 0, HEV real-world corrected) ===\n")
print(real_2024_anchor)

# Plausibility guard: new cars shouldn't be extrapolated ABOVE the age-3 figure
real_2024_anchor <- real_2024_anchor %>%
  left_join(real_new_car_anchor %>% filter(Vehicle_Age == 3) %>% select(Fuel_Group, CO2_age3 = CO2),
            by = "Fuel_Group") %>%
  mutate(
    CO2_2024_raw = pmin(CO2_2024_raw, CO2_age3, na.rm = TRUE),
    CO2_2024 = ifelse(Fuel_Group == "HEV", CO2_2024_raw * HEV_REALWORLD_UPLIFT, CO2_2024_raw)
  ) %>%
  select(-CO2_age3)

# ============================================================================
# STEP 2: FULL-RESOLUTION HISTORICAL g/km BY BUILD YEAR
# ============================================================================
historical_measured <- results$fleet[["2024"]] %>%
  filter(Fuel_Group %in% c("Petrol","Diesel","HEV"), !is.na(Avg_CO2)) %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(Real_CO2 = weighted.mean(Avg_CO2, Fleet, na.rm = TRUE), .groups = "drop") %>%
  mutate(Build_Year = 2024 - Vehicle_Age)

cat("=== RAW MOT-MEASURED BUILD-YEAR RANGE (before any filling) ===\n")
print(range(historical_measured$Build_Year))

bridge_point <- real_2024_anchor %>%
  transmute(Fuel_Group, Build_Year = 2024, Real_CO2 = CO2_2024_raw)

historical_co2_by_buildyear <- bind_rows(historical_measured, bridge_point) %>%
  group_by(Fuel_Group) %>%
  tidyr::complete(Build_Year = min(historical_measured$Build_Year):2024) %>%
  arrange(Build_Year, .by_group = TRUE) %>%
  mutate(
    Real_CO2 = zoo::na.approx(Real_CO2, x = Build_Year, na.rm = FALSE),
    Real_CO2 = zoo::na.locf(Real_CO2, na.rm = FALSE),
    Real_CO2 = zoo::na.locf(Real_CO2, fromLast = TRUE, na.rm = FALSE),
    Real_CO2 = ifelse(Fuel_Group == "HEV", Real_CO2 * HEV_REALWORLD_UPLIFT, Real_CO2)
  ) %>%
  ungroup() %>%
  select(Fuel_Group, Build_Year, g_per_km = Real_CO2)

cat("\n=== GAP CHECK: Build_Year 2021-2025, all three fuels ===\n")
print(historical_co2_by_buildyear %>%
        filter(Build_Year %in% 2021:2024) %>%
        arrange(Fuel_Group, Build_Year))

# ============================================================================
# STEP 3: SIZE MULTIPLIERS FROM REAL 2024 FLEET DATA
# ============================================================================
paper_emissions_table <- results$fleet[["2024"]] %>%
  filter(Fuel_Group %in% c("Petrol","Diesel","HEV","PHEV","BEV"), !is.na(Avg_CO2)) %>%
  mutate(vehicle_class_clean = extract_size_class(vehicle_class)) %>%
  filter(!is.na(vehicle_class_clean)) %>%
  group_by(vehicle_class_clean, Fuel_Group) %>%
  summarise(
    Avg_CO2_gkm = weighted.mean(Avg_CO2, Fleet, na.rm = TRUE),
    N_Vehicles  = sum(Fleet, na.rm = TRUE),
    .groups = "drop"
  )

# Merge the thin Large/HEV cell into Very Large/HEV before using it
size_multiplier_from_data <- paper_emissions_table %>%
  mutate(vehicle_class_clean = ifelse(
    Fuel_Group == "HEV" & vehicle_class_clean == "Large", "Very Large", vehicle_class_clean
  )) %>%
  group_by(vehicle_class_clean, Fuel_Group) %>%
  summarise(
    Avg_CO2_gkm = weighted.mean(Avg_CO2_gkm, N_Vehicles, na.rm = TRUE),
    N_Vehicles  = sum(N_Vehicles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Fuel_Group) %>%
  mutate(
    fuel_avg_CO2    = weighted.mean(Avg_CO2_gkm, N_Vehicles, na.rm = TRUE),
    size_multiplier = Avg_CO2_gkm / fuel_avg_CO2
  ) %>%
  ungroup() %>%
  select(Fuel_Group, vehicle_class = vehicle_class_clean, size_multiplier)

cat("\n=== SIZE MULTIPLIERS (thin Large/HEV cell merged into Very Large) ===\n")
print(size_multiplier_from_data, n = Inf)

# ============================================================================
# STEP 4: RECONCILED 2030 ENDPOINT
# ============================================================================
shares_2030 <- fuel_shares_complete %>%
  filter(Year == 2030, Fuel_Group %in% c("Petrol","Diesel","HEV","PHEV")) %>%
  mutate(sale_percentage = sale_percentage / sum(sale_percentage))

w_phev <- shares_2030$sale_percentage[shares_2030$Fuel_Group == "PHEV"]

scaled_shares <- shares_2030 %>%
  filter(Fuel_Group != "PHEV") %>%
  left_join(relative_multiplier, by = "Fuel_Group")

k <- (blended_target_2030 - w_phev * PHEV_REALWORLD_GKM) /
  sum(scaled_shares$sale_percentage * scaled_shares$multiplier)

fuel_2030_target <- scaled_shares %>%
  mutate(CO2_2030 = multiplier * k) %>%
  select(Fuel_Group, CO2_2030) %>%
  bind_rows(tibble(Fuel_Group = "PHEV", CO2_2030 = PHEV_REALWORLD_GKM))

cat("\n=== RECONCILED 2030 TARGETS BY FUEL ===\n")
print(fuel_2030_target)

check_2030 <- shares_2030 %>%
  left_join(fuel_2030_target, by = "Fuel_Group") %>%
  summarise(weighted_avg = weighted.mean(CO2_2030, sale_percentage))
cat("\n=== RECONCILIATION CHECK (must equal 130.0) ===\n")
print(check_2030)
stopifnot(abs(check_2030$weighted_avg - blended_target_2030) < 0.01)

# ============================================================================
# STEP 5: FORWARD TRAJECTORY (Build_Year 2025-2037)
# ============================================================================
forward_co2_by_buildyear <- real_2024_anchor %>%
  left_join(fuel_2030_target %>% filter(Fuel_Group != "PHEV"), by = "Fuel_Group") %>%
  tidyr::crossing(Build_Year = 2025:2037) %>%
  mutate(g_per_km = ifelse(
    Build_Year <= 2030,
    CO2_2024 + (CO2_2030 - CO2_2024) * (Build_Year - 2024) / 6,
    CO2_2030
  )) %>%
  select(Fuel_Group, Build_Year, g_per_km)
print(forward_co2_by_buildyear)

# ============================================================================
# STEP 6: COMBINE HISTORICAL + FORWARD
# ============================================================================
base_gkm_by_buildyear <- bind_rows(historical_co2_by_buildyear, forward_co2_by_buildyear)

cat("\n=== SAMPLE: HEV g/km BY BUILD YEAR ===\n")
print(base_gkm_by_buildyear %>% filter(Fuel_Group == "HEV") %>%
        filter(Build_Year %in% c(2000, 2010, 2020, 2024, 2025, 2030, 2037)))

# ============================================================================
# STEP 7: LOOKUP FUNCTION
# ============================================================================
create_emissions_lookup <- function(build_year, fuel, target_class) {
  if (fuel == "PHEV") {
    base_gkm <- PHEV_REALWORLD_GKM
  } else if (fuel == "BEV") {
    return(0)
  } else {
    clamped_year <- pmin(pmax(build_year, min(base_gkm_by_buildyear$Build_Year)),
                         max(base_gkm_by_buildyear$Build_Year))
    base_gkm <- base_gkm_by_buildyear %>%
      filter(Fuel_Group == fuel, Build_Year == clamped_year) %>%
      pull(g_per_km)
    if (length(base_gkm) == 0) base_gkm <- ifelse(build_year < 2025, 150, 130)
  }

  size_mult <- size_multiplier_from_data %>%
    filter(Fuel_Group == fuel, vehicle_class == target_class) %>%
    pull(size_multiplier)
  if (length(size_mult) == 0) size_mult <- 1.0

  base_gkm * size_mult
}

cat("\n=== TEST CASES ===\n")
test_cases <- tibble(
  Build_Year = c(2010, 2024, 2030, 2035, 2035),
  Fuel       = c("Petrol", "HEV", "Diesel", "PHEV", "Petrol"),
  Class      = c("Small", "Very Large", "Medium", "Large", "Very Large")
)
test_cases %>% rowwise() %>%
  mutate(g_per_km = create_emissions_lookup(Build_Year, Fuel, Class)) %>%
  ungroup() %>% print()

# ============================================================================
# STEP 8: EMISSIONS CALCULATION (SLOW rowwise — reference only)
# ============================================================================
calculate_emissions_size_fuel <- function(fleet_data, scenario_name) {
  fleet_data %>%
    filter(Fuel_Group %in% c("Petrol","Diesel","HEV","PHEV")) %>%
    mutate(
      Build_Year = Year - Vehicle_Age,
      vehicle_class_clean = coalesce(extract_size_class(vehicle_class), "Medium")
    ) %>%
    rowwise() %>%
    mutate(
      g_per_km = create_emissions_lookup(Build_Year, Fuel_Group, vehicle_class_clean),
      annual_km = 11000,
      emissions_Mt = Fleet * g_per_km * annual_km / 1e12
    ) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(Car_CO2_Mt = sum(emissions_Mt, na.rm = TRUE), .groups = "drop") %>%
    mutate(Scenario = scenario_name)
}

# ============================================================================
# STEP 8b: FAST vectorized version (this is what actually runs)
# ============================================================================
min_by <- min(base_gkm_by_buildyear$Build_Year)
max_by <- max(base_gkm_by_buildyear$Build_Year)

phev_lookup <- base_gkm_by_buildyear %>%
  distinct(Build_Year) %>%
  mutate(Fuel_Group = "PHEV", g_per_km = PHEV_REALWORLD_GKM)

full_lookup_fuelyear <- bind_rows(base_gkm_by_buildyear, phev_lookup)

full_lookup <- full_lookup_fuelyear %>%
  tidyr::crossing(vehicle_class = unique(size_multiplier_from_data$vehicle_class)) %>%
  left_join(size_multiplier_from_data, by = c("Fuel_Group", "vehicle_class")) %>%
  mutate(size_multiplier = coalesce(size_multiplier, 1.0),
         g_per_km = g_per_km * size_multiplier) %>%
  select(Fuel_Group, Build_Year, vehicle_class, g_per_km)

dup_check <- full_lookup %>% count(Fuel_Group, Build_Year, vehicle_class) %>% filter(n > 1)
stopifnot(nrow(dup_check) == 0)

full_lookup_avg <- full_lookup %>%
  group_by(Fuel_Group, Build_Year) %>%
  summarise(g_per_km_avg = mean(g_per_km, na.rm = TRUE), .groups = "drop")

calculate_emissions_size_fuel_fast <- function(fleet_data, scenario_name) {
  fleet_data %>%
    filter(Fuel_Group %in% c("Petrol","Diesel","HEV","PHEV")) %>%
    mutate(
      Build_Year_clamped = pmin(pmax(Build_Year, min_by), max_by),
      vehicle_class_clean = coalesce(extract_size_class(vehicle_class), "Medium")
    ) %>%
    left_join(full_lookup, by = c("Fuel_Group","Build_Year_clamped"="Build_Year","vehicle_class_clean"="vehicle_class")) %>%
    left_join(full_lookup_avg, by = c("Fuel_Group","Build_Year_clamped"="Build_Year")) %>%
    mutate(
      g_per_km = coalesce(g_per_km, g_per_km_avg, ifelse(Build_Year < 2025, 150, 130)),
      annual_km_final = coalesce(Final_Mileage, Predicted_Mileage, 11000),
      emissions_Mt = Fleet * g_per_km * annual_km_final / 1e12
    ) %>%
    group_by(Year) %>%
    summarise(Car_CO2_Mt = sum(emissions_Mt, na.rm = TRUE), .groups = "drop") %>%
    mutate(Scenario = scenario_name)
}

all_possible_build_years <- (2024 - 30):2037
coverage_gaps <- expand.grid(Fuel_Group = c("Petrol","Diesel","HEV"), Build_Year = all_possible_build_years) %>%
  anti_join(base_gkm_by_buildyear, by = c("Fuel_Group", "Build_Year"))

cat("\n=== ANY REMAINING BUILD-YEAR GAPS? (should be empty) ===\n")
print(coverage_gaps)

# ============================================================================
# STEP 9: SIZE-CLASS FALLBACK CHECK
# ============================================================================
all_baseline_fleet <- bind_rows(results_baseline$fleet)

fallback_by_year <- all_baseline_fleet %>%
  mutate(vehicle_class_clean = extract_size_class(vehicle_class)) %>%
  group_by(Year) %>%
  summarise(
    Total_Fleet   = sum(Fleet, na.rm = TRUE),
    Missing_Class = sum(Fleet[is.na(vehicle_class_clean)], na.rm = TRUE),
    Pct_Missing   = round(100 * Missing_Class / Total_Fleet, 2),
    .groups = "drop"
  )
cat("\n=== 'MEDIUM' FALLBACK SHARE BY YEAR ===\n")
print(fallback_by_year, n = Inf)

# ============================================================================
# STEP 10: RUN ALL FOUR SCENARIOS
# ============================================================================
fleet_for_emissions <- bind_rows(results_baseline$fleet) %>%
  group_by(Year, Vehicle_Age, Fuel_Group, vehicle_class) %>%
  summarise(Fleet = sum(Fleet, na.rm = TRUE), .groups = "drop")

cat("Rows before collapsing:", nrow(bind_rows(results_baseline$fleet)), "\n")
cat("Rows after collapsing: ", nrow(fleet_for_emissions), "\n")

cat("\n=== VERIFYING FAST VERSION AGAINST SLOW (collapsed baseline, one-time check) ===\n")
system.time({
  slow_check <- calculate_emissions_size_fuel(fleet_for_emissions, "Baseline")
})
fast_check <- calculate_emissions_size_fuel_fast(fleet_for_emissions, "Baseline")

verification <- slow_check %>% rename(Slow = Car_CO2_Mt) %>%
  left_join(fast_check %>% rename(Fast = Car_CO2_Mt), by = "Year") %>%
  mutate(diff = round(Slow - Fast, 6))
print(verification, n = Inf)
stopifnot(all(abs(verification$diff) < 1e-6))
cat("Verification passed.\n")

collapse_for_emissions <- function(results_obj) {
  bind_rows(results_obj$fleet) %>%
    group_by(Year, Vehicle_Age, Fuel_Group, vehicle_class) %>%
    summarise(Fleet = sum(Fleet, na.rm = TRUE), .groups = "drop")
}

system.time({
  em_baseline_revised <- calculate_emissions_size_fuel_fast(collapse_for_emissions(results_baseline), "Baseline")
  em_70_revised       <- calculate_emissions_size_fuel_fast(collapse_for_emissions(results_70pct), "70% by 2030")
  em_60_revised       <- calculate_emissions_size_fuel_fast(collapse_for_emissions(results_60pct), "60% by 2030")
  em_s4_revised       <- calculate_emissions_size_fuel_fast(collapse_for_emissions(results_scenario4), "Scenario 4")
}) %>% print()

all_car_emissions_revised <- bind_rows(em_baseline_revised, em_70_revised, em_60_revised, em_s4_revised)

cat("\n=== REVISED EMISSIONS - 2035 ===\n")
print(all_car_emissions_revised %>% filter(Year == 2035))

# ============================================================================
# STEP 9b: ORIGINAL (uncorrected) VERSION - for comparison
# ============================================================================
emissions_new_cars_original <- data.frame(
  Year = 2027:2035,
  g_per_km = c(147, 145, 139, 130, 130, 130, 130, 130, 130)
)

get_emissions_factor_original <- function(build_year, fuel) {
  if (fuel %in% c("Petrol","Diesel","HEV","PHEV")) {
    val <- emissions_new_cars_original$g_per_km[emissions_new_cars_original$Year == build_year]
    if (length(val) > 0) return(val)
    if (build_year < 2027) return(150)
    return(130)
  }
  return(0)
}

calculate_emissions_original <- function(fleet_data, scenario_name) {
  fleet_data %>%
    filter(Fuel_Group %in% c("Petrol","Diesel","HEV","PHEV")) %>%
    mutate(Build_Year = Year - Vehicle_Age) %>%
    rowwise() %>%
    mutate(g_per_km = get_emissions_factor_original(Build_Year, Fuel_Group),
           emissions_Mt = Fleet * g_per_km * 11000 / 1e12) %>%
    ungroup() %>%
    group_by(Year) %>%
    summarise(Car_CO2_Mt = sum(emissions_Mt, na.rm = TRUE), .groups = "drop") %>%
    mutate(Scenario = scenario_name)
}

all_car_emissions_original <- bind_rows(
  calculate_emissions_original(collapse_for_emissions(results_baseline), "Baseline"),
  calculate_emissions_original(collapse_for_emissions(results_70pct), "70% by 2030"),
  calculate_emissions_original(collapse_for_emissions(results_60pct), "60% by 2030"),
  calculate_emissions_original(collapse_for_emissions(results_scenario4), "Scenario 4")
)

all_car_emissions_revised <- bind_rows(em_baseline_revised, em_70_revised, em_60_revised, em_s4_revised)

cat("\n=== REVISED EMISSIONS - 2035 ===\n")
print(all_car_emissions_revised %>% filter(Year == 2035))

# ============================================================================
# STEP 11: COMPARE WITH CCC TARGET
# ============================================================================
ccc_car_target_2035 <- 19.3

comparison_full <- all_car_emissions_revised %>%
  left_join(ccc_surface_residual, by = "Year") %>%
  mutate(Car_Share_of_Target_pct = round(100 * Car_CO2_Mt / CCC_Surface_CO2_Mt, 1))

cat("\n=== EMISSIONS VS CCC TARGET, ALL SCENARIOS ===\n")
print(comparison_full %>% arrange(Scenario, Year), n = Inf)

all_car_emissions_original %>%
  filter(Year == 2035) %>%
  mutate(Overshoot = Car_CO2_Mt - ccc_car_target_2035,
         Pct_Overshoot = round(100 * Overshoot / ccc_car_target_2035, 1)) %>%
  print()

# ============================================================================
# STEP 10b: COMPARISON
# ============================================================================
comparison_2035 <- all_car_emissions_original %>%
  filter(Year == 2035) %>% rename(Original = Car_CO2_Mt) %>%
  left_join(all_car_emissions_revised %>% filter(Year == 2035) %>% rename(Revised = Car_CO2_Mt),
            by = "Scenario") %>%
  mutate(Difference = Revised - Original, Pct_Change = round(100 * Difference / Original, 1))

cat("\n=== ORIGINAL VS REVISED - 2035 ===\n")
print(comparison_2035)

comparison_full <- all_car_emissions_revised %>%
  left_join(ccc_surface_residual, by = "Year") %>%
  mutate(Car_Share_of_Target_pct = round(100 * Car_CO2_Mt / CCC_Surface_CO2_Mt, 1))
cat("\n=== REVISED VS CCC TARGET ===\n")
print(comparison_full %>% arrange(Scenario, Year), n = Inf)

# ============================================================================
# STEP 11b: VISUALISATION
# ============================================================================
comparison_plot_data <- all_car_emissions_original %>% rename(Original = Car_CO2_Mt) %>%
  left_join(all_car_emissions_revised %>% rename(Revised = Car_CO2_Mt), by = c("Year","Scenario")) %>%
  filter(Year >= 2025) %>%
  pivot_longer(cols = c(Original, Revised), names_to = "Version", values_to = "Emissions")

p_comparison_revised <- ggplot(comparison_plot_data, aes(x = Year, y = Emissions, color = Version, linetype = Scenario)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 19.3, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 2035, y = 20.5, label = "CCC Car Target (19.3 Mt)", color = "red", hjust = 1, fontface = "bold") +
  scale_color_manual(values = c("Original" = "#2E86AB", "Revised" = "#C73E1D")) +
  labs(title = "Original vs Revised Emissions - Real-Data-Anchored, Reconciled",
       subtitle = "Revised: real 2024 MOT data + size multiplier + T&E/ICCT-corrected HEV/PHEV, reconciled to blended target",
       x = "Year", y = "CO2 Emissions (Mt)", color = "Method", linetype = "Scenario") +
  theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 14))
print(p_comparison_revised)

# ============================================================================
# STEP 12: PAPER SUMMARY TABLE + EXPORT
# ============================================================================
emissions_table_paper <- all_car_emissions_revised %>%
  filter(Year %in% c(2025, 2030, 2035)) %>%
  pivot_wider(id_cols = Scenario, names_from = Year, values_from = Car_CO2_Mt, names_prefix = "Emissions_") %>%
  left_join(
    all_car_emissions_original %>% filter(Year %in% c(2025, 2030, 2035)) %>%
      pivot_wider(id_cols = Scenario, names_from = Year, values_from = Car_CO2_Mt, names_prefix = "Original_"),
    by = "Scenario"
  ) %>%
  mutate(
    Diff_2035 = Emissions_2035 - Original_2035,
    Pct_Change_2035 = round(100 * Diff_2035 / Original_2035, 1),
    Meets_Target = ifelse(Emissions_2035 <= 19.3, "Yes", "No")
  )

cat("\n=== FINAL PAPER TABLE ===\n")
print(emissions_table_paper)

cat("\n=== EXPORTED: 5 CSV files ===\n")
