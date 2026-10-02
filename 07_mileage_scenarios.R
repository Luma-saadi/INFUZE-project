# ============================================================================
# 07_mileage_scenarios.R
# ----------------------------------------------------------------------------
# VKT-conserving mileage reallocation scenarios. Reallocates total annual
# vehicle-km across the fleet (by age, by fuel) so that the total VKT is
# held constant across scenarios. Compares four tilt schemes:
#   1. Flat baseline (constant per-vehicle mileage)
#   2. Age-specific mileage (empirical profile)
#   3. Exponential decay mileage (gamma = 0.05)
#   4. EV road pricing (delta = 0.20)
#
# Depends on: 00–06
# Produces  : FLAT_KM, base_emp, base_flat, calib_k, headline,
#             range_young, range_road, traj, mileage scenario figures
# ============================================================================

library(dplyr); library(tidyr); library(ggplot2); library(zoo)

FLAT_KM <- annual_km_lookup %>%
  left_join(
    paper_emissions_table %>%
      filter(Fuel_Group != "BEV") %>%
      select(Fuel_Group, vehicle_class = vehicle_class_clean, N_Vehicles),
    by = c("Fuel_Group", "vehicle_class")
  ) %>%
  summarise(weighted_avg = weighted.mean(annual_km, N_Vehicles, na.rm = TRUE)) %>%
  pull(weighted_avg)

if(length(FLAT_KM) == 0 || is.na(FLAT_KM)) {
  FLAT_KM <- 11000
}

TARGET_YEAR       <- 2035
CCC_TARGET        <- ccc_car_allocation_2035

# Keep the original emissions results under a different name
baseline_emissions_results <- all_car_emissions_revised

BASELINE_HEADLINE <- baseline_emissions_results$Car_CO2_Mt[
  baseline_emissions_results$Year == 2035 &
    baseline_emissions_results$Scenario == "Baseline"
]

GAMMA_PRIMARY <- 0.05;  GAMMA_RANGE <- c(0.02, 0.05, 0.08)
DELTA_PRIMARY <- 0.20;  DELTA_RANGE <- c(0.10, 0.20, 0.30)

# ============================================================================
# 1. Complete base per-vehicle mileage profile m0(age, fuel)
# ============================================================================
build_base_mileage <- function(decay_tbl, max_age = 30) {
  fuels <- c("Petrol", "Diesel", "HEV", "PHEV", "BEV")
  grid <- expand.grid(Vehicle_Age = 0:max_age, Fuel_Group = fuels,
                      stringsAsFactors = FALSE)
  base <- grid %>%
    left_join(decay_tbl %>%
                select(Fuel_Group, Vehicle_Age, Base_Mileage = Avg_Mileage),
              by = c("Fuel_Group", "Vehicle_Age")) %>%
    group_by(Fuel_Group) %>% arrange(Vehicle_Age, .by_group = TRUE) %>%
    mutate(
      Base_Mileage = zoo::na.approx(Base_Mileage, x = Vehicle_Age, na.rm = FALSE),
      Base_Mileage = zoo::na.locf(Base_Mileage, na.rm = FALSE),
      Base_Mileage = zoo::na.locf(Base_Mileage, fromLast = TRUE, na.rm = FALSE)
    ) %>% ungroup()
  petrol <- base %>% filter(Fuel_Group == "Petrol") %>%
    select(Vehicle_Age, Petrol_M = Base_Mileage)
  base %>% left_join(petrol, by = "Vehicle_Age") %>%
    mutate(Base_Mileage = ifelse(is.na(Base_Mileage), Petrol_M, Base_Mileage)) %>%
    select(Vehicle_Age, Fuel_Group, Base_Mileage)
}

# ============================================================================
# 2. VKT-conserving reallocation
# ============================================================================
reallocate_mileage <- function(fleet_year, base_tbl, flat_km = FLAT_KM,
                               age_mult = function(a) rep(1, length(a)),
                               fuel_mult = function(f) rep(1, length(f))) {
  fleet_year %>%
    left_join(base_tbl, by = c("Vehicle_Age", "Fuel_Group")) %>%
    mutate(
      Base_Mileage = ifelse(is.na(Base_Mileage), flat_km, Base_Mileage),
      raw_m = Base_Mileage * age_mult(Vehicle_Age) * fuel_mult(Fuel_Group)
    ) %>%
    group_by(Year) %>%
    mutate(
      vkt_target = sum(Fleet * flat_km, na.rm = TRUE),
      lambda = vkt_target / sum(Fleet * raw_m, na.rm = TRUE),
      Mileage_scen = raw_m * lambda
    ) %>% ungroup()
}

# ============================================================================
# 3. Vectorised emissions factor using reconciled fuel-specific data
# ============================================================================
emis_lookup_full <- base_gkm_by_buildyear %>%
  tidyr::complete(Fuel_Group, Build_Year = min(Build_Year):max(Build_Year)) %>%
  bind_rows(
    data.frame(
      Fuel_Group = "PHEV",
      Build_Year = min(base_gkm_by_buildyear$Build_Year):max(base_gkm_by_buildyear$Build_Year),
      g_per_km = PHEV_REALWORLD_GKM
    )
  )

min_by_lookup <- min(emis_lookup_full$Build_Year)
max_by_lookup <- max(emis_lookup_full$Build_Year)

emis_factor_join <- function(fleet_with_buildyear) {
  fleet_with_buildyear %>%
    mutate(Build_Year_clamped = pmin(pmax(Build_Year, min_by_lookup), max_by_lookup)) %>%
    left_join(emis_lookup_full, by = c("Fuel_Group", "Build_Year_clamped" = "Build_Year")) %>%
    mutate(
      g = ifelse(
        Fuel_Group == "BEV", 0,
        coalesce(g_per_km, ifelse(Build_Year < 2027, 150, 130))
      )
    )
}

# ============================================================================
# 4. Collapse fleet first
# ============================================================================
fleet_all_base <- bind_rows(results_baseline$fleet) %>%
  group_by(Year, Vehicle_Age, Fuel_Group) %>%
  summarise(Fleet = sum(Fleet, na.rm = TRUE), .groups = "drop")

cat("Rows before collapsing:", nrow(bind_rows(results_baseline$fleet)), "\n")
cat("Rows after collapsing: ", nrow(fleet_all_base), "\n")

# ============================================================================
# 5. scenario_emissions_fast
# ============================================================================
scenario_emissions_fast <- function(fleet_all, base_tbl, flat_km = FLAT_KM,
                                    age_mult = NULL, fuel_mult = NULL, calib = 1,
                                    years = 2025:2035) {
  am <- if (is.null(age_mult)) function(a) rep(1, length(a)) else age_mult
  fm <- if (is.null(fuel_mult)) function(f) rep(1, length(f)) else fuel_mult

  fleet_all %>%
    filter(Year %in% years) %>%
    reallocate_mileage(base_tbl, flat_km, am, fm) %>%
    mutate(Build_Year = Year - Vehicle_Age) %>%
    emis_factor_join() %>%
    mutate(emis_Mt = Fleet * g * Mileage_scen / 1e12 * calib) %>%
    group_by(Year) %>%
    summarise(Emissions_Mt = sum(emis_Mt, na.rm = TRUE), .groups = "drop")
}

scenario_emissions_single <- function(fleet_all, base_tbl, year = TARGET_YEAR,
                                      flat_km = FLAT_KM, age_mult = NULL,
                                      fuel_mult = NULL, calib = 1) {
  am <- if (is.null(age_mult)) function(a) rep(1, length(a)) else age_mult
  fm <- if (is.null(fuel_mult)) function(f) rep(1, length(f)) else fuel_mult

  fleet_all %>%
    filter(Year == year) %>%
    reallocate_mileage(base_tbl, flat_km, am, fm) %>%
    mutate(Build_Year = Year - Vehicle_Age) %>%
    emis_factor_join() %>%
    mutate(emis_Mt = Fleet * g * Mileage_scen / 1e12 * calib) %>%
    summarise(Emissions_Mt = sum(emis_Mt, na.rm = TRUE)) %>%
    pull(Emissions_Mt)
}

# ============================================================================
# 5b. Base tables + calibration
# ============================================================================
base_emp <- build_base_mileage(mileage_decay_raw)
base_flat <- base_emp %>% mutate(Base_Mileage = FLAT_KM)

young_skew <- function(gamma) function(a) exp(-gamma * a)
road_price <- function(delta) function(f)
  ifelse(f %in% c("Petrol", "Diesel"), 1 + delta,
         ifelse(f == "BEV", 1 - delta, 1))

raw_flat <- scenario_emissions_single(fleet_all_base, base_flat, TARGET_YEAR)
calib_k <- BASELINE_HEADLINE / raw_flat
cat(sprintf("\nTRUE flat 2035 raw = %.2f Mt  ->  calib_k = %.4f\n", raw_flat, calib_k))

# ============================================================================
# 6. Four headline scenarios
# ============================================================================
headline_data <- list(
  flat = scenario_emissions_fast(fleet_all_base, base_flat, calib = calib_k, years = TARGET_YEAR),
  emp  = scenario_emissions_fast(fleet_all_base, base_emp,  calib = calib_k, years = TARGET_YEAR),
  exp  = scenario_emissions_fast(fleet_all_base, base_emp,  calib = calib_k,
                                 age_mult = young_skew(GAMMA_PRIMARY), years = TARGET_YEAR),
  road = scenario_emissions_fast(fleet_all_base, base_emp,  calib = calib_k,
                                 fuel_mult = road_price(DELTA_PRIMARY), years = TARGET_YEAR)
)

headline <- tibble(
  Scenario = factor(
    c("1. Flat baseline", "2. Age-specific mileage",
      "3. Exponential decay mileage(gamma=0.05)", "4. EV road pricing (delta=0.20)"),
    levels = c("1. Flat baseline", "2. Age-specific mileage",
               "3. Exponential decay mileage(gamma=0.05)", "4. EV road pricing (delta=0.20)")
  ),
  Emissions_Mt = c(
    headline_data$flat$Emissions_Mt,
    headline_data$emp$Emissions_Mt,
    headline_data$exp$Emissions_Mt,
    headline_data$road$Emissions_Mt
  )
) %>% mutate(
  Meets_CCC = ifelse(Emissions_Mt <= CCC_TARGET, "Yes", "No"),
  vs_Flat = round(Emissions_Mt - Emissions_Mt[1], 1),
  pct_vs_Flat = round(100 * (Emissions_Mt / Emissions_Mt[1] - 1), 1)
)

cat("\n=== HEADLINE 2035 SCENARIOS (VKT conserved) ===\n")
print(headline)

# ============================================================================
# 7. Illustrative ranges
# ============================================================================
range_young <- purrr::map_dfr(GAMMA_RANGE, function(g) {
  scenario_emissions_fast(fleet_all_base, base_emp, calib = calib_k,
                          age_mult = young_skew(g), years = TARGET_YEAR) %>%
    mutate(Lever = "Exponential decay mileage (gamma)", x = g)
})

range_road <- purrr::map_dfr(DELTA_RANGE, function(d) {
  scenario_emissions_fast(fleet_all_base, base_emp, calib = calib_k,
                          fuel_mult = road_price(d), years = TARGET_YEAR) %>%
    mutate(Lever = "EV road pricing (delta)", x = d)
})

cat("\n=== Exponential decay mileage RANGE ===\n")
print(range_young)
cat("\n=== ROAD-PRICING RANGE ===\n")
print(range_road)

# ============================================================================
# 8. Trajectory 2025-2035
# ============================================================================
traj_flat <- scenario_emissions_fast(fleet_all_base, base_flat, calib = calib_k) %>%
  mutate(Scenario = "Flat baseline")
traj_emp <- scenario_emissions_fast(fleet_all_base, base_emp, calib = calib_k) %>%
  mutate(Scenario = "Age-specific mileage")
traj_exp <- scenario_emissions_fast(fleet_all_base, base_emp, calib = calib_k,
                                    age_mult = young_skew(GAMMA_PRIMARY)) %>%
  mutate(Scenario = "Exponential decay mileage")
traj_road <- scenario_emissions_fast(fleet_all_base, base_emp, calib = calib_k,
                                     fuel_mult = road_price(DELTA_PRIMARY)) %>%
  mutate(Scenario = "EV road pricing")

traj <- bind_rows(traj_flat, traj_emp, traj_exp, traj_road)
print(traj, n = Inf)

traj$Scenario <- factor(traj$Scenario,
                        levels = c("Flat baseline", "Age-specific mileage",
                                   "Exponential decay mileage", "EV road pricing"))

# ============================================================================
# 9. FIGURES
# ============================================================================
scen_cols <- c("1. Flat baseline" = "#999999",
               "2. Age-specific mileage" = "#2E86AB",
               "3. Exponential decay mileage(gamma=0.05)" = "#4DAF4A",
               "4. EV road pricing (delta=0.20)" = "#C73E1D")

p_headline <- ggplot(headline, aes(Scenario, Emissions_Mt, fill = Scenario)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = CCC_TARGET, linetype = "dashed", colour = "red", linewidth = 1) +
  annotate("text", x = 0.6, y = CCC_TARGET + 0.8,
           label = paste0("CCC Car Target (", CCC_TARGET, " Mt)"),
           colour = "red", fontface = "bold", hjust = 0, size = 3.3) +
  geom_text(aes(label = round(Emissions_Mt, 1)), vjust = -0.5, fontface = "bold", size = 4) +
  scale_fill_manual(values = scen_cols) +
  labs(x = NULL, y = expression(CO[2]~"emissions (Mt, 2035)")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 12, hjust = 0.8),
        panel.grid.major.x = element_blank())
print(p_headline)

p_fan <- bind_rows(range_young, range_road) %>%
  ggplot(aes(factor(x), Emissions_Mt, fill = Lever)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = CCC_TARGET, linetype = "dashed", colour = "red", linewidth = 1) +
  geom_text(aes(label = round(Emissions_Mt, 1)), vjust = -0.5, size = 3.4) +
  facet_wrap(~Lever, scales = "free_x") +
  scale_fill_manual(values = c("Exponential decay mileage (gamma)" = "#4DAF4A",
                               "EV road pricing (delta)" = "#C73E1D")) +
  labs(x = "Tilt strength", y = expression(CO[2]~"emissions (Mt, 2035)")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))
print(p_fan)

p_traj <- ggplot(traj, aes(Year, Emissions_Mt, colour = Scenario)) +
  geom_line(linewidth = 1.1) + geom_point(size = 1.8) +
  geom_hline(yintercept = CCC_TARGET, linetype = "dashed", colour = "red") +
  scale_colour_manual(values = c("Flat baseline" = "#999999", "Age-specific mileage" = "#2E86AB",
                                 "Exponential decay mileage" = "#4DAF4A", "EV road pricing" = "#C73E1D")) +
  scale_x_continuous(breaks = 2025:2035) +
  labs(x = "Year", y = expression(CO[2]~"emissions (Mt)"), colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
print(p_traj)

p_traj_bar <- ggplot(traj, aes(factor(Year), Emissions_Mt, fill = Scenario)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(aes(label = round(Emissions_Mt, 1)),
            position = position_stack(vjust = 0.5),
            size = 3, colour = "black") +
  scale_fill_manual(
    values = c("Flat baseline" = "#999999",
               "Age-specific mileage" = "#2E86AB",
               "Exponential decay mileage" = "#4DAF4A",
               "EV road pricing" = "#C73E1D"),
    labels = c("Flat baseline",
               "Age-specific mileage",
               "Exponential decay mileage (gamma=0.05)",
               "EV road pricing (delta=0.20)")
  ) +
  labs(x = "Year", y = expression(CO[2]~"emissions (Mt)"), fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
print(p_traj_bar)

# ============================================================================
# CCC surface transport and car allocation
# ============================================================================
ccc_surface_residual <- data.frame(
  Year = 2024:2035,
  CCC_Surface_CO2_Mt = c(98, 92, 87, 81, 76, 69, 62, 55, 49, 43, 38, 32)
)

car_share <- 0.602
ccc_car <- ccc_surface_residual %>%
  mutate(CCC_Car_CO2_Mt = round(CCC_Surface_CO2_Mt * car_share, 1))

ccc_car_2035 <- ccc_car %>%
  filter(Year == 2035) %>%
  pull(CCC_Car_CO2_Mt)
cat("2035 Car Target:", ccc_car_2035, "MtC\n")

ccc_surface_cumulative <- ccc_surface_residual %>%
  filter(Year >= 2027, Year <= 2035) %>%
  summarise(total = sum(CCC_Surface_CO2_Mt)) %>%
  pull(total)
cat("Surface Transport Cumulative (2027-2035):",
    round(ccc_surface_cumulative, 1), "MtC\n")

ccc_car_cumulative <- ccc_car %>%
  filter(Year >= 2027, Year <= 2035) %>%
  summarise(total = sum(CCC_Car_CO2_Mt)) %>%
  pull(total)
cat("Car Cumulative (2027-2035):",
    round(ccc_car_cumulative, 1), "MtC\n")

# ============================================================================
# Sensitivity analysis - scenario grid
# ============================================================================
run_scenario <- function(growth_mult = 1, max_age = 30, zev_scenario = "baseline") {
  zev_scenario <- as.character(zev_scenario)

  fuel_shares_use <- switch(zev_scenario,
                            "baseline" = fuel_shares_baseline,
                            "mild"     = fuel_shares_70pct,
                            "medium"   = fuel_shares_60pct,
                            stop("Unknown ZEV scenario: ", zev_scenario))

  natcop_adj <- natcop_targets %>% mutate(Target_Fleet = Target_Fleet * growth_mult)
  fleet_base <- fleet_2024_complete %>% filter(Vehicle_Age <= max_age)
  retire_use <- retirement_avg %>% filter(Vehicle_Age <= max_age)

  res <- project_fleet(fleet_base,
                       retire_use,
                       fuel_shares_use,
                       natcop_adj,
                       engine_sales_distribution)

  fleet_2037 <- bind_rows(res$fleet) %>% filter(Year == 2037)

  total_sales <- sum(res$sales$Required_Sales, na.rm = TRUE)

  fleet_2037 %>%
    summarise(
      Fleet_2037 = sum(Fleet),
      Avg_Age_2037 = sum(Vehicle_Age * Fleet) / sum(Fleet),
      BEV_Share_2037 = sum(Fleet[Fuel_Group == "BEV"]) / sum(Fleet),
      Fossil_Fleet_2037 = sum(Fleet[Fuel_Group %in% c("Petrol", "Diesel")]),
      Total_Sales_2025_2037 = total_sales
    )
}

test_result <- run_scenario()
print(test_result)

param_grid <- expand.grid(
  growth_mult   = c(0.9, 1.0, 1.1),
  max_age       = c(20, 25, 30),
  zev_scenario  = c("baseline", "mild", "medium"),
  stringsAsFactors = FALSE
)

sensitivity_results <- param_grid %>%
  rowwise() %>%
  mutate(out = list(run_scenario(growth_mult, max_age, zev_scenario))) %>%
  unnest(out)

print(sensitivity_results)
print(unique(sensitivity_results$zev_scenario))
print(class(sensitivity_results$zev_scenario))

library(ggplot2)
library(tidyr)

sensitivity_results %>%
  pivot_longer(
    cols = c(BEV_Share_2037, Fossil_Fleet_2037, Avg_Age_2037, Total_Sales_2025_2037),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  ggplot(aes(x = growth_mult, y = Value, color = factor(max_age), shape = zev_scenario)) +
  geom_point(size = 2.5) +
  geom_line(aes(group = interaction(max_age, zev_scenario)), alpha = 0.4) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(
    x = "NATCOP Growth Multiplier",
    y = "Metric Value",
    color = "Max Age (years)",
    shape = "ZEV Scenario"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

sensitivity_results %>%
  group_by(zev_scenario) %>%
  summarise(
    BEV_Share_Mean = mean(BEV_Share_2037),
    BEV_Share_SD = sd(BEV_Share_2037),
    Fossil_Fleet_Mean = mean(Fossil_Fleet_2037),
    Fossil_Fleet_SD = sd(Fossil_Fleet_2037),
    Age_Mean = mean(Avg_Age_2037),
    Age_SD = sd(Avg_Age_2037),
    Total_Sales_Mean = mean(Total_Sales_2025_2037),
    Total_Sales_SD = sd(Total_Sales_2025_2037),
    .groups = 'drop'
  ) %>%
  print()

sensitivity_results %>%
  group_by(growth_mult, max_age) %>%
  summarise(
    BEV_Spread = max(BEV_Share_2037) - min(BEV_Share_2037),
    Fossil_Spread = max(Fossil_Fleet_2037) - min(Fossil_Fleet_2037),
    .groups = 'drop'
  ) %>%
  print()

cor(sensitivity_results[, c("growth_mult", "max_age", "BEV_Share_2037",
                            "Fossil_Fleet_2037", "Avg_Age_2037",
                            "Total_Sales_2025_2037")],
    use = "complete.obs") %>%
  round(3) %>%
  print()

# ============================================================================
# Backtest validation (2019 -> 2024)
# ============================================================================
retirement_backtest <- survival_by_year_all %>%
  filter(Data_Year %in% 2017:2019, Vehicle_Age <= 30) %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(Retirement_Rate = mean(1 - Annual_Survival, na.rm = TRUE),
            Survival_Rate   = mean(Annual_Survival, na.rm = TRUE), .groups = "drop")

fleet_2019 <- fleet_complete %>% filter(Data_Year == 2019, Vehicle_Age <= 30)

fuel_shares_historical <- data %>%
  filter(Data_Year %in% 2020:2024, Vehicle_Age == 0) %>%
  group_by(Data_Year, Fuel_Group) %>%
  summarise(n = sum(Fleet_Count), .groups = "drop") %>%
  group_by(Data_Year) %>%
  mutate(sale_percentage = n / sum(n)) %>%
  rename(Year = Data_Year) %>%
  ungroup()

natcop_historical <- data %>%
  filter(Data_Year %in% 2020:2024) %>%
  group_by(Data_Year) %>%
  summarise(Target_Fleet = sum(Fleet_Count)) %>%
  rename(Year = Data_Year) %>%
  ungroup()

fuel_shares_shifted <- fuel_shares_historical %>%
  mutate(Year_Shifted = Year + 5) %>%
  select(Year = Year_Shifted, Fuel_Group, sale_percentage)

natcop_shifted <- natcop_historical %>%
  mutate(Year_Shifted = Year + 5) %>%
  select(Year = Year_Shifted, Target_Fleet)

last_shifted_fuel <- fuel_shares_shifted %>%
  filter(Year == 2029) %>%
  select(Fuel_Group, sale_percentage)

last_shifted_natcop <- natcop_shifted %>%
  filter(Year == 2029) %>%
  pull(Target_Fleet)

fuel_shares_extended <- bind_rows(
  fuel_shares_shifted,
  expand.grid(Year = 2030:2037, Fuel_Group = unique(last_shifted_fuel$Fuel_Group)) %>%
    left_join(last_shifted_fuel, by = "Fuel_Group") %>%
    mutate(sale_percentage = ifelse(is.na(sale_percentage), 0.2, sale_percentage))
)

natcop_extended <- bind_rows(
  natcop_shifted,
  tibble(Year = 2030:2037, Target_Fleet = last_shifted_natcop)
)

backtest <- project_fleet(
  base_fleet = fleet_2019,
  retirement_rates = retirement_backtest,
  fuel_shares = fuel_shares_extended,
  natcop_targets = natcop_extended,
  engine_sales_distribution = engine_sales_distribution
)

if (!is.null(backtest)) {

  predicted_2024 <- bind_rows(backtest$fleet) %>%
    filter(Year == 2029) %>%
    group_by(Fuel_Group) %>%
    summarise(Predicted = sum(Fleet))

  actual_2024 <- data %>%
    filter(Data_Year == 2024) %>%
    group_by(Fuel_Group) %>%
    summarise(Actual = sum(Fleet_Count))

  comparison <- predicted_2024 %>%
    full_join(actual_2024, by = "Fuel_Group") %>%
    mutate(
      Predicted = ifelse(is.na(Predicted), 0, Predicted),
      Actual = ifelse(is.na(Actual), 0, Actual),
      Pct_Error = ifelse(Actual != 0, 100 * (Predicted - Actual) / Actual, NA)
    )

  cat("\n========================================\n")
  cat("BACKTEST RESULTS (2019 -> 2024):\n")
  cat("========================================\n")
  print(comparison)

  library(ggplot2)
  library(tidyr)

  comparison <- comparison %>%
    mutate(
      Error_Label = paste0(round(Pct_Error, 1), "%"),
      Error_Direction = ifelse(Pct_Error > 0, "Over-predicted", "Under-predicted")
    )

  comparison %>%
    pivot_longer(cols = c(Predicted, Actual), names_to = "Source", values_to = "Count") %>%
    ggplot(aes(x = Fuel_Group, y = Count, fill = Source)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(aes(label = format(round(Count), big.mark = ",")),
              position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
    geom_text(data = comparison,
              aes(x = Fuel_Group,
                  y = pmax(Predicted, Actual) * 1.12,
                  label = Error_Label,
                  color = Error_Direction),
              size = 4, fontface = "bold", inherit.aes = FALSE) +
    scale_color_manual(values = c("Over-predicted" = "#e74c3c",
                                  "Under-predicted" = "#3498db")) +
    labs(
      x = "Fuel Group",
      y = "Fleet Count",
      fill = "Source",
      color = "Prediction"
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold")
    )
}

comparison %>%
  pivot_longer(cols = c(Predicted, Actual), names_to = "Source", values_to = "Count") %>%
  ggplot(aes(x = Fuel_Group, y = Count, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = format(round(Count), big.mark = ",")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(
    x = "Fuel Group",
    y = "Fleet Count",
    fill = "Source"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    legend.position = "right"
  )

# ============================================================================
# Real mileage patterns by age, fuel, and size class
# ============================================================================
real_mileage_by_age_size <- fleet_2024_complete %>%
  filter(!is.na(Avg_Mileage_Calibrated), !is.na(vehicle_class), Vehicle_Age <= 25) %>%
  group_by(Fuel_Group, vehicle_class, Vehicle_Age) %>%
  summarise(
    Avg_Mileage = weighted.mean(Avg_Mileage_Calibrated, Fleet_Count, na.rm = TRUE),
    Total_Fleet = sum(Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Fuel_Group, vehicle_class, Vehicle_Age)

print("=== REAL MILEAGE BY FUEL, SIZE, AND AGE ===")
print(real_mileage_by_age_size, n = Inf)
