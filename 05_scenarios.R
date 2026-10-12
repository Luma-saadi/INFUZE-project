# ============================================================================
# 05_scenarios.R
# ----------------------------------------------------------------------------
# Fuel-share forecasts, NATCOP fleet targets, inflow/sales profiles, the
# four policy scenarios (Baseline, 70% BEV by 2030, 60% BEV by 2030, Fixed
# 2M sales), and the fleet-projection engine `project_fleet()`.
#
# Depends on: 00_setup.R, 01_data_prep.R, 02_fleet_classification.R,
#             03_origin_analysis.R, 04_survival_retirement.R
# Produces  : fuel_shares_complete, natcop_targets, inflow_unique,
#             smmt_sales_forecast, sales_by_fuel_smmt,
#             engine_sales_distribution,
#             results_baseline, results_70pct, results_60pct,
#             results_scenario4
# ============================================================================

# ============================================================================
# CAR SHARING / MILEAGE SCENARIOS (Mileage reallocation)
# ============================================================================
# The VKT-conserving mileage reallocation work is handled in
# 07_mileage_scenarios.R and depends on the objects produced here.

# ============================================================================
# 1. Fuel-type forecasting model
# ============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(stringr)
library(nls2)
library(purrr)

summary_table <- join_final_complete %>%
  group_by(Fuel_Group, Data_Year) %>%
  summarise(
    total_fleet = sum(Fleet_Count, na.rm = TRUE),
    Avg_Vehicle_Age = weighted.mean(Vehicle_Age, Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Data_Year, desc(total_fleet))

print(summary_table)

fuel_percentages <- summary_table %>%
  group_by(Data_Year) %>%
  mutate(
    yearly_total = sum(total_fleet, na.rm = TRUE),
    percentage = (total_fleet / yearly_total) * 100
  ) %>%
  ungroup()
print(fuel_percentages)

fuel_summary_table <- fuel_percentages %>%
  select(Data_Year, Fuel_Group, percentage) %>%
  group_by(Data_Year, Fuel_Group) %>%
  summarise(
    percentage = sum(percentage, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = Fuel_Group,
    values_from = percentage,
    values_fill = 0
  ) %>%
  mutate(
    Total_Sum = rowSums(select(., -Data_Year)),
    Check = round(Total_Sum, 10) == 100
  ) %>%
  select(Data_Year, BEV, Diesel, HEV, PHEV, Petrol, Other, Total_Sum, Check)

cat("Fuel Type Composition Summary (Proportions Summing to 1):\n")
print(fuel_summary_table, n = 20)

cars_Numbers <- fuel_percentages %>%
  select(Data_Year, Fuel_Group, total_fleet) %>%
  pivot_wider(
    names_from = Fuel_Group,
    values_from = total_fleet,
    values_fill = 0,
    values_fn = sum
  ) %>%
  mutate(Total_Nu = rowSums(select(., -Data_Year)))

cat("total_numer of vehicles")
print(cars_Numbers, n = 30)

# ============================================================================
# 2. Forecast function (auto-detect model type)
# ============================================================================
forecast_fuel <- function(fuel_data, years_ahead = 10) {
  fuel_name <- unique(na.omit(fuel_data$Fuel))
  if (length(fuel_name) == 0) fuel_name <- "Unknown"

  ts_data <- ts(fuel_data$percentage, start = min(fuel_data$Data_Year, na.rm = TRUE))

  model_linear <- lm(percentage ~ Data_Year, data = fuel_data)
  model_ets <- ets(ts_data)

  logistic_fit <- NULL
  if (nrow(fuel_data) >= 5 && diff(range(fuel_data$percentage, na.rm = TRUE)) > 1) {
    logistic_fit <- tryCatch(
      nls(percentage ~ SSlogis(Data_Year, Asym, xmid, scal), data = fuel_data),
      error = function(e) NULL
    )
  }

  future_years <- data.frame(
    Data_Year = seq(max(fuel_data$Data_Year, na.rm = TRUE) + 1,
                    max(fuel_data$Data_Year, na.rm = TRUE) + years_ahead)
  )

  linear_pred <- predict(model_linear, newdata = future_years)
  ets_pred <- forecast(model_ets, h = years_ahead)$mean

  use_logistic <- !is.null(logistic_fit) &&
    fuel_name[1] %in% c("Battery electric",
                        "Plug-in hybrid electric (Petrol)",
                        "Plug-in hybrid electric (Diesel)")

  if (use_logistic) {
    logistic_pred <- predict(logistic_fit, newdata = future_years)
    final_pred <- 0.5 * logistic_pred + 0.5 * ets_pred
    model_used <- "Hybrid Logistic+ETS"
  } else {
    final_pred <- 0.5 * linear_pred + 0.5 * ets_pred
    model_used <- "Hybrid Linear+ETS"
  }

  final_pred <- pmin(pmax(final_pred, 0), 100)

  tibble(
    Data_Year = future_years$Data_Year,
    Predicted_Percentage = as.numeric(final_pred),
    Model_Used = model_used
  )
}

fuel_forecasts <- fuel_percentages %>%
  filter(!Fuel_Group %in% c("Other")) %>%
  group_by(Fuel_Group) %>%
  group_modify(~ forecast_fuel(.x)) %>%
  ungroup()

# ============================================================================
# Manual forecasts for BEV, PHEV, ZEV based on CCC Sixth Carbon Budget
# ============================================================================
manual_forecasts <- tibble(
  Data_Year = rep(2025:2037, each = 5),
  Fuel_Group = rep(c("BEV", "Diesel", "HEV", "PHEV", "Petrol"), times = 13),
  Predicted_Percentage = c(
    # 2025
    23.31013917, 5.168986083, 14.11530815, 11.23260437, 46.17296223,
    # 2026
    28.24803150, 4.281496063, 15.45275591, 12.05708661, 39.96062992,
    # 2027
    32.21574344, 3.644314869, 16.61807580, 12.92517007, 34.59669582,
    # 2028
    36.63330802, 2.881979261, 15.69464022, 15.36669198, 29.42338052,
    # 2029
    58, 1.357308, 15, 15, 10.64271007,
    # 2030
    80, 0, 5, 15, 0,
    # 2031
    84, 0, 0, 16, 0,
    # 2032
    88, 0, 0, 12, 0,
    # 2033
    92, 0, 0, 8, 0,
    # 2034
    96, 0, 0, 4, 0,
    # 2035
    100, 0, 0, 0, 0,
    # 2036
    100, 0, 0, 0, 0,
    # 2037
    100, 0, 0, 0, 0
  ),
  Model_Used = "Manual (CCC)"
)

cat("=== CHECKING YEARLY SUMS ===\n")
yearly_sums <- manual_forecasts %>%
  group_by(Data_Year) %>%
  summarise(
    Total = sum(Predicted_Percentage),
    .groups = "drop"
  )

print(yearly_sums)

adjusted_forecasts <- fuel_forecasts %>%
  bind_rows(manual_forecasts) %>%
  group_by(Data_Year) %>%
  ungroup()

cat("=== ADJUSTED FORECASTS ===\n")
print(adjusted_forecasts, n = 50)

fuel_forecasts_updated <- adjusted_forecasts %>%
  select(Data_Year, Fuel_Group, Percentage = Predicted_Percentage, Model_Used)

write_csv_zipped <- function(df, filename, row.names = FALSE) {
  csv_name <- filename
  zip_name <- paste0(tools::file_path_sans_ext(filename), ".zip")
  data.table::fwrite(df, csv_name, row.names = row.names)
  zip::zip(zip_name, csv_name)
  file.remove(csv_name)
  cat("Saved:", zip_name, "\n")
}

write_csv_zipped(fuel_forecasts_updated, file.path(out_dir, "fuel_forecasts_updated.csv"), row.names = FALSE)

cat("\n=== FILE SAVED SUCCESSFULLY ===\n")
cat("Saved to: fuel_forecasts_updated.csv\n")

# ============================================================================
# 3. Comprehensive visualisation of all fuels
# ============================================================================
historical_data <- fuel_percentages %>%
  filter(Data_Year == "2024",
         Avg_Vehicle_Age == "0",
         !is.na(Avg_Vehicle_Age)) %>%
  select(Data_Year, Fuel_Group, yearly_total, total_fleet) %>%
  mutate(Model_Used = "Historical") %>%
  ungroup() %>%
  mutate(
    total = sum(total_fleet, na.rm = TRUE),
    Percentage = 100 * total_fleet / total,
    Data_Year = as.numeric(Data_Year))

forecast_data <- fuel_forecasts_updated

plot_data <- bind_rows(
  historical_data,
  forecast_data
) %>%
  filter(Data_Year >= 2024, Data_Year <= 2034)

plot_data <- plot_data %>%
  group_by(Data_Year) %>%
  mutate(Percentage = 100 * Percentage / sum(Percentage, na.rm = TRUE)) %>%
  ungroup()

plot_data_aggregated <- plot_data %>%
  group_by(Data_Year, Fuel_Group) %>%
  summarise(
    Percentage = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

plot_data_wide <- plot_data_aggregated %>%
  pivot_wider(
    names_from = Fuel_Group,
    values_from = Percentage,
    values_fill = 0
  ) %>%
  arrange(Data_Year)

fuel_order <- c("Petrol", "Diesel", "HEV", "PHEV", "BEV")

existing_fuels <- fuel_order[fuel_order %in% names(plot_data_wide)]
plot_data_wide <- plot_data_wide %>%
  select(Data_Year, all_of(existing_fuels))

plot_data_long <- plot_data_wide %>%
  pivot_longer(
    cols = -Data_Year,
    names_to = "Fuel",
    values_to = "Percentage"
  ) %>%
  mutate(
    Fuel = factor(Fuel, levels = fuel_order)
  )

plot_data_long <- plot_data_long %>%
  mutate(
    Fuel = factor(Fuel, levels = fuel_order),
    Percentage = ifelse(Percentage <= 0, 1e-9, Percentage)
  ) %>%
  arrange(Data_Year, Fuel)

p_comprehensive <- ggplot(plot_data_long,
                          aes(x = Data_Year, y = Percentage,
                              fill = Fuel, group = Fuel)) +
  geom_area(position = "stack", alpha = 0.85, size = 0.3, color = "white") +
  scale_x_continuous(breaks = seq(2023, 2034, 2)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  scale_fill_manual(values = c(
    Petrol = "#DC143C", Diesel = "#696969", HEV = "#FFD700",
    PHEV = "#FFA500", ZEV = "#8B4513", BEV = "#2E8B57"
  )) +
  theme_minimal(base_size = 15)

print(p_comprehensive)

p_stacked_bar <- ggplot(plot_data_long, aes(x = Data_Year, y = Percentage, fill = Fuel)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  scale_x_continuous(breaks = seq(2023, 2034, 2)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  scale_fill_manual(values = c(
    Petrol = "#DC143C", Diesel = "#696969", HEV = "#FFD700",
    PHEV = "#FFA500", ZEV = "#8B4513", BEV = "#2E8B57"
  )) +
  labs(
    x = "Year", y = "Fleet Share (%)",
    fill = "Fuel Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_stacked_bar)

cat("\nStacking order (bottom to top):\n")
for(i in seq_along(fuel_order)) {
  cat(i, ":", fuel_order[i], "\n")
}

yearly_totals <- plot_data_long %>%
  group_by(Data_Year) %>%
  summarise(
    Total = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nYearly percentage totals (should be 100%):\n")
print(yearly_totals)

cat("\nSample: Composition for 2030:\n")
sample_2030 <- plot_data_long %>%
  filter(Data_Year == 2030) %>%
  arrange(desc(Fuel))
print(sample_2030)

# ============================================================================
# 4. Preparation: forecast fleet, fuel shares, NATCOP targets
# ============================================================================
Forcast_fleet <- fread(file.path(out_dir, "your_forecast_core_trend_2024_2037.csv"))
Forcast_fleet <- Forcast_fleet %>% select(Year, Annual_Growth_Rate, Total_Vehicles, Core_Model)

Fuel_forecast <- fread(file.path(out_dir, "fuel_forecasts_updated.zip"))

Fuel_forecast <- Fuel_forecast %>%
  mutate(Fuel_percentage = Percentage / 100) %>%
  rename(Year = Data_Year) %>%
  filter(Model_Used == "Manual (CCC)")

Fuel_check <- Fuel_forecast %>%
  group_by(Year) %>%
  summarise(
    Total = sum(Fuel_percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Check = ifelse(round(Total, 10) == 1, "Yes", "No"))

cat("Check if percentages sum to 1 (100%) for each year:\n")
print(Fuel_check)

merged_fleet <- Forcast_fleet %>%
  select(Annual_Growth_Rate, Core_Model, Year) %>%
  left_join(Fuel_forecast, by = "Year")

merged_fleet <- merged_fleet %>%
  filter(Year >= 2024) %>%
  rename(sale_percentage = Fuel_percentage) %>%
  select(Year, Core_Model, Fuel_Group, sale_percentage, Annual_Growth_Rate) %>%
  arrange(Fuel_Group, Year)
print(merged_fleet, width = Inf)

# ============================================================================
# PART 3: Load inflow data, create sales forecast 2025-2037
# ============================================================================
fleet_2024_base <- Forcast_fleet$Total_Vehicles[Forcast_fleet$Year == 2024]
print(fleet_2024_base)

cat("2024 Base Fleet (age <= 30):", format(fleet_2024_base, big.mark = ","), "\n")

inflow_data <- merged_fleet

fuel_shares_all <- inflow_data %>%
  select(Year, Fuel_Group, sale_percentage) %>%
  group_by(Year, Fuel_Group) %>%
  summarise(sale_percentage = mean(sale_percentage), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(sale_percentage = sale_percentage / sum(sale_percentage)) %>%
  ungroup()

fuel_shares_complete <- expand.grid(
  Year = 2025:2037,
  Fuel_Group = c("Petrol", "Diesel", "BEV", "PHEV", "HEV")
) %>%
  left_join(fuel_shares_all, by = c("Year", "Fuel_Group")) %>%
  group_by(Year) %>%
  mutate(
    sale_percentage = ifelse(is.na(sale_percentage), mean(sale_percentage, na.rm = TRUE), sale_percentage),
    sale_percentage = ifelse(is.na(sale_percentage), 0.2, sale_percentage),
    sale_percentage = sale_percentage / sum(sale_percentage)
  ) %>%
  ungroup()

print(fuel_shares_complete, n = Inf)

fuel_shares_table <- fuel_shares_complete %>%
  select(Year, Fuel_Group, sale_percentage) %>%
  arrange(Year, Fuel_Group) %>%
  mutate(
    sale_percentage = sale_percentage * 100,
    sale_percentage = round(sale_percentage, 2)
  ) %>%
  pivot_wider(
    names_from = Fuel_Group,
    values_from = sale_percentage,
    values_fill = 0
  ) %>%
  arrange(Year)

print(fuel_shares_table)

# ============================================================================
# FENCE 3 - Save retirement, fuel shares, NATCOP, base fleet
# ============================================================================
saveRDS(retirement_avg,      file.path(out_dir, "retirement_avg.rds"))
saveRDS(fleet_2024_complete, file.path(out_dir, "fleet_2024_complete.rds"))
saveRDS(fleet_complete,      file.path(out_dir, "fleet_complete.rds"))
saveRDS(fuel_shares_complete, file.path(out_dir, "fuel_shares_complete.rds"))
saveRDS(weight_model,        file.path(out_dir, "weight_model.rds"))
saveRDS(growth_rates,        file.path(out_dir, "growth_rates.rds"))
saveRDS(bev_country_2024,    file.path(out_dir, "bev_country_2024.rds"))

cat("\n=== FENCE 3: retirement/fuel/base objects saved ===\n")

# ============================================================================
# PART 4: Sales forecast 2025-2037 using SMMT + growth rates
# ============================================================================
target_sales <- data.frame(
  Year = c(2025, 2026, 2027),
  Total_Sales = c(2020520, 2032000, 2058000)
)

cat("\n=== SMMT TOTAL SALES TARGETS ===\n")
print(target_sales)

growth_rate <- inflow_data %>%
  select(Year, Annual_Growth_Rate) %>%
  distinct() %>%
  arrange(Year)

sales_2027 <- target_sales$Total_Sales[3]

smmt_sales_forecast <- data.frame(Year = 2025:2037)
smmt_sales_forecast$Total_Sales <- NA
smmt_sales_forecast$Total_Sales[1:3] <- target_sales$Total_Sales

for(i in 4:13) {
  yr <- smmt_sales_forecast$Year[i]
  prev_sales <- smmt_sales_forecast$Total_Sales[i-1]

  growth <- growth_rate$Annual_Growth_Rate[growth_rate$Year == yr]
  if(length(growth) > 0 && !is.na(growth)) {
    smmt_sales_forecast$Total_Sales[i] <- prev_sales * (1 + growth/100)
  } else {
    smmt_sales_forecast$Total_Sales[i] <- prev_sales * 1.01
  }
}

cat("\n=== COMPLETE SALES FORECAST 2025-2037 ===\n")
print(smmt_sales_forecast)

# ============================================================================
# PART 5: Fuel shares for new sales
# ============================================================================
sales_by_fuel_smmt <- fuel_shares_complete %>%
  left_join(smmt_sales_forecast, by = "Year") %>%
  mutate(
    New_Sales = sale_percentage * Total_Sales,
    Vehicle_Age = 0
  ) %>%
  select(Year, Fuel_Group, New_Sales, Vehicle_Age)

cat("\n=== SALES BY FUEL TYPE (First few years) ===\n")
print(sales_by_fuel_smmt %>% filter(Year <= 2037) %>% pivot_wider(names_from = Fuel_Group, values_from = New_Sales))

# ============================================================================
# Engine sales distribution from 2024 fleet composition
# ============================================================================
engine_sales_distribution <- fleet_2024_complete %>%
  filter(
    Vehicle_Age == 0,
    !is.na(engine_size)
  ) %>%
  mutate(
    engine_size_liters = round(engine_size / 1000, 1)
  ) %>%
  group_by(Fuel_Group, engine_size_liters, vehicle_class) %>%
  summarise(
    registrations = sum(Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Fuel_Group) %>%
  mutate(
    share = registrations / sum(registrations)
  ) %>%
  ungroup()

names(engine_sales_distribution)

# ============================================================================
# 6. project_fleet()
# ============================================================================
project_fleet <- function(base_fleet, retirement_rates, fuel_shares,
                          natcop_targets, engine_sales_distribution) {
  if (!"kerb_weight_kg" %in% names(base_fleet)) {
    base_fleet <- base_fleet %>% mutate(kerb_weight_kg = NA_real_)
  }

  base_dimensions <- fleet_2024_complete %>%
    filter(!is.na(rdw_length_mm), !is.na(rdw_width_mm), !is.na(rdw_height_mm)) %>%
    group_by(Fuel_Group, vehicle_class) %>%
    summarise(
      avg_length_mm = weighted.mean(rdw_length_mm, Fleet_Count, na.rm = TRUE),
      avg_width_mm  = weighted.mean(rdw_width_mm,  Fleet_Count, na.rm = TRUE),
      avg_height_mm = weighted.mean(rdw_height_mm, Fleet_Count, na.rm = TRUE),
      .groups = "drop"
    )

  fleet_list <- list()
  fleet_list[["2024"]] <- base_fleet %>%
    rename(Fleet = Fleet_Count) %>%
    mutate(Year = 2024,
           engine_size = engine_size,
           engine_size_liters = engine_size / 1000) %>%
    select(Year, Vehicle_Age, Fuel_Group, Fleet, engine_size, engine_size_liters,
           vehicle_class, kerb_weight_kg, everything())

  accounting <- data.frame()
  sales_by_fuel_list   <- list()
  retired_by_fuel_list <- list()

  current_fleet <- base_fleet %>%
    rename(Fleet = Fleet_Count) %>%
    mutate(engine_size = engine_size,
           engine_size_liters = engine_size / 1000) %>%
    mutate(Fleet = ifelse(is.na(Fleet), 0, Fleet),
           engine_size = ifelse(is.na(engine_size), 1.5, engine_size),
           engine_size_liters = ifelse(is.na(engine_size_liters), 1.5, engine_size_liters),
           kerb_weight_kg = ifelse(is.na(kerb_weight_kg), NA_real_, kerb_weight_kg))

  for (yr in 2025:2037) {

    target_fleet <- natcop_targets %>% filter(Year == yr) %>% pull(Target_Fleet)

    aged_fleet_all <- current_fleet %>%
      left_join(retirement_rates, by = c("Vehicle_Age", "Fuel_Group")) %>%
      mutate(
        Retirement_Rate = coalesce(Retirement_Rate, 1),
        Survival_Rate   = coalesce(Survival_Rate, 0),
        Survivors           = round(Fleet * Survival_Rate, 0),
        Natural_Retirements = Fleet - Survivors,
        Vehicle_Age = Vehicle_Age + 1,
        engine_size = engine_size,
        engine_size_liters = engine_size_liters,
        kerb_weight_kg = kerb_weight_kg
      ) %>%
      mutate(
        Survivors = ifelse(is.na(Survivors), 0, Survivors),
        Natural_Retirements = ifelse(is.na(Natural_Retirements), 0, Natural_Retirements)
      )

    aged_fleet <- aged_fleet_all %>%
      filter(Vehicle_Age <= 30) %>%
      select(Vehicle_Age, Fuel_Group, Survivors, engine_size, engine_size_liters, vehicle_class, kerb_weight_kg)

    retired_by_fuel <- aged_fleet_all %>%
      filter(Natural_Retirements > 0) %>%
      group_by(Fuel_Group, Vehicle_Age, engine_size, engine_size_liters, vehicle_class) %>%
      summarise(Retired = sum(Natural_Retirements, na.rm = TRUE), .groups = "drop") %>%
      mutate(Year = yr)
    retired_by_fuel_list[[as.character(yr)]] <- retired_by_fuel

    total_survivors <- sum(aged_fleet$Survivors)
    total_natural_retirements <- sum(current_fleet$Fleet) - total_survivors

    required_total_sales <- max(0, target_fleet - total_survivors)

    year_shares <- fuel_shares %>% filter(Year == yr)
    if (nrow(year_shares) > 0) {
      year_sales <- year_shares %>%
        mutate(Required_Sales = round(sale_percentage * required_total_sales, 0),
               Vehicle_Age = 0)
      actual_new_sales <- sum(year_sales$Required_Sales)
      if (actual_new_sales != required_total_sales) {
        diff <- required_total_sales - actual_new_sales
        largest_idx <- which.max(year_sales$Required_Sales)
        year_sales$Required_Sales[largest_idx] <- year_sales$Required_Sales[largest_idx] + diff
        actual_new_sales <- sum(year_sales$Required_Sales)
      }
      required_total_sales <- actual_new_sales
    } else {
      year_sales <- expand.grid(
        Year = yr,
        Fuel_Group = c("Petrol", "Diesel", "BEV", "PHEV", "HEV"),
        sale_percentage = 0.2,
        Required_Sales = round(0.2 * required_total_sales, 0),
        Vehicle_Age = 0)
      required_total_sales <- sum(year_sales$Required_Sales)
    }

    new_vehicles <- year_sales %>%
      rename(Fleet = Required_Sales) %>%
      left_join(engine_sales_distribution, by = "Fuel_Group", relationship = "many-to-many") %>%
      mutate(
        Fleet_original = Fleet,
        Fleet = round(Fleet * share, 0),
        engine_size = engine_size_liters * 1000,
        Vehicle_Age = 0,
        Year = yr
      ) %>%
      group_by(Fuel_Group) %>%
      mutate(
        rounding_diff = first(Fleet_original) - sum(Fleet),
        Fleet = ifelse(row_number() == which.max(Fleet), Fleet + rounding_diff, Fleet)
      ) %>%
      ungroup() %>%
      left_join(base_dimensions, by = c("Fuel_Group", "vehicle_class")) %>%
      mutate(
        projected_length_mm = avg_length_mm + growth_rates$length_mm_per_year * (yr - 2024),
        projected_width_mm = avg_width_mm + growth_rates$width_mm_per_year * (yr - 2024),
        projected_height_mm = avg_height_mm + growth_rates$height_mm_per_year * (yr - 2024),

        kerb_weight_kg = predict(weight_model,
                                 newdata = data.frame(
                                   length_m = projected_length_mm / 1000,
                                   width_m = projected_width_mm / 1000,
                                   height_m = projected_height_mm / 1000,
                                   Fuel_Group = Fuel_Group,
                                   vehicle_class = vehicle_class
                                 )),
        kerb_weight_kg = round(kerb_weight_kg, 0)
      ) %>%
      select(Year, Vehicle_Age, Fuel_Group, Fleet, engine_size, engine_size_liters,
             vehicle_class, kerb_weight_kg) %>%
      filter(Fleet > 0)

    combined_fleet <- bind_rows(
      aged_fleet %>%
        mutate(Year = yr, Fleet = Survivors) %>%
        select(Year, Vehicle_Age, Fuel_Group, Fleet, engine_size, engine_size_liters, vehicle_class, kerb_weight_kg),
      new_vehicles
    )

    final_fleet_total <- sum(combined_fleet$Fleet, na.rm = TRUE)

    if (total_survivors + required_total_sales != target_fleet) {
      diff <- target_fleet - (total_survivors + required_total_sales)
      largest_idx <- which.max(new_vehicles$Fleet)
      new_vehicles$Fleet[largest_idx] <- new_vehicles$Fleet[largest_idx] + diff
      required_total_sales <- required_total_sales + diff
      final_fleet_total <- total_survivors + required_total_sales
      combined_fleet <- bind_rows(
        aged_fleet %>% mutate(Year = yr, Fleet = Survivors) %>%
          select(Year, Vehicle_Age, Fuel_Group, Fleet, engine_size, engine_size_liters, vehicle_class, kerb_weight_kg),
        new_vehicles
      )
    }

    sales_by_fuel_list[[as.character(yr)]] <- year_sales %>%
      mutate(Year = yr) %>% select(Year, Fuel_Group, Required_Sales)

    accounting <- bind_rows(accounting, data.frame(
      Year = yr,
      Starting_Fleet = sum(current_fleet$Fleet),
      Target_Fleet = target_fleet,
      Predefined_Sales = required_total_sales,
      Natural_Retirements = total_natural_retirements,
      Additional_Retirements = 0,
      Total_Retirements = total_natural_retirements,
      Fleet_Before_Forced = total_survivors + required_total_sales,
      Final_Fleet = final_fleet_total,
      Error = final_fleet_total - target_fleet,
      Pct_Error = 100 * (final_fleet_total - target_fleet) / target_fleet
    ))

    fleet_list[[as.character(yr)]] <- combined_fleet
    current_fleet <- combined_fleet %>%
      select(Vehicle_Age, Fuel_Group, Fleet, engine_size, engine_size_liters, vehicle_class, kerb_weight_kg)

    rm(aged_fleet_all, aged_fleet, retired_by_fuel, new_vehicles, combined_fleet,
       year_sales, year_shares)
    gc(verbose = FALSE)
  }

  retired_by_fuel_df <- bind_rows(retired_by_fuel_list)

  return(list(
    fleet = fleet_list,
    accounting = accounting,
    retired_by_fuel = retired_by_fuel_df,
    additional_retirements = data.frame(Year = integer(), Additional_Retirements = numeric()),
    sales_by_fuel = bind_rows(sales_by_fuel_list)
  ))
}

# ============================================================================
# 7. NATCOP targets
# ============================================================================
# HOW THE NATCOP TARGETS ARE BUILT
# ----------------------------------------------------------------------------
# The annual growth rates used here were obtained from the NATCOP core model
# for the years 2025-2037. They are:
#
#     Year : 2025   2026   2027   2028   2029   2030   2031
#     g(%) : 1.04   1.00   0.96   0.92   0.89   0.85   0.81
#
#     Year : 2032   2033   2034   2035   2036   2037
#     g(%) : 0.78   0.75   0.71   0.68   0.65   0.62
#
# These rates come from the core-model growth factors:
#     Annual_Growth_Rate = (Growth_Factor - 1) * 100
#
# The 2024 fleet number (observed cars, i.e. fleet_2024_base) is used as the
# BASE, and the growth rates are compounded forward from it to estimate the
# future fleet number of cars for each year 2025-2037:
#
#     Target_Fleet(t) = fleet_2024_base * prod_{y=2025..t}(1 + g_y/100)
#
# Equivalently, using the cumulative product in the code:
#     Cumulative_Growth = cumprod(1 + Annual_Growth_Rate/100)
#     Target_Fleet      = fleet_2024_base * Cumulative_Growth
#
# 2024 is anchored as the base year (Cumulative_Growth[2024] = 1, so
# Target_Fleet[2024] = fleet_2024_base exactly).

inflow_unique <- inflow_data %>%
  select(Year, Annual_Growth_Rate, Core_Model) %>%
  distinct() %>%
  arrange(Year)

inflow_unique <- inflow_unique %>%
  mutate(
    Cumulative_Growth = cumprod(1 + coalesce(Annual_Growth_Rate/100, 0)),
    Cumulative_Growth = ifelse(Year == 2024, 1, Cumulative_Growth),
    Target_Fleet = fleet_2024_base * Cumulative_Growth
  )

natcop_targets <- inflow_unique %>%
  select(Year, Target_Fleet, Annual_Growth_Rate, Core_Model)

cat("\n=== NATCOP TARGETS (from unique data) ===\n")
print(natcop_targets)

# ============================================================================
# Run the fixed function for the baseline
# ============================================================================
fleet_2024_complete <- fleet_2024_complete %>% filter(Vehicle_Age <= 30)
sum(fleet_2024_complete$Fleet_Count)
print(natcop_targets)

retirement_avg <- retirement_avg %>% filter(Vehicle_Age <= 30)

base_fleet_2024 <- fleet_2024_complete

results <- project_fleet(
  base_fleet = base_fleet_2024,
  retirement_rates = retirement_avg,
  fuel_shares = fuel_shares_complete,
  natcop_targets = natcop_targets,
  engine_sales_distribution = engine_sales_distribution
)

bind_rows(results$fleet) %>%
  filter(Vehicle_Age == 0, Fuel_Group == "HEV") %>%
  summarise(
    total = n(),
    n_valid = sum(!is.na(kerb_weight_kg) & !is.nan(kerb_weight_kg)),
    pct_valid = round(100 * n_valid / total, 1)
  )

weight_check <- bind_rows(results$fleet) %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Year, Fuel_Group) %>%
  summarise(
    total_new_sales = sum(Fleet, na.rm = TRUE),
    n_rows = n(),
    n_valid = sum(!is.na(kerb_weight_kg) & !is.nan(kerb_weight_kg)),
    pct_valid = round(100 * n_valid / n_rows, 1),
    .groups = "drop"
  ) %>%
  arrange(Fuel_Group, Year)

print(weight_check, n = Inf)

weight_check %>% filter(pct_valid < 100)

bind_rows(results$fleet) %>% group_by(Year) %>%
  summarise(total = sum(Fleet)) %>% print(n = Inf)

all_fleet <- bind_rows(results$fleet)

print(results$accounting)

age_summary <- data.frame(
  Year = 2024:2037,
  Max_Vehicle_Age = sapply(2024:2037, function(y) {
    max(results$fleet[[as.character(y)]]$Vehicle_Age)
  })
)
print(age_summary)

sales_comparison <- results$accounting %>%
  select(Year, Predefined_Sales) %>%
  rename(Model_Predicted_Sales = Predefined_Sales) %>%
  left_join(smmt_sales_forecast, by = "Year") %>%
  rename(smmt_Sales = Total_Sales)

cat("\n=== ORIGINAL VS PREDICTED SALES ===\n")
print(sales_comparison)

sal_at_age_0 <- bind_rows(results$fleet) %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Year) %>%
  summarise(Total_Sales = sum(Fleet))

print(sal_at_age_0)

fleet_targets <- results$accounting %>%
  select(Year, Target_Fleet, Starting_Fleet, Predefined_Sales) %>%
  mutate(
    Target_Growth = Target_Fleet - lag(Target_Fleet),
    Sales_Ratio = Predefined_Sales / Target_Fleet * 100
  )

print(fleet_targets)

ggplot(fleet_targets, aes(x = Year)) +
  geom_line(aes(y = Target_Fleet/1e6, color = "Target Fleet"), size = 1.2) +
  geom_line(aes(y = Predefined_Sales/1e6, color = "New Sales"), size = 1.2) +
  scale_y_continuous(labels = scales::comma_format(suffix = "M")) +
  labs(
    title = "NATCOP Fleet Targets vs Required Sales",
    subtitle = "Sales cap reflects slowing fleet growth",
    x = "Year", y = "Millions of Vehicles"
  )

fleet_analysis <- fleet_targets %>%
  mutate(
    Implied_Survivors = Starting_Fleet - (Starting_Fleet - lag(Starting_Fleet, default = first(Starting_Fleet))),
    Retirement_Rate_Implied = 1 - (Starting_Fleet - Predefined_Sales) / lag(Starting_Fleet, default = first(Starting_Fleet)),
    Sales_Change = Predefined_Sales - lag(Predefined_Sales, default = first(Predefined_Sales)),
    Target_Change = Target_Fleet - lag(Target_Fleet, default = first(Target_Fleet))
  )

print(fleet_analysis)

# ============================================================================
# ORIGIN SCENARIOS
# ============================================================================
origin_by_fuel <- data %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year, Region, Fuel_Group, Origin) %>%
  summarise(Reg = sum(Fleet_Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Data_Year, Fuel_Group) %>%
  mutate(Share_Within_Fuel = 100 * Reg / sum(Reg)) %>%
  ungroup()
print(origin_by_fuel, n = Inf)

origin_by_fuel %>%
  filter(Region %in% c("China", "Japan", "Korea", "UK", "USA"), Data_Year == 2024) %>%
  select(Region, Fuel_Group, Share_Within_Fuel) %>%
  pivot_wider(names_from = Region, values_from = Share_Within_Fuel) %>%
  print(n = Inf)

all_fuel_by_region <- data %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year, Region, Fuel_Group) %>%
  summarise(
    Vehicles = sum(Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Data_Year, Region) %>%
  summarise(
    Total_Vehicles = sum(Vehicles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Data_Year) %>%
  mutate(
    Global_Share = 100 * Total_Vehicles / sum(Total_Vehicles, na.rm = TRUE)
  ) %>%
  ungroup()

print("All vehicles by region over time (all fuel groups combined):")
print(all_fuel_by_region, n = Inf)

all_fuel_global <- all_fuel_by_region %>%
  select(Data_Year, Region, Global_Share) %>%
  pivot_wider(
    id_cols = Data_Year,
    names_from = Region,
    values_from = Global_Share,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(across(where(is.numeric) & !matches("Data_Year")), na.rm = TRUE))

print("All regions' share of global sales (all fuel types):")
print(all_fuel_global, n = Inf)

bev_by_region <- data %>%
  filter(Vehicle_Age == 0, Fuel_Group == "BEV") %>%
  group_by(Data_Year, Region) %>%
  summarise(
    Vehicles = sum(Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Data_Year) %>%
  mutate(
    Global_Share = 100 * Vehicles / sum(Vehicles)
  )

print("BEV sales by region over time:")
print(bev_by_region, n = Inf)

all_bev_global <- bev_by_region %>%
  group_by(Data_Year) %>%
  pivot_wider(
    id_cols = Data_Year,
    names_from = Region,
    values_from = Global_Share
  ) %>%
  mutate(Total = rowSums(across(where(is.numeric) & !matches("Data_Year")), na.rm = TRUE))

print("China's share of global BEV sales:")
print(all_bev_global, n = Inf)

bev_by_region <- data %>%
  filter(Vehicle_Age == 0, Fuel_Group == "BEV") %>%
  group_by(Data_Year, Region) %>%
  summarise(
    Vehicles = sum(Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Data_Year) %>%
  mutate(
    Global_Share = 100 * Vehicles / sum(Vehicles, na.rm = TRUE)
  ) %>%
  ungroup()

china_bev_global <- bev_by_region %>%
  filter(Region == "China") %>%
  select(Data_Year, Global_Share)

print("China's share of global BEV sales:")
print(china_bev_global, n = Inf)

library(ggplot2)

p_china_global <- china_bev_global %>%
  ggplot(aes(x = Data_Year, y = Global_Share)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "China's Share of Global BEV Sales",
    subtitle = "Post-pandemic recovery may explain the jump from 27.3% to 31.5%",
    x = "Year",
    y = "Share of Global BEV Sales (%)"
  ) +
  theme_minimal()

print(p_china_global)

# ============================================================================
# Origin x Fuel x Region shares (global)
# ============================================================================
fuel_region_share <- data %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year, Fuel_Group, Region) %>%
  summarise(
    Vehicles = sum(Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Data_Year, Fuel_Group) %>%
  mutate(
    Global_Share = 100 * Vehicles / sum(Vehicles, na.rm = TRUE)
  ) %>%
  ungroup()

print("Global share for each Region x Fuel_Group:")
print(fuel_region_share, n = Inf)

fuel_region_wide <- fuel_region_share %>%
  select(Data_Year, Fuel_Group, Region, Global_Share) %>%
  pivot_wider(
    id_cols = c(Data_Year, Fuel_Group),
    names_from = Region,
    values_from = Global_Share,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(across(where(is.numeric) & !matches("Data_Year|Fuel_Group")), na.rm = TRUE))

print("Wide format: Year, Fuel_Group, and Regions as columns:")
print(fuel_region_wide, n = Inf)

bev_table <- fuel_region_wide %>% filter(Fuel_Group == "BEV") %>% select(-Fuel_Group)
print("BEV - share of global BEV sales by region:")
print(bev_table, n = Inf)

petrol_table <- fuel_region_wide %>% filter(Fuel_Group == "Petrol") %>% select(-Fuel_Group)
print("Petrol - share of global Petrol sales by region:")
print(petrol_table, n = Inf)

diesel_table <- fuel_region_wide %>% filter(Fuel_Group == "Diesel") %>% select(-Fuel_Group)
print("Diesel - share of global Diesel sales by region:")
print(diesel_table, n = Inf)

hev_table <- fuel_region_wide %>% filter(Fuel_Group == "HEV") %>% select(-Fuel_Group)
print("HEV - share of global HEV sales by region:")
print(hev_table, n = Inf)

phev_table <- fuel_region_wide %>% filter(Fuel_Group == "PHEV") %>% select(-Fuel_Group)
print("PHEV - share of global PHEV sales by region:")
print(phev_table, n = Inf)

# Slopes
slopes <- fuel_region_share %>%
  group_by(Fuel_Group, Region) %>%
  summarise(
    Slope_pp = if (n() >= 2) coef(lm(Global_Share ~ Data_Year))[2] else NA,
    Last_Value = Global_Share[Data_Year == max(Data_Year)],
    R_Squared = if (n() >= 2) summary(lm(Global_Share ~ Data_Year))$r.squared else NA,
    .groups = "drop"
  ) %>%
  mutate(
    Slope_pp = round(Slope_pp, 2),
    Last_Value = round(Last_Value, 1),
    R_Squared = round(R_Squared, 3)
  )

print("Slopes for each Region x Fuel_Group (pp per year):")
print(slopes, n = Inf)

slopes_wide <- slopes %>%
  select(Fuel_Group, Region, Slope_pp) %>%
  pivot_wider(
    id_cols = Fuel_Group,
    names_from = Region,
    values_from = Slope_pp,
    values_fill = 0
  )

print("Slopes in wide format (Fuel_Group x Region):")
print(slopes_wide, n = Inf)

# ============================================================================
# Region x Fuel_Group slopes with R^2 gating
# ============================================================================
slopes_detailed <- fuel_region_share %>%
  group_by(Fuel_Group, Region) %>%
  summarise(
    Slope_pp = if (n() >= 2) coef(lm(Global_Share ~ Data_Year))[2] else NA,
    R_Squared = if (n() >= 2) summary(lm(Global_Share ~ Data_Year))$r.squared else NA,
    Std_Error = if (n() >= 2) summary(lm(Global_Share ~ Data_Year))$coefficients[2, 2] else NA,
    P_Value = if (n() >= 2) summary(lm(Global_Share ~ Data_Year))$coefficients[2, 4] else NA,
    Last_Value = Global_Share[Data_Year == max(Data_Year)],
    N_Years = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Slope_pp = round(Slope_pp, 2),
    R_Squared = round(R_Squared, 3),
    Std_Error = round(Std_Error, 3),
    P_Value = round(P_Value, 4),
    Last_Value = round(Last_Value, 1)
  )

print("=== DETAILED SLOPES WITH STATISTICS ===")
print(slopes_detailed, n = Inf)

R2_THRESHOLD <- 0.5

slopes_gated <- slopes_detailed %>%
  mutate(
    Slope_Base = ifelse(!is.na(R_Squared) & R_Squared >= R2_THRESHOLD, Slope_pp, 0),
    Slope_Plus = ifelse(!is.na(R_Squared) & R_Squared >= R2_THRESHOLD,
                        Slope_pp + 1.96 * Std_Error, 0),
    Slope_Minus = ifelse(!is.na(R_Squared) & R_Squared >= R2_THRESHOLD,
                         Slope_pp - 1.96 * Std_Error, 0),
    Trend_Quality = case_when(
      is.na(R_Squared) ~ "Insufficient Data",
      R_Squared >= 0.7 ~ "Strong",
      R_Squared >= 0.5 ~ "Moderate",
      R_Squared >= 0.3 ~ "Weak",
      TRUE ~ "Noise"
    ),
    Interpretation = case_when(
      is.na(R_Squared) ~ "Flat (insufficient data)",
      R_Squared >= 0.5 ~ paste0("Trend (R2=", round(R_Squared, 2), ")"),
      TRUE ~ paste0("Flat (R2=", round(R_Squared, 2), " < 0.5)")
    )
  ) %>%
  select(Fuel_Group, Region, Slope_Base, Slope_Plus, Slope_Minus,
         Slope_pp, R_Squared, Std_Error, Trend_Quality, Interpretation, Last_Value, N_Years)

print("=== GATED SLOPES WITH 95% CONFIDENCE BANDS ===")
print(slopes_gated, n = Inf)

slopes_gated_wide <- slopes_gated %>%
  select(Fuel_Group, Region, Slope_Base, Slope_Plus, Slope_Minus, R_Squared, Trend_Quality) %>%
  pivot_wider(
    id_cols = Fuel_Group,
    names_from = Region,
    values_from = c(Slope_Base, Slope_Plus, Slope_Minus, R_Squared, Trend_Quality),
    values_fill = list(Slope_Base = 0, Slope_Plus = 0, Slope_Minus = 0,
                       R_Squared = 0, Trend_Quality = "No Data")
  )

print("=== GATED SLOPES - WIDE FORMAT ===")
print(slopes_gated_wide, n = Inf)

trend_summary <- slopes_gated %>%
  group_by(Trend_Quality) %>%
  summarise(
    Count = n(),
    Fuel_Regions = paste(paste(Fuel_Group, Region, sep = "-"), collapse = ", ")
  ) %>%
  arrange(desc(Count))

print("=== TREND QUALITY SUMMARY ===")
print(trend_summary)

slope_lookup <- slopes_gated %>%
  select(Fuel_Group, Region,
         Slope_Base, Slope_Plus, Slope_Minus,
         Last_Value, R_Squared, Trend_Quality) %>%
  mutate(
    Projection_Base = ifelse(Trend_Quality %in% c("Strong", "Moderate"),
                             Slope_Base, 0),
    Projection_Plus = ifelse(Trend_Quality %in% c("Strong", "Moderate"),
                             Slope_Plus, 0),
    Projection_Minus = ifelse(Trend_Quality %in% c("Strong", "Moderate"),
                              Slope_Minus, 0)
  )

print("=== FINAL SLOPE LOOKUP FOR BUILD_TRAJECTORY() ===")
print(slope_lookup, n = Inf)

library(ggplot2)

p_slope_quality <- slopes_gated %>%
  ggplot(aes(x = reorder(paste(Fuel_Group, Region, sep = "-"), R_Squared),
             y = R_Squared, fill = Trend_Quality)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = R2_THRESHOLD, linetype = "dashed", color = "red") +
  geom_text(aes(label = ifelse(R_Squared > 0.1, round(R_Squared, 2), "")),
            hjust = -0.1, size = 2.5) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Strong" = "#2CA02C",
    "Moderate" = "#FFD700",
    "Weak" = "#FF7F0E",
    "Noise" = "#D62728",
    "Insufficient Data" = "#9467BD"
  )) +
  labs(
    x = "Fuel_Group - Region",
    y = "R2 (proportion of variance explained)",
    fill = "Trend Quality"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_slope_quality)

cat("\n=== PROJECTIONS FOR 2025 (USING GATED SLOPES) ===\n")

bev_2025 <- slope_lookup %>%
  filter(Fuel_Group == "BEV") %>%
  mutate(
    Projected_Base = Last_Value + Projection_Base * 1,
    Projected_Plus = Last_Value + Projection_Plus * 1,
    Projected_Minus = Last_Value + Projection_Minus * 1
  ) %>%
  select(Region, Last_Value, Projected_Base, Projected_Plus, Projected_Minus, Trend_Quality)

print("BEV - 2025 Projections by Region:")
print(bev_2025, n = Inf)

china_bev_2025 <- bev_2025 %>% filter(Region == "China")
cat("\nChina BEV: 2024 =", round(china_bev_2025$Last_Value, 1),
    "% -> 2025 Base =", round(china_bev_2025$Projected_Base, 1),
    "% (95% CI:", round(china_bev_2025$Projected_Minus, 1),
    "-", round(china_bev_2025$Projected_Plus, 1), "%)\n")

cat("\n=== SUMMARY TABLE: TRENDS USED VS HELD FLAT ===\n")

summary_table <- slope_lookup %>%
  mutate(
    Status = ifelse(Trend_Quality %in% c("Strong", "Moderate"),
                    "Trend Used", "Held Flat"),
    Projection_2025 = Last_Value + Projection_Base * 1
  ) %>%
  select(Fuel_Group, Region, Status, Trend_Quality,
         Slope_Base, R_Squared, Last_Value, Projection_2025) %>%
  arrange(Fuel_Group, desc(Status))

print(summary_table, n = Inf)

# Bridge for downstream Step 14
new_china_bev_base  <- china_bev_2025$Projected_Base
new_china_bev_plus  <- china_bev_2025$Projected_Plus
new_china_bev_minus <- china_bev_2025$Projected_Minus

cat("\nChecking bridged values:\n")
cat("Base:", new_china_bev_base, "| Plus:", new_china_bev_plus, "| Minus:", new_china_bev_minus, "\n")

# ============================================================================
# STEP 14: China BEV projection plot
# ============================================================================
china_bev_historical <- fuel_region_share %>%
  filter(Fuel_Group == "BEV", Region == "China") %>%
  select(Data_Year, Global_Share)

projection_data <- data.frame(
  Data_Year = 2025,
  Global_Share = new_china_bev_base,
  Type = "Projected"
)

ci_data <- data.frame(
  Data_Year = 2025,
  ymin = new_china_bev_minus,
  ymax = new_china_bev_plus
)

china_bev_plot_data <- china_bev_historical %>%
  mutate(Type = "Historical") %>%
  bind_rows(projection_data)

china_bev_stats <- slopes_gated %>%
  filter(Fuel_Group == "BEV", Region == "China")

cat("\n=== CHECKING china_bev_stats ===\n")
print(china_bev_stats)

if(nrow(china_bev_stats) == 0) {
  stop("No row for Fuel_Group == 'BEV' & Region == 'China' in slopes_gated. ",
       "Check that fuel_region_share has at least 3 years of China BEV data.")
}

p_china_bev_projection <- ggplot() +
  geom_line(data = china_bev_historical,
            aes(x = Data_Year, y = Global_Share),
            color = "#1F77B4", size = 1.2) +
  geom_point(data = china_bev_historical,
             aes(x = Data_Year, y = Global_Share),
             color = "#1F77B4", size = 3) +
  geom_point(data = projection_data,
             aes(x = Data_Year, y = Global_Share),
             color = "#D62728", size = 4) +
  geom_errorbar(data = ci_data,
                aes(x = Data_Year, ymin = ymin, ymax = ymax),
                width = 0.3, color = "#D62728", size = 1.2) +
  geom_text(data = projection_data,
            aes(x = Data_Year + 0.5, y = Global_Share,
                label = paste0(round(Global_Share, 1), "%")),
            hjust = 0, vjust = -0.5, size = 4, color = "#D62728", fontface = "bold") +
  geom_text(data = ci_data,
            aes(x = Data_Year, y = ymax + 1,
                label = paste0("95% CI: ", round(ymin, 1), " - ", round(ymax, 1), "%")),
            hjust = 0.5, size = 3.5, color = "#D62728") +
  labs(
    title = "China BEV Global Share: Historical Trend with 95% CI",
    subtitle = paste0("Slope = ", round(china_bev_stats$Slope_pp, 2),
                      " pp/year (R2 = ", round(china_bev_stats$R_Squared, 3),
                      ") | 2025 Projection: ", round(new_china_bev_base, 1),
                      "% (95% CI: ", round(new_china_bev_minus, 1),
                      " - ", round(new_china_bev_plus, 1), "%)"),
    x = "Year",
    y = "Share of Global BEV Sales (%)"
  ) +
  scale_x_continuous(breaks = seq(2014, 2026, 2)) +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, 5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10)
  )

print(p_china_bev_projection)

cat("\n=== FACETED PLOT: ALL BEV REGIONS ===\n")

bev_all_regions <- fuel_region_share %>%
  filter(Fuel_Group == "BEV") %>%
  select(Data_Year, Region, Global_Share)

bev_2025_data <- slope_lookup %>%
  filter(Fuel_Group == "BEV") %>%
  mutate(
    Projected_2025 = Last_Value + Projection_Base * 1,
    Projected_2025_Plus = Last_Value + Projection_Plus * 1,
    Projected_2025_Minus = Last_Value + Projection_Minus * 1
  ) %>%
  select(Region, Projected_2025, Projected_2025_Plus, Projected_2025_Minus, Trend_Quality)

p_bev_all <- bev_all_regions %>%
  ggplot(aes(x = Data_Year, y = Global_Share, color = Region)) +
  geom_line(size = 1) +
  geom_point(size = 2.5) +
  geom_point(data = bev_2025_data %>%
               left_join(bev_all_regions %>%
                           filter(Data_Year == max(Data_Year)) %>%
                           select(Region, Last_Value = Global_Share), by = "Region"),
             aes(x = 2025, y = Projected_2025), size = 3, shape = 17) +
  facet_wrap(~Region, scales = "free_y") +
  labs(
    x = "Year",
    y = "Share of Global BEV Sales (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 11)
  ) +
  scale_x_continuous(breaks = seq(2014, 2025, 3))

print(p_bev_all)

cat("\n=== TREND USAGE SUMMARY PLOT ===\n")

p_trend_usage <- slope_lookup %>%
  mutate(
    Status = ifelse(Trend_Quality %in% c("Strong", "Moderate"),
                    "Trend Used", "Held Flat"),
    Label = paste(Fuel_Group, Region, sep = "-")
  ) %>%
  ggplot(aes(x = reorder(Label, R_Squared), y = R_Squared, fill = Status)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  coord_flip() +
  scale_fill_manual(values = c("Trend Used" = "#2CA02C", "Held Flat" = "#D62728")) +
  labs(
    title = "R2 Values by Region x Fuel_Group",
    subtitle = paste("Threshold = 0.5 (Red line) | Trends above are used; below are held flat"),
    x = "Region x Fuel_Group",
    y = "R2 (proportion of variance explained)",
    fill = "Status"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_trend_usage)

cat("\n=== FINAL SUMMARY ===\n")
cat("========================================\n")
cat("CHINA BEV GLOBAL SHARE PROJECTION\n")
cat("========================================\n")
cat("Historical slope (2014-2024):", 3.62, "pp/year\n")
cat("R2:", 0.771, "(Strong trend)\n")
cat("2024 Actual:", 27.3, "%\n")
cat("2025 Projection (Base):", round(new_china_bev_base, 1), "%\n")
cat("95% Confidence Interval:", round(new_china_bev_minus, 1), "% -", round(new_china_bev_plus, 1), "%\n")
cat("========================================\n")
cat("\nNumber of trends used:",
    sum(slope_lookup$Trend_Quality %in% c("Strong", "Moderate")), "out of",
    nrow(slope_lookup), "combinations\n")
cat("Number held flat:",
    sum(!slope_lookup$Trend_Quality %in% c("Strong", "Moderate")), "out of",
    nrow(slope_lookup), "combinations\n")

# ============================================================================
# Save scenario objects
# ============================================================================
saveRDS(fuel_shares_complete, file.path(out_dir, "fuel_shares_complete.rds"))
saveRDS(natcop_targets, file.path(out_dir, "natcop_targets.rds"))
saveRDS(inflow_unique, file.path(out_dir, "inflow_unique.rds"))
saveRDS(engine_sales_distribution, file.path(out_dir, "engine_sales_distribution.rds"))
saveRDS(smmt_sales_forecast, file.path(out_dir, "smmt_sales_forecast.rds"))
saveRDS(sales_by_fuel_smmt, file.path(out_dir, "sales_by_fuel_smmt.rds"))
saveRDS(origin_by_fuel, file.path(out_dir, "origin_by_fuel.rds"))

cat("\n=== 05_scenarios.R complete ===\n")
