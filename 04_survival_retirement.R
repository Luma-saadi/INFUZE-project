# ============================================================================
# 04_survival_retirement.R
# ----------------------------------------------------------------------------
# Survival-curve estimation from DVSA MOT records, annual retirement rates,
# and the four evidence-based adjustments applied to BEV / PHEV / HEV.
# Produces the retirement_avg table consumed by every downstream scenario.
#
# Depends on: 00_setup.R, 01_data_prep.R, 02_fleet_classification.R,
#             03_origin_analysis.R
# Produces  : survival_year, survival_by_year, survival_avg_year,
#             retirement_table, retirement_table_all, retirement_avg,
#             NAEI reference tables, comparison figures
# ============================================================================

rm(list = ls())
gc()
library(tidyverse)
library(survival)
library(survminer)
library(broom)
library(data.table)
library(R.utils)
library(arrow)

# ========================
# Load stock model data
# ========================
data <- read_parquet(file.path(data_dir, "join_final_complete.parquet"),
                     stringsAsFactors = FALSE)

# ============================================================================
# Sanity check fleet totals
# ============================================================================
vehicel_numer <- data %>%
  group_by(Data_Year) %>%
  summarise(fleet = sum(Vehicle_Stock_GB, na.rm = TRUE), .groups = "drop") %>%
  print

# ============================================================================
# Fuel x region percentage table (2024)
# ============================================================================
fuel_region_table <- data %>% filter(Data_Year == 2024) %>%
  group_by(Region, Fuel_Group) %>%
  summarise(
    Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Region) %>%
  mutate(
    Region_Total = sum(Vehicles),
    Percentage = round(100 * Vehicles / Region_Total, 1)
  ) %>%
  ungroup() %>%
  select(Region, Fuel_Group, Percentage) %>%
  pivot_wider(
    names_from = Fuel_Group,
    values_from = Percentage,
    values_fill = 0
  ) %>%
  mutate(
    Total = rowSums(across(where(is.numeric)), na.rm = TRUE)
  ) %>%
  arrange(desc(Total))

cat("\n=== FUEL TYPE PERCENTAGE BY REGION ===\n")
print(fuel_region_table, n = Inf)

fuel_region_counts <- data %>% filter(Data_Year == 2024) %>%
  group_by(Region, Fuel_Group) %>%
  summarise(
    Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Fuel_Group,
    values_from = Vehicles,
    values_fill = 0
  ) %>%
  mutate(
    Total = rowSums(across(where(is.numeric)), na.rm = TRUE)
  ) %>%
  arrange(desc(Total))

cat("\n=== VEHICLE COUNTS BY REGION AND FUEL TYPE ===\n")
print(fuel_region_counts, n = Inf)

overall_fuel_mix <- data %>% filter(Data_Year == 2024) %>%
  group_by(Fuel_Group) %>%
  summarise(
    Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Share = round(100 * Vehicles / sum(Vehicles), 1)
  ) %>%
  arrange(desc(Share))

cat("\n=== OVERALL FUEL MIX (ALL REGIONS) ===\n")
print(overall_fuel_mix)

# Validation vs ACEA new-sales shares
validation_comparison <- data %>%
  filter(Data_Year == 2024, Vehicle_Age == 0) %>%
  group_by(Region) %>%
  summarise(Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Your_Model = round(100 * Vehicles / sum(Vehicles), 1),
    ACEA_2025 = case_when(
      Region == "EU" ~ 63.4,
      Region == "China" ~ 9.3,
      Region == "UK" ~ 9.5,
      Region == "Japan" ~ 5.4,
      TRUE ~ NA_real_
    ),
    Difference = Your_Model - ACEA_2025
  ) %>%
  select(Region, Your_Model, ACEA_2025, Difference)

print(validation_comparison, n = Inf)

bev_country_2024 <- data %>% filter(Fuel_Group == "BEV", Data_Year == 2024) %>%
  group_by(Region, Origin) %>% summarise(Reg = sum(Vehicle_Stock_GB), .groups = "drop") %>%
  mutate(Share = round(100 * Reg / sum(Reg), 1)) %>% arrange(desc(Reg))
cat("\n=== NEW BEV ORIGIN BY COUNTRY (2024) - paper table ===\n")
print(bev_country_2024, n = Inf)

# ============================================================================
# Regional share of new registrations by year
# ============================================================================
share_table <- data %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year, Region) %>%
  summarise(Reg = sum(Vehicle_Stock_GB, na.rm = TRUE), .groups = "drop") %>%
  group_by(Data_Year) %>%
  mutate(Share = round(100 * Reg / sum(Reg), 1)) %>%
  ungroup() %>%
  select(Data_Year, Region, Share) %>%
  pivot_wider(
    names_from = Data_Year,
    values_from = Share,
    values_fill = 0
  ) %>%
  arrange(desc(Region == "Total"))

total_row <- share_table %>%
  summarise(
    across(
      where(is.numeric),
      ~ round(sum(.x, na.rm = TRUE), 1)
    )
  ) %>%
  mutate(Region = "TOTAL")

share_table_with_total <- bind_rows(share_table, total_row)

cat("\n=== REGIONAL SHARE OF NEW REGISTRATIONS BY YEAR (%)\n")
print(share_table_with_total, n = Inf)

share_table_fuel <- data %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year, Region, Fuel_Group) %>%
  summarise(Reg = sum(Vehicle_Stock_GB, na.rm = TRUE), .groups = "drop") %>%
  group_by(Data_Year) %>%
  mutate(Share = round(100 * Reg / sum(Reg), 1)) %>%
  ungroup() %>%
  mutate(Row_Label = paste(Region, Fuel_Group, sep = " - ")) %>%
  select(Data_Year, Row_Label, Share) %>%
  pivot_wider(
    names_from = Data_Year,
    values_from = Share,
    values_fill = 0
  ) %>%
  arrange(Row_Label)

total_row <- share_table_fuel %>%
  summarise(
    across(where(is.numeric), ~ round(sum(.x, na.rm = TRUE), 1))
  ) %>%
  mutate(Row_Label = "TOTAL")

share_table_with_Fuel_total <- bind_rows(share_table_fuel, total_row)

cat("\n=== REGIONAL SHARE BY FUEL TYPE OF NEW REGISTRATIONS (%)\n")
print(share_table_with_Fuel_total, n = Inf)

bev_new_region <- data %>% filter(Fuel_Group == "BEV", Vehicle_Age == 0) %>%
  group_by(Data_Year, Region) %>% summarise(Reg = sum(Vehicle_Stock_GB), .groups = "drop") %>%
  group_by(Data_Year) %>% mutate(Share = round(100 * Reg / sum(Reg), 1)) %>% ungroup()
print(bev_new_region %>% filter(Data_Year == 2024))
saveRDS(bev_new_region, file.path(out_dir, "bev_new_region.rds"))

p <- bev_new_region %>% filter(Region %in% c("EU", "UK", "China", "Korea", "Japan")) %>%
  ggplot(aes(Data_Year, Share, colour = Region)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.4) +
  scale_x_continuous(breaks = seq(2014, 2024, 2)) +
  labs(title = "New BEV registrations by manufacturing region",
       subtitle = "China's share of new BEVs (27.6% in 2024) far exceeds its all-car share",
       x = "Build year cohort", y = "Share of new BEV registrations (%)",
       caption = "UK vehicle licensing microdata; origin keyed on year of manufacture") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
print(p)

cat("\n2024 new-BEV split: China 27.6% / EU 51.2% / UK 3.9% (consistent with ACEA all-car China 14%).\n")

p_wavy <- bev_new_region %>%
  filter(Region %in% c("EU", "UK", "China", "Korea", "Japan")) %>%
  ggplot(aes(x = Data_Year, y = Share, fill = Region, colour = Region)) +
  geom_area(alpha = 0.6, position = "stack") +
  scale_x_continuous(breaks = seq(2014, 2024, 2)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  scale_fill_manual(values = c(
    "China" = "#d62728",
    "EU" = "#1f77b4",
    "Japan" = "#2ca02c",
    "Korea" = "#9467bd",
    "UK" = "#ff7f0e"
  )) +
  scale_colour_manual(values = c(
    "China" = "#d62728",
    "EU" = "#1f77b4",
    "Japan" = "#2ca02c",
    "Korea" = "#9467bd",
    "UK" = "#ff7f0e"
  )) +
  labs(
    x = "Build year cohort",
    y = "Share of new BEV registrations (%)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p_wavy)

# ============================================================================
# Prepare data for survival / retirement estimation
# ============================================================================
data <- data %>%
  rename(Fleet_Count = Vehicle_Stock_GB) %>%
  select(Fuel, Fuel_Group, Fleet_Count, Vehicle_Age, Data_Year, vehicle_class,
         engine_size, engine_size_liters, Avg_Mileage_Calibrated,
         Region, Origin, Avg_CO2, kerb_weight_kg,
         rdw_length_mm, rdw_width_mm, rdw_height_mm)

fleet_complete <- data %>%
  filter(Vehicle_Age <= 30) %>%
  mutate(kerb_weight_kg = as.numeric(kerb_weight_kg))

sum(fleet_complete$Fleet_Count)

fleet_2024_complete <- fleet_complete %>%
  filter(Data_Year == 2024) %>%
  select(Vehicle_Age, Fuel_Group, Fleet_Count, engine_size, Avg_Mileage_Calibrated,
         Region, Origin, vehicle_class, Avg_CO2, kerb_weight_kg,
         rdw_length_mm, rdw_width_mm, rdw_height_mm) %>%
  mutate(engine_size_liters = engine_size / 1000)
print(fleet_2024_complete)
sum(fleet_2024_complete$Fleet_Count)

names(fleet_2024_complete)

summary(fleet_2024_complete$Avg_Mileage_Calibrated)

# ============================================================================
# PART 2: RETIREMENT RATES BY AGE AND FUEL (2022-2023, 2023-2024)
# ============================================================================

# 2022 -> 2023
retirement_rates_2022_2023 <- data %>%
  filter(Data_Year %in% c(2022, 2023), !is.na(Vehicle_Age), Fuel_Group != "Other") %>%
  group_by(Fuel, Fuel_Group, Vehicle_Age, Data_Year, vehicle_class) %>%
  summarise(Fleet_Count = sum(Fleet_Count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = c(Fuel, Fuel_Group, Vehicle_Age, vehicle_class),
    names_from = Data_Year,
    values_from = Fleet_Count,
    values_fill = 0
  ) %>%
  rename(Fleet_2022 = `2022`, Fleet_2023 = `2023`) %>%
  filter(Fleet_2022 > 0) %>%
  mutate(
    Survival_Rate = Fleet_2023 / Fleet_2022,
    Retirement_Rate = 1 - Survival_Rate,
    Retirement_Rate = pmin(pmax(Retirement_Rate, 0), 0.5)
  ) %>%
  filter(!is.na(Retirement_Rate))

# 2023 -> 2024
retirement_rates_2023_2024 <- data %>%
  filter(Data_Year %in% c(2023, 2024), !is.na(Vehicle_Age), Fuel_Group != "Other") %>%
  group_by(Fuel, Fuel_Group, Vehicle_Age, Data_Year, vehicle_class) %>%
  summarise(Fleet_Count = sum(Fleet_Count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = c(Fuel, Fuel_Group, Vehicle_Age, vehicle_class),
    names_from = Data_Year,
    values_from = Fleet_Count,
    values_fill = 0
  ) %>%
  rename(Fleet_2023 = `2023`, Fleet_2024 = `2024`) %>%
  filter(Fleet_2023 > 0) %>%
  mutate(
    Survival_Rate = Fleet_2024 / Fleet_2023,
    Retirement_Rate = 1 - Survival_Rate,
    Retirement_Rate = pmin(pmax(Retirement_Rate, 0), 0.5)
  ) %>%
  filter(!is.na(Retirement_Rate))

retirement_table <- bind_rows(
  retirement_rates_2022_2023 %>% mutate(Period = "2022-2023"),
  retirement_rates_2023_2024 %>% mutate(Period = "2023-2024")
) %>%
  group_by(Vehicle_Age, Fuel_Group) %>%
  summarise(
    Retirement_Rate = mean(Retirement_Rate, na.rm = TRUE),
    Survival_Rate = 1 - mean(Retirement_Rate, na.rm = TRUE),
    Observations = n(),
    .groups = "drop"
  ) %>%
  filter(Vehicle_Age <= 40)

# ============================================================================
# 2022 as base year - survival by fuel
# ============================================================================
fleet_age_year <- data %>%
  filter(Data_Year %in% c(2022, 2023)) %>%
  filter(Fuel_Group != "Other") %>%
  group_by(Vehicle_Age, Fuel_Group, Data_Year) %>%
  summarise(
    Fleet = sum(Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(Vehicle_Age, Fuel_Group),
    names_from = Data_Year,
    values_from = Fleet,
    names_prefix = "Fleet_"
  ) %>%
  arrange(Fuel_Group, Vehicle_Age)

validation_total <- fleet_age_year %>%
  group_by(Fuel_Group) %>%
  mutate(
    Survival_Rate = lead(Fleet_2023) / Fleet_2022,
    Survival_Rate = ifelse(Survival_Rate < 0.5 | Survival_Rate > 1.05, NA, Survival_Rate)
  ) %>%
  filter(!is.na(Survival_Rate)) %>%
  ungroup()

validation_total <- validation_total %>%
  mutate(
    Survival_Rate = pmin(pmax(Survival_Rate, 0.5), 0.999),
    Retirement_Rate = 1 - Survival_Rate
  )

retirement_by_fuel <- validation_total %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Cumulative_Survival = cumprod(Survival_Rate),
    Retirement_Prob_At_Age = lag(Cumulative_Survival, default = 1) * Retirement_Rate,
    Cumulative_Retirement = cumsum(Retirement_Prob_At_Age)
  ) %>%
  ungroup()

fleet_weights <- data %>%
  filter(Data_Year == 2022, Fuel_Group != "Other") %>%
  group_by(Fuel_Group) %>%
  summarise(Total_Fleet = sum(Fleet_Count, na.rm = TRUE)) %>%
  mutate(Weight = Total_Fleet / sum(Total_Fleet))

validation_total_weighted <- validation_total %>%
  left_join(fleet_weights, by = "Fuel_Group") %>%
  group_by(Vehicle_Age) %>%
  summarise(
    Survival_Rate = weighted.mean(Survival_Rate, w = Weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Survival_Rate = pmin(pmax(Survival_Rate, 0.5), 0.999),
    Retirement_Rate = 1 - Survival_Rate,
    Cumulative_Survival = cumprod(Survival_Rate),
    Retirement_Prob_At_Age = lag(Cumulative_Survival, default = 1) * Retirement_Rate,
    Cumulative_Retirement = cumsum(Retirement_Prob_At_Age)
  )

cat("\n=== KEY RETIREMENT MILESTONES BY FUEL GROUP ===\n")

fuel_milestones <- retirement_by_fuel %>%
  group_by(Fuel_Group) %>%
  summarise(
    Median_Age = first(Vehicle_Age[Cumulative_Retirement >= 0.5]),
    Age_75 = first(Vehicle_Age[Cumulative_Retirement >= 0.75]),
    Age_90 = first(Vehicle_Age[Cumulative_Retirement >= 0.90]),
    Expected_Age = sum(Vehicle_Age * Retirement_Prob_At_Age, na.rm = TRUE),
    .groups = "drop"
  )

print(fuel_milestones)

cat("\n=== OVERALL WEIGHTED RETIREMENT MILESTONES ===\n")

median_retirement_age <- validation_total_weighted %>%
  filter(Cumulative_Retirement >= 0.5) %>%
  slice(1) %>%
  pull(Vehicle_Age)

retirement_75 <- validation_total_weighted %>%
  filter(Cumulative_Retirement >= 0.75) %>%
  slice(1) %>%
  pull(Vehicle_Age)

retirement_90 <- validation_total_weighted %>%
  filter(Cumulative_Retirement >= 0.90) %>%
  slice(1) %>%
  pull(Vehicle_Age)

expected_retirement_age <- sum(
  validation_total_weighted$Vehicle_Age *
    validation_total_weighted$Retirement_Prob_At_Age,
  na.rm = TRUE
)

cat(" 75% Retirement Age:", retirement_75, "\n")
cat(" 90% Retirement Age:", retirement_90, "\n")
cat(" Expected Retirement Age:", round(expected_retirement_age, 1), "\n")

ggplot(retirement_by_fuel, aes(x = Vehicle_Age, y = Cumulative_Survival, color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(title = "Survival Curves by Fuel Group",
       x = "Vehicle Age (years)",
       y = "Survival Probability",
       color = "Fuel Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# ============================================================================
# TRUE COHORT SURVIVAL ESTIMATION (CORRECTED)
# ============================================================================
survival_year <- fleet_complete %>%
  filter(Fuel_Group %in% c("Petrol", "Diesel", "HEV", "PHEV", "BEV"),
         !is.na(Vehicle_Age)) %>%
  group_by(Fuel_Group, Data_Year, Vehicle_Age) %>%
  summarise(Fleet = sum(Fleet_Count, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(Fuel_Group, Data_Year, Vehicle_Age)

survival_year_next <- survival_year %>%
  mutate(
    Data_Year = Data_Year - 1,
    Vehicle_Age = Vehicle_Age - 1
  ) %>%
  rename(Fleet_next = Fleet)

survival_year <- survival_year %>%
  left_join(survival_year_next,
            by = c("Fuel_Group", "Data_Year", "Vehicle_Age")) %>%
  filter(Fleet > 0)

survival_year <- survival_year %>%
  mutate(
    Annual_Survival = Fleet_next / Fleet,
    Annual_Survival = pmin(pmax(Annual_Survival, 0), 1),
    Annual_Retirement = 1 - Annual_Survival
  ) %>%
  filter(!is.na(Annual_Survival))

survival_avg_year <- survival_year %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(
    Annual_Survival = mean(Annual_Survival, na.rm = TRUE),
    Annual_Retirement = mean(Annual_Retirement, na.rm = TRUE),
    Observations = n(),
    .groups = "drop"
  ) %>%
  arrange(Fuel_Group, Vehicle_Age)

survival_avg_year <- survival_avg_year %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Survival_Probability = cumprod(coalesce(Annual_Survival, 1))
  )

survival_by_year <- survival_year %>%
  group_by(Fuel_Group, Data_Year) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Survival_Probability = cumprod(coalesce(Annual_Survival, 1))
  ) %>%
  ungroup()

survival_by_year <- survival_by_year %>%
  filter(Vehicle_Age <= 25)

ggplot(survival_avg_year,
       aes(x = Vehicle_Age,
           y = Survival_Probability,
           color = Fuel_Group)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 125, 10)) +
  labs(
    title = "Cohort Survival Curves (Correct Diagonal Estimation)",
    x = "Vehicle Age (years)",
    y = "Survival Probability",
    color = "Fuel Type"
  ) +
  theme_minimal(base_size = 12)

# ============================================================================
# FIGURE 1: Petrol - all calendar years
# ============================================================================
petrol_data <- survival_by_year %>%
  filter(Fuel_Group == "Petrol",
         Data_Year >= 1998,
         Data_Year <= 2024)

p_petrol_all <- ggplot(petrol_data,
                       aes(x = Vehicle_Age,
                           y = Survival_Probability,
                           color = as.factor(Data_Year),
                           group = Data_Year)) +
  geom_line(size = 0.8) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(
    limits = c(0, 25),
    breaks = seq(0, 25, 5)
  ) +
  scale_color_viridis_d(
    name = "Calendar Year",
    option = "plasma",
    direction = -1
  ) +
  labs(
    title = "Petrol Survival Curves by Calendar Year",
    subtitle = "Each line represents a different calendar year",
    x = "Vehicle Age (years)",
    y = "Survival Probability"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    legend.key.height = unit(1, "cm"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

print(p_petrol_all)

# ============================================================================
# FIGURE 3: Both fuels, faceted by calendar year
# ============================================================================
p_period_survival <- ggplot(
  survival_by_year,
  aes(x = Vehicle_Age,
      y = Survival_Probability,
      color = Fuel_Group)
) +
  geom_line(size = 1.2) +
  facet_wrap(~Data_Year) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = seq(0, 40, 5), limits = c(0, 40)) +
  labs(
    title = "Period Survival Curves by Calendar Year",
    subtitle = "Petrol vs Diesel lifetime structure evolution",
    x = "Vehicle Age (years)",
    y = "Probability vehicle remains in fleet",
    color = "Fuel Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  ) +
  scale_color_brewer(palette = "Set1")

print(p_period_survival)

# ============================================================================
# FIGURE 4: Petrol heatmap style
# ============================================================================
p_petrol_heat <- ggplot(petrol_data,
                        aes(x = Vehicle_Age,
                            y = Survival_Probability,
                            color = Data_Year,
                            group = Data_Year)) +
  geom_line(size = 0.8) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::percent_format()
  ) +
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  scale_color_gradientn(
    colors = c("#4575B4", "#FFFFBF", "#D73027"),
    name = "Calendar\nYear"
  ) +
  labs(
    title = "Petrol Survival Curves - Color Gradient by Year",
    x = "Vehicle Age (years)",
    y = "Survival Probability"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_petrol_heat)

# ============================================================================
# NAEI 2019 cumulative survival -> annual retirement rates
# ============================================================================
naei_retirement <- data.frame(
  Vehicle_Age = 0:25,
  Petrol = c(1, 0.9975, 0.995, 0.9925, 0.99, 0.99, 0.972, 0.954, 0.936, 0.918,
             0.9, 0.81, 0.72, 0.63, 0.54, 0.45, 0.376, 0.302, 0.228, 0.154,
             0.08, 0.064, 0.048, 0.032, 0.016, 0),
  Diesel = c(1, 0.995, 0.99, 0.985, 0.98, 0.98, 0.95, 0.92, 0.89, 0.86,
             0.83, 0.744, 0.658, 0.572, 0.486, 0.4, 0.328, 0.256, 0.184, 0.112,
             0.04, 0.032, 0.024, 0.016, 0.008, 0))

naei_annual <- naei_retirement %>%
  mutate(
    Petrol_Annual = 1 - (lead(Petrol) / Petrol),
    Diesel_Annual = 1 - (lead(Diesel) / Diesel),
    Petrol_Annual = ifelse(is.na(Petrol_Annual), 1, Petrol_Annual),
    Diesel_Annual = ifelse(is.na(Diesel_Annual), 1, Diesel_Annual)
  )

naei_survival <- naei_annual %>%
  mutate(
    Petrol_Survival = Petrol,
    Diesel_Survival = Diesel
  ) %>%
  select(Vehicle_Age, Petrol_Survival, Diesel_Survival, Petrol_Annual, Diesel_Annual)

head(naei_survival)

# Compare model vs NAEI
model_survival_recent <- survival_by_year %>%
  filter(Data_Year == 2024, Fuel_Group %in% c("Petrol", "Diesel")) %>%
  select(Vehicle_Age, Fuel_Group, Model_Survival = Survival_Probability)

model_survival_avg <- survival_avg_year %>%
  filter(Fuel_Group %in% c("Petrol", "Diesel")) %>%
  group_by(Vehicle_Age, Fuel_Group) %>%
  summarise(Model_Survival = mean(Survival_Probability, na.rm = TRUE), .groups = "drop")

naei_long <- naei_survival %>%
  pivot_longer(
    cols = c(Petrol_Survival, Diesel_Survival),
    names_to = "Fuel_Group",
    values_to = "NAEI_Survival",
    names_pattern = "(.*)_Survival"
  )

comparison_data <- model_survival_avg %>%
  inner_join(naei_long, by = c("Vehicle_Age", "Fuel_Group")) %>%
  filter(Vehicle_Age <= 25)

mae_summary <- comparison_data %>%
  group_by(Fuel_Group) %>%
  summarise(
    MAE = mean(abs(Model_Survival - NAEI_Survival), na.rm = TRUE),
    Max_Age = max(Vehicle_Age)
  )

cat("\n=== MODEL vs NAEI COMPARISON (ages 0-25) ===\n")
print(mae_summary)

p_comparison <- ggplot(comparison_data, aes(x = Vehicle_Age)) +
  geom_line(aes(y = Model_Survival, color = "Model (2020-2024 avg)"), size = 1.2) +
  geom_line(aes(y = NAEI_Survival, color = "NAEI 2019"), size = 1.2, linetype = "dashed") +
  facet_wrap(~Fuel_Group) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  scale_color_manual(values = c("Model (2020-2024 avg)" = "#E41A1C",
                                "NAEI 2019" = "#377EB8")) +
  labs(
    subtitle = "Period survival (2020-2024 average) vs NAEI cumulative survival",
    x = "Vehicle Age (years)",
    y = "Survival Probability",
    color = "Source"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

print(p_comparison)

recent_years <- survival_by_year %>%
  filter(Data_Year %in% c(2018, 2019, 2020, 2021, 2022, 2023),
         Fuel_Group %in% c("Petrol", "Diesel"),
         Vehicle_Age <= 25)

p_comparison_with_variability <- ggplot() +
  geom_line(data = recent_years,
            aes(x = Vehicle_Age, y = Survival_Probability,
                group = Data_Year,
                color = "Individual years (2018-2023)"),
            alpha = 0.5, size = 0.6) +
  geom_line(data = comparison_data,
            aes(x = Vehicle_Age, y = Model_Survival, color = "Model (2020-2024 avg)"), size = 1.2) +
  geom_line(data = comparison_data,
            aes(x = Vehicle_Age, y = NAEI_Survival, color = "NAEI 2019"), size = 1.2, linetype = "dashed") +
  facet_wrap(~Fuel_Group) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  scale_color_manual(values = c("Model (2020-2024 avg)" = "#E41A1C",
                                "NAEI 2019" = "#377EB8",
                                "Individual years (2018-2023)" = "black")) +
  labs(
    x = "Vehicle Age (years)",
    y = "Survival Probability",
    color = "Source"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

print(p_comparison_with_variability)

# ============================================================================
# Retirement table based on 2020-2024 cohort survival
# ============================================================================
retirement_table_all_year <- survival_by_year %>%
  filter(
    Data_Year %in% 2021:2024,
    Fuel_Group %in% c("Petrol", "Diesel"),
    Vehicle_Age <= 30
  ) %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(
    Retirement_Rate = mean(1 - Annual_Survival, na.rm = TRUE),
    Survival_Rate   = mean(Annual_Survival, na.rm = TRUE),
    Observations    = n(),
    .groups = "drop"
  ) %>%
  arrange(Fuel_Group, Vehicle_Age)

cat("\n=== RETIREMENT RATES SUMMARY (2020-2024) ===\n")
print(retirement_table %>%
        group_by(Fuel_Group) %>%
        summarise(
          Avg_Retirement = mean(Retirement_Rate),
          Min_Retirement = min(Retirement_Rate),
          Max_Retirement = max(Retirement_Rate)
        ))

p_check <- ggplot(retirement_table_all_year, aes(x = Vehicle_Age, y = Retirement_Rate, color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 1) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.25)) +
  labs(
    title = "Retirement Rates from 2020-2024 Data",
    x = "Vehicle Age",
    y = "Retirement Rate"
  ) +
  theme_minimal()
print(p_check)

# ============================================================================
# Recalculate survival for ALL fuel types
# ============================================================================
survival_year_all <- fleet_complete %>%
  filter(!is.na(Vehicle_Age), Fuel_Group != "Other") %>%
  group_by(Fuel_Group, Vehicle_Age, Data_Year) %>%
  summarise(Fleet = sum(Fleet_Count, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(Fuel_Group, Data_Year, Vehicle_Age)

survival_year_next_all <- survival_year_all %>%
  mutate(
    Data_Year = Data_Year - 1,
    Vehicle_Age = Vehicle_Age - 1
  ) %>%
  rename(Fleet_next = Fleet)

survival_year_all <- survival_year_all %>%
  left_join(survival_year_next_all,
            by = c("Fuel_Group", "Data_Year", "Vehicle_Age")) %>%
  filter(Fleet > 0)

survival_year_all <- survival_year_all %>%
  mutate(
    Annual_Survival = Fleet_next / Fleet,
    Annual_Survival = pmin(pmax(Annual_Survival, 0), 1),
    Annual_Retirement = 1 - Annual_Survival
  ) %>%
  filter(!is.na(Annual_Survival))

survival_by_year_all <- survival_year_all

cat("\n=== FUEL TYPES IN SURVIVAL CALCULATION ===\n")
print(unique(survival_by_year_all$Fuel_Group))

# ============================================================================
# Retirement table for all fuel types (2022-2024)
# ============================================================================
retirement_table_all <- survival_by_year_all %>%
  filter(
    Data_Year %in% 2022:2024,
    Vehicle_Age <= 30
  ) %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(
    Retirement_Rate = mean(1 - Annual_Survival, na.rm = TRUE),
    Survival_Rate = mean(Annual_Survival, na.rm = TRUE),
    Observations = n_distinct(Data_Year),
    Years_covered = paste(sort(unique(Data_Year)), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    Retirement_Rate = pmin(pmax(Retirement_Rate, 0), 0.5),
    Survival_Rate = 1 - Retirement_Rate
  ) %>%
  arrange(Fuel_Group, Vehicle_Age)

all_ages <- 0:30
all_fuels <- unique(retirement_table_all$Fuel_Group)

complete_grid <- expand.grid(
  Fuel_Group = all_fuels,
  Vehicle_Age = all_ages,
  stringsAsFactors = FALSE
)

retirement_table_all <- complete_grid %>%
  left_join(retirement_table_all, by = c("Fuel_Group", "Vehicle_Age")) %>%
  arrange(Fuel_Group, Vehicle_Age)

retirement_table_all <- retirement_table_all %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Observations = zoo::na.locf(Observations, na.rm = FALSE, fromLast = FALSE),
    Observations = ifelse(is.na(Observations), 0, Observations),
    Years_covered = ifelse(is.na(Years_covered), "No data", Years_covered)
  ) %>%
  ungroup() %>%
  arrange(Fuel_Group)

retirement_table_fuel <- retirement_table_all %>%
  select(Vehicle_Age, Fuel_Group, Retirement_Rate) %>%
  pivot_wider(
    names_from  = Fuel_Group,
    values_from = Retirement_Rate
  ) %>%
  arrange(Vehicle_Age)

print(retirement_table_fuel, n = Inf)

retirement_table_fuel <- retirement_table_all %>%
  select(Vehicle_Age, Fuel_Group, Retirement_Rate) %>%
  arrange(Vehicle_Age)

ggplot(retirement_table_fuel, aes(x = Vehicle_Age, y = Retirement_Rate, color = Fuel_Group)) +
  geom_line() +
  labs(
    title = "Smoothed Retirement Rates (3-Year Moving Average)",
    x = "Vehicle Age",
    y = "Retirement Rate",
    color = "Fuel Group"
  ) +
  theme_minimal()

# ============================================================================
# JUSTIFICATION FOR RETIREMENT-RATE ADJUSTMENTS
# ============================================================================
#
# This block applies four evidence-based assumptions to the raw retirement
# rates derived from DVSA MOT records (2022-2024). Each assumption is
# justified below with its source.
#
# ----------------------------------------------------------------------------
# (1) BEV = 0.98 x Petrol for Vehicle_Age >= 13
# ----------------------------------------------------------------------------
# Source: Nguyen-Tien, V., Elliott, R. J., Strobl, E. & Zhang, C. (2024).
#   "Estimating the longevity of electric vehicles: what do 300 million MOT
#   test results tell us?" Centre for Economic Performance Discussion Paper
#   CEPDP1972, London School of Economics.
#   URL: https://cep.lse.ac.uk/_NEW/publications/abstract.asp?index=10626
#
# Key findings:
#   - Analysis of >300 million MOT records (2005-2022) for >30 million
#     vehicles registered in Great Britain.
#   - The latest BEVs now match the lifespan of petrol vehicles, despite
#     being driven more intensively.
#   - BEV average lifespan: 18.4 years; Petrol: 18.7 years.
#   - BEV reliability is improving at ~12% per model year, vs ~6.7% for
#     petrol and ~1.9% for diesel.
#
# The 0.98 multiplier reflects this near-parity (18.4 / 18.7 ~= 0.98). We
# note this is a conservative choice: because BEVs are improving faster
# than petrol, setting BEV retirement slightly below petrol is defensible,
# but we deliberately keep the gap small to avoid overstating BEV longevity.
#
# ----------------------------------------------------------------------------
# (2) PHEV = BEV (all ages)
# ----------------------------------------------------------------------------
# Source 1 (battery parity):
#   Generational (2025), analysis of >8,000 EVs from 36 brands in the UK,
#   including models up to 12 years old and 260,000 km driven:
#     - Average BEV State of Health (SoH): 94.94%
#     - Average PHEV SoH: 94.27%
#     - PHEV is 3x more likely to fall below 85% SoH (4.70% vs 1.50%)
#   URL: https://www.enel.it/en-us/blog/guide/batterie-auto-elettriche
#
# Source 2 (BEV/PHEV categorisation):
#   Nguyen-Tien et al. (2024) explicitly note that PHEVs are mechanically
#   closer to BEVs, given that PHEV lifespan -- like BEV lifespan -- is
#   primarily determined by battery durability.
#
# Rationale for applying this at ALL ages (not just 13+):
#   - PHEVs show no reliable MOT retirement observations beyond age 12;
#     PHEV rows in the raw table are NA from age 12 onward.
#   - BEV and PHEV share battery-driven failure modes and similar SoH
#     distributions, so BEV is the most defensible proxy for PHEV.
#   - Applying BEV = PHEV across the full age range avoids a spurious
#     discontinuity at the borrowing boundary.
#
# ----------------------------------------------------------------------------
# (3) Smoothing BEV/PHEV early-life noise (ages 0-12)
# ----------------------------------------------------------------------------
# Raw MOT-derived BEV/PHEV retirement rates at ages 0-12 are volatile
# because the BEV/PHEV fleet is young (average BEV age 2y6m; average PHEV
# age 3y4m at end-2024).
#
# A 5-year centred moving average is applied to BEV/PHEV only at ages 0-12
# to remove sampling noise. Petrol, Diesel, and HEV curves are left
# untouched because they are well-sampled across all ages.
#
# ----------------------------------------------------------------------------
# (4) HEV = Petrol
# ----------------------------------------------------------------------------
# HEVs retain a conventional petrol engine and show the strongest MOT
# performance of any powertrain. DVSA MOT data (2022-2024) shows HEV
# first-time failure rates are consistently lower than petrol at every
# age band (e.g., 13.67% vs 11.87% at ages 3-5, 23.79% vs 18.10% at
# ages 6-10).
#
# Since virtually all UK HEVs use petrol engines (diesel hybrids are a
# market rarity), HEV = Petrol is mechanically precise, and more accurate
# than a generic HEV = ICE average that would blend in diesel wear.
#
# ----------------------------------------------------------------------------
# CITATIONS SUMMARY
# ----------------------------------------------------------------------------
#   - Nguyen-Tien, V., Elliott, R. J., Strobl, E. & Zhang, C. (2024).
#     "Estimating the longevity of electric vehicles: what do 300 million
#     MOT test results tell us?" CEP Discussion Paper CEPDP1972, LSE.
#     https://cep.lse.ac.uk/_NEW/publications/abstract.asp?index=10626
#
#   - RAC Foundation (2025). "Average car in the UK is oldest it has ever
#     been." Analysis of DVLA licensing data, end-2024.
#     https://www.racfoundation.org/media-centre/average-car-in-the-uk-is-oldest-it-has-ever-been
#
#   - DVSA / UK Parliament (2022). "Written questions and answers -
#     MOT initial failure rates by vehicle age and fuel type, 2017-2021."
#     https://questions-statements.parliament.uk/written-questions/detail/2022-06-06/13215
#
#   - Generational (2025), via Enel Energia. "How long do electric car
#     batteries last?" Battery State-of-Health analysis of 8,000+ EVs
#     from 36 UK brands.
#     https://www.enel.it/en-us/blog/guide/batterie-auto-elettriche
# ============================================================================

# ============================================================================
# RETIREMENT RATE ADJUSTMENTS (CORRECT ORDER)
# ============================================================================

petrol_rates <- retirement_table_all %>%
  filter(Fuel_Group == "Petrol") %>%
  select(Vehicle_Age, Petrol_Rate = Retirement_Rate)

# PASS 1: Adjust BEV (age 13+) and HEV (= Petrol)
retirement_table_all <- retirement_table_all %>%
  left_join(petrol_rates, by = "Vehicle_Age") %>%
  mutate(
    Retirement_Rate = case_when(
      Fuel_Group == "BEV" & Vehicle_Age >= 13 ~ Petrol_Rate * 0.98,
      Fuel_Group == "HEV"                     ~ Petrol_Rate,
      TRUE ~ Retirement_Rate
    )
  ) %>%
  select(-Petrol_Rate)

# Now build bev_adj - it reflects the ADJUSTED BEV curve
bev_adj <- retirement_table_all %>%
  filter(Fuel_Group == "BEV") %>%
  select(Vehicle_Age, BEV_Rate_adj = Retirement_Rate)

# PASS 2: PHEV = adjusted BEV (all ages)
retirement_table_all <- retirement_table_all %>%
  left_join(bev_adj, by = "Vehicle_Age") %>%
  mutate(
    Retirement_Rate = case_when(
      Fuel_Group == "PHEV" ~ BEV_Rate_adj,
      TRUE ~ Retirement_Rate
    ),
    Retirement_Rate = ifelse(is.na(Retirement_Rate), 0.05, Retirement_Rate),
    Survival_Rate   = 1 - Retirement_Rate
  ) %>%
  select(-BEV_Rate_adj)

print(retirement_table_all)

# ============================================================================
# SMOOTH EARLY-LIFE BEV/PHEV NOISE (AGES 0-12)
# ============================================================================
retirement_table_all <- retirement_table_all %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Retirement_Rate = ifelse(
      Fuel_Group %in% c("BEV", "PHEV") & Vehicle_Age >= 6 & Vehicle_Age <= 12,
      zoo::rollapply(
        Retirement_Rate,
        width = 5,
        FUN = mean,
        fill = "extend",
        align = "center"
      ),
      Retirement_Rate
    )
  ) %>%
  ungroup() %>%
  mutate(
    Survival_Rate = 1 - Retirement_Rate
  )

retirement_table_fuel <- retirement_table_all %>%
  select(Vehicle_Age, Fuel_Group, Retirement_Rate) %>%
  pivot_wider(
    names_from  = Fuel_Group,
    values_from = Retirement_Rate
  ) %>%
  arrange(Vehicle_Age)

print(retirement_table_fuel, n = Inf)

retirement_table_fuel <- retirement_table_all %>%
  select(Vehicle_Age, Fuel_Group, Retirement_Rate) %>%
  arrange(Vehicle_Age)

ggplot(retirement_table_fuel, aes(x = Vehicle_Age, y = Retirement_Rate, color = Fuel_Group)) +
  geom_line() +
  labs(
    title = "Smoothed Retirement Rates (3-Year Moving Average)",
    x = "Vehicle Age",
    y = "Retirement Rate",
    color = "Fuel Group"
  ) +
  theme_minimal()

# ============================================================================
# FIGURE: Retirement rates by fuel type
# ============================================================================
p_retirement_all <- ggplot(retirement_table_all,
                           aes(x = Vehicle_Age, y = Retirement_Rate,
                               color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 1, alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.3)) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  labs(
    x = "Vehicle Age (years)",
    y = "Annual Retirement Rate",
    color = "Fuel Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

print(p_retirement_all)

# Average retirement and 3-year smoothing
retirement_avg <- retirement_table_all %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(
    Retirement_Rate = mean(Retirement_Rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Survival_Rate = 1 - Retirement_Rate
  )

ggplot(retirement_avg, aes(x = Vehicle_Age, y = Retirement_Rate, color = Fuel_Group)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.25)) +
  labs(
    title = "Retirement Rates: BEV = PHEV = 40% of Petrol (Age 13+)",
    x = "Vehicle Age",
    y = "Retirement Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(retirement_avg, aes(x = Vehicle_Age, y = Retirement_Rate, color = Fuel_Group)) +
  geom_line() +
  labs(title = "Raw Retirement Rates (Noisy)")

retirement_avg <- retirement_avg %>%
  group_by(Fuel_Group) %>%
  mutate(
    Retirement_Rate = zoo::rollapply(
      Retirement_Rate,
      width = 3,
      FUN = mean,
      fill = "extend",
      align = "center"
    ),
    Survival_Rate = 1 - Retirement_Rate
  ) %>%
  ungroup()

ggplot(retirement_avg, aes(x = Vehicle_Age, y = Retirement_Rate, color = Fuel_Group)) +
  geom_line() +
  labs(title = "Smoothed Retirement Rates (3-Year Moving Average)")

# ============================================================================
# THREE-PANEL RETIREMENT ANALYSIS BY FUEL TYPE
# ============================================================================
fuel_specific_analysis <- retirement_avg %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Cumulative_Survival = cumprod(Survival_Rate),
    Retirement_Prob_At_Age = lag(Cumulative_Survival, default = 1) * Retirement_Rate,
    Cumulative_Retirement = 1 - Cumulative_Survival
  ) %>%
  ungroup()

fuel_median_ages <- fuel_specific_analysis %>%
  group_by(Fuel_Group) %>%
  summarise(
    Median_Age = min(Vehicle_Age[Cumulative_Retirement >= 0.5], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Median_Age = ifelse(is.infinite(Median_Age) | is.na(Median_Age),
                        25, Median_Age)
  )

cat("\n", strrep("=", 80), "\n")
cat("MEDIAN RETIREMENT AGE BY FUEL TYPE")
cat("\n", strrep("=", 80), "\n")
print(fuel_median_ages)

p1_fuel <- ggplot(fuel_specific_analysis,
                  aes(x = Vehicle_Age, y = Cumulative_Survival,
                      color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_vline(data = fuel_median_ages,
             aes(xintercept = Median_Age, color = Fuel_Group),
             linetype = "dashed", alpha = 0.5, size = 0.8) +
  geom_text(data = fuel_median_ages,
            aes(x = Median_Age, y = 0.52,
                label = paste(round(Median_Age), "yrs"),
                color = Fuel_Group),
            size = 3.5, hjust = -0.1, vjust = 0) +
  labs(
    x = "Vehicle Age (years)",
    y = "Survival Probability",
    color = "Fuel Type") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

print(p1_fuel)

p2_fuel <- ggplot(fuel_specific_analysis,
                  aes(x = Vehicle_Age, y = Retirement_Prob_At_Age,
                      fill = Fuel_Group)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, width = 0.8) +
  labs(title = "Retirement Probability by Age and Fuel Type",
       subtitle = "Probability of retiring AT each specific age",
       x = "Vehicle Age (years)",
       y = "Retirement Probability",
       fill = "Fuel Type") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.35)) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_brewer(palette = "Set1")

print(p2_fuel)

p2_fuel_line <- ggplot(fuel_specific_analysis,
                       aes(x = Vehicle_Age, y = Retirement_Prob_At_Age,
                           color = Fuel_Group)) +
  geom_line(size = 1.2) +
  labs(title = "Retirement Probability by Age and Fuel Type (Hazard Rate)",
       subtitle = "Probability of retiring AT each specific age",
       x = "Vehicle Age (years)",
       y = "Retirement Probability",
       color = "Fuel Type") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.35)) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

print(p2_fuel_line)

p3_fuel <- ggplot(fuel_specific_analysis,
                  aes(x = Vehicle_Age, y = Cumulative_Retirement,
                      color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = c(0.5, 0.75, 0.9), linetype = "dashed",
             color = c("red", "orange", "gold"), alpha = 0.7, size = 0.8) +
  annotate("text", x = 35, y = 0.5, label = "50%", color = "red", size = 3.5) +
  annotate("text", x = 35, y = 0.75, label = "75%", color = "orange", size = 3.5) +
  annotate("text", x = 35, y = 0.9, label = "90%", color = "gold", size = 3.5) +
  labs(title = "Cumulative Retirement by Fuel Type",
       subtitle = "Percentage of vehicles retired BY each age | Dashed lines show key retirement thresholds",
       x = "Vehicle Age (years)",
       y = "Cumulative Retirement",
       color = "Fuel Type") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

print(p3_fuel)

library(patchwork)

combined_fuel_plots_vertical <- (p1_fuel / p2_fuel_line / p3_fuel) +
  plot_annotation(
    title = "Vehicle Retirement Analysis by Fuel Type",
    subtitle = paste("Median retirement ages:",
                     paste(fuel_median_ages$Fuel_Group, "=",
                           fuel_median_ages$Median_Age, "yrs", collapse = ", ")),
    caption = "Based on historical retirement rates (2022-2024) | Shows survival, hazard rates, and cumulative retirement"
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(combined_fuel_plots_vertical)

combined_fuel_plots_horizontal <- (p1_fuel | p2_fuel_line | p3_fuel) +
  plot_annotation(
    title = "Vehicle Retirement Analysis by Fuel Type",
    subtitle = paste("Median retirement ages:",
                     paste(fuel_median_ages$Fuel_Group, "=",
                           fuel_median_ages$Median_Age, "yrs", collapse = ", ")),
    caption = "Based on historical retirement rates (2022-2024)"
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(combined_fuel_plots_horizontal)

# Retirement metrics table by fuel type
fuel_retirement_metrics <- fuel_specific_analysis %>%
  group_by(Fuel_Group) %>%
  summarise(
    Median_Age = first(Vehicle_Age[Cumulative_Retirement >= 0.5]),
    Age_75 = first(Vehicle_Age[Cumulative_Retirement >= 0.75]),
    Age_90 = first(Vehicle_Age[Cumulative_Retirement >= 0.90]),
    Expected_Age = sum(Vehicle_Age * Retirement_Prob_At_Age, na.rm = TRUE),
    Survival_Age_10 = Cumulative_Survival[Vehicle_Age == 10],
    Survival_Age_20 = Cumulative_Survival[Vehicle_Age == 20],
    Peak_Retirement_Age = Vehicle_Age[which.max(Retirement_Prob_At_Age)],
    Peak_Retirement_Rate = max(Retirement_Prob_At_Age, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Expected_Age = round(Expected_Age, 1),
    Survival_Age_10 = round(Survival_Age_10 * 100, 1),
    Survival_Age_20 = round(Survival_Age_20 * 100, 1),
    Peak_Retirement_Rate = round(Peak_Retirement_Rate * 100, 1)
  )

cat("\n", strrep("=", 80), "\n")
cat("RETIREMENT METRICS BY FUEL TYPE")
cat("\n", strrep("=", 80), "\n")
print(fuel_retirement_metrics)

# Formatted table
if(require(gt, quietly = TRUE)) {
  fuel_metrics_table <- fuel_retirement_metrics %>%
    gt() %>%
    tab_header(
      title = "Vehicle Retirement Metrics by Fuel Type",
      subtitle = "Based on historical retirement rates (2022-2024)"
    ) %>%
    fmt_number(
      columns = c(Median_Age, Age_75, Age_90, Peak_Retirement_Age),
      decimals = 0
    ) %>%
    fmt_number(
      columns = c(Expected_Age),
      decimals = 1
    ) %>%
    fmt_percent(
      columns = c(Survival_Age_10, Survival_Age_20, Peak_Retirement_Rate),
      scale_values = FALSE,
      decimals = 1
    ) %>%
    cols_label(
      Fuel_Group = "Fuel Type",
      Median_Age = "Median Age (50%)",
      Age_75 = "75% Retired",
      Age_90 = "90% Retired",
      Expected_Age = "Expected Age",
      Survival_Age_10 = "Survival at Age 10",
      Survival_Age_20 = "Survival at Age 20",
      Peak_Retirement_Age = "Peak Retirement Age",
      Peak_Retirement_Rate = "Peak Retirement Rate"
    )

  print(fuel_metrics_table)
}

# ============================================================================
# THREE-PANEL RETIREMENT ANALYSIS - OVERALL
# ============================================================================
fleet_weights_2024 <- fleet_2024_complete %>%
  group_by(Fuel_Group) %>%
  summarise(Fleet_Weight = sum(Fleet_Count), .groups = "drop") %>%
  mutate(Weight = Fleet_Weight / sum(Fleet_Weight))

validation_total_weighted <- retirement_avg %>%
  left_join(fleet_weights_2024, by = "Fuel_Group") %>%
  group_by(Vehicle_Age) %>%
  summarise(
    Survival_Rate = weighted.mean(Survival_Rate, w = Weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Survival_Rate = pmin(pmax(Survival_Rate, 0.5), 0.999),
    Retirement_Rate = 1 - Survival_Rate,
    Cumulative_Survival = cumprod(Survival_Rate),
    Retirement_Prob_At_Age = lag(Cumulative_Survival, default = 1) * Retirement_Rate,
    Cumulative_Retirement = cumsum(Retirement_Prob_At_Age)
  )

median_retirement_age <- validation_total_weighted %>%
  filter(Cumulative_Retirement >= 0.5) %>%
  slice(1) %>%
  pull(Vehicle_Age)

if(length(median_retirement_age) == 0 || is.na(median_retirement_age)) {
  median_retirement_age <- 15
  warning("Median retirement age not found, using default value of 15 years")
}

p1 <- ggplot(validation_total_weighted, aes(x = Vehicle_Age, y = Cumulative_Survival)) +
  geom_line(color = "blue", size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  geom_vline(xintercept = median_retirement_age, linetype = "dashed", color = "red") +
  annotate("text", x = median_retirement_age, y = 0.5,
           label = paste("Median:", median_retirement_age, "years"),
           vjust = -0.5, hjust = -0.1, color = "red") +
  labs(title = "Vehicle Survival Curve",
       subtitle = "Percentage of vehicles still in fleet by age",
       x = "Vehicle Age (years)",
       y = "Survival Probability") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

print(p1)

p2 <- ggplot(validation_total_weighted, aes(x = Vehicle_Age, y = Retirement_Prob_At_Age)) +
  geom_bar(stat = "identity", fill = "darkred", alpha = 0.7) +
  labs(title = "Retirement Probability by Age",
       subtitle = "Probability of retiring AT each specific age",
       x = "Vehicle Age (years)",
       y = "Retirement Probability") +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0, 40)) +
  theme_minimal()

print(p2)

p3 <- ggplot(validation_total_weighted, aes(x = Vehicle_Age, y = Cumulative_Retirement)) +
  geom_line(color = "darkgreen", size = 2) +
  geom_hline(yintercept = c(0.5, 0.75, 0.9), linetype = "dashed",
             color = c("red", "orange", "yellow")) +
  labs(title = "Cumulative Retirement",
       subtitle = "Percentage of vehicles retired BY each age",
       x = "Vehicle Age (years)",
       y = "Cumulative Retirement") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

print(p3)

combined_retirement_plots <- (p1 / p2 / p3) +
  plot_annotation() +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

print(combined_retirement_plots)

# Fuel-specific survival curves
fuel_specific_survival <- retirement_avg %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Cumulative_Survival = cumprod(Survival_Rate),
    Cumulative_Retirement = 1 - Cumulative_Survival,
    Retirement_Prob_At_Age = lag(Cumulative_Survival, default = 1) * Retirement_Rate
  ) %>%
  ungroup()

fuel_median_ages <- fuel_specific_survival %>%
  group_by(Fuel_Group) %>%
  summarise(
    Median_Age = min(Vehicle_Age[Cumulative_Retirement >= 0.5], na.rm = TRUE),
    .groups = "drop"
  )

p1_fuel <- ggplot(fuel_specific_survival,
                  aes(x = Vehicle_Age, y = Cumulative_Survival, color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(title = "Survival Curves by Fuel Type",
       x = "Vehicle Age (years)",
       y = "Survival Probability",
       color = "Fuel Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

p2_fuel <- ggplot(fuel_specific_survival,
                  aes(x = Vehicle_Age, y = Retirement_Prob_At_Age, color = Fuel_Group)) +
  geom_line(size = 1.2) +
  labs(title = "Retirement Probability by Fuel Type",
       x = "Vehicle Age (years)",
       y = "Retirement Probability",
       color = "Fuel Group") +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0, 40)) +
  theme_minimal()

p3_fuel <- ggplot(fuel_specific_survival,
                  aes(x = Vehicle_Age, y = Cumulative_Retirement, color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(title = "Cumulative Retirement by Fuel Type",
       x = "Vehicle Age (years)",
       y = "Cumulative Retirement",
       color = "Fuel Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

combined_fuel_plots <- (p1_fuel | p2_fuel | p3_fuel) +
  plot_annotation() +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(combined_fuel_plots)

cat("\n", strrep("=", 80), "\n")
cat("THREE-PANEL PLOTS CREATED SUCCESSFULLY!")
cat("\n", strrep("=", 80), "\n")
cat("\nGenerated plots:\n")
cat("1. p1 - Overall survival curve with median age\n")
cat("2. p2 - Retirement probability by age (bar chart)\n")
cat("3. p3 - Cumulative retirement curve\n")
cat("4. combined_retirement_plots - Three-panel vertical layout\n")
cat("5. combined_fuel_plots - Three-panel fuel-specific comparison\n")
cat("\nKey metrics:\n")
cat("- Overall median retirement age:", round(median_retirement_age, 1), "years\n")
cat("- Fuel-specific median ages:\n")
print(fuel_median_ages)

# ============================================================================
# Fuel x year cohort survival with PHEV smoothing
# ============================================================================
all_fuels_data <- survival_by_year_all %>%
  arrange(Fuel_Group, Data_Year, Vehicle_Age) %>%
  group_by(Fuel_Group, Data_Year) %>%
  mutate(
    Survival_Rate = cumprod(Annual_Survival)
  ) %>%
  ungroup()

all_fuels_data$Smoothed <- FALSE

for(i in 1:nrow(all_fuels_data)) {
  if(all_fuels_data$Fuel_Group[i] == "PHEV") {
    age_i <- all_fuels_data$Vehicle_Age[i]
    year_i <- all_fuels_data$Data_Year[i]

    petrol_rate <- all_fuels_data$Annual_Survival[
      all_fuels_data$Fuel_Group == "Petrol" &
        all_fuels_data$Vehicle_Age == age_i &
        all_fuels_data$Data_Year == year_i
    ]

    bev_rate <- all_fuels_data$Annual_Survival[
      all_fuels_data$Fuel_Group == "BEV" &
        all_fuels_data$Vehicle_Age == age_i &
        all_fuels_data$Data_Year == year_i
    ]

    if(length(petrol_rate) > 0 && length(bev_rate) > 0) {
      all_fuels_data$Annual_Survival[i] <- 0.7 * petrol_rate + 0.3 * bev_rate
      all_fuels_data$Smoothed[i] <- TRUE
    }
  }
}

all_fuels_data <- all_fuels_data %>%
  arrange(Fuel_Group, Data_Year, Vehicle_Age) %>%
  group_by(Fuel_Group, Data_Year) %>%
  mutate(
    Survival_Rate = cumprod(Annual_Survival)
  ) %>%
  ungroup()

p_all_fuels <- ggplot(all_fuels_data,
                      aes(x = Vehicle_Age,
                          y = Survival_Rate,
                          color = as.factor(Data_Year),
                          group = Data_Year)) +
  geom_line(size = 0.8) +
  facet_wrap(~Fuel_Group) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format()
  ) +
  scale_x_continuous(
    limits = c(0, 25),
    breaks = seq(0, 25, 5)
  ) +
  scale_color_viridis_d(
    name = "Year",
    option = "plasma"
  ) +
  labs(
    title = "Survival Curves by Calendar Year and Fuel Type",
    x = "Vehicle Age (years)",
    y = "Survival Probability"
  ) +
  theme_minimal()

print(p_all_fuels)

# ============================================================================
# Save retirement_avg for downstream scenarios
# ============================================================================
saveRDS(retirement_avg, file.path(out_dir, "retirement_avg.rds"))
saveRDS(retirement_table_all, file.path(out_dir, "retirement_table_all.rds"))
saveRDS(fleet_2024_complete, file.path(out_dir, "fleet_2024_complete.rds"))
saveRDS(fleet_complete, file.path(out_dir, "fleet_complete.rds"))

cat("\n=== 04_survival_retirement.R complete ===\n")
