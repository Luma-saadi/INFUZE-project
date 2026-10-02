# ============================================================================
# 01_data_prep.R
# ----------------------------------------------------------------------------
# Loads raw DVLA/DfT vehicle licensing data, filters to licensed cars,
# attaches engine sizes and BEV kerb weights, and prepares the fuel/age
# distributions used throughout the pipeline.
#
# Depends on: 00_setup.R
# Produces  : VEH0120, VEH0220, Vcar_1, Vcar2, Vcar, Vcar_clean,
#             VBEV_File, VBEV_with_manual, VBEV_complete, VBEV_engine_sizes,
#             Vcar_for_plots, outlier_analysis, Vcar_clean_plot,
#             percentile_thresholds, Vcar_classified,
#             engine_size_table, vehicle_summary_detailed,
#             age_data, data_with_age, data_age_groups,
#             percentile_analysis, filtered_data,
#             filtered_data_with_engine, join_final_complete
# ============================================================================

# ---------------------------
# 2. DATA PREPARATION
# ---------------------------
# VEH0120 - DfT / DVLA "Vehicles at the end of the quarter by licence status,
# body type, make, generic model and mode: Great Britain."
VEH0120 <- read_csv(file.path(data_dir, "df_VEH0120_GB.csv"))

# VEH0220 - DfT / DVLA first-registration file with engine size
VEH0220 <- readxl::read_excel(file.path(data_dir, "df_VEH0220.xlsx"))

# ---------------------------------------------------------------
# 2.1 Cars only, licensed only, wide year columns 2014-2024 Q4
# ---------------------------------------------------------------
Vcar_1 <- VEH0120 %>%
  filter(BodyType == "Cars", LicenceStatus == "Licensed") %>%
  select(1:6, matches("(201[4-9]|202[0-4]) Q4")) %>%
  rename_with(~ str_remove(., " Q4"), matches("\\d{4} Q4"))

Vcar2 <- VEH0220 %>%
  filter(BodyType == "Cars", LicenceStatus == "Licensed") %>%
  rename(engine_size = `EngineSizeSimple`)

# ---------------------------------------------------------------
# 2.2 Match VEH0120 to VEH0220 engine sizes on Make/GenModel/Model
# ---------------------------------------------------------------
Vcar <- Vcar_1 %>%
  mutate(across(c(Make, GenModel, Model), ~ str_to_upper(str_trim(.x))))

Vcar2_unique <- Vcar2 %>%
  mutate(across(c(Make, GenModel, Model), ~ str_to_upper(str_trim(.x)))) %>%
  select(Make, GenModel, Model, engine_size) %>%
  distinct(Make, GenModel, Model, .keep_all = TRUE)

Vcar <- Vcar %>%
  left_join(Vcar2_unique, by = c("Make", "GenModel", "Model"))

totalvehicle2024 <- Vcar %>%
  summarise(totalfleet = sum(`2024`, na.rm = TRUE))

print(totalvehicle2024)

cat("Engine size transfer summary:\n")
cat("Original VEH0120 rows:", nrow(Vcar_1), "\n")
cat("Original VEH0220 rows:", nrow(Vcar2_unique), "\n")
cat("Original Vcar rows:", nrow(Vcar), "\n")
cat("Vcar with engine size rows:", nrow(Vcar), "\n")
cat("Rows with engine size matched:", sum(!is.na(Vcar$engine_size)), "\n")
cat("Match rate:", round(mean(!is.na(Vcar$engine_size)) * 100, 1), "%\n")

Vcar <- Vcar %>%
  mutate(Fuel = str_to_title(Fuel)) %>%
  mutate(engine_size_num = str_extract(engine_size, "\\d+\\.*\\d*") %>% as.numeric())

cat("Original Vcar rows:", nrow(Vcar), "\n")

# ---------------------------------------------------------------
# 2.3 Attach BEV DVLA revenue weight to the fleet
# ---------------------------------------------------------------
Vcar_clean <- Vcar

VBEV <- Vcar_clean %>%
  filter(Fuel == "Battery Electric") %>%
  select(-c(engine_size, engine_size_num)) %>%
  mutate(across(c(Make, Model), str_to_upper))

BEV_numer <- Vcar_clean %>%
  pivot_longer(cols = `2014`:`2024`,
               names_to = "Data_year",
               values_to = "BEV_count")

summary_BEV <- BEV_numer %>%
  filter(Fuel == "Battery Electric") %>%
  group_by(Data_year) %>%
  summarise(Vehicle_num = sum(BEV_count, na.rm = TRUE))

print(summary_BEV)

# BEV file includes the BEV DVLA_revenueWeight supplied from RCA
VBEV_File <- read.csv(file.path(data_dir, "BEVrevenueWeight2024.csv")) %>%
  mutate(across(c(Make, Model), str_to_upper))

VBEV_File_clean <- VBEV_File %>%
  filter(!is.na(DVLA_revenueWeight)) %>%
  rename(engine_size = DVLA_revenueWeight) %>%
  mutate(
    Make_clean = str_to_upper(str_trim(Make)),
    Model_clean = str_to_upper(str_trim(Model)) %>%
      str_replace_all("[^A-Z0-9]", "")
  ) %>%
  group_by(Make_clean, Model_clean) %>%
  summarise(engine_size = first(engine_size), .groups = "drop")

VBEV_File_genmodel <- VBEV_File_clean %>%
  group_by(Make_clean) %>%
  summarise(engine_size = mean(engine_size, na.rm = TRUE), .groups = "drop")

VBEV_File_model <- VBEV_File_clean %>%
  group_by(Model_clean) %>%
  summarise(engine_size = mean(engine_size, na.rm = TRUE), .groups = "drop")

VBEV_File_make <- VBEV_File_clean %>%
  group_by(Make_clean) %>%
  summarise(engine_size = mean(engine_size, na.rm = TRUE), .groups = "drop")

VBEV_with_weight <- VBEV %>%
  mutate(
    Make_clean = str_to_upper(str_trim(Make)),
    Model_clean = str_to_upper(str_trim(Model)) %>%
      str_replace_all("[^A-Z0-9]", "")
  ) %>%
  left_join(VBEV_File_clean, by = c("Make_clean", "Model_clean")) %>%
  left_join(VBEV_File_genmodel, by = "Make_clean", suffix = c("", "_genmodel")) %>%
  left_join(VBEV_File_model, by = "Model_clean", suffix = c("", "_model")) %>%
  left_join(VBEV_File_make, by = "Make_clean", suffix = c("", "_make")) %>%
  mutate(
    engine_size = coalesce(engine_size, engine_size_genmodel, engine_size_model, engine_size_make),
    Match_Level = case_when(
      !is.na(engine_size) & is.na(engine_size_genmodel) & is.na(engine_size_model) & is.na(engine_size_make) ~ "Exact",
      !is.na(engine_size_genmodel) ~ "MakeOnly",
      !is.na(engine_size_model) ~ "ModelOnly",
      !is.na(engine_size_make) ~ "MakeAverage",
      TRUE ~ "NoMatch"
    )
  )

match_summary <- VBEV_with_weight %>%
  count(Match_Level) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 2),
    Cumulative_Percentage = round(cumsum(Percentage), 2)
  )

cat("=== MATCH SUMMARY ===\n")
print(match_summary)

unmatched_percentage <- match_summary %>%
  filter(Match_Level == "NoMatch") %>%
  pull(Percentage)

cat("\n UNMATCHED RECORDS:", unmatched_percentage, "%\n")

original_matches <- match_summary %>%
  filter(Match_Level == "Exact") %>%
  pull(Percentage)

improved_matches <- 100 - unmatched_percentage

cat(" Original Exact Matches:", original_matches, "%\n")
cat(" Improved Total Matches:", improved_matches, "%\n")
cat(" Match Improvement:", round(improved_matches - original_matches, 2), "% points\n")

cat("=== MAKES COMPLETELY MISSING FROM REFERENCE DATA ===\n")

unmatched_data <- VBEV_with_weight %>% filter(Match_Level == "NoMatch")

missing_makes <- unmatched_data %>%
  distinct(Make_clean) %>%
  mutate(
    in_reference = Make_clean %in% VBEV_File_clean$Make_clean,
    record_count = sapply(Make_clean, function(x) sum(unmatched_data$Make_clean == x))
  ) %>%
  arrange(desc(record_count))

print(missing_makes)

cat("\n=== ADDING MANUAL ENGINE SIZES FOR MISSING MAKES ===\n")

manual_engine_sizes <- data.frame(
  Make_clean = c("VOLVO", "POLESTAR", "LEXUS", "ROVER", "AUSTIN",
                 "LOTUS", "TRIUMPH", "MORRIS", "ROLLS ROYCE", "FISKER"),
  manual_engine_size = c(2650, 2600, 2640, 2030, 645,
                         3150, 1350, 905, 3400, 2974)
)

VBEV_with_manual <- VBEV_with_weight %>%
  left_join(manual_engine_sizes, by = "Make_clean") %>%
  mutate(
    engine_size = coalesce(engine_size, manual_engine_size),
    Match_Level_complete = case_when(
      Match_Level != "NoMatch" ~ Match_Level,
      !is.na(manual_engine_size) ~ "Manual",
      TRUE ~ "NoMatch"
    )
  )

engine_dist <- VBEV_with_manual %>%
  filter(!is.na(engine_size)) %>%
  count(engine_size) %>%
  mutate(prob = n / sum(n))

set.seed(123)

VBEV_complete <- VBEV_with_manual %>%
  mutate(
    engine_size = ifelse(
      is.na(engine_size),
      sample(engine_dist$engine_size,
             size = sum(is.na(engine_size)),
             replace = TRUE,
             prob = engine_dist$prob),
      engine_size
    ),
    Match_Level_complete = ifelse(
      Match_Level_complete == "NoMatch",
      "Distributed",
      Match_Level_complete
    )
  )

final_complete_summary <- VBEV_complete %>%
  count(Match_Level_complete) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 2),
    Cumulative_Percentage = round(cumsum(Percentage), 2)
  )

print(final_complete_summary)

sum(is.na(VBEV_complete$engine_size))

final_unmatched <- final_complete_summary %>%
  filter(Match_Level_complete == "NoMatch") %>%
  pull(Percentage)

final_matched <- 100 - final_unmatched

cat("\n FINAL MATCH RATE:", final_matched, "%\n")
cat(" Improvement from original:", round(final_matched - original_matches, 2), "% points\n")

manual_added <- VBEV_with_manual %>%
  filter(Match_Level_complete == "Manual") %>%
  count(Make_clean, sort = TRUE)

cat("\n MANUALLY ADDED MAKES:\n")
print(manual_added)

VBEV_engine_sizes <- VBEV_complete %>%
  filter(!is.na(engine_size)) %>%
  select(Make, Model, Fuel, engine_size_BEV = engine_size) %>%
  distinct(Make, Model, Fuel, engine_size_BEV) %>%
  mutate(engine_size_BEV = as.character(engine_size_BEV))

Vcar_clean <- Vcar_clean %>%
  mutate(row_id = row_number()) %>%
  left_join(VBEV_engine_sizes, by = c("Make", "Model", "Fuel")) %>%
  mutate(
    engine_size = ifelse(Fuel == "Battery Electric",
                         coalesce(engine_size_BEV, engine_size),
                         engine_size),
    engine_size_num = ifelse(Fuel == "Battery Electric" & !is.na(engine_size_BEV),
                             as.numeric(engine_size_BEV),
                             engine_size_num)
  ) %>%
  select(- c(engine_size_BEV, row_id))

nrow(Vcar)
nrow(Vcar_clean)

VBEV_clean <- Vcar_clean %>%
  filter(Fuel == "Battery Electric")

cat("\nBEV engine_size_num summary after fix:\n")
print(summary(VBEV_clean$engine_size_num))
cat("\nNumber of BEV with NA engine_size_num:", sum(is.na(VBEV_clean$engine_size_num)), "\n")

BEV_numer <- Vcar_clean %>%
  pivot_longer(cols = `2014`:`2024`,
               names_to = "Data_year",
               values_to = "BEV_count")

summary_BEV <- BEV_numer %>%
  filter(Fuel == "Battery Electric") %>%
  group_by(Data_year) %>%
  summarise(Vehicle_num = sum(BEV_count, na.rm = TRUE))

print(summary_BEV)

# ---------------------------------------------------------------
# Arrange fuel and engine sizes
# ---------------------------------------------------------------
V_fuel_group <- Vcar_clean %>%
  select(-c(engine_size_num)) %>%
  mutate(
    engine_size_clean = case_when(
      engine_size %in% c("[x]", "[z]", "N/A", "NA", "") ~ NA_character_,
      TRUE ~ engine_size
    ),
    engine_size_numeric = as.numeric(engine_size_clean)
  ) %>%
  arrange(Fuel, engine_size_numeric) %>%
  select(-engine_size_clean)

unique_engine_sizes <- Vcar_clean %>%
  mutate(
    engine_size_clean = as.numeric(ifelse(engine_size %in% c("[x]", "[z]"), NA, engine_size)),
    engine_size_liters = engine_size_clean / 1000
  ) %>%
  group_by(Fuel) %>%
  summarise(
    unique_engine_sizes = paste(sort(unique(engine_size_clean)), collapse = ", "),
    count_unique = n_distinct(engine_size_clean),
    .groups = 'drop'
  )

print(unique_engine_sizes, n = 20)

result_before_filter <- Vcar_clean %>%
  summarise(total_fleet_2024 = sum(`2024`, na.rm = TRUE)) %>%
  print()

# ---------------------------------------------------------------
# Frequency distribution of engine sizes
# ---------------------------------------------------------------
Vcar_for_plot <- Vcar_clean %>%
  mutate(
    engine_size_numeric = case_when(
      engine_size %in% c("[x]", "[z]", "N/A", "NA", "") ~ NA_real_,
      TRUE ~ as.numeric(str_extract(engine_size, "\\d+\\.*\\d*"))
    ),
    engine_size_liters = engine_size_numeric / 1000,
    Fuel_Group = case_when(
      str_detect(Fuel, "Plug-In Hybrid Electric") ~ "PHEV",
      Fuel %in% c("Other Fuel Types", "Gas", "Fuel Cell Electric") ~ "Other",
      Fuel %in% c("Battery Electric", "Range Extended Electric") ~ "BEV",
      str_detect(Fuel, "Hybrid Electric ") ~ "HEV",
      TRUE ~ Fuel
    )
  )

# ---------------------------------------------------------------
# Impute missing engine sizes per fuel group
# ---------------------------------------------------------------
cat("Missing values BEFORE imputation:\n")
Vcar_for_plot %>%
  group_by(Fuel_Group) %>%
  summarise(
    total = n(),
    na_count = sum(is.na(engine_size_numeric)),
    na_pct = round(mean(is.na(engine_size_numeric)) * 100, 2)
  ) %>%
  print()

Vcar_for_plot <- Vcar_for_plot %>%
  mutate(
    engine_size = case_when(
      engine_size %in% c("[x]", "[z]", "N/A", "NA", "", " ") ~ NA_character_,
      TRUE ~ engine_size
    ),
    engine_size_numeric = case_when(
      engine_size %in% c("[x]", "[z]", "N/A", "NA", "", " ") ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(str_extract(engine_size, "\\d+\\.*\\d*")))
    )
  )

known_values_by_fuel <- Vcar_for_plot %>%
  filter(!is.na(engine_size_numeric)) %>%
  group_by(Fuel_Group) %>%
  summarise(
    known_values = list(engine_size_numeric),
    known_count = n(),
    .groups = 'drop'
  )

set.seed(123)

Vcar_for_plots <- Vcar_for_plot %>%
  mutate(
    row_id = row_number(),
    engine_size_numeric = case_when(
      Fuel_Group == "BEV" & is.na(engine_size_numeric) ~
        sample(known_values_by_fuel$known_values[known_values_by_fuel$Fuel_Group == "BEV"][[1]],
               size = n(), replace = TRUE)[row_id],
      Fuel_Group == "Petrol" & is.na(engine_size_numeric) ~
        sample(known_values_by_fuel$known_values[known_values_by_fuel$Fuel_Group == "Petrol"][[1]],
               size = n(), replace = TRUE)[row_id],
      Fuel_Group == "Diesel" & is.na(engine_size_numeric) ~
        sample(known_values_by_fuel$known_values[known_values_by_fuel$Fuel_Group == "Diesel"][[1]],
               size = n(), replace = TRUE)[row_id],
      Fuel_Group == "HEV" & is.na(engine_size_numeric) ~
        sample(known_values_by_fuel$known_values[known_values_by_fuel$Fuel_Group == "HEV"][[1]],
               size = n(), replace = TRUE)[row_id],
      Fuel_Group == "PHEV" & is.na(engine_size_numeric) ~
        sample(known_values_by_fuel$known_values[known_values_by_fuel$Fuel_Group == "PHEV"][[1]],
               size = n(), replace = TRUE)[row_id],
      Fuel_Group == "Other" & is.na(engine_size_numeric) ~
        sample(known_values_by_fuel$known_values[known_values_by_fuel$Fuel_Group == "Other"][[1]],
               size = n(), replace = TRUE)[row_id],
      TRUE ~ engine_size_numeric
    ),
    engine_size_liters = engine_size_numeric / 1000
  ) %>%
  select(-row_id)

cat("\n\n========================================")
cat("\nMissing values AFTER imputation (ALL FUEL GROUPS):\n")
Vcar_for_plots %>%
  group_by(Fuel_Group) %>%
  summarise(
    total = n(),
    na_count = sum(is.na(engine_size_numeric)),
    na_pct = round(mean(is.na(engine_size_numeric)) * 100, 2)
  ) %>%
  print()

cat("\nRange Extended Electric vehicles sample:\n")
Vcar_for_plots %>%
  filter(Fuel == "Range Extended Electric") %>%
  select(Make, Model, Fuel_Group, engine_size_numeric, engine_size_liters) %>%
  head(10) %>%
  print()

cat("\n=== FINAL VERIFICATION ===\n")
cat("Total missing engine sizes after imputation:",
    sum(is.na(Vcar_for_plots$engine_size_numeric)), "\n")

Vcar_clean <- Vcar_for_plots

Vcar_classified_BEV <- Vcar_for_plots %>%
  filter(Fuel_Group == "BEV") %>%
  pivot_longer(cols = `2024`:`2014`,
               names_to = "Data_year",
               values_to = "Fleet_Nu")
summary_BEV <- Vcar_classified_BEV %>%
  group_by(Data_year) %>%
  summarise(Fleet_Count = sum(Fleet_Nu))
print(summary_BEV)
n_distinct(Vcar_classified_BEV$Model)

# ---------------------------------------------------------------
# Density plot of engine sizes by fuel type
# ---------------------------------------------------------------
p1 <- ggplot(Vcar_for_plots %>% select(Fuel_Group, `2024`, engine_size_liters),
             aes(x = engine_size_liters, fill = Fuel_Group)) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Engine Size Distribution by Fuel Type",
    x = "Engine Size (Liters)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~Fuel_Group, scales = "free_y")

print(p1)

# ---------------------------------------------------------------
# IQR-based outlier analysis per fuel type
# ---------------------------------------------------------------
library(purrr)

outlier_analysis <- Vcar_for_plots %>%
  select(`2024`, Fuel, engine_size_liters) %>%
  group_by(Fuel) %>%
  summarise(
    total_vehicles = n(),
    Q1 = quantile(engine_size_liters, 0.25, na.rm = TRUE),
    Q3 = quantile(engine_size_liters, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    outlier_min = Q1 - 1.5 * IQR,
    outlier_max = Q3 + 1.5 * IQR,
    min_engine_size = min(engine_size_liters, na.rm = TRUE),
    max_engine_size = max(engine_size_liters, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    outlier_count = map2_dbl(outlier_min, outlier_max, ~ {
      Vcar_for_plots %>%
        filter(Fuel == cur_data()$Fuel) %>%
        filter(engine_size_liters < .x | engine_size_liters > .y) %>%
        nrow()
    }),
    outlier_percentage = (outlier_count / total_vehicles) * 100
  ) %>%
  select(Fuel, total_vehicles, outlier_count, outlier_percentage, min_engine_size, max_engine_size, outlier_min, outlier_max)

print(outlier_analysis)

# ---------------------------------------------------------------
# Extreme values per fuel type
# ---------------------------------------------------------------
cat("\n=== EXTREME VALUES ANALYSIS ===\n")

extreme_values <- Vcar_for_plots %>%
  select(`2024`, Fuel, engine_size_liters) %>%
  group_by(Fuel) %>%
  summarise(
    smallest_5 = paste(round(sort(unique(engine_size_liters))[1:min(5, n_distinct(engine_size_liters))], 2), collapse = ", "),
    largest_5 = paste(round(tail(sort(unique(engine_size_liters)), min(5, n_distinct(engine_size_liters))), 2), collapse = ", "),
    .groups = 'drop'
  )

print(extreme_values)

# ---------------------------------------------------------------
# Custom min/max ranges per fuel type
# ---------------------------------------------------------------
Vcar_clean_plot <- Vcar_for_plots %>%
  left_join(outlier_analysis %>% select(Fuel, outlier_min, outlier_max), by = "Fuel") %>%
  mutate(
    custom_min = case_when(
      Fuel %in% c("Petrol", "Diesel") ~ 0.5,
      str_detect(Fuel, "Hybrid") ~ 1,
      TRUE ~ outlier_min
    ),
    custom_max = case_when(
      Fuel %in% c("Petrol", "Diesel") ~ 6.5,
      str_detect(Fuel, "Hybrid") ~ 4,
      TRUE ~ outlier_max
    )
  )

cat("Rows in Vcar_for_plots:", nrow(Vcar_for_plots), "\n")
cat("Rows in Vcar_clean_plot:", nrow(Vcar_clean_plot), "\n")
cat("Rows lost:", nrow(Vcar_for_plots) - nrow(Vcar_clean_plot), "\n")

Vcar_classified_BEV <- Vcar_clean_plot %>%
  filter(Fuel_Group == "BEV") %>%
  pivot_longer(cols = `2024`:`2014`,
               names_to = "Data_year",
               values_to = "Fleet_Nu")
summary_BEV <- Vcar_classified_BEV %>%
  group_by(Data_year) %>%
  summarise(Fleet_Count = sum(Fleet_Nu))
print(summary_BEV)
n_distinct(Vcar_classified_BEV$Model)

# ---------------------------------------------------------------
# Vehicle leaves fleet per year
# ---------------------------------------------------------------
vehicle_leave_fleet <- Vcar_clean_plot %>%
  pivot_longer(cols = `2024`:`2014`,
               names_to = "Data_year",
               values_to = "Fleet_Nu") %>%
  group_by(Fuel_Group, Data_year) %>%
  summarise(
    min_out = sum(Fleet_Nu < custom_min),
    max_out = sum(Fleet_Nu > custom_max),
    count_leave = sum(Fleet_Nu < custom_min | Fleet_Nu > custom_max),
    .groups = 'drop'
  )
vehicle_leave_fleet %>% filter(Fuel_Group == "BEV")
print(vehicle_leave_fleet, n = 15)

# ---------------------------------------------------------------
# Apply filter: BEV always kept, others within custom range
# ---------------------------------------------------------------
Vcar_clean_plot <- Vcar_clean_plot %>%
  filter(
    Fuel_Group == "BEV" |
      (engine_size_liters >= custom_min & engine_size_liters <= custom_max)
  )

final_check <- Vcar_clean_plot %>%
  summarise(
    total_2024 = sum(`2024`, na.rm = TRUE),
    BEV_2024 = sum(`2024`[Fuel_Group == "BEV"], na.rm = TRUE)
  )

cat("\n=== FINAL COUNTS AFTER FILTER ===\n")
print(final_check)

Vcar_classified_BEV <- Vcar_clean_plot %>%
  filter(Fuel_Group == "BEV") %>%
  pivot_longer(cols = `2024`:`2014`,
               names_to = "Data_year",
               values_to = "Fleet_Nu")
summary_BEV <- Vcar_classified_BEV %>%
  group_by(Data_year) %>%
  summarise(Fleet_Count = sum(Fleet_Nu))
print(summary_BEV)
n_distinct(Vcar_classified_BEV$Model)

filter_summary <- Vcar_clean_plot %>%
  mutate(
    engine_size_clean = case_when(
      engine_size %in% c("[x]", "[z]", "N/A", "NA", "") ~ NA_character_,
      TRUE ~ engine_size
    ),
    engine_size_num = suppressWarnings(as.numeric(engine_size_clean)),
    engine_size_liters_fixed = engine_size_num / 1000
  ) %>%
  group_by(Fuel_Group) %>%
  summarise(
    min_engine = min(engine_size_liters_fixed, na.rm = TRUE),
    max_engine = max(engine_size_liters_fixed, na.rm = TRUE),
    count = n(),
    .groups = 'drop'
  )

print(filter_summary)

p4 <- ggplot(Vcar_clean_plot %>%
               filter(engine_size_liters >= custom_min & engine_size_liters <= custom_max) %>%
               select(-custom_min, -custom_max) %>%
               select(`2024`, Fuel_Group, engine_size_liters),
             aes(x = engine_size_liters, fill = Fuel_Group)) +
  geom_density(alpha = 0.6) +
  labs(
    x = "Engine Size (Liters)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~Fuel_Group, scales = "free_y")

print(p4)

# ---------------------------------------------------------------
# Frequency table for specific engine ranges
# ---------------------------------------------------------------
engine_range_freq <- Vcar_for_plots %>%
  select(`2024`, engine_size_liters, Fuel_Group) %>%
  mutate(
    engine_range = cut(engine_size_liters,
                       breaks = c(0, 1, 1.5, 2, 2.5, 3, 4, 6, Inf),
                       labels = c("0-1L", "1-1.5L", "1.5-2L", "2-2.5L", "2.5-3L", "3-4L", "4-6L", "6L+"),
                       include.lowest = TRUE)
  ) %>%
  count(Fuel_Group, engine_range) %>%
  group_by(Fuel_Group) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  arrange(Fuel_Group, engine_range)

print(engine_range_freq, n = 50)

# Violin plot
p5 <- ggplot(Vcar_clean_plot %>%
               filter(engine_size_liters >= custom_min & engine_size_liters <= custom_max) %>%
               select(-custom_min, -custom_max) %>%
               select(`2024`, Fuel_Group, engine_size_liters),
             aes(x = Fuel_Group, y = engine_size_liters, fill = Fuel_Group)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, alpha = 0.5) +
  labs(
    title = "Engine Size Distribution (Violin Plot)",
    x = "Fuel Type",
    y = "Engine Size (Liters)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

print(p5)

# ---------------------------------------------------------------
# Percentile thresholds per fuel type
# ---------------------------------------------------------------
percentile_thresholds <- Vcar_clean_plot %>%
  filter(engine_size_liters >= custom_min & engine_size_liters <= custom_max) %>%
  select(-custom_min, -custom_max) %>%
  select(`2024`, Fuel_Group, engine_size_liters, engine_size) %>%
  mutate(
    engine_size_clean = as.numeric(ifelse(engine_size %in% c("[x]", "[z]"), NA, engine_size)),
    engine_size_liters = engine_size_clean / 1000
  ) %>%
  filter(!is.na(engine_size_liters)) %>%
  group_by(Fuel_Group) %>%
  summarise(
    count = n(),
    p25 = quantile(engine_size_liters, 0.25, na.rm = TRUE),
    p50 = quantile(engine_size_liters, 0.50, na.rm = TRUE),
    p75 = quantile(engine_size_liters, 0.75, na.rm = TRUE),
    p90 = quantile(engine_size_liters, 0.90, na.rm = TRUE),
    .groups = 'drop'
  )

print(percentile_thresholds, n = 20)

Vcar_classified_BEV <- Vcar_clean_plot %>%
  filter(Fuel_Group == "BEV") %>%
  pivot_longer(cols = `2024`:`2014`,
               names_to = "Data_year",
               values_to = "Fleet_Nu")
summary_BEV <- Vcar_classified_BEV %>%
  group_by(Data_year) %>%
  summarise(Fleet_Count = sum(Fleet_Nu))
print(summary_BEV)

Vcar_clean_plot %>%
  filter(Fuel_Group == "BEV") %>%
  summarise(
    na_in_engine_size = sum(is.na(engine_size)),
    na_in_engine_size_numeric = sum(is.na(engine_size_numeric)),
    total_bev_rows = n()
  ) %>%
  print()

# ---------------------------------------------------------------
# Data-driven percentile-based classification per fuel type
# ---------------------------------------------------------------
Vcar_classified <- Vcar_clean_plot %>%
  mutate(engine_size_liters = engine_size_numeric / 1000) %>%
  left_join(percentile_thresholds %>% select(Fuel_Group, p25, p50, p75, p90), by = "Fuel_Group") %>%
  mutate(vehicle_class = case_when(
    Fuel_Group == "Diesel" & is.na(engine_size_liters) ~ "Unsized Diesel",
    Fuel_Group == "Diesel" & engine_size_liters <= p25 ~ "Small Diesel",
    Fuel_Group == "Diesel" & engine_size_liters > p25 & engine_size_liters <= p50 ~ "Medium Diesel",
    Fuel_Group == "Diesel" & engine_size_liters > p50 & engine_size_liters <= p75 ~ "Large Diesel",
    Fuel_Group == "Diesel" & engine_size_liters > p75 ~ "Very Large Diesel",

    Fuel_Group == "Petrol" & is.na(engine_size_liters) ~ "Unsized Petrol",
    Fuel_Group == "Petrol" & engine_size_liters <= p25 ~ "Small Petrol",
    Fuel_Group == "Petrol" & engine_size_liters > p25 & engine_size_liters <= p50 ~ "Medium Petrol",
    Fuel_Group == "Petrol" & engine_size_liters > p50 & engine_size_liters <= p75 ~ "Large Petrol",
    Fuel_Group == "Petrol" & engine_size_liters > p75 ~ "Very Large Petrol",

    Fuel_Group == "HEV" & is.na(engine_size_liters) ~ "Unsized HEV",
    Fuel_Group == "HEV" & engine_size_liters <= p25 ~ "Small HEV",
    Fuel_Group == "HEV" & engine_size_liters > p25 & engine_size_liters <= p50 ~ "Medium HEV",
    Fuel_Group == "HEV" & engine_size_liters > p50 & engine_size_liters <= p75 ~ "Large HEV",
    Fuel_Group == "HEV" & engine_size_liters > p75 ~ "Very Large HEV",

    Fuel_Group == "PHEV" & is.na(engine_size_liters) ~ "Unsized PHEV",
    Fuel_Group == "PHEV" & engine_size_liters <= p25 ~ "Small PHEV",
    Fuel_Group == "PHEV" & engine_size_liters > p25 & engine_size_liters <= p50 ~ "Medium PHEV",
    Fuel_Group == "PHEV" & engine_size_liters > p50 & engine_size_liters <= p75 ~ "Large PHEV",
    Fuel_Group == "PHEV" & engine_size_liters > p75 ~ "Very Large PHEV",

    Fuel_Group == "BEV" & is.na(engine_size_liters) ~ "Unsized BEV",
    Fuel_Group == "BEV" & engine_size_liters <= p25 ~ "Small BEV",
    Fuel_Group == "BEV" & engine_size_liters > p25 & engine_size_liters <= p50 ~ "Medium BEV",
    Fuel_Group == "BEV" & engine_size_liters > p50 & engine_size_liters <= p75 ~ "Large BEV",
    Fuel_Group == "BEV" & engine_size_liters > p75 ~ "Very Large BEV",

    Fuel_Group == "Other" ~ "Unclassified",
    Fuel_Group == "Fuel cell electric" ~ "Unclassified",
    TRUE ~ "Unclassified"
  ))

class_size_table <- Vcar_classified %>%
  select(Fuel_Group, `2024`, p25, p50, p75, p90) %>%
  group_by(Fuel_Group, p25, p50, p75, p90) %>%
  summarise(
    Fleet_2024 = sum(`2024`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(Fuel_Group)

print("=== FLEET COUNTS WITH PERCENTILES BY FUEL TYPE ===")
print(class_size_table)

write_csv_zipped(Vcar_classified, "Vcar_classified.csv")

percentile_fleet_simple <- Vcar_classified %>%
  select(Fuel_Group, `2024`, engine_size_liters, p25, p50, p75, p90) %>%
  group_by(Fuel_Group) %>%
  summarise(
    Total_Fleet = sum(`2024`, na.rm = TRUE),
    `≤p25` = sum(`2024`[engine_size_liters <= first(p25)], na.rm = TRUE),
    `≤p50` = sum(`2024`[engine_size_liters <= first(p50)], na.rm = TRUE),
    `≤p75` = sum(`2024`[engine_size_liters <= first(p75)], na.rm = TRUE),
    `≤p90` = sum(`2024`[engine_size_liters <= first(p90)], na.rm = TRUE),
    .groups = 'drop'
  )

print("=== FLEET COUNTS AT EACH PERCENTILE ===")
print(percentile_fleet_simple)

Vcar_classified_BEV <- Vcar_classified %>%
  filter(Fuel_Group == "BEV") %>%
  pivot_longer(cols = `2024`:`2014`,
               names_to = "Data_year",
               values_to = "Fleet_Nu")
summary_BEV <- Vcar_classified_BEV %>%
  group_by(Data_year) %>%
  summarise(Fleet_Count = sum(Fleet_Nu))
print(summary_BEV)

Vcar_classified %>%
  filter(Fuel_Group == "BEV") %>%
  summarise(
    missing_vehicle_class = sum(is.na(vehicle_class) | vehicle_class == ""),
    unknown_vehicle_class = sum(str_detect(tolower(vehicle_class), "unknown|unsized|other"), na.rm = TRUE),
    total_bev_rows = n()
  ) %>%
  print()

# ---------------------------------------------------------------
# Engine size distribution table
# ---------------------------------------------------------------
engine_size_table <- Vcar_clean_plot %>%
  pivot_longer(cols = "2024":"2024",
               names_to = "Data_Year",
               values_to = "Fleet_Count") %>%
  filter(Data_Year == 2024, !is.na(engine_size)) %>%
  mutate(
    engine_size_num = suppressWarnings(as.numeric(engine_size)),
    size_range = case_when(
      engine_size_num >= 100 & engine_size_num <= 1400 ~ "100 - 1400",
      engine_size_num >= 1500 & engine_size_num <= 1700 ~ "1500 - 1700",
      engine_size_num >= 1800 & engine_size_num <= 2000 ~ "1800 - 2000",
      engine_size_num >= 2100 & engine_size_num <= 3000 ~ "2100 - 3000",
      engine_size_num >= 3100 & engine_size_num <= 15000 ~ "3100 - 15000",
      is.na(engine_size_num) | engine_size_num < 100 | engine_size_num > 15000 ~ "Unclassified",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(size_range, Fuel_Group) %>%
  summarise(
    Vehicle_Count = sum(Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = size_range,
    names_from = Fuel_Group,
    values_from = Vehicle_Count,
    values_fill = 0
  ) %>%
  mutate(
    Total = rowSums(across(where(is.numeric)), na.rm = TRUE)
  ) %>%
  arrange(
    factor(size_range, levels = c(
      "100 - 1400", "1500 - 1700", "1800 - 2000",
      "2100 - 3000", "3100 - 15000", "Unclassified", "Other"
    ))
  ) %>%
  filter(Total > 0)

print("=== ENGINE SIZE DISTRIBUTION BY FUEL TYPE (2024) ===")
print(engine_size_table)

engine_size_percentage <- engine_size_table %>%
  mutate(across(where(is.numeric) & !Total,
                ~ round(. / Total * 100, 1),
                .names = "{.col}_pct"))

print("=== PERCENTAGE WITHIN EACH ENGINE SIZE CATEGORY ===")
print(engine_size_percentage)

# ============================================================================
# CAR CLASSIFICATION & THEIR AGE
# ============================================================================
# df_VEH0124_NZ / df_VEH0124_AM are internal splits of the published DfT
# VEH0124 dataset (new registrations by body type, make, model, year of first use).
library(tidyverse)

df_VEH0124_NZ <- readxl::read_excel(file.path(data_dir, "df_VEH0124_NZ (updated).xlsx"))
df_VEH0124_AM <- readxl::read_excel(file.path(data_dir, "df_VEH0124_AM (updated).xlsx"))
data <- bind_rows(df_VEH0124_NZ, df_VEH0124_AM) %>%
  filter(LicenceStatus == "Licensed", BodyType == "Cars")

totalvehicle2024 <- data %>%
  summarise(totalfleet = sum(`2024`, na.rm = TRUE))

print(totalvehicle2024)

# ---------------------------------------------------------------
# Age percentiles for cut-off decision
# ---------------------------------------------------------------
age_percentiles <- data %>%
  mutate(YearFirstUsed = as.numeric(YearFirstUsed)) %>%
  summarise(
    p10 = quantile(YearFirstUsed, 0.10, na.rm = TRUE),
    p15 = quantile(YearFirstUsed, 0.15, na.rm = TRUE)
  )

library(Hmisc)

percentile_analysis <- data %>%
  select(YearFirstUsed, `2024`) %>%
  mutate(YearFirstUsed = as.numeric(YearFirstUsed)) %>%
  summarise(
    p01 = quantile(YearFirstUsed, 0.01, na.rm = TRUE),
    p05 = quantile(YearFirstUsed, 0.05, na.rm = TRUE),
    p10 = quantile(YearFirstUsed, 0.10, na.rm = TRUE),
    p11.5 = quantile(YearFirstUsed, 0.115, na.rm = TRUE),
    p18.1 = quantile(YearFirstUsed, 0.181, na.rm = TRUE),
    p15 = quantile(YearFirstUsed, 0.15, na.rm = TRUE),
    p26 = quantile(YearFirstUsed, 0.26, na.rm = TRUE)
  )

cat("ACTUAL AGE DISTRIBUTION PERCENTILES:\n")
cat("1st percentile (oldest 1%):", percentile_analysis$p01, "\n")
cat("5th percentile (oldest 5%):", percentile_analysis$p05, "\n")
cat("10th percentile (oldest 10%):", percentile_analysis$p10, "\n")
cat("11.5th percentile (oldest 11.5%):", percentile_analysis$p11.5, "\n")
cat("18.1th percentile (oldest 18.1%):", percentile_analysis$p18.1, "\n")
cat("15th percentile (oldest 15%):", percentile_analysis$p15, "\n")
cat("26th percentile (oldest 26%):", percentile_analysis$p26, "\n")

year_1984_percentile <- data %>%
  select(YearFirstUsed, `2024`) %>%
  mutate(YearFirstUsed = as.numeric(YearFirstUsed)) %>%
  summarise(
    percentile_1984 = ecdf(YearFirstUsed)(1994) * 100
  )

cat("Year 1994 represents the:", round(year_1984_percentile$percentile_1984, 1), "percentile\n")

# ---------------------------------------------------------------
# Vehicle age distribution
# ---------------------------------------------------------------
data_with_age <- data %>%
  mutate(across(`2014`:`2024`, ~ {
    char_col <- as.character(.x)
    char_col <- ifelse(char_col %in% c("[x]", "[z]"), NA, char_col)
    as.numeric(char_col)
  })) %>%
  pivot_longer(
    cols = `2014`:`2024`,
    names_to = "Data_Year",
    values_to = "Vehicle_count"
  ) %>%
  filter(Data_Year == 2024, !is.na(Vehicle_count), Vehicle_count > 0) %>%
  mutate(
    YearFirstUsed = as.numeric(YearFirstUsed),
    Data_Year = as.numeric(Data_Year),
    Vehicle_Age = Data_Year - YearFirstUsed
  )

library(scales)

max_vehicles <- max(tapply(data_with_age$Vehicle_count,
                           data_with_age$Vehicle_Age,
                           sum, na.rm = TRUE))

# Histogram with log scale
p_age_dist <- ggplot(data_with_age, aes(x = Vehicle_Age, weight = Vehicle_count)) +
  geom_histogram(binwidth = 1, fill = "#2E86AB", color = "white", alpha = 0.7) +
  geom_vline(xintercept = c(2024 - percentile_analysis$p01,
                            2024 - percentile_analysis$p05,
                            2024 - percentile_analysis$p11.5,
                            2024 - percentile_analysis$p18.1,
                            2024 - percentile_analysis$p26),
             linetype = "dashed", color = "red", alpha = 0.5) +
  annotate("text", x = 2024 - percentile_analysis$p01, y = max_vehicles * 0.9,
           label = "P01", angle = 90, vjust = -0.5, size = 3, color = "red") +
  annotate("text", x = 2024 - percentile_analysis$p05, y = max_vehicles * 0.9,
           label = "P05", angle = 90, vjust = -0.5, size = 3, color = "red") +
  annotate("text", x = 2024 - percentile_analysis$p11.5, y = max_vehicles * 0.9,
           label = "P11.5", angle = 90, vjust = -0.5, size = 3, color = "red") +
  annotate("text", x = 2024 - percentile_analysis$p18.1, y = max_vehicles * 0.9,
           label = "P18.1", angle = 90, vjust = -0.5, size = 3, color = "red") +
  annotate("text", x = 2024 - percentile_analysis$p26, y = max_vehicles * 0.9,
           label = "P26", angle = 90, vjust = -0.5, size = 3, color = "red") +
  labs(
    x = "Vehicle Age (Years)",
    y = "Number of Vehicles (log scale)"
  ) +
  scale_x_continuous(
    limits = c(0, 124),
    breaks = seq(0, 124, by = 20)
  ) +
  scale_y_log10(
    labels = scales::comma,
    breaks = c(1000, 10000, 100000, 1000000, 10000000)
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p_age_dist)

# ---------------------------------------------------------------
# Two-panel plot
# ---------------------------------------------------------------
p1 <- ggplot(data_with_age, aes(x = Vehicle_Age, weight = Vehicle_count)) +
  geom_histogram(binwidth = 1, fill = "#2E86AB", color = "white", alpha = 0.7) +
  geom_vline(xintercept = c(2024 - percentile_analysis$p01,
                            2024 - percentile_analysis$p05,
                            2024 - percentile_analysis$p11.5,
                            2024 - percentile_analysis$p26),
             linetype = "dashed", color = "red", alpha = 0.5) +
  labs(x = "Age", y = "Vehicles", title = "Full Age Range") +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

p2 <- ggplot(data_with_age %>% filter(Vehicle_Age >= 30),
             aes(x = Vehicle_Age, weight = Vehicle_count)) +
  geom_histogram(binwidth = 1, fill = "#2E86AB", color = "white", alpha = 0.7) +
  geom_vline(xintercept = c(2024 - percentile_analysis$p01,
                            2024 - percentile_analysis$p05,
                            2024 - percentile_analysis$p11.5,
                            2024 - percentile_analysis$p26) %>%
               .[. >= 30],
             linetype = "dashed", color = "red", alpha = 0.5) +
  labs(x = "Age", y = "Vehicles", title = "Vehicles 30+ Years") +
  scale_x_continuous(limits = c(30, 80), breaks = seq(30, 80, by = 5)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()
print(p2)

p_combined <- p1 / (p1 + p2) +
  plot_annotation(
    title = "Vehicle Age Distribution (2024)",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  )

print(p_combined)

older_distribution <- data_with_age %>%
  filter(Vehicle_Age >= 30) %>%
  group_by(Vehicle_Age) %>%
  summarise(vehicles = sum(Vehicle_count), .groups = "drop") %>%
  arrange(Vehicle_Age)

cat("\n=== OLDER VEHICLES DISTRIBUTION (30+ years) ===\n")
print(older_distribution, n = 50)

data_age_groups <- data_with_age %>%
  select(Data_Year, Vehicle_Age) %>%
  mutate(
    Age_Group = cut(Vehicle_Age,
                    breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 50, 100),
                    labels = c("0-5", "6-10", "11-15", "16-20", "21-25",
                               "26-30", "31-35", "36-40", "41-50", "50+"),
                    include.lowest = TRUE)
  ) %>%
  count(Age_Group) %>%
  mutate(
    Percentage = n / sum(n) * 100,
    Cumulative = cumsum(Percentage)
  ) %>%
  filter(!Age_Group == "NA")

p1_age <- ggplot(data_with_age, aes(x = Vehicle_Age)) +
  geom_histogram(binwidth = 1, fill = "#2E86AB", alpha = 0.7) +
  labs(title = "Histogram", x = "Age", y = "Count")

p2_age <- ggplot(data_with_age, aes(x = Vehicle_Age)) +
  geom_density(fill = "#4A90E2", alpha = 0.5) +
  labs(title = "Density Plot", x = "Age", y = "Density")

p3_age <- ggplot(data_age_groups, aes(x = Age_Group, y = Percentage)) +
  geom_col(fill = "#6A9FB5", alpha = 0.7) +
  labs(title = "Age Groups", x = "Age Group", y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plot <- (p1_age + p2_age) / p3_age +
  plot_annotation(
    title = "Comprehensive Vehicle Age Analysis",
    subtitle = "Multiple views of the age distribution",
    caption = "DVLA Vehicle Licensing Data"
  )

print(combined_plot)

# ============================================================================
# CUT-OFF APPLICATION
# ============================================================================
# Based on the percentile analysis above, a 1994 cut-off (approximately the
# 26th percentile of YearFirstUsed) is applied. This removes the pre-1994 tail
# of the fleet so subsequent modelling operates on a well-sampled age range.
# ============================================================================
recommended_cutoff <- 1994

filtered_data <- data %>%
  filter(YearFirstUsed >= recommended_cutoff)

cat("FILTERING RESULTS:\n")
cat("Original vehicle count:", nrow(data), "\n")
cat("After removing vehicles before 1994:", nrow(filtered_data), "\n")
cat("Vehicles removed:", nrow(data) - nrow(filtered_data), "\n")
cat("Percentage kept:", round(nrow(filtered_data) / nrow(data) * 100, 1), "%\n")

filtered_age_summary <- filtered_data %>%
  summarise(
    oldest_vehicle = min(YearFirstUsed, na.rm = TRUE),
    newest_vehicle = max(YearFirstUsed, na.rm = TRUE),
    median_year = median(YearFirstUsed, na.rm = TRUE)
  )

cat("\nFILTERED FLEET AGE PROFILE:\n")
cat("Oldest vehicle:", filtered_age_summary$oldest_vehicle, "\n")
cat("Newest vehicle:", filtered_age_summary$newest_vehicle, "\n")
cat("Median year:", filtered_age_summary$median_year, "\n")

# ---------------------------------------------------------------
# Multi-level matching to attach engine sizes to filtered_data
# ---------------------------------------------------------------
filtered_data_with_engine <- filtered_data %>%
  mutate(
    Make_clean = str_to_upper(str_trim(Make)),
    Model_clean = str_to_upper(str_trim(Model)) %>%
      str_replace_all("[^A-Z0-9]", "")
  ) %>%
  left_join(VBEV_File_clean, by = c("Make_clean", "Model_clean")) %>%
  left_join(VBEV_File_genmodel, by = "Make_clean", suffix = c("", "_genmodel")) %>%
  left_join(VBEV_File_model, by = "Model_clean", suffix = c("", "_model")) %>%
  left_join(VBEV_File_make, by = "Make_clean", suffix = c("", "_make")) %>%
  mutate(
    engine_size_combined = coalesce(engine_size, engine_size_genmodel,
                                    engine_size_model, engine_size_make),
    Match_Level = case_when(
      !is.na(engine_size) & is.na(engine_size_genmodel) &
        is.na(engine_size_model) & is.na(engine_size_make) ~ "Exact",
      !is.na(engine_size_genmodel) ~ "MakeOnly",
      !is.na(engine_size_model) ~ "ModelOnly",
      !is.na(engine_size_make) ~ "MakeAverage",
      TRUE ~ "NoMatch"
    ),
    Fuel = ifelse(!is.na(engine_size_combined), "Battery electric", "Other Fuel Types"),
    engine_size = ifelse(Fuel == "Battery electric",
                         engine_size_combined,
                         engine_size)
  ) %>%
  select(-Make_clean, -Model_clean,
         -engine_size_genmodel, -engine_size_model, -engine_size_make,
         -engine_size_combined, -Match_Level)

match_summary <- filtered_data_with_engine %>%
  filter(Fuel == "Battery electric") %>%
  summarise(
    Count = n(),
    Percentage_of_Total = round(n() / nrow(filtered_data_with_engine) * 100, 2)
  )

cat("=== BATTERY ELECTRIC VEHICLE COUNT ===\n")
print(match_summary)

cat("\n=== FUEL DISTRIBUTION ===\n")
fuel_summary <- filtered_data_with_engine %>%
  count(Fuel) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))
print(fuel_summary)

cat("\n=== ENGINE SIZE COVERAGE ===\n")
engine_summary <- filtered_data_with_engine %>%
  summarise(
    Total_Vehicles = n(),
    With_Engine_Size = sum(!is.na(engine_size)),
    Percent_With_Engine = round(With_Engine_Size / Total_Vehicles * 100, 1)
  )
print(engine_summary)

cat("\n=== ROW COUNTS ===\n")
cat("Rows number before adding engine size:", nrow(filtered_data), "\n")
cat("Rows number after adding engine size:", nrow(filtered_data_with_engine), "\n")

# ---------------------------------------------------------------
# Find vehicles in filtered_data that are NOT in Vcar_classified
# ---------------------------------------------------------------
cat("\n3. Finding unmatched filtered_data rows...\n")

filtered_only <- filtered_data_with_engine %>%
  anti_join(
    Vcar_classified,
    by = c("BodyType", "GenModel", "Model", "Make", "LicenceStatus")
  ) %>%
  mutate(
    across(`2024`:`2014`,
           ~ suppressWarnings(as.numeric(ifelse(. %in% c("[x]", "[z]"), NA, .)))),
    YearFirstUsed = suppressWarnings(as.numeric(YearFirstUsed)),
    YearManufacture = suppressWarnings(as.numeric(YearManufacture)),
    source = "df_VEH0124_NZ&df_VEH0124_AM",
    engine_size = as.character(engine_size)
  )

cat("   Unmatched rows found:", nrow(filtered_only), "\n")

filtered_only <- filtered_only %>%
  mutate(
    Fuel = "Other Fuel Types",
    Fuel_Group = "Other"
  )

cat("=== filtered_only just built at", format(Sys.time()), "===\n")
cat("Does filtered_only have a Fuel column?", "Fuel" %in% names(filtered_only), "\n")
if ("Fuel" %in% names(filtered_only)) {
  cat("Distinct Fuel values:\n")
  print(unique(filtered_only$Fuel))
}

# ---------------------------------------------------------------
# Find vehicles in filtered_data that ARE in Vcar_classified
# Proportional fuel allocation at model level (year-stable), rather
# than first()-collapse which would assign one arbitrary fuel per model.
# ---------------------------------------------------------------
fuel_shares_by_model <- Vcar_classified %>%
  pivot_longer(cols = `2014`:`2024`, names_to = "Data_Year", values_to = "Fleet_Count") %>%
  mutate(
    Fleet_Count = suppressWarnings(as.numeric(ifelse(as.character(Fleet_Count) %in% c("[x]", "[z]"), NA, Fleet_Count)))
  ) %>%
  filter(!is.na(Fleet_Count), Fleet_Count > 0) %>%
  group_by(Make, GenModel, Model, Fuel, Fuel_Group, vehicle_class, engine_size) %>%
  summarise(variant_total = sum(Fleet_Count), .groups = "drop") %>%
  group_by(Make, GenModel, Model) %>%
  mutate(Fuel_Share = variant_total / sum(variant_total)) %>%
  ungroup() %>%
  select(Make, GenModel, Model, Fuel, Fuel_Group, vehicle_class, engine_size, Fuel_Share)

filtered_data_long <- filtered_data %>%
  mutate(across(`2014`:`2024`, as.character)) %>%
  pivot_longer(cols = `2014`:`2024`, names_to = "Data_Year", values_to = "Fleet_Count") %>%
  mutate(
    Data_Year = as.numeric(Data_Year),
    Fleet_Count = suppressWarnings(as.numeric(ifelse(Fleet_Count %in% c("[x]", "[z]"), NA, Fleet_Count))),
    YearFirstUsed = as.numeric(YearFirstUsed),
    YearManufacture = as.numeric(YearManufacture)
  ) %>%
  filter(!is.na(Fleet_Count), Fleet_Count > 0)

filtered_matched_long <- filtered_data_long %>%
  inner_join(
    fuel_shares_by_model,
    by = c("Make", "GenModel", "Model"),
    relationship = "many-to-many"
  ) %>%
  mutate(Fleet_Count = Fleet_Count * Fuel_Share) %>%
  select(-Fuel_Share) %>%
  group_by(
    BodyType, Make, GenModel, Model, LicenceStatus,
    YearFirstUsed, YearManufacture,
    Fuel, Fuel_Group, vehicle_class, engine_size, Data_Year
  ) %>%
  summarise(Fleet_Count = sum(Fleet_Count, na.rm = TRUE), .groups = "drop")

# ---------------------------------------------------------------
# BEV/PHEV/HEV manufacture-year plausibility cutoffs
# ---------------------------------------------------------------
# HEV  = 1997: Toyota Prius, the world's first mass-produced hybrid (Japan, Dec 1997).
# PHEV = 2010: Chevrolet Volt, first mass-market PHEV in Western/UK markets
#              (as Vauxhall Ampera, 2012). BYD F3DM (2008, China-only) excluded.
# BEV  = 2005: set above the earliest UK-market EVs (Peugeot 106 Electrique,
#              Citroen AX/Saxo Electrique, 1995-2005; REVA/G-Wiz, 2001-2012)
#              as a conservative floor. Empirically excludes none of these
#              genuine models from the dataset while flagging implausible
#              fuel-type misclassifications on much older vehicles.
plausibility_year <- c(BEV = 2005, PHEV = 2010, HEV = 1997)

filtered_matched_long <- filtered_matched_long %>%
  mutate(
    plausible_min_year = case_when(
      Fuel_Group == "BEV"  ~ plausibility_year["BEV"],
      Fuel_Group == "PHEV" ~ plausibility_year["PHEV"],
      Fuel_Group == "HEV"  ~ plausibility_year["HEV"],
      TRUE ~ -Inf
    ),
    is_plausible = is.na(YearManufacture) | YearManufacture >= plausible_min_year
  ) %>%
  group_by(Make, GenModel, Model, YearFirstUsed, YearManufacture, Data_Year) %>%
  mutate(
    total_before = sum(Fleet_Count),
    plausible_total = sum(Fleet_Count[is_plausible]),
    Fleet_Count = case_when(
      !is_plausible ~ 0,
      plausible_total > 0 ~ Fleet_Count * (total_before / plausible_total),
      TRUE ~ Fleet_Count
    )
  ) %>%
  ungroup() %>%
  filter(Fleet_Count > 0) %>%
  select(-plausible_min_year, -is_plausible, -total_before, -plausible_total)

cat("=== filtered_matched_long rebuilt at", format(Sys.time()), "===\n")
cat("BEV vehicles with YearManufacture < 2005:",
    sum(filtered_matched_long$Fleet_Count[filtered_matched_long$Fuel_Group == "BEV" &
                                            as.numeric(filtered_matched_long$YearManufacture) < 2005], na.rm = TRUE), "\n")
cat("Elapsed since run start:", round(difftime(Sys.time(), run_start_time, units = "secs"), 1), "sec\n")

filtered_matched_counts <- filtered_matched_long %>%
  mutate(Data_Year = as.character(Data_Year)) %>%
  pivot_wider(names_from = Data_Year, values_from = Fleet_Count, values_fill = 0) %>%
  mutate(
    engine_size = as.character(engine_size),
    source = "matched_data(VEH0124&VEH0120&0220)"
  )

for (yr in as.character(2014:2024)) {
  if (!yr %in% names(filtered_matched_counts)) {
    filtered_matched_counts[[yr]] <- 0
  }
}

filtered_matched_counts <- filtered_matched_counts %>%
  relocate(all_of(as.character(2024:2014)), .after = last_col())

cat("   Matched rows found:", nrow(filtered_matched_counts), "\n")

# ---------------------------------------------------------------
# Vehicles in Vcar_classified that are NOT in filtered_data
# ---------------------------------------------------------------
vcar_only <- Vcar_classified %>%
  anti_join(filtered_data,
            by = c("BodyType", "GenModel", "Model", "Make", "LicenceStatus")) %>%
  select(BodyType, Make, GenModel, Model, LicenceStatus,
         Fuel, Fuel_Group, vehicle_class, `2024`:`2014`) %>%
  mutate(
    across(`2024`:`2014`, as.numeric),
    YearFirstUsed = NA_real_,
    YearManufacture = NA_real_,
    source = "VEH0120&VEH0220"
  )

cat("   Vcar-only rows found:", nrow(vcar_only), "\n")

vcar_only <- vcar_only %>%
  mutate(
    plausible_min_year = case_when(
      Fuel_Group == "BEV"  ~ 2005,
      Fuel_Group == "PHEV" ~ 2010,
      Fuel_Group == "HEV"  ~ 1997,
      TRUE ~ -Inf
    ),
    is_implausible_ev = Fuel_Group %in% c("BEV", "PHEV", "HEV") &
      !is.na(YearFirstUsed) &
      YearFirstUsed < plausible_min_year,
    Fuel       = ifelse(is_implausible_ev, "Other Fuel Types", Fuel),
    Fuel_Group = ifelse(is_implausible_ev, "Other", Fuel_Group)
  ) %>%
  select(-plausible_min_year, -is_implausible_ev)

# ---------------------------------------------------------------
# Combine: filtered_matched + filtered_unmatched + vcar_only
# ---------------------------------------------------------------
join_final_complete <- bind_rows(
  filtered_matched_counts,
  filtered_only,
  vcar_only
) %>%
  mutate(
    Fuel = ifelse(is.na(Fuel), "Other Fuel Types", Fuel),
    Fuel_Group = ifelse(is.na(Fuel_Group), "Other", Fuel_Group),
    vehicle_class = ifelse(is.na(vehicle_class), "Unclassified", vehicle_class)
  )

cat("\nFull dataset with Vcar-only vehicles:", nrow(join_final_complete), "rows\n")

BE <- join_final_complete %>%
  filter(Fuel_Group == "BEV", is.na(engine_size))

print(BE)
