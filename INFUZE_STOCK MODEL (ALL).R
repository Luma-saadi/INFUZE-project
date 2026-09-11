# 1. SETUP ENVIRONMENT
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
library(openxlsx)

if (!require(zip)) install.packages("zip")
library(zip)


#Checkpoint helper — paste this once, near the top of your script

cp_report <- function(df, count_col, label, years = c(2019, 2023)) {
  totals <- df %>%
    group_by(Data_Year) %>%
    summarise(Total = sum(.data[[count_col]], na.rm = TRUE), .groups = "drop") %>%
    filter(Data_Year %in% years)
  cat("\n>>> CHECKPOINT:", label, "\n")
  print(totals)
  invisible(totals)
}



# Writes a data frame to a temp CSV, zips it, deletes the uncompressed CSV
write_csv_zipped <- function(df, filename, row.names = FALSE) {
  csv_name <- filename
  zip_name <- paste0(tools::file_path_sans_ext(filename), ".zip")
  
  write.csv(df, csv_name, row.names = row.names)
  zip::zip(zipfile = zip_name, files = csv_name)
  file.remove(csv_name)
  
  cat("Saved:", zip_name, "(", round(file.size(zip_name)/1e6, 1), "MB )\n")
}
# ---------------------------
# 2. DATA PREPARATION
# ---------------------------
# CORRECTED PATH - use the simple filename
# this data updated one and take from uk gove weppage this yaer ( 2026)
#VEH0120 <- read_csv("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\Leeds Fleet Size\\df_VEH0120_GB.csv")
# this data taken from teh gov weppage on 2025 
VEH0120 <- read_csv("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\Leeds Fleet Size\\df_VEH0120_GB (Vehicles at the end of the quarter by licence status, body type, make, generic model and mode-Great Britain ).csv")

Vcar_1 <- VEH0120 %>%
  filter(BodyType == "Cars", LicenceStatus == "Licensed") %>%select(1:6, matches("(201[4-9]|202[0-4]) Q4"))%>%
  rename_with(~ str_remove(., " Q4"), matches("\\d{4} Q4"))

# Correct path with double backslashes
# Use read_xlsx (with 'x') for .xlsx files
VEH0220 <- openxlsx::read.xlsx("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\Infuze\\Used_Files_for_Last_Analysis\\df_VEH0220.xlsx")

# open the data from Tim RCA foundation to view the vehicle kerb weight


Vcar2 <- VEH0220 %>%  filter(BodyType == "Cars", LicenceStatus == "Licensed")%>%
  rename(engine_size = `EngineSizeSimple`)

# 1️⃣ Clean column text in both datasets
Vcar <- Vcar_1 %>%
  mutate(across(c(Make, GenModel, Model), ~ str_to_upper(str_trim(.x))))

Vcar2_unique <- Vcar2 %>%
  mutate(across(c(Make, GenModel, Model), ~ str_to_upper(str_trim(.x)))) %>%
  select(Make, GenModel, Model, engine_size) %>%
  distinct(Make, GenModel, Model, .keep_all = TRUE)

# 2️⃣ Perform the join
Vcar <- Vcar %>%
  left_join(Vcar2_unique, by = c("Make", "GenModel", "Model"))

# Get just the total (single value)
totalvehicle2024 <- Vcar %>%
  summarise(totalfleet = sum(`2024`, na.rm = TRUE))

print(totalvehicle2024)

# 4️⃣ Check match rate

cat("Engine size transfer summary:\n")
cat("Original VEH0120 rows:", nrow(Vcar_1), "\n")
cat("Original VEH0220 rows:", nrow(Vcar2_unique), "\n")
cat("Original Vcar rows:", nrow(Vcar), "\n")
cat("Vcar with engine size rows:", nrow(Vcar), "\n")
cat("Rows with engine size matched:", sum(!is.na(Vcar$engine_size)), "\n")
cat("Match rate:", round(mean(!is.na(Vcar$engine_size)) * 100, 1), "%\n")


# Convert engine size to numeric
Vcar <- Vcar %>%mutate(Fuel = str_to_title(Fuel))%>%
  mutate(engine_size_num = str_extract(engine_size, "\\d+\\.*\\d*") %>% as.numeric())
cat("Original Vcar rows:", nrow(Vcar), "\n")

###########################################
# attach BEV DVLA_revenue Weight to the fleet
##########################################
# Remove NA values for the classification
Vcar_clean <- Vcar
VBEV <- Vcar_clean %>%
  filter(Fuel == "Battery Electric") %>%
  select(-c(engine_size, engine_size_num)) %>%
  mutate(across(c(Make, Model), str_to_upper))



# numer of BEV in the fleet
BEV_numer <- Vcar_clean %>%
  pivot_longer(cols = `2014`:`2024`,  # Use backticks, not quotes
               names_to = "Data_year",
               values_to = "BEV_count")  # Better column name

summary_BEV <- BEV_numer %>%filter(Fuel=="Battery Electric" )%>%
  group_by(Data_year) %>%
  summarise(Vehicle_num = sum(BEV_count, na.rm = TRUE))  # Fixed syntax

print(summary_BEV)

#------------------------------------------------
# BEV file include the BEV DVLA_revenueWeight supplied from RCA
#------------------------------------------------
VBEV_File <- read.csv("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\RAC-Tim Chatterton\\BEVrevenueWeight2024.csv") %>%
  mutate(across(c(Make, Model), str_to_upper))

# Join the DVLA_revenueWeight from VBEV_File to your main data
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

# Create additional matching levels
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
  # Try multiple matching levels
  left_join(VBEV_File_clean, by = c("Make_clean", "Model_clean")) %>%
  left_join(VBEV_File_genmodel, by = "Make_clean", suffix = c("", "_genmodel")) %>%
  left_join(VBEV_File_model, by = "Model_clean", suffix = c("", "_model")) %>%
  left_join(VBEV_File_make, by = "Make_clean", suffix = c("", "_make")) %>%
  # Use the best available match
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

# MATCH SUMMARY
match_summary <- VBEV_with_weight %>%
  count(Match_Level) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 2),
    Cumulative_Percentage = round(cumsum(Percentage), 2)
  )

cat("=== MATCH SUMMARY ===\n")
print(match_summary)

# Calculate unmatched percentage
unmatched_percentage <- match_summary %>% 
  filter(Match_Level == "NoMatch") %>% 
  pull(Percentage)

cat("\n📊 UNMATCHED RECORDS:", unmatched_percentage, "%\n")

# Show match improvement
original_matches <- match_summary %>% 
  filter(Match_Level == "Exact") %>% 
  pull(Percentage)

improved_matches <- 100 - unmatched_percentage

cat("🎯 Original Exact Matches:", original_matches, "%\n")
cat("🚀 Improved Total Matches:", improved_matches, "%\n")
cat("📈 Match Improvement:", round(improved_matches - original_matches, 2), "% points\n")

# Check which makes are completely missing from reference data - FIXED
cat("=== MAKES COMPLETELY MISSING FROM REFERENCE DATA ===\n")

# Use the actual unmatched data from VBEV_with_weight
unmatched_data <- VBEV_with_weight %>% filter(Match_Level == "NoMatch")

missing_makes <- unmatched_data %>%
  distinct(Make_clean) %>%
  mutate(
    in_reference = Make_clean %in% VBEV_File_clean$Make_clean,
    record_count = sapply(Make_clean, function(x) sum(unmatched_data$Make_clean == x))
  ) %>%
  arrange(desc(record_count))

print(missing_makes)

# Add manual engine sizes for common missing makes
cat("\n=== ADDING MANUAL ENGINE SIZES FOR MISSING MAKES ===\n")

manual_engine_sizes <- data.frame(
  Make_clean = c("VOLVO", "POLESTAR", "LEXUS", "ROVER", "AUSTIN", 
                 "LOTUS", "TRIUMPH", "MORRIS", "ROLLS ROYCE", "FISKER"),
  # Values updated to Revenue Weight (Gross Vehicle Weight) in "kg"
  manual_engine_size = c(2650, 2600, 2640, 2030, 645, # manual_engine_size == Revenue weight
                         3150, 1350, 905, 3400, 2974)   # Electric = 0 for Fisker
)

# Apply manual revenue weight

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

#distribution based on known values

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

final_complete_summary  <- VBEV_complete %>%
  count(Match_Level_complete) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 2),
    Cumulative_Percentage = round(cumsum(Percentage), 2)
  )

print(final_complete_summary)

sum(is.na(VBEV_complete$engine_size))





# Calculate final match rate
final_unmatched <- final_complete_summary %>% 
  filter(Match_Level_complete == "NoMatch") %>% 
  pull(Percentage)

final_matched <- 100 - final_unmatched

cat("\n🎯 FINAL MATCH RATE:", final_matched, "%\n")
cat("📈 Improvement from original:", round(final_matched - original_matches, 2), "% points\n")

# Show which makes we manually added
manual_added <- VBEV_with_manual %>%
  filter(Match_Level_complete == "Manual") %>%
  count(Make_clean, sort = TRUE)

cat("\n📊 MANUALLY ADDED MAKES:\n")
print(manual_added)
#-----------------------------------------------------------
# Prepare the BEV data with the matched engine sizes
VBEV_engine_sizes <- VBEV_complete %>%
  filter(!is.na(engine_size)) %>%
  select(Make, Model, Fuel, engine_size_BEV = engine_size) %>%
  distinct(Make, Model, Fuel, engine_size_BEV)  %>%
  mutate(engine_size_BEV = as.character(engine_size_BEV))  # Convert to character

# Join and update engine sizes

Vcar_clean <- Vcar_clean %>%
  mutate(row_id = row_number()) %>%
  left_join(VBEV_engine_sizes, by = c("Make", "Model", "Fuel")) %>%
  mutate(
    # Update engine_size for BEV vehicles
    engine_size = ifelse(Fuel == "Battery Electric", 
                         coalesce(engine_size_BEV, engine_size), 
                         engine_size),
    # Also update engine_size_num for BEV vehicles
    engine_size_num = ifelse(Fuel == "Battery Electric" & !is.na(engine_size_BEV),
                             as.numeric(engine_size_BEV),
                             engine_size_num)
  ) %>%
  select(- c(engine_size_BEV,row_id))
nrow(Vcar)
nrow(Vcar_clean)



# make sure i am not losing BEV after matching 
VBEV_clean <- Vcar_clean %>%
  filter(Fuel == "Battery Electric") 

cat("\nBEV engine_size_num summary after fix:\n")
print(summary(VBEV_clean$engine_size_num))
cat("\nNumber of BEV with NA engine_size_num:", sum(is.na(VBEV_clean$engine_size_num)), "\n")


#------------------------------------------------------------
# number of BEV in the fleet
BEV_numer <- Vcar_clean %>%
  pivot_longer(cols = `2014`:`2024`,  # Use backticks, not quotes
               names_to = "Data_year",
               values_to = "BEV_count")  # Better column name

summary_BEV <- BEV_numer %>%filter(Fuel=="Battery Electric" )%>%
  group_by(Data_year) %>%
  summarise(Vehicle_num = sum(BEV_count, na.rm = TRUE))  # Fixed syntax

print(summary_BEV)


#--------------------------------------------------------------


# arrange fuel and engine sizes'
V_fuel_group <- Vcar_clean %>%
  select(-c( engine_size_num)) %>%
  mutate(
    engine_size_clean = case_when(
      engine_size %in% c("[x]", "[z]", "N/A", "NA", "") ~ NA_character_,
      TRUE ~ engine_size
    ),
    engine_size_numeric = as.numeric(engine_size_clean)
  ) %>%
  arrange(Fuel, engine_size_numeric) %>%
  select(-engine_size_clean)  # Remove temporary column

# Clean the engine sizes and see unique values
unique_engine_sizes <- Vcar_clean %>%
  mutate(
    # Convert to numeric, treating [x] and [z] as NA
    engine_size_clean = as.numeric(ifelse(engine_size %in% c("[x]", "[z]"), NA, engine_size)),
    # Convert cc to liters for easier interpretation
    engine_size_liters = engine_size_clean / 1000
  ) %>%
  group_by(Fuel) %>%
  summarise(
    unique_engine_sizes = paste(sort(unique(engine_size_clean)), collapse = ", "),
    count_unique = n_distinct(engine_size_clean),
    .groups = 'drop'
  )

# Print the correct dataframe
print(unique_engine_sizes, n = 20)

result_before_filter <- Vcar_clean %>%
  summarise(total_fleet_2024 = sum(`2024`, na.rm = TRUE))%>%print()





# --------------------------------------------
# FREQUENCY DISTRIBUTION PLOTS OF ENGINE SIZES
# --------------------------------------------

# 1. Create the main frequency distribution plot


# First, prepare the data with engine sizes in liters
# First, create the plotting dataframe with proper engine size handling
Vcar_for_plot <- Vcar_clean %>%
  mutate(
    # Convert engine_size to numeric, handling special codes
    engine_size_numeric = case_when(
      engine_size %in% c("[x]", "[z]", "N/A", "NA", "") ~ NA_real_,
      TRUE ~ as.numeric(str_extract(engine_size, "\\d+\\.*\\d*"))
    ),
    engine_size_liters = engine_size_numeric / 1000,  # Convert cc to liters
    # Create new fuel groups
    Fuel_Group = case_when(
      str_detect(Fuel, "Plug-In Hybrid Electric") ~ "PHEV",
      Fuel %in% c("Other Fuel Types", "Gas", "Fuel Cell Electric") ~ "Other",
      Fuel %in% c("Battery Electric", "Range Extended Electric") ~ "BEV",
      str_detect(Fuel, "Hybrid Electric ") ~ "HEV",
      TRUE ~ Fuel
    )
  )



#-----------------------------------------------------------
# Impute missing engine sizes for BEV only
#-----------------------------------------------------------

# Check current missing values by fuel group
cat("Missing values BEFORE imputation:\n")
Vcar_for_plot %>%
  group_by(Fuel_Group) %>%
  summarise(
    total = n(),
    na_count = sum(is.na(engine_size_numeric)),
    na_pct = round(mean(is.na(engine_size_numeric)) * 100, 2)
  ) %>%
  print()

Vcar_for_plot<- Vcar_for_plot %>%
  mutate(
    # Convert special codes and empty strings to NA in the original engine_size
    engine_size = case_when(
      engine_size %in% c("[x]", "[z]", "N/A", "NA", "", " ") ~ NA_character_,
      TRUE ~ engine_size
    ),
    # Also ensure engine_size_numeric reflects these changes
    engine_size_numeric = case_when(
      engine_size %in% c("[x]", "[z]", "N/A", "NA", "", " ") ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(str_extract(engine_size, "\\d+\\.*\\d*")))
    )
  )

# Create distribution from KNOWN engine sizes for ALL fuel groups
# First, get known values for each fuel group
known_values_by_fuel <- Vcar_for_plot %>%
  filter(!is.na(engine_size_numeric)) %>%
  group_by(Fuel_Group) %>%
  summarise(
    known_values = list(engine_size_numeric),
    known_count = n(),
    .groups = 'drop'
  )

# Set seed for reproducibility
set.seed(123)

# Impute missing values for ALL fuel groups
Vcar_for_plots <- Vcar_for_plot %>%
  mutate(
    # Create a row number for sampling
    row_id = row_number(),
    
    # Impute engine_size_numeric for ALL fuel groups with missing values
    engine_size_numeric = case_when(
      # If BEV and missing, impute from BEV known values
      Fuel_Group == "BEV" & is.na(engine_size_numeric) ~ 
        sample(
          known_values_by_fuel$known_values[known_values_by_fuel$Fuel_Group == "BEV"][[1]],
          size = n(),
          replace = TRUE
        )[row_id],
      
      # If Petrol and missing, impute from Petrol known values
      Fuel_Group == "Petrol" & is.na(engine_size_numeric) ~ 
        sample(
          known_values_by_fuel$known_values[known_values_by_fuel$Fuel_Group == "Petrol"][[1]],
          size = n(),
          replace = TRUE
        )[row_id],
      
      # If Diesel and missing, impute from Diesel known values
      Fuel_Group == "Diesel" & is.na(engine_size_numeric) ~ 
        sample(
          known_values_by_fuel$known_values[known_values_by_fuel$Fuel_Group == "Diesel"][[1]],
          size = n(),
          replace = TRUE
        )[row_id],
      
      # If HEV and missing, impute from HEV known values
      Fuel_Group == "HEV" & is.na(engine_size_numeric) ~ 
        sample(
          known_values_by_fuel$known_values[known_values_by_fuel$Fuel_Group == "HEV"][[1]],
          size = n(),
          replace = TRUE
        )[row_id],
      
      # If PHEV and missing, impute from PHEV known values
      Fuel_Group == "PHEV" & is.na(engine_size_numeric) ~ 
        sample(
          known_values_by_fuel$known_values[known_values_by_fuel$Fuel_Group == "PHEV"][[1]],
          size = n(),
          replace = TRUE
        )[row_id],
      
      # If Other and missing, impute from Other known values
      Fuel_Group == "Other" & is.na(engine_size_numeric) ~ 
        sample(
          known_values_by_fuel$known_values[known_values_by_fuel$Fuel_Group == "Other"][[1]],
          size = n(),
          replace = TRUE
        )[row_id],
      
      # Otherwise keep original value
      TRUE ~ engine_size_numeric
    ),
    
    # Recalculate engine_size_liters
    engine_size_liters = engine_size_numeric / 1000
  ) %>%
  select(-row_id)  # Remove temporary row_id column

# Verify imputation worked for ALL fuel groups
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

# Check the specific Range Extended Electric vehicles
cat("\nRange Extended Electric vehicles sample:\n")
Vcar_for_plots %>%
  filter(Fuel == "Range Extended Electric") %>%
  select(Make, Model, Fuel_Group, engine_size_numeric, engine_size_liters) %>%
  head(10) %>%
  print()

# Quick verification that all missing values are gone
cat("\n=== FINAL VERIFICATION ===\n")
cat("Total missing engine sizes after imputation:", 
    sum(is.na(Vcar_for_plots$engine_size_numeric)), "\n")

# Show sample of imputed values for each fuel group
cat("\n=== SAMPLE OF IMPUTED VALUES ===\n")
for(fg in unique(Vcar_for_plots$Fuel_Group)) {
  
  # Get a few rows that were originally missing for this fuel group
  originally_missing <- Vcar_for_plot %>%
    filter(Fuel_Group == fg, is.na(engine_size_numeric)) %>%
    select(Make, Model) %>%
    head(3)
  
  if(nrow(originally_missing) > 0) {
    cat("\n", fg, "- Originally missing, now imputed:\n")
    
    imputed_examples <- Vcar_for_plots %>%
      inner_join(originally_missing, by = c("Make", "Model")) %>%
      select(Make, Model, engine_size_numeric, engine_size_liters)
    
    print(imputed_examples)
  }
}

# Rename back to Vcar_clean to maintain your pipeline
Vcar_clean <- Vcar_for_plots

Vcar_classified_BEV <-  Vcar_clean %>%
  filter(Fuel == "Battery Electric")%>%
  pivot_longer(cols = `2024`:`2014`,
               names_to = "Data_year",
               values_to = "Fleet_Nu")

summery_BEV <-Vcar_classified_BEV %>%
  group_by(Data_year)%>%
  summarise(Fleet_Count=sum(Fleet_Nu))
print(summery_BEV)
print(is.na(Vcar_classified_BEV$engine_size))
unique(Vcar_classified_BEV$engine_size)
n_distinct(Vcar_classified_BEV$Model)



#check the fleet Number of BEV after classification 
Vcar_classified_BEV <-  Vcar_for_plots %>%
  filter(Fuel_Group == "BEV")%>%
  pivot_longer(cols = `2024`:`2014`,
               names_to = "Data_year",
               values_to = "Fleet_Nu")
summery_BEV <-Vcar_classified_BEV %>%
  group_by(Data_year)%>%
  summarise(Fleet_Count=sum(Fleet_Nu))
print(summery_BEV)

n_distinct(Vcar_classified_BEV$Model)





# Plot 1: Density plot of engine sizes by fuel type
p1 <- ggplot(Vcar_for_plots%>% select (Fuel_Group,`2024`,engine_size_liters), aes(x = engine_size_liters, fill = Fuel_Group )) +
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
  facet_wrap(~Fuel_Group , scales = "free_y")  # Separate plots for each fuel type

print(p1)



#-------------------------------------------------------
# Calculate IQR and identify outliers for each fuel type
#--------------------------------------------------------
library(purrr) 
outlier_analysis <- Vcar_for_plots %>%select(`2024`,Fuel,engine_size_liters)%>%
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

# 3. Extreme values analysis
cat("\n=== EXTREME VALUES ANALYSIS ===\n")

extreme_values <- Vcar_for_plots %>%select(`2024`,Fuel,engine_size_liters)%>%
  group_by(Fuel) %>%
  summarise(
    smallest_5 = paste(round(sort(unique(engine_size_liters))[1:min(5, n_distinct(engine_size_liters))], 2), collapse = ", "),
    largest_5 = paste(round(tail(sort(unique(engine_size_liters)), min(5, n_distinct(engine_size_liters))), 2), collapse = ", "),
    .groups = 'drop'
  )

print(extreme_values)

# Create cleaner plot with custom ranges AND fuel grouping
Vcar_clean_plot <- Vcar_for_plots %>%
  left_join(outlier_analysis %>% select(Fuel, outlier_min, outlier_max), by = "Fuel") %>%
  mutate(
    
    # Define custom min/max based on fuel type (using original Fuel for filtering)
    custom_min = case_when(
      Fuel %in% c("Petrol", "Diesel") ~ 0.5,
      str_detect(Fuel, "Hybrid") ~ 1,
      TRUE ~ outlier_min  # Use outlier range for others
    ),
    custom_max = case_when(
      Fuel %in% c("Petrol", "Diesel") ~ 6.5,
      str_detect(Fuel, "Hybrid") ~ 4,
      TRUE ~ outlier_max  # Use outlier range for others
    )
  ) 

# Verify no rows were lost
cat("Rows in Vcar_for_plots:", nrow(Vcar_for_plots), "\n")
cat("Rows in Vcar_clean_plot:", nrow(Vcar_clean_plot), "\n")
cat("Rows lost:", nrow(Vcar_for_plots) - nrow(Vcar_clean_plot), "\n")


#check the bEv
Vcar_classified_BEV <- Vcar_clean_plot %>%
  filter(Fuel_Group == "BEV")%>%
  pivot_longer(cols = `2024`:`2014`,
               names_to = "Data_year",
               values_to = "Fleet_Nu")
summery_BEV <-Vcar_classified_BEV %>%
  group_by(Data_year)%>%
  summarise(Fleet_Count=sum(Fleet_Nu))
print(summery_BEV)
n_distinct(Vcar_classified_BEV$Model)

#--------------------
#vehicle_leave_fleet
#---------------------
vehicle_leave_fleet <- Vcar_clean_plot %>%
  pivot_longer(cols = `2024`:`2014`,
               names_to = "Data_year",
               values_to = "Fleet_Nu")%>%
  
  group_by(Fuel_Group,Data_year) %>%
  summarise(
    min_out = sum(Fleet_Nu < custom_min),
    max_out = sum(Fleet_Nu > custom_max),
    count_leave = sum(Fleet_Nu < custom_min | Fleet_Nu > custom_max),
    .groups = 'drop'
  )
vehicle_leave_fleet%>% filter(Fuel_Group=="BEV")
print(vehicle_leave_fleet,n=15)
#---------------------------------------------------------------------------------
# Apply the custom filter to remove the min and max engine size outside the ranges
#-----------------------------------------------------------------------------------
Vcar_clean_plot <- Vcar_clean_plot %>%
  filter(
    Fuel_Group == "BEV" |  # Keep all BEV vehicles regardless of engine size
      (engine_size_liters >= custom_min & engine_size_liters <= custom_max)  # Apply filter to others
  )



# Verify final counts
final_check <- Vcar_clean_plot %>%
  summarise(
    total_2024 = sum(`2024`, na.rm = TRUE),
    BEV_2024 = sum(`2024`[Fuel_Group == "BEV"], na.rm = TRUE)
  )

cat("\n=== FINAL COUNTS AFTER FILTER ===\n")
print(final_check)



#check the bEv
Vcar_classified_BEV <- Vcar_clean_plot %>%
  filter(Fuel_Group == "BEV")%>%
  pivot_longer(cols = `2024`:`2014`,
               names_to = "Data_year",
               values_to = "Fleet_Nu")
summery_BEV <-Vcar_classified_BEV %>%
  group_by(Data_year)%>%
  summarise(Fleet_Count=sum(Fleet_Nu))
print(summery_BEV)
n_distinct(Vcar_classified_BEV$Model)


# Summary by the NEW fuel groups
filter_summary <- Vcar_clean_plot %>%
  mutate(
    # First clean engine_size
    engine_size_clean = case_when(
      engine_size %in% c("[x]", "[z]", "N/A", "NA", "") ~ NA_character_,
      TRUE ~ engine_size
    ),
    # Then convert to numeric
    engine_size_num = suppressWarnings(as.numeric(engine_size_clean)),
    # Then convert to liters
    engine_size_liters_fixed = engine_size_num / 1000
  )%>%
  group_by(Fuel_Group) %>%
  summarise(
    min_engine = min(engine_size_liters_fixed, na.rm = TRUE),
    max_engine = max(engine_size_liters_fixed, na.rm = TRUE),
    count = n(),
    .groups = 'drop'
  )

print(filter_summary)


p4 <- ggplot(Vcar_clean_plot%>% filter(engine_size_liters >= custom_min & engine_size_liters <= custom_max) %>% 
               select(-custom_min, -custom_max)%>%select(`2024`,Fuel_Group,engine_size_liters), aes(x = engine_size_liters, fill = Fuel_Group)) +
  geom_density(alpha = 0.6) +
  labs(
    # title = "Engine Size Distribution by Fuel Type",
    x = "Engine Size (Liters)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~Fuel_Group, scales = "free_y")  # Separate plots for each fuel type

print(p4)



# 6. Frequency table for specific ranges

engine_range_freq <- Vcar_for_plots  %>% select(`2024`,engine_size_liters,Fuel_Group)%>%
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


# 9. Additional: Violin plot for distribution visualization
p5 <- ggplot(Vcar_clean_plot%>% filter(engine_size_liters >= custom_min & engine_size_liters <= custom_max) %>% 
               select(-custom_min, -custom_max)%>%select(`2024`,Fuel_Group,engine_size_liters), aes(x = Fuel_Group, y = engine_size_liters, fill = Fuel_Group)) +
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



#-----------------------------------------
# Calculate percentiles for each fuel type
#-----------------------------------------
percentile_thresholds <- Vcar_clean_plot%>% filter(engine_size_liters >= custom_min & engine_size_liters <= custom_max) %>% 
  select(-custom_min, -custom_max) %>%select(`2024`,Fuel_Group,engine_size_liters,engine_size)%>%
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
  filter(Fuel_Group == "BEV")%>%
  pivot_longer(cols = `2024`:`2014`,
               names_to = "Data_year",
               values_to = "Fleet_Nu")
summery_BEV <-Vcar_classified_BEV %>%
  group_by(Data_year)%>%
  summarise(Fleet_Count=sum(Fleet_Nu))
print(summery_BEV)

Vcar_clean_plot %>%
  filter(Fuel_Group == "BEV") %>%
  summarise(
    na_in_engine_size = sum(is.na(engine_size)),
    na_in_engine_size_numeric = sum(is.na(engine_size_numeric)),
    total_bev_rows = n()
  ) %>%
  print()


#-----------------------------------------------------------------------
# Create classification using data-driven percentiles for ALL fuel types
#-----------------------------------------------------------------------

Vcar_classified <- Vcar_clean_plot%>%
  mutate(engine_size_liters = engine_size_numeric / 1000
  ) %>%
  left_join(percentile_thresholds %>% select(Fuel_Group, p25, p50, p75, p90), by = "Fuel_Group") %>%
  mutate(vehicle_class = case_when(
    # Diesel
    Fuel_Group == "Diesel" & is.na(engine_size_liters) ~ "Unsized Diesel",
    Fuel_Group == "Diesel" & engine_size_liters <= p25 ~ "Small Diesel",
    Fuel_Group == "Diesel" & engine_size_liters > p25 & engine_size_liters <= p50 ~ "Medium Diesel",
    Fuel_Group == "Diesel" & engine_size_liters > p50 & engine_size_liters <= p75 ~ "Large Diesel",
    Fuel_Group == "Diesel" & engine_size_liters > p75 ~ "Very Large Diesel",
    
    # Petrol
    Fuel_Group == "Petrol" & is.na(engine_size_liters) ~ "Unsized Petrol",
    Fuel_Group == "Petrol" & engine_size_liters <= p25 ~ "Small Petrol",
    Fuel_Group == "Petrol" & engine_size_liters > p25 & engine_size_liters <= p50 ~ "Medium Petrol",
    Fuel_Group == "Petrol" & engine_size_liters > p50 & engine_size_liters <= p75 ~ "Large Petrol",
    Fuel_Group == "Petrol" & engine_size_liters > p75 ~ "Very Large Petrol",
    
    # Hybrid (HEV) - Fixed logic
    Fuel_Group == "HEV" & is.na(engine_size_liters) ~ "Unsized HEV",
    Fuel_Group == "HEV" & engine_size_liters <= p25 ~ "Small HEV",
    Fuel_Group == "HEV" & engine_size_liters > p25 & engine_size_liters <= p50 ~ "Medium HEV",
    Fuel_Group == "HEV" & engine_size_liters > p50 & engine_size_liters <= p75 ~ "Large HEV",
    Fuel_Group == "HEV" & engine_size_liters > p75 ~ "Very Large HEV",
    
    # Plug-ins (PHEV) - Fixed overlapping conditions
    Fuel_Group == "PHEV" & is.na(engine_size_liters) ~ "Unsized PHEV",
    Fuel_Group == "PHEV" & engine_size_liters <= p25 ~ "Small PHEV",
    Fuel_Group == "PHEV" & engine_size_liters > p25 & engine_size_liters <= p50 ~ "Medium PHEV",
    Fuel_Group == "PHEV" & engine_size_liters > p50 & engine_size_liters <= p75 ~ "Large PHEV",
    Fuel_Group == "PHEV" & engine_size_liters > p75 ~ "Very Large PHEV",
    
    # BEV - Fixed incorrect logic and added more categories
    Fuel_Group == "BEV" & is.na(engine_size_liters) ~ "Unsized BEV",
    Fuel_Group == "BEV" & engine_size_liters <= p25 ~ "Small BEV",
    Fuel_Group == "BEV" & engine_size_liters > p25 & engine_size_liters <= p50 ~ "Medium BEV",
    Fuel_Group == "BEV" & engine_size_liters > p50 & engine_size_liters <= p75 ~ "Large BEV",
    Fuel_Group == "BEV" & engine_size_liters > p75 ~ "Very Large BEV",
    
    # Other fuel types - Fixed syntax error
    Fuel_Group == "Other" ~ "Unclassified",
    
    # Fuel cell electric
    Fuel_Group == "Fuel cell electric" ~ "Unclassified",
    
    TRUE ~ "Unclassified"
  )) 
# %>%
#   select(-p25, -p50, -p75, -p90)


class_size_table <- Vcar_classified %>%
  # Select relevant columns
  select(Fuel_Group, `2024`, p25, p50, p75, p90) %>%
  # Group by fuel group and percentiles
  group_by(Fuel_Group, p25, p50, p75, p90) %>%
  # Sum the 2024 fleet counts
  summarise(
    Fleet_2024 = sum(`2024`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Arrange by fuel group
  arrange(Fuel_Group)

print("=== FLEET COUNTS WITH PERCENTILES BY FUEL TYPE ===")
print(class_size_table)



# Simpler format with just the fleet counts
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



#check the fleet Number of BEV after classification 
Vcar_classified_BEV <- Vcar_classified %>%
  filter(Fuel_Group == "BEV")%>%
  pivot_longer(cols = `2024`:`2014`,
               names_to = "Data_year",
               values_to = "Fleet_Nu")

summery_BEV <-Vcar_classified_BEV %>%
  group_by(Data_year)%>%
  summarise(Fleet_Count=sum(Fleet_Nu))
print(summery_BEV)


Vcar_classified %>%
  filter(Fuel_Group == "BEV") %>%
  summarise(
    missing_vehicle_class = sum(is.na(vehicle_class) | vehicle_class == ""),
    unknown_vehicle_class = sum(str_detect(tolower(vehicle_class), "unknown|unsized|other"), na.rm = TRUE),
    total_bev_rows = n()
  ) %>%
  print()


#write.csv(Vcar_classified,"Classified_data.csv")



# Create the engine size distribution table
engine_size_table <- Vcar_clean_plot %>%
  pivot_longer(cols = "2024":"2024",
               names_to = "Data_Year",
               values_to ="Fleet_Count" )%>%
  # Filter for the most recent year (2024) and non-missing engine sizes
  filter(Data_Year == 2024, !is.na(engine_size)) %>%
  # Convert engine_size to numeric, handling any non-numeric values
  mutate(
    engine_size_num = suppressWarnings(as.numeric(engine_size)),
    # Create size categories based on your specified ranges
    size_range = case_when(
      engine_size_num >= 100 & engine_size_num <= 1400 ~ "100 - 1400",
      engine_size_num >= 1500 & engine_size_num <= 1700 ~ "1500 - 1700",
      engine_size_num >= 1800 & engine_size_num <= 2000 ~ "1800 - 2000",
      engine_size_num >= 2100 & engine_size_num <= 3000 ~ "2100 - 3000",
      engine_size_num >= 3100 & engine_size_num <= 15000 ~ "3100 - 15000",
      is.na(engine_size_num) | engine_size_num < 100 | engine_size_num > 15000 ~ "Unclassified",
      TRUE ~ "Other"  # Catch any values that don't fit the ranges
    )
  ) %>%
  # Group by size range and fuel type, then sum the fleet counts
  group_by(size_range, Fuel_Group) %>%
  summarise(
    Vehicle_Count = sum(Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Pivot wider to create columns for each fuel type
  pivot_wider(
    id_cols = size_range,
    names_from = Fuel_Group,
    values_from = Vehicle_Count,
    values_fill = 0  # Fill missing combinations with 0
  ) %>%
  # Add a total column
  mutate(
    Total = rowSums(across(where(is.numeric)), na.rm = TRUE)
  ) %>%
  # Arrange in custom order
  arrange(
    factor(size_range, levels = c(
      "100 - 1400", "1500 - 1700", "1800 - 2000", 
      "2100 - 3000", "3100 - 15000", "Unclassified", "Other"
    ))
  ) %>%
  # Remove rows where total is 0 (empty categories)
  filter(Total > 0)

# Print the table
print("=== ENGINE SIZE DISTRIBUTION BY FUEL TYPE (2024) ===")
print(engine_size_table)

# Optional: Add a formatted version with percentages
engine_size_percentage <- engine_size_table %>%
  mutate(across(where(is.numeric) & !Total, 
                ~ round(. / Total * 100, 1),
                .names = "{.col}_pct"))

print("=== PERCENTAGE WITHIN EACH ENGINE SIZE CATEGORY ===")
print(engine_size_percentage)




#################################################################################################################################################
#===============================
# car classification & their Age
#===============================
library (tidyverse)
library(dplyr)
library(readxl)
df_VEH0124_NZ  <- openxlsx::read.xlsx("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\Infuze\\Bodytype -first use\\df_VEH0124_NZ (updated).xlsx")
df_VEH0124_AM <- openxlsx::read.xlsx("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\Infuze\\Bodytype -first use\\df_VEH0124_AM (updated).xlsx")
data <- bind_rows(df_VEH0124_NZ,df_VEH0124_AM)%>% filter(LicenceStatus == "Licensed", BodyType=="Cars")

# Get just the total (single value)
totalvehicle2024 <- data%>%
  summarise(totalfleet = sum(`2024`, na.rm = TRUE))

print(totalvehicle2024)


# STEP 1: Filter data to reasonable years (2010+ for better matching)

age_percentiles <- data %>%
  mutate(YearFirstUsed = as.numeric(YearFirstUsed))%>%
  summarise(
    p10 = quantile(YearFirstUsed, 0.10, na.rm = TRUE),  # Remove oldest 10%
    p15 = quantile(YearFirstUsed, 0.15, na.rm = TRUE)   # Remove oldest 15%
  )


# Analyze the actual age distribution
library(Hmisc)
library(dplyr)
percentile_analysis <- data %>%select(YearFirstUsed,`2024`)%>% 
  mutate(YearFirstUsed = as.numeric(YearFirstUsed)) %>%
  summarise( p01 = quantile(YearFirstUsed, 0.01, na.rm = TRUE), p05 = quantile(YearFirstUsed, 0.05, na.rm = TRUE), p10 = quantile(YearFirstUsed, 0.10, na.rm = TRUE), p11.5 = quantile(YearFirstUsed, 0.115, na.rm = TRUE),p18.1 = quantile(YearFirstUsed, 0.181, na.rm = TRUE), p15 = quantile(YearFirstUsed, 0.15, na.rm = TRUE), p26 = quantile(YearFirstUsed, 0.26, na.rm = TRUE) )

cat("ACTUAL AGE DISTRIBUTION PERCENTILES:\n")
cat("1st percentile (oldest 1%):", percentile_analysis$p01, "\n")
cat("5th percentile (oldest 5%):", percentile_analysis$p05, "\n")
cat("10th percentile (oldest 10%):", percentile_analysis$p10, "\n")
cat("11.5th percentile (oldest 11.5%):", percentile_analysis$p11.5, "\n")
cat("18.1th percentile (oldest 18.1%):", percentile_analysis$p18.1,"\n")
cat("15th percentile (oldest 15%):", percentile_analysis$p15, "\n")
cat("26th percentile (oldest 26%):", percentile_analysis$p26, "\n")

# Find what percentile 1984 represents
year_1984_percentile <- data %>%
  select(YearFirstUsed, `2024`) %>%
  mutate(YearFirstUsed = as.numeric(YearFirstUsed)) %>%
  summarise(
    percentile_1984 = ecdf(YearFirstUsed)(1994) * 100
  )

cat("Year 1994 represents the:", round(year_1984_percentile$percentile_1984, 1), "percentile\n")

# Calculate vehicle ages
library(dplyr)
library(ggplot2)
library(tidyr)
data_with_age <- data %>%
  # First convert year columns to numeric (handle [x], [z] etc.)
  mutate(across(`2014`:`2024`, ~ {
    # Convert to character first
    char_col <- as.character(.x)
    # Replace [x] and [z] with NA
    char_col <- ifelse(char_col %in% c("[x]", "[z]"), NA, char_col)
    # Convert to numeric
    as.numeric(char_col)
  })) %>%
  # Pivot to long format
  pivot_longer(
    cols = `2014`:`2024`,
    names_to = "Data_Year",
    values_to = "Vehicle_count"
  ) %>%
  # Filter for 2024 and valid counts
  filter(Data_Year == 2024, !is.na(Vehicle_count), Vehicle_count > 0) %>%
  # Calculate vehicle age
  mutate(
    YearFirstUsed = as.numeric(YearFirstUsed),
    Data_Year = as.numeric(Data_Year),
    Vehicle_Age = Data_Year - YearFirstUsed
  )

# Create histogram
# Create histogram with LOG SCALE y-axis
library(ggplot2)
library(scales)

# Calculate max vehicles for annotation positioning
max_vehicles <- max(tapply(data_with_age$Vehicle_count,
                           data_with_age$Vehicle_Age,
                           sum, na.rm = TRUE))

# Create histogram with log scale
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
    y = "Number of Vehicles (log scale)",
  ) +
  scale_x_continuous(
    limits = c(0, 124),  # Changed from 100 to 80 to focus on relevant ages
    breaks = seq(0, 124, by = 20)
  ) +
  scale_y_log10(  # THIS IS THE KEY CHANGE - log scale
    labels = scales::comma,
    breaks = c( 1000, 10000, 100000, 1000000, 10000000)
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p_age_dist)

# Alternative: Two-panel plot to clearly show older vehicles
library(patchwork)

# Panel 1: Full range
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

# Panel 2: Zoomed on older vehicles
p2 <- ggplot(data_with_age %>% filter(Vehicle_Age >= 30), 
             aes(x = Vehicle_Age, weight = Vehicle_count)) +
  geom_histogram(binwidth = 1, fill = "#2E86AB", color = "white", alpha = 0.7) +
  geom_vline(xintercept = c(2024 - percentile_analysis$p01, 
                            2024 - percentile_analysis$p05,
                            2024 - percentile_analysis$p11.5,
                            2024 - percentile_analysis$p26) %>% 
               .[. >= 30],  # Only show lines that are in range
             linetype = "dashed", color = "red", alpha = 0.5) +
  labs(x = "Age", y = "Vehicles", title = "Vehicles 30+ Years") +
  scale_x_continuous(limits = c(30, 80), breaks = seq(30, 80, by = 5)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()
print(p2)

# Combine
p_combined <- p1 / (p1+p2) + 
  plot_annotation(
    title = "Vehicle Age Distribution (2024)",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  )

print(p_combined)

# Check the older vehicles distribution
older_distribution <- data_with_age %>%
  filter(Vehicle_Age >= 30) %>%
  group_by(Vehicle_Age) %>%
  summarise(vehicles = sum(Vehicle_count), .groups = "drop") %>%
  arrange(Vehicle_Age)

cat("\n=== OLDER VEHICLES DISTRIBUTION (30+ years) ===\n")
print(older_distribution, n = 50)


library(patchwork)

# Create age groups
data_age_groups <- data_with_age %>%select(Data_Year,Vehicle_Age)%>%
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
  )%>%filter(! Age_Group=="NA")

# Create individual plots
p1 <- ggplot(data_with_age, aes(x = Vehicle_Age)) +
  geom_histogram(binwidth = 1, fill = "#2E86AB", alpha = 0.7) +
  labs(title = "Histogram", x = "Age", y = "Count")

p2 <- ggplot(data_with_age, aes(x = Vehicle_Age)) +
  geom_density(fill = "#4A90E2", alpha = 0.5) +
  labs(title = "Density Plot", x = "Age", y = "Density")

p3 <- ggplot(data_age_groups, aes(x = Age_Group, y = Percentage)) +
  geom_col(fill = "#6A9FB5", alpha = 0.7) +
  labs(title = "Age Groups", x = "Age Group", y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine plots
combined_plot <- (p1 + p2) / p3 +
  plot_annotation(
    title = "Comprehensive Vehicle Age Analysis",
    subtitle = "Multiple views of the age distribution",
    caption = "DVLA Vehicle Licensing Data"
  )

print(combined_plot)




######################################################################################################################
# Based on above results, I'd recommend the 26th percentile (2000 cutoff)

##################################################################################
recommended_cutoff <- 1994  # 2000 From your 15th percentile result ( 2004 or 2005), however i want to make it to 40 max
filtered_data <- data%>%
  filter(YearFirstUsed >= recommended_cutoff)

# Check the impact
cat("FILTERING RESULTS:\n")
cat("Original vehicle count:", nrow(data), "\n")
cat("After removing vehicles before 1991:", nrow(filtered_data), "\n")
cat("Vehicles removed:", nrow(data) - nrow(filtered_data), "\n")
cat("Percentage kept:", round(nrow(filtered_data) / nrow(data) * 100, 1), "%\n")

# Check the age distribution of the filtered data
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




# STEP 2: Enhanced multi-level matching - FIXED VERSION
library(dplyr)
library(stringr)
library(tidyr)


#----------------------------------------------------------------
# add the BEV sizes to filter data 
#----------------------------------------------------------------


# Apply the matching logic directly to filtered_data
filtered_data_with_engine <- filtered_data %>%
  # Create clean versions for matching
  mutate(
    Make_clean = str_to_upper(str_trim(Make)),
    Model_clean = str_to_upper(str_trim(Model)) %>%
      str_replace_all("[^A-Z0-9]", "")
  ) %>%
  # Try multiple matching levels DIRECTLY
  left_join(VBEV_File_clean, by = c("Make_clean", "Model_clean")) %>%
  left_join(VBEV_File_genmodel, by = "Make_clean", suffix = c("", "_genmodel")) %>%
  left_join(VBEV_File_model, by = "Model_clean", suffix = c("", "_model")) %>%
  left_join(VBEV_File_make, by = "Make_clean", suffix = c("", "_make")) %>%
  # Use the best available match
  mutate(
    # Consolidate engine sizes from all sources
    engine_size_combined = coalesce(engine_size, engine_size_genmodel, 
                                    engine_size_model, engine_size_make),
    
    # Determine match level
    Match_Level = case_when(
      !is.na(engine_size) & is.na(engine_size_genmodel) & 
        is.na(engine_size_model) & is.na(engine_size_make) ~ "Exact",
      !is.na(engine_size_genmodel) ~ "MakeOnly",
      !is.na(engine_size_model) ~ "ModelOnly", 
      !is.na(engine_size_make) ~ "MakeAverage",
      TRUE ~ "NoMatch"
    ),
    
    # Create Fuel column: Battery electric if any match found
    Fuel = ifelse(!is.na(engine_size_combined), "Battery electric", "Other Fuel Types"),
    
    # Update the main engine_size column
    engine_size = ifelse(Fuel == "Battery electric", 
                         engine_size_combined, 
                         engine_size)
  ) %>%
  # Remove temporary columns (keep only what you need)
  select(-Make_clean, -Model_clean, 
         -engine_size_genmodel, -engine_size_model, -engine_size_make,
         -engine_size_combined,-Match_Level)

# MATCH SUMMARY
match_summary <- filtered_data_with_engine %>%
  filter(Fuel == "Battery electric") %>%
  summarise(
    Count = n(),
    Percentage_of_Total = round(n() / nrow(filtered_data_with_engine) * 100, 2)
  )

cat("=== BATTERY ELECTRIC VEHICLE COUNT ===\n")
print(match_summary)

# Show results
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
# Finds vehicles in filtered_data that are NOT in Vcar_classified
#----------------------------------------------------------------
cat("\n3. Finding unmatched filtered_data rows...\n")

filtered_only <- filtered_data_with_engine %>%
  anti_join(
    Vcar_classified,
    by = c("BodyType", "GenModel", "Model", "Make", "LicenceStatus")
  ) %>%
  mutate(
    across(`2024`:`2014`, 
           ~ suppressWarnings(as.numeric(ifelse(. %in% c("[x]", "[z]"), NA, .)))),
    
    # Use suppressWarnings around each conversion
    YearFirstUsed = suppressWarnings(as.numeric(YearFirstUsed)),
    YearManufacture = suppressWarnings(as.numeric(YearManufacture)),
    
    source = "df_VEH0124_NZ&df_VEH0124_AM",
    engine_size = as.character(engine_size)
  )

cat("   Unmatched rows found:", nrow(filtered_only), "\n")

#--------------------------------------------------------------
# Finds vehicles in filtered_data that ARE in Vcar_classified
#--------------------------------------------------------------

# Vcar_classified_unique <- Vcar_classified %>%
#   group_by(
#     BodyType, Make, GenModel, Model, LicenceStatus
#   ) %>%
#   summarise(
#     Fuel = first(Fuel),
#     Fuel_Group = first(Fuel_Group),
#     vehicle_class=first(vehicle_class),
#     engine_size = first(engine_size),   # ✅ ADD THIS
#     .groups = "drop"
#   )
# 
# filtered_matched_counts <- filtered_data%>%
#   inner_join(
#     Vcar_classified_unique,
#     by = c("BodyType", "Make", "GenModel", "Model", "LicenceStatus")
#   ) %>%
#   mutate(
#     across(`2024`:`2014`,
#            ~ suppressWarnings(as.numeric(
#              ifelse(. %in% c("[x]", "[z]"), NA, .)
#            ))),
#     # ADD THIS: Convert YearFirstUsed and YearManufacture to numeric
#     YearFirstUsed = as.numeric(YearFirstUsed),
#     YearManufacture = as.numeric(YearManufacture),
#     engine_size = as.character(engine_size)
#   ) %>%
#   group_by(
#     BodyType, Make, GenModel, Model, LicenceStatus,
#     YearFirstUsed, YearManufacture,
#     Fuel, Fuel_Group,vehicle_class, engine_size
#   ) %>%
#   summarise(
#     across(`2024`:`2014`, ~ sum(., na.rm = TRUE)),
#     .groups = "drop"
#   ) %>%
#   mutate(source = "matched_data(VEH0124&VEH0120&0220)")
# 
# 
# cat("   Matched rows found:", nrow(filtered_matched_counts), "\n")
#--------------------------------------------------------------
# Finds vehicles in filtered_data that ARE in Vcar_classified
# FIXED: proportional fuel allocation instead of first()-collapse.
# filtered_data (VEH0124) has no Fuel column of its own, so any
# Make/GenModel/Model shared across multiple real fuel variants
# was previously being assigned ONE arbitrary fuel for ALL its
# vehicles. This splits each vehicle count proportionally across
# its model's real fuel-type mix, taken from Vcar_classified
# (VEH0120/VEH0220 - the only source with real Fuel info).
#--------------------------------------------------------------

# fuel_shares_long <- Vcar_classified %>%
#   pivot_longer(cols = `2014`:`2024`, names_to = "Data_Year", values_to = "Fleet_Count") %>%
#   mutate(
#     Fleet_Count = suppressWarnings(as.numeric(ifelse(as.character(Fleet_Count) %in% c("[x]", "[z]"), NA, Fleet_Count))),
#     Data_Year = as.numeric(Data_Year)
#   ) %>%
#   filter(!is.na(Fleet_Count), Fleet_Count > 0) %>%
#   group_by(Make, GenModel, Model, Data_Year) %>%
#   mutate(Fuel_Share = Fleet_Count / sum(Fleet_Count)) %>%
#   ungroup() %>%
#   select(Make, GenModel, Model, Data_Year, Fuel, Fuel_Group, vehicle_class, engine_size, Fuel_Share)
# 
# filtered_data_long <- filtered_data %>%
#   mutate(across(`2014`:`2024`, as.character)) %>%
#   pivot_longer(cols = `2014`:`2024`, names_to = "Data_Year", values_to = "Fleet_Count") %>%
#   mutate(
#     Data_Year = as.numeric(Data_Year),
#     Fleet_Count = suppressWarnings(as.numeric(ifelse(Fleet_Count %in% c("[x]", "[z]"), NA, Fleet_Count))),
#     YearFirstUsed = as.numeric(YearFirstUsed),
#     YearManufacture = as.numeric(YearManufacture)
#   ) %>%
#   filter(!is.na(Fleet_Count), Fleet_Count > 0)
# 
# filtered_matched_long <- filtered_data_long %>%
#   inner_join(
#     fuel_shares_long,
#     by = c("Make", "GenModel", "Model", "Data_Year"),
#     relationship = "many-to-many"   # expected: one vehicle count splits across several fuel variants
#   ) %>%
#   mutate(Fleet_Count = Fleet_Count * Fuel_Share) %>%
#   select(-Fuel_Share) %>%
#   group_by(
#     BodyType, Make, GenModel, Model, LicenceStatus,
#     YearFirstUsed, YearManufacture,
#     Fuel, Fuel_Group, vehicle_class, engine_size, Data_Year
#   ) %>%
#   summarise(Fleet_Count = sum(Fleet_Count, na.rm = TRUE), .groups = "drop")
# 
# 
# 
# # Pivot back to WIDE to match the shape the rest of the pipeline expects
# filtered_matched_counts <- filtered_matched_long %>%
#   mutate(Data_Year = as.character(Data_Year)) %>%
#   pivot_wider(names_from = Data_Year, values_from = Fleet_Count, values_fill = 0) %>%
#   mutate(
#     engine_size = as.character(engine_size),
#     source = "matched_data(VEH0124&VEH0120&0220)"
#   )
# 
# for (yr in as.character(2014:2024)) {
#   if (!yr %in% names(filtered_matched_counts)) {
#     filtered_matched_counts[[yr]] <- 0
#   }
# }


# # ADD THIS: force year columns into a fixed, known order (descending, matching
# # filtered_only/vcar_only), so bind_rows() always combines consistently-ordered frames
# filtered_matched_counts <- filtered_matched_counts %>%
#   select(-any_of(as.character(2014:2024)), all_of(as.character(2024:2014)), everything()) %>%
#   select(everything(), -all_of(as.character(2024:2014)), all_of(as.character(2024:2014)))
# 
# cat("   Matched rows found:", nrow(filtered_matched_counts), "\n")



#--------------------------------------------------------------
# Finds vehicles in filtered_data that ARE in Vcar_classified
# FIXED (model-level, year-stable): proportional fuel allocation,
# computed ONCE per model across all years combined — not per year.
# This fixes two bugs at once:
#   1. the original first()-collapse mislabeling ~43% of the fleet
#   2. a later Data_Year-matched version that let a model's fuel
#      split flip between years whenever Vcar_classified had a
#      zero-count snapshot for that model in a given year, which
#      corrupted retirement-rate / Age-0-vs-Age-1 cohort comparisons
#--------------------------------------------------------------

fuel_shares_by_model <- Vcar_classified %>%
  pivot_longer(cols = `2014`:`2024`, names_to = "Data_Year", values_to = "Fleet_Count") %>%
  mutate(
    Fleet_Count = suppressWarnings(as.numeric(ifelse(as.character(Fleet_Count) %in% c("[x]", "[z]"), NA, Fleet_Count)))
  ) %>%
  filter(!is.na(Fleet_Count), Fleet_Count > 0) %>%
  # Sum across ALL years first — one overall fuel-mix per model
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
    by = c("Make", "GenModel", "Model"),   # Data_Year NOT in the key — this is the fix
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

# Pivot back to WIDE to match the shape the rest of the pipeline expects
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

# Force year columns into a fixed, known order (descending, matching
# filtered_only/vcar_only), so bind_rows() always combines consistently-ordered frames
filtered_matched_counts <- filtered_matched_counts %>%
  relocate(all_of(as.character(2024:2014)), .after = last_col())

cat("   Matched rows found:", nrow(filtered_matched_counts), "\n")



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
# Option 2: Use Vcar_classified (original) which has year columns
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

#-----------------------------------------------------------------
# Combine ALL: filtered_matched + filtered_unmatched + vcar_only
#---------------------------------------------------------------
join_final_complete <- bind_rows(
  filtered_matched_counts,
  filtered_only,vcar_only
) %>%
  mutate(
    Fuel = ifelse(is.na(Fuel), "Other Fuel Types", Fuel),
    Fuel_Group = ifelse(is.na(Fuel_Group), "Other", Fuel_Group),
    vehicle_class =ifelse(is.na(vehicle_class),"Unclassified",vehicle_class)
  )

cat("\nFull dataset with Vcar-only vehicles:", nrow(join_final_complete), "rows\n")

BE <- join_final_complete %>%
  filter(Fuel_Group=="BEV", is.na(engine_size))

print(BE)

#CP1 — right after the origin join on join_final_complete. Find this exact block:



#-----------------------------
# Create summary & Vehicle age 
#-----------------------------
join_final_complete_summary <- join_final_complete %>%
  pivot_longer(
    cols = `2024`:`2014`,
    names_to = "Data_Year",
    values_to = "Fleet_Count"
  ) %>%
  mutate(
    Data_Year = as.numeric(Data_Year),
    YearFirstUsed = as.numeric(YearFirstUsed),
    Vehicle_Age = ifelse(Data_Year >= YearFirstUsed,
                         Data_Year - YearFirstUsed,
                         NA)
  ) %>%
  filter(!is.na(Fleet_Count) & Fleet_Count > 0) %>%
  group_by(Data_Year) %>%
  summarise(
    vehicle_count = sum(Fleet_Count, na.rm = TRUE),
    avg_vehicle_age = weighted.mean(Vehicle_Age, Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  )

print(join_final_complete_summary)






# ============================================================================
# UK VEHICLE ORIGIN — FINAL CORRECTED ANALYSIS
# ----------------------------------------------------------------------------
# Settled design decisions:
#  * Origin keyed on YearManufacture (build year), not the snapshot year, so
#    year-specific production rules (Tesla Shanghai post-2020, Honda Swindon
#    pre-2023, Volvo EX30 etc.) apply to when the car was actually built.
#  * "New registrations" = Vehicle_Age==0 AND YearManufacture >= Data_Year-1,
#    which strips the vintage-import tail (cars first-registered new but built
#    decades earlier) so the cohort is comparable to ACEA new-sales.
#  * Validation kept separate by construction:
#       - NEW registrations  -> compare to ACEA new-sales (China 14%, EU 61%)
#       - whole PARC          -> compare to SMMT Motorparc (plant-derived)
#    The two are never crossed (stock != flow).
#
# Input: join_final_complete (long; Make, GenModel, Fuel_Group, Data_Year,
#        YearManufacture, YearFirstUsed, Vehicle_Age, Fleet_Count)
# ============================================================================

library(dplyr); library(tidyr); library(stringr); library(ggplot2)

# ---------------------------------------------------------------------------
# 1. PREP
# ---------------------------------------------------------------------------

join_final_complete  <-join_final_complete %>%
  pivot_longer(
    cols = `2024`:`2014`,
    names_to = "Data_Year",
    values_to = "Fleet_Count"
  ) %>%
  mutate(
    Data_Year = as.numeric(Data_Year),
    YearFirstUsed = as.numeric(YearFirstUsed),
    Vehicle_Age = ifelse(Data_Year >= YearFirstUsed,
                         Data_Year - YearFirstUsed,
                         NA)
  )

veh_long <- join_final_complete  %>%
  mutate(
    Year            = as.integer(Data_Year),
    Build_Year      = as.integer(YearManufacture),
    Count           = as.numeric(Fleet_Count),
    Make            = toupper(str_trim(Make)),
    GenModel        = toupper(str_trim(GenModel)),
    Fuel            = toupper(str_trim(Fuel))
  ) %>%
  filter(!is.na(Count), Count > 0)

# ---------------------------------------------------------------------------
# 2. ORIGIN FUNCTION (keyed on build year) + concordance
# ---------------------------------------------------------------------------
model_origin_corrected <- function(make, gm, fuel_group, byear) {
  case_when(
    
    # ============================================================
    # TESLA
    # ============================================================
    make=="TESLA" & str_detect(gm,"MODEL 3") & byear < 2020 ~ "USA",
    make=="TESLA" & str_detect(gm,"MODEL 3") & byear >= 2020 ~ "China",
    make=="TESLA" & str_detect(gm,"MODEL Y") & byear >= 2021 ~ "China",
    make=="TESLA" & str_detect(gm,"MODEL Y") ~ "China",
    make=="TESLA" & str_detect(gm,"MODEL S|MODEL X") ~ "USA",
    make=="TESLA" & fuel_group=="BEV" ~ "China",
    make=="TESLA" ~ "China",
    
    # ============================================================
    # BYD
    # ============================================================
    make=="BYD" ~ "China",
    
    # ============================================================
    # MG (SAIC)
    # ============================================================
    make=="MG" & str_detect(gm,"MG4|MG5|MG3|ZS|HS|CYBERSTER|MARVEL|EHS") ~ "China",
    make=="MG" & byear < 2010 ~ "UK",
    make=="MG" ~ "China",
    
    # ============================================================
    # BMW
    # ============================================================
    make=="BMW" & str_detect(gm,"IX3") ~ "China",
    make=="BMW" & str_detect(gm,"\\bI3\\b") ~ "Germany",
    make=="BMW" & str_detect(gm,"IX|I4") ~ "Germany",
    make=="BMW" ~ "Germany",
    
    # ============================================================
    # MERCEDES
    # ============================================================
    make=="MERCEDES" & str_detect(gm,"EQB") ~ "Hungary",
    make=="MERCEDES" & str_detect(gm,"EQA|EQS|EQE") ~ "Germany",
    make=="MERCEDES" ~ "Germany",
    
    # ============================================================
    # VOLVO
    # ============================================================
    make=="VOLVO" & str_detect(gm,"EX30") ~ "China",
    make=="VOLVO" & str_detect(gm,"XC40|C40") & fuel_group=="BEV" ~ "Belgium",
    make=="VOLVO" ~ "Sweden",
    
    # ============================================================
    # POLESTAR
    # ============================================================
    make=="POLESTAR" ~ "China",
    
    # ============================================================
    # VOLKSWAGEN GROUP
    # ============================================================
    make=="VOLKSWAGEN" & str_detect(gm,"ID\\.?3|ID\\.?4|ID\\.?5") ~ "Germany",
    make=="VOLKSWAGEN" ~ "Germany",
    
    make=="AUDI" & str_detect(gm,"E-TRON|ETRON|Q4") ~ "Belgium",
    make=="AUDI" ~ "Germany",
    
    make=="SKODA" & str_detect(gm,"ENYAQ") ~ "Czechia",
    make=="SKODA" ~ "Czechia",
    
    make=="SEAT" ~ "Spain",
    make=="CUPRA" ~ "Spain",
    
    # ============================================================
    # STELLANTIS (ex-PSA/Opel/Vauxhall)
    # ============================================================
    make=="PEUGEOT" ~ "France",
    make=="CITROEN" ~ "France",
    make=="DS" ~ "France",
    
    make=="RENAULT" & str_detect(gm,"ZOE|MEGANE") ~ "France",
    make=="RENAULT" ~ "France",
    
    make=="OPEL" ~ "Germany",
    
    # ============================================================
    # NISSAN
    # ============================================================
    make=="NISSAN" & str_detect(gm,"LEAF|QASHQAI|JUKE") & byear <= 2024 ~ "UK",
    make=="NISSAN" & str_detect(gm,"ARIYA") ~ "Japan",
    make=="NISSAN" ~ "Japan",
    
    # ============================================================
    # TOYOTA
    # ============================================================
    make=="TOYOTA" & str_detect(gm,"COROLLA") & byear >= 2019 ~ "UK",
    make=="TOYOTA" & str_detect(gm,"AVENSIS|AURIS") ~ "UK",
    make=="TOYOTA" & str_detect(gm,"YARIS") ~ "France",
    make=="TOYOTA" & str_detect(gm,"AYGO") ~ "Czechia",
    make=="TOYOTA" & str_detect(gm,"BZ4X") ~ "Japan",
    make=="TOYOTA" ~ "Japan",
    
    # ============================================================
    # HYUNDAI / KIA
    # ============================================================
    make=="HYUNDAI" ~ "South Korea",
    make=="KIA" ~ "South Korea",
    
    # ============================================================
    # HONDA
    # ============================================================
    make=="HONDA" & str_detect(gm,"CIVIC") & byear <= 2022 ~ "UK",
    make=="HONDA" ~ "Japan",
    
    # ============================================================
    # FORD
    # ============================================================
    make=="FORD" & str_detect(gm,"KUGA|MONDEO") ~ "Spain",
    make=="FORD" & str_detect(gm,"PUMA|ECOSPORT") ~ "Romania",
    make=="FORD" & str_detect(gm,"MUSTANG|EDGE") ~ "USA",
    make=="FORD" & str_detect(gm,"MUSTANG MACH-E") ~ "Mexico",
    make=="FORD" ~ "Germany",
    
    # ============================================================
    # VAUXHALL
    # ============================================================
    make=="VAUXHALL" & str_detect(gm,"ASTRA|CORSA") & byear <= 2022 ~ "UK",
    make=="VAUXHALL" & str_detect(gm,"MOKKA|CROSSLAND") ~ "Spain",
    make=="VAUXHALL" ~ "Germany",
    
    # ============================================================
    # MINI
    # ============================================================
    make=="MINI" & str_detect(gm,"COUNTRYMAN") & byear >= 2023 ~ "Germany",
    make=="MINI" ~ "UK",
    
    # ============================================================
    # JAGUAR / LAND ROVER
    # ============================================================
    make=="JAGUAR" & str_detect(gm,"I-PACE") ~ "Austria",
    make=="JAGUAR" ~ "UK",
    make=="LAND ROVER" ~ "UK",
    
    # ============================================================
    # OTHER MAKES (concordance fallback)
    # ============================================================
    make=="ABARTH" ~ "Italy",
    make=="AIXAM" ~ "France",
    make=="FIAT" ~ "Italy",
    make=="ALFA ROMEO" ~ "Italy",
    make=="LANCIA" ~ "Italy",
    make=="FERRARI" ~ "Italy",
    make=="LAMBORGHINI" ~ "Italy",
    make=="MASERATI" ~ "Italy",
    
    make=="PORSCHE" ~ "Germany",
    
    make=="MAZDA" ~ "Japan",
    make=="SUBARU" ~ "Japan",
    make=="SUZUKI" ~ "Japan",
    make=="MITSUBISHI" ~ "Japan",
    make=="LEXUS" ~ "Japan",
    make=="INFINITI" ~ "Japan",
    
    make=="JEEP" ~ "USA",
    make=="CHEVROLET" ~ "USA",
    make=="CADILLAC" ~ "USA",
    
    make=="DACIA" ~ "Romania",
    
    # ============================================================
    # UNMAPPED MAKES - ADDED FROM COVERAGE CHECK
    # ============================================================
    
    # UK Heritage / Luxury
    make=="ROVER" ~ "UK",
    make=="LOTUS" ~ "UK",
    make=="ASTON MARTIN" ~ "UK",
    make=="BENTLEY" ~ "UK",
    make=="DAIMLER" ~ "UK",
    make=="MCLAREN" ~ "UK",
    make=="TVR" ~ "UK",
    make=="MORGAN" ~ "UK",
    make=="TRIUMPH" ~ "UK",
    make=="ROLLS ROYCE" ~ "UK",
    make=="ROLLS-ROYCE" ~ "UK",
    make=="AUSTIN" ~ "UK",
    make=="LDV" ~ "UK",
    make=="MORRIS" ~ "UK",
    make=="BRISTOL" ~ "UK",
    make=="SUNBEAM" ~ "UK",
    make=="CATERHAM" ~ "UK",
    make=="RELIANT" ~ "UK",
    make=="HILLMAN" ~ "UK",
    make=="WOLSELEY" ~ "UK",
    make=="HUMBER" ~ "UK",
    make=="JENSEN" ~ "UK",
    make=="SINGER" ~ "UK",
    make=="LEVC" ~ "UK",
    make=="INEOS" ~ "UK",
    make=="METROCAB" ~ "UK",
    make=="LONDON TAXI" ~ "UK",
    make=="LONDON TAXIS INT." ~ "UK",
    make=="COLEMAN MILNE" ~ "UK",
    make=="CLASSIC REPLICAS" ~ "UK",
    make=="CARBODIES" ~ "UK",
    make=="NOBLE" ~ "UK",
    make=="ZENOS" ~ "UK",
    make=="STUART TAYLOR" ~ "UK",
    make=="CCM" ~ "UK",
    
    # Smart / German
    make=="SMART" ~ "Germany",
    make=="MAYBACH" ~ "Germany",
    make=="MCC" ~ "Germany",
    make=="MAN" ~ "Germany",
    
    # Asian
    make=="SSANGYONG" ~ "South Korea",
    make=="KGM" ~ "South Korea",
    make=="DAIHATSU" ~ "Japan",
    make=="PROTON" ~ "Malaysia",
    make=="PERODUA" ~ "Malaysia",
    make=="ISUZU" ~ "Japan",
    make=="DAEWOO" ~ "South Korea",
    make=="GENESIS" ~ "South Korea",
    make=="HOLDEN" ~ "Australia",
    make=="TATA" ~ "India",
    make=="REVA" ~ "India",
    make=="MAXUS" ~ "China",
    make=="ORA" ~ "China",
    make=="OMODA" ~ "China",
    make=="JAECOO" ~ "China",
    make=="GREAT WALL" ~ "China",
    make=="SKYWELL" ~ "China",
    make=="XPENG" ~ "China",
    make=="DFSK" ~ "China",
    make=="GWM" ~ "China",
    make=="MG XPOWER" ~ "China",
    make=="ASIA" ~ "South Korea",
    make=="DATSUN" ~ "Japan",
    make=="ISUZU TRUCKS" ~ "Japan",
    make=="KAWASAKI" ~ "Japan",
    make=="YMCA" ~ "Japan",
    make=="KYMCO" ~ "Taiwan",
    make=="SYM" ~ "Taiwan",
    
    # North American
    make=="CHRYSLER" ~ "USA",
    make=="DODGE" ~ "USA",
    make=="CHEVROLET GMC" ~ "USA",
    make=="HUMMER" ~ "USA",
    make=="CORVETTE" ~ "USA",
    make=="LINCOLN" ~ "USA",
    make=="PONTIAC" ~ "USA",
    make=="PLYMOUTH" ~ "USA",
    make=="GMC" ~ "USA",
    make=="BUICK" ~ "USA",
    make=="CAN-AM" ~ "Canada",
    make=="POLARIS" ~ "USA",
    make=="FISKER" ~ "USA",
    
    # European
    make=="ALPINE" ~ "France",
    make=="RENAULT TRUCKS" ~ "France",
    make=="IVECO" ~ "Italy",
    make=="MOTO GUZZI" ~ "Italy",
    make=="PIAGGIO" ~ "Italy",
    make=="SECMA" ~ "France",
    make=="LIGIER" ~ "France",
    make=="MICROCAR" ~ "France",
    make=="SANTANA" ~ "Spain",
    make=="TALBOT" ~ "France",
    make=="SIMCA" ~ "France",
    make=="VENTURI" ~ "France",
    make=="GAS GAS" ~ "Spain",
    make=="X-BOW" ~ "Austria",
    make=="SAAB" ~ "Sweden",
    make=="LADA" ~ "Russia",
    
    # Agricultural / Other
    make=="ALL WHEEL DRIVE" ~ "Other",
    make=="IRISBUS" ~ "Other",
    make=="STRATHCARRON" ~ "Other",
    make=="DINLI" ~ "Other",
    make=="TAIWAN GOLDEN BEE" ~ "Other",
    make=="CHARGE" ~ "Other",
    make=="SAN" ~ "Other",
    make=="GORDON MURRAY" ~ "Other",
    make=="BOMBARDIER" ~ "Other",
    make=="GOKA" ~ "Other",
    make=="CFMOTO" ~ "China",
    make=="QUADZILLA" ~ "China",
    # Remaining unmapped
    make=="MERCEDES-BENZ" ~ "Germany",
    make=="MIA" ~ "France",  # French microcar
    make=="FSO CARS" ~ "Poland",
    make=="YAMAHA" ~ "Japan",
    make=="THINK" ~ "Norway",  # Norwegian EV
    make=="VESPA (DOUGLAS)" ~ "Italy",
    
    # ============================================================
    # FINAL FALLBACK
    # ============================================================
    TRUE ~ NA_character_
  )
}
concordance <- tribble(
  ~Make, ~Origin,
  "BYD","China","GWM","China","GREAT WALL","China","ORA","China","OMODA","China",
  "JAECOO","China","LEAPMOTOR","China","XPENG","China","SERES","China",
  "SKYWELL","China","DFSK","China","MAXUS","China","SAIC","China","FARIZON","China",
  "POLESTAR","China",
  "MINI","UK","LAND ROVER","UK","JAGUAR","UK","JAGUAR LAND ROVER","UK",
  "ASTON MARTIN","UK","BENTLEY","UK","ROLLS ROYCE","UK","ROLLS-ROYCE","UK",
  "MCLAREN","UK","MORGAN","UK","LEVC","UK","CATERHAM","UK","INEOS","UK","LOTUS","UK",
  "VOLKSWAGEN","Germany","AUDI","Germany","BMW","Germany","MERCEDES-BENZ","Germany",
  "MERCEDES","Germany","PORSCHE","Germany","OPEL","Germany","MAYBACH","Germany",
  "ALPINA","Germany","SMART","Germany",
  "RENAULT","France","PEUGEOT","France","CITROEN","France","DS","France","ALPINE","France",
  "SEAT","Spain","CUPRA","Spain","SKODA","Czechia",
  "FIAT","Italy","ALFA ROMEO","Italy","LANCIA","Italy","ABARTH","Italy",
  "FERRARI","Italy","LAMBORGHINI","Italy","MASERATI","Italy",
  "VOLVO","Sweden","DACIA","Romania",
  "LEXUS","Japan","MAZDA","Japan","SUBARU","Japan","MITSUBISHI","Japan",
  "SUZUKI","Japan","INFINITI","Japan",
  "KIA","South Korea","HYUNDAI","South Korea","GENESIS","South Korea",
  "SSANGYONG","South Korea","KGM","South Korea",
  "JEEP","USA","CHEVROLET","USA","CADILLAC","USA","VAUXHALL","Germany"
)

region_map <- tribble(
  ~Origin,~Region,
  "UK","UK","Germany","EU","France","EU","Spain","EU","Italy","EU","Czechia","EU",
  "Sweden","EU","Romania","EU","Belgium","EU","Hungary","EU","Austria","EU",
  "Japan","Japan","South Korea","Korea","China","China","USA","USA",
  "Other (unmapped)","Other"
)

veh_origin <- veh_long %>%
  left_join(concordance, by="Make") %>%
  mutate(Origin = coalesce(model_origin_corrected(Make, GenModel, Fuel_Group, Build_Year),
                           Origin, "Other (unmapped)")) %>%
  left_join(region_map, by="Origin") %>%
  mutate(Region = coalesce(Region, "Other"))





# ---------------------------------------------------------------------------
# 3. NEW REGISTRATIONS (genuine new builds) — the ACEA-comparable cohort
# ---------------------------------------------------------------------------
new_regs <- veh_origin %>% filter(Vehicle_Age==0, Build_Year >= Year-1)

# ---- 3a. NEW BEV origin by region, all years ----
bev_new_region <- new_regs %>% filter(Fuel_Group=="BEV") %>%
  group_by(Year, Region) %>% summarise(Reg=sum(Count), .groups="drop") %>%
  group_by(Year) %>% mutate(Share=round(100*Reg/sum(Reg),1)) %>% ungroup()

cat("\n=== NEW BEV ORIGIN BY REGION (share %) ===\n")
bev_new_region %>% select(Year,Region,Share) %>%
  pivot_wider(names_from=Region, values_from=Share, values_fill=0) %>%
  arrange(Year) %>% print(n=Inf)

# ---- 3b. China share: all new cars vs new BEVs (the headline) ----
china_split <- new_regs %>% filter(Year %in% 2022:2024) %>%
  group_by(Year, Fuel_Group) %>%
  summarise(China=round(100*sum(Count[Region=="China"])/sum(Count),1), .groups="drop") %>%
  pivot_wider(names_from=Fuel_Group, values_from=China, values_fill=0)
cat("\n=== CHINA SHARE OF NEW REGISTRATIONS: by fuel ===\n")
print(china_split)

# ---------------------------------------------------------------------------
# 4. VALIDATION A: NEW registrations vs ACEA new-sales (CORRECT comparison)
# ---------------------------------------------------------------------------
new_allcar_2024 <- new_regs %>% filter(Year==2024) %>%
  group_by(Region) %>% summarise(Reg=sum(Count), .groups="drop") %>%
  mutate(Model_pct=round(100*Reg/sum(Reg),1)) %>% arrange(desc(Reg))

acea_newsales <- tibble(Region=c("EU","China","UK","Japan"),
                        ACEA_2025_pct=c(63.4, 9.3, 9.5,5.4))  # ACEA all-cars new sales

validation_new <- new_allcar_2024 %>% left_join(acea_newsales, by="Region") %>%
  mutate(Gap_pp = round(Model_pct - ACEA_2025_pct,1))
cat("\n=== VALIDATION A: NEW REGISTRATIONS (2024) vs ACEA new-sales (2025) ===\n")
cat("Compares like with like: new sales vs new sales.\n")
print(validation_new)

# ---------------------------------------------------------------------------
# 5. VALIDATION B: whole PARC vs SMMT Motorparc (plant-derived stock)
# ---------------------------------------------------------------------------
parc_2024 <- veh_origin %>% filter(Year==2024) %>%
  group_by(Origin) %>% summarise(Veh=sum(Count), .groups="drop") %>%
  mutate(Model_pct=round(100*Veh/sum(Veh),1))

smmt <- tribble(~Origin,~Mn, "Germany",10.7,"UK",5.1,"Spain",3.7,"France",2.4,
                "Japan",2.2,"Czechia",2.2,"South Korea",1.4,"Slovakia",1.3,"Poland",1.0,"Belgium",0.9) %>%
  mutate(SMMT_pct=round(100*Mn/36.7,1)) %>% select(Origin,SMMT_pct)

validation_parc <- parc_2024 %>% left_join(smmt, by="Origin") %>%
  filter(!is.na(SMMT_pct)) %>% mutate(Gap_pp=round(Model_pct-SMMT_pct,1)) %>%
  arrange(desc(SMMT_pct))
cat("\n=== VALIDATION B: PARC (2024) vs SMMT Motorparc (2025) ===\n")
cat("Compares like with like: stock vs stock.\n")
print(validation_parc)
cat("\nParc mean absolute gap (pp):",
    round(mean(abs(validation_parc$Gap_pp)),2), "\n")

# ---------------------------------------------------------------------------
# 6. CLEAN BEV-ORIGIN TABLE FOR THE PAPER (2024, new registrations)
# ---------------------------------------------------------------------------
bev_country_2024 <- new_regs %>% filter(Fuel_Group=="BEV", Year==2024) %>%
  group_by(Region, Origin) %>% summarise(Reg=sum(Count), .groups="drop") %>%
  mutate(Share=round(100*Reg/sum(Reg),1)) %>% arrange(desc(Reg))
cat("\n=== NEW BEV ORIGIN BY COUNTRY (2024) — paper table ===\n")
print(bev_country_2024, n=Inf)

# ---------------------------------------------------------------------------
# 7. PLOT: new-BEV origin region trend
# ---------------------------------------------------------------------------
p <- bev_new_region %>% filter(Region %in% c("EU","UK","China","Korea","Japan")) %>%
  ggplot(aes(Year, Share, colour=Region)) +
  geom_line(linewidth=1.2) + geom_point(size=2.4) +
  scale_x_continuous(breaks=seq(2014,2024,2)) +
  labs(title="New BEV registrations by manufacturing region",
       subtitle="China's share of new BEVs (27.6% in 2024) far exceeds its all-car share",
       x="Build year cohort", y="Share of new BEV registrations (%)",
       caption="UK vehicle licensing microdata; origin keyed on year of manufacture") +
  theme_minimal(base_size=12) + theme(legend.position="bottom")
print(p)

cat("\n2024 new-BEV split: China 27.6% / EU 51.2% / UK 3.9% (consistent with ACEA all-car China 14%).\n")



# 1. Build a lookup of unique Make x GenModel x Build_Year -> Origin/Region
origin_lookup <- veh_origin %>%
  distinct(Make, GenModel, Fuel_Group, Build_Year, Origin, Region)

n_before <- nrow(join_final_complete)  

# 2. Attach to join_final_complete WITHOUT altering existing columns
join_final_complete <- join_final_complete %>%
  mutate(
    Make_key     = toupper(str_trim(Make)),
    GenModel_key = toupper(str_trim(GenModel)),
    Build_Year   = as.integer(YearManufacture)
  ) %>%
  left_join(
    origin_lookup %>% rename(Make_key = Make, GenModel_key = GenModel),
    by = c("Make_key", "GenModel_key","Fuel_Group", "Build_Year")
  ) %>%
  select(-Make_key, -GenModel_key)   # drop the temporary keys


n_before <- nrow(join_final_complete)
# ... run the join ...
cat("Rows before:", n_before, " after:", nrow(join_final_complete), "\n")
# These MUST be equal. If 'after' is larger, the lookup had duplicate keys.

join_final_complete <- join_final_complete %>%
  mutate(
    Make_key     = toupper(str_trim(Make)),
    GenModel_key = toupper(str_trim(GenModel)),
    Build_Year   = as.integer(YearManufacture)
  ) %>%
  left_join(
    origin_lookup %>% rename(Make_key = Make, GenModel_key = GenModel),
    by = c("Make_key", "GenModel_key","Fuel_Group", "Build_Year")
  ) %>%
  select(-Make_key, -GenModel_key)

cp_report(join_final_complete, "Fleet_Count", "CP1: after origin join")


#------------------------------------------------
# Create final documents
#------------------------------------------------

# Stock model for 2024 - FIXED
join_final_complete_Stock_model <- join_final_complete %>%
  # pivot_longer(cols = `2024`:`2014`,
  #              names_to = "Data_Year",
  #              values_to = "Fleet_Count") %>%
  mutate(
    Data_Year = as.numeric(Data_Year),
    YearFirstUsed = as.numeric(YearFirstUsed),
    Vehicle_Age = ifelse(Data_Year >= YearFirstUsed,
                         Data_Year - YearFirstUsed,
                         NA)) %>%
  filter(Data_Year == 2024, !is.na(Fleet_Count) & Fleet_Count > 0)

Stock_model_summary <- join_final_complete_Stock_model %>%
  filter(!is.na(Vehicle_Age)) %>%
  group_by(Data_Year, Fuel,engine_size) %>%
  summarise(
    vehicle_count = sum(Fleet_Count, na.rm = TRUE),
    avg_vehicle_age = weighted.mean(Vehicle_Age, Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Data_Year)

cat("\nStock Model - Vehicle age distribution by Fuel (2024):\n")

print(Stock_model_summary)

#--------------------------------------------------------------------------------------------------------------------------------------
# Create summary table with source information to make sure that the engine size and fuel type added to df_VEH0124_NZ&df_VEH0124_AM"
#--------------------------------------------------------------------------------------------------------------------------------------
summary_with_source <- join_final_complete  %>%
  #   pivot_longer(
  #     cols = `2024`:`2014`,
  #     names_to = "Data_Year",
  #     values_to = "Fleet_Count"
  #   ) 
  mutate(
    Data_Year = as.numeric(Data_Year),
    YearFirstUsed = as.numeric(YearFirstUsed),
    Vehicle_Age = ifelse(Data_Year >= YearFirstUsed,
                         Data_Year - YearFirstUsed,
                         NA)
  ) %>%
  filter(!is.na(Fleet_Count) & Fleet_Count > 0,source=="df_VEH0124_NZ&df_VEH0124_AM") %>%
  # Group by Data_Year AND source
  group_by(Data_Year,Fuel,engine_size) %>%
  summarise(
    vehicle_count = sum(Fleet_Count, na.rm = TRUE),
    avg_vehicle_age = weighted.mean(Vehicle_Age, Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Data_Year)

cat("\nSummary by Data Year and Source:\n")
print(summary_with_source)




summary_with_source <- summary_with_source %>% filter(Data_Year=="2024")

join_final_complete <- join_final_complete %>%
  # pivot_longer(
  #   cols = `2024`:`2014`,
  #   names_to = "Data_Year",
  #   values_to = "Fleet_Count"
  # ) %>%
  mutate(
    Data_Year = as.numeric(Data_Year),
    YearFirstUsed = as.numeric(YearFirstUsed),
    Vehicle_Age = ifelse(Data_Year >= YearFirstUsed,
                         Data_Year - YearFirstUsed,
                         NA)
  )




#----------------------------------------------------
#check the fleet Number of BEV after classification 
#----------------------------------------------------
Vcar_classified_BEV <- join_final_complete%>%
  filter(Fuel_Group == "BEV")
summery_BEV <-Vcar_classified_BEV %>%
  
  group_by(Data_Year) %>%
  summarise(
    vehicle_count = sum(Fleet_Count, na.rm = TRUE),
    avg_vehicle_age = weighted.mean(Vehicle_Age, Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  )
print(summery_BEV)

################################################################################

# apply filet and vehicle classification to new vehicle in the final document
################################################################################
# Check the classification you created earlier
cat("=== CHECKING EXISTING CLASSIFICATION IN Vcar_classified ===\n")
classification_summary <- Vcar_classified %>%
  count(Fuel_Group, vehicle_class) %>%
  arrange(Fuel_Group, vehicle_class)
print(classification_summary)

# Check if classification exists in join_final_complete
cat("\n=== CHECKING join_final_complete FOR EXISTING CLASSIFICATION ===\n")
if("vehicle_class" %in% names(join_final_complete)) {
  cat("vehicle_class column already exists in join_final_complete\n")
  current_class_summary <- join_final_complete %>%
    count(Fuel_Group, vehicle_class) %>%
    arrange(Fuel_Group, vehicle_class)
  print(current_class_summary)
} else {
  cat("vehicle_class column does NOT exist in join_final_complete\n")
}

#-------------------------------------------
#Filter out large vehicles 
#-------------------------------------------

library(purrr)

# 1. First, create the outlier analysis with the correct data source
outlier_analysis_joint <- join_final_complete %>%
  filter(Data_Year == 2024) %>%  # Changed from "2024" to 2024
  mutate(
    # Clean engine size first
    engine_size_clean = ifelse(
      engine_size %in% c("[x]", "[z]", "N/A", "NA", "", NA) | is.na(engine_size), 
      NA, 
      engine_size
    ),
    engine_size_liters = as.numeric(engine_size_clean) / 1000
  ) %>%
  filter(!is.na(engine_size_liters)) %>%  # Remove NA values for calculation
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
    # Use the SAME dataset for outlier counting
    outlier_count = map2_dbl(outlier_min, outlier_max, ~ {
      join_final_complete %>%
        filter(Data_Year == 2024) %>%
        mutate(
          engine_size_clean = ifelse(
            engine_size %in% c("[x]", "[z]", "N/A", "NA", "", NA) | is.na(engine_size), 
            NA, 
            engine_size
          ),
          engine_size_liters = as.numeric(engine_size_clean) / 1000
        ) %>%
        filter(Fuel == cur_data()$Fuel) %>%
        filter(engine_size_liters < .x | engine_size_liters > .y) %>%
        nrow()
    }),
    outlier_percentage = (outlier_count / total_vehicles) * 100
  ) %>%
  select(Fuel, total_vehicles, outlier_count, outlier_percentage, 
         min_engine_size, max_engine_size, outlier_min, outlier_max)

print(outlier_analysis_joint)

# 2. Check what columns already exist in join_final_complete
cat("Current columns in join_final_complete:\n")
print(names(join_final_complete))


############################
# remove NA and other
############################
library(tidyr)
library(dplyr)

# Remove existing outlier columns if they exist
clean <- join_final_complete %>%
  select(-any_of(c("outlier_min", "outlier_max", "custom_min", "custom_max")))
Fleet_complete <- clean
cp_report(Fleet_complete, "Fleet_Count", "CP2: Fleet_complete before kerb-weight join")




library(openxlsx)




# Check sales before imputation
sal_before <- Fleet_complete %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year) %>%
  summarise(fleet = sum(Fleet_Count, na.rm = TRUE), .groups = "drop")

print("Sales BEFORE imputation:")
print(sal_before)

totalvehicle2024 <- Fleet_complete %>%
  filter(Data_Year == 2024) %>%
  summarise(fleet = sum(Fleet_Count, na.rm = TRUE)) %>%
  print()


#=======================================================
# lets incorporate the vehicle kerb_weight to our data 
#=======================================================
library(arrow)

# Set the path to your zip file
zip_path <- "C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\RAC-Tim Chatterton\\uk_fleet_enriched_v2.zip"

# First, let's check if the zip file exists and is readable
if (!file.exists(zip_path)) {
  stop("Zip file not found at: ", zip_path)
}

# Check if we can list the contents
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

# Method 1: Extract to a specific directory (not temp)
# Create a dedicated extraction folder
extract_dir <- "C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\RAC-Tim Chatterton\\extracted"
if (!dir.exists(extract_dir)) {
  dir.create(extract_dir)
}

# Try extraction with overwrite option
tryCatch({
  unzip(zip_path, exdir = extract_dir, overwrite = TRUE)
  print("Extraction successful!")
}, error = function(e) {
  print(paste("Extraction error:", e$message))
})

# Find the parquet file
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
# === CORRECT WAYS TO VIEW THE DATA ===

# Method 1: View the entire dataset (opens in RStudio viewer)
#View(data)  # Note: Capital V, not lowercase

# Method 2: If View doesn't work, try using utils package
#utils::View(data)

# Method 3: View first few rows in console
#print(head(data, 20))  # First 20 rows

# Method 4: View structure
#str(data)

# Method 5: View column names
#names(data)

# Method 6: View summary statistics
#summary(data)

# When you load the parquet data, give it a specific name
parquet_data <- read_parquet(parquet_files[1])


# 2. Create dimension lookup from the original parquet data
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



# Impute missing kerb weights by make and model
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

# Or use fuel-specific averages
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

# Check if all NAs are filled
sum(is.na(data_imputed$rdw_kerb_weight_kg))
#------------------------------------------------------------
# include the vehicle kerb weight to the fleet complete data
#------------------------------------------------------------
# Extract unique kerb weights by vehicle identifier
data_1 <- data_1 %>% 
  filter(!is.na(rdw_kerb_weight_kg),LicenceStatus=="LICENSED") 
names(data)
library(data.table)

# Convert to data.table (this is fast)
dt <- as.data.table(data_1)

# Level 1: Exact match (Make + Model + GenModel + YearManufacture + Fuel_Group)
lookup_level1 <- dt[
  
  !is.na(rdw_kerb_weight_kg)&
    rdw_kerb_weight_kg >= 500&
    rdw_kerb_weight_kg <= 3000&
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


# Level 2: Make + Model + GenModel + YearManufacture (without Fuel_Group)
lookup_level2 <- dt[
  
  !is.na(rdw_kerb_weight_kg)&
    rdw_kerb_weight_kg >= 500&
    rdw_kerb_weight_kg <= 3000&
    LicenceStatus == "LICENSED"
]%>%
  group_by(Make, Model, GenModel, YearManufacture) %>%
  summarise(
    kerb_level2 = median(rdw_kerb_weight_kg, na.rm = TRUE),
    .groups = 'drop'
  )

# Level 3: Make + Model + GenModel (without YearManufacture and Fuel_Group)
lookup_level3 <- dt[
  
  !is.na(rdw_kerb_weight_kg)&
    rdw_kerb_weight_kg >= 500&
    rdw_kerb_weight_kg <= 3000&
    LicenceStatus == "LICENSED"
] %>%
  group_by(Make, Model, GenModel) %>%
  summarise(
    kerb_level3 = median(rdw_kerb_weight_kg, na.rm = TRUE),
    .groups = 'drop'
  )

# Level 4: Make + Model (without GenModel, YearManufacture, Fuel_Group)
lookup_level4 <- dt[
  
  !is.na(rdw_kerb_weight_kg)&
    rdw_kerb_weight_kg >= 500&
    rdw_kerb_weight_kg <= 3000&
    LicenceStatus == "LICENSED"
] %>%
  group_by(Make, Model) %>%
  summarise(
    kerb_level4 = median(rdw_kerb_weight_kg, na.rm = TRUE),
    .groups = 'drop'
  )

# Level 5: Make only
lookup_level5 <- dt[
  
  !is.na(rdw_kerb_weight_kg)&
    rdw_kerb_weight_kg >= 500&
    rdw_kerb_weight_kg <= 3000&
    LicenceStatus == "LICENSED"
] %>%
  group_by(Make) %>%
  summarise(
    kerb_level5 = median(rdw_kerb_weight_kg, na.rm = TRUE),
    .groups = 'drop'
  )

# Level 6: Fuel_Group only (overall average by fuel type)
lookup_level6 <- dt[
  
  !is.na(rdw_kerb_weight_kg)&
    rdw_kerb_weight_kg >= 500&
    rdw_kerb_weight_kg <= 3000&
    LicenceStatus == "LICENSED"
]%>%
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






# Remove any existing kerb_weight columns
Fleet_complete <- Fleet_complete %>%
  select(-any_of(c("kerb_weight_kg", "kerb_weight_kg.x", "kerb_weight_kg.y")))

# Apply all joins with priority (Level 1 is most specific, Level 6 is least)
Fleet_complete <- Fleet_complete %>%
  # Level 1: Exact match (most specific)
  left_join(lookup_level1, by = c("Make", "Model", "GenModel", "YearManufacture", "Fuel_Group")) %>%
  # Level 2: Without Fuel_Group
  left_join(lookup_level2, by = c("Make", "Model", "GenModel", "YearManufacture")) %>%
  # Level 3: Without YearManufacture and Fuel_Group
  left_join(lookup_level3, by = c("Make", "Model", "GenModel")) %>%
  # Level 4: Make + Model only
  left_join(lookup_level4, by = c("Make", "Model")) %>%
  # Level 5: Make only
  left_join(lookup_level5, by = "Make") %>%
  # Level 6: Fuel_Group only (least specific)
  left_join(lookup_level6, by = "Fuel_Group") %>%
  # Combine using coalesce (priority order: Level 1 > Level 2 > Level 3 > Level 4 > Level 5 > Level 6)
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
  # Remove intermediate columns
  select(-kerb_level1, -kerb_level2, -kerb_level3, -kerb_level4, -kerb_level5, -kerb_level6)


cp_report(Fleet_complete, "Fleet_Count", "CP3: Fleet_complete after kerb-weight join")
# Check the results
names(Fleet_complete)

# Overall match rate
total_rows <- nrow(Fleet_complete)
matched_rows <- sum(!is.na(Fleet_complete$kerb_weight_kg))
match_rate <- matched_rows / total_rows * 100

cat(sprintf("Total rows: %d\n", total_rows))
cat(sprintf("Rows with kerb weight: %d\n", matched_rows))
cat(sprintf("Overall match rate: %.1f%%\n", match_rate))

# Match rate by level
match_level_summary <- Fleet_complete %>%
  count(match_level) %>%
  mutate(
    percentage = n / sum(n) * 100,
    cumulative = cumsum(percentage)
  ) %>%
  arrange(desc(percentage))

print("=== MATCH LEVEL SUMMARY ===")
print(match_level_summary)

# Match rate by Fuel_Group
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


# Recalculate match rate
total_rows <- nrow(Fleet_complete)
matched_rows <- sum(!is.na(Fleet_complete$kerb_weight_kg))
match_rate <- matched_rows / total_rows * 100

cat(sprintf("Total rows: %d\n", total_rows))
cat(sprintf("Rows with kerb weight: %d\n", matched_rows))
cat(sprintf("Match rate: %.1f%%\n", match_rate))

# Check match level distribution
match_level_summary <- Fleet_complete %>%
  count(match_level) %>%
  mutate(
    percentage = n / sum(n) * 100,
    cumulative = cumsum(percentage)
  ) %>%
  arrange(desc(percentage))

print("=== MATCH LEVEL SUMMARY ===")
print(match_level_summary)
#------------------------------------------------------------------------------
# MODIFIED: Keep Make, Model, GenModel in fleet_known
#------------------------------------------------------------------------------

fleet_known <- Fleet_complete %>%
  filter(!is.na(Vehicle_Age), Fuel_Group != "Other") %>%
  mutate(
    engine_size = as.character(engine_size),
    vehicle_class = as.character(vehicle_class)
  ) %>%
  group_by(
    Make,           # ADDED
    Model,          # ADDED
    GenModel,       # ADDED
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

#------------------------------------------------------------------------------
# STEP 1 — Modified to keep Make, Model, GenModel
#------------------------------------------------------------------------------
# Calculate age shares by fuel group from known vehicles (aggregated for proportions)
age_shares_by_fuel <- fleet_known %>%
  filter(Fuel_Group %in% c("BEV", "Diesel", "HEV", "PHEV", "Petrol")) %>%
  group_by(Fuel_Group, Vehicle_Age, Data_Year) %>%
  summarise(n = sum(Fleet_Count), .groups = "drop") %>%
  group_by(Fuel_Group, Data_Year) %>%
  mutate(age_share = n / sum(n))

# Get totals of NA age vehicles by fuel group
na_age_fuel_totals <- Fleet_complete %>%
  filter(is.na(Vehicle_Age), Fuel_Group %in% c("BEV", "Diesel", "HEV", "PHEV", "Petrol")) %>%
  group_by(Fuel_Group, Data_Year) %>%
  summarise(total_na = sum(Fleet_Count, na.rm = TRUE), .groups = "drop")

# Get vehicle_class, engine_size, Make, Model, GenModel profiles for each fuel group
class_engine_profile_fuel <- fleet_known %>%
  filter(Fuel_Group %in% c("BEV", "Diesel", "HEV", "PHEV", "Petrol")) %>%
  group_by(Fuel_Group, Make, Model, GenModel, vehicle_class, engine_size,kerb_weight_kg, Data_Year) %>%
  summarise(
    profile_n = sum(Fleet_Count),
    .groups = "drop"
  ) %>%
  group_by(Fuel_Group, Data_Year) %>%
  mutate(profile_share = profile_n / sum(profile_n))
# Create step1 allocations
step1_base <- age_shares_by_fuel %>%
  left_join(na_age_fuel_totals, by = c("Fuel_Group", "Data_Year")) %>%
  filter(!is.na(total_na) & total_na > 0) %>%
  mutate(base_count = age_share * total_na) %>%
  filter(base_count > 0)
# Create step1 allocations with Make, Model, GenModel
step1_allocated <- step1_base %>%
  left_join(class_engine_profile_fuel, by = c("Fuel_Group", "Data_Year")) %>%
  mutate(
    Fleet_Count = base_count * profile_share,
    Fuel = NA_character_
  ) %>%
  select(Make, Model, GenModel, Vehicle_Age, Fuel, Fuel_Group, vehicle_class, engine_size,kerb_weight_kg, Fleet_Count, Data_Year) %>%
  filter(!is.na(Fleet_Count) & Fleet_Count > 0)

#------------------------------------------------------------------------------
# STEP 2 — Modified to keep Make, Model, GenModel
#------------------------------------------------------------------------------
# Get vehicle_class and engine_size profiles for each age and fuel group
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
  group_by(Vehicle_Age, Fuel_Group, Make, Model, GenModel, vehicle_class, engine_size,kerb_weight_kg, Data_Year) %>%
  summarise(
    profile_n = sum(Fleet_Count),
    .groups = "drop"
  ) %>%
  group_by(Vehicle_Age, Fuel_Group, Data_Year) %>%
  mutate(profile_share = profile_n / sum(profile_n))
step2_base <- fuel_shares_by_age %>%
  #
  left_join(other_fuel_by_age, by = c("Vehicle_Age", "Data_Year")) %>%
  filter(!is.na(total_other) & total_other > 0) %>%
  mutate(base_count = fuel_share * total_other) %>%
  filter(base_count > 0)
# Create step2 allocations with Make, Model, GenModel
step2_allocated <- step2_base %>%
  left_join(class_engine_profile_age_fuel, by = c("Vehicle_Age", "Fuel_Group", "Data_Year")) %>%
  mutate(
    Fleet_Count = base_count * profile_share,
    Fuel = NA_character_
  ) %>%
  select(Make, Model, GenModel, Vehicle_Age, Fuel, Fuel_Group, vehicle_class, engine_size,kerb_weight_kg, Fleet_Count, Data_Year) %>%
  filter(!is.na(Fleet_Count) & Fleet_Count > 0)

#------------------------------------------------------------------------------
# STEP 3 — Modified to keep Make, Model, GenModel
#------------------------------------------------------------------------------
# Get vehicle_class and engine_size profiles for each fuel group (overall)
class_engine_profile_overall <- fleet_known %>%
  group_by(Fuel_Group, Make, Model, GenModel, vehicle_class, engine_size,kerb_weight_kg, Data_Year) %>%
  summarise(
    profile_n = sum(Fleet_Count),
    .groups = "drop"
  ) %>%
  group_by(Fuel_Group, Data_Year) %>%
  mutate(profile_share = profile_n / sum(profile_n))
# Get totals of completely unknown vehicles
total_unknown_by_year <- Fleet_complete %>%
  filter(is.na(Vehicle_Age), Fuel_Group == "Other") %>%
  group_by(Data_Year) %>%
  summarise(total_unknown = sum(Fleet_Count, na.rm = TRUE), .groups = "drop")
# # Calculate age profile
age_profile <- fleet_known %>%
  group_by(Vehicle_Age, Data_Year) %>%
  summarise(n = sum(Fleet_Count), .groups = "drop") %>%
  group_by(Data_Year) %>%
  mutate(age_share = n / sum(n))

# # Calculate fuel profile within each age
fuel_profile <- fleet_known %>%
  group_by(Vehicle_Age, Fuel_Group, Data_Year) %>%
  summarise(n = sum(Fleet_Count), .groups = "drop") %>%
  group_by(Vehicle_Age, Data_Year) %>%
  mutate(fuel_share = n / sum(n))

# Step 3 allocations with Make, Model, GenModel
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
        select(Make, Model, GenModel, Vehicle_Age, Fuel, Fuel_Group, vehicle_class, engine_size,kerb_weight_kg, Fleet_Count, Data_Year) %>%
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
    kerb_weight_kg =character(),
    Fleet_Count = numeric(),
    Data_Year = integer()
  )
}

#------------------------------------------------------------------------------
# Combine all parts into final dataset WITH Make, Model, GenModel
#------------------------------------------------------------------------------
final_complete <- bind_rows(
  fleet_known,
  step1_allocated,
  step2_allocated,
  step3_allocated
) %>%
  group_by(
    Make,           # ADDED
    Model,          # ADDED
    GenModel,       # ADDED
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
# Verify Make, Model, GenModel are now present
cat("\n=== FINAL_COMPLETE NOW HAS MAKE, MODEL, GENMODEL ===\n")
cat("Columns:", paste(names(final_complete), collapse = ", "), "\n")
cat("Unique Makes:", paste(head(unique(final_complete$Make), 10), collapse = ", "), "\n")
#------------------------------------------------------------------------------
# FINAL VERIFICATION
#------------------------------------------------------------------------------
cat("\n--- FINAL VERIFICATION ---\n")
cat("Final total:", sum(final_complete$Fleet_Count), "\n")
cat("Original total:", sum(Fleet_complete$Fleet_Count, na.rm = TRUE), "\n\n")

# Check sales after imputation
sal_after <- final_complete %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year) %>%
  summarise(fleet = sum(Fleet_Count, na.rm = TRUE), .groups = "drop")

print("Sales AFTER imputation:")
print(sal_after)


# Look at 2023 new sales vs 2024 age 1
# First create the base comparison
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

# Then add the difference row
comparison <- comparison_base %>%
  bind_rows(
    data.frame(
      category = "Difference (2024 Age 1 Vehicles - 2023 New Sales)",
      vehicles = comparison_base$vehicles[2] - comparison_base$vehicles[1]
    )
  )

cat("\n=== 2023 NEW VS 2024 AGE 1 ===\n")
print(comparison)

# Create comparison across all years for Age 0 and Age 1
# Create comparison for all years (Age 0 vs next year's Age 1)
comparison_all_years <- bind_rows(
  # Age 0 for each year (except last year)
  map_dfr(2014:2023, function(year) {
    final_complete %>%
      filter(Data_Year == year, Vehicle_Age == 0) %>%
      summarise(
        category = paste0(year, " New Sales (Age 0)"),
        vehicles = sum(Fleet_Count, na.rm = TRUE),
        .groups = "drop"
      )
  }),
  
  # Age 1 for each year (except first year)
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


# Create paired comparisons directly
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

#------------------------------------------------------------------------------
# DETAILED 2024 ANALYSIS
#------------------------------------------------------------------------------
cat("\n=== DETAILED 2024 SALES ANALYSIS ===\n")

# Get 2024 sales before and after
before_2024 <- sal_before %>% filter(Data_Year == 2024) %>% pull(fleet)
after_2024 <- sal_after %>% filter(Data_Year == 2024) %>% pull(fleet)
direct_increase <- after_2024 - before_2024

# Calculate step contributions properly
step1_2024 <- step1_allocated %>%
  filter(Data_Year == 2024, Vehicle_Age == 0) %>%
  pull(Fleet_Count) %>% sum()

step2_2024 <- step2_allocated %>%
  filter(Data_Year == 2024, Vehicle_Age == 0) %>%
  pull(Fleet_Count) %>% sum()

step3_2024 <- step3_allocated %>%
  filter(Data_Year == 2024, Vehicle_Age == 0) %>%
  pull(Fleet_Count) %>% sum()

# Step 2 breakdown by fuel group
step2_by_fuel <- step2_allocated %>%
  filter(Data_Year == 2024, Vehicle_Age == 0) %>%
  group_by(Fuel_Group) %>%
  summarise(vehicles = sum(Fleet_Count), .groups = "drop")

# Verify Step 2 matches "Other" fuel before imputation
other_before <- Fleet_complete %>%
  filter(Data_Year == 2024, Fuel_Group == "Other", Vehicle_Age == 0) %>%
  summarise(total = sum(Fleet_Count)) %>% pull(total)

# Print results
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

# Verification
cat("\nVerification:\n")
cat("  Before + Step1 + Step3 =", before_2024 + step1_2024 + step3_2024, "\n")
cat("  After imputation       =", after_2024, "\n")
cat("  Match?                 =", 
    abs((before_2024 + step1_2024 + step3_2024) - after_2024) < 0.1, "\n")


########################################################################################

# Check by year
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

# Check for NAs
cat("\n--- NA CHECK ---\n")
na_summary <- final_complete %>%
  summarise(across(everything(), ~sum(is.na(.))))
print(na_summary)

# View 2024 distribution
cat("\n--- 2024 DISTRIBUTION BY FUEL GROUP ---\n")
final_complete %>%
  filter(Data_Year == 2024) %>%
  group_by(Fuel_Group) %>%
  summarise(total = sum(Fleet_Count)) %>%
  mutate(share = total / sum(total) * 100) %>%
  arrange(desc(total)) %>%
  print()

# Sample of the data
cat("\n--- SAMPLE OF FINAL DATA (2024) ---\n")
final_complete %>%
  filter(Data_Year == 2024) %>%
  sample_n(10) %>%
  print()


cat("\n=== 2024 FLEET COMPOSITION ===\n")
cat("Total vehicles:", format(sum(final_complete$Fleet_Count), big.mark = ","), "\n")
cat("Fuel types:", paste(unique(final_complete$Fuel_Group), collapse = ", "), "\n")
cat("Max age:", max(final_complete$Vehicle_Age), "\n")


# Quick checks for NA and "Other" in final data
sum(is.na(final_complete$Fleet_Count))
sum(final_complete$Fuel_Group == "Other", na.rm = TRUE)
sum(is.na(final_complete$Fuel_Group))
sum(is.na(final_complete$Vehicle_Age))
sum(is.na(final_complete$Data_Year))
sum(is.na(final_complete$engine_size))



########################
#handle NA in engine sizes 
########################



# Check missing values by fuel group before imputation
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

# Set seed for reproducibility
set.seed(123)

# Impute missing engine sizes for ALL fuel groups
final_complete_imputed <- final_complete %>%
  group_by(Fuel_Group) %>%
  mutate(
    # Convert engine_size to numeric if it's character
    engine_size_numeric = as.numeric(as.character(engine_size)),
    
    # For each fuel group, sample from its own known values
    engine_size_imputed = ifelse(
      is.na(engine_size_numeric) & n() > sum(is.na(engine_size_numeric)),
      sample(engine_size_numeric[!is.na(engine_size_numeric)], 
             size = n(), 
             replace = TRUE),
      engine_size_numeric
    )
  ) %>%
  ungroup() %>%
  # Replace the original engine_size with imputed values
  mutate(
    engine_size = engine_size_imputed
  ) %>%
  select(-engine_size_numeric, -engine_size_imputed)

# Verify imputation worked
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

# Show improvement
cat("\n=== IMPROVEMENT SUMMARY ===\n")
improvement <- missing_before %>%
  left_join(missing_after, by = "Fuel_Group", suffix = c("_before", "_after")) %>%
  mutate(
    nas_removed = na_count_before - na_count_after,
    pct_improvement = na_pct_before - na_pct_after
  ) %>%
  select(Fuel_Group, na_count_before, na_count_after, nas_removed, na_pct_before, na_pct_after)
print(improvement)

# Update your working dataset
final_complete <- final_complete_imputed
join_final_complete_clean <- final_complete
cp_report(join_final_complete_clean, "Fleet_Count", "CP5: join_final_complete_clean")

# Final check
cat("\n=== FINAL CHECK ===\n")
cat("Total NAs remaining:", sum(is.na(final_complete$engine_size)), "\n")

sum(is.na(join_final_complete_clean$engine_size))
sum(is.na(join_final_complete_clean$kerb_weight_kg))
unique(join_final_complete_clean$engine_size)
# Convert kerb_weight_kg to numeric

join_final_complete_clean <- join_final_complete_clean %>%
  mutate(
    kerb_weight_kg = as.numeric(as.character(kerb_weight_kg))
  )

# Check if any conversion issues
cat("Rows with NA after conversion:", sum(is.na(join_final_complete_clean$kerb_weight_kg)), "\n")

# our analysis shows that there are 829 rows missing vehicle ker weight with HEV vehicles 
# we take median kerb weight of HEV vehicles and impute it in the missing rows.
# Get the median kerb weight for HEV vehicles
hev_median <- join_final_complete_clean %>%
  filter(Fuel_Group == "HEV", !is.na(kerb_weight_kg)) %>%
  summarise(median_kerb = median(kerb_weight_kg, na.rm = TRUE)) %>%
  pull(median_kerb)

cat(sprintf("HEV median kerb weight: %.0f kg\n", hev_median))

# Impute COLEMAN MILNE vehicles with HEV median
join_final_complete_clean <- join_final_complete_clean %>%
  mutate(
    kerb_weight_kg = ifelse(
      Make == "COLEMAN MILNE" & is.na(kerb_weight_kg),
      hev_median,
      kerb_weight_kg
    )
  )

# Check if all are filled
sum(is.na(join_final_complete_clean$kerb_weight_kg))

vehicel_numer <- join_final_complete_clean%>%
  group_by(Data_Year)%>%
  summarise(fleet = sum(Fleet_Count, na.rm = TRUE), .groups = "drop")%>%
  print

#######################################################################################


# Check current missing values by fuel group
cat("Missing values BEFORE imputation:\n")
join_final_complete_clean %>%
  group_by(Fuel_Group) %>%
  summarise(
    total = n(),
    na_count = sum(is.na(engine_size)),
    na_pct = round(mean(is.na(engine_size)) * 100, 2)
  ) %>%
  print()

# Create distribution from KNOWN BEV engine sizes only
bev_known_values <- join_final_complete_clean %>%
  filter(Fuel_Group == "BEV", !is.na(engine_size)) %>%
  pull(engine_size)

cat("\nBEV known values for imputation:", length(bev_known_values), "\n")
cat("BEV missing values to impute:", 
    sum(is.na(join_final_complete_clean$engine_size[join_final_complete_clean$Fuel_Group == "BEV"])), "\n")

# Set seed for reproducibility
set.seed(123)

# Impute ONLY BEV missing values
Vcar_for_plotss <- join_final_complete_clean %>%
  mutate(
    engine_size= ifelse(
      Fuel_Group == "BEV" & is.na(engine_size),
      sample(bev_known_values, size = n(), replace = TRUE)[row_number()],
      engine_size
    ),
    engine_size_liters = engine_size / 1000
  )
cp_report(Vcar_for_plotss, "Fleet_Count", "CP6: after BEV engine-size imputation") 

# Verify imputation worked
cat("\nMissing values AFTER imputation (BEV only):\n")
Vcar_for_plotss %>%
  group_by(Fuel_Group) %>%
  summarise(
    total = n(),
    na_count = sum(is.na(engine_size)),
    na_pct = round(mean(is.na(engine_size)) * 100, 2)
  ) %>%
  print()

# Check the specific Range Extended Electric vehicles
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


join_final_complete_clean<-Vcar_for_plotss 

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



# # Count distinct engine size values (not vehicles)
vehicle_size_range <- join_final_complete_clean%>%  #join_final_complete_classified should
  filter(Data_Year == "2024", !is.na(engine_size)) %>%
  # Create the size_range column from engine_size_numeric
  mutate(
    size_range = case_when(
      engine_size>= 0 & engine_size < 500 ~ "0 - 499",
      engine_size>= 500 & engine_size < 1000 ~ "500 - 999",
      engine_size >= 1000 & engine_size < 1500 ~ "1000 - 1499",
      engine_size >= 1500 & engine_size < 2000 ~ "1500 - 1999",
      engine_size >= 2000 & engine_size < 2500 ~ "2000 - 2499",
      engine_size >= 2500 & engine_size < 3000 ~ "2500 - 2999",
      engine_size >= 3000 & engine_size < 3500 ~ "3000 - 3499",
      engine_size >= 3500 & engine_size < 4000 ~ "3500 - 3999",
      engine_size >= 4000 & engine_size < 4500 ~ "4000 - 4499",
      engine_size >= 4500 & engine_size < 5000 ~ "4500 - 4999",
      engine_size >= 5000 & engine_size < 5500 ~ "5000 - 5499",
      engine_size >= 5500 & engine_size< 6000 ~ "5500 - 5999",
      engine_size >= 6000 & engine_size< 6500 ~ "6000 - 6499",
      engine_size== 6500 ~ "6500",
      TRUE ~ "Other"
    )
  ) %>%
  # Group by size_range and Fuel_Group
  group_by(size_range, Fuel_Group) %>%
  # Summarise to get counts
  summarise(
    Count = (sum(Fleet_Count)),  # Counts rows (each row is a vehicle model/year combination)
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = size_range,
    names_from = Fuel_Group,
    values_from = Count,
    values_fill = 0
  ) %>%
  # ROUND ONLY AT THE VERY END
  mutate(across(BEV:Petrol, ~ round(.x, 0))) %>%
  arrange(
    factor(size_range, levels = c(
      "0 - 499", "500 - 999", "1000 - 1499", "1500 - 1999",
      "2000 - 2499", "2500 - 2999", "3000 - 3499", "3500 - 3999",
      "4000 - 4499", "4500 - 4999", "5000 - 5499", "5500 - 5999",
      "6000 - 6499", "6500", "Other"
    ))
  )

print("=== ROW COUNTS BY ENGINE SIZE RANGE ===")
print(vehicle_size_range)



#####################################################
# First, make sure you have the vehicle_class column from your classification code
# Then create the summary table for 2024 data

library(dplyr)
library(tidyr)
library(stringr)

vehicle_summary <- join_final_complete_clean %>%   # i think this should be join_final_complete_classified
  filter(Data_Year == "2024") %>%
  
  # Extract base size category from vehicle_class
  mutate(
    size_category = case_when(
      str_detect(vehicle_class, "Very Large") ~ "Very Large",
      str_detect(vehicle_class, "Very") ~ "Very Large",  # Catch any "Very" variations
      str_detect(vehicle_class, "Large") & !str_detect(vehicle_class, "Very") ~ "Large",
      str_detect(vehicle_class, "Medium") ~ "Medium",
      str_detect(vehicle_class, "Small") ~ "Small",
      str_detect(vehicle_class, "Unsized") ~ "Unsized",
      TRUE ~ "Unclassified"
    )
    
  ) %>%
  
  # Group by size category and fuel group
  group_by(size_category, Fuel_Group) %>%
  summarise(
    Count = sum(Fleet_Count, na.rm = TRUE),
    # Get engine size statistics
    engine_size_min = ifelse(all(is.na(engine_size_clean)),
                             NA_real_,
                             min(engine_size_clean, na.rm = TRUE)),
    engine_size_max = ifelse(all(is.na(engine_size_clean)),
                             NA_real_,
                             max(engine_size_clean, na.rm = TRUE)),
    engine_size_mean = ifelse(all(is.na(engine_size_clean)),
                              NA_real_,
                              mean(engine_size_clean, na.rm = TRUE)),
    engine_size_range = ifelse(all(is.na(engine_size_clean)),
                               "N/A",
                               paste0(
                                 round(min(engine_size_clean, na.rm = TRUE)/1000, 2), " - ",
                                 round(max(engine_size_clean, na.rm = TRUE)/1000, 2), " L"
                               )),
    .groups = "drop"
  )%>%
  
  # Create the pivot table for counts
  select(size_category, Fuel_Group, Count, engine_size_range) %>%
  pivot_wider(
    id_cols = size_category,
    names_from = Fuel_Group,
    values_from = Count,
    values_fill = 0
  )%>%
  # Order categories properly
  mutate(size_category = factor(size_category,
                                levels = c("Small", "Medium", "Large", "Very Large", "Unsized", "Unclassified"))) %>%
  arrange(size_category)

# View the result
print(vehicle_summary)



# If you want a more detailed table with engine size ranges for each fuel type:
vehicle_summary_detailed <- join_final_complete_clean %>%
  filter(Data_Year == "2024") %>%
  
  mutate(
    size_category = case_when(
      str_detect(vehicle_class, "Very Large") ~ "Very Large",
      str_detect(vehicle_class, "Very") ~ "Very Large",  # Catch any "Very" variations
      str_detect(vehicle_class, "Large") & !str_detect(vehicle_class, "Very") ~ "Large",
      str_detect(vehicle_class, "Medium") ~ "Medium",
      str_detect(vehicle_class, "Small") ~ "Small",
      str_detect(vehicle_class, "Unsized") ~ "Unsized",
      TRUE ~ "Unclassified"
    ),
    
    engine_size_liters = engine_size_clean / 1000
  ) %>%
  
  group_by(size_category, Fuel_Group) %>%
  summarise(
    Count = sum(Fleet_Count),
    Engine_Size_Range = ifelse(
      all(is.na(engine_size_liters)),
      "N/A",
      paste0(
        round(min(engine_size_liters, na.rm = TRUE), 2), " - ",
        round(max(engine_size_liters, na.rm = TRUE), 2), " L"
      )
    ),
    Avg_Engine_Size = round(mean(engine_size_liters, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  
  # Create a formatted column
  mutate(Summary = paste0(Count, " (", Engine_Size_Range, ")")) %>%
  select(size_category, Fuel_Group, Summary) %>%
  pivot_wider(
    id_cols = size_category,
    names_from = Fuel_Group,
    values_from = Summary,
    values_fill = "0 (N/A)"
  )%>%
  # Order categories properly
  mutate(size_category = factor(size_category, 
                                levels = c("Small", "Medium", "Large", "Very Large", "Unsized", "Unclassified"))) %>%
  arrange(size_category)

# View detailed table
print(vehicle_summary_detailed,width = Inf)

# Order the size categories properly
size_order <- c("Small", "Medium", "Large", "Very Large", "Unsized", "Other", "Unclassified")

vehicle_summary <- vehicle_summary %>%
  mutate(size_category = factor(size_category, levels = size_order)) %>%
  arrange(size_category)

vehicle_summary_detailed <- vehicle_summary_detailed %>%
  mutate(size_category = factor(size_category, levels = size_order)) %>%
  arrange(size_category)

print(vehicle_summary)
print(vehicle_summary_detailed)





# 4. Add new outlier analysis and custom ranges
join_final_complete_updated <- join_final_complete_clean %>%
  left_join(outlier_analysis_joint %>% select(Fuel, outlier_min, outlier_max), 
            by = "Fuel") 
cp_report(join_final_complete_updated, "Fleet_Count", "CP7: after outlier bounds join")

# 5. Verify the update
cat("\nUpdated columns in join_final_complete:\n")
print(names(join_final_complete_updated))

# 6. Check BEV outliers specifically
# Fix the inconsistent Fuel names
join_final_complete_updated <- join_final_complete_updated %>%
  mutate(
    # Standardize "Battery electric" to "Battery Electric"
    Fuel = case_when(
      str_to_lower(Fuel) == "battery electric" ~ "Battery Electric",
      TRUE ~ Fuel
    ),
    # Ensure all Battery Electric vehicles are in BEV group
    Fuel_Group = case_when(
      Fuel == "Battery Electric" ~ "BEV",
      Fuel == "Range Extended Electric" ~ "BEV",
      TRUE ~ Fuel_Group
    )
  )

# Verify the fix
cat("Standardized Fuel values:\n")
fuel_summary <- join_final_complete_updated %>%
  count( Fuel_Group) %>%
  arrange(Fuel_Group)
print(fuel_summary)

# Check BEV specifically
cat("\nBEV Summary:\n")
bev_summary <- join_final_complete_updated %>%
  filter(Fuel_Group == "BEV") %>%
  count(Fuel_Group) %>%
  mutate(percentage = n / sum(n) * 100)
print(bev_summary)


# 7. APPLY THE FILTER TO REMOVE OUTLIERS
join_final_complete_filtered <- join_final_complete_updated 



join_final_complete_filtered_summery <- join_final_complete_filtered %>%
  group_by(Data_Year) %>%
  summarise(total_vehicle = sum(Fleet_Count, na.rm = TRUE)) %>%
  ungroup()

print(join_final_complete_filtered_summery)



#----------------------------------------
# 8. Compare before and after filtering
cat("\n=== FILTERING RESULTS ===\n")
#------------------------------------
# Count vehicles before filtering
before_filter <- join_final_complete_updated %>%
  filter(Data_Year == 2024) %>%
  summarise(
    total_vehicles = sum(Fleet_Count, na.rm = TRUE),
    total_rows = n()
  )
print(before_filter)
# Count vehicles after filtering
after_filter <- join_final_complete_filtered %>%
  filter(Data_Year == 2024) %>%
  summarise(
    total_vehicles = sum(Fleet_Count, na.rm = TRUE),
    total_rows = n()
  )
print(after_filter)



################################################################################
# APPLY VEHICLE CLASSIFICATION TO THE FINAL DOCUMENT
################################################################################

# 1. First, extract the classification rules from Vcar_classified
cat("\n=== EXTRACTING CLASSIFICATION RULES ===\n")

# Get the percentile thresholds used for classification
classification_thresholds <- Vcar_classified %>%
  
  filter(!is.na(engine_size_liters)) %>%
  group_by(Fuel_Group) %>%
  summarise(
    p25 = quantile(engine_size_liters, 0.25, na.rm = TRUE),
    p50 = quantile(engine_size_liters, 0.50, na.rm = TRUE),
    p75 = quantile(engine_size_liters, 0.75, na.rm = TRUE),
    p100= quantile(engine_size_liters, 1, na.rm = TRUE),
    .groups = 'drop'
  )

print(classification_thresholds)


# Calculate actual min values from Vcar_classified
min_values <- Vcar_classified %>%
  filter(`2024` > 0) %>%
  group_by(Fuel_Group) %>%
  summarise(min_value = min(engine_size_liters, na.rm = TRUE))

# Join with classification thresholds
classification_thresholds <- classification_thresholds %>%
  left_join(min_values, by = "Fuel_Group")

# Create the size ranges table
size_ranges_by_fuel <- classification_thresholds %>%
  mutate(
    `Small` = paste0(round(min_value, 2), " - ", round(p25, 2)),
    `Medium` = paste0(round(p25, 2), " - ", round(p50, 2)),
    `Large` = paste0(round(p50, 2), " - ", round(p75, 2)),
    `Very Large` = paste0(round(p75, 2), " - ", round(p100, 2))
  ) %>%
  select(Fuel_Group, `Small`, `Medium`, `Large`, `Very Large`)

print("=== ENGINE SIZE RANGES BY FUEL TYPE AND VEHICLE CLASS ===")
print(size_ranges_by_fuel)


# 2. Apply classification to join_final_complete_filtered
cat("\n=== APPLYING CLASSIFICATION TO FILTERED DATASET ===\n")

join_final_complete_classified <- join_final_complete_filtered %>%
  
  left_join(classification_thresholds, by = "Fuel_Group") %>%
  mutate(
    # Apply the same classification logic
    vehicle_class = case_when(
      # Diesel
      Fuel_Group == "Diesel" & is.na(engine_size_liters) ~ "Unsized Diesel",
      Fuel_Group == "Diesel" & engine_size_liters <= p25 ~ "Small Diesel",
      Fuel_Group == "Diesel" & engine_size_liters > p25 & engine_size_liters <= p50 ~ "Medium Diesel",
      Fuel_Group == "Diesel" & engine_size_liters > p50 & engine_size_liters <= p75 ~ "Large Diesel",
      Fuel_Group == "Diesel" & engine_size_liters > p75 ~ "Very Large Diesel",
      
      # Petrol
      Fuel_Group == "Petrol" & is.na(engine_size_liters) ~ "Unsized Petrol",
      Fuel_Group == "Petrol" & engine_size_liters <= p25 ~ "Small Petrol",
      Fuel_Group == "Petrol" & engine_size_liters > p25 & engine_size_liters <= p50 ~ "Medium Petrol",
      Fuel_Group == "Petrol" & engine_size_liters > p50 & engine_size_liters <= p75 ~ "Large Petrol",
      Fuel_Group == "Petrol" & engine_size_liters > p75 ~ "Very Large Petrol",
      
      # HEV (Hybrid Electric Vehicle)
      Fuel_Group == "HEV" & is.na(engine_size_liters) ~ "Unsized HEV",
      Fuel_Group == "HEV" & engine_size_liters <= p25 ~ "Small HEV",
      Fuel_Group == "HEV" & engine_size_liters > p25 & engine_size_liters <= p50 ~ "Medium HEV",
      Fuel_Group == "HEV" & engine_size_liters > p50 & engine_size_liters <= p75 ~ "Large HEV",
      Fuel_Group == "HEV" & engine_size_liters > p75 ~ "Very Large HEV",
      
      # PHEV (Plug-in Hybrid Electric Vehicle)
      Fuel_Group == "PHEV" & is.na(engine_size_liters) ~ "Unsized PHEV",
      Fuel_Group == "PHEV" & engine_size_liters <= p25 ~ "Small PHEV",
      Fuel_Group == "PHEV" & engine_size_liters > p25 & engine_size_liters <= p50 ~ "Medium PHEV",
      Fuel_Group == "PHEV" & engine_size_liters > p50 & engine_size_liters <= p75 ~ "Large PHEV",
      Fuel_Group == "PHEV" & engine_size_liters > p75 ~ "Very Large PHEV",
      
      # BEV (Battery Electric Vehicle) - using DVLA revenue weight
      Fuel_Group == "BEV" & is.na(engine_size_liters) ~ "Unsized BEV",
      Fuel_Group == "BEV" & engine_size_liters <= p25 ~ "Small BEV",
      Fuel_Group == "BEV" & engine_size_liters > p25 & engine_size_liters <= p50 ~ "Medium BEV",
      Fuel_Group == "BEV" & engine_size_liters > p50 & engine_size_liters <= p75 ~ "Large BEV",
      Fuel_Group == "BEV" & engine_size_liters > p75 ~ "Very Large BEV",
      
      # Other
      Fuel_Group == "Other" ~ "Unclassified",
      
      TRUE ~ "Unclassified"
    )
  ) %>%
  select(-p25, -p50, -p75)

cp_report(join_final_complete_classified, "Fleet_Count", "CP8: after classification join")

# 3. Check the classification results
cat("\n=== CLASSIFICATION RESULTS FOR FILTERED DATASET ===\n")
classified_summary <- join_final_complete_classified %>%
  count(Fuel_Group, vehicle_class) %>%
  arrange(Fuel_Group, vehicle_class)
print(classified_summary)



# 5. Check fleet counts by classification
cat("\n=== FLEET COUNTS BY VEHICLE CLASS (2024) ===\n")
fleet_by_class <- join_final_complete_classified %>%
  filter(Data_Year == 2024) %>%
  group_by(Fuel_Group, vehicle_class) %>%
  summarise(
    total_vehicles = sum(Fleet_Count, na.rm = TRUE),
    # unique_models = n_distinct(Model),
    .groups = 'drop'
  ) %>%
  arrange(Fuel_Group, vehicle_class)

print(fleet_by_class)

engine_size_range<-join_final_complete_classified%>%
  group_by(Fuel_Group)%>%
  summarise(
    p25 = quantile(engine_size_liters, 0.25, na.rm = TRUE),
    p50 = quantile(engine_size_liters, 0.50, na.rm = TRUE),
    p75 = quantile(engine_size_liters, 0.75, na.rm = TRUE),
    p100= quantile(engine_size_liters, 1, na.rm = TRUE),
    .groups = 'drop'
  )
print(engine_size_range)

# Quick verification
verification <- join_final_complete_classified %>%
  filter(Data_Year == 2024, Fuel_Group != "BEV") %>%
  summarise(
    min_cc = min(engine_size, na.rm = TRUE),
    max_cc = max(engine_size, na.rm = TRUE),
    below_500 = sum(engine_size < 500, na.rm = TRUE),
    above_6500 = sum(engine_size > 6500, na.rm = TRUE),
    total_vehicles = sum(Fleet_Count, na.rm = TRUE)
  )

cat("\n=== VERIFICATION ===\n")
print(verification)

join_final_complete_classified_summery <- join_final_complete_classified %>%
  group_by(Data_Year,vehicle_class) %>%
  summarise(total_vehicle = sum(Fleet_Count, na.rm = TRUE)) %>%
  ungroup()

print(join_final_complete_classified_summery)

# Corrected code for new sales calculation
new_sale <- join_final_complete_classified %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year) %>%
  summarise(sale = sum(Fleet_Count, na.rm = TRUE), .groups = "drop")

print(new_sale)




# Look at 2023 new sales vs 2024 age 1
comparison <- bind_rows(
  join_final_complete_classified %>%
    filter(Data_Year == 2023, Vehicle_Age == 0) %>%
    summarise(
      category = "2023 New Sales",
      vehicles = sum(Fleet_Count, na.rm = TRUE)
    ),
  join_final_complete_classified %>%
    filter(Data_Year == 2024, Vehicle_Age == 1) %>%
    summarise(
      category = "2024 Age 1 Vehicles",
      vehicles = sum(Fleet_Count, na.rm = TRUE)
    )
)

cat("\n=== 2023 NEW VS 2024 AGE 1 ===\n")
print(comparison)



#---------
# Export
#---------
setwd("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\R-programming-Infuze\\R-programming_Updated analysis\\New-updated_data\\Excell_files")
#write_csv_zipped(join_final_complete_Stock_model, "stock_model.csv")


######################################################################################



# ===============================================
# VISUALIZE VEHICLE CLASSIFICATION AND CAR AGES
#================================================

# --- 1. VEHICLE CLASS COLOR GRADIENTS -----------------
petrol_colors <- colorRampPalette(c("#81c3d7", "#3a7ca5", "#2f6690", "#16425b"))(5)
diesel_colors <- colorRampPalette(c("#a7333f", "#ce4257", "#a22522", "#720026"))(5)
hev_colors    <- colorRampPalette(c("#BCD4A1", "#a3b18a", "#87B26B", "#558B2F"))(5)
phev_colors   <- colorRampPalette(c("#F8E8F2", "#D4A1BC", "#B26B87", "#8B2F55"))(5)
bev_colors    <- colorRampPalette(c("#FFE0A1", "#FFC96B", "#FFA52F", "#bc6c25"))(5)

vehicle_class_colors <- c(
  # PETROL
  "Small Petrol"       = petrol_colors[1],
  "Medium Petrol"      = petrol_colors[2],
  "Large Petrol"       = petrol_colors[3],
  "Very Large Petrol"  = petrol_colors[4],
  "Unsized Petrol"     = petrol_colors[5],
  
  # DIESEL
  "Small Diesel"       = diesel_colors[1],
  "Medium Diesel"      = diesel_colors[2],
  "Large Diesel"       = diesel_colors[3],
  "Very Large Diesel"  = diesel_colors[4],
  "Unsized Diesel"     = diesel_colors[5],
  
  # HEV
  "Small HEV"          = hev_colors[1],
  "Medium HEV"         = hev_colors[2],
  "Large HEV"          = hev_colors[3],
  "Very Large HEV"     = hev_colors[4],
  "Unsized HEV"        = hev_colors[5],
  
  # PHEV
  "Small PHEV"         = phev_colors[1],
  "Medium PHEV"        = phev_colors[2],
  "Large PHEV"         = phev_colors[3],
  "Very Large PHEV"    = phev_colors[4],
  "Unsized PHEV"       = phev_colors[5],
  
  # BEV
  "Small BEV"          = bev_colors[1],
  "Medium BEV"         = bev_colors[2],
  "Large BEV"          = bev_colors[3],
  "Very Large BEV"     = bev_colors[4],
  "Unsized BEV"        = bev_colors[5],
  
  # Unclassified
  "Unclassified"       = "#D3D3D3"
)
# 1. FLEET COMPOSITION BY VEHICLE CLASS OVER TIME
# Define the correct order for the legend - grouped by fuel type with gradients
vehicle_class_levels <- c(
  # Petrol group
  "Small Petrol", "Medium Petrol", "Large Petrol", "Very Large Petrol", 
  # Diesel group
  "Small Diesel", "Medium Diesel", "Large Diesel", "Very Large Diesel", 
  # HEV group
  "Small HEV", "Medium HEV", "Large HEV", "Very Large HEV", 
  # PHEV group
  "Small PHEV", "Medium PHEV", "Large PHEV", "Very Large PHEV",
  # BEV group
  "Small BEV", "Medium BEV", "Large BEV", "Very Large BEV"
)

# 1. FLEET COMPOSITION BY VEHICLE CLASS OVER TIME
fleet_composition_plot <- join_final_complete_clean %>%
  # Don't filter out Unclassified if you want to see them
  filter(
    !is.na(Fleet_Count),
    Fleet_Count > 0
    # Fuel_Group %in% c("HEV", "PHEV", "Diesel", "Petrol", "BEV", "Other")
  ) %>%
  # Convert to factor with the ordered levels
  mutate(
    vehicle_class = factor(vehicle_class, levels = vehicle_class_levels),
    # Ensure NA vehicle_class becomes "Unclassified"
    vehicle_class = ifelse(is.na(vehicle_class), "Unclassified", as.character(vehicle_class)),
    vehicle_class = factor(vehicle_class, levels = vehicle_class_levels)
  ) %>%
  group_by(Data_Year, vehicle_class) %>%
  summarise(total_fleet = sum(Fleet_Count, na.rm = TRUE), .groups = 'drop') %>%
  # Check what vehicle classes actually exist
  {
    cat("=== ACTUAL VEHICLE CLASSES IN DATA ===\n")
    print(unique(.$vehicle_class))
    cat("\n")
    .
  }

fleet_plot_visual <- ggplot(fleet_composition_plot, 
                            aes(x = Data_Year, y = total_fleet, fill = vehicle_class)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(
    values = vehicle_class_colors,
    name = "Vehicle Class",
    breaks = vehicle_class_levels,  # Force legend order
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(
      min(fleet_composition_plot$Data_Year, na.rm = TRUE),
      max(fleet_composition_plot$Data_Year, na.rm = TRUE),
      by = 2
    )
  ) +
  labs(
    x = "Year",
    y = "Number of Vehicles in Fleet"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.2, "cm"),
    legend.key.height = unit(0.5, "cm")
  ) +
  guides(fill = guide_legend(
    ncol = 1,
    keyheight = unit(0.4, "cm"),
    byrow = TRUE
  ))

print(fleet_plot_visual)

# Check what's actually being plotted
cat("\n=== DATA SUMMARY ===\n")
summary_data <- join_final_complete %>%
  filter(!is.na(Fleet_Count), Fleet_Count > 0) %>%
  count(vehicle_class) %>%
  arrange(desc(n))

print(summary_data)


#==========================
#fleet composition
#=========================
# First, let's check what vehicle_class values actually exist
cat("=== CHECKING VEHICLE CLASS VALUES ===\n")
unique_classes <- join_final_complete_clean %>%
  filter(Fuel_Group %in% c("HEV", "PHEV", "Diesel", "Petrol", "BEV")) %>%
  distinct(vehicle_class) %>%
  arrange(vehicle_class)

print(unique_classes)

# Now create the table with proper handling
fleet_composition_table <- join_final_complete_clean %>%
  filter(
    !is.na(vehicle_class),
    !is.na(Fleet_Count),
    Fleet_Count > 0,
    Fuel_Group %in% c("HEV", "PHEV", "Diesel", "Petrol", "BEV")
  ) %>%
  mutate(
    # Clean vehicle_class - ensure it's a factor
    vehicle_class = factor(vehicle_class, levels = vehicle_class_levels)
  ) %>%
  group_by(Data_Year, Fuel_Group, vehicle_class) %>%
  summarise(total_fleet = sum(Fleet_Count, na.rm = TRUE), .groups = 'drop') %>%
  # Pivot to wide format for the years
  pivot_wider(
    names_from = Data_Year,
    values_from = total_fleet,
    values_fill = 0
  )

# For 2024 table with size categories
fleet_composition_2024_table <- fleet_composition_table %>%
  select(Fuel_Group, vehicle_class, `2024`) %>%
  rename(Vehicles_2024 = `2024`) %>%
  mutate(
    # Extract size from vehicle_class (handles all cases)
    Vehicle_Size = case_when(
      str_detect(vehicle_class, "^Small") ~ "Small",
      str_detect(vehicle_class, "^Medium") ~ "Medium",
      str_detect(vehicle_class, "^Large") & !str_detect(vehicle_class, "Very Large") ~ "Large",
      str_detect(vehicle_class, "^Very Large") ~ "Very Large",
      str_detect(vehicle_class, "^Unsized") ~ "Unsized",
      TRUE ~ "Other"
    ),
    
    # Clean up fuel type from vehicle_class
    Clean_Fuel = str_replace_all(
      as.character(vehicle_class),
      c("^Small " = "", "^Medium " = "", "^Large " = "", 
        "^Very Large " = "", "^Unsized " = "")
    )
  ) %>%
  # Group by Fuel_Group and Vehicle_Size (sum across any fuel type variations)
  group_by(Fuel_Group, Vehicle_Size) %>%
  summarise(Vehicles_2024 = sum(Vehicles_2024, na.rm = TRUE), .groups = 'drop') %>%
  # Pivot to wide format
  pivot_wider(
    names_from = Vehicle_Size,
    values_from = Vehicles_2024,
    values_fill = list(Vehicles_2024 = 0)
  ) %>%
  # Ensure all size columns exist
  {
    # Define all possible size columns
    all_size_cols <- c("Small", "Medium", "Large", "Very Large", "Unsized")
    
    # Add missing columns with 0
    for(col in all_size_cols) {
      if(!col %in% names(.)) {
        .[[col]] <- 0
      }
    }
    
    # Reorder columns
    select(., Fuel_Group, all_of(all_size_cols))
  } %>%
  # Add row sums
  mutate(
    Row_Total = rowSums(across(where(is.numeric)), na.rm = TRUE)
  ) %>%
  # Order by Fuel_Group
  arrange(Fuel_Group)

# Calculate column totals
column_totals <- fleet_composition_2024_table %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(Fuel_Group = "TOTAL (Columns)")

# Create total row
total_row <- column_totals %>%
  mutate(
    # Row_Total should be sum of all vehicles (same as sum of column totals)
    Row_Total = sum(column_totals %>% select(-Fuel_Group) %>% rowSums(na.rm = TRUE))
  )

# Combine everything
fleet_composition_2024_table_with_totals <- bind_rows(
  fleet_composition_2024_table,
  total_row
)

# Print the complete table
cat("\n=== FLEET COMPOSITION 2024 ===\n")
print(fleet_composition_2024_table_with_totals, n = Inf)


#========================================================================
# PLOT WITH SIMPLIFIED CLASSIFICATION
#========================================================================

# Check what vehicle_class values we have for the plot
plot_data_check <- join_final_complete_clean %>%
  filter(
    !is.na(vehicle_class),
    !is.na(Fleet_Count),
    Fleet_Count > 0,
    Fuel_Group %in% c("HEV", "PHEV", "Diesel", "Petrol", "BEV")
  ) %>%
  count(vehicle_class) %>%
  arrange(desc(n))

cat("\n=== VEHICLE CLASS DISTRIBUTION FOR PLOT ===\n")
print(plot_data_check)

# Create the plot with simplified classification
fleet_composition_plot <- join_final_complete_clean %>%
  filter(
    !is.na(vehicle_class),
    !is.na(Fleet_Count),
    Fleet_Count > 0,
    Fuel_Group %in% c("HEV", "PHEV", "Diesel", "Petrol", "BEV")
  ) %>%
  # Create new classification
  mutate(
    new_vehicle_class = case_when(
      str_detect(vehicle_class, "Petrol") ~ 
        case_when(
          str_detect(vehicle_class, "Small") ~ "Small Petrol",
          str_detect(vehicle_class, "Medium") ~ "Medium Petrol",
          str_detect(vehicle_class, "Large") & !str_detect(vehicle_class, "Very Large") ~ "Large Petrol",
          str_detect(vehicle_class, "Very Large") ~ "Very Large Petrol",
          str_detect(vehicle_class, "Unsized") ~ "Unsized Petrol",
          TRUE ~ "Petrol"
        ),
      
      str_detect(vehicle_class, "Diesel") ~ 
        case_when(
          str_detect(vehicle_class, "Small") ~ "Small Diesel",
          str_detect(vehicle_class, "Medium") ~ "Medium Diesel",
          str_detect(vehicle_class, "Large") & !str_detect(vehicle_class, "Very Large") ~ "Large Diesel",
          str_detect(vehicle_class, "Very Large") ~ "Very Large Diesel",
          str_detect(vehicle_class, "Unsized") ~ "Unsized Diesel",
          TRUE ~ "Diesel"
        ),
      
      str_detect(vehicle_class, "PHEV") ~ "PHEV",
      str_detect(vehicle_class, "HEV") ~ "HEV",
      str_detect(vehicle_class, "BEV") ~ "BEV",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(Data_Year, new_vehicle_class) %>%
  summarise(total_fleet = sum(Fleet_Count, na.rm = TRUE), .groups = 'drop')

# Define colors for simplified classification
new_vehicle_class_colors <- c(
  # Petrol variants
  "Small Petrol"       = petrol_colors[1],
  "Medium Petrol"      = petrol_colors[2],
  "Large Petrol"       = petrol_colors[3],
  "Very Large Petrol"  = petrol_colors[4],
  "Unsized Petrol"     = petrol_colors[5],
  
  # Diesel variants
  "Small Diesel"       = diesel_colors[1],
  "Medium Diesel"      = diesel_colors[2],
  "Large Diesel"       = diesel_colors[3],
  "Very Large Diesel"  = diesel_colors[4],
  "Unsized Diesel"     = diesel_colors[5],
  
  # Other fuel types
  "HEV"  = "#8FB573",    # Medium olive green
  "PHEV" = "#B5738F",    # Medium dusty red
  "BEV"  = "#FFB366",    # Medium golden yellow
  "Other" = "#D3D3D3"    # Light gray
)

# Define the order for legend (based on what actually exists)
actual_classes <- unique(fleet_composition_plot$new_vehicle_class)
new_vehicle_class_levels <- c(
  intersect(c("Small Petrol", "Medium Petrol", "Large Petrol", "Very Large Petrol", "Unsized Petrol"), actual_classes),
  intersect(c("Small Diesel", "Medium Diesel", "Large Diesel", "Very Large Diesel", "Unsized Diesel"), actual_classes),
  intersect(c("HEV", "PHEV", "BEV", "Other"), actual_classes)
)

cat("\n=== CLASSES FOR PLOT LEGEND ===\n")
print(new_vehicle_class_levels)

# Create the plot
fleet_composition_plot_visual <- fleet_composition_plot %>%
  mutate(
    new_vehicle_class = factor(new_vehicle_class, levels = new_vehicle_class_levels)
  ) %>%
  ggplot(aes(x = Data_Year, y = total_fleet, fill = new_vehicle_class)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(
    values = new_vehicle_class_colors,
    name = "Vehicle Class",
    breaks = new_vehicle_class_levels,
    drop = FALSE  # Keep all levels in legend
  ) +
  scale_x_continuous(
    breaks = seq(
      min(fleet_composition_plot$Data_Year, na.rm = TRUE),
      max(fleet_composition_plot$Data_Year, na.rm = TRUE),
      by = 2
    )
  )+
  labs(
    x = "Year",
    y = "Number of Vehicles in Fleet"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.text = element_text(size = 10)
  ) +
  guides(fill = guide_legend(ncol = 1))

print(fleet_composition_plot_visual)


#=============================================================
# Create new vehicle classification that groups all fuel types 
#=============================================================
# Create the DATA with a different name
fleet_composition_data <- join_final_complete_clean %>%
  filter(
    !is.na(vehicle_class),
    !is.na(Fleet_Count),
    Fleet_Count > 0,
    Fuel_Group %in% c("HEV", "PHEV", "Diesel", "Petrol", "BEV")
  ) %>%
  # Create new classification: keep Diesel/Petrol subclasses, group others
  mutate(
    new_vehicle_class = case_when(
      str_detect(vehicle_class, "Petrol") ~ "Petrol",  # Keep all Petrol subclasses
      str_detect(vehicle_class, "Diesel") ~ "Diesel",  # Keep all Diesel subclasses
      str_detect(vehicle_class, "PHEV") ~ "PHEV",      # Group all PHEV
      str_detect(vehicle_class, "HEV") ~ "HEV",        # Group all HEV
      str_detect(vehicle_class, "BEV") ~ "BEV",        # Group all BEV
      TRUE ~ vehicle_class
    )
  ) %>%
  group_by(Data_Year, new_vehicle_class) %>%
  summarise(total_fleet = sum(Fleet_Count, na.rm = TRUE), .groups = 'drop')

# Create new color palette for the simplified classification
new_vehicle_class_colors <- c(
  # Petrol and Diesel - single colors
  "Petrol" = petrol_colors[3],  # Changed from " Petrol" to "Petrol"
  "Diesel" = diesel_colors[3],  # Changed from " Diesel" to "Diesel"
  
  # Other fuel types (single colors)
  "HEV"  = "#8FB573",    # Medium olive green from HEV palette
  "PHEV" = "#B5738F",    # Medium dusty red from PHEV palette
  "BEV"  = "#FFB366"     # Medium golden yellow from BEV palette
)

# Define the order for legend (SIMPLIFIED - remove size categories)
new_vehicle_class_levels <- c("Petrol", "Diesel", "HEV", "PHEV", "BEV")  # Simplified

# Create the PLOT with a different name
fleet_composition_plot <- fleet_composition_data %>%
  mutate(new_vehicle_class = factor(new_vehicle_class, levels = new_vehicle_class_levels)) %>%
  ggplot(aes(x = Data_Year, y = total_fleet, fill = new_vehicle_class)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(
    values = new_vehicle_class_colors,
    name = "Fuel Type",  # Changed from "Vehicle Class" to "Fuel Type"
    breaks = new_vehicle_class_levels
  ) +
  labs(
    x = "Year",
    y = "Number of Vehicles in Fleet"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    legend.spacing.y = unit(0.2, "cm")
  ) +
  guides(fill = guide_legend(
    ncol = 1, 
    keyheight = unit(0.3, "cm"),
    byrow = TRUE
  ))

print(fleet_composition_plot)

#-------------------------------------
# 3. AVERAGE VEHICLE AGE TREND BY FUEL TYPE
#-------------------------------------
avg_age_trend_plot <- join_final_complete %>%
  filter(!is.na(Vehicle_Age), !is.na(Fuel_Group), Fleet_Count > 0,Fuel_Group %in% c("HEV", "PHEV", "Diesel", "Petrol", "BEV")) %>%
  mutate(
    new_vehicle_class = case_when(
      str_detect(vehicle_class, "Petrol") ~ "Petrol",         # Group all Petrol 
      str_detect(vehicle_class, "Diesel") ~ "Diesel",         # Group all Diesel 
      str_detect(vehicle_class, "PHEV") ~ "PHEV",           # Group all PHEV
      str_detect(vehicle_class, "HEV") ~ "HEV",             # Group all HEV
      str_detect(vehicle_class, "BEV") ~ "BEV",             # Group all BEV
      TRUE ~ vehicle_class
    )
  ) %>%
  group_by(Data_Year, new_vehicle_class) %>%
  summarise(
    avg_age = weighted.mean(Vehicle_Age, Fleet_Count, na.rm = TRUE),
    total_vehicles = sum(Fleet_Count, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = Data_Year, y = avg_age, fill = new_vehicle_class)) +
  geom_col(position = "stack", width = 0.7, alpha = 0.9) +
  scale_fill_brewer(palette = "Set1", name = "Fuel Type") +
  scale_x_continuous(breaks = seq(min(join_final_complete$Data_Year, na.rm = TRUE), 
                                  max(join_final_complete$Data_Year, na.rm = TRUE), 2)) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Average Vehicle Age Composition by Fuel Type (2014-2024)",
    subtitle = "Stacked average age showing contribution of each fuel type",
    x = "Year",
    y = "Average Vehicle Age (Years)",
    caption = "Source: UK Vehicle Licensing Data | Note: Stacked ages represent composition, not cumulative total"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(color = "gray50", size = 9),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  guides(fill = guide_legend(
    ncol = 1,
    title.position = "top",
    keywidth = unit(1.2, "cm"),
    keyheight = unit(0.8, "cm"),
    override.aes = list(alpha = 1)
  )) +
  facet_wrap(~ new_vehicle_class, ncol = 3)  # Add this line for subfigures

print(avg_age_trend_plot)




#=================================================
# Calculate average age for all car types combined
#=================================================
total_avg_age <- join_final_complete %>%
  filter(!is.na(Vehicle_Age), !is.na(Fleet_Count), Fleet_Count > 0,Fuel_Group !="other") %>%
  group_by(Data_Year) %>%
  summarise(
    avg_age = weighted.mean(Vehicle_Age,Fleet_Count, na.rm = TRUE),
    total_vehicles = sum(Fleet_Count, na.rm = TRUE),
    .groups = 'drop'
  ) 
print(total_avg_age)

total_avg_age_plot <- total_avg_age %>%
  ggplot(aes(x = Data_Year, y = avg_age)) +
  geom_line(color = "#2E86AB", size = 1.2) +
  geom_point(color = "#2E86AB", size = 3) +
  geom_area(fill = "#2E86AB", alpha = 0.2) +
  scale_x_continuous(breaks = seq(min(join_final_complete$Data_Year, na.rm = TRUE), 
                                  max(join_final_complete$Data_Year, na.rm = TRUE), 1)) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    # title = "Average Vehicle Age of Total Car Fleet (2014-2024)",
    #subtitle = "Weighted average age across all fuel types and vehicle classes",
    x = "Year",
    y = "Average Vehicle Age (Years)",
    # caption = "Source: UK Vehicle Licensing Data"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(color = "gray50", size = 9),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

print(total_avg_age_plot)

#----------------------------------------
# Average age plot for EACH fuel type
#---------------------------------------
avg_age_by_fuel_plot <- join_final_complete %>%
  filter(!is.na(Fleet_Count), Fleet_Count > 0) %>%
  # First, group by both Data_Year and Fuel_Group
  group_by(Data_Year, Fuel_Group) %>%
  summarise(
    avg_age = weighted.mean(Vehicle_Age,Fleet_Count, na.rm = TRUE),
    total_vehicles = sum(Fleet_Count, na.rm = TRUE),
    .groups = 'drop'
  )%>% filter(Fuel_Group !="Other")%>%
  pivot_wider(
    id_cols = Data_Year,
    names_from = Fuel_Group,
    values_from = avg_age,
    values_fill = NA  # Fill missing combinations with NA
  ) %>%
  # Arrange by year
  arrange(Data_Year)

print(avg_age_by_fuel_plot)


avg_age_2024 <- avg_age_by_fuel_plot %>%
  filter(Data_Year == 2024)

print(avg_age_2024)


#-------------------------------------------
# 4. VEHICLE CLASS DISTRIBUTION BY AGE GROUP
#-------------------------------------------
age_group_plot <- join_final_complete %>%
  filter(!is.na(Vehicle_Age), Fleet_Count > 0, Fuel_Group %in% c("HEV", "PHEV", "Diesel", "Petrol", "BEV")) %>%
  mutate(
    # First create new_vehicle_class
    new_vehicle_class = case_when(
      str_detect(vehicle_class, "Petrol") ~ "Petrol",         # Group all Petrol 
      str_detect(vehicle_class, "Diesel") ~ "Diesel",         # Group all Diesel 
      str_detect(vehicle_class, "PHEV") ~ "PHEV",           # Group all PHEV
      str_detect(vehicle_class, "HEV") ~ "HEV",             # Group all HEV
      str_detect(vehicle_class, "BEV") ~ "BEV",             # Group all BEV
      TRUE ~ vehicle_class
    ),
    # Then create age_group
    age_group = cut(Vehicle_Age, 
                    breaks = c(0, 2, 5, 10, 15, 20, 25, 30),
                    labels = c("0-2", "3-5", "6-10", "11-15", "16-20", "21-25", "26+"),
                    include.lowest = TRUE)
  ) %>%
  # Now filter by new_vehicle_class after creating it
  filter(!is.na(new_vehicle_class)) %>%
  group_by(age_group, new_vehicle_class) %>%
  summarise(total_vehicles = sum(Fleet_Count, na.rm = TRUE), .groups = 'drop') %>%
  group_by(age_group) %>%
  mutate(percentage = total_vehicles / sum(total_vehicles) * 100) %>%
  ggplot(aes(x = age_group, y = percentage, fill = new_vehicle_class)) +
  geom_col(position = "stack", width = 0.5, alpha = 0.9) +
  scale_fill_brewer(palette = "Set1", name = "Fuel Type") +
  facet_wrap(~ new_vehicle_class, ncol = 3, scales = "free_y") +  # ADDED + HERE
  labs(
    #title = "Vehicle Fuel Type Distribution by Age Group",
    # subtitle = "Percentage composition within each age group",
    x = "Vehicle Age Group (Years)",
    y = "Percentage of Fleet"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",  # Remove legend since faceting shows labels
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  guides(fill = guide_legend(ncol = 1, keyheight = unit(0.3, "cm")))

print(age_group_plot)


###########################################################################################################################


#=============================
# integrate Vehicle Mileage
#=============================
# Read the files
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)
library(ggridges)

# Load data
DVLA_1 <- read.csv("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\R-programming-Infuze\\R-programming_Updated analysis\\New-updated_data\\Excell_files\\Mileage_CO2_data.csv")

#-----------------------------------
# update Fuel in all Files (Unified)
#------------------------------------


# First: Create the harmonized data
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

# Check the table results separately
fuel_table <- table(DVLA$fuel, DVLA$fuel_harmonised, useNA = "always")
print(fuel_table)

# Now filter your DATA (not the table)
DVLA<- DVLA %>%
  filter(Age >= 3 & Age <= 23) %>%
  arrange(EndDate, fuel_harmonised, Age) %>%
  select(EndDate, size, Age, fuel_harmonised, Total_Vehicles_stats, Avg_CO2_g_km, Avg_Mileage) %>%
  rename(
    Avg_CO2 =Avg_CO2_g_km,
    engine_size = size,
    Fuel_Group = fuel_harmonised,
    Data_Year = EndDate,
    `Total_Vehicles(mileage_data)` = Total_Vehicles_stats
  ) %>%
  mutate(
    Data_Year = as.numeric(str_sub(Data_Year, 1, 4)),
    Vehicle_Age = Age # this was age+1 i removed it as it can be wrong
  ) 

# Check data
cat("=== DATA CHECK ===\n")
cat("Years in data:", paste(sort(unique(DVLA$Data_Year)), collapse = ", "), "\n")
cat("Age range:", min(DVLA$Age), "-", max(DVLA$Age), "\n")
cat("Fuel groups:", paste(unique(DVLA$Fuel_Group), collapse = ", "), "\n")



# ==============================================
# MILEAGE ANALYSIS PIPELINE - CORRECTED
# ==============================================

# Check if required dataset exists
if(!exists("join_final_complete_classified")) {
  cat("\nERROR: 'join_final_complete_classified' dataset not found!\n")
  cat("Please load this dataset before running this code.\n")
  stop("Missing required dataset: join_final_complete_classified")
}

# Check column names
cat("\nAvailable columns in join_final_complete_classified:\n")
print(names(join_final_complete_classified))

# 1. Define engine size bands
engine_breaks <- c(0, 1000, 1400, 2000, 3000, Inf)

# 2. Prepare DVLA / MOT mileage data
DVLA_prepared <- DVLA %>%
  mutate(
    engine_size = as.numeric(engine_size),
    engine_band = cut(engine_size,
                      breaks = engine_breaks,
                      right = FALSE)
  )


# 3. Collapse MOT data with cleaning - CORRECTED
DVLA_collapsed <- DVLA_prepared %>%
  # DATA CLEANING: Remove impossible ANNUAL values
  # Based on your analysis, Avg_Mileage appears to be ANNUAL
  # UK typical range: 1,000-30,000 miles/YEAR
  filter(Avg_Mileage >= 1000, Avg_Mileage <= 30000) %>%
  
  # NO UNIT CONVERSION NEEDED - Data is already annual
  group_by(Fuel_Group, Data_Year, Vehicle_Age) %>%
  summarise(
    Avg_Mileage = weighted.mean(Avg_Mileage, `Total_Vehicles(mileage_data)`, na.rm = TRUE),
    Avg_CO2 = weighted.mean(Avg_CO2, `Total_Vehicles(mileage_data)`, na.rm = TRUE),
    MOT_Sample_Size = sum(`Total_Vehicles(mileage_data)`, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Prepare GB stock data
# Check for correct column name
vehicle_count_col <- ifelse("Fleet_Count" %in% names(join_final_complete_classified), 
                            "Fleet_Count", 
                            names(join_final_complete_classified)[grepl("count|vehicle|stock|fleet", 
                                                                        names(join_final_complete_classified), 
                                                                        ignore.case = TRUE)][1])





# the trend-extrapolation method to estimate mileage for vehicle less than 3 years 
young_vehicle_mileage <- DVLA_collapsed %>%
  filter(Vehicle_Age == 3) %>%
  select(Fuel_Group, Data_Year, Avg_Mileage, Avg_CO2, MOT_Sample_Size) %>%
  tidyr::crossing(Vehicle_Age = c(1, 2)) %>%
  select(Fuel_Group, Data_Year, Vehicle_Age, Avg_Mileage, Avg_CO2, MOT_Sample_Size)

DVLA_collapsed_v2 <- bind_rows(DVLA_collapsed, young_vehicle_mileage) %>%
  arrange(Fuel_Group, Data_Year, Vehicle_Age)

# --- Method 3: trend extrapolation, fit ages 3-8, project to 1-2 ---
extrapolate_young_ages <- function(df, max_fit_age = 8) {
  df %>%
    filter(Vehicle_Age >= 3, Vehicle_Age <= max_fit_age) %>%
    group_by(Fuel_Group, Data_Year) %>%
    group_modify(~ {
      if (nrow(.x) < 3) return(tibble())
      fit <- lm(Avg_Mileage ~ Vehicle_Age, data = .x)
      tibble(
        Vehicle_Age = c(1, 2),
        Avg_Mileage = pmax(predict(fit, newdata = tibble(Vehicle_Age = c(1, 2))), 0),
        Avg_CO2     = mean(.x$Avg_CO2, na.rm = TRUE)
      )
    }) %>%
    ungroup()
}



cat("\nUsing column '", vehicle_count_col, "' for vehicle counts\n", sep="")
library(data.table)

# Convert to data.table to make it faster 
setDT(join_final_complete_classified)
setDT(dimension_lookup)

# 1. Prep Stock_prepared FIRST, so we know which keys we actually need
Stock_prepared <- copy(join_final_complete_classified)
Stock_prepared[, engine_size := as.numeric(engine_size)]
Stock_prepared[, engine_band := cut(engine_size, breaks = engine_breaks, right = FALSE)]
setnames(Stock_prepared, vehicle_count_col, "Vehicle_Stock_GB")
Stock_prepared[, kerb_weight_kg := as.numeric(kerb_weight_kg)]

# 2. Restrict dimension_lookup to only the keys present in Stock_prepared
#    BEFORE collapsing — this can cut the group-by workload dramatically
needed_keys <- unique(Stock_prepared[, .(Make, Model, GenModel, Fuel_Group)])

dimension_lookup_small <- dimension_lookup[
  needed_keys, on = .(Make, Model, GenModel, Fuel_Group), nomatch = NULL
]

# 3. Collapse with data.table syntax (fast: sum/mean are GForce-optimized;
#    median isn't, but the table is now much smaller so it's fine)
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

# 4. Fast keyed join — data.table binary-search merge instead of dplyr hash join
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
stopifnot(nrow(Stock_prepared) == n_before)   # catches any fan-out immediately

cat("\nDimensions added to Stock_prepared:\n")
dimension_cols <- c("rdw_length_mm", "rdw_width_mm", "rdw_height_mm",
                    "rdw_kerb_weight_kg", "rdw_wheelbase_mm")
existing_dims <- dimension_cols[dimension_cols %in% names(Stock_prepared)]
cat(paste("  -", existing_dims, collapse = "\n"))


vehicel_numer <- Stock_prepared%>%
  group_by(Data_Year)%>%
  summarise(fleet = sum(Vehicle_Stock_GB, na.rm = TRUE), .groups = "drop")%>%
  print


#------------------------------------------------------------------------------------------

fleet_complete <- Stock_prepared %>%
  filter(Vehicle_Age >= 3 & Vehicle_Age <= 23)

# 5. Join mileage with GB stock
# Join mileage with GB stock
Stock_model <- fleet_complete %>%
  left_join(DVLA_collapsed,
            by = c("Fuel_Group", "Data_Year", "Vehicle_Age"))

# --- IMPUTE MISSING VALUES BEFORE ANY SUMMARY ---
Stock_model <- Stock_model %>%
  group_by(Fuel_Group) %>%
  mutate(
    Avg_Mileage = ifelse(
      is.na(Avg_Mileage),
      weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
      Avg_Mileage
    ),
    Avg_CO2 = ifelse(
      is.na(Avg_CO2),
      weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
      Avg_CO2
    )
  ) %>%
  ungroup()
print(Stock_model)
# --------------------------------------------------


# here how to use the right method to estimate the mileage for vehicle less than 3 years. 
# --- Method 2: flat fill, Age 1-2 = Age 3 rate ---


young_vehicle_mileage_extrap <- extrapolate_young_ages(DVLA_collapsed)

DVLA_collapsed_v3 <- bind_rows(DVLA_collapsed, young_vehicle_mileage_extrap) %>%
  arrange(Fuel_Group, Data_Year, Vehicle_Age)

cat("DVLA_collapsed_v2:", nrow(DVLA_collapsed_v2), "rows\n")
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
#-------------------------------------------------
# mileage validation 
#--------------------------------------------------

# Create the DfT estimates dataframe https://roadtraffic.dft.gov.uk/
dft_estimates <- data.frame(
  Data_Year = c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022,2023, 2024),
  DfT_Estimate = c(250.3, 254.8, 257.9, 259.1, 262.9, 197.3, 221.4, 244,251.3, 256.1)
)


# Optional: Create a summary table for comparison
comparison_table <- national_trend_no_age_uncalibrated %>%
  select(Data_Year, GB_Total_Mileage) %>%
  mutate(
    my_Data_Billions = GB_Total_Mileage / 1e9,
    DfT_Estimate_Billions = dft_estimates$DfT_Estimate[match(Data_Year, dft_estimates$Data_Year)],
    Difference = my_Data_Billions - DfT_Estimate_Billions,
    Percent_Difference = round(Difference / DfT_Estimate_Billions * 100, 1)
  ) %>%
  filter(!is.na(DfT_Estimate_Billions))%>% select(-GB_Total_Mileage)

print("=== COMPARISON TABLE (Billions) ===")
print(comparison_table)

#===========================================================================
# SIMPLIFIED CALIBRATION - 50/50 SPLIT
#===========================================================================

# STEP 1: Get your current unfiltered trend (from your decomposition)
current_trend <- data.frame(
  Data_Year = 2015:2024,
  Your_Mileage = comparison_table$my_Data_Billions,  # Fixed typo
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

# View decomposition results
print("=== DECOMPOSITION RESULTS ===")
decomposition %>% select(Data_Year, Car_Count_Ratio, Mileage_Rate_Ratio)

# STEP 2: Calculate calibration factors from decomposition
# From your output: Car_Count_Ratio = Mileage_Rate_Ratio = 0.901 for 2024
cal_factors <- decomposition %>%
  group_by(Data_Year) %>%
  mutate(
    cal_factor = 1 / Car_Count_Ratio  # Calculate factor for each year
  ) %>%
  ungroup() %>%
  select(Data_Year, cal_factor)

print(cal_factors)

# STEP 3: Apply year-specific calibration
calibrated_trend <- current_trend %>%
  left_join(cal_factors, by = "Data_Year") %>%
  mutate(
    # Apply year-specific factor
    Calibrated_Mileage = Your_Mileage * cal_factor,
    Error_Before = round((Your_Mileage - DfT_Target) / DfT_Target * 100, 1),
    Error_After = round((Calibrated_Mileage - DfT_Target) / DfT_Target * 100, 1),
    Improvement = abs(Error_Before) - abs(Error_After)
  )

# STEP 4: View results
print(calibrated_trend %>% 
        select(Data_Year, Your_Mileage, Calibrated_Mileage, DfT_Target, 
               cal_factor, Error_Before, Error_After, Improvement))

# STEP 5: Summary statistics
summary_stats <- calibrated_trend %>%
  filter(Data_Year != 2020) %>%  # Exclude pandemic year
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






# =========================================================
# Rebuild compare_three_methods() and its inputs, then run
# the final gap comparison across all three fill methods
# =========================================================
stopifnot(
  "DVLA_collapsed does not exist" = exists("DVLA_collapsed"),
  "Stock_prepared does not exist" = exists("Stock_prepared"),
  "dft_estimates does not exist" = exists("dft_estimates")
)


# --- The comparison function itself ---
compare_three_methods <- function(dvla_variant, label) {
  Stock_prepared %>%
    left_join(dvla_variant, by = c("Fuel_Group", "Data_Year", "Vehicle_Age")) %>%
    group_by(Fuel_Group) %>%
    mutate(Avg_Mileage = ifelse(is.na(Avg_Mileage),
                                weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
                                Avg_Mileage)) %>%
    ungroup() %>%
    group_by(Data_Year) %>%
    summarise(GB_Total_Mileage = sum(Avg_Mileage * Vehicle_Stock_GB, na.rm = TRUE), .groups = "drop") %>%
    mutate(Method = label)
}

# --- Run all three, on the NOW-FIXED Stock_prepared ---
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




#===========================================================================
# OR - Even SIMPLER: Single factor calibration
#===========================================================================

# Create national trend WITHOUT age filter and without remove any vehicel 

national_trend_no_age <- Stock_prepared %>%
  # Join DVLA/mileage data
  left_join(DVLA_collapsed_v3, by = c("Fuel_Group", "Data_Year", "Vehicle_Age")) %>%
  filter(Data_Year >= 2015) %>%  # Add this line to exclude 2014
  # Impute missing mileage
  group_by(Fuel_Group) %>%
  mutate(
    Avg_Mileage = ifelse(
      is.na(Avg_Mileage),
      weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
      Avg_Mileage
    )
  ) %>%
  ungroup() %>%
  
  # Apply calibration factors
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
  
  # Calculate totals for each year - USING CALIBRATED VALUES
  group_by(Data_Year) %>%
  summarise(
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Vehicles_with_Data = sum(Vehicle_Stock_GB[!is.na(Avg_Mileage_Calibrated)], na.rm = TRUE),
    
    # ✅ Use calibrated mileage!
    GB_Total_Mileage = sum(Total_Mileage_GB_Calibrated, na.rm = TRUE),
    
    # Adjust for missing data
    Adjusted_Mileage = GB_Total_Mileage * (Total_Vehicles / Vehicles_with_Data),
    
    # Average mileage per vehicle (calibrated)
    Avg_Mileage_Per_Vehicle = Adjusted_Mileage / Total_Vehicles,
    
    .groups = "drop"
  )

print(national_trend_no_age)




# Optional: Create a summary table for comparison
comparison_table_cal <- national_trend_no_age %>%
  select(Data_Year, GB_Total_Mileage) %>%
  mutate(
    my_Data_Billions = GB_Total_Mileage / 1e9,
    DfT_Estimate_Billions = dft_estimates$DfT_Estimate[match(Data_Year, dft_estimates$Data_Year)],
    Difference = my_Data_Billions - DfT_Estimate_Billions,
    Percent_Difference = round(Difference / DfT_Estimate_Billions * 100, 1)
  ) %>%
  filter(!is.na(DfT_Estimate_Billions))%>% select(-GB_Total_Mileage)

print("=== COMPARISON TABLE (Billions) ===")
print(comparison_table_cal)

#CALIBRATION FRAMEWORK


# Decompose the difference
decomposition_cal <- national_trend_no_age %>%
  filter(Data_Year >= 2015) %>%
  left_join(dft_estimates, by = "Data_Year") %>%
  mutate(
    # Implied DfT total cars assuming their mileage estimate and your fleet size
    Implied_DfT_Cars = (DfT_Estimate * 1e9) / Avg_Mileage_Per_Vehicle,
    
    # Implied DfT average mileage assuming DfT estimate with your fleet size
    Implied_DfT_AvgMileage = (DfT_Estimate * 1e9) / Total_Vehicles,
    
    Car_Count_Ratio = Total_Vehicles / Implied_DfT_Cars, #Car_Count_Ratio (<1 means your fleet is smaller than the DfT-implied fleet, >1 means larger)
    Mileage_Rate_Ratio = Avg_Mileage_Per_Vehicle / Implied_DfT_AvgMileage #Mileage_Rate_Ratio (<1 means your average mileage is below DfT-implied mileage, >1 means above)
  )

decomposition_cal %>% select(Data_Year, Car_Count_Ratio, Mileage_Rate_Ratio)




# ============================================
# FULL STOCK MODEL - WITHOUT SUMMARIZING BY YEAR
# ============================================


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

# 1. Primary lookup NOW includes Fuel_Group
origin_lookup <- veh_origin %>%
  distinct(Make, GenModel, Fuel_Group, Build_Year, Origin, Region)

# 2. Confirm the key is now unique — if this isn't 0, there's a deeper
#    inconsistency in veh_origin itself that needs resolving before joining
dup_check <- origin_lookup %>%
  count(Make, GenModel, Fuel_Group, Build_Year) %>%
  filter(n > 1)
cat("Remaining ambiguous keys:", nrow(dup_check), "\n")
if (nrow(dup_check) > 0) print(head(dup_check, 20))

# 3. Make-level fallback (unchanged)
origin_make_fallback <- veh_origin %>%
  count(Make, Origin, Region) %>%
  group_by(Make) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Make, Origin_mk = Origin, Region_mk = Region)

# 4. Join — now matches 1:1 (or safely falls back)
full_stock_model <- full_stock_model %>%
  left_join(origin_lookup, by = c("Make", "GenModel", "Fuel_Group", "Build_Year")) %>%
  left_join(origin_make_fallback, by = "Make") %>%
  mutate(
    Origin = coalesce(Origin, Origin_mk, "Other (unmapped)"),
    Region = coalesce(Region, Region_mk, "Other")
  ) %>%
  select(-Origin_mk, -Region_mk)

# 5. HARD check — this must hold, or you still have duplication
stopifnot(nrow(full_stock_model) == n_before)
cat("Row count preserved:", nrow(full_stock_model) == n_before, "\n")


# 5. Check coverage
cat("\n=== FINAL ORIGIN COVERAGE ===\n")
cat("Total rows:", nrow(full_stock_model), "\n")
cat("NA Origin count:", sum(is.na(full_stock_model$Origin)), "\n")
cat("NA Origin percentage:", 
    round(100 * sum(is.na(full_stock_model$Origin)) / nrow(full_stock_model), 2), "%\n")

# 6. Verify ABARTH and AIXAM now have origins
cat("\n=== CHECK ABARTH AND AIXAM ===\n")
full_stock_model %>%
  filter(Make %in% c("ABARTH", "AIXAM")) %>%
  select(Make, Origin, Region) %>%
  distinct() %>%
  print()
cat("Stock_prepared total vehicles:", sum(Stock_prepared$Vehicle_Stock_GB, na.rm = TRUE), "\n")
cat("full_stock_model total vehicles:", sum(full_stock_model$Vehicle_Stock_GB, na.rm = TRUE), "\n")

# 7. Check the origin distribution (should show China 27.6%, Germany 27.8%, etc.)
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

print(bev_origin_2024, n=Inf)

# Check coverage
cat("Origin coverage:", 
    round(100 * sum(!is.na(full_stock_model$Origin)) / nrow(full_stock_model), 1), "%\n")

full_stock_model_export <- full_stock_model %>%
  mutate(
    Avg_Mileage = round(Avg_Mileage, 1),
    Avg_Mileage_Calibrated = round(Avg_Mileage_Calibrated, 1),
    Total_Mileage_GB_Calibrated = round(Total_Mileage_GB_Calibrated, 0),
    Avg_CO2 = round(Avg_CO2, 2),
    across(starts_with("rdw_"), ~ round(.x, 1))
    # Vehicle_Stock_GB intentionally left at full precision
  )

cat("Unrounded total, 2019:",
    full_stock_model %>% filter(Data_Year == 2019) %>% summarise(x = sum(Vehicle_Stock_GB, na.rm = TRUE)) %>% pull(x), "\n")
cat("Rounded total, 2019:",
    full_stock_model_export %>% filter(Data_Year == 2019) %>% summarise(x = sum(Vehicle_Stock_GB, na.rm = TRUE)) %>% pull(x), "\n")

vehicel_numer <- full_stock_model_export%>%
  group_by(Data_Year)%>%
  summarise(fleet = sum(Vehicle_Stock_GB, na.rm = TRUE), .groups = "drop")%>%
  print

# # Convert to data.table if not already
# #setDT(full_stock_model)
# library(arrow)
# write_parquet(full_stock_model_export, "join_final_complete.parquet")
# 
# # fwrite(full_stock_model, 
# #        "join_final_complete.csv.gz",   # .gz extension triggers streaming compression
# #        row.names = FALSE,
# #        sep = ",", 
# #        dec = ".",
# #        nThread = 4,
# #        verbose = FALSE,
# #        showProgress = FALSE,
# #        scipen = 999,
# #        compress = "gzip")              # was "auto" — "auto" only compresses if extension is already .gz

#--------------------------------------------------------
# After Stock_model is created and before any summaries
#--------------------------------------------------------
Stock_model_calibrated <- Stock_model %>%
  # Join calibration factors
  left_join(calibration_factors, by = "Data_Year") %>%
  mutate(
    # Apply calibration to mileage
    Avg_Mileage_Calibrated = ifelse(
      !is.na(Calibration_Multiplier),
      Avg_Mileage * Calibration_Multiplier,
      Avg_Mileage  # Keep original if no calibration factor
    ),
    
    # Recalculate totals with calibrated mileage
    Total_Mileage_GB_Calibrated = Avg_Mileage_Calibrated * Vehicle_Stock_GB,
    Total_CO2_GB_Calibrated = ifelse(
      !is.na(Avg_CO2),
      Avg_CO2 * Vehicle_Stock_GB,
      NA
    ),
    
    # Flag for calibrated data availability
    Has_Calibrated_Mileage = !is.na(Avg_Mileage_Calibrated)
  ) %>%
  # Optional: Drop the temporary calibration factor column if you want
  select(-Calibration_Multiplier)

Stock_model <- Stock_model_calibrated %>%
  mutate(
    # Replace original with calibrated
    Avg_Mileage = Avg_Mileage_Calibrated,
    
    # Calculate totals using the calibrated Avg_Mileage
    Total_Mileage_GB = ifelse(is.na(Avg_Mileage), NA, Avg_Mileage * Vehicle_Stock_GB),
    Total_CO2_GB = ifelse(is.na(Avg_CO2), NA, Avg_CO2 * Vehicle_Stock_GB),
    
    # Add flag for data availability
    Has_Mileage_Data = !is.na(Avg_Mileage)
  )

print(Stock_model$Has_Mileage_Data)


# 7. Validation checks
cat("\n=== VALIDATION CHECKS ===\n")

# Check if results are realistic
cat("\nChecking if mileage values are realistic (should be ~7,000 miles/year average):\n")
summary_stats <- Stock_model %>%
  filter(Has_Mileage_Data==TRUE) %>%
  summarise(
    min_mileage = min(Avg_Mileage, na.rm = TRUE),
    max_mileage = max(Avg_Mileage, na.rm = TRUE),
    mean_mileage = mean(Avg_Mileage, na.rm = TRUE),
    median_mileage = median(Avg_Mileage, na.rm = TRUE)
  )
print(summary_stats)


# 7. Validation checks

# 7.1 Average ANNUAL mileage per vehicle for 2024
mileage_2024 <- Stock_model %>%
  filter(Data_Year == 2024) %>%
  summarise(
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Vehicles_with_Data = sum(Vehicle_Stock_GB[!is.na(Avg_Mileage)], na.rm = TRUE),
    Total_Mileage = sum(Total_Mileage_GB, na.rm = TRUE),
    
    Adjusted_Total_Mileage = Total_Mileage * (Total_Vehicles / Vehicles_with_Data),#This corrects for missing mileage within your dataset
    
    # ✅ ADD THIS LINE
    avg_mileage_per_vehicle = Adjusted_Total_Mileage / Total_Vehicles
  )

cat("\n=== 2024 ANNUAL MILEAGE SUMMARY ===\n")
print(mileage_2024)


# 7.2 National total trend
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

print(national_trend,width=Inf)



# Just multiply everything by the year-specific calibration factors


simple_calibration <- current_trend %>%
  left_join(cal_factors, by = "Data_Year") %>%
  mutate(
    Calibrated_Mileage = Your_Mileage * cal_factor,
    Error_Before = round((Your_Mileage - DfT_Target) / DfT_Target * 100, 1),
    Error_After = round((Calibrated_Mileage - DfT_Target) / DfT_Target * 100, 1)
  )

print(simple_calibration %>% 
        select(Data_Year, Your_Mileage, Calibrated_Mileage, DfT_Target, Error_Before, Error_After))


#plot them with calibrated Mileage
p3 <- ggplot() +
  #actual data
  geom_line(data = simple_calibration, 
            aes(x = Data_Year, y = Your_Mileage, color = "This Study"), 
            size = 1.2) +
  geom_point(data = simple_calibration, 
             aes(x = Data_Year, y = Your_Mileage , color = "This Study"), 
             size = 3) +
  # calibrated
  geom_line(data = simple_calibration, 
            aes(x = Data_Year, y = Calibrated_Mileage , color = "Calibrated"), 
            size = 1.2) +
  geom_point(data = simple_calibration, 
             aes(x = Data_Year, y = Calibrated_Mileage , color = "Calibrated"), 
             size = 3) +
  # DfT estimates
  geom_line(data = dft_estimates, 
            aes(x = Data_Year, y = DfT_Estimate, color = "DfT Estimates"), 
            size = 1.2, linetype = "dashed") +
  geom_point(data = dft_estimates, 
             aes(x = Data_Year, y = DfT_Estimate, color = "DfT Estimates"), 
             size = 3, shape = 17) +
  labs(
    #title = "GB Total Vehicle Mileage: This Study vs DfT Estimates",
    x = "Year",
    y = "Mileage (Billions)",
    color = "Source"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c(
    "Calibrated" = "blue",
    "DfT Estimates" = "red",
    "This Study" = "pink"   # instead of pink
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





#----------------------------------
#visualize vehicle _mileage
#-------------------------------------

DVLA_calibrated <- DVLA %>%
  left_join(calibration_factors, by = "Data_Year") %>%
  mutate(
    # Apply calibration to mileage
    Avg_Mileage_Calibrated = ifelse(
      !is.na(Calibration_Multiplier),
      Avg_Mileage * Calibration_Multiplier,
      Avg_Mileage
    ),
    # Keep original for comparison if needed
    Avg_Mileage_Original = Avg_Mileage
  ) %>%
  select(-Calibration_Multiplier)

dff <- DVLA_calibrated %>%
  filter(Fuel_Group != "Other") %>%
  group_by(Fuel_Group, engine_size, Age, Data_Year) %>%
  summarise(
    # Use the calibrated mileage
    Avg_Mileage = weighted.mean(Avg_Mileage_Calibrated, `Total_Vehicles(mileage_data)`, na.rm = TRUE),
    Avg_CO2 = weighted.mean(Avg_CO2, `Total_Vehicles(mileage_data)`, na.rm = TRUE),
    Total_Vehicles = sum(`Total_Vehicles(mileage_data)`, na.rm = TRUE),
    .groups = "drop"
  )


# Check what we have
cat("\nSummary of dff:\n")
cat("Years:", paste(sort(unique(dff$Data_Year)), collapse = ", "), "\n")
cat("Ages:", paste(sort(unique(dff$Age)), collapse = ", "), "\n")
cat("Fuel groups:", paste(unique(dff$Fuel_Group), collapse = ", "), "\n")
cat("Engine size range:", min(dff$engine_size, na.rm = TRUE), "-", max(dff$engine_size, na.rm = TRUE), "\n")
cat("Total vehicles in dff:", format(sum(dff$Total_Vehicles, na.rm = TRUE), big.mark = ","), "\n")

# Now filter and plot
filtered_df <- dff%>%
  filter(engine_size > 500 & engine_size < 5000,
         Age <= 8,
         Data_Year == 2024,
         Avg_Mileage >= 1000 & Avg_Mileage <= 20000,
         Total_Vehicles >= 0)  # Added minimum vehicle count

# Create size categories for better visualization
filtered_df <- filtered_df%>%
  mutate(
    size_category = cut(engine_size,
                        breaks = c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000),
                        labels = c("500-1000", "1001-1500", "1501-2000", "2001-2500",
                                   "2501-3000", "3001-3500", "3501-4000", "4001-4500", "4501-5000"))
  ) %>%
  group_by(size_category, Age, Fuel_Group,Data_Year,engine_size) %>%
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

# View the simple table
print(simple_table,width = Inf)

# Enhanced table with additional metrics
enhanced_table <- simple_table %>%
  mutate(
    # Calculate mileage differences between fuel types
    Diesel_vs_Petrol = Avg_Mileage_Diesel - Avg_Mileage_Petrol,
    BEV_vs_Petrol = Avg_Mileage_BEV - Avg_Mileage_Petrol,
    HEV_vs_Petrol = Avg_Mileage_HEV - Avg_Mileage_Petrol,
    
    # Calculate percentage differences
    Diesel_pct_higher = round((Avg_Mileage_Diesel / Avg_Mileage_Petrol - 1) * 100, 1),
    BEV_pct_higher = round((Avg_Mileage_BEV / Avg_Mileage_Petrol - 1) * 100, 1),
    HEV_pct_higher = round((Avg_Mileage_HEV / Avg_Mileage_Petrol - 1) * 100, 1),
    
    # Calculate market share percentages
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

# View the enhanced table
print(enhanced_table, width = Inf)

# Create a summary table focusing on mileage trends
mileage_trends <- enhanced_table %>%
  select(Age, ends_with("_pct_higher")) %>%
  rename_with(~str_remove(., "_pct_higher"), ends_with("_pct_higher"))

print(mileage_trends, width = Inf)

# Create a market share table
market_share <- enhanced_table %>%
  select(Age, ends_with("_share")) %>%
  rename_with(~str_remove(., "_share"), ends_with("_share"))

print(market_share, width = Inf)
#----------------------------------------------------
# 1. BAR PLOT - Exact same figure, updated variables
#---------------------------------------------------
df<-filtered_df
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





#----------------------------------------------------------
# 2. RIDGELINE PLOT - Exact same figure, updated variables
#----------------------------------------------------------

# Ridgeline plot (requires ggridges)
library(ggridges)

ggplot(df, 
       aes(x = Avg_Mileage, y = as.factor(Age), fill = Fuel_Group)) +
  geom_density_ridges(alpha = 0.7, scale = 0.9) +
  facet_wrap(~ Fuel_Group) +
  scale_x_continuous(labels = comma) +
  theme_classic() +
  labs(
    title = "Mileage Distribution by Age and Fuel Type",
    x = "Annual Mileage",
    y = "Vehicle Age",
    fill = "Fuel Type"
  )

#-------------------------------------------------------
# 3. VIOLIN PLOT - Exact same figure, updated variables
#------------------------------------------------------

# Instead of density ridges, use violin plots or boxplots with averages
ggplot(df, 
       aes(x = as.factor(Age), y = Avg_Mileage, fill = Fuel_Group)) +
  geom_violin() +
  facet_wrap(~ Fuel_Group) +
  scale_y_continuous(labels = comma) +
  theme_classic() +
  labs(
    title = "Average Mileage Distribution by Age and Fuel Type",
    x = "Vehicle Age",
    y = "Average Annual Mileage"
  )


#-----------------------
#correct the data
#---------------------

# Fix the syntax error first
cat("\n", strrep("=", 60), "\n", sep="")
cat("FINAL RECOMMENDED VALUES (PROPER Weighted Average)\n")
cat(strrep("=", 60), "\n")

# CRITICAL INSIGHT: You have Total_Vehicles column!
# The extreme values come from engine sizes with very few vehicles

# Check vehicle counts for extreme values
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

# SOLUTION: Use PROPER weighted average with Total_Vehicles
# This is the CORRECT way to calculate fleet averages
weighted_correct <- dff %>%
  filter(Data_Year == 2024, Age <= 8) %>%
  # Optional: Filter extreme values that distort averages
  # filter(Avg_Mileage >= 1000 & Avg_Mileage <= 20000) %>%
  group_by(Age, Fuel_Group) %>%
  summarise(
    total_miles = sum(Avg_Mileage * Total_Vehicles, na.rm = TRUE),
    total_vehicles = sum(Total_Vehicles, na.rm = TRUE),
    weighted_avg = total_miles / total_vehicles,
    .groups = 'drop'
  )

cat("\nPROPER weighted averages (using Total_Vehicles):\n")
print(weighted_correct,n=Inf)

# Compare weighted vs simple average
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
print(comparison_weighted)

# Show the impact of weighting
impact <- comparison_weighted %>%
  mutate(
    difference = weighted_avg - simple_avg,
    pct_difference = difference / simple_avg * 100
  )

cat("\nImpact of using weighted average:\n")
print(impact)

# Plot the CORRECT weighted averages
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
    #  title = " Average Annual Mileage by Age and Fuel Type (2024)",
    #subtitle = "Properly weighted by vehicle counts - accounts for fleet composition",
    y = "Average Annual Mileage (miles)",
    x = "Vehicle Age (years)",
    color = "Fuel Type"
    # caption = paste("Data based on", format(sum(weighted_correct$total_vehicles), big.mark=","), 
    #                "vehicles in 2024")
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray50")
  ) +
  # Add value labels
  geom_text(aes(label = paste0(round(weighted_avg/1000, 1), "k")),
            vjust = -0.8, size = 3, fontface = "bold", show.legend  =FALSE,check_overlap = TRUE)

# Additional analysis: Check why simple average gives wrong results
# Look at distribution of vehicle counts
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


# FINAL CLEAN SOLUTION: Weighted average with minimum vehicle count threshold
final_correct <- df %>%
  filter(Data_Year == 2024, Age <= 8) %>%
  # Exclude rows with very few vehicles (statistically unreliable)
  filter(Total_Vehicles >= 10) %>%
  # Optional: Also filter extreme mileage values
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

# Create final table in the format you showed earlier
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



#-----------------------------------------------

#visualization

library(ggplot2)
library(scales)

# Plot total mileage trend
p1_main<- ggplot(national_trend %>% filter(Data_Year >= 2015), 
                 aes(x = Data_Year, y = Avg_Mileage_Per_Vehicle )) +
  geom_line(color = "steelblue", size = 1.5) +
  geom_point(color = "darkblue", size = 3) +
  scale_y_continuous(
    labels = comma,  # Use comma for thousands separator
    # limits = c(6000, 8500),  # Set appropriate limits for your data
    breaks = seq(6000, 11500, by = 1000))+
  labs(
    title = "Average Annual Mileage per Vehicle (2015-2024)",
    #subtitle = "Based on DVLA/MOT data ",
    x = "Year",
    y = "Average Mileage(miles)",
    # caption = paste("2024: ", round(mileage_2024$Total_Mileage / 1e9, 1), 
    #                 " billion miles | Average: ", 
    #                 round(mileage_2024$avg_mileage_per_vehicle), " miles/vehicle")
  ) +
  theme_minimal() +
  theme(
    # plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray50")
  ) +
  # Add value labels
  geom_text(aes(label = paste0(round(Avg_Mileage_Per_Vehicle  / 1e3,2), "k")),
            vjust = -1, size = 3, color = "darkblue")+
  scale_x_continuous(
    limits = c(2015, 2024),  # Use c() for limits
    breaks = seq(2015, 2024, by = 3)  # Use seq() with by parameter
  )
print(p1_main)
print(national_trend,n=Inf)


# Calculate mileage by fuel type for 2024
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
print(mileage_by_fuel_2024,n=Inf)
# Bar chart
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




# Average mileage by fuel type
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


# Mileage by vehicle age
mileage_by_age_2024 <- Stock_model %>%
  filter(Data_Year == 2024, !is.na(Avg_Mileage)) %>%
  group_by(Vehicle_Age) %>%
  summarise(
    Total_Mileage = sum(Total_Mileage_GB, na.rm = TRUE),
    Vehicle_Count = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage = Total_Mileage / Vehicle_Count,
    .groups = "drop"
  )
print(mileage_by_age_2024,n=Inf)

# Line plot
p3<- ggplot(mileage_by_age_2024 %>% filter(Vehicle_Age <= 25), 
            aes(x = Vehicle_Age, y = Avg_Mileage)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "loess", se = TRUE, color = "orange", alpha = 0.2) +
  labs(
    title = " Mileage by Vehicle Age (2024)",
    #subtitle = "Mileage typically declines as vehicles age",
    x = "Vehicle Age (years)",
    y = "Average Mileage (miles)"
  ) +
  theme_minimal()
print(p3)
# Multi-panel dashboard
library(patchwork)



p2 <- ggplot(mileage_by_fuel_2024,
             aes(x = reorder(Fuel_Group, -Total_Mileage),
                 y = Total_Mileage / 1e9, fill = Fuel_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Mileage by Fuel Type", x = "", y = "Billion miles") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# p3 <- ggplot(mileage_by_age_2024 %>% filter(Vehicle_Age <= 25), 
#              aes(x = Vehicle_Age, y = Avg_Mileage)) +
#   geom_line(color = "darkgreen", size = 1) +
#   labs(title = "Mileage by Vehicle Age", x = "Age (years)", y = "Miles") +
#   theme_minimal()

# Combine plots
(p1_main ) / p3 +
  plot_annotation(
    #title = "UK Vehicle Mileage Analysis 2024",
    #subtitle = paste("Total: ", round(mileage_2024$Total_Mileage / 1e9, 1), 
    # "B miles | Average: ", round(mileage_2024$avg_mileage_per_vehicle), 
    #   " miles/vehicle"),
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )




# Create a publication-ready summary table
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







#========================================
# CO2 VISUALIZATIONS - LINKED WITH MILEAGE
#========================================

#----------------------------------------------------------------------------------
#Co2
#------------------------------------------------------------------------------------

# Step 1: Calculate CO2 with calibrated mileage and proper units
Stock_model <- Stock_model %>%
  mutate(
    # Convert calibrated mileage from miles to km (1 mile = 1.60934 km)
    Avg_Mileage_km = Avg_Mileage * 1.60934,
    
    # Calculate annual CO2 per vehicle (kg)
    # Formula: (Avg_CO2 in g/km * Avg_Mileage_km) / 1000 = kg CO2
    Annual_CO2_kg = (Avg_CO2 * Avg_Mileage_km) / 1000,
    
    # Recalculate total CO2 with proper units (tonnes = kg/1000)
    Total_CO2_GB_corrected = (Annual_CO2_kg * Vehicle_Stock_GB) / 1000
  )


# Step 2: CO2 trend over time with calibrated mileage
CO2_trend <- Stock_model %>%
  filter(!is.na(Avg_CO2), Vehicle_Stock_GB > 0) %>%
  group_by(Data_Year) %>%
  summarise(
    GB_Total_CO2 = sum(Total_CO2_GB_corrected, na.rm = TRUE),  # Now in tonnes
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Avge_CO2 = weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),  # This stays as g/km
    Avg_Mileage_miles = weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage_km = weighted.mean(Avg_Mileage_km, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Annual_CO2_kg = weighted.mean(Annual_CO2_kg, Vehicle_Stock_GB, na.rm = TRUE),
    Rows_Used = n(),
    .groups = "drop"
  )

print("=== UPDATED CO2 TREND WITH CORRECT UNITS ===")
print(CO2_trend)

# Step 3: CO2 by fuel type over time
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
print(co2_trend_by_fuel,n=Inf,width = Inf)

# Step 4: Data coverage analysis
cat("\n=== 2024 DATA COVERAGE ANALYSIS ===\n")
coverage_2024 <- Stock_model %>%
  filter(Data_Year == 2024) %>%
  summarise(
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Vehicles_with_Data = sum(Vehicle_Stock_GB * Has_Mileage_Data, na.rm = TRUE),
    Coverage_Pct = round(100 * Vehicles_with_Data / Total_Vehicles, 1),
    .groups = "drop"
  )

print(coverage_2024)


# Step 5: Validation against Greg's data
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
print(co2_validation, width = Inf)



# Step 6: Create scatter data for visualizations
scatter_data <- Stock_model %>%
  filter(!is.na(Avg_CO2), !is.na(Avg_Mileage), Vehicle_Stock_GB > 0, Data_Year == 2024) %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(
    Avg_CO2 = weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage = weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  )
print(scatter_data,n=Inf)




# Step 7: Age 3 data with corrected annual CO2 calculation
age3_data <- scatter_data %>%
  filter(Vehicle_Age == 3) %>%
  mutate(
    # Calculate annual CO2 per vehicle (kg) - using calibrated mileage
    Annual_CO2_kg = (Avg_CO2 * Avg_Mileage) / 1000,
    # Format for labels
    CO2_label = paste0(round(Annual_CO2_kg), " kg"),
    Mileage_label = paste0(round(Avg_Mileage/1000, 1), "k km"),
    CO2_rate_label = paste0(round(Avg_CO2), " g/km")
  )

print(age3_data, n = Inf)

# Step 8: Age 3 comparison table
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



# Step 9: Save the results
Stock_model_2024 <- Stock_model %>% 
  filter(Data_Year == 2024) %>% 
  select(-c(Has_Mileage_Data, MOT_Sample_Size))

setwd("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\R-programming-Infuze\\R-programming_Updated analysis\\New-updated_data\\Excell_files")
#write_csv_zipped(Stock_model_2024, "Stock_model(all).csv", row.names = FALSE)




#-------------------------
#visulisation
#-------------------------

# Create age 3 comparison plot
ggplot(age3_data, aes(x = reorder(Fuel_Group, -Annual_CO2_kg), y = Annual_CO2_kg, fill = Fuel_Group)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = CO2_label), vjust = -0.5, size = 4, fontface = "bold") +
  # Add mileage information as secondary annotation
  geom_text(aes(label = paste0("Mileage: ", Mileage_label, "\nRate: ", CO2_rate_label)), 
            vjust = 1.5, color = "white", size = 3.5, fontface = "bold") +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = comma_format(suffix = " kg"), 
                     limits = c(0, max(age3_data$Annual_CO2_kg) * 1.2)) +
  labs(
    title = "Annual CO2 Emissions per Vehicle at Age 3",
    #  subtitle = "Diesel emits 60% more CO₂ than petrol due to higher mileage and emission rate",
    x = "Fuel Type",
    y = "Annual CO2 Emissions (kg per vehicle)",
    # caption = "Note: BEV shows zero tailpipe emissions (excludes electricity generation)"
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


# Create a decomposition of what drives CO2 differences at age 3
decomp_data <- age3_data %>%
  filter(Fuel_Group != "Other") %>%
  mutate(
    # Baseline is petrol
    Emission_effect = Avg_CO2 / age3_data$Avg_CO2[age3_data$Fuel_Group == "Petrol"] - 1,
    Mileage_effect = Avg_Mileage / age3_data$Avg_Mileage[age3_data$Fuel_Group == "Petrol"] - 1,
    Combined_effect = (Emission_effect + 1) * (Mileage_effect + 1) - 1
  )

# Create a waterfall-style visualization
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
    title = "What Drives CO₂ Differences vs Petrol? (Age 3 Vehicles)",
    subtitle = "Diesel: Higher mileage (41%) + higher emission rate (13%) = 59% more CO₂",
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


# Scatter plot: CO2 vs Mileage
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
  # Add trend line statistics
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x.npc = "left", label.y.npc = "top", size = 3)

# 2. CO2 Intensity (CO2 per mile) by Fuel Type
# Calculate CO2 per mile (conversion: 1 km = 0.621371 miles)
co2_intensity_data <- Stock_model %>%
  filter(!is.na(Avg_CO2), !is.na(Avg_Mileage), Vehicle_Stock_GB > 0, Data_Year == 2024) %>%
  mutate(
    # Convert g/km to g/mile (1 km = 0.621371 miles)
    CO2_per_mile = Avg_CO2 / 0.621371,
    # Calculate annual CO2 per vehicle
    Annual_CO2_per_vehicle = CO2_per_mile * Avg_Mileage
  ) %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(
    CO2_per_mile = weighted.mean(CO2_per_mile, Vehicle_Stock_GB, na.rm = TRUE),
    Annual_CO2_per_vehicle = weighted.mean(Annual_CO2_per_vehicle, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage = weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  )

# Plot CO2 intensity (g/mile)
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

# 3. Annual CO2 per Vehicle vs Mileage
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
  # Highlight BEVs with 0 emissions
  annotate("text", x = max(co2_intensity_data$Avg_Mileage) * 0.8, y = 0,
           label = "BEVs: Zero tailpipe emissions\nregardless of mileage",
           color = "green", size = 3.5, fontface = "bold")

# 4. CO2 Efficiency Dashboard (Multi-panel)
library(patchwork)

# Panel A: CO2 trend over time
p_co2_trend <- ggplot(CO2_trend %>% filter(Data_Year >= 2015), 
                      aes(x = Data_Year, y = Avge_CO2)) +
  geom_line(color = "darkred", size = 1.5) +
  geom_point(color = "darkred", size = 3) +
  labs(
    title = "Average CO2 Emissions Trend",
    x = "Year",
    y = "Average CO2 (g/km)"
  ) +scale_x_continuous(
    limits = c(2015, 2024),
    breaks = seq(2015, 2024, by = 3),
    expand = c(0.05, 0.05)
  )+
  theme_minimal()
print(p_co2_trend)

# Panel B: CO2 by fuel type (2024)
co2_by_fuel_2024 <- Stock_model %>%
  filter(Data_Year == 2024, !is.na(Avg_CO2), Vehicle_Stock_GB > 0) %>%
  group_by(Fuel_Group) %>%
  summarise(
    Avg_CO2 = weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  )

print(co2_by_fuel_2024,n=Inf)


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

# Panel C: CO2 vs Age
co2_age_2024 <- Stock_model %>%
  filter(Data_Year == 2024, !is.na(Avg_CO2), Vehicle_Stock_GB > 0) %>%
  group_by(Vehicle_Age) %>%
  summarise(
    Avg_CO2 = weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  )
print(co2_age_2024,n=Inf)
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
# Panel D: Mileage vs CO2 correlation
p_correlation <- ggplot(scatter_data, 
                        aes(x = Avg_Mileage, y = Avg_CO2, color = Fuel_Group)) +
  geom_point(size = 3, alpha = 0.7) +
  stat_ellipse(level = 0.68) +  # Show 1 standard deviation ellipse
  labs(
    title = "Mileage-CO2 Relationship(2024)",
    x = "Average Mileage (miles)",
    y = "Average CO2 (g/km)",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_correlation)

# Combine all CO2 visualizations
co2_dashboard <- (p_co2_trend | p_co2_fuel) / (p_co2_age | p_correlation)
print(co2_dashboard)






# 5. CO2 Efficiency by Engine Size and Fuel Type
# Prepare data for engine size analysis



# Create CO2 engine data with proper NA handling
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
    Avg_CO2_gmile = Avg_CO2_gkm / 0.621371,  # Convert g/km to g/mile
    Vehicle_Count = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  )

# Alternative: Simpler approach without CO2_per_mile for BEVs
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
  # Calculate CO2 per mile only for non-BEV vehicles
  mutate(
    CO2_per_mile = ifelse(Fuel_Group == "BEV", 0, Avg_CO2 / 0.621371),
    # Ensure BEVs show 0 for Avg_CO2 instead of NaN
    Avg_CO2 = ifelse(Fuel_Group == "BEV", 0, Avg_CO2)
  )

# Heatmap: CO2 by Engine Size and Fuel (with improved color scaling)
ggplot(co2_engine_data, aes(x = engine_size_category, y = Fuel_Group, fill = Avg_CO2)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = ifelse(Fuel_Group == "BEV", 
                               "0 g/km\nBEV",
                               paste0(round(Avg_CO2), " g/km\n(", round(Avg_Mileage/1000,1), "k)"))), 
            color = "white", size = 3, fontface = "bold") +
  scale_fill_gradientn(
    colors = c("green", "yellow", "orange", "red"),
    na.value = "gray90",  # For NA values
    name = "Avg CO2 (g/km)",
    limits = c(0, max(co2_engine_data$Avg_CO2[!is.na(co2_engine_data$Avg_CO2)], na.rm = TRUE))
  ) +
  labs(
    title = "CO2 Emissions by Engine Size and Fuel Type (2024)",
    subtitle = "Numbers show: CO2 (g/km) and Annual Mileage (k miles)\nBEVs show 0 tailpipe emissions",
    x = "Engine Size Category",
    y = "Fuel Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

# Alternative visualization: Faceted bar chart
ggplot(co2_engine_data, aes(x = engine_size_category, y = Avg_CO2, fill = Fuel_Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Avg_CO2), " g/km")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  facet_wrap(~ Fuel_Group, scales = "free_y") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "CO2 Emissions by Engine Size and Fuel Type (2024)",
    subtitle = "BEVs show 0 emissions across all engine sizes",
    x = "Engine Size Category",
    y = "Average CO2 (g/km)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Scatter plot: CO2 vs Engine Size with mileage as bubble size
ggplot(co2_engine_data %>% filter(Fuel_Group != "BEV"), 
       aes(x = engine_size_category, y = Avg_CO2, color = Fuel_Group)) +
  geom_point(aes(size = Vehicle_Count), alpha = 0.7) +
  geom_line(aes(group = Fuel_Group), size = 0.5, alpha = 0.5) +
  scale_size_continuous(range = c(3, 12), 
                        breaks = c(10000, 100000, 1000000),
                        labels = scales::label_number(scale = 1e-3, suffix = "k")) +
  labs(
    title = "CO2 Emissions by Engine Size and Fuel Type (2024)",
    subtitle = "Bubble size represents number of vehicles. BEVs excluded (0 emissions).",
    x = "Engine Size Category",
    y = "Average CO2 (g/km)",
    size = "Number of Vehicles",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  # Add BEV annotation
  annotate("text", x = 4, y = max(co2_engine_data$Avg_CO2, na.rm = TRUE) * 0.9,
           label = "BEVs: 0 g/km\nfor all engine sizes",
           color = "darkgreen", size = 4, fontface = "bold")



# 6. Cumulative CO2 Impact Analysis
# Calculate total annual CO2 emissions by fuel type
total_co2_impact <- Stock_model %>%
  filter(Data_Year == 2024, !is.na(Avg_CO2), !is.na(Avg_Mileage), Vehicle_Stock_GB > 0) %>%
  mutate(
    Annual_CO2_per_vehicle = (Avg_CO2 / 0.621371) * Avg_Mileage,  # g/mile * miles = g
    Total_Annual_CO2 = Annual_CO2_per_vehicle * Vehicle_Stock_GB  # Total g
  ) %>%
  group_by(Fuel_Group) %>%
  summarise(
    Total_CO2_tonnes = sum(Total_Annual_CO2, na.rm = TRUE) / 1e6,  # Convert to tonnes
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),
    Avg_CO2_per_vehicle_tonnes = Total_CO2_tonnes / Total_Vehicles,
    .groups = "drop"
  )

# Bar plot of total CO2 impact
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







#===============================================
# VEHICLE CLASS ANALYSIS: MILEAGE & CO2 PATTERNS
#===============================================

# Check unique vehicle classes
vehicle_classes <- unique(Stock_model$vehicle_class)
cat("Number of vehicle classes:", length(vehicle_classes), "\n")
print(vehicle_classes)

# 1. Extract size category from vehicle_class for better ordering
Stock_model_plot <- Stock_model %>%
  mutate(
    # Extract size category (Small, Medium, Large, Very Large, Unsized, Unclassified)
    size_category = case_when(
      grepl("Small", vehicle_class) ~ "Small",
      grepl("Medium", vehicle_class) ~ "Medium", 
      grepl("Large", vehicle_class) & !grepl("Very Large", vehicle_class) ~ "Large",
      grepl("Very Large", vehicle_class) ~ "Very Large",
      grepl("Unsized", vehicle_class) ~ "Unsized",
      grepl("Unclassified", vehicle_class) ~ "Unclassified",
      TRUE ~ "Other"
    ),
    # Extract fuel type
    fuel_type = case_when(
      grepl("Petrol", vehicle_class) ~ "Petrol",
      grepl("Diesel", vehicle_class) ~ "Diesel",
      grepl("BEV", vehicle_class) ~ "BEV",
      grepl("HEV", vehicle_class) ~ "HEV",
      grepl("PHEV", vehicle_class) ~ "PHEV",
      TRUE ~ "Other"
    ),
    # Create ordered factor for size
    size_category = factor(size_category, 
                           levels = c("Small", "Medium", "Large", "Very Large", "Unsized", "Unclassified"))
  )%>%filter(size_category != "Unclassified" & !str_detect(size_category, "Unsized"),fuel_type !="Other") 

# 2. Summary by Vehicle Class (2024 data)
class_summary_2024 <- Stock_model_plot %>%
  filter(Data_Year == 2024, !is.na(Avg_Mileage), Vehicle_Stock_GB > 0,
         vehicle_class != "Unclassified" & !grepl("Unsized", vehicle_class)) %>%
  group_by(vehicle_class, size_category, Fuel_Group) %>%
  summarise(
    Avg_CO2 = if(all(is.na(Avg_CO2))) 0 else 
      weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
    Avg_Mileage = weighted.mean(Avg_Mileage, Vehicle_Stock_GB, na.rm = TRUE),
    Total_Vehicles = sum(Vehicle_Stock_GB, na.rm = TRUE),  # RENAMED FROM Vehicle_Count
    Total_Mileage = sum(Total_Mileage_GB, na.rm = TRUE),
    Avg_Engine_Size = weighted.mean(engine_size, Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    CO2_per_mile = ifelse(Fuel_Group == "BEV", 0, Avg_CO2 / 0.621371),
    Annual_CO2_per_vehicle = CO2_per_mile * Avg_Mileage,
    # Calculate share of total
    Share_of_Vehicles = Total_Vehicles / sum(Total_Vehicles) * 100,
    Share_of_Mileage = Total_Mileage / sum(Total_Mileage) * 100
  )
print(class_summary_2024)
# Print summary table
cat("\n=== VEHICLE CLASS SUMMARY (2024) ===\n")
print(class_summary_2024 %>% 
        select(vehicle_class, size_category, Fuel_Group, 
               Total_Vehicles, Avg_Mileage, Avg_CO2, 
               Share_of_Vehicles, Share_of_Mileage) %>%
        arrange(desc(Total_Vehicles)), n = 30)



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

print(paper_emissions_table, n = Inf)



paper_emissions_table %>% filter(N_Vehicles < 1000) %>% print()

paper_age_table <- scatter_data %>%   # or Stock_model filtered to 2024
  mutate(age_band = cut(Vehicle_Age, breaks = c(-1,5,10,15,23),
                        labels = c("0-5","6-10","11-15","16-23"))) %>%
  group_by(Fuel_Group, age_band) %>%
  summarise(
    Avg_CO2_gkm = weighted.mean(Avg_CO2, Total_Vehicles, na.rm = TRUE),
    Avg_Mileage = weighted.mean(Avg_Mileage, Total_Vehicles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Fuel_Group, age_band)



paper_age_size_table <- Stock_model_plot %>%   # or whichever object has Vehicle_Age + size_category + Fuel_Group
  filter(Data_Year == 2024, Fuel_Group %in% c("Petrol","Diesel","HEV","PHEV")) %>%
  mutate(age_band = cut(Vehicle_Age, breaks = c(-1,5,10,15,23),
                        labels = c("0-5","6-10","11-15","16-23"))) %>%
  group_by(Fuel_Group, age_band, size_category) %>%
  summarise(
    Avg_CO2 = weighted.mean(Avg_CO2, Vehicle_Stock_GB, na.rm = TRUE),
    N = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(N >= 1000) %>%    # drop thin cells at this finer grain
  arrange(Fuel_Group, age_band, size_category)

print(paper_age_size_table, n = Inf)

# Directly test the size-mix story: has the size distribution shifted toward
# larger vehicles in the newest cohort vs the middle-aged one?
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

print(size_mix_by_age, n = Inf)




# 3. HEATMAP: CO2 by Vehicle Class and Size
ggplot(class_summary_2024, 
       aes(x = size_category, y = reorder(Fuel_Group, Avg_CO2), fill = Avg_CO2)) +
  geom_tile(color = "white", size = 0.8) +
  geom_text(aes(label = paste0(round(Avg_CO2), "\n", round(Avg_Mileage/1000,1), "k")),
            color = "white", size = 3.5, fontface = "bold") +
  scale_fill_gradientn(
    colors = c("green", "yellow", "orange", "red", "darkred"),
    na.value = "gray90",
    name = "Avg CO2 (g/km)",
    limits = c(0, max(class_summary_2024$Avg_CO2, na.rm = TRUE))
  ) +
  labs(
    title = "CO2 Emissions and Mileage by Vehicle Size and Fuel Type (2024)",
    subtitle = "Numbers show: CO2 (g/km) and Annual Mileage (thousands)\nTile size represents number of vehicles",
    x = "Vehicle Size Category",
    y = "Fuel Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    panel.grid = element_blank()
  )

# 4. BUBBLE CHART: CO2 vs Mileage by Vehicle Class
ggplot(class_summary_2024, 
       aes(x = Avg_Mileage, y = Avg_CO2, color = size_category, size = Total_Vehicles)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = Fuel_Group), 
            size = 3, vjust = -1.2, fontface = "bold", check_overlap = TRUE) +
  scale_size_continuous(
    range = c(2, 20),
    breaks = c(10000, 100000, 1000000, 5000000),
    labels = scales::label_number(scale = 1e-3, suffix = "k"),
    name = "Number of Vehicles"
  ) +
  scale_color_brewer(palette = "Set1", name = "Vehicle Size") +
  labs(
    title = "CO2 vs Annual Mileage by Vehicle Class (2024)",
    subtitle = "Bubble size = Number of vehicles. BEVs cluster at bottom left (0 CO2).",
    x = "Average Annual Mileage (miles)",
    y = "Average CO2 Emissions (g/km)",
    caption = "Note: Higher mileage + higher CO2 = greater environmental impact"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major = element_line(color = "gray90")
  ) +
  # Highlight key areas
  annotate("rect", xmin = 5000, xmax = 15000, ymin = 0, ymax = 50,
           alpha = 0.1, fill = "green") +
  annotate("text", x = 10000, y = 25, 
           label = "Low Impact:\nLow CO2, Moderate Mileage", 
           color = "darkgreen", size = 3) +
  annotate("rect", xmin = 8000, xmax = 15000, ymin = 150, ymax = 250,
           alpha = 0.1, fill = "red") +
  annotate("text", x = 11500, y = 200, 
           label = "High Impact:\nHigh CO2, High Mileage", 
           color = "darkred", size = 3)

# 5. FACETED BAR CHART: Mileage by Vehicle Class
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

# If you want to keep the original vehicle_class but show just the fuel
class_summary_2024_simple <- class_summary_2024 %>%
  filter(!size_category %in% c("Unsized", "Unclassified"),Fuel_Group !="BEV") %>%
  mutate(
    # Remove size prefix from vehicle_class
    fuel_simple = gsub("(Small |Medium |Large |Very Large )", "", vehicle_class),
    fuel_simple = factor(fuel_simple, levels = c("BEV", "HEV", "PHEV", "Petrol", "Diesel"))
  )

p_class_simple <- ggplot(class_summary_2024_simple,
                         aes(x = size_category, y = Avg_CO2, fill = fuel_simple)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(Avg_CO2), " g/km")),
            position = position_stack(vjust = 0.5),   # centre label in each segment
            size = 3) +
  scale_fill_manual(values = c(
    "BEV"    = "#4DAF4A",
    "HEV"    = "#F18F01",
    "PHEV"   = "#984EA3",
    "Petrol" = "#377EB8",
    "Diesel" = "#E41A1C"
  ), name = "Fuel Type") +
  labs(
    x = "Vehicle Size",
    y = "Average CO2 Emissions (g/km)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank()
  )

print(p_class_simple)
#a grouped (dodged) bar
p_class_simple <- ggplot(class_summary_2024_simple,
                         aes(x = size_category, y = Avg_CO2, fill = fuel_simple)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(Avg_CO2)),
            position = position_dodge(width = 0.8),
            vjust = -0.4, size = 3) +
  scale_fill_manual(values = c(
    "BEV"    = "#4DAF4A",
    "HEV"    = "#F18F01",
    "PHEV"   = "#984EA3",
    "Petrol" = "#377EB8",
    "Diesel" = "#E41A1C"
  ), name = "Fuel Type") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Vehicle Size", y = "Average CO2 Emissions (g/km)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

print(p_class_simple)

# 7. STACKED AREA: Vehicle Distribution by Class
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

ggplot(vehicle_distribution %>% filter(!size_category %in% c("Unsized", "Unclassified")),
       aes(x = size_category, y = Total_Vehicles, fill = simple_class)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(round(Total_Vehicles/sum(Total_Vehicles)*100,0), "%")),
            position = position_fill(vjust = 0.5), size = 3, color = "white") +
  scale_fill_brewer(palette = "Set3", name = "Fuel Type") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Vehicle Fleet Composition by Size Category (2024)",
    subtitle = "Percentage distribution of fuel types within each vehicle size",
    x = "Vehicle Size Category",
    y = "Percentage of Vehicles"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

# 8. DUAL-AXIS PLOT: Mileage and CO2 by Size Category
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
    # title = "Mileage and CO2 Trends by Vehicle Size (2024)",
    # subtitle = "Bars: Average Mileage | Line: Average CO2 (right axis)\nLarger vehicles have higher CO2 but similar or lower mileage",
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

# 9. RADAR/SPIDER CHART for comprehensive comparison
# Install and load fmsb package if needed
if (!require(fmsb)) install.packages("fmsb")
library(fmsb)

# Prepare data for radar chart (top 10 vehicle classes by count)
top_classes <- class_summary_2024 %>%
  arrange(desc(Total_Vehicles)) %>%
  head(10) %>%
  select(vehicle_class, Avg_Mileage, Avg_CO2, Total_Vehicles) %>%
  mutate(
    # Normalize values for radar chart (0-100 scale)
    Mileage_Score = (Avg_Mileage - min(Avg_Mileage)) / 
      (max(Avg_Mileage) - min(Avg_Mileage)) * 100,
    CO2_Score = (Avg_CO2 - min(Avg_CO2)) / 
      (max(Avg_CO2) - min(Avg_CO2)) * 100,
    Vehicle_Score = (Total_Vehicles - min(Total_Vehicles)) / 
      (max(Total_Vehicles) - min(Total_Vehicles)) * 100
  )

# Print top classes table
cat("\n=== TOP 10 VEHICLE CLASSES BY COUNT (2024) ===\n")
print(top_classes %>% 
        select(vehicle_class, Total_Vehicles, Avg_Mileage, Avg_CO2) %>%
        arrange(desc(Total_Vehicles)))

# 10. DENSITY PLOT: Mileage Distribution by Vehicle Class
# Sample data for density plot (using individual rows weighted by vehicle count)
set.seed(123)
density_data <- Stock_model_plot %>%
  filter(Data_Year == 2024, !is.na(Avg_Mileage), Vehicle_Stock_GB > 0) %>%
  # Take top 8 most common classes for clarity
  group_by(vehicle_class) %>%
  filter(sum(Vehicle_Stock_GB, na.rm = TRUE) > 100000) %>%
  ungroup() %>%
  # Sample rows weighted by vehicle count
  sample_n(min(10000, nrow(.)), weight = Vehicle_Stock_GB, replace = TRUE)
print(density_data,n=Inf)

# Create mileage table by age group, size category, and fuel type
library(dplyr)
library(tidyr)

mileage_table <- density_data %>%
  # Create age groups
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
  
  # Group and calculate average mileage
  group_by(age_group, size_category, fuel_type) %>%
  summarise(
    avg_mileage = round(mean(Avg_Mileage, na.rm = TRUE), 0),
    vehicle_count = sum(Vehicle_Stock_GB, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Create the wide format table - MILEAGE ONLY
  pivot_wider(
    id_cols = c(age_group, size_category),
    names_from = fuel_type,
    values_from = avg_mileage,
    values_fill = 0
  ) %>%
  
  # Sort by age group and size category
  arrange(
    factor(age_group, levels = c("0-5 years", "6-10 years", "11-15 years", "16-23 years")),
    factor(size_category, levels = c("Small", "Medium", "Large", "Very Large"))
  )

# View the mileage table
cat("\n=== AVERAGE MILEAGE BY AGE GROUP, SIZE CATEGORY, AND FUEL TYPE ===\n")
print(mileage_table, n = Inf)


# First get baseline (Small Petrol) for each age group
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

# Calculate ratios
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

print(ratio_table, n = Inf)




#visualization 

ggplot(density_data, aes(x = Avg_Mileage, fill = fuel_type)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ size_category, scales = "free_y") +
  scale_fill_brewer(palette = "Set2", name = "Fuel Type") +
  labs(
    # title = "Mileage Distribution by Vehicle Size Category (2024)",
    # subtitle = "Density plots show mileage patterns for different vehicle sizes",
    x = "Annual Mileage (miles)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )
#Option B — ECDF
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

#Option C —

ggplot(density_data, aes(x = fuel_type, y = Avg_Mileage, fill = fuel_type)) +
  geom_violin(alpha = 0.6, scale = "width") +
  geom_boxplot(width = 0.15, alpha = 0.7, outlier.shape = NA) +
  facet_wrap(~ size_category) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Fuel Type", y = "Annual Mileage (miles)") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

#4-8 years
density_data_peak <- density_data %>%
  filter(Vehicle_Age >= 4, Vehicle_Age <= 8)

ggplot(density_data_peak, aes(x = Avg_Mileage, fill = fuel_type)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ size_category, scales = "free_y") +
  scale_fill_brewer(palette = "Set2", name = "Fuel Type") +
  labs(
    # title = "Mileage Distribution by Vehicle Size (Peak Usage: 4-8 Years)",
    #subtitle = "Clearest view of size-mileage relationship",
    x = "Annual Mileage (miles)",
    y = "Density"
  ) +
  theme_minimal()+
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14))


#(0-3 years) 

density_data_new <- density_data %>%
  filter(Vehicle_Age <= 3)
print(density_data_new,n=Inf)
ggplot(density_data_new, aes(x = Avg_Mileage, fill = fuel_type)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ size_category, scales = "free_y") +
  labs(title = "Mileage Distribution by Vehicle Size (New: 0-3 Years)")




#(9-15 years)

density_data_mature <- density_data %>%
  filter(Vehicle_Age >= 9, Vehicle_Age <= 15)

ggplot(density_data_mature, aes(x = Avg_Mileage, fill = fuel_type)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ size_category, scales = "free_y") +
  labs(title = "Mileage Distribution by Vehicle Size (Mature: 9-15 Years)")


#Three-Panel Comparison

# Create age-filtered datasets
density_data %>%
  mutate(age_group = case_when(
    Vehicle_Age <= 3 ~ "New (0-3 yrs)",
    Vehicle_Age >= 4 & Vehicle_Age <= 8 ~ "Peak (4-8 yrs)",
    Vehicle_Age >= 9 ~ "Mature (9+ yrs)"
  )) %>%
  filter(!is.na(age_group)) %>%
  
  ggplot(aes(x = Avg_Mileage, fill = fuel_type)) +
  geom_density(alpha = 0.6) +
  facet_grid(age_group ~ size_category, scales = "free_y") +
  scale_fill_brewer(palette = "Set2", name = "Fuel Type") +
  labs(
    title = "Mileage Distribution by Vehicle Size and Age",
    subtitle = "The size-mileage relationship changes dramatically with vehicle age",
    x = "Annual Mileage (miles)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




# 11. SUMMARY TABLE with kableExtra
library(kableExtra)

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



#########################################################################################################################################
# ============================================================
# 🚗 FUEL TYPE FORECASTING MODEL (IMPROVED VERSION)
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(stringr)
library(nls2)
library(purrr)

# ------------------------------------------------------------
# 1️⃣ PREPARE INPUT DATA
# ------------------------------------------------------------
summary_table <- join_final_complete %>%
  group_by(Fuel_Group, Data_Year) %>%
  summarise(
    total_fleet = sum(Fleet_Count, na.rm = TRUE),
    Avg_Vehicle_Age = weighted.mean(Vehicle_Age, Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Data_Year, desc(total_fleet))

print(summary_table)

# Filter out "Other" if not needed
fuel_percentages <- summary_table %>%
  
  group_by(Data_Year) %>%
  mutate(
    yearly_total = sum(total_fleet, na.rm = TRUE),
    percentage = (total_fleet / yearly_total) * 100
  ) %>%
  ungroup()
print(fuel_percentages )

# ------------------------------------------------------------

fuel_summary_table <- fuel_percentages %>%
  select(Data_Year, Fuel_Group, percentage) %>%
  # ADD THIS: Group and sum any duplicates
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
  # Calculate row sums to verify they equal 1
  mutate(
    Total_Sum = rowSums(select(., -Data_Year)),
    Check = round(Total_Sum, 10) == 100  # Allow for tiny floating-point errors
  ) %>%
  select(Data_Year,BEV, Diesel, HEV,PHEV, Petrol,Other, Total_Sum, Check)

# Print the summary table
cat("Fuel Type Composition Summary (Proportions Summing to 1):\n")
print(fuel_summary_table, n = 20)



cars_Numbers <-fuel_percentages %>% select(Data_Year,Fuel_Group,total_fleet)%>%
  pivot_wider(
    names_from = Fuel_Group,
    values_from = total_fleet,
    values_fill = 0,
    values_fn = sum  # This tells pivot_wider how to handle duplicates
  )%>%
  mutate(Total_Nu=rowSums(select(.,-Data_Year)))

cat("total_numer of vehicles")
print(cars_Numbers,n=30)
##########################################################
#------------------------------------------------------------
# 2️⃣ DEFINE FORECAST FUNCTION (auto-detect model type)
# ------------------------------------------------------------

#=======================
#create liner forecast
#=======================
forecast_fuel <- function(fuel_data, years_ahead = 10) {
  # Defensive extraction
  fuel_name <- unique(na.omit(fuel_data$Fuel))
  if (length(fuel_name) == 0) fuel_name <- "Unknown"
  
  # Time series
  ts_data <- ts(fuel_data$percentage, start = min(fuel_data$Data_Year, na.rm = TRUE))
  
  # Models
  model_linear <- lm(percentage ~ Data_Year, data = fuel_data)
  model_ets <- ets(ts_data)
  
  # Optional logistic model
  logistic_fit <- NULL
  if (nrow(fuel_data) >= 5 && diff(range(fuel_data$percentage, na.rm = TRUE)) > 1) {
    logistic_fit <- tryCatch(
      nls(percentage ~ SSlogis(Data_Year, Asym, xmid, scal), data = fuel_data),
      error = function(e) NULL
    )
  }
  
  # Future years
  future_years <- data.frame(
    Data_Year = seq(max(fuel_data$Data_Year, na.rm = TRUE) + 1,
                    max(fuel_data$Data_Year, na.rm = TRUE) + years_ahead)
  )
  
  # Predictions
  linear_pred <- predict(model_linear, newdata = future_years)
  ets_pred <- forecast(model_ets, h = years_ahead)$mean
  
  # Choose model
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
  
  # Keep between 0–100%
  final_pred <- pmin(pmax(final_pred, 0), 100)
  
  # Return WITHOUT Fuel (it will be added by group_modify)
  tibble(
    Data_Year = future_years$Data_Year,
    Predicted_Percentage = as.numeric(final_pred),
    Model_Used = model_used
  )
}


# Forecast all fuels EXCEPT BEV,PHEV
fuel_forecasts <- fuel_percentages %>% 
  filter(! Fuel_Group %in%c("Other")) %>%
  group_by(Fuel_Group) %>%
  group_modify(~ forecast_fuel(.x)) %>%
  ungroup()


#==============================================================================================
# Create manual forecasts for BEV, PHEV, ZEV based on the data from The Sixth Carbon Budget
#Methodology Report
#=============================================================================================
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

# Check the sums
cat("=== CHECKING YEARLY SUMS ===\n")
yearly_sums <- manual_forecasts %>%
  group_by(Data_Year) %>%
  summarise(
    Total = sum(Predicted_Percentage),
    .groups = "drop"
  )

print(yearly_sums)


# Now adjust the forecasts using your Excel formula logic
# For each year, adjust Diesel, Petrol, HEV proportionally
adjusted_forecasts <- fuel_forecasts %>%
  bind_rows(manual_forecasts) %>%
  group_by(Data_Year) %>%
  # No need for adjusting since all % are entered manually based on the outlook forecast (2025-2027) and
  # since 2028-2034 not given, the ZEV mandate used to estimate BEV and PHEV while other values (diesel, HEV) 
  # were estimated based on the grow/year from outlook data for (2025-2027), the remaining data adjusted to be logical
  # is the petrol and the HEV recalculated based on the original data get from outlook grow rate and adjusted to get 100% for total
  ungroup()

# View the adjusted forecasts
cat("=== ADJUSTED FORECASTS ===\n")
print(adjusted_forecasts, n = 50)

# Optional: Create final dataset with only adjusted percentages
fuel_forecasts_updated <- adjusted_forecasts %>%
  select(Data_Year, Fuel_Group, Percentage = Predicted_Percentage, Model_Used)

# # Set working directory
# setwd("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\R-programming-Infuze\\R-programming_Updated analysis\\New-updated_data\\Excell_files")
# write_csv_zipped(fuel_forecasts_updated, "fuel_forecasts_updated.csv", row.names = FALSE)
# 
# cat("\n=== FILE SAVED SUCCESSFULLY ===\n")
# cat("Saved to: fuel_forecasts_updated.csv\n")


# ==========================================================
# 8. COMPREHENSIVE VISUALIZATION - ALL FUELS IN ONE FIGURE
# ==========================================================

# Get historical data (2024 and before) - RENAME FCEV to ZEV
historical_data <- fuel_percentages %>%
  filter(Data_Year == "2024",
         Avg_Vehicle_Age == "0",
         !is.na(Avg_Vehicle_Age)) %>%
  select(Data_Year, Fuel_Group, yearly_total, total_fleet) %>%
  mutate(Model_Used = "Historical") %>%
  ungroup() %>%                                # <-- remove all grouping
  mutate(
    total = sum(total_fleet, na.rm = TRUE),    # calculate total across all fuels
    Percentage = 100 * total_fleet / total ,    # compute proper percentage
    Data_Year = as.numeric(Data_Year))


# For forecasts: Ensure ZEV is included
forecast_data <- fuel_forecasts_updated 
# mutate(
#   # If forecasts use FCEV name, rename to ZEV
#   Fuel = ifelse(Fuel == "HEV", "HEV", Fuel)
# )

# Combine historical data with forecasts
plot_data <- bind_rows(
  historical_data,
  forecast_data
) %>%
  filter(Data_Year >= 2024, Data_Year <= 2034)
plot_data <- plot_data %>%
  group_by(Data_Year) %>%
  mutate(Percentage = 100 * Percentage / sum(Percentage, na.rm = TRUE)) %>%
  ungroup()

# Convert to wide format for stacked area plot
# First, aggregate any duplicate Data_Year + Fuel combinations
plot_data_aggregated <- plot_data %>%
  group_by(Data_Year, Fuel_Group) %>%
  summarise(
    Percentage = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

# Now convert to wide format
plot_data_wide <- plot_data_aggregated %>%
  pivot_wider(
    names_from = Fuel_Group,
    values_from = Percentage,
    values_fill = 0
  ) %>%
  arrange(Data_Year)

# Define the desired stacking order: Traditional at bottom, EVs on top
# Order matters: First in list = bottom of stack, last = top of stack
fuel_order <- c("Petrol", "Diesel", "HEV", "PHEV",  "BEV")

# Reorder columns in the wide data according to our stacking order
existing_fuels <- fuel_order[fuel_order %in% names(plot_data_wide)]
plot_data_wide <- plot_data_wide %>%
  select(Data_Year, all_of(existing_fuels))

# Convert back to long format with proper factor levels

#===============
#plot-1
#================

plot_data_long <- plot_data_wide %>%
  pivot_longer(
    cols = -Data_Year,
    names_to = "Fuel",
    values_to = "Percentage"
  ) %>%
  mutate(
    Fuel = factor(Fuel, levels = fuel_order)
  )

# Create STACKED AREA CHART with reordered stacking

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
  scale_y_continuous(breaks = seq(0, 100, 20)) +   # ← NO limits
  scale_fill_manual(values = c(
    Petrol = "#DC143C", Diesel = "#696969", HEV = "#FFD700",
    PHEV = "#FFA500", ZEV = "#8B4513", BEV = "#2E8B57"
  )) +
  theme_minimal(base_size = 15)

print(p_comprehensive)

# ==========================================================
# ALTERNATIVE: STACKED BAR CHART with same ordering
# ==========================================================
p_stacked_bar <- ggplot(plot_data_long, aes(x = Data_Year, y = Percentage, fill = Fuel)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  scale_x_continuous(breaks = seq(2023, 2034, 2)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  scale_fill_manual(values = c(
    Petrol = "#DC143C", Diesel = "#696969", HEV = "#FFD700",
    PHEV = "#FFA500", ZEV = "#8B4513", BEV = "#2E8B57"
  )) +
  labs(
    #title = "Sales of Fleet Composition  (2024–2034)",
    # subtitle = "Stacked from bottom: Traditional → Transition → Electric",
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

# ==========================================================
# VERIFICATION: Show the stacking order
# ==========================================================
cat("\nStacking order (bottom to top):\n")
for(i in seq_along(fuel_order)) {
  cat(i, ":", fuel_order[i], "\n")
}

# Check yearly totals
yearly_totals <- plot_data_long %>%
  group_by(Data_Year) %>%
  summarise(
    Total = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nYearly percentage totals (should be 100%):\n")
print(yearly_totals)

# Show a sample year to verify stacking
cat("\nSample: Composition for 2030:\n")
sample_2030 <- plot_data_long %>%
  filter(Data_Year == 2030) %>%
  arrange(desc(Fuel))  # Show from top to bottom
print(sample_2030)


# ====================================================
# PART 1: AVERAGE AGE ANALYSIS (2000-2024)
# ====================================================

age_data_full <- data %>%
  filter(LicenceStatus == "Licensed", BodyType == "Cars", YearFirstUsed>=2000) %>%
  # First convert all year columns to character, then clean them
  mutate(across(8:18, ~ {
    # Convert to character first
    char_col <- as.character(.x)
    # Replace [z] with NA
    char_col <- ifelse(char_col == "[z]", NA, char_col)
    # Convert to numeric (will turn non-numeric to NA)
    as.numeric(char_col)
  })) %>%
  # Now pivot (all columns should be numeric)
  pivot_longer(
    cols = 8:18,
    names_to = "Data_Year",
    values_to = "Fleet_Count"
  ) %>%
  mutate(
    Data_Year = as.numeric(Data_Year),
    YearFirstUsed = as.numeric(YearFirstUsed),
    Vehicle_Age = Data_Year - YearFirstUsed
  ) %>%
  # Remove rows with NA Vehicle_Count or negative ages
  filter(!is.na(Fleet_Count) & Fleet_Count > 0 & Vehicle_Age >= 0)

# Remove any unrealistic ages (negative or extremely high)
age_data_full <- age_data_full %>%
  filter(Vehicle_Age >= 0 & Vehicle_Age <= 100, !Vehicle_Age=="NA")

# ====================================================
# 1.1 Calculate average age from 2000-2024
# ====================================================

# Calculate average age by YearFirstUsed
average_age_by_year <- age_data_full %>%
  group_by(Data_Year) %>%
  summarise(
    Total_Vehicles = sum(Fleet_Count, na.rm = TRUE),
    Average_Age = weighted.mean(Vehicle_Age, Fleet_Count, na.rm = TRUE),
    Median_Age = median(rep(Vehicle_Age, Fleet_Count),na.rm = TRUE),
    Q1_Age = quantile(rep(Vehicle_Age, Fleet_Count), 0.25,na.rm = TRUE),
    Q3_Age = quantile(rep(Vehicle_Age, Fleet_Count), 0.75,na.rm = TRUE)
  ) %>%
  filter(Data_Year >= 2000 & Data_Year <= 2024)

# Create a table for average age
cat("=== AVERAGE AGE OF FLEET (2000-2024) ===\n")
print(average_age_by_year, n = 25)

# Write to CSV for further analysis
# write.csv(average_age_by_year, "average_age_2000_2024.csv", row.names = FALSE)

# ====================================================
# 1.2 Create visualization
# ====================================================

p_age_trend <- ggplot(average_age_by_year, aes(x = Data_Year)) +
  geom_ribbon(aes(ymin = Q1_Age, ymax = Q3_Age), 
              fill = "#ADD8E6", alpha = 0.3) +
  geom_line(aes(y = Average_Age), color = "#2E86AB", size = 1.5) +
  geom_point(aes(y = Average_Age), color = "#2E86AB", size = 2) +
  geom_line(aes(y = Median_Age), color = "#A7333F", size = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(2000, 2024, 2)) +
  scale_y_continuous(breaks = seq(0, 15, 2)) +
  labs(
    title = "Average Age of UK Car Fleet (2000-2024)",
    subtitle = "Vehicles first registered in each year, measured in 2024",
    x = "Year ",
    y = "Average Vehicle Age (Years)",
    caption = "Blue line: Average age\nRed dashed line: Median age\nGray ribbon: Interquartile range (25th-75th percentile)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray90", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(p_age_trend)



# ====================================================
# 1.3 ADDITIONAL: Average age of vehicles by birth cohort
# ====================================================

# This shows what you were originally trying to show:
# For vehicles first registered in each year, what was their average age over time?
cohort_age_analysis <- age_data_full %>%
  filter(YearFirstUsed >= 2000 & YearFirstUsed <= 2024) %>%
  group_by(YearFirstUsed, Data_Year) %>%
  summarise(
    Total_Vehicles = sum(Fleet_Count, na.rm = TRUE),
    Average_Age = weighted.mean(Vehicle_Age, Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Data_Year >= YearFirstUsed)  # Only show years when vehicles exist

# Visualize a few cohorts
selected_cohorts <- c(2000, 2005, 2010, 2015, 2020)

p_cohort_ages <- cohort_age_analysis %>%
  filter(YearFirstUsed %in% selected_cohorts) %>%
  ggplot(aes(x = Data_Year, y = Average_Age, color = factor(YearFirstUsed))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2000, 2024, 4)) +
  scale_y_continuous(breaks = seq(0, 25, 5)) +
  scale_color_brewer(palette = "Set1", name = "First Registered") +
  labs(
    title = "Aging of Vehicle Cohorts Over Time",
    subtitle = "How vehicles from different registration years age",
    x = "Year",
    y = "Average Age (Years)",
    caption = "Each line shows vehicles first registered in a specific year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(p_cohort_ages)


# ====================================================
# Summary Statistics
# ====================================================
average_fleet_age_by_year <- average_age_by_year
cat("\n=== FLEET AGE SUMMARY ===\n")
cat("Overall trend 2000-2024:\n")

# Calculate trend
trend_model <- lm(Average_Age ~ Data_Year, data = average_fleet_age_by_year)
trend_slope <- coef(trend_model)[2]
trend_p <- summary(trend_model)$coefficients[2,4]

cat(sprintf("  Average age increased by %.3f years per year\n", trend_slope))
cat(sprintf("  Trend significance: p = %.4f\n", trend_p))
cat(sprintf("  2000 average age: %.1f years\n", 
            average_fleet_age_by_year$Average_Age[1]))
cat(sprintf("  2024 average age: %.1f years\n", 
            average_fleet_age_by_year$Average_Age[nrow(average_fleet_age_by_year)]))
cat(sprintf("Total increase: %.1f years over 24 years\n",
            average_fleet_age_by_year$Average_Age[nrow(average_fleet_age_by_year)] - 
              average_fleet_age_by_year$Average_Age[1]))



# ====================================================
# PART 2: ENGINE SIZE ANALYSIS FOR 2004, 2014, 2024
# ====================================================

# ----------------------------------------------------
# 2.0 Prepare engine size data with proper fuel grouping
# ----------------------------------------------------

# Start with your Vcar_classified data (already created in your original code)
Vcar_classified_all_years <- Vcar_classified %>%
  # Convert year columns to numeric
  mutate(across(7:17, ~ {
    char_col <- as.character(.x)
    char_col <- ifelse(char_col == "[z]", NA, char_col)
    as.numeric(char_col)
  })) %>%
  # Pivot to long format
  pivot_longer(
    cols = 7:17,
    names_to = "Data_Year",
    values_to = "Fleet_Count"
  ) %>%
  mutate(
    Data_Year = as.numeric(Data_Year),
    # Clean engine size
    engine_size_numeric = as.numeric(
      ifelse(engine_size %in% c("[x]", "[z]", "N/A", "NA", "", "NA"), 
             NA, engine_size)
    ),
    engine_size_liters = engine_size_numeric / 1000,
    # Create consistent fuel groups
    Fuel_Group = case_when(
      str_detect(Fuel, "Plug-In Hybrid Electric") ~ "PHEV",
      str_detect(Fuel, "Hybrid Electric") & !str_detect(Fuel, "Plug-In") ~ "HEV",
      Fuel %in% c("Battery Electric", "Range Extended Electric") ~ "BEV",
      Fuel == "Petrol" ~ "Petrol",
      Fuel == "Diesel" ~ "Diesel",
      TRUE ~ "Other"
    )
  ) %>%
  # Filter for valid data
  filter(!is.na(engine_size_liters),
         !is.na(Fleet_Count),
         Fleet_Count > 0,
         engine_size_liters > 0,
         engine_size_liters <= 10,
         Fuel_Group %in% c("Petrol", "Diesel", "HEV", "PHEV", "BEV"))

# Selected years for analysis
selected_years <- c(2004, 2014, 2024)

# ----------------------------------------------------
# 2.1 Fleet-weighted violin plots (EFFICIENT)
# ----------------------------------------------------

# Filter for selected years
violin_data <- Vcar_classified_all_years %>%
  filter(Data_Year %in% selected_years) %>%
  mutate(
    Year_Label = factor(Data_Year, levels = selected_years,
                        labels = paste0(selected_years, " Fleet"))
  )

# Create weighted violin plots
p_violin_weighted <- ggplot(
  violin_data,
  aes(
    x = Fuel_Group,
    y = engine_size_liters,
    fill = Fuel_Group,
    weight = Fleet_Count  # KEY: Weight by fleet count
  )
) +
  geom_violin(trim = TRUE, scale = "width", alpha = 0.7) +
  # Add boxplot without weights (shows model-level distribution)
  geom_boxplot(
    aes(weight = NULL),  # Remove weight for boxplot
    width = 0.15, 
    outlier.shape = NA, 
    alpha = 0.5,
    color = "black"
  ) +
  # Add weighted median points
  stat_summary(
    aes(weight = Fleet_Count),
    fun = function(x, w) {
      Hmisc::wtd.quantile(x, weights = w, probs = 0.5)
    },
    geom = "point",
    shape = 23,
    size = 3,
    fill = "white",
    color = "black"
  ) +
  facet_wrap(~ Year_Label, nrow = 1) +
  scale_fill_manual(values = c(
    "Petrol" = "#2E86AB",
    "Diesel" = "#A7333F", 
    "HEV" = "#8FB573",
    "PHEV" = "#B5738F",
    "BEV" = "#FFB366"
  )) +
  scale_y_continuous(
    breaks = seq(0, 6, 1),
    limits = c(0, 6),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Engine Size Distribution by Fuel Type",
    subtitle = "Fleet-weighted violin plots (density represents vehicle counts)",
    x = "Fuel Type",
    y = "Engine Size (Liters)",
    caption = "Violins weighted by fleet count | Boxplots show model-level distribution | Diamonds show weighted medians"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "gray95", color = "gray80"),
    panel.spacing = unit(1.2, "lines")
  )

print(p_violin_weighted)






# ----------------------------------------------------
# 2.2 Fleet-weighted quartile analysis
# ----------------------------------------------------

quartile_analysis_weighted <- Vcar_classified_all_years %>%
  filter(Data_Year %in% selected_years) %>%
  group_by(Data_Year, Fuel_Group) %>%
  summarise(
    Total_Vehicles = sum(Fleet_Count, na.rm = TRUE),
    Min_Size = min(engine_size_liters, na.rm = TRUE),
    # Weighted quantiles
    Q1_Size = Hmisc::wtd.quantile(engine_size_liters, 
                                  weights = Fleet_Count,
                                  probs = 0.25, na.rm = TRUE),
    Median_Size = Hmisc::wtd.quantile(engine_size_liters,
                                      weights = Fleet_Count,
                                      probs = 0.5, na.rm = TRUE),
    Q3_Size = Hmisc::wtd.quantile(engine_size_liters,
                                  weights = Fleet_Count,
                                  probs = 0.75, na.rm = TRUE),
    Max_Size = max(engine_size_liters, na.rm = TRUE),
    Mean_Size = weighted.mean(engine_size_liters, Fleet_Count, na.rm = TRUE),
    Std_Dev = sqrt(Hmisc::wtd.var(engine_size_liters, 
                                  weights = Fleet_Count, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(Data_Year, Fuel_Group)

# Display quartile analysis
cat("\n=== FLEET-WEIGHTED QUARTILE ANALYSIS OF ENGINE SIZES ===\n")
print(quartile_analysis_weighted, n = 30)

# Write to CSV
write.csv(quartile_analysis_weighted, 
          "engine_size_quartile_analysis_weighted.csv", 
          row.names = FALSE)

# ----------------------------------------------------
# 2.3 Define 2024 size thresholds for categorization
# ----------------------------------------------------

size_thresholds_2024 <- quartile_analysis_weighted %>%
  filter(Data_Year == 2024) %>%
  select(Fuel_Group, Q1_Size, Median_Size, Q3_Size)

cat("\n=== 2024 SIZE THRESHOLDS BY FUEL TYPE ===\n")
print(size_thresholds_2024)

# ----------------------------------------------------
# 2.4 Categorize vehicles using 2024 thresholds
# ----------------------------------------------------

size_category_analysis <- Vcar_classified_all_years %>%
  filter(Data_Year %in% selected_years) %>%
  # Join with 2024 thresholds
  left_join(size_thresholds_2024, by = "Fuel_Group") %>%
  mutate(
    Size_Category = case_when(
      engine_size_liters <= Q1_Size ~ "Small (≤Q1)",
      engine_size_liters <= Median_Size ~ "Small-Medium (Q1-Median)",
      engine_size_liters <= Q3_Size ~ "Medium-Large (Median-Q3)",
      TRUE ~ "Large (>Q3)"
    ),
    Size_Category = factor(Size_Category,
                           levels = c("Small (≤Q1)", 
                                      "Small-Medium (Q1-Median)",
                                      "Medium-Large (Median-Q3)", 
                                      "Large (>Q3)"))
  ) %>%
  # Calculate weighted counts
  group_by(Data_Year, Fuel_Group, Size_Category) %>%
  summarise(
    Vehicle_Count = sum(Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Data_Year, Fuel_Group) %>%
  mutate(
    Percentage = Vehicle_Count / sum(Vehicle_Count) * 100
  ) %>%
  arrange(Data_Year, Fuel_Group, Size_Category)

# Display size category analysis
cat("\n=== VEHICLE DISTRIBUTION BY SIZE CATEGORIES (Using 2024 Thresholds) ===\n")
print(size_category_analysis, n = 50)

# Create a wide format table for comparison
size_category_wide <- size_category_analysis %>%
  select(Data_Year, Fuel_Group, Size_Category, Percentage) %>%
  pivot_wider(
    names_from = c(Size_Category, Data_Year),
    values_from = Percentage,
    values_fill = 0
  ) %>%
  arrange(Fuel_Group)

cat("\n=== SIZE CATEGORY DISTRIBUTION (Wide Format) ===\n")
print(size_category_wide, n = 20)

# Write to CSV
write.csv(size_category_analysis, 
          "vehicle_size_category_analysis_2024_thresholds.csv", 
          row.names = FALSE)

# ----------------------------------------------------
# 2.5 Visualize size category distribution
# ----------------------------------------------------

p_size_category <- size_category_analysis %>%
  mutate(
    Year_Label = factor(Data_Year, levels = selected_years,
                        labels = paste0(selected_years, " Fleet"))
  ) %>%
  ggplot(aes(x = Fuel_Group, y = Percentage, fill = Size_Category)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  facet_wrap(~ Year_Label, nrow = 1) +
  scale_fill_brewer(palette = "Set2", name = "Size Category") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Vehicle Size Distribution by Fuel Type",
    subtitle = "Using 2024 fleet-weighted quartile thresholds applied to all years",
    x = "Fuel Type",
    y = "Percentage of Fleet",
    caption = "Size categories based on 2024 fleet-weighted quartiles for each fuel type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "gray95", color = "gray80"),
    panel.spacing = unit(1.2, "lines")
  )

print(p_size_category)

# ----------------------------------------------------
# 2.6 Create a detailed summary table
# ----------------------------------------------------

library(dplyr)
library(tidyr)

summary_table <- size_category_analysis %>%
  ungroup() %>%   # 🔑 CRITICAL FIX
  complete(
    Fuel_Group,
    Size_Category,
    Data_Year = c(2004, 2014, 2024),
    fill = list(Percentage = 0)
  ) %>%
  pivot_wider(
    id_cols = c(Fuel_Group, Size_Category),
    names_from = Data_Year,
    values_from = Percentage
  ) %>%
  mutate(
    Change_2004_2024 = `2024` - `2004`,
    Size_Category = factor(
      Size_Category,
      levels = c(
        "Small (≤Q1)",
        "Small-Medium (Q1-Median)",
        "Medium-Large (Median-Q3)",
        "Large (>Q3)"
      )
    )
  ) %>%
  arrange(Fuel_Group, Size_Category)

cat("\n=== SIZE CATEGORY TRENDS (2004–2024) ===\n")
print(summary_table, n = 30)


# ----------------------------------------------------
# FINAL SUMMARY
# ----------------------------------------------------

average_fleet_age_by_year <- average_age_by_year
cat(paste0("\n", strrep("=", 60), "\n"))
cat("SUMMARY OF ANALYSES\n")
cat(paste0(strrep("=", 60), "\n\n"))

cat("1. AVERAGE FLEET AGE (2000–2024):\n")
cat("   - Years analyzed: 2000 to 2024\n")
cat("   - 2000 average age:",
    round(average_fleet_age_by_year$Average_Age[1], 1),
    "years\n")
cat("   - 2024 average age:",
    round(average_fleet_age_by_year$Average_Age[nrow(average_fleet_age_by_year)], 1),
    "years\n")
cat("   - Change:",
    round(
      average_fleet_age_by_year$Average_Age[nrow(average_fleet_age_by_year)] -
        average_fleet_age_by_year$Average_Age[1], 1
    ),
    "years increase\n")
cat("   - Trend:",
    ifelse(
      cor(average_fleet_age_by_year$Data_Year,
          average_fleet_age_by_year$Average_Age,
          use = "complete.obs") > 0,
      "Increasing", "Decreasing"
    ),
    "average age\n\n")

cat("2. ENGINE SIZE ANALYSIS (2004, 2014, 2024):\n")
for (year in selected_years) {
  year_data <- quartile_analysis_weighted %>% filter(Data_Year == year)
  cat(sprintf("   %d Fleet:\n", year))
  cat(sprintf("     - Total vehicles: %s\n",
              format(sum(year_data$Total_Vehicles), big.mark = ",")))
  cat(sprintf("     - Petrol median: %.2f L\n",
              year_data %>% filter(Fuel_Group == "Petrol") %>% pull(Median_Size)))
  cat(sprintf("     - Diesel median: %.2f L\n",
              year_data %>% filter(Fuel_Group == "Diesel") %>% pull(Median_Size)))
  cat(sprintf("     - BEV median: %.2f L\n",
              year_data %>% filter(Fuel_Group == "BEV") %>% pull(Median_Size)))
  cat("\n")
}

cat("3. SIZE CATEGORISATION (Using 2024 Thresholds):\n")
cat("   - Small (≤Q1): Below 25th percentile of 2024 fleet\n")
cat("   - Small-Medium (Q1–Median): 25th to 50th percentile\n")
cat("   - Medium-Large (Median–Q3): 50th to 75th percentile\n")
cat("   - Large (>Q3): Above 75th percentile\n\n")

cat("Output files created:\n")
cat("   - average_fleet_age_2000_2024.csv\n")
cat("   - engine_size_quartile_analysis_weighted.csv\n")
cat("   - vehicle_size_category_analysis_2024_thresholds.csv\n")
cat(paste0(strrep("=", 60), "\n"))



# ---------------------------
# 1. SETUP ENVIRONMENT NATCOP
# ---------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(patchwork)
library(lubridate)
library(stringr)
library(data.table)
library(scales)
# Read all probability files with robust error handling
Probs_2021 <- read.table("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\NTEM\\NTEM 8.0 Core\\Core\\NATCOP\\Outputs\\probs_2021.txt", header = TRUE,skip = 1,sep = "",fill = TRUE,stringsAsFactors = FALSE,blank.lines.skip = TRUE,comment.char = "=")
Probs_2026 <- read.table("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\NTEM\\NTEM 8.0 Core\\Core\\NATCOP\\Outputs\\probs_2026.txt",header = TRUE,skip = 1,sep = "",fill = TRUE,stringsAsFactors = FALSE,blank.lines.skip = TRUE,comment.char = "=")
Probs_2031 <- read.table("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\NTEM\\NTEM 8.0 Core\\Core\\NATCOP\\Outputs\\probs_2031.txt",header = TRUE,skip = 1,sep = "",fill = TRUE,stringsAsFactors = FALSE,blank.lines.skip = TRUE,comment.char = "=")
Probs_2036 <- read.table("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\NTEM\\NTEM 8.0 Core\\Core\\NATCOP\\Outputs\\probs_2036.txt",header = TRUE,skip = 1,sep = "",fill = TRUE,stringsAsFactors = FALSE,blank.lines.skip = TRUE,comment.char = "=")
Probs_2041 <- read.table("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\NTEM\\NTEM 8.0 Core\\Core\\NATCOP\\Outputs\\probs_2041.txt",header = TRUE,skip = 1,sep = "",fill = TRUE,stringsAsFactors = FALSE,blank.lines.skip = TRUE,comment.char = "=")
Probs_2046 <- read.table("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\NTEM\\NTEM 8.0 Core\\Core\\NATCOP\\Outputs\\probs_2046.txt",header = TRUE,skip = 1,sep = "",fill = TRUE,stringsAsFactors = FALSE,blank.lines.skip = TRUE,comment.char = "=")

# Remove problematic row 7701 from all datasets
Probs_2021 <- Probs_2021[-7701, ]
Probs_2026 <- Probs_2026[-7701, ]
Probs_2031 <- Probs_2031[-7701, ]
Probs_2036 <- Probs_2036[-7701, ]
Probs_2041 <- Probs_2041[-7701, ]
Probs_2046 <- Probs_2046[-7701, ]

# Add Year column to each dataset
Probs_2021$Year <- 2021
Probs_2026$Year <- 2026
Probs_2031$Year <- 2031
Probs_2036$Year <- 2036
Probs_2041$Year <- 2041
Probs_2046$Year <- 2046

# Function to clean and convert numeric columns
clean_numeric_columns <- function(df) {
  # Identify numeric columns (assuming they are P0, P1, P2, P3P)
  numeric_cols <- c("P0", "P1", "P2", "P3P")
  
  # Clean and convert each numeric column
  for(col in numeric_cols) {
    if(col %in% names(df)) {
      # Remove commas and convert to numeric
      df[[col]] <- as.numeric(gsub(",", "", as.character(df[[col]])))
    }
  }
  return(df)
}

# Apply cleaning to all dataframes
Probs_2021 <- clean_numeric_columns(Probs_2021)
Probs_2026 <- clean_numeric_columns(Probs_2026)
Probs_2031 <- clean_numeric_columns(Probs_2031)
Probs_2036 <- clean_numeric_columns(Probs_2036)
Probs_2041 <- clean_numeric_columns(Probs_2041)
Probs_2046 <- clean_numeric_columns(Probs_2046)

# Check the structure of one dataframe to verify
cat("Structure of Probs_2021:\n")
str(Probs_2021)

# Combine all data
all_probs <- rbind(Probs_2021, Probs_2026, Probs_2031, Probs_2036, Probs_2041, Probs_2046)

# Verify the combined data structure
cat("\nStructure of combined all_probs:\n")
str(all_probs)

# Check for NA values in numeric columns
cat("\nNA values in numeric columns:\n")
print(colSums(is.na(all_probs[, c("P0", "P1", "P2", "P3P")])))

# Step 1: Calculate total vehicles, households, and vehicles per household
all_probs_calculated <- all_probs %>%
  mutate(
    # Total vehicles (using 3.5 for P3P category as recommended)
    Total_Vehicles = (1 * P1) + (2 * P2) + (3.5 * P3P),
    # Total households
    Total_Households = P0 + P1 + P2 + P3P,
    # Vehicles per household
    Vehicles_per_Household = Total_Vehicles / Total_Households,
    # Vehicle ownership proportions
    Prop_0_vehicles = P0 / Total_Households,
    Prop_1_vehicle = P1 / Total_Households,
    Prop_2_vehicles = P2 / Total_Households,
    Prop_3plus_vehicles = P3P / Total_Households
  )

# Step 2: National level summary by year
national_summary <- all_probs_calculated %>%
  group_by(Year) %>%
  summarise(
    Total_Households = sum(Total_Households, na.rm = TRUE),
    Total_Vehicles = sum(Total_Vehicles, na.rm = TRUE),
    Avg_Vehicles_per_HH = mean(Vehicles_per_Household, na.rm = TRUE),
    Median_Vehicles_per_HH = median(Vehicles_per_Household, na.rm = TRUE),
    # Vehicle ownership composition
    Prop_0_vehicles = sum(P0, na.rm = TRUE) / Total_Households,
    Prop_1_vehicle = sum(P1, na.rm = TRUE) / Total_Households,
    Prop_2_vehicles = sum(P2, na.rm = TRUE) / Total_Households,
    Prop_3plus_vehicles = sum(P3P, na.rm = TRUE) / Total_Households,
    .groups = 'drop'
  ) %>%
  mutate(
    # Calculate growth rates
    HH_Growth_Rate = (Total_Households / lag(Total_Households) - 1) * 100,
    Vehicle_Growth_Rate = (Total_Vehicles / lag(Total_Vehicles) - 1) * 100,
    VPH_Growth_Rate = (Avg_Vehicles_per_HH / lag(Avg_Vehicles_per_HH) - 1) * 100
  )

# Display the national summary
cat("\nNational Summary 2021-2046:\n")
print(national_summary)

# Step 3: Create visualizations

# Plot 1: Total Households and Vehicles over time
p1 <- ggplot(national_summary, aes(x = Year)) +
  geom_line(aes(y = Total_Households / 1000000, color = "Households"), size = 1.5) +
  geom_line(aes(y = Total_Vehicles / 1000000, color = "Vehicles"), size = 1.5) +
  geom_point(aes(y = Total_Households / 1000000), size = 2) +
  geom_point(aes(y = Total_Vehicles / 1000000), size = 2) +
  labs(title = "Total Households vs Total Vehicles (2021-2046)",
       y = "Millions",
       x = "Year",
       color = "Metric") +
  scale_color_manual(values = c("Households" = "blue", "Vehicles" = "red")) +
  theme_minimal()

# Plot 2: Vehicles per Household trend
p2 <- ggplot(national_summary, aes(x = Year, y = Avg_Vehicles_per_HH)) +
  geom_line(size = 1.5, color = "darkgreen") +
  geom_point(size = 3, color = "darkgreen") +
  labs(title = "Average Vehicles per Household (2021-2046)",
       y = "Vehicles per Household",
       x = "Year") +
  theme_minimal()

# Plot 3: Vehicle ownership composition

p3 <- national_summary %>%
  dplyr::select(Year, Prop_0_vehicles, Prop_1_vehicle, Prop_2_vehicles, Prop_3plus_vehicles) %>%
  pivot_longer(cols = -Year, names_to = "Vehicle_Count", values_to = "Proportion") %>%
  mutate(Vehicle_Count = factor(Vehicle_Count, 
                                levels = c("Prop_3plus_vehicles", "Prop_2_vehicles", 
                                           "Prop_1_vehicle", "Prop_0_vehicles"))) %>%
  ggplot(aes(x = Year, y = Proportion, fill = Vehicle_Count)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(
    values = c("Prop_0_vehicles" = "#2E86AB", "Prop_1_vehicle" = "#A23B72",
               "Prop_2_vehicles" = "#F18F01", "Prop_3plus_vehicles" = "#C73E1D"),
    labels = c("3+ vehicles", "2 vehicles", "1 vehicle", "0 vehicles")
  ) +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  labs(title = "Vehicle Ownership Distribution", y = "Proportion of Households", fill = "")


# Plot 4: Growth Rates
p4 <- national_summary %>%
  filter(!is.na(HH_Growth_Rate)) %>%
  dplyr::select(Year, HH_Growth_Rate, Vehicle_Growth_Rate, VPH_Growth_Rate) %>%
  pivot_longer(cols = -Year, names_to = "Growth_Type", values_to = "Rate") %>%
  ggplot(aes(x = Year, y = Rate, color = Growth_Type)) +
  geom_line(size = 1.5) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("HH_Growth_Rate" = "#2E86AB", "Vehicle_Growth_Rate" = "#A23B72", 
               "VPH_Growth_Rate" = "#F18F01"),
    labels = c("Households", "Vehicles", "Vehicles per HH")
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(title = "Annual Growth Rates", y = "Growth Rate", color = "")

# Combine all plots
(p1 + p2) / (p3 + p4) +
  plot_annotation(title = "Vehicle Ownership Trends: 2021-2046 Projection",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))


# ---------------------------
# COMPLETE MODEL VALIDATION WITH 2024 DATA
# ---------------------------

# First, let's make sure we have all the prediction methods defined
# and then validate them with your 2024 data
# Install if not already installed
if (!require(arrow)) install.packages("arrow")
library(arrow)
# Define variables first
stock_model <- read_parquet("C:/Users/fzzb134/OneDrive - University of Leeds/Infuze/R-programming-Infuze/R-programming_Updated analysis/New-updated_data/Excell_files/join_final_complete.parquet", stringsAsFactors = FALSE)%>%
  filter(Data_Year==2024,Vehicle_Age <=30) %>% rename(Fleet_Count=Vehicle_Stock_GB)
stock_2024_total_fleet<- stock_model%>%summarise(sum(Fleet_Count))
print(stock_2024_total_fleet)
total_vehicles_2024 <- stock_2024_total_fleet%>% pull() # # based on current data fleet from C:/Users/fzzb134/OneDrive - University of Leeds/Infuze/R-programming-Infuze/R-programming_Updated analysis/New-updated_data/Stock_model/New Car Classfications (GB)(recovered).R
total_households_2024 <- 28600000

actual_2024 <- data.frame(
  Year = 2024,
  Total_Households = total_households_2024,
  Total_Vehicles = total_vehicles_2024,
  Avg_Vehicles_per_HH = total_vehicles_2024 / total_households_2024
)

print(actual_2024)

cat("ACTUAL 2024 DATA:\n")
print(actual_2024)

# METHOD 1: SPLINE INTERPOLATION
spline_predictions <- function() {
  known_years <- national_summary$Year
  all_years <- 2021:2037
  
  spline_pred <- data.frame(Year = all_years)
  
  # Cubic spline interpolation
  spline_pred$Total_Households <- spline(known_years, national_summary$Total_Households, xout = all_years)$y
  spline_pred$Total_Vehicles <- spline(known_years, national_summary$Total_Vehicles, xout = all_years)$y
  spline_pred$Avg_Vehicles_per_HH <- spline_pred$Total_Vehicles / spline_pred$Total_Households
  
  return(spline_pred)
}

# METHOD 2: LINEAR INTERPOLATION
linear_interpolation_predictions <- function() {
  known_years <- national_summary$Year
  all_years <- 2021:2037
  
  linear_pred <- data.frame(Year = all_years)
  
  # Linear interpolation
  linear_pred$Total_Households <- approx(known_years, national_summary$Total_Households, xout = all_years)$y
  linear_pred$Total_Vehicles <- approx(known_years, national_summary$Total_Vehicles, xout = all_years)$y
  linear_pred$Avg_Vehicles_per_HH <- linear_pred$Total_Vehicles / linear_pred$Total_Households
  
  return(linear_pred)
}

# METHOD 3: POLYNOMIAL REGRESSION
polynomial_predictions <- function() {
  known_years <- national_summary$Year
  all_years <- 2021:2037
  
  poly_pred <- data.frame(Year = all_years)
  
  # Convert years to numeric sequence
  years_numeric <- known_years - 2020
  
  # Quadratic model for households
  hh_model <- lm(Total_Households ~ poly(years_numeric, 2), data = national_summary)
  poly_pred$Total_Households <- predict(hh_model, newdata = data.frame(years_numeric = all_years - 2020))
  
  # Quadratic model for vehicles
  veh_model <- lm(Total_Vehicles ~ poly(years_numeric, 2), data = national_summary)
  poly_pred$Total_Vehicles <- predict(veh_model, newdata = data.frame(years_numeric = all_years - 2020))
  
  poly_pred$Avg_Vehicles_per_HH <- poly_pred$Total_Vehicles / poly_pred$Total_Households
  
  return(poly_pred)
}

# METHOD 4: SIMPLE GROWTH RATE (Your original method)
simple_growth_predictions <- function() {
  # Calculate average annual growth rates
  growth_rates <- data.frame(
    Year = national_summary$Year[-1],
    HH_growth = diff(national_summary$Total_Households) / head(national_summary$Total_Households, -1),
    Vehicle_growth = diff(national_summary$Total_Vehicles) / head(national_summary$Total_Vehicles, -1)
  )
  
  avg_hh_growth <- mean(growth_rates$HH_growth, na.rm = TRUE)
  avg_veh_growth <- mean(growth_rates$Vehicle_growth, na.rm = TRUE)
  
  # Start from 2021
  last_known <- national_summary %>% filter(Year == 2021)
  predictions <- data.frame(Year = 2022:2037)
  
  predictions$Total_Households <- last_known$Total_Households * (1 + avg_hh_growth)^(predictions$Year - 2021)
  predictions$Total_Vehicles <- last_known$Total_Vehicles * (1 + avg_veh_growth)^(predictions$Year - 2021)
  predictions$Avg_Vehicles_per_HH <- predictions$Total_Vehicles / predictions$Total_Households
  
  return(predictions)
}

# METHOD 5: GROWTH DECAY (More realistic long-term)
growth_decay_predictions <- function() {
  # Calculate growth rates
  growth_rates <- data.frame(
    Year = national_summary$Year[-1],
    HH_growth = diff(national_summary$Total_Households) / head(national_summary$Total_Households, -1),
    Vehicle_growth = diff(national_summary$Total_Vehicles) / head(national_summary$Total_Vehicles, -1)
  )
  
  avg_hh_growth <- mean(growth_rates$HH_growth, na.rm = TRUE)
  avg_veh_growth <- mean(growth_rates$Vehicle_growth, na.rm = TRUE)
  
  # Start from 2021
  last_known <- national_summary %>% filter(Year == 2021)
  predictions <- data.frame(Year = 2022:2037)
  
  # Initialize
  current_hh <- last_known$Total_Households
  current_veh <- last_known$Total_Vehicles
  
  hh_predictions <- c()
  veh_predictions <- c()
  
  for (i in 1:nrow(predictions)) {
    year <- predictions$Year[i]
    
    # Growth rate decays each year
    years_from_base <- year - 2021
    decay_factor <- 0.98^(years_from_base)
    
    hh_growth <- avg_hh_growth * decay_factor
    veh_growth <- avg_veh_growth * decay_factor
    
    current_hh <- current_hh * (1 + hh_growth)
    current_veh <- current_veh * (1 + veh_growth)
    
    hh_predictions <- c(hh_predictions, current_hh)
    veh_predictions <- c(veh_predictions, current_veh)
  }
  
  predictions$Total_Households <- hh_predictions
  predictions$Total_Vehicles <- veh_predictions
  predictions$Avg_Vehicles_per_HH <- veh_predictions / hh_predictions
  
  return(predictions)
}

# NOW GENERATE ALL PREDICTIONS
cat("Generating predictions using all methods...\n")

spline_pred <- spline_predictions()
linear_pred <- linear_interpolation_predictions()
poly_pred <- polynomial_predictions()
simple_growth_pred <- simple_growth_predictions()
growth_decay_pred <- growth_decay_predictions()

# FUNCTION TO CALCULATE ACCURACY METRICS
calculate_accuracy <- function(predicted, actual, method_name) {
  # Check if we have 2024 prediction
  pred_2024 <- predicted %>% filter(Year == 2024)
  
  if(nrow(pred_2024) == 0) {
    return(NULL)  # Skip if no 2024 prediction
  }
  
  # Percentage errors
  hh_error_pct <- (pred_2024$Total_Households - actual$Total_Households) / actual$Total_Households * 100
  veh_error_pct <- (pred_2024$Total_Vehicles - actual$Total_Vehicles) / actual$Total_Vehicles * 100
  vph_error_pct <- (pred_2024$Avg_Vehicles_per_HH - actual$Avg_Vehicles_per_HH) / actual$Avg_Vehicles_per_HH * 100
  
  # Absolute errors
  hh_error_abs <- abs(pred_2024$Total_Households - actual$Total_Households)
  veh_error_abs <- abs(pred_2024$Total_Vehicles - actual$Total_Vehicles)
  
  data.frame(
    Method = method_name,
    HH_Error_Pct = round(hh_error_pct, 2),
    Vehicle_Error_Pct = round(veh_error_pct, 2),
    VPH_Error_Pct = round(vph_error_pct, 2),
    HH_Error_Abs = round(hh_error_abs),
    Vehicle_Error_Abs = round(veh_error_abs),
    HH_Predicted = round(pred_2024$Total_Households),
    Vehicles_Predicted = round(pred_2024$Total_Vehicles),
    VPH_Predicted = round(pred_2024$Avg_Vehicles_per_HH, 3)
  )
}

# VALIDATE ALL METHODS
cat("\n=== MODEL VALIDATION RESULTS ===\n")

validation_results <- data.frame()

# Test each method
methods <- list(
  list(pred = spline_pred, name = "Spline Interpolation"),
  list(pred = linear_pred, name = "Linear Interpolation"), 
  list(pred = poly_pred, name = "Polynomial Regression"),
  list(pred = simple_growth_pred, name = "Simple Growth Rate"),
  list(pred = growth_decay_pred, name = "Growth Decay")
)

for(method in methods) {
  result <- calculate_accuracy(method$pred, actual_2024, method$name)
  if(!is.null(result)) {
    validation_results <- rbind(validation_results, result)
  }
}

# Display results
print(validation_results)

# Find the best method
best_method <- validation_results %>%
  mutate(
    # Use weighted scoring that prioritizes both metrics appropriately
    Overall_Score = (abs(HH_Error_Pct) * 0.6 + abs(Vehicle_Error_Pct) * 0.4),
    # Alternative: Use RMSE-like approach
    RMSE_Score = sqrt(HH_Error_Pct^2 + Vehicle_Error_Pct^2)
  ) %>%
  arrange(Overall_Score) %>%  # Use the most balanced metric
  slice(1)

cat("\n🏆 BEST PERFORMING METHOD (IMPROVED SELECTION):\n")
print(best_method[, c("Method", "HH_Error_Pct", "Vehicle_Error_Pct", "Overall_Score")])

# Visualization
p_validation <- ggplot(validation_results, aes(x = reorder(Method, HH_Error_Pct), y = HH_Error_Pct)) +
  geom_col(aes(fill = abs(HH_Error_Pct)), alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(label = paste0(round(HH_Error_Pct, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Model Validation: Household Prediction Error for 2024",
       subtitle = paste("Actual 2024:", round(actual_2024$Total_Households/1e6, 2), "million households"),
       x = "Forecasting Method", 
       y = "Prediction Error (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradient(low = "green", high = "red", guide = "none")

print(p_validation)

# DYNAMIC BEST METHOD PREDICTION
# Use the best method identified from validation

# Get the best method name from validation
best_method_name <- best_method$Method

cat("🎯 USING BEST METHOD:", best_method_name, "\n")

# Function to get predictions from the best method
get_best_method_predictions <- function(years_range) {
  switch(best_method_name,
         "Spline Interpolation" = {
           pred <- data.frame(Year = years_range)
           pred$Total_Households <- spline(national_summary$Year, national_summary$Total_Households, xout = years_range)$y
           pred$Total_Vehicles <- spline(national_summary$Year, national_summary$Total_Vehicles, xout = years_range)$y
           pred
         },
         "Linear Interpolation" = {
           pred <- data.frame(Year = years_range)
           pred$Total_Households <- approx(national_summary$Year, national_summary$Total_Households, xout = years_range)$y
           pred$Total_Vehicles <- approx(national_summary$Year, national_summary$Total_Vehicles, xout = years_range)$y
           pred
         },
         "Polynomial Regression" = {
           # Use your existing polynomial function
           pred <- data.frame(Year = years_range)
           years_numeric <- national_summary$Year - 2020
           target_years_numeric <- years_range - 2020
           
           hh_model <- lm(Total_Households ~ poly(years_numeric, 2), data = national_summary)
           veh_model <- lm(Total_Vehicles ~ poly(years_numeric, 2), data = national_summary)
           
           pred$Total_Households <- predict(hh_model, newdata = data.frame(years_numeric = target_years_numeric))
           pred$Total_Vehicles <- predict(veh_model, newdata = data.frame(years_numeric = target_years_numeric))
           pred
         },
         "Simple Growth Rate" = {
           # Recreate simple growth predictions for any year range
           growth_rates <- data.frame(
             Year = national_summary$Year[-1],
             HH_growth = diff(national_summary$Total_Households) / head(national_summary$Total_Households, -1),
             Vehicle_growth = diff(national_summary$Total_Vehicles) / head(national_summary$Total_Vehicles, -1)
           )
           
           avg_hh_growth <- mean(growth_rates$HH_growth, na.rm = TRUE)
           avg_veh_growth <- mean(growth_rates$Vehicle_growth, na.rm = TRUE)
           
           last_known <- national_summary %>% filter(Year == 2021)
           pred <- data.frame(Year = years_range)
           
           pred$Total_Households <- last_known$Total_Households * (1 + avg_hh_growth)^(pred$Year - 2021)
           pred$Total_Vehicles <- last_known$Total_Vehicles * (1 + avg_veh_growth)^(pred$Year - 2021)
           pred
         },
         "Growth Decay" = {
           # Recreate growth decay predictions for any year range
           growth_rates <- data.frame(
             Year = national_summary$Year[-1],
             HH_growth = diff(national_summary$Total_Households) / head(national_summary$Total_Households, -1),
             Vehicle_growth = diff(national_summary$Total_Vehicles) / head(national_summary$Total_Vehicles, -1)
           )
           
           avg_hh_growth <- mean(growth_rates$HH_growth, na.rm = TRUE)
           avg_veh_growth <- mean(growth_rates$Vehicle_growth, na.rm = TRUE)
           
           last_known <- national_summary %>% filter(Year == 2021)
           pred <- data.frame(Year = years_range)
           
           # Initialize
           hh_predictions <- c()
           veh_predictions <- c()
           
           for (i in 1:length(years_range)) {
             year <- years_range[i]
             
             # Growth rate decays each year
             years_from_base <- year - 2021
             decay_factor <- 0.98^(years_from_base)
             
             hh_growth <- avg_hh_growth * decay_factor
             veh_growth <- avg_veh_growth * decay_factor
             
             if (i == 1) {
               current_hh <- last_known$Total_Households
               current_veh <- last_known$Total_Vehicles
             }
             
             current_hh <- current_hh * (1 + hh_growth)
             current_veh <- current_veh * (1 + veh_growth)
             
             hh_predictions <- c(hh_predictions, current_hh)
             veh_predictions <- c(veh_predictions, current_veh)
           }
           
           pred$Total_Households <- hh_predictions
           pred$Total_Vehicles <- veh_predictions
           pred
         }
  )
}

# Generate complete estimates using the best method
all_years <- 2021:2037

complete_best_estimates <- get_best_method_predictions(all_years) %>%
  mutate(Avg_Vehicles_per_HH = Total_Vehicles / Total_Households) %>%
  mutate(across(where(is.numeric), ~ round(., 0)))

# Filter for 2024
estimate_2024_best <- complete_best_estimates %>% filter(Year == 2024)

cat("\n=== 2024 ESTIMATE USING BEST METHOD (", best_method_name, ") ===\n", sep = "")
print(estimate_2024_best)

# Display complete estimates from 2021-2037
cat("\n=== COMPLETE YEARLY ESTIMATES (2021-2037) USING BEST METHOD (", best_method_name, ") ===\n", sep = "")
print(complete_best_estimates %>% filter(Year >= 2021 & Year <= 2037))

# Compare with actual 2024 data
cat("\n=== COMPARISON WITH ACTUAL 2024 DATA ===\n")
comparison_2024 <- data.frame(
  Metric = c("Total_Households", "Total_Vehicles", "Avg_Vehicles_per_HH"),
  Actual = c(actual_2024$Total_Households, actual_2024$Total_Vehicles, actual_2024$Avg_Vehicles_per_HH),
  Predicted = c(estimate_2024_best$Total_Households, estimate_2024_best$Total_Vehicles, estimate_2024_best$Avg_Vehicles_per_HH),
  Error_Pct = c(
    (estimate_2024_best$Total_Households - actual_2024$Total_Households) / actual_2024$Total_Households * 100,
    (estimate_2024_best$Total_Vehicles - actual_2024$Total_Vehicles) / actual_2024$Total_Vehicles * 100,
    (estimate_2024_best$Avg_Vehicles_per_HH - actual_2024$Avg_Vehicles_per_HH) / actual_2024$Avg_Vehicles_per_HH * 100
  )
) %>% mutate(across(where(is.numeric), ~ round(., 2)))

print(comparison_2024)



library(ggplot2)

ggplot() +
  geom_line(data = national_summary, aes(x = Year, y = Total_Vehicles), color = "blue", size = 1.2) +
  geom_point(data = actual_2024, aes(x = Year, y = Total_Vehicles), color = "black", size = 3) +
  geom_line(data = complete_best_estimates, aes(x = Year, y = Total_Vehicles), color = "red", size = 1, linetype = "dashed") +
  labs(
    #title = "Vehicle Forecast vs Actual (2021–2037)",
    #subtitle = paste("Best Model:", best_method_name),
    x = "Year", y = "Total Vehicles",
    caption = "Blue: Observed | Black: Actual 2024 | Red dashed: Forecast") +
  theme_minimal(base_size = 13)

# Enhanced accuracy calculation for all methods
calculate_comprehensive_accuracy <- function(predicted, actual, method_name) {
  pred_2024 <- predicted %>% filter(Year == 2024)
  if(nrow(pred_2024) == 0) return(NULL)
  
  vehicle_rmse <- sqrt(mean((pred_2024$Total_Vehicles - actual$Total_Vehicles)^2))
  vehicle_mape <- mean(abs((pred_2024$Total_Vehicles - actual$Total_Vehicles) / actual$Total_Vehicles)) * 100
  
  data.frame(
    Method = method_name,
    RMSE = round(vehicle_rmse, 0),
    MAPE = round(vehicle_mape, 2),
    Vehicle_Error_Pct = round((pred_2024$Total_Vehicles - actual$Total_Vehicles) / actual$Total_Vehicles * 100, 2)
  )
}

# Calculate comprehensive results
comprehensive_results <- do.call(rbind, lapply(methods, function(m) 
  calculate_comprehensive_accuracy(m$pred, actual_2024, m$name)
))

cat("\n=== COMPREHENSIVE ACCURACY METRICS ===\n")
print(comprehensive_results)

# Clear winner announcement
cat("\n")
cat("🎯" , rep("=", 50), "🎯\n")
cat("🏆 FINAL VERDICT: ", best_method$Method, " IS THE BEST METHOD! 🏆\n")
cat("📊 Household Error: ", best_method$HH_Error_Pct, "% | Vehicle Error: ", best_method$Vehicle_Error_Pct, "%\n")
cat("🎯" , rep("=", 50), "🎯\n")



#--------------------------------------------------------------------------------------------------
#fleet_after_add_filter: this number taken from the New Car Classification (GB)_result_orginal file.  
#--------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(forecast)

#==============================================================
# 1. Your real historical fleet
#==============================================================

joint_final_model<- stock_model
joint_final_model <-joint_final_model %>% filter(Vehicle_Age <=30)

# Extract fleet totals from your data
your_fleet_data <- joint_final_model %>%
  group_by(Data_Year) %>%
  summarise(
    Your_Fleet = sum(Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Year = Data_Year) %>%
  arrange(Year) %>%
  as.data.frame()

# Print the result
cat("=== AUTOMATICALLY EXTRACTED FLEET DATA ===\n")
print(your_fleet_data)

#==============================================================
# # 2. Core model forecast (your existing file)
# #==============================================================
# Make sure complete_best_estimates exists
if(!exists("complete_best_estimates")) {
  # If not, create a simple version for testing
  complete_best_estimates <- data.frame(
    Year = 2014:2037,
    Total_Vehicles = seq(32000000, 37000000, length.out = 21)
  )
}

model_forecast <- complete_best_estimates %>%
  filter(Year >= 2014 & Year <= 2037) %>%
  select(Year, Model_Fleet = Total_Vehicles)
# 
#==============================================================
# CORRECTED APPROACH: USE CORE MODEL'S GROWTH TREND ON YOUR DATA
#==============================================================

cat("\n=== APPLYING CORE MODEL'S GROWTH TREND TO YOUR DATA ===\n")

# Your actual 2024 data
your_2024 <- your_fleet_data$Your_Fleet[your_fleet_data$Year == 2024]
core_2024 <- model_forecast$Model_Fleet[model_forecast$Year == 2024]

cat("Your 2024 fleet:", format(your_2024, big.mark = ","), "vehicles\n")
cat("Core model 2024:", format(core_2024, big.mark = ","), "vehicles\n")

# Calculate core model's growth factors
core_growth_factors <- model_forecast %>%
  filter(Year >= 2024) %>%
  arrange(Year) %>%
  mutate(
    Growth_Factor = Model_Fleet / lag(Model_Fleet)  # How much it grows each year
  ) %>%
  filter(!is.na(Growth_Factor))

cat("\nCore Model Growth Factors:\n")
print(core_growth_factors %>% select(Year, Model_Fleet, Growth_Factor))

# Apply the same growth factors to your data
your_forecast_core_trend <- data.frame(Year = 2024:2037)
your_forecast_core_trend$Total_Vehicles[1] <- your_2024  # Start with your actual 2024

# Apply core model's growth pattern year by year
for(i in 2:nrow(your_forecast_core_trend)) {
  year <- your_forecast_core_trend$Year[i]
  core_growth <- core_growth_factors$Growth_Factor[core_growth_factors$Year == year]
  your_forecast_core_trend$Total_Vehicles[i] <- round(
    your_forecast_core_trend$Total_Vehicles[i-1] * core_growth
  )
}

# Calculate growth rates
your_forecast_core_trend <- your_forecast_core_trend %>%
  mutate(
    Annual_Growth_Rate = c(NA, round(diff(Total_Vehicles) / head(Total_Vehicles, -1) * 100, 2)),
    Total_Vehicles_Millions = round(Total_Vehicles / 1e6, 3),
    Core_Model = model_forecast$Model_Fleet[model_forecast$Year >= 2024 & model_forecast$Year <= 2037],
    Difference_from_Core = Total_Vehicles - Core_Model
  )

cat("\n=== YOUR FORECAST USING CORE MODEL'S GROWTH TREND ===\n")
print(your_forecast_core_trend)

# Compare with core model
cat("\n=== COMPARISON: YOUR DATA VS CORE MODEL ===\n")
comparison_table <- your_forecast_core_trend %>%
  select(Year, Your_Forecast = Total_Vehicles, Core_Model, Difference_from_Core) %>%
  mutate(
    Your_Millions = round(Your_Forecast / 1e6, 2),
    Core_Millions = round(Core_Model / 1e6, 2),
    Gap_Percent = round((Difference_from_Core / Core_Model) * 100, 1)
  )

print(comparison_table)

# Summary statistics
cat("\n📊 SUMMARY STATISTICS:\n")
cat("• Your 2024 fleet:", format(your_2024, big.mark = ","), "vehicles\n")
cat("• Core model 2024:", format(core_2024, big.mark = ","), "vehicles\n")
cat("• Initial gap (2024):", round((your_2024 - core_2024) / core_2024 * 100, 1), "%\n")

final_2037_yours <- your_forecast_core_trend$Total_Vehicles[your_forecast_core_trend$Year == 2037]
final_2037_core <- model_forecast$Model_Fleet[model_forecast$Year == 2037]

cat("• Your 2037 forecast:", format(final_2037_yours, big.mark = ","), "vehicles\n")
cat("• Core model 2037:", format(final_2037_core, big.mark = ","), "vehicles\n")
cat("• Final gap (2037):", round((final_2037_yours - final_2037_core) / final_2037_core * 100, 1), "%\n")

total_growth_yours <- (final_2037_yours - your_2024) / your_2024 * 100
total_growth_core <- (final_2037_core - core_2024) / core_2024 * 100

cat("• Your total growth 2024-2037:", round(total_growth_yours, 1), "%\n")
cat("• Core model total growth 2024-2037:", round(total_growth_core, 1), "%\n")
cat("• Growth difference:", round(total_growth_yours - total_growth_core, 1), "%\n")

# Visualization
plot_comparison <- your_forecast_core_trend %>%
  select(Year, Your_Forecast = Total_Vehicles, Core_Model) %>%
  pivot_longer(cols = -Year, names_to = "Series", values_to = "Vehicles") %>%
  mutate(Vehicles_Millions = Vehicles / 1e6)

# Fixed plot with correct color mapping
p <- ggplot(plot_comparison, aes(x = Year, y = Vehicles_Millions, 
                                 color = Series, linetype = Series)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  scale_color_manual(
    values = c("Core_Model" = "blue", 
               "Your_Forecast" = "red"),
    labels = c("Core_Model" = "Core Model", 
               "Your_Forecast" = "Forecast")
  ) +
  scale_linetype_manual(
    values = c("Core_Model" = "dashed",
               "Your_Forecast" = "solid"),
    labels = c("Core_Model" = "Core Model", 
               "Your_Forecast" = "Forecast")
  ) +
  labs(
    # title = "Forecast Using Core Model's Growth Trend",
    #subtitle = "Same annual growth rates, different starting points",
    y = "Fleet Size (Millions)",
    x = "Year",
    color = "Series",
    linetype = "Series"
  ) +
  theme(legend.position = "bottom")

print(p)

# # Save final forecast
# #setwd("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\R-programming-Infuze\\R-programming_Updated analysis\\New-updated_data\\Excell_files")
# #write.csv(your_forecast_core_trend, "your_forecast_core_trend_2024_2037.csv", row.names = FALSE)
# cat("\n✅ Forecast saved to 'your_forecast_core_trend_2024_2037.csv'\n")
# 
# # Final clean output
# cat("\n🎯 FINAL FORECAST USING CORE MODEL'S GROWTH TREND:\n")
# final_output <- your_forecast_core_trend %>%
#   select(Year, Total_Vehicles, Annual_Growth_Rate, Total_Vehicles_Millions)
# 
# print(final_output)






#========================
# Retirement rate 
#=======================
library(tidyverse)
library(survival)
library(survminer)
library(broom)
library(data.table)
library(R.utils)
# ========================
# Load stock model data 
# ========================
# Install if not already installed
if (!require(arrow)) install.packages("arrow")
library(arrow)
data <- read_parquet("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\R-programming-Infuze\\R-programming_Updated analysis\\New-updated_data\\Excell_files\\join_final_complete.parquet", stringsAsFactors = FALSE)
#======================================================================================================

vehicel_numer <- data%>%
  group_by(Data_Year)%>%
  summarise(fleet = sum(Vehicle_Stock_GB, na.rm = TRUE), .groups = "drop")%>%
  print


# Create fuel by region percentage table
fuel_region_table <- data %>% filter(Data_Year==2024)%>%
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

# Print the table
cat("\n=== FUEL TYPE PERCENTAGE BY REGION ===\n")
print(fuel_region_table, n = Inf)


# Also create a version with vehicle counts for reference
fuel_region_counts <- data %>%filter(Data_Year==2024)%>%
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



# Overall fuel mix across all regions
overall_fuel_mix <- data %>%filter(Data_Year==2024)%>%
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


# Simple validation table with ACEA for the new vehicle (source here :https://www.acea.auto/fact/fact-sheet-eu-uk-vehicle-trade-2/)
validation_comparison <- data %>%
  filter(Data_Year == 2024,Vehicle_Age==0) %>%
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


bev_country_2024 <- data %>% filter(Fuel_Group=="BEV", Data_Year==2024) %>%
  group_by(Region, Origin) %>% summarise(Reg=sum(Vehicle_Stock_GB), .groups="drop") %>%
  mutate(Share=round(100*Reg/sum(Reg),1)) %>% arrange(desc(Reg))
cat("\n=== NEW BEV ORIGIN BY COUNTRY (2024) — paper table ===\n")
print(bev_country_2024, n=Inf)

# ---------------------------------------------------------------------------
# PLOT: new-BEV origin region trend
# ---------------------------------------------------------------------------
# Create the share table with a total row
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
  arrange(desc(Region == "Total"))  # Put Total at bottom

# Add a total row at the bottom
total_row <- share_table %>%
  summarise(
    across(
      where(is.numeric),
      ~ round(sum(.x, na.rm=TRUE),1)
    )
  ) %>%
  mutate(Region="TOTAL")

share_table_with_total <- bind_rows(share_table, total_row)

# Print the table
cat("\n=== REGIONAL SHARE OF NEW REGISTRATIONS BY YEAR (%)\n")
print(share_table_with_total, n = Inf)

# Option 1: Create a table with Region + Fuel_Group as rows
share_table_fuel <- data %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year, Region, Fuel_Group) %>%
  summarise(Reg = sum(Vehicle_Stock_GB, na.rm = TRUE), .groups = "drop") %>%
  group_by(Data_Year) %>%
  mutate(Share = round(100 * Reg / sum(Reg), 1)) %>%
  ungroup() %>%
  # Create a combined row identifier
  mutate(Row_Label = paste(Region, Fuel_Group, sep = " - ")) %>%
  select(Data_Year, Row_Label, Share) %>%
  pivot_wider(
    names_from = Data_Year,
    values_from = Share,
    values_fill = 0
  ) %>%
  arrange(Row_Label)

# Add total row
total_row <- share_table_fuel %>%
  summarise(
    across(where(is.numeric), ~ round(sum(.x, na.rm = TRUE), 1))
  ) %>%
  mutate(Row_Label = "TOTAL")

share_table_with_Fuel_total <- bind_rows(share_table_fuel, total_row)

cat("\n=== REGIONAL SHARE BY FUEL TYPE OF NEW REGISTRATIONS (%)\n")
print(share_table_with_Fuel_total, n = Inf)


bev_new_region <- data %>% filter(Fuel_Group=="BEV",Vehicle_Age==0) %>%
  group_by(Data_Year, Region) %>% summarise(Reg=sum(Vehicle_Stock_GB), .groups="drop") %>%
  group_by(Data_Year) %>% mutate(Share=round(100*Reg/sum(Reg),1)) %>% ungroup()
print(bev_new_region%>% filter(Data_Year==2024))

p <- bev_new_region %>% filter(Region %in% c("EU","UK","China","Korea","Japan")) %>%
  ggplot(aes(Data_Year, Share, colour=Region)) +
  geom_line(linewidth=1.2) + geom_point(size=2.4) +
  scale_x_continuous(breaks=seq(2014,2024,2)) +
  labs(title="New BEV registrations by manufacturing region",
       subtitle="China's share of new BEVs (27.6% in 2024) far exceeds its all-car share",
       x="Build year cohort", y="Share of new BEV registrations (%)",
       caption="UK vehicle licensing microdata; origin keyed on year of manufacture") +
  theme_minimal(base_size=12) + theme(legend.position="bottom")
print(p)

cat("\n2024 new-BEV split: China 27.6% / EU 51.2% / UK 3.9% (consistent with ACEA all-car China 14%).\n")

# Option 1: Stacked area plot (wavy/ribbon style)
p_wavy <- bev_new_region %>% 
  filter(Region %in% c("EU","UK","China","Korea","Japan")) %>%
  ggplot(aes(x = Data_Year, y = Share, fill = Region, colour = Region)) +
  geom_area(alpha = 0.6, position = "stack") +
  # geom_line(aes(group = Region), size = 0.8) +
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
    #title = "New BEV registrations by manufacturing region",
    # subtitle = "China's share of new BEVs (27.6% in 2024) far exceeds its all-car share",
    x = "Build year cohort", 
    y = "Share of new BEV registrations (%)")+
  #caption = "UK vehicle licensing microdata; origin keyed on year of manufacture") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "bottom")

print(p_wavy)
#===================================================================================================================================

data <- data %>% 
  rename(Fleet_Count = Vehicle_Stock_GB) %>%
  select(Fuel, Fuel_Group, Fleet_Count, Vehicle_Age, Data_Year, vehicle_class,engine_size,engine_size_liters,Avg_Mileage_Calibrated,Region,Origin,Avg_CO2,kerb_weight_kg,rdw_length_mm,rdw_width_mm,rdw_height_mm)


fleet_complete <- data%>% 
  filter(Vehicle_Age<=30)%>%
  # Ensure kerb_weight_kg is numeric
  mutate(kerb_weight_kg = as.numeric(kerb_weight_kg))
sum(fleet_complete$Fleet_Count)

# Total fleet preserved
fleet_2024_complete <- fleet_complete %>%
  filter(Data_Year==2024) %>%
  select(Vehicle_Age, Fuel_Group, Fleet_Count,engine_size,Avg_Mileage_Calibrated,Region,Origin,vehicle_class,Avg_CO2,kerb_weight_kg,rdw_length_mm,rdw_width_mm,rdw_height_mm)%>%
  mutate(engine_size_liters = engine_size / 1000)
print(fleet_2024_complete)
sum(fleet_2024_complete$Fleet_Count)
names(fleet_2024_complete)


summary(fleet_2024_complete $Avg_Mileage_Calibrated)
#-----------------------------------------------------------------------------------------------


# ============================================
# IMPUTE MISSING DIMENSIONS FOR HEV (AND ANY OTHER GAP) — 3-TIER FALLBACK
# ============================================

# Tier 1: Fuel_Group + vehicle_class (most specific)
dimension_impute <- fleet_2024_complete %>%
  filter(!is.na(rdw_length_mm) & !is.na(rdw_width_mm) & !is.na(rdw_height_mm)) %>%
  group_by(Fuel_Group, vehicle_class) %>%
  summarise(
    median_length_mm = median(rdw_length_mm, na.rm = TRUE),
    median_width_mm  = median(rdw_width_mm, na.rm = TRUE),
    median_height_mm = median(rdw_height_mm, na.rm = TRUE),
    .groups = "drop"
  )

# Tier 2: Fuel_Group only (fallback)
fuel_dimension_impute <- fleet_2024_complete %>%
  filter(!is.na(rdw_length_mm) & !is.na(rdw_width_mm) & !is.na(rdw_height_mm)) %>%
  group_by(Fuel_Group) %>%
  summarise(
    fuel_median_length_mm = median(rdw_length_mm, na.rm = TRUE),
    fuel_median_width_mm  = median(rdw_width_mm, na.rm = TRUE),
    fuel_median_height_mm = median(rdw_height_mm, na.rm = TRUE),
    .groups = "drop"
  )

# NEW — Tier 3: vehicle_class only, fuel-agnostic (final fallback for HEV)
class_dimension_impute <- fleet_2024_complete %>%
  filter(!is.na(rdw_length_mm) & !is.na(rdw_width_mm) & !is.na(rdw_height_mm)) %>%
  group_by(vehicle_class) %>%
  summarise(
    class_median_length_mm = median(rdw_length_mm, na.rm = TRUE),
    class_median_width_mm  = median(rdw_width_mm, na.rm = TRUE),
    class_median_height_mm = median(rdw_height_mm, na.rm = TRUE),
    .groups = "drop"
  )



# Extract pure size category, independent of fuel type
fleet_2024_complete <- fleet_2024_complete %>%
  mutate(size_category = case_when(
    str_detect(vehicle_class, "Very Large") ~ "Very Large",  # check this BEFORE "Large"
    str_detect(vehicle_class, "Large")      ~ "Large",
    str_detect(vehicle_class, "Medium")     ~ "Medium",
    str_detect(vehicle_class, "Small")      ~ "Small",
    TRUE ~ NA_character_
  ))

# Confirm every row got a size_category (should be 0 unmatched)
fleet_2024_complete %>% filter(is.na(size_category)) %>% nrow()

# NEW Tier 3: pure size, no fuel type — this is the one that actually escapes the HEV gap
class_dimension_impute <- fleet_2024_complete %>%
  filter(!is.na(rdw_length_mm) & !is.na(rdw_width_mm) & !is.na(rdw_height_mm)) %>%
  group_by(size_category) %>%
  summarise(
    class_median_length_mm = median(rdw_length_mm, na.rm = TRUE),
    class_median_width_mm  = median(rdw_width_mm, na.rm = TRUE),
    class_median_height_mm = median(rdw_height_mm, na.rm = TRUE),
    .groups = "drop"
  )

print(class_dimension_impute)  # should show 4 rows: Small/Medium/Large/Very Large, all non-NA

fleet_2024_complete <- fleet_2024_complete %>%
  left_join(dimension_impute, by = c("Fuel_Group", "vehicle_class")) %>%
  left_join(fuel_dimension_impute, by = "Fuel_Group") %>%
  left_join(class_dimension_impute, by = "size_category") %>%   # join on size_category, not vehicle_class
  mutate(
    rdw_length_mm = coalesce(rdw_length_mm, median_length_mm, fuel_median_length_mm, class_median_length_mm),
    rdw_width_mm  = coalesce(rdw_width_mm,  median_width_mm,  fuel_median_width_mm,  class_median_width_mm),
    rdw_height_mm = coalesce(rdw_height_mm, median_height_mm, fuel_median_height_mm, class_median_height_mm)
  ) %>%
  select(-median_length_mm, -median_width_mm, -median_height_mm,
         -fuel_median_length_mm, -fuel_median_width_mm, -fuel_median_height_mm,
         -class_median_length_mm, -class_median_width_mm, -class_median_height_mm)

# Re-verify
hev_dimension_check <- fleet_2024_complete %>%
  filter(Fuel_Group == "HEV") %>%
  summarise(total = n(), has_length = sum(!is.na(rdw_length_mm)),
            pct_complete = round(100 * has_length / total, 1))
print(hev_dimension_check)
#-------------------------------------------
#Step 1: Build a Weight Model from 2024 Data
#-------------------------------------------
# Prepare training data (2024 vehicles with complete dimensions)
weight_training <- fleet_2024_complete %>%
  filter(!is.na(kerb_weight_kg), !is.na(rdw_length_mm), !is.na(rdw_width_mm), !is.na(rdw_height_mm)) %>%
  mutate(length_m = rdw_length_mm / 1000, width_m = rdw_width_mm / 1000, height_m = rdw_height_mm / 1000)

cat("\n=== FUEL GROUPS IN TRAINING DATA ===\n")
print(table(weight_training$Fuel_Group))   # HEV should now show a real, nonzero count

weight_model <- lm(kerb_weight_kg ~ length_m + width_m + height_m + Fuel_Group + vehicle_class,
                   data = weight_training)

cat("\n=== MODEL FACTOR LEVELS ===\n")
print(weight_model$xlevels$Fuel_Group)   # should now include "HEV"
summary(weight_model)
#---------------------------------------------------------
#Step 2: Project Future Dimensions Using T&E Growth Rates (https://www.transportenvironment.org/articles/ever-bigger-car-size-at-a-crossroads)
#---------------------------------------------------------


# Define growth rates from T&E report
growth_rates <- list(
  length_mm_per_year = 12,   # 1.2 cm = 12 mm
  width_mm_per_year = 5,     # 0.5 cm = 5 mm
  height_mm_per_year = 5     # 0.5 cm = 5 mm
)

# Function to project dimensions for new vehicles
project_dimensions <- function(base_length_mm, base_width_mm, base_height_mm, 
                               target_year, base_year = 2024) {
  years_ahead <- target_year - base_year
  data.frame(
    projected_length_mm = base_length_mm + growth_rates$length_mm_per_year * years_ahead,
    projected_width_mm = base_width_mm + growth_rates$width_mm_per_year * years_ahead,
    projected_height_mm = base_height_mm + growth_rates$height_mm_per_year * years_ahead
  )
}



# 1. Establish 2024 baseline dimensions per Fuel_Group x vehicle_class
#    (uses your already-imputed rdw_length/width/height, so HEV has real values too)
baseline_dimensions_2024 <- fleet_2024_complete %>%
  filter(!is.na(rdw_length_mm), !is.na(rdw_width_mm), !is.na(rdw_height_mm)) %>%
  group_by(Fuel_Group, vehicle_class) %>%
  summarise(
    base_length_mm = median(rdw_length_mm, na.rm = TRUE),
    base_width_mm  = median(rdw_width_mm, na.rm = TRUE),
    base_height_mm = median(rdw_height_mm, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Project dimensions forward for every combination, for every projection year
projected_dimensions_by_year <- baseline_dimensions_2024 %>%
  tidyr::crossing(Year = 2025:2037) %>%
  mutate(
    years_ahead = Year - 2024,
    proj_length_mm = base_length_mm + growth_rates$length_mm_per_year * years_ahead,
    proj_width_mm  = base_width_mm  + growth_rates$width_mm_per_year  * years_ahead,
    proj_height_mm = base_height_mm + growth_rates$height_mm_per_year * years_ahead
  )

# 3. Predict kerb weight for each projected year's dimensions, using your existing model
projected_dimensions_by_year <- projected_dimensions_by_year %>%
  mutate(
    length_m = proj_length_mm / 1000,
    width_m  = proj_width_mm / 1000,
    height_m = proj_height_mm / 1000,
    projected_kerb_weight_kg = predict(
      weight_model,
      newdata = data.frame(
        length_m = length_m, width_m = width_m, height_m = height_m,
        Fuel_Group = Fuel_Group, vehicle_class = vehicle_class
      )
    )
  )

# Quick sanity check: does BEV weight trend upward as expected across years?
projected_dimensions_by_year %>%
  filter(Fuel_Group == "BEV", vehicle_class == "Medium BEV") %>%
  select(Year, proj_length_mm, projected_kerb_weight_kg)


#####################################################################




data_2024 <- fleet_2024_complete %>%
  
  summarise(total = sum(Fleet_Count, na.rm=TRUE))
print(data_2024)


# Round and summarize in one chain
yearly_summary <- fleet_complete %>%
  group_by(Data_Year) %>%
  summarise(total = sum(Fleet_Count), .groups = "drop")

print(yearly_summary)



cat("\n=== 2024 FLEET COMPOSITION ===\n")
cat("Total vehicles:", format(sum(fleet_complete$Fleet_Count), big.mark = ","), "\n")
cat("Fuel types:", paste(unique(fleet_complete$Fuel_Group), collapse = ", "), "\n")
cat("Max age:", max(fleet_complete$Vehicle_Age), "\n")


# Quick checks for NA and "Other" in final data
sum(is.na(fleet_complete$Fleet_Count))
sum(fleet_complete$Fuel_Group == "Other", na.rm = TRUE)
sum(is.na(fleet_complete$Fuel_Group))
sum(is.na(fleet_complete$Vehicle_Age))
sum(is.na(fleet_complete$Data_Year))



# new sale 

# Check new sales (age 0 vehicles) by year from historical data
Newsale_check <- fleet_complete %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year) %>%
  summarise(Fleet_total = sum(Fleet_Count), .groups = "drop")  # Use .groups = "drop"

print(Newsale_check)

fleet_2024_complete %>%
  summarise(
    total = sum(Fleet_Count, na.rm = TRUE),
    missing = sum(Fleet_Count[is.na(kerb_weight_kg)], na.rm = TRUE),
    pct_missing = round(100 * missing / total, 1)
  )

# ============================================
# PART 2: CALCULATE SURVIVAL/RETIREMENT RATES BY AGE AND FUEL
# ============================================

# Calculate retirement rates from 2022 to 2023
retirement_rates_2022_2023 <- data %>%
  filter(Data_Year %in% c(2022, 2023), !is.na(Vehicle_Age), Fuel_Group != "Other") %>%
  group_by(Fuel, Fuel_Group, Vehicle_Age, Data_Year,vehicle_class) %>%
  summarise(Fleet_Count = sum(Fleet_Count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = c(Fuel, Fuel_Group, Vehicle_Age,vehicle_class),
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

# Calculate retirement rates from 2023 to 2024
retirement_rates_2023_2024 <- data %>%
  filter(Data_Year %in% c(2023, 2024), !is.na(Vehicle_Age), Fuel_Group != "Other") %>%
  group_by(Fuel, Fuel_Group, Vehicle_Age, Data_Year,vehicle_class) %>%
  summarise(Fleet_Count = sum(Fleet_Count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = c(Fuel, Fuel_Group, Vehicle_Age,vehicle_class),
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

# Combine and average retirement rates
retirement_table <- bind_rows(
  retirement_rates_2022_2023 %>% mutate(Period = "2022-2023"),
  retirement_rates_2023_2024 %>% mutate(Period = "2023-2024")
) %>%
  group_by(Vehicle_Age,Fuel_Group) %>%
  summarise(
    Retirement_Rate = mean(Retirement_Rate, na.rm = TRUE),
    Survival_Rate = 1 - mean(Retirement_Rate, na.rm = TRUE),
    Observations = n(),
    .groups = "drop"
  )%>% filter(Vehicle_Age <=40)


#--------------------------
# analysis 2022 as base year
#-------------------------

library(dplyr)
library(tidyr)

# CORRECTED VERSION - Group by Fuel_Group and exclude "Other"
fleet_age_year <- data %>%
  filter(Data_Year %in% c(2022, 2023)) %>%
  filter(Fuel_Group != "Other") %>%  # EXCLUDE "Other" fuel type
  group_by(Vehicle_Age, Fuel_Group, Data_Year) %>%  # ADD Fuel_Group to grouping
  summarise(
    Fleet = sum(Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(Vehicle_Age, Fuel_Group),  # Keep Fuel_Group as identifier
    names_from = Data_Year,
    values_from = Fleet,
    names_prefix = "Fleet_"
  ) %>%
  arrange(Fuel_Group, Vehicle_Age)

# Calculate survival rates by Fuel_Group
validation_total <- fleet_age_year %>%
  group_by(Fuel_Group) %>%  # Calculate within each fuel group
  mutate(
    Survival_Rate = lead(Fleet_2023) / Fleet_2022,
    # Filter out unreasonable rates
    Survival_Rate = ifelse(Survival_Rate < 0.5 | Survival_Rate > 1.05, NA, Survival_Rate)
  ) %>%
  filter(!is.na(Survival_Rate)) %>%
  ungroup()

# Cap rates at reasonable values
validation_total <- validation_total %>%
  mutate(
    Survival_Rate = pmin(pmax(Survival_Rate, 0.5), 0.999),  # Lower bound 0.5
    Retirement_Rate = 1 - Survival_Rate
  )

# Calculate retirement analysis BY FUEL GROUP
retirement_by_fuel <- validation_total %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Cumulative_Survival = cumprod(Survival_Rate),
    Retirement_Prob_At_Age = lag(Cumulative_Survival, default = 1) * Retirement_Rate,
    Cumulative_Retirement = cumsum(Retirement_Prob_At_Age)
  ) %>%
  ungroup()

# Also calculate overall (weighted by fleet size)
# First get fleet weights
fleet_weights <- data %>%
  filter(Data_Year == 2022, Fuel_Group != "Other") %>%
  group_by(Fuel_Group) %>%
  summarise(Total_Fleet = sum(Fleet_Count, na.rm = TRUE)) %>%
  mutate(Weight = Total_Fleet / sum(Total_Fleet))

# Calculate weighted average survival rates
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

# Calculate milestones by fuel group
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


cat("• 75% Retirement Age:", retirement_75, "\n")
cat("• 90% Retirement Age:", retirement_90, "\n")
cat("• Expected Retirement Age:", round(expected_retirement_age, 1), "\n")

# Visualize by fuel group
library(ggplot2)

ggplot(retirement_by_fuel, aes(x = Vehicle_Age, y = Cumulative_Survival, color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  labs(title = "Survival Curves by Fuel Group",
       x = "Vehicle Age (years)",
       y = "Survival Probability",
       color = "Fuel Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
# ============================================
# TRUE COHORT SURVIVAL ESTIMATION (CORRECTED)
# ============================================

library(dplyr)
library(tidyr)
library(ggplot2)

# -------------------------------------------------
# STEP 1: Aggregate fleet by Year × Age × Fuel
# -------------------------------------------------

survival_year <- fleet_complete %>%
  filter(Fuel_Group %in% c("Petrol","Diesel","HEV","PHEV","BEV" ),
         !is.na(Vehicle_Age)) %>%
  group_by(Fuel_Group, Data_Year, Vehicle_Age) %>%
  summarise(Fleet = sum(Fleet_Count, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(Fuel_Group, Data_Year, Vehicle_Age)

# -------------------------------------------------
# STEP 2: Create diagonal match (t,a) → (t+1,a+1)
# -------------------------------------------------

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

# -------------------------------------------------
# STEP 3: Compute TRUE annual survival & retirement
# -------------------------------------------------

survival_year <- survival_year %>%
  mutate(
    Annual_Survival = Fleet_next / Fleet,
    Annual_Survival = pmin(pmax(Annual_Survival, 0), 1),
    Annual_Retirement = 1 - Annual_Survival
  ) %>%
  filter(!is.na(Annual_Survival))

#survival_by_year <- survival_year

# -------------------------------------------------
# STEP 4: Average hazard across years
# -------------------------------------------------

survival_avg_year <- survival_year %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(
    Annual_Survival = mean(Annual_Survival, na.rm = TRUE),
    Annual_Retirement = mean(Annual_Retirement, na.rm = TRUE),
    Observations = n(),
    .groups = "drop"
  ) %>%
  arrange(Fuel_Group, Vehicle_Age)

survival_avg_year<-survival_avg_year %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Survival_Probability = cumprod(coalesce(Annual_Survival, 1))
  )

# -------------------------------------------------
# STEP 5: Reconstruct cumulative survival curve
# -------------------------------------------------

survival_by_year <- survival_year %>%
  group_by(Fuel_Group,Data_Year) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Survival_Probability = cumprod(coalesce(Annual_Survival, 1))
  ) %>%
  ungroup()

# Restrict to comparable age range
survival_by_year <- survival_by_year %>%
  filter(Vehicle_Age <= 25)

# ============================================
# PLOT: Model Cohort Survival Curves
# ============================================

ggplot(survival_avg_year,
       aes(x = Vehicle_Age,
           y = Survival_Probability,
           color = Fuel_Group)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0,1)) +
  scale_x_continuous(breaks = seq(0,125,10)) +
  labs(
    title = "Cohort Survival Curves (Correct Diagonal Estimation)",
    x = "Vehicle Age (years)",
    y = "Survival Probability",
    color = "Fuel Type"
  ) +
  theme_minimal(base_size = 12)



# ============================================
# FIGURE 1: Petrol - All calendar years in one figure
# ============================================

petrol_data <- survival_by_year%>%
  filter(Fuel_Group == "Petrol", 
         Data_Year >= 1998,  # Adjust year range as needed
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
# ============================================
# FIGURE 3: Both fuels with faceted calendar years
# ============================================

p_period_survival <- ggplot(
  survival_by_year,
  aes(x = Vehicle_Age,
      y = Survival_Probability,
      color = Fuel_Group)
) +
  geom_line(size = 1.2) +
  facet_wrap(~Data_Year) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = seq(0,40,5), limits = c(0,40)) +
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
    strip.text = element_text(face="bold")
  ) +
  scale_color_brewer(palette="Set1")

print(p_period_survival)

# ============================================
# FIGURE 4: Heatmap style - Color by calendar year
# ============================================

# For Petrol only - showing year gradient
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



# ============================================
# CORRECT: NAEI 2019 Cumulative Survival -> Annual Retirement Rates
# Using your exact variable names
# ============================================

naei_retirement <- data.frame(
  Vehicle_Age = 0:25,
  Petrol = c(1, 0.9975, 0.995, 0.9925, 0.99, 0.99, 0.972, 0.954, 0.936, 0.918, 
             0.9, 0.81, 0.72, 0.63, 0.54, 0.45, 0.376, 0.302, 0.228, 0.154, 
             0.08, 0.064, 0.048, 0.032, 0.016, 0),
  Diesel = c(1, 0.995, 0.99, 0.985, 0.98, 0.98, 0.95, 0.92, 0.89, 0.86, 
             0.83, 0.744, 0.658, 0.572, 0.486, 0.4, 0.328, 0.256, 0.184, 0.112, 
             0.04, 0.032, 0.024, 0.016, 0.008, 0))

# FIRST: Calculate ANNUAL RETIREMENT RATES from cumulative survival
naei_annual <- naei_retirement %>%
  mutate(
    # Annual retirement rate = 1 - (survival at age t+1 / survival at age t)
    Petrol_Annual = 1 - (lead(Petrol) / Petrol),
    Diesel_Annual = 1 - (lead(Diesel) / Diesel),
    # For age 25 (last age), set retirement rate to 1 (all retire)
    Petrol_Annual = ifelse(is.na(Petrol_Annual), 1, Petrol_Annual),
    Diesel_Annual = ifelse(is.na(Diesel_Annual), 1, Diesel_Annual)
  )

# THEN create the survival rates (keep original columns)
naei_survival <- naei_annual %>%
  mutate(
    Petrol_Survival = Petrol,
    Diesel_Survival = Diesel
  ) %>%
  select(Vehicle_Age, Petrol_Survival, Diesel_Survival, Petrol_Annual, Diesel_Annual)

# View the result
head(naei_survival)

# ============================================
# COMPARE MODEL SURVIVAL WITH NAEI REFERENCE
# ============================================

# Extract the most recent year's survival curve from the model
model_survival_recent <- survival_by_year %>%
  filter(Data_Year == 2024, Fuel_Group %in% c("Petrol", "Diesel")) %>%
  select(Vehicle_Age, Fuel_Group, Model_Survival = Survival_Probability)

# Alternatively, use average over recent years (e.g., 2020-2024) for smoother comparison
model_survival_avg <- survival_avg_year %>%
  filter( Fuel_Group %in% c("Petrol", "Diesel")) %>%
  group_by(Vehicle_Age, Fuel_Group) %>%
  summarise(Model_Survival = mean(Survival_Probability, na.rm = TRUE), .groups = "drop")

# Reshape NAEI survival to long format for joining
naei_long <- naei_survival %>%
  pivot_longer(
    cols = c(Petrol_Survival, Diesel_Survival),
    names_to = "Fuel_Group",
    values_to = "NAEI_Survival",
    names_pattern = "(.*)_Survival"
  )

# Join model (choose one) with NAEI, restrict to ages 0-25
comparison_data <- model_survival_avg %>%
  inner_join(naei_long, by = c("Vehicle_Age", "Fuel_Group")) %>%
  filter(Vehicle_Age <= 25)

# Calculate mean absolute error (MAE) for each fuel
mae_summary <- comparison_data %>%
  group_by(Fuel_Group) %>%
  summarise(
    MAE = mean(abs(Model_Survival - NAEI_Survival), na.rm = TRUE),
    Max_Age = max(Vehicle_Age)
  )

cat("\n=== MODEL vs NAEI COMPARISON (ages 0-25) ===\n")
print(mae_summary)

# Create comparison plot
p_comparison <- ggplot(comparison_data, aes(x = Vehicle_Age)) +
  geom_line(aes(y = Model_Survival, color = "Model (2020-2024 avg)"), size = 1.2) +
  geom_line(aes(y = NAEI_Survival, color = "NAEI 2019"), size = 1.2, linetype = "dashed") +
  facet_wrap(~Fuel_Group) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  scale_color_manual(values = c("Model (2020-2024 avg)" = "#E41A1C", 
                                "NAEI 2019" = "#377EB8")) +
  labs(
    # title = "Comparison of Model Survival Curves with NAEI 2019 Reference",
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

# Optional: Show multiple recent years as faint background
recent_years <- survival_by_year %>%
  filter(Data_Year %in% c(2018,2019,2020, 2021, 2022, 2023),
         Fuel_Group %in% c("Petrol", "Diesel"),
         Vehicle_Age <= 25)

p_comparison_with_variability <- ggplot() +
  geom_line(data = recent_years, 
            aes(x = Vehicle_Age, y = Survival_Probability, 
                group = Data_Year, 
                color = "Individual years (2018-2023)"),  # This goes to legend
            alpha = 0.5, size = 0.6)  +
  geom_line(data = comparison_data, 
            aes(x = Vehicle_Age, y = Model_Survival, color = "Model (2020-2024 avg)"), size = 1.2) +
  geom_line(data = comparison_data, 
            aes(x = Vehicle_Age, y = NAEI_Survival, color = "NAEI 2019"), size = 1.2, linetype = "dashed") +
  facet_wrap(~Fuel_Group) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  scale_color_manual(values = c("Model (2020-2024 avg)" = "#E41A1C", 
                                "NAEI 2019" = "#377EB8",
                                "Individual years (2018-2023)"="black")) +
  labs(
    # title = "Model Variability (2020-2024) vs NAEI 2019",
    #subtitle = "Gray lines show individual years",
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





# ============================================================
# NEW retirement_table based on 2020–2024 cohort survival
# Retirement = 1 - Annual_Survival 
# ============================================================

# # Your code (which is correct):
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
  # mutate(
  #   Retirement_Rate = pmin(pmax(Retirement_Rate, 0), 0.5),
  #   Survival_Rate   = 1 - Retirement_Rate
  # ) %>%
  arrange(Fuel_Group, Vehicle_Age)

cat("\n=== RETIREMENT RATES SUMMARY (2020-2024) ===\n")
print(retirement_table %>%
        group_by(Fuel_Group) %>%
        summarise(
          Avg_Retirement = mean(Retirement_Rate),
          Min_Retirement = min(Retirement_Rate),
          Max_Retirement = max(Retirement_Rate)
        ))

# Optional: Quick visualization to check patterns
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

# ============================================
# RECALCULATE SURVIVAL FOR ALL FUEL TYPES
# ============================================

# STEP 1: Aggregate fleet by Year × Age × Fuel for ALL fuel types
survival_year_all <- fleet_complete %>%
  filter(!is.na(Vehicle_Age), Fuel_Group!="Other") %>%  # Remove "Other" but keep ALL others
  group_by(Fuel_Group, Vehicle_Age,Data_Year) %>%
  summarise(Fleet = sum(Fleet_Count, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(Fuel_Group, Data_Year, Vehicle_Age)

# STEP 2: Create diagonal match (t,a) → (t+1,a+1)
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

# STEP 3: Compute annual survival & retirement for ALL fuels
survival_year_all <- survival_year_all %>%
  mutate(
    Annual_Survival = Fleet_next / Fleet,
    Annual_Survival = pmin(pmax(Annual_Survival, 0), 1),
    Annual_Retirement = 1 - Annual_Survival
  ) %>%
  filter(!is.na(Annual_Survival))

# Save with year information
survival_by_year_all <- survival_year_all

# Check what fuel types we have
cat("\n=== FUEL TYPES IN SURVIVAL CALCULATION ===\n")
print(unique(survival_by_year_all$Fuel_Group))


# ============================================
# CREATE RETIREMENT TABLE FOR ALL FUEL TYPES (2020-2024)
# ============================================
# # Your code (which is correct):
retirement_table_all <- survival_by_year_all %>%
  filter(
    Data_Year %in% 2022:2024,# this range cause the change in new sales values. 
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

# Create complete grid
complete_grid <- expand.grid(
  Fuel_Group = all_fuels,
  Vehicle_Age = all_ages,
  stringsAsFactors = FALSE
)

# Join with existing data
retirement_table_all <- complete_grid %>%
  left_join(retirement_table_all, by = c("Fuel_Group", "Vehicle_Age")) %>%
  arrange(Fuel_Group, Vehicle_Age)

# Fill missing Observations and Years_covered
retirement_table_all <- retirement_table_all %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    # Carry forward last known value for Observations
    Observations = zoo::na.locf(Observations, na.rm = FALSE, fromLast = FALSE),
    # Fill remaining NAs with 0
    Observations = ifelse(is.na(Observations), 0, Observations),
    Years_covered = ifelse(is.na(Years_covered), "No data", Years_covered)
  ) %>%
  ungroup()%>% arrange(Fuel_Group)


# PHEV age 0-10: Has 4,4,4,4,4,4,4,4,4,3,2 observations (decreasing)
# PHEV age 11: Only 1 observation
# PHEV ages 12-18: COMPLETELY MISSING (no data!)
# PHEV age 19: 1 observation with 50% retirement rate!
#   PHEV ages 20-23: 1 observation each

# Loop through each row of PHEV
# Weighted approach (70% Petrol + 30% BEV)
for(i in 1:nrow(retirement_table_all)) {
  
  # Case 1: Set HEV = Petrol
  if(retirement_table_all$Fuel_Group[i] == "HEV") {
    age_i <- retirement_table_all$Vehicle_Age[i]
    
    # Get Petrol rate for this age
    petrol_rate <- retirement_table_all$Retirement_Rate[
      retirement_table_all$Fuel_Group == "Petrol" & retirement_table_all$Vehicle_Age == age_i
    ]
    
    if(length(petrol_rate) > 0 && !is.na(petrol_rate)) {
      retirement_table_all$Retirement_Rate[i] <- petrol_rate
    } else {
      retirement_table_all$Retirement_Rate[i] <- 0.05
    }
    
    retirement_table_all$Survival_Rate[i] <- 1 - retirement_table_all$Retirement_Rate[i]
  }
  
  # Case 2: Set PHEV = BEV for ALL ages (no age restriction)
  if(retirement_table_all$Fuel_Group[i] == "PHEV") {
    age_i <- retirement_table_all$Vehicle_Age[i]
    
    # Get BEV rate for this age
    bev_rate <- retirement_table_all$Retirement_Rate[
      retirement_table_all$Fuel_Group == "BEV" & retirement_table_all$Vehicle_Age == age_i
    ]
    
    if(length(bev_rate) > 0 && !is.na(bev_rate)) {
      retirement_table_all$Retirement_Rate[i] <- bev_rate
    } else {
      retirement_table_all$Retirement_Rate[i] <- 0.05
    }
    
    retirement_table_all$Survival_Rate[i] <- 1 - retirement_table_all$Retirement_Rate[i]
  }
}

# Final cleanup - fill any remaining NAs with defaults
retirement_table_all <- retirement_table_all %>%
  mutate(
    Retirement_Rate = ifelse(is.na(Retirement_Rate), 0.05, Retirement_Rate),
    Survival_Rate = 1 - Retirement_Rate
  )

# ============================================
# CREATE THE FIGURE
# ============================================

# Figure 1: Retirement Rates by Fuel Type

p_retirement_all <- ggplot(retirement_table_all, 
                           aes(x = Vehicle_Age, y = Retirement_Rate, 
                               color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 1, alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.3)) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  labs(
    # title = "Annual Retirement Rates by Vehicle Age and Fuel Type",
    #  subtitle = "Based on 2020-2024 historical data (all fuel types)",
    x = "Vehicle Age (years)",
    y = "Annual Retirement Rate",
    color = "Fuel Type",
    #caption = "Shows probability a vehicle retires in a given year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

# Display the figure
print(p_retirement_all)



retirement_avg <- retirement_table_all %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(
    Retirement_Rate = mean(Retirement_Rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Survival_Rate = 1 - Retirement_Rate
  )
# Before smoothing 
ggplot(retirement_avg, aes(x = Vehicle_Age, y = Retirement_Rate, color = Fuel_Group)) +
  geom_line() + 
  labs(title = "Raw Retirement Rates (Noisy)")


# smoothing the retirment (much cleaner trend) smoothed, non-monotonic) rates
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



# After smoothing (much cleaner trend)
ggplot(retirement_avg, aes(x = Vehicle_Age, y = Retirement_Rate, color = Fuel_Group)) +
  geom_line() + 
  labs(title = "Smoothed Retirement Rates (3-Year Moving Average)") 

# Enhanced retirement rate figure
p_retirement_enhanced <- retirement_avg %>%
  mutate(
    Fuel_Group = factor(Fuel_Group, 
                        levels = c("Diesel", "Petrol", "HEV", "PHEV", "BEV"))
  ) %>%
  ggplot(aes(x = Vehicle_Age, y = Retirement_Rate, color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_point(data = . %>% filter(Vehicle_Age %% 5 == 0), size = 2, alpha = 0.7) +
  
  # Add reference lines
  geom_vline(xintercept = c(10, 15, 20), linetype = "dashed", alpha = 0.5) +
  annotate("text", x = c(10, 15, 20), y = 0.22, 
           label = c("Age 10", "Age 15", "Age 20"), 
           size = 3, angle = 90, vjust = -0.5) +
  
  # # Add annotation for diesel disadvantage
  # annotate("text", x = 12, y = 0.08, 
  #          label = "Diesel retirement rates\nconsistently highest", 
  #          size = 3.5, color = "#E41A1C") +
  
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 0.25),
    breaks = seq(0, 0.25, 0.05)
  ) +
  scale_x_continuous(
    limits = c(0, 30),
    breaks = seq(0, 30, 5)
  ) +
  labs(
    #  title = "Annual Retirement Rates by Fuel Type",
    #subtitle = "Probability a vehicle retires in a given year",
    x = "Vehicle Age (years)",
    y = "Annual Retirement Rate",
    color = "Fuel Type",
    # caption = "Diesel vehicles show consistently higher retirement rates across all ages"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

print(p_retirement_enhanced)
# ============================================
# THREE-PANEL RETIREMENT ANALYSIS BY FUEL TYPE (Matching Original Style)
# ============================================

# Prepare fuel-specific data for the three-panel plot
# Using your updated model variable names

# Calculate fuel-specific cumulative survival, retirement probability, and cumulative retirement
fuel_specific_analysis <- retirement_avg %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    # Cumulative survival (percentage still alive at each age)
    Cumulative_Survival = cumprod(Survival_Rate),
    
    # Retirement probability at each age (hazard rate)
    Retirement_Prob_At_Age = lag(Cumulative_Survival, default = 1) * Retirement_Rate,
    
    # Cumulative retirement (percentage retired by each age)
    Cumulative_Retirement = 1 - Cumulative_Survival
  ) %>%
  ungroup()

# Calculate median retirement age for each fuel type
fuel_median_ages <- fuel_specific_analysis %>%
  group_by(Fuel_Group) %>%
  summarise(
    Median_Age = min(Vehicle_Age[Cumulative_Retirement >= 0.5], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Replace Inf or NA with reasonable defaults
  mutate(
    Median_Age = ifelse(is.infinite(Median_Age) | is.na(Median_Age), 
                        25, Median_Age)
  )

# Print median ages for reference
cat("\n", strrep("=", 80), "\n")
cat("MEDIAN RETIREMENT AGE BY FUEL TYPE")
cat("\n", strrep("=", 80), "\n")
print(fuel_median_ages)

# ============================================
# PLOT 1: Survival Curve by Fuel Type (with fuel-specific median markers)
# ============================================

p1_fuel <- ggplot(fuel_specific_analysis, 
                  aes(x = Vehicle_Age, y = Cumulative_Survival, 
                      color = Fuel_Group)) +
  geom_line(size = 1.2) +
  # Add horizontal line at 50%
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50", alpha = 0.7) +
  # Add vertical lines at median age for each fuel type
  geom_vline(data = fuel_median_ages, 
             aes(xintercept = Median_Age, color = Fuel_Group),
             linetype = "dashed", alpha = 0.5, size = 0.8) +
  # Add text labels for median ages
  geom_text(data = fuel_median_ages,
            aes(x = Median_Age, y = 0.52, 
                label = paste(round(Median_Age), "yrs"),
                color = Fuel_Group),
            size = 3.5, hjust = -0.1, vjust = 0) +
  labs(
    #title = "Vehicle Survival Curves by Fuel Type",
    # subtitle = "Percentage of vehicles still in fleet by age | Dashed lines show median age for each fuel",
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

# ============================================
# PLOT 2: Retirement Probability by Age (Hazard Rate) by Fuel Type
# ============================================

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

# Alternative: Line plot for retirement probability (sometimes clearer than bars)
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

# ============================================
# PLOT 3: Cumulative Retirement by Fuel Type
# ============================================

p3_fuel <- ggplot(fuel_specific_analysis, 
                  aes(x = Vehicle_Age, y = Cumulative_Retirement, 
                      color = Fuel_Group)) +
  geom_line(size = 1.2) +
  # Add horizontal lines at 50%, 75%, 90%
  geom_hline(yintercept = c(0.5, 0.75, 0.9), linetype = "dashed", 
             color = c("red", "orange", "gold"), alpha = 0.7, size = 0.8) +
  # Add text labels for the horizontal lines
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

# ============================================
# COMBINE ALL THREE PLOTS (Vertical Layout)
# ============================================

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

# Display the combined plot
print(combined_fuel_plots_vertical)

# ============================================
# ALTERNATIVE: COMBINE THREE PLOTS (Horizontal Layout for Comparison)
# ============================================

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

# Display the horizontal combined plot
print(combined_fuel_plots_horizontal)

# ============================================
# CREATE A TABLE WITH KEY RETIREMENT METRICS BY FUEL TYPE
# ============================================

# Calculate additional retirement metrics by fuel type
fuel_retirement_metrics <- fuel_specific_analysis %>%
  group_by(Fuel_Group) %>%
  summarise(
    # Median retirement age (50% retired)
    Median_Age = first(Vehicle_Age[Cumulative_Retirement >= 0.5]),
    
    # 75% retirement age
    Age_75 = first(Vehicle_Age[Cumulative_Retirement >= 0.75]),
    
    # 90% retirement age
    Age_90 = first(Vehicle_Age[Cumulative_Retirement >= 0.90]),
    
    # Expected retirement age (weighted average)
    Expected_Age = sum(Vehicle_Age * Retirement_Prob_At_Age, na.rm = TRUE),
    
    # Maximum survival rate at age 10
    Survival_Age_10 = Cumulative_Survival[Vehicle_Age == 10],
    
    # Maximum survival rate at age 20
    Survival_Age_20 = Cumulative_Survival[Vehicle_Age == 20],
    
    # Peak retirement age (age with highest retirement probability)
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

# ============================================
# CREATE A VISUAL TABLE USING GT PACKAGE (Optional)
# ============================================

# If you have gt package installed, create a formatted table
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




# ============================================
# THREE-PANEL RETIREMENT ANALYSIS PLOT (Matching Original Style)
# ============================================

# Prepare the data for the three-panel plot
# Using your updated model variable names:

# 1. Create cumulative survival data (overall weighted)
# First, calculate weights based on 2024 fleet composition for each fuel type
fleet_weights_2024 <- fleet_2024_complete %>%
  group_by(Fuel_Group) %>%
  summarise(Fleet_Weight = sum(Fleet_Count), .groups = "drop") %>%
  mutate(Weight = Fleet_Weight / sum(Fleet_Weight))
names(fleet_weights_2024)

# Calculate weighted survival and retirement metrics
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

# Calculate median retirement age (when cumulative retirement >= 0.5)
median_retirement_age <- validation_total_weighted %>%
  filter(Cumulative_Retirement >= 0.5) %>%
  slice(1) %>%
  pull(Vehicle_Age)

# If no median found, use a reasonable default
if(length(median_retirement_age) == 0 || is.na(median_retirement_age)) {
  median_retirement_age <- 15
  warning("Median retirement age not found, using default value of 15 years")
}

# Plot 1: Survival curve (p1)
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

# Plot 2: Retirement probability by age (p2)
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

# Plot 3: Cumulative retirement (p3)
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

# Combine all three plots vertically
library(patchwork)
combined_retirement_plots <- (p1 / p2 / p3) +
  plot_annotation(
    # title = "Vehicle Retirement and Survival Analysis",
    # subtitle = paste("Based on historical data (2022-2024) | Median retirement age:", 
    #                  round(median_retirement_age, 1), "years"),
    # caption = "Shows survival probability, hazard rates, and cumulative retirement patterns"
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

# Display the combined plot
print(combined_retirement_plots)


# Also create a version with fuel-specific curves for comparison
# Create fuel-specific cumulative survival
fuel_specific_survival <- retirement_avg %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Cumulative_Survival = cumprod(Survival_Rate),
    Cumulative_Retirement = 1 - Cumulative_Survival,
    Retirement_Prob_At_Age = lag(Cumulative_Survival, default = 1) * Retirement_Rate
  ) %>%
  ungroup()

# Calculate median retirement age by fuel type
fuel_median_ages <- fuel_specific_survival %>%
  group_by(Fuel_Group) %>%
  summarise(
    Median_Age = min(Vehicle_Age[Cumulative_Retirement >= 0.5], na.rm = TRUE),
    .groups = "drop"
  )

# Fuel-specific survival curve
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

# Fuel-specific retirement probability
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

# Fuel-specific cumulative retirement
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

# Combine fuel-specific plots
combined_fuel_plots <- (p1_fuel | p2_fuel | p3_fuel) +
  plot_annotation(
    # title = "Retirement Analysis by Fuel Type",
    # subtitle = paste("Median retirement ages:",
    # paste(fuel_median_ages$Fuel_Group, "=", 
    #   fuel_median_ages$Median_Age, "yrs", collapse = ", "))
  ) +
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
cat("\nFiles saved:\n")
cat("- Three_Panel_Retirement_Analysis.png\n")
cat("- Fuel_Specific_Retirement_Analysis.png\n")
cat("\nKey metrics:\n")
cat("- Overall median retirement age:", round(median_retirement_age, 1), "years\n")
cat("- Fuel-specific median ages:\n")
print(fuel_median_ages)




#============================================

#============================================

all_fuels_data <- survival_by_year_all %>%
  arrange(Fuel_Group, Data_Year, Vehicle_Age) %>%
  group_by(Fuel_Group, Data_Year) %>%
  mutate(
    Survival_Rate = cumprod(Annual_Survival)
  ) %>%
  ungroup()

# Add Smoothed column
all_fuels_data$Smoothed <- FALSE

# Apply smoothing to ALL PHEV entries (or you could add a different condition)
for(i in 1:nrow(all_fuels_data)) {
  if(all_fuels_data$Fuel_Group[i] == "PHEV") {  # Removed the Observations condition
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
    
    # Check if both rates exist
    if(length(petrol_rate) > 0 && length(bev_rate) > 0) {
      # Weighted average: 70% petrol, 30% BEV
      all_fuels_data$Annual_Survival[i] <- 0.7 * petrol_rate + 0.3 * bev_rate
      all_fuels_data$Smoothed[i] <- TRUE
    }
  }
}

# Recalculate cumulative survival after smoothing
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



#=====================================
# data to prepare the inflow data 
#====================================
# ---------------------------
# 2. DATA PREPARATION
# ---------------------------

Forcast_fleet<- your_forecast_core_trend
Forcast_fleet <-Forcast_fleet %>% select(Year,Annual_Growth_Rate,Total_Vehicles,Core_Model)
# rename(Core_Model = Total_Vehicles) 
Fuel_forecast <- fuel_forecasts_updated

Fuel_forecast <- Fuel_forecast %>% 
  mutate(Fuel_percentage = Percentage / 100) %>%
  rename(Year = Data_Year)%>%filter(Model_Used=="Manual (CCC)") 


# Check if percentages sum to 1 for each year
Fuel_check <- Fuel_forecast %>%
  group_by(Year) %>%
  summarise(
    Total = sum(Fuel_percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Check = ifelse(round(Total, 10) == 1, "✓ Yes", "✗ No"))

# Print the result
cat("Check if percentages sum to 1 (100%) for each year:\n")
print(Fuel_check)


# Merge the tables
merged_fleet <- Forcast_fleet %>% select(Annual_Growth_Rate,Core_Model,Year)%>%
  left_join(Fuel_forecast , by = "Year")

# Calculate inflow
merged_fleet <- merged_fleet %>% filter(Year>=2024)%>%
  rename(sale_percentage= Fuel_percentage) %>%
  select(Year,Core_Model, Fuel_Group, sale_percentage,Annual_Growth_Rate)%>% arrange(Fuel_Group,Year)
# View output
print(merged_fleet,wide=Inf)

# setwd("C:\\Users\\fzzb134\\OneDrive - University of Leeds\\Infuze\\R-programming-Infuze\\R-programming_Updated analysis\\New-updated_data\\Excell_files")
# write.csv(merged_fleet,"inflow_model.csv")

# ============================================
# PART 3: LOAD INFLOW DATA AND CREATE SALES FORECAST 2025-2037
# ============================================
# Calculate 2024 base fleet (filtering vehicles age <= 30)
fleet_2024_base <- Forcast_fleet$Total_Vehicles[Forcast_fleet$Year == 2024]
print(fleet_2024_base)

cat("2024 Base Fleet (age <= 30):", format(fleet_2024_base, big.mark = ","), "\n")



# Step 1: Keep ALL columns in inflow_data (for fuel shares, etc.)
inflow_data <- merged_fleet  # Keep all columns, multiple rows per year


# Get fuel shares from inflow data
fuel_shares_all <- inflow_data %>%
  select(Year, Fuel_Group , sale_percentage) %>%
  group_by(Year, Fuel_Group) %>%
  summarise(sale_percentage = mean(sale_percentage), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(sale_percentage = sale_percentage / sum(sale_percentage)) %>%
  ungroup()

# Create complete fuel shares for all years
fuel_shares_complete <- expand.grid(Year = 2025:2037, Fuel_Group = c("Petrol", "Diesel", "BEV", "PHEV", "HEV")) %>%
  left_join(fuel_shares_all, by = c("Year", "Fuel_Group")) %>%
  group_by(Year) %>%
  mutate(
    sale_percentage = ifelse(is.na(sale_percentage), mean(sale_percentage, na.rm = TRUE), sale_percentage),
    sale_percentage = ifelse(is.na(sale_percentage), 0.2, sale_percentage),
    sale_percentage = sale_percentage / sum(sale_percentage)
  ) %>%
  ungroup()

print(fuel_shares_complete,n=Inf)
# Extract fuel future sale shares data
fuel_shares_table <- fuel_shares_complete %>%
  select(Year, Fuel_Group, sale_percentage) %>%
  arrange(Year, Fuel_Group) %>%
  # Convert percentage to percentage format (0-100)
  mutate(
    sale_percentage = sale_percentage * 100,
    sale_percentage = round(sale_percentage, 2)
  ) %>%
  # Pivot to wide format for better readability
  pivot_wider(
    names_from = Fuel_Group,
    values_from = sale_percentage,
    values_fill = 0
  ) %>%
  arrange(Year)

# View the table
print(fuel_shares_table)


# Step 2: Create unique version for target calculation (one row per year)
inflow_unique <- inflow_data %>%
  select(Year, Annual_Growth_Rate, Core_Model) %>%
  distinct() %>%
  arrange(Year)

# Step 3: Calculate cumulative growth and targets using unique data
inflow_unique <- inflow_unique %>%
  mutate(
    Cumulative_Growth = cumprod(1 + coalesce(Annual_Growth_Rate/100, 0)),
    Cumulative_Growth = ifelse(Year == 2024, 1, Cumulative_Growth),
    Target_Fleet = fleet_2024_base * Cumulative_Growth
  )

# Step 4: Create natcop_targets from the unique data
natcop_targets <- inflow_unique %>%
  select(Year, Target_Fleet, Annual_Growth_Rate, Core_Model)

cat("\n=== NATCOP TARGETS (from unique data) ===\n")
print(natcop_targets)


# ============================================
# PART 4: CREATE SALES FORECAST 2025-2037 USING SMMT + GROWTH RATES
# ============================================

# SMMT total sales targets for 2025-2027
target_sales <- data.frame(
  Year = c(2025, 2026, 2027),
  Total_Sales = c(2020520, 2032000, 2058000)  # Already in actual numbers
)

cat("\n=== SMMT TOTAL SALES TARGETS ===\n")
print(target_sales)

# Get growth rates from inflow data
growth_rate <- inflow_data %>%
  select(Year, Annual_Growth_Rate) %>%
  distinct() %>%
  arrange(Year)

# Start from 2027 sales
sales_2027 <- target_sales$Total_Sales[3]

# Calculate sales for 2028-2037 using growth rates
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
    smmt_sales_forecast$Total_Sales[i] <- prev_sales * 1.01  # Default 1% growth
  }
}

cat("\n=== COMPLETE SALES FORECAST 2025-2037 ===\n")
print(smmt_sales_forecast)


# ============================================
# PART 5: GET FUEL SHARES FOR NEW SALES
# ============================================

# Calculate sales by fuel type
sales_by_fuel_smmt <- fuel_shares_complete %>%
  left_join(smmt_sales_forecast, by = "Year") %>%
  mutate(
    New_Sales = sale_percentage * Total_Sales,
    Vehicle_Age = 0
  ) %>%
  select(Year, Fuel_Group, New_Sales, Vehicle_Age)

cat("\n=== SALES BY FUEL TYPE (First few years) ===\n")
print(sales_by_fuel_smmt %>% filter(Year <= 2037) %>% pivot_wider(names_from = Fuel_Group, values_from = New_Sales))

#=========================
#project model
#=========================
engine_sales_distribution <- fleet_2024_complete %>%
  
  filter(
    Vehicle_Age == 0,
    !is.na(engine_size)
  ) %>%
  
  mutate(
    engine_size_liters = round(engine_size / 1000, 1)
  ) %>%
  
  group_by(Fuel_Group, engine_size_liters,vehicle_class) %>%
  
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



#============================================================================================

project_fleet <- function(base_fleet, retirement_rates, fuel_shares,
                          natcop_targets, engine_sales_distribution) {
  if (!"kerb_weight_kg" %in% names(base_fleet)) {
    base_fleet <- base_fleet %>% mutate(kerb_weight_kg = NA_real_)
  }
  
  
  # Get average dimensions by Fuel_Group and vehicle_class from 2024 data
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
           engine_size_liters = engine_size / 1000 )%>%
    # kerb_weight_kg should already be in base_fleet, so just select it
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
      select(Vehicle_Age, Fuel_Group, Survivors, engine_size, engine_size_liters, vehicle_class,kerb_weight_kg)
    
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
      # Join dimensions AFTER ungroup
      left_join(base_dimensions, by = c("Fuel_Group", "vehicle_class")) %>%
      mutate(
        # Project dimensions for this year
        projected_length_mm = avg_length_mm + growth_rates$length_mm_per_year * (yr - 2024),
        projected_width_mm = avg_width_mm + growth_rates$width_mm_per_year * (yr - 2024),
        projected_height_mm = avg_height_mm + growth_rates$height_mm_per_year * (yr - 2024),
        
        # Predict kerb weight using the model - FIXED parenthesis
        kerb_weight_kg = predict(weight_model, 
                                 newdata = data.frame(
                                   length_m = projected_length_mm / 1000,
                                   width_m = projected_width_mm / 1000,
                                   height_m = projected_height_mm / 1000,
                                   Fuel_Group = Fuel_Group,
                                   vehicle_class = vehicle_class
                                 )),
        # Round the predicted weight
        kerb_weight_kg = round(kerb_weight_kg, 0)
      ) %>%
      select(Year, Vehicle_Age, Fuel_Group, Fleet, engine_size, engine_size_liters, 
             vehicle_class, kerb_weight_kg) %>%
      filter(Fleet > 0)
    
    
    combined_fleet <- bind_rows(
      aged_fleet %>%
        mutate(Year = yr, Fleet = Survivors) %>%
        select(Year, Vehicle_Age, Fuel_Group, Fleet, engine_size, engine_size_liters, vehicle_class,kerb_weight_kg),
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
          select(Year, Vehicle_Age, Fuel_Group, Fleet, engine_size, engine_size_liters, vehicle_class,kerb_weight_kg),
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
      select(Vehicle_Age, Fuel_Group, Fleet, engine_size, engine_size_liters, vehicle_class,kerb_weight_kg)
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

#===============================================================================

# Run the fixed function
library(tidyverse)
fleet_2024_complete <- fleet_2024_complete %>% filter(Vehicle_Age <= 30)
sum(fleet_2024_complete$Fleet_Count)
print(natcop_targets)

retirement_avg <- retirement_avg %>% filter(Vehicle_Age <= 30)

base_fleet_2024 <- fleet_2024_complete

# Now run the projection
results <- project_fleet(
  base_fleet = base_fleet_2024,
  retirement_rates = retirement_avg,
  fuel_shares = fuel_shares_complete,
  natcop_targets = natcop_targets,
  engine_sales_distribution = engine_sales_distribution
)


# HEV new sales should now have real, non-NaN/non-NA weight ( row count)
bind_rows(results$fleet) %>%
  filter(Vehicle_Age == 0, Fuel_Group == "HEV") %>%
  summarise(
    total = n(),
    n_valid = sum(!is.na(kerb_weight_kg) & !is.nan(kerb_weight_kg)),
    pct_valid = round(100 * n_valid / total, 1)
  )
#total HEV vehicles entering the fleet as new sales, 2025–2037 — sum Fleet

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

# Fast way to spot any gaps across all 65 combinations at once
weight_check %>% filter(pct_valid < 100)


# total vehicle in fleet 
bind_rows(results$fleet) %>% group_by(Year) %>%
  summarise(total = sum(Fleet)) %>% print(n = Inf)

#Full Fleet Composition
all_fleet <- bind_rows(results$fleet)

# Check results
print(results$accounting)


# Check maximum ages now
age_summary <- data.frame(
  Year = 2024:2037,
  Max_Vehicle_Age = sapply(2024:2037, function(y) {
    max(results$fleet[[as.character(y)]]$Vehicle_Age)
  })
)
print(age_summary)


# Compare with original fixed sales
sales_comparison <- results$accounting %>%
  select(Year, Predefined_Sales) %>%
  rename(Model_Predicted_Sales = Predefined_Sales) %>%
  left_join(smmt_sales_forecast, by = "Year") %>%
  rename(smmt_Sales = Total_Sales)

cat("\n=== ORIGINAL VS PREDICTED SALES ===\n")
print(sales_comparison)

# Combine all years into one dataframe
sal_at_age_0 <- bind_rows(results$fleet) %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Year) %>%
  summarise(Total_Sales = sum(Fleet))

print(sal_at_age_0)


#------------------------------
# Show NATCOP targets vs Sales
#------------------------------
fleet_targets <- results$accounting %>%
  select(Year, Target_Fleet, Starting_Fleet, Predefined_Sales) %>%
  mutate(
    Target_Growth = Target_Fleet - lag(Target_Fleet),
    Sales_Ratio = Predefined_Sales / Target_Fleet * 100
  )

print(fleet_targets)

# Visualize
ggplot(fleet_targets, aes(x = Year)) +
  geom_line(aes(y = Target_Fleet/1e6, color = "Target Fleet"), size = 1.2) +
  geom_line(aes(y = Predefined_Sales/1e6, color = "New Sales"), size = 1.2) +
  scale_y_continuous(labels = scales::comma_format(suffix = "M")) +
  labs(
    title = "NATCOP Fleet Targets vs Required Sales",
    subtitle = "Sales cap reflects slowing fleet growth",
    x = "Year", y = "Millions of Vehicles"
  )


# Add calculations to understand the dynamics
fleet_analysis <- fleet_targets %>%
  mutate(
    Implied_Survivors = Starting_Fleet - (Starting_Fleet - lag(Starting_Fleet, default = first(Starting_Fleet))),
    Retirement_Rate_Implied = 1 - (Starting_Fleet - Predefined_Sales) / lag(Starting_Fleet, default = first(Starting_Fleet)),
    Sales_Change = Predefined_Sales - lag(Predefined_Sales, default = first(Predefined_Sales)),
    Target_Change = Target_Fleet - lag(Target_Fleet, default = first(Target_Fleet))
  )

print(fleet_analysis)

# ============================================================================
# CREATE ORIGIN SCENARIOS (Year-Varying)
# ============================================================================

# STEP 1: Historical origin shares within each fuel (from your microdata)
origin_by_fuel <- data %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year, Region, Fuel_Group,Origin) %>%
  summarise(Reg = sum(Fleet_Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Data_Year, Fuel_Group) %>%
  mutate(Share_Within_Fuel = 100 * Reg / sum(Reg)) %>%
  ungroup()
print(origin_by_fuel,n=Inf)

# Confirm China/Japan/Korea/UK/USA now differ meaningfully within each fuel group
origin_by_fuel %>%
  filter(Region %in% c("China","Japan","Korea","UK","USA"), Data_Year == 2024) %>%
  select(Region, Fuel_Group, Share_Within_Fuel) %>%
  pivot_wider(names_from = Region, values_from = Share_Within_Fuel) %>%
  print(n = Inf)


# Check China's BEV production over time (global share)
# This requires data on China's BEV exports to calculate global share
# check data at difrrent region and all fuel trypes 

# FIXED: Aggregate across fuel groups first

all_fuel_by_region <- data %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year, Region, Fuel_Group) %>%
  summarise(
    Vehicles = sum(Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # SUM across fuel groups for each Region-Year
  group_by(Data_Year, Region) %>%
  summarise(
    Total_Vehicles = sum(Vehicles, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Now calculate global share based on total vehicles
  group_by(Data_Year) %>%
  mutate(
    Global_Share = 100 * Total_Vehicles / sum(Total_Vehicles, na.rm = TRUE)
  ) %>%
  ungroup()

print("All vehicles by region over time (all fuel groups combined):")
print(all_fuel_by_region, n = Inf)

# Pivot to wide format
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


# Let's check the data for different regions
bev_by_region <- data %>%
  filter(Vehicle_Age == 0,Fuel_Group == "BEV") %>%
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

# Check all region specifically
all_bev_global <- bev_by_region %>%
  group_by(Data_Year)%>%
  pivot_wider(
    id_cols = Data_Year,
    names_from = Region,
    values_from = Global_Share
  ) %>%
  mutate(Total = rowSums(across(where(is.numeric) & !matches("Data_Year")), na.rm = TRUE))

print("China's share of global BEV sales:")
print(all_bev_global, n = Inf)

# Step 1: Get BEV sales by region (only BEV)
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

# Step 2: Get China's share specifically
china_bev_global <- bev_by_region %>%
  filter(Region == "China") %>%
  select(Data_Year, Global_Share)

print("China's share of global BEV sales:")
print(china_bev_global, n = Inf)

# Step 3: Create the plot
library(ggplot2)

# Plot to see the trend
p_china_global <- china_bev_global  %>%
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



# ============================================
# SIMPLE CODE: Global share for each Region × Fuel_Group
# ============================================

# Step 1: Calculate global share for each Region × Fuel_Group
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

# View the data
print("Global share for each Region × Fuel_Group:")
print(fuel_region_share, n = Inf)

# Step 2: Pivot to wide format - one column per Region, with values as Global_Share
# This will give you a table with Year, Fuel_Group, and each Region as a column
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

# Step 3: For each fuel type, create a separate table
# BEV only
bev_table <- fuel_region_wide %>%
  filter(Fuel_Group == "BEV") %>%
  select(-Fuel_Group)

print("BEV - share of global BEV sales by region:")
print(bev_table, n = Inf)

# Petrol only
petrol_table <- fuel_region_wide %>%
  filter(Fuel_Group == "Petrol") %>%
  select(-Fuel_Group)

print("Petrol - share of global Petrol sales by region:")
print(petrol_table, n = Inf)

# Diesel only
diesel_table <- fuel_region_wide %>%
  filter(Fuel_Group == "Diesel") %>%
  select(-Fuel_Group)

print("Diesel - share of global Diesel sales by region:")
print(diesel_table, n = Inf)

# HEV only
hev_table <- fuel_region_wide %>%
  filter(Fuel_Group == "HEV") %>%
  select(-Fuel_Group)

print("HEV - share of global HEV sales by region:")
print(hev_table, n = Inf)

# PHEV only
phev_table <- fuel_region_wide %>%
  filter(Fuel_Group == "PHEV") %>%
  select(-Fuel_Group)

print("PHEV - share of global PHEV sales by region:")
print(phev_table, n = Inf)

# Step 4: Calculate slopes for each Region × Fuel_Group
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

print("Slopes for each Region × Fuel_Group (pp per year):")
print(slopes, n = Inf)

# Step 5: Pivot slopes to wide format
slopes_wide <- slopes %>%
  select(Fuel_Group, Region, Slope_pp) %>%
  pivot_wider(
    id_cols = Fuel_Group,
    names_from = Region,
    values_from = Slope_pp,
    values_fill = 0
  )

print("Slopes in wide format (Fuel_Group × Region):")
print(slopes_wide, n = Inf)


# ============================================
# REVISED: Region × Fuel_Group Slopes with R² Gating
# ============================================

# Step 1: Calculate slopes with R² and standard errors
slopes_detailed <- fuel_region_share %>%
  group_by(Fuel_Group, Region) %>%
  summarise(
    # Slope and statistics
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

# Step 2: Apply R² threshold and create gated slopes
R2_THRESHOLD <- 0.5  # Trend explains at least 50% of variance

slopes_gated <- slopes_detailed %>%
  mutate(
    # Base slope: use if R² >= threshold, otherwise 0 (flat)
    Slope_Base = ifelse(!is.na(R_Squared) & R_Squared >= R2_THRESHOLD, Slope_pp, 0),
    
    # Plus slope: use if R² >= threshold, otherwise 0
    # Use Slope + 1.96 * Std_Error for 95% confidence upper bound
    Slope_Plus = ifelse(!is.na(R_Squared) & R_Squared >= R2_THRESHOLD, 
                        Slope_pp + 1.96 * Std_Error, 0),
    
    # Minus slope: use if R² >= threshold, otherwise 0
    # Use Slope - 1.96 * Std_Error for 95% confidence lower bound
    Slope_Minus = ifelse(!is.na(R_Squared) & R_Squared >= R2_THRESHOLD, 
                         Slope_pp - 1.96 * Std_Error, 0),
    
    # Flag for trend quality
    Trend_Quality = case_when(
      is.na(R_Squared) ~ "Insufficient Data",
      R_Squared >= 0.7 ~ "Strong",
      R_Squared >= 0.5 ~ "Moderate",
      R_Squared >= 0.3 ~ "Weak",
      TRUE ~ "Noise"
    ),
    
    # Interpretation
    Interpretation = case_when(
      is.na(R_Squared) ~ "Flat (insufficient data)",
      R_Squared >= 0.5 ~ paste0("Trend (R²=", round(R_Squared, 2), ")"),
      TRUE ~ paste0("Flat (R²=", round(R_Squared, 2), " < 0.5)")
    )
  ) %>%
  select(Fuel_Group, Region, Slope_Base, Slope_Plus, Slope_Minus, 
         Slope_pp, R_Squared, Std_Error, Trend_Quality, Interpretation, Last_Value, N_Years)

print("=== GATED SLOPES WITH 95% CONFIDENCE BANDS ===")
print(slopes_gated, n = Inf)

# Step 3: Create wide format for easy viewing
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

# Step 4: Summary of which trends are usable
trend_summary <- slopes_gated %>%
  group_by(Trend_Quality) %>%
  summarise(
    Count = n(),
    Fuel_Regions = paste(paste(Fuel_Group, Region, sep="-"), collapse=", ")
  ) %>%
  arrange(desc(Count))

print("=== TREND QUALITY SUMMARY ===")
print(trend_summary)

# Step 5: Create the final slope lookup for build_trajectory()
slope_lookup <- slopes_gated %>%
  select(Fuel_Group, Region, 
         Slope_Base, Slope_Plus, Slope_Minus, 
         Last_Value, R_Squared, Trend_Quality) %>%
  # For regions with insufficient data, use Last_Value as flat projection
  mutate(
    # If no trend, projection is flat (Last_Value)
    Projection_Base = ifelse(Trend_Quality %in% c("Strong", "Moderate"), 
                             Slope_Base, 0),
    Projection_Plus = ifelse(Trend_Quality %in% c("Strong", "Moderate"), 
                             Slope_Plus, 0),
    Projection_Minus = ifelse(Trend_Quality %in% c("Strong", "Moderate"), 
                              Slope_Minus, 0)
  )

print("=== FINAL SLOPE LOOKUP FOR BUILD_TRAJECTORY() ===")
print(slope_lookup, n = Inf)

# Step 6: Visualize which slopes pass the threshold
library(ggplot2)

p_slope_quality <- slopes_gated %>%
  ggplot(aes(x = reorder(paste(Fuel_Group, Region, sep="-"), R_Squared), 
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
    # title = "R² Values for Region × Fuel_Group Trends",
    # subtitle = paste("Threshold =", R2_THRESHOLD, "(trends above are used; below are held flat)"),
    x = "Fuel_Group - Region",
    y = "R² (proportion of variance explained)",
    fill = "Trend Quality"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_slope_quality)


# Step 8: Quick check of what this means for 2025 projections
cat("\n=== PROJECTIONS FOR 2025 (USING GATED SLOPES) ===\n")

# BEV projections
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

# China BEV specifically
china_bev_2025 <- bev_2025 %>% filter(Region == "China")
cat("\nChina BEV: 2024 =", round(china_bev_2025$Last_Value, 1), 
    "% → 2025 Base =", round(china_bev_2025$Projected_Base, 1), 
    "% (95% CI:", round(china_bev_2025$Projected_Minus, 1), 
    "-", round(china_bev_2025$Projected_Plus, 1), "%)\n")

# Step 9: Create a clean summary table for your paper
cat("\n=== SUMMARY TABLE: TRENDS USED VS HELD FLAT ===\n")


# Step 9: Create a clean summary table for your paper
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


# ADD THIS before "Step 14" — bridges Step 8's output to Step 14's expected names
new_china_bev_base  <- china_bev_2025$Projected_Base
new_china_bev_plus  <- china_bev_2025$Projected_Plus
new_china_bev_minus <- china_bev_2025$Projected_Minus

cat("\nChecking bridged values:\n")
cat("Base:", new_china_bev_base, "| Plus:", new_china_bev_plus, "| Minus:", new_china_bev_minus, "\n")
# ============================================
# BRIDGE (run once, before Step 14) 
# ============================================
bev_2025 <- slope_lookup %>%
  filter(Fuel_Group == "BEV") %>%
  mutate(
    Projected_Base  = Last_Value + Projection_Base * 1,
    Projected_Plus  = Last_Value + Projection_Plus * 1,
    Projected_Minus = Last_Value + Projection_Minus * 1
  ) %>%
  select(Region, Last_Value, Projected_Base, Projected_Plus, Projected_Minus, Trend_Quality)

china_bev_2025 <- bev_2025 %>% filter(Region == "China")

new_china_bev_base  <- china_bev_2025$Projected_Base
new_china_bev_plus  <- china_bev_2025$Projected_Plus
new_china_bev_minus <- china_bev_2025$Projected_Minus

# ============================================
# STEP 14: China BEV projection plot (CORRECTED)
# ============================================
library(ggplot2)

# Get historical data for China BEV
china_bev_historical <- fuel_region_share %>%
  filter(Fuel_Group == "BEV", Region == "China") %>%
  select(Data_Year, Global_Share)

# Create projection data
projection_data <- data.frame(
  Data_Year = 2025,
  Global_Share = new_china_bev_base,
  Type = "Projected"
)

# Create confidence interval data
ci_data <- data.frame(
  Data_Year = 2025,
  ymin = new_china_bev_minus,
  ymax = new_china_bev_plus
)

# Combine historical and projection
china_bev_plot_data <- china_bev_historical %>%
  mutate(Type = "Historical") %>%
  bind_rows(projection_data)

# FIX: pull from slopes_gated, not slope_lookup — this is the object that
# actually carries Slope_pp and R_Squared together
china_bev_stats <- slopes_gated %>% 
  filter(Fuel_Group == "BEV", Region == "China")

cat("\n=== CHECKING china_bev_stats ===\n")
print(china_bev_stats)

# Guard: fail loudly and clearly if this combo doesn't exist in slopes_gated,
# rather than getting a cryptic downstream error again
if(nrow(china_bev_stats) == 0) {
  stop("No row for Fuel_Group == 'BEV' & Region == 'China' in slopes_gated. ",
       "Check that fuel_region_share has at least 3 years of China BEV data.")
}

# Create plot
p_china_bev_projection <- ggplot() +
  # Historical line
  geom_line(data = china_bev_historical, 
            aes(x = Data_Year, y = Global_Share), 
            color = "#1F77B4", size = 1.2) +
  # Historical points
  geom_point(data = china_bev_historical, 
             aes(x = Data_Year, y = Global_Share), 
             color = "#1F77B4", size = 3) +
  # Projection point
  geom_point(data = projection_data, 
             aes(x = Data_Year, y = Global_Share), 
             color = "#D62728", size = 4) +
  # Confidence interval
  geom_errorbar(data = ci_data,
                aes(x = Data_Year, ymin = ymin, ymax = ymax), 
                width = 0.3, color = "#D62728", size = 1.2) +
  # Labels
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
                      " pp/year (R\u00b2 = ", round(china_bev_stats$R_Squared, 3), 
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



# Step 15: Also create a faceted plot for all BEV regions
cat("\n=== FACETED PLOT: ALL BEV REGIONS ===\n")

# Get BEV data for all regions
bev_all_regions <- fuel_region_share %>%
  filter(Fuel_Group == "BEV") %>%
  select(Data_Year, Region, Global_Share)

# Get 2025 projections for BEV
bev_2025_data <- slope_lookup %>%
  filter(Fuel_Group == "BEV") %>%
  mutate(
    Projected_2025 = Last_Value + Projection_Base * 1,
    Projected_2025_Plus = Last_Value + Projection_Plus * 1,
    Projected_2025_Minus = Last_Value + Projection_Minus * 1
  ) %>%
  select(Region, Projected_2025, Projected_2025_Plus, Projected_2025_Minus, Trend_Quality)

# Plot all BEV regions
p_bev_all <- bev_all_regions %>%
  ggplot(aes(x = Data_Year, y = Global_Share, color = Region)) +
  geom_line(size = 1) +
  geom_point(size = 2.5) +
  # Add projection points
  geom_point(data = bev_2025_data %>%
               left_join(bev_all_regions %>% 
                           filter(Data_Year == max(Data_Year)) %>% 
                           select(Region, Last_Value = Global_Share), by = "Region"),
             aes(x = 2025, y = Projected_2025), size = 3, shape = 17) +
  facet_wrap(~Region, scales = "free_y") +
  labs(
    # title = "BEV Global Share by Region (2014-2025)",
    #  subtitle = "Points show 2025 projections (triangles)",
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

# Step 16: Create a summary visualization of which trends are used
cat("\n=== TREND USAGE SUMMARY PLOT ===\n")

p_trend_usage <- slope_lookup %>%
  mutate(
    Status = ifelse(Trend_Quality %in% c("Strong", "Moderate"), 
                    "Trend Used", "Held Flat"),
    Label = paste(Fuel_Group, Region, sep="-")
  ) %>%
  ggplot(aes(x = reorder(Label, R_Squared), y = R_Squared, fill = Status)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  coord_flip() +
  scale_fill_manual(values = c("Trend Used" = "#2CA02C", "Held Flat" = "#D62728")) +
  labs(
    title = "R² Values by Region × Fuel_Group",
    subtitle = paste("Threshold = 0.5 (Red line) | Trends above are used; below are held flat"),
    x = "Region × Fuel_Group",
    y = "R² (proportion of variance explained)",
    fill = "Status"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_trend_usage)

# Step 17: Save final results
cat("\n=== FINAL SUMMARY ===\n")
cat("========================================\n")
cat("CHINA BEV GLOBAL SHARE PROJECTION\n")
cat("========================================\n")
cat("Historical slope (2014-2024):", 3.62, "pp/year\n")
cat("R²:", 0.771, "(Strong trend)\n")
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



# ============================================================
# FULL CORRECTED ORIGIN-PROJECTION PIPELINE
# Assumes these objects already exist from earlier in your script:
#   data                - your main vehicle microdata
#   sales_by_fuel_smmt   - New_Sales by Year/Fuel_Group (from your sales forecast)
# ============================================================

library(tidyverse)

proj_years <- 2025:2037

# ------------------------------------------------------------
# STEP 1: Historical origin shares, by Region and by Fuel_Group
# ------------------------------------------------------------

origin_by_fuel <- data %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year, Region, Fuel_Group, Origin) %>%
  summarise(Reg = sum(Fleet_Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Data_Year, Fuel_Group) %>%
  mutate(Share_Within_Fuel = 100 * Reg / sum(Reg)) %>%
  ungroup()

# Region-level (collapsed across Origin), used for the regressions
fuel_region_share <- origin_by_fuel %>%
  group_by(Data_Year, Fuel_Group, Region) %>%
  summarise(Vehicles = sum(Reg, na.rm = TRUE), .groups = "drop") %>%
  group_by(Data_Year, Fuel_Group) %>%
  mutate(Global_Share = 100 * Vehicles / sum(Vehicles, na.rm = TRUE)) %>%
  ungroup()

# Origin composition WITHIN each Region x Fuel_Group, using the latest year -
# used only to split a region's total back out into individual Origin countries
origin_within_region <- origin_by_fuel %>%
  filter(Data_Year == max(Data_Year)) %>%
  group_by(Fuel_Group, Region) %>%
  mutate(Origin_Share_of_Region = 100 * Reg / sum(Reg)) %>%
  ungroup() %>%
  select(Fuel_Group, Region, Origin, Origin_Share_of_Region)

# ------------------------------------------------------------
# STEP 2: Quality-gated slopes (R-squared, standard error, per Region x Fuel_Group)
# ------------------------------------------------------------

R2_THRESHOLD <- 0.5   # trend must explain at least half the variance to be trusted
Z_BAND       <- 1.96  # 95% CI multiplier for Plus/Minus

slopes_detailed <- fuel_region_share %>%
  group_by(Fuel_Group, Region) %>%
  filter(n() >= 3) %>%                      # need >=3 points to fit a trend at all
  summarise(
    n_obs      = n(),
    model      = list(lm(Global_Share ~ Data_Year)),
    Last_Value = Global_Share[Data_Year == max(Data_Year)],
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    Slope_pp  = coef(model)[["Data_Year"]],
    Std_Error = summary(model)$coefficients["Data_Year", "Std. Error"],
    R_Squared = summary(model)$r.squared
  ) %>%
  ungroup() %>%
  select(-model)

# combinations with < 3 years of data (e.g. HEV-China) - held flat, not modelled
insufficient_data <- fuel_region_share %>%
  group_by(Fuel_Group, Region) %>%
  filter(n() < 3) %>%
  summarise(Last_Value = Global_Share[Data_Year == max(Data_Year)],
            n_obs = n(), .groups = "drop") %>%
  mutate(Slope_pp = 0, Std_Error = 0, R_Squared = NA_real_)

slopes_all <- bind_rows(slopes_detailed, insufficient_data)

slope_lookup <- slopes_all %>%
  mutate(
    Trend_Quality = case_when(
      is.na(R_Squared)        ~ "Insufficient Data",
      R_Squared >= 0.7        ~ "Strong",
      R_Squared >= R2_THRESHOLD ~ "Moderate",
      R_Squared >= 0.3        ~ "Weak",
      TRUE                    ~ "Noise"
    ),
    Trusted     = !is.na(R_Squared) & R_Squared >= R2_THRESHOLD,
    Slope_Base  = ifelse(Trusted, Slope_pp, 0),
    Slope_Plus  = ifelse(Trusted, Slope_pp + Z_BAND * Std_Error, 0),
    Slope_Minus = ifelse(Trusted, Slope_pp - Z_BAND * Std_Error, 0)
  ) %>%
  select(Fuel_Group, Region, Last_Value, Slope_Base, Slope_Plus, Slope_Minus,
         Slope_pp, R_Squared, Std_Error, Trend_Quality, n_obs)

cat("\n=== SLOPE LOOKUP (quality-gated) ===\n")
print(slope_lookup, n = Inf)

cat("\n=== Trend quality summary ===\n")
slope_lookup %>% count(Trend_Quality) %>% print()

origin_slopes_gated <- slope_lookup %>%
  select(Fuel_Group, Region, last_share = Last_Value, Slope_Base, Slope_Plus, Slope_Minus)

# ------------------------------------------------------------
# STEP 3: TWO ceiling scenarios for China/Japan/Korea BEV caps
# (this is the assumption we found is doing most of the work in the
#  "EU declines" finding - test sensitivity to it explicitly)
# ------------------------------------------------------------

region_bounds_original <- tribble(
  ~Region,  ~Fuel_Group, ~floor, ~ceiling,
  "China",  "BEV",        0,      55,
  "China",  "PHEV",       0,      30,
  "China",  "HEV",        0,      15,
  "China",  "Petrol",     0,      35,
  "China",  "Diesel",     0,      5,
  "Japan",  "BEV",        0,      12,
  "Japan",  "PHEV",       0,      35,
  "Japan",  "HEV",        0,      30,
  "Japan",  "Petrol",     0,      10,
  "Japan",  "Diesel",     0,      2,
  "Korea",  "BEV",        0,      10,
  "Korea",  "PHEV",       0,      35,
  "Korea",  "HEV",        0,      30,
  "Korea",  "Petrol",     0,      15,
  "Korea",  "Diesel",     0,      2
)

# Conservative variant: lower China's BEV ceiling substantially (55 -> 40),
# leaving everything else the same, to test how much of the EU-decline
# finding depends on this one number
region_bounds_conservative <- region_bounds_original %>%
  mutate(ceiling = ifelse(Region == "China" & Fuel_Group == "BEV", 40, ceiling))

# ------------------------------------------------------------
# STEP 4: Corrected build_trajectory() - takes a ceiling table as an
# explicit argument, so you can compare scenarios side by side. No more
# scenario_china_mult / china_decel() - Plus/Minus already come from each
# region-fuel pair's own regression uncertainty in origin_slopes_gated.
# ------------------------------------------------------------

build_trajectory <- function(scenario_name, bounds_table) {
  
  slope_col <- switch(scenario_name,
                      "Base"  = "Slope_Base",
                      "Plus"  = "Slope_Plus",
                      "Minus" = "Slope_Minus",
                      stop("Unknown scenario: ", scenario_name)
  )
  
  raw <- origin_slopes_gated %>%
    mutate(slope_used = .data[[slope_col]]) %>%
    tidyr::crossing(Year = proj_years) %>%
    mutate(
      years_ahead = Year - 2024,
      raw_share   = last_share + slope_used * years_ahead
    ) %>%
    left_join(bounds_table, by = c("Region", "Fuel_Group")) %>%
    mutate(
      has_cap     = !is.na(ceiling) & ceiling < 100,
      raw_clamped = pmax(coalesce(floor, 0), pmin(coalesce(ceiling, 100), pmax(0.001, raw_share)))
    )
  
  capped <- raw %>% filter(has_cap) %>% mutate(Region_Share = raw_clamped)
  
  uncapped <- raw %>% filter(!has_cap) %>%
    left_join(
      capped %>% group_by(Fuel_Group, Year) %>%
        summarise(Capped_Total = sum(Region_Share), .groups = "drop"),
      by = c("Fuel_Group", "Year")
    ) %>%
    mutate(Capped_Total = coalesce(Capped_Total, 0)) %>%
    group_by(Fuel_Group, Year) %>%
    mutate(Region_Share = pmax(0, 100 - Capped_Total) * raw_clamped / sum(raw_clamped)) %>%
    ungroup()
  
  bind_rows(capped, uncapped) %>%
    select(Fuel_Group, Region, Year, Region_Share) %>%
    left_join(origin_within_region, by = c("Fuel_Group", "Region"), relationship = "many-to-many") %>%
    mutate(
      Origin_Share_of_Region = coalesce(Origin_Share_of_Region, 100),
      Share_exact = Region_Share * Origin_Share_of_Region / 100,
      Share = round(Share_exact, 1),
      Scenario = scenario_name
    ) %>%
    select(Fuel_Group, Region, Origin, Year, Share, Share_exact, Scenario)
}

# ------------------------------------------------------------
# STEP 5: Run both ceiling scenarios
# ------------------------------------------------------------

origin_scenarios_year <- bind_rows(
  build_trajectory("Base",  region_bounds_original),
  build_trajectory("Plus",  region_bounds_original),
  build_trajectory("Minus", region_bounds_original)
)


origin_scenarios_year_conservative <- bind_rows(
  build_trajectory("Base",  region_bounds_conservative),
  build_trajectory("Plus",  region_bounds_conservative),
  build_trajectory("Minus", region_bounds_conservative)
)

# ------------------------------------------------------------
# STEP 6: Validation - China BEV 2025 should match the hand-calculated
# values from slope_lookup (Base ~30.9%, 95% CI ~29.3-33.7%)
# ------------------------------------------------------------

origin_scenarios_year %>%
  filter(Fuel_Group == "BEV", Year == 2025) %>%
  # Aggregate across Origins within each Region
  group_by(Scenario, Region) %>%
  summarise(
    Share_exact = sum(Share_exact, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Now pivot to wide format
  pivot_wider(
    id_cols = Scenario,
    names_from = Region,
    values_from = Share_exact,
    values_fill = 0
  ) %>%
  print()

# ------------------------------------------------------------
# STEP 7: Volume-weighted total origin share (corrects the earlier
# unweighted-average bug that overstated China's speed and understated EU/UK)
# ------------------------------------------------------------

sales_by_fuel_volume <- sales_by_fuel_smmt %>%
  select(Year, Fuel_Group, Total_Sales_Fuel = New_Sales)

make_weighted_origin_table <- function(scenario_data, scenario_filter = "Base") {
  scenario_data %>%
    filter(Scenario == scenario_filter) %>%
    select(Year, Fuel_Group, Region, Share_exact) %>%
    group_by(Year, Fuel_Group, Region) %>%
    summarise(Origin_Share = sum(Share_exact), .groups = "drop") %>%
    left_join(sales_by_fuel_volume, by = c("Year", "Fuel_Group")) %>%
    mutate(
      Vehicles_From_Region = Origin_Share / 100 * Total_Sales_Fuel,
      Vehicles_From_Region = ifelse(is.na(Vehicles_From_Region), 0, Vehicles_From_Region)
    ) %>%
    group_by(Year, Region) %>%
    summarise(Vehicles = sum(Vehicles_From_Region, na.rm = TRUE), .groups = "drop") %>%
    group_by(Year) %>%
    mutate(Share = round(100 * Vehicles / sum(Vehicles), 1)) %>%
    ungroup() %>%
    select(Year, Region, Share) %>%
    pivot_wider(names_from = Region, values_from = Share, values_fill = 0) %>%
    mutate(Total = rowSums(across(where(is.numeric) & !matches("Year")), na.rm = TRUE))
}

origin_shares_weighted_original     <- make_weighted_origin_table(origin_scenarios_year, "Base")
origin_shares_weighted_conservative <- make_weighted_origin_table(origin_scenarios_year_conservative, "Base")

cat("\n=== VOLUME-WEIGHTED TOTAL ORIGIN SHARE - ORIGINAL CEILINGS (China BEV cap = 55%) ===\n")
print(origin_shares_weighted_original, n = Inf)

cat("\n=== VOLUME-WEIGHTED TOTAL ORIGIN SHARE - CONSERVATIVE CEILINGS (China BEV cap = 40%) ===\n")
print(origin_shares_weighted_conservative, n = Inf)

# ------------------------------------------------------------
# STEP 8: Sensitivity diagnostic - how much does EU's fate actually
# depend on the China BEV ceiling assumption?
# ------------------------------------------------------------

eu_sensitivity <- origin_shares_weighted_original %>%
  select(Year, EU_Original = EU) %>%
  left_join(origin_shares_weighted_conservative %>% select(Year, EU_Conservative = EU), by = "Year") %>%
  mutate(Difference = EU_Conservative - EU_Original)

cat("\n=== EU SHARE: ORIGINAL vs CONSERVATIVE CHINA-BEV-CEILING ASSUMPTION ===\n")
print(eu_sensitivity, n = Inf)

# Unconstrained comparison: what EU's OWN trend alone would predict for BEV,
# with no ceiling squeeze at all - shows the gap the ceiling assumption creates
cat("\n=== EU's own BEV trend, unconstrained by any ceiling ===\n")
origin_slopes_gated %>%
  filter(Fuel_Group == "BEV", Region == "EU") %>%
  tidyr::crossing(Year = c(2025, 2030, 2035, 2037)) %>%
  mutate(years_ahead = Year - 2024,
         Unconstrained_EU_BEV_Share = round(last_share + Slope_Base * years_ahead, 1)) %>%
  select(Year, Unconstrained_EU_BEV_Share) %>%
  print()

# ------------------------------------------------------------
# STEP 9: Plot - EU trajectory under both ceiling assumptions
# ------------------------------------------------------------

p_eu_sensitivity <- bind_rows(
  origin_shares_weighted_original %>% select(Year, EU) %>% mutate(Ceiling = "Original (China BEV = 55%)"),
  origin_shares_weighted_conservative %>% select(Year, EU) %>% mutate(Ceiling = "Conservative (China BEV = 40%)")
) %>%
  ggplot(aes(x = Year, y = EU, color = Ceiling)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2025, 2037, 2)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "EU's Overall Origin Share: Sensitivity to China's Assumed BEV Ceiling",
    subtitle = "Volume-weighted across all fuel types",
    x = "Year", y = "EU Share of New Vehicle Sales (%)", color = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p_eu_sensitivity)

# ------------------------------------------------------------
# STEP 10: Downstream object needed elsewhere in your pipeline
# (uses the ORIGINAL ceiling scenario / Base case as the primary result)
# ------------------------------------------------------------

origin_sales_distribution <- origin_scenarios_year %>%
  filter(Scenario == "Base") %>%
  select(Year, Fuel_Group, Region, Origin, Origin_Share = Share_exact) %>%
  arrange(Fuel_Group, Year)




# Calculate volume-weighted origin shares
origin_shares_weighted <- origin_scenarios_year %>%
  filter(Scenario == "Base", Year >= 2025, Year <= 2037) %>%
  left_join(sales_by_fuel_volume, by = c("Year", "Fuel_Group")) %>%
  mutate(
    # Convert Share_exact from percentage to proportion
    Share_Proportion = Share_exact / 100,
    # Calculate actual vehicles from this origin-fuel combination
    Vehicles_From_Origin = Share_Proportion * Total_Sales_Fuel,
    # Handle NAs
    Vehicles_From_Origin = ifelse(is.na(Vehicles_From_Origin), 0, Vehicles_From_Origin)
  ) %>%
  group_by(Year, Region) %>%
  summarise(
    Total_Vehicles = sum(Vehicles_From_Origin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Year) %>%
  mutate(
    Share = 100 * Total_Vehicles / sum(Total_Vehicles, na.rm = TRUE),
    Share = round(Share, 1)
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Region,
    values_from = Share,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(across(where(is.numeric) & !matches("Year")), na.rm = TRUE))

# Compare old (unweighted) vs new (weighted)
cat("\n=== COMPARISON: UNWEIGHTED VS WEIGHTED ORIGIN SHARES ===\n")
cat("\nUNWEIGHTED (treats all fuel types equally):\n")


cat("\nWEIGHTED (by actual sales volume):\n")
print(origin_shares_weighted, n = Inf)
# Diagnostic 1: Check how much the capped regions are squeezing the pie
capped_share_by_year <- origin_scenarios_year %>%
  filter(Scenario == "Base", Year >= 2025, Year <= 2037) %>%
  left_join(sales_by_fuel_volume, by = c("Year", "Fuel_Group")) %>%
  mutate(
    Share_Proportion = Share_exact / 100,
    Vehicles_From_Origin = Share_Proportion * Total_Sales_Fuel,
    Vehicles_From_Origin = ifelse(is.na(Vehicles_From_Origin), 0, Vehicles_From_Origin)
  ) %>%
  group_by(Year, Fuel_Group) %>%
  mutate(
    Total_Fuel_Sales = sum(Total_Sales_Fuel, na.rm = TRUE),
    Region_Share_Of_Fuel = 100 * Vehicles_From_Origin / Total_Fuel_Sales
  ) %>%
  ungroup() %>%
  # Identify capped regions
  mutate(
    Is_Capped = Region %in% c("China", "Japan", "Korea")
  ) %>%
  group_by(Year, Fuel_Group, Is_Capped) %>%
  summarise(
    Total_Share = sum(Region_Share_Of_Fuel, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Is_Capped,
    values_from = Total_Share,
    names_prefix = "Capped_"
  ) %>%
  mutate(
    Capped_Total = coalesce(Capped_TRUE, 0),
    Uncapped_Total = coalesce(Capped_FALSE, 0),
    Remaining_Pie = 100 - Capped_Total
  )

cat("\n=== DIAGNOSTIC 1: CAPPED VS UNCAPPED SHARES BY FUEL TYPE ===\n")
print(capped_share_by_year, n = Inf)


# Diagnostic 2: Check UK's historical trends by fuel type
uk_trends <- origin_by_fuel %>%
  filter(Region == "UK", Data_Year %in% 2019:2024) %>%
  group_by(Fuel_Group) %>%
  summarise(
    last_share = Share_Within_Fuel[Data_Year == max(Data_Year)][1],
    slope_pp = if (n() >= 2) coef(lm(Share_Within_Fuel ~ Data_Year))[2] else 0,
    .groups = "drop"
  )

cat("\n=== DIAGNOSTIC 2: UK HISTORICAL TRENDS BY FUEL TYPE ===\n")
print(uk_trends, n = Inf)

# Calculate volume-weighted fuel mix by region
fuel_mix_by_region_weighted <- origin_sales_distribution %>%
  left_join(sales_by_fuel_volume, by = c("Year", "Fuel_Group")) %>%
  mutate(
    Vehicles_From_Region = Origin_Share / 100 * Total_Sales_Fuel,
    # Handle any NAs
    Vehicles_From_Region = ifelse(is.na(Vehicles_From_Region), 0, Vehicles_From_Region)
  ) %>%
  group_by(Year, Region) %>%
  mutate(
    Percent_of_Total = 100 * Vehicles_From_Region / sum(Vehicles_From_Region, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(Year, Region, Fuel_Group, Percent_of_Total) %>%
  # FIX: Use a single value for values_fill (not a list)
  group_by(Year, Region, Fuel_Group) %>%
  summarise(
    Percent_of_Total = sum(Percent_of_Total, na.rm = TRUE),
    .groups = "drop"
  )%>%
  pivot_wider(
    names_from = Fuel_Group,
    values_from = Percent_of_Total,
    values_fill = list(Percent_of_Total = 0)
  ) %>%
  mutate(
    Total = rowSums(select(., where(is.numeric), -Year), na.rm = TRUE)
  )
print(fuel_mix_by_region_weighted)
# Check results for China
cat("\n=== VOLUME-WEIGHTED FUEL MIX FOR CHINA (2025-2030) ===\n")
fuel_mix_by_region_weighted %>%
  filter(Region == "China", Year %in% 2025:2037) %>%
  select(Year, BEV, PHEV, HEV, Petrol, Diesel, Total) %>%
  print(n = Inf)


# calculate volume weighted fule by year

origin_sales_distribution %>%
  filter(Year == 2025) %>%
  group_by(Region, Fuel_Group) %>%
  summarise(Origin_Share = sum(Origin_Share, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = Region,
    names_from = Fuel_Group,
    values_from = Origin_Share,
    values_fill = 0
  ) %>%
  mutate(
    Total = rowSums(across(where(is.numeric)), na.rm = TRUE)
  ) %>%
  print()


data %>%
  filter(Vehicle_Age == 0,Data_Year==2024) %>%
  group_by(
    Region,
    Fuel_Group
  ) %>%
  summarise(
    Reg = sum(Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Fuel_Group) %>%
  mutate(
    Share_Within_Fuel = 100 * Reg / sum(Reg)
  ) %>%
  ungroup()%>%
  select(Region, Fuel_Group, Share_Within_Fuel) %>%
  pivot_wider(
    id_cols = Region,
    names_from = Fuel_Group,
    values_from = Share_Within_Fuel,
    values_fill = 0
  ) %>%
  mutate(
    Total = rowSums(across(where(is.numeric)), na.rm = TRUE))%>%
  print()



# ============================================================
# TABLE 4: STATISTICAL TREND QUALITY
# ============================================================

trend_quality_table <- slope_lookup %>%
  filter(Trend_Quality %in% c("Strong", "Moderate")) %>%
  mutate(
    Status = ifelse(Trend_Quality == "Strong", "✅ Strong", "⚠️ Moderate"),
    R_Squared = round(R_Squared, 3),
    Slope_Display = paste0(round(Slope_pp, 2), " pp/yr"),
    CI_Display = paste0(
      round(Last_Value + Slope_Minus, 1), " - ",
      round(Last_Value + Slope_Plus, 1)
    )
  ) %>%
  select(
    Fuel_Group, 
    Region, 
    Status, 
    R_Squared, 
    Slope_Display, 
    Last_Value, 
    CI_Display
  ) %>%
  arrange(Fuel_Group, desc(R_Squared))

cat("\n=== TABLE 4: STATISTICAL TREND QUALITY ===\n")
print(trend_quality_table, n = Inf)
#-----------------------------
# additional analysis for paper 
#----------------------------

# Extract and format origin shares for new vehicles
origin_shares_table <- origin_scenarios_year %>%
  filter(Scenario == "Base", Year >= 2025, Year <= 2037) %>%
  group_by(Year, Region) %>%
  summarise(Share = sum(Share_exact, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(
    Share = 100 * Share / sum(Share),  # Normalize to 100% per year
    Share = round(Share, 1)
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Region,
    values_from = Share,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(across(where(is.numeric) & !matches("Year")), na.rm = TRUE))

# Print the table
print(origin_shares_table, n = Inf)



# Fuel-specific origin shares for 2030
fuel_origin_2030 <- origin_scenarios_year %>%
  filter(Scenario == "Base", Year == 2030) %>%
  group_by(Fuel_Group, Region) %>%
  summarise(Share_exact = sum(Share_exact, na.rm = TRUE), .groups = "drop") %>%  # Aggregate duplicates
  pivot_wider(
    names_from = Region,
    values_from = Share_exact,
    values_fill = 0
  ) %>%
  mutate(Total = round(rowSums(across(where(is.numeric))), 1))

print(fuel_origin_2030, n = Inf)



# Extract China BEV share across scenarios
china_bev_scenarios <- origin_scenarios_year %>%
  filter(Fuel_Group == "BEV", Region == "China", Year >= 2025, Year <= 2037) %>%
  select(Year, Scenario, Share_exact) %>%
  pivot_wider(
    names_from = Scenario,
    values_from = Share_exact
  )

print(china_bev_scenarios, n = Inf)


# Impact on all regions in 2030
bev_scenario_2030 <- origin_scenarios_year %>%
  filter(Fuel_Group == "BEV", Year == 2030) %>%
  group_by(Scenario, Region) %>%
  summarise(Share = sum(Share, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Scenario,
    values_from = Share,
    values_fill = 0
  ) %>%
  mutate(
    Change_Plus = Plus - Base,
    Change_Minus = Minus - Base
  )

print(bev_scenario_2030)

#------------------------------------------------
# Visualization: Compare unweighted vs weighted
#------------------------------------------------

# Calculate the change from 2025 to 2037 for key regions (FIXED)
# First, aggregate by Year and Region properly
changes <- origin_shares_weighted %>%
  # Pivot to long format first
  pivot_longer(
    cols = c(China, EU, UK, Japan, Korea, Other, USA),
    names_to = "Region",
    values_to = "Share"
  ) %>%
  # Aggregate by Year and Region (summing across fuel groups)
  group_by(Year, Region) %>%
  summarise(
    Share = sum(Share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Filter to years we want
  filter(Year %in% c(2025, 2037)) %>%
  # Pivot wider to get 2025 and 2037 as columns
  pivot_wider(
    id_cols = Region,
    names_from = Year,
    values_from = Share,
    values_fill = 0
  ) %>%
  # Calculate changes
  mutate(
    Change = `2037` - `2025`,
    Pct_Change = ifelse(`2025` != 0, 100 * Change / `2025`, NA)
  )

cat("\n=== CHANGES 2025 → 2037 (WEIGHTED) ===\n")
print(changes, n = Inf)

# Now create the clean summary table (aggregated by region)
origin_shares_summary <- origin_shares_weighted %>%
  pivot_longer(
    cols = c(China, EU, UK, Japan, Korea, Other, USA),
    names_to = "Region",
    values_to = "Share"
  ) %>%
  group_by(Year, Region) %>%
  summarise(
    Share = sum(Share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = Year,
    names_from = Region,
    values_from = Share,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(across(where(is.numeric) & !matches("Year")), na.rm = TRUE))

cat("\n=== CLEAN ORIGIN SHARES SUMMARY (WEIGHTED, AGGREGATED BY REGION) ===\n")
print(origin_shares_summary, n = Inf)

# Calculate changes from the summary
changes_summary <- origin_shares_summary %>%
  filter(Year %in% c(2025, 2037)) %>%
  pivot_longer(
    cols = c(China, EU, UK, Japan, Korea),
    names_to = "Region",
    values_to = "Share"
  ) %>%
  pivot_wider(
    id_cols = Region,
    names_from = Year,
    values_from = Share,
    values_fill = 0
  ) %>%
  mutate(
    Change = `2037` - `2025`,
    Pct_Change = ifelse(`2025` != 0, 100 * Change / `2025`, NA)
  )

cat("\n=== CHANGES 2025 → 2037 (FROM SUMMARY) ===\n")
print(changes_summary, n = Inf)

# Now the clean comparison visualization
comparison_data_clean <- bind_rows(
  # Unweighted (old, buggy)
  origin_shares_table %>%
    select(Year, China, EU, UK, Japan, Korea) %>%
    pivot_longer(cols = c(China, EU, UK, Japan, Korea),
                 names_to = "Region",
                 values_to = "Share") %>%
    mutate(Type = "Unweighted"),
  # Weighted (new, corrected)
  origin_shares_summary %>%
    select(Year, China, EU, UK, Japan, Korea) %>%
    pivot_longer(cols = c(China, EU, UK, Japan, Korea),
                 names_to = "Region",
                 values_to = "Share") %>%
    mutate(Type = "Weighted")
)

# Method 1: Filter the comparison_data_clean correctly
weighted_data <- comparison_data_clean %>%
  filter(Type == "Weighted")
print(weighted_data,n=Inf)



# Plot comparison
p_comparison_clean <- comparison_data_clean %>%
  ggplot(aes(x = Year, y = Share, color = Region, linetype = Type)) +
  geom_line(size = 1.2) +
  facet_wrap(~Type, ncol = 2) +
  scale_color_manual(values = c(
    "China" = "#D62728",
    "EU" = "#1F77B4",
    "UK" = "#FF7F0E",
    "Japan" = "#2CA02C",
    "Korea" = "#9467BD"
  )) +
  scale_x_continuous(breaks = seq(2025, 2037, 2)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 80)) +
  labs(
    title = "Origin Share Evolution: Unweighted vs Volume-Weighted",
    subtitle = "Weighted approach accounts for declining fuel types (Petrol/Diesel)",
    x = "Year",
    y = "Share of New Vehicle Sales (%)",
    color = "Region",
    linetype = "Calculation Method"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11)
  )

print(p_comparison_clean)



# ============================================================
# PLOT: WEIGHTED ORIGIN SHARE EVOLUTION (ONLY)
# ============================================================

p_weighted_only <- weighted_data %>%
  ggplot(aes(x = Year, y = Share, color = Region)) +
  geom_line(size = 1.2, linetype = "dashed") +
  geom_point(size = 2) +
  geom_vline(xintercept = 2030, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c(
    "China" = "#D62728",
    "EU" = "#1F77B4",
    "UK" = "#FF7F0E",
    "Japan" = "#2CA02C",
    "Korea" = "#9467BD"
  )) +
  scale_x_continuous(breaks = seq(2025, 2037, 2)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 80)) +
  labs(
    # title = "Origin Share Evolution (Volume-Weighted)",
    #subtitle = "China rises from 11.3% (2025) to 55% (2035), EU declines from 63.7% to 23.1%",
    x = "Year",
    y = "Share of New Vehicle Sales (%)",
    color = "Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

print(p_weighted_only)



# Paper-ready table (selected years)
paper_table <- origin_shares_summary %>%
  filter(Year %in% c(2025, 2030, 2035, 2037)) %>%
  select(Year, China, EU, UK, Japan, Korea, Total) %>%
  mutate(across(where(is.numeric), ~ round(., 1)))

cat("\n=== PAPER-READY TABLE (SELECTED YEARS) ===\n")
print(paper_table, n = Inf)

# Create a LaTeX-ready table (for your paper)
library(knitr)
library(kableExtra)

# kable(paper_table, format = "latex", booktabs = TRUE, 
#       caption = "Volume-Weighted Origin Shares of New Vehicle Sales (Base Scenario)") %>%
#   kable_styling(latex_options = c("striped", "hold_position"))

# Compare weighted vs unweighted at key years
comparison_table <- origin_shares_summary %>%
  filter(Year %in% c(2025, 2030, 2035, 2037)) %>%
  select(Year, China_W = China, EU_W = EU, UK_W = UK, Japan_W = Japan, Korea_W = Korea) %>%
  left_join(
    origin_shares_table %>%
      filter(Year %in% c(2025, 2030, 2035, 2037)) %>%
      select(Year, China_U = China, EU_U = EU, UK_U = UK, Japan_U = Japan, Korea_U = Korea),
    by = "Year"
  ) %>%
  mutate(
    China_Diff = China_W - China_U,
    EU_Diff = EU_W - EU_U,
    UK_Diff = UK_W - UK_U
  )

cat("\n=== COMPARISON: WEIGHTED VS UNWEIGHTED AT KEY YEARS ===\n")
print(comparison_table, n = Inf)

# Final diagnostic: Check what's actually happening with fuel types
cat("\n=== DIAGNOSTIC: FUEL TYPE SALES VOLUMES OVER TIME ===\n")
sales_by_fuel_volume %>%
  filter(Year %in% 2025:2030) %>%
  pivot_wider(
    id_cols = Year,
    names_from = Fuel_Group,
    values_from = Total_Sales_Fuel
  ) %>%
  print(n = Inf)

# Check which fuel types are dominating in 2030
fuel_volume_2030 <- sales_by_fuel_volume %>%
  filter(Year == 2030) %>%
  mutate(Share_of_Total = 100 * Total_Sales_Fuel / sum(Total_Sales_Fuel))

cat("\n=== FUEL TYPE SHARES IN 2030 ===\n")
print(fuel_volume_2030, n = Inf)

# Create a combined visualization showing the mechanism
library(ggplot2)
library(patchwork)
library(tidyr)

# Plot 1: Fuel mix evolution
fuel_mix_plot <- sales_by_fuel_volume %>%
  filter(Year >= 2025, Year <= 2037) %>%
  group_by(Year) %>%
  mutate(Share = 100 * Total_Sales_Fuel / sum(Total_Sales_Fuel)) %>%
  ggplot(aes(x = Year, y = Share, fill = Fuel_Group)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(values = c(
    "BEV" = "#2CA02C",
    "PHEV" = "#FFD700",
    "HEV" = "#FF7F0E",
    "Petrol" = "#D62728",
    "Diesel" = "#9467BD"
  )) +
  scale_x_continuous(breaks = seq(2025, 2037, 2)) +
  labs(
    title = "A) Fuel Mix Evolution",
    x = "Year",
    y = "Share of New Sales (%)",
    fill = "Fuel Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )
print(fuel_mix_plot)
# Plot 2: Origin shares (weighted)
origin_plot <- origin_shares_summary %>%
  select(Year, China, EU, UK, Japan, Korea) %>%
  pivot_longer(cols = c(China, EU, UK, Japan, Korea),
               names_to = "Region",
               values_to = "Share") %>%
  ggplot(aes(x = Year, y = Share, color = Region)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2030, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c(
    "China" = "#D62728",
    "EU" = "#1F77B4",
    "UK" = "#FF7F0E",
    "Japan" = "#2CA02C",
    "Korea" = "#9467BD"
  )) +
  scale_x_continuous(breaks = seq(2025, 2037, 2)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 80)) +
  labs(
    title = "B) Origin Share Evolution (Volume-Weighted)",
    x = "Year",
    y = "Share of New Sales (%)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )
print(origin_plot)
# Combine plots
combined_plot <- fuel_mix_plot / origin_plot +
  plot_annotation(
    title = "The Mechanism: BEV Dominance Drives Origin Share Shifts",
    subtitle = "As BEV reaches 80% of sales by 2030, China's BEV production advantage increases its total share",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

print(combined_plot)


#---------------------------------------------------------------------------------

# ============================================
# UPDATED FIGURE 1: Regional Origin Share Evolution
# ============================================
library(ggplot2)
library(tidyr)

p_origin_evolution <- origin_shares_summary %>%
  select(Year, China, EU, UK, Japan, Korea) %>%
  pivot_longer(cols = c(China, EU, UK, Japan, Korea),
               names_to = "Region",
               values_to = "Share") %>%
  mutate(
    Region = factor(Region, levels = c("EU", "China", "UK", "Korea", "Japan"))
  ) %>%
  ggplot(aes(x = Year, y = Share, color = Region, group = Region)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray50", alpha = 0.5) +
  scale_color_manual(values = c(
    "EU" = "#1F77B4",
    "China" = "#D62728",
    "UK" = "#FF7F0E",
    "Korea" = "#9467BD",
    "Japan" = "#2CA02C"
  )) +
  scale_x_continuous(breaks = seq(2025, 2037, 2)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), 
                     breaks = seq(0, 80, 10)) +
  labs(
    title = "Regional Origin Share Evolution (2025-2037)",
    subtitle = "Base Scenario: EU declines from 61.2% to 54.1%, China grows from 9.1% to 16.4%",
    x = "Year",
    y = "Share of New Vehicle Sales (%)",
    color = "Manufacturing Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

print(p_origin_evolution)



# ============================================
# UPDATED FIGURE 2: Fuel-Specific Origin Shares
# ============================================


# Create fuel-origin shares for 2030 using WEIGHTED data
fuel_origin_2030_weighted <- origin_scenarios_year %>%
  filter(Scenario == "Base", Year == 2030) %>%
  group_by(Fuel_Group, Region) %>%
  summarise(Share = round(sum(Share_exact, na.rm = TRUE), 1), .groups = "drop") %>%
  pivot_wider(
    names_from = Region,
    values_from = Share,
    values_fill = 0
  )

# Also calculate the fuel mix for context
fuel_mix_2030 <- sales_by_fuel_volume %>%
  filter(Year == 2030) %>%
  mutate(
    Fuel_Share = 100 * Total_Sales_Fuel / sum(Total_Sales_Fuel),
    Fuel_Label = paste0(Fuel_Group, " (", round(Fuel_Share, 0), "%)")
  )

print("Fuel mix in 2030:")
print(fuel_mix_2030)

p_fuel_origin_updated <- fuel_origin_2030_weighted %>%
  select(Fuel_Group, China, EU, UK, Japan, Korea) %>%
  pivot_longer(cols = c(China, EU, UK, Japan, Korea),
               names_to = "Region",
               values_to = "Share") %>%
  mutate(
    Region = factor(Region, levels = c("EU", "China", "UK", "Japan", "Korea")),
    Fuel_Group = factor(Fuel_Group, levels = c("BEV", "PHEV", "HEV", "Diesel", "Petrol"))
  ) %>%
  ggplot(aes(x = Fuel_Group, y = Share, fill = Region)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_text(aes(label = ifelse(Share > 5, paste0(round(Share, 1), "%"), "")),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c(
    "EU" = "#1F77B4",
    "China" = "#D62728",
    "UK" = "#FF7F0E",
    "Japan" = "#2CA02C",
    "Korea" = "#9467BD"
  )) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  labs(
    title = "Fuel-Specific Origin Shares (2030) - Volume Weighted",
    subtitle = paste0(
      "BEV: China ", round(fuel_origin_2030_weighted$China[fuel_origin_2030_weighted$Fuel_Group=="BEV"], 1), 
      "%, EU ", round(fuel_origin_2030_weighted$EU[fuel_origin_2030_weighted$Fuel_Group=="BEV"], 1), "% | ",
      "BEV = 80% of sales"
    ),
    x = "Fuel Type",
    y = "Share of New Vehicle Sales (%)",
    fill = "Manufacturing Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

print(p_fuel_origin_updated)

# ============================================
# FIGURE 3: China BEV Sensitivity (Keep as-is, but verify data)
# ============================================

p_china_sensitivity <- china_bev_scenarios %>%
  pivot_longer(cols = c(Base, Plus, Minus),
               names_to = "Scenario",
               values_to = "Share") %>%
  mutate(
    Scenario = factor(Scenario, levels = c("Base", "Plus", "Minus"))
  ) %>%
  ggplot(aes(x = Year, y = Share, color = Scenario, group = Scenario)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  # FIX: Use geom_ribbon with correct aesthetics
  geom_ribbon(
    data = china_bev_scenarios %>%
      mutate(
        Range_Max = pmax(Base, Plus, Minus),
        Range_Min = pmin(Base, Plus, Minus)
      ),
    aes(x = Year, ymin = Range_Min, ymax = Range_Max),  # <-- FIX HERE
    fill = "grey70", alpha = 0.3,
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = c(
    "Base" = "#2E86AB",
    "Plus" = "#E41A1C",
    "Minus" = "#4DAF4A"
  )) +
  scale_x_continuous(breaks = seq(2025, 2037, 2)) +
  scale_y_continuous(limits = c(25, 55), breaks = seq(25, 55, 5)) +
  labs(
    title = "China BEV Share: Sensitivity Analysis",
    subtitle = "Base: 37.4% in 2030 | Plus: 40.4% | Minus: 33.7%",
    x = "Year",
    y = "China's Share of BEV New Sales (%)",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

print(p_china_sensitivity)


# ============================================
# NEW FIGURE 4: Combined Mechanism Figure
# ============================================

# Show the relationship between fuel mix and origin shares
library(patchwork)

# Fuel mix evolution
fuel_mix_plot <- sales_by_fuel_volume %>%
  filter(Year >= 2025, Year <= 2037) %>%
  group_by(Year) %>%
  mutate(Share = 100 * Total_Sales_Fuel / sum(Total_Sales_Fuel)) %>%
  ggplot(aes(x = Year, y = Share, fill = Fuel_Group)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(values = c(
    "BEV" = "#2CA02C",
    "PHEV" = "#FFD700",
    "HEV" = "#FF7F0E",
    "Petrol" = "#D62728",
    "Diesel" = "#9467BD"
  )) +
  scale_x_continuous(breaks = seq(2025, 2037, 2)) +
  labs(
    title = "A) Fuel Mix Evolution (New Sales)",
    x = "Year",
    y = "Share of Sales (%)",
    fill = "Fuel Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# Origin shares evolution
origin_plot <- origin_shares_summary %>%
  select(Year, China, EU, UK, Japan, Korea) %>%
  pivot_longer(cols = c(China, EU, UK, Japan, Korea),
               names_to = "Region",
               values_to = "Share") %>%
  ggplot(aes(x = Year, y = Share, color = Region)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2030, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c(
    "China" = "#D62728",
    "EU" = "#1F77B4",
    "UK" = "#FF7F0E",
    "Japan" = "#2CA02C",
    "Korea" = "#9467BD"
  )) +
  scale_x_continuous(breaks = seq(2025, 2037, 2)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 80)) +
  labs(
    title = "B) Origin Share Evolution (Volume-Weighted)",
    x = "Year",
    y = "Share of Sales (%)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# Combined figure
combined_mechanism <- fuel_mix_plot / origin_plot +
  plot_annotation(
    title = "The Mechanism: BEV Dominance Drives Origin Share Shifts",
    subtitle = "As BEV reaches 80% of sales by 2030, China's BEV production advantage increases its total share",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )
  )

print(combined_mechanism)


# ============================================================
# SUMMARY TABLES FOR PAPER DISCUSSION
# ============================================================

# 1. BEV SHARES BY REGION AND SCENARIO (2025, 2030, 2035, 2037)
cat("========================================\n")
cat("TABLE 1: BEV SHARES BY REGION AND SCENARIO\n")
cat("========================================\n")

bev_summary <- origin_scenarios_year %>%
  filter(Fuel_Group == "BEV") %>%
  group_by(Scenario, Year, Region) %>%
  summarise(Share = sum(Share_exact, na.rm = TRUE), .groups = "drop") %>%
  filter(Year %in% c(2025, 2030, 2035, 2037)) %>%
  pivot_wider(
    id_cols = c(Scenario, Year),
    names_from = Region,
    values_from = Share,
    values_fill = 0
  ) %>%
  arrange(Scenario, Year)

print(bev_summary, n = Inf)

# 2. KEY FINDINGS: CHINA BEV
cat("\n========================================\n")
cat("KEY FINDING 1: CHINA BEV SHARE\n")
cat("========================================\n")

china_bev <- origin_scenarios_year %>%
  filter(Fuel_Group == "BEV", Region == "China") %>%
  group_by(Scenario, Year) %>%
  summarise(Share = sum(Share_exact, na.rm = TRUE), .groups = "drop") %>%
  filter(Year %in% c(2024, 2025, 2030, 2035, 2037))

print(china_bev, n = Inf)

# 3. KEY FINDINGS: EU BEV
cat("\n========================================\n")
cat("KEY FINDING 2: EU BEV SHARE\n")
cat("========================================\n")

eu_bev <- origin_scenarios_year %>%
  filter(Fuel_Group == "BEV", Region == "EU") %>%
  group_by(Scenario, Year) %>%
  summarise(Share = sum(Share_exact, na.rm = TRUE), .groups = "drop") %>%
  filter(Year %in% c(2024, 2025, 2030, 2035, 2037))

print(eu_bev, n = Inf)

# 4. KEY FINDINGS: UK BEV
cat("\n========================================\n")
cat("KEY FINDING 3: UK BEV SHARE\n")
cat("========================================\n")

uk_bev <- origin_scenarios_year %>%
  filter(Fuel_Group == "BEV", Region == "UK") %>%
  group_by(Scenario, Year) %>%
  summarise(Share = sum(Share_exact, na.rm = TRUE), .groups = "drop") %>%
  filter(Year %in% c(2024, 2025, 2030, 2035, 2037))

print(uk_bev, n = Inf)

# 5. TOTAL ORIGIN SHARES (VOLUME-WEIGHTED) BY YEAR
cat("\n========================================\n")
cat("TABLE 2: VOLUME-WEIGHTED ORIGIN SHARES (BASE SCENARIO)\n")
cat("========================================\n")

print(origin_shares_weighted_original, n = Inf)

# 6. EU SENSITIVITY: ORIGINAL VS CONSERVATIVE CEILING
cat("\n========================================\n")
cat("TABLE 3: EU SENSITIVITY TO CHINA BEV CEILING\n")
cat("========================================\n")

print(eu_sensitivity, n = Inf)

# 7. CHANGES 2025 TO 2037 (BASE SCENARIO)
cat("\n========================================\n")
cat("TABLE 4: CHANGES 2025 → 2037 (BASE SCENARIO)\n")
cat("========================================\n")

changes_2025_2037 <- origin_shares_weighted_original %>%
  filter(Year %in% c(2025, 2037)) %>%
  pivot_longer(cols = c(China, EU, UK, Japan, Korea, USA),
               names_to = "Region",
               values_to = "Share") %>%
  pivot_wider(id_cols = Region, names_from = Year, values_from = Share) %>%
  mutate(
    Change = `2037` - `2025`,
    Pct_Change = round(100 * Change / `2025`, 1)
  ) %>%
  arrange(desc(Change))

print(changes_2025_2037, n = Inf)

# 8. STATISTICAL TREND QUALITY SUMMARY
cat("\n========================================\n")
cat("TABLE 5: TREND QUALITY SUMMARY\n")
cat("========================================\n")

trend_quality_summary <- slope_lookup %>%
  group_by(Trend_Quality) %>%
  summarise(
    Count = n(),
    Examples = paste(paste(Fuel_Group, Region, sep="-"), collapse=", ")
  ) %>%
  arrange(desc(Count))

print(trend_quality_summary, n = Inf)


# Exact crossover year for China overtaking EU in BEV share
china_eu_crossover <- origin_scenarios_year %>%
  filter(Fuel_Group == "BEV", Region %in% c("China", "EU")) %>%
  group_by(Scenario, Year, Region) %>%
  summarise(Share = sum(Share_exact, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Region, values_from = Share) %>%
  mutate(China_Leads = China > EU) %>%
  group_by(Scenario) %>%
  filter(China_Leads) %>%
  slice_min(Year, n = 1) %>%
  select(Scenario, Year, China, EU)

cat("\n=== CHINA OVERTAKES EU IN BEV SHARE ===\n")
print(china_eu_crossover)



# Extend feasibility check to ALL fuel types
capped_total_check_all <- origin_slopes_gated %>%
  tidyr::crossing(Year = proj_years) %>%
  mutate(
    years_ahead = Year - 2024,
    raw_share = last_share + Slope_Base * years_ahead
  ) %>%
  left_join(region_bounds_original, by = c("Region", "Fuel_Group")) %>%
  mutate(
    has_cap     = !is.na(ceiling) & ceiling < 100,
    raw_clamped = pmax(coalesce(floor, 0), pmin(coalesce(ceiling, 100), pmax(0.001, raw_share)))
  ) %>%
  filter(has_cap) %>%
  group_by(Fuel_Group, Year) %>%
  summarise(Capped_Total = sum(raw_clamped), .groups = "drop") %>%
  filter(Capped_Total > 100)

cat("\n=== CAPPED TOTAL CHECK (ALL FUEL TYPES) ===\n")
cat("Should be zero rows:\n")
print(capped_total_check_all, n = Inf)


# Determine whether each region-fuel combination is trend-driven or assumption-driven
binding_constraint_table <- origin_slopes_gated %>%
  tidyr::crossing(Year = proj_years) %>%
  mutate(
    years_ahead = Year - 2024,
    raw_share = last_share + Slope_Base * years_ahead
  ) %>%
  left_join(region_bounds_original, by = c("Region", "Fuel_Group")) %>%
  mutate(
    raw_clamped = pmax(coalesce(floor, 0), pmin(coalesce(ceiling, 100), pmax(0.001, raw_share))),
    Binding = case_when(
      !is.na(ceiling) & raw_clamped >= ceiling - 0.05 ~ "At ceiling (assumption-driven)",
      raw_share <= 0.01                                ~ "At zero floor (assumption-driven)",
      TRUE                                              ~ "Trend-driven"
    )
  ) %>%
  filter(Fuel_Group == "BEV", Year %in% c(2025, 2030, 2035, 2037))


binding_table_wide <- binding_constraint_table %>%
  # select(Scenario = "Base", Region, Year, Binding)%>%
  pivot_wider(names_from = Year, values_from = Binding) %>%
  arrange(Region)

cat("\n=== BINDING CONSTRAINT TABLE (BEV) ===\n")
print(binding_table_wide, n = Inf)



# Extend Table 2 to Plus and Minus scenarios
origin_shares_weighted_all_scenarios <- origin_scenarios_year %>%
  left_join(sales_by_fuel_volume, by = c("Year", "Fuel_Group")) %>%
  mutate(
    Vehicles = Share_exact / 100 * Total_Sales_Fuel,
    Vehicles = ifelse(is.na(Vehicles), 0, Vehicles)
  ) %>%
  group_by(Scenario, Year, Region) %>%
  summarise(Vehicles = sum(Vehicles, na.rm = TRUE), .groups = "drop") %>%
  group_by(Scenario, Year) %>%
  mutate(Share = round(100 * Vehicles / sum(Vehicles), 1)) %>%
  ungroup() %>%
  select(Scenario, Year, Region, Share) %>%
  pivot_wider(names_from = Region, values_from = Share, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric) & !matches("Year")), na.rm = TRUE)) %>%
  filter(Year %in% 2025:2037) %>%
  arrange(Scenario, Year)

cat("\n=== TABLE: VOLUME-WEIGHTED ORIGIN SHARES (ALL SCENARIOS) ===\n")
print(origin_shares_weighted_all_scenarios, n = Inf)



# Test sensitivity to a UK BEV floor (2%)
region_bounds_ukfloor <- region_bounds_original %>%
  mutate(floor = ifelse(Region == "UK" & Fuel_Group == "BEV", 2, floor))

# Rebuild with UK floor
origin_scenarios_ukfloor <- bind_rows(
  build_trajectory("Base", region_bounds_ukfloor),
  build_trajectory("Plus", region_bounds_ukfloor),
  build_trajectory("Minus", region_bounds_ukfloor)
)

# Compare UK BEV with and without floor
uk_comparison <- bind_rows(
  origin_scenarios_year %>%
    filter(Fuel_Group == "BEV", Region == "UK", Scenario == "Base") %>%
    group_by(Year) %>%
    summarise(Share = sum(Share_exact, na.rm = TRUE)) %>%
    mutate(Scenario = "No Floor"),
  origin_scenarios_ukfloor %>%
    filter(Fuel_Group == "BEV", Region == "UK", Scenario == "Base") %>%
    group_by(Year) %>%
    summarise(Share = sum(Share_exact, na.rm = TRUE)) %>%
    mutate(Scenario = "With 2% Floor")
) %>%
  pivot_wider(names_from = Scenario, values_from = Share) %>%
  filter(Year %in% c(2025, 2030, 2035, 2037))

cat("\n=== UK BEV FLOOR SENSITIVITY ===\n")
print(uk_comparison, n = Inf)

# Check if China/EU move materially
china_eu_ukfloor <- origin_scenarios_ukfloor %>%
  filter(Fuel_Group == "BEV", Region %in% c("China", "EU"), Scenario == "Base") %>%
  group_by(Year, Region) %>%
  summarise(Share = sum(Share_exact, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Region, values_from = Share) %>%
  filter(Year %in% c(2025, 2030, 2035, 2037))

cat("\n=== CHINA/EU WITH UK FLOOR ===\n")
print(china_eu_ukfloor, n = Inf)


# ============================================================
# FIXED: Scenario Comparison Plot (China and EU)
# ============================================================

# First, convert from wide to long format
scenario_comparison_data <- origin_shares_weighted_all_scenarios %>%
  select(Scenario, Year, China, EU) %>%
  pivot_longer(
    cols = c(China, EU),
    names_to = "Region",
    values_to = "Share"
  )

# Now create the plot
scenario_comparison <- scenario_comparison_data %>%
  ggplot(aes(x = Year, y = Share, color = Scenario, linetype = Region)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Base" = "#2E86AB", "Plus" = "#E41A1C", "Minus" = "#4DAF4A")) +
  scale_linetype_manual(values = c("China" = "solid", "EU" = "dashed")) +
  scale_x_continuous(breaks = seq(2025, 2037, 2)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 70)) +
  labs(
    # title = "Scenario Comparison: China and EU Origin Shares",
    #subtitle = "Solid = China, Dashed = EU",
    x = "Year",
    y = "Share of New Vehicle Sales (%)",
    color = "Scenario",
    linetype = "Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(1, "cm"),  # Increase legend box size
    legend.key.width = unit(2, "cm"),   # Increase legend line width
    plot.title = element_text(face = "bold", size = 14)
  )+
  guides(
    linetype = guide_legend(override.aes = list(size = 2))  # Override legend line size
  )

print(scenario_comparison)



#=========================================================================================================================
#=========================================================================================================================

# Create explanation with visualizations
library(ggplot2)

# Plot 1: Show the discontinuity
p1 <- ggplot(fleet_targets, aes(x = Year)) +
  geom_line(aes(y = Predefined_Sales/1e6, color = "Model Sales"), size = 1.2) +
  geom_point(aes(y = Predefined_Sales/1e6), size = 2) +
  geom_line(data = smmt_sales_forecast, aes(x = Year, y = Total_Sales/1e6, color = "SMMT Forecast"), 
            linetype = "dashed", size = 1) +
  scale_y_continuous(labels = scales::comma_format(suffix = "M")) +
  labs(
    title = "Sales Projections: Model vs SMMT Forecast",
    subtitle = "Note the 2025 discontinuity",
    x = "Year", y = "New Vehicle Sales (millions)",
    color = "Scenario"
  ) +
  theme_minimal()

print(p1)

# Plot 2: Show the relationship with target growth
p2 <- ggplot(fleet_targets, aes(x = Year)) +
  geom_line(aes(y = Target_Growth/1e3, color = "Target Growth (thousands)"), size = 1) +
  geom_line(aes(y = Predefined_Sales/1e3, color = "New Sales (thousands)"), size = 1.2) +
  labs(
    title = "Sales vs Target Fleet Growth",
    subtitle = "Sales should roughly track target growth",
    x = "Year", y = "Thousands of Vehicles",
    color = "Metric"
  ) +
  theme_minimal()

print(p2)

#--------------------------------------------------
# Run sensitivity analysis for different max ages
# Using your existing project_fleet function
#--------------------------------------------------

# Modified sensitivity analysis function
sensitivity_analysis <- function(max_age) {
  
  cat("\n=== RUNNING SENSITIVITY FOR MAX AGE =", max_age, "===\n")
  
  # Filter data for this sensitivity run
  fleet_base <- fleet_2024_complete %>% filter(Vehicle_Age <= max_age)
  retire_rates <- retirement_avg %>% filter(Vehicle_Age <= max_age)
  
  # CRITICAL FIX: Recalculate base fleet for this scenario
  fleet_base_total <- sum(fleet_base$Fleet_Count)
  
  # Recalculate natcop_targets for this scenario
  natcop_targets_scenario <- inflow_unique %>%
    mutate(
      Target_Fleet = fleet_base_total * Cumulative_Growth
    ) %>%
    select(Year, Target_Fleet, Annual_Growth_Rate, Core_Model)
  
  # Run projection with scenario-specific targets
  results_temp <- project_fleet(
    base_fleet = fleet_base,
    retirement_rates = retire_rates,
    fuel_shares = fuel_shares_complete,
    natcop_targets = natcop_targets_scenario,
    engine_sales_distribution = engine_sales_distribution  # ← ADD THIS LINE
  )
  
  # Extract sales
  sales_temp <- bind_rows(results_temp$fleet) %>%
    filter(Vehicle_Age == 0) %>%
    group_by(Year) %>%
    summarise(Sales = sum(Fleet), .groups = "drop") %>%
    mutate(Max_Age = max_age)
  
  return(list(
    sales = sales_temp,
    results = results_temp
  ))
}
# Run for different age cutoffs
ages_to_test <- c(25, 30, 35)
sensitivity_results_list <- list()

for(age in ages_to_test) {
  sensitivity_results_list[[as.character(age)]] <- sensitivity_analysis(age)
}

# Combine sales results
sensitivity_sales <- bind_rows(
  lapply(sensitivity_results_list, function(x) x$sales)
)

# Combine accounting results
sensitivity_accounting <- bind_rows(
  lapply(sensitivity_results_list, function(x) x$accounting)
)

#--------------------------------------------------
# VISUALIZE RESULTS
#--------------------------------------------------

# Plot 1: Sales comparison by max age
library(ggplot2)

p1 <- ggplot(sensitivity_sales, aes(x = Year, y = Sales/1e6, color = factor(Max_Age))) +
  geom_line(size = 1.2) +
  geom_point(data = subset(sensitivity_sales, Year == 2025), size = 3) +
  scale_y_continuous(labels = scales::comma_format(suffix = "M")) +
  labs(
    title = "Sensitivity Analysis: Impact of Maximum Age Assumption",
    subtitle = "Testing age cut-offs at 25, 30, and 35 years",
    x = "Year",
    y = "New Vehicle Sales (millions)",
    color = "Max Age"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p1)

# Plot 2: Show as percentage difference from baseline (age 30)
baseline_sales <- sensitivity_sales %>%
  filter(Max_Age == 30) %>%
  select(Year, Baseline_Sales = Sales)

p2 <- sensitivity_sales %>%
  left_join(baseline_sales, by = "Year") %>%
  mutate(Pct_Difference = (Sales - Baseline_Sales) / Baseline_Sales * 100) %>%
  ggplot(aes(x = Year, y = Pct_Difference, color = factor(Max_Age))) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Percentage Difference from Baseline (Max Age = 30)",
    x = "Year",
    y = "Percentage Difference",
    color = "Max Age"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p2)

#--------------------------------------------------
# SUMMARY TABLE
#--------------------------------------------------

# Create summary table for key years
key_years <- c(2025, 2030, 2035, 2037)

sensitivity_summary <- sensitivity_sales %>%
  filter(Year %in% key_years) %>%
  pivot_wider(
    id_cols = Year,
    names_from = Max_Age,
    values_from = Sales,
    names_prefix = "Age_"
  ) %>%
  mutate(
    Age_30_vs_25 = (Age_30 - Age_25) / Age_25 * 100,
    Age_35_vs_30 = (Age_35 - Age_30) / Age_30 * 100
  )

cat("\n=== SENSITIVITY ANALYSIS SUMMARY ===\n")
print(sensitivity_summary)

#--------------------------------------------------
# TOTAL SALES OVER PROJECTION PERIOD
#--------------------------------------------------

total_sales_by_age <- sensitivity_sales %>%
  group_by(Max_Age) %>%
  summarise(
    Total_Sales_2025_2037 = sum(Sales),
    Avg_Annual_Sales = mean(Sales)
  ) %>%
  mutate(
    Diff_from_30 = Total_Sales_2025_2037 - Total_Sales_2025_2037[Max_Age == 30],
    Pct_Diff = (Total_Sales_2025_2037 / Total_Sales_2025_2037[Max_Age == 30] - 1) * 100
  )

cat("\n=== TOTAL SALES 2025-2037 BY MAX AGE ===\n")
print(total_sales_by_age)

#--------------------------------------------------
# CHECK MAXIMUM AGES IN RESULTS
#--------------------------------------------------

cat("\n=== VERIFYING MAXIMUM AGES IN EACH SCENARIO ===\n")
for(age in ages_to_test) {
  max_age_in_results <- max(sensitivity_results_list[[as.character(age)]]$results$fleet$`2037`$Vehicle_Age)
  cat("Max Age", age, "scenario - Actual max age in 2037:", max_age_in_results, "\n")
}




#------------------------------
# Show NATCOP targets vs Sales
#------------------------------
fleet_targets <- results$accounting %>%
  select(Year, Target_Fleet, Starting_Fleet, Predefined_Sales) %>%
  mutate(
    Target_Growth = Target_Fleet - lag(Target_Fleet),
    Sales_Ratio = Predefined_Sales / Target_Fleet * 100
  )

print(fleet_targets)

# Visualize
ggplot(fleet_targets, aes(x = Year)) +
  geom_line(aes(y = Target_Fleet/1e6, color = "Target Fleet"), size = 1.2) +
  geom_line(aes(y = Predefined_Sales/1e6, color = "New Sales"), size = 1.2) +
  scale_y_continuous(labels = scales::comma_format(suffix = "M")) +
  labs(
    title = "NATCOP Fleet Targets vs Required Sales",
    subtitle = "Sales cap reflects slowing fleet growth",
    x = "Year", y = "Millions of Vehicles"
  )


# Add calculations to understand the dynamics
fleet_analysis <- fleet_targets %>%
  mutate(
    Implied_Survivors = Starting_Fleet - (Starting_Fleet - lag(Starting_Fleet, default = first(Starting_Fleet))),
    Retirement_Rate_Implied = 1 - (Starting_Fleet - Predefined_Sales) / lag(Starting_Fleet, default = first(Starting_Fleet)),
    Sales_Change = Predefined_Sales - lag(Predefined_Sales, default = first(Predefined_Sales)),
    Target_Change = Target_Fleet - lag(Target_Fleet, default = first(Target_Fleet))
  )

print(fleet_analysis)



# Plot 2: Show the relationship with target growth
p2 <- ggplot(fleet_targets, aes(x = Year)) +
  geom_line(aes(y = Target_Growth/1e3, color = "Target Growth (thousands)"), size = 1) +
  geom_line(aes(y = Predefined_Sales/1e3, color = "New Sales (thousands)"), size = 1.2) +
  labs(
    title = "Sales vs Target Fleet Growth",
    subtitle = "Sales should roughly track target growth",
    x = "Year", y = "Thousands of Vehicles",
    color = "Metric"
  ) +
  theme_minimal()

print(p2)

#--------------------------------------------------
# Run sensitivity analysis for different max ages
# Using your existing project_fleet function
#--------------------------------------------------

# Modified sensitivity analysis function
sensitivity_analysis <- function(max_age) {
  
  cat("\n=== RUNNING SENSITIVITY FOR MAX AGE =", max_age, "===\n")
  
  # Filter data for this sensitivity run
  fleet_base <- fleet_2024_complete %>% filter(Vehicle_Age <= max_age)
  retire_rates <- retirement_avg %>% filter(Vehicle_Age <= max_age)
  
  # CRITICAL FIX: Recalculate base fleet for this scenario
  fleet_base_total <- sum(fleet_base$Fleet_Count)
  
  # Recalculate natcop_targets for this scenario
  natcop_targets_scenario <- inflow_unique %>%
    mutate(
      Target_Fleet = fleet_base_total * Cumulative_Growth
    ) %>%
    select(Year, Target_Fleet, Annual_Growth_Rate, Core_Model)
  
  # Run projection with scenario-specific targets
  results_temp <- project_fleet(
    base_fleet = fleet_base,
    retirement_rates = retire_rates,
    fuel_shares = fuel_shares_complete,
    natcop_targets = natcop_targets_scenario,
    engine_sales_distribution = engine_sales_distribution  # ← ADD THIS LINE
  )
  
  # Extract sales
  sales_temp <- bind_rows(results_temp$fleet) %>%
    filter(Vehicle_Age == 0) %>%
    group_by(Year) %>%
    summarise(Sales = sum(Fleet), .groups = "drop") %>%
    mutate(Max_Age = max_age)
  
  return(list(
    sales = sales_temp,
    results = results_temp
  ))
}
# Run for different age cutoffs
ages_to_test <- c(25, 30, 35)
sensitivity_results_list <- list()

for(age in ages_to_test) {
  sensitivity_results_list[[as.character(age)]] <- sensitivity_analysis(age)
}

# Combine sales results
sensitivity_sales <- bind_rows(
  lapply(sensitivity_results_list, function(x) x$sales)
)

# Combine accounting results
sensitivity_accounting <- bind_rows(
  lapply(sensitivity_results_list, function(x) x$accounting)
)

#--------------------------------------------------
# VISUALIZE RESULTS
#--------------------------------------------------

# Plot 1: Sales comparison by max age
library(ggplot2)

p1 <- ggplot(sensitivity_sales, aes(x = Year, y = Sales/1e6, color = factor(Max_Age))) +
  geom_line(size = 1.2) +
  geom_point(data = subset(sensitivity_sales, Year == 2025), size = 3) +
  scale_y_continuous(labels = scales::comma_format(suffix = "M")) +
  labs(
    title = "Sensitivity Analysis: Impact of Maximum Age Assumption",
    subtitle = "Testing age cut-offs at 25, 30, and 35 years",
    x = "Year", 
    y = "New Vehicle Sales (millions)",
    color = "Max Age"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p1)

# Plot 2: Show as percentage difference from baseline (age 30)
baseline_sales <- sensitivity_sales %>%
  filter(Max_Age == 30) %>%
  select(Year, Baseline_Sales = Sales)

p2 <- sensitivity_sales %>%
  left_join(baseline_sales, by = "Year") %>%
  mutate(Pct_Difference = (Sales - Baseline_Sales) / Baseline_Sales * 100) %>%
  ggplot(aes(x = Year, y = Pct_Difference, color = factor(Max_Age))) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Percentage Difference from Baseline (Max Age = 30)",
    x = "Year",
    y = "Percentage Difference",
    color = "Max Age"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p2)

#--------------------------------------------------
# SUMMARY TABLE
#--------------------------------------------------

# Create summary table for key years
key_years <- c(2025, 2030, 2035, 2037)

sensitivity_summary <- sensitivity_sales %>%
  filter(Year %in% key_years) %>%
  pivot_wider(
    id_cols = Year,
    names_from = Max_Age,
    values_from = Sales,
    names_prefix = "Age_"
  ) %>%
  mutate(
    Age_30_vs_25 = (Age_30 - Age_25) / Age_25 * 100,
    Age_35_vs_30 = (Age_35 - Age_30) / Age_30 * 100
  )

cat("\n=== SENSITIVITY ANALYSIS SUMMARY ===\n")
print(sensitivity_summary)

#--------------------------------------------------
# TOTAL SALES OVER PROJECTION PERIOD
#--------------------------------------------------

total_sales_by_age <- sensitivity_sales %>%
  group_by(Max_Age) %>%
  summarise(
    Total_Sales_2025_2037 = sum(Sales),
    Avg_Annual_Sales = mean(Sales)
  ) %>%
  mutate(
    Diff_from_30 = Total_Sales_2025_2037 - Total_Sales_2025_2037[Max_Age == 30],
    Pct_Diff = (Total_Sales_2025_2037 / Total_Sales_2025_2037[Max_Age == 30] - 1) * 100
  )

cat("\n=== TOTAL SALES 2025-2037 BY MAX AGE ===\n")
print(total_sales_by_age)

#--------------------------------------------------
# CHECK MAXIMUM AGES IN RESULTS
#--------------------------------------------------

cat("\n=== VERIFYING MAXIMUM AGES IN EACH SCENARIO ===\n")
for(age in ages_to_test) {
  max_age_in_results <- max(sensitivity_results_list[[as.character(age)]]$results$fleet$`2037`$Vehicle_Age)
  cat("Max Age", age, "scenario - Actual max age in 2037:", max_age_in_results, "\n")
}



# Accounting summary (now shows predicted sales)
cat("\n", strrep("=", 80), "\n")
cat("PROJECTION RESULTS WITH PREDICTED SALES (2025-2037)")
cat("\n", strrep("=", 80), "\n")
print(results$accounting)

# Fleet composition (still works)
fleet_evolution <- bind_rows(results$fleet) %>%
  group_by(Year, Fuel_Group) %>%
  summarise(Fleet = sum(Fleet), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(Share = Fleet / sum(Fleet) * 100)

cat("\n=== FLEET COMPOSITION OVER TIME ===\n")
fleet_wide <- fleet_evolution %>%
  select(Year, Fuel_Group, Fleet) %>%
  pivot_wider(names_from = Fuel_Group, values_from = Fleet, values_fill = 0)
print(fleet_wide)

# Average fleet age (still works)
avg_age <- bind_rows(results$fleet) %>% 
  group_by(Year) %>%
  summarise(
    Avg_Age = sum(Vehicle_Age * Fleet) / sum(Fleet),
    Max_Age = max(Vehicle_Age),
    .groups = "drop"
  )

cat("\n=== AVERAGE FLEET AGE ===\n")
print(avg_age)




# ============================================
# ANALYSIS OF 2024 BASE FLEET
# ============================================

# Get 2024 fleet data
fleet_2024 <- bind_rows(results$fleet[["2024"]])

# 1. New sales in 2024 (age 0)
new_sales_2024 <- fleet_2024 %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Fuel_Group) %>%
  summarise(New_Sales_2024 = sum(Fleet)) %>%
  mutate(Share = New_Sales_2024 / sum(New_Sales_2024) * 100)

cat("\n", strrep("=", 80), "\n")
cat("NEW SALES IN 2024 (AGE 0 VEHICLES)")
cat("\n", strrep("=", 80), "\n")
print(new_sales_2024)
cat("\nTotal new sales in 2024:", format(sum(new_sales_2024$New_Sales_2024), big.mark = ","), "vehicles\n")

# 2. Vehicles above age 40 in 2024
vehicles_above_40 <- fleet_2024 %>%
  filter(Vehicle_Age > 40) %>%
  group_by(Fuel_Group) %>%
  summarise(Fleet_Above_40 = sum(Fleet)) %>%
  mutate(Share = Fleet_Above_40 / sum(Fleet_Above_40) * 100)

cat("\n", strrep("=", 80), "\n")
cat("VEHICLES ABOVE AGE 40 IN 2024")
cat("\n", strrep("=", 80), "\n")
print(vehicles_above_40)
cat("\nTotal vehicles above age 40:", format(sum(vehicles_above_40$Fleet_Above_40), big.mark = ","), "vehicles\n")

# 3. Vehicles above age 25 in 2024
vehicles_above_25 <- fleet_2024 %>%
  filter(Vehicle_Age > 25) %>%
  group_by(Fuel_Group) %>%
  summarise(Fleet_Above_25 = sum(Fleet)) %>%
  mutate(Share = Fleet_Above_25 / sum(Fleet_Above_25) * 100)

cat("\n", strrep("=", 80), "\n")
cat("VEHICLES ABOVE AGE 25 IN 2024")
cat("\n", strrep("=", 80), "\n")
print(vehicles_above_25)
cat("\nTotal vehicles above age 25:", format(sum(vehicles_above_25$Fleet_Above_25), big.mark = ","), "vehicles\n")

# 4. Survival analysis - what happens to these vehicles by 2025
cat("\n", strrep("=", 80), "\n")
cat("SURVIVAL ANALYSIS - WHAT HAPPENS TO 2024 VEHICLES IN 2025")
cat("\n", strrep("=", 80), "\n")

# Get 2025 fleet
fleet_2025 <- bind_rows(results$fleet[["2025"]])

# Track 2024 vehicles into 2025
survival_2024_to_2025 <- fleet_2024 %>%
  left_join(
    retirement_table %>% select(Vehicle_Age, Fuel_Group, Retirement_Rate),
    by = c("Vehicle_Age", "Fuel_Group")
  ) %>%
  mutate(
    # Apply retirement rate to calculate survivors
    Applied_Rate = coalesce(Retirement_Rate, ifelse(Vehicle_Age >= 40, 1, 0.05)),
    Expected_Survivors = Fleet * (1 - Applied_Rate),
    Expected_Retirements = Fleet * Applied_Rate,
    Age_in_2025 = Vehicle_Age + 1
  ) %>%
  select(Vehicle_Age, Fuel_Group, Fleet, Applied_Rate, 
         Expected_Survivors, Expected_Retirements, Age_in_2025)

# Summary by age group
survival_summary <- survival_2024_to_2025 %>%
  mutate(
    Age_Group = case_when(
      Vehicle_Age == 0 ~ "New (0)",
      Vehicle_Age <= 5 ~ "0-5",
      Vehicle_Age <= 10 ~ "6-10",
      Vehicle_Age <= 15 ~ "11-15",
      Vehicle_Age <= 20 ~ "16-20",
      Vehicle_Age <= 25 ~ "21-25",
      Vehicle_Age <= 30 ~ "26-30",
      Vehicle_Age <= 35 ~ "31-35",
      Vehicle_Age <= 40 ~ "36-40",
      TRUE ~ "40+"
    )
  ) %>%
  group_by(Age_Group) %>%
  summarise(
    Fleet_2024 = sum(Fleet),
    Avg_Retirement_Rate = mean(Applied_Rate),
    Expected_Survivors_2025 = sum(Expected_Survivors),
    Expected_Retirements = sum(Expected_Retirements),
    Survival_Rate = Expected_Survivors_2025 / Fleet_2024
  ) %>%
  arrange(factor(Age_Group, levels = c("New (0)", "0-5", "6-10", "11-15", 
                                       "16-20", "21-25", "26-30", "31-35", 
                                       "36-40", "40+")))

print(survival_summary)

# 5. Focus on vehicles above 25 - what happens to them?
above_25_survival <- survival_2024_to_2025 %>%
  filter(Vehicle_Age > 25) %>%
  mutate(
    Age_Group = case_when(
      Vehicle_Age <= 30 ~ "26-30",
      Vehicle_Age <= 35 ~ "31-35",
      Vehicle_Age <= 40 ~ "36-40",
      TRUE ~ "40+"
    )
  ) %>%
  group_by(Age_Group, Fuel_Group) %>%
  summarise(
    Fleet_2024 = sum(Fleet),
    Retirement_Rate = mean(Applied_Rate),
    Survivors_2025 = sum(Expected_Survivors),
    Retirements = sum(Expected_Retirements),
    .groups = "drop"
  )

cat("\n", strrep("=", 80), "\n")
cat("DETAILED SURVIVAL OF VEHICLES ABOVE AGE 25")
cat("\n", strrep("=", 80), "\n")
print(above_25_survival)

# 6. Visualize the age distribution and survival
library(ggplot2)

# Age distribution plot
p_age_dist_2024 <- ggplot(fleet_2024, aes(x = Vehicle_Age, y = Fleet/1e6, fill = Fuel_Group)) +
  geom_area(alpha = 0.7) +
  geom_vline(xintercept = 25, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = 40, linetype = "dashed", color = "blue", size = 1) +
  annotate("text", x = 22, y = max(fleet_2024$Fleet/1e6) * 0.9, 
           label = "Age 25", color = "red", angle = 90) +
  annotate("text", x = 38, y = max(fleet_2024$Fleet/1e6) * 0.9, 
           label = "Age 40", color = "blue", angle = 90) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "2024 Fleet Age Distribution",
    subtitle = paste("Total fleet:", format(sum(fleet_2024$Fleet), big.mark = ","), "vehicles"),
    x = "Vehicle Age",
    y = "Fleet Size (millions)",
    fill = "Fuel Type"
  ) +
  theme_minimal()

print(p_age_dist_2024)


#--------------------
age_data_2024 <- fleet_2024 %>%
  filter(Vehicle_Age <= 25, !is.na(Fleet), Fleet > 0) %>%
  mutate(
    Age_Group = case_when(
      Vehicle_Age <= 7  ~ "0-7",
      Vehicle_Age <= 15 ~ "8-15",
      TRUE              ~ "16-25"
    )
  ) %>%
  group_by(Year, Fuel_Group, Age_Group) %>%
  summarise(
    Fleet = sum(Fleet, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Year, Fuel_Group) %>%
  mutate(
    Share_Percent = 100 * Fleet / sum(Fleet)
  ) %>%
  ungroup()

# Create the summary table
age_summary_2024 <- age_data_2024 %>%
  select(Year, Fuel_Group, Age_Group, Share_Percent) %>%
  arrange(Fuel_Group, Age_Group)

print(age_summary_2024, n = 30)

# Optional: Create a wide format table for easier viewing
age_summary_2024_wide <- age_data_2024 %>%
  select(Year, Fuel_Group, Age_Group, Share_Percent) %>%
  pivot_wider(
    names_from = Age_Group,
    values_from = Share_Percent,
    values_fill = 0
  ) %>%
  arrange(Fuel_Group)

print(age_summary_2024_wide)


#---------------------------------

# 7. Calculate what percentage of 2024 fleet is above thresholds
threshold_summary <- data.frame(
  Threshold = c("Age 0 (New)", "Age > 25", "Age > 40"),
  Vehicles = c(
    sum(fleet_2024$Fleet[fleet_2024$Vehicle_Age == 0]),
    sum(fleet_2024$Fleet[fleet_2024$Vehicle_Age > 25]),
    sum(fleet_2024$Fleet[fleet_2024$Vehicle_Age > 40])
  )
) %>%
  mutate(
    Percentage = Vehicles / sum(fleet_2024$Fleet) * 100
  )

cat("\n", strrep("=", 80), "\n")
cat("SUMMARY: 2024 FLEET THRESHOLDS")
cat("\n", strrep("=", 80), "\n")
print(threshold_summary)
# ============================================
# PART 8: VALIDATION - Check if NATCOP targets are met
# ============================================

cat("\n", strrep("=", 80), "\n")
cat("VALIDATION - NATCOP TARGETS CHECK")
cat("\n", strrep("=", 80), "\n")

validation <- results$accounting %>%
  select(Year, Target_Fleet, Final_Fleet, Error, Pct_Error) %>%
  mutate(
    Target_Met = abs(Error) < 100,
    Pct_Within_1pct = abs(Pct_Error) < 1
  )

print(validation)

cat("\nSummary:\n")
cat("Years meeting target within 100 vehicles:", sum(validation$Target_Met), "out of", nrow(validation), "\n")
cat("Years within 1% of target:", sum(validation$Pct_Within_1pct), "out of", nrow(validation), "\n")




# ==========================================================
# CREATE COMPREHENSIVE FLEET DATASET (CORRECTED)
# ==========================================================

library(dplyr)


# ----------------------------------------------------------
# 2. Annual sales summary by fuel type
# ----------------------------------------------------------
sales_by_fuel_smmt_summary <- sales_by_fuel_smmt %>%
  group_by(Year, Fuel_Group) %>%
  summarise(
    New_Sales_Annual = sum(New_Sales, na.rm = TRUE),
    .groups = "drop"
  )


# ----------------------------------------------------------
# 3. Retirement data
# KEEP ORIGINAL GRANULARITY
# ----------------------------------------------------------
retired_by_fuel_summary <- results$retired_by_fuel %>%
  select(
    Year,
    Fuel_Group,
    Vehicle_Age,
    engine_size,
    engine_size_liters,
    Retired
  )



# See the full age distribution
fleet_age_2024 <- bind_rows(results$fleet[["2024"]]) %>%
  group_by(Vehicle_Age) %>%
  summarise(Fleet = sum(Fleet)) %>%
  mutate(
    Age_Group = case_when(
      Vehicle_Age <= 5 ~ "0-5",
      Vehicle_Age <= 10 ~ "6-10", 
      Vehicle_Age <= 15 ~ "11-15",
      Vehicle_Age <= 20 ~ "16-20",
      Vehicle_Age <= 30 ~ "21-30",
      Vehicle_Age <= 40 ~ "31-40",
      TRUE ~ "40+"
    )
  ) %>%
  group_by(Age_Group) %>%
  summarise(
    Total_Fleet = sum(Fleet),
    Retirement_Rate = mean(retirement_table$Retirement_Rate[match(Vehicle_Age, retirement_table$Vehicle_Age)], na.rm = TRUE)
  )

print(fleet_age_2024)


# Plot the age distribution
ggplot(fleet_age_2024, aes(x = Age_Group, y = Total_Fleet/1e6)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "2024 Fleet Age Distribution",
    subtitle = "The 'bulge' in middle ages causes 2025 sales spike",
    x = "Age Group",
    y = "Fleet Size (millions)"
  )




# Calculate retirements by age in 2025
retirements_2025 <- bind_rows(results$fleet[["2024"]]) %>%
  left_join(retirement_table, by = c("Vehicle_Age", "Fuel_Group")) %>%
  mutate(
    Expected_Retirements = Fleet * coalesce(Retirement_Rate, 0.25)
  ) %>%
  group_by(grp = cut(Vehicle_Age, breaks = c(0,5,10,15,20,30,40,Inf))) %>%
  summarise(Retirements = sum(Expected_Retirements))

print(retirements_2025)


# Calculate 2025 retirements based on 2024 fleet
retirements_2025_detailed <- bind_rows(results$fleet[["2024"]]) %>%
  # Add the retirement rates for the age they WILL BE in 2025
  mutate(
    Age_in_2025 = Vehicle_Age + 1
  ) %>%
  left_join(
    retirement_table %>% 
      select(Vehicle_Age, Fuel_Group, Retirement_Rate_2025 = Retirement_Rate),
    by = c("Age_in_2025" = "Vehicle_Age", "Fuel_Group")
  ) %>%
  mutate(
    # Use the rate for their age in 2025, or default for 40+
    Retirement_Rate_Applied = ifelse(
      Age_in_2025 > 40, 1, 
      coalesce(Retirement_Rate_2025, 0.05)
    ),
    Expected_Retirements = Fleet * Retirement_Rate_Applied
  )

# Summary by age group (using 2024 age)
retirements_by_2024_age <- retirements_2025_detailed %>%
  mutate(
    Age_Group_2024 = case_when(
      Vehicle_Age <= 5 ~ "0-5",
      Vehicle_Age <= 10 ~ "6-10",
      Vehicle_Age <= 15 ~ "11-15",
      Vehicle_Age <= 20 ~ "16-20",
      Vehicle_Age <= 30 ~ "21-30",
      Vehicle_Age <= 40 ~ "31-40",
      TRUE ~ "40+"
    )
  ) %>%
  group_by(Age_Group_2024) %>%
  summarise(
    Fleet_2024 = sum(Fleet),
    Retirements_2025 = sum(Expected_Retirements),
    Effective_Rate = Retirements_2025 / Fleet_2024
  ) %>%
  arrange(Age_Group_2024)

print(retirements_by_2024_age)

# Show detailed example for ages 10-15 (the bulge)
bulge_example <- retirements_2025_detailed %>%
  filter(Vehicle_Age >= 10, Vehicle_Age <= 15) %>%
  select(Vehicle_Age, Fuel_Group, Fleet, Age_in_2025, 
         Retirement_Rate_Applied, Expected_Retirements) %>%
  arrange(Vehicle_Age, Fuel_Group)

cat("\n=== DETAILED RETIREMENTS FOR AGES 10-15 (THE BULGE) ===\n")
print(bulge_example)

# Total retirements
total_retirements_2025 <- sum(retirements_2025_detailed$Expected_Retirements)
cat("\nTotal expected retirements in 2025:", 
    format(round(total_retirements_2025), big.mark = ","), "vehicles\n")
cat("This matches your accounting:", 
    format(round(results$accounting$Natural_Retirements[1]), big.mark = ","), "\n")


# ============================================
# FIGURE 1: Survival Rate per Fuel Type by Age
# ============================================
retirement_table %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(Survival_Probability = cumprod(Survival_Rate))


# Verify the rates
phev_rates <- retirement_avg %>%
  filter(Fuel_Group == "PHEV") %>%
  select(Vehicle_Age, Retirement_Rate)

print(phev_rates, n=30)

# Recalculate survival curves
survival_curves <- retirement_avg %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(Survival_Probability = cumprod(Survival_Rate))%>% arrange(Fuel_Group)

# Key findings from your survival_curves output
survival_curves %>%
  filter(Vehicle_Age %in% c(5,10,15,20,25,30)) %>%
  arrange(Fuel_Group, Vehicle_Age)
print(survival_curves ,n=Inf)
# Check new median
median_life <- survival_curves %>%
  group_by(Fuel_Group) %>%
  summarise(Median_Lifetime = min(Vehicle_Age[Survival_Probability <= 0.5]))

print(median_life)
# ============================================
# COMBINED RETIREMENT ANALYSIS PLOTS
# ============================================

# Prepare data for combined plots using your updated model names

# 1. Cumulative Retirement (percentage retired by each age)
cumulative_retirement <- retirement_avg %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Survival_Prob = cumprod(Survival_Rate),
    Cumulative_Retired = 1 - Survival_Prob
  ) %>%
  ungroup()

# 2. Retirement Probability at each age (hazard rate)
retirement_probability <- retirement_avg %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Survival_to_Age = lag(cumprod(Survival_Rate), default = 1),
    Hazard_Rate = Survival_to_Age * Retirement_Rate
  ) %>%
  ungroup()

# 3. Survival Probability (already in survival_curves)
survival_probability <- survival_curves

# Create combined plot with three panels
library(patchwork)

# Plot A: Cumulative Retirement
p_cumulative <- ggplot(cumulative_retirement, 
                       aes(x = Vehicle_Age, y = Cumulative_Retired, 
                           color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = c(0.5, 0.75, 0.9), 
             linetype = "dashed", color = "gray50", alpha = 0.7) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 10)) +
  labs(
    title = "Cumulative Retirement",
    subtitle = "Percentage of vehicles retired by each age",
    x = "Vehicle Age (years)",
    y = "Cumulative Retirement",
    color = "Fuel Type"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

# Plot B: Retirement Probability at Age
p_hazard <- ggplot(retirement_probability, 
                   aes(x = Vehicle_Age, y = Hazard_Rate, 
                       color = Fuel_Group)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.3)) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 10)) +
  labs(
    title = "Retirement Probability by Age",
    subtitle = "Probability of retiring AT each specific age",
    x = "Vehicle Age (years)",
    y = "Retirement Probability",
    color = "Fuel Type"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

# Plot C: Survival Probability
p_survival <- ggplot(survival_probability, 
                     aes(x = Vehicle_Age, y = Survival_Probability, 
                         color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 10)) +
  labs(
    title = "Survival Probability",
    subtitle = "Percentage of vehicles still in fleet by age",
    x = "Vehicle Age (years)",
    y = "Survival Probability",
    color = "Fuel Type"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

# Combine all three plots
combined_retirement_plot <- (p_cumulative | p_hazard | p_survival) +
  plot_annotation(
    title = "Vehicle Retirement and Survival Analysis by Fuel Type",
    subtitle = paste("Based on historical data | Median lifetime by fuel:",
                     paste(median_life$Fuel_Group, "=", median_life$Median_Lifetime, "yrs", 
                           collapse = ", ")),
    caption = "Shows survival probability, hazard rates, and cumulative retirement"
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Display the combined plot
print(combined_retirement_plot)

# Optional: Save the combined plot
##ggsave("Combined_Retirement_Analysis.png", combined_retirement_plot, 
#  width = 15, height = 8, dpi = 300)

# Also create a version with fuel-specific median lines
p_cumulative_with_medians <- ggplot(cumulative_retirement, 
                                    aes(x = Vehicle_Age, y = Cumulative_Retired, 
                                        color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_vline(data = median_life, 
             aes(xintercept = Median_Lifetime, color = Fuel_Group),
             linetype = "dashed", alpha = 0.5, size = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 10)) +
  labs(
    title = "Cumulative Retirement with Median Age Indicators",
    subtitle = "Dashed vertical lines show median retirement age for each fuel type",
    x = "Vehicle Age (years)",
    y = "Cumulative Retirement",
    color = "Fuel Type"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")

print(p_cumulative_with_medians)
#-------------------
#extract median lifetime
#--------------------------
median_life <- survival_curves %>%
  group_by(Fuel_Group) %>%
  summarise(
    Median_Lifetime = min(Vehicle_Age[Survival_Probability <= 0.5])
  )

print(median_life)

# Plot survival curves
library(tidyverse)
p_survival <- ggplot(survival_curves, aes(x = Vehicle_Age, y = Survival_Probability, 
                                          color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_point(data = survival_curves %>% filter(Vehicle_Age %% 5 == 0), size = 2) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  labs(
    title = "Vehicle Survival Probability by Fuel Type and Age",
    #subtitle = "Based on historical retirement rates (2020-2024)",
    x = "Vehicle Age (years)",
    y = "Survival Probability",
    color = "Fuel Type",
    #caption = "Shows probability a vehicle survives to each age"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

print(p_survival)

# ============================================
# FIGURE 2: New Sales by Fuel Type (2025-2037)
# ============================================

# Prepare sales data
sales_plot_data <- results$sales_by_fuel %>%
  group_by(Year, Fuel_Group) %>%
  summarise(New_Sales = sum(Required_Sales), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(Total_Sales = sum(New_Sales)) %>%
  ungroup()
print(sales_plot_data,n=Inf)

# Sales by fuel type - area chart
p_sales_area <- ggplot(sales_plot_data, aes(x = Year, y = New_Sales, fill = Fuel_Group)) +
  geom_area(alpha = 0.8, position = "stack") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 2025:2037) +
  labs(
    #title = "New Vehicle Sales by Fuel Type (2025-2037)",
    #subtitle = paste("SMMT targets (2025-2027) + NATCOP growth rates (2028-2037) | Total:",
    #           format(sum(sales_plot_data$New_Sales), big.mark = ","), "vehicles"),
    x = "Year",
    y = "Number of New Vehicles",
    fill = "Fuel Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set1")

print(p_sales_area)

# Sales by fuel type - grouped bar chart (alternative)
p_sales_bar <- ggplot(sales_plot_data, aes(x = factor(Year), y = New_Sales, fill = Fuel_Group)) +
  geom_col(position = "stack", alpha = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "New Vehicle Sales by Fuel Type (2025-2037)",
    x = "Year",
    y = "Number of New Vehicles",
    fill = "Fuel Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set1")

print(p_sales_bar)

# ============================================
# FIGURE 3: Total Fleet by Fuel Type (2024-2037)
# ============================================

# Prepare fleet composition data
fleet_composition <- bind_rows(results$fleet) %>%
  group_by(Year, Fuel_Group) %>%
  summarise(Fleet = sum(Fleet), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(
    Total_Fleet = sum(Fleet),
    Share = Fleet / Total_Fleet * 100
  ) %>%
  ungroup()
print(fleet_composition,n=Inf)

# Fleet by fuel type - area chart
p_fleet_area <- ggplot(fleet_composition, aes(x = Year, y = Fleet, fill = Fuel_Group)) +
  geom_area(alpha = 0.8, position = "stack") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 2024:2037) +
  labs(
    title = "Total Fleet Composition by Fuel Type (2024-2037)",
    subtitle = paste("Fleet grows from",
                     format(fleet_composition$Total_Fleet[fleet_composition$Year == 2024][1], big.mark = ","),
                     "to",
                     format(fleet_composition$Total_Fleet[fleet_composition$Year == 2037][1], big.mark = ","),
                     "vehicles"),
    x = "Year",
    y = "Number of Vehicles",
    fill = "Fuel Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set1")

print(p_fleet_area)

# Fleet share by fuel type - percentage area chart
p_share_area <- ggplot(fleet_composition, aes(x = Year, y = Share, fill = Fuel_Group)) +
  geom_area(alpha = 0.8, position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = 2024:2037) +
  labs(
    # title = "Fleet Share by Fuel Type (2024-2037)",
    x = "Year",
    y = "Percentage of Total Fleet",
    fill = "Fuel Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set1")

print(p_share_area)



# ============================================
# FIGURE 5: Retirements vs New Sales
# ============================================

# Prepare retirement vs sales data
retirement_vs_sales <- results$accounting %>%
  select(Year, Predefined_Sales, Natural_Retirements, Additional_Retirements) %>%
  mutate(
    Total_Retirements = Natural_Retirements + Additional_Retirements,
    Net_Change = Predefined_Sales - Total_Retirements
  )

p_retire_vs_sales <- ggplot(retirement_vs_sales, aes(x = Year)) +
  geom_col(aes(y = Predefined_Sales, fill = "New Sales"), alpha = 0.8, width = 0.4, position = position_nudge(x = -0.2)) +
  geom_col(aes(y = Total_Retirements, fill = "Total Retirements"), alpha = 0.8, width = 0.4, position = position_nudge(x = 0.2)) +
  geom_line(aes(y = Net_Change * 10, color = "Net Change (×10)"), size = 1.2) +
  geom_point(aes(y = Net_Change * 10), size = 2) +
  scale_y_continuous(
    name = "Number of Vehicles",
    labels = scales::comma,
    sec.axis = sec_axis(~ . / 10, name = "Net Change", labels = scales::comma)
  ) +
  scale_x_continuous(breaks = 2025:2037) +
  labs(
    title = "Annual New Sales vs Retirements",
    subtitle = "Net change = New Sales - Total Retirements",
    x = "Year",
    fill = "",
    color = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = c("New Sales" = "#377EB8", "Total Retirements" = "#E41A1C")) +
  scale_color_manual(values = c("Net Change (×10)" = "#4DAF4A"))

print(p_retire_vs_sales)

# ============================================
# FIGURE 6: Average Fleet Age
# ============================================

# Clean up avg_age to remove NA year if present
library(ggplot2)
library(dplyr)

avg_age_clean <- avg_age %>% filter(!is.na(Year))

p_age <- ggplot(avg_age_clean, aes(x = Year, y = Avg_Age)) +
  geom_line(color = "#0072B2", size = 1.5) +           # main line
  geom_point(color = "#D55E00", size = 3) +            # points
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "gray50", alpha = 0.3) + # trend
  scale_y_continuous(limits = c(8, 10), breaks = seq(8, 10, 0.5)) +
  scale_x_continuous(breaks = 2024:2037) +
  labs(
    # title = "Projected Average Fleet Age (2024-2037)",
    # subtitle = paste0("Rising from ", round(avg_age_clean$Avg_Age[avg_age_clean$Year == 2024], 1),
    # " to ", round(avg_age_clean$Avg_Age[avg_age_clean$Year == 2037], 1), " years"),
    x = "Year",
    y = "Average Age (years)"
  ) +
  geom_text(aes(label = round(Avg_Age,1)), vjust = -1.2, size = 3, color = "black") + # annotate points
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )

print(p_age)


# If you have standard errors or confidence intervals
p_age_ci <- ggplot(avg_age_clean, aes(x = Year, y = Avg_Age)) +
  # Confidence band
  geom_ribbon(aes(ymin = Avg_Age - 0.12, ymax = Avg_Age + 0.12),
              fill = "#984EA3", alpha = 0.15) +
  
  # Main elements
  geom_line(color = "#984EA3", size = 1.2) +
  geom_point(color = "#984EA3", size = 3) +
  
  # Linear trend
  geom_smooth(method = "lm", se = FALSE, color = "#E6AB02", 
              linetype = "dashed", size = 0.8) +
  
  # Scale adjustments
  scale_y_continuous(
    limits = c(8, 10),
    breaks = seq(8, 10, 0.5),
    labels = function(x) sprintf("%.1f", x)
  ) +
  
  scale_x_continuous(breaks = seq(2024, 2037, 2)) +
  
  labs(
    #title = "Projected Average Fleet Age with 95% Confidence Interval",
    x = "Year",
    y = "Average Age (years)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_age_ci)

# Academic journal style (more restrained)
p_age_academic <- ggplot(avg_age_clean, aes(x = Year, y = Avg_Age)) +
  geom_line(color = "black", size = 0.8) +
  geom_point(color = "black", size = 2, shape = 19) +
  geom_smooth(method = "lm", se = FALSE, color = "gray50", 
              linetype = "dashed", size = 0.5) +
  
  scale_y_continuous(
    limits = c(8, 10),
    breaks = seq(8, 10, 0.5),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  
  scale_x_continuous(
    breaks = seq(2024, 2037, 2),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  labs(
    #  title = "Figure 1: Projected Average Age of UK Vehicle Fleet, 2024-2037",
    x = "Year",
    y = "Average Age (years)",
    caption = "Source: Author's calculations based on SMMT ZEV Mandate Compliance Model"
  ) +
  
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_age_academic)

# ============================================
# PLOT: Age Distribution by Fuel Type (selected years)
# ============================================

# Get age distribution for selected years
selected_years <- c(2024, 2028, 2032, 2037)
age_distribution <- bind_rows(results$fleet) %>%
  filter(Year %in% selected_years) %>%
  group_by(Year, Fuel_Group, Vehicle_Age) %>%
  summarise(Fleet = sum(Fleet), .groups = "drop") %>%
  group_by(Year, Vehicle_Age) %>%
  mutate(Share = Fleet / sum(Fleet) * 100) %>%
  ungroup()

age_distribution <- age_distribution %>%
  tidyr::complete(Year, Vehicle_Age, Fuel_Group, fill = list(Fleet = 0)) %>%
  group_by(Year, Vehicle_Age) %>%
  mutate(
    total_age = sum(Fleet),
    Share = ifelse(total_age > 0, Fleet / total_age * 100, NA_real_)
  ) %>%
  ungroup() %>%
  filter(!is.na(Share))
print(age_distribution,n=Inf)

# Faceted plot - Age distribution by fuel type for each selected year
p_age_distribution <- ggplot(age_distribution, aes(x = Vehicle_Age, y = Share, fill = Fuel_Group)) +
  geom_area(alpha = 0.6, position = "stack", stat = "identity")+
  facet_wrap(~Year, ncol = 2) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 10)) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 105),  # ← ADD THIS LINE to set y-axis from 0 to 100%
    expand = c(0, 0)     # ← Optional: removes gap at bottom and top
  ) +
  labs(
    #title = "Vehicle Age Distribution by Fuel Type",
    #subtitle = "Stacked area shows fuel composition at each age",
    x = "Vehicle Age (years)",
    y = "Percentage of Fleet",
    fill = "Fuel Type"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set1")

print(p_age_distribution)

# ============================================
# PLOT: Age Distribution by Fuel Type (small multiples)
# ============================================

# Alternative: Small multiples by fuel type
p_age_fuel_facet <- ggplot(age_distribution, aes(x = Vehicle_Age, y = Share)) +
  geom_area(aes(fill = Fuel_Group), alpha = 0.7, position = "stack") +
  facet_grid(Fuel_Group ~ Year) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 10)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Age Distribution Evolution by Fuel Type",
    subtitle = "Each row shows one fuel type across selected years",
    x = "Vehicle Age (years)",
    y = "Percentage of Fuel Type",
    fill = "Fuel Type"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set1")

print(p_age_fuel_facet)

# ============================================
# PLOT: Median Age by Fuel Type
# ============================================

# Calculate median age by fuel type for each year
median_age_by_fuel <- bind_rows(results$fleet) %>%
  filter(!is.na(Year)) %>%
  group_by(Year, Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  summarise(
    Median_Age = {
      cum_fleet <- cumsum(Fleet)
      total <- sum(Fleet)
      min(Vehicle_Age[cum_fleet >= total/2])
    },
    .groups = "drop"
  )

p_median_age <- ggplot(median_age_by_fuel, aes(x = Year, y = Median_Age, color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  scale_x_continuous(breaks = 2024:2037) +
  labs(
    title = "Median Vehicle Age by Fuel Type (2024-2037)",
    subtitle = "Age at which half of each fuel type's fleet is older",
    x = "Year",
    y = "Median Age (years)",
    color = "Fuel Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_brewer(palette = "Set1")

print(p_median_age)

# ============================================
# PLOT: Age Distribution Comparison (2024 vs 2037)
# ============================================
age_comparison <- bind_rows(
  # 2024 data - ensure no duplicates
  bind_rows(results$fleet) %>%
    filter(Year == 2024) %>%
    group_by(Vehicle_Age, Fuel_Group) %>%
    summarise(Fleet = sum(Fleet, na.rm = TRUE), .groups = "drop") %>%
    mutate(Year = 2024),
  
  # 2037 data
  bind_rows(results$fleet) %>%
    filter(Year == 2037) %>%
    group_by(Vehicle_Age, Fuel_Group) %>%
    summarise(Fleet = sum(Fleet, na.rm = TRUE), .groups = "drop") %>%
    mutate(Year = 2037)
) %>%
  tidyr::complete(Year, Vehicle_Age, Fuel_Group, fill = list(Fleet = 0)) %>%
  group_by(Year, Vehicle_Age) %>%
  mutate(Share = Fleet / sum(Fleet) * 100) %>%
  ungroup()

age_comparison <- age_comparison %>% filter(Vehicle_Age <=30)
p_age_comparison <- ggplot(age_comparison, aes(x = Vehicle_Age, y = Share, fill = Fuel_Group)) +
  geom_area(alpha = 0.7, position = "stack") +
  facet_wrap(~Year, ncol = 2, labeller = labeller(Year = c("2024" = "2024 Base Fleet", 
                                                           "2037" = "2037 Projected Fleet"))) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Fleet Age Distribution: 2024 vs 2037",
    #subtitle = "Shift towards older vehicles and changing fuel mix",
    x = "Vehicle Age (years)",
    y = "Percentage of Fleet",
    fill = "Fuel Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set1")

print(p_age_comparison)



#-----------------
# data
#----------------

age_data <- age_comparison %>%
  filter(Vehicle_Age <= 25) %>%
  mutate(Age_Group = case_when(
    Vehicle_Age <= 7  ~ "0-7",
    Vehicle_Age <= 15 ~ "8-15",
    TRUE              ~ "16-25"
  )) %>%
  group_by(Year, Fuel_Group, Age_Group) %>%  # Group by all three
  summarise(Share = sum(Share, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year, Fuel_Group) %>%  # Then calculate percentages within each fuel
  mutate(
    Total_Fuel = sum(Share, na.rm = TRUE),
    Share_Percent = 100 * Share / Total_Fuel
  ) %>%
  ungroup()

# Now this will sum to 100% for each Fuel_Group within each Year
age_summary <- age_data %>%
  select(Year, Fuel_Group, Age_Group, Share_Percent) %>%
  arrange(Year, Fuel_Group, Age_Group)

print(age_summary, n=30)

# ============================================
# SUMMARY TABLE: Age Statistics by Fuel Type
# ============================================

# Create summary table of age statistics
age_stats <- bind_rows(results$fleet) %>%
  filter(Year %in% c(2025, 2030, 2037)) %>%
  group_by(Year, Fuel_Group) %>%
  summarise(
    Mean_Age = sum(Vehicle_Age * Fleet) / sum(Fleet),
    Median_Age = {
      cum_fleet <- cumsum(Fleet[order(Vehicle_Age)])
      total <- sum(Fleet)
      Vehicle_Age[order(Vehicle_Age)][which(cum_fleet >= total/2)[1]]
    },
    P25_Age = {
      cum_fleet <- cumsum(Fleet[order(Vehicle_Age)])
      total <- sum(Fleet)
      Vehicle_Age[order(Vehicle_Age)][which(cum_fleet >= total*0.25)[1]]
    },
    P75_Age = {
      cum_fleet <- cumsum(Fleet[order(Vehicle_Age)])
      total <- sum(Fleet)
      Vehicle_Age[order(Vehicle_Age)][which(cum_fleet >= total*0.75)[1]]
    },
    Max_Age = max(Vehicle_Age),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(., 1)))

cat("\n=== AGE STATISTICS BY FUEL TYPE (Selected Years) ===\n")
print(age_stats)

# Wide format for easier viewing
age_stats_wide <- age_stats %>%
  pivot_wider(
    id_cols = Fuel_Group,
    names_from = Year,
    values_from = c(Mean_Age, Median_Age, P25_Age, P75_Age, Max_Age),
    names_glue = "{.value}_{Year}"
  )

cat("\n=== AGE STATISTICS WIDE FORMAT ===\n")
print(age_stats_wide)


# ============================================
# FINAL MILEAGE MODEL (ROBUST + CORRECTED)
# ============================================

library(dplyr)
library(tibble)
install.packages("zoo")
library(zoo)

# ------------------------------------------------
# STEP 1: 2024 observed only
# ------------------------------------------------
fleet_2024_with_mileage <- bind_rows(results$fleet) %>%
  filter(Year == 2024) %>%
  
  left_join(
    fleet_2024_complete %>%
      select(Vehicle_Age, Fuel_Group, Avg_Mileage_Calibrated) %>%
      rename(Base_Mileage = Avg_Mileage_Calibrated) %>%
      distinct(),
    by = c("Vehicle_Age", "Fuel_Group")
  ) %>%
  
  mutate(
    Final_Mileage = Base_Mileage,
    Predicted_Mileage = NA_real_
  ) %>%
  
  select(
    Year, Vehicle_Age, Fuel_Group,
    Fleet, engine_size, engine_size_liters,
    Final_Mileage, Predicted_Mileage
  )

# ------------------------------------------------
# STEP 2: AGE CURVE (ROBUST ANCHORING)
# ------------------------------------------------
mileage_decay_raw <- fleet_2024_complete %>%
  filter(!is.na(Avg_Mileage_Calibrated), Vehicle_Age <= 25) %>%
  
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(
    Avg_Mileage = weighted.mean(
      Avg_Mileage_Calibrated,
      Fleet_Count,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age, .by_group = TRUE) %>%
  
  mutate(
    # smooth curve first
    Smoothed = zoo::rollmean(
      Avg_Mileage,
      k = 3,
      fill = NA,
      align = "center"
    ),
    Smoothed = ifelse(is.na(Smoothed), Avg_Mileage, Smoothed)
  ) %>%
  
  mutate(
    # FIX: anchor at young fleet (0–2 years average)
    Anchor = mean(Smoothed[Vehicle_Age <= 1], na.rm = TRUE),
    Decay_Factor = Smoothed / Anchor,
    Decay_Factor = cummin(Decay_Factor)
  ) %>%
  
  ungroup()


# ------------------------------------------------
# STEP 3: SAFE FALLBACK (calibrated)
# ------------------------------------------------
decay_fallback <- function(age, fuel) {
  
  base_rate <- case_when(
    fuel == "BEV"    ~ 0.035,
    fuel == "PHEV"   ~ 0.045,
    fuel == "HEV"    ~ 0.05,
    fuel == "Diesel" ~ 0.055,
    TRUE             ~ 0.06
  )
  
  # calibrated to avoid extreme collapse
  pmax(0.35, exp(-base_rate * age))
}

# ------------------------------------------------
# STEP 4: BASELINE
# ------------------------------------------------
baseline_mileage_2024 <- tibble(
  Fuel_Group = c("Petrol", "Diesel", "BEV", "PHEV", "HEV"),
  Mileage_New = c(7585, 10728, 10054, 9367, 8500)
)

# ------------------------------------------------
# STEP 5: FUTURE FLEET
# ------------------------------------------------
fleet_future <- bind_rows(results$fleet) %>%
  filter(Year >= 2025)

# ------------------------------------------------
# STEP 6: PREDICTION ENGINE
# ------------------------------------------------
fleet_future_with_mileage <- fleet_future %>%
  
  left_join(
    mileage_decay_raw,
    by = c("Fuel_Group", "Vehicle_Age")
  ) %>%
  
  left_join(
    baseline_mileage_2024,
    by = "Fuel_Group"
  ) %>%
  
  mutate(
    Decay_Factor = coalesce(
      Decay_Factor,
      decay_fallback(Vehicle_Age, Fuel_Group)
    ),
    
    Decay_Factor = pmin(Decay_Factor, 1),
    
    fuel_floor = case_when(
      Fuel_Group == "BEV" ~ 0.45,
      Fuel_Group == "PHEV" ~ 0.40,
      Fuel_Group == "HEV" ~ 0.35,
      Fuel_Group == "Diesel" ~ 0.25,
      TRUE ~ 0.22
    ),
    
    Decay_Factor = pmax(Decay_Factor, fuel_floor),
    
    Year_Factor = (1 - 0.005) ^ (Year - 2024),
    
    Predicted_Mileage = Mileage_New * Decay_Factor * Year_Factor,
    Final_Mileage = Predicted_Mileage
  ) %>%
  
  select(
    Year, Vehicle_Age, Fuel_Group,
    Fleet, engine_size, engine_size_liters,
    Final_Mileage, Predicted_Mileage
  )


# ============================================
# BUILD FLEET WITH REGION/ORIGIN (2024-2037) — replaces old broken fleet_all block
# ============================================

# STEP A: Region/Origin shares at birth (FIXED — derived from ALL ages, not just Vehicle_Age == 0)
origin_hist <- data %>%
  filter(Fuel_Group != "Other", !is.na(Region), !is.na(Origin)) %>%
  mutate(Build_Year = Data_Year - Vehicle_Age) %>%
  group_by(Build_Year, Fuel_Group, Region, Origin) %>%
  summarise(Reg = sum(Fleet_Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Build_Year, Fuel_Group) %>%
  mutate(Origin_Share = 100 * Reg / sum(Reg)) %>%
  ungroup() %>%
  rename(Year = Build_Year) %>%
  select(Year, Fuel_Group, Region, Origin, Origin_Share)

origin_proj <- origin_sales_distribution %>%
  select(Year, Fuel_Group, Region, Origin, Origin_Share)

origin_at_birth <- bind_rows(origin_hist, origin_proj) %>%
  group_by(Year, Fuel_Group, Region, Origin) %>%
  summarise(Origin_Share = sum(Origin_Share), .groups = "drop")

# STEP B: split Region/Origin at low granularity (fast, memory-safe) — UNCHANGED
library(data.table)
fleet_all_raw <- bind_rows(results$fleet) %>%
  mutate(Build_Year = Year - Vehicle_Age) %>%
  select(-any_of(c("Region", "Origin")))%>%
  # Ensure kerb_weight_kg is included
  mutate(kerb_weight_kg = as.numeric(kerb_weight_kg))

fleet_collapsed <- fleet_all_raw %>%
  group_by(Year, Vehicle_Age, Build_Year, Fuel_Group, engine_size, engine_size_liters) %>%
  summarise(
    kerb_weight_kg = weighted.mean(kerb_weight_kg, Fleet, na.rm = TRUE),
    Fleet = sum(Fleet, na.rm = TRUE),
    .groups = "drop"
  )


fc_dt <- as.data.table(fleet_collapsed)
fc_dt[, row_id := .I]
origin_dt <- as.data.table(origin_at_birth) %>% setnames("Year", "Build_Year")
joined <- origin_dt[fc_dt, on = .(Build_Year, Fuel_Group), allow.cartesian = TRUE]
joined[, Origin_Share := ifelse(is.na(Origin_Share), 100 / .N, Origin_Share), by = row_id]
joined[, Fleet_exact := Fleet * Origin_Share / 100]
joined[, Fleet_floor := floor(Fleet_exact)]
joined[, remainder := Fleet_exact - Fleet_floor]
joined[, n_extra := round(sum(Fleet_exact)) - sum(Fleet_floor), by = row_id]
joined[, rank_remainder := frank(-remainder, ties.method = "first"), by = row_id]
joined[, Fleet_final := Fleet_floor + as.numeric(rank_remainder <= n_extra)]
fleet_all <- joined[, .(Year, Vehicle_Age, Build_Year, Fuel_Group, Region, Origin,engine_size,kerb_weight_kg,
                        Fleet = Fleet_final)]
fleet_all <- fleet_all %>% left_join(bind_rows(
  fleet_2024_with_mileage,
  fleet_future_with_mileage
) %>%
  group_by(Year, Vehicle_Age, Fuel_Group) %>%
  summarise(
    Final_Mileage = mean(Final_Mileage, na.rm = TRUE),
    Predicted_Mileage = mean(Predicted_Mileage, na.rm = TRUE),
    .groups = "drop"
  ), by = c("Year", "Vehicle_Age", "Fuel_Group")) %>%
  mutate(
    Final_Mileage = ifelse(is.na(Final_Mileage), 0, Final_Mileage),
    Predicted_Mileage = ifelse(is.na(Predicted_Mileage), 0, Predicted_Mileage)
  ) %>%
  arrange(Year, Fuel_Group, Vehicle_Age, Region, Origin)

cat("\n=== Sanity check: fleet_all totals ===\n")
print(fleet_all[, .(Total = sum(Fleet)), by = Year][order(Year)])
fleet_all <- as_tibble(fleet_all)
fleet_all %>%
  filter(Year == 2030, Vehicle_Age == 5, Fuel_Group == "BEV") %>%
  select(Region, Origin, Fleet, Final_Mileage) %>%
  print(n = Inf)

cat("\n=== CHECKING REGION AND ORIGIN BY YEAR ===\n")
region_check <- fleet_all %>%
  group_by(Year) %>%
  summarise(
    Regions = paste(unique(Region), collapse=", "),
    Origins = paste(unique(Origin), collapse=", "),
    Total_Fleet = sum(Fleet, na.rm = TRUE)
  )
print(region_check, n = Inf)

# NEW: confirm zero NAs anywhere in Region/Origin
cat("\n=== Remaining NA count in Region/Origin ===\n")
cat("Region NAs:", sum(is.na(fleet_all$Region)), "\n")
cat("Origin NAs:", sum(is.na(fleet_all$Origin)), "\n")


# ------------------------------------------------
# STEP 8: VALIDATION
# ------------------------------------------------
fleet_all %>%
  filter(Year == 2024) %>%
  summarise(
    mean_mileage = mean(Final_Mileage, na.rm = TRUE),
    min_mileage = min(Final_Mileage, na.rm = TRUE),
    max_mileage = max(Final_Mileage, na.rm = TRUE)
  ) %>% print()

fleet_all %>%
  filter(Year == 2037) %>%
  summarise(
    mean_mileage = mean(Final_Mileage, na.rm = TRUE),
    min_mileage = min(Final_Mileage, na.rm = TRUE),
    max_mileage = max(Final_Mileage, na.rm = TRUE)
  ) %>% print()




# ============================================
# COMBINED FIGURE: All key plots in one layout
# ============================================

if(require(patchwork)) {
  
  # Arrange four main plots
  combined_plot <- (p_survival + p_sales_area) / 
    (p_fleet_area + p_retire_vs_sales) +
    plot_annotation(
      title = "UK Vehicle Fleet Projection Model Results",
      subtitle = paste("SMMT Sales (2025-2027) + NATCOP Growth Rates |",
                       "Total Additional Retirements Needed:",
                       format(ifelse(!is.null(results$additional_retirements), 
                                     sum(results$additional_retirements$Additional_Retirements), 0), 
                              big.mark = ","), "vehicles"),
      caption = "Based on historical retirement rates (2022-2024) and NATCOP fleet targets"
    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  print(combined_plot)
  
  # Save the combined plot
  # #ggsave("Fleet_Projection_Complete.png", combined_plot, width = 16, height = 12, dpi = 300)
}

# ============================================
# ADDITIONAL: Key metrics tables
# ============================================

# Survival rates at key ages
survival_key_ages <- survival_curves %>%
  filter(Vehicle_Age %in% c(5, 10, 15, 20, 25, 30)) %>%
  select(Fuel_Group, Vehicle_Age, Survival_Probability) %>%
  pivot_wider(names_from = Vehicle_Age, values_from = Survival_Probability, 
              names_prefix = "Age_")

cat("\n=== SURVIVAL PROBABILITY AT KEY AGES ===\n")
print(survival_key_ages)

# Sales summary by fuel type
sales_summary <- sales_plot_data %>%
  group_by(Fuel_Group) %>%
  summarise(
    Total_Sales_2025_2037 = sum(New_Sales),
    Share_of_Total = sum(New_Sales) / sum(sales_plot_data$New_Sales) * 100
  ) %>%
  arrange(desc(Share_of_Total))

cat("\n=== TOTAL NEW SALES BY FUEL TYPE (2025-2037) ===\n")
print(sales_summary)

# Fleet composition change (2024 vs 2037)
fleet_change <- fleet_composition %>%
  filter(Year %in% c(2024, 2037)) %>%
  group_by(Year, Fuel_Group) %>%
  summarise(Fleet = sum(Fleet), .groups = "drop") %>%
  pivot_wider(names_from = Year, values_from = Fleet, names_prefix = "Fleet_") %>%
  mutate(
    Change = Fleet_2037 - Fleet_2024,
    Pct_Change = (Change / Fleet_2024) * 100
  ) %>%
  arrange(desc(Pct_Change))


cat("\n=== FLEET COMPOSITION CHANGE (2024 vs 2037) ===\n")
print(fleet_change)

# Summary of additional retirements
if(!is.null(results$additional_retirements)) {
  cat("\n=== ADDITIONAL RETIREMENTS SUMMARY ===\n")
  print(results$additional_retirements %>%
          mutate(Additional_Retirements_thousands = round(Additional_Retirements/1000, 0)) %>%
          select(Year, Additional_Retirements, Additional_Retirements_thousands))
}

# Validation summary
cat("\n=== VALIDATION SUMMARY ===\n")
cat("Years meeting NATCOP target within 100 vehicles:", 
    sum(validation$Target_Met), "out of", nrow(validation), "\n")
cat("Years within 1% of NATCOP target:", 
    sum(validation$Pct_Within_1pct), "out of", nrow(validation), "\n")
cat("Average absolute error:", 
    format(round(mean(abs(validation$Error)), 0), big.mark = ","), "vehicles\n")
cat("Average absolute % error:", 
    round(mean(abs(validation$Pct_Error), na.rm = TRUE), 2), "%\n")


# ---------------------------------------------------------------------------
# 1. Historical origin shares within each fuel, by build year (measured, <=2024)
# ---------------------------------------------------------------------------
# origin_by_fuel must exist: Data_Year, Region, Fuel_Group, Share_Within_Fuel
origin_hist <- origin_by_fuel %>%
  filter(Data_Year <= 2024) %>%
  transmute(Build_Year = Data_Year, Fuel_Group, Region,
            Origin_Share = Share_Within_Fuel)

# ---------------------------------------------------------------------------
# 2. Projected origin shares by build year (2025–2037, Base scenario)
# ---------------------------------------------------------------------------
# origin_scenarios_year must exist: Fuel_Group, Region, Year, Share_exact, Scenario
origin_proj <- origin_scenarios_year %>%
  filter(Scenario == "Base", Year >= 2025) %>%
  transmute(Build_Year = Year, Fuel_Group, Region,
            Origin_Share = Share_exact)

# ---------------------------------------------------------------------------
# 3. Combine + normalise so each Build_Year × Fuel_Group sums to exactly 100
# ---------------------------------------------------------------------------
origin_by_build_year <- bind_rows(origin_hist, origin_proj) %>%
  group_by(Build_Year, Fuel_Group) %>%
  mutate(Origin_Share = 100 * Origin_Share / sum(Origin_Share, na.rm = TRUE)) %>%
  ungroup()

# quick confirm it exists and is clean
cat("origin_by_build_year rows:", nrow(origin_by_build_year), "\n")
origin_by_build_year %>%
  group_by(Build_Year, Fuel_Group) %>%
  summarise(s = sum(Origin_Share), .groups = "drop") %>%
  filter(abs(s - 100) > 1) %>% print()   # should be zero rows


# code for publication library(tidyverse)
library(tidyverse)
library(knitr)
library(kableExtra)

# Extract data from your results
fleet_dynamics <- results$accounting %>%
  select(
    Year,
    Starting_Fleet,      # Fleet(t)
    Natural_Retirements, # Retirements(t)
    Predefined_Sales,    # NewSales(t+1)
    Final_Fleet         # Fleet(t+1)
  ) %>%
  mutate(
    # Calculate Survivors using your formula: Survivors(t) = Fleet(t) - Retirements(t)
    Survivors = Starting_Fleet - Natural_Retirements,
    
    # Verify: Fleet(t+1) should equal Survivors + NewSales
    Verification = round(Final_Fleet - (Survivors + Predefined_Sales), 0),
    
    # Convert to millions for easier reading
    Fleet_Start_M = Starting_Fleet / 1e6,
    Survivors_M = Survivors / 1e6,
    Retirements_M = Natural_Retirements / 1e6,
    New_Sales_M = Predefined_Sales / 1e6,
    Fleet_End_M = Final_Fleet / 1e6
  ) %>%
  select(
    Year,
    Fleet_Start_M,
    Survivors_M,
    Retirements_M,
    New_Sales_M,
    Fleet_End_M,
    Verification
  )

# Print the table
print(fleet_dynamics)


# figure for analysis and discussion for our paper 
# Enhanced retirement rate figure
p_retirement_enhanced <- retirement_avg %>%
  mutate(
    Fuel_Group = factor(Fuel_Group, 
                        levels = c("Diesel", "Petrol", "HEV", "PHEV", "BEV"))
  ) %>%
  ggplot(aes(x = Vehicle_Age, y = Retirement_Rate, color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_point(data = . %>% filter(Vehicle_Age %% 5 == 0), size = 2, alpha = 0.7) +
  
  # Add reference lines
  geom_vline(xintercept = c(10, 15, 20), linetype = "dashed", alpha = 0.5) +
  annotate("text", x = c(10, 15, 20), y = 0.22, 
           label = c("Age 10", "Age 15", "Age 20"), 
           size = 3, angle = 90, vjust = -0.5) +
  
  # # Add annotation for diesel disadvantage
  # annotate("text", x = 12, y = 0.08, 
  #          label = "Diesel retirement rates\nconsistently highest", 
  #          size = 3.5, color = "#E41A1C") +
  
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 0.25),
    breaks = seq(0, 0.25, 0.05)
  ) +
  scale_x_continuous(
    limits = c(0, 30),
    breaks = seq(0, 30, 5)
  ) +
  labs(
    #  title = "Annual Retirement Rates by Fuel Type",
    #subtitle = "Probability a vehicle retires in a given year",
    x = "Vehicle Age (years)",
    y = "Annual Retirement Rate",
    color = "Fuel Type",
    # caption = "Diesel vehicles show consistently higher retirement rates across all ages"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    panel.grid.minor = element_blank()
  ) +
  scale_color_brewer(palette = "Set1")

print(p_retirement_enhanced)

# ============================================
# EXTRACT AGE 20 RETIREMENT DATA (2025 → 2026)
# ============================================

# Get fleet data
fleet_2025 <- results$fleet[["2025"]] %>% mutate(Year = 2025)
fleet_2026 <- results$fleet[["2026"]] %>% mutate(Year = 2026)

# Extract age 20 vehicles in 2025 and age 21 vehicles in 2026
age20_retirement <- fleet_2025 %>%
  filter(Vehicle_Age == 20) %>%
  group_by(Fuel_Group) %>%
  summarise(
    `Age 20 Fleet (2025)` = sum(Fleet),
    .groups = "drop"
  ) %>%
  left_join(
    fleet_2026 %>%
      filter(Vehicle_Age == 21) %>%
      group_by(Fuel_Group) %>%
      summarise(
        `Survivors to Age 21 (2026)` = sum(Fleet),
        .groups = "drop"
      ),
    by = "Fuel_Group"
  ) %>%
  mutate(
    `Survivors to Age 21 (2026)` = coalesce(`Survivors to Age 21 (2026)`, 0),
    Retired = `Age 20 Fleet (2025)` - `Survivors to Age 21 (2026)`,
    `Retirement Rate` = Retired / `Age 20 Fleet (2025)`,
    
    # Format for display
    `Age 20 Fleet (2025)` = format(`Age 20 Fleet (2025)`, big.mark = ","),
    `Survivors to Age 21 (2026)` = format(`Survivors to Age 21 (2026)`, big.mark = ","),
    Retired = format(Retired, big.mark = ","),
    `Retirement Rate` = paste0(round(`Retirement Rate` * 100, 1), "%")
  ) %>%
  arrange(Fuel_Group)

# Display the table
cat("\n", strrep("=", 80), "\n")
cat("AGE 20 RETIREMENT ANALYSIS (2025 → 2026)")
cat("\n", strrep("=", 80), "\n")
print(age20_retirement, row.names = FALSE)


# ============================================
# FIXED: MILEAGE-BASED RETIREMENT ANALYSIS
# Using SINGLE YEAR (2024) to avoid time aggregation
# ============================================
library(ggplot2)
library(dplyr)
library(tidyr)


fleet_2024_complete <- fleet_2024_complete %>%
  arrange(Fuel_Group, Vehicle_Age) %>%
  group_by(Fuel_Group) %>%
  mutate(
    Avg_Mileage_Calibrated = zoo::na.approx(
      Avg_Mileage_Calibrated,
      x = Vehicle_Age,
      na.rm = FALSE
    )
  ) %>%
  ungroup()
fleet_2024 <- fleet_2024_complete %>%
  select(Vehicle_Age, Fuel_Group, Fleet_Count, Avg_Mileage_Calibrated) %>%
  filter(!is.na(Avg_Mileage_Calibrated)) %>%
  mutate(
    # Convert to proper miles (divide by 100)
    Annual_Mileage_Miles = Avg_Mileage_Calibrated 
  )

cat("After conversion to miles:\n")
print(head(fleet_2024))

# ============================================
# STEP 2: CALCULATE ANNUAL MILEAGE BY AGE
# ============================================

mileage_by_age <- fleet_2024_complete %>%
  filter(!is.na(Avg_Mileage_Calibrated)) %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(
    Fleet_Total = sum(Fleet_Count, na.rm = TRUE),
    Avg_Annual_Mileage = weighted.mean(
      Avg_Mileage_Calibrated,
      Fleet_Count,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

cat("\nAnnual mileage by age (proper miles/year):\n")
print(head(mileage_by_age, 20))

# ============================================
# STEP 3: VERIFY VALUES ARE REASONABLE
# ============================================

mileage_summary <- mileage_by_age %>%
  group_by(Fuel_Group) %>%
  summarise(
    Avg_Mileage = mean(Avg_Annual_Mileage, na.rm = TRUE),
    Min_Mileage = min(Avg_Annual_Mileage, na.rm = TRUE),
    Max_Mileage = max(Avg_Annual_Mileage, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n Annual mileage summary (proper miles/year):\n")
print(mileage_summary)

# ============================================
# STEP 4: CALCULATE CUMULATIVE MILEAGE
# ============================================

mileage_scenarios <- mileage_by_age %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age) %>%
  mutate(
    Cumulative_Current = cumsum(Avg_Annual_Mileage),
    Cumulative_Shared = cumsum(Avg_Annual_Mileage * 2.5),
    Cumulative_Autonomous = cumsum(Avg_Annual_Mileage * 4.0)
  ) %>%
  ungroup()

# ============================================
# STEP 5: FIND RETIREMENT AGES
# ============================================

retirement_ages <- data.frame()

for(fuel in unique(mileage_scenarios$Fuel_Group)) {
  fuel_data <- mileage_scenarios %>% filter(Fuel_Group == fuel)
  
  # Current: 150,000 miles
  retire_current <- fuel_data %>% filter(Cumulative_Current >= 150000)
  age_current <- if(nrow(retire_current) > 0) min(retire_current$Vehicle_Age) else NA
  
  # Shared: 100,000 miles (2.5x usage)
  retire_shared <- fuel_data %>% filter(Cumulative_Shared >= 100000)
  age_shared <- if(nrow(retire_shared) > 0) min(retire_shared$Vehicle_Age) else NA
  
  # Autonomous: 70,000 miles (4x usage)
  retire_auto <- fuel_data %>% filter(Cumulative_Autonomous >= 70000)
  age_auto <- if(nrow(retire_auto) > 0) min(retire_auto$Vehicle_Age) else NA
  
  retirement_ages <- rbind(retirement_ages,
                           data.frame(Fuel_Group = fuel,
                                      Current_150k = age_current,
                                      Shared_100k = age_shared,
                                      Autonomous_70k = age_auto))
}

cat("\n", strrep("=", 80), "\n")
cat("MILEAGE-BASED RETIREMENT AGES (years) - PROPER MILES")
cat("\n", strrep("=", 80), "\n")
print(retirement_ages)

# ============================================
# STEP 6: VISUALIZE
# ============================================

# Plot annual mileage by age
p_annual <- ggplot(mileage_by_age %>% filter(Fuel_Group %in% c("Petrol", "Diesel", "BEV")), 
                   aes(x = Vehicle_Age, y = Avg_Annual_Mileage, color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Annual Mileage by Age (2024 Data - Proper Miles)",
    subtitle = "Values now realistic: 5,000-15,000 miles/year",
    x = "Vehicle Age (years)", 
    y = "Annual Mileage (miles/year)",
    color = "Fuel Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_annual)

# Plot cumulative mileage for Petrol
p_cumulative <- ggplot(mileage_scenarios %>% filter(Fuel_Group == "Petrol", Vehicle_Age <= 25), 
                       aes(x = Vehicle_Age)) +
  geom_line(aes(y = Cumulative_Current / 1000, color = "Current (150k mi)"), size = 1.2) +
  geom_line(aes(y = Cumulative_Shared / 1000, color = "Shared (100k mi, 2.5x)"), size = 1.2) +
  geom_line(aes(y = Cumulative_Autonomous / 1000, color = "Autonomous (70k mi, 4x)"), size = 1.2) +
  geom_hline(yintercept = c(70, 100, 150), linetype = "dashed", alpha = 0.5) +
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 25)) +
  labs(
    title = "Petrol: Cumulative Mileage (Proper Miles)",
    subtitle = "Annual mileage × years × utilization multiplier",
    x = "Vehicle Age (years)", 
    y = "Cumulative Mileage (thousand miles)",
    color = "Scenario"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_cumulative)

# ============================================
# PLOT CUMULATIVE MILEAGE FOR ALL FUEL TYPES (Shared Scenario)
# ============================================

p_shared_all_fuels <- ggplot(mileage_scenarios %>% filter(Vehicle_Age <= 25), 
                             aes(x = Vehicle_Age, y = Cumulative_Shared / 1000, color = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 100, linetype = "dashed", alpha = 0.5, color = "gray50") +
  annotate("text", x = 22, y = 103, label = "100k miles (Shared threshold)", size = 3.5, hjust = 0) +
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 25)) +
  labs(
    title = "Shared Vehicles (2.5× Annual Mileage): Cumulative Mileage by Fuel Type",
    subtitle = "HEV reaches 100k miles fastest (3 years), Petrol slowest (5 years)",
    x = "Vehicle Age (years)", 
    y = "Cumulative Mileage (thousand miles)",
    color = "Fuel Type",
    caption = "Shared vehicles assumed to retire at 100,000 cumulative miles"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")

print(p_shared_all_fuels)
#ggsave("Shared_Vehicles_All_Fuels.png", p_shared_all_fuels, width = 10, height = 6, dpi = 300)




# ============================================
# BAR CHART: RETIREMENT AGES BY FUEL AND SCENARIO (INCLUDING HYBRID)
# ============================================

# Prepare data including hybrid scenario
retirement_plot_data <- retirement_ages %>%
  mutate(
    Hybrid_30pct = 0.7 * Current_150k + 0.3 * Shared_100k
  ) %>%
  select(Fuel_Group, Current_150k, Shared_100k, Autonomous_70k, Hybrid_30pct) %>%
  pivot_longer(cols = c(Current_150k, Shared_100k, Autonomous_70k, Hybrid_30pct),
               names_to = "Scenario",
               values_to = "Retirement_Years") %>%
  mutate(
    Scenario = case_when(
      Scenario == "Current_150k" ~ "Current (150k miles, 1x)",
      Scenario == "Shared_100k" ~ "Shared (100k miles, 2.5x)",
      Scenario == "Autonomous_70k" ~ "Autonomous (70k miles, 4x)",
      Scenario == "Hybrid_30pct" ~ "Hybrid (30% Shared + 70% Private)",
      TRUE ~ Scenario
    ),
    Scenario = factor(Scenario, levels = c("Current (150k miles, 1x)", 
                                           "Hybrid (30% Shared + 70% Private)",
                                           "Shared (100k miles, 2.5x)",
                                           "Autonomous (70k miles, 4x)"))
  ) %>%
  filter(!is.na(Retirement_Years))

# Create the bar chart
p_retirement_bars <- ggplot(retirement_plot_data, aes(x = Fuel_Group, y = Retirement_Years, fill = Scenario)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = Retirement_Years), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3.5) +
  labs(
    #title = "Retirement Age by Fuel Type and Scenario",
    #subtitle = "Shared vehicles retire in 3-5 years | Hybrid (30% shared) reduces fleet age by 3-6 years",
    x = "Fuel Type", 
    y = "Retirement Age (years)",
    fill = "Scenario",
    #  caption = "Hybrid scenario assumes 30% shared vehicles + 70% private vehicles"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.x = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_manual(values = c(
    "Current (150k miles, 1x)" = "#377EB8",           # Blue
    "Hybrid (30% Shared + 70% Private)" = "#984EA3",  # Purple
    "Shared (100k miles, 2.5x)" = "#E41A1C",          # Red
    "Autonomous (70k miles, 4x)" = "#4DAF4A"          # Green
  ))

print(p_retirement_bars)

# ============================================
# STEP 7: HYBRID RETIREMENT MODEL
# ============================================

hybrid_retirement <- retirement_ages %>%
  mutate(
    Hybrid_30pct_Shared = 0.7 * Current_150k + 0.3 * Shared_100k,
    Hybrid_50pct_Shared = 0.5 * Current_150k + 0.5 * Shared_100k
  )

cat("\n", strrep("=", 80), "\n")
cat("HYBRID RETIREMENT MODEL RESULTS")
cat("\n", strrep("=", 80), "\n")
print(hybrid_retirement %>% select(Fuel_Group, Current_150k, Shared_100k, Hybrid_30pct_Shared))

# ============================================
# STEP 8: SUMMARY FOR PUBLICATION
# ============================================

cat("\n", strrep("=", 80), "\n")
cat("KEY FINDINGS (Proper Miles)")
cat("\n", strrep("=", 80), "\n")

cat("\n1. ANNUAL MILEAGE BY FUEL TYPE (miles/year):\n")
for(i in 1:nrow(mileage_summary)) {
  cat(sprintf("   • %s: ~%s miles/year\n", 
              mileage_summary$Fuel_Group[i],
              format(round(mileage_summary$Avg_Mileage[i], 0), big.mark = ",")))
}

cat("\n2. MILEAGE-BASED RETIREMENT (150k mile threshold):\n")
for(i in 1:nrow(retirement_ages)) {
  if(!is.na(retirement_ages$Current_150k[i])) {
    cat(sprintf("   • %s: %d years\n", 
                retirement_ages$Fuel_Group[i],
                retirement_ages$Current_150k[i]))
  }
}

cat("\n3. SHARED VEHICLES (2.5x annual, 100k threshold):\n")
for(i in 1:nrow(retirement_ages)) {
  if(!is.na(retirement_ages$Shared_100k[i])) {
    cat(sprintf("   • %s: %d years\n", 
                retirement_ages$Fuel_Group[i],
                retirement_ages$Shared_100k[i]))
  }
}

cat("\n4. AUTONOMOUS VEHICLES (4x annual, 70k threshold):\n")
for(i in 1:nrow(retirement_ages)) {
  if(!is.na(retirement_ages$Autonomous_70k[i])) {
    cat(sprintf("   • %s: %d years\n", 
                retirement_ages$Fuel_Group[i],
                retirement_ages$Autonomous_70k[i]))
  }
}



# -------------------------------------------------------------------
# STEP 1: Create modified fuel shares for scenarios 2 and 3 (FIXED)
# -------------------------------------------------------------------
# Scenarios to present in the first paper:
#   
#   1)	Current Government Policy as our Baseline – planned fleet growth, retirement rates as based on existing data and the ZEV mandate EV adoption rates.
# 2)	As 1) but with 70% new car sales as electric by 2030 (and the line from 2026 levels to 2030 levels of EV sales adjusted so it is linear) and 100% by 2035 (again linear from 2030-2035)
# 3)	As 1) but with 60% new car sales as electric by 2030 (and the line from 2026 levels to 2030 levels of EV sales adjusted so it is linear) and 100% by 2035 (again, linear from 2030-2035)
# Then
# 4)	Keep the total planned fleet size as with 1). Keep the ZEV mandate as with 1) but fix the volume of new car sales at 2m per annum (this would mean increasing the retirement age of vehicles to balance the total fleet).
# 

# Function to modify BEV share (linear from 2026 to 2030 to 2035)
modify_bev_shares <- function(base_fuel_shares, target_BEV_2030_pct) {
  
  # Make a copy
  modified_shares <- base_fuel_shares
  
  # Get years from 2025 to 2037
  years <- 2025:2037
  
  # Get actual 2026 BEV share from base data
  BEV_2026 <- base_fuel_shares %>%
    filter(Year == 2026, Fuel_Group == "BEV") %>%
    pull(sale_percentage)
  
  if(length(BEV_2026) == 0) BEV_2026 <- 0.282
  
  # Target BEV values
  BEV_2030 <- target_BEV_2030_pct / 100
  BEV_2035 <- 1.00
  
  # Linear interpolation from 2026 → 2030 → 2035
  BEV_targets <- approx(
    x = c(2026, 2030, 2035),
    y = c(BEV_2026, BEV_2030, BEV_2035),
    xout = years,
    method = "linear"
  )$y
  
  # For years BEFORE 2026, keep original BEV shares
  for(yr in years) {
    if(yr < 2026) {
      original_bev <- base_fuel_shares %>%
        filter(Year == yr, Fuel_Group == "BEV") %>%
        pull(sale_percentage)
      if(length(original_bev) > 0 && !is.na(original_bev)) {
        BEV_targets[years == yr] <- original_bev
      }
    }
  }
  
  # Cap at reasonable bounds
  BEV_targets <- pmax(0, pmin(1, BEV_targets))
  
  # For each year, adjust shares
  for(i in seq_along(years)) {
    yr <- years[i]
    target_bev <- BEV_targets[i]
    
    # Skip if target_bev is NA
    if(is.na(target_bev)) next
    
    # Get current BEV share for this year
    current_bev <- modified_shares$sale_percentage[
      modified_shares$Year == yr & modified_shares$Fuel_Group == "BEV"]
    
    # Skip if current_bev is NA or invalid
    if(length(current_bev) == 0 || is.na(current_bev) || is.nan(current_bev)) next
    
    # If target is same as current (within tolerance), no adjustment needed
    if(!is.na(current_bev) && abs(target_bev - current_bev) < 0.001) next
    
    # If current BEV is already 100% or target is 100%
    if(current_bev >= 0.999 || target_bev >= 0.999) {
      modified_shares$sale_percentage[
        modified_shares$Year == yr & modified_shares$Fuel_Group == "BEV"] <- target_bev
      
      # Set non-BEV fuels to 0 if target is 100%
      if(target_bev >= 0.999) {
        non_bev_idx <- modified_shares$Year == yr & modified_shares$Fuel_Group != "BEV"
        modified_shares$sale_percentage[non_bev_idx] <- 0
      }
      next
    }
    
    # Calculate scaling factor for non-BEV fuels
    # Avoid division by zero
    if((1 - current_bev) > 0.001) {
      scale <- (1 - target_bev) / (1 - current_bev)
    } else {
      scale <- 1
    }
    
    # Update BEV
    modified_shares$sale_percentage[
      modified_shares$Year == yr & modified_shares$Fuel_Group == "BEV"] <- target_bev
    
    # Update non-BEV fuels
    non_bev_idx <- modified_shares$Year == yr & modified_shares$Fuel_Group != "BEV"
    modified_shares$sale_percentage[non_bev_idx] <- 
      modified_shares$sale_percentage[non_bev_idx] * scale
  }
  
  # Replace NaN, NA, Inf with 0, then renormalize each year to sum to 1
  modified_shares <- modified_shares %>%
    group_by(Year) %>%
    mutate(
      sale_percentage = ifelse(is.na(sale_percentage) | is.nan(sale_percentage) | is.infinite(sale_percentage), 0, sale_percentage),
      # Renormalize to sum to 1 (only if total > 0)
      sale_percentage = if(sum(sale_percentage, na.rm = TRUE) > 0) sale_percentage / sum(sale_percentage, na.rm = TRUE) else sale_percentage
    ) %>%
    ungroup()
  
  return(modified_shares)
}

# -------------------------------------------------------------------
# STEP 2: Create fuel shares for all scenarios
# -------------------------------------------------------------------

# Scenario 1: Baseline (original fuel shares)
fuel_shares_baseline <- fuel_shares_complete
print(fuel_shares_baseline,n=Inf)
# Scenario 2: 70% BEV by 2030
fuel_shares_70pct <- modify_bev_shares(fuel_shares_complete, 70)
print(fuel_shares_70pct,n=Inf)
# Remove any remaining NaN values
fuel_shares_70pct <- fuel_shares_70pct %>%
  mutate(sale_percentage = ifelse(is.nan(sale_percentage), 0, sale_percentage))

# Scenario 3: 60% BEV by 2030
fuel_shares_60pct <- modify_bev_shares(fuel_shares_complete, 60)
# Remove any remaining NaN values
fuel_shares_60pct <- fuel_shares_60pct %>%
  mutate(sale_percentage = ifelse(is.nan(sale_percentage), 0, sale_percentage))

# -------------------------------------------------------------------
# STEP 3: Verify modified BEV shares and check for NAs
# -------------------------------------------------------------------

cat("\n=== VERIFYING MODIFIED BEV SHARES ===\n")
cat("\nBaseline BEV shares (2030):", 
    fuel_shares_baseline %>% filter(Year == 2030, Fuel_Group == "BEV") %>% pull(sale_percentage))
cat("\n70% Scenario BEV shares (2030):", 
    fuel_shares_70pct %>% filter(Year == 2030, Fuel_Group == "BEV") %>% pull(sale_percentage))
cat("\n60% Scenario BEV shares (2030):", 
    fuel_shares_60pct %>% filter(Year == 2030, Fuel_Group == "BEV") %>% pull(sale_percentage))

# Check for NAs
cat("\n\n=== NA CHECK ===\n")
cat("Baseline NAs:", sum(is.na(fuel_shares_baseline$sale_percentage)), "\n")
cat("70% Scenario NAs:", sum(is.na(fuel_shares_70pct$sale_percentage)), "\n")
cat("60% Scenario NAs:", sum(is.na(fuel_shares_60pct$sale_percentage)), "\n")

# Verify 2025 fuel shares for 70% scenario
cat("\n=== 2025 FUEL SHARES (70% Scenario) ===\n")
fuel_shares_70pct %>% filter(Year == 2025) %>% select(Fuel_Group, sale_percentage) %>% print()

# Verify sums to 1 for 70% scenario
cat("\n=== SUM CHECK (70% Scenario - 2025) ===\n")
sum_2025 <- sum(fuel_shares_70pct %>% filter(Year == 2025) %>% pull(sale_percentage))
cat("Sum of 2025 fuel shares:", sum_2025, "\n")

# -------------------------------------------------------------------
# STEP 4: Run Scenarios 1-3 using existing project_fleet function
# -------------------------------------------------------------------

cat("\n\n>>> RUNNING SCENARIO 1: Baseline (ZEV mandate) <<<\n")
results_baseline <- project_fleet(
  base_fleet = base_fleet_2024,
  retirement_rates = retirement_avg,
  fuel_shares = fuel_shares_baseline,
  natcop_targets = natcop_targets,
  engine_sales_distribution = engine_sales_distribution   # ← ADD THIS
)

cat("\n\n>>> RUNNING SCENARIO 2: 70% BEV by 2030 <<<\n")
results_70pct <- project_fleet(
  base_fleet = base_fleet_2024,
  retirement_rates = retirement_avg,
  fuel_shares = fuel_shares_70pct,
  natcop_targets = natcop_targets,
  engine_sales_distribution = engine_sales_distribution   # ← ADD THIS
)

cat("\n\n>>> RUNNING SCENARIO 3: 60% BEV by 2030 <<<\n")
results_60pct <- project_fleet(
  base_fleet = base_fleet_2024,
  retirement_rates = retirement_avg,
  fuel_shares = fuel_shares_60pct,
  natcop_targets = natcop_targets,
  engine_sales_distribution = engine_sales_distribution   # ← ADD THIS
)
# -------------------------------------------------------------------
# STEP 5: Verify sales_by_fuel for 2025
# -------------------------------------------------------------------

cat("\n\n=== VERIFYING SALES BY FUEL (2025) ===\n")
cat("\nBaseline 2025 sales:\n")
results_baseline$sales_by_fuel %>% filter(Year == 2026) %>% print()

cat("\n70% Scenario 2025 sales:\n")
results_70pct$sales_by_fuel %>% filter(Year == 2026) %>% print()

cat("\n60% Scenario 2025 sales:\n")
results_60pct$sales_by_fuel %>% filter(Year == 2026) %>% print()




#2030 sales
cat("\n=== 2030 SALES COMPARISON ===\n")

cat("\nBaseline 2030 sales:\n")
results_baseline$sales_by_fuel %>% filter(Year == 2030) %>% print()

cat("\n70% Scenario 2030 sales:\n")
results_70pct$sales_by_fuel %>% filter(Year == 2030) %>% print()

cat("\n60% Scenario 2030 sales:\n")
results_60pct$sales_by_fuel %>% filter(Year == 2030) %>% print()   



# Calculate total sales 2025-2037 for each scenario
total_baseline <- sum(results_baseline$accounting$Predefined_Sales)
total_70pct <- sum(results_70pct$accounting$Predefined_Sales)
total_60pct <- sum(results_60pct$accounting$Predefined_Sales)

cat("\n=== TOTAL SALES (2025-2037) ===\n")
cat("Baseline:", round(total_baseline / 1e6, 2), "M\n")
cat("70% Scenario:", round(total_70pct / 1e6, 2), "M\n")
cat("60% Scenario:", round(total_60pct / 1e6, 2), "M\n")
# -------------------------------------------------------------------
# STEP 3: Verify modified BEV shares and check for NAs
# -------------------------------------------------------------------

cat("\n=== VERIFYING MODIFIED BEV SHARES ===\n")
cat("\nBaseline BEV shares (2030):", 
    fuel_shares_baseline %>% filter(Year == 2030, Fuel_Group == "BEV") %>% pull(sale_percentage))
cat("\n70% Scenario BEV shares (2030):", 
    fuel_shares_70pct %>% filter(Year == 2030, Fuel_Group == "BEV") %>% pull(sale_percentage))
cat("\n60% Scenario BEV shares (2030):", 
    fuel_shares_60pct %>% filter(Year == 2030, Fuel_Group == "BEV") %>% pull(sale_percentage))

# Check for NAs
cat("\n\n=== NA CHECK ===\n")
cat("Baseline NAs:", sum(is.na(fuel_shares_baseline$sale_percentage)), "\n")
cat("70% Scenario NAs:", sum(is.na(fuel_shares_70pct$sale_percentage)), "\n")
cat("60% Scenario NAs:", sum(is.na(fuel_shares_60pct$sale_percentage)), "\n")

# Verify sums to 1
cat("\n=== SUM CHECK (70% Scenario - first 3 years) ===\n")
fuel_shares_70pct %>%
  filter(Year %in% c(2025, 2026, 2027)) %>%
  group_by(Year) %>%
  summarise(Total = sum(sale_percentage)) %>%
  print()

# -------------------------------------------------------------------
# STEP 4: Run Scenarios 1-3 using existing project_fleet function
# -------------------------------------------------------------------

cat("\n\n>>> RUNNING SCENARIO 1: Baseline (ZEV mandate) <<<\n")
results_baseline <- project_fleet(
  base_fleet = base_fleet_2024,
  retirement_rates = retirement_avg,
  fuel_shares = fuel_shares_baseline,
  natcop_targets = natcop_targets,
  engine_sales_distribution = engine_sales_distribution   # ← Make sure this parameter is included!
)
print(results_baseline)

cat("\n\n>>> RUNNING SCENARIO 2: 70% BEV by 2030 <<<\n")
results_70pct <- project_fleet(
  base_fleet = base_fleet_2024,
  retirement_rates = retirement_avg,
  fuel_shares = fuel_shares_70pct,
  natcop_targets = natcop_targets,
  engine_sales_distribution = engine_sales_distribution   # ← Add this!
)
print(results_70pct)
cat("\n\n>>> RUNNING SCENARIO 3: 60% BEV by 2030 <<<\n")
results_60pct <- project_fleet(
  base_fleet = base_fleet_2024,
  retirement_rates = retirement_avg,
  fuel_shares = fuel_shares_60pct,
  natcop_targets = natcop_targets,
  engine_sales_distribution = engine_sales_distribution   # ← Add this!
)
print(results_60pct)
# -------------------------------------------------------------------
# STEP 5: Scenario 4 - Fixed sales at 2M (using Baseline fuel shares)
# -------------------------------------------------------------------

# ============================================
# UPDATED: project_fleet_fixed_sales (with automatic retirement adjustment)
# ============================================

project_fleet_fixed_sales <- function(base_fleet, retirement_rates, fuel_shares, 
                                      natcop_targets, engine_sales_distribution, fixed_sales = 2000000) {
  
  # Make a copy of retirement rates to adjust dynamically
  current_retirement_rates <- retirement_rates
  
  # Iterate to find correct retirement rates (max 15 iterations)
  for(iter in 1:15) {
    
    fleet_list <- list()
    fleet_list[["2024"]] <- base_fleet %>%
      rename(Fleet = Fleet_Count) %>%
      mutate(Year = 2024)
    
    accounting <- data.frame()
    sales_by_fuel_list <- list()
    
    current_fleet <- base_fleet %>% rename(Fleet = Fleet_Count)
    
    for (yr in 2025:2037) {
      
      target_fleet <- natcop_targets %>% filter(Year == yr) %>% pull(Target_Fleet)
      
      # Age the fleet with current retirement rates
      aged_fleet_all <- current_fleet %>%
        left_join(current_retirement_rates, by = c("Vehicle_Age", "Fuel_Group")) %>%
        mutate(
          Retirement_Rate = coalesce(Retirement_Rate, 0),
          Survival_Rate = 1 - Retirement_Rate,
          Survivors = round(Fleet * Survival_Rate, 0),
          Natural_Retirements = Fleet - Survivors,
          Vehicle_Age = Vehicle_Age + 1
        )
      
      aged_fleet <- aged_fleet_all %>% filter(Vehicle_Age <= 30)
      total_survivors <- sum(aged_fleet$Survivors)
      
      # FORCE fixed sales
      required_total_sales <- fixed_sales
      
      # Distribute fixed sales by fuel shares
      year_shares <- fuel_shares %>% filter(Year == yr)
      
      if(nrow(year_shares) > 0) {
        year_sales <- year_shares %>%
          mutate(
            Required_Sales = round(sale_percentage * required_total_sales, 0),
            Vehicle_Age = 0
          )
        actual_new_sales <- sum(year_sales$Required_Sales)
        
        if(actual_new_sales != required_total_sales) {
          diff <- required_total_sales - actual_new_sales
          largest_idx <- which.max(year_sales$Required_Sales)
          year_sales$Required_Sales[largest_idx] <- year_sales$Required_Sales[largest_idx] + diff
        }
      } else {
        year_sales <- expand.grid(
          Year = yr,
          Fuel_Group = c("Petrol", "Diesel", "BEV", "PHEV", "HEV"),
          sale_percentage = 0.2,
          Required_Sales = round(0.2 * required_total_sales, 0),
          Vehicle_Age = 0
        )
      }
      
      # Add new vehicles
      fleet_with_sales <- aged_fleet %>%
        select(Vehicle_Age, Fuel_Group, Fleet = Survivors)
      
      new_vehicles <- year_sales %>%
        rename(Fleet = Required_Sales) %>%
        mutate(Year = yr) %>%
        select(Year, Vehicle_Age, Fuel_Group, Fleet) %>%
        filter(Fleet > 0)
      
      combined_fleet <- bind_rows(
        fleet_with_sales %>% mutate(Year = yr),
        new_vehicles
      )
      
      final_fleet_total <- sum(combined_fleet$Fleet)
      total_natural_retirements <- sum(current_fleet$Fleet) - total_survivors
      
      # Record accounting
      accounting <- bind_rows(accounting, data.frame(
        Year = yr,
        Starting_Fleet = sum(current_fleet$Fleet),
        Target_Fleet = target_fleet,
        Fixed_Sales = required_total_sales,
        Natural_Retirements = total_natural_retirements,
        Final_Fleet = final_fleet_total,
        Error = final_fleet_total - target_fleet,
        Pct_Error = 100 * (final_fleet_total - target_fleet) / target_fleet
      ))
      
      sales_by_fuel_list[[as.character(yr)]] <- year_sales %>% mutate(Year = yr)
      fleet_list[[as.character(yr)]] <- combined_fleet
      current_fleet <- combined_fleet %>% select(Vehicle_Age, Fuel_Group, Fleet)
    }
    
    # Check final error (2037 fleet vs target)
    final_error <- accounting %>% filter(Year == 2037) %>% pull(Error)
    
    # If within 0.5% tolerance, stop iterating
    if(abs(final_error) < 0.005 * (natcop_targets %>% filter(Year == 2037) %>% pull(Target_Fleet))) {
      cat("Converged after", iter, "iterations. Final fleet error:", round(final_error), "vehicles\n")
      break
    }
    
    # Adjust retirement rates based on error
    # If fleet is too small (negative error), reduce retirement rates (keep cars longer)
    # If fleet is too large (positive error), increase retirement rates (scrap more cars)
    if(final_error < 0) {
      # Fleet too small -> need to retire LESS -> reduce retirement rates by 5%
      adjustment <- 0.95
    } else {
      # Fleet too large -> need to retire MORE -> increase retirement rates by 5%
      adjustment <- 1.05
    }
    
    # Apply adjustment to retirement rates (only for ages where retirement is not already 0 or 1)
    current_retirement_rates <- current_retirement_rates %>%
      mutate(
        Retirement_Rate = case_when(
          Retirement_Rate <= 0.01 ~ 0,
          Retirement_Rate >= 0.99 ~ 1,
          TRUE ~ pmin(0.99, pmax(0.01, Retirement_Rate * adjustment))
        )
      )
    
    cat("Iteration", iter, "- Error:", round(final_error), "- Adjusted retirement rates by", 
        ifelse(final_error < 0, "-5%", "+5%"), "\n")
  }
  
  return(list(
    fleet = fleet_list,
    accounting = accounting,
    sales_by_fuel = bind_rows(sales_by_fuel_list)
  ))
}

cat("\n\n>>> RUNNING SCENARIO 4: Fixed 2M Sales (Balanced Fleet) <<<\n")
results_scenario4 <- project_fleet_fixed_sales(
  base_fleet = base_fleet_2024,
  retirement_rates = retirement_avg,
  fuel_shares = fuel_shares_baseline,
  natcop_targets = natcop_targets,
  fixed_sales = 2000000
)
# -------------------------------------------------------------------
# STEP 6: Extract and compare results
# -------------------------------------------------------------------

# Extract metrics function
# Fixed extract_scenario_metrics function
extract_scenario_metrics <- function(results, scenario_name, sales_type = "Predefined") {
  
  # Fix: Handle case where results$fleet might be a list
  if(is.list(results$fleet)) {
    fleet_2037 <- bind_rows(results$fleet) %>% filter(Year == 2037)
  } else {
    fleet_2037 <- results$fleet %>% filter(Year == 2037)
  }
  
  # Calculate total sales
  if(sales_type == "Predefined") {
    if("Predefined_Sales" %in% colnames(results$accounting)) {
      total_sales <- sum(results$accounting$Predefined_Sales, na.rm = TRUE)
    } else if("Fixed_Sales" %in% colnames(results$accounting)) {
      total_sales <- sum(results$accounting$Fixed_Sales, na.rm = TRUE)
    } else {
      total_sales <- NA
    }
  } else {
    if("Fixed_Sales" %in% colnames(results$accounting)) {
      total_sales <- sum(results$accounting$Fixed_Sales, na.rm = TRUE)
    } else {
      total_sales <- NA
    }
  }
  
  # Calculate BEV share in 2037
  bev_share_2037 <- fleet_2037 %>%
    group_by(Fuel_Group) %>%
    summarise(Fleet = sum(Fleet, na.rm = TRUE)) %>%
    mutate(Share = Fleet / sum(Fleet) * 100) %>%
    filter(Fuel_Group == "BEV") %>%
    pull(Share)
  if(length(bev_share_2037) == 0) bev_share_2037 <- 0
  
  # Calculate average fleet age in 2037
  avg_age_2037 <- fleet_2037 %>%
    summarise(Age = sum(Vehicle_Age * Fleet, na.rm = TRUE) / sum(Fleet, na.rm = TRUE)) %>%
    pull(Age)
  
  return(data.frame(
    Scenario = scenario_name,
    Total_Sales_2025_2037_M = round(total_sales / 1e6, 2),
    BEV_Share_2037_pct = round(bev_share_2037, 1),
    Avg_Fleet_Age_2037 = round(avg_age_2037, 1)
  ))
}

# Extract all metrics
metrics_baseline <- extract_scenario_metrics(results_baseline, "Baseline (ZEV mandate)", "Predefined")
metrics_70pct <- extract_scenario_metrics(results_70pct, "70% BEV by 2030", "Predefined")
metrics_60pct <- extract_scenario_metrics(results_60pct, "60% BEV by 2030", "Predefined")
metrics_scenario4 <- extract_scenario_metrics(results_scenario4, "Scenario 4 (Fixed 2M Sales)", "Fixed")

# Combine all results
all_scenario_metrics <- bind_rows(
  metrics_baseline,
  metrics_70pct,
  metrics_60pct,
  metrics_scenario4
)

cat("\n\n", strrep("=", 80), "\n")
cat("FINAL SCENARIO COMPARISON SUMMARY")
cat("\n", strrep("=", 80), "\n")
print(all_scenario_metrics)

# -------------------------------------------------------------------
# STEP 7: Verify Scenario 4 sales are fixed at 2M
# -------------------------------------------------------------------

cat("\n\n", strrep("=", 80), "\n")
cat("SCENARIO 4 SALES VERIFICATION (should be 2M each year)")
cat("\n", strrep("=", 80), "\n")
results_scenario4$accounting %>%
  select(Year, Fixed_Sales) %>%
  head(5) %>%
  print()


# ============================================
# COMBINED FIGURE: BEV Share, New Sales & Fleet Age
# Using results from your scenario runs
# ============================================

library(ggplot2)
library(patchwork)
library(scales)
library(dplyr)
library(tidyr)

# -------------------------------------------------------------------
# STEP 1: Prepare BEV Share data (from fuel shares)
# -------------------------------------------------------------------

# Create BEV comparison from fuel shares
bev_comparison <- bind_rows(
  fuel_shares_baseline %>% filter(Fuel_Group == "BEV", Year >= 2025, Year <= 2035) %>% mutate(Scenario = "Baseline"),
  fuel_shares_70pct %>% filter(Fuel_Group == "BEV", Year >= 2025, Year <= 2035) %>% mutate(Scenario = "70% by 2030"),
  fuel_shares_60pct %>% filter(Fuel_Group == "BEV", Year >= 2025, Year <= 2035) %>% mutate(Scenario = "60% by 2030")
) %>%
  select(Scenario, Year, BEV_Share = sale_percentage) %>%
  mutate(BEV_Share = round(BEV_Share * 100, 1))

# Add Scenario 4 (same as Baseline for BEV share)
bev_scenario4 <- bev_comparison %>%
  filter(Scenario == "Baseline") %>%
  mutate(Scenario = "Scenario 4")

bev_comparison_all <- bind_rows(bev_comparison, bev_scenario4)

# Convert to long format for plotting
bev_long <- bev_comparison_all %>%
  mutate(
    Year = as.numeric(Year),
    BEV_Share = as.numeric(BEV_Share),
    Metric = "BEV Share (%)"
  ) %>%
  rename(Value = BEV_Share)

# -------------------------------------------------------------------
# STEP 2: Prepare New Sales data from accounting results
# -------------------------------------------------------------------

# Baseline sales
sales_baseline <- results_baseline$sales_by_fuel %>%
  filter(Fuel_Group %in% c("BEV")) %>%  # ← ADD THIS LINE
  group_by(Year) %>%
  summarise(New_Sales = sum(Required_Sales, na.rm = TRUE)) %>%
  mutate(
    Scenario = "Baseline",
    New_Sales_M = New_Sales / 1e6,
    Metric = "New Sales (M vehicles)"
  )

# 70% scenario sales
sales_70pct <- results_70pct$sales_by_fuel %>%
  filter(Fuel_Group %in% c("BEV")) %>%  # ← ADD THIS LINE
  group_by(Year) %>%
  summarise(New_Sales = sum(Required_Sales, na.rm = TRUE)) %>%
  mutate(
    Scenario = "70% by 2030",
    New_Sales_M = New_Sales / 1e6,
    Metric = "New Sales (M vehicles)"
  )

# 60% scenario sales
sales_60pct <- results_60pct$sales_by_fuel %>%
  filter(Fuel_Group %in% c("BEV")) %>%  # ← ADD THIS LINE
  group_by(Year) %>%
  summarise(New_Sales = sum(Required_Sales, na.rm = TRUE)) %>%
  mutate(
    Scenario = "60% by 2030",
    New_Sales_M = New_Sales / 1e6,
    Metric = "New Sales (M vehicles)"
  )

# Scenario 4 sales (uses Fixed_Sales)
sales_scenario4 <- results_scenario4$sales_by_fuel %>%
  filter(Fuel_Group %in% c("BEV")) %>%  # ← ADD THIS LINE
  group_by(Year) %>%
  summarise(New_Sales = sum(Required_Sales, na.rm = TRUE)) %>%
  mutate(
    Scenario = "Scenario 4",
    New_Sales_M = New_Sales / 1e6,
    Metric = "New Sales (M vehicles)"
  )

# Combine all sales
sales_long <- bind_rows(sales_baseline, sales_70pct, sales_60pct, sales_scenario4) %>%
  select(Year, Scenario, Value = New_Sales_M, Metric)

# -------------------------------------------------------------------
# STEP 3: Prepare Fleet Age data from fleet results
# -------------------------------------------------------------------

# Function to extract age data
get_age_data <- function(fleet_list, scenario_name) {
  bind_rows(fleet_list) %>%
    group_by(Year) %>%
    summarise(
      Avg_Age = sum(Vehicle_Age * Fleet, na.rm = TRUE) / sum(Fleet, na.rm = TRUE)
    ) %>%
    mutate(
      Scenario = scenario_name,
      Metric = "Avg Fleet Age (years)"
    ) %>%
    rename(Value = Avg_Age)
}

# Extract age for all scenarios
age_baseline <- get_age_data(results_baseline$fleet, "Baseline")
age_70pct <- get_age_data(results_70pct$fleet, "70% by 2030")
age_60pct <- get_age_data(results_60pct$fleet, "60% by 2030")
age_scenario4 <- get_age_data(results_scenario4$fleet, "Scenario 4")

# Combine all age data
age_long <- bind_rows(age_baseline, age_70pct, age_60pct, age_scenario4)

# -------------------------------------------------------------------
# STEP 4: Combine all metrics
# -------------------------------------------------------------------

all_metrics <- bind_rows(
  bev_long %>% select(Year, Scenario, Value, Metric),
  sales_long,
  age_long
) %>%
  mutate(
    Scenario = factor(Scenario, levels = c("Baseline", "70% by 2030", "60% by 2030", "Scenario 4")),
    Metric = factor(Metric, levels = c("BEV Share (%)", "New Sales (M vehicles)", "Avg Fleet Age (years)"))
  )

# -------------------------------------------------------------------
# STEP 5: Create individual plots
# -------------------------------------------------------------------

# Plot 1: BEV Share (%)
p1 <- all_metrics %>%
  filter(Metric == "BEV Share (%)") %>%
  ggplot(aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5, aes(shape = Scenario)) +
  geom_vline(xintercept = 2030, linetype = "dashed", color = "gray50", size = 0.5) +
  geom_vline(xintercept = 2035, linetype = "dashed", color = "gray50", size = 0.5) +
  scale_x_continuous(breaks = seq(2025, 2035, 2), limits = c(2025, 2035)) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%")) +
  scale_color_manual(values = c(
    "Baseline" = "#2E86AB",
    "70% by 2030" = "#F18F01",
    "60% by 2030" = "#C73E1D",
    "Scenario 4" = "#4DAF4A"
  )) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  labs(
    title = "A) BEV Share of New Sales",
    x = NULL,
    y = "BEV Share (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Plot 2: New Sales (M vehicles)
p2 <- all_metrics %>%
  filter(Metric == "New Sales (M vehicles)") %>%
  ggplot(aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5, aes(shape = Scenario)) +
  scale_x_continuous(breaks = seq(2025, 2035, 2), limits = c(2025, 2035)) +
  scale_y_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, 0.5),
                     labels = function(x) paste0(x, "M")) +
  scale_color_manual(values = c(
    "Baseline" = "#2E86AB",
    "70% by 2030" = "#F18F01",
    "60% by 2030" = "#C73E1D",
    "Scenario 4" = "#4DAF4A"
  )) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  labs(
    title = "B) Annual New BEVs Sales",
    x = NULL,
    y = "New Sales (millions)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Plot 3: Average Fleet Age (years)
p3 <- all_metrics %>%
  filter(Metric == "Avg Fleet Age (years)") %>%
  ggplot(aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5, aes(shape = Scenario)) +
  scale_x_continuous(breaks = seq(2025, 2035, 2), limits = c(2025, 2035)) +
  scale_y_continuous(limits = c(8.5, 10.5), breaks = seq(8.5, 10.5, 0.5)) +
  scale_color_manual(name = "Scenario", values = c(
    "Baseline" = "#2E86AB",
    "70% by 2030" = "#F18F01",
    "60% by 2030" = "#C73E1D",
    "Scenario 4" = "#4DAF4A"
  )) +
  scale_shape_manual(name = "Scenario", values = c(16, 17, 15, 18)) +
  labs(
    title = "C) Average Fleet Age",
    x = "Year",
    y = "Age (years)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",  # Remove legend from p3 - we'll add it globally
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# -------------------------------------------------------------------
# STEP 6: Combine all three plots
# -------------------------------------------------------------------

combined_figure <- (p1 | p2) / p3 +
  plot_annotation(
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5)
    )
  ) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

# Save and display
#ggsave("Figure_Combined_Scenarios.png", combined_figure, 
# width = 12, height = 10, dpi = 300, bg = "white")

print(combined_figure)

# -------------------------------------------------------------------
# STEP 8: Summary table for key years
# -------------------------------------------------------------------

cat("\n\n", strrep("=", 80), "\n")
cat("SCENARIO COMPARISON SUMMARY (Key Years)")
cat("\n", strrep("=", 80), "\n")

key_years <- c(2025, 2028, 2030, 2032, 2035)

summary_table <- all_metrics %>%
  filter(Year %in% key_years) %>%
  pivot_wider(
    id_cols = c(Scenario, Year),
    names_from = Metric,
    values_from = Value
  ) %>%
  arrange(Scenario, Year) %>%
  mutate(
    `BEV Share (%)` = round(`BEV Share (%)`, 1),
    `New Sales (M)` = round(`New Sales (M vehicles)`, 2),
    `Fleet Age (yrs)` = round(`Avg Fleet Age (years)`, 1)
  ) %>%
  select(Scenario, Year, `BEV Share (%)`, `New Sales (M)`, `Fleet Age (yrs)`)

print(summary_table, n = 20)



# ============================================================================
# SIMPLE EMISSIONS CALCULATION - CORRECTED
# ============================================================================

library(tidyr)
library(dplyr)
library(ggplot2)
library(zoo)

# ----------------------------------------------------------------------------
# PART 1: GET EMISSIONS DATA FROM 2024 FLEET
# ----------------------------------------------------------------------------

# 1.1 Get age-based emissions and mileage
paper_age_table <- results$fleet[["2024"]] %>%
  mutate(age_band = case_when(
    Vehicle_Age <= 5 ~ "0-5",
    Vehicle_Age <= 10 ~ "6-10",
    Vehicle_Age <= 15 ~ "11-15",
    Vehicle_Age <= 20 ~ "16-20",
    Vehicle_Age <= 25 ~ "21-25",
    TRUE ~ "26-30"
  )) %>%
  filter(!is.na(Avg_CO2)) %>%
  group_by(Fuel_Group, age_band) %>%
  summarise(
    Avg_CO2_gkm = weighted.mean(Avg_CO2, Fleet, na.rm = TRUE),
    Avg_Mileage = weighted.mean(Avg_Mileage_Calibrated, Fleet, na.rm = TRUE),
    .groups = "drop"
  )

print(paper_age_table %>% filter(Fuel_Group != "BEV"), n = Inf)

# 1.2 Get emissions by fuel and vehicle class
paper_emissions_table <- results$fleet[["2024"]] %>%
  filter(Fuel_Group %in% c("Petrol", "Diesel", "HEV", "PHEV", "BEV"), !is.na(Avg_CO2)) %>%
  mutate(
    vehicle_class_clean = case_when(
      grepl("^Small", vehicle_class) ~ "Small",
      grepl("^Medium", vehicle_class) ~ "Medium",
      grepl("^Large", vehicle_class) ~ "Large",
      grepl("^Very Large", vehicle_class) ~ "Very Large",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(vehicle_class_clean)) %>%
  group_by(vehicle_class_clean, Fuel_Group) %>%
  summarise(
    Avg_CO2_gkm = weighted.mean(Avg_CO2, Fleet, na.rm = TRUE),
    Avg_Mileage_miles = weighted.mean(Avg_Mileage_Calibrated, Fleet, na.rm = TRUE),
    N_Vehicles = sum(Fleet, na.rm = TRUE),
    .groups = "drop"
  )

print(paper_emissions_table, n = Inf)

# ----------------------------------------------------------------------------
# PART 2: CREATE LOOKUP TABLES
# ----------------------------------------------------------------------------

# 2.1 Annual mileage lookup (km per fuel + vehicle class)
annual_km_lookup <- paper_emissions_table %>%
  filter(Fuel_Group != "BEV") %>%
  select(Fuel_Group, vehicle_class = vehicle_class_clean, annual_km = Avg_Mileage_miles) %>%
  mutate(annual_km = round(annual_km * 1.60934, 0))

print(annual_km_lookup, n = Inf)

# 2.2 Size multipliers (per fuel + vehicle class)
size_multiplier_from_data <- paper_emissions_table %>%
  mutate(vehicle_class_clean = ifelse(
    Fuel_Group == "HEV" & vehicle_class_clean == "Large", "Very Large", vehicle_class_clean
  )) %>%
  group_by(vehicle_class_clean, Fuel_Group) %>%
  summarise(
    Avg_CO2_gkm = weighted.mean(Avg_CO2_gkm, N_Vehicles, na.rm = TRUE),
    Avg_Mileage_miles = weighted.mean(Avg_Mileage_miles, N_Vehicles, na.rm = TRUE),
    N_Vehicles = sum(N_Vehicles),
    .groups = "drop"
  ) %>%
  group_by(Fuel_Group) %>%
  mutate(
    fuel_avg_CO2 = weighted.mean(Avg_CO2_gkm, N_Vehicles, na.rm = TRUE),
    fuel_avg_Mi = weighted.mean(Avg_Mileage_miles, N_Vehicles, na.rm = TRUE),
    size_multiplier_Co2 = Avg_CO2_gkm / fuel_avg_CO2,
    size_multiper_Mileage = Avg_Mileage_miles / fuel_avg_Mi
  ) %>%
  ungroup() %>%
  select(Fuel_Group, vehicle_class = vehicle_class_clean, 
         size_multiplier_Co2, size_multiper_Mileage)

print(size_multiplier_from_data %>% filter(Fuel_Group != "BEV"), n = Inf)

# ----------------------------------------------------------------------------
# PART 3: EMISSION FACTORS AND TRAJECTORY
# ----------------------------------------------------------------------------

# 3.1 Constants
HEV_REALWORLD_UPLIFT <- 1.15
PHEV_REALWORLD_GKM <- 139

# 3.2 Greg's ICE trajectory (new car targets)
greg_ice_trajectory <- tibble(
  Year = 2027:2035,
  Target_blend = c(147, 145, 139, 130, 130, 130, 130, 130, 130)
)

# 3.3 Fuel-level multipliers from 2024 data
fuel_level_avg <- paper_emissions_table %>%
  filter(Fuel_Group %in% c("Petrol", "Diesel", "HEV")) %>%
  group_by(Fuel_Group) %>%
  summarise(fuel_avg_CO2 = weighted.mean(Avg_CO2_gkm, N_Vehicles), .groups = "drop") %>%
  mutate(fuel_avg_CO2_corrected = ifelse(Fuel_Group == "HEV",
                                         fuel_avg_CO2 * HEV_REALWORLD_UPLIFT,
                                         fuel_avg_CO2))

ice_baseline <- fuel_level_avg %>%
  filter(Fuel_Group %in% c("Petrol", "Diesel")) %>%
  summarise(ice_avg = weighted.mean(fuel_avg_CO2_corrected,
                                    paper_emissions_table %>%
                                      filter(Fuel_Group %in% c("Petrol", "Diesel")) %>%
                                      group_by(Fuel_Group) %>%
                                      summarise(n = sum(N_Vehicles)) %>%
                                      pull(n))) %>%
  pull(ice_avg)

fuel_level_multiplier <- fuel_level_avg %>%
  mutate(multiplier = fuel_avg_CO2_corrected / ice_baseline) %>%
  select(Fuel_Group, multiplier)

print(fuel_level_multiplier)

# 3.4 Reconcile targets for each year (with zero-share guard)
reconcile_year <- function(target_blend, year, shares) {
  s_raw <- shares %>% 
    filter(Year == year, Fuel_Group %in% c("Petrol", "Diesel", "HEV", "PHEV"))
  
  total_share <- sum(s_raw$sale_percentage, na.rm = TRUE)
  
  # If no shares exist for this year, return NA
  if (total_share <= 0) {
    return(tibble(Fuel_Group = c("Petrol", "Diesel", "HEV", "PHEV"), 
                  CO2 = NA_real_, 
                  Year = year))
  }
  
  s <- s_raw %>% mutate(sale_percentage = sale_percentage / total_share)
  w_phev <- s$sale_percentage[s$Fuel_Group == "PHEV"]
  scaled <- s %>% 
    filter(Fuel_Group != "PHEV") %>% 
    left_join(fuel_level_multiplier, by = "Fuel_Group")
  
  denom <- sum(scaled$sale_percentage * scaled$multiplier, na.rm = TRUE)
  
  if (denom <= 0) {
    scaled_co2 <- tibble(Fuel_Group = scaled$Fuel_Group, CO2 = NA_real_)
  } else {
    k <- (target_blend - w_phev * PHEV_REALWORLD_GKM) / denom
    scaled_co2 <- scaled %>% 
      mutate(CO2 = multiplier * k) %>% 
      select(Fuel_Group, CO2)
  }
  
  scaled_co2 %>%
    bind_rows(tibble(Fuel_Group = "PHEV", 
                     CO2 = ifelse(w_phev > 0, PHEV_REALWORLD_GKM, NA_real_))) %>%
    mutate(Year = year)
}

# Generate fuel trajectory by year
fuel_trajectory_by_year <- purrr::map2_dfr(
  greg_ice_trajectory$Target_blend, 
  greg_ice_trajectory$Year,
  ~ reconcile_year(.x, .y, fuel_shares_complete)
) %>%
  group_by(Fuel_Group) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(CO2 = zoo::na.locf(CO2, na.rm = FALSE)) %>%
  ungroup()

print(fuel_trajectory_by_year, n = Inf)

# ============================================================================
# STEP 1: REAL 2024 NEW-CAR ANCHOR
# ============================================================================
real_new_car_anchor <- results$fleet[["2024"]] %>%
  filter(Fuel_Group %in% c("Petrol","Diesel","HEV"), !is.na(Avg_CO2), Vehicle_Age <= 10) %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(CO2 = weighted.mean(Avg_CO2, Fleet, na.rm = TRUE),
            N   = sum(Fleet, na.rm = TRUE), .groups = "drop") %>%
  group_by(Fuel_Group) %>%
  arrange(Vehicle_Age, .by_group = TRUE)
print(real_new_car_anchor,n=Inf)
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
print(real_2024_anchor)
# Plausibility guard
real_2024_anchor <- real_2024_anchor %>%
  left_join(real_new_car_anchor %>% filter(Vehicle_Age == 3) %>% select(Fuel_Group, CO2_age3 = CO2),
            by = "Fuel_Group") %>%
  mutate(
    CO2_2024_raw = pmin(CO2_2024_raw, CO2_age3, na.rm = TRUE),
    CO2_2024 = ifelse(Fuel_Group == "HEV", CO2_2024_raw * HEV_REALWORLD_UPLIFT, CO2_2024_raw)
  ) %>%
  select(-CO2_age3)
print(real_2024_anchor)
# ----------------------------------------------------------------------------
# PART 4: BUILD HISTORICAL AND FORWARD EMISSION FACTORS
# ----------------------------------------------------------------------------

# 4.1 Historical CO2 from 2024 MOT data
historical_co2_by_buildyear <- results$fleet[["2024"]] %>%
  filter(Fuel_Group %in% c("Petrol", "Diesel", "HEV"), !is.na(Avg_CO2)) %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(Real_CO2 = weighted.mean(Avg_CO2, Fleet, na.rm = TRUE), .groups = "drop") %>%
  mutate(Build_Year = 2024 - Vehicle_Age) %>%
  mutate(Real_CO2 = ifelse(Fuel_Group == "HEV", Real_CO2 * HEV_REALWORLD_UPLIFT, Real_CO2)) %>%
  select(Fuel_Group, Build_Year, g_per_km = Real_CO2)

# 4.2 Forward 2027-2035 (from trajectory)
forward_2027_2035 <- fuel_trajectory_by_year %>%
  rename(Build_Year = Year, g_per_km = CO2)

# 4.3 Forward 2036-2037 (flat from 2035)
forward_2036_2037 <- fuel_trajectory_by_year %>%
  filter(Year == 2035) %>%
  select(Fuel_Group, g_per_km = CO2) %>%
  crossing(Build_Year = 2036:2037)

# 4.4 Bridge 2025-2026 (interpolate from 2024 to 2027 using real_2024_anchor)
bridge_2025_2026 <- real_2024_anchor %>%
  select(Fuel_Group, CO2_2024 = CO2_2024) %>%
  left_join(
    fuel_trajectory_by_year %>% 
      filter(Year == 2027) %>% 
      select(Fuel_Group, CO2_2027 = CO2),
    by = "Fuel_Group"
  ) %>%
  crossing(Build_Year = 2025:2026) %>%
  mutate(g_per_km = CO2_2024 + (CO2_2027 - CO2_2024) * (Build_Year - 2024) / 3) %>%
  select(Fuel_Group, Build_Year, g_per_km)

# 4.5 Combine all forward components
forward_co2_by_buildyear <- bind_rows(
  bridge_2025_2026,
  forward_2027_2035,
  forward_2036_2037
)

# 4.6 Combine historical + forward
base_gkm_by_buildyear <- bind_rows(historical_co2_by_buildyear, forward_co2_by_buildyear)

# 4.7 Confirm 2025-2026 are now covered
cat("\n=== CHECKING 2025-2026 COVERAGE ===\n")
print(base_gkm_by_buildyear %>% 
        filter(Build_Year %in% 2025:2026) %>% 
        arrange(Fuel_Group, Build_Year))

# 4.8 Apply size multipliers to get g/km by vehicle class
full_lookup <- base_gkm_by_buildyear %>%
  crossing(vehicle_class = unique(size_multiplier_from_data$vehicle_class)) %>%
  left_join(size_multiplier_from_data, by = c("Fuel_Group", "vehicle_class")) %>%
  mutate(
    size_multiplier = coalesce(size_multiplier_Co2, 1.0),
    g_per_km = g_per_km * size_multiplier
  ) %>%
  select(Fuel_Group, Build_Year, vehicle_class, g_per_km)

# 4.9 Average lookup for fallback
full_lookup_avg <- full_lookup %>%
  group_by(Fuel_Group, Build_Year) %>%
  summarise(g_per_km_avg = mean(g_per_km, na.rm = TRUE), .groups = "drop")

# 4.10 Set min/max years for lookup
min_by <- min(base_gkm_by_buildyear$Build_Year)
max_by <- max(base_gkm_by_buildyear$Build_Year)

cat("\n=== BUILD YEAR RANGE ===\n")
cat("Min:", min_by, "| Max:", max_by, "\n")



# ----------------------------------------------------------------------------
# PART 5: RECONCILIATION CHECK (must be added back!)
# ----------------------------------------------------------------------------

cat("\n=== RECONCILIATION CHECK ===\n")

fuel_trajectory_by_year %>%
  left_join(fuel_shares_complete, by = c("Fuel_Group", "Year")) %>%
  group_by(Year) %>%
  summarise(check = weighted.mean(CO2, sale_percentage), .groups = "drop") %>%
  left_join(greg_ice_trajectory, by = "Year") %>%
  mutate(diff = round(check - Target_blend, 4)) %>%
  print(n = Inf)

# ----------------------------------------------------------------------------
# PART 6: EMISSIONS CALCULATION FUNCTION
# ----------------------------------------------------------------------------

# Helper function to extract size class
extract_size_class <- function(vehicle_class_vec) {
  case_when(
    grepl("^Small", vehicle_class_vec) ~ "Small",
    grepl("^Medium", vehicle_class_vec) ~ "Medium",
    grepl("^Very Large", vehicle_class_vec) ~ "Very Large",
    grepl("^Large", vehicle_class_vec) ~ "Large",
    TRUE ~ NA_character_
  )
}

# Set min/max years for lookup
min_by <- min(base_gkm_by_buildyear$Build_Year)
max_by <- max(base_gkm_by_buildyear$Build_Year)

# Fast emissions calculation function
calculate_emissions_size_fuel_fast <- function(fleet_data, scenario_name) {
  fleet_data %>%
    filter(Fuel_Group %in% c("Petrol", "Diesel", "HEV", "PHEV")) %>%
    mutate(
      Build_Year = Year - Vehicle_Age,
      Build_Year_clamped = pmin(pmax(Build_Year, min_by), max_by),
      vehicle_class_clean = coalesce(extract_size_class(vehicle_class), "Medium")
    ) %>%
    left_join(full_lookup, 
              by = c("Fuel_Group", "Build_Year_clamped" = "Build_Year",
                     "vehicle_class_clean" = "vehicle_class")) %>%
    left_join(full_lookup_avg, 
              by = c("Fuel_Group", "Build_Year_clamped" = "Build_Year")) %>%
    left_join(annual_km_lookup, 
              by = c("Fuel_Group", "vehicle_class_clean" = "vehicle_class")) %>%
    mutate(
      g_per_km = coalesce(g_per_km, g_per_km_avg, ifelse(Build_Year < 2025, 150, 130)),
      annual_km = coalesce(annual_km, 11000),
      emissions_Mt = Fleet * g_per_km * annual_km / 1e12
    ) %>%
    group_by(Year) %>%
    summarise(
      Car_CO2_Mt = sum(emissions_Mt, na.rm = TRUE),
      Avg_gkm = weighted.mean(g_per_km, Fleet, na.rm = TRUE),
      Avg_Mileage_km = weighted.mean(annual_km, Fleet, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Scenario = scenario_name)
}

# Collapse function
collapse_for_emissions <- function(results_obj) {
  bind_rows(results_obj$fleet) %>%
    group_by(Year, Vehicle_Age, Fuel_Group, vehicle_class) %>%
    summarise(Fleet = sum(Fleet, na.rm = TRUE), .groups = "drop")
}

# ----------------------------------------------------------------------------
# PART 7: RUN EMISSIONS FOR ALL SCENARIOS
# ----------------------------------------------------------------------------

system.time({
  em_baseline_revised <- calculate_emissions_size_fuel_fast(
    collapse_for_emissions(results_baseline), "Baseline"
  )
  em_70_revised <- calculate_emissions_size_fuel_fast(
    collapse_for_emissions(results_70pct), "70% by 2030"
  )
  em_60_revised <- calculate_emissions_size_fuel_fast(
    collapse_for_emissions(results_60pct), "60% by 2030"
  )
  em_s4_revised <- calculate_emissions_size_fuel_fast(
    collapse_for_emissions(results_scenario4), "Scenario 4"
  )
}) %>% print()

all_car_emissions_revised <- bind_rows(
  em_baseline_revised,
  em_70_revised,
  em_60_revised,
  em_s4_revised
)

cat("\n=== REVISED EMISSIONS — 2035 ===\n")
print(all_car_emissions_revised %>% filter(Year == 2035))

# ----------------------------------------------------------------------------
# PART 8: COMPARE WITH CCC TARGET
# ----------------------------------------------------------------------------

ccc_car_target_2035 <- 19.3

all_car_emissions_revised %>%
  filter(Year == 2035) %>%
  mutate(
    CCC_Target = ccc_car_target_2035,
    Overshoot = round(Car_CO2_Mt - ccc_car_target_2035, 1),
    Pct_Overshoot = round(100 * Overshoot / ccc_car_target_2035, 1)
  ) %>%
  print()



# ============================================
# UPDATED FIGURE: Car Emissions vs CCC Total Surface Transport Target
# ============================================

library(ggplot2)

# CCC Total Surface Transport target for 2035 (from Balanced Net Zero Pathway)
ccc_surface_total_2035 <- 32.0  # MtCO₂

# CCC Car allocation (60.2% of surface target based on 2020 share)
ccc_car_allocation_2035 <- 19.3  # MtCO₂

# Your actual car emissions from model
car_emissions <- data.frame(
  Scenario = c("Baseline", "70% by 2030", "60% by 2030", "Scenario 4"),
  Emissions = c(all_car_emissions_revised$Car_CO2_Mt[all_car_emissions_revised$Year == 2035 & 
                                                       all_car_emissions_revised$Scenario == "Baseline"],
                all_car_emissions_revised$Car_CO2_Mt[all_car_emissions_revised$Year == 2035 & 
                                                       all_car_emissions_revised$Scenario == "70% by 2030"],
                all_car_emissions_revised$Car_CO2_Mt[all_car_emissions_revised$Year == 2035 & 
                                                       all_car_emissions_revised$Scenario == "60% by 2030"],
                all_car_emissions_revised$Car_CO2_Mt[all_car_emissions_revised$Year == 2035 & 
                                                       all_car_emissions_revised$Scenario == "Scenario 4"]
  ))

# Ensure the Scenario column is a factor with the correct order
car_emissions$Scenario <- factor(car_emissions$Scenario, 
                                 levels = c("Baseline", "70% by 2030", "60% by 2030", "Scenario 4"))


# Rename the scenarios in your data to match what you want displayed
car_emissions$Scenario <- factor(car_emissions$Scenario, 
                                 levels = c("Baseline", "70% by 2030", "60% by 2030", "Scenario 4"),
                                 labels = c("Baseline", "Mild ZEV Rowback", "Medium ZEV Rowback", "Slow sales"))

# Now create the plot with your original color scheme
p_ccc_comparison <- ggplot(car_emissions, aes(x = Scenario, y = Emissions, fill = Scenario)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = ccc_car_allocation_2035, linetype = "dashed", color = "black", size = 1) +
  geom_text(aes(label = round(Emissions, 1)), vjust = -0.5, size = 5, fontface = "bold") +
  annotate("text", x = 4.5, y = ccc_car_allocation_2035 + 1,
           label = paste("CCC Car Allocation (60.2%):", ccc_car_allocation_2035, "Mt"),
           size = 3, color = "black", hjust = 1, fontface = "bold") +
  scale_fill_manual(values = c("Baseline" = "#2E86AB",
                               "Mild ZEV Rowback" = "#F18F01",
                               "Medium ZEV Rowback" = "#C73E1D",
                               "Slow sales" = "#4DAF4A")) +
  labs(
    x = "",
    y = "CO2 Emissions (MtCO2)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

print(p_ccc_comparison)
##ggsave("Figure_Car_Emissions_vs_CCC.png", p_ccc_comparison, width = 8, height = 6, dpi = 300, bg = "white")






# ============================================
# CORRECTED CAR SHARING SCENARIO 
# USING ONLY ACTUAL VEHICLE NUMBERS FROM YOUR MODEL
# BEV EMISSIONS BASED ON UK GRID INTENSITY
# ============================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)

# -------------------------------------------------------------------
# STEP 1: Get Actual Vehicle Numbers from Your Model ONLY
# -------------------------------------------------------------------

# Baseline non-zero fleet (Petrol, Diesel, HEV, PHEV) from your model
non_zero_fleet <- bind_rows(results_baseline$fleet) %>%
  filter(Year == 2035, Fuel_Group %in% c("Petrol", "Diesel", "HEV", "PHEV")) %>%
  summarise(total = sum(Fleet, na.rm = TRUE)) %>%
  pull(total)


print(non_zero_fleet)
# Baseline emissions from your detailed model
baseline_emissions <- all_car_emissions_revised$Car_CO2_Mt[all_car_emissions_revised$Year == 2035 & 
                                                             all_car_emissions_revised$Scenario == "Baseline"]  # Mt CO₂

cat("\n=== ACTUAL VEHICLE NUMBERS FROM YOUR MODEL ===\n")
cat("Baseline non-zero fleet:", format(non_zero_fleet, big.mark = ","), "vehicles\n")
cat("Baseline emissions:", baseline_emissions, "Mt CO₂\n")

# -------------------------------------------------------------------
# STEP 2: UK Grid Intensity for BEV Emissions
# -------------------------------------------------------------------

# UK Grid Carbon Intensity (January 2026 data)
uk_grid_intensity <- 129  # gCO₂/kWh

# BEV efficiency (average consumption)
bev_efficiency_kwh_per_km <- 0.18  # 18 kWh/100km = 0.18 kWh/km
annual_km <- 11000  # km per year

# Calculate BEV emissions per vehicle per year
bev_annual_kwh <- bev_efficiency_kwh_per_km * annual_km  # 1,980 kWh
bev_annual_g <- bev_annual_kwh * uk_grid_intensity  # 255,420 g
bev_annual_kg <- bev_annual_g / 1000  # 255.4 kg
bev_tonnes_per_vehicle <- bev_annual_kg / 1000  # 0.255 tonnes/year

# Calculate petrol emissions per vehicle (for comparison)
petrol_tonnes_per_vehicle <- baseline_emissions * 1e6 / non_zero_fleet  # 1.62 tonnes/year

cat("\n=== UK GRID INTENSITY CALCULATION ===\n")
cat("Grid intensity:", uk_grid_intensity, "gCO₂/kWh\n")
cat("BEV efficiency:", bev_efficiency_kwh_per_km * 100, "kWh/100km\n")
cat("Annual BEV electricity consumption:", bev_annual_kwh, "kWh\n")
cat("Annual BEV CO₂ per vehicle:", round(bev_annual_kg, 0), "kg (", round(bev_tonnes_per_vehicle, 2), "tonnes)\n")
cat("Annual Petrol CO₂ per vehicle:", round(petrol_tonnes_per_vehicle, 2), "tonnes\n")
cat("BEV reduction vs Petrol:", round((1 - bev_tonnes_per_vehicle/petrol_tonnes_per_vehicle)*100, 1), "%\n")

# -------------------------------------------------------------------
# STEP 3: Define Sharing Scenarios (Direct Vehicle Reduction)
# -------------------------------------------------------------------

# Sharing reduces number of vehicles needed (same total mileage)
# Assumptions:
# - Hybrid (30% sharing): 10% fewer vehicles
# - 100% sharing: 35% fewer vehicles

sharing_scenarios <- data.frame(
  Scenario = c(
    "Baseline (Private)",
    "Hybrid (30% Shared)",
    "100% Shared (Petrol)",
    "100% Shared (BEV - Grid)"
  ),
  Vehicle_Reduction_Pct = c(0, 0.10, 0.35, 0.35)
)

# Calculate fleet size and emissions based ONLY on vehicle numbers
sharing_scenarios <- sharing_scenarios %>%
  mutate(
    # Fleet size from baseline vehicles
    Fleet_Size = non_zero_fleet * (1 - Vehicle_Reduction_Pct),
    Fleet_Size_M = round(Fleet_Size / 1e6, 1),
    # Emissions for petrol/hybrid: scale directly with fleet size
    Emissions_Mt = baseline_emissions * (1 - Vehicle_Reduction_Pct),
    # For BEV: use grid intensity emissions
    Emissions_Mt = ifelse(Scenario == "100% Shared (BEV - Grid)", 
                          (Fleet_Size * bev_tonnes_per_vehicle) / 1e6, 
                          Emissions_Mt),
    # Reduction percentage
    Reduction_vs_Baseline = round((1 - Emissions_Mt / baseline_emissions) * 100, 1),
    # Check if meets CCC car target (19.3 Mt)
    Meets_Car_Target = ifelse(Emissions_Mt <= 19.3, "✅ Yes", "❌ No")
  )

# -------------------------------------------------------------------
# STEP 4: Display Results
# -------------------------------------------------------------------

cat("\n", strrep("=", 80), "\n")
cat("CAR SHARING RESULTS (Using ONLY Vehicle Numbers + BEV Grid)")
cat("\n", strrep("=", 80), "\n")

results_table <- sharing_scenarios %>%
  select(Scenario, Fleet_Size_M, Emissions_Mt, Reduction_vs_Baseline, Meets_Car_Target)

print(results_table)

# -------------------------------------------------------------------
# STEP 5: Create Figure
# -------------------------------------------------------------------

p_emissions <- ggplot(sharing_scenarios, aes(x = Scenario, y = Emissions_Mt, fill = Scenario)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_hline(yintercept = 19.3, linetype = "dotted", color = "orange", size = 1.2) +
  geom_hline(yintercept = 32.0, linetype = "dashed", color = "red", size = 1.2) +
  geom_text(aes(label = paste0(round(Emissions_Mt, 1), " Mt")), 
            vjust = -0.5, size = 4.5, fontface = "bold") +
  geom_text(aes(label = paste0("-", Vehicle_Reduction_Pct * 100, "% vehicles")), 
            vjust = 1.2, size = 3.5, color = "white") +
  annotate("text", x = 4.2, y = 20.5, label = "CCC Car Target (19.3 Mt)", 
           size = 3.5, color = "orange", fontface = "bold", hjust = 1) +
  annotate("text", x = 4.2, y = 33.7, label = "CCC Surface Target (32 Mt)", 
           size = 3.5, color = "red", fontface = "bold", hjust = 1) +
  scale_fill_manual(values = c("Baseline (Private)" = "#2E86AB",
                               "Hybrid (30% Shared)" = "#F18F01",
                               "100% Shared (Petrol)" = "#C73E1D",
                               "100% Shared (BEV - Grid)" = "#4DAF4A")) +
  scale_y_continuous(limits = c(0, 38), breaks = seq(0, 35, 5)) +
  labs(title = "Car Sharing Impact on 2035 CO₂ Emissions",
       subtitle = paste0("BEV emissions based on UK grid intensity (", uk_grid_intensity, " gCO₂/kWh)"),
       x = "", y = "CO₂ Emissions (MtCO₂ per year)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 0.8))

print(p_emissions)
#ggsave("Figure_Car_Sharing_BEV_Grid.png", p_emissions, width = 8, height = 6, dpi = 300, bg = "white")

# -------------------------------------------------------------------
# STEP 6: Summary Table
# -------------------------------------------------------------------

cat("\n", strrep("=", 80), "\n")
cat("FINAL SUMMARY")
cat("\n", strrep("=", 80), "\n")
cat("\n")
cat("| Scenario                    | Vehicles (M) | CO₂ Emissions (Mt) | Meets CCC Target? |\n")
cat("|-----------------------------|--------------|---------------------|-------------------|\n")
for(i in 1:nrow(sharing_scenarios)) {
  cat("| ", sprintf("%-27s", sharing_scenarios$Scenario[i]), 
      "| ", sprintf("%10.1f", sharing_scenarios$Fleet_Size_M[i]), 
      "| ", sprintf("%17.1f", sharing_scenarios$Emissions_Mt[i]), 
      "| ", sharing_scenarios$Meets_Car_Target[i], " |\n", sep="")
}





# Sensitivity Analysis Ranges
# Parameter	Low	Medium	High	Justification
# People per shared vehicle	2 people (50% reduction)	3 people (67% reduction)	6 people (83% reduction)	Car clubs: 3-5 households per car
# BEV share in shared fleet	0% (all petrol)	50% (mixed)	100% (all BEV)	Policy-driven transition
# Mileage behavior	Same mileage (no change)	Reduced mileage (20% less)	Increased mileage (20% more)	Induced demand effect



# ============================================
# CAR SHARING SENSITIVITY ANALYSIS
# ============================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)

# -------------------------------------------------------------------
# STEP 1: Base Data from Your Model
# -------------------------------------------------------------------

# Baseline non-zero fleet (Petrol, Diesel, HEV, PHEV) from your model
non_zero_fleet <- non_zero_fleet  # 19,999,033 vehicles (20.0M)

# Baseline emissions from your detailed model
baseline_emissions <- all_car_emissions_revised$Car_CO2_Mt[all_car_emissions_revised$Year == 2035 & 
                                                             all_car_emissions_revised$Scenario == "Baseline"]  # Mt CO₂

# UK Grid Intensity for BEV
uk_grid_intensity <- 129  # gCO₂/kWh
bev_efficiency_kwh_per_km <- 0.18  # 18 kWh/100km
# Calculate weighted average annual km from your lookup
annual_km <- annual_km_lookup %>%
  left_join(
    paper_emissions_table %>%
      filter(Fuel_Group != "BEV") %>%
      select(Fuel_Group, vehicle_class = vehicle_class_clean, N_Vehicles),
    by = c("Fuel_Group", "vehicle_class")
  ) %>%
  summarise(weighted_avg = weighted.mean(annual_km, N_Vehicles, na.rm = TRUE)) %>%
  pull(weighted_avg)

# If the above fails, use a fallback
if(length(annual_km) == 0 || is.na(annual_km)) annual_km <- 11000


# BEV emissions per vehicle per year (tonnes)
bev_annual_kwh <- bev_efficiency_kwh_per_km * annual_km
bev_annual_kg <- bev_annual_kwh * uk_grid_intensity / 1000
bev_tonnes_per_vehicle <- bev_annual_kg / 1000  # 0.255 tonnes/year

# Petrol emissions per vehicle per year (tonnes)
petrol_tonnes_per_vehicle <- baseline_emissions * 1e6 / non_zero_fleet  # 1.62 tonnes/year

cat("\n=== BASE ASSUMPTIONS ===\n")
cat("Petrol emissions per vehicle:", round(petrol_tonnes_per_vehicle, 2), "tonnes/year\n")
cat("BEV emissions per vehicle (grid):", round(bev_tonnes_per_vehicle, 2), "tonnes/year\n")
cat("BEV reduction vs petrol:", round((1 - bev_tonnes_per_vehicle/petrol_tonnes_per_vehicle)*100, 1), "%\n")

# -------------------------------------------------------------------
# STEP 2: Define Sensitivity Scenarios
# -------------------------------------------------------------------

# Scenario 1: Different Sharing Intensities (People per vehicle)
sharing_intensity <- data.frame(
  Scenario = c("Private (Baseline)", "Light Sharing", "Moderate Sharing", "Heavy Sharing"),
  People_per_Vehicle = c(1.5, 1.9, 2.2, 2.5),
  Vehicle_Reduction_Pct = c(0, 1 - (1.5/1.9), 1 - (1.5/2.2), 1 - (1.5/2.5))
)

# Calculate emissions for different sharing intensities (petrol only)
sharing_intensity <- sharing_intensity %>%
  mutate(
    Fleet_Size = non_zero_fleet * (1 - Vehicle_Reduction_Pct),
    Fleet_Size_M = round(Fleet_Size / 1e6, 1),
    Emissions_Mt = baseline_emissions * (1 - Vehicle_Reduction_Pct),
    Meets_Car_Target = ifelse(Emissions_Mt <= 19.3, "✅ Yes", "❌ No")
  )%>%print()

# -------------------------------------------------------------------
# STEP 3: Scenario 2 - BEV Mix in Shared Fleet (0%, 50%, 100% BEV)
# -------------------------------------------------------------------

bev_mix <- data.frame(
  BEV_Share = c(0, 0.25, 0.50, 0.75, 1.00),
  Description = c("0% BEV (all petrol)", "25% BEV, 75% petrol", 
                  "50% BEV, 50% petrol", "75% BEV, 25% petrol", "100% BEV")
)

# Assume 21% vehicle reduction (100% sharing)
vehicle_reduction <- 0.21
shared_fleet <- non_zero_fleet * (1 - vehicle_reduction)  # 15.8M vehicles

bev_mix <- bev_mix %>%
  mutate(
    BEV_Fleet = shared_fleet * BEV_Share,
    Petrol_Fleet = shared_fleet * (1 - BEV_Share),
    # Emissions = BEV_fleet * BEV_rate + Petrol_fleet * Petrol_rate
    Emissions_Mt = (BEV_Fleet * bev_tonnes_per_vehicle + Petrol_Fleet * petrol_tonnes_per_vehicle) / 1e6,
    Reduction_vs_Baseline = round((1 - Emissions_Mt / baseline_emissions) * 100, 1),
    Meets_Car_Target = ifelse(Emissions_Mt <= 19.3, "✅ Yes", "❌ No")
  )


# -------------------------------------------------------------------
# STEP 5: Scenario 4 - Mixed Sharing + BEV (2x2 matrix)
# --------------------------------------------------------------------

# Create combination of sharing intensity and BEV mix
sharing_levels <- c(0, 0.21)  # 0% and 21% reduction
bev_shares <- c(0, 0.5, 1.0)

mixed_scenarios <- expand.grid(
  Vehicle_Reduction_Pct = sharing_levels,
  BEV_Share = bev_shares
) %>%
  mutate(
    Scenario = case_when(
      Vehicle_Reduction_Pct == 0 & BEV_Share == 0 ~ "Private (Petrol)",
      Vehicle_Reduction_Pct == 0 & BEV_Share == 0.5 ~ "Private (50% BEV)",
      Vehicle_Reduction_Pct == 0 & BEV_Share == 1 ~ "Private (100% BEV)",
      Vehicle_Reduction_Pct == 0.21 & BEV_Share == 0 ~ "Shared (Petrol)",
      Vehicle_Reduction_Pct == 0.21 & BEV_Share == 0.5 ~ "Shared (50% BEV)",
      Vehicle_Reduction_Pct == 0.21 & BEV_Share == 1 ~ "Shared (100% BEV)",
      TRUE ~ "Other"
    ),
    Fleet_Size = non_zero_fleet * (1 - Vehicle_Reduction_Pct),
    Fleet_Size_M = round(Fleet_Size / 1e6, 1),
    BEV_Fleet = Fleet_Size * BEV_Share,
    Petrol_Fleet = Fleet_Size * (1 - BEV_Share),
    Emissions_Mt = (BEV_Fleet * bev_tonnes_per_vehicle + Petrol_Fleet * petrol_tonnes_per_vehicle) / 1e6,
    Reduction_vs_Baseline = round((1 - Emissions_Mt / baseline_emissions) * 100, 1),
    Meets_Car_Target = ifelse(Emissions_Mt <= 19.3, "✅ Yes", "❌ No")
  )

# -------------------------------------------------------------------
# STEP 6: Visualisation - All Sensitivity Results
# -------------------------------------------------------------------

# Plot 1: Sharing Intensity Impact
p1 <- ggplot(sharing_intensity, aes(x = factor(People_per_Vehicle), y = Emissions_Mt, 
                                    fill = factor(People_per_Vehicle))) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_hline(yintercept = 19.3, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 1.5, y = 19.3 + 1,  # Above the line
           label = paste("CCC Target:", 19.3, "Mt"), 
           size = 3, color = "red", hjust = 1, fontface = "bold") +
  geom_text(aes(label = paste0(round(Emissions_Mt, 1), " Mt")), vjust = -0.5, size = 4) +
  geom_text(aes(label = paste0(round(Vehicle_Reduction_Pct*100, 0), "% fewer cars")), 
            vjust = 2.5, size = 3, color = "black") +
  scale_fill_brewer(palette = "OrRd", name = "People per vehicle") +
  labs(
    #title = "A) Impact of Sharing Intensity on CO₂ Emissions",
    #subtitle = "Assuming petrol vehicles only",
    x = "People per shared vehicle", y = "CO2 Emissions (Mt)") +
  theme_minimal() +
  theme(legend.position = "none")

print(p1)

# -------------------------------------------------------------------
# STEP 5: Scenario 3 - Mileage Behaviour Change
# -------------------------------------------------------------------

# Shared vehicles may be used more intensively (induced demand)
mileage_scenarios <- data.frame(
  Scenario = c("Reduced mileage (-50%)", "Reduced mileage (-40%)", "Reduced mileage (-30%)", 
               "Reduced mileage (-20%)", "Reduced mileage (-10%)", "Same mileage (0%)",
               "Increased mileage (+10%)", "Increased mileage (+20%)", "Increased mileage (+30%)",
               "Increased mileage (+40%)", "Increased mileage (+50%)"),
  Mileage_Factor = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5),
  Description = c("50% less travel", "40% less travel", "30% less travel", 
                  "20% less travel", "10% less travel", "Same total distance travelled",
                  "10% more travel", "20% more travel (induced demand)", 
                  "30% more travel", "40% more travel", "50% more travel (high induced demand)")
)


#assume 0% reduction , petrol only 


mileage_scenarios_1 <- mileage_scenarios %>%
  mutate(
    Fleet_Size = non_zero_fleet ,
    Fleet_Size_M = round(Fleet_Size / 1e6, 1),
    # Emissions scale with mileage factor
    Emissions_Mt = baseline_emissions  * Mileage_Factor,
    Reduction_vs_Baseline = round((1 - Emissions_Mt / baseline_emissions) * 100, 1),
    Meets_Car_Target = ifelse(Emissions_Mt <= 19.3, "✅ Yes", "❌ No")
  )



# Assume 21% vehicle reduction, petrol only
mileage_scenarios <- mileage_scenarios %>%
  mutate(
    Fleet_Size = non_zero_fleet * (1 - vehicle_reduction),
    Fleet_Size_M = round(Fleet_Size / 1e6, 1),
    # Emissions scale with mileage factor
    Emissions_Mt = baseline_emissions * (1 - vehicle_reduction) * Mileage_Factor,
    Reduction_vs_Baseline = round((1 - Emissions_Mt / baseline_emissions) * 100, 1),
    Meets_Car_Target = ifelse(Emissions_Mt <= 19.3, "✅ Yes", "❌ No")
  )


# Plot 3: Mileage Behaviour Impact
p3 <- ggplot(mileage_scenarios, aes(x = Scenario, y = Emissions_Mt, fill = Scenario)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_hline(yintercept = 19.3, linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = paste0(round(Emissions_Mt, 1), " Mt")), vjust = -0.5, size = 3.5) +
  scale_fill_brewer(palette = "Oranges") +
  labs(title = "C) Mileage Behaviour Impact (21% reduction, petrol)",
       x = "", y = "CO₂ Emissions (Mt)") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 0.8))
print(p3)



# Prepare data for butterfly plot
mileage_scenarios_butterfly <- mileage_scenarios %>%
  mutate(
    # Create simpler labels
    Simple_Label = case_when(
      Mileage_Factor == 0.5 ~ "-50%",
      Mileage_Factor == 0.6 ~ "-40%",
      Mileage_Factor == 0.7 ~ "-30%",
      Mileage_Factor == 0.8 ~ "-20%",
      Mileage_Factor == 0.9 ~ "-10%",
      Mileage_Factor == 1.0 ~ "0%",
      Mileage_Factor == 1.1 ~ "+10%",
      Mileage_Factor == 1.2 ~ "+20%",
      Mileage_Factor == 1.3 ~ "+30%",
      Mileage_Factor == 1.4 ~ "+40%",
      Mileage_Factor == 1.5 ~ "+50%"
    ),
    # Create positive and negative values for butterfly plot
    Emissions_Above_Target = ifelse(Emissions_Mt > 19.3, Emissions_Mt - 19.3, 0),
    Emissions_Below_Target = ifelse(Emissions_Mt < 19.3, 19.3 - Emissions_Mt, 0),
    # Direction for plotting (negative values go left, positive go right)
    Emissions_Diff = Emissions_Mt - 19.3
  ) %>%
  # Order by mileage factor (ascending)
  arrange(Mileage_Factor)

# Butterfly plot (diverging from CCC target)


library(dplyr); library(ggplot2)

# Pivot point: emissions at 0% mileage change (Mileage_Factor == 1.0)
zero_change_emissions <- mileage_scenarios_butterfly$Emissions_Mt[
  mileage_scenarios_butterfly$Mileage_Factor == 1.0
]

mileage_scenarios_butterfly <- mileage_scenarios_butterfly %>%
  arrange(Mileage_Factor) %>%
  mutate(
    # Build the label from Mileage_Factor instead of relying on Simple_Label
    Pct          = round((Mileage_Factor - 1) * 100),
    Simple_Label = ifelse(Pct >= 0, paste0("+", Pct, "%"), paste0(Pct, "%")),
    Emissions_Diff = Emissions_Mt - zero_change_emissions,
    Direction      = ifelse(Emissions_Diff >= 0, "More emissions", "Fewer emissions")
  )

# Lock x-axis order (ascending mileage) — set levels OUTSIDE the column self-reference
mileage_scenarios_butterfly$Simple_Label <- factor(
  mileage_scenarios_butterfly$Simple_Label,
  levels = mileage_scenarios_butterfly$Simple_Label
)

target_offset <- 19.3 - zero_change_emissions

p31 <- ggplot(mileage_scenarios_butterfly,
              aes(x = Simple_Label, y = Emissions_Diff, fill = Direction)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_hline(yintercept = target_offset, linetype = "dashed",
             color = "red", size = 1) +
  annotate("text", x = 8, y = target_offset, label = "CCC Target: 19.3 Mt",
           vjust = -0.6, hjust = 0, size = 3, color = "red", fontface = "bold") +
  geom_text(aes(label = paste0(round(Emissions_Mt, 1), " Mt")),
            vjust = ifelse(mileage_scenarios_butterfly$Emissions_Diff >= 0, -0.4, 1.3),
            size = 3.3) +
  #scale_fill_manual(values = c("More emissions" = "coral",
  #             "Fewer emissions" = "steelblue")) +
  labs(
    title = "B) Light Sharing (1.9 people:1 vehicle, 21% reduction)",
    x = NULL, # "Change in vehicle mileage (vs same-mileage scenario)",
    y = "Change in CO2 emissions vs 0% (Mt)") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

print(p31)

#-------
#plot with no sharing


# Prepare data for butterfly plot
mileage_scenarios_butterfly_1 <- mileage_scenarios_1 %>%
  mutate(
    # Create simpler labels
    Simple_Label = case_when(
      Mileage_Factor == 0.5 ~ "-50%",
      Mileage_Factor == 0.6 ~ "-40%",
      Mileage_Factor == 0.7 ~ "-30%",
      Mileage_Factor == 0.8 ~ "-20%",
      Mileage_Factor == 0.9 ~ "-10%",
      Mileage_Factor == 1.0 ~ "0%",
      Mileage_Factor == 1.1 ~ "+10%",
      Mileage_Factor == 1.2 ~ "+20%",
      Mileage_Factor == 1.3 ~ "+30%",
      Mileage_Factor == 1.4 ~ "+40%",
      Mileage_Factor == 1.5 ~ "+50%"
    ),
    # Create positive and negative values for butterfly plot
    Emissions_Above_Target = ifelse(Emissions_Mt > 19.3, Emissions_Mt - 19.3, 0),
    Emissions_Below_Target = ifelse(Emissions_Mt < 19.3, 19.3 - Emissions_Mt, 0),
    # Direction for plotting (negative values go left, positive go right)
    Emissions_Diff = Emissions_Mt - 19.3
  ) %>%
  # Order by mileage factor (ascending)
  arrange(Mileage_Factor)

# Butterfly plot (diverging from CCC target)


library(dplyr); library(ggplot2)

# Pivot point: emissions at 0% mileage change (Mileage_Factor == 1.0)
zero_change_emissions_1 <- mileage_scenarios_butterfly_1$Emissions_Mt[
  mileage_scenarios_butterfly_1$Mileage_Factor == 1.0
]

mileage_scenarios_butterfly_1 <- mileage_scenarios_butterfly_1 %>%
  arrange(Mileage_Factor) %>%
  mutate(
    # Build the label from Mileage_Factor instead of relying on Simple_Label
    Pct          = round((Mileage_Factor - 1) * 100),
    Simple_Label = ifelse(Pct >= 0, paste0("+", Pct, "%"), paste0(Pct, "%")),
    Emissions_Diff = Emissions_Mt - zero_change_emissions_1,
    Direction      = ifelse(Emissions_Diff >= 0, "More emissions", "Fewer emissions")
  )

# Lock x-axis order (ascending mileage) — set levels OUTSIDE the column self-reference
mileage_scenarios_butterfly_1$Simple_Label <- factor(
  mileage_scenarios_butterfly_1$Simple_Label,
  levels = mileage_scenarios_butterfly_1$Simple_Label
)

target_offset <- 19.3 - zero_change_emissions_1

p33 <- ggplot(mileage_scenarios_butterfly_1,
              aes(x = Simple_Label, y = Emissions_Diff, fill = Direction)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_hline(yintercept = target_offset, linetype = "dashed",
             color = "red", size = 1) +
  annotate("text", x = 8, y = target_offset, label = "CCC Target: 19.3 Mt",
           vjust = -0.6, hjust = 0, size = 3, color = "red", fontface = "bold") +
  geom_text(aes(label = paste0(round(Emissions_Mt, 1), " Mt")),
            vjust = ifelse(mileage_scenarios_butterfly_1$Emissions_Diff >= 0, -0.4, 1.3),
            size = 3.3) +
  # scale_fill_manual(values = c("More emissions" = "coral",
  #                          "Fewer emissions" = "steelblue")) +
  labs( title = "A) Without Sharing (1.5 people:1 vehicle)",
        x = NULL, # "Change in vehicle mileage (vs same-mileage scenario)",
        y = "Change in CO2 emissions vs 0% (Mt)") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

print(p33)
# -------------------------------------------------------------------
# Create plots for moderate and heavy sharing
# -------------------------------------------------------------------

# Moderate Sharing (32% reduction)
vehicle_reduction_moderate <- 1 - (1.5/2.2)  # 0.318 or 32%

mileage_scenarios_moderate <- mileage_scenarios %>%
  mutate(
    Fleet_Size = non_zero_fleet * (1 - vehicle_reduction_moderate),
    Fleet_Size_M = round(Fleet_Size / 1e6, 1),
    Emissions_Mt = baseline_emissions * (1 - vehicle_reduction_moderate) * Mileage_Factor,
    Reduction_vs_Baseline = round((1 - Emissions_Mt / baseline_emissions) * 100, 1),
    Meets_Car_Target = ifelse(Emissions_Mt <= 19.3, "✅ Yes", "❌ No")
  )

# Prepare butterfly data for moderate
mileage_scenarios_butterfly_moderate <- mileage_scenarios_moderate %>%
  mutate(
    Simple_Label = case_when(
      Mileage_Factor == 0.5 ~ "-50%",
      Mileage_Factor == 0.6 ~ "-40%",
      Mileage_Factor == 0.7 ~ "-30%",
      Mileage_Factor == 0.8 ~ "-20%",
      Mileage_Factor == 0.9 ~ "-10%",
      Mileage_Factor == 1.0 ~ "0%",
      Mileage_Factor == 1.1 ~ "+10%",
      Mileage_Factor == 1.2 ~ "+20%",
      Mileage_Factor == 1.3 ~ "+30%",
      Mileage_Factor == 1.4 ~ "+40%",
      Mileage_Factor == 1.5 ~ "+50%"
    ),
    Emissions_Diff = Emissions_Mt - 19.3
  ) %>%
  arrange(Mileage_Factor)

zero_change_moderate <- mileage_scenarios_butterfly_moderate$Emissions_Mt[
  mileage_scenarios_butterfly_moderate$Mileage_Factor == 1.0
]

mileage_scenarios_butterfly_moderate <- mileage_scenarios_butterfly_moderate %>%
  arrange(Mileage_Factor) %>%
  mutate(
    Pct = round((Mileage_Factor - 1) * 100),
    Simple_Label = ifelse(Pct >= 0, paste0("+", Pct, "%"), paste0(Pct, "%")),
    Emissions_Diff = Emissions_Mt - zero_change_moderate,
    Direction = ifelse(Emissions_Diff >= 0, "More emissions", "Fewer emissions")
  )

mileage_scenarios_butterfly_moderate$Simple_Label <- factor(
  mileage_scenarios_butterfly_moderate$Simple_Label,
  levels = mileage_scenarios_butterfly_moderate$Simple_Label
)

target_offset_moderate <- 19.3 - zero_change_moderate

p_moderate <- ggplot(mileage_scenarios_butterfly_moderate,
                     aes(x = Simple_Label, y = Emissions_Diff, fill = Direction)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_hline(yintercept = target_offset_moderate, linetype = "dashed",
             color = "red", size = 1) +
  annotate("text", x = 8, y = target_offset_moderate, label = "CCC Target: 19.3 Mt",
           vjust = -0.6, hjust = 0, size = 3, color = "red", fontface = "bold") +
  geom_text(aes(label = paste0(round(Emissions_Mt, 1), " Mt")),
            vjust = ifelse(mileage_scenarios_butterfly_moderate$Emissions_Diff >= 0, -0.4, 1.3),
            size = 3.3) +
  scale_fill_manual(values = c("More emissions" = "coral",
                               "Fewer emissions" = "steelblue")) +
  labs(
    title = "C) Moderate Sharing (2.2 people:1 vehicle, 32% reduction)",
    x = "Change in vehicle mileage",
    y = "Change in CO2 emissions vs 0% (Mt)"
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

print(p_moderate)

# -------------------------------------------------------------------
# Heavy Sharing (40% reduction)
# -------------------------------------------------------------------

vehicle_reduction_heavy <- 1 - (1.5/2.5)  # 0.40 or 40%

mileage_scenarios_heavy <- mileage_scenarios %>%
  mutate(
    Fleet_Size = non_zero_fleet * (1 - vehicle_reduction_heavy),
    Fleet_Size_M = round(Fleet_Size / 1e6, 1),
    Emissions_Mt = baseline_emissions * (1 - vehicle_reduction_heavy) * Mileage_Factor,
    Reduction_vs_Baseline = round((1 - Emissions_Mt / baseline_emissions) * 100, 1),
    Meets_Car_Target = ifelse(Emissions_Mt <= 19.3, "✅ Yes", "❌ No")
  )

mileage_scenarios_butterfly_heavy <- mileage_scenarios_heavy %>%
  mutate(
    Simple_Label = case_when(
      Mileage_Factor == 0.5 ~ "-50%",
      Mileage_Factor == 0.6 ~ "-40%",
      Mileage_Factor == 0.7 ~ "-30%",
      Mileage_Factor == 0.8 ~ "-20%",
      Mileage_Factor == 0.9 ~ "-10%",
      Mileage_Factor == 1.0 ~ "0%",
      Mileage_Factor == 1.1 ~ "+10%",
      Mileage_Factor == 1.2 ~ "+20%",
      Mileage_Factor == 1.3 ~ "+30%",
      Mileage_Factor == 1.4 ~ "+40%",
      Mileage_Factor == 1.5 ~ "+50%"
    ),
    Emissions_Diff = Emissions_Mt - 19.3
  ) %>%
  arrange(Mileage_Factor)

zero_change_heavy <- mileage_scenarios_butterfly_heavy$Emissions_Mt[
  mileage_scenarios_butterfly_heavy$Mileage_Factor == 1.0
]

mileage_scenarios_butterfly_heavy <- mileage_scenarios_butterfly_heavy %>%
  arrange(Mileage_Factor) %>%
  mutate(
    Pct = round((Mileage_Factor - 1) * 100),
    Simple_Label = ifelse(Pct >= 0, paste0("+", Pct, "%"), paste0(Pct, "%")),
    Emissions_Diff = Emissions_Mt - zero_change_heavy,
    Direction = ifelse(Emissions_Diff >= 0, "More emissions", "Fewer emissions")
  )

mileage_scenarios_butterfly_heavy$Simple_Label <- factor(
  mileage_scenarios_butterfly_heavy$Simple_Label,
  levels = mileage_scenarios_butterfly_heavy$Simple_Label
)

target_offset_heavy <- 19.3 - zero_change_heavy

p_heavy <- ggplot(mileage_scenarios_butterfly_heavy,
                  aes(x = Simple_Label, y = Emissions_Diff, fill = Direction)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_hline(yintercept = target_offset_heavy, linetype = "dashed",
             color = "red", size = 1) +
  annotate("text", x = 8, y = target_offset_heavy-2, label = "CCC Target: 19.3 Mt",
           vjust = -0.6, hjust = 0, size = 3, color = "red", fontface = "bold") +
  geom_text(aes(label = paste0(round(Emissions_Mt, 1), " Mt")),
            vjust = ifelse(mileage_scenarios_butterfly_heavy$Emissions_Diff >= 0, -0.4, 1.3),
            size = 3.3) +
  scale_fill_manual(values = c("More emissions" = "coral",
                               "Fewer emissions" = "steelblue")) +
  labs(
    title = "D) Heavy Sharing (2.5 people:1 vehicle, 40% reduction)",
    x = "Change in vehicle mileage",
    y = "Change in CO2 emissions vs 0% (Mt)"
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

print(p_heavy)

# -------------------------------------------------------------------
# Combine ALL plots (2 columns x 2 rows OR 1 column x 4 rows)
# -------------------------------------------------------------------

library(patchwork)

# Option 1: 2x2 grid
combined_all_2x2 <- (p33 | p31) / (p_moderate | p_heavy) +
  plot_annotation(
    # title = "Mileage Change Impact on CO₂ Emissions Across Sharing Intensities",
    #caption = "Red dashed line = CCC Car Target (19.3 Mt)"
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

print(combined_all_2x2)








# -------------------------------------------------------------------
# STEP 7: Summary Tables for Paper
# -------------------------------------------------------------------

cat("\n\n", strrep("=", 90), "\n")
cat("SENSITIVITY ANALYSIS SUMMARY")
cat("\n", strrep("=", 90), "\n")

cat("\n1. SHARING INTENSITY (Petrol only):\n")
print(sharing_intensity %>% select(Scenario, People_per_Vehicle, Fleet_Size_M, Emissions_Mt, Meets_Car_Target))

cat("\n2. BEV MIX IN SHARED FLEET (35% reduction):\n")
print(bev_mix %>% select(BEV_Share, Emissions_Mt, Reduction_vs_Baseline, Meets_Car_Target))

cat("\n3. MILEAGE BEHAVIOUR (35% reduction, petrol):\n")
print(mileage_scenarios %>% select(Scenario, Mileage_Factor, Emissions_Mt, Meets_Car_Target))

cat("\n4. MIXED SCENARIOS (Sharing + BEV):\n")
print(mixed_scenarios %>% select(Scenario, Fleet_Size_M, Emissions_Mt, Reduction_vs_Baseline, Meets_Car_Target))

# -------------------------------------------------------------------
# STEP 8: Key Findings for Paper
# -------------------------------------------------------------------

cat("\n\n", strrep("=", 90), "\n")
cat("KEY FINDINGS FROM SENSITIVITY ANALYSIS")
cat("\n", strrep("=", 90), "\n")

cat("\n• Sharing Intensity:\n")
cat("  - Light sharing (2:1): 29.3 Mt → ❌ Does NOT meet CCC target\n")
cat("  - Moderate sharing (3:1): 21.1 Mt → ❌ Does NOT meet CCC target\n")
cat("  - Heavy sharing (6:1): 10.8 Mt → ✅ Meets CCC target\n")

cat("\n• BEV Mix (35% reduction):\n")
cat("  - 0% BEV (all petrol): 21.1 Mt → ❌ Does NOT meet target\n")
cat("  - 50% BEV: 11.2 Mt → ✅ Meets target\n")
cat("  - 100% BEV: 1.7 Mt → ✅ Meets target\n")

cat("\n• Mileage Behaviour (35% reduction, petrol):\n")
cat("  - Reduced mileage (-20%): 16.9 Mt → ✅ Meets target\n")
cat("  - Same mileage: 21.1 Mt → ❌ Does NOT meet target\n")
cat("  - Increased mileage (+20%): 25.3 Mt → ❌ Does NOT meet target\n")

cat("\n• Combined Impact (Sharing + BEV):\n")
cat("  - Private (petrol): 33.7 Mt → ❌ Does NOT meet target\n")
cat("  - Shared (petrol): 21.1 Mt → ❌ Does NOT meet target\n")
cat("  - Shared (50% BEV): 11.2 Mt → ✅ Meets target\n")
cat("  - Shared (100% BEV): 1.7 Mt → ✅ Meets target\n")






# ============================================
# BUTTERFLY PLOT: Private vs Shared Fleet Emissions
# 0%, 25%, 50%, 75%, 100% BEV Share (Ordered Correctly)
# Only CCC Car Target (19.3 Mt)
# ============================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# -------------------------------------------------------------------
# STEP 1: Create expanded mixed scenarios with 0%, 25%, 50%, 75%, 100% BEV
# -------------------------------------------------------------------

sharing_levels <- c(0, 1 - (1.5/1.9))  # 0% reduction (Private) and 21% reduction (Shared)
bev_shares <- c(0, 0.25, 0.50, 0.75, 1.00)  # 0%, 25%, 50%, 75%, 100% BEV

mixed_scenarios_expanded <- expand.grid(
  Vehicle_Reduction_Pct = sharing_levels,
  BEV_Share = bev_shares
) %>%
  mutate(
    Fleet_Type = ifelse(Vehicle_Reduction_Pct == 0, "Private", "Shared"),
    # Order BEV shares correctly
    BEV_Share_Label = factor(
      paste0(BEV_Share * 100, "% BEV"),
      levels = c("0% BEV", "25% BEV", "50% BEV", "75% BEV", "100% BEV")
    ),
    Fleet_Size = non_zero_fleet * (1 - Vehicle_Reduction_Pct),
    Fleet_Size_M = round(Fleet_Size / 1e6, 1),
    BEV_Fleet = Fleet_Size * BEV_Share,
    Petrol_Fleet = Fleet_Size * (1 - BEV_Share),
    Emissions_Mt = (BEV_Fleet * bev_tonnes_per_vehicle + Petrol_Fleet * petrol_tonnes_per_vehicle) / 1e6,
    Reduction_vs_Baseline = round((1 - Emissions_Mt / baseline_emissions) * 100, 1),
    Meets_Car_Target = ifelse(Emissions_Mt <= 19.3, "✅ Yes", "❌ No")
  )

# -------------------------------------------------------------------
# STEP 2 (UPDATED): No butterfly transformation
# -------------------------------------------------------------------

plot_data <- mixed_scenarios_expanded %>%
  mutate(
    Bar_Label = paste0(round(Emissions_Mt, 1), " Mt"),
    Fleet_Label = paste0(Fleet_Size_M, "M vehicles"),
    Fleet_Type = factor(Fleet_Type, levels = c("Private", "Shared"))
  )
print(plot_data)
# -------------------------------------------------------------------
# STEP 3 (UPDATED): Two-panel plot (side-by-side)
# -------------------------------------------------------------------
bev_colors <- c(
  "0% BEV" = "#D73027",      # Red
  "25% BEV" = "#FC8D59",     # Orange
  "50% BEV" = "#FEE08B",     # Yellow
  "75% BEV" = "#91BFDB",     # Light blue
  "100% BEV" = "#4575B4"     # Dark blue
)
p_split <- ggplot(plot_data, 
                  aes(x = BEV_Share_Label, y = Emissions_Mt, fill = BEV_Share_Label)) +
  
  geom_col(width = 0.6, alpha = 0.9) +
  
  # CCC Target in BOTH plots
  geom_hline(yintercept = 19.3, linetype = "dashed", 
             color = "red", size = 1) +
  # 👇 Text label ONLY for Shared facet (Fleet_Type == "Shared")
  geom_text(data = subset(plot_data, Fleet_Type == "Shared"),
            aes(x = 5.5, y = 20.8, label = "CCC Car Target (19.3 Mt)"),
            size = 3.5, color = "red", fontface = "bold", hjust = 1,
            inherit.aes = FALSE) +
  
  # Labels above bars
  geom_text(aes(label = Bar_Label, y = Emissions_Mt + 1),
            size = 3.5, fontface = "bold") +
  
  # Fleet size inside bars
  geom_text(aes(label = Fleet_Label, y = Emissions_Mt / 2),
            size = 3, color = "black", fontface = "bold") +
  
  # 👇 THE KEY: split into two plots
  facet_wrap(~Fleet_Type, nrow = 1) +
  
  scale_fill_manual(values = bev_colors, name = "BEV Share") +
  
  scale_y_continuous(
    limits = c(0, 35),
    breaks = seq(0, 35, 10)
  ) +
  
  labs(
    # title = "Private vs Shared Fleet CO₂ Emissions (2035)",
    # subtitle = "Private fleet (0% reduction) vs Shared fleet (35% reduction)",
    x = "BEV Share in Fleet",
    y = "CO2 Emissions (Mt per year)",
    #caption = "Red dashed line = CCC Car Target (19.3 Mt)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "non",
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_split)

# #ggsave("Figure_Split_Private_vs_Shared.png",
#        p_split, width = 12, height = 6, dpi = 300, bg = "white")

# -------------------------------------------------------------------
# STEP 4: Summary Table
# -------------------------------------------------------------------

cat("\n\n", strrep("=", 90), "\n")
cat("MIXED SCENARIOS SUMMARY (Ordered by BEV Share)")
cat("\n", strrep("=", 90), "\n")

summary_expanded <- mixed_scenarios_expanded %>%
  arrange(Fleet_Type, BEV_Share) %>%
  mutate(
    BEV_Share_Pct = paste0(BEV_Share * 100, "%"),
    Emissions_Mt = round(Emissions_Mt, 1),
    BEV_Fleet_M = round(BEV_Fleet / 1e6, 1),
    Petrol_Fleet_M = round(Petrol_Fleet / 1e6, 1)
  ) %>%
  select(Fleet_Type, BEV_Share_Pct, Fleet_Size_M, BEV_Fleet_M, Petrol_Fleet_M, 
         Emissions_Mt, Meets_Car_Target)

print(summary_expanded)

# -------------------------------------------------------------------
# STEP 5: Key Finding
# -------------------------------------------------------------------

cat("\n\n", strrep("=", 90), "\n")
cat("KEY INSIGHTS")
cat("\n", strrep("=", 90), "\n")

cat("\n📊 Private Fleet (20.0M vehicles, 0% reduction):\n")
cat("   • 0% BEV: 33.7 Mt → ❌ Does NOT meet CCC target\n")
cat("   • 25% BEV: 25.7 Mt → ❌ Does NOT meet CCC target\n")
cat("   • 50% BEV: 18.8 Mt → ✅ Meets CCC target\n")
cat("   • 75% BEV: 11.9 Mt → ✅ Meets CCC target\n")
cat("   • 100% BEV: 5.1 Mt → ✅ Meets CCC target\n")

cat("\n🚗 Shared Fleet (13.0M vehicles, 35% reduction):\n")
cat("   • 0% BEV: 21.1 Mt → ❌ Does NOT meet CCC target\n")
cat("   • 25% BEV: 16.7 Mt → ✅ Meets CCC target\n")
cat("   • 50% BEV: 12.2 Mt → ✅ Meets CCC target\n")
cat("   • 75% BEV: 7.8 Mt → ✅ Meets CCC target\n")
cat("   • 100% BEV: 3.3 Mt → ✅ Meets CCC target\n")

cat("\n💡 INSIGHT: Shared fleet requires only 25% BEV to meet CCC target,\n")
cat("   while private fleet requires 50% BEV. Sharing halves the electrification burden!\n")




# Alternative: Horizontal butterfly plot (often easier to read)
p3_horizontal <- ggplot(mileage_scenarios_butterfly, 
                        aes(x = Emissions_Diff, 
                            y = factor(Simple_Label, levels = Simple_Label),
                            fill = ifelse(Emissions_Diff > 0, "Above Target", "Below Target"))) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "solid", color = "red", size = 1) +
  geom_text(aes(label = paste0(round(Emissions_Mt, 1), " Mt"), 
                x = Emissions_Diff + ifelse(Emissions_Diff > 0, 0.5, -0.5)), 
            hjust = ifelse(mileage_scenarios_butterfly$Emissions_Diff > 0, -0.2, 1.2), 
            size = 3.5) +
  annotate("text", x = 2, y = 6, label = "CCC Target: 19.3 Mt", 
           size = 3, color = "red", fontface = "bold") +
  scale_fill_manual(values = c("Above Target" = "coral", "Below Target" = "steelblue")) +
  labs(title = "C) Mileage Behaviour Impact (35% reduction, petrol)",
       x = "Distance from CCC Target (Mt)", 
       y = "Change in vehicle mileage") +
  theme_minimal() +
  theme(legend.position = "none")

print(p3_horizontal)




# ============================================
# TABLE 2: FUEL TYPE PERCENTAGES WITH VEHICLE TOTAL (2025-2035)
# ============================================

# Extract fuel type composition for all years
fuel_type_table <- bind_rows(results$fleet) %>%
  filter(Year >= 2025 & Year <= 2035) %>%
  group_by(Year, Fuel_Group) %>%
  summarise(
    Number_of_Vehicles = sum(Fleet, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Year, Fuel_Group)

# Calculate percentages by year and get total vehicles
fuel_type_percent <- fuel_type_table %>%
  group_by(Year) %>%
  mutate(
    Total_Year = sum(Number_of_Vehicles, na.rm = TRUE),
    Percentage = round(Number_of_Vehicles / Total_Year * 100, 1)
  ) %>%
  ungroup() %>%
  arrange(Year, Fuel_Group)

# Create wide format with percentages
fuel_type_wide_percent <- fuel_type_percent %>%
  select(Year, Fuel_Group, Percentage) %>%
  pivot_wider(
    names_from = Fuel_Group,
    values_from = Percentage,
    values_fill = 0
  ) %>%
  # Add Total column showing actual number of vehicles (not percentage)
  left_join(
    fuel_type_percent %>%
      group_by(Year) %>%
      summarise(Total_Vehicles = first(Total_Year), .groups = "drop"),
    by = "Year"
  ) %>%
  mutate(
    Total_Vehicles_Millions = round(Total_Vehicles / 1e6, 2),
    Total_Display = paste0(format(Total_Vehicles, big.mark = ","), 
                           " (", Total_Vehicles_Millions, "M)")
  ) %>%
  select(Year, Petrol, Diesel, BEV, PHEV, HEV, Total_Display)

# Rename the Total column for clarity
names(fuel_type_wide_percent)[ncol(fuel_type_wide_percent)] <- "Total Vehicles"

# Print the table
cat("\n", strrep("=", 80), "\n")
cat("TABLE 2: FUEL TYPE PERCENTAGES WITH TOTAL VEHICLES (2025-2035)")
cat("\n", strrep("=", 80), "\n")
print(fuel_type_wide_percent)

##########################################################################################################
# updated figures based on paper feedback
##########################################################################################################
# ============================================
# UPDATED COMBINED FIGURE WITH NEW SCENARIO NAMES
# ============================================

# -------------------------------------------------------------------
# STEP 1: Prepare BEV Share data (from fuel shares) with new names
# -------------------------------------------------------------------

# Create BEV comparison from fuel shares with renamed scenarios
bev_comparison <- bind_rows(
  fuel_shares_baseline %>% filter(Fuel_Group == "BEV", Year >= 2025, Year <= 2035) %>% 
    mutate(Scenario = "Baseline"),
  fuel_shares_70pct %>% filter(Fuel_Group == "BEV", Year >= 2025, Year <= 2035) %>% 
    mutate(Scenario = "Mild ZEV Rowback"),
  fuel_shares_60pct %>% filter(Fuel_Group == "BEV", Year >= 2025, Year <= 2035) %>% 
    mutate(Scenario = "Medium ZEV Rowback")
) %>%
  select(Scenario, Year, BEV_Share = sale_percentage) %>%
  mutate(BEV_Share = round(BEV_Share * 100, 1))

# Add Scenario 4 (Slow sales) - same as Baseline for BEV share
bev_scenario4 <- bev_comparison %>%
  filter(Scenario == "Baseline") %>%
  mutate(Scenario = "Slow sales")

bev_comparison_all <- bind_rows(bev_comparison, bev_scenario4)

# Convert to long format for plotting
bev_long <- bev_comparison_all %>%
  mutate(
    Year = as.numeric(Year),
    BEV_Share = as.numeric(BEV_Share),
    Metric = "BEV Share (%)"
  ) %>%
  rename(Value = BEV_Share)

# -------------------------------------------------------------------
# STEP 2: Prepare New Sales data with new scenario names
# -------------------------------------------------------------------

# Baseline sales
sales_baseline <- results_baseline$sales_by_fuel %>%
  filter(Fuel_Group %in% c("BEV")) %>%
  group_by(Year) %>%
  summarise(New_Sales = sum(Required_Sales, na.rm = TRUE)) %>%
  mutate(
    Scenario = "Baseline",
    New_Sales_M = New_Sales / 1e6,
    Metric = "New Sales (M vehicles)"
  )

# Mild ZEV Rowback (70% scenario) sales
sales_mild <- results_70pct$sales_by_fuel %>%
  filter(Fuel_Group %in% c("BEV")) %>%
  group_by(Year) %>%
  summarise(New_Sales = sum(Required_Sales, na.rm = TRUE)) %>%
  mutate(
    Scenario = "Mild ZEV Rowback",
    New_Sales_M = New_Sales / 1e6,
    Metric = "New Sales (M vehicles)"
  )

# Medium ZEV Rowback (60% scenario) sales
sales_medium <- results_60pct$sales_by_fuel %>%
  filter(Fuel_Group %in% c("BEV")) %>%
  group_by(Year) %>%
  summarise(New_Sales = sum(Required_Sales, na.rm = TRUE)) %>%
  mutate(
    Scenario = "Medium ZEV Rowback",
    New_Sales_M = New_Sales / 1e6,
    Metric = "New Sales (M vehicles)"
  )

# Slow sales scenario (Scenario 4)
sales_slow <- results_scenario4$sales_by_fuel %>%
  filter(Fuel_Group %in% c("BEV")) %>%
  group_by(Year) %>%
  summarise(New_Sales = sum(Required_Sales, na.rm = TRUE)) %>%
  mutate(
    Scenario = "Slow sales",
    New_Sales_M = New_Sales / 1e6,
    Metric = "New Sales (M vehicles)"
  )

# Combine all sales
sales_long <- bind_rows(sales_baseline, sales_mild, sales_medium, sales_slow) %>%
  select(Year, Scenario, Value = New_Sales_M, Metric)

# -------------------------------------------------------------------
# STEP 3: Prepare Fleet Age data with new scenario names
# -------------------------------------------------------------------

# Function to extract age data
get_age_data <- function(fleet_list, scenario_name) {
  bind_rows(fleet_list) %>%
    group_by(Year) %>%
    summarise(
      Avg_Age = sum(Vehicle_Age * Fleet, na.rm = TRUE) / sum(Fleet, na.rm = TRUE)
    ) %>%
    mutate(
      Scenario = scenario_name,
      Metric = "Avg Fleet Age (years)"
    ) %>%
    rename(Value = Avg_Age)
}

# Extract age for all scenarios with new names
age_baseline <- get_age_data(results_baseline$fleet, "Baseline")
age_mild <- get_age_data(results_70pct$fleet, "Mild ZEV Rowback")
age_medium <- get_age_data(results_60pct$fleet, "Medium ZEV Rowback")
age_slow <- get_age_data(results_scenario4$fleet, "Slow sales")

# Combine all age data
age_long <- bind_rows(age_baseline, age_mild, age_medium, age_slow)

# -------------------------------------------------------------------
# STEP 4: Combine all metrics
# -------------------------------------------------------------------

all_metrics <- bind_rows(
  bev_long %>% select(Year, Scenario, Value, Metric),
  sales_long,
  age_long
) %>%
  mutate(
    Scenario = factor(Scenario, levels = c("Baseline", "Mild ZEV Rowback", "Medium ZEV Rowback", "Slow sales")),
    Metric = factor(Metric, levels = c("BEV Share (%)", "New Sales (M vehicles)", "Avg Fleet Age (years)"))
  )

# -------------------------------------------------------------------
# STEP 5: Create individual plots with updated colors and names
# -------------------------------------------------------------------

# Define color palette for the four scenarios
scenario_colors <- c(
  "Baseline" = "#2E86AB",           # Blue
  "Mild ZEV Rowback" = "#F18F01",   # Orange
  "Medium ZEV Rowback" = "#C73E1D", # Red
  "Slow sales" = "#4DAF4A"          # Green
)

# Define shapes for the four scenarios
scenario_shapes <- c(
  "Baseline" = 16,
  "Mild ZEV Rowback" = 17,
  "Medium ZEV Rowback" = 15,
  "Slow sales" = 18
)

# Plot 1: BEV Share (%)
p1 <- all_metrics %>%
  filter(Metric == "BEV Share (%)") %>%
  ggplot(aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5, aes(shape = Scenario)) +
  geom_vline(xintercept = 2030, linetype = "dashed", color = "gray50", size = 0.5) +
  geom_vline(xintercept = 2035, linetype = "dashed", color = "gray50", size = 0.5) +
  scale_x_continuous(breaks = seq(2025, 2035, 2), limits = c(2025, 2035)) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%")) +
  scale_color_manual(values = scenario_colors) +
  scale_shape_manual(values = scenario_shapes) +
  labs(
    title = "A) BEV Share of New Sales",
    x = NULL,
    y = "BEV Share (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Plot 2: New Sales (M vehicles)
p2 <- all_metrics %>%
  filter(Metric == "New Sales (M vehicles)") %>%
  ggplot(aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5, aes(shape = Scenario)) +
  scale_x_continuous(breaks = seq(2025, 2035, 2), limits = c(2025, 2035)) +
  scale_y_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, 0.5),
                     labels = function(x) paste0(x, "M")) +
  scale_color_manual(values = scenario_colors) +
  scale_shape_manual(values = scenario_shapes) +
  labs(
    title = "B) Annual New BEVs Sales",
    x = NULL,
    y = "New Sales (millions)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Plot 3: Average Fleet Age (years)
p3 <- all_metrics %>%
  filter(Metric == "Avg Fleet Age (years)") %>%
  ggplot(aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5, aes(shape = Scenario)) +
  scale_x_continuous(breaks = seq(2025, 2035, 2), limits = c(2025, 2035)) +
  scale_y_continuous(limits = c(8.5, 10.5), breaks = seq(8.5, 10.5, 0.5)) +
  scale_color_manual(name = "Scenario", values = scenario_colors) +
  scale_shape_manual(name = "Scenario", values = scenario_shapes) +
  labs(
    title = "C) Average Fleet Age",
    x = "Year",
    y = "Age (years)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# -------------------------------------------------------------------
# STEP 6: Combine all three plots
# -------------------------------------------------------------------

combined_figure <- (p1 | p2) / p3 +
  plot_annotation(
    #title = "ZEV Mandate Scenario Comparison",
    #subtitle = "Comparing baseline policy, ZEV rowback scenarios, and slow sales growth",
    # caption = "Note: Slow sales scenario assumes 2M annual sales cap with unchanged ZEV mandate percentages"
  ) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

# Save and display
#ggsave("Figure_Combined_Scenarios.png", combined_figure, 
#  width = 12, height = 10, dpi = 300, bg = "white")

print(combined_figure)

# -------------------------------------------------------------------
# STEP 7: Updated Summary Table with new scenario names
# -------------------------------------------------------------------

cat("\n\n", strrep("=", 80), "\n")
cat("SCENARIO COMPARISON SUMMARY (Key Years)")
cat("\n", strrep("=", 80), "\n")

key_years <- c(2025, 2028, 2030, 2032, 2035)

summary_table <- all_metrics %>%
  filter(Year %in% key_years) %>%
  pivot_wider(
    id_cols = c(Scenario, Year),
    names_from = Metric,
    values_from = Value
  ) %>%
  arrange(Scenario, Year) %>%
  mutate(
    `BEV Share (%)` = round(`BEV Share (%)`, 1),
    `New Sales (M)` = round(`New Sales (M vehicles)`, 2),
    `Fleet Age (yrs)` = round(`Avg Fleet Age (years)`, 1)
  ) %>%
  select(Scenario, Year, `BEV Share (%)`, `New Sales (M)`, `Fleet Age (yrs)`)

print(summary_table, n = 20)

# -------------------------------------------------------------------
# STEP 8: Updated extract_scenario_metrics function with new names
# -------------------------------------------------------------------

# Re-run metrics extraction with new names
metrics_baseline <- extract_scenario_metrics(results_baseline, "Baseline", "Predefined")
metrics_mild <- extract_scenario_metrics(results_70pct, "Mild ZEV Rowback", "Predefined")
metrics_medium <- extract_scenario_metrics(results_60pct, "Medium ZEV Rowback", "Predefined")
metrics_slow <- extract_scenario_metrics(results_scenario4, "Slow sales", "Fixed")

# Combine all results
all_scenario_metrics <- bind_rows(
  metrics_baseline,
  metrics_mild,
  metrics_medium,
  metrics_slow
)

cat("\n\n", strrep("=", 80), "\n")
cat("FINAL SCENARIO COMPARISON SUMMARY (2037 Metrics)")
cat("\n", strrep("=", 80), "\n")
print(all_scenario_metrics)



# Extract age distribution for each scenario in 2035
get_age_distribution <- function(results, scenario_name, target_year = 2035) {
  bind_rows(results$fleet) %>%
    filter(Year == target_year) %>%
    group_by(Fuel_Group, Vehicle_Age) %>%
    summarise(Fleet = sum(Fleet), .groups = "drop") %>%
    mutate(Scenario = scenario_name)
}

# Get age distributions for all scenarios
age_dist_baseline <- get_age_distribution(results_baseline, "Baseline")
age_dist_mild <- get_age_distribution(results_70pct, "Mild ZEV Rowback")
age_dist_medium <- get_age_distribution(results_60pct, "Medium ZEV Rowback")
age_dist_slow <- get_age_distribution(results_scenario4, "Slow sales")

# Calculate average age by fuel type for each scenario
avg_age_by_fuel <- bind_rows(age_dist_baseline, age_dist_mild, age_dist_medium, age_dist_slow) %>%
  group_by(Scenario, Fuel_Group) %>%
  summarise(
    Avg_Age = sum(Vehicle_Age * Fleet) / sum(Fleet),
    .groups = "drop"
  )

print(avg_age_by_fuel)

###Fleet Turnover Rate - The Missing Metric###


# Calculate fleet turnover metrics for each scenario
calculate_turnover <- function(results, scenario_name, target_year = 2035) {
  fleet_year <- bind_rows(results$fleet) %>% filter(Year == target_year)
  sales_year <- results$sales_by_fuel %>% filter(Year == target_year)
  
  turnover <- fleet_year %>%
    group_by(Fuel_Group) %>%
    summarise(Fleet_Count = sum(Fleet), .groups = "drop") %>%
    left_join(
      sales_year %>%
        group_by(Fuel_Group) %>%
        summarise(New_Sales = sum(Required_Sales), .groups = "drop"),
      by = "Fuel_Group"
    ) %>%
    mutate(
      Turnover_Rate = New_Sales / Fleet_Count * 100,
      Scenario = scenario_name,
      Year = target_year
    )
  
  return(turnover)
}

# Calculate turnover for all scenarios in 2035
turnover_2035 <- bind_rows(
  calculate_turnover(results_baseline, "Baseline", 2035),
  calculate_turnover(results_70pct, "Mild ZEV Rowback", 2035),
  calculate_turnover(results_60pct, "Medium ZEV Rowback", 2035),
  calculate_turnover(results_scenario4, "Slow sales", 2035)
)

print(turnover_2035)


# # Create comprehensive scenario comparison table
# scenario_fleet_metrics <- bind_rows(
#   # Baseline
#   data.frame(
#     Scenario = "Baseline",
#     BEV_Share_2035 = all_scenario_metrics$BEV_Share_2037_pct[1],
#     Avg_Fleet_Age = all_scenario_metrics$Avg_Fleet_Age_2037[1],
#     Fossil_Fleet_Size_M = fossil_fleet_baseline_2035 / 1e6,
#     Petrol_Turnover_pct = turnover_2035$Turnover_Rate[turnover_2035$Scenario == "Baseline" & 
#                                                         turnover_2035$Fuel_Group == "Petrol"],
#     Diesel_Turnover_pct = turnover_2035$Turnover_Rate[turnover_2035$Scenario == "Baseline" & 
#                                                         turnover_2035$Fuel_Group == "Diesel"],
#     Median_Petrol_Retirement_Age = retirement_waves_baseline %>%
#       filter(Fuel_Group == "Petrol", Cumulative_Retired >= 0.5 * sum(Retired)) %>%
#       pull(Vehicle_Age) %>% min(),
#     Median_Diesel_Retirement_Age = retirement_waves_baseline %>%
#       filter(Fuel_Group == "Diesel", Cumulative_Retired >= 0.5 * sum(Retired)) %>%
#       pull(Vehicle_Age) %>% min()
#   ),
#   # Add similar rows for Mild, Medium, Slow...
# )
# 
# print(scenario_fleet_metrics)


# Create a figure showing age distribution shifts
p_age_shift <- bind_rows(
  age_dist_baseline %>% mutate(Scenario = "Baseline"),
  age_dist_mild %>% mutate(Scenario = "Mild ZEV Rowback"),
  age_dist_medium %>% mutate(Scenario = "Medium ZEV Rowback"),
  age_dist_slow %>% mutate(Scenario = "Slow sales")
) %>%
  filter(Fuel_Group %in% c("Petrol", "Diesel", "BEV")) %>%
  group_by(Scenario, Fuel_Group, Vehicle_Age) %>%
  summarise(Fleet = sum(Fleet), .groups = "drop") %>%
  ggplot(aes(x = Vehicle_Age, y = Fleet/1e6, fill = Fuel_Group)) +
  geom_area(alpha = 0.7) +
  facet_wrap(~Scenario, nrow = 2) +
  labs(title = "2035 Fleet Age Distribution by Scenario",
       subtitle = "Shows how ZEV rowback scenarios affect fossil fuel fleet composition",
       x = "Vehicle Age (years)", y = "Fleet Size (millions)") +
  theme_minimal()

print(p_age_shift)
##ggsave("Figure_Fleet_Age_by_Scenario.png", p_age_shift, width = 12, height = 8, dpi = 300)


# ============================================
# AGE DISPARITY TABLE BY FUEL TYPE AND SCENARIO
# ============================================

library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# -------------------------------------------------------------------
# STEP 1: Calculate average age for each scenario and fuel type
# -------------------------------------------------------------------

# Function to calculate average age from fleet results
calculate_avg_age <- function(results, scenario_name) {
  bind_rows(results$fleet) %>%
    filter(Year == 2035) %>%  # Use 2035 as the target year
    group_by(Fuel_Group) %>%
    summarise(
      Avg_Age = sum(Vehicle_Age * Fleet, na.rm = TRUE) / sum(Fleet, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Scenario = scenario_name)
}

# Calculate for all four scenarios
age_baseline <- calculate_avg_age(results_baseline, "Baseline")
age_mild <- calculate_avg_age(results_70pct, "Mild ZEV Rowback")
age_medium <- calculate_avg_age(results_60pct, "Medium ZEV Rowback")
age_slow <- calculate_avg_age(results_scenario4, "Slow sales")

# Combine all results
all_ages <- bind_rows(age_baseline, age_mild, age_medium, age_slow)

# -------------------------------------------------------------------
# STEP 2: Pivot to wide format for the table
# -------------------------------------------------------------------

age_table_wide <- all_ages %>%
  # Round to 1 decimal place
  mutate(Avg_Age = round(Avg_Age, 1)) %>%
  # Pivot wider
  pivot_wider(
    id_cols = Fuel_Group,
    names_from = Scenario,
    values_from = Avg_Age
  ) %>%
  # Reorder fuel types as desired
  mutate(Fuel_Group = factor(Fuel_Group, 
                             levels = c("BEV", "PHEV", "HEV", "Petrol", "Diesel"))) %>%
  arrange(Fuel_Group) %>%
  # Add insight column based on fuel type
  mutate(
    Insight = case_when(
      Fuel_Group == "BEV" ~ "New technology, young fleet",
      Fuel_Group == "PHEV" ~ "Getting younger in faster ZEV scenarios",
      Fuel_Group == "HEV" ~ "Mature technology, stable age",
      Fuel_Group == "Petrol" ~ "⚠️ Aging fleet",
      Fuel_Group == "Diesel" ~ "⚠️ Critically old fleet",
      TRUE ~ ""
    )
  )%>%print()

# -------------------------------------------------------------------
# STEP 3: Display the formatted table
# -------------------------------------------------------------------

# Print basic table
cat("\n", strrep("=", 100), "\n")
cat("AGE DISPARITY BY FUEL TYPE AND SCENARIO (2035)")
cat("\n", strrep("=", 100), "\n")
print(age_table_wide)

# -------------------------------------------------------------------
# STEP 4: Create publication-ready formatted table
# -------------------------------------------------------------------

# Create a nicely formatted table for publication
age_table_formatted <- age_table_wide %>%
  select(Fuel_Group, Baseline, `Mild ZEV Rowback`, `Medium ZEV Rowback`, `Slow sales`, Insight)

# Print with kable for better formatting
if(require(kableExtra, quietly = TRUE)) {
  age_table_formatted %>%
    kable(
      caption = "Average Vehicle Age by Fuel Type and Scenario (2035)",
      format = "markdown",
      align = c("l", "c", "c", "c", "c", "l")
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE
    ) %>%
    column_spec(2:5, width = "8em") %>%
    row_spec(4, bold = TRUE, color = "red") %>%
    row_spec(5, bold = TRUE, color = "red")
} else {
  # Simple print if kableExtra not available
  print(age_table_formatted)
}

# -------------------------------------------------------------------
# STEP 5: Calculate additional statistics for discussion
# -------------------------------------------------------------------

# Calculate the range (max - min) across scenarios for each fuel type
age_variability <- all_ages %>%
  group_by(Fuel_Group) %>%
  summarise(
    Min_Age = min(Avg_Age),
    Max_Age = max(Avg_Age),
    Range = Max_Age - Min_Age,
    Median_Age = median(Avg_Age),
    .groups = "drop"
  ) %>%
  mutate(
    Interpretation = case_when(
      Range < 0.3 ~ "Very stable across scenarios",
      Range < 0.8 ~ "Moderately affected by policy",
      Range >= 0.8 ~ "Strongly affected by policy",
      TRUE ~ ""
    )
  )

cat("\n", strrep("=", 100), "\n")
cat("AGE VARIABILITY ACROSS SCENARIOS")
cat("\n", strrep("=", 100), "\n")
print(age_variability)

# -------------------------------------------------------------------
# STEP 6: Calculate the difference from Baseline for each scenario
# -------------------------------------------------------------------

age_differences <- all_ages %>%
  pivot_wider(
    id_cols = Fuel_Group,
    names_from = Scenario,
    values_from = Avg_Age
  ) %>%
  mutate(
    `Mild Difference` = `Mild ZEV Rowback` - Baseline,
    `Medium Difference` = `Medium ZEV Rowback` - Baseline,
    `Slow Difference` = `Slow sales` - Baseline
  ) %>%
  select(Fuel_Group, `Mild Difference`, `Medium Difference`, `Slow Difference`)

cat("\n", strrep("=", 100), "\n")
cat("AGE DIFFERENCE FROM BASELINE (years)")
cat("\n", strrep("=", 100), "\n")
cat("Positive = older than Baseline, Negative = younger than Baseline\n\n")
print(age_differences)

# -------------------------------------------------------------------
# STEP 7: Visualization of age disparities
# -------------------------------------------------------------------

library(ggplot2)

# Create bar chart comparing ages across scenarios
p_age_comparison <- all_ages %>%
  mutate(
    Fuel_Group = factor(Fuel_Group, levels = c("BEV", "PHEV", "HEV", "Petrol", "Diesel")),
    Scenario = factor(Scenario, levels = c("Baseline", "Mild ZEV Rowback", "Medium ZEV Rowback", "Slow sales"))
  ) %>%
  ggplot(aes(x = Fuel_Group, y = Avg_Age, fill = Scenario)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(Avg_Age, 1)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c(
    "Baseline" = "#2E86AB",
    "Mild ZEV Rowback" = "#F18F01",
    "Medium ZEV Rowback" = "#C73E1D",
    "Slow sales" = "#4DAF4A"
  )) +
  labs(
    title = "Average Vehicle Age by Fuel Type (2035)",
    subtitle = "Diesel fleet critically old (17.6-18.2 yrs) across all scenarios",
    x = "Fuel Type",
    y = "Average Age (years)",
    fill = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

print(p_age_comparison)
#ggsave("Figure_Age_Disparity.png", p_age_comparison, width = 10, height = 6, dpi = 300)

# -------------------------------------------------------------------
# STEP 8: Heatmap of age differences
# -------------------------------------------------------------------

# Create a heatmap showing age patterns
age_matrix <- all_ages %>%
  pivot_wider(
    id_cols = Fuel_Group,
    names_from = Scenario,
    values_from = Avg_Age
  ) %>%
  tibble::column_to_rownames("Fuel_Group") %>%
  as.matrix()

if(require(pheatmap, quietly = TRUE)) {
  pheatmap(age_matrix,
           main = "Average Vehicle Age by Fuel Type and Scenario (2035)",
           cluster_rows = FALSE,
           cluster_cols = FALSE,
           display_numbers = TRUE,
           number_format = "%.1f",
           colorRampPalette(c("#2E86AB", "#FFFFFF", "#C73E1D"))(50),
           width = 8, height = 6)
}

# -------------------------------------------------------------------
# STEP 9: Export to CSV for further analysis
# -------------------------------------------------------------------

# Export the main age table
write.csv(age_table_wide, 
          "Age_Disparity_Table.csv", 
          row.names = FALSE)

# Export the differences table
write.csv(age_differences, 
          "Age_Differences_from_Baseline.csv", 
          row.names = FALSE)

cat("\n", strrep("=", 100), "\n")
cat("FILES EXPORTED:")
cat("\n", strrep("=", 100), "\n")
cat("• Age_Disparity_Table.csv - Main age table\n")
cat("• Age_Differences_from_Baseline.csv - Differences from Baseline\n")
cat("• Figure_Age_Disparity.png - Bar chart visualization\n")

# -------------------------------------------------------------------
# STEP 10: Key findings for discussion (CORRECTED)
# -------------------------------------------------------------------

cat("\n", strrep("=", 100), "\n")
cat("KEY FINDINGS FOR DISCUSSION")
cat("\n", strrep("=", 100), "\n")

# Calculate key insights - CORRECTED VERSION
# Method 1: Using the age_table_wide directly
diesel_age <- age_table_wide %>% 
  filter(Fuel_Group == "Diesel") %>% 
  pull(Baseline)

petrol_age <- age_table_wide %>% 
  filter(Fuel_Group == "Petrol") %>% 
  pull(Baseline)

diesel_vs_petrol_gap <- diesel_age - petrol_age

cat("\n1. DIESEL VS PETROL AGE GAP (Baseline):\n")
cat("   Diesel vehicles are", round(diesel_vs_petrol_gap, 1), 
    "years older than petrol vehicles on average\n")
cat("   This gap persists across all scenarios\n\n")

# Calculate the gap for all scenarios
age_gaps <- data.frame(
  Scenario = c("Baseline", "Mild ZEV Rowback", "Medium ZEV Rowback", "Slow sales"),
  Diesel_Age = c(
    age_table_wide %>% filter(Fuel_Group == "Diesel") %>% pull(Baseline),
    age_table_wide %>% filter(Fuel_Group == "Diesel") %>% pull(`Mild ZEV Rowback`),
    age_table_wide %>% filter(Fuel_Group == "Diesel") %>% pull(`Medium ZEV Rowback`),
    age_table_wide %>% filter(Fuel_Group == "Diesel") %>% pull(`Slow sales`)
  ),
  Petrol_Age = c(
    age_table_wide %>% filter(Fuel_Group == "Petrol") %>% pull(Baseline),
    age_table_wide %>% filter(Fuel_Group == "Petrol") %>% pull(`Mild ZEV Rowback`),
    age_table_wide %>% filter(Fuel_Group == "Petrol") %>% pull(`Medium ZEV Rowback`),
    age_table_wide %>% filter(Fuel_Group == "Petrol") %>% pull(`Slow sales`)
  )
) %>%
  mutate(Gap = Diesel_Age - Petrol_Age)

print(age_gaps)

# Method 2: Alternative calculation using the differences table
cat("\n2. SLOW SALES SCENARIO IMPACT ON FLEET AGE:\n")

# Extract slow sales differences
slow_impact_petrol <- age_differences %>%
  filter(Fuel_Group == "Petrol") %>%
  pull(`Slow Difference`)

slow_impact_diesel <- age_differences %>%
  filter(Fuel_Group == "Diesel") %>%
  pull(`Slow Difference`)

slow_impact_hev <- age_differences %>%
  filter(Fuel_Group == "HEV") %>%
  pull(`Slow Difference`)

slow_impact_phev <- age_differences %>%
  filter(Fuel_Group == "PHEV") %>%
  pull(`Slow Difference`)

slow_impact_bev <- age_differences %>%
  filter(Fuel_Group == "BEV") %>%
  pull(`Slow Difference`)

cat("   • Petrol fleet ages by +", round(slow_impact_petrol, 1), "years\n", sep="")
cat("   • Diesel fleet ages by +", round(slow_impact_diesel, 1), "years\n", sep="")
cat("   • HEV fleet ages by +", round(slow_impact_hev, 1), "years\n", sep="")
cat("   • PHEV fleet ages by +", round(slow_impact_phev, 1), "years\n", sep="")
cat("   • BEV fleet ages by +", round(slow_impact_bev, 1), "years\n", sep="")

cat("\n3. PHEV AGE DYNAMICS:\n")
phev_ages <- age_table_wide %>%
  filter(Fuel_Group == "PHEV") %>%
  select(Baseline, `Mild ZEV Rowback`, `Medium ZEV Rowback`, `Slow sales`) %>%
  pivot_longer(everything(), names_to = "Scenario", values_to = "Age")

cat("   PHEV age ranges from", round(min(phev_ages$Age), 1), "to", round(max(phev_ages$Age), 1), "years\n")
cat("   • Fastest ZEV scenario (Medium) =", 
    round(phev_ages$Age[phev_ages$Scenario == "Medium ZEV Rowback"], 1), "years (youngest)\n")
cat("   • Slow sales =", 
    round(phev_ages$Age[phev_ages$Scenario == "Slow sales"], 1), "years (oldest)\n")
cat("   • Baseline =", 
    round(phev_ages$Age[phev_ages$Scenario == "Baseline"], 1), "years\n")

cat("\n4. POLICY SENSITIVITY ANALYSIS:\n")
cat("   Fuel types most affected by policy changes:\n")
age_variability %>%
  filter(Interpretation == "Strongly affected by policy") %>%
  mutate(Message = paste0("   • ", Fuel_Group, ": Range = ", round(Range, 2), " years")) %>%
  pull(Message) %>%
  cat(sep = "\n")

cat("\n   Fuel types moderately affected:\n")
age_variability %>%
  filter(Interpretation == "Moderately affected by policy") %>%
  mutate(Message = paste0("   • ", Fuel_Group, ": Range = ", round(Range, 2), " years")) %>%
  pull(Message) %>%
  cat(sep = "\n")

cat("\n5. CRITICAL INSIGHTS FOR DISCUSSION:\n")
cat("   • Diesel fleet is consistently", round(age_gaps$Gap[1], 1), 
    "years older than petrol fleet\n")
cat("   • Slow sales scenario ages ALL fuel types by", 
    round(max(slow_impact_petrol, slow_impact_diesel, slow_impact_hev, 
              slow_impact_phev, slow_impact_bev), 1), "years\n")
cat("   • PHEVs show the strongest policy response (", 
    round(age_variability$Range[age_variability$Fuel_Group == "PHEV"], 1), 
    "year range across scenarios)\n")
cat("   • BEVs remain young (4.1-4.4 years) regardless of scenario\n")

# -------------------------------------------------------------------
# STEP 11: Create a formatted summary table for publication
# -------------------------------------------------------------------

# Create a comprehensive summary table
summary_for_publication <- age_table_wide %>%
  left_join(age_variability %>% select(Fuel_Group, Range, Interpretation), by = "Fuel_Group") %>%
  mutate(
    `Policy Sensitivity` = case_when(
      Range < 0.3 ~ "Low",
      Range < 0.8 ~ "Medium",
      Range >= 0.8 ~ "High",
      TRUE ~ "Unknown"
    ),
    `Climate Implication` = case_when(
      Fuel_Group == "Diesel" ~ "Critical - highest emissions, oldest fleet",
      Fuel_Group == "Petrol" ~ "High - large fleet, aging",
      Fuel_Group == "HEV" ~ "Moderate - transitional technology",
      Fuel_Group == "PHEV" ~ "Moderate - policy-sensitive",
      Fuel_Group == "BEV" ~ "Low - zero tailpipe emissions",
      TRUE ~ ""
    )
  )

cat("\n", strrep("=", 100), "\n")
cat("SUMMARY TABLE FOR PUBLICATION")
cat("\n", strrep("=", 100), "\n")
print(summary_for_publication %>%
        select(Fuel_Group, Baseline, `Mild ZEV Rowback`, `Medium ZEV Rowback`, 
               `Slow sales`, `Policy Sensitivity`, `Climate Implication`))

# Export the summary table
write.csv(summary_for_publication, 
          "Age_Disparity_Summary_for_Publication.csv", 
          row.names = FALSE)

# -------------------------------------------------------------------
# STEP 12: Create a line plot showing age trends across scenarios
# -------------------------------------------------------------------

p_age_trends <- all_ages %>%
  mutate(
    Fuel_Group = factor(Fuel_Group, levels = c("BEV", "PHEV", "HEV", "Petrol", "Diesel")),
    Scenario = factor(Scenario, levels = c("Baseline", "Mild ZEV Rowback", "Medium ZEV Rowback", "Slow sales"))
  ) %>%
  ggplot(aes(x = Scenario, y = Avg_Age, color = Fuel_Group, group = Fuel_Group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = round(Avg_Age, 1)), vjust = -0.8, size = 3) +
  scale_color_manual(values = c(
    "BEV" = "#2E86AB",
    "PHEV" = "#F18F01",
    "HEV" = "#4DAF4A",
    "Petrol" = "#C73E1D",
    "Diesel" = "#8B0000"
  )) +
  labs(
    title = "Fleet Age Trends Across Scenarios",
    subtitle = "Diesel and Petrol show consistent aging; PHEV most policy-responsive",
    x = "Scenario",
    y = "Average Age (years)",
    color = "Fuel Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 15, hjust = 0.8)
  )

print(p_age_trends)
#ggsave("Figure_Age_Trends_Across_Scenarios.png", p_age_trends, width = 10, height = 6, dpi = 300)

cat("\n", strrep("=", 100), "\n")
cat("ADDITIONAL FILES EXPORTED:")
cat("\n", strrep("=", 100), "\n")
cat("• Age_Disparity_Summary_for_Publication.csv - Complete summary table\n")
cat("• Figure_Age_Trends_Across_Scenarios.png - Line plot showing age trends\n")


# ============================================================================
# MILEAGE REALLOCATION SCENARIOS (VKT-conserving) — CORRECTED
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
CCC_TARGET        <- ccc_car_allocation_2035   # 19.3

# 🔴 FIX: Keep the original emissions results under a different name
baseline_emissions_results <- all_car_emissions_revised

# 🔴 FIX: Get BASELINE HEADLINE from the renamed object
BASELINE_HEADLINE <- baseline_emissions_results$Car_CO2_Mt[
  baseline_emissions_results$Year == 2035 & 
    baseline_emissions_results$Scenario == "Baseline"
]

GAMMA_PRIMARY <- 0.05;  GAMMA_RANGE <- c(0.02, 0.05, 0.08)
DELTA_PRIMARY <- 0.20;  DELTA_RANGE <- c(0.10, 0.20, 0.30)

# === 1. Complete base per-vehicle mileage profile m0(age, fuel) ==============
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


# === 2. VKT-conserving reallocation ==========================================
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
# 🔴 CRITICAL FIX: Vectorized emissions factor using reconciled fuel-specific data
# ============================================================================

# 🔴 FIX: Use base_gkm_by_buildyear (reconciled, fuel-specific) not flat single-column
emis_lookup_full <- base_gkm_by_buildyear %>%
  tidyr::complete(Fuel_Group, Build_Year = min(Build_Year):max(Build_Year)) %>%
  # For PHEV, use the constant value
  bind_rows(
    data.frame(
      Fuel_Group = "PHEV",
      Build_Year = min(base_gkm_by_buildyear$Build_Year):max(base_gkm_by_buildyear$Build_Year),
      g_per_km = PHEV_REALWORLD_GKM
    )
  )

min_by_lookup <- min(emis_lookup_full$Build_Year)
max_by_lookup <- max(emis_lookup_full$Build_Year)

# Vectorized function - no for loop!
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
# 🔴 FIX: Collapse fleet first (Region/Origin not used)
# ============================================================================

fleet_all_base <- bind_rows(results_baseline$fleet) %>%
  group_by(Year, Vehicle_Age, Fuel_Group) %>%
  summarise(Fleet = sum(Fleet, na.rm = TRUE), .groups = "drop")

cat("Rows before collapsing:", nrow(bind_rows(results_baseline$fleet)), "\n")
cat("Rows after collapsing: ", nrow(fleet_all_base), "\n")


# ============================================================================
# 🔴 FIX: Updated scenario_emissions_fast with vectorized factor
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


# ============================================================================
# 🔴 FIX: Single-year version for calibration (uses same vectorized logic)
# ============================================================================

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


# === 5. Inputs, two base tables, calibration to TRUE flat ====================

base_emp <- build_base_mileage(mileage_decay_raw)  # empirical m0
base_flat <- base_emp %>% mutate(Base_Mileage = FLAT_KM)  # constant per vehicle

young_skew <- function(gamma) function(a) exp(-gamma * a)
road_price <- function(delta) function(f)
  ifelse(f %in% c("Petrol", "Diesel"), 1 + delta,
         ifelse(f == "BEV", 1 - delta, 1))

# 🔴 FIX: Use the single-year version for calibration
raw_flat <- scenario_emissions_single(fleet_all_base, base_flat, TARGET_YEAR)
calib_k <- BASELINE_HEADLINE / raw_flat
cat(sprintf("\nTRUE flat 2035 raw = %.2f Mt  ->  calib_k = %.4f\n", raw_flat, calib_k))


# === 6. Four headline scenarios ==============================================

# 🔴 FIX: Use scenario_emissions_fast with collapsed fleet
headline_data <- list(
  flat = scenario_emissions_fast(fleet_all_base, base_flat, calib = calib_k, years = TARGET_YEAR),
  emp = scenario_emissions_fast(fleet_all_base, base_emp, calib = calib_k, years = TARGET_YEAR),
  exp = scenario_emissions_fast(fleet_all_base, base_emp, calib = calib_k,
                                age_mult = young_skew(GAMMA_PRIMARY), years = TARGET_YEAR),
  road = scenario_emissions_fast(fleet_all_base, base_emp, calib = calib_k,
                                 fuel_mult = road_price(DELTA_PRIMARY), years = TARGET_YEAR)
)

headline <- tibble(
  Scenario = factor(
    c("1. Flat baseline", "2. Age-specific mileage",
      "3. Exponential decay mileage(\u03b3=0.05)", "4. EV road pricing (\u03b4=0.20)"),
    levels = c("1. Flat baseline", "2. Age-specific mileage",
               "3. Exponential decay mileage(\u03b3=0.05)", "4. EV road pricing (\u03b4=0.20)")
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


# === 7. Illustrative ranges ==================================================

range_young <- purrr::map_dfr(GAMMA_RANGE, function(g) {
  scenario_emissions_fast(fleet_all_base, base_emp, calib = calib_k,
                          age_mult = young_skew(g), years = TARGET_YEAR) %>%
    mutate(Lever = "Exponential decay mileage (\u03b3)", x = g)
})

range_road <- purrr::map_dfr(DELTA_RANGE, function(d) {
  scenario_emissions_fast(fleet_all_base, base_emp, calib = calib_k,
                          fuel_mult = road_price(d), years = TARGET_YEAR) %>%
    mutate(Lever = "EV road pricing (\u03b4)", x = d)
})

cat("\n=== Exponential decay mileage RANGE ===\n")
print(range_young)
cat("\n=== ROAD-PRICING RANGE ===\n")
print(range_road)


# === 8. Trajectory 2025-2035 =================================================

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
print(traj,n=Inf)

traj$Scenario <- factor(traj$Scenario,
                        levels = c("Flat baseline", "Age-specific mileage",
                                   "Exponential decay mileage", "EV road pricing"))

# === 9. FIGURES ==============================================================

scen_cols <- c("1. Flat baseline" = "#999999",
               "2. Age-specific mileage" = "#2E86AB",
               "3. Exponential decay mileage(\u03b3=0.05)" = "#4DAF4A",
               "4. EV road pricing (\u03b4=0.20)" = "#C73E1D")

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
  scale_fill_manual(values = c("Exponential decay mileage (\u03b3)" = "#4DAF4A",
                               "EV road pricing (\u03b4)" = "#C73E1D")) +
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

# Also create the stacked bar version
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
               "Exponential decay mileage (γ=0.05)",
               "EV road pricing (δ=0.20)")
  ) +
  labs(x = "Year", y = expression(CO[2]~"emissions (Mt)"), fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
print(p_traj_bar)

# ============================================
# CCC SURFACE TRANSPORT AND CAR ALLOCATION
# ============================================

# Step 1: Surface transport targets
ccc_surface_residual <- data.frame(
  Year = 2024:2035,
  CCC_Surface_CO2_Mt = c(98, 92, 87, 81, 76, 69, 62, 55, 49, 43, 38, 32)
)

# Step 2: Calculate car allocation (60.2% of surface)
car_share <- 0.602
ccc_car <- ccc_surface_residual %>%
  mutate(CCC_Car_CO2_Mt = round(CCC_Surface_CO2_Mt * car_share, 1))

# Step 3: Check 2035 car target
ccc_car_2035 <- ccc_car %>% 
  filter(Year == 2035) %>% 
  pull(CCC_Car_CO2_Mt)
cat("2035 Car Target:", ccc_car_2035, "MtC\n")  # Should be 19.3

# Step 4: Cumulative surface transport (2027-2035)
ccc_surface_cumulative <- ccc_surface_residual %>%
  filter(Year >= 2027, Year <= 2035) %>%
  summarise(total = sum(CCC_Surface_CO2_Mt)) %>%
  pull(total)
cat("Surface Transport Cumulative (2027-2035):", 
    round(ccc_surface_cumulative, 1), "MtC\n")  # 505

# Step 5: Cumulative cars only (2027-2035)
ccc_car_cumulative <- ccc_car %>%
  filter(Year >= 2027, Year <= 2035) %>%
  summarise(total = sum(CCC_Car_CO2_Mt)) %>%
  pull(total)
cat("Car Cumulative (2027-2035):", 
    round(ccc_car_cumulative, 1), "MtC\n")  # 304


# ============================================
# CREATE COMPREHENSIVE EXCEL FILE WITH ALL DATA
# ============================================

library(openxlsx)
library(tidyverse)

# Create a new workbook
wb <- createWorkbook()




# ============================================
# SHEET 1: Fuel Groups and Sale Percentages
# ============================================

# Get fuel shares data
fuel_shares_export <- fuel_shares_complete %>%
  rename(
    Sale_Percentage = sale_percentage,
    Fuel_Type = Fuel_Group
  ) %>%
  arrange(Year, Fuel_Type)

addWorksheet(wb, "Fuel_Shares")
writeData(wb, "Fuel_Shares", fuel_shares_export)
# Add conditional formatting to highlight percentages
addStyle(wb, "Fuel_Shares",
         style = createStyle(numFmt = "0.00%"),
         rows = 2:(nrow(fuel_shares_export)+1),
         cols = 3, gridExpand = TRUE)

# ============================================
# SHEET 2: NATCOP Targets by Year
# ============================================

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
# Format numbers
addStyle(wb, "NATCOP_Targets",
         style = createStyle(numFmt = "#,##0"),
         rows = 2:(nrow(natcop_export)+1),
         cols = c(2,4), gridExpand = TRUE)
addStyle(wb, "NATCOP_Targets",
         style = createStyle(numFmt = "0.00%"),
         rows = 2:(nrow(natcop_export)+1),
         cols = 3, gridExpand = TRUE)

# ============================================
# SHEET 3: Retirement Rates by Age and Fuel Type
# ============================================

# Get retirement rates in wide format (easier to read)
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
# Format as percentages
for(col in 2:ncol(retirement_wide)) {
  addStyle(wb, "Retirement_Rates",
           style = createStyle(numFmt = "0.00%"),
           rows = 2:(nrow(retirement_wide)+1),
           cols = col, gridExpand = TRUE)
}

# ============================================
# SHEET 4: Survival Rates by Age and Fuel Type
# ============================================

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
# Format as percentages
for(col in 2:ncol(survival_wide)) {
  addStyle(wb, "Survival_Rates",
           style = createStyle(numFmt = "0.00%"),
           rows = 2:(nrow(survival_wide)+1),
           cols = col, gridExpand = TRUE)
}

# ============================================
# SHEET 5: Annual New Sales by Fuel Type
# ============================================

# Get sales from results
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
# Format numbers
for(col in 2:ncol(sales_by_fuel)) {
  if(col <= ncol(sales_by_fuel)-1) {
    addStyle(wb, "New_Sales_by_Fuel",
             style = createStyle(numFmt = "#,##0"),
             rows = 2:(nrow(sales_by_fuel)+1),
             cols = col, gridExpand = TRUE)
  }
}


# Use results$retired_by_fuel directly - it already has engine_size and engine_size_liters
addWorksheet(wb, "Retired_by_Fuel")
writeData(wb, "Retired_by_Fuel", results$retired_by_fuel)

# Format the Retired column (column 6 in results$retired_by_fuel)
addStyle(wb, "Retired_by_Fuel",
         style = createStyle(numFmt = "#,##0"),
         rows = 2:(nrow(results$retired_by_fuel)+1),
         cols = 6, gridExpand = TRUE)
# ============================================
# SHEET 7: Complete Fleet Composition (Age × Fuel × Year) WITH ENGINE SIZE and MILEAGE
# ============================================

# # Get all fleet data with engine size columns
# Get all fleet data with Region and Origin included
# fleet_all <- bind_rows(results$fleet) %>%
#   select(Year, Vehicle_Age, Fuel_Group, Fleet, engine_size, engine_size_liters, Region, Origin) %>%
#   arrange(Year, Fuel_Group, Vehicle_Age, Region, Origin)


# Use fleet_all from your mileage model (already created)
addWorksheet(wb, "Fleet_Composition_Full")
writeData(wb, "Fleet_Composition_Full", fleet_all)

# Format numbers - Fleet column (col 4)
addStyle(wb, "Fleet_Composition_Full",
         style = createStyle(numFmt = "#,##0"),
         rows = 2:(nrow(fleet_all)+1),
         cols = 4, gridExpand = TRUE)

# Format mileage columns (col 7 and 8 - Final_Mileage and Predicted_Mileage)
addStyle(wb, "Fleet_Composition_Full",
         style = createStyle(numFmt = "#,##0"),
         rows = 2:(nrow(fleet_all)+1),
         cols = 7:8, gridExpand = TRUE)

# Optional: Add column headers formatting
addStyle(wb, "Fleet_Composition_Full",
         style = createStyle(textDecoration = "bold", fontSize = 12),
         rows = 1, cols = 1:8, gridExpand = TRUE)

# ============================================
# SHEET 8: Summary Statistics
# ============================================

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
      paste0(format(round(Value/1e6, 2), nsmall=2), "M"),
      format(round(Value, 0), big.mark=",")
    )
  )

addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_stats)

# ============================================
# SHEET 9: Accounting Summary
# ============================================

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

# ============================================
# SHEET 10: Retirement Rates Summary (Average by Fuel)
# ============================================

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
# Format percentages
for(col in 2:ncol(retirement_summary)) {
  addStyle(wb, "Retirement_Summary",
           style = createStyle(numFmt = "0.00%"),
           rows = 2:(nrow(retirement_summary)+1),
           cols = col, gridExpand = TRUE)
}
# ============================================
# SHEET 11: Origin Scenarios by Fuel Type
# ============================================

# Calculate origin scenarios (2021-2024 average)
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
  arrange(Fuel_Group, desc(Base))%>% select(Region,Fuel_Group,Base,Plus,Minus)
print(origin_scenarios, n=Inf)

addWorksheet(wb, "Origin_Scenarios")
writeData(wb, "Origin_Scenarios", origin_scenarios)
# Format as percentages
for(col in c("Base", "Plus", "Minus")) {
  addStyle(wb, "Origin_Scenarios",
           style = createStyle(numFmt = "0.0"),
           rows = 2:(nrow(origin_scenarios)+1),
           cols = which(names(origin_scenarios) == col),
           gridExpand = TRUE)
}

# ============================================
# SHEET 12: Origin Distribution by Year (Base Scenario)
# ============================================

# Apply origin scenarios to results
# apply_origin_scenario <- function(results, origin_scenarios, scenario_name = "Base") {
#   sales_by_fuel <- results$sales_by_fuel
#   sales_with_origin <- sales_by_fuel %>%
#     left_join(
#       origin_scenarios %>%
#         distinct(Fuel_Group, Region, .keep_all = TRUE) %>%
#         select(Fuel_Group, Region, Share = !!sym(scenario_name)),
#       by = "Fuel_Group",
#       relationship = "many-to-many"
#     ) %>%
#     mutate(
#       Vehicles_By_Origin = Required_Sales * Share / 100,
#       Scenario = scenario_name
#     )
#   return(sales_with_origin)
# }
# 
# # Apply scenarios
# sales_base <- apply_origin_scenario(results, origin_scenarios, "Base")
# sales_plus <- apply_origin_scenario(results, origin_scenarios, "Plus")
# sales_minus <- apply_origin_scenario(results, origin_scenarios, "Minus")
# all_origin_scenarios <- bind_rows(sales_base, sales_plus, sales_minus)

apply_origin_scenario <- function(results, origin_scenarios, scenario_name = "Base") {
  sales_by_fuel <- results$sales_by_fuel
  
  region_shares <- origin_scenarios %>%
    group_by(Fuel_Group, Region) %>%
    summarise(Share = sum(!!sym(scenario_name), na.rm = TRUE), .groups = "drop")
  
  sales_with_origin <- sales_by_fuel %>%          # <-- must start from sales_by_fuel
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


# Summarize
origin_summary <- all_origin_scenarios %>%
  group_by(Scenario, Year, Region) %>%
  summarise(Vehicles = sum(Vehicles_By_Origin, na.rm = TRUE), .groups = "drop") %>%
  group_by(Scenario, Year) %>%
  mutate(Share = round(100 * Vehicles / sum(Vehicles), 1)) %>%
  ungroup()

# Wide format for Base scenario
origin_wide_base <- origin_summary %>%
  filter(Scenario == "Base") %>%
  select(Year, Region, Share) %>%
  pivot_wider(names_from = Region, values_from = Share, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric) & !c(Year)), na.rm = TRUE))
print(origin_wide_base)

addWorksheet(wb, "Origin_By_Year_Base_newsales")
writeData(wb, "Origin_By_Year_Base_newsales", origin_wide_base)
# Format as percentages
for(col in 2:ncol(origin_wide_base)) {
  addStyle(wb, "Origin_By_Year_Base_newsales",
           style = createStyle(numFmt = "0.0"),
           rows = 2:(nrow(origin_wide_base)+1),
           cols = col, gridExpand = TRUE)
}



# ============================================
# SHEET 14: Origin Sales Detail (All Scenarios)
# ============================================


addWorksheet(wb, "Origin_Sales_Detail")
writeData(wb, "Origin_Sales_Detail", all_origin_scenarios)
# Format numbers
addStyle(wb, "Origin_Sales_Detail",
         style = createStyle(numFmt = "#,##0"),
         rows = 2:(nrow(all_origin_scenarios)+1),
         cols = which(names(all_origin_scenarios) %in% c("Required_Sales", "Vehicles_By_Origin")),
         gridExpand = TRUE)
# ============================================
# SHEET 15: Fleet Composition by Region
# ============================================
# Fleet by Origin — same idea
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

# Format numbers
for(col in 2:ncol(fleet_by_origin)) {
  if(col < ncol(fleet_by_origin)) {
    addStyle(wb, "fleet_by_Region",
             style = createStyle(numFmt = "#,##0"),
             rows = 2:(nrow(fleet_by_origin)+1),
             cols = col, gridExpand = TRUE)
  }
}



# ============================================
# SHEET 19: Region Share by Year (Percentage)
# ============================================

# Calculate region share percentages by year
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

# Format as percentages
for(col in 2:ncol(region_share_by_year)) {
  addStyle(wb, "Region_Share_By_Year",
           style = createStyle(numFmt = "0.0"),
           rows = 2:(nrow(region_share_by_year)+1),
           cols = col, gridExpand = TRUE)
}

# ============================================
# SHEET 20: Origin Sales Distribution by Year (from projection)
# ============================================

# Get the origin distribution used in the projection
origin_distribution_export <- origin_scenarios_year %>%
  filter(Scenario == "Base") %>%
  select(Year, Fuel_Group, Region,Origin, Origin_Share = Share_exact) %>%
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

# Format as percentages
for(col in 3:ncol(origin_distribution_export)) {
  addStyle(wb, "Origin_Distribution_Used",
           style = createStyle(numFmt = "0.0"),
           rows = 2:(nrow(origin_distribution_export)+1),
           cols = col, gridExpand = TRUE)
}

# ============================================
# Update the save section
# ============================================

# Set column widths for better readability (updated for 20 sheets)
for(sheet in seq_along(wb$worksheets)) {
  setColWidths(wb, sheet = sheet, cols = 1:16, widths = 15)
}

library(data.table)

# ============================================
# SAVE FLEET DATA AS GZIPPED CSV
# ============================================

output_dir <- "C:/Users/fzzb134/OneDrive - University of Leeds/Infuze/R-programming-Infuze/R-programming_Updated analysis/New-updated_data/Excell_files"

# Define the full file path for the final message
csv_gz_path <- file.path(output_dir, "Fleet_Composition_Full.csv.gz")

# Write directly to .csv.gz (compressed CSV)
fwrite(fleet_all, 
       csv_gz_path,
       row.names = FALSE,
       sep = ",", 
       dec = ".",
       nThread = 4,
       verbose = FALSE,
       showProgress = FALSE,
       scipen = 999,
       compress = "gzip")  # This creates .gz file automatically

cat("File created: Fleet_Composition_Full.csv.gz\n")

# To read it back:
fleet_data <- fread(csv_gz_path)

# Check it worked
cat("Rows in saved file:", nrow(fleet_data), "\n")
cat("Columns in saved file:", ncol(fleet_data), "\n")
head(fleet_data)

# Final message
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





# ============================================
# FIGURES: VEHICLE ORIGIN AND REGION ANALYSIS
# ============================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)
library(viridis)


# -------------------------------------------------------------------
# FIGURE 2: BEV Origin Breakdown (2024)
# -------------------------------------------------------------------

# Top origins for BEV (group smaller origins)
# Aggregate BEV origin by Region only
bev_origin_top <- bev_country_2024 %>%
  # Combine USA with Other (rename USA to Other)
  mutate(Region = ifelse(Region %in% c("USA", "Other"), "Other", Region)) %>%
  group_by(Region) %>%
  summarise(Share = sum(Share, na.rm = TRUE), .groups = "drop") %>%
  
  arrange(desc(Share))

# Create color palette for regions
bev_colors <- c(
  "China" = "#D62728",      # Red
  "EU" = "#1F77B4",         # Blue
  "UK" = "#FF7F0E",         # Orange
  "Korea" = "#9467BD",      # Purple
  "Japan" = "#2CA02C",      # Green
  "Other" = "#8C564B"         # Brown
)

# Assign colors to groups - FIXED: using bev_colors (not bev_region_colors)
bev_origin_top <- bev_origin_top %>%
  mutate(Color = bev_colors[Region])

# Create the plot
p_bev_origin <- ggplot(bev_origin_top, aes(x = reorder(Region, Share), y = Share, fill = Region)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.9) +
  geom_text(aes(label = paste0(round(Share, 1), "%")), 
            hjust = -0.1, size = 5, fontface = "bold") +
  scale_fill_manual(values = setNames(bev_origin_top$Color, bev_origin_top$Region)) +
  coord_flip() +
  labs(
    # title = "B) Origin of Battery Electric Vehicles by Region (2024)",
    # subtitle = "China leads at 24.3%, EU combined accounts for ~46%",
    x = "Manufacturing Region",
    y = "Share of BEV Fleet (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10))

print(p_bev_origin)

# -------------------------------------------------------------------
# FIGURE 3: China's BEV Share Trend (2014-2024)
# -------------------------------------------------------------------

# Extract China BEV trend
china_trend <- bev_new_region %>%
  filter(Region == "China") %>%
  select(Year = Data_Year, Share)

# Add EU and UK for comparison
region_trend <- bev_new_region %>%
  # filter(Data_Year >= 2019) %>%
  # Combine smaller regions into "Other"
  mutate(Region = ifelse(Region %in% c("EU", "UK", "China", "Japan", "Korea"), 
                         Region, "Other")) %>%
  group_by(Data_Year, Region) %>%
  summarise(
    Share = sum(Share, na.rm = TRUE),
    Reg = sum(Reg, na.rm = TRUE),  # Keep Reg for totals
    .groups = "drop"
  )

print(region_trend, n = Inf)

check_sums <- region_trend%>%
  group_by(Data_Year) %>%
  summarise(Total = sum(Share, na.rm = TRUE), .groups = "drop")
print(check_sums)



# Calculate yearly totals (your version)
yearly_total <- region_trend %>%
  #filter(Data_Year >= 2019) %>%
  group_by(Data_Year) %>%
  summarise(
    Total_Reg = sum(Reg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Total_Label = ifelse(
      Total_Reg >= 1000,
      paste0(round(Total_Reg / 1000, 0), "k"),
      paste0(round(Total_Reg, 0))
    )
  )

table <- region_trend %>%
  group_by(Data_Year, Region) %>%
  summarise(
    Total_Reg = sum(Reg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = Data_Year,           # One row per year
    names_from = Region,
    values_from = Total_Reg,
    values_fill = 0                # Fill missing with 0
  ) %>%
  mutate(
    Total = rowSums(across(where(is.numeric)), na.rm = TRUE),
    Total_Label = ifelse(
      Total >= 1000000,
      paste0(round(Total / 1000000, 1), "M"),
      paste0(round(Total / 1000, 0), "k")
    )
  ) %>%
  select(Data_Year, China, EU, UK, Japan, Korea, Other, Total, Total_Label)

print(table, n = Inf)

p_stacked_counts <- ggplot(region_trend, aes(x = factor(Data_Year), y = Share, fill = Region)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7, alpha = 0.9) +
  # Add percentage labels inside bars
  geom_text(aes(label = ifelse(Share > 5, paste0(round(Share, 1), "%"), "")),
            position = position_fill(vjust = 0.5), 
            size = 3.5, color = "white", fontface = "bold") +
  # Add total registration counts above bars
  geom_text(data = yearly_total,
            aes(x = factor(Data_Year), y = 1.05, label = Total_Label),
            inherit.aes = FALSE, size = 3.5, color = "black", fontface = "bold") +
  scale_fill_manual(values = c(
    "China" = "#D62728",
    "EU" = "#1F77B4",
    "UK" = "#FF7F0E",
    "Japan" = "#2CA02C",
    "Korea" = "#9467BD",
    "Other" = "#8C564B"         # Brown
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     breaks = seq(0, 1, 0.2)) +
  labs(
    # title = "C) New BEV Registrations by Manufacturing Region (2019-2024)",
    #subtitle = "Numbers above bars show total BEV registrations (thousands)",
    x = "Year",
    y = "Share of New BEV Registrations (%)",
    fill = "Manufacturing Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
    axis.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p_stacked_counts)



# -------------------------------------------------------------------
# FIGURE 5: Stacked Area - Origin Evolution (2014-2024)
# -------------------------------------------------------------------

# Prepare stacked area data for new registrations
stacked_data <- data %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year, Region) %>%
  summarise(Reg = sum(Fleet_Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Data_Year) %>%
  mutate(Share = 100 * Reg / sum(Reg)) %>%
  ungroup()
print(stacked_data)

yearly_totals <- stacked_data %>%
  group_by(Data_Year) %>%
  summarise(
    Total_Reg = sum(Reg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Simple formatting: millions with 1 decimal, or thousands with k
    Total_Label = ifelse(
      Total_Reg >= 1000000,
      paste0(round(Total_Reg / 1000000, 3), "M"),
      paste0(round(Total_Reg / 1000, 0), "k")
    )
  )

print(yearly_totals)

# Filter to key regions
stacked_data <- stacked_data %>%
  mutate(Region = ifelse(Region %in% c("EU", "UK", "China", "Japan", "Korea"), 
                         Region, "Other")) 

p_stacked <- ggplot(stacked_data, aes(x = factor(Data_Year), y = Share, fill = Region)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7, alpha = 0.9) +
  # Add percentage labels inside bars
  geom_text(aes(label = ifelse(Share > 5, paste0(round(Share, 1), "%"), "")),
            position = position_fill(vjust = 0.5), 
            size = 3.5, color = "white", fontface = "bold") +
  # Add total registration counts above bars
  geom_text(data = yearly_totals,
            aes(x = factor(Data_Year), y = 1.05, label = Total_Label),
            inherit.aes = FALSE, size = 3.5, color = "black", fontface = "bold") +
  scale_fill_manual(values = c(
    "China" = "#D62728",
    "EU" = "#1F77B4",
    "UK" = "#FF7F0E",
    "Japan" = "#2CA02C",
    "Korea" = "#9467BD",
    "Other" = "#8C564B"         # Brown
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     breaks = seq(0, 1, 0.2)) +
  labs(
    #title = "New Vehicle Registrations by Manufacturing Region (2014-2024)",
    #subtitle = "Numbers above bars show total BEV registrations (thousands)",
    x = "Year",
    y = "Share of New Vehicle Registrations (%)",
    fill = "Manufacturing Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
    axis.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p_stacked)








# -------------------------------------------------------------------
# FIGURE 9: Fuel Mix by Region (2024)
# -------------------------------------------------------------------

# Prepare fuel mix data
fuel_mix_2024 <- data %>%
  filter(Data_Year == 2024) %>%
  mutate(Region = ifelse(Region %in% c("EU", "UK", "China", "Japan", "Korea"), 
                         Region, "Other")) %>%
  group_by(Region, Fuel_Group) %>%
  summarise(Vehicles = sum(Fleet_Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Region) %>%
  mutate(Share = 100 * Vehicles / sum(Vehicles)) %>%
  filter(Region %in% c("EU", "China", "UK", "Japan", "Korea","Other"))

p_fuel_mix <- ggplot(fuel_mix_2024, aes(x = Region, y = Share, fill = Fuel_Group)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6, alpha = 0.85) +
  geom_text(aes(label = ifelse(Share > 5, paste0(round(Share, 1), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3.5, color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "Set1", name = "Fuel Type") +
  labs(
    #title = "Fuel Mix by Manufacturing Region (2024)",
    #subtitle = "Petrol and Diesel dominate in all regions, but EU has higher HEV/PHEV shares",
    x = "Manufacturing Region",
    y = "Share of Vehicles (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%"))

print(p_fuel_mix)




#-----------------------------------------
# Adding uncertainty/sensitivity analysis
#-----------------------------------------
run_scenario <- function(growth_mult = 1, max_age = 30, zev_scenario = "baseline") {
  zev_scenario <- as.character(zev_scenario)
  
  # FIXED: Removed the extra 'zev_scenario' at the end
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
  
  # Extract 2037 results with fuel-mix-sensitive metrics
  fleet_2037 <- bind_rows(res$fleet) %>% filter(Year == 2037)
  
  # CORRECTED: Use the actual column name 'Required_Sales'
  total_sales <- sum(res$sales$Required_Sales, na.rm = TRUE)
  
  fleet_2037 %>%
    summarise(
      # Total fleet (structurally fixed by NATCOP - won't vary much)
      Fleet_2037 = sum(Fleet),
      
      # Average age (diluted by 2024 base - weak signal)
      Avg_Age_2037 = sum(Vehicle_Age * Fleet) / sum(Fleet),
      
      # PRIMARY METRICS - these will actually show ZEV scenario effects
      BEV_Share_2037 = sum(Fleet[Fuel_Group == "BEV"]) / sum(Fleet),
      Fossil_Fleet_2037 = sum(Fleet[Fuel_Group %in% c("Petrol", "Diesel")]),
      
      # Additional useful metric for manufacturing/embodied carbon trade-off
      Total_Sales_2025_2037 = total_sales
    )
}

# Test it runs
test_result <- run_scenario()
print(test_result)

# Build parameter grid
param_grid <- expand.grid(
  growth_mult   = c(0.9, 1.0, 1.1),      # NATCOP growth ±10%
  max_age       = c(20,25, 30),         # retirement cutoff
  zev_scenario  = c("baseline", "mild", "medium"),
  stringsAsFactors = FALSE
)

# Run sensitivity analysis
sensitivity_results <- param_grid %>%
  rowwise() %>%
  mutate(out = list(run_scenario(growth_mult, max_age, zev_scenario))) %>%
  unnest(out)

print(sensitivity_results)

# Verify the fix worked
print(unique(sensitivity_results$zev_scenario))
print(class(sensitivity_results$zev_scenario))

# Visualization - now showing metrics that actually respond to ZEV scenarios
library(ggplot2)
library(tidyr)

# Reshape for faceted visualization
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
    # title = "Sensitivity Analysis: Key Fleet Metrics in 2037",
    #subtitle = "Note: BEV_Share and Fossil_Fleet show clear ZEV scenario effects",
    x = "NATCOP Growth Multiplier",
    y = "Metric Value",
    color = "Max Age (years)",
    shape = "ZEV Scenario"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Summary statistics by ZEV scenario
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

# Additional analysis: Interaction effects
sensitivity_results %>%
  group_by(growth_mult, max_age) %>%
  summarise(
    BEV_Spread = max(BEV_Share_2037) - min(BEV_Share_2037),
    Fossil_Spread = max(Fossil_Fleet_2037) - min(Fossil_Fleet_2037),
    .groups = 'drop'
  ) %>%
  print()

# Correlation matrix
cor(sensitivity_results[, c("growth_mult", "max_age", "BEV_Share_2037", 
                            "Fossil_Fleet_2037", "Avg_Age_2037", 
                            "Total_Sales_2025_2037")], 
    use = "complete.obs") %>%
  round(3) %>%
  print()




#---------------------------------------------------
# Strengthening validation with a genuine back-test
# FIXED: Properly maps historical data to internal year labels
#---------------------------------------------------

# Calibrate using only data through 2019
retirement_backtest <- survival_by_year_all %>%
  filter(Data_Year %in% 2017:2019, Vehicle_Age <= 30) %>%
  group_by(Fuel_Group, Vehicle_Age) %>%
  summarise(Retirement_Rate = mean(1 - Annual_Survival, na.rm = TRUE),
            Survival_Rate   = mean(Annual_Survival, na.rm = TRUE), .groups = "drop")

# Base fleet is 2019
fleet_2019 <- fleet_complete %>% filter(Data_Year == 2019, Vehicle_Age <= 30)

# Historical fuel shares for 2020-2024 (actual data)
fuel_shares_historical <- data %>%
  filter(Data_Year %in% 2020:2024, Vehicle_Age == 0) %>%
  group_by(Data_Year, Fuel_Group) %>%
  summarise(n = sum(Fleet_Count), .groups = "drop") %>%
  group_by(Data_Year) %>%
  mutate(sale_percentage = n / sum(n)) %>%
  rename(Year = Data_Year) %>%
  ungroup()

# Historical NATCOP targets for 2020-2024 (actual data)
natcop_historical <- data %>% 
  filter(Data_Year %in% 2020:2024) %>%
  group_by(Data_Year) %>% 
  summarise(Target_Fleet = sum(Fleet_Count)) %>%
  rename(Year = Data_Year) %>%
  ungroup()



# Shift fuel shares
fuel_shares_shifted <- fuel_shares_historical %>%
  mutate(
    # Shift Year: 2020→2025, 2021→2026, ..., 2024→2029
    Year_Shifted = Year + 5
  ) %>%
  select(Year = Year_Shifted, Fuel_Group, sale_percentage)

# Shift NATCOP targets
natcop_shifted <- natcop_historical %>%
  mutate(
    Year_Shifted = Year + 5
  ) %>%
  select(Year = Year_Shifted, Target_Fleet)

# Now extend to 2037 using 2024 (now at 2029) as projection
# We have shifted data for 2025-2029 (originally 2020-2024)
# Need to fill 2030-2037

# Get the last shifted year's data (originally 2024 → shifted to 2029)
last_shifted_fuel <- fuel_shares_shifted %>% 
  filter(Year == 2029) %>%
  select(Fuel_Group, sale_percentage)

last_shifted_natcop <- natcop_shifted %>% 
  filter(Year == 2029) %>%
  pull(Target_Fleet)

# Extend fuel shares for 2030-2037 using 2029 data
fuel_shares_extended <- bind_rows(
  fuel_shares_shifted,
  expand.grid(Year = 2030:2037, Fuel_Group = unique(last_shifted_fuel$Fuel_Group)) %>%
    left_join(last_shifted_fuel, by = "Fuel_Group") %>%
    mutate(sale_percentage = ifelse(is.na(sale_percentage), 0.2, sale_percentage))
)

# Extend NATCOP targets for 2030-2037
natcop_extended <- bind_rows(
  natcop_shifted,
  tibble(
    Year = 2030:2037,
    Target_Fleet = last_shifted_natcop
  )
)

# Now run the backtest
# IMPORTANT: The base fleet (2019) will be labeled as 2024 internally
backtest <- project_fleet(
  base_fleet = fleet_2019,
  retirement_rates = retirement_backtest,
  fuel_shares = fuel_shares_extended,
  natcop_targets = natcop_extended,
  engine_sales_distribution = engine_sales_distribution
)

# Extract results for validation
if (!is.null(backtest)) {
  
  # The internal year 2029 corresponds to actual 2024
  # (because 2019 → 2024 is a 5-year shift)
  
  # Compare predicted 2024 (internal year 2029) vs actual 2024
  predicted_2024 <- bind_rows(backtest$fleet) %>% 
    filter(Year == 2029) %>%  # 2029 internal = 2024 actual
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
  cat("BACKTEST RESULTS (2019 → 2024):\n")
  cat("========================================\n")
  print(comparison)

  
  # Enhanced visualization
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
      #title = "Backtest Validation: Predicted vs Actual 2024 Fleet",
      # subtitle = paste0(
      #   "Model: 2017-2019 calibration → 2020-2024 forecast | ",
      #   "Overall Error: ", round(100 * (total_predicted - total_actual) / total_actual, 2), "%"
      #),
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


# Enhanced visualization WITHOUT percentage labels
comparison %>%
  pivot_longer(cols = c(Predicted, Actual), names_to = "Source", values_to = "Count") %>%
  ggplot(aes(x = Fuel_Group, y = Count, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = format(round(Count), big.mark = ",")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  # REMOVED: geom_text for error labels
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




# ============================================
# EXTRACT REAL MILEAGE PATTERNS FROM YOUR DATA
# ============================================

# 1. Get actual mileage by age, fuel, and size class
real_mileage_by_age_size <- fleet_2024_complete %>%
  filter(!is.na(Avg_Mileage_Calibrated), !is.na(vehicle_class), Vehicle_Age <= 25) %>%
  group_by(Fuel_Group, vehicle_class, Vehicle_Age) %>%
  summarise(
    Avg_Mileage = weighted.mean(Avg_Mileage_Calibrated, Fleet_Count, na.rm = TRUE),
    Total_Fleet = sum(Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Fuel_Group, vehicle_class, Vehicle_Age)

# View the results
print("=== REAL MILEAGE BY FUEL, SIZE, AND AGE ===")
print(real_mileage_by_age_size, n = Inf)


