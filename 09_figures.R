# ============================================================================
# 09_figures.R
# ----------------------------------------------------------------------------
# Consolidated figure generation. All paper figures in one place.
# Collects the figures produced across 01-07 and adds the origin/regional
# fleet composition figures.
#
# Depends on: 00-08 (all prior scripts)
# Produces  : paper figures, dashboard composites
# ============================================================================

library(tidyverse)
library(patchwork)
library(scales)
library(viridis)
library(ggplot2)

# ============================================================================
# FIGURE: BEV Origin Breakdown (2024)
# ============================================================================
bev_origin_top <- bev_country_2024 %>%
  mutate(Region = ifelse(Region %in% c("USA", "Other"), "Other", Region)) %>%
  group_by(Region) %>%
  summarise(Share = sum(Share, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Share))

bev_colors <- c(
  "China" = "#D62728",
  "EU" = "#1F77B4",
  "UK" = "#FF7F0E",
  "Korea" = "#9467BD",
  "Japan" = "#2CA02C",
  "Other" = "#8C564B"
)

bev_origin_top <- bev_origin_top %>%
  mutate(Color = bev_colors[Region])

p_bev_origin <- ggplot(bev_origin_top, aes(x = reorder(Region, Share), y = Share, fill = Region)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.9) +
  geom_text(aes(label = paste0(round(Share, 1), "%")),
            hjust = -0.1, size = 5, fontface = "bold") +
  scale_fill_manual(values = setNames(bev_origin_top$Color, bev_origin_top$Region)) +
  coord_flip() +
  labs(
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

# ============================================================================
# FIGURE: China BEV Share Trend (2014-2024)
# ============================================================================
china_trend <- bev_new_region %>%
  filter(Region == "China") %>%
  select(Year = Data_Year, Share)

region_trend <- bev_new_region %>%
  mutate(Region = ifelse(Region %in% c("EU", "UK", "China", "Japan", "Korea"),
                         Region, "Other")) %>%
  group_by(Data_Year, Region) %>%
  summarise(
    Share = sum(Share, na.rm = TRUE),
    Reg = sum(Reg, na.rm = TRUE),
    .groups = "drop"
  )

print(region_trend, n = Inf)

check_sums <- region_trend %>%
  group_by(Data_Year) %>%
  summarise(Total = sum(Share, na.rm = TRUE), .groups = "drop")
print(check_sums)

yearly_total <- region_trend %>%
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
    id_cols = Data_Year,
    names_from = Region,
    values_from = Total_Reg,
    values_fill = 0
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
  geom_text(aes(label = ifelse(Share > 5, paste0(round(Share, 1), "%"), "")),
            position = position_fill(vjust = 0.5),
            size = 3.5, color = "white", fontface = "bold") +
  geom_text(data = yearly_total,
            aes(x = factor(Data_Year), y = 1.05, label = Total_Label),
            inherit.aes = FALSE, size = 3.5, color = "black", fontface = "bold") +
  scale_fill_manual(values = c(
    "China" = "#D62728",
    "EU" = "#1F77B4",
    "UK" = "#FF7F0E",
    "Japan" = "#2CA02C",
    "Korea" = "#9467BD",
    "Other" = "#8C564B"
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.2)) +
  labs(
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

# ============================================================================
# FIGURE: Stacked Area - Origin Evolution (2014-2024)
# ============================================================================
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
    Total_Label = ifelse(
      Total_Reg >= 1000000,
      paste0(round(Total_Reg / 1000000, 3), "M"),
      paste0(round(Total_Reg / 1000, 0), "k")
    )
  )

print(yearly_totals)

stacked_data <- stacked_data %>%
  mutate(Region = ifelse(Region %in% c("EU", "UK", "China", "Japan", "Korea"),
                         Region, "Other"))

p_stacked <- ggplot(stacked_data, aes(x = factor(Data_Year), y = Share, fill = Region)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7, alpha = 0.9) +
  geom_text(aes(label = ifelse(Share > 5, paste0(round(Share, 1), "%"), "")),
            position = position_fill(vjust = 0.5),
            size = 3.5, color = "white", fontface = "bold") +
  geom_text(data = yearly_totals,
            aes(x = factor(Data_Year), y = 1.05, label = Total_Label),
            inherit.aes = FALSE, size = 3.5, color = "black", fontface = "bold") +
  scale_fill_manual(values = c(
    "China" = "#D62728",
    "EU" = "#1F77B4",
    "UK" = "#FF7F0E",
    "Japan" = "#2CA02C",
    "Korea" = "#9467BD",
    "Other" = "#8C564B"
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.2)) +
  labs(
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

# ============================================================================
# FIGURE: Fuel Mix by Region (2024)
# ============================================================================
fuel_mix_2024 <- fleet_complete %>%
  filter(Data_Year == 2024) %>%
  mutate(Region = ifelse(Region %in% c("EU", "UK", "China", "Japan", "Korea"),
                         Region, "Other")) %>%
  group_by(Region, Fuel_Group) %>%
  summarise(Vehicles = sum(Fleet_Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Region) %>%
  mutate(Share = 100 * Vehicles / sum(Vehicles)) %>%
  filter(Region %in% c("EU", "China", "UK", "Japan", "Korea", "Other"))

p_fuel_mix <- ggplot(fuel_mix_2024, aes(x = Region, y = Share, fill = Fuel_Group)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6, alpha = 0.85) +
  geom_text(aes(label = ifelse(Share > 5, paste0(round(Share, 1), "%"), "")),
            position = position_stack(vjust = 0.5), size = 3.5, color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "Set1", name = "Fuel Type") +
  labs(
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

# ============================================================================
# UPDATED COMBINED SCENARIO FIGURE (new scenario names)
# ============================================================================
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

bev_scenario4 <- bev_comparison %>%
  filter(Scenario == "Baseline") %>%
  mutate(Scenario = "Slow sales")

bev_comparison_all <- bind_rows(bev_comparison, bev_scenario4)

bev_long <- bev_comparison_all %>%
  mutate(
    Year = as.numeric(Year),
    BEV_Share = as.numeric(BEV_Share),
    Metric = "BEV Share (%)"
  ) %>%
  rename(Value = BEV_Share)

sales_baseline <- results_baseline$sales_by_fuel %>%
  filter(Fuel_Group %in% c("BEV")) %>%
  group_by(Year) %>%
  summarise(New_Sales = sum(Required_Sales, na.rm = TRUE)) %>%
  mutate(
    Scenario = "Baseline",
    New_Sales_M = New_Sales / 1e6,
    Metric = "New Sales (M vehicles)"
  )

sales_mild <- results_70pct$sales_by_fuel %>%
  filter(Fuel_Group %in% c("BEV")) %>%
  group_by(Year) %>%
  summarise(New_Sales = sum(Required_Sales, na.rm = TRUE)) %>%
  mutate(
    Scenario = "Mild ZEV Rowback",
    New_Sales_M = New_Sales / 1e6,
    Metric = "New Sales (M vehicles)"
  )

sales_medium <- results_60pct$sales_by_fuel %>%
  filter(Fuel_Group %in% c("BEV")) %>%
  group_by(Year) %>%
  summarise(New_Sales = sum(Required_Sales, na.rm = TRUE)) %>%
  mutate(
    Scenario = "Medium ZEV Rowback",
    New_Sales_M = New_Sales / 1e6,
    Metric = "New Sales (M vehicles)"
  )

sales_slow <- results_scenario4$sales_by_fuel %>%
  filter(Fuel_Group %in% c("BEV")) %>%
  group_by(Year) %>%
  summarise(New_Sales = sum(Required_Sales, na.rm = TRUE)) %>%
  mutate(
    Scenario = "Slow sales",
    New_Sales_M = New_Sales / 1e6,
    Metric = "New Sales (M vehicles)"
  )

sales_long <- bind_rows(sales_baseline, sales_mild, sales_medium, sales_slow) %>%
  select(Year, Scenario, Value = New_Sales_M, Metric)

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

age_baseline <- get_age_data(results_baseline$fleet, "Baseline")
age_mild <- get_age_data(results_70pct$fleet, "Mild ZEV Rowback")
age_medium <- get_age_data(results_60pct$fleet, "Medium ZEV Rowback")
age_slow <- get_age_data(results_scenario4$fleet, "Slow sales")

age_long <- bind_rows(age_baseline, age_mild, age_medium, age_slow)

all_metrics <- bind_rows(
  bev_long %>% select(Year, Scenario, Value, Metric),
  sales_long,
  age_long
) %>%
  mutate(
    Scenario = factor(Scenario, levels = c("Baseline", "Mild ZEV Rowback", "Medium ZEV Rowback", "Slow sales")),
    Metric = factor(Metric, levels = c("BEV Share (%)", "New Sales (M vehicles)", "Avg Fleet Age (years)"))
  )

scenario_colors <- c(
  "Baseline" = "#2E86AB",
  "Mild ZEV Rowback" = "#F18F01",
  "Medium ZEV Rowback" = "#C73E1D",
  "Slow sales" = "#4DAF4A"
)

scenario_shapes <- c(
  "Baseline" = 16,
  "Mild ZEV Rowback" = 17,
  "Medium ZEV Rowback" = 15,
  "Slow sales" = 18
)

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

combined_figure <- (p1 | p2) / p3 +
  plot_annotation() +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(combined_figure)

# ============================================================================
# Summary table
# ============================================================================
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
# Age disparity table by fuel type and scenario
# ============================================================================
calculate_avg_age <- function(results, scenario_name) {
  bind_rows(results$fleet) %>%
    filter(Year == 2035) %>%
    group_by(Fuel_Group) %>%
    summarise(
      Avg_Age = sum(Vehicle_Age * Fleet, na.rm = TRUE) / sum(Fleet, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Scenario = scenario_name)
}

age_baseline <- calculate_avg_age(results_baseline, "Baseline")
age_mild <- calculate_avg_age(results_70pct, "Mild ZEV Rowback")
age_medium <- calculate_avg_age(results_60pct, "Medium ZEV Rowback")
age_slow <- calculate_avg_age(results_scenario4, "Slow sales")

all_ages <- bind_rows(age_baseline, age_mild, age_medium, age_slow)

age_table_wide <- all_ages %>%
  mutate(Avg_Age = round(Avg_Age, 1)) %>%
  pivot_wider(
    id_cols = Fuel_Group,
    names_from = Scenario,
    values_from = Avg_Age
  ) %>%
  mutate(Fuel_Group = factor(Fuel_Group,
                             levels = c("BEV", "PHEV", "HEV", "Petrol", "Diesel"))) %>%
  arrange(Fuel_Group) %>%
  mutate(
    Insight = case_when(
      Fuel_Group == "BEV" ~ "New technology, young fleet",
      Fuel_Group == "PHEV" ~ "Getting younger in faster ZEV scenarios",
      Fuel_Group == "HEV" ~ "Mature technology, stable age",
      Fuel_Group == "Petrol" ~ "Aging fleet",
      Fuel_Group == "Diesel" ~ "Critically old fleet",
      TRUE ~ ""
    )
  ) %>% print()

cat("\n", strrep("=", 100), "\n")
cat("AGE DISPARITY BY FUEL TYPE AND SCENARIO (2035)")
cat("\n", strrep("=", 100), "\n")
print(age_table_wide)

if(require(kableExtra, quietly = TRUE)) {
  age_table_formatted <- age_table_wide %>%
    select(Fuel_Group, Baseline, `Mild ZEV Rowback`, `Medium ZEV Rowback`, `Slow sales`, Insight)

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
  print(age_table_formatted)
}

# Variability across scenarios
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

# Age comparison plot
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

# Export
write.csv(age_table_wide,
          file.path(out_dir, "Age_Disparity_Table.csv"),
          row.names = FALSE)

write.csv(age_differences,
          file.path(out_dir, "Age_Differences_from_Baseline.csv"),
          row.names = FALSE)

# Summary for publication
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

write.csv(summary_for_publication,
          file.path(out_dir, "Age_Disparity_Summary_for_Publication.csv"),
          row.names = FALSE)

# Trend plot
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

# ============================================================================
# CCC car emissions vs target
# ============================================================================
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

car_emissions$Scenario <- factor(car_emissions$Scenario,
                                 levels = c("Baseline", "70% by 2030", "60% by 2030", "Scenario 4"),
                                 labels = c("Baseline", "Mild ZEV Rowback", "Medium ZEV Rowback", "Slow sales"))

ccc_car_allocation_2035 <- 19.3

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

# ============================================================================
# UK grid intensity BEV emissions
# ============================================================================
uk_grid_intensity <- 129  # gCO2/kWh
bev_efficiency_kwh_per_km <- 0.18
annual_km <- 11000

bev_annual_kwh <- bev_efficiency_kwh_per_km * annual_km
bev_annual_g <- bev_annual_kwh * uk_grid_intensity
bev_annual_kg <- bev_annual_g / 1000
bev_tonnes_per_vehicle <- bev_annual_kg / 1000

cat("\n=== UK GRID INTENSITY CALCULATION ===\n")
cat("Grid intensity:", uk_grid_intensity, "gCO2/kWh\n")
cat("BEV efficiency:", bev_efficiency_kwh_per_km * 100, "kWh/100km\n")
cat("Annual BEV electricity consumption:", bev_annual_kwh, "kWh\n")
cat("Annual BEV CO2 per vehicle:", round(bev_annual_kg, 0), "kg (", round(bev_tonnes_per_vehicle, 2), "tonnes)\n")

cat("\n=== 09_figures.R complete ===\n")
