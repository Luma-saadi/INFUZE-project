# ============================================================================
# 02_fleet_classification.R
# ----------------------------------------------------------------------------
# Fleet composition figures, age trend analysis, average-age-by-fuel,
# vehicle-class colour palettes, and the fuel-type composition/plots used
# throughout the paper.
#
# Depends on: 00_setup.R, 01_data_prep.R
# Produces  : fleet_composition_plot, avg_age_trend_plot, total_avg_age,
#             total_avg_age_plot, avg_age_by_fuel_plot, age_group_plot,
#             vehicle_class_colors, petrol_colors, diesel_colors,
#             hev_colors, phev_colors, bev_colors,
#             vehicle_class_levels, new_vehicle_class_colors,
#             new_vehicle_class_levels, fleet_composition_data,
#             summary_with_source, join_final_complete_summary,
#             join_final_complete_Stock_model, Stock_model_summary
# ============================================================================

# ============================================================================
# JOIN_FINAL_COMPLETE: summary, age, and vehicle origin attachment
# ============================================================================
# This script assumes join_final_complete exists from 01_data_prep.R.

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
# ============================================================================

library(dplyr); library(tidyr); library(stringr); library(ggplot2)

# ---------------------------------------------------------------------------
# 1. PREP
# ---------------------------------------------------------------------------

join_final_complete <- join_final_complete %>%
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

library(arrow)
write_parquet(join_final_complete, file.path(data_dir, "join_final.parquet"),
              compression = "zstd", compression_level = 22)

veh_long <- join_final_complete %>%
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
    make=="MIA" ~ "France",
    make=="FSO CARS" ~ "Poland",
    make=="YAMAHA" ~ "Japan",
    make=="THINK" ~ "Norway",
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

write_parquet(veh_origin, file.path(data_dir, "veh_origin.parquet"),
              compression = "zstd", compression_level = 22)

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
                        ACEA_2025_pct=c(63.4, 9.3, 9.5,5.4))

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

# Wavy/ribbon stacked area
p_wavy <- bev_new_region %>%
  filter(Region %in% c("EU","UK","China","Korea","Japan")) %>%
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

# ---------------------------------------------------------------------------
# 8. ATTACH ORIGIN TO JOIN_FINAL_COMPLETE
# ---------------------------------------------------------------------------
origin_lookup <- veh_origin %>%
  distinct(Make, GenModel, Fuel_Group, Build_Year, Origin, Region)

n_before <- nrow(join_final_complete)

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

cat("Rows before:", n_before, " after:", nrow(join_final_complete), "\n")
cp_report(join_final_complete, "Fleet_Count", "CP1: after origin join")

# ---------------------------------------------------------------------------
# 9. Stock model for 2024
# ---------------------------------------------------------------------------
join_final_complete_Stock_model <- join_final_complete %>%
  mutate(
    Data_Year = as.numeric(Data_Year),
    YearFirstUsed = as.numeric(YearFirstUsed),
    Vehicle_Age = ifelse(Data_Year >= YearFirstUsed,
                         Data_Year - YearFirstUsed,
                         NA)) %>%
  filter(Data_Year == 2024, !is.na(Fleet_Count) & Fleet_Count > 0)

Stock_model_summary <- join_final_complete_Stock_model %>%
  filter(!is.na(Vehicle_Age)) %>%
  group_by(Data_Year, Fuel, engine_size) %>%
  summarise(
    vehicle_count = sum(Fleet_Count, na.rm = TRUE),
    avg_vehicle_age = weighted.mean(Vehicle_Age, Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Data_Year)

cat("\nStock Model - Vehicle age distribution by Fuel (2024):\n")
print(Stock_model_summary)

# ---------------------------------------------------------------------------
# 10. Summary table with source
# ---------------------------------------------------------------------------
summary_with_source <- join_final_complete %>%
  mutate(
    Data_Year = as.numeric(Data_Year),
    YearFirstUsed = as.numeric(YearFirstUsed),
    Vehicle_Age = ifelse(Data_Year >= YearFirstUsed,
                         Data_Year - YearFirstUsed,
                         NA)
  ) %>%
  filter(!is.na(Fleet_Count) & Fleet_Count > 0,
         source == "df_VEH0124_NZ&df_VEH0124_AM") %>%
  group_by(Data_Year, Fuel, engine_size) %>%
  summarise(
    vehicle_count = sum(Fleet_Count, na.rm = TRUE),
    avg_vehicle_age = weighted.mean(Vehicle_Age, Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Data_Year)

cat("\nSummary by Data Year and Source:\n")
print(summary_with_source)

summary_with_source <- summary_with_source %>% filter(Data_Year == "2024")

join_final_complete <- join_final_complete %>%
  mutate(
    Data_Year = as.numeric(Data_Year),
    YearFirstUsed = as.numeric(YearFirstUsed),
    Vehicle_Age = ifelse(Data_Year >= YearFirstUsed,
                         Data_Year - YearFirstUsed,
                         NA)
  )

# ---------------------------------------------------------------------------
# 11. BEV fleet totals post-classification
# ---------------------------------------------------------------------------
Vcar_classified_BEV <- join_final_complete %>%
  filter(Fuel_Group == "BEV")
summary_BEV <- Vcar_classified_BEV %>%
  group_by(Data_Year) %>%
  summarise(
    vehicle_count = sum(Fleet_Count, na.rm = TRUE),
    avg_vehicle_age = weighted.mean(Vehicle_Age, Fleet_Count, na.rm = TRUE),
    .groups = "drop"
  )
print(summary_BEV)

# ============================================================================
# FLEET CLASSIFICATION & COMPOSITION FIGURES
# ============================================================================

cat("=== CHECKING EXISTING CLASSIFICATION IN Vcar_classified ===\n")
classification_summary <- Vcar_classified %>%
  count(Fuel_Group, vehicle_class) %>%
  arrange(Fuel_Group, vehicle_class)
print(classification_summary)

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

# ---------------------------------------------------------------------------
# Outlier analysis on join_final_complete (2024)
# ---------------------------------------------------------------------------
outlier_analysis_joint <- join_final_complete %>%
  filter(Data_Year == 2024) %>%
  mutate(
    engine_size_clean = ifelse(
      engine_size %in% c("[x]", "[z]", "N/A", "NA", "", NA) | is.na(engine_size),
      NA,
      engine_size
    ),
    engine_size_liters = as.numeric(engine_size_clean) / 1000
  ) %>%
  filter(!is.na(engine_size_liters)) %>%
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

cat("Current columns in join_final_complete:\n")
print(names(join_final_complete))

# ---------------------------------------------------------------------------
# Remove NA rows and non-classified fuel
# ---------------------------------------------------------------------------
library(tidyr)

clean <- join_final_complete %>%
  select(-any_of(c("outlier_min", "outlier_max", "custom_min", "custom_max")))
Fleet_complete <- clean
cp_report(Fleet_complete, "Fleet_Count", "CP2: Fleet_complete before kerb-weight join")

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

# ---------------------------------------------------------------------------
# STEP 0: Prepare known vehicles with all attributes
# ---------------------------------------------------------------------------
fleet_known <- Fleet_complete %>%
  filter(!is.na(Vehicle_Age), Fuel_Group != "Other") %>%
  mutate(
    engine_size = as.character(engine_size),
    vehicle_class = as.character(vehicle_class)
  ) %>%
  group_by(
    Vehicle_Age,
    Fuel,
    Fuel_Group,
    vehicle_class,
    engine_size,
    Data_Year
  ) %>%
  summarise(
    Fleet_Count = sum(Fleet_Count),
    .groups = "drop"
  )

cat("\nKnown vehicles total:", sum(fleet_known$Fleet_Count), "\n")
cat("2024 known vehicles:", sum(fleet_known$Fleet_Count[fleet_known$Data_Year == 2024]), "\n")

# ---------------------------------------------------------------------------
# STEP 1 — Allocate NA age vehicles with known fuel (YEAR-SPECIFIC)
# ---------------------------------------------------------------------------
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
  group_by(Fuel_Group, vehicle_class, engine_size, Data_Year) %>%
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
  select(Vehicle_Age, Fuel, Fuel_Group, vehicle_class, engine_size, Fleet_Count, Data_Year) %>%
  filter(!is.na(Fleet_Count) & Fleet_Count > 0)

cat("\nStep 1 total:", sum(step1_allocated$Fleet_Count), "\n")
cat("Step 1 2024 total:", sum(step1_allocated$Fleet_Count[step1_allocated$Data_Year == 2024]), "\n")

# ---------------------------------------------------------------------------
# STEP 2 — Allocate "Other" fuel vehicles with known age (YEAR-SPECIFIC)
# ---------------------------------------------------------------------------
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
  group_by(Vehicle_Age, Fuel_Group, vehicle_class, engine_size, Data_Year) %>%
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
  select(Vehicle_Age, Fuel, Fuel_Group, vehicle_class, engine_size, Fleet_Count, Data_Year) %>%
  filter(!is.na(Fleet_Count) & Fleet_Count > 0)

cat("\nStep 2 total:", sum(step2_allocated$Fleet_Count), "\n")
cat("Step 2 2024 total:", sum(step2_allocated$Fleet_Count[step2_allocated$Data_Year == 2024]), "\n")

# ---------------------------------------------------------------------------
# STEP 3 — Allocate completely unknown vehicles (NA age AND "Other" fuel)
# ---------------------------------------------------------------------------
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

class_engine_profile_overall <- fleet_known %>%
  group_by(Fuel_Group, vehicle_class, engine_size, Data_Year) %>%
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
        select(Vehicle_Age, Fuel, Fuel_Group, vehicle_class, engine_size, Fleet_Count, Data_Year) %>%
        filter(!is.na(Fleet_Count) & Fleet_Count > 0)

      step3_allocated_list[[as.character(year)]] <- step3_allocated_year
    }
  }

  step3_allocated <- bind_rows(step3_allocated_list)

  cat("\nStep 3 total:", sum(step3_allocated$Fleet_Count), "\n")
  cat("Step 3 2024 total:", sum(step3_allocated$Fleet_Count[step3_allocated$Data_Year == 2024]), "\n")

} else {
  step3_allocated <- data.frame(
    Vehicle_Age = integer(),
    Fuel = character(),
    Fuel_Group = character(),
    vehicle_class = character(),
    engine_size = character(),
    Fleet_Count = numeric(),
    Data_Year = integer()
  )
  cat("\nStep 3 total: 0 (no completely unknown vehicles)\n")
}

# ---------------------------------------------------------------------------
# Combine all parts into final dataset
# ---------------------------------------------------------------------------
final_complete <- bind_rows(
  fleet_known,
  step1_allocated,
  step2_allocated,
  step3_allocated
) %>%
  group_by(
    Vehicle_Age,
    Fuel,
    Fuel_Group,
    vehicle_class,
    engine_size,
    Data_Year
  ) %>%
  summarise(
    Fleet_Count = sum(Fleet_Count),
    .groups = "drop"
  ) %>%
  mutate(
    Fleet_Count = ifelse(is.na(Fleet_Count) | is.nan(Fleet_Count), 0, Fleet_Count),
    engine_size = as.character(engine_size),
    vehicle_class = as.character(vehicle_class)
  )

cat("\n--- FINAL VERIFICATION ---\n")
cat("Final total:", sum(final_complete$Fleet_Count), "\n")
cat("Original total:", sum(Fleet_complete$Fleet_Count, na.rm = TRUE), "\n\n")

sal_after <- final_complete %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year) %>%
  summarise(fleet = sum(Fleet_Count, na.rm = TRUE), .groups = "drop")

print("Sales AFTER imputation:")
print(sal_after)

# ---------------------------------------------------------------------------
# 2023 new sales vs 2024 age 1 comparison
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# Detailed 2024 sales analysis
# ---------------------------------------------------------------------------
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
# Final checks and dimensions addition
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

# ---------------------------------------------------------------------------
# Handle NA in engine sizes
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# Engine size distribution by fuel type (2024)
# ---------------------------------------------------------------------------
vehicle_size_range <- join_final_complete_clean %>%
  filter(Data_Year == "2024", !is.na(engine_size)) %>%
  mutate(
    size_range = case_when(
      engine_size >= 0 & engine_size < 500 ~ "0 - 499",
      engine_size >= 500 & engine_size < 1000 ~ "500 - 999",
      engine_size >= 1000 & engine_size < 1500 ~ "1000 - 1499",
      engine_size >= 1500 & engine_size < 2000 ~ "1500 - 1999",
      engine_size >= 2000 & engine_size < 2500 ~ "2000 - 2499",
      engine_size >= 2500 & engine_size < 3000 ~ "2500 - 2999",
      engine_size >= 3000 & engine_size < 3500 ~ "3000 - 3499",
      engine_size >= 3500 & engine_size < 4000 ~ "3500 - 3999",
      engine_size >= 4000 & engine_size < 4500 ~ "4000 - 4499",
      engine_size >= 4500 & engine_size < 5000 ~ "4500 - 4999",
      engine_size >= 5000 & engine_size < 5500 ~ "5000 - 5499",
      engine_size >= 5500 & engine_size < 6000 ~ "5500 - 5999",
      engine_size >= 6000 & engine_size < 6500 ~ "6000 - 6499",
      engine_size == 6500 ~ "6500",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(size_range, Fuel_Group) %>%
  summarise(
    Count = sum(Fleet_Count),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = size_range,
    names_from = Fuel_Group,
    values_from = Count,
    values_fill = 0
  ) %>%
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

# ---------------------------------------------------------------------------
# Vehicle class summary tables
# ---------------------------------------------------------------------------
library(stringr)

vehicle_summary <- join_final_complete_clean %>%
  filter(Data_Year == "2024") %>%
  mutate(
    size_category = case_when(
      str_detect(vehicle_class, "Very Large") ~ "Very Large",
      str_detect(vehicle_class, "Very") ~ "Very Large",
      str_detect(vehicle_class, "Large") & !str_detect(vehicle_class, "Very") ~ "Large",
      str_detect(vehicle_class, "Medium") ~ "Medium",
      str_detect(vehicle_class, "Small") ~ "Small",
      str_detect(vehicle_class, "Unsized") ~ "Unsized",
      TRUE ~ "Unclassified"
    )
  ) %>%
  group_by(size_category, Fuel_Group) %>%
  summarise(
    Count = sum(Fleet_Count, na.rm = TRUE),
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
  ) %>%
  select(size_category, Fuel_Group, Count, engine_size_range) %>%
  pivot_wider(
    id_cols = size_category,
    names_from = Fuel_Group,
    values_from = Count,
    values_fill = 0
  ) %>%
  mutate(size_category = factor(size_category,
                                levels = c("Small", "Medium", "Large", "Very Large", "Unsized", "Unclassified"))) %>%
  arrange(size_category)

print(vehicle_summary)

vehicle_summary_detailed <- join_final_complete_clean %>%
  filter(Data_Year == "2024") %>%
  mutate(
    size_category = case_when(
      str_detect(vehicle_class, "Very Large") ~ "Very Large",
      str_detect(vehicle_class, "Very") ~ "Very Large",
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
  mutate(Summary = paste0(Count, " (", Engine_Size_Range, ")")) %>%
  select(size_category, Fuel_Group, Summary) %>%
  pivot_wider(
    id_cols = size_category,
    names_from = Fuel_Group,
    values_from = Summary,
    values_fill = "0 (N/A)"
  ) %>%
  mutate(size_category = factor(size_category,
                                levels = c("Small", "Medium", "Large", "Very Large", "Unsized", "Unclassified"))) %>%
  arrange(size_category)

print(vehicle_summary_detailed, width = Inf)

size_order <- c("Small", "Medium", "Large", "Very Large", "Unsized", "Other", "Unclassified")

vehicle_summary <- vehicle_summary %>%
  mutate(size_category = factor(size_category, levels = size_order)) %>%
  arrange(size_category)

vehicle_summary_detailed <- vehicle_summary_detailed %>%
  mutate(size_category = factor(size_category, levels = size_order)) %>%
  arrange(size_category)

print(vehicle_summary)
print(vehicle_summary_detailed)

# ---------------------------------------------------------------------------
# Outlier join and fuel standardisation
# ---------------------------------------------------------------------------
join_final_complete_updated <- join_final_complete_clean %>%
  left_join(outlier_analysis_joint %>% select(Fuel, outlier_min, outlier_max),
            by = "Fuel")
cp_report(join_final_complete_updated, "Fleet_Count", "CP7: after outlier bounds join")

cat("\nUpdated columns in join_final_complete:\n")
print(names(join_final_complete_updated))

join_final_complete_updated <- join_final_complete_updated %>%
  mutate(
    Fuel = case_when(
      str_to_lower(Fuel) == "battery electric" ~ "Battery Electric",
      TRUE ~ Fuel
    ),
    Fuel_Group = case_when(
      Fuel == "Battery Electric" ~ "BEV",
      Fuel == "Range Extended Electric" ~ "BEV",
      TRUE ~ Fuel_Group
    )
  )

cat("Standardized Fuel values:\n")
fuel_summary <- join_final_complete_updated %>%
  count(Fuel_Group) %>%
  arrange(Fuel_Group)
print(fuel_summary)

cat("\nBEV Summary:\n")
bev_summary <- join_final_complete_updated %>%
  filter(Fuel_Group == "BEV") %>%
  count(Fuel_Group) %>%
  mutate(percentage = n / sum(n) * 100)
print(bev_summary)

join_final_complete_filtered <- join_final_complete_updated

join_final_complete_filtered_summery <- join_final_complete_filtered %>%
  group_by(Data_Year) %>%
  summarise(total_vehicle = sum(Fleet_Count, na.rm = TRUE)) %>%
  ungroup()

print(join_final_complete_filtered_summery)

# ============================================================================
# APPLY VEHICLE CLASSIFICATION TO THE FINAL DOCUMENT
# ============================================================================
cat("\n=== EXTRACTING CLASSIFICATION RULES ===\n")

classification_thresholds <- Vcar_classified %>%
  filter(!is.na(engine_size_liters)) %>%
  group_by(Fuel_Group) %>%
  summarise(
    p25 = quantile(engine_size_liters, 0.25, na.rm = TRUE),
    p50 = quantile(engine_size_liters, 0.50, na.rm = TRUE),
    p75 = quantile(engine_size_liters, 0.75, na.rm = TRUE),
    p100 = quantile(engine_size_liters, 1, na.rm = TRUE),
    .groups = 'drop'
  )

print(classification_thresholds)

min_values <- Vcar_classified %>%
  filter(`2024` > 0) %>%
  group_by(Fuel_Group) %>%
  summarise(min_value = min(engine_size_liters, na.rm = TRUE))

classification_thresholds <- classification_thresholds %>%
  left_join(min_values, by = "Fuel_Group")

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

cat("\n=== APPLYING CLASSIFICATION TO FILTERED DATASET ===\n")

join_final_complete_classified <- join_final_complete_filtered %>%
  left_join(classification_thresholds, by = "Fuel_Group") %>%
  mutate(
    vehicle_class = case_when(
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
      TRUE ~ "Unclassified"
    )
  ) %>%
  select(-p25, -p50, -p75)

cp_report(join_final_complete_classified, "Fleet_Count", "CP8: after classification join")

cat("\n=== CLASSIFICATION RESULTS FOR FILTERED DATASET ===\n")
classified_summary <- join_final_complete_classified %>%
  count(Fuel_Group, vehicle_class) %>%
  arrange(Fuel_Group, vehicle_class)
print(classified_summary)

cat("\n=== FLEET COUNTS BY VEHICLE CLASS (2024) ===\n")
fleet_by_class <- join_final_complete_classified %>%
  filter(Data_Year == 2024) %>%
  group_by(Fuel_Group, vehicle_class) %>%
  summarise(
    total_vehicles = sum(Fleet_Count, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(Fuel_Group, vehicle_class)

print(fleet_by_class)

engine_size_range <- join_final_complete_classified %>%
  group_by(Fuel_Group) %>%
  summarise(
    p25 = quantile(engine_size_liters, 0.25, na.rm = TRUE),
    p50 = quantile(engine_size_liters, 0.50, na.rm = TRUE),
    p75 = quantile(engine_size_liters, 0.75, na.rm = TRUE),
    p100 = quantile(engine_size_liters, 1, na.rm = TRUE),
    .groups = 'drop'
  )
print(engine_size_range)

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
  group_by(Data_Year, vehicle_class) %>%
  summarise(total_vehicle = sum(Fleet_Count, na.rm = TRUE)) %>%
  ungroup()

print(join_final_complete_classified_summery)

new_sale <- join_final_complete_classified %>%
  filter(Vehicle_Age == 0) %>%
  group_by(Data_Year) %>%
  summarise(sale = sum(Fleet_Count, na.rm = TRUE), .groups = "drop")

print(new_sale)

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

saveRDS(
  join_final_complete_classified,
  file.path(data_dir, "join_final_complete_classified.rds"),
  compress = "gzip"
)

# ============================================================================
# VISUALISE VEHICLE CLASSIFICATION AND CAR AGES
# ============================================================================

# Colour gradients for each fuel family
petrol_colors <- colorRampPalette(c("#81c3d7", "#3a7ca5", "#2f6690", "#16425b"))(5)
diesel_colors <- colorRampPalette(c("#a7333f", "#ce4257", "#a22522", "#720026"))(5)
hev_colors    <- colorRampPalette(c("#BCD4A1", "#a3b18a", "#87B26B", "#558B2F"))(5)
phev_colors   <- colorRampPalette(c("#F8E8F2", "#D4A1BC", "#B26B87", "#8B2F55"))(5)
bev_colors    <- colorRampPalette(c("#FFE0A1", "#FFC96B", "#FFA52F", "#bc6c25"))(5)

vehicle_class_colors <- c(
  "Small Petrol"       = petrol_colors[1],
  "Medium Petrol"      = petrol_colors[2],
  "Large Petrol"       = petrol_colors[3],
  "Very Large Petrol"  = petrol_colors[4],
  "Unsized Petrol"     = petrol_colors[5],

  "Small Diesel"       = diesel_colors[1],
  "Medium Diesel"      = diesel_colors[2],
  "Large Diesel"       = diesel_colors[3],
  "Very Large Diesel"  = diesel_colors[4],
  "Unsized Diesel"     = diesel_colors[5],

  "Small HEV"          = hev_colors[1],
  "Medium HEV"         = hev_colors[2],
  "Large HEV"          = hev_colors[3],
  "Very Large HEV"     = hev_colors[4],
  "Unsized HEV"        = hev_colors[5],

  "Small PHEV"         = phev_colors[1],
  "Medium PHEV"        = phev_colors[2],
  "Large PHEV"         = phev_colors[3],
  "Very Large PHEV"    = phev_colors[4],
  "Unsized PHEV"       = phev_colors[5],

  "Small BEV"          = bev_colors[1],
  "Medium BEV"         = bev_colors[2],
  "Large BEV"          = bev_colors[3],
  "Very Large BEV"     = bev_colors[4],
  "Unsized BEV"        = bev_colors[5],

  "Unclassified"       = "#D3D3D3"
)

vehicle_class_levels <- c(
  "Small Petrol", "Medium Petrol", "Large Petrol", "Very Large Petrol",
  "Small Diesel", "Medium Diesel", "Large Diesel", "Very Large Diesel",
  "Small HEV", "Medium HEV", "Large HEV", "Very Large HEV",
  "Small PHEV", "Medium PHEV", "Large PHEV", "Very Large PHEV",
  "Small BEV", "Medium BEV", "Large BEV", "Very Large BEV"
)

# ---------------------------------------------------------------------------
# Fleet composition by vehicle class over time
# ---------------------------------------------------------------------------
fleet_composition_plot <- join_final_complete_clean %>%
  filter(
    !is.na(Fleet_Count),
    Fleet_Count > 0
  ) %>%
  mutate(
    vehicle_class = factor(vehicle_class, levels = vehicle_class_levels),
    vehicle_class = ifelse(is.na(vehicle_class), "Unclassified", as.character(vehicle_class)),
    vehicle_class = factor(vehicle_class, levels = vehicle_class_levels)
  ) %>%
  group_by(Data_Year, vehicle_class) %>%
  summarise(total_fleet = sum(Fleet_Count, na.rm = TRUE), .groups = 'drop') %>%
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
    breaks = vehicle_class_levels,
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

cat("\n=== DATA SUMMARY ===\n")
summary_data <- join_final_complete %>%
  filter(!is.na(Fleet_Count), Fleet_Count > 0) %>%
  count(vehicle_class) %>%
  arrange(desc(n))

print(summary_data)

# ---------------------------------------------------------------------------
# Fleet composition table
# ---------------------------------------------------------------------------
cat("=== CHECKING VEHICLE CLASS VALUES ===\n")
unique_classes <- join_final_complete_clean %>%
  filter(Fuel_Group %in% c("HEV", "PHEV", "Diesel", "Petrol", "BEV")) %>%
  distinct(vehicle_class) %>%
  arrange(vehicle_class)

print(unique_classes)

fleet_composition_table <- join_final_complete_clean %>%
  filter(
    !is.na(vehicle_class),
    !is.na(Fleet_Count),
    Fleet_Count > 0,
    Fuel_Group %in% c("HEV", "PHEV", "Diesel", "Petrol", "BEV")
  ) %>%
  mutate(
    vehicle_class = factor(vehicle_class, levels = vehicle_class_levels)
  ) %>%
  group_by(Data_Year, Fuel_Group, vehicle_class) %>%
  summarise(total_fleet = sum(Fleet_Count, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(
    names_from = Data_Year,
    values_from = total_fleet,
    values_fill = 0
  )

fleet_composition_2024_table <- fleet_composition_table %>%
  select(Fuel_Group, vehicle_class, `2024`) %>%
  rename(Vehicles_2024 = `2024`) %>%
  mutate(
    Vehicle_Size = case_when(
      str_detect(vehicle_class, "^Small") ~ "Small",
      str_detect(vehicle_class, "^Medium") ~ "Medium",
      str_detect(vehicle_class, "^Large") & !str_detect(vehicle_class, "Very Large") ~ "Large",
      str_detect(vehicle_class, "^Very Large") ~ "Very Large",
      str_detect(vehicle_class, "^Unsized") ~ "Unsized",
      TRUE ~ "Other"
    ),
    Clean_Fuel = str_replace_all(
      as.character(vehicle_class),
      c("^Small " = "", "^Medium " = "", "^Large " = "",
        "^Very Large " = "", "^Unsized " = "")
    )
  ) %>%
  group_by(Fuel_Group, Vehicle_Size) %>%
  summarise(Vehicles_2024 = sum(Vehicles_2024, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(
    names_from = Vehicle_Size,
    values_from = Vehicles_2024,
    values_fill = list(Vehicles_2024 = 0)
  ) %>%
  {
    all_size_cols <- c("Small", "Medium", "Large", "Very Large", "Unsized")

    for(col in all_size_cols) {
      if(!col %in% names(.)) {
        .[[col]] <- 0
      }
    }

    select(., Fuel_Group, all_of(all_size_cols))
  } %>%
  mutate(
    Row_Total = rowSums(across(where(is.numeric)), na.rm = TRUE)
  ) %>%
  arrange(Fuel_Group)

column_totals <- fleet_composition_2024_table %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(Fuel_Group = "TOTAL (Columns)")

total_row <- column_totals %>%
  mutate(
    Row_Total = sum(column_totals %>% select(-Fuel_Group) %>% rowSums(na.rm = TRUE))
  )

fleet_composition_2024_table_with_totals <- bind_rows(
  fleet_composition_2024_table,
  total_row
)

# ---------------------------------------------------------------------------
# Simplified fuel-group stacked area plot
# ---------------------------------------------------------------------------
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

fleet_composition_plot <- join_final_complete_clean %>%
  filter(
    !is.na(vehicle_class),
    !is.na(Fleet_Count),
    Fleet_Count > 0,
    Fuel_Group %in% c("HEV", "PHEV", "Diesel", "Petrol", "BEV")
  ) %>%
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

new_vehicle_class_colors <- c(
  "Small Petrol"       = petrol_colors[1],
  "Medium Petrol"      = petrol_colors[2],
  "Large Petrol"       = petrol_colors[3],
  "Very Large Petrol"  = petrol_colors[4],
  "Unsized Petrol"     = petrol_colors[5],

  "Small Diesel"       = diesel_colors[1],
  "Medium Diesel"      = diesel_colors[2],
  "Large Diesel"       = diesel_colors[3],
  "Very Large Diesel"  = diesel_colors[4],
  "Unsized Diesel"     = diesel_colors[5],

  "HEV"  = "#8FB573",
  "PHEV" = "#B5738F",
  "BEV"  = "#FFB366",
  "Other" = "#D3D3D3"
)

actual_classes <- unique(fleet_composition_plot$new_vehicle_class)
new_vehicle_class_levels <- c(
  intersect(c("Small Petrol", "Medium Petrol", "Large Petrol", "Very Large Petrol", "Unsized Petrol"), actual_classes),
  intersect(c("Small Diesel", "Medium Diesel", "Large Diesel", "Very Large Diesel", "Unsized Diesel"), actual_classes),
  intersect(c("HEV", "PHEV", "BEV", "Other"), actual_classes)
)

cat("\n=== CLASSES FOR PLOT LEGEND ===\n")
print(new_vehicle_class_levels)

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
    legend.text = element_text(size = 10)
  ) +
  guides(fill = guide_legend(ncol = 1))

# ---------------------------------------------------------------------------
# Aggregate fuel types at plot level
# ---------------------------------------------------------------------------
fleet_composition_data <- join_final_complete_clean %>%
  filter(
    !is.na(vehicle_class),
    !is.na(Fleet_Count),
    Fleet_Count > 0,
    Fuel_Group %in% c("HEV", "PHEV", "Diesel", "Petrol", "BEV")
  ) %>%
  mutate(
    new_vehicle_class = case_when(
      str_detect(vehicle_class, "Petrol") ~ "Petrol",
      str_detect(vehicle_class, "Diesel") ~ "Diesel",
      str_detect(vehicle_class, "PHEV") ~ "PHEV",
      str_detect(vehicle_class, "HEV") ~ "HEV",
      str_detect(vehicle_class, "BEV") ~ "BEV",
      TRUE ~ vehicle_class
    )
  ) %>%
  group_by(Data_Year, new_vehicle_class) %>%
  summarise(total_fleet = sum(Fleet_Count, na.rm = TRUE), .groups = 'drop')

new_vehicle_class_colors <- c(
  "Petrol" = petrol_colors[3],
  "Diesel" = diesel_colors[3],
  "HEV"  = "#8FB573",
  "PHEV" = "#B5738F",
  "BEV"  = "#FFB366"
)

new_vehicle_class_levels <- c("Petrol", "Diesel", "HEV", "PHEV", "BEV")

fleet_composition_plot <- fleet_composition_data %>%
  mutate(new_vehicle_class = factor(new_vehicle_class, levels = new_vehicle_class_levels)) %>%
  ggplot(aes(x = Data_Year, y = total_fleet, fill = new_vehicle_class)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(
    values = new_vehicle_class_colors,
    name = "Fuel Type",
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

# ---------------------------------------------------------------------------
# Average vehicle age trend by fuel type
# ---------------------------------------------------------------------------
avg_age_trend_plot <- join_final_complete %>%
  filter(!is.na(Vehicle_Age), !is.na(Fuel_Group), Fleet_Count > 0,
         Fuel_Group %in% c("HEV", "PHEV", "Diesel", "Petrol", "BEV")) %>%
  mutate(
    new_vehicle_class = case_when(
      str_detect(vehicle_class, "Petrol") ~ "Petrol",
      str_detect(vehicle_class, "Diesel") ~ "Diesel",
      str_detect(vehicle_class, "PHEV") ~ "PHEV",
      str_detect(vehicle_class, "HEV") ~ "HEV",
      str_detect(vehicle_class, "BEV") ~ "BEV",
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
  facet_wrap(~ new_vehicle_class, ncol = 3)

print(avg_age_trend_plot)

# ---------------------------------------------------------------------------
# Total average age (all fuel types combined)
# ---------------------------------------------------------------------------
total_avg_age <- join_final_complete %>%
  filter(!is.na(Vehicle_Age), !is.na(Fleet_Count), Fleet_Count > 0, Fuel_Group != "other") %>%
  group_by(Data_Year) %>%
  summarise(
    avg_age = weighted.mean(Vehicle_Age, Fleet_Count, na.rm = TRUE),
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
    x = "Year",
    y = "Average Vehicle Age (Years)"
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

# ---------------------------------------------------------------------------
# Average age by fuel type (wide format)
# ---------------------------------------------------------------------------
avg_age_by_fuel_plot <- join_final_complete %>%
  filter(!is.na(Fleet_Count), Fleet_Count > 0) %>%
  group_by(Data_Year, Fuel_Group) %>%
  summarise(
    avg_age = weighted.mean(Vehicle_Age, Fleet_Count, na.rm = TRUE),
    total_vehicles = sum(Fleet_Count, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(Fuel_Group != "Other") %>%
  pivot_wider(
    id_cols = Data_Year,
    names_from = Fuel_Group,
    values_from = avg_age,
    values_fill = NA
  ) %>%
  arrange(Data_Year)

print(avg_age_by_fuel_plot)

avg_age_2024 <- avg_age_by_fuel_plot %>%
  filter(Data_Year == 2024)

print(avg_age_2024)

# ---------------------------------------------------------------------------
# Vehicle class distribution by age group
# ---------------------------------------------------------------------------
age_group_plot <- join_final_complete %>%
  filter(!is.na(Vehicle_Age), Fleet_Count > 0, Fuel_Group %in% c("HEV", "PHEV", "Diesel", "Petrol", "BEV")) %>%
  mutate(
    new_vehicle_class = case_when(
      str_detect(vehicle_class, "Petrol") ~ "Petrol",
      str_detect(vehicle_class, "Diesel") ~ "Diesel",
      str_detect(vehicle_class, "PHEV") ~ "PHEV",
      str_detect(vehicle_class, "HEV") ~ "HEV",
      str_detect(vehicle_class, "BEV") ~ "BEV",
      TRUE ~ vehicle_class
    ),
    age_group = cut(Vehicle_Age,
                    breaks = c(0, 2, 5, 10, 15, 20, 25, 30),
                    labels = c("0-2", "3-5", "6-10", "11-15", "16-20", "21-25", "26+"),
                    include.lowest = TRUE)
  ) %>%
  filter(!is.na(new_vehicle_class)) %>%
  group_by(age_group, new_vehicle_class) %>%
  summarise(total_vehicles = sum(Fleet_Count, na.rm = TRUE), .groups = 'drop') %>%
  group_by(age_group) %>%
  mutate(percentage = total_vehicles / sum(total_vehicles) * 100) %>%
  ggplot(aes(x = age_group, y = percentage, fill = new_vehicle_class)) +
  geom_col(position = "stack", width = 0.5, alpha = 0.9) +
  scale_fill_brewer(palette = "Set1", name = "Fuel Type") +
  facet_wrap(~ new_vehicle_class, ncol = 3, scales = "free_y") +
  labs(
    x = "Vehicle Age Group (Years)",
    y = "Percentage of Fleet"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  guides(fill = guide_legend(ncol = 1, keyheight = unit(0.3, "cm")))

print(age_group_plot)
