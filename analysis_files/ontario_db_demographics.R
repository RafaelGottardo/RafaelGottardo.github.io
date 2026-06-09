# ============================================================
# Ontario Dissemination Block Demographics via cancensus
# Census year: 2021 (most recent with DB-level data)
# Geography: DB = Dissemination Block (DBUID)
# ============================================================

# ----- 0. Setup -----
if (!requireNamespace("cancensus",  quietly = TRUE)) install.packages("cancensus")
if (!requireNamespace("dplyr",      quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("readr",      quietly = TRUE)) install.packages("readr")
if (!requireNamespace("stringr",    quietly = TRUE)) install.packages("stringr")

library(cancensus)
library(dplyr)
library(readr)
library(stringr)

# ── API key ──────────────────────────────────────────────────────────────────
# Get a free key at: https://censusmapper.ca/users/sign_up
# Then set it once per session (or add to ~/.Renviron as CANCENSUS_API_KEY=...)
options(cancensus.api_key  = Sys.getenv("CANCENSUS_API_KEY", "CensusMapper_164474c9705b09054f031a9bff098515"))
options(cancensus.cache_path = "cancensus_cache")   # local cache avoids repeat downloads

# ============================================================
# 1. Explore available datasets & vectors
# ============================================================
# Uncomment to browse interactively:
# list_census_datasets()            # shows CA16, CA21, etc.
# search_census_vectors("population density", "CA21")
# search_census_vectors("income",             "CA21")
# search_census_vectors("median total income","CA21")
# search_census_vectors("age",                "CA21")
# search_census_vectors("dwelling",           "CA21")
# search_census_vectors("immigration",        "CA21")

# ============================================================
# 2. Select vectors (Census 2021 = "CA21")
# ============================================================
# NOTE: DB-level data is more limited than DA/CT level.
# Population, dwellings, area (and thus density) are always
# available at DB. Many income/age breakdowns live at DA.
# We pull what's available and flag the rest below.

# IMPORTANT: Do NOT use names that clash with cancensus built-in columns
# (Population, Dwellings, Households, etc.). Use the raw vector IDs as-is;
# cancensus will name the columns "v_CA21_1: Total - Age", etc.
# We rename them explicitly in step 4 below.
vectors_db <- c(
  "v_CA21_1",    # Total population
  "v_CA21_9",    # Male
  "v_CA21_10",   # Female
  "v_CA21_434",  # Total private dwellings
  "v_CA21_435",  # Occupied private dwellings
  "v_CA21_386",  # Median age
  "v_CA21_8",    # Age 0–14
  "v_CA21_251",  # Age 15–64
  "v_CA21_252",  # Age 65+
  "v_CA21_452",  # Average household size
  "v_CA21_411"   # Apartments 5+ storeys
)

# ============================================================
# 3. Pull Dissemination Blocks for Ontario
# ============================================================
# Ontario's Census region code (CMA/CA parent) = "35"
# We use region = list(PR = "35") to get all DBs in Ontario.
# This is a large request (~500k blocks); cancensus caches it.

message("Downloading DB-level data for Ontario – this may take a few minutes on first run...")

ontario_db <- get_census(
  dataset    = "CA21",
  regions    = list(PR = "35"),          # Province of Ontario
  vectors    = vectors_db,
  level      = "DB",                     # Dissemination Block
  geo_format = NA,                       # no geometry (faster); use "sf" if you want spatial
  quiet      = FALSE
)

# ============================================================
# 4. Clean & engineer key columns
# ============================================================
# cancensus names vector columns as "v_CA21_XX: <label>" — map to clean names
# vec_lookup <- c(
#   "v_CA21_1"   = "pop_total",
#   "v_CA21_9"   = "pop_male",
#   "v_CA21_10"  = "pop_female",
#   "v_CA21_434" = "private_dwellings",
#   "v_CA21_435" = "occupied_dwellings",
#   "v_CA21_386" = "age_median",
#   "v_CA21_8"   = "age_0_14",
#   "v_CA21_251" = "age_15_64",
#   "v_CA21_252" = "age_65_plus",
#   "v_CA21_452" = "avg_household_size",
#   "v_CA21_411" = "apt_5plus"
# )
# 
# ontario_db_clean <- ontario_db |>
#   rename(
#     DBUID      = GeoUID,
#     db_name    = `Region Name`,
#     area_sq_km = `Area (sq km)`
#   ) |>
#   # Rename "v_CA21_XX: label" columns to clean names via lookup
#   rename_with(
#     .fn   = ~ vec_lookup[stringr::str_extract(.x, "v_CA21_\\d+")],
#     .cols = dplyr::starts_with("v_CA21_")
#   ) |>
#   mutate(
#     # Population density (persons / km²)
#     pop_density_km2    = if_else(area_sq_km > 0, pop_total / area_sq_km, NA_real_),
# 
#     # Dependency ratio  (young + elderly) / working-age
#     dependency_ratio   = if_else(
#       !is.na(age_15_64) & age_15_64 > 0,
#       (age_0_14 + age_65_plus) / age_15_64,
#       NA_real_
#     ),
# 
#     # Share 65+
#     pct_65_plus        = if_else(pop_total > 0, age_65_plus / pop_total * 100, NA_real_),
# 
#     # Share 0–14
#     pct_0_14           = if_else(pop_total > 0, age_0_14    / pop_total * 100, NA_real_),
# 
#     # Dwelling occupancy rate
#     occupancy_rate     = if_else(
#       !is.na(private_dwellings) & private_dwellings > 0,
#       occupied_dwellings / private_dwellings * 100,
#       NA_real_
#     ),
# 
#     # Urban density tier
#     density_tier = case_when(
#       is.na(pop_density_km2)        ~ "Unknown",
#       pop_density_km2 == 0          ~ "Uninhabited",
#       pop_density_km2 < 150         ~ "Rural",
#       pop_density_km2 < 1500        ~ "Suburban",
#       pop_density_km2 < 4000        ~ "Urban",
#       TRUE                          ~ "Dense Urban"
#     )
#   ) |>
#   # Reorder columns for readability
#   select(
#     DBUID, db_name, area_sq_km,
#     pop_total, pop_male, pop_female,
#     pop_density_km2, density_tier,
#     age_median, age_0_14, age_15_64, age_65_plus,
#     pct_0_14, pct_65_plus, dependency_ratio,
#     private_dwellings, occupied_dwellings, occupancy_rate,
#     avg_household_size, apt_5plus
#   )
# 
# # ============================================================
# # 5. Income note – pull at Dissemination Area (DA) level
# # ============================================================
# # Most income vectors (median total income, LICO-AT, etc.) are
# # suppressed at DB level for privacy. Pull at DA and join via
# # the DAUID prefix embedded in the DBUID (first 8 digits).
# 
# message("\nDownloading DA-level income data for Ontario...")
# 
# vectors_da_income <- c(
#   median_total_income      = "v_CA21_906",   # Median total income – all persons 15+
#   median_household_income  = "v_CA21_906",   # (re-use or swap for household variant)
#   pct_low_income_lico      = "v_CA21_1085",  # % in low income (LICO-AT)
#   median_after_tax_income  = "v_CA21_929",   # Median after-tax income
#   avg_total_income         = "v_CA21_907"    # Average total income
# )
# 
# ontario_da_income <- get_census(
#   dataset    = "CA21",
#   regions    = list(PR = "35"),
#   vectors    = vectors_da_income,
#   level      = "DA",
#   geo_format = NA,
#   quiet      = FALSE
# ) |>
#   rename(DAUID = GeoUID) |>
#   select(DAUID,
#          median_total_income, pct_low_income_lico,
#          median_after_tax_income, avg_total_income)
# 
# # Join income back to DBs via DAUID (first 8 chars of DBUID)
# ontario_db_final <- ontario_db_clean |>
#   mutate(DAUID = str_sub(DBUID, 1, 8)) |>
#   left_join(ontario_da_income, by = "DAUID") |>
#   select(-DAUID)
# 
# # ============================================================
# # 6. Quick summary
# # ============================================================
# message("\n===== Summary =====")
# message("Total DBUIDs: ", nrow(ontario_db_final))
# message("Density tiers:\n")
# print(count(ontario_db_final, density_tier, sort = TRUE))
# 
# message("\nIncome overview (non-suppressed DAs):")
# ontario_db_final |>
#   filter(!is.na(median_total_income)) |>
#   summarise(
#     median_income_p50 = median(median_total_income, na.rm = TRUE),
#     avg_density       = mean(pop_density_km2, na.rm = TRUE)
#   ) |>
#   print()
# 
# # ============================================================
# # 7. Export
# # ============================================================
# output_file <- "ontario_db_demographics_2021.csv"
# write_csv(ontario_db_final, output_file)
# message("\nSaved to: ", output_file)
# 
# # Optional: save as RDS for fast reloading
# saveRDS(ontario_db_final, "ontario_db_demographics_2021.rds")
# message("Also saved as .rds for fast R reloading.")
# 
# # ============================================================
# # 8. Usage examples
# # ============================================================
# # Filter to a specific CSD (municipality) by DBUID prefix:
# #   toronto_db <- filter(ontario_db_final, str_starts(DBUID, "3520005"))
# #
# # Top 10 densest populated blocks:
# #   ontario_db_final |> filter(pop_total > 0) |>
# #     arrange(desc(pop_density_km2)) |> head(10) |>
# #     select(DBUID, pop_total, area_sq_km, pop_density_km2, density_tier)
# #
# # Map with sf (re-run get_census with geo_format = "sf"):
# #   library(ggplot2); library(sf)
# #   ggplot(ontario_db_final_sf) +
# #     geom_sf(aes(fill = pop_density_km2), colour = NA) +
# #     scale_fill_viridis_c(trans = "log1p") + theme_minimal()
