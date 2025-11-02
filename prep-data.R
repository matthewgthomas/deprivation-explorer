library(tidyverse)
library(geographr)
library(IMD)
library(sf)
library(googlesheets4)

lookup_ltla24_region <- read_sf(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD24_RGN24_EN_LU/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
)

lookup_ltla24_region <- lookup_ltla24_region |>
  st_drop_geometry() |>
  select(ltla24_code = LAD24CD, region = RGN24NM)

ruc21_lsoa21 <- read_sf(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA21_RUC21_EW_LU/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
)

ruc21_lsoa21 <- ruc21_lsoa21 |>
  st_drop_geometry() |>
  select(lsoa21_code = LSOA21CD, classification = RUC21NM, ruc = Urban_rural_flag)

# ---- Local Authority data ----
lad_names <- boundaries_ltla24 |>
  st_drop_geometry() |>
  rename(lad_code = ltla24_code, lad_name = ltla24_name) |>
  left_join(
    lookup_ltla23_region23 |>
      select(lad_code = ltla23_code, region_name = region23_name)
  )

# IMD for Local Authority Districts (LAD)
imd_lad <- IMD::imd2025_england_ltla24 |>
  rename(lad_code = ltla24_code) |>
  left_join(
    lookup_ltla24_region |>
      select(lad_code = ltla24_code, region_name = region)
  ) |>
  left_join(lad_names)

# Load LAD boundaries as an sf object (GeoJSON or shapefile)
# (Replace "lad_boundaries.geojson" with your actual file)
lad_boundaries <- boundaries_ltla24 |>
  filter(str_detect(ltla24_code, "^E")) |>
  rename(lad_code = ltla24_code, lad_name = ltla24_name)

# Merge the LAD-level IMD data with spatial boundaries based on a common code.
# (Make sure the field names match; here we assume both have 'lad_code'.)
lad_boundaries <- left_join(lad_boundaries, imd_lad, by = c("lad_code", "lad_name"))

# ---- Neighbourhood-level data ----
lsoa_names <- boundaries_lsoa21 |>
  st_drop_geometry() |>
  rename(lsoa_code = lsoa21_code, lsoa_name = lsoa21_name) |>
  left_join(
    lookup_lsoa21_ward24_ltla24 |>
      select(lsoa_code = lsoa21_code, lad_code = ltla24_code, lad_name = ltla24_name)
  ) |>
  left_join(
    lookup_ltla24_region |>
      select(lad_code = ltla24_code, region_name = region)
  )

# Calculate quintiles
imd_lsoa <- IMD::imd2025_england_lsoa21 |>
  rename(lsoa_code = lsoa21_code) |>
  left_join(lsoa_names) |>
  left_join(ruc21_lsoa21, by = join_by(lsoa_code == lsoa21_code))

# Show only 20% most deprived areas on the map
lsoa_boundaries <-
  boundaries_lsoa21 |>
  left_join(imd_lsoa, by = join_by(lsoa21_code == lsoa_code))

# ---- Analyse income and employment deprivation ----
imd_income_employment <-
  imd2025_england_lsoa21 |>
  select(lsoa21_code, IMD_decile) |>
  left_join(
    imd2025_england_lsoa21_indicators |>
      select(lsoa21_code, income_domain_numerator, employment_domain_numerator)
  ) |>
  left_join(
    lookup_lsoa21_ward24_ltla24 |>
      select(lsoa21_code, lad_code = ltla24_code, lad_name = ltla24_name)
  ) |>
  left_join(
    lookup_ltla24_region |>
      select(lad_code = ltla24_code, region_name = region)
  ) |>
  rename(lsoa_code = lsoa21_code)

imd_props <-
  imd_income_employment |>
  mutate(Core20 = if_else(IMD_decile <= 2, "20% most deprived", "Other")) |>
  group_by(Core20) |>
  summarise(
    people_income_deprived = sum(income_domain_numerator, na.rm = TRUE),
    people_employment_deprived = sum(employment_domain_numerator, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    prop_income_deprived = people_income_deprived / sum(people_income_deprived),
    prop_employment_deprived = people_employment_deprived / sum(people_employment_deprived)
  )

imd_income_employment <-
  imd_income_employment |>
  rename(`Number of income-deprived people` = income_domain_numerator, `Number of employment-deprived people` = employment_domain_numerator) |>
  pivot_longer(cols = contains("people"), values_to = "n")

# ---- Metadata ----
metadata_england <- read_sheet("https://docs.google.com/spreadsheets/d/1FmxLZl_WObibUoGkw1on5SAifVB6Pudsctio5apaKlI/edit?usp=sharing", sheet = "England 2025")

# ---- Save data ----
write_csv(imd_lad, "data/imd_lad.csv")
write_csv(imd_lsoa, "data/imd_lsoa.csv")

write_sf(lad_boundaries, "data/lad_boundaries.geojson")
write_sf(lsoa_boundaries, "data/lsoa_boundaries.geojson")

write_rds(lad_boundaries, "data/lad_boundaries.rds")
write_rds(lsoa_boundaries, "data/lsoa_boundaries.rds")

write_csv(imd_props, "data/imd_props.csv")
write_csv(imd_income_employment, "data/imd_income_employment.csv")

write_csv(metadata_england, "data/metadata_england.csv")
