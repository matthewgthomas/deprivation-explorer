library(tidyverse)
library(geographr)
library(IMD)
library(sf)

# ---- Local Authority data ----
lad_names <- boundaries_ltla24 |>
  st_drop_geometry() |>
  rename(lad_code = ltla24_code, lad_name = ltla24_name) |>
  left_join(
    lookup_ltla23_region23 |>
      select(lad_code = ltla23_code, region_name = region23_name)
  )

# IMD for Local Authority Districts (LAD)
imd_lad <- IMD::imd2019_england_ltla23 |>
  rename(lad_code = ltla23_code) |>
  left_join(
    lookup_ltla23_region23 |>
      select(lad_code = ltla23_code, region_name = region23_name)
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
lookup_lsoa11_ltla23 <-
  geographr::lookup_lsoa11_ltla21 |>
  select(lsoa11_code, ltla22_code = ltla21_code) |>
  left_join(
    lookup_ltla22_ltla23 |> select(ltla22_code, ltla23_code, ltla23_name)
  ) |>
  select(-ltla22_code)

lsoa_names <- boundaries_lsoa11 |>
  st_drop_geometry() |>
  rename(lsoa_code = lsoa11_code, lsoa_name = lsoa11_name) |>
  left_join(
    lookup_lsoa11_ltla23 |>
      select(lsoa_code = lsoa11_code, lad_code = ltla23_code, lad_name = ltla23_name)
  ) |>
  left_join(
    lookup_ltla23_region23 |>
      select(lad_code = ltla23_code, region_name = region23_name)
  )

# Calculate quintiles
imd_lsoa <- IMD::imd2019_england_lsoa11 |>
  rename(lsoa_code = lsoa11_code) |>
  left_join(lsoa_names) |>
  left_join(ruc11_lsoa11, by = join_by(lsoa_code == lsoa11_code))

# Show only 20% most deprived areas on the map
lsoa_boundaries <-
  boundaries_lsoa11 |>
  left_join(imd_lsoa, by = join_by(lsoa11_code == lsoa_code))

# ---- Analyse income and employment deprivation ----
imd_income_employment <-
  imd2019_england_lsoa11 |>
  select(lsoa11_code, IMD_decile) |>
  left_join(
    imd2019_england_lsoa11_indicators |>
      select(lsoa11_code, `Income Domain numerator`, `Employment Domain numerator`)
  ) |>
  left_join(
    lookup_lsoa11_ltla23 |>
      select(lsoa11_code, lad_code = ltla23_code, lad_name = ltla23_name)
  ) |>
  left_join(
    lookup_ltla23_region23 |>
      select(lad_code = ltla23_code, region_name = region23_name)
  ) |>
  rename(lsoa_code = lsoa11_code)

imd_props <-
  imd_income_employment |>
  mutate(Core20 = if_else(IMD_decile <= 2, "20% most deprived", "Other")) |>
  group_by(Core20) |>
  summarise(
    people_income_deprived = sum(`Income Domain numerator`, na.rm = TRUE),
    people_employment_deprived = sum(`Employment Domain numerator`, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    prop_income_deprived = people_income_deprived / sum(people_income_deprived),
    prop_employment_deprived = people_employment_deprived / sum(people_employment_deprived)
  )

imd_income_employment <-
  imd_income_employment |>
  rename(`Number of income-deprived people` = `Income Domain numerator`, `Number of employment-deprived people` = `Employment Domain numerator`) |>
  pivot_longer(cols = contains("people"), values_to = "n")

# ---- Save data ----
write_csv(imd_lad, "data/imd_lad.csv")
write_csv(imd_lsoa, "data/imd_lsoa.csv")

write_sf(lad_boundaries, "data/lad_boundaries.geojson")
write_sf(lsoa_boundaries, "data/lsoa_boundaries.geojson")

write_rds(lad_boundaries, "data/lad_boundaries.rds")
write_rds(lsoa_boundaries, "data/lsoa_boundaries.rds")

write_csv(imd_props, "data/imd_props.csv")
write_csv(imd_income_employment, "data/imd_income_employment.csv")
