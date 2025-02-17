library(tidyverse)
library(geographr)
library(IMD)
library(sf)

# ---- Local Authority data ----
lad_names <- boundaries_ltla21 |>
  st_drop_geometry() |>
  rename(lad_code = ltla21_code, lad_name = ltla21_name) |>
  left_join(
    lookup_ltla21_region21 |>
      select(lad_code = ltla21_code, region_name = region21_name)
  )

# IMD for Local Authority Districts (LAD)
imd_lad <- IMD::imd_england_lad |>
  left_join(
    lookup_ltla21_region21 |>
      select(lad_code = ltla21_code, region_name = region21_name)
  ) |>
  left_join(lad_names)

# Load LAD boundaries as an sf object (GeoJSON or shapefile)
# (Replace "lad_boundaries.geojson" with your actual file)
lad_boundaries <- boundaries_ltla21 |>
  filter(str_detect(ltla21_code, "^E")) |>
  rename(lad_code = ltla21_code, lad_name = ltla21_name)

# Merge the LAD-level IMD data with spatial boundaries based on a common code.
# (Make sure the field names match; here we assume both have 'lad_code'.)
lad_boundaries <- left_join(lad_boundaries, imd_lad, by = c("lad_code", "lad_name"))

# ---- Neighbourhood-level data ----
lsoa_names <- boundaries_lsoa11 |>
  st_drop_geometry() |>
  rename(lsoa_code = lsoa11_code, lsoa_name = lsoa11_name) |>
  left_join(
    lookup_lsoa11_ltla21 |>
      select(lsoa_code = lsoa11_code, lad_code = ltla21_code, lad_name = ltla21_name)
  ) |>
  left_join(
    lookup_ltla21_region21 |>
      select(lad_code = ltla21_code, region_name = region21_name)
  )

# Calculate quintiles
imd_lsoa <- IMD::imd_england_lsoa |>
  left_join(lsoa_names) |>
  left_join(ruc11_lsoa11, by = join_by(lsoa_code == lsoa11_code))

# Show only 20% most deprived areas on the map
lsoa_boundaries <-
  boundaries_lsoa11 |>
  left_join(imd_lsoa, by = join_by(lsoa11_code == lsoa_code))

# ---- Save data ----
write_csv(imd_lad, "data/imd_lad.csv")
write_csv(imd_lsoa, "data/imd_lsoa.csv")

write_sf(lad_boundaries, "data/lad_boundaries.geojson")
write_sf(lsoa_boundaries, "data/lsoa_boundaries.geojson")
