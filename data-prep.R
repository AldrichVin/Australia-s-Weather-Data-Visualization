# Install required packages (only once)
install.packages(c("sf", "dplyr"))

# Load libraries
library(sf)
library(dplyr)

# -----------------------------
# 1. Load golf course dataset
# -----------------------------
golf <- st_read("export.json")

# Set CRS to EPSG:4326 (WGS 84) since it’s likely in lat/long
golf <- st_set_crs(golf, 4326)

# Check CRS to confirm
print("CRS of golf dataset:")
print(st_crs(golf))

# -----------------------------
# 2. Load Australian state boundaries
# -----------------------------
states <- st_read("ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp")

# Keep only Australia
states <- states %>% filter(admin == "Australia")

# Check CRS of states
print("CRS of states dataset:")
print(st_crs(states))

# Verify column names in states
print("Columns in states dataset:")
print(names(states))

# Check for invalid geometries
if (sum(!st_is_valid(golf)) > 0) {
  golf <- st_make_valid(golf)
  print("Fixed invalid geometries in golf dataset")
}
if (sum(!st_is_valid(states)) > 0) {
  states <- st_make_valid(states)
  print("Fixed invalid geometries in states dataset")
}

# -----------------------------
# 3. Spatial join: assign each golf point to a state
# -----------------------------
golf_with_state <- st_join(golf, states["name"])   # "name" = state name column

# -----------------------------
# 4. Clean and check results
# -----------------------------
# Drop geometry if you only need table for Vega-Lite
golf_table <- st_drop_geometry(golf_with_state)

# Preview
head(golf_table)

# -----------------------------
# 5. Export results
# -----------------------------
# As CSV (easy for Vega-Lite)
write.csv(golf_table, "golf_with_state.csv", row.names = FALSE)

# OR as GeoJSON if you want to keep geometry
st_write(golf_with_state, "golf_with_state.geojson", driver = "GeoJSON", append = FALSE)
