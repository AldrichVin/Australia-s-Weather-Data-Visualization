# Install required packages (only once)
install.packages(c("sf", "dplyr", "geojsonio"))

# Load libraries
library(sf)
library(dplyr)
library(geojsonio)

# -----------------------------
# 1. Load golf course dataset
# -----------------------------
golf <- st_read("export.json")
golf <- st_set_crs(golf, 4326)

# Check CRS
print("CRS of golf dataset:")
print(st_crs(golf))

# Check total points and unique names
print("Total points in golf dataset:")
print(nrow(golf))
print("Number of unique golf course names:")
print(length(unique(golf$name.x)))

# Identify unnamed points
print("Number of points with missing name.x:")
print(sum(is.na(golf$name.x) | golf$name.x == ""))

# -----------------------------
# 2. Load Australian state boundaries
# -----------------------------
states <- st_read("ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp")
states <- states %>% filter(admin == "Australia")

# Map state names to abbreviations
states <- states %>% 
  mutate(state = case_when(
    name == "Queensland" ~ "QLD",
    name == "New South Wales" ~ "NSW",
    name == "Victoria" ~ "VIC",
    name == "South Australia" ~ "SA",
    name == "Western Australia" ~ "WA",
    name == "Tasmania" ~ "TAS",
    name == "Northern Territory" ~ "NT",
    name == "Australian Capital Territory" ~ "ACT",
    TRUE ~ "Unknown"
  ))

# Check CRS and columns
print("CRS of states dataset:")
print(st_crs(states))
print("Columns in states dataset:")
print(names(states))

# Align CRS
if (st_crs(golf) != st_crs(states)) {
  states <- st_transform(states, 4326)
  print("Transformed states CRS to EPSG:4326")
}

# Validate geometries
if (sum(!st_is_valid(golf)) > 0) {
  golf <- st_make_valid(golf)
  print("Fixed invalid geometries in golf dataset")
}
if (sum(!st_is_valid(states)) > 0) {
  states <- st_make_valid(states)
  print("Fixed invalid geometries in states dataset")
}

# -----------------------------
# 3. Spatial join and simplify dataset
# -----------------------------
golf_with_state <- st_join(golf, states %>% select(name, state), left = TRUE)

# Check for missing states before filtering
print("Number of points with missing state (name.y) before filtering:")
print(sum(is.na(golf_with_state$name.y)))
print("Number of points with missing state (state) before filtering:")
print(sum(is.na(golf_with_state$state)))
print("Total number of features before filtering:")
print(nrow(golf_with_state))
print("Number of unique golf course names before filtering:")
print(length(unique(golf_with_state$name.x)))

# Log missing states with coordinates
missing_states <- golf_with_state %>% 
  filter(is.na(name.y)) %>% 
  select(name.x, golf.course, golf.par, geometry) %>%
  mutate(longitude = st_coordinates(geometry)[,1], latitude = st_coordinates(geometry)[,2])
print("Golf courses with missing states (including coordinates):")
print(st_drop_geometry(missing_states))

# Simplify and filter dataset
golf_with_state <- golf_with_state %>% 
  select(name.x, name.y, state, golf.course, golf.par, geometry) %>%
  rename(golf_course = name.x) %>%
  filter(!is.na(name.y) & !is.na(state)) %>%
  mutate(golf.par = as.numeric(gsub(" \\+.*", "", as.character(golf.par))))

# Check after filtering
print("Number of points with missing state (name.y) after filtering:")
print(sum(is.na(golf_with_state$name.y)))
print("Number of points with missing state (state) after filtering:")
print(sum(is.na(golf_with_state$state)))
print("Total number of features after filtering:")
print(nrow(golf_with_state))
print("Number of unique golf course names after filtering:")
print(length(unique(golf_with_state$golf_course)))

# Preview first few rows
print("Preview of golf_with_state:")
print(head(st_drop_geometry(golf_with_state), 10))

# Optional: Deduplicate by golf_course (uncomment if desired)
# golf_with_state <- golf_with_state %>% 
#   group_by(golf_course) %>% 
#   slice(1) %>% 
#   ungroup()
# print("Total number of features after deduplication:")
# print(nrow(golf_with_state))
# print("Number of unique golf course names after deduplication:")
# print(length(unique(golf_with_state$golf_course)))

# -----------------------------
# 4. Export results
# -----------------------------
# As CSV
golf_table <- st_drop_geometry(golf_with_state)
write.csv(golf_table, "golf_with_state.csv", row.names = FALSE)

# As TopoJSON
st_write(golf_with_state, "golf_with_state_temp.geojson", driver = "GeoJSON", append = FALSE)
geojsonio::topojson_write(golf_with_state, file = "golf_with_state.json", object_name = "golf_with_state")
file.remove("golf_with_state_temp.geojson")




library(readr)
library(dplyr)
library(lubridate)

# ENSO (weekly tidy format)
enso <- read_csv("rnino_3.4.csv", col_names = c("Start","End","Value")) %>%
  mutate(Date = ymd(Start),
         Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(ENSO = mean(Value, na.rm = TRUE))


# IOD (weekly tidy format)
iod <- read_csv("iod_1.csv", col_names = c("Start","End","Value")) %>%
  mutate(Date = ymd(Start),
         Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(IOD = mean(Value, na.rm = TRUE))

# SAM (monthly grid format)
sam <- read_table("SAM.txt")
View(enso)
colnames(sam) <- c("Year","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

sam <- sam %>%
  rowwise() %>%
  mutate(SAM = mean(c_across(JAN:DEC), na.rm = TRUE)) %>%
  ungroup() %>%
  select(Year, SAM)


# Merge all three by Year
climate_drivers <- enso %>%
  inner_join(iod, by = "Year") %>%
  inner_join(sam, by = "Year")

# Save to CSV
write_csv(climate_drivers, "climate_drivers_yearly.csv")

head(climate_drivers)
View(climate_drivers)

library(jsonlite)

# Fetch the data
df <- read.csv("https://ourworldindata.org/grapher/average-precipitation-per-year.csv?v=1&csvType=full&useColumnShortNames=true")
View(df)

# Filter only Australia
aus <- df %>% filter(Entity == "Australia")

# Pick baseline period (e.g., 1961–1990)
baseline <- aus %>% filter(Year >= 1961 & Year <= 1990)

baseline_mean <- mean(baseline$total_precipitation, na.rm = TRUE)

# Calculate anomalies
aus <- aus %>%
  mutate(RainfallAnomaly = total_precipitation - baseline_mean)

# Save as CSV
write_csv(aus, "Australia_rainfall_anomalies.csv")

head(aus)
# Fetch the metadata
metadata <- fromJSON("https://ourworldindata.org/grapher/average-precipitation-per-year.metadata.json?v=1&csvType=full&useColumnShortNames=true")


# Keep only Year and RainfallAnomaly
rain <- aus %>% select(Year, RainfallAnomaly)

# Merge all together
climate_with_rain <- climate_drivers %>%
  inner_join(rain, by = "Year")

# Save to CSV
write_csv(climate_with_rain, "climate_drivers_with_rain.csv")

# Preview
head(climate_with_rain)














# Install once
install.packages("tidygeocoder")

# ===========================================
# 📍 City-level Rainfall with Coordinates + Region
# ===========================================

# Load required libraries
library(tidygeocoder)   # For geocoding city names to lat/long
library(dplyr)          # For data manipulation

# -------------------------------------------
# STEP 1: Load your rainfall dataset
# -------------------------------------------
# Expected structure:
# Location, Annual_Rainfall
# Adelaide, 550.6
# Alice Springs, 281.2
# Bamaga, 1753.3
# Birdsville, 164.9
# etc.

rainfall <- read.csv("map_rainfall.csv", header = TRUE, skip = 1)
head(rainfall)

# -------------------------------------------
# STEP 2: Geocode locations using OpenStreetMap (OSM)
# -------------------------------------------
# This adds latitude and longitude columns for each city
rainfall_geo <- rainfall %>%
  geocode(address = Location, method = "osm", lat = latitude, long = longitude)

# -------------------------------------------
# STEP 3: Clean the rainfall column
# -------------------------------------------
# Rename incorrect column if necessary
names(rainfall_geo)[names(rainfall_geo) == "Annual.Rainall"] <- "Annual_Rainfall"

# Remove “(mm)” units and convert to numeric
rainfall_geo$Annual_Rainfall <- gsub(" \\(mm\\)", "", rainfall_geo$Annual_Rainfall)
rainfall_geo$Annual_Rainfall <- as.numeric(rainfall_geo$Annual_Rainfall)

# -------------------------------------------
# STEP 4: Fix coordinate direction (hemisphere correction)
# -------------------------------------------
# Australia is in the southern (–) and eastern (+) hemisphere

# Flip any positive latitudes (should be negative)
rainfall_geo$latitude <- ifelse(rainfall_geo$latitude > 0, -rainfall_geo$latitude, rainfall_geo$latitude)

# Flip any negative longitudes (should be positive)
rainfall_geo$longitude <- ifelse(rainfall_geo$longitude < 0, -rainfall_geo$longitude, rainfall_geo$longitude)

# -------------------------------------------
# STEP 5: Remove invalid or out-of-range coordinates
# -------------------------------------------
rainfall_geo <- rainfall_geo %>%
  filter(latitude <= -10 & latitude >= -44 & longitude >= 113 & longitude <= 154)

# -------------------------------------------
# STEP 6: Assign broad Australian climate regions
# -------------------------------------------
# Simple latitude-based classification:
# > –20°: Tropical North
# –30° to –20°: Arid Interior
# –40° to –30°: Temperate South
# < –40°: Other

rainfall_geo$Region <- with(rainfall_geo,
                            ifelse(latitude > -20, "Tropical",
                                   ifelse(latitude > -30, "Arid",
                                          ifelse(latitude > -40, "Temperate", "Other")))
)

# -------------------------------------------
# STEP 7: Save final cleaned dataset
# -------------------------------------------
write.csv(rainfall_geo, "map_rainfall_with_coords.csv", row.names = FALSE)

# -------------------------------------------
# STEP 8: Preview the final output
# -------------------------------------------
head(rainfall_geo)
View(rainfall_geo)





















# ----------------------------------------------------
# STEP 1: Load packages
# ----------------------------------------------------
library(sf)           # spatial polygons
library(dplyr)        # data manipulation
library(readr)        # CSV read/write
install.packages("rnaturalearthdata")
# ----------------------------------------------------
# STEP 2: Load gridded data
# ----------------------------------------------------
grid_data <- read_csv("global_temp.csv") %>%
  mutate(Anomaly = as.numeric(Anomaly))

# Convert to spatial points (sf)
grid_sf <- st_as_sf(
  grid_data,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

# ----------------------------------------------------
# STEP 3: Load country boundaries
# ----------------------------------------------------
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  select(iso_a3, name)

# ----------------------------------------------------
# STEP 4: Spatial join to find which country each grid point belongs to
# ----------------------------------------------------
joined <- st_join(grid_sf, world, join = st_within)

# Remove ocean points (NA country)
joined <- joined %>% filter(!is.na(iso_a3))

# ----------------------------------------------------
# STEP 5: Calculate mean anomaly per country
# ----------------------------------------------------
country_means <- joined %>%
  group_by(iso_a3, name) %>%
  summarise(Anomaly = mean(Anomaly, na.rm = TRUE)) %>%
  arrange(desc(Anomaly))

# ----------------------------------------------------
# STEP 6: Save to CSV
# ----------------------------------------------------
write_csv(country_means, "data/country_temp_anomaly.csv")

# Preview top results
head(country_means)
View(country_means)













# =====================================================
# 1. Load libraries
# =====================================================
library(readr)
library(dplyr)
library(lubridate)

# =====================================================
# 2. Load OWID dataset
# =====================================================
df <- read_csv(
  "https://ourworldindata.org/grapher/monthly-temperature-anomalies.csv?v=1&csvType=full&useColumnShortNames=true",
  show_col_types = FALSE
)

# Check structure
glimpse(df)
# Columns: Entity | Code | Day | temperature_anomaly

# =====================================================
# 3. Extract year and clean data
# =====================================================
df_clean <- df %>%
  filter(!is.na(Code)) %>%                # keep only countries with ISO3 codes
  mutate(
    Year = year(Day),                    # extract year from date
    Month = month(Day),
    temperature_anomaly = as.numeric(temperature_anomaly)
  )
df_clean
# =====================================================
# 4. Aggregate to annual mean per country
# =====================================================
annual_df <- df_clean %>%
  group_by(Code, Entity, Year) %>%
  summarise(
    Anomaly = mean(temperature_anomaly, na.rm = TRUE),
    .groups = "drop"
  )

# Sanity check — typical anomaly range is around -2°C to +2°C
summary(annual_df$Anomaly)

# =====================================================
# 5. (Optional) Create a multi-year mean (2000–2024) for static maps
# =====================================================
recent_means <- annual_df %>%
  filter(Year >= 2000 & Year <= 2024) %>%
  group_by(Code, Entity) %>%
  summarise(
    Anomaly = mean(Anomaly, na.rm = TRUE),
    .groups = "drop"
  )

# =====================================================
# 6. Save to CSV for Vega-Lite (ensure no quotes)
# =====================================================

# Ensure Year is numeric (no hidden factor conversion)
annual_df <- annual_df %>%
  mutate(
    Year = as.numeric(Year),
    Anomaly = round(Anomaly, 3) # optional: tidy up decimals
  )

recent_means <- recent_means %>%
  mutate(
    Anomaly = round(Anomaly, 3)
  )

# Use write.table to prevent R from quoting numbers
write.table(
  annual_df,
  "country_temp_anomaly_by_year.csv",
  sep = ",",
  row.names = FALSE,
  quote = FALSE
)       # for slider map
write_csv(recent_means, "country_temp_anomaly_mean2000_2024.csv") # for static map

# =====================================================
# 7. Preview — check Australia trend
# =====================================================
annual_df %>% filter(Code == "AUS") %>% arrange(desc(Year)) %>% head(10)
unique(annual_df$Year)
str(annual_df[annual_df$Year == 2024, ])







# Load packages
library(tidyverse)

# 1️⃣ Read the data
data <- read.csv("climate_drivers_with_rain.csv")

# 2️⃣ Reshape it from wide → long format
tidy_data <- data %>%
  pivot_longer(
    cols = c(ENSO, IOD, SAM),
    names_to = "Driver",
    values_to = "Index"
  )

# 3️⃣ Optional: rename RainfallAnomaly for consistency
tidy_data <- tidy_data %>%
  rename(Rainfall_Anomaly = RainfallAnomaly)

head(tidy_data)
# 4️⃣ Save to CSV for Vega-Lite
write.csv(tidy_data, "driver_rainfall_tidy.csv", row.names = FALSE)















# ==============================
# 📊 Radar Chart Data Preparation (Corrected)
# ==============================

library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
library(readr)

# ---- 1️⃣ Load Data ----
climate <- read_csv("climate_change_data.csv")

# ---- 2️⃣ Clean & Select Relevant Columns ----
climate_clean <- climate %>%
  select(Date, Country, Temperature, `CO2 Emissions`, `Sea Level Rise`,
         Precipitation, Humidity, `Wind Speed`) %>%
  mutate(
    Date = ymd_hms(Date),
    Temperature = as.numeric(Temperature),
    `CO2 Emissions` = as.numeric(`CO2 Emissions`),
    `Sea Level Rise` = as.numeric(`Sea Level Rise`),
    Precipitation = as.numeric(Precipitation),
    Humidity = as.numeric(Humidity),
    `Wind Speed` = as.numeric(`Wind Speed`)
  ) %>%
  filter(year(Date) >= 2000 & year(Date) <= 2022)

# ---- 3️⃣ Country Means ----
country_means <- climate_clean %>%
  group_by(Country) %>%
  summarise(
    Temperature = mean(Temperature, na.rm = TRUE),
    CO2 = mean(`CO2 Emissions`, na.rm = TRUE),
    SeaLevel = mean(`Sea Level Rise`, na.rm = TRUE),
    Precipitation = mean(Precipitation, na.rm = TRUE),
    Humidity = mean(Humidity, na.rm = TRUE),
    Wind = mean(`Wind Speed`, na.rm = TRUE)
  ) %>%
  ungroup()

# ---- 4️⃣ Compute Global Mean ----
global_mean <- country_means %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  mutate(Country = "Global")

# ---- 5️⃣ Scale Each Metric Using Full Global Range ----
vars <- c("Temperature", "CO2", "SeaLevel", "Precipitation", "Humidity", "Wind")

country_scaled <- country_means %>%
  mutate(across(all_of(vars), ~ rescale(., to = c(0, 1)), .names = "{.col}_scaled"))

# ---- 6️⃣ Extract Scaled Australia and Global ----
australia_scaled <- country_scaled %>% filter(Country == "Australia") %>%
  select(Country, ends_with("_scaled"))
global_scaled <- global_mean %>%
  mutate(across(all_of(vars), ~ rescale(., 
                                        to = c(0, 1), 
                                        from = range(country_means[[cur_column()]], na.rm = TRUE)), 
                .names = "{.col}_scaled")) %>%
  select(Country, ends_with("_scaled"))

# ---- 7️⃣ Combine for Radar Chart ----
radar_data <- bind_rows(australia_scaled, global_scaled) %>%
  pivot_longer(cols = ends_with("_scaled"), 
               names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = sub("_scaled", "", Metric),
         Country = paste0(Country, "_scaled")) %>%
  select(Metric, Country, Value)

# ---- 8️⃣ Save to CSV ----
write_csv(radar_data, "radar_chart.csv")

# ---- 9️⃣ Inspect ----
print(radar_data)

                      


# ====================================================
# Global Climate Events Dataset (2020–2025) — Preparation Script
# ====================================================

# 1. Load Required Packages
library(tidyverse)
library(lubridate)
install.packages("countrycode")
library(countrycode)

# 2. Import the Dataset
# Make sure the CSV file is in your working directory
# e.g., setwd("path/to/your/folder")
events_raw <- read_csv("global_climate_events_economic_impact_2020_2025.csv")

# 3. Inspect the structure
glimpse(events_raw)
summary(events_raw)

# 4. Clean and Standardize Column Names
events <- events_raw %>%
  janitor::clean_names() %>%
  rename(
    event_id = event_id,
    date = date,
    year = year,
    month = month,
    country = country,
    event_type = event_type,
    severity = severity,
    duration_days = duration_days,
    affected_population = affected_population,
    deaths = deaths,
    injuries = injuries,
    economic_impact_million_usd = economic_impact_million_usd,
    infrastructure_damage_score = infrastructure_damage_score,
    response_time_hours = response_time_hours,
    international_aid_million_usd = international_aid_million_usd,
    latitude = latitude,
    longitude = longitude
  )

# 5. Parse Dates and Derive Time Components
events <- events %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date)
  )

# 6. Derive Additional Metrics
events <- events %>%
  mutate(
    total_casualties = deaths + if_else(is.na(injuries), 0, injuries),
    impact_per_capita = economic_impact_million_usd / pmax(affected_population, 1),
    aid_percentage = (international_aid_million_usd / pmax(economic_impact_million_usd, 1)) * 100
  )

# 7. Add Continent Classification for Bubble Timeline
events <- events %>%
  mutate(
    continent = countrycode(country, origin = "country.name", destination = "continent"),
    region_group = if_else(country == "Australia", "Australia", "Rest of World")
  )

# 8. Handle Missing or Extreme Values
events <- events %>%
  mutate(
    severity = pmin(pmax(severity, 1), 10),
    infrastructure_damage_score = pmin(pmax(infrastructure_damage_score, 0), 100),
    aid_percentage = if_else(aid_percentage > 1000 | is.na(aid_percentage), NA_real_, aid_percentage)
  )

# 9. Verify Cleaned Data
summary(events)
glimpse(events)

# 10. Create Aggregated Dataset for Alluvial/Sankey Visualization
alluvial_data <- events %>%
  group_by(event_type, region_group) %>%
  summarise(
    count_events = n(),
    avg_severity = mean(severity, na.rm = TRUE),
    total_casualties = sum(total_casualties, na.rm = TRUE),
    total_impact = sum(economic_impact_million_usd, na.rm = TRUE),
    avg_damage = mean(infrastructure_damage_score, na.rm = TRUE)
  ) %>%
  ungroup()

events
# 11. Export Cleaned and Aggregated Datasets
write_csv(events, "events_dataset_clean.csv")
write_csv(alluvial_data, "alluvial_dataset.csv")

# 12. Quick Data Quality Check
cat("\n✅ Cleaned data saved as 'events_dataset_clean.csv' (for Vega-Lite visualizations)")
cat("\n✅ Aggregated data saved as 'alluvial_dataset.csv' (for Sankey / Alluvial chart)\n")

# 13. Optional: Simple Exploratory Plot
events %>%
  ggplot(aes(x = year, y = severity, color = continent, size = economic_impact_million_usd)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Extreme Event Severity and Impact by Continent (2020–2025)",
    x = "Year",
    y = "Severity"
  )


library(tidyverse)
library(countrycode)

# Assume you already have cleaned dataset `events`
bubble_plot_data <- events %>%
  group_by(year, country, continent, event_type) %>%
  summarise(
    avg_severity = mean(severity, na.rm = TRUE),
    total_economic_impact_million_usd = sum(economic_impact_million_usd, na.rm = TRUE),
    total_casualties = sum(total_casualties, na.rm = TRUE),
    n_events = n()
  ) %>%
  ungroup() %>%
  mutate(region_group = if_else(country == "Australia", "Australia", "Rest of World"))

write_csv(bubble_plot_data, "bubble_plot_dataset_casualties.csv")
cat("\n✅ Saved 'bubble_plot_dataset_casualties.csv'\n")

max(bubble_plot_data$total_economic_impact_million_usd)
bubble_plot_data <- bubble_plot_data[bubble_plot_data$total_economic_impact_million_usd != 
                                       max(bubble_plot_data$total_economic_impact_million_usd), ]
View(bubble_plot_data)

View(bubble_plot_data %>% filter(year == "2025", event_type == "Wildfire"))

unique(bubble_plot_data$event_type)


# Compute 99th percentile threshold
threshold <- quantile(
  bubble_plot_data$total_economic_impact_million_usd,
  0.99, na.rm = TRUE
)

# Filter out top 1% extreme outliers
bubble_plot_data <- bubble_plot_data %>%
  filter(total_economic_impact_million_usd <= threshold)

cat("Removed values above", threshold, "million USD\n")



install.packages("sf")   # for reading/writing spatial data
library(sf)
habitat_core <- st_read("Habitat_atrisk_cc_core.gpkg")




st_layers("Habitat_atrisk_cc_core.gpkg")

habitat_core <- st_read("Habitat_atrisk_cc_core.gpkg", layer = "Habitat_atrisk_cc_core")
st_write(habitat_core, "koala_habitat_core.geojson", driver = "GeoJSON")


# Load required packages
library(sf)
install.packages("rmapshaper")
library(rmapshaper)
library(geojsonio)
# Step 1: Read the GPKG file
core <- st_read("Habitat_atrisk_cc_core.gpkg")

# Step 2: Reproject to WGS84
core_wgs84 <- st_transform(core, 4326)

# Step 3: Simplify to reduce file size
core_simple <- ms_simplify(core_wgs84, keep_shapes = TRUE, keep = 0.1)

# Step 4: Write to TopoJSON using geojsonio
geojson_write(core_simple, file = "koala_habitat_core.json", topology = TRUE)

names(core_simple)











# Install if needed
# install.packages(c("terra", "sf"))

library(terra)
library(sf)

# 1️⃣ Load your raster (use the full file path)
fire_path <- "Fires_in_Australias_forests_2018_geotiff/fire_for18.tif"
fire_raster <- rast(fire_path)

# 2️⃣ Check what it looks like
print(fire_raster)
plot(fire_raster, main = "Bushfire Raster - 2018")

# 3️⃣ Create a mask to keep only burned areas (assuming > 0 means burned)
fire_mask <- fire_raster > 0

# 4️⃣ Convert to polygons (vector)
fire_poly <- as.polygons(fire_raster, dissolve = TRUE)
fire_sf <- st_as_sf(fire_poly)

# 5️⃣ (Optional) Simplify for lighter file size
fire_simplified <- st_simplify(fire_sf, dTolerance = 0.01)

# 6️⃣ Export as GeoJSON
st_write(fire_simplified, "fires_2018.geojson", driver = "GeoJSON", delete_dsn = TRUE)

# 7️⃣ After this, convert to TopoJSON via:
# mapshaper fires_2018.geojson -simplify 10% -o format=topojson fires_2018.json




# install.packages("dplyr")
library(dplyr)

fires <- read.csv("fire_archive_M6_96619.csv")

# 1️⃣ Keep only Australia (lon between 110–155, lat between -45–-10)
# 2️⃣ Filter for 2019–2020 (Black Summer)
# 3️⃣ Keep only confidence >= 80
# 4️⃣ Select minimal useful columns

fires_small <- fires %>%
  filter(longitude >= 110, longitude <= 155,
         latitude >= -45, latitude <= -10,
         acq_date >= "2019-07-01", acq_date <= "2020-03-31",
         confidence >= 80) %>%
  select(latitude, longitude, brightness, acq_date, acq_time, confidence, satellite)


fires_smaller = fires_small[,1:3]

min(fires_smaller$brightness)
# Save new smaller CSV
write.csv(fires_smaller, "fire_dataset.csv", row.names = FALSE)








library(readxl)
library(dplyr)

# Read Excel
df <- read_excel("Wilderness_fire_impact.xlsx")

# Filter endangered and vulnerable only
filtered <- df %>%
  filter(`Threat status` %in% c("Endangered", "Vulnerable"))

# Check the result
print(filtered)

# Optionally, export it to CSV
write.csv(filtered, "filtered_fire_impact.csv", row.names = FALSE)





# Step 1: Load required libraries
library(dplyr)

# Step 2: Load the dataset (replace 'species_data.csv' with your file path)
# Assumes columns like 'Scientific.name', 'Threat.status', etc.
data <- read.csv("filtered_fire_impact.csv", stringsAsFactors = FALSE)

# Clean the dataset (e.g., replace "-" in Unburned habitat with 0)
data$Unburned.habitat..Ha.[data$Unburned.habitat..Ha. == "-"] <- 0

# Step 3: Create the full mapping table (scientific to common names)
common_names <- data.frame(
  Scientific.name = c(
    "Saltuarius kateae", "Sminthopsis griseoventer aitkeni", "Potorous longipes",
    "Calyptorhynchus lathami halmaturinus", "Philoria pughi", "Pseudomys oralis",
    "Philoria sphagnicolus", "Trichosurus caninus", "Zoothera lunulata halmaturina",
    "Litoria littlejohni", "Uperoleia tyleri", "Atrichornis rufescens",
    "Litoria subglandulosa", "Tachyglossus aculeatus multiaculeatus",
    "Hoplocephalus bungaroides", "Eulamprus leuraensis", "Litoria citropa",
    "Scoteanax rueppellii", "Mixophyes balbus", "Pseudophryne australis",
    "Litoria jervisiensis", "Kerivoula papuensis", "Drysdalia rhodogaster",
    "Heleioporus australiacus", "Pycnoptilus floccosus", "Harrisoniascincus zia",
    "Maccullochella ikei", "Dasyornis brachypterus", "Petrogale penicillata",
    "Litoria verreauxii alpina", "Potorous tridactylus tridactylus",
    "Pseudomys novaehollandiae", "Nannoperca oxleyana", "Macquaria australasica",
    "Petauroides volans", "Pseudomys fumeus", "Dasyurus maculatus maculatus (SE mainland population)",
    "Mixophyes iteratus", "Isoodon obesulus obesulus", "Mastacomys fuscus mordicus",
    "Litoria aurea", "Litoria spenceri", "Litoria piperata", "Pseudomugil mellis",
    "Chalinolobus dwyeri", "Cyclopsitta diophthalma coxeni", "Hirundapus caudacutus",
    "Burramys parvus", "Litoria olongburensis", "Pteropus poliocephalus",
    "Mixophyes fleayi", "Litoria booroolongensis", "Prototroctes maraena",
    "Phascolarctos cinereus (combined populations of Qld, NSW and the ACT)", "Cyclodomorphus praealtus",
    "Botaurus poiciloptilus", "Turnix melanogaster", "Myrmecobius fasciatus",
    "Delma torquata", "Uvidicolus sphyrurus", "Nannoperca pygmaea",
    "Wollumbinia belli", "Rostratula australis", "Cereopsis novaehollandiae grisea",
    "Phascogale calura", "Litoria raniformis", "Thinornis cucullatus cucullatus",
    "Limosa lapponica baueri", "Setonix brachyurus", "Epinephelus daemelii",
    "Calyptorhynchus baudinii", "Grantiella picta", "Erythrotriorchis radiatus",
    "Parantechinus apicalis", "Dasyurus geoffroii", "Liopholis guthega",
    "Furina dunmalli", "Aprasia parapulchella", "Galaxias truttaceus (Western Australian population)",
    "Maccullochella mariensis", "Galaxiella nigrostriata", "Nannatherina balstoni",
    "Galaxias parvus", "Maccullochella peelii", "Stipiturus malachurus intermedius",
    "Calidris canutus", "Aquila audax fleayi", "Calyptorhynchus latirostris",
    "Sternula nereis nereis", "Tyto novaehollandiae castanops (Tasmanian population)",
    "Calyptorhynchus banksii naso", "Geophaps scripta scripta", "Leipoa ocellata",
    "Petrogale persephone", "Petrogale lateralis lateralis", "Bettongia penicillata ogilbyi",
    "Pseudomys shortridgei", "Dasyurus viverrinus", "Sarcophilus harrisii",
    "Antechinus minimus maritimus", "Xeromys myoides", "Perameles gunnii gunnii",
    "Dasyurus maculatus maculatus (Tasmanian population)", "Nyctophilus corbeni",
    "Coeranoscincus reticulatus", "Neoceratodus forsteri"
  ),
  Common.name = c(
    "Kate’s Leaf-tailed Gecko", "Kangaroo Island Grey-bellied Dunnart", "Long-footed Potoroo",
    "Kangaroo Island Glossy Black-cockatoo", "Pugh’s Mountain Frog", "Hastings River Mouse",
    "Sphagnum Frog", "Mountain Brushtail Possum", "Kangaroo Island Bassian Thrush",
    "Littlejohn’s Tree Frog", "Tyler’s Toadlet", "Rufous Scrub-bird",
    "Growling Grass Frog (New England population)", "Kangaroo Island Short-beaked Echidna",
    "Broad-headed Snake", "Blue Mountains Water Skink", "Blue Mountains Tree Frog",
    "Eastern False Pipistrelle", "Stuttering Frog", "Red-crowned Toadlet",
    "Jervis Bay Tree Frog", "Papua Tube-nosed Bat", "Pink-bellied Snake",
    "Giant Burrowing Frog", "Pilotbird", "Border Ranges Cool-skink",
    "Clarence River Cod", "Eastern Bristlebird", "Brush-tailed Rock-wallaby",
    "Alpine Tree Frog", "Long-nosed Potoroo (SE mainland population)",
    "New Holland Mouse", "Oxleyan Pygmy Perch", "Macquarie Perch",
    "Greater Glider", "Smoky Mouse", "Spot-tailed Quoll (SE mainland population)",
    "Giant Barred River Frog", "Southern Brown Bandicoot (eastern subspecies)", "Broad-toothed Rat (swamp population)",
    "Green and Golden Bell Frog", "Spotted Tree Frog", "Peppered Tree Frog", "Honey Blue-eye",
    "Large-eared Pied Bat", "Coxen's Fig-parrot", "White-throated Needletail",
    "Mountain Pygmy-possum", "Olongburra Frog", "Grey-headed Flying-fox",
    "Fleay's Barred Frog", "Booroolong Frog", "Australian Grayling",
    "Koala (combined populations of Qld, NSW and the ACT)", "Alpine She-oak Skink",
    "Australasian Bittern", "Black-breasted Button-quail", "Numbat",
    "Collared Delma", "Border Thick-toed Gecko", "Little Pygmy Perch",
    "Bellinger River Turtle", "Australian Painted Snipe", "Cape Barren Goose (Tasmanian population)",
    "Red-tailed Phascogale", "Southern Bell Frog", "Hooded Plover (eastern subspecies)",
    "Bar-tailed Godwit (Eastern Australian population)", "Quokka", "Blacktip Rockcod",
    "Baudin's Black-cockatoo", "Painted Honeyeater", "Red Goshawk",
    "Dibbler", "Western Quoll", "Guthega Skink",
    "Dunmall's Snake", "Pink-tailed Worm-lizard", "Kimberley Galaxias (WA population)",
    "Mary River Cod", "Black-stripe Dwarf Galaxias", "Balston's Pygmy Perch",
    "Dwarf Galaxias", "Murray Cod", "Chestnut-breasted Rufous Calyptomena (intermediate subspecies)",
    "Red Knot", "Tasmanian Wedge-tailed Eagle", "Carnaby's Black-cockatoo",
    "Fairy Tern (Australian population)", "Tasmanian Masked Owl",
    "Northern Glossy Black-cockatoo", "Squatter Pigeon (southern subspecies)", "Malleefowl",
    "Proserpine Rock-wallaby", "Black-flanked Rock-wallaby (MacDonnell Ranges)", "Brush-tailed Bettong (western subspecies)",
    "Heath Rat", "Eastern Quoll", "Tasmanian Devil",
    "Swamp Antechinus (swamp population)", "Water Mouse", "Eastern Barred Bandicoot (Tasmanian population)",
    "Spot-tailed Quoll (Tasmanian population)", "Corben's Long-eared Bat",
    "Three-toed Snake-tooth Skink", "Queensland Lungfish"
  ),
  stringsAsFactors = FALSE
)

# Step 4: Merge the common names with the dataset
data_updated <- data %>%
  left_join(common_names, by = "Scientific.name") %>%
  # Reorder columns to place Common.name next to Scientific.name
  select(Scientific.name, Common.name, everything())

# Step 5: Handle missing common names (fallback to scientific name)
data_updated$Common.name[is.na(data_updated$Common.name)] <- data_updated$Scientific.name[is.na(data_updated$Common.name)]

# Step 6: Display and save
# Show first few rows
head(data_updated)
View(data_updated)
# Save to new CSV
write.csv(data_updated, "filtered_fire_impact1.csv", row.names = FALSE)

# Optional: Summary of matches
cat("Total species in dataset:", nrow(data_updated), "\n")
cat("Species with common names:", sum(!is.na(data_updated$Common.name)), "\n")








# Install if not already
install.packages("taxize")
library(taxize)

# Read your CSV file
df <- read.csv("filtered_fire_impact.csv")

# Extract the scientific names column
species <- unique(df$Scientific.name)

# Query for common names
common_names <- sapply(species, function(x) {
  res <- tryCatch({
    comm <- taxize::commonnames(x)
    if (length(comm) > 0) comm[1] else NA
  }, error = function(e) NA)
  return(res)
})

# Combine results
name_map <- data.frame(
  Scientific.name = species,
  Common.name = common_names,
  stringsAsFactors = FALSE
)

# Join back to original dataframe
df <- merge(df, name_map, by = "Scientific.name", all.x = TRUE)

# View results
head(df[, c("Scientific.name", "Common.name", "Threat.status", "Taxonomic.group")])

# Export for use in Vega-Lite
write.csv(df, "filtered_fire_impact_common.csv", row.names = FALSE)
