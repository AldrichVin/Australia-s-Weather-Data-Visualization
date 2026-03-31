# Data Visualisation of Australia's Weather

Interactive analytical dashboards exploring Australia's climate patterns, bushfire hotspots, and koala habitat distribution using geospatial and time-series visualisations.

**Live Demo:** [https://alie0025.github.io/DV2_Final/](https://alie0025.github.io/DV2_Final/)

## Overview

This project analyses publicly available climate and GIS datasets to surface environmental insights across Australia. It combines multiple data sources — temperature anomalies, fire impact records, rainfall patterns, and wildlife habitat data — into a cohesive interactive dashboard.

### Key Visualisations

- **Geospatial choropleth maps** of temperature anomalies and fire impact zones across Australian states
- **Bushfire hotspot analysis** with casualty data overlay
- **Koala habitat distribution** using high-resolution spatial data
- **Radar charts** comparing climate drivers across regions
- **Time-series analysis** of weather patterns and seasonal trends

## Tech Stack

- **Data Preparation:** R (cleaning, transformation, aggregation)
- **Visualisation:** Vega-Lite, Vega (declarative grammar of interactive graphics)
- **Frontend:** HTML, CSS, JavaScript
- **Data Formats:** CSV, GeoJSON (Natural Earth shapefiles)

## Datasets

| File | Description |
|------|-------------|
| `country_temp_anomaly_by_year.csv` | Temperature anomalies by country and year |
| `fire_dataset.csv` | Australian fire incident records |
| `filtered_fire_impact3.csv` | Processed fire impact data with casualties |
| `bubble_plot_dataset_casualties.csv` | Casualty data for bubble plot visualisation |
| `climate_drivers_with_rain.csv` | Climate drivers correlated with rainfall |
| `radar_chart.csv` | Multi-dimensional climate comparison data |
| `koala_habitat_core.json` | Koala habitat distribution (GeoJSON) |
| `ne_10m_admin_1_states_provinces.json` | Australian state boundaries (Natural Earth) |
| `ne_110m_admin_0_countries.json` | Country boundaries (Natural Earth) |

## How to Run

1. Clone the repository
2. Open `index.html` in a browser (no server required)
3. Interact with the dashboards using cross-filtering, parameter controls, and tooltips

## Project Context

Built as part of the Data Visualisation unit at Monash University (FIT3179), this project demonstrates ETL pipeline design, geospatial analysis, and interactive data storytelling.
