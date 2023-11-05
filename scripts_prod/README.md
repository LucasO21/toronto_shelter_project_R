# Production Scripts Overview

---

This directory hosts the essential scripts that support daily operations for shelter 
data management and predictive tasks.

* **00_extract_shelter_data_prod.R:** Automates the retrieval of raw shelter data from the 
Open Data Toronto API.

* **01_extract_weather_forecast_prod.R:** Gathers a 5-day weather forecast from the AccuWeather API, 
integral for planning and preparedness.

* **02_prediction_prod.R:** Performs data feature extraction from BigQuery and forecasts overnight 
shelter occupancy for the upcoming five days. Subsequently, it uploads these forecasts to 
BigQuery, integrating them for visualization in a Shiny application.
