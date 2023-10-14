# Toronto Overnight Shelter Data Project (R)
---

## Project Overview

This repo contains the R project for modeling overnight service occupancy in homeless shelters in the city of Toronto. Note that is R projects is used along with the [DBT](https://github.com/LucasO21/toronto_shelter_project) project with housed the data models for the project. 

### Objectives
- **Data Extraction:** Extract data from [Open Data Toronto](https://open.toronto.ca/dataset/daily-shelter-overnight-service-occupancy-capacity/) API. Also extract weather forecast data from [AccuWeather]() API.
- **Data Loading:** Load raw data into Big Query for further transformation using DBT
- **Machine Learning:** Extract transformed data from Big Query. Use [H2o AutoML]() to create machine learning models to predict overnight shelter occupancy for the next 5 days based on multiple features.
- **Data Reporting:** Report on predictions using R related technologies and packages like Quarto, datatable, etc

## Data Sources

- **Shelter Data:** [Toronto Open Data](https://open.toronto.ca/dataset/daily-shelter-overnight-service-occupancy-capacity/)
- **Weather Data:** [NOAA Big Query Data](https://console.cloud.google.com/marketplace/product/noaa-public/gsod?project=toronto-shelter-project) for historical weather data and [AccuWeather API](https://developer.accuweather.com/) for weather forecast data.

- ## Technology Stack

- **[DBT](https://www.getdbt.com/)**: For data transformation and modeling.
- **[Google BigQuery](https://cloud.google.com/bigquery)**: As the data warehouse for storing and querying the dataset.
- **[Posit](https://posit.co/)**: For data manipulation using multiple packagers like `tidyverse`, `data.table`, `DT`, `DBI`, `h2o`.
- **[H2o AutoML](https://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html)**: For building machine learning models
- **[Quarto](https://quarto.org/index.html)**: For reporting

- ## Repository Structure

- `/scripts`: Contains scripts for analysis.
- `/functions`: Contains functions to modularize code.
- `/qmd`: Contains Quarto documents
  

## Resources

Coming soon...
