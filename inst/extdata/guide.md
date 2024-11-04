# Welcome to the DPM Dashboard

This interactive dashboard empowers you to explore and model your data using the dynamic population model (DPM). This guide will walk you through the features and functionalities of the dashboard, helping you make the most of your data insights.

## Introduction

The Dynamic Population Model (DPM) aims to provide a coherent statistical demographic accounting framework. The core of this framework is a set of internally consistent estimates of population, births, deaths, and migration by age, sex, and time.

The first step in producing the population estimate is to define the system models, these are constructed using estimated rates of births, deaths, and migration, and associated measures of uncertainty that approximate what a skilled analyst would know about such demographic trends.

The next step is to estimate statistical data models for population stocks and flows. They incorporate the variability seen in the data because of systematic inaccuracies, including coverage and reporting error. These statistical models approximate what a skilled analyst would know about the quality of the data sources.


## Getting Started

### 1. Global Configuration

- Sets useful global parameters for the subsequent model running 
  - global_data_dir : Defaults to the /data directory for dummy data, set to location of any data used for system/data models.
  - global_output_dir : Defaults to the /output directory, set to location to save results.RDS file/.csv file from 'Fit Model' tab.
  - global_time_selection : Defaults to empty, optional parameter to subset all data to for testing (comma separated years).
  - global_seed_value : Defaults to a random prime number, can be used for reproducibility.

### 2. DPM Specification

- The DPM requires both system and data models. Whereas the system models are always consistent (with 4 system models always required), the data models are more open-ended. Aside from 'births' and 'deaths' the DPM does not have any direct requirements for data models. However, the premise of the DPM is that its performance will increase with the quantity/quality of the data models, so we would recommend having at least data models for the two required 'births' and 'deaths', but also for at least 1 stock ('population') and data models for flows (immigration ('ins') and emigration ('outs'))

#### 2.1 System Models

- Define the 4 required system models for births, deaths, ins, and outs.
- Rate dispersion can be a single numeric value or can be a table if data is available.

#### 2.2 Data Models

- Define data models for births & deaths (required)
- Define any optional data models from admin data (e.g. stocks, flows (ins, outs etc))
- Can select from the currently available data models (from accountTMB)
  - Exact Data Model
  - Normal Data Model
  - T-Dist Data Model
  - Negative Binomial Data Model
  - Poisson Data Model
- Can add more data models to the list than you will use in the model.
- Unwanted data models can be deleted from the list here as well, data models in the dashboard are defined by their 'dm_name' value so these must be unique. 

### 3. DPM Running

#### 3.1 Fit Models

- The loaded system models will show at the top.
- Select the required data models from those loaded using the checkboxes.
- Fit the model.
- The population and migration tables should populate.

#### 3.2 Population Estimates

- Interactive population plot, can select different 'Time', 'Sex', and a column to compare against (e.g. 'stock' if one was used)

#### 3.3 Migration Estimates

- Interactive migration plots (immigration, emigration), can select different 'Time', 'Sex', and a column to compare against (e.g. 'immig'/'emig' if available.)
