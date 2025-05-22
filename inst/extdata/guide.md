
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Welcome to the DPM Dashboard

This interactive dashboard empowers you to explore and model your data
using the dynamic population model (DPM) framework provided by the
`accountTMB` R package. This guide will walk you through the features
and functionalities of the dashboard, helping you to configure models,
run estimations, compare different setups, and analyse model sensitivity
efficiently.

## 0. Introduction

The Dynamic Population Model (DPM) aims to provide a **coherent
statistical demographic accounting framework**. The core of this
framework is a set of internally consistent estimates of population,
births, deaths, and migration by age, sex, and time.

The dashboard facilitates a structured approach to demographic
accounting:

1.  **System Models Definition**: The first step is to define system
    models. These are constructed using estimated rates of births,
    deaths, and migration (ins and outs), along with associated measures
    of uncertainty in the form of dispersions. These models approximate
    what a skilled analyst would know or assume about underlying
    demographic trends and their regularity.
2.  **Data Models Specification**: The next step is to specify
    statistical data models for observed population stocks and
    demographic flows (e.g., births, deaths, migration counts from
    administrative sources). These models incorporate the variability
    and potential biases seen in the data due to factors like coverage
    issues, reporting errors, or definitional differences. They
    approximate what a skilled analyst would know about the quality and
    characteristics of the available data sources.
3.  **Model Fitting & Estimation**: Using the defined system and data
    models, the `accountTMB` package fits the demographic account,
    reconciling different data sources and prior knowledge to produce
    coherent estimates.
4.  **Results Exploration & Comparison**: The dashboard allows for
    detailed exploration of these estimates, including population
    structures and migration flows, and provides tools to compare
    results from different model configurations.
5.  **Sensitivity Analysis**: Finally, tools are provided to assess how
    sensitive the model outputs are to changes in key input parameters.

This dashboard aims to streamline this process, making the powerful
features of `accountTMB` more accessible.

------------------------------------------------------------------------

## 1. Getting Started

This section guides you through each tab of the dashboard.

### 1.1 Global Configuration Tab

This tab is the starting point for any analysis. It sets essential
global parameters that will be used throughout your session.

- **Input Data Directory (`global_data_dir`)**:
  - **Purpose**: Specifies the directory where your input data files
    (e.g., CSVs for rates, counts, dispersions) are located.
  - **Default**: `/data` directory within the application’s structure
    (intended for dummy/example data).
  - **Usage**: **Crucially, set this to the path on your system where
    your actual data files are stored.** All subsequent file inputs in
    other tabs (like system model rates files or data model counts
    files) will be expected to be found relative to this directory.
- **Output Directory (`global_output_dir`)**:
  - **Purpose**: Specifies the directory where results and exported
    files will be saved.
  - **Default**: `/output` directory within the application’s structure.
  - **Usage**: Set this to your preferred location for saving model
    outputs (e.g., `.RDS` files of fitted models, `.csv` files of
    estimates, exported model setups).
- **Time Selection (`global_time_selection`)**:
  - **Purpose**: An optional parameter to subset all input data to a
    specific range or set of time periods (typically years). This is
    useful for testing, quick runs, or focusing on a specific historical
    period.
  - **Default**: Empty (all time periods in the data will be used).
  - **Format**: Enter as comma-separated years, e.g., `2000,2001,2005`
    or a range like `2000,2001,2002,2003,2004,2005`.
- **Seed Value (`global_seed_value`)**:
  - **Purpose**: Sets the random seed for any stochastic processes
    within the model fitting (e.g., in `accountTMB`). Using a specific
    seed ensures reproducibility of your results.
  - **Default**: A randomly generated prime number (changes with each
    session).
  - **Usage**: For reproducible research or debugging, set this to a
    specific integer.

**Action**: After setting these parameters, click **“Save Global
Configuration”**.

------------------------------------------------------------------------

### 1.2 DPM Specification

This section is divided into two sub-tabs for defining the core
components of your demographic model.

#### 1.2.1 System Models Setup Tab

System models represent your prior knowledge and assumptions about the
underlying demographic rates. You need to define a **set of four system
models**: one each for births, deaths, immigration (ins), and emigration
(outs).

- **Defining a System Model Set**:
  - For each of the four components (births, deaths, ins, outs):
    - **Rates CSV Filename**: Provide the name of the `.csv` file
      containing the rates data. This file should be in the **Input Data
      Directory** (specified in Global Config) and generally include
      columns for `age`, `sex` (except for births), `time`, and `rate`.
    - **Dispersion Input Type**: Dispersion represents the uncertainty
      associated with the rates.
      - **Single Value**: If you assume a constant dispersion across all
        ages/sexes/times for that rate, select this and enter a numeric
        value (e.g., `0.05`).
      - **CSV File**: If you have specific dispersion values (e.g.,
        varying by age or time), provide the name of a `.csv` file
        containing this data. The file should have a structure
        compatible with what `accountTMB` expects for dispersion.
    - **Optional Parameters (Collapsible Section)**:
      - **Lower Rate Limit**: Minimum plausible rate. Any rate in your
        input file below this will be imputed with this limit. Default:
        `1e-6`.
      - **Rate Scaler**: A factor to multiply the rates by. Useful for
        adjustments or scenario testing. Default: `1`.
      - **Rate Set**: A value to override all rates if needed (e.g., for
        specific debugging). Default: `-1` (not applied).
      - **Noise Set**: Adds normally distributed random noise to the
        rates, value set is the standard deviation of the normal noise
        to add. Default: `0`.
  - **System Models Setup Name**: Assign a unique name to this
    collection of four system models (e.g., “Baseline_HighFertility”,
    “UK_Standard_Assumptions”). This name is used for exporting the
    setup and selecting it later for model fitting.
- **Actions**:
  - **“Create System Models”**: After filling in the details for all
    four components and providing a setup name, click this to create the
    set. Upon creation:
    - The setup will be added to the “Available System Models” list.
    - Summaries and plots for each of the four created system model
      components (based on their mean rates) will be displayed below,
      allowing you to visually inspect your inputs.
  - **“Export System Models”**: Saves the **currently defined set** (the
    one you just created or the one whose parameters are currently in
    the input fields) to an `.RDS` file in your **Output Directory**.
    The filename will be `<setup_name>_sysmods.RDS`.
- **Managing System Models**:
  - **Available System Models**: This box lists all system model setups
    you have created or imported during the current session.
  - **Delete Selected System Models**: Choose a setup from the dropdown
    and click this to remove it from the session.
  - **Import System Models**:
    - **System Model list .RDS file**: Enter the filename of a
      previously exported system model setup (`.RDS` file). This file
      should be in your **Input Data Directory**.
    - **“Import Selected System Models”**: Click to load the setup. It
      will appear in the “Available System Models” list.

#### 1.2.2 Data Models Setup Tab

Data models describe the characteristics of your observed data sources
(population stocks, demographic flows).

- **Required Data Models (Births & Deaths)**:
  - The DPM typically requires “Exact” data models for **births** and
    **deaths** flows as these are considered exact counts collected via
    vital registrations.
  - For each:
    - Provide the **Counts CSV Filename**.
    - Click the respective **“Create \[births/deaths\] Data Model”**
      button.
- **Adding More Data Models (Stocks and other Flows)**:
  - Use the “Add More Data Models” section for population stocks or
    other flows like immigration/emigration counts from administrative
    data.
  - **Required Inputs**:
    - **Data Model Name (`dm_name`)**: A unique name for this specific
      data model (e.g., “Population_Census_2021”,
      “Admin_Immigration_Data”). **This name must be unique across all
      data models.**
    - **Choose Series Type**: Select the type of demographic series this
      data represents: `population` (for stocks), `ins` (for immigration
      flows), or `outs` (for emigration flows).
    - **Choose Data Model**: Select the statistical distribution that
      best describes the error structure of your data source. Options
      available from `accountTMB` include:
      - `Exact Data Model`: Assumes data is perfectly accurate (no
        error).
      - `Normal Data Model`: Assumes errors are normally distributed.
        Requires an uncertainty (SD or variance) CSV.
      - `T-Dist Data Model`: Assumes errors follow a t-distribution
        (heavier tails than normal). Requires scale parameter input.
      - `Negative Binomial Data Model`: Suitable for overdispersed count
        data. Requires dispersion parameter input.
      - `Poisson Data Model`: Suitable for count data where mean equals
        variance.
      - `Log-Normal Data Model`: Assumes errors are log-normally
        distributed. Requires an uncertainty CSV for the log scale.
    - **Counts CSV Filename**: The name of the `.csv` file containing
      the observed counts for this data model.
    - **Additional Inputs (Dynamic)**: Depending on the “Choose Data
      Model” selection, further inputs might appear (e.g., filename for
      uncertainty/scale/dispersion CSVs, or direct numeric input for
      parameters).
  - **Optional Inputs (Collapsible Section)**:
    - **Coverage Ratio Type**: Specify if the coverage ratio (adjustment
      for under/over-count) is a single value or comes from a CSV file.
    - **Scale Ratio**: A parameter available in `accountTMB` to account
      for potential issues with coverage adjustment of stocks/flows data
      (e.g. scale_ratio = 0.05 can be used to account for a
      misadjustment on 5%).
    - **Count Scaler**: Allows for scaling the stock/flow counts.
    - **Age/Time Selection**: Strings to subset the data from this
      specific data model (e.g., `0:10,65:80` for ages, `2010,2015:2019`
      for time).
    - **Minimum SD**: A parameter to set a minumum uncertainty to apply
      for data models with set uncertainties (e.g. Normal/Log-Normal).
    - **SD Scaler**: A parameter to scale all uncertainty values by a
      specified factor.
    - **SD Set**: A parameter to overide the specified uncertainty with
      a single fixed value for all the data in the data model.
  - **“Create Data Model”**: Click to add this data model to your
    session.
- **Managing Data Models**:
  - **Loaded Data Models**: A table displays all data models currently
    loaded in the session, showing their name, series type, model type,
    and data columns.
  - **Delete Selected Data Model**: Choose a data model from the
    dropdown and click to remove it.
  - **Export/Import Data Models**:
    - **Export**: Provide a “Data Models list tag” (e.g.,
      “baseline_models”) and click **“Export Data Models”**. This saves
      all currently loaded data models as a single list in an `.RDS`
      file named `<tag>_datamods.RDS` in your **Output Directory**.
    - **Import**: Provide the “Data Model list .RDS file” name (expected
      in your **Input Data Directory**) and click **“Import Selected
      Data Models”**.
  - **Plot Data Model Data**: Select a data model and click **“Plot
    Selected Data Model”** to generate an interactive plot of its counts
    by age, sex, and time.
  - **Inspect Data Model Data**: Select a data model and click
    **“Inspect Selected Data Model”** to view its underlying data in an
    interactive table. Two inspection panels are available for comparing
    different data models.

------------------------------------------------------------------------

### 1.3 DPM Running Tab

This tab is where you fit the demographic accounting model using your
specified system and data models.

- **Model Setup Selection**:
  - **Select System Models**: Choose **one** system model setup (created
    in tab 1.2.1) from the radio buttons. This setup (comprising births,
    deaths, ins, and outs system models) will be used for the fit.
  - **Select Data Models**: Choose **one or more** data models (created
    in tab 1.2.2) using the checkboxes. These represent the observed
    data the model will try to reconcile. You must include data models
    that cover births, deaths, and population stocks for a standard run.
- **Action**:
  - Click **“Fit Model”**. This initiates the
    `accountTMB::estimate_account` function. A progress bar will
    indicate the status.
- **Results Display**:
  - **Cohort Results**: Shows a summary of how many cohorts were
    successfully fitted by the model.
  - **Cohort Failures**: If any cohorts failed to converge or had
    issues, they will be listed in this table with diagnostic
    information.
  - **Model Summary Tables**:
    - **Population Table**: Displays the augmented population estimates
      (fitted values, lower/upper bounds) by age, sex, and time.
    - **Migration Table**: Displays the augmented migration (ins/outs)
      and other event estimates.
  - **Saved Outputs**: Upon successful fitting, the raw model result
    object is saved as `fit_model_result.RDS`, and the augmented
    population and migration estimates are saved as
    `population_estimates.csv` and `migration_estimates.csv`
    respectively, in your **Output Directory**.

#### 1.3.1 Population Estimates Sub-Tab

This sub-tab provides an interactive visualisation of the population
estimates from the last model fit on the “Fit Model” tab.

- **Controls**:
  - **Time**: Select a specific time period to display.
  - **Compare Column**: Optionally, enter the name of a column from your
    input data (that was included in the `population_estimates.csv` by
    `accountTMB`, often an original stock count data model output) to
    overlay onto the plot for comparison against the fitted estimates
    (e.g., `stock_observed`).
- **Plot**: An interactive plot shows the population counts by age and
  sex for the selected time, including confidence intervals for the
  fitted estimates. If a “Compare Column” is specified and valid, those
  points will also be shown.

#### 1.3.2 Migration Estimates Sub-Tab

Visualises immigration and emigration estimates from the last model fit.

- **Controls**:
  - **Time**: Select a specific time period.
  - **Compare Ins**: Optionally, enter a column name to compare
    immigration estimates against.
  - **Compare Outs**: Optionally, enter a column name to compare
    emigration estimates against.
- **Plots**: Two interactive plots are displayed:
  - Immigration estimates by age and sex.
  - Emigration estimates by age and sex. Both include confidence
    intervals and can overlay comparison data.

------------------------------------------------------------------------

### 1.4 Compare Setups Tab

This section allows you to fit two different DPM configurations
side-by-side and compare their outputs.

#### 1.4.1 Fit Models Sub-Tab

Here, you define and run the two setups you wish to compare.

- **Setup (1) & Setup (2) Selection**:
  - For each setup (1 and 2):
    - **System Models**: Select one system model setup.
    - **Data Models**: Select one or more data models.
- **Action**:
  - Click **“Compare Models”**. The dashboard will fit both setups
    sequentially using `accountTMB::estimate_account`.
- **Results Display**:
  - **Aggregate estimates comparisons**: A series of plots will appear,
    showing aggregated comparisons (e.g., total population, total
    immigration/emigration, net migration over time) between Setup 1 and
    Setup 2. It also includes plots of the differences (absolute and
    percent) in these aggregate estimates.

#### 1.4.2 Compare Population Estimates Sub-Tab

Visualise detailed population estimate comparisons from the two setups
run in 1.4.1.

- **Controls**:
  - **Time**: Select the time period for comparison.
  - **Setup**: Choose to display “Both” setups, “Setup 1” only, or
    “Setup 2” only.
  - **Residual Type**: For the residual plot, choose “Absolute”
    difference (Setup 2 - Setup 1) or “Percent” difference.
  - **Compare Column 1 / Compare Column 2**: Optionally, specify column
    names from your original input data to overlay on the plots for
    Setup 1 and Setup 2 respectively.
- **Plots**:
  - **Overlay Plot**: Shows population estimates (with CIs) for the
    selected setup(s) by age and sex.
  - **Residual Plot**: Shows the difference (absolute or percent)
    between population estimates of Setup 2 and Setup 1 by age and sex.

#### 1.4.3 Compare Migration Estimates Sub-Tab

Visualise detailed migration estimate comparisons.

- **Controls**: Similar to population comparison (Time, Setup, Residual
  Type).
  - **Compare Ins 1/2, Compare Outs 1/2**: Optional comparison columns
    for immigration and emigration for each setup.
- **Plots**:
  - **Immigration Overlay & Residual Plots**.
  - **Emigration Overlay & Residual Plots**.

------------------------------------------------------------------------

### 1.5 Model Sensitivity Analysis Tab

This powerful feature allows you to assess how sensitive your model
outputs are to changes in a specific input parameter.

#### 1.5.1 Setup & Run Sub-Tab

Configure and execute the sensitivity analysis.

- **Base Model Selection**:
  - **Select Base System Model Setup**: Choose one of your existing
    system model setups to serve as the baseline.
  - **Select Base Data Models**: Choose the set of data models that,
    along with the base system models, form your baseline DPM
    specification.
- **Parameter Variation**: You can choose to vary **either** a system
  model parameter **or** a data model parameter.
  - **System Model Parameter Sensitivity**:
    - **Select System Model Parameter to Vary**: Choose a parameter from
      the dropdown (e.g., “Dispersion (Births)”, “Lower Rate Limit
      (Deaths)”, “Rate Scaler (Ins)”).
    - **Parameter Range**: Dynamically generated input fields will
      appear. Specify the **Min** value, **Max** value, and **Number of
      Steps** for the chosen parameter. The dashboard will test the
      model at equally spaced values within this range.
  - **Data Model Parameter Sensitivity**:
    - **Select Data Model to Target**: Choose which of your selected
      base data models will have its parameter varied.
    - **Select Data Model Parameter to Vary**: Based on the target data
      model’s type, a list of variable parameters will appear (e.g.,
      “Count Scaler”, “SD Scaler”, “Coverage Ratio”, “Dispersion”).
    - **Parameter Range**: Similar to system model parameters, specify
      the Min, Max, and Number of Steps.
- **Action**:
  - Click **“Run Sensitivity Analysis”**. The dashboard will:
    1.  Take your base model configuration.
    2.  For each step in the specified parameter range:
        1.  Modify the chosen parameter to the current step’s value.
        2.  Re-run the `accountTMB::estimate_account` function with this
            modified configuration.
        3.  Store the key results (augmented population and migration).
  - **Feedback**: A status message will indicate the progress and
    completion of the analysis. This can take some time if many steps or
    complex models are involved.

#### 1.5.2 Population Results Sub-Tab

Visualise how population estimates change as the selected parameter
varies.

- **Controls**:
  - **Parameter to Display on Plot**: This should automatically reflect
    the parameter you varied.
  - **Select Time**: Choose a specific time period for which to view the
    sensitivity.
- **Plot**: An interactive plot will display multiple population
  estimates (e.g., population counts). Each line/ribbon will be colored
  according to the different values of the parameter you varied in the
  “Setup & Run” tab. This allows you to visually assess how much the
  population estimates change in response to changes in that input
  parameter.

#### 1.5.3 Migration Results Sub-Tab

Visualise how migration estimates (immigration and emigration) change.

- **Controls**: Similar to Population Results (Parameter to Display,
  Select Time).
- **Plots**: Two interactive plots are generated:
  - One for **Immigration Sensitivity**.
  - One for **Emigration Sensitivity**. Each plot shows multiple
    migration estimates by age and sex, colored by the different values
    of the varied parameter.

------------------------------------------------------------------------

## 2. FAQs

- **Q: My input CSV file (e.g., for rates or counts) is not found, but
  I’m sure the filename is correct. What’s wrong?**
  - A: Double-check the **Input Data Directory** in the “Global
    Configuration” tab. All filenames you provide in other tabs are
    interpreted as being *inside* this directory. Ensure this global
    path is correctly set to where your files are actually stored on
    your computer.
- **Q: I’ve filled in all the system model details, but the “Create
  System Models” button doesn’t seem to do anything, or I get an
  error.**
  - A: Ensure you have provided valid inputs for all four components
    (births, deaths, ins, outs), including both the rates CSV filename
    and a dispersion input (either a single value or a dispersion CSV
    filename). Also, make sure you’ve given a “System Models Setup
    Name”. Check the console for any error messages from R if the
    problem persists. The rates and dispersion CSV files must exist in
    the specified Input Data Directory.
- **Q: How do I use a CSV file for dispersion (in System Models) or for
  a coverage ratio (in Data Models) instead of just typing a single
  number?**
  - A: In the relevant section, there will be a radio button or similar
    selector (e.g., “Dispersion Input Type” or “Coverage Ratio Type”).
    Choose the “CSV File” option. A text input field will then appear
    where you should type the filename of the CSV. This file must be
    located in your global **Input Data Directory**.
- **Q: What do “Rate Scaler” or “Lower Rate Limit” in the System Models
  Optional Inputs do?**
  - A:
    - **Lower Rate Limit**: This sets a floor for your input rates. If
      any rate in your CSV is below this value, it will be replaced by
      this limit before being used in the model. This can prevent issues
      with extremely low or zero rates.
    - **Rate Scaler**: This is a multiplier applied to all rates in that
      component. A value of `1` means no change. A value of `1.1` would
      increase all rates by 10%. This is useful for simple scenario
      testing (e.g., “what if death rates were 5% lower?”).
- **Q: Which “Data Model” type should I choose for my population stock
  data (e.g., census counts)?**
  - A: The choice depends on your assumptions about the error in that
    data:
    - `Exact Data Model`: If you believe the stock data is perfectly
      accurate (rarely the case for admin data).
    - `Normal Data Model`: If you assume errors are symmetrically
      distributed around the true value and you can provide estimates of
      the standard deviation (or variance) of these errors in a separate
      CSV file.
    - `Log-Normal Data Model`: If errors are multiplicative and the log
      of the counts is normally distributed. Also requires an
      uncertainty CSV (for the log scale).
    - `T-Dist Data Model`: If you suspect errors might be symmetric but
      with more extreme outliers (heavier tails) than a normal
      distribution would suggest. You’ll need to provide a ‘scale’
      parameter (similar to SD) and degrees of freedom (often fixed
      within the `accountTMB` functions using this).
    - Consider the nature of your data and the likely sources and
      magnitude of error when choosing.
- **Q: I’ve clicked “Fit Model” (or “Compare Models” / “Run Sensitivity
  Analysis”), but I don’t see any plots or tables updating in the
  results tabs.**
  - A:
    1.  Check the main R console where you launched the Shiny app. There
        might be error messages from `accountTMB` or other R functions
        that didn’t appear as pop-ups in the dashboard.
    2.  In the “Fit Model” tab, look at the “Cohort Results” and “Cohort
        Failures” tables. If many cohorts failed, the overall estimation
        might be problematic, leading to no valid augmented results to
        display.
    3.  Ensure that your input data (rates, counts, dispersions) are
        correctly formatted and cover the necessary age, sex, and time
        dimensions without large gaps or inconsistencies.
- **Q: In the “Compare Setups” tab, what do the residual plots show?**
  - A: The residual plots show the difference between the estimates from
    “Setup 2” and “Setup 1”.
    - **Absolute Residuals**: `Estimate_Setup2 - Estimate_Setup1`.
    - **Percent Residuals**:
      `100 * (Estimate_Setup2 - Estimate_Setup1) / Estimate_Setup1`.
      These help you quantify where and by how much the two model
      configurations differ in their outputs.
- **Q: How do I interpret the plots in the “Model Sensitivity Analysis”
  results tabs?**
  - A: Each line on these plots represents a full model run where the
    parameter you selected was held at a specific value (from the range
    you defined).
    - If the lines are very close together, your model results are **not
      very sensitive** to changes in that parameter within the tested
      range.
    - If the lines are spread far apart, or show significantly different
      shapes, your model results are **highly sensitive** to that
      parameter. This might indicate a need for more precise estimation
      of that parameter or further investigation.
- **Q: Where are my detailed results (like CSV files or the main RDS
  model object) saved?**
  - A: All files generated by actions like “Fit Model”, “Export System
    Models”, or “Export Data Models” are saved in the **Output
    Directory** that you specified in the “Global Configuration” tab.

------------------------------------------------------------------------

## 3. Additional Resources

- **`accountTMB` Package Documentation**: For detailed information on
  the underlying functions, model specifications, and theoretical
  background, please refer to the official documentation for the
  `accountTMB` R package.
  - [View the accountTMB package on GitHub for detailed vignettes and
    function help.](https://github.com/ONSdigital/accountTMB)
- **Demographic Accounting Principles**: Understanding the principles of
  demographic accounting can greatly enhance your use of this tool.
  Consider resources on:
  - Coherent demographic estimation and population reconciliation.
  - Bayesian approaches to demographic modeling.
  - Relevant literature such as:
    - Bryant, J., & Graham, J. (2013). Bayesian demographic accounts:
      Subnational Population Estimation Using Multiple Data Sources.
      *Bayesian Analysis* 8(3).
- **Example Datasets**: The `/data` directory within the `accountTMB`
  package may contain example RDA/CSV files that illustrate the expected
  formats for rates, counts, and dispersion data. Examining these can be
  very helpful for preparing your own input files.
- **Reporting Issues/Seeking Support**:
  - For questions related to the `accountTMB` package itself, refer to
    the support channels provided by that package [accountTMB
    issues](https://github.com/ONSdigital/accountTMB/issues).
  - If you encounter bugs with the dashboard or have suggestions for
    improvement, please report them via the project’s issue tracker:
    [dpm.dash Issues](https://github.com/ONSBigData/dpm.dash/issues).
