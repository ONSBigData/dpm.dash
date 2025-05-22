## UI Setup
# The dashboard is organised into several tabs, each serving a distinct purpose:
# 1. Introduction: Provides a general overview/guide to using the dashboard
# 2. Global Configuration: Allows users to set global parameters for the dashboard/models
# 3. DPM Specification: For defining and managing models (system/data)
#   3.1. System Model setup: For defining and managing system models (births, deaths, ins, outs)
#   3.2. Data Model setup: For defining and managing data models (stocks and flows)
# 4. DPM Running: Interface for running a single DPM model using the defined system/data models
#   4.1. Fit Model: Fit a single DPM model by selecting a set of system models and data models, view cohort results/failures and a summary of estimates
#   4.2. Population Estimates: View plots of the population estimates
#   4.3. Migration Estimates: View plots of the migration estimates
# 5. Compare Setups: Fit two DPM models with different setups (different system/data models) to compare
#   5.1. Fit Models: Select the setups for two different DPM runs to compare
#   5.2. Compare Population Estimates: View plots comparing the population estimates from the two different DPM runs
#   5.3. Compare Migration Estimates: View plots comparing the migration estimates from the two different DPM runs
# 6. Model Sensitivity Analysis: Run a simple parameter sensisivity analysis for parameters related to either the system or data models
#   6.1. Setup & Run: Select the system and data models, along with the parameter and range to analyse
#   6.2. Population Results: View the population results of the parameter sweep chosen
#   6.3. Migration Results: View the migration results of the parameter sweep chosen

# Load necessary libraries for the shiny dashboard, UI elements, data manipulation and plotting
library(shinydashboard) # Provides the dashboard structure (header, sidebar, body).
library(shiny) # C  ore Shiny library for reactive programming and UI elements.
library(DT) # For creating interactive DataTables.
library(ggplot2) # For creating static plots.
library(plotly) # For creating interactive plots.
library(dplyr) # For data manipulation.
library(here) # For constructing file paths relative to the project root.
library(numbers) # Used here for generating a prime number for the default seed.
library(stringr) # For string manipulation.

# Define the main UI structure for the dashboard page.
ui_sens <- shinydashboard::dashboardPage(
  # Define the header of the dashboard.
  shinydashboard::dashboardHeader(title = "DPM Dashboard (dev)"), # Sets the title displayed in the dashboard header.
  # Define the sidebar of the dashboard.
  shinydashboard::dashboardSidebar(
    # Create a menu in the sidebar for navigation.
    shinydashboard::sidebarMenu(
      id = "tabs", # Assign an ID to the sidebar menu for referencing.
      # Define the "Introduction" tab.
      shinydashboard::menuItem("Introduction", tabName = "introDoc", icon = icon("book")), # Links to the "introDoc" tab content.
      # Define the "Global Configuration" tab.
      shinydashboard::menuItem("Global Configuration", tabName = "globalConfig", icon = icon("cog")), # Links to the "globalConfig" tab content.
      # Define a top-level menu item for "DPM Specification" with sub-items.
      shinydashboard::menuItem("DPM Specification",
        tabName = "dpmSpec", icon = icon("cogs"), startExpanded = FALSE, # Main menu item, not directly a tab, but groups sub-items.
        # Sub-item for "System Models" setup.
        shinydashboard::menuSubItem("System Models", tabName = "systemModels", icon = icon("cogs")), # Links to the "systemModels" tab content.
        # Sub-item for "Data Models" setup.
        shinydashboard::menuSubItem("Data Models", tabName = "dataModels", icon = icon("database")) # Links to the "dataModels" tab content.
      ),
      # Define a top-level menu item for "DPM Running" with sub-items.
      shinydashboard::menuItem("DPM Running",
        tabName = "dpmRunning", icon = icon("sliders-h"), startExpanded = FALSE, # Main menu item for model execution and results.
        # Sub-item for "Fit Model".
        shinydashboard::menuSubItem("Fit Model", tabName = "fitModel", icon = icon("sliders-h")), # Links to the "fitModel" tab content.
        # Sub-item for "Population Estimates".
        shinydashboard::menuSubItem("Population Estimates", tabName = "popEstimates", icon = icon("people-roof")), # Links to the "popEstimates" tab content.
        # Sub-item for "Migration Estimates".
        shinydashboard::menuSubItem("Migration Estimates", tabName = "migEstimates", icon = icon("plane")) # Links to the "migEstimates" tab content.
      ),
      # Define a top-level menu item for "Compare Setups" with sub-items.
      shinydashboard::menuItem("Compare Setups",
        tabName = "setupComparing", icon = icon("chart-bar"), startExpanded = FALSE, # Main menu item for comparing different model setups.
        # Sub-item for fitting models to compare.
        shinydashboard::menuSubItem("Fit Models", tabName = "fitModels", icon = icon("sliders-h")), # Links to the "fitModels" tab for comparison.
        # Sub-item for comparing population estimates.
        shinydashboard::menuSubItem("Compare Population Estimates", tabName = "compPopEstimates", icon = icon("people-roof")), # Links to compare population results.
        # Sub-item for comparing migration estimates.
        shinydashboard::menuSubItem("Compare Migration Estimates", tabName = "compMigEstimates", icon = icon("plane")) # Links to compare migration results.
      ),
      # Define a top-level menu item for "Model Sensitivity Analysis" with sub-items.
      shinydashboard::menuItem("Model Sensitivity Analysis",
        tabName = "sensitivityAnalysis", icon = icon("flask"), startExpanded = FALSE, # Main menu item for sensitivity analysis.
        # Sub-item for setting up and running sensitivity analysis.
        shinydashboard::menuSubItem("Setup & Run", tabName = "saSetupRun", icon = icon("play-circle")), # Links to sensitivity analysis setup.
        # Sub-item for viewing population results from sensitivity analysis.
        shinydashboard::menuSubItem("Population Results", tabName = "saPopResults", icon = icon("people-group")), # Links to sensitivity analysis population results.
        # Sub-item for viewing migration results from sensitivity analysis.
        shinydashboard::menuSubItem("Migration Results", tabName = "saMigResults", icon = icon("route")) # Links to sensitivity analysis migration results.
      )
    )
  ),
  # Define the main body of the dashboard where tab contents will be displayed.
  shinydashboard::dashboardBody(
    # Define the content for each tab specified in the sidebar menu.
    shinydashboard::tabItems(
      # Content for the "Introduction" tab.
      shinydashboard::tabItem(
        tabName = "introDoc", # Must match the tabName in menuItem.
        # Include content from an external Markdown file.
        shiny::includeMarkdown(here::here("inst/extdata/guide.md")) # Uses 'here' to locate the file relative to project root.
      ),
      # Content for the "Global Configuration" tab.
      shinydashboard::tabItem(
        tabName = "globalConfig", # Must match the tabName in menuItem.
        # Use a fluidRow to arrange elements in a row.
        shiny::fluidRow(
          # Create a box to group global configuration parameters.
          shinydashboard::box(
            title = "Global Configuration Parameters", width = 12, solidHeader = TRUE, status = "primary", # Box styling.
            # Add a paragraph with a list explaining the parameters.
            shiny::p(
              "Please provide the following global configuration parameters:",
              shiny::tags$ul( # Unordered list.
                shiny::tags$li("Input Data Directory : Defaults to the /data directory for dummy data, set to location of any data used for system/data models."),
                shiny::tags$li("Output Directory : Defaults to the /output directory, set to location to save results.RDS file/.csv file from 'Fit Model' tab."),
                shiny::tags$li("Time Selection : Defaults to empty (all), optional parameter to subset all data to for testing (comma separated years)."),
                shiny::tags$li("Seed Value : Defaults to a random prime number, can be used for reproducibility.")
              )
            ),
            # Input field for the data directory.
            shiny::textInput("global_data_dir", "Input Data Directory:", value = here::here("data/")), # Default value set using 'here'.
            # Input field for the output directory.
            shiny::textInput("global_output_dir", "Output Directory:", value = here::here("output/")), # Default value set using 'here'.
            # Input field for time selection (e.g., specific years).
            shiny::textInput("global_time_selection", "Time Selection (comma-separated):"),
            # Numeric input for the seed value for reproducibility.
            shiny::numericInput("global_seed_value", "Seed Value:", value = numbers::nextPrime(as.integer(Sys.time()))) # Default seed is the next prime after current time.
          )
        ),
        # Row for the save button.
        shiny::fluidRow(
          shinydashboard::box(
            width = 12, solidHeader = TRUE, status = "primary", # Box styling.
            # Action button to save the global configuration.
            shiny::actionButton("save_global_config", "Save Global Configuration")
          )
        ),
        # Row for navigation buttons.
        shiny::fluidRow(
          # Column for the "Continue" button.
          shiny::column(width = 2, actionButton("goSM", "Continue"), icon = icon("arrow-right")), # Navigates to System Models.
        )
      ),
      # Content for the "System Models" tab.
      shinydashboard::tabItem(
        tabName = "systemModels", # Must match the tabName in menuSubItem.
        shiny::fluidRow(
          # Box for specifying system models.
          shinydashboard::box(
            title = "Specify System Models", width = 12, solidHeader = TRUE, status = "primary",
            # Explanatory text for system models.
            shiny::p(
              "Specify the system models setups (requires at-least one, multiple can be specified for experimentation)",
              shiny::tags$ul(
                shiny::tags$li("Each DPM run requires a unique set of 4 system models, one for each of the demographic rates we want to capture the regularities in (births, deaths, ins, outs)."),
                shiny::tags$li("They are formal representations of what an experienced analyst expects about demographic patterns."),
                shiny::tags$li("The setup for each system model has required inputs"),
                shiny::tags$ul(
                  shiny::tags$li(".CSV filename - filename of the rates data (.csv format) with age, sex*, time, rate colums (*no sex for 'births' rates)."),
                  shiny::tags$li("Dispersion - numeric dispersion to assign to the system model, representative of the assumed uncertainty in the rates. (can be a single numeric value, or a dataframe in a .csv file)")
                ),
                shiny::tags$li("along with some optional inputs (useful for experimentation/debugging)"),
                shiny::tags$ul(
                  shiny::tags$li("Lower Rate Limit - numeric smallest allowed rate for system model, defaults to 1e-6, any rate lower than this value is imputed with the lower rate limit."),
                  shiny::tags$li("Rate Scaler - numeric scaler to apply to the rate, defaults to 1"),
                  shiny::tags$li("Rate Set - numeric override for the rate, defaults to -1 to not apply")
                ),
                shiny::tags$li("A unique setup name is also required, this is asigned to a particular collection of the 4 system models and is used when exporting the system model setup and when specifying which setups to use.")
              ),
              "Exporting the system models setup will save a .RDS file of the format <setup_name>_sysmods.RDS in the provided Input Data Directory (global config), which can be used to quickly import a previous setup."
            ),
            # Dynamically generate input boxes for each model type (births, deaths, ins, outs).
            lapply(c("births", "deaths", "ins", "outs"), function(model) {
              shinydashboard::box(
                title = paste(model, "System Model"), width = 6, status = "primary", # Title for each model type.
                fluidRow(
                  # Box for required inputs for each system model.
                  shinydashboard::box(
                    title = "Required", width = 12,
                    # Input for rates CSV filename.
                    shiny::textInput(paste0(model, "_rates_file"), paste(model, "Rates CSV Filename"), value = paste0("sm_", model, ".csv")), # Default filename.
                    # Radio buttons to choose dispersion input type.
                    shiny::radioButtons(paste0(model, "_disp_type"), "Dispersion Input Type", choices = c("Single Value", "CSV File")),
                    # Conditional panel: shows if "Single Value" dispersion is selected.
                    shiny::conditionalPanel(
                      condition = sprintf("input.%s_disp_type == 'Single Value'", model), # Condition based on radio button selection.
                      numericInput(paste0(model, "_disp_value"), paste(model, "Dispersion Value"), value = 0.05) # Numeric input for dispersion value.
                    ),
                    # Conditional panel: shows if "CSV File" dispersion is selected.
                    shiny::conditionalPanel(
                      condition = sprintf("input.%s_disp_type == 'CSV File'", model), # Condition based on radio button selection.
                      shiny::textInput(paste0(model, "_disp_file"), paste(model, "Dispersion CSV Filename")) # Text input for dispersion CSV filename.
                    )
                  )
                ),
                fluidRow(
                  # Box for optional inputs, collapsible.
                  shinydashboard::box(
                    title = "Optional", width = 12, collapsible = TRUE, collapsed = TRUE, # Initially collapsed.
                    # Numeric input for lower rate limit.
                    numericInput(paste0(model, "_lower_rate_limit"), paste("Optional: ", model, " Lower Rate Limit"), value = 1e-6),
                    # Slider input for rate scaler.
                    sliderInput(paste0(model, "_rate_scale"), paste("Optional: ", model, " Rate Scaler"), value = 1, min = 0, max = 3, step = 0.1),
                    # Numeric input for rate override.
                    numericInput(paste0(model, "_rate_overide"), paste("Optional: ", model, " Rate Set"), value = -1),
                    # Numeric input for rate noise.
                    numericInput(paste0(model, "_rate_noise"), paste("Optional: ", model, " Noise Set"), value = 0)
                  )
                )
              )
            }),
            # Input for the name of the system models setup.
            shiny::textInput("sysmods_name", "System Models Setup Name:", value = "default"), # Default setup name.
            # Action button to create the system models.
            shiny::actionButton("create_system_models", "Create System Models"),
            # Action button to export the system models.
            shiny::actionButton("export_system_models", "Export System Models")
          )
        ),
        # Row for managing available and imported system models.
        shiny::fluidRow(
          # Box to display and delete available system models.
          shinydashboard::box(
            title = "Available System Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("loadedSystemModels"), # UI output to display the list of loaded system models.
            shiny::selectInput("delete_sysmod_list", "Select System Models to Delete", choices = NULL), # Dropdown to select system models for deletion.
            shiny::actionButton("delete_button_sysmod", "Delete Selected System Models") # Button to trigger deletion.
          ),
          # Box to import system models from a file.
          shinydashboard::box(
            title = "Import System Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::textInput("sysmod_list_file", "System Model list .RDS file", value = "default_sysmods.RDS"), # Input for the RDS file name.
            shiny::actionButton("import_button_sysmod", "Import Selected System Models") # Button to trigger import.
          )
        ),
        # Row for navigation buttons.
        shiny::fluidRow(
          shiny::column(width = 2, actionButton("goGC", "Back"), icon = icon("arrow-left")), # Navigates back to Global Config.
          shiny::column(width = 2, actionButton("goDM", "Continue"), icon = icon("arrow-right")), # Navigates to Data Models.
        ),
        # Row for displaying model summaries and plots.
        shiny::fluidRow(
          shiny::uiOutput("modelSummaries"), # UI output for displaying summaries of the system models.
          shiny::uiOutput("modelPlots") # UI output for displaying plots related to system models.
        )
      ),
      # Content for the "Data Models" tab.
      shinydashboard::tabItem(
        tabName = "dataModels", # Must match the tabName in menuSubItem.
        shiny::fluidRow(
          # Box for specifying data models.
          shinydashboard::box(
            title = "Specify Data Models", width = 12, solidHeader = TRUE, status = "primary",
            # Explanatory text for data models.
            shiny::p(
              "The DPM requires at least 3 data models ('births', 'deaths', 'stock') containing the flow/stock counts",
              shiny::tags$ul(
                shiny::tags$li("births/deaths flows are modelled as exact counts."),
                shiny::tags$li("population stocks are considered uncertain counts, different data models have different approximations of this uncertainty.")
              ),
              "In addition to these required data models, it is possible to add any number of additional data models consisting of stocks (population) and flows (ins/outs). The specification of these data models"
            ),
            # Row for creating 'births' and 'deaths' data models (typically exact counts).
            shiny::fluidRow(
              lapply(c("births", "deaths"), function(model) {
                shinydashboard::box(
                  title = paste0("'", model, "'", " Data Model"), width = 6, solidHeader = TRUE, status = "primary",
                  # Input for counts CSV filename for births/deaths.
                  shiny::textInput(paste0(model, "_counts_file"), paste(model, "Counts CSV Filename"), value = paste0("dm_", model, ".csv")), # Default filename.
                  # Button to create the specific data model.
                  shiny::actionButton(paste0("create_", model, "_data_model"), paste("Create", model, "Data Model"))
                )
              })
            ),
            # Row for adding more (flexible) data models.
            shiny::fluidRow(
              shinydashboard::box(
                title = "Add More Data Models", width = 12, solidHeader = TRUE, status = "primary",
                # Box for required inputs for additional data models.
                shinydashboard::box(
                  title = "Required Inputs", width = 12, solidHeader = TRUE,
                  shiny::textInput("dm_name", "Data Model Name"), # Name for the new data model.
                  shiny::selectInput("series_name", "Choose Series Type", # Type of demographic series.
                    choices = c("population", "ins", "outs")
                  ),
                  shiny::selectInput("data_model", "Choose Data Model", # Statistical model type.
                    choices = c(
                      "Normal Data Model", "T-Dist Data Model",
                      "Negative Binomial Data Model", "Poisson Data Model",
                      "Log-Normal Data Model"
                    )
                  ),
                  shiny::textInput("counts_file", "Counts CSV Filename"), # CSV file for the counts data.
                  shiny::uiOutput("additionalInputs") # Dynamic UI for additional inputs based on model choice.
                ),
                # Box for optional inputs, collapsible.
                shinydashboard::box(
                  title = "Optional Inputs", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, # Initially expanded.
                  shiny::uiOutput("optionalInputs") # Dynamic UI for optional inputs based on model choice.
                ),
                # Button to create the data model.
                shiny::actionButton("create_data_model", "Create Data Model")
              )
            )
          )
        ),
        # Row for displaying and managing loaded data models.
        shiny::fluidRow(
          shinydashboard::box(
            title = "Loaded Data Models", width = 12, solidHeader = TRUE, status = "primary",
            DT::DTOutput("loadedDataModels"), # Interactive table to display loaded data models.
            shiny::selectInput("delete_data_model", "Select Data Model to Delete", choices = NULL), # Dropdown to select a data model for deletion.
            shiny::actionButton("delete_button", "Delete Selected Data Model") # Button to trigger deletion.
          )
        ),
        # Row for exporting and importing data models.
        shiny::fluidRow(
          # Box for exporting data models.
          shinydashboard::box(
            title = "Export Data Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::textInput("datamod_list_tag", "Data Models list tag", value = ""), # Tag for the exported file name.
            shiny::actionButton("export_data_models", "Export Data Models"), # Button to trigger export.
          ),
          # Box for importing data models.
          shinydashboard::box(
            title = "Import Data Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::textInput("datamod_list_file", "Data Model list .RDS file"), # Input for the RDS file to import.
            shiny::actionButton("import_button_datamod", "Import Selected Data Models") # Button to trigger import.
          )
        ),
        # Row for plotting data model data.
        shiny::fluidRow(
          shinydashboard::box(
            title = "Plot Data Model Data", width = 12, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("dmModelPlots"), # UI output to display plots of data model data.
            shiny::selectInput("plot_data_model", "Select Data Model to Plot", choices = NULL), # Dropdown to select data model for plotting.
            shiny::actionButton("plotdm_button", "Plot Selected Data Model") # Button to trigger plotting.
          )
        ),
        # Row for inspecting data model data in tables (two panels for comparison).
        shiny::fluidRow(
          # First inspection panel.
          shinydashboard::box(
            title = "Inspect Data Model Data (1)", width = 6, solidHeader = TRUE, status = "primary",
            DT::DTOutput("dmModelInspect1"), # Interactive table for data inspection.
            shiny::selectInput("inspect_data_model1", "Select Data Model to Inspect", choices = NULL), # Dropdown for model selection.
            shiny::actionButton("inspectdm_button1", "Inspect Selected Data Model") # Button to trigger inspection.
          ),
          # Second inspection panel.
          shinydashboard::box(
            title = "Inspect Data Model Data (2)", width = 6, solidHeader = TRUE, status = "primary",
            DT::DTOutput("dmModelInspect2"), # Interactive table for data inspection.
            shiny::selectInput("inspect_data_model2", "Select Data Model to Inspect", choices = NULL), # Dropdown for model selection.
            shiny::actionButton("inspectdm_button2", "Inspect Selected Data Model") # Button to trigger inspection.
          )
        ),
        # Row for navigation buttons.
        shiny::fluidRow(
          shiny::column(width = 2, actionButton("goSM", "Back"), icon = icon("arrow-left")), # Navigates back to System Models.
          shiny::column(width = 2, actionButton("goFM", "Continue"), icon = icon("arrow-right")), # Navigates to Fit Model.
        )
      ),
      # Content for the "Fit Model" tab.
      shinydashboard::tabItem(
        tabName = "fitModel", # Must match the tabName in menuSubItem.
        # Row for selecting system and data models to use for fitting.
        shiny::fluidRow(
          # Box for selecting system models.
          shinydashboard::box(
            title = "Select System Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("system_model_checklist") # UI output for a checklist of available system models.
          ),
          # Box for selecting data models.
          shinydashboard::box(
            title = "Select Data Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("data_model_checklist") # UI output for a checklist of available data models.
          )
        ),
        # Row for the "Fit Model" button.
        shiny::fluidRow(
          shinydashboard::box(
            width = 12, solidHeader = TRUE, status = "primary",
            shiny::actionButton("fit_account_model", "Fit Model") # Button to trigger the model fitting process.
          )
        ),
        # Row for displaying cohort results and diagnostics.
        shiny::fluidRow(
          # Box for cohort results summary.
          shinydashboard::box(
            title = "Cohort Results", solidHeader = TRUE, status = "primary",
            column(
              width = 6,
              shiny::textOutput("cohortResults") # Text output for displaying cohort results.
            )
          ),
          # Box for cohort diagnostic information (e.g., failures).
          shinydashboard::box(
            title = "Cohort Failures", solidHeader = TRUE, status = "primary",
            column(
              width = 6,
              DT::DTOutput("cohortDiagnostics") # Interactive table for cohort diagnostics.
            )
          )
        ),
        # Row for displaying detailed model summary tables.
        shiny::fluidRow(
          shinydashboard::box(
            title = "Model summary", width = 12, solidHeader = TRUE, status = "primary",
            DT::DTOutput("population_table"), # Table for population estimates.
            DT::DTOutput("migration_table") # Table for migration estimates.
          )
        ),
        # Row for navigation buttons.
        shiny::fluidRow(
          shiny::column(width = 2, actionButton("goDM", "Back"), icon = icon("arrow-left")), # Navigates back to Data Models.
          shiny::column(width = 2, actionButton("goPE", "Continue"), icon = icon("arrow-right")), # Navigates to Population Estimates.
        )
      ),
      # Content for the "Population Estimates" tab.
      shinydashboard::tabItem("Population Estimates", # Note: tabName is explicitly given here, matching menuSubItem.
        tabName = "popEstimates",
        # Row for input controls for population plots.
        shiny::fluidRow(
          shiny::column(4, selectInput("time_select_pop", "Time", choices = NULL)), # Dropdown to select time period for plots.
          shiny::column(4, textInput("compare_select_pop", "Compare Column")) # Text input to specify a column for comparison.
        ),
        # UI output for displaying population plots.
        uiOutput("popPlots"),
        # Row for navigation buttons.
        shiny::fluidRow(
          shiny::column(width = 2, actionButton("goFM", "Back"), icon = icon("arrow-left")), # Navigates back to Fit Model.
          shiny::column(width = 2, actionButton("goME", "Continue"), icon = icon("arrow-right")), # Navigates to Migration Estimates.
        )
      ),
      # Content for the "Migration Estimates" tab.
      shinydashboard::tabItem("Migration Estimates", # Note: tabName is explicitly given here.
        tabName = "migEstimates",
        # Row for input controls for migration plots.
        shiny::fluidRow(
          shiny::column(4, selectInput("time_select_mig", "Time", choices = NULL)), # Dropdown for time period.
          shiny::column(4, textInput("compare_select_ins", "Compare Ins")), # Text input for comparing immigration data.
          shiny::column(4, textInput("compare_select_outs", "Compare Outs")) # Text input for comparing emigration data.
        ),
        # UI outputs for immigration and emigration plots.
        shiny::uiOutput("immPlots"),
        shiny::uiOutput("emPlots"),
        # Row for navigation buttons.
        shiny::fluidRow(
          shiny::column(width = 2, actionButton("goPE", "Back"), icon = icon("arrow-left")), # Navigates back to Population Estimates.
          shiny::column(width = 2, actionButton("goSC", "Continue"), icon = icon("arrow-right")) # Navigates to Compare Setups (Fit Models part).
        ),
      ),
      # Content for the "Fit Models" sub-tab under "Compare Setups".
      shinydashboard::tabItem(
        tabName = "fitModels", # Must match the tabName in menuSubItem.
        # Row for selecting system models for two different setups to compare.
        shiny::fluidRow(
          # System models for Setup 1.
          shinydashboard::box(
            title = "Setup (1) System Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("sys_model_checklist_1") # Checklist for system models of setup 1.
          ),
          # System models for Setup 2.
          shinydashboard::box(
            title = "Setup (2) System Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("sys_model_checklist_2") # Checklist for system models of setup 2.
          )
        ),
        # Row for selecting data models for the two setups.
        shiny::fluidRow(
          # Data models for Setup 1.
          shinydashboard::box(
            title = "Setup (1) Data Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("data_model_checklist_1") # Checklist for data models of setup 1.
          ),
          # Data models for Setup 2.
          shinydashboard::box(
            title = "Setup (2) Data Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("data_model_checklist_2") # Checklist for data models of setup 2.
          )
        ),
        # Row for the "Compare Models" button.
        shiny::fluidRow(
          shinydashboard::box(
            width = 12, solidHeader = TRUE, status = "primary",
            shiny::actionButton("compare_account_model", "Compare Models") # Button to trigger the comparison.
          )
        ),
        # Row for displaying aggregate estimate comparisons.
        shiny::fluidRow(
          shinydashboard::box(
            title = "Aggregate estimates comparisons", width = 12, solidHeader = TRUE, status = "primary",
            # UI outputs for various aggregate comparison plots/tables.
            shiny::uiOutput("compAggPop"),
            shiny::uiOutput("compAggImmig"),
            shiny::uiOutput("compAggEmig"),
            shiny::uiOutput("compAggNetMig"),
            shiny::uiOutput("compAggPopDiffs"),
            shiny::uiOutput("compAggImmigDiffs"),
            shiny::uiOutput("compAggEmigDiffs")
            # shiny::uiOutput("compAggNetMigDiffs") # This line is commented out in the original code.
          )
        )
      ),
      # Content for the "Compare Population Estimates" sub-tab.
      shinydashboard::tabItem(
        tabName = "compPopEstimates", # Must match the tabName in menuSubItem.
        # Row for selection inputs for comparison plots.
        shiny::fluidRow(
          shiny::column(4, selectInput("time_select_comp", "Time", choices = NULL)), # Time selection.
          shiny::column(4, selectInput("setup_select", "Setup", choices = c("Both", "Setup 1", "Setup 2"))), # Setup selection.
          shiny::column(4, selectInput("residual_type", "Residual Type", choices = c("Absolute", "Percent"))) # Residual type for differences.
        ),
        # Row for selecting columns to compare from each setup's population data.
        shiny::fluidRow(
          shiny::column(6, selectInput("compare_select_pop_1", "Compare Column 1", choices = NULL)), # Comparison column for setup 1.
          shiny::column(6, selectInput("compare_select_pop_2", "Compare Column 2", choices = NULL)) # Comparison column for setup 2.
        ),
        # UI outputs for comparison plots and residual plots.
        shiny::uiOutput("compPlots"),
        shiny::uiOutput("compPlotsResiduals")
      ),
      # Content for the "Compare Migration Estimates" sub-tab.
      shinydashboard::tabItem(
        tabName = "compMigEstimates", # Must match the tabName in menuSubItem.
        # Row for selection inputs.
        shiny::fluidRow(
          shiny::column(4, selectInput("time_select_comp_mig", "Time", choices = NULL)), # Time selection.
          shiny::column(4, selectInput("setup_select", "Setup", choices = c("Both", "Setup 1", "Setup 2"))), # Setup selection.
          shiny::column(4, selectInput("residual_type", "Residual Type", choices = c("Absolute", "Percent"))) # Residual type.
        ),
        # Row for selecting comparison columns for immigration and emigration for both setups.
        shiny::fluidRow(
          shiny::column(3, selectInput("compare_select_ins_1", "Compare Ins 1", choices = NULL)), # Immigration compare column setup 1.
          shiny::column(3, selectInput("compare_select_ins_2", "Compare Ins 2", choices = NULL)), # Immigration compare column setup 2.
          shiny::column(3, selectInput("compare_select_outs_1", "Compare Outs 1", choices = NULL)), # Emigration compare column setup 1.
          shiny::column(3, selectInput("compare_select_outs_2", "Compare Outs 2", choices = NULL)) # Emigration compare column setup 2.
        ),
        shiny::fluidRow(), # Empty fluidRow, possibly for spacing or future content.
        # UI outputs for migration comparison plots and residuals.
        shiny::uiOutput("compImmig"),
        shiny::uiOutput("compEmig"),
        shiny::uiOutput("compPlotsImmigResiduals"),
        shiny::uiOutput("compPlotsEmigResiduals"),
        # Row for navigation button.
        shiny::fluidRow(
          shiny::column(width = 2, actionButton("goME", "Back"), icon = icon("arrow-left")) # Navigates back to Migration Estimates (single model).
        )
      ),
      # Content for the "Setup & Run" sub-tab under "Model Sensitivity Analysis".
      shinydashboard::tabItem(
        tabName = "saSetupRun", # Must match the tabName in menuSubItem.
        # Row for the main sensitivity analysis setup box.
        shiny::fluidRow(
          shinydashboard::box(
            title = "Sensitivity Analysis Setup", width = 12, solidHeader = TRUE, status = "primary",
            shiny::p("Select a base system model setup and base data models. Then, choose parameters to vary for sensitivity analysis."),
            # Row for selecting base system and data models.
            shiny::fluidRow(
              column(
                6,
                shiny::uiOutput("sa_system_model_selector_ui") # UI output for selecting the base system model setup.
              ),
              column(
                6,
                shiny::uiOutput("sa_data_model_selector_ui") # UI output for selecting the base data models.
              )
            )
          )
        ),
        # Row for specifying parameters to vary for sensitivity analysis.
        shiny::fluidRow(
          # Box for system model parameter sensitivity.
          shinydashboard::box(
            title = "System Model Parameter Sensitivity", width = 6, solidHeader = TRUE, status = "info", collapsible = TRUE,
            # Dropdown to select which system model parameter to vary.
            shiny::selectInput("sa_sysmod_param_select", "Select System Model Parameter to Vary:",
              choices = c( # Predefined choices for system model parameters.
                "None",
                "Dispersion (Births)" = "births_dispersion",
                "Dispersion (Deaths)" = "deaths_dispersion",
                "Dispersion (Ins)" = "ins_dispersion",
                "Dispersion (Outs)" = "outs_dispersion",
                "Lower Rate Limit (Births)" = "births_lower_rate_limit",
                "Lower Rate Limit (Deaths)" = "deaths_lower_rate_limit",
                "Lower Rate Limit (Ins)" = "ins_lower_rate_limit",
                "Lower Rate Limit (Outs)" = "outs_lower_rate_limit",
                "Rate Scaler (Births)" = "births_rate_scale",
                "Rate Scaler (Deaths)" = "deaths_rate_scale",
                "Rate Scaler (Ins)" = "ins_rate_scale",
                "Rate Scaler (Outs)" = "outs_rate_scale",
                "Rate Noise (Births)" = "births_rate_noise",
                "Rate Noise (Deaths)" = "deaths_rate_noise",
                "Rate Noise (Ins)" = "ins_rate_noise",
                "Rate Noise (Outs)" = "outs_rate_noise"
              )
            ),
            shiny::uiOutput("sa_sysmod_param_range_ui") # Dynamic UI for setting min, max, and steps for the selected parameter.
          ),
          # Box for data model parameter sensitivity.
          shinydashboard::box(
            title = "Data Model Parameter Sensitivity", width = 6, solidHeader = TRUE, status = "info", collapsible = TRUE,
            shiny::selectInput("sa_datamod_name_select", "Select Data Model to Target:", choices = NULL), # Dropdown to select the target data model (populated dynamically).
            shiny::selectInput("sa_datamod_param_select", "Select Data Model Parameter to Vary:", # Dropdown for data model parameter.
              choices = NULL # Choices populated dynamically based on selected data model.
            ),
            shiny::uiOutput("sa_datamod_param_range_ui") # Dynamic UI for setting parameter range.
          )
        ),
        # Row for the run button and status feedback.
        shiny::fluidRow(
          shinydashboard::box(
            width = 12, solidHeader = TRUE, status = "primary",
            shiny::actionButton("run_sensitivity_analysis", "Run Sensitivity Analysis", icon = icon("cogs")), # Button to start the analysis.
            shiny::hr(), # Horizontal rule for separation.
            shiny::textOutput("sensitivity_run_status") # Text output for displaying run status or feedback.
          )
        )
      ),
      # Content for the "Population Results" sub-tab under "Model Sensitivity Analysis".
      shinydashboard::tabItem(
        tabName = "saPopResults", # Must match the tabName in menuSubItem.
        shiny::fluidRow(
          shinydashboard::box(
            title = "Sensitivity Population Estimates", width = 12, solidHeader = TRUE, status = "success",
            shiny::selectInput("sa_pop_plot_param_display", "Parameter to Display on Plot:", choices = NULL), # Select parameter to visualize.
            shiny::selectInput("sa_pop_time_select", "Select Time:", choices = NULL), # Select time period for results.
            plotly::plotlyOutput("sa_population_plot_output", height = "600px") # Interactive plot output for population sensitivity.
          )
        )
      ),
      # Content for the "Migration Results" sub-tab under "Model Sensitivity Analysis".
      shinydashboard::tabItem(
        tabName = "saMigResults", # Must match the tabName in menuSubItem.
        shiny::fluidRow(
          shinydashboard::box(
            title = "Sensitivity Migration Estimates", width = 12, solidHeader = TRUE, status = "success",
            shiny::selectInput("sa_mig_plot_param_display", "Parameter to Display on Plot:", choices = NULL), # Select parameter to visualize.
            shiny::selectInput("sa_mig_time_select", "Select Time:", choices = NULL), # Select time period for results.
            plotly::plotlyOutput("sa_immigration_plot_output", height = "600px"), # Interactive plot for immigration sensitivity.
            plotly::plotlyOutput("sa_emigration_plot_output", height = "600px") # Interactive plot for emigration sensitivity.
          )
        )
      )
    )
  )
)