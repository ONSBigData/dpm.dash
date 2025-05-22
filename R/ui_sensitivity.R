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
library(shinydashboard)
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(here)
library(numbers)
library(stringr)

# Define the main UI structure for the dashboard
ui_sens <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "DPM Dashboard (dev)"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Introduction", tabName = "introDoc", icon = icon("book")),
      shinydashboard::menuItem("Global Configuration", tabName = "globalConfig", icon = icon("cog")),
      shinydashboard::menuItem("DPM Specification",
        tabName = "dpmSpec", icon = icon("cogs"), startExpanded = FALSE,
        shinydashboard::menuSubItem("System Models", tabName = "systemModels", icon = icon("cogs")),
        shinydashboard::menuSubItem("Data Models", tabName = "dataModels", icon = icon("database"))
      ),
      shinydashboard::menuItem("DPM Running",
        tabName = "dpmRunning", icon = icon("sliders-h"), startExpanded = FALSE,
        shinydashboard::menuSubItem("Fit Model", tabName = "fitModel", icon = icon("sliders-h")),
        shinydashboard::menuSubItem("Population Estimates", tabName = "popEstimates", icon = icon("people-roof")),
        shinydashboard::menuSubItem("Migration Estimates", tabName = "migEstimates", icon = icon("plane"))
      ),
      shinydashboard::menuItem("Compare Setups",
        tabName = "setupComparing", icon = icon("chart-bar"), startExpanded = FALSE, # Changed icon for variety
        shinydashboard::menuSubItem("Fit Models", tabName = "fitModels", icon = icon("sliders-h")),
        shinydashboard::menuSubItem("Compare Population Estimates", tabName = "compPopEstimates", icon = icon("people-roof")),
        shinydashboard::menuSubItem("Compare Migration Estimates", tabName = "compMigEstimates", icon = icon("plane"))
      ),
      shinydashboard::menuItem("Model Sensitivity Analysis",
        tabName = "sensitivityAnalysis", icon = icon("flask"), startExpanded = FALSE,
        shinydashboard::menuSubItem("Setup & Run", tabName = "saSetupRun", icon = icon("play-circle")),
        shinydashboard::menuSubItem("Population Results", tabName = "saPopResults", icon = icon("people-group")),
        shinydashboard::menuSubItem("Migration Results", tabName = "saMigResults", icon = icon("route"))
      )
    )
  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "introDoc",
        shiny::includeMarkdown(here::here("inst/extdata/guide.md"))
      ),
      shinydashboard::tabItem(
        tabName = "globalConfig",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Global Configuration Parameters", width = 12, solidHeader = TRUE, status = "primary",
            shiny::p(
              "Please provide the following global configuration parameters:",
              shiny::tags$ul(
                shiny::tags$li("Input Data Directory : Defaults to the /data directory for dummy data, set to location of any data used for system/data models."),
                shiny::tags$li("Output Directory : Defaults to the /output directory, set to location to save results.RDS file/.csv file from 'Fit Model' tab."),
                shiny::tags$li("Time Selection : Defaults to empty (all), optional parameter to subset all data to for testing (comma separated years)."),
                shiny::tags$li("Seed Value : Defaults to a random prime number, can be used for reproducibility.")
              )
            ),
            shiny::textInput("global_data_dir", "Input Data Directory:", value = here::here("data/")), # Default data directory
            shiny::textInput("global_output_dir", "Output Directory:", value = here::here("output/")), # Default output directory
            shiny::textInput("global_time_selection", "Time Selection (comma-separated):"),
            shiny::numericInput("global_seed_value", "Seed Value:", value = numbers::nextPrime(as.integer(Sys.time()))) # Default seed value
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            width = 12, solidHeader = TRUE, status = "primary",
            shiny::actionButton("save_global_config", "Save Global Configuration")
          )
        ),
        shiny::fluidRow(
          shiny::column(width = 2, actionButton("goSM", "Continue"), icon = icon("arrow-right")),
        )
      ),
      shinydashboard::tabItem(
        tabName = "systemModels",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Specify System Models", width = 12, solidHeader = TRUE, status = "primary",
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
            lapply(c("births", "deaths", "ins", "outs"), function(model) {
              shinydashboard::box(
                title = paste(model, "System Model"), width = 6, status = "primary",
                fluidRow(
                  shinydashboard::box(
                    title = "Required", width = 12,
                    shiny::textInput(paste0(model, "_rates_file"), paste(model, "Rates CSV Filename"), value = paste0("sm_", model, ".csv")),
                    shiny::radioButtons(paste0(model, "_disp_type"), "Dispersion Input Type", choices = c("Single Value", "CSV File")),
                    shiny::conditionalPanel(
                      condition = sprintf("input.%s_disp_type == 'Single Value'", model),
                      numericInput(paste0(model, "_disp_value"), paste(model, "Dispersion Value"), value = 0.05)
                    ),
                    shiny::conditionalPanel(
                      condition = sprintf("input.%s_disp_type == 'CSV File'", model),
                      shiny::textInput(paste0(model, "_disp_file"), paste(model, "Dispersion CSV Filename"))
                    )
                  )
                ),
                fluidRow(
                  shinydashboard::box(
                    title = "Optional", width = 12, collapsible = TRUE, collapsed = TRUE,
                    numericInput(paste0(model, "_lower_rate_limit"), paste("Optional: ", model, " Lower Rate Limit"), value = 1e-6),
                    sliderInput(paste0(model, "_rate_scale"), paste("Optional: ", model, " Rate Scaler"), value = 1, min = 0, max = 3, step = 0.1),
                    numericInput(paste0(model, "_rate_overide"), paste("Optional: ", model, " Rate Set"), value = -1),
                    numericInput(paste0(model, "_rate_noise"), paste("Optional: ", model, " Noise Set"), value = 0)
                  )
                )
              )
            }),
            shiny::textInput("sysmods_name", "System Models Setup Name:", value = "default"),
            shiny::actionButton("create_system_models", "Create System Models"),
            shiny::actionButton("export_system_models", "Export System Models")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Available System Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("loadedSystemModels"),
            shiny::selectInput("delete_sysmod_list", "Select System Models to Delete", choices = NULL),
            shiny::actionButton("delete_button_sysmod", "Delete Selected System Models")
          ),
          shinydashboard::box(
            title = "Import System Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::textInput("sysmod_list_file", "System Model list .RDS file", value = "default_sysmods.RDS"),
            shiny::actionButton("import_button_sysmod", "Import Selected System Models")
          )
        ),
        shiny::fluidRow(
          shiny::column(width = 2, actionButton("goGC", "Back"), icon = icon("arrow-left")),
          shiny::column(width = 2, actionButton("goDM", "Continue"), icon = icon("arrow-right")),
        ),
        shiny::fluidRow(
          shiny::uiOutput("modelSummaries"),
          shiny::uiOutput("modelPlots")
        )
      ),
      shinydashboard::tabItem(
        tabName = "dataModels",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Specify Data Models", width = 12, solidHeader = TRUE, status = "primary",
            shiny::p(
              "The DPM requires at least 3 data models ('births', 'deaths', 'stock') containing the flow/stock counts",
              shiny::tags$ul(
                shiny::tags$li("births/deaths flows are modelled as exact counts."),
                shiny::tags$li("population stocks are considered uncertain counts, different data models have different approximations of this uncertainty.")
              ),
              "In addition to these required data models, it is possible to add any number of additional data models consisting of stocks (population) and flows (ins/outs). The specification of these data models"
            ),
            shiny::fluidRow(
              lapply(c("births", "deaths"), function(model) {
                shinydashboard::box(
                  title = paste0("'", model, "'", " Data Model"), width = 6, solidHeader = TRUE, status = "primary",
                  shiny::textInput(paste0(model, "_counts_file"), paste(model, "Counts CSV Filename"), value = paste0("dm_", model, ".csv")),
                  shiny::actionButton(paste0("create_", model, "_data_model"), paste("Create", model, "Data Model"))
                )
              })
            ),
            shiny::fluidRow(
              shinydashboard::box(
                title = "Add More Data Models", width = 12, solidHeader = TRUE, status = "primary",
                shinydashboard::box(
                  title = "Required Inputs", width = 12, solidHeader = TRUE,
                  shiny::textInput("dm_name", "Data Model Name"),
                  shiny::selectInput("series_name", "Choose Series Type",
                    choices = c("population", "ins", "outs")
                  ),
                  shiny::selectInput("data_model", "Choose Data Model",
                    choices = c(
                      "Normal Data Model", "T-Dist Data Model",
                      "Negative Binomial Data Model", "Poisson Data Model",
                      "Log-Normal Data Model"
                    )
                  ),
                  shiny::textInput("counts_file", "Counts CSV Filename"),
                  shiny::uiOutput("additionalInputs")
                ),
                shinydashboard::box(
                  title = "Optional Inputs", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                  shiny::uiOutput("optionalInputs")
                ),
                shiny::actionButton("create_data_model", "Create Data Model")
              )
            )
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Loaded Data Models", width = 12, solidHeader = TRUE, status = "primary",
            DT::DTOutput("loadedDataModels"),
            shiny::selectInput("delete_data_model", "Select Data Model to Delete", choices = NULL),
            shiny::actionButton("delete_button", "Delete Selected Data Model")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Export Data Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::textInput("datamod_list_tag", "Data Models list tag", value = ""),
            shiny::actionButton("export_data_models", "Export Data Models"),
          ),
          shinydashboard::box(
            title = "Import Data Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::textInput("datamod_list_file", "Data Model list .RDS file"),
            shiny::actionButton("import_button_datamod", "Import Selected Data Models")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Plot Data Model Data", width = 12, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("dmModelPlots"),
            shiny::selectInput("plot_data_model", "Select Data Model to Plot", choices = NULL),
            shiny::actionButton("plotdm_button", "Plot Selected Data Model")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Inspect Data Model Data (1)", width = 6, solidHeader = TRUE, status = "primary",
            DT::DTOutput("dmModelInspect1"),
            shiny::selectInput("inspect_data_model1", "Select Data Model to Inspect", choices = NULL),
            shiny::actionButton("inspectdm_button1", "Inspect Selected Data Model")
          ),
          shinydashboard::box(
            title = "Inspect Data Model Data (2)", width = 6, solidHeader = TRUE, status = "primary",
            DT::DTOutput("dmModelInspect2"),
            shiny::selectInput("inspect_data_model2", "Select Data Model to Inspect", choices = NULL),
            shiny::actionButton("inspectdm_button2", "Inspect Selected Data Model")
          )
        ),
        shiny::fluidRow(
          shiny::column(width = 2, actionButton("goSM", "Back"), icon = icon("arrow-left")),
          shiny::column(width = 2, actionButton("goFM", "Continue"), icon = icon("arrow-right")),
        )
      ),
      shinydashboard::tabItem(
        tabName = "fitModel",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Select System Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("system_model_checklist")
          ),
          shinydashboard::box(
            title = "Select Data Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("data_model_checklist")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            width = 12, solidHeader = TRUE, status = "primary",
            shiny::actionButton("fit_account_model", "Fit Model")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Cohort Results", solidHeader = TRUE, status = "primary",
            column(
              width = 6,
              shiny::textOutput("cohortResults")
            )
          ),
          shinydashboard::box(
            title = "Cohort Failures", solidHeader = TRUE, status = "primary",
            column(
              width = 6,
              DT::DTOutput("cohortDiagnostics")
            )
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Model summary", width = 12, solidHeader = TRUE, status = "primary",
            DT::DTOutput("population_table"),
            DT::DTOutput("migration_table")
          )
        ),
        shiny::fluidRow(
          shiny::column(width = 2, actionButton("goDM", "Back"), icon = icon("arrow-left")),
          shiny::column(width = 2, actionButton("goPE", "Continue"), icon = icon("arrow-right")),
        )
      ),
      shinydashboard::tabItem("Population Estimates",
        tabName = "popEstimates",
        shiny::fluidRow(
          shiny::column(4, selectInput("time_select_pop", "Time", choices = NULL)),
          shiny::column(4, textInput("compare_select_pop", "Compare Column"))
        ),
        uiOutput("popPlots"),
        shiny::fluidRow(
          shiny::column(width = 2, actionButton("goFM", "Back"), icon = icon("arrow-left")),
          shiny::column(width = 2, actionButton("goME", "Continue"), icon = icon("arrow-right")),
        )
      ),
      shinydashboard::tabItem("Migration Estimates",
        tabName = "migEstimates",
        shiny::fluidRow(
          shiny::column(4, selectInput("time_select_mig", "Time", choices = NULL)),
          shiny::column(4, textInput("compare_select_ins", "Compare Ins")),
          shiny::column(4, textInput("compare_select_outs", "Compare Outs"))
        ),
        shiny::uiOutput("immPlots"),
        shiny::uiOutput("emPlots"),
        shiny::fluidRow(
          shiny::column(width = 2, actionButton("goPE", "Back"), icon = icon("arrow-left")),
          shiny::column(width = 2, actionButton("goSC", "Continue"), icon = icon("arrow-right"))
        ),
      ),
      shinydashboard::tabItem(
        tabName = "fitModels",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Setup (1) System Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("sys_model_checklist_1")
          ),
          shinydashboard::box(
            title = "Setup (2) System Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("sys_model_checklist_2")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Setup (1) Data Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("data_model_checklist_1")
          ),
          shinydashboard::box(
            title = "Setup (2) Data Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("data_model_checklist_2")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            width = 12, solidHeader = TRUE, status = "primary",
            shiny::actionButton("compare_account_model", "Compare Models")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Aggregate estimates comparisons", width = 12, solidHeader = TRUE, status = "primary",
            shiny::uiOutput("compAggPop"),
            shiny::uiOutput("compAggImmig"),
            shiny::uiOutput("compAggEmig"),
            shiny::uiOutput("compAggNetMig"),
            shiny::uiOutput("compAggPopDiffs"),
            shiny::uiOutput("compAggImmigDiffs"),
            shiny::uiOutput("compAggEmigDiffs") # ,
            # shiny::uiOutput("compAggNetMigDiffs")
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "compPopEstimates",
        shiny::fluidRow(
          shiny::column(4, selectInput("time_select_comp", "Time", choices = NULL)),
          shiny::column(4, selectInput("setup_select", "Setup", choices = c("Both", "Setup 1", "Setup 2"))),
          shiny::column(4, selectInput("residual_type", "Residual Type", choices = c("Absolute", "Percent")))
        ),
        shiny::fluidRow(
          shiny::column(6, selectInput("compare_select_pop_1", "Compare Column 1", choices = NULL)),
          shiny::column(6, selectInput("compare_select_pop_2", "Compare Column 2", choices = NULL))
        ),
        shiny::uiOutput("compPlots"),
        shiny::uiOutput("compPlotsResiduals")
      ),
      shinydashboard::tabItem(
        tabName = "compMigEstimates",
        shiny::fluidRow(
          shiny::column(4, selectInput("time_select_comp_mig", "Time", choices = NULL)),
          shiny::column(4, selectInput("setup_select", "Setup", choices = c("Both", "Setup 1", "Setup 2"))),
          shiny::column(4, selectInput("residual_type", "Residual Type", choices = c("Absolute", "Percent")))
        ),
        shiny::fluidRow(
          shiny::column(3, selectInput("compare_select_ins_1", "Compare Ins 1", choices = NULL)),
          shiny::column(3, selectInput("compare_select_ins_2", "Compare Ins 2", choices = NULL)),
          shiny::column(3, selectInput("compare_select_outs_1", "Compare Outs 1", choices = NULL)),
          shiny::column(3, selectInput("compare_select_outs_2", "Compare Outs 2", choices = NULL))
        ),
        shiny::fluidRow(),
        shiny::uiOutput("compImmig"),
        shiny::uiOutput("compEmig"),
        shiny::uiOutput("compPlotsImmigResiduals"),
        shiny::uiOutput("compPlotsEmigResiduals"),
        shiny::fluidRow(
          shiny::column(width = 2, actionButton("goME", "Back"), icon = icon("arrow-left"))
        )
      ),
      shinydashboard::tabItem(
        tabName = "saSetupRun",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Sensitivity Analysis Setup", width = 12, solidHeader = TRUE, status = "primary",
            shiny::p("Select a base system model setup and base data models. Then, choose parameters to vary for sensitivity analysis."),
            shiny::fluidRow(
              column(
                6,
                shiny::uiOutput("sa_system_model_selector_ui") # For selecting base system model
              ),
              column(
                6,
                shiny::uiOutput("sa_data_model_selector_ui") # For selecting base data models
              )
            )
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "System Model Parameter Sensitivity", width = 6, solidHeader = TRUE, status = "info", collapsible = TRUE,
            shiny::selectInput("sa_sysmod_param_select", "Select System Model Parameter to Vary:",
              choices = c(
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
            shiny::uiOutput("sa_sysmod_param_range_ui") # Dynamic UI for min, max, steps
          ),
          shinydashboard::box(
            title = "Data Model Parameter Sensitivity", width = 6, solidHeader = TRUE, status = "info", collapsible = TRUE,
            shiny::selectInput("sa_datamod_name_select", "Select Data Model to Target:", choices = NULL), # Will be populated by selected data models
            shiny::selectInput("sa_datamod_param_select", "Select Data Model Parameter to Vary:",
              choices = NULL
            ),
            shiny::uiOutput("sa_datamod_param_range_ui") # Dynamic UI for min, max, steps
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            width = 12, solidHeader = TRUE, status = "primary",
            shiny::actionButton("run_sensitivity_analysis", "Run Sensitivity Analysis", icon = icon("cogs")),
            shiny::hr(),
            shiny::textOutput("sensitivity_run_status") # For feedback
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "saPopResults",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Sensitivity Population Estimates", width = 12, solidHeader = TRUE, status = "success",
            shiny::selectInput("sa_pop_plot_param_display", "Parameter to Display on Plot:", choices = NULL),
            shiny::selectInput("sa_pop_time_select", "Select Time:", choices = NULL),
            plotly::plotlyOutput("sa_population_plot_output", height = "600px")
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "saMigResults",
        shiny::fluidRow(
          shinydashboard::box(
            title = "Sensitivity Migration Estimates", width = 12, solidHeader = TRUE, status = "success",
            shiny::selectInput("sa_mig_plot_param_display", "Parameter to Display on Plot:", choices = NULL),
            shiny::selectInput("sa_mig_time_select", "Select Time:", choices = NULL),
            plotly::plotlyOutput("sa_immigration_plot_output", height = "600px"),
            plotly::plotlyOutput("sa_emigration_plot_output", height = "600px")
          )
        )
      )
    )
  )
)
