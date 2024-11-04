## UI Setup
# Tabs
# 1. Introduction
# 2. Global Configuration
# 3. System Model setup
# 4. Data Model setup
# 5. Model fitting
# 6. Output estimation (population, ins, outs)
library(shinydashboard)
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(here)
library(numbers)
library(stringr)

ui_dev <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "DPM Dashboard (demo)"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Introdution", tabName = "introDoc", icon = icon("book")),
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
                shiny::tags$li("Output Directory : Defaults to the /output directory, set to location to save results.RDS file/.csv file from ‘Fit Model’ tab."),
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
                    numericInput(paste0(model, "_rate_overide"), paste("Optional: ", model, " Rate Set"), value = -1)
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
                                       "Negative Binomial Data Model", "Poisson Data Model"
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
                                shiny::column(4, selectInput("sex_select_pop", "Sex", choices = c("Female", "Male"))),
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
                                shiny::column(4, selectInput("sex_select_mig", "Sex", choices = c("Female", "Male"))),
                                shiny::column(4, textInput("compare_select_ins", "Compare Ins")),
                                shiny::column(4, textInput("compare_select_outs", "Compare Outs"))
                              ),
                              shiny::uiOutput("immPlots"),
                              shiny::uiOutput("emPlots"),
                              shiny::fluidRow(
                                shiny::column(width = 2, actionButton("goPE", "Back"), icon = icon("arrow-left")),
                                shiny::column(width = 2, actionButton("goSC", "Continue"), icon = icon("arrow-right"))
                              ),
      )
    )
  )
)
