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



ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "DPM Dashboard"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Introdution", tabName = "introDoc", icon = icon("book")),
      shinydashboard::menuItem("Global Configuration", tabName = "globalConfig", icon = icon("cog")),
      shinydashboard::menuItem("DPM Specification", tabName = "dpmSpec", icon = icon("cogs"), startExpanded = FALSE,
                               shinydashboard::menuSubItem("System Models", tabName = "systemModels", icon = icon("cogs")),
                               shinydashboard::menuSubItem("Data Models", tabName = "dataModels", icon = icon("database"))
      ),
      shinydashboard::menuItem("DPM Running", tabName = "dpmRunning", icon = icon("sliders-h"), startExpanded = FALSE,
                               shinydashboard::menuSubItem("Fit Model", tabName = "fitModel", icon = icon("sliders-h")),
                               shinydashboard::menuSubItem("Population Estimates", tabName = "popEstimates", icon = icon("people-roof")),
                               shinydashboard::menuSubItem("Migration Estimates", tabName = "migEstimates", icon = icon("plane"))
      ),
      shinydashboard::menuItem("Compare Setups", tabName = "setupComp", icon = icon("code-compare"))
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
            shiny::textInput("global_data_dir", "Data Directory:", value = here::here("data/")), # Default data directory
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
          lapply(c("births", "deaths", "ins", "outs"), function(model) {
            shinydashboard::box(
              title = paste(model, "Model"), width = 6, solidHeader = TRUE, status = "primary",
              shiny::textInput(paste0(model, "_rates_file"), paste(model, "Rates CSV Filename")),
              shiny::radioButtons(paste0(model, "_disp_type"), "Dispersion Input Type", choices = c("Single Value", "CSV File")),
              shiny::conditionalPanel(
                condition = sprintf("input.%s_disp_type == 'Single Value'", model),
                numericInput(paste0(model, "_disp_value"), paste(model, "Dispersion Value"), value = 0.1)
              ),
              shiny::conditionalPanel(
                condition = sprintf("input.%s_disp_type == 'CSV File'", model),
                shiny::textInput(paste0(model, "_disp_file"), paste(model, "Dispersion CSV Filename"))
              ),
              numericInput(paste0(model, "_lower_rate_limit"), paste(model, "Lower Rate Limit"), value = 0)
            )
          })
        ),
        shiny::fluidRow(
          shinydashboard::box(
            width = 12, solidHeader = TRUE, status = "primary",
            shiny::actionButton("create_system_models", "Create System Models")
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
            title = "Select Data Model", width = 12, solidHeader = TRUE, status = "primary",
            shiny::textInput("dm_name", "Data Model Name"),
            shiny::selectInput("series_name", "Choose Series Type",
              choices = c("births", "deaths", "population", "ins", "outs")
            ),
            shiny::selectInput("data_model", "Choose Data Model",
              choices = c(
                "Exact Data Model", "Normal Data Model",
                "T-Dist Data Model", "Negative Binomial Data Model",
                "Poisson Data Model"
              )
            ),
            shiny::textInput("counts_file", "Counts CSV Filename"),
            shiny::textInput("age_selection", "Age Selection (comma separated or leave blank for all)", value = ""),
            shiny::textInput("time_selection", "Time Selection (comma separated or leave blank for all)", value = ""),
            shiny::uiOutput("additionalInputs"),
            shiny::actionButton("create_data_model", "Create Data Model")
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Loaded Data Models", width = 12, solidHeader = TRUE, status = "primary",
            DT::DTOutput("loadedDataModels"),
            # New dropdown and delete button
            shiny::selectInput("delete_data_model", "Select Data Model to Delete", choices = NULL),
            shiny::actionButton("delete_button", "Delete Selected Data Model")
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
            title = "System Models", width = 6, solidHeader = TRUE, status = "primary",
            shiny::verbatimTextOutput("sysmod_list_output")
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
      ),
      shinydashboard::tabItem(
        tabName = "setupComp",
        shiny::fluidRow(
          shinydashboard::box(
            title = "System Models", width = 12, solidHeader = TRUE, status = "primary",
            shiny::verbatimTextOutput("sysmod_list_output_comp")
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
          shiny::column(4, selectInput("time_select_comp", "Time", choices = NULL)),
          shiny::column(4, selectInput("sex_select_comp", "Sex", choices = c("Female", "Male"))),
          shiny::column(4, selectInput("setup_select", "Setup", choices = c("Both", "Setup 1", "Setup 2")))
        ),
        shiny::uiOutput("compPlots"),
        shiny::uiOutput("compImmig"),
        shiny::uiOutput("compEmig"),
        shiny::fluidRow(
          shiny::column(width = 2, actionButton("goME", "Back"), icon = icon("arrow-left"))
        )
      )
    )
  )
)
