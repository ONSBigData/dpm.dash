# ==============================================================================
# === 3. USER INTERFACE (UI)
# ==============================================================================
ui_dpm <- shinydashboard::dashboardPage(
  skin = "blue",
  shinydashboard::dashboardHeader(title = "Multi-Step DPM Dashboard"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("User Guide", tabName = "user_guide_tab", icon = icon("book-open")),
      shinydashboard::menuItem("Global Configuration", tabName = "globalConfig", icon = icon("cog")),

      # --- Step 1: Bage Modeling ---
      shinydashboard::menuItem("Step 1: Rate Modeling (bage)",
        tabName = "bage_step", icon = icon("chart-line"), startExpanded = FALSE,
        shinydashboard::menuSubItem("1.1 Upload Count Data", tabName = "bage_upload_tab", icon = icon("file-csv")),
        shinydashboard::menuSubItem("1.2 Define bage Model", tabName = "bage_define_model_tab", icon = icon("sitemap")),
        shinydashboard::menuSubItem("1.3 Set bage Priors", tabName = "bage_priors_tab", icon = icon("sliders-h")),
        shinydashboard::menuSubItem("1.4 Fit & Review bage Model", tabName = "bage_results_tab", icon = icon("chart-bar"))
      ),

      # --- Step 2: DPM Accounting ---
      shinydashboard::menuItem("Step 2: DPM Accounting (dpmaccount)",
        tabName = "dpm_step", icon = icon("calculator"),
        shinydashboard::menuSubItem("2.1 System Model Setup", tabName = "dpm_system_models_tab", icon = icon("cogs")),
        shinydashboard::menuSubItem("2.2 Data Model Setup", tabName = "dpm_data_models_tab", icon = icon("database")),
        shinydashboard::menuSubItem("2.3 Fit Single DPM", tabName = "dpm_fit_single_tab", icon = icon("play")),
        shinydashboard::menuSubItem("2.4 View DPM Results", tabName = "dpm_results_tab", icon = icon("table"))
      ),

      # --- Advanced Tools ---
      shinydashboard::menuItem("Advanced Tools",
        tabName = "advanced_tools", icon = icon("wrench"),
        shinydashboard::menuSubItem("DPM: Compare Setups", tabName = "dpm_compare_tab", icon = icon("balance-scale"))
      ),
      shinydashboard::menuItem("Generated R Code", tabName = "generated_code_tab", icon = icon("code"))
    ),
    shiny::hr(),
    shiny::div(
      style = "padding: 15px;",
      shiny::actionButton("fit_bage_model_button", "Fit Current bage Component", icon = shiny::icon("play"), class = "btn-success", width = "100%")
      )
  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      # --- User Guide Tab ---
      shinydashboard::tabItem(
        tabName = "user_guide_tab",
        shinydashboard::box(
          title = "Welcome to the Integrated Dashboard", status = "primary", solidHeader = TRUE, width = 12,
          shiny::h3("Purpose"),
          shiny::p("This dashboard combines two powerful modeling steps into a single workflow:"),
          shiny::tags$ol(
            shiny::tags$li(shiny::strong("Step 1: Rate Modeling"), "- Uses the `{bage}` package to fit Bayesian hierarchical models to count data, producing robust estimates of age-specific rates."),
            shiny::tags$li(shiny::strong("Step 2: DPM Accounting"), "- Uses the `{dpmaccount}` package to fit the model (DPM), which reconciles population stocks and flows using the rates from Step 1.")
          ),
          shiny::h3("Workflow Guide"),
          shiny::p("Follow the numbered tabs in the sidebar to complete the analysis:"),
          shiny::tags$ul(
            shiny::tags$li(shiny::strong("Global Configuration:"), "Set your data and output directories first."),
            shiny::tags$li(shiny::strong("Step 1: Rate Modeling (bage):"), "This step now allows for fitting an independent model for each demographic component (Births, Deaths, Ins, Outs). For each one, you will upload data, define the model, set priors, and click the 'Fit Current bage Component' button. Repeat for all components you need."),
            shiny::tags$li(shiny::strong("Step 2: DPM Accounting (dpmaccount):"), "Once the `bage` models are fitted, proceed to set up the DPM. In the 'System Model Setup' tab, you can now choose to use the corresponding rates generated from `bage` as inputs for the accounting model."),
            shiny::tags$li(shiny::strong("Advanced Tools:"), "Explore deeper diagnostics for both models, run forecasts, compare different DPM setups, or perform sensitivity analyses."),
            shiny::tags$li(shiny::strong("Generated R Code:"), "Find a complete, reproducible script for your entire analysis.")
          )
        )
      ),

      # --- Global Configuration Tab ---
      shinydashboard::tabItem(
        tabName = "globalConfig",
        h2("Global Configuration"),
        shinydashboard::box(
          title = "Global Configuration Parameters", width = 12, solidHeader = TRUE, status = "primary",
          shiny::textInput("global_data_dir", "Input Data Directory:", value = here::here("data/")),
          shiny::textInput("global_output_dir", "Output Directory:", value = here::here("output/")),
          shiny::textInput("global_time_selection", "Global Time Selection (e.g., 2000:2010):", placeholder = "Leave blank to use all time points"),
          shiny::numericInput("global_seed_value", "Seed Value:", value = numbers::nextPrime(as.integer(Sys.time()))),
          shiny::actionButton("save_global_config", "Save Global Configuration", icon = icon("save"))
        )
      ),

      # --- Bage Tab: Upload Data ---
      shinydashboard::tabItem(
        tabName = "bage_upload_tab",
        h2("Step 1.1: Upload Count Data for bage"),
        shiny::fluidRow(
          shinydashboard::box(
            title = "Upload CSV Data", status = "primary", solidHeader = TRUE, width = 12,
            p("Upload a single CSV file. You will specify which columns to use for each component model (Births, Deaths, etc.) in the next step."),
            shiny::fileInput("dataFile", "Select CSV file with count data", accept = c("text/csv"))
          ),
          shinydashboard::box(title = "Data Preview", status = "primary", solidHeader = TRUE, width = 12, DT::dataTableOutput("data_table"))
        )
      ),

      # --- Bage Tab: Define Model ---
      shinydashboard::tabItem(
        tabName = "bage_define_model_tab",
        h2("Step 1.2: Define bage Model Specification"),
        selectInput("bage_component_selector", "Select Component to Configure:",
          choices = c("Births", "Deaths", "Ins", "Outs"),
          selected = "Births"
        ),
        hr(),
        shiny::uiOutput("formula_builder_ui_config"),
        hr(),
        shiny::uiOutput("formula_builder_ui")
      ),

      # --- Bage Tab: Set Priors ---
      shinydashboard::tabItem(
        tabName = "bage_priors_tab",
        h2("Step 1.3: Set Priors for bage Predictors"),
        p("Priors shown are for the component selected in the 'Define Model' tab."),
        shiny::uiOutput("priorControls")
      ),

      # --- Bage Tab: Model Results ---
      shinydashboard::tabItem(
        tabName = "bage_results_tab",
        h2("Step 1.4: Fit and Review bage Model"),
        fluidRow(
          shinydashboard::box(
            title = "Fit & Review", status = "success", solidHeader = TRUE, width = 12,
            p("After defining your model and priors for a component, click the 'Fit Current bage Component' button in the sidebar."),
            p("Use the dropdown below to review the results for each component you have fitted."),
            selectInput("bage_results_selector", "Select Fitted Component to Review:",
              choices = c("Births", "Deaths", "Ins", "Outs")
            ),
            hr(),
            h4("Fitted Models Status:"),
            uiOutput("fitted_bage_models_status")
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = "Plot Controls", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
            p("Select a single value for each variable to generate a specific plot."),
            uiOutput("results_plot_filters_ui")
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = "bage Model Output", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
            shinycssloaders::withSpinner(plotlyOutput("results_plot", height = "500px")),
            hr(),
            DT::dataTableOutput("results_table")
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = "bage Model Summary & Downloads", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
            h4("Model Summary"),
            verbatimTextOutput("model_summary_print"),
            hr(),
            downloadButton("download_model_rds", "Save Selected bage Model (.rds)"),
            downloadButton("download_results_csv", "Save Selected bage Results (.csv)")
          )
        )
      ),

      # --- DPM Tab: System Model Setup (The Bridge) ---
      shinydashboard::tabItem(
        tabName = "dpm_system_models_tab",
        h2("Step 2.1: DPM System Model Setup"),
        fluidRow(
          box(
            title = "Specify System Models", width = 12, solidHeader = TRUE, status = "primary",
            p("For each demographic component, specify the source for the rates data. You can either use the corresponding fitted `bage` model or upload a separate CSV file."),
            textInput("sysmods_name", "System Models Setup Name:", value = "default_setup"),
            hr(),
            lapply(c("births", "deaths", "ins", "outs"), function(model) {
              box(
                title = paste(stringr::str_to_title(model), "System Model"), width = 6, status = "info", solidHeader = TRUE,
                radioButtons(paste0("rate_source_", model), "Select Rate Source:",
                  choices = c(
                    "Use fitted bage model results" = "bage",
                    "Upload rates from CSV" = "csv"
                  ),
                  selected = "bage"
                ),
                conditionalPanel(
                  condition = sprintf("input.rate_source_%s == 'csv'", model),
                  textInput(paste0(model, "_rates_file"), "Rates CSV Filename", value = paste0("sm_", model, ".csv"))
                ),
                conditionalPanel(
                  condition = sprintf("input.rate_source_%s == 'bage'", model),
                  p(paste0("Rates will be taken from the fitted '", stringr::str_to_title(model), "' bage model."))
                ),
                hr(),
                radioButtons(paste0(model, "_disp_type"), "Dispersion Input Type", choices = c("Single Value", "CSV File")),
                conditionalPanel(
                  condition = sprintf("input.%s_disp_type == 'Single Value'", model),
                  numericInput(paste0(model, "_disp_value"), "Dispersion Value", value = 0.05)
                ),
                conditionalPanel(
                  condition = sprintf("input.%s_disp_type == 'CSV File'", model),
                  textInput(paste0(model, "_disp_file"), "Dispersion CSV Filename")
                ),
                actionButton(paste0("toggle_optional_", model), "Show/Hide Optional Overrides", icon = icon("gear")),
                conditionalPanel(
                  condition = sprintf("input.toggle_optional_%s %% 2 == 1", model),
                  hr(),
                  h4("Optional Overrides"),
                  shiny::textInput(paste0(model, "_age_target"), paste("Optional: ", model, " Age Target"), value = "0:105"),
                  shiny::textInput(paste0(model, "_sex_target"), paste("Optional: ", model, " Sex Target"), value = "Male, Female"),
                  shiny::textInput(paste0(model, "_time_target"), paste("Optional: ", model, " Time Target"), value = "all"),
                  shiny::numericInput(paste0(model, "_lower_rate_limit"), "Lower Rate Limit", value = 1e-6),
                  shiny::sliderInput(paste0(model, "_rate_scale"), "Rate Scaler", value = 1, min = 0, max = 3, step = 0.1),
                  shiny::numericInput(paste0(model, "_rate_overide"), paste("Optional: ", model, " Rate Set"), value = -1),
                  shiny::numericInput(paste0(model, "_rate_noise"), paste("Optional: ", model, " Noise Set"), value = 0)
                )
              )
            }),
            actionButton("create_system_models", "Create DPM System Models", icon = icon("cogs")),
            actionButton("export_system_models", "Export DPM System Models", icon = icon("download"))
          )
        ),
        fluidRow(
          box(
            title = "Available & Imported DPM System Models", width = 12, status = "primary", solidHeader = TRUE,
            column(
              6,
              h4("Loaded Setups"),
              uiOutput("loadedSystemModels"),
              hr(),
              selectInput("delete_sysmod_list", "Select Setup to Delete", choices = NULL),
              actionButton("delete_button_sysmod", "Delete Selected", icon = icon("trash"))
            ),
            column(
              6,
              h4("Import Setup from File"),
              textInput("sysmod_list_file", "System Model list .RDS file", value = "default_sysmods.RDS"),
              actionButton("import_button_sysmod", "Import Setup", icon = icon("upload"))
            )
          )
        ),
        fluidRow(
          h3("Last Created System Model Summaries"),
          uiOutput("modelSummaries"),
          uiOutput("modelPlots")
        )
      ),

      # --- DPM Tab: Data Model Setup ---
      shinydashboard::tabItem(
        tabName = "dpm_data_models_tab",
        h2("Step 2.2: DPM Data Model Setup"),
        fluidRow(
          box(
            title = "Specify Data Models", width = 12, solidHeader = TRUE, status = "primary",
            p("The DPM requires at least 3 data models ('births', 'deaths', 'stock') containing the flow/stock counts. You can add more as needed."),
            fluidRow(
              lapply(c("births", "deaths"), function(model) {
                box(
                  title = paste0("'", model, "'", " Data Model (Exact)"), width = 6, solidHeader = TRUE, status = "primary",
                  textInput(paste0(model, "_counts_file"), paste(model, "Counts CSV Filename"), value = paste0("dm_", model, ".csv")),
                  actionButton(paste0("create_", model, "_data_model"), paste("Create", model, "Data Model"))
                )
              })
            ),
            fluidRow(
              box(
                title = "Add More Data Models", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
                box(
                  title = "Required Inputs", width = 12, solidHeader = TRUE,
                  textInput("dm_name", "Data Model Name"),
                  selectInput("series_name", "Choose Series Type", choices = c("population", "ins", "outs")),
                  selectInput("data_model", "Choose Data Model Type",
                    choices = c("Normal Data Model", "T-Dist Data Model", "Negative Binomial Data Model", "Poisson Data Model", "Log-Normal Data Model")
                  ),
                  textInput("counts_file", "Counts CSV Filename"),
                  uiOutput("additionalInputs")
                ),
                box(
                  title = "Optional Inputs", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                  uiOutput("optionalInputs")
                ),
                actionButton("create_data_model", "Create Data Model")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Loaded & Imported Data Models", width = 12, solidHeader = TRUE, status = "primary",
            column(8, DTOutput("loadedDataModels")),
            column(
              4,
              selectInput("delete_data_model", "Select Data Model to Delete", choices = NULL),
              actionButton("delete_button", "Delete Selected Data Model"),
              hr(),
              textInput("datamod_list_tag", "Export file tag:", value = "default_datamods"),
              actionButton("export_data_models", "Export Data Models"),
              hr(),
              textInput("datamod_list_file", "Import from .RDS file:"),
              actionButton("import_button_datamod", "Import Data Models")
            )
          )
        )
      ),

      # --- DPM Tab: Fit Single Model ---
      shinydashboard::tabItem(
        tabName = "dpm_fit_single_tab",
        h2("Step 2.3: Fit a Single DPM Account"),
        p("Select one System Model setup and one or more Data Models, then click the 'Fit DPM Account' button in the sidebar."),
        fluidRow(
          box(title = "Select System Models", width = 6, solidHeader = TRUE, status = "primary", uiOutput("system_model_checklist")),
          box(title = "Select Data Models", width = 6, solidHeader = TRUE, status = "primary", uiOutput("data_model_checklist"))
        ),
        fluidRow(
        shiny::div(
          style = "padding: 15px;",
          shiny::actionButton("fit_dpm_model_button", "Fit DPM Account", icon = shiny::icon("play"), class = "btn-primary", width = "100%")
        )),
        fluidRow(
          box(
            title = "Cohort Results", width = 6, solidHeader = TRUE, status = "success",
            textOutput("cohortResults")
          ),
          box(
            title = "Cohort Failures", width = 6, solidHeader = TRUE, status = "warning",
            DTOutput("cohortDiagnostics")
          )
        )
      ),

      # --- DPM Tab: View Results ---
      shinydashboard::tabItem(
        tabName = "dpm_results_tab",
        h2("Step 2.4: View DPM Results"),
        tabsetPanel(
          tabPanel(
            "Population Estimates",
            fluidRow(
              column(4, selectInput("time_select_pop", "Time", choices = NULL)),
              column(4, textInput("compare_select_pop", "Compare Column"))
            ),
            uiOutput("popPlots")
          ),
          tabPanel(
            "Migration Estimates",
            fluidRow(
              column(4, selectInput("time_select_mig", "Time", choices = NULL)),
              column(4, textInput("compare_select_ins", "Compare Ins")),
              column(4, textInput("compare_select_outs", "Compare Outs"))
            ),
            uiOutput("immPlots"),
            uiOutput("emPlots")
          ),
          tabPanel(
            "Results Data Tables",
            h3("Population Estimates Data"),
            DTOutput("population_table"),
            hr(),
            h3("Migration Estimates Data"),
            DTOutput("migration_table")
          )
        )
      ),

      # --- Advanced: DPM Compare ---
      shinydashboard::tabItem(
        tabName = "dpm_compare_tab",
        h2("Advanced: Compare DPM Setups"),
        shiny::fluidRow(
          shinydashboard::box(title = "Setup (1) System Models", width = 6, solidHeader = TRUE, status = "primary", shiny::uiOutput("sys_model_checklist_1")),
          shinydashboard::box(title = "Setup (2) System Models", width = 6, solidHeader = TRUE, status = "primary", shiny::uiOutput("sys_model_checklist_2"))
        ),
        shiny::fluidRow(
          shinydashboard::box(title = "Setup (1) Data Models", width = 6, solidHeader = TRUE, status = "primary", shiny::uiOutput("data_model_checklist_1")),
          shinydashboard::box(title = "Setup (2) Data Models", width = 6, solidHeader = TRUE, status = "primary", shiny::uiOutput("data_model_checklist_2"))
        ),
        shiny::fluidRow(
          shinydashboard::box(width = 12, solidHeader = TRUE, status = "primary", shiny::actionButton("compare_account_model", "Compare Models", icon = icon("balance-scale")))
        ),
        tabsetPanel(
          tabPanel(
            "Aggregate Comparisons",
            shiny::fluidRow(shinydashboard::box(
              title = "Aggregate estimates comparisons", width = 12, solidHeader = TRUE, status = "primary",
              shiny::uiOutput("compAggPop"), shiny::uiOutput("compAggImmig"), shiny::uiOutput("compAggEmig"), shiny::uiOutput("compAggNetMig"),
              shiny::uiOutput("compAggPopDiffs"), shiny::uiOutput("compAggImmigDiffs"), shiny::uiOutput("compAggEmigDiffs")
            ))
          ),
          tabPanel(
            "Population Comparisons",
            shiny::fluidRow(shiny::column(4, selectInput("time_select_comp", "Time", choices = NULL)), shiny::column(4, selectInput("setup_select_pop", "Setup", choices = c("Both", "Setup 1", "Setup 2"))), shiny::column(4, selectInput("residual_type_pop", "Residual Type", choices = c("Absolute", "Percent")))),
            shiny::fluidRow(shiny::column(6, selectInput("compare_select_pop_1", "Compare Column 1", choices = NULL)), shiny::column(6, selectInput("compare_select_pop_2", "Compare Column 2", choices = NULL))),
            shiny::uiOutput("compPlots"), shiny::uiOutput("compPlotsResiduals")
          ),
          tabPanel(
            "Migration Comparisons",
            shiny::fluidRow(shiny::column(4, selectInput("time_select_comp_mig", "Time", choices = NULL)), shiny::column(4, selectInput("setup_select_mig", "Setup", choices = c("Both", "Setup 1", "Setup 2"))), shiny::column(4, selectInput("residual_type_mig", "Residual Type", choices = c("Absolute", "Percent")))),
            shiny::fluidRow(shiny::column(3, selectInput("compare_select_ins_1", "Compare Ins 1", choices = NULL)), shiny::column(3, selectInput("compare_select_ins_2", "Compare Ins 2", choices = NULL)), shiny::column(3, selectInput("compare_select_outs_1", "Compare Outs 1", choices = NULL)), shiny::column(3, selectInput("compare_select_outs_2", "Compare Outs 2", choices = NULL))),
            shiny::uiOutput("compImmig"), shiny::uiOutput("compEmig"), shiny::uiOutput("compPlotsImmigResiduals"), shiny::uiOutput("compPlotsEmigResiduals")
          )
        )
      ),

      # --- Generated Code Tab ---
      shinydashboard::tabItem(
        tabName = "generated_code_tab",
        h2("Reproducible R Code"),
        p("Use the code below to reproduce your full analysis in a standard R script."),
        verbatimTextOutput("generated_code")
      )
    )
  )
)
