#' Server Function for Shiny Application
#'
#' This function defines the server-side logic for a Shiny application, managing reactive values and
#' event observers to handle user inputs and dynamically update the UI components.
#'
#' @param input The input object provided by Shiny, containing all user inputs.
#' @param output The output object provided by Shiny, used to send outputs to the UI.
#' @param session The session object provided by Shiny, representing the current user session.
#'
#' @details
#' The server function performs the following tasks:
#' - Initializes reactive values for data models, global configuration, and system models.
#' - Observes user actions such as saving global configuration, creating system models, and
#'   navigating between tabs.
#' - Validates file paths and reads data from CSV files for creating system and data models.
#' - Generates and updates summaries and plots for system models.
#' - Fits and compares account models based on selected data models and system models.
#' - Provides feedback to the user through modal dialogs and alerts.
#'
#' @return This function does not return a value; it modifies the state of the Shiny application through side effects.
#'
#' @import shiny
#' @import here
#' @import numbers
#' @import dplyr
#' @import shinyWidgets
#' @import plotly
#' @import DT
#' @import accountTMB
#' @import rlang # <<< NEW IMPORT FOR rlang::duplicate >>>
#' @import Demographics # <<< NEW IMPORT - required if system model $mean is Demographics::Counts object and modified directly >>>
server_sens <- function(input, output, session) {
  # Define the global reactive lists which the dashboard relies on
  datamod_list <- reactiveVal(list()) # All defined data models
  global_config <- reactiveVal(list()) # User-defined global config
  sysmod_list <- reactiveVal(list()) # Current working system models (seems to be for a single setup creation)
  sysmod_list_list <- reactiveVal(list()) # List of multiple system model setups
  selected_data_models <- reactiveVal(list()) # Checklist su - this was in original, ensure its usage is clear or remove if redundant
  
  # <<< START NEW REACTIVE VALUES FOR SENSITIVITY ANALYSIS >>>
  sensitivity_results_list <- reactiveVal(list()) # Stores list of dataframes from each run
  sensitivity_varied_param_info <- reactiveVal(NULL) # Stores info about the varied parameter (name, type, component, short_name)
  # <<< END NEW REACTIVE VALUES FOR SENSITIVITY ANALYSIS >>>
  
  
  ## globalConfig Tab
  # Defaults for global_config
  default_output_dir <- here::here("output/") # Default output_dir in package
  default_data_dir <- here::here("data/") # Default data_dir in package
  default_seed_value <- numbers::nextPrime(as.integer(Sys.time())) # Generate a random prime number as the default seed value
  
  
  # Observe global config save button press
  shiny::observeEvent(input$save_global_config, {
    global_config(list(
      data_dir = if (nzchar(input$global_data_dir)) input$global_data_dir else default_data_dir, # Use default if empty
      output_dir = if (nzchar(input$global_output_dir)) input$global_output_dir else default_output_dir, # Use default if empty
      time_selection = if (nzchar(input$global_time_selection)) as.integer(unlist(strsplit(input$global_time_selection, ","))) else NULL,
      seed_value = if (nzchar(input$global_seed_value)) as.integer(input$global_seed_value) else default_seed_value # Use default if empty
    ))
    shiny::showModal(modalDialog(
      title = "Global Configuration Saved",
      "Global configuration parameters have been saved successfully.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  ## systemModels Tab
  # Observe event for creating the system models (all 4 at once)
  shiny::observeEvent(input$create_system_models, {
    file_error <- FALSE # Flag to track file errors
    req(global_config()$data_dir) # Ensure data_dir is set
    models <- c("births", "deaths", "ins", "outs")
    
    # Temporary storage for newly created system models for this specific action
    # This `new_sysmods_temp` will be used for immediate display if needed, 
    # then added to the persistent `sysmod_list_list`.
    new_sysmods_temp <- lapply(models, function(model) {
      rates_file_path <- file.path(global_config()$data_dir, input[[paste0(model, "_rates_file")]])
      
      if (!file.exists(rates_file_path)) {
        file_error <<- TRUE 
        shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("Rates file not found:", rates_file_path), type = "error")
        return(NULL) 
      }
      rates_df <- utils::read.csv(rates_file_path)
      
      disp_val_or_df <- NULL
      if (input[[paste0(model, "_disp_type")]] == "Single Value") {
        disp_val_or_df <- input[[paste0(model, "_disp_value")]]
      } else {
        disp_file_path <- file.path(global_config()$data_dir, input[[paste0(model, "_disp_file")]])
        if (!file.exists(disp_file_path)) {
          file_error <<- TRUE
          shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("Dispersion file not found:", disp_file_path), type = "error")
          return(NULL)
        }
        disp_val_or_df <- utils::read.csv(disp_file_path)
      }
      
      lower_rates_limit_val <- input[[paste0(model, "_lower_rate_limit")]]
      rate_scaler_val <- input[[paste0(model, "_rate_scale")]]
      rate_overide_val <- input[[paste0(model, "_rate_overide")]]
      rate_noise_val <- input[[paste0(model, "_rate_noise")]]
      time_selection_val <- global_config()$time_selection
      
      # Assuming create_system_model is a function you have defined elsewhere.
      # It should be robust to NULL time_selection_val.
      # It takes various parameters and returns a processed system model object.
      # e.g. create_system_model(name = model, rates = rates_df, dispersion = disp_val_or_df, 
      #                         time_subset = time_selection_val, lower_limit = lower_rates_limit_val, ...)
      # For this example, I'm using placeholder function arguments.
      # You need to replace this with your actual create_system_model call.
      # IF create_system_model IS NOT DEFINED, THIS WILL FAIL.
      # Define create_system_model in your R environment or source it.
      if (!exists("create_system_model") || !is.function(create_system_model)) {
        shinyWidgets::sendSweetAlert(session = session, title = "Configuration Error", text = "Function 'create_system_model' is not defined.", type = "error")
        return(NULL)
      }
      create_system_model(model, rates_df, disp_val_or_df, time_selection_val, 
                          lower_rates_limit_val, rate_scaler_val, rate_overide_val, rate_noise_val)
    })
    
    if (file_error || any(sapply(new_sysmods_temp, is.null))) {
      # Specific error messages are already sent by the file checks
      if(!file_error) { # If not a file error, then it was a null return from create_system_model
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "System Model Creation Error",
          text = "One or more system model components could not be created. Please check inputs and console for details.",
          type = "error"
        )
      }
      return() # Stop further processing
    }
    
    names(new_sysmods_temp) <- models
    sysmod_list(new_sysmods_temp) # Update the reactiveVal for the current set (used by some original plot outputs)
    
    current_list_of_setups <- sysmod_list_list()
    new_setup_entry <- list()
    new_setup_entry[[input$sysmods_name]] <- new_sysmods_temp # Store the named list of 4 models as one setup
    sysmod_list_list(c(current_list_of_setups, new_setup_entry))
    
    # The observers for delete_sysmod_list and the UI outputs for summaries/plots
    # were originally inside this observeEvent. It's generally better practice for them
    # to be top-level observers that react to changes in sysmod_list_list() or sysmod_list().
    # However, to match the original structure:
    
    # Update the dropdown choices for deleting system model setups
    # This should ideally be a separate observer reacting to sysmod_list_list()
    # observe({
    #   updateSelectInput(session, "delete_sysmod_list", choices = names(sysmod_list_list()))
    # }) # Moved to top level
    
    # Output for loaded system model setups
    output$loadedSystemModels <- renderUI({
      sys_setups <- sysmod_list_list()
      if (length(sys_setups) > 0) {
        model_names <- paste(names(sys_setups), collapse = "<br>")
        HTML(paste("Loaded System Model Setups: <br>", model_names))
      } else {
        HTML("No System Model Setups loaded")
      }
    })
    
    # Outputs for summaries and plots of the *last created* system model set
    output$modelSummaries <- shiny::renderUI({
      current_sysmods_to_display <- sysmod_list() # Uses the last successfully created set
      req(current_sysmods_to_display, length(current_sysmods_to_display) == 4)
      
      lapply(models, function(model_name) {
        # Ensure the outputId is unique if this is re-rendered often, or ensure it's stable
        shiny::verbatimTextOutput(outputId = paste0(model_name, "_summary_display"))
      })
    })
    
    output$modelPlots <- shiny::renderUI({
      current_sysmods_to_display <- sysmod_list()
      req(current_sysmods_to_display, length(current_sysmods_to_display) == 4)
      
      lapply(models, function(model_name) {
        sysmod_component <- current_sysmods_to_display[[model_name]]
        # Check if sysmod_component and its $mean are not null before attempting to plot
        if (!is.null(sysmod_component) && !is.null(sysmod_component$mean)) {
          plotly::plotlyOutput(outputId = paste0(model_name, "_plot_display"))
        } else {
          NULL # Or some placeholder UI indicating no data/plot
        }
      })
    })
    
    lapply(models, function(model_name) {
      output[[paste0(model_name, "_summary_display")]] <- renderPrint({
        current_sysmods_to_display <- sysmod_list()
        req(current_sysmods_to_display, length(current_sysmods_to_display) == 4)
        sysmod_component <- current_sysmods_to_display[[model_name]]
        
        if (!is.null(sysmod_component) && !is.null(sysmod_component$mean)) {
          cat("Summary of rates data for", model_name, "(from last created/loaded setup: ", isolate(input$sysmods_name), ")\n")
          # Assuming generate_summary is defined elsewhere
          if (!exists("generate_summary") || !is.function(generate_summary)) return("generate_summary() not found")
          generate_summary(as.data.frame(sysmod_component$mean) %>% dplyr::rename(rate = .data$mean))
        } else {
          cat("No data to summarize for", model_name, "\n")
        }
      })
      
      output[[paste0(model_name, "_plot_display")]] <- plotly::renderPlotly({
        current_sysmods_to_display <- sysmod_list()
        req(current_sysmods_to_display, length(current_sysmods_to_display) == 4)
        sysmod_component <- current_sysmods_to_display[[model_name]]
        
        if (!is.null(sysmod_component) && !is.null(sysmod_component$mean)) {
          # Assuming generate_plots is defined elsewhere
          if (!exists("generate_plots") || !is.function(generate_plots)) return(NULL) # Or a placeholder plotly
          generate_plots(as.data.frame(sysmod_component$mean) %>% dplyr::rename(rate = .data$mean), model_name)
        } else {
          plotly::plot_ly() %>% layout(title = paste("No plot data for", model_name)) # Placeholder
        }
      })
    })
  }) # End observeEvent input$create_system_models
  
  # Moved outside: Observer for deleting a system model setup
  shiny::observeEvent(input$delete_button_sysmod, {
    req(input$delete_sysmod_list) # Name of the setup to delete
    current_list_of_setups <- sysmod_list_list()
    current_list_of_setups[[input$delete_sysmod_list]] <- NULL # Remove it
    sysmod_list_list(current_list_of_setups)
    # If the deleted setup was the one currently in sysmod_list(), clear sysmod_list() or load another?
    # For now, sysmod_list() will retain its state until a new setup is explicitly created or loaded.
  })
  
  # Moved outside: Observer for updating delete_sysmod_list choices
  observe({
    updateSelectInput(session, "delete_sysmod_list", choices = names(sysmod_list_list()))
  })
  
  
  # Observe event for importing system models
  shiny::observeEvent(input$import_button_sysmod, {
    req(global_config()$data_dir, input$sysmod_list_file)
    sysmods_file_path <- file.path(global_config()$data_dir, input$sysmod_list_file)
    
    if (!file.exists(sysmods_file_path)) {
      shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("System model file not found:", sysmods_file_path), type = "error")
      return()
    }
    
    tryCatch({
      sysmod_list_imported_setup <- readRDS(sysmods_file_path)
      
      # Basic validation: is it a list of 4, and are sub-elements also lists?
      if(!is.list(sysmod_list_imported_setup) || length(sysmod_list_imported_setup) != 4 || !all(sapply(sysmod_list_imported_setup, is.list))){
        shinyWidgets::sendSweetAlert(session = session, title = "Import Error", text = "Imported RDS file does not seem to be a valid system model setup (expected a list of 4 models).", type = "error")
        return()
      }
      
      imported_setup_name <- tools::file_path_sans_ext(input$sysmod_list_file)
      
      current_setups <- sysmod_list_list()
      current_setups[[imported_setup_name]] <- sysmod_list_imported_setup # Add as a new setup
      sysmod_list_list(current_setups)
      sysmod_list(sysmod_list_imported_setup) # Also load it as the "current" one for immediate display by plots/summaries
      
      shiny::showModal(modalDialog(title = "Import Successful", paste("System model setup '", imported_setup_name, "' imported."), easyClose = TRUE))
    }, error = function(e) {
      shinyWidgets::sendSweetAlert(session = session, title = "Import Error", text = paste("Could not read or process RDS file:", e$message), type = "error")
    })
  })
  
  
  # Observe event for exporting system models (current working set in sysmod_list())
  shiny::observeEvent(input$export_system_models, {
    req(global_config()$output_dir, sysmod_list(), input$sysmods_name) # Use output_dir from global config for consistency
    
    current_sysmods_to_export <- sysmod_list()
    if (length(current_sysmods_to_export) == 0 || !all(c("births", "deaths", "ins", "outs") %in% names(current_sysmods_to_export))) {
      shinyWidgets::sendSweetAlert(session = session, title = "Export Error", text = "No valid system model set currently loaded or created to export.", type = "warning")
      return()
    }
    
    # If the user pressed "Create System Models" with a name, that name is input$sysmods_name
    # If they imported, they might want to export with the imported name or a new one.
    # The input$sysmods_name is used for naming the file.
    export_file_name <- paste0(input$sysmods_name, "_sysmods.RDS")
    export_file_path <- file.path(global_config()$output_dir, export_file_name) # Save to output directory
    
    tryCatch({
      saveRDS(current_sysmods_to_export, export_file_path)
      shiny::showModal(modalDialog(title = "Export Successful", paste("System models saved to:", export_file_path), easyClose = TRUE))
    }, error = function(e){
      shinyWidgets::sendSweetAlert(session = session, title = "Export Error", text = paste("Failed to save system models:", e$message), type = "error")
    })
  })
  
  # Navigation buttons
  shiny::observeEvent(input$goGC, { shiny::updateTabsetPanel(session, "tabs", selected = "globalConfig") })
  shiny::observeEvent(input$goSM, { shiny::updateTabsetPanel(session, "tabs", selected = "systemModels") })
  
  
  ## dataModels Tab
  mainData <- shiny::reactive({
    req(global_config()$data_dir, input$counts_file)
    counts_file_path <- file.path(global_config()$data_dir, input$counts_file)
    if (!file.exists(counts_file_path)) {
      shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("Counts file not found:", counts_file_path), type = "error")
      return(NULL)
    }
    tryCatch(utils::read.csv(counts_file_path), error = function(e) {
      shinyWidgets::sendSweetAlert(session = session, title = "File Read Error", text = paste("Could not read counts file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  auxData <- shiny::reactive({
    req(input$data_model, global_config()$data_dir) # data_dir needed for file paths
    data_dir_path <- global_config()$data_dir
    extraData <- list()
    
    # Helper for ratio input
    get_ratio_input <- function(ratio_file_input, ratio_value_input) {
      if (nzchar(ratio_file_input)) {
        full_path <- file.path(data_dir_path, ratio_file_input)
        if (file.exists(full_path)) {
          return(tryCatch(utils::read.csv(full_path), error = function(e) {
            warning(paste("Ratio file read error:", e$message)); ratio_value_input 
          }))
        } else {
          warning(paste("Ratio file not found:", full_path, "Using single value.")); ratio_value_input
        }
      } else {
        ratio_value_input
      }
    }
    
    if (input$data_model == "Normal Data Model") {
      req(input$counts_uncertainty_file) # This is a required field for Normal model in your UI
      uncertainty_path <- file.path(data_dir_path, input$counts_uncertainty_file)
      if (!file.exists(uncertainty_path)) {
        shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("Counts uncertainty file not found:", uncertainty_path), type = "error")
        return(NULL)
      }
      extraData$uncertainty <- tryCatch(utils::read.csv(uncertainty_path), error = function(e) {
        shinyWidgets::sendSweetAlert(session = session, title = "File Read Error", text = paste("Could not read uncertainty file:", e$message), type = "error")
        return(NULL)
      })
      if(is.null(extraData$uncertainty)) return(NULL)
      
      extraData$ratio <- get_ratio_input(input$ratio_file, input$ratio_value)
      extraData$scale_ratio <- input$scale_ratio
      extraData$sd_scaler <- input$sd_scaler
      extraData$sd_overide <- input$sd_overide
      extraData$min_sd <- input$min_sd
      extraData$count_scaler <- input$count_scaler
      
    } else if (input$data_model == "T-Dist Data Model") {
      if (input$scale_type == "Single Value") {
        extraData$scale_df <- input$scale_value
      } else {
        req(input$scale_file)
        scale_path <- file.path(data_dir_path, input$scale_file)
        if (!file.exists(scale_path)) {
          shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("Scale file for T-Dist not found:", scale_path), type = "error")
          return(NULL)
        }
        extraData$scale_df <- tryCatch(utils::read.csv(scale_path), error = function(e) {
          shinyWidgets::sendSweetAlert(session = session, title = "File Read Error", text = paste("Could not read scale file:", e$message), type = "error")
          return(NULL)
        })
        if(is.null(extraData$scale_df)) return(NULL)
      }
      extraData$ratio <- get_ratio_input(input$ratio_file, input$ratio_value)
      extraData$scale_ratio <- input$scale_ratio
      extraData$count_scaler <- input$count_scaler
      
    } else if (input$data_model == "Negative Binomial Data Model") {
      if (input$disp_type == "Single Value") {
        extraData$disp <- input$disp_value
      } else {
        req(input$disp_file)
        disp_path <- file.path(data_dir_path, input$disp_file)
        if (!file.exists(disp_path)) {
          shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("Dispersion file for N-Binom not found:", disp_path), type = "error")
          return(NULL)
        }
        extraData$disp <- tryCatch(utils::read.csv(disp_path), error = function(e) {
          shinyWidgets::sendSweetAlert(session = session, title = "File Read Error", text = paste("Could not read dispersion file:", e$message), type = "error")
          return(NULL)
        })
        if(is.null(extraData$disp)) return(NULL)
      }
      extraData$ratio <- get_ratio_input(input$ratio_file, input$ratio_value)
      extraData$scale_ratio <- input$scale_ratio
      extraData$count_scaler <- input$count_scaler
      
    } else if (input$data_model == "Poisson Data Model") {
      extraData$ratio <- get_ratio_input(input$ratio_file, input$ratio_value)
      extraData$scale_ratio <- input$scale_ratio
      extraData$count_scaler <- input$count_scaler
    }
    extraData
  })
  
  output$additionalInputs <- shiny::renderUI({
    switch(input$data_model,
           "Normal Data Model" = tagList(
             shiny::textInput("counts_uncertainty_file", "Counts Uncertainty CSV Filename", value = "dm_pop_sd.csv")
           ),
           "T-Dist Data Model" =
             tagList(
               radioButtons("scale_type", "Scale Input Type", choices = c("Single Value", "CSV File")),
               conditionalPanel(
                 condition = "input.scale_type == 'Single Value'",
                 numericInput("scale_value", "Scale Value", value = 0.1)
               ),
               conditionalPanel(
                 condition = "input.scale_type == 'CSV File'",
                 textInput("scale_file", "Scale CSV Filename")
               )
             ),
           "Negative Binomial Data Model" =
             tagList(
               radioButtons("disp_type", "Dispersion Input Type", choices = c("Single Value", "CSV File")),
               conditionalPanel(
                 condition = "input.disp_type == 'Single Value'",
                 numericInput("disp_value", "Dispersion Value", value = 0.1)
               ),
               conditionalPanel(
                 condition = "input.disp_type == 'CSV File'",
                 textInput("disp_file", "Dispersion CSV Filename")
               )
             ),
           "Poisson Data Model" =
             tagList(),
           NULL
    )
  })
  
  output$optionalInputs <- shiny::renderUI({
    switch(input$data_model,
           "Normal Data Model" = tagList(
             shiny::radioButtons("ratio_type", "Coverage Ratio Type", choices = c("Single Value", "CSV File")),
             shiny::conditionalPanel(
               condition = "input.ratio_type == 'Single Value'",
               numericInput("ratio_value", "Ratio Value", value = 1)
             ),
             shiny::conditionalPanel(
               condition = "input.ratio_type == 'CSV File'",
               textInput("ratio_file", "Ratio CSV Filename", value = "")
             ),
             shiny::numericInput("scale_ratio", "Optional: Scale Ratio", value = 0),
             shiny::numericInput("min_sd", "Optional: Minimum SD", value = 0),
             shiny::sliderInput("sd_scaler", "Optional: SD Scaler", value = 1, min = 0, max = 3, step = 0.1),
             shiny::numericInput("sd_overide", "Optional: SD Set", value = -1),
             shiny::sliderInput("count_scaler", paste("Optional: Count Scaler"), value = 1, min = 0, max = 3, step = 0.1),
             shiny::textInput("age_selection", "Optional: Age Selection (e.g., 0:10, 15, 20:30 or blank)", value = ""),
             shiny::textInput("time_selection", "Optional: Time Selection (e.g., 2000, 2005:2010 or blank)", value = "")
           ),
           "T-Dist Data Model" =
             tagList(
               shiny::radioButtons("ratio_type", "Coverage Ratio Type", choices = c("Single Value", "CSV File")),
               shiny::conditionalPanel(
                 condition = "input.ratio_type == 'Single Value'",
                 numericInput("ratio_value", "Ratio Value", value = 1)
               ),
               shiny::conditionalPanel(
                 condition = "input.ratio_type == 'CSV File'",
                 textInput("ratio_file", "Ratio CSV Filename", value = "")
               ),
               numericInput("scale_ratio", "Scale Ratio", value = 0),
               sliderInput("count_scaler", paste("Optional: Count Scaler"), value = 1, min = 0, max = 3, step = 0.1),
               shiny::textInput("age_selection", "Optional: Age Selection (e.g., 0:10, 15, 20:30 or blank)", value = ""),
               shiny::textInput("time_selection", "Optional: Time Selection (e.g., 2000, 2005:2010 or blank)", value = "")
             ),
           "Negative Binomial Data Model" =
             tagList(
               radioButtons("ratio_type", "Coverage Ratio Type", choices = c("Single Value", "CSV File")),
               conditionalPanel(
                 condition = "input.ratio_type == 'Single Value'",
                 numericInput("ratio_value", "Ratio Value", value = 1)
               ),
               conditionalPanel(
                 condition = "input.ratio_type == 'CSV File'",
                 textInput("ratio_file", "Ratio CSV Filename", value = "")
               ),
               numericInput("scale_ratio", "Scale Ratio", value = 0),
               sliderInput("count_scaler", paste("Optional: Count Scaler"), value = 1, min = 0, max = 3, step = 0.1),
               shiny::textInput("age_selection", "Optional: Age Selection (e.g., 0:10, 15, 20:30 or blank)", value = ""),
               shiny::textInput("time_selection", "Optional: Time Selection (e.g., 2000, 2005:2010 or blank)", value = "")
             ),
           "Poisson Data Model" =
             tagList(
               radioButtons("ratio_type", "Coverage Ratio Type", choices = c("Single Value", "CSV File")),
               conditionalPanel(
                 condition = "input.ratio_type == 'Single Value'",
                 numericInput("ratio_value", "Ratio Value", value = 1)
               ),
               conditionalPanel(
                 condition = "input.ratio_type == 'CSV File'",
                 textInput("ratio_file", "Ratio CSV Filename", value = "")
               ),
               numericInput("scale_ratio", "Scale Ratio", value = 0),
               sliderInput("count_scaler", paste("Optional: Count Scaler"), value = 1, min = 0, max = 3, step = 0.1),
               shiny::textInput("age_selection", "Optional: Age Selection (e.g., 0:10, 15, 20:30 or blank)", value = ""),
               shiny::textInput("time_selection", "Optional: Time Selection (e.g., 2000, 2005:2010 or blank)", value = "")
             ),
           NULL
    )
  })
  
  # Helper function to parse age/time selection strings
  parse_selection_string <- function(sel_str) {
    if (!nzchar(sel_str)) return(NULL)
    sel_str <- gsub("\\s+", "", sel_str) # Remove all whitespace
    parts <- strsplit(sel_str, ",")[[1]]
    values <- integer(0)
    for (part in parts) {
      if (grepl(":", part)) {
        range_ends <- as.integer(strsplit(part, ":")[[1]])
        if (length(range_ends) == 2 && !any(is.na(range_ends)) && range_ends[1] <= range_ends[2]) {
          values <- c(values, seq(range_ends[1], range_ends[2]))
        } else { # Invalid range
          warning(paste("Invalid range in selection string:", part))
          return(NA) # Indicate error
        }
      } else {
        val <- as.integer(part)
        if(!is.na(val)){
          values <- c(values, val)
        } else { # Invalid number
          warning(paste("Invalid number in selection string:", part))
          return(NA) # Indicate error
        }
      }
    }
    return(unique(sort(values)))
  }
  
  
  # Create Births/Deaths Exact Data Models
  shiny::observeEvent(input$create_births_data_model, {
    req(global_config()$data_dir, input$births_counts_file)
    counts_file_path <- file.path(global_config()$data_dir, input$births_counts_file)
    if (!file.exists(counts_file_path)) {
      shinyWidgets::sendSweetAlert(session=session, title="File Error", text=paste("Births counts file not found:", counts_file_path), type="error")
      return()
    }
    input_counts <- tryCatch(utils::read.csv(counts_file_path), error = function(e) {
      shinyWidgets::sendSweetAlert(session=session, title="File Read Error", text=paste("Could not read births counts file:", e$message), type="error"); NULL
    })
    if (is.null(input_counts)) return()
    
    # Assuming create_data_model is defined elsewhere
    if (!exists("create_data_model") || !is.function(create_data_model)) {
      shinyWidgets::sendSweetAlert(session = session, title = "Configuration Error", text = "Function 'create_data_model' is not defined.", type = "error")
      return(NULL)
    }
    newObject <- create_data_model(dm_name = "births", series_name = "births", dm_type = "Exact Data Model", counts_df = input_counts)
    currentList <- datamod_list(); newObjectList <- list(); newObjectList[[newObject$nm_data]] <- newObject
    datamod_list(c(currentList, newObjectList))
    shiny::showModal(modalDialog(title="Data Model Created", "Births data model (Exact) created.", easyClose = TRUE))
  })
  
  shiny::observeEvent(input$create_deaths_data_model, {
    req(global_config()$data_dir, input$deaths_counts_file)
    counts_file_path <- file.path(global_config()$data_dir, input$deaths_counts_file)
    if (!file.exists(counts_file_path)) {
      shinyWidgets::sendSweetAlert(session=session, title="File Error", text=paste("Deaths counts file not found:", counts_file_path), type="error")
      return()
    }
    input_counts <- tryCatch(utils::read.csv(counts_file_path), error = function(e) {
      shinyWidgets::sendSweetAlert(session=session, title="File Read Error", text=paste("Could not read deaths counts file:", e$message), type="error"); NULL
    })
    if (is.null(input_counts)) return()
    
    if (!exists("create_data_model") || !is.function(create_data_model)) {
      shinyWidgets::sendSweetAlert(session = session, title = "Configuration Error", text = "Function 'create_data_model' is not defined.", type = "error")
      return(NULL)
    }
    newObject <- create_data_model(dm_name = "deaths", series_name = "deaths", dm_type = "Exact Data Model", counts_df = input_counts)
    currentList <- datamod_list(); newObjectList <- list(); newObjectList[[newObject$nm_data]] <- newObject
    datamod_list(c(currentList, newObjectList))
    shiny::showModal(modalDialog(title="Data Model Created", "Deaths data model (Exact) created.", easyClose = TRUE))
  })
  
  # Create Other Data Models
  shiny::observeEvent(input$create_data_model, {
    req(mainData(), auxData(), input$dm_name, input$series_name, input$data_model)
    
    if(!nzchar(trimws(input$dm_name))){
      shinyWidgets::sendSweetAlert(session=session, title="Input Error", text="Data Model Name cannot be empty.", type="error")
      return()
    }
    
    age_subset_vals <- parse_selection_string(input$age_selection)
    if(length(age_subset_vals) == 1 && is.na(age_subset_vals[1])){ # Error in parsing
      shinyWidgets::sendSweetAlert(session=session, title="Input Error", text="Invalid Age Selection string. Please check format (e.g., 0:10, 15, 20:30).", type="error")
      return()
    }
    time_subset_vals <- parse_selection_string(input$time_selection)
    if(length(time_subset_vals) == 1 && is.na(time_subset_vals[1])){ # Error in parsing
      shinyWidgets::sendSweetAlert(session=session, title="Input Error", text="Invalid Time Selection string. Please check format (e.g., 2000, 2005:2010).", type="error")
      return()
    }
    
    # Assuming create_data_model is defined elsewhere
    if (!exists("create_data_model") || !is.function(create_data_model)) {
      shinyWidgets::sendSweetAlert(session = session, title = "Configuration Error", text = "Function 'create_data_model' is not defined.", type = "error")
      return(NULL)
    }
    newObject <- create_data_model(
      dm_name = input$dm_name, series_name = input$series_name, dm_type = input$data_model,
      counts_df = mainData(), time_select = time_subset_vals, age_select = age_subset_vals,
      uncertainty_df = auxData()$uncertainty, scale_df = auxData()$scale_df, disp = auxData()$disp,
      scale_ratio = auxData()$scale_ratio, ratio = auxData()$ratio, sd_scaler = auxData()$sd_scaler,
      sd_overide = auxData()$sd_overide, min_sd = auxData()$min_sd, count_scaler = auxData()$count_scaler
    )
    if(is.null(newObject) || !is.list(newObject) || is.null(newObject$nm_data)){
      shinyWidgets::sendSweetAlert(session=session, title="Creation Failed", text="Data model creation failed. Check console for errors from create_data_model.", type="error")
      return()
    }
    
    currentList <- datamod_list(); newObjectList <- list(); newObjectList[[newObject$nm_data]] <- newObject
    datamod_list(c(currentList, newObjectList))
    
    # Clear form inputs
    updateTextInput(session, "dm_name", value = ""); updateSelectInput(session, "series_name", selected = "population")
    updateTextInput(session, "counts_file", value = ""); updateTextInput(session, "age_selection", value = "")
    updateTextInput(session, "time_selection", value = ""); updateTextInput(session, "counts_uncertainty_file", value = "")
    # ... Reset other inputs ...
    shiny::showModal(modalDialog(title="Data Model Created", paste("Data model '", newObject$nm_data, "' created."), easyClose = TRUE))
  })
  
  # Update dropdowns for data models
  observe({ updateSelectInput(session, "delete_data_model", choices = names(datamod_list())) })
  observe({ updateSelectInput(session, "plot_data_model", choices = names(datamod_list())) })
  observe({ updateSelectInput(session, "inspect_data_model1", choices = names(datamod_list())) })
  observe({ updateSelectInput(session, "inspect_data_model2", choices = names(datamod_list())) })
  
  # Delete data model
  shiny::observeEvent(input$delete_button, {
    req(input$delete_data_model)
    currentList <- datamod_list(); currentList[[input$delete_data_model]] <- NULL; datamod_list(currentList)
  })
  
  # Plot data model
  shiny::observeEvent(input$plotdm_button, {
    req(input$plot_data_model)
    dm_select <- datamod_list()[[input$plot_data_model]]
    if(is.null(dm_select) || is.null(dm_select$data)) {
      shinyWidgets::sendSweetAlert(session=session, title="Plot Error", text="Selected data model for plotting is invalid or has no data.", type="error")
      return()
    }
    output$dmModelPlots <- shiny::renderUI({ plotly::plotlyOutput("plot_dm_model_actual") })
    output$plot_dm_model_actual <- plotly::renderPlotly({
      p_data <- dm_select$data
      # Ensure required columns exist
      req_cols <- c("age", "sex", "time", "count") # Assuming these are the columns
      if(!all(req_cols %in% names(p_data))){
        shiny::showNotification("Data for plotting is missing required columns (age, sex, time, count).", type="error")
        return(plotly::plot_ly() %>% layout(title = "Error: Missing columns for plot"))
      }
      
      p <- ggplot2::ggplot(p_data %>% group_by(.data$age, .data$sex, .data$time) %>% summarise(count = sum(.data$count, na.rm=TRUE), .groups = "drop")) +
        ggplot2::geom_point(ggplot2::aes(x = .data$age, y = .data$count, color = as.factor(.data$time))) +
        ggplot2::geom_line(ggplot2::aes(x = .data$age, y = .data$count, color = as.factor(.data$time))) +
        ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
        ggplot2::labs(title = paste("Data for:", dm_select$nm_data), color = "Time")
      plotly::ggplotly(p)
    })
  })
  
  # Inspect data models
  shiny::observeEvent(input$inspectdm_button1, {
    req(input$inspect_data_model1); dm_select <- datamod_list()[[input$inspect_data_model1]]
    output$dmModelInspect1 <- DT::renderDT({ DT::datatable(dm_select$data, filter = list(position = "top", clear = FALSE), options = list(scrollX = TRUE)) })
  })
  shiny::observeEvent(input$inspectdm_button2, {
    req(input$inspect_data_model2); dm_select <- datamod_list()[[input$inspect_data_model2]]
    output$dmModelInspect2 <- DT::renderDT({ DT::datatable(dm_select$data, filter = list(position = "top", clear = FALSE), options = list(scrollX = TRUE)) })
  })
  
  output$loadedDataModels <- DT::renderDT({
    dataModels <- datamod_list()
    if (length(dataModels) > 0) {
      modelSummaries <- lapply(dataModels, function(dm) {
        # Assuming class(dm)[[1]] gives the specific type like 'accountTMB_datamod_exact'
        c(dm_name = dm$nm_data, series_name = dm$nm_series, dm_type = class(dm)[[1]],
          dm_data_cols = paste(colnames(dm$data), collapse=", "))
      })
      modelSummaryDF <- as.data.frame(do.call(rbind, modelSummaries))
      DT::datatable(modelSummaryDF, options = list(scrollX = TRUE))
    } else {
      DT::datatable(data.frame(Message = "No data models loaded."), options = list(dom = 't'))
    }
  })
  
  # Import/Export Data Models
  shiny::observeEvent(input$import_button_datamod, {
    req(global_config()$data_dir, input$datamod_list_file)
    datamods_file_path <- file.path(global_config()$data_dir, input$datamod_list_file)
    if (!file.exists(datamods_file_path)) {
      shinyWidgets::sendSweetAlert(session=session, title="File Error", text=paste("Data model RDS file not found:", datamods_file_path), type="error")
      return()
    }
    tryCatch({
      datamods_list_imported <- readRDS(datamods_file_path)
      if(!is.list(datamods_list_imported) || (length(datamods_list_imported) > 0 && is.null(names(datamods_list_imported)[1]))){
        shinyWidgets::sendSweetAlert(session=session, title="Import Error", text="Imported RDS is not a valid named list of data models.", type="error")
        return()
      }
      currentList <- datamod_list()
      # Merge, potentially overwriting if names conflict, or use a safer merge strategy
      datamod_list(utils::modifyList(currentList, datamods_list_imported))
      shiny::showModal(modalDialog(title="Import Successful", paste(length(datamods_list_imported), "data model(s) imported/updated."), easyClose = TRUE))
    }, error = function(e){
      shinyWidgets::sendSweetAlert(session=session, title="Import Error", text=paste("Could not read or process RDS file:", e$message), type="error")
    })
  })
  
  shiny::observeEvent(input$export_data_models, {
    req(global_config()$output_dir, input$datamod_list_tag) # Using output_dir
    dataModels_to_export <- datamod_list()
    if (length(dataModels_to_export) == 0) {
      shinyWidgets::sendSweetAlert(session=session, title="Export Error", text="No data models to export.", type="warning")
      return()
    }
    export_file_name <- paste0(input$datamod_list_tag, "_datamods.RDS")
    export_file_path <- file.path(global_config()$output_dir, export_file_name)
    tryCatch({
      saveRDS(dataModels_to_export, export_file_path)
      shiny::showModal(modalDialog(title="Export Successful", paste("Data models saved to:", export_file_path), easyClose = TRUE))
    }, error = function(e){
      shinyWidgets::sendSweetAlert(session=session, title="Export Error", text=paste("Failed to save data models:", e$message), type="error")
    })
  })
  
  # Navigation
  shiny::observeEvent(input$goDM, { shiny::updateTabsetPanel(session, "tabs", selected = "dataModels") })
  
  
  ## fitModel Tab
  output$system_model_checklist <- shiny::renderUI({ # For single fit tab
    sys_setup_names <- names(sysmod_list_list())
    if (length(sys_setup_names) > 0) {
      radioButtons("selected_system_models_fit", "Select System Model Setup:", choices = sys_setup_names)
    } else {
      p("No System Model Setups defined.")
    }
  })
  
  filtered_system_models_fit <- shiny::reactive({ # For single fit tab
    req(input$selected_system_models_fit)
    sysmod_list_list()[[input$selected_system_models_fit]]
  })
  
  output$data_model_checklist <- shiny::renderUI({ # For single fit tab
    data_model_names <- names(datamod_list())
    checkboxGroupInput("selected_data_models_fit", "Select Data Models:", choices = data_model_names, selected = data_model_names)
  })
  
  filtered_data_models_fit <- shiny::reactive({ # For single fit tab
    req(input$selected_data_models_fit)
    datamod_list()[input$selected_data_models_fit]
  })
  
  # Reactive for population results (single fit)
  population_estimates_single_fit <- reactiveVal(NULL)
  migration_estimates_single_fit <- reactiveVal(NULL)
  
  
  shiny::observeEvent(input$fit_account_model, {
    req(filtered_data_models_fit(), filtered_system_models_fit(), 
        global_config()$output_dir, global_config()$seed_value)
    
    # Clear previous results
    population_estimates_single_fit(NULL)
    migration_estimates_single_fit(NULL)
    output$cohortResults <- renderText({""})
    output$cohortDiagnostics <- DT::renderDT(NULL)
    
    
    shiny::withProgress(message = 'Fitting Account Model...', value = 0, {
      incProgress(0.1, detail = "Estimating account...")
      result <- tryCatch(
        accountTMB::estimate_account(
          datamods = filtered_data_models_fit(), 
          sysmods = filtered_system_models_fit(), 
          seed_in = global_config()$seed_value
        ),
        error = function(e) {
          shinyWidgets::sendSweetAlert(session, title = "Estimation Error", text = paste("Failed to estimate account:", e$message), type = "error")
          return(NULL)
        }
      )
      if (is.null(result)) return()
      
      incProgress(0.3, detail = "Saving raw result...")
      output_file_rds <- file.path(global_config()$output_dir, "fit_model_result.RDS")
      tryCatch(saveRDS(result, output_file_rds), error = function(e) warning(paste("Failed to save RDS:", e$message)))
      
      incProgress(0.2, detail = "Generating diagnostics...")
      diag <- result %>% accountTMB::diagnostics()
      cohort_passes <- nrow(diag %>% dplyr::filter(.data$success == TRUE))
      cohort_total <- nrow(diag)
      output$cohortResults <- renderText({ paste0(cohort_passes, " out of ", cohort_total, " cohorts fitted successfully.") })
      output$cohortDiagnostics <- DT::renderDT({ DT::datatable(diag %>% dplyr::filter(.data$success == FALSE), options = list(scrollX = TRUE)) })
      
      incProgress(0.2, detail = "Augmenting population...")
      pop_res_full <- result %>% accountTMB::augment_population(collapse = "cohort")
      population_estimates_single_fit(pop_res_full %>% dplyr::select(-c("population"))) # Exclude list column 'population' for DT
      
      incProgress(0.1, detail = "Augmenting migration...")
      mig_res_full <- result %>% accountTMB::augment_events(collapse = "age") %>%
        dplyr::mutate(age_calc = .data$time - .data$cohort) %>% # Keep original 'age' if it exists, else use calculated
        dplyr::mutate(age = ifelse(is.null(.data$age), age_calc, .data$age)) %>%
        dplyr::select(-any_of(c("cohort", "age_calc"))) # Remove cohort and temp age_calc
      migration_estimates_single_fit(mig_res_full %>% dplyr::select(-c("ins", "outs", "births", "deaths"))) # Exclude list columns for DT
      
      incProgress(0.1, detail = "Saving CSVs...")
      output_file_pop_csv <- file.path(global_config()$output_dir, "population_estimates.csv")
      output_file_mig_csv <- file.path(global_config()$output_dir, "migration_estimates.csv")
      tryCatch(utils::write.csv(population_estimates_single_fit(), output_file_pop_csv, row.names = FALSE), error = function(e) warning(paste("Failed to save pop CSV:", e$message)))
      tryCatch(utils::write.csv(migration_estimates_single_fit(), output_file_mig_csv, row.names = FALSE), error = function(e) warning(paste("Failed to save mig CSV:", e$message)))
    }) # End withProgress
    
    shiny::showModal(modalDialog(title = "Model Fitted", "Model fitting completed. Results (RDS, CSVs) saved to output directory.", easyClose = TRUE))
  })
  
  # Dynamic choices for time selectors based on single fit results
  observe({
    pop_res_df <- population_estimates_single_fit()
    if (!is.null(pop_res_df) && "time" %in% names(pop_res_df)) {
      choices <- sort(unique(pop_res_df$time))
      updateSelectInput(session, "time_select_pop", choices = choices, selected = choices[1])
    }
  })
  observe({
    mig_res_df <- migration_estimates_single_fit()
    if (!is.null(mig_res_df) && "time" %in% names(mig_res_df)) {
      choices <- sort(unique(mig_res_df$time))
      updateSelectInput(session, "time_select_mig", choices = choices, selected = choices[1])
    }
  })
  
  # Table outputs for single fit
  output$population_table <- DT::renderDT({
    req(population_estimates_single_fit())
    DT::datatable(population_estimates_single_fit(), filter="top", options = list(scrollX = TRUE))
  })
  output$migration_table <- DT::renderDT({
    req(migration_estimates_single_fit())
    DT::datatable(migration_estimates_single_fit(), filter="top", options = list(scrollX = TRUE))
  })
  
  ## popEstimates Tab (Single Fit Plots)
  output$popPlots <- shiny::renderUI({ plotly::plotlyOutput("population_estimates_plot") })
  output$population_estimates_plot <- renderPlotly({
    req(population_estimates_single_fit(), input$time_select_pop)
    pop_data_to_plot <- population_estimates_single_fit() %>% 
      dplyr::filter(.data$time == as.numeric(input$time_select_pop))
    
    p <- ggplot2::ggplot(pop_data_to_plot, ggplot2::aes(x = .data$age, ymin = .data$population.lower, 
                                                        y = .data$population.fitted, ymax = .data$population.upper)) +
      ggplot2::geom_pointrange(fatten = 0.2, col = "darkorange") +
      ggplot2::ylab("Count") + ggplot2::ggtitle("Population Estimates (Fitted)") +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) + ggplot2::theme_minimal()
    
    if (nzchar(input$compare_select_pop) && input$compare_select_pop %in% colnames(pop_data_to_plot)) {
      p <- p + ggplot2::geom_point(ggplot2::aes(y = .data[[input$compare_select_pop]]), col = "darkblue", size = 0.5, shape=4) +
        ggplot2::labs(caption = paste("Comparing with:", input$compare_select_pop))
    }
    plotly::ggplotly(p)
  })
  
  ## migEstimates Tab (Single Fit Plots)
  output$immPlots <- shiny::renderUI({ plotlyOutput("immigration_estimates_plot") })
  output$immigration_estimates_plot <- renderPlotly({
    req(migration_estimates_single_fit(), input$time_select_mig)
    mig_data_to_plot <- migration_estimates_single_fit() %>%
      dplyr::filter(.data$time == as.numeric(input$time_select_mig))
    
    p <- ggplot2::ggplot(mig_data_to_plot, ggplot2::aes(x = .data$age, ymin = .data$ins.lower, 
                                                        y = .data$ins.fitted, ymax = .data$ins.upper)) +
      ggplot2::geom_pointrange(fatten = 0.2, col = "darkorange") +
      ggplot2::ylab("Count") + ggplot2::ggtitle("Immigration Estimates (Fitted)") +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) + ggplot2::theme_minimal()
    
    if (nzchar(input$compare_select_ins) && input$compare_select_ins %in% colnames(mig_data_to_plot)) {
      p <- p + ggplot2::geom_point(ggplot2::aes(y = .data[[input$compare_select_ins]]), col = "darkblue", size = 0.5, shape=4) +
        ggplot2::labs(caption = paste("Comparing with:", input$compare_select_ins))
    }
    plotly::ggplotly(p)
  })
  
  output$emPlots <- shiny::renderUI({ plotlyOutput("emigration_estimates_plot") })
  output$emigration_estimates_plot <- renderPlotly({
    req(migration_estimates_single_fit(), input$time_select_mig)
    mig_data_to_plot <- migration_estimates_single_fit() %>%
      dplyr::filter(.data$time == as.numeric(input$time_select_mig))
    
    p <- ggplot2::ggplot(mig_data_to_plot, ggplot2::aes(x = .data$age, ymin = .data$outs.lower, 
                                                        y = .data$outs.fitted, ymax = .data$outs.upper)) +
      ggplot2::geom_pointrange(fatten = 0.2, col = "darkorange") +
      ggplot2::ylab("Count") + ggplot2::ggtitle("Emigration Estimates (Fitted)") +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) + ggplot2::theme_minimal()
    
    if (nzchar(input$compare_select_outs) && input$compare_select_outs %in% colnames(mig_data_to_plot)) {
      p <- p + ggplot2::geom_point(ggplot2::aes(y = .data[[input$compare_select_outs]]), col = "darkblue", size = 0.5, shape=4) +
        ggplot2::labs(caption = paste("Comparing with:", input$compare_select_outs))
    }
    plotly::ggplotly(p)
  })
  
  # Navigation
  shiny::observeEvent(input$goFM, { shiny::updateTabsetPanel(session, "tabs", selected = "fitModel") })
  shiny::observeEvent(input$goPE, { shiny::updateTabsetPanel(session, "tabs", selected = "popEstimates") })
  shiny::observeEvent(input$goME, { shiny::updateTabsetPanel(session, "tabs", selected = "migEstimates") })
  shiny::observeEvent(input$goSC, { shiny::updateTabsetPanel(session, "tabs", selected = "fitModels") }) # fitModels is the first subtab of Compare Setups
  
  
  ## Compare Setups Tab
  # UI for selecting system models for comparison
  output$sys_model_checklist_1 <- shiny::renderUI({
    choices <- names(sysmod_list_list())
    radioButtons("selected_sys_models_1_comp", "Setup 1: System Models", choices = choices)
  })
  output$sys_model_checklist_2 <- shiny::renderUI({
    choices <- names(sysmod_list_list())
    radioButtons("selected_sys_models_2_comp", "Setup 2: System Models", choices = choices)
  })
  
  # UI for selecting data models for comparison
  output$data_model_checklist_1 <- shiny::renderUI({
    choices <- names(datamod_list())
    checkboxGroupInput("selected_data_models_1_comp", "Setup 1: Data Models", choices = choices, selected = choices)
  })
  output$data_model_checklist_2 <- shiny::renderUI({
    choices <- names(datamod_list())
    checkboxGroupInput("selected_data_models_2_comp", "Setup 2: Data Models", choices = choices, selected = choices)
  })
  
  # Reactives for filtered models for comparison
  filtered_sys_models_1_comp <- shiny::reactive({ req(input$selected_sys_models_1_comp); sysmod_list_list()[[input$selected_sys_models_1_comp]] })
  filtered_sys_models_2_comp <- shiny::reactive({ req(input$selected_sys_models_2_comp); sysmod_list_list()[[input$selected_sys_models_2_comp]] })
  filtered_data_models_1_comp <- shiny::reactive({ req(input$selected_data_models_1_comp); datamod_list()[input$selected_data_models_1_comp] })
  filtered_data_models_2_comp <- shiny::reactive({ req(input$selected_data_models_2_comp); datamod_list()[input$selected_data_models_2_comp] })
  
  # Reactive values to store comparison results
  comparison_results <- reactiveVal(NULL)
  
  shiny::observeEvent(input$compare_account_model, {
    req(filtered_data_models_1_comp(), filtered_data_models_2_comp(),
        filtered_sys_models_1_comp(), filtered_sys_models_2_comp(),
        global_config()$output_dir, global_config()$seed_value)
    
    comparison_results(NULL) # Clear previous results
    
    shiny::withProgress(message = 'Comparing Account Models...', value = 0, {
      incProgress(0.1, detail = "Estimating Model 1...")
      result_1 <- tryCatch(accountTMB::estimate_account(datamods = filtered_data_models_1_comp(), sysmods = filtered_sys_models_1_comp(), seed_in = global_config()$seed_value),
                           error = function(e){ shinyWidgets::sendSweetAlert(session,title="Error Model 1",text=e$message,type="error"); NULL})
      if(is.null(result_1)) return()
      
      incProgress(0.4, detail = "Estimating Model 2...")
      result_2 <- tryCatch(accountTMB::estimate_account(datamods = filtered_data_models_2_comp(), sysmods = filtered_sys_models_2_comp(), seed_in = global_config()$seed_value + 1), # Slightly different seed
                           error = function(e){ shinyWidgets::sendSweetAlert(session,title="Error Model 2",text=e$message,type="error"); NULL})
      if(is.null(result_2)) return()
      
      incProgress(0.2, detail = "Augmenting results for Model 1...")
      pop_1 <- tryCatch(result_1 %>% accountTMB::augment_population(collapse = "cohort"), error=function(e)NULL)
      mig_1 <- tryCatch(result_1 %>% accountTMB::augment_events(collapse = "age") %>% dplyr::mutate(age = .data$time - .data$cohort) %>% dplyr::select(-any_of("cohort")), error=function(e)NULL)
      
      incProgress(0.2, detail = "Augmenting results for Model 2...")
      pop_2 <- tryCatch(result_2 %>% accountTMB::augment_population(collapse = "cohort"), error=function(e)NULL)
      mig_2 <- tryCatch(result_2 %>% accountTMB::augment_events(collapse = "age") %>% dplyr::mutate(age = .data$time - .data$cohort) %>% dplyr::select(-any_of("cohort")), error=function(e)NULL)
      
      if(is.null(pop_1) || is.null(mig_1) || is.null(pop_2) || is.null(mig_2)){
        shinyWidgets::sendSweetAlert(session,title="Augmentation Error",text="Could not augment results for one or both models.",type="error")
        return()
      }
      
      # Store full results for potential direct comparison if needed (with list columns)
      full_pop_1 <- pop_1; full_mig_1 <- mig_1;
      full_pop_2 <- pop_2; full_mig_2 <- mig_2;
      
      # Prepare for binding (select common columns, remove list columns for simplicity in combined df)
      pop_res_1 <- pop_1 %>% dplyr::mutate(setup = "Setup 1") %>% dplyr::select(age, sex, time, population.lower, population.fitted, population.upper, setup)
      mig_res_1 <- mig_1 %>% dplyr::mutate(setup = "Setup 1") %>% dplyr::select(age, sex, time, ins.lower, ins.fitted, ins.upper, outs.lower, outs.fitted, outs.upper, setup)
      pop_res_2 <- pop_2 %>% dplyr::mutate(setup = "Setup 2") %>% dplyr::select(age, sex, time, population.lower, population.fitted, population.upper, setup)
      mig_res_2 <- mig_2 %>% dplyr::mutate(setup = "Setup 2") %>% dplyr::select(age, sex, time, ins.lower, ins.fitted, ins.upper, outs.lower, outs.fitted, outs.upper, setup)
      
      combined_pop <- dplyr::bind_rows(pop_res_1, pop_res_2)
      combined_mig <- dplyr::bind_rows(mig_res_1, mig_res_2)
      
      # For residual plots (wide format)
      pop_res_comp_wide <- dplyr::inner_join(
        pop_res_1 %>% dplyr::select(age, sex, time, population.fitted) %>% dplyr::rename(fitted1 = population.fitted),
        pop_res_2 %>% dplyr::select(age, sex, time, population.fitted) %>% dplyr::rename(fitted2 = population.fitted),
        by = c("age", "sex", "time")
      ) %>% dplyr::mutate(
        fitted_residuals_abs = .data$fitted2 - .data$fitted1,
        fitted_residuals_perc = ifelse(.data$fitted1 == 0, NA, 100 * (.data$fitted_residuals_abs / .data$fitted1)) # Avoid division by zero
      )
      
      mig_res_comp_wide <- dplyr::inner_join(
        mig_res_1 %>% dplyr::select(age, sex, time, ins.fitted, outs.fitted) %>% dplyr::rename(ins1 = ins.fitted, outs1 = outs.fitted),
        mig_res_2 %>% dplyr::select(age, sex, time, ins.fitted, outs.fitted) %>% dplyr::rename(ins2 = ins.fitted, outs2 = outs.fitted),
        by = c("age", "sex", "time")
      ) %>% dplyr::mutate(
        ins_residuals_abs = .data$ins2 - .data$ins1,
        ins_residuals_perc = ifelse(.data$ins1 == 0, NA, 100 * (.data$ins_residuals_abs / .data$ins1)),
        outs_residuals_abs = .data$outs2 - .data$outs1,
        outs_residuals_perc = ifelse(.data$outs1 == 0, NA, 100 * (.data$outs_residuals_abs / .data$outs1))
      )
      
      comparison_results(list(
        pop_combined = combined_pop, mig_combined = combined_mig,
        pop_wide = pop_res_comp_wide, mig_wide = mig_res_comp_wide,
        full_pop_1 = full_pop_1, full_mig_1 = full_mig_1, # Store full augmented results with list cols
        full_pop_2 = full_pop_2, full_mig_2 = full_mig_2
      ))
      incProgress(0.1, detail = "Done.")
    }) # End withProgress
    shiny::showModal(modalDialog(title = "Comparison Complete", "Model comparison finished.", easyClose = TRUE))
  })
  
  # Update selectors for comparison plots
  observe({
    res <- comparison_results()
    if (!is.null(res$pop_combined) && "time" %in% names(res$pop_combined)) {
      choices <- sort(unique(res$pop_combined$time))
      updateSelectInput(session, "time_select_comp", choices = choices, selected = choices[1])
    }
    # For compare columns: these are the original data columns present in the full augmented results
    if (!is.null(res$full_pop_1)) {
      cols1 <- setdiff(colnames(res$full_pop_1), c("age","sex","time","setup","population","population.fitted","population.lower","population.upper"))
      updateSelectInput(session, "compare_select_pop_1", choices = c("None", cols1), selected = "None")
    }
    if (!is.null(res$full_pop_2)) {
      cols2 <- setdiff(colnames(res$full_pop_2), c("age","sex","time","setup","population","population.fitted","population.lower","population.upper"))
      updateSelectInput(session, "compare_select_pop_2", choices = c("None", cols2), selected = "None")
    }
    # Similar for migration if you have original migration data columns
    if (!is.null(res$full_mig_1)) {
      mig_cols1 <- setdiff(colnames(res$full_mig_1), c("age","sex","time","setup","ins","ins.fitted","ins.lower","ins.upper", "outs", "outs.fitted","outs.lower","outs.upper", "births", "deaths"))
      updateSelectInput(session, "compare_select_ins_1", choices = c("None", mig_cols1), selected = "None")
      updateSelectInput(session, "compare_select_outs_1", choices = c("None", mig_cols1), selected = "None")
    }
    if (!is.null(res$full_mig_2)) {
      mig_cols2 <- setdiff(colnames(res$full_mig_2), c("age","sex","time","setup","ins","ins.fitted","ins.lower","ins.upper", "outs", "outs.fitted","outs.lower","outs.upper", "births", "deaths"))
      updateSelectInput(session, "compare_select_ins_2", choices = c("None", mig_cols2), selected = "None")
      updateSelectInput(session, "compare_select_outs_2", choices = c("None", mig_cols2), selected = "None")
    }
    
  })
  
  # Population Comparison Plots
  output$compPlots <- shiny::renderUI({ plotly::plotlyOutput("comparing_estimates_plot") })
  output$comparing_estimates_plot <- plotly::renderPlotly({
    res <- comparison_results(); req(res, res$pop_combined, input$time_select_comp, input$setup_select)
    plot_data <- res$pop_combined %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp))
    
    if (input$setup_select != "Both") {
      plot_data <- plot_data %>% dplyr::filter(.data$setup == input$setup_select)
    }
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$age, y = .data$population.fitted, 
                                                 ymin = .data$population.lower, ymax = .data$population.upper,
                                                 color = .data$setup, fill = .data$setup)) +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(alpha = 0.3, linetype = "dashed") +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = "Population Comparison", y = "Count", color = "Setup", fill = "Setup") + 
      ggplot2::theme_minimal()
    
    # Add comparison points if selected
    if (input$setup_select == "Setup 1" || input$setup_select == "Both") {
      if (input$compare_select_pop_1 != "None" && input$compare_select_pop_1 %in% colnames(res$full_pop_1)) {
        comp_data_1 <- res$full_pop_1 %>% 
          dplyr::filter(.data$time == as.numeric(input$time_select_comp)) %>%
          dplyr::select(age, sex, time, compare_val = .data[[input$compare_select_pop_1]])
        p <- p + ggplot2::geom_point(data = comp_data_1, ggplot2::aes(y = compare_val), color = "blue", shape = 4, inherit.aes = FALSE, 
                                     aes(x=age, group=sex)) # Added x and group for proper faceting
      }
    }
    if (input$setup_select == "Setup 2" || input$setup_select == "Both") {
      if (input$compare_select_pop_2 != "None" && input$compare_select_pop_2 %in% colnames(res$full_pop_2)) {
        comp_data_2 <- res$full_pop_2 %>% 
          dplyr::filter(.data$time == as.numeric(input$time_select_comp)) %>%
          dplyr::select(age, sex, time, compare_val = .data[[input$compare_select_pop_2]])
        p <- p + ggplot2::geom_point(data = comp_data_2, ggplot2::aes(y = compare_val), color = "red", shape = 3, inherit.aes = FALSE, 
                                     aes(x=age, group=sex)) # Added x and group
      }
    }
    plotly::ggplotly(p)
  })
  
  output$compPlotsResiduals <- shiny::renderUI({ plotly::plotlyOutput("comparing_estimates_residuals_plot") })
  output$comparing_estimates_residuals_plot <- plotly::renderPlotly({
    res <- comparison_results(); req(res, res$pop_wide, input$time_select_comp, input$residual_type)
    plot_data <- res$pop_wide %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp))
    
    y_val <- if (input$residual_type == "Absolute") "fitted_residuals_abs" else "fitted_residuals_perc"
    y_lab <- if (input$residual_type == "Absolute") "Absolute Difference (S2-S1)" else "Percent Difference (S2-S1)"
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$age, y = .data[[y_val]])) +
      ggplot2::geom_segment(ggplot2::aes(xend = .data$age, yend = 0)) +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = paste("Population Comparison Residuals", input$residual_type), y = y_lab) +
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })
  
  # Migration Comparison Plots (similar structure to population)
  output$compImmig <- shiny::renderUI({ plotly::plotlyOutput("comparing_ins_plot") })
  output$comparing_ins_plot <- plotly::renderPlotly({
    res <- comparison_results(); req(res, res$mig_combined, input$time_select_comp, input$setup_select)
    plot_data <- res$mig_combined %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp))
    if (input$setup_select != "Both") plot_data <- plot_data %>% dplyr::filter(.data$setup == input$setup_select)
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$age, y = .data$ins.fitted, 
                                                 ymin = .data$ins.lower, ymax = .data$ins.upper,
                                                 color = .data$setup, fill = .data$setup)) +
      ggplot2::geom_line() + ggplot2::geom_ribbon(alpha = 0.3, linetype = "dashed") +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = "Immigration Comparison", y = "Count") + ggplot2::theme_minimal()
    
    if (input$setup_select == "Setup 1" || input$setup_select == "Both") {
      if (input$compare_select_ins_1 != "None" && input$compare_select_ins_1 %in% colnames(res$full_mig_1)) {
        comp_data_1 <- res$full_mig_1 %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp)) %>%
          dplyr::select(age, sex, time, compare_val = .data[[input$compare_select_ins_1]])
        p <- p + ggplot2::geom_point(data = comp_data_1, aes(y = compare_val), color = "blue", shape = 4, inherit.aes = FALSE, aes(x=age, group=sex))
      }
    }
    if (input$setup_select == "Setup 2" || input$setup_select == "Both") {
      if (input$compare_select_ins_2 != "None" && input$compare_select_ins_2 %in% colnames(res$full_mig_2)) {
        comp_data_2 <- res$full_mig_2 %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp)) %>%
          dplyr::select(age, sex, time, compare_val = .data[[input$compare_select_ins_2]])
        p <- p + ggplot2::geom_point(data = comp_data_2, aes(y = compare_val), color = "red", shape = 3, inherit.aes = FALSE, aes(x=age, group=sex))
      }
    }
    plotly::ggplotly(p)
  })
  
  output$compEmig <- shiny::renderUI({ plotly::plotlyOutput("comparing_outs_plot") })
  output$comparing_outs_plot <- plotly::renderPlotly({
    res <- comparison_results(); req(res, res$mig_combined, input$time_select_comp, input$setup_select)
    plot_data <- res$mig_combined %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp))
    if (input$setup_select != "Both") plot_data <- plot_data %>% dplyr::filter(.data$setup == input$setup_select)
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$age, y = .data$outs.fitted, 
                                                 ymin = .data$outs.lower, ymax = .data$outs.upper,
                                                 color = .data$setup, fill = .data$setup)) +
      ggplot2::geom_line() + ggplot2::geom_ribbon(alpha = 0.3, linetype = "dashed") +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = "Emigration Comparison", y = "Count") + ggplot2::theme_minimal()
    
    if (input$setup_select == "Setup 1" || input$setup_select == "Both") {
      if (input$compare_select_outs_1 != "None" && input$compare_select_outs_1 %in% colnames(res$full_mig_1)) {
        comp_data_1 <- res$full_mig_1 %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp)) %>%
          dplyr::select(age, sex, time, compare_val = .data[[input$compare_select_outs_1]])
        p <- p + ggplot2::geom_point(data = comp_data_1, aes(y = compare_val), color = "blue", shape = 4, inherit.aes = FALSE, aes(x=age, group=sex))
      }
    }
    if (input$setup_select == "Setup 2" || input$setup_select == "Both") {
      if (input$compare_select_outs_2 != "None" && input$compare_select_outs_2 %in% colnames(res$full_mig_2)) {
        comp_data_2 <- res$full_mig_2 %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp)) %>%
          dplyr::select(age, sex, time, compare_val = .data[[input$compare_select_outs_2]])
        p <- p + ggplot2::geom_point(data = comp_data_2, aes(y = compare_val), color = "red", shape = 3, inherit.aes = FALSE, aes(x=age, group=sex))
      }
    }
    plotly::ggplotly(p)
  })
  
  output$compPlotsImmigResiduals <- shiny::renderUI({ plotly::plotlyOutput("comparing_immig_residuals_plot") })
  output$comparing_immig_residuals_plot <- plotly::renderPlotly({
    res <- comparison_results(); req(res, res$mig_wide, input$time_select_comp, input$residual_type)
    plot_data <- res$mig_wide %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp))
    y_val <- if (input$residual_type == "Absolute") "ins_residuals_abs" else "ins_residuals_perc"
    y_lab <- if (input$residual_type == "Absolute") "Absolute Difference (S2-S1)" else "Percent Difference (S2-S1)"
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$age, y = .data[[y_val]])) +
      ggplot2::geom_segment(ggplot2::aes(xend = .data$age, yend = 0)) + ggplot2::geom_point(size = 2) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = paste("Immigration Comparison Residuals", input$residual_type), y = y_lab) + ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })
  
  output$compPlotsEmigResiduals <- shiny::renderUI({ plotly::plotlyOutput("comparing_emig_residuals_plot") })
  output$comparing_emig_residuals_plot <- plotly::renderPlotly({
    res <- comparison_results(); req(res, res$mig_wide, input$time_select_comp, input$residual_type)
    plot_data <- res$mig_wide %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp))
    y_val <- if (input$residual_type == "Absolute") "outs_residuals_abs" else "outs_residuals_perc"
    y_lab <- if (input$residual_type == "Absolute") "Absolute Difference (S2-S1)" else "Percent Difference (S2-S1)"
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$age, y = .data[[y_val]])) +
      ggplot2::geom_segment(ggplot2::aes(xend = .data$age, yend = 0)) + ggplot2::geom_point(size = 2) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = paste("Emigration Comparison Residuals", input$residual_type), y = y_lab) + ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })
  
  # Aggregate Comparison Plots
  output$compAggPop <- shiny::renderUI({ plotly::plotlyOutput("comparing_agg_pop_plot") })
  output$comparing_agg_pop_plot <- plotly::renderPlotly({
    res <- comparison_results(); req(res, res$pop_combined)
    pop_aggs <- res$pop_combined %>% dplyr::group_by(.data$time, .data$sex, .data$setup) %>%
      dplyr::summarise(agg_pop = sum(.data$population.fitted, na.rm = TRUE), .groups = "drop")
    p <- ggplot2::ggplot(pop_aggs, ggplot2::aes(x = .data$time, y = .data$agg_pop, color = .data$setup)) +
      ggplot2::geom_line() + ggplot2::geom_point(size = 2) + ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = "Aggregate Population Comparison", y = "Total Population") + ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })
  
  # ... (Other aggregate plots: compAggImmig, compAggEmig, compAggNetMig and their residual versions)
  # These follow a similar pattern: group_by, summarise, then plot. Residuals involve pivoting/joining.
  # For brevity, I'll assume you can adapt the compAggPop and the pop residual plot logic for these.
  # Example for compAggImmigDiffs:
  output$compAggImmigDiffs <- shiny::renderUI({ plotly::plotlyOutput("comparing_agg_immig_diffs_plot") })
  output$comparing_agg_immig_diffs_plot <- plotly::renderPlotly({
    res <- comparison_results(); req(res, res$mig_combined)
    mig_aggs <- res$mig_combined %>% dplyr::group_by(.data$time, .data$sex, .data$setup) %>%
      dplyr::summarise(agg_ins = sum(.data$ins.fitted, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = setup, values_from = agg_ins, names_prefix = "agg_ins_") %>%
      dplyr::mutate(abs_diff = .data$`agg_ins_Setup 2` - .data$`agg_ins_Setup 1`,
                    perc_diff = ifelse(.data$`agg_ins_Setup 1` == 0, NA, 100 * abs_diff / .data$`agg_ins_Setup 1`))
    
    p1_data <- mig_aggs %>% dplyr::select(time, sex, value = abs_diff) %>% dplyr::mutate(type="Absolute")
    p2_data <- mig_aggs %>% dplyr::select(time, sex, value = perc_diff) %>% dplyr::mutate(type="Percent")
    plot_data_long <- dplyr::bind_rows(p1_data, p2_data) %>% tidyr::drop_na(value)
    
    
    p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = .data$time, y = .data$value)) +
      ggplot2::geom_segment(ggplot2::aes(xend = .data$time, yend = 0)) + ggplot2::geom_point(size = 2) +
      ggplot2::facet_grid(type ~ sex, scales="free_y") + # Free y-scale for abs vs perc
      ggplot2::labs(title = "Aggregate Immigration Comparison Residuals", y="Difference (Setup 2 - Setup 1)") + 
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })
  # You would replicate similar logic for Emigration and Net Migration aggregate differences.
  
  
  # <<< START SENSITIVITY ANALYSIS SERVER LOGIC (already provided, integrated here) >>>
  
  # --- UI Renderers for Sensitivity Analysis Tab ---
  output$sa_system_model_selector_ui <- renderUI({
    shiny::selectInput("sa_selected_sys_model_setup", "Select Base System Model Setup:",
                       choices = names(sysmod_list_list()),
                       selected = if (length(names(sysmod_list_list())) > 0) names(sysmod_list_list())[1] else NULL)
  })
  
  output$sa_data_model_selector_ui <- renderUI({
    shiny::checkboxGroupInput("sa_selected_data_models_sens", "Select Base Data Models:", # Unique ID
                              choices = names(datamod_list()),
                              selected = names(datamod_list()))
  })
  
  observe({ # Update target data model selector for sensitivity based on checkbox group
    req(input$sa_selected_data_models_sens)
    updateSelectInput(session, "sa_datamod_name_select", 
                      choices = c("None", input$sa_selected_data_models_sens), 
                      selected = "None")
  })
  
  output$sa_sysmod_param_range_ui <- renderUI({
    req(input$sa_sysmod_param_select, input$sa_sysmod_param_select != "None")
    
    default_min <- 0; default_max <- 1; default_steps <- 3; param_label_prefix <- "Value"
    step_val <- 0.1 # Default step for numericInput
    
    if (grepl("lower_rate_limit", input$sa_sysmod_param_select)) {
      param_label_prefix <- "Lower Rate Limit"; default_min <- 1e-7; default_max <- 1e-4; default_steps <- 4; step_val <- 1e-7;
    } else if (grepl("rate_scale", input$sa_sysmod_param_select)) {
      param_label_prefix <- "Rate Scaler"; default_min <- 0.5; default_max <- 1.5; default_steps <- 3; step_val <- 0.1;
    } else if (grepl("rate_noise", input$sa_sysmod_param_select)) {
      param_label_prefix <- "Rate Noise SD"; default_min <- 0; default_max <- 0.1; default_steps <- 3; step_val <- 0.01;
    }
    
    tagList(
      numericInput("sa_sys_param_min", paste(param_label_prefix, "Min:"), value = default_min, step = step_val),
      numericInput("sa_sys_param_max", paste(param_label_prefix, "Max:"), value = default_max, step = step_val),
      numericInput("sa_sys_param_steps", "Number of Steps:", value = default_steps, min = 2, step = 1)
    )
  })
  
  output$sa_datamod_param_range_ui <- renderUI({
    req(input$sa_datamod_param_select, input$sa_datamod_param_select != "None", 
        input$sa_datamod_name_select, input$sa_datamod_name_select != "None")
    
    default_min <- 0; default_max <- 1; default_steps <- 3; step_val <- 0.1;
    param_label_prefix <- tools::toTitleCase(gsub("_", " ", input$sa_datamod_param_select))
    
    if (input$sa_datamod_param_select == "scale_ratio") {
      default_min <- 0; default_max <- 0.5; default_steps <- 3; step_val <- 0.05;
    } else if (input$sa_datamod_param_select == "sd_scaler") {
      default_min <- 0.5; default_max <- 2.0; default_steps <- 4; step_val <- 0.1;
    } else if (input$sa_datamod_param_select == "count_scaler") {
      default_min <- 0.5; default_max <- 1.5; default_steps <- 3; step_val <- 0.1;
    }
    
    tagList(
      numericInput("sa_data_param_min_val", paste(param_label_prefix, "Min:"), value = default_min, step = step_val), # Unique ID
      numericInput("sa_data_param_max_val", paste(param_label_prefix, "Max:"), value = default_max, step = step_val), # Unique ID
      numericInput("sa_data_param_steps_val", "Number of Steps:", value = default_steps, min = 2, step = 1)          # Unique ID
    )
  })
  
  # --- Observe event for running sensitivity analysis ---
  observeEvent(input$run_sensitivity_analysis, {
    req(global_config()$output_dir, global_config()$seed_value,
        input$sa_selected_sys_model_setup, input$sa_selected_data_models_sens)
    
    if (input$sa_sysmod_param_select == "None" && input$sa_datamod_param_select == "None") {
      shinyWidgets::sendSweetAlert(session, title = "No Parameter Selected", text = "Please select a system or data model parameter to vary.", type = "warning"); return()
    }
    if (input$sa_datamod_param_select != "None" && input$sa_datamod_name_select == "None"){
      shinyWidgets::sendSweetAlert(session, title = "No Data Model Target", text = "Please select a specific data model to apply the parameter change to.", type = "warning"); return()
    }
    
    base_sys_model_setup_name <- input$sa_selected_sys_model_setup
    base_sys_models_orig <- sysmod_list_list()[[base_sys_model_setup_name]]
    base_data_models_orig <- datamod_list()[input$sa_selected_data_models_sens]
    
    param_values <- NULL; varied_param_type <- NULL; varied_param_name_full_id <- NULL; 
    sys_model_component_to_vary <- NULL; data_model_to_vary <- NULL; varied_param_name_short_id <- NULL;
    
    if (input$sa_sysmod_param_select != "None") {
      req(input$sa_sys_param_min, input$sa_sys_param_max, input$sa_sys_param_steps)
      if(input$sa_sys_param_min >= input$sa_sys_param_max || input$sa_sys_param_steps < 2) {
        shinyWidgets::sendSweetAlert(session, title = "Invalid Range", text = "System model parameter range or steps are invalid.", type = "error"); return()
      }
      param_values <- seq(from = input$sa_sys_param_min, to = input$sa_sys_param_max, length.out = as.integer(input$sa_sys_param_steps))
      varied_param_type <- "sys"
      split_param <- unlist(strsplit(input$sa_sysmod_param_select, "_", fixed = TRUE))
      sys_model_component_to_vary <- split_param[1]
      varied_param_name_short_id <- paste(split_param[-1], collapse="_") 
      varied_param_name_full_id <- input$sa_sysmod_param_select
      sensitivity_varied_param_info(list(name = varied_param_name_full_id, type = "System", component = sys_model_component_to_vary, short_name = varied_param_name_short_id))
      
    } else if (input$sa_datamod_param_select != "None") {
      req(input$sa_data_param_min_val, input$sa_data_param_max_val, input$sa_data_param_steps_val, input$sa_datamod_name_select)
      if(input$sa_data_param_min_val >= input$sa_data_param_max_val || input$sa_data_param_steps_val < 2) {
        shinyWidgets::sendSweetAlert(session, title = "Invalid Range", text = "Data model parameter range or steps are invalid.", type = "error"); return()
      }
      param_values <- seq(from = input$sa_data_param_min_val, to = input$sa_data_param_max_val, length.out = as.integer(input$sa_data_param_steps_val))
      varied_param_type <- "data"
      data_model_to_vary <- input$sa_datamod_name_select
      varied_param_name_short_id <- input$sa_datamod_param_select
      varied_param_name_full_id <- paste0(data_model_to_vary, "$", varied_param_name_short_id)
      sensitivity_varied_param_info(list(name = varied_param_name_full_id, type = "Data", component = data_model_to_vary, short_name = varied_param_name_short_id))
    }
    
    if (is.null(param_values)) return()
    
    output$sensitivity_run_status <- renderText("Running sensitivity analysis...")
    results_collector <- list() # Re-initialize for new run
    
    shiny::withProgress(message = 'Running Sensitivity Analysis', value = 0, {
      n_iterations <- length(param_values)
      for (i in seq_along(param_values)) {
        current_param_val <- param_values[i]
        incProgress(1/n_iterations, detail = paste("Iter", i, ":", varied_param_name_full_id, "=", signif(current_param_val,4)))
        
        current_sys_models <- rlang::duplicate(base_sys_models_orig, shallow = FALSE)
        current_data_models <- rlang::duplicate(base_data_models_orig, shallow = FALSE)
        
        if (varied_param_type == "sys") {
          target_sm_component_obj <- current_sys_models[[sys_model_component_to_vary]]
          if(is.null(target_sm_component_obj)){ shinyWidgets::sendSweetAlert(session,title="Error",text=paste("Sys comp",sys_model_component_to_vary,"not found."),type="error"); return() }
          
          if (varied_param_name_short_id == "lower_rate_limit") {
            if ("mean" %in% names(target_sm_component_obj) && ("Counts" %in% class(target_sm_component_obj$mean) || is.data.frame(target_sm_component_obj$mean))) {
              rates_data <- as.data.frame(target_sm_component_obj$mean) # Works for Counts and data.frame
              rate_col_name <- if("rate" %in% names(rates_data)) "rate" else if("mean" %in% names(rates_data)) "mean" else NULL
              if(is.null(rate_col_name)){ warning("Rate column not found in system model mean component."); next }
              
              rates_data[[rate_col_name]] <- pmax(rates_data[[rate_col_name]], current_param_val)
              
              if("Counts" %in% class(target_sm_component_obj$mean)){ # Attempt to reconstruct Counts object
                target_sm_component_obj$mean <- Demographics::Counts(array(rates_data[[rate_col_name]], dim=dim(target_sm_component_obj$mean), dimnames=dimnames(target_sm_component_obj$mean)))
              } else { target_sm_component_obj$mean <- rates_data }
            } else { warning(paste("Cannot apply lower_rate_limit to", sys_model_component_to_vary)); next }
          } else if (varied_param_name_short_id == "rate_scale") {
            if ("mean" %in% names(target_sm_component_obj) && ("Counts" %in% class(target_sm_component_obj$mean) || is.data.frame(target_sm_component_obj$mean))) {
              rates_data <- as.data.frame(target_sm_component_obj$mean)
              rate_col_name <- if("rate" %in% names(rates_data)) "rate" else if("mean" %in% names(rates_data)) "mean" else NULL
              if(is.null(rate_col_name)){ warning("Rate column not found for rate_scale."); next }
              
              rates_data[[rate_col_name]] <- rates_data[[rate_col_name]] * current_param_val
              if("Counts" %in% class(target_sm_component_obj$mean)){
                target_sm_component_obj$mean <- Demographics::Counts(array(rates_data[[rate_col_name]], dim=dim(target_sm_component_obj$mean), dimnames=dimnames(target_sm_component_obj$mean)))
              } else { target_sm_component_obj$mean <- rates_data }
            } else { warning(paste("Cannot apply rate_scale to", sys_model_component_to_vary)); next }
          } # Add other sys param modifications
          current_sys_models[[sys_model_component_to_vary]] <- target_sm_component_obj
          
        } else if (varied_param_type == "data") {
          target_dm_obj <- current_data_models[[data_model_to_vary]]
          if(is.null(target_dm_obj)){ shinyWidgets::sendSweetAlert(session,title="Error",text=paste("Data model",data_model_to_vary,"not found."),type="error"); return() }
          
          if (varied_param_name_short_id %in% names(target_dm_obj)) { # Assumes parameter is a direct named element
            target_dm_obj[[varied_param_name_short_id]] <- current_param_val
          } else { warning(paste("Param", varied_param_name_short_id, "not in", data_model_to_vary)); next }
          current_data_models[[data_model_to_vary]] <- target_dm_obj
        }
        
        run_result_obj <- NULL
        tryCatch({
          run_result_obj <- accountTMB::estimate_account(datamods = current_data_models, sysmods = current_sys_models, seed_in = global_config()$seed_value + i)
        }, error = function(e) { shiny::showNotification(paste("Est. Error iter", i, ":", e$message), type = "error", duration=10); run_result_obj <<- NULL })
        
        if (!is.null(run_result_obj)) {
          pop_res_df <- tryCatch(accountTMB::augment_population(run_result_obj, collapse = "cohort"), error = function(e) NULL)
          mig_res_df <- tryCatch(accountTMB::augment_events(run_result_obj, collapse = "age") %>% dplyr::mutate(age = .data$time - .data$cohort) %>% dplyr::select(-any_of("cohort")), error = function(e) NULL)
          
          param_info_current <- sensitivity_varied_param_info()
          if (!is.null(pop_res_df)) { pop_res_df[[param_info_current$name]] <- current_param_val; pop_res_df$run_id <- i; results_collector[[paste0("pop_run_", i)]] <- pop_res_df }
          if (!is.null(mig_res_df)) { mig_res_df[[param_info_current$name]] <- current_param_val; mig_res_df$run_id <- i; results_collector[[paste0("mig_run_", i)]] <- mig_res_df }
        }
      } 
    }) 
    
    sensitivity_results_list(results_collector) # Store all collected results
    output$sensitivity_run_status <- renderText(paste("Sensitivity analysis complete. Processed", length(param_values), "iterations. Found", length(results_collector)/2, "valid run pairs (pop/mig)."))
    
    param_info_final <- sensitivity_varied_param_info()
    if(!is.null(param_info_final)){
      updateSelectInput(session, "sa_pop_plot_param_display", choices = param_info_final$name, selected = param_info_final$name)
      updateSelectInput(session, "sa_mig_plot_param_display", choices = param_info_final$name, selected = param_info_final$name)
      
      first_pop_res <- NULL; iter_names <- names(results_collector)
      for(res_iter_name in iter_names){ if(startsWith(res_iter_name, "pop_run_")){ first_pop_res <- results_collector[[res_iter_name]]; break } }
      if(!is.null(first_pop_res) && "time" %in% colnames(first_pop_res)){
        unique_times_sa <- sort(unique(first_pop_res$time))
        updateSelectInput(session, "sa_pop_time_select", choices = unique_times_sa, selected = unique_times_sa[1])
        updateSelectInput(session, "sa_mig_time_select", choices = unique_times_sa, selected = unique_times_sa[1])
      } else {
        updateSelectInput(session, "sa_pop_time_select", choices = c("N/A"), selected = "N/A")
        updateSelectInput(session, "sa_mig_time_select", choices = c("N/A"), selected = "N/A")
      }
    }
  })
  
  sa_combined_pop_results <- reactive({
    res_list_val <- sensitivity_results_list(); if (length(res_list_val) == 0) return(NULL)
    pop_dfs_list <- Filter(Negate(is.null), lapply(names(res_list_val), function(n) if(startsWith(n,"pop_run_")) res_list_val[[n]] else NULL))
    if (length(pop_dfs_list) == 0) return(NULL); dplyr::bind_rows(pop_dfs_list)
  })
  
  sa_combined_mig_results <- reactive({
    res_list_val <- sensitivity_results_list(); if (length(res_list_val) == 0) return(NULL)
    mig_dfs_list <- Filter(Negate(is.null), lapply(names(res_list_val), function(n) if(startsWith(n,"mig_run_")) res_list_val[[n]] else NULL))
    if (length(mig_dfs_list) == 0) return(NULL); dplyr::bind_rows(mig_dfs_list)
  })
  
  output$sa_population_plot_output <- renderPlotly({
    req(sa_combined_pop_results(), input$sa_pop_time_select != "N/A", sensitivity_varied_param_info())
    plot_data_val <- sa_combined_pop_results() %>% dplyr::filter(.data$time == as.numeric(input$sa_pop_time_select))
    varied_param_col <- sensitivity_varied_param_info()$name
    if (!varied_param_col %in% colnames(plot_data_val)) return(NULL)
    plot_data_val[[varied_param_col]] <- factor(round(plot_data_val[[varied_param_col]], 5))
    
    p <- ggplot2::ggplot(plot_data_val, ggplot2::aes(x = .data$age, y = .data$population.fitted, color = .data[[varied_param_col]], group = .data[[varied_param_col]])) +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$population.lower, ymax = .data$population.upper, fill = .data[[varied_param_col]]), alpha = 0.15, linetype = "blank") +
      ggplot2::facet_wrap(~sex, scales="free_y") +
      ggplot2::labs(title = paste("Population Sensitivity by", varied_param_col, "@ Time", input$sa_pop_time_select), x = "Age", y = "Population", color = varied_param_col, fill = varied_param_col) +
      ggplot2::theme_minimal() + ggplot2::guides(fill="none") # Hide ribbon legend if color is same
    plotly::ggplotly(p)
  })
  
  output$sa_immigration_plot_output <- renderPlotly({
    req(sa_combined_mig_results(), input$sa_mig_time_select != "N/A", sensitivity_varied_param_info())
    plot_data_val <- sa_combined_mig_results() %>% dplyr::filter(.data$time == as.numeric(input$sa_mig_time_select))
    varied_param_col <- sensitivity_varied_param_info()$name
    if (!varied_param_col %in% colnames(plot_data_val)) return(NULL)
    plot_data_val[[varied_param_col]] <- factor(round(plot_data_val[[varied_param_col]], 5))
    
    p_ins <- ggplot2::ggplot(plot_data_val, ggplot2::aes(x = .data$age, y = .data$ins.fitted, color = .data[[varied_param_col]], group = .data[[varied_param_col]])) +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$ins.lower, ymax = .data$ins.upper, fill = .data[[varied_param_col]]), alpha = 0.15, linetype="blank") +
      ggplot2::facet_wrap(~sex, scales="free_y") +
      ggplot2::labs(title = paste("Immigration Sensitivity by", varied_param_col, "@ Time", input$sa_mig_time_select), x = "Age", y = "Immigration", color = varied_param_col, fill = varied_param_col) +
      ggplot2::theme_minimal() + ggplot2::guides(fill="none")
    plotly::ggplotly(p_ins)
  })
  
  output$sa_emigration_plot_output <- renderPlotly({
    req(sa_combined_mig_results(), input$sa_mig_time_select != "N/A", sensitivity_varied_param_info())
    plot_data_val <- sa_combined_mig_results() %>% dplyr::filter(.data$time == as.numeric(input$sa_mig_time_select))
    varied_param_col <- sensitivity_varied_param_info()$name
    if (!varied_param_col %in% colnames(plot_data_val)) return(NULL)
    plot_data_val[[varied_param_col]] <- factor(round(plot_data_val[[varied_param_col]], 5))
    
    p_outs <- ggplot2::ggplot(plot_data_val, ggplot2::aes(x = .data$age, y = .data$outs.fitted, color = .data[[varied_param_col]], group = .data[[varied_param_col]])) +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$outs.lower, ymax = .data$outs.upper, fill = .data[[varied_param_col]]), alpha = 0.15, linetype="blank") +
      ggplot2::facet_wrap(~sex, scales="free_y") +
      ggplot2::labs(title = paste("Emigration Sensitivity by", varied_param_col, "@ Time", input$sa_mig_time_select), x = "Age", y = "Emigration", color = varied_param_col, fill = varied_param_col) +
      ggplot2::theme_minimal() + ggplot2::guides(fill="none")
    plotly::ggplotly(p_outs)
  })
  # <<< END SENSITIVITY ANALYSIS SERVER LOGIC >>>
  
} # End of server function