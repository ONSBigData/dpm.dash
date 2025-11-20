#' Server Function for DPM Dashboard Shiny Application
#'
#' This function defines the server-side logic for the DPM Dashboard Shiny application, 
#' managing reactive values and event observers to handle user inputs and dynamically update the UI components.
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
#' - Implements sensitivity analysis by systematically varying model parameters and evaluating outcomes.
#'
#' @return This function does not return a value; it modifies the state of the Shiny application through side effects.
#'
#' @import dpmaccount
#' @importFrom shiny shinyApp fluidRow column fileInput hr actionButton tabsetPanel
#' @importFrom shiny tabPanel h3 p tags uiOutput renderUI selectInput selectizeInput
#' @importFrom shiny numericInput checkboxInput textInput conditionalPanel verbatimTextOutput
#' @importFrom shiny renderPrint plotOutput renderPlot reactive reactiveVal req
#' @importFrom shiny showNotification observeEvent observe downloadButton
#' @importFrom shiny downloadHandler includeMarkdown helpText updateSelectizeInput
#' @importFrom shiny withProgress setProgress icon NS moduleServer
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom shinycssloaders withSpinner
#' @importFrom sortable bucket_list add_rank_list
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point geom_hline labs
#' @importFrom ggplot2 theme_minimal facet_wrap facet_grid vars theme element_text
#' @importFrom ggplot2 scale_color_manual scale_fill_manual
#' @importFrom dplyr filter select mutate summarise group_by all_of where
#' @importFrom magrittr %>%
#' @importFrom rlang expr sym new_formula call2 parse_expr env syms
#' @importFrom rvec draws_ci is_rvec
#' @importFrom bage mod_pois set_prior fit augment components replicate_data
#' @importFrom bage forecast report_sim
#' @importFrom utils read.csv write.csv head tail
#' @importFrom stats as.formula formula
#' @importFrom tools file_path_sans_ext
#' @importFrom purrr map_dfr
#'
#' @export
server_dash <- function(input, output, session) {
  # Define the global reactive values which store the state of the application.
  # These values will be updated as the user interacts with the UI and will trigger
  # re-rendering of relevant outputs.

  datamod_list <- reactiveVal(list()) # Stores a named list of all defined data models.
  global_config <- reactiveVal(list()) # Stores user-defined global configuration parameters (e.g., directories, seed).
  sysmod_list <- reactiveVal(list()) # Stores the components (births, deaths, ins, outs) of the *currently active* or *last created* system model setup.
  sysmod_list_list <- reactiveVal(list()) # Stores a named list of multiple system model setups. Each setup is a list of 4 system model components.
  selected_data_models <- reactiveVal(list()) # This reactive value was noted as potentially redundant; its usage should be verified. If unused, it can be removed.
  sensitivity_results_list <- reactiveVal(list()) # Stores a list of dataframes, each containing results from a single run within a sensitivity analysis.
  sensitivity_varied_param_info <- reactiveVal(NULL) # Stores information about the parameter being varied in the sensitivity analysis (name, type, component, short name).

  ##############################################################################
  ############################## globalConfig Tab ##############################
  ##############################################################################
  # This section handles logic related to the 'Global Configuration' tab.

  # Define default values for global configuration parameters.
  default_output_dir <- here::here("output/") # Default directory for saving outputs, constructed relative to the project root.
  default_data_dir <- here::here("data/") # Default directory for input data, constructed relative to the project root.
  default_seed_value <- numbers::nextPrime(as.integer(Sys.time())) # Generates a pseudo-random prime number based on the current time as the default seed for reproducibility.

  # Observer for the 'Save Global Configuration' button.
  # This event triggers when the user clicks the button to save their global settings.
  shiny::observeEvent(input$save_global_config, {
    # Update the 'global_config' reactiveVal with the values from the UI inputs.
    # If an input field is empty, the corresponding default value is used.
    global_config(list(
      data_dir = if (nzchar(input$global_data_dir)) input$global_data_dir else default_data_dir, # Use user input or default for data directory.
      output_dir = if (nzchar(input$global_output_dir)) input$global_output_dir else default_output_dir, # Use user input or default for output directory.
      time_selection = if (nzchar(input$global_time_selection)) parse_values(input$global_time_selection) else NULL, # Parse comma-separated time selection string to integer vector.
      seed_value = if (nzchar(input$global_seed_value)) as.integer(input$global_seed_value) else default_seed_value # Use user input or default for seed value.
    ))
    # Display a modal dialog to confirm that the global configuration has been saved.
    shiny::showModal(modalDialog(
      title = "Global Configuration Saved",
      "Global configuration parameters have been saved successfully.",
      easyClose = TRUE, # Allows closing the modal by clicking outside or pressing Esc.
      footer = NULL # No footer buttons in the modal.
    ))
  })
  ##############################################################################

  ##############################################################################
  ############################## systemModels Tab ##############################
  ##############################################################################
  # This section handles logic related to the 'System Models' tab, including creation,
  # display, import, export, and deletion of system model setups.

  # Observer for the 'Create System Models' button.
  # This event creates a complete set of four system models (births, deaths, ins, outs)
  # based on user-provided CSV files and parameters.
  shiny::observeEvent(input$create_system_models, {
    file_error <- FALSE # Initialize a flag to track if any file-related errors occur.
    req(global_config()$data_dir) # Ensure that the global data directory is set before proceeding.
    models <- c("births", "deaths", "ins", "outs") # Define the four required system model components.

    # Temporarily store the newly created system model components for this action.
    # These will be used for immediate display and then added to the persistent `sysmod_list_list`.
    new_sysmods_temp <- lapply(models, function(model) {
      # Construct the full file path for the rates CSV file.
      rates_file_path <- file.path(global_config()$data_dir, input[[paste0(model, "_rates_file")]])

      # Check if the rates file exists. If not, set error flag and show an alert.
      if (!file.exists(rates_file_path)) {
        file_error <<- TRUE # Set the outer scope file_error flag.
        shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("Rates file not found:", rates_file_path), type = "error")
        return(NULL) # Return NULL for this model component if file is not found.
      }
      rates_df <- utils::read.csv(rates_file_path) # Read the rates data from the CSV file.

      # Load or set the dispersion for the rates. This can be a single numeric value or a CSV file.
      disp_val_or_df <- NULL
      if (input[[paste0(model, "_disp_type")]] == "Single Value") {
        disp_val_or_df <- input[[paste0(model, "_disp_value")]] # Use the single numeric dispersion value.
      } else {
        # Construct the full file path for the dispersion CSV file.
        disp_file_path <- file.path(global_config()$data_dir, input[[paste0(model, "_disp_file")]])
        # Check if the dispersion file exists.
        if (!file.exists(disp_file_path)) {
          file_error <<- TRUE
          shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("Dispersion file not found:", disp_file_path), type = "error")
          return(NULL)
        }
        disp_val_or_df <- utils::read.csv(disp_file_path) # Read the dispersion data from the CSV file.
      }

      # Retrieve optional system model parameters from UI inputs.
      lower_rates_limit_val <- input[[paste0(model, "_lower_rate_limit")]]
      rate_scaler_val <- input[[paste0(model, "_rate_scale")]]
      rate_overide_val <- input[[paste0(model, "_rate_overide")]]
      rate_noise_val <- input[[paste0(model, "_rate_noise")]]
      time_selection_val <- global_config()$time_selection # Use global time selection if specified.
      age_target <- input[[paste0(model, "_age_target")]]
      sex_target <- input[[paste0(model, "_sex_target")]]
      time_target <- input[[paste0(model, "_time_target")]]

      # Check if the 'create_system_model' function (assumed to be from the 'dpmaccount' package or defined elsewhere) exists.
      if (!exists("create_system_model") || !is.function(create_system_model)) {
        shinyWidgets::sendSweetAlert(session = session, title = "Configuration Error", text = "Function 'create_system_model' is not defined. This function is required to create system models.", type = "error")
        return(NULL)
      }
      # Call the function to create the individual system model component.
      # This function is expected to handle the provided rates, dispersion, and optional parameters.
      create_system_model(
        model, rates_df, disp_val_or_df, time_selection_val,
        lower_rates_limit_val, rate_scaler_val, rate_overide_val, rate_noise_val,
        age_target, sex_target, time_target
      )
    })

    # If a file error occurred or any model component failed to create, show an alert and stop.
    if (file_error || any(sapply(new_sysmods_temp, is.null))) {
      if (!file_error) { # If not a file error, implies a null return from create_system_model.
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "System Model Creation Error",
          text = "One or more system model components could not be created. Please check inputs and console for details from 'create_system_model'.",
          type = "error"
        )
      }
      return() # Stop further processing for this event.
    }

    names(new_sysmods_temp) <- models # Assign names (births, deaths, ins, outs) to the created model components.
    sysmod_list(new_sysmods_temp) # Update 'sysmod_list' to hold this newly created set, making it the "current" one.

    # Add this new system model setup to the list of all setups ('sysmod_list_list').
    current_list_of_setups <- sysmod_list_list()
    new_setup_entry <- list()
    new_setup_entry[[input$sysmods_name]] <- new_sysmods_temp # Store the named list of 4 models as one setup, using the user-provided setup name.
    sysmod_list_list(c(current_list_of_setups, new_setup_entry)) # Append the new setup to the existing list.

    # Dynamically render UI to display the names of all loaded system model setups.
    output$loadedSystemModels <- renderUI({
      sys_setups <- sysmod_list_list()
      if (length(sys_setups) > 0) {
        model_names <- paste(names(sys_setups), collapse = "<br>") # Create an HTML string of setup names.
        HTML(paste("Loaded System Model Setups: <br>", model_names))
      } else {
        HTML("No System Model Setups loaded")
      }
    })

    # Dynamically render UI placeholders for summaries of the *last created* system model set.
    output$modelSummaries <- shiny::renderUI({
      current_sysmods_to_display <- sysmod_list() # Get the currently active system model set.
      req(current_sysmods_to_display, length(current_sysmods_to_display) == 4) # Ensure it's valid.

      # Create verbatimTextOutput placeholders for each model component's summary.
      lapply(models, function(model_name) {
        shiny::verbatimTextOutput(outputId = paste0(model_name, "_summary_display"))
      })
    })

    # Dynamically render UI placeholders for plots of the *last created* system model set.
    output$modelPlots <- shiny::renderUI({
      current_sysmods_to_display <- sysmod_list() # Get the currently active system model set.
      req(current_sysmods_to_display, length(current_sysmods_to_display) == 4) # Ensure it's valid.

      # Create plotlyOutput placeholders for each model component's plot.
      lapply(models, function(model_name) {
        sysmod_component <- current_sysmods_to_display[[model_name]]
        # Only create a plot output if the component and its mean data are valid.
        if (!is.null(sysmod_component) && !is.null(sysmod_component$mean)) {
          plotly::plotlyOutput(outputId = paste0(model_name, "_plot_display"))
        } else {
          NULL # Or render a placeholder indicating no data/plot.
        }
      })
    })

    # Generate and render summaries and plots for each component of the *last created* system model set.
    lapply(models, function(model_name) {
      # Render the summary for the model component.
      output[[paste0(model_name, "_summary_display")]] <- renderPrint({
        current_sysmods_to_display <- sysmod_list() # Get the current set.
        req(current_sysmods_to_display, length(current_sysmods_to_display) == 4)
        sysmod_component <- current_sysmods_to_display[[model_name]]

        if (!is.null(sysmod_component) && !is.null(sysmod_component$mean)) {
          cat("Summary of rates data for", model_name, "(from last created/loaded setup: ", isolate(input$sysmods_name), ")\n")
          # 'generate_summary' is assumed to be a helper function defined elsewhere for summarizing rate data.
          if (!exists("generate_summary") || !is.function(generate_summary)) {
            return("Error: generate_summary() function not found.")
          }
          # The mean component is expected to be a data frame or coercible to one.
          generate_summary(as.data.frame(sysmod_component$mean) %>% dplyr::rename(rate = .data$mean))
        } else {
          cat("No data to summarize for", model_name, "\n")
        }
      })

      # Render the plot for the model component.
      output[[paste0(model_name, "_plot_display")]] <- plotly::renderPlotly({
        current_sysmods_to_display <- sysmod_list() # Get the current set.
        req(current_sysmods_to_display, length(current_sysmods_to_display) == 4)
        sysmod_component <- current_sysmods_to_display[[model_name]]

        if (!is.null(sysmod_component) && !is.null(sysmod_component$mean)) {
          # 'generate_plots' is assumed to be a helper function defined elsewhere for plotting rate data.
          if (!exists("generate_plots") || !is.function(generate_plots)) {
            return(NULL) # Or a placeholder plotly object indicating an error.
          }
          generate_plots(as.data.frame(sysmod_component$mean) %>% dplyr::rename(rate = .data$mean), model_name)
        }
      })
    })
  }) # End observeEvent input$create_system_models

  # Observer for the 'Delete Selected System Models' button.
  shiny::observeEvent(input$delete_button_sysmod, {
    req(input$delete_sysmod_list) # Requires a system model setup name to be selected from the dropdown.
    current_list_of_setups <- sysmod_list_list()
    current_list_of_setups[[input$delete_sysmod_list]] <- NULL # Remove the selected setup from the list.
    sysmod_list_list(current_list_of_setups) # Update the reactiveVal.
    # Note: 'sysmod_list' (the "current" setup) is not automatically cleared or updated here.
    # It will retain its state until a new setup is explicitly created or loaded.
  })

  # Observer to update the choices in the 'delete_sysmod_list' selectInput.
  # This ensures the dropdown always reflects the currently available system model setups.
  observe({
    updateSelectInput(session, "delete_sysmod_list", choices = names(sysmod_list_list()))
  })

  # Observer for the 'Import Selected System Models' button.
  shiny::observeEvent(input$import_button_sysmod, {
    req(global_config()$data_dir, input$sysmod_list_file) # Require data directory and filename for the .RDS file.
    sysmods_file_path <- file.path(global_config()$data_dir, input$sysmod_list_file) # Construct full path.

    # Check if the specified .RDS file exists.
    if (!file.exists(sysmods_file_path)) {
      shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("System model file not found:", sysmods_file_path), type = "error")
      return()
    }

    tryCatch(
      {
        sysmod_list_imported_setup <- readRDS(sysmods_file_path) # Read the .RDS file.

        # Basic validation of the imported object structure.
        # It should be a list of 4 components, and each component should also be a list.
        if (!is.list(sysmod_list_imported_setup) || length(sysmod_list_imported_setup) != 4 || !all(sapply(sysmod_list_imported_setup, is.list))) {
          shinyWidgets::sendSweetAlert(session = session, title = "Import Error", text = "Imported RDS file does not seem to be a valid system model setup (expected a named list of 4 model components).", type = "error")
          return()
        }

        imported_setup_name <- tools::file_path_sans_ext(input$sysmod_list_file) # Use filename (without extension) as setup name.

        current_setups <- sysmod_list_list()
        current_setups[[imported_setup_name]] <- sysmod_list_imported_setup # Add the imported setup to the list of setups.
        sysmod_list_list(current_setups) # Update the reactiveVal for all setups.
        sysmod_list(sysmod_list_imported_setup) # Set the imported setup as the "current" one for immediate display.

        shiny::showModal(modalDialog(title = "Import Successful", paste("System model setup '", imported_setup_name, "' imported."), easyClose = TRUE))
      },
      error = function(e) { # Handle errors during file reading or processing.
        shinyWidgets::sendSweetAlert(session = session, title = "Import Error", text = paste("Could not read or process RDS file:", e$message), type = "error")
      }
    )
  })

  # Observer for the 'Export System Models' button.
  # This exports the *currently active* system model setup (from `sysmod_list()`) to an .RDS file.
  shiny::observeEvent(input$export_system_models, {
    # Require output directory, a current system model set, and a name for the setup.
    req(global_config()$output_dir, sysmod_list(), input$sysmods_name)

    current_sysmods_to_export <- sysmod_list() # Get the system model set currently in `sysmod_list`.
    # Validate that it's a complete set of 4 models.
    if (length(current_sysmods_to_export) == 0 || !all(c("births", "deaths", "ins", "outs") %in% names(current_sysmods_to_export))) {
      shinyWidgets::sendSweetAlert(session = session, title = "Export Error", text = "No valid system model set currently loaded or created to export. Please create or import a set first.", type = "warning")
      return()
    }

    # Use the name from `input$sysmods_name` (typically the name used when creating or last input) for the exported file.
    export_file_name <- paste0(input$sysmods_name, "_sysmods.RDS")
    export_file_path <- file.path(global_config()$output_dir, export_file_name) # Save to the global output directory.

    tryCatch(
      {
        saveRDS(current_sysmods_to_export, export_file_path) # Save the system model list to an RDS file.
        shiny::showModal(modalDialog(title = "Export Successful", paste("System models saved to:", export_file_path), easyClose = TRUE))
      },
      error = function(e) { # Handle errors during saving.
        shinyWidgets::sendSweetAlert(session = session, title = "Export Error", text = paste("Failed to save system models:", e$message), type = "error")
      }
    )
  })

  # Navigation button observers: Switch to the specified tab when clicked.
  shiny::observeEvent(input$goGC, { # "Back" to Global Configuration
    shiny::updateTabsetPanel(session, "tabs", selected = "globalConfig")
  })
  shiny::observeEvent(input$goSM, { # "Continue" to System Models or "Back" to System Models
    shiny::updateTabsetPanel(session, "tabs", selected = "systemModels")
  })
  ##############################################################################


  ##############################################################################
  ############################### dataModels Tab ###############################
  ##############################################################################
  # This section handles logic related to the 'Data Models' tab, including creation,
  # display, plotting, inspection, import, and export of data models.

  # Reactive expression to read the main counts data for a new data model.
  mainData <- shiny::reactive({
    req(global_config()$data_dir, input$counts_file) # Require data directory and counts filename.
    counts_file_path <- file.path(global_config()$data_dir, input$counts_file) # Construct full path.

    # Check if the counts file exists.
    if (!file.exists(counts_file_path)) {
      shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("Counts file not found:", counts_file_path), type = "error")
      return(NULL)
    }
    # Try to read the CSV file, handling potential errors.
    tryCatch(utils::read.csv(counts_file_path), error = function(e) {
      shinyWidgets::sendSweetAlert(session = session, title = "File Read Error", text = paste("Could not read counts file:", e$message), type = "error")
      return(NULL)
    })
  })

  # Reactive expression to process auxiliary data inputs based on the selected data model type.
  auxData <- shiny::reactive({
    req(input$data_model, global_config()$data_dir) # Require selected data model type and data directory.
    data_dir_path <- global_config()$data_dir
    extraData <- list() # Initialize a list to store auxiliary data.

    # Helper function to get ratio input (either from a CSV file or a single numeric value).
    get_ratio_input <- function(ratio_file_input_id, ratio_value_input_id) {
      # The actual input values are accessed via input[[id]]
      ratio_file_name <- input[[ratio_file_input_id]]
      ratio_numeric_value <- input[[ratio_value_input_id]]

      if (nzchar(ratio_file_name)) { # If a ratio file name is provided.
        full_path <- file.path(data_dir_path, ratio_file_name)
        if (file.exists(full_path)) {
          return(tryCatch(utils::read.csv(full_path), error = function(e) { # Try to read the CSV.
            warning(paste("Ratio file read error:", e$message, "- using single value instead."))
            ratio_numeric_value # Fallback to single value on error.
          }))
        } else {
          warning(paste("Ratio file not found:", full_path, "- Using single value instead."))
          ratio_numeric_value # Fallback to single value if file not found.
        }
      } else {
        ratio_numeric_value # Use single numeric value if no file name provided.
      }
    }

    # Based on the selected data_model type, retrieve specific inputs.
    if (input$data_model == "Normal Data Model" || input$data_model == "Log-Normal Data Model") {
      req(input$counts_uncertainty_file) # Requires uncertainty file for Normal and Log-Normal models.
      uncertainty_path <- file.path(data_dir_path, input$counts_uncertainty_file)
      if (!file.exists(uncertainty_path)) {
        shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("Counts uncertainty file not found:", uncertainty_path), type = "error")
        return(NULL)
      }
      extraData$uncertainty <- tryCatch(utils::read.csv(uncertainty_path), error = function(e) {
        shinyWidgets::sendSweetAlert(session = session, title = "File Read Error", text = paste("Could not read uncertainty file:", e$message), type = "error")
        return(NULL)
      })
      if (is.null(extraData$uncertainty)) {
        return(NULL)
      } # Stop if uncertainty reading failed.

      # Get common optional parameters for Normal/Log-Normal.
      # Note: The UI for ratio selection is input$ratio_type, input$ratio_file, input$ratio_value.
      # We need to pass the correct input IDs to get_ratio_input.
      extraData$ratio <- get_ratio_input("ratio_file", "ratio_value") # Corrected based on UI structure.
      extraData$scale_ratio <- input$scale_ratio
      extraData$sd_scaler <- input$sd_scaler
      extraData$sd_overide <- input$sd_overide
      extraData$min_sd <- input$min_sd
      extraData$count_scaler <- input$count_scaler
    } else if (input$data_model == "T-Dist Data Model") {
      # For T-Distribution, get scale parameter (single value or CSV).
      if (input$scale_type == "Single Value") { # scale_type is from additionalInputs UI
        extraData$scale_df <- input$scale_value # scale_value is from additionalInputs UI
      } else {
        req(input$scale_file) # scale_file is from additionalInputs UI
        scale_path <- file.path(data_dir_path, input$scale_file)
        if (!file.exists(scale_path)) {
          shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("Scale file for T-Dist not found:", scale_path), type = "error")
          return(NULL)
        }
        extraData$scale_df <- tryCatch(utils::read.csv(scale_path), error = function(e) {
          shinyWidgets::sendSweetAlert(session = session, title = "File Read Error", text = paste("Could not read scale file:", e$message), type = "error")
          return(NULL)
        })
        if (is.null(extraData$scale_df)) {
          return(NULL)
        }
      }
      extraData$ratio <- get_ratio_input("ratio_file", "ratio_value")
      extraData$scale_ratio <- input$scale_ratio
      extraData$count_scaler <- input$count_scaler
    } else if (input$data_model == "Negative Binomial Data Model") {
      # For Negative Binomial, get dispersion parameter.
      if (input$disp_type == "Single Value") { # disp_type is from additionalInputs UI
        extraData$disp <- input$disp_value # disp_value is from additionalInputs UI
      } else {
        req(input$disp_file) # disp_file is from additionalInputs UI
        disp_path <- file.path(data_dir_path, input$disp_file)
        if (!file.exists(disp_path)) {
          shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("Dispersion file for N-Binom not found:", disp_path), type = "error")
          return(NULL)
        }
        extraData$disp <- tryCatch(utils::read.csv(disp_path), error = function(e) {
          shinyWidgets::sendSweetAlert(session = session, title = "File Read Error", text = paste("Could not read dispersion file:", e$message), type = "error")
          return(NULL)
        })
        if (is.null(extraData$disp)) {
          return(NULL)
        }
      }
      extraData$ratio <- get_ratio_input("ratio_file", "ratio_value")
      extraData$scale_ratio <- input$scale_ratio
      extraData$count_scaler <- input$count_scaler
    } else if (input$data_model == "Poisson Data Model") {
      # Poisson model has fewer specific auxiliary inputs from this section in the original UI.
      extraData$ratio <- get_ratio_input("ratio_file", "ratio_value")
      extraData$scale_ratio <- input$scale_ratio
      extraData$count_scaler <- input$count_scaler
    }
    extraData # Return the collected auxiliary data.
  })

  # Dynamically render UI for additional inputs based on the selected data model type.
  # These are typically "required" conditional inputs.
  output$additionalInputs <- shiny::renderUI({
    switch(input$data_model,
      "Normal Data Model" = tagList(
        shiny::textInput("counts_uncertainty_file", "Counts Uncertainty CSV Filename (Required)") # Labelled as required.
      ),
      "T-Dist Data Model" =
        tagList(
          radioButtons("scale_type", "Scale Input Type (for T-Dist)", choices = c("Single Value", "CSV File")),
          conditionalPanel( # Show if "Single Value" is chosen for scale.
            condition = "input.scale_type == 'Single Value'",
            numericInput("scale_value", "Scale Value (T-Dist)", value = 0.1)
          ),
          conditionalPanel( # Show if "CSV File" is chosen for scale.
            condition = "input.scale_type == 'CSV File'",
            textInput("scale_file", "Scale CSV Filename (T-Dist)")
          )
        ),
      "Negative Binomial Data Model" =
        tagList(
          radioButtons("disp_type", "Dispersion Input Type (for N-Binom)", choices = c("Single Value", "CSV File")),
          conditionalPanel( # Show if "Single Value" is chosen for dispersion.
            condition = "input.disp_type == 'Single Value'",
            numericInput("disp_value", "Dispersion Value (N-Binom)", value = 0.1)
          ),
          conditionalPanel( # Show if "CSV File" is chosen for dispersion.
            condition = "input.disp_type == 'CSV File'",
            textInput("disp_file", "Dispersion CSV Filename (N-Binom)")
          )
        ),
      "Poisson Data Model" =
        tagList(), # Poisson typically doesn't have extra required inputs here beyond counts.
      "Log-Normal Data Model" = tagList(
        shiny::textInput("counts_uncertainty_file", "Counts Uncertainty CSV Filename (Required)") # Labelled as required.
      ),
      NULL # Default case, renders nothing.
    )
  })

  # Dynamically render UI for optional inputs based on the selected data model type.
  output$optionalInputs <- shiny::renderUI({
    # Common optional inputs related to coverage ratio, scaling, age/time selection.
    common_optional_inputs <- tagList(
      shiny::radioButtons("ratio_type", "Coverage Ratio Type", choices = c("Single Value", "CSV File")),
      shiny::conditionalPanel(
        condition = "input.ratio_type == 'Single Value'",
        numericInput("ratio_value", "Coverage Ratio Value", value = 1) # Default ratio is 1.
      ),
      shiny::conditionalPanel(
        condition = "input.ratio_type == 'CSV File'",
        textInput("ratio_file", "Coverage Ratio CSV Filename", value = "") # Optional CSV for ratio.
      ),
      shiny::numericInput("scale_ratio", "Optional: Scale Ratio (adjusts coverage)", value = 0),
      shiny::sliderInput("count_scaler", "Optional: Count Scaler (multiplies counts)", value = 1, min = 0, max = 3, step = 0.1),
      shiny::textInput("age_selection", "Optional: Age Selection (e.g., 0:10, 15, 20:30 or blank for all)", value = ""),
      shiny::textInput("time_selection", "Optional: Time Selection (e.g., 2000, 2005:2010 or blank for all)", value = "")
    )

    # Specific optional inputs for certain models (e.g., SD related params for Normal/Log-Normal).
    model_specific_optional_inputs <- switch(input$data_model,
      "Normal Data Model" = tagList(
        shiny::numericInput("min_sd", "Optional: Minimum SD", value = 0),
        shiny::sliderInput("sd_scaler", "Optional: SD Scaler (multiplies SD)", value = 1, min = 0, max = 3, step = 0.1),
        shiny::numericInput("sd_overide", "Optional: SD Set (constant SD, -1 to disable)", value = -1)
      ),
      "Log-Normal Data Model" = tagList( # Same SD options as Normal model.
        shiny::numericInput("min_sd", "Optional: Minimum SD", value = 0),
        shiny::sliderInput("sd_scaler", "Optional: SD Scaler (multiplies SD)", value = 1, min = 0, max = 3, step = 0.1),
        shiny::numericInput("sd_overide", "Optional: SD Set (constant SD, -1 to disable)", value = -1)
      ),
      NULL
    )
    tagList(common_optional_inputs, model_specific_optional_inputs) # Combine common and model-specific optional inputs.
  })

  # Helper function to parse age or time selection strings (e.g., "0:5,10,12:15") into an integer vector.
  parse_selection_string <- function(sel_str) {
    if (!nzchar(sel_str)) { # If the string is empty, return NULL (no selection).
      return(NULL)
    }
    sel_str <- gsub("\\s+", "", sel_str) # Remove all whitespace from the string.
    parts <- strsplit(sel_str, ",")[[1]] # Split the string by commas into parts.
    values <- integer(0) # Initialize an empty integer vector to store parsed values.
    for (part in parts) {
      if (grepl(":", part)) { # If the part contains a colon, it's a range.
        range_ends <- as.integer(strsplit(part, ":")[[1]])
        # Validate the range.
        if (length(range_ends) == 2 && !any(is.na(range_ends)) && range_ends[1] <= range_ends[2]) {
          values <- c(values, seq(range_ends[1], range_ends[2])) # Add sequence to values.
        } else {
          warning(paste("Invalid range in selection string:", part))
          return(NA) # Indicate an error in parsing.
        }
      } else { # Otherwise, it's a single number.
        val <- as.integer(part)
        if (!is.na(val)) {
          values <- c(values, val) # Add the number to values.
        } else {
          warning(paste("Invalid number in selection string:", part))
          return(NA) # Indicate an error in parsing.
        }
      }
    }
    return(unique(sort(values))) # Return unique, sorted values.
  }

  # Observer for creating the 'births' data model (typically an "Exact Data Model").
  shiny::observeEvent(input$create_births_data_model, {
    req(global_config()$data_dir, input$births_counts_file) # Requires data directory and births counts filename.
    counts_file_path <- file.path(global_config()$data_dir, input$births_counts_file)
    if (!file.exists(counts_file_path)) {
      shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("Births counts file not found:", counts_file_path), type = "error")
      return()
    }
    input_counts <- tryCatch(utils::read.csv(counts_file_path), error = function(e) { # Read counts.
      shinyWidgets::sendSweetAlert(session = session, title = "File Read Error", text = paste("Could not read births counts file:", e$message), type = "error")
      NULL
    })
    if (is.null(input_counts)) {
      return()
    } # Stop if reading failed.

    # 'create_data_model' is assumed to be an external function (e.g., from dpmaccount).
    if (!exists("create_data_model") || !is.function(create_data_model)) {
      shinyWidgets::sendSweetAlert(session = session, title = "Configuration Error", text = "Function 'create_data_model' is not defined.", type = "error")
      return()
    }
    # Create the births data model, explicitly setting series and type.
    newObject <- create_data_model(dm_name = "births", series_name = "births", dm_type = "Exact Data Model", counts_df = input_counts)
    currentList <- datamod_list()
    newObjectList <- list()
    newObjectList[[newObject$nm_data]] <- newObject # Use the name from the created object (e.g., newObject$nm_data).
    datamod_list(c(currentList, newObjectList)) # Add to the list of data models.
    shiny::showModal(modalDialog(title = "Data Model Created", "Births data model (Exact) created.", easyClose = TRUE))
  })

  # Observer for creating the 'deaths' data model (similarly, "Exact Data Model").
  shiny::observeEvent(input$create_deaths_data_model, {
    req(global_config()$data_dir, input$deaths_counts_file) # Requires data directory and deaths counts filename.
    counts_file_path <- file.path(global_config()$data_dir, input$deaths_counts_file)
    if (!file.exists(counts_file_path)) {
      shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("Deaths counts file not found:", counts_file_path), type = "error")
      return()
    }
    input_counts <- tryCatch(utils::read.csv(counts_file_path), error = function(e) { # Read counts.
      shinyWidgets::sendSweetAlert(session = session, title = "File Read Error", text = paste("Could not read deaths counts file:", e$message), type = "error")
      NULL
    })
    if (is.null(input_counts)) {
      return()
    }

    if (!exists("create_data_model") || !is.function(create_data_model)) {
      shinyWidgets::sendSweetAlert(session = session, title = "Configuration Error", text = "Function 'create_data_model' is not defined.", type = "error")
      return()
    }
    # Create the deaths data model.
    newObject <- create_data_model(dm_name = "deaths", series_name = "deaths", dm_type = "Exact Data Model", counts_df = input_counts)
    currentList <- datamod_list()
    newObjectList <- list()
    newObjectList[[newObject$nm_data]] <- newObject
    datamod_list(c(currentList, newObjectList))
    shiny::showModal(modalDialog(title = "Data Model Created", "Deaths data model (Exact) created.", easyClose = TRUE))
  })

  # Observer for creating other types of data models (Normal, T-Dist, N-Binom, Poisson, Log-Normal).
  shiny::observeEvent(input$create_data_model, {
    # Requires main counts data, auxiliary data, model name, series name, and model type.
    req(mainData(), auxData(), input$dm_name, input$series_name, input$data_model)

    # Validate that Data Model Name is not empty.
    if (!nzchar(trimws(input$dm_name))) {
      shinyWidgets::sendSweetAlert(session = session, title = "Input Error", text = "Data Model Name cannot be empty.", type = "error")
      return()
    }

    # Parse age and time selection strings.
    age_subset_vals <- parse_selection_string(input$age_selection)
    if (length(age_subset_vals) == 1 && is.na(age_subset_vals[1])) { # Check for parsing error.
      shinyWidgets::sendSweetAlert(session = session, title = "Input Error", text = "Invalid Age Selection string. Please check format (e.g., 0:10, 15, 20:30).", type = "error")
      return()
    }
    time_subset_vals <- parse_selection_string(input$time_selection)
    if (length(time_subset_vals) == 1 && is.na(time_subset_vals[1])) { # Check for parsing error.
      shinyWidgets::sendSweetAlert(session = session, title = "Input Error", text = "Invalid Time Selection string. Please check format (e.g., 2000, 2005:2010).", type = "error")
      return()
    }

    if (!exists("create_data_model") || !is.function(create_data_model)) {
      shinyWidgets::sendSweetAlert(session = session, title = "Configuration Error", text = "Function 'create_data_model' is not defined.", type = "error")
      return()
    }
    # Call the external 'create_data_model' function with all relevant parameters.
    # The auxData() reactive provides a list of relevant parameters like uncertainty_df, scale_df, etc.
    newObject <- create_data_model(
      dm_name = input$dm_name, series_name = input$series_name, dm_type = input$data_model,
      counts_df = mainData(), time_select = time_subset_vals, age_select = age_subset_vals,
      # Pass auxiliary data components. These might be NULL if not applicable for the model type.
      uncertainty_df = auxData()$uncertainty, scale_df = auxData()$scale_df, disp = auxData()$disp,
      scale_ratio = auxData()$scale_ratio, ratio = auxData()$ratio, sd_scaler = auxData()$sd_scaler,
      sd_overide = auxData()$sd_overide, min_sd = auxData()$min_sd, count_scaler = auxData()$count_scaler
    )

    # Check if the data model object was created successfully.
    if (is.null(newObject) || !is.list(newObject) || is.null(newObject$nm_data)) {
      shinyWidgets::sendSweetAlert(session = session, title = "Creation Failed", text = "Data model creation failed. The 'create_data_model' function may have returned NULL. Check console for errors from that function.", type = "error")
      return()
    }

    currentList <- datamod_list()
    newObjectList <- list()
    newObjectList[[newObject$nm_data]] <- newObject # Name the entry in the list by the model's name.
    datamod_list(c(currentList, newObjectList)) # Add to the list of data models.

    # Clear form inputs after successful creation for user convenience.
    updateTextInput(session, "dm_name", value = "")
    updateSelectInput(session, "series_name", selected = "population") # Reset to default.
    updateTextInput(session, "counts_file", value = "")
    updateTextInput(session, "age_selection", value = "")
    updateTextInput(session, "time_selection", value = "")
    updateTextInput(session, "counts_uncertainty_file", value = "") # Clear specific file inputs.
    # Consider resetting other dynamically generated inputs if necessary (e.g., via observeEvent on input$data_model change).
    shiny::showModal(modalDialog(title = "Data Model Created", paste("Data model '", newObject$nm_data, "' created."), easyClose = TRUE))
  })

  # Observers to update the choices in various selectInput dropdowns for data models.
  # These ensure that dropdowns for deleting, plotting, and inspecting always list the currently loaded data models.
  observe({
    updateSelectInput(session, "delete_data_model", choices = names(datamod_list()))
  })
  observe({
    updateSelectInput(session, "plot_data_model", choices = names(datamod_list()))
  })
  observe({
    updateSelectInput(session, "inspect_data_model1", choices = names(datamod_list()))
  })
  observe({
    updateSelectInput(session, "inspect_data_model2", choices = names(datamod_list()))
  })

  # Observer for the 'Delete Selected Data Model' button.
  shiny::observeEvent(input$delete_button, {
    req(input$delete_data_model) # Requires a data model name to be selected.
    currentList <- datamod_list()
    currentList[[input$delete_data_model]] <- NULL # Remove the selected model from the list.
    datamod_list(currentList) # Update the reactiveVal.
  })

  # Observer for the 'Plot Selected Data Model' button.
  shiny::observeEvent(input$plotdm_button, {
    req(input$plot_data_model) # Requires a data model to be selected for plotting.
    dm_select <- datamod_list()[[input$plot_data_model]] # Get the selected data model object.

    # Validate the selected data model and its data component.
    if (is.null(dm_select) || is.null(dm_select$data)) {
      shinyWidgets::sendSweetAlert(session = session, title = "Plot Error", text = "Selected data model for plotting is invalid or has no data component (dm_select$data is NULL).", type = "error")
      return()
    }
    # Define the UI output for the plot. This will create a plotlyOutput element.
    output$dmModelPlots <- shiny::renderUI({
      plotly::plotlyOutput("plot_dm_model_actual") # The actual plot will be rendered here.
    })
    # Render the plot using plotly.
    output$plot_dm_model_actual <- plotly::renderPlotly({
      p_data <- dm_select$data # The data to plot is expected to be in dm_select$data.
      # Ensure required columns (age, sex, time, count) exist in the data for plotting.
      req_cols <- c("age", "sex", "time", "count")
      if (!all(req_cols %in% names(p_data))) {
        shiny::showNotification("Data for plotting is missing required columns (age, sex, time, count). Cannot generate plot.", type = "error", duration = 10)
        return(plotly::plot_ly() %>% plotly::layout(title = "Error: Missing required columns for plot")) # Return empty plot on error.
      }

      # Create a ggplot object: points and lines by age, colored by time, faceted by sex.
      # Summarize counts by age, sex, time first to avoid overplotting if data is not already aggregated.
      p <- ggplot2::ggplot(p_data %>%
        dplyr::group_by(.data$age, .data$sex, .data$time) %>%
        dplyr::summarise(count = sum(.data$count, na.rm = TRUE), .groups = "drop")) +
        ggplot2::geom_point(ggplot2::aes(x = .data$age, y = .data$count, color = as.factor(.data$time))) +
        ggplot2::geom_line(ggplot2::aes(x = .data$age, y = .data$count, color = as.factor(.data$time))) +
        ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) + # Facet by sex.
        ggplot2::labs(title = paste("Data for:", dm_select$nm_data), color = "Time", x = "Age", y = "Count") +
        ggplot2::theme_minimal()
      plotly::ggplotly(p) # Convert ggplot to plotly for interactivity.
    })
  })

  # Observers for inspecting data model data in interactive tables (DT).
  # Allows users to view the raw data of selected data models.
  shiny::observeEvent(input$inspectdm_button1, { # For the first inspection panel.
    req(input$inspect_data_model1)
    dm_select <- datamod_list()[[input$inspect_data_model1]]
    output$dmModelInspect1 <- DT::renderDT({ # Render an interactive DataTable.
      DT::datatable(dm_select$data, filter = list(position = "top", clear = FALSE), options = list(scrollX = TRUE))
    })
  })
  shiny::observeEvent(input$inspectdm_button2, { # For the second inspection panel.
    req(input$inspect_data_model2)
    dm_select <- datamod_list()[[input$inspect_data_model2]]
    output$dmModelInspect2 <- DT::renderDT({
      DT::datatable(dm_select$data, filter = list(position = "top", clear = FALSE), options = list(scrollX = TRUE))
    })
  })

  # Render a DataTable summarizing the loaded data models.
  output$loadedDataModels <- DT::renderDT({
    dataModels <- datamod_list()
    if (length(dataModels) > 0) {
      # Create a summary for each data model (name, series, type, column names).
      modelSummaries <- lapply(dataModels, function(dm) {
        c(
          dm_name = dm$nm_data, # Name of the data model.
          series_name = dm$nm_series, # Series type (e.g., population, ins, outs).
          dm_type = class(dm)[[1]], # Class of the data model object (e.g., dpmaccount_datamod_exact).
          dm_data_cols = paste(colnames(dm$data), collapse = ", ") # Comma-separated list of columns in its data.
        )
      })
      modelSummaryDF <- as.data.frame(do.call(rbind, modelSummaries)) # Combine summaries into a data frame.
      DT::datatable(modelSummaryDF, options = list(scrollX = TRUE))
    } else {
      # Display a message if no data models are loaded.
      DT::datatable(data.frame(Message = "No data models loaded."), options = list(dom = "t")) # 'dom="t"' shows only the table.
    }
  })

  # Observer for the 'Import Selected Data Models' button (imports a list of data models from .RDS).
  shiny::observeEvent(input$import_button_datamod, {
    req(global_config()$data_dir, input$datamod_list_file) # Requires data directory and .RDS filename.
    datamods_file_path <- file.path(global_config()$data_dir, input$datamod_list_file)
    if (!file.exists(datamods_file_path)) {
      shinyWidgets::sendSweetAlert(session = session, title = "File Error", text = paste("Data model RDS file not found:", datamods_file_path), type = "error")
      return()
    }
    tryCatch(
      {
        datamods_list_imported <- readRDS(datamods_file_path) # Read the list of data models.
        # Validate that the imported object is a named list.
        if (!is.list(datamods_list_imported) || (length(datamods_list_imported) > 0 && is.null(names(datamods_list_imported)[1]))) {
          shinyWidgets::sendSweetAlert(session = session, title = "Import Error", text = "Imported RDS is not a valid named list of data models. Each data model in the list must be named.", type = "error")
          return()
        }
        currentList <- datamod_list()
        # Merge the imported list with the current list. `utils::modifyList` updates existing items and adds new ones.
        datamod_list(utils::modifyList(currentList, datamods_list_imported))
        shiny::showModal(modalDialog(title = "Import Successful", paste(length(datamods_list_imported), "data model(s) imported/updated."), easyClose = TRUE))
      },
      error = function(e) { # Handle errors during reading or processing.
        shinyWidgets::sendSweetAlert(session = session, title = "Import Error", text = paste("Could not read or process RDS file:", e$message), type = "error")
      }
    )
  })

  # Observer for the 'Export Data Models' button (exports all current data models to an .RDS file).
  shiny::observeEvent(input$export_data_models, {
    req(global_config()$output_dir, input$datamod_list_tag) # Requires output directory and a tag for the filename.
    dataModels_to_export <- datamod_list()
    if (length(dataModels_to_export) == 0) { # Check if there are any models to export.
      shinyWidgets::sendSweetAlert(session = session, title = "Export Error", text = "No data models to export.", type = "warning")
      return()
    }
    export_file_name <- paste0(input$datamod_list_tag, "_datamods.RDS") # Construct filename.
    export_file_path <- file.path(global_config()$output_dir, export_file_name) # Construct full path.
    tryCatch(
      {
        saveRDS(dataModels_to_export, export_file_path) # Save the list of data models.
        shiny::showModal(modalDialog(title = "Export Successful", paste("Data models saved to:", export_file_path), easyClose = TRUE))
      },
      error = function(e) { # Handle errors during saving.
        shinyWidgets::sendSweetAlert(session = session, title = "Export Error", text = paste("Failed to save data models:", e$message), type = "error")
      }
    )
  })

  # Navigation button to switch to the Data Models tab.
  shiny::observeEvent(input$goDM, {
    shiny::updateTabsetPanel(session, "tabs", selected = "dataModels")
  })
  ##############################################################################


  ##############################################################################
  ################################ fitModel Tab ################################
  ##############################################################################
  # This section handles the logic for the 'Fit Model' tab, where a single demographic
  # account model is fitted using selected system and data models.

  # Dynamically render UI for selecting a single system model setup for fitting.
  output$system_model_checklist <- shiny::renderUI({
    sys_setup_names <- names(sysmod_list_list()) # Get names of available system model setups.
    if (length(sys_setup_names) > 0) {
      # Use radio buttons for single selection.
      radioButtons("selected_system_models_fit", "Select System Model Setup:", choices = sys_setup_names)
    } else {
      p("No System Model Setups defined. Please create or import a setup in the 'System Models' tab.")
    }
  })

  # Reactive expression to get the system model list corresponding to the selected setup name for fitting.
  filtered_system_models_fit <- shiny::reactive({
    req(input$selected_system_models_fit) # Ensure a system model setup is selected.
    sysmod_list_list()[[input$selected_system_models_fit]] # Return the list of 4 model components.
  })

  # Dynamically render UI for selecting multiple data models for fitting.
  output$data_model_checklist <- shiny::renderUI({
    data_model_names <- names(datamod_list()) # Get names of available data models.
    # Use checkboxGroupInput for multiple selections.
    checkboxGroupInput("selected_data_models_fit", "Select Data Models (at least one required):", choices = data_model_names)
  })

  # Reactive expression to get the list of selected data model objects for fitting.
  filtered_data_models_fit <- shiny::reactive({
    req(input$selected_data_models_fit) # Ensure at least one data model is selected.
    datamod_list()[input$selected_data_models_fit] # Return a list of the selected data model objects.
  })

  # Reactive values to store the results of the single model fit (population and migration estimates).
  population_estimates_single_fit <- reactiveVal(NULL)
  migration_estimates_single_fit <- reactiveVal(NULL)


  # Observer for the 'Fit Model' button.
  shiny::observeEvent(input$fit_account_model, {
    # Ensure selected data models, system models, output directory, and seed value are available.
    req(
      filtered_data_models_fit(), filtered_system_models_fit(),
      global_config()$output_dir, global_config()$seed_value
    )

    # Clear any previous results from these reactiveValues and UI outputs.
    population_estimates_single_fit(NULL)
    migration_estimates_single_fit(NULL)
    output$cohortResults <- renderText({
      ""
    }) # Clear cohort success/failure text.
    output$cohortDiagnostics <- DT::renderDT(NULL) # Clear cohort diagnostics table.

    # Use withProgress to show a progress bar during model fitting.
    shiny::withProgress(message = "Fitting Account Model...", value = 0, {
      shiny::incProgress(0.1, detail = "Estimating account using dpmaccount...")
      # Call the 'estimate_account' function from the 'dpmaccount' package.
      # This is the core model fitting step.
      result <- tryCatch(
        dpmaccount::estimate_account(
          datamods = filtered_data_models_fit(), # Pass selected data models.
          sysmods = filtered_system_models_fit(), # Pass selected system models.
          seed_in = global_config()$seed_value # Pass the seed for reproducibility.
        ),
        error = function(e) { # Handle errors during estimation.
          shinyWidgets::sendSweetAlert(session, title = "Estimation Error", text = paste("Failed to estimate account with 'dpmaccount::estimate_account':", e$message), type = "error")
          return(NULL)
        }
      )
      if (is.null(result)) { # If estimation failed, stop.
        return()
      }

      shiny::incProgress(0.3, detail = "Saving raw model result (RDS)...")
      output_file_rds <- file.path(global_config()$output_dir, "fit_model_result.RDS") # Path for saving raw result.
      tryCatch(saveRDS(result, output_file_rds), error = function(e) warning(paste("Failed to save raw model result RDS:", e$message))) # Save and warn on error.

      shiny::incProgress(0.2, detail = "Generating diagnostics...")
      # Generate diagnostics from the model result (e.g., cohort success/failure).
      # Assumes 'dpmaccount::diagnostics' function exists.
      diag <- result %>% dpmaccount::diagnostics()
      cohort_passes <- nrow(diag %>% dplyr::filter(.data$success == TRUE)) # Count successful cohorts.
      cohort_total <- nrow(diag) # Total cohorts.
      output$cohortResults <- renderText({ # Display success rate.
        paste0(cohort_passes, " out of ", cohort_total, " cohorts fitted successfully.")
      })
      output$cohortDiagnostics <- DT::renderDT({ # Display table of failed cohorts.
        DT::datatable(diag %>% dplyr::filter(.data$success == FALSE), options = list(scrollX = TRUE))
      })

      shiny::incProgress(0.2, detail = "Augmenting population estimates...")
      # Augment population estimates from the model result.
      # Assumes 'dpmaccount::augment_population' function exists.
      pop_res_full <- result %>% dpmaccount::augment_population(collapse = "cohort")
      # Store augmented population data, excluding list columns for simpler display in DT.
      population_estimates_single_fit(pop_res_full %>% dplyr::select(-any_of(c("population"))))

      shiny::incProgress(0.1, detail = "Augmenting migration estimates...")
      # Augment migration (events) estimates.
      # Assumes 'dpmaccount::augment_events' function exists.
      mig_res_full <- result %>%
        dpmaccount::augment_events(collapse = "age") %>% # Collapse by age for migration.
        dplyr::mutate(age = .data$time - .data$cohort) %>% # Calculate age if not directly present.
        dplyr::select(-any_of(c("cohort", "age_calc"))) # Clean up temporary columns.
      # Store augmented migration data, excluding list columns for DT.
      migration_estimates_single_fit(mig_res_full %>% dplyr::select(-any_of(c("ins", "outs", "births", "deaths"))))

      shiny::incProgress(0.1, detail = "Saving augmented results (CSVs)...")
      output_file_pop_csv <- file.path(global_config()$output_dir, "population_estimates.csv")
      output_file_mig_csv <- file.path(global_config()$output_dir, "migration_estimates.csv")
      # Save augmented results as CSV files.
      tryCatch(utils::write.csv(population_estimates_single_fit(), output_file_pop_csv, row.names = FALSE), error = function(e) warning(paste("Failed to save population CSV:", e$message)))
      tryCatch(utils::write.csv(migration_estimates_single_fit(), output_file_mig_csv, row.names = FALSE), error = function(e) warning(paste("Failed to save migration CSV:", e$message)))
    }) # End withProgress

    shiny::showModal(modalDialog(title = "Model Fitted", "Model fitting completed. Results (RDS, CSVs) saved to output directory.", easyClose = TRUE))
  })

  # Observers to dynamically update time selection dropdowns based on the results of the single fit.
  observe({ # For population estimates time selector.
    pop_res_df <- population_estimates_single_fit()
    if (!is.null(pop_res_df) && "time" %in% names(pop_res_df)) {
      choices <- sort(unique(pop_res_df$time))
      updateSelectInput(session, "time_select_pop", choices = choices, selected = if (length(choices) > 0) choices[1] else NULL)
    }
  })
  observe({ # For migration estimates time selector.
    mig_res_df <- migration_estimates_single_fit()
    if (!is.null(mig_res_df) && "time" %in% names(mig_res_df)) {
      choices <- sort(unique(mig_res_df$time))
      choices_mig <- choices[choices != min(choices, na.rm = TRUE)] # Often migration is not for first period
      updateSelectInput(session, "time_select_mig", choices = choices_mig, selected = if (length(choices_mig) > 0) choices_mig[1] else NULL)
    }
  })

  # Render DataTables for the augmented population and migration estimates from the single fit.
  output$population_table <- DT::renderDT({
    req(population_estimates_single_fit()) # Require results to be available.
    DT::datatable(population_estimates_single_fit(), filter = "top", options = list(scrollX = TRUE))
  })
  output$migration_table <- DT::renderDT({
    req(migration_estimates_single_fit())
    DT::datatable(migration_estimates_single_fit(), filter = "top", options = list(scrollX = TRUE))
  })

  ## popEstimates Tab (Plots for Single Fit)
  # This section handles the display of plots for population estimates from the single model fit.
  output$popPlots <- shiny::renderUI({ # UI placeholder for the population plot.
    plotly::plotlyOutput("population_estimates_plot")
  })
  output$population_estimates_plot <- plotly::renderPlotly({
    req(population_estimates_single_fit(), input$time_select_pop) # Require results and selected time.
    pop_data_to_plot <- population_estimates_single_fit() %>%
      dplyr::filter(.data$time == as.numeric(input$time_select_pop)) # Filter by selected time.

    # Create ggplot: pointrange for fitted population with confidence intervals.
    p <- ggplot2::ggplot(pop_data_to_plot, ggplot2::aes(
      x = .data$age, ymin = .data$population.lower, # Lower bound of CI.
      y = .data$population.fitted, # Fitted value.
      ymax = .data$population.upper # Upper bound of CI.
    )) +
      ggplot2::geom_pointrange(fatten = 0.2, col = "darkorange") + # fatten adjusts point size relative to line thickness.
      ggplot2::ylab("Count") +
      ggplot2::ggtitle(paste("Population Estimates (Fitted) for Time:", input$time_select_pop)) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) + # Facet by sex.
      ggplot2::theme_minimal()

    # Optionally, add points from a comparison column if specified by the user.
    if (nzchar(input$compare_select_pop) && input$compare_select_pop %in% colnames(pop_data_to_plot)) {
      p <- p + ggplot2::geom_point(ggplot2::aes(y = .data[[input$compare_select_pop]]), col = "darkblue", size = 0.5, shape = 4, na.rm = TRUE) +
        ggplot2::labs(caption = paste("Comparing with observed/alternative data from column:", input$compare_select_pop))
    }
    plotly::ggplotly(p) # Convert to interactive plotly.
  })

  ## migEstimates Tab (Plots for Single Fit)
  # This section handles plots for immigration and emigration estimates from the single model fit.
  output$immPlots <- shiny::renderUI({ # UI placeholder for immigration plot.
    plotlyOutput("immigration_estimates_plot")
  })
  output$immigration_estimates_plot <- plotly::renderPlotly({
    req(migration_estimates_single_fit(), input$time_select_mig) # Require results and selected time.
    mig_data_to_plot <- migration_estimates_single_fit() %>%
      dplyr::filter(.data$time == as.numeric(input$time_select_mig))

    # Plot for immigration (ins).
    p <- ggplot2::ggplot(mig_data_to_plot, ggplot2::aes(
      x = .data$age, ymin = .data$ins.lower, y = .data$ins.fitted, ymax = .data$ins.upper
    )) +
      ggplot2::geom_pointrange(fatten = 0.2, col = "darkorange") +
      ggplot2::ylab("Count") +
      ggplot2::ggtitle(paste("Immigration Estimates (Fitted) for Time:", input$time_select_mig)) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::theme_minimal()

    if (nzchar(input$compare_select_ins) && input$compare_select_ins %in% colnames(mig_data_to_plot)) {
      p <- p + ggplot2::geom_point(ggplot2::aes(y = .data[[input$compare_select_ins]]), col = "darkblue", size = 0.5, shape = 4, na.rm = TRUE) +
        ggplot2::labs(caption = paste("Comparing with:", input$compare_select_ins))
    }
    plotly::ggplotly(p)
  })

  output$emPlots <- shiny::renderUI({ # UI placeholder for emigration plot.
    plotlyOutput("emigration_estimates_plot")
  })
  output$emigration_estimates_plot <- plotly::renderPlotly({
    req(migration_estimates_single_fit(), input$time_select_mig)
    mig_data_to_plot <- migration_estimates_single_fit() %>%
      dplyr::filter(.data$time == as.numeric(input$time_select_mig))

    # Plot for emigration (outs).
    p <- ggplot2::ggplot(mig_data_to_plot, ggplot2::aes(
      x = .data$age, ymin = .data$outs.lower, y = .data$outs.fitted, ymax = .data$outs.upper
    )) +
      ggplot2::geom_pointrange(fatten = 0.2, col = "darkorange") +
      ggplot2::ylab("Count") +
      ggplot2::ggtitle(paste("Emigration Estimates (Fitted) for Time:", input$time_select_mig)) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::theme_minimal()

    if (nzchar(input$compare_select_outs) && input$compare_select_outs %in% colnames(mig_data_to_plot)) {
      p <- p + ggplot2::geom_point(ggplot2::aes(y = .data[[input$compare_select_outs]]), col = "darkblue", size = 0.5, shape = 4, na.rm = TRUE) +
        ggplot2::labs(caption = paste("Comparing with:", input$compare_select_outs))
    }
    plotly::ggplotly(p)
  })

  # Navigation buttons for moving between fit/results tabs.
  shiny::observeEvent(input$goFM, {
    shiny::updateTabsetPanel(session, "tabs", selected = "fitModel")
  }) # To Fit Model
  shiny::observeEvent(input$goPE, {
    shiny::updateTabsetPanel(session, "tabs", selected = "popEstimates")
  }) # To Population Estimates
  shiny::observeEvent(input$goME, {
    shiny::updateTabsetPanel(session, "tabs", selected = "migEstimates")
  }) # To Migration Estimates
  shiny::observeEvent(input$goSC, {
    shiny::updateTabsetPanel(session, "tabs", selected = "fitModels")
  }) # To Compare Setups (fitModels is first subtab)
  ##############################################################################


  ##############################################################################
  ################################ setupComp Tab ###############################
  ##############################################################################
  # This section handles logic for the "Compare Setups" functionality, allowing users
  # to fit two different model configurations and compare their results.

  # UI for selecting system models for Setup 1 and Setup 2 for comparison.
  output$sys_model_checklist_1 <- shiny::renderUI({
    choices <- names(sysmod_list_list())
    radioButtons("selected_sys_models_1_comp", "Setup 1: System Models", choices = choices, selected = if (length(choices) > 0) choices[1] else NULL)
  })
  output$sys_model_checklist_2 <- shiny::renderUI({
    choices <- names(sysmod_list_list())
    # Select the second choice if available, otherwise the first, for default difference.
    radioButtons("selected_sys_models_2_comp", "Setup 2: System Models", choices = choices, selected = if (length(choices) > 1) choices[2] else if (length(choices) > 0) choices[1] else NULL)
  })

  # UI for selecting data models for Setup 1 and Setup 2 for comparison.
  output$data_model_checklist_1 <- shiny::renderUI({
    choices <- names(datamod_list())
    checkboxGroupInput("selected_data_models_1_comp", "Setup 1: Data Models", choices = choices)
  })
  output$data_model_checklist_2 <- shiny::renderUI({
    choices <- names(datamod_list())
    checkboxGroupInput("selected_data_models_2_comp", "Setup 2: Data Models", choices = choices)
  })

  # Reactive expressions to get the filtered system and data models for Setup 1.
  filtered_sys_models_1_comp <- shiny::reactive({
    req(input$selected_sys_models_1_comp)
    sysmod_list_list()[[input$selected_sys_models_1_comp]]
  })
  filtered_data_models_1_comp <- shiny::reactive({
    req(input$selected_data_models_1_comp)
    datamod_list()[input$selected_data_models_1_comp]
  })
  # Reactive expressions for Setup 2.
  filtered_sys_models_2_comp <- shiny::reactive({
    req(input$selected_sys_models_2_comp)
    sysmod_list_list()[[input$selected_sys_models_2_comp]]
  })
  filtered_data_models_2_comp <- shiny::reactive({
    req(input$selected_data_models_2_comp)
    datamod_list()[input$selected_data_models_2_comp]
  })

  # Reactive value to store the results of the comparison.
  # This will hold combined data frames and wide-format data for residuals.
  comparison_results <- reactiveVal(NULL)

  # Observer for the 'Compare Models' button.
  shiny::observeEvent(input$compare_account_model, {
    # Ensure all required inputs (models for both setups, global config) are available.
    req(
      filtered_data_models_1_comp(), filtered_data_models_2_comp(),
      filtered_sys_models_1_comp(), filtered_sys_models_2_comp(),
      global_config()$output_dir, global_config()$seed_value
    )

    comparison_results(NULL) # Clear previous comparison results.

    shiny::withProgress(message = "Comparing Account Models...", value = 0, {
      shiny::incProgress(0.1, detail = "Estimating Model for Setup 1...")
      # Estimate account for Setup 1.
      result_1 <- tryCatch(dpmaccount::estimate_account(datamods = filtered_data_models_1_comp(), sysmods = filtered_sys_models_1_comp(), seed_in = global_config()$seed_value),
        error = function(e) {
          shinyWidgets::sendSweetAlert(session, title = "Error Estimating Model 1", text = e$message, type = "error")
          NULL
        }
      )
      if (is.null(result_1)) {
        return()
      } # Stop if error.

      shiny::incProgress(0.4, detail = "Estimating Model for Setup 2...")
      # Estimate account for Setup 2.
      result_2 <- tryCatch(dpmaccount::estimate_account(datamods = filtered_data_models_2_comp(), sysmods = filtered_sys_models_2_comp(), seed_in = global_config()$seed_value),
        error = function(e) {
          shinyWidgets::sendSweetAlert(session, title = "Error Estimating Model 2", text = e$message, type = "error")
          NULL
        }
      )
      if (is.null(result_2)) {
        return()
      }

      shiny::incProgress(0.2, detail = "Augmenting results for Model 1...")
      # Augment population and migration for Setup 1.
      pop_1 <- tryCatch(result_1 %>% dpmaccount::augment_population(collapse = "cohort"), error = function(e) NULL)
      mig_1 <- tryCatch(result_1 %>% dpmaccount::augment_events(collapse = "age") %>% dplyr::mutate(age = .data$time - .data$cohort) %>% dplyr::select(-any_of("cohort")), error = function(e) NULL)

      shiny::incProgress(0.2, detail = "Augmenting results for Model 2...")
      # Augment population and migration for Setup 2.
      pop_2 <- tryCatch(result_2 %>% dpmaccount::augment_population(collapse = "cohort"), error = function(e) NULL)
      mig_2 <- tryCatch(result_2 %>% dpmaccount::augment_events(collapse = "age") %>% dplyr::mutate(age = .data$time - .data$cohort) %>% dplyr::select(-any_of("cohort")), error = function(e) NULL)

      if (is.null(pop_1) || is.null(mig_1) || is.null(pop_2) || is.null(mig_2)) {
        shinyWidgets::sendSweetAlert(session, title = "Augmentation Error", text = "Could not augment results for one or both models after estimation.", type = "error")
        return()
      }

      # Store full augmented results (including list columns like 'population', 'ins', 'outs') for detailed inspection if needed.
      full_pop_1 <- pop_1
      full_mig_1 <- mig_1
      full_pop_2 <- pop_2
      full_mig_2 <- mig_2

      # Prepare data for combined plotting: select key columns and remove list columns for easier binding.
      # Add a 'setup' column to distinguish between the two model runs.
      pop_res_1 <- pop_1 %>%
        dplyr::mutate(setup = "Setup 1") %>%
        dplyr::select(age, sex, time, population.lower, population.fitted, population.upper, setup)
      mig_res_1 <- mig_1 %>%
        dplyr::mutate(setup = "Setup 1") %>%
        dplyr::select(age, sex, time, ins.lower, ins.fitted, ins.upper, outs.lower, outs.fitted, outs.upper, setup)
      pop_res_2 <- pop_2 %>%
        dplyr::mutate(setup = "Setup 2") %>%
        dplyr::select(age, sex, time, population.lower, population.fitted, population.upper, setup)
      mig_res_2 <- mig_2 %>%
        dplyr::mutate(setup = "Setup 2") %>%
        dplyr::select(age, sex, time, ins.lower, ins.fitted, ins.upper, outs.lower, outs.fitted, outs.upper, setup)

      combined_pop <- dplyr::bind_rows(pop_res_1, pop_res_2) # Combine population results.
      combined_mig <- dplyr::bind_rows(mig_res_1, mig_res_2) # Combine migration results.

      # Prepare data in wide format for residual plots (difference between Setup 2 and Setup 1).
      pop_res_comp_wide <- dplyr::inner_join( # Join based on age, sex, time.
        pop_res_1 %>%
          dplyr::select(age, sex, time, population.fitted) %>%
          dplyr::rename(fitted1 = population.fitted),
        pop_res_2 %>% dplyr::select(age, sex, time, population.fitted) %>% dplyr::rename(fitted2 = population.fitted),
        by = c("age", "sex", "time")
      ) %>% dplyr::mutate( # Calculate absolute and percent residuals.
        fitted_residuals_abs = .data$fitted2 - .data$fitted1,
        fitted_residuals_perc = ifelse(.data$fitted1 == 0, NA, 100 * (.data$fitted_residuals_abs / .data$fitted1)) # Avoid division by zero.
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

      # Store all processed results in the reactiveVal.
      comparison_results(list(
        pop_combined = combined_pop, mig_combined = combined_mig, # For overlay plots.
        pop_wide = pop_res_comp_wide, mig_wide = mig_res_comp_wide, # For residual plots.
        full_pop_1 = full_pop_1, full_mig_1 = full_mig_1, # Full results for Setup 1.
        full_pop_2 = full_pop_2, full_mig_2 = full_mig_2 # Full results for Setup 2.
      ))
      shiny::incProgress(0.1, detail = "Comparison data prepared.")
    }) # End withProgress
    shiny::showModal(modalDialog(title = "Comparison Complete", "Model comparison finished. You can now view comparison plots.", easyClose = TRUE))
  })

  # Observer to update time selectors and comparison column selectors for comparison plots.
  observe({
    res <- comparison_results() # Get the comparison results.
    if (!is.null(res$pop_combined) && "time" %in% names(res$pop_combined)) {
      choices <- sort(unique(res$pop_combined$time))
      choices_mig <- choices[choices != min(choices, na.rm = TRUE)] # Migration plots often exclude first time point.
      updateSelectInput(session, "time_select_comp", choices = choices, selected = if (length(choices) > 0) choices[1] else NULL) # For population comparison time.
      updateSelectInput(session, "time_select_comp_mig", choices = choices_mig, selected = if (length(choices_mig) > 0) choices_mig[1] else NULL) # For migration comparison time.
    }

    # Populate 'Compare Column' dropdowns with actual data column names from the *full* augmented results.
    # These are used to overlay original/observed data onto the fitted estimates plots.
    if (!is.null(res$full_pop_1)) {
      # Exclude standard columns generated by augmentation to list only potential original data columns.
      cols1 <- setdiff(colnames(res$full_pop_1), c("age", "sex", "time", "setup", "population", "population.fitted", "population.lower", "population.upper"))
      updateSelectInput(session, "compare_select_pop_1", choices = c("None", cols1), selected = "None")
    }
    if (!is.null(res$full_pop_2)) {
      cols2 <- setdiff(colnames(res$full_pop_2), c("age", "sex", "time", "setup", "population", "population.fitted", "population.lower", "population.upper"))
      updateSelectInput(session, "compare_select_pop_2", choices = c("None", cols2), selected = "None")
    }
    if (!is.null(res$full_mig_1)) {
      mig_cols1 <- setdiff(colnames(res$full_mig_1), c("age", "sex", "time", "setup", "ins", "ins.fitted", "ins.lower", "ins.upper", "outs", "outs.fitted", "outs.lower", "outs.upper", "births", "deaths"))
      updateSelectInput(session, "compare_select_ins_1", choices = c("None", mig_cols1), selected = "None")
      updateSelectInput(session, "compare_select_outs_1", choices = c("None", mig_cols1), selected = "None")
    }
    if (!is.null(res$full_mig_2)) {
      mig_cols2 <- setdiff(colnames(res$full_mig_2), c("age", "sex", "time", "setup", "ins", "ins.fitted", "ins.lower", "ins.upper", "outs", "outs.fitted", "outs.lower", "outs.upper", "births", "deaths"))
      updateSelectInput(session, "compare_select_ins_2", choices = c("None", mig_cols2), selected = "None")
      updateSelectInput(session, "compare_select_outs_2", choices = c("None", mig_cols2), selected = "None")
    }
  })

  # Population Comparison Plots (Overlay and Residuals)
  output$compPlots <- shiny::renderUI({ # UI for overlay plot.
    plotly::plotlyOutput("comparing_estimates_plot")
  })
  output$comparing_estimates_plot <- plotly::renderPlotly({ # Actual overlay plot.
    res <- comparison_results()
    req(res, res$pop_combined, input$time_select_comp, input$setup_select) # Ensure data and inputs are ready.
    plot_data <- res$pop_combined %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp)) # Filter by selected time.

    # Filter by selected setup ("Both", "Setup 1", or "Setup 2").
    if (input$setup_select != "Both") {
      plot_data <- plot_data %>% dplyr::filter(.data$setup == input$setup_select)
    }

    # Create ggplot: lines for fitted population, ribbons for confidence intervals, colored by setup.
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
      x = .data$age, y = .data$population.fitted,
      ymin = .data$population.lower, ymax = .data$population.upper,
      color = .data$setup, fill = .data$setup # Color and fill by setup.
    )) +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(alpha = 0.3, linetype = "dashed") + # Semi-transparent ribbon.
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = paste("Population Comparison for Time:", input$time_select_comp), y = "Count", color = "Setup", fill = "Setup") +
      ggplot2::theme_minimal()

    # Add points for comparison data if selected for Setup 1.
    if (input$setup_select == "Setup 1" || input$setup_select == "Both") {
      if (input$compare_select_pop_1 != "None" && !is.null(res$full_pop_1) && input$compare_select_pop_1 %in% colnames(res$full_pop_1)) {
        comp_data_1 <- res$full_pop_1 %>%
          dplyr::filter(.data$time == as.numeric(input$time_select_comp)) %>%
          dplyr::select(age, sex, time, compare_val = .data[[input$compare_select_pop_1]])
        p <- p + ggplot2::geom_point(
          data = comp_data_1, ggplot2::aes(x = .data$age, y = .data$compare_val), color = "red", shape = 4, inherit.aes = FALSE, na.rm = TRUE
        )
      }
    }
    # Add points for comparison data if selected for Setup 2.
    if (input$setup_select == "Setup 2" || input$setup_select == "Both") {
      if (input$compare_select_pop_2 != "None" && !is.null(res$full_pop_2) && input$compare_select_pop_2 %in% colnames(res$full_pop_2)) {
        comp_data_2 <- res$full_pop_2 %>%
          dplyr::filter(.data$time == as.numeric(input$time_select_comp)) %>%
          dplyr::select(age, sex, time, compare_val = .data[[input$compare_select_pop_2]])
        p <- p + ggplot2::geom_point(
          data = comp_data_2, ggplot2::aes(x = .data$age, y = .data$compare_val), color = "blue", shape = 3, inherit.aes = FALSE, na.rm = TRUE
        )
      }
    }
    plotly::ggplotly(p)
  })

  output$compPlotsResiduals <- shiny::renderUI({ # UI for residual plot.
    plotly::plotlyOutput("comparing_estimates_residuals_plot")
  })
  output$comparing_estimates_residuals_plot <- plotly::renderPlotly({ # Actual residual plot.
    res <- comparison_results()
    req(res, res$pop_wide, input$time_select_comp, input$residual_type) # Use wide-format data.
    plot_data <- res$pop_wide %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp))

    # Determine which residual column to plot (absolute or percent).
    y_val_col <- if (input$residual_type == "Absolute") "fitted_residuals_abs" else "fitted_residuals_perc"
    y_lab_text <- if (input$residual_type == "Absolute") "Absolute Difference (Setup 2 - Setup 1)" else "Percent Difference (Setup 2 - Setup 1)"

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$age, y = .data[[y_val_col]])) +
      ggplot2::geom_segment(ggplot2::aes(xend = .data$age, yend = 0)) + # Segments from zero line to point.
      ggplot2::geom_point(size = 2, na.rm = TRUE) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = paste("Population Comparison Residuals (", input$residual_type, ") for Time:", input$time_select_comp), y = y_lab_text) +
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })

  # Migration Comparison Plots (Overlay and Residuals for Immigration and Emigration)
  # These follow a similar structure to the population comparison plots.

  # Immigration Overlay Plot
  output$compImmig <- shiny::renderUI({
    plotly::plotlyOutput("comparing_ins_plot")
  })
  output$comparing_ins_plot <- plotly::renderPlotly({
    res <- comparison_results()
    req(res, res$mig_combined, input$time_select_comp_mig, input$setup_select)
    plot_data <- res$mig_combined %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp_mig))
    if (input$setup_select != "Both") plot_data <- plot_data %>% dplyr::filter(.data$setup == input$setup_select)

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
      x = .data$age, y = .data$ins.fitted, ymin = .data$ins.lower, ymax = .data$ins.upper,
      color = .data$setup, fill = .data$setup
    )) +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(alpha = 0.3, linetype = "dashed") +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = paste("Immigration Comparison for Time:", input$time_select_comp_mig), y = "Count") +
      ggplot2::theme_minimal()

    if (input$setup_select == "Setup 1" || input$setup_select == "Both") {
      if (input$compare_select_ins_1 != "None" && !is.null(res$full_mig_1) && input$compare_select_ins_1 %in% colnames(res$full_mig_1)) {
        comp_data_1 <- res$full_mig_1 %>%
          dplyr::filter(.data$time == as.numeric(input$time_select_comp_mig)) %>%
          dplyr::select(age, sex, time, compare_val = .data[[input$compare_select_ins_1]])
        p <- p + ggplot2::geom_point(data = comp_data_1, aes(x = .data$age, y = .data$compare_val), color = "red", shape = 4, inherit.aes = FALSE, na.rm = TRUE)
      }
    }
    if (input$setup_select == "Setup 2" || input$setup_select == "Both") {
      if (input$compare_select_ins_2 != "None" && !is.null(res$full_mig_2) && input$compare_select_ins_2 %in% colnames(res$full_mig_2)) {
        comp_data_2 <- res$full_mig_2 %>%
          dplyr::filter(.data$time == as.numeric(input$time_select_comp_mig)) %>%
          dplyr::select(age, sex, time, compare_val = .data[[input$compare_select_ins_2]])
        p <- p + ggplot2::geom_point(data = comp_data_2, aes(x = .data$age, y = .data$compare_val), color = "blue", shape = 3, inherit.aes = FALSE, na.rm = TRUE)
      }
    }
    plotly::ggplotly(p)
  })

  # Emigration Overlay Plot
  output$compEmig <- shiny::renderUI({
    plotly::plotlyOutput("comparing_outs_plot")
  })
  output$comparing_outs_plot <- plotly::renderPlotly({
    res <- comparison_results()
    req(res, res$mig_combined, input$time_select_comp_mig, input$setup_select)
    plot_data <- res$mig_combined %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp_mig))
    if (input$setup_select != "Both") plot_data <- plot_data %>% dplyr::filter(.data$setup == input$setup_select)

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
      x = .data$age, y = .data$outs.fitted, ymin = .data$outs.lower, ymax = .data$outs.upper,
      color = .data$setup, fill = .data$setup
    )) +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(alpha = 0.3, linetype = "dashed") +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = paste("Emigration Comparison for Time:", input$time_select_comp_mig), y = "Count") +
      ggplot2::theme_minimal()

    if (input$setup_select == "Setup 1" || input$setup_select == "Both") {
      if (input$compare_select_outs_1 != "None" && !is.null(res$full_mig_1) && input$compare_select_outs_1 %in% colnames(res$full_mig_1)) {
        comp_data_1 <- res$full_mig_1 %>%
          dplyr::filter(.data$time == as.numeric(input$time_select_comp_mig)) %>%
          dplyr::select(age, sex, time, compare_val = .data[[input$compare_select_outs_1]])
        p <- p + ggplot2::geom_point(data = comp_data_1, aes(x = .data$age, y = .data$compare_val), color = "red", shape = 4, inherit.aes = FALSE, na.rm = TRUE)
      }
    }
    if (input$setup_select == "Setup 2" || input$setup_select == "Both") {
      if (input$compare_select_outs_2 != "None" && !is.null(res$full_mig_2) && input$compare_select_outs_2 %in% colnames(res$full_mig_2)) {
        comp_data_2 <- res$full_mig_2 %>%
          dplyr::filter(.data$time == as.numeric(input$time_select_comp_mig)) %>%
          dplyr::select(age, sex, time, compare_val = .data[[input$compare_select_outs_2]])
        p <- p + ggplot2::geom_point(data = comp_data_2, aes(x = .data$age, y = .data$compare_val), color = "blue", shape = 3, inherit.aes = FALSE, na.rm = TRUE)
      }
    }
    plotly::ggplotly(p)
  })

  # Immigration Residual Plot
  output$compPlotsImmigResiduals <- shiny::renderUI({
    plotly::plotlyOutput("comparing_immig_residuals_plot")
  })
  output$comparing_immig_residuals_plot <- plotly::renderPlotly({
    res <- comparison_results()
    req(res, res$mig_wide, input$time_select_comp_mig, input$residual_type)
    plot_data <- res$mig_wide %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp_mig))
    y_val_col <- if (input$residual_type == "Absolute") "ins_residuals_abs" else "ins_residuals_perc"
    y_lab_text <- if (input$residual_type == "Absolute") "Abs. Diff (S2-S1)" else "Perc. Diff (S2-S1)"
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$age, y = .data[[y_val_col]])) +
      ggplot2::geom_segment(ggplot2::aes(xend = .data$age, yend = 0)) +
      ggplot2::geom_point(size = 2, na.rm = TRUE) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = paste("Immigration Residuals (", input$residual_type, ") for Time:", input$time_select_comp_mig), y = y_lab_text) +
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })

  # Emigration Residual Plot
  output$compPlotsEmigResiduals <- shiny::renderUI({
    plotly::plotlyOutput("comparing_emig_residuals_plot")
  })
  output$comparing_emig_residuals_plot <- plotly::renderPlotly({
    res <- comparison_results()
    req(res, res$mig_wide, input$time_select_comp_mig, input$residual_type)
    plot_data <- res$mig_wide %>% dplyr::filter(.data$time == as.numeric(input$time_select_comp_mig))
    y_val_col <- if (input$residual_type == "Absolute") "outs_residuals_abs" else "outs_residuals_perc"
    y_lab_text <- if (input$residual_type == "Absolute") "Abs. Diff (S2-S1)" else "Perc. Diff (S2-S1)"
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$age, y = .data[[y_val_col]])) +
      ggplot2::geom_segment(ggplot2::aes(xend = .data$age, yend = 0)) +
      ggplot2::geom_point(size = 2, na.rm = TRUE) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = paste("Emigration Residuals (", input$residual_type, ") for Time:", input$time_select_comp_mig), y = y_lab_text) +
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })

  # Aggregate Comparison Plots (e.g., total population over time for each setup)
  output$compAggPop <- shiny::renderUI({
    plotly::plotlyOutput("comparing_agg_pop_plot")
  })
  output$comparing_agg_pop_plot <- plotly::renderPlotly({
    res <- comparison_results()
    req(res, res$pop_combined)
    # Aggregate total population by time, sex, and setup.
    pop_aggs <- res$pop_combined %>%
      dplyr::group_by(.data$time, .data$sex, .data$setup) %>%
      dplyr::summarise(agg_pop = sum(.data$population.fitted, na.rm = TRUE), .groups = "drop")
    p <- ggplot2::ggplot(pop_aggs, ggplot2::aes(x = .data$time, y = .data$agg_pop, color = .data$setup)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = "Aggregate Population Comparison Over Time", y = "Total Population", x = "Time") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })

  output$compAggImmig <- shiny::renderUI({
    plotly::plotlyOutput("comparing_agg_immig_plot")
  })
  output$comparing_agg_immig_plot <- plotly::renderPlotly({
    res <- comparison_results()
    req(res, res$mig_combined)
    # Aggregate total immigration by time, sex, and setup.
    immig_aggs <- res$mig_combined %>%
      dplyr::group_by(.data$time, .data$sex, .data$setup) %>%
      dplyr::summarise(agg_immig = sum(.data$ins.fitted, na.rm = TRUE), .groups = "drop")
    p <- ggplot2::ggplot(immig_aggs, ggplot2::aes(x = .data$time, y = .data$agg_immig, color = .data$setup)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = "Aggregate Immigration Comparison Over Time", y = "Total Immigration", x = "Time") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })

  output$compAggEmig <- shiny::renderUI({
    plotly::plotlyOutput("comparing_agg_emig_plot")
  })
  output$comparing_agg_emig_plot <- plotly::renderPlotly({
    res <- comparison_results()
    req(res, res$mig_combined)
    # Aggregate total emigration by time, sex, and setup.
    emig_aggs <- res$mig_combined %>%
      dplyr::group_by(.data$time, .data$sex, .data$setup) %>%
      dplyr::summarise(agg_emig = sum(.data$outs.fitted, na.rm = TRUE), .groups = "drop")
    p <- ggplot2::ggplot(emig_aggs, ggplot2::aes(x = .data$time, y = .data$agg_emig, color = .data$setup)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = "Aggregate Emigration Comparison Over Time", y = "Total Emigration", x = "Time") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })

  # Placeholder for other aggregate plots (Immigration, Emigration, Net Migration, and their differences).
  # These would follow a similar pattern: group_by relevant variables, summarise the metric (e.g., sum of ins.fitted),
  # then plot over time, often faceted by sex and colored by setup.
  # For difference plots, data would be pivoted wider to calculate differences between setups.

  output$compAggPopDiffs <- shiny::renderUI({
    plotly::plotlyOutput("comparing_agg_pop_diffs_plot")
  })
  output$comparing_agg_pop_diffs_plot <- plotly::renderPlotly({
    res <- comparison_results()
    req(res, res$pop_combined)
    pop_aggs_diff <- res$pop_combined %>%
      dplyr::group_by(.data$time, .data$sex, .data$setup) %>%
      dplyr::summarise(agg_pop = sum(.data$population.fitted, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = .data$setup, values_from = .data$agg_pop, names_prefix = "agg_pop_") %>% # Pivot to wide format.
      # Ensure columns for "Setup 1" and "Setup 2" exist before trying to subtract.
      # The actual column names will be 'agg_ins_Setup 1' and 'agg_ins_Setup 2' due to names_prefix.
      dplyr::mutate(
        abs_diff = .data$`agg_pop_Setup 2` - .data$`agg_pop_Setup 1`,
        perc_diff = ifelse(.data$`agg_pop_Setup 1` == 0, NA, 100 * .data$abs_diff / .data$`agg_pop_Setup 1`)
      )

    # Prepare for plotting both absolute and percent differences in one go using faceting.
    p1_data <- pop_aggs_diff %>%
      dplyr::select(.data$time, .data$sex, value = .data$abs_diff) %>%
      dplyr::mutate(type = "Absolute Difference")
    p2_data <- pop_aggs_diff %>%
      dplyr::select(.data$time, .data$sex, value = .data$perc_diff) %>%
      dplyr::mutate(type = "Percent Difference")
    plot_data_long <- dplyr::bind_rows(p1_data, p2_data) %>% tidyr::drop_na(.data$value) # Combine and remove NAs.

    p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = .data$time, y = .data$value)) +
      ggplot2::geom_segment(ggplot2::aes(xend = .data$time, yend = 0)) +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_grid(type ~ sex, scales = "free_y") + # Free y-scale for abs vs. perc.
      ggplot2::labs(title = "Aggregate Population Comparison: Differences (Setup 2 - Setup 1)", y = "Difference Value", x = "Time") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })

  # Example for Aggregate Immigration Differences plot:
  output$compAggImmigDiffs <- shiny::renderUI({
    plotly::plotlyOutput("comparing_agg_immig_diffs_plot")
  })
  output$comparing_agg_immig_diffs_plot <- plotly::renderPlotly({
    res <- comparison_results()
    req(res, res$mig_combined)
    mig_aggs_diff <- res$mig_combined %>%
      dplyr::group_by(.data$time, .data$sex, .data$setup) %>%
      dplyr::summarise(agg_ins = sum(.data$ins.fitted, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = .data$setup, values_from = .data$agg_ins, names_prefix = "agg_ins_") %>% # Pivot to wide format.
      # Ensure columns for "Setup 1" and "Setup 2" exist before trying to subtract.
      # The actual column names will be 'agg_ins_Setup 1' and 'agg_ins_Setup 2' due to names_prefix.
      dplyr::mutate(
        abs_diff = .data$`agg_ins_Setup 2` - .data$`agg_ins_Setup 1`,
        perc_diff = ifelse(.data$`agg_ins_Setup 1` == 0, NA, 100 * .data$abs_diff / .data$`agg_ins_Setup 1`)
      )

    # Prepare for plotting both absolute and percent differences in one go using faceting.
    p1_data <- mig_aggs_diff %>%
      dplyr::select(.data$time, .data$sex, value = .data$abs_diff) %>%
      dplyr::mutate(type = "Absolute Difference")
    p2_data <- mig_aggs_diff %>%
      dplyr::select(.data$time, .data$sex, value = .data$perc_diff) %>%
      dplyr::mutate(type = "Percent Difference")
    plot_data_long <- dplyr::bind_rows(p1_data, p2_data) %>% tidyr::drop_na(.data$value) # Combine and remove NAs.

    p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = .data$time, y = .data$value)) +
      ggplot2::geom_segment(ggplot2::aes(xend = .data$time, yend = 0)) +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_grid(type ~ sex, scales = "free_y") + # Free y-scale for abs vs. perc.
      ggplot2::labs(title = "Aggregate Immigration Comparison: Differences (Setup 2 - Setup 1)", y = "Difference Value", x = "Time") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })

  output$compAggEmigDiffs <- shiny::renderUI({
    plotly::plotlyOutput("comparing_agg_emig_diffs_plot")
  })
  output$comparing_agg_emig_diffs_plot <- plotly::renderPlotly({
    res <- comparison_results()
    req(res, res$mig_combined)
    mig_aggs_diff <- res$mig_combined %>%
      dplyr::group_by(.data$time, .data$sex, .data$setup) %>%
      dplyr::summarise(agg_outs = sum(.data$outs.fitted, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = .data$setup, values_from = .data$agg_outs, names_prefix = "agg_outs_") %>% # Pivot to wide format.
      # Ensure columns for "Setup 1" and "Setup 2" exist before trying to subtract.
      # The actual column names will be 'agg_ins_Setup 1' and 'agg_ins_Setup 2' due to names_prefix.
      dplyr::mutate(
        abs_diff = .data$`agg_outs_Setup 2` - .data$`agg_outs_Setup 1`,
        perc_diff = ifelse(.data$`agg_outs_Setup 1` == 0, NA, 100 * .data$abs_diff / .data$`agg_outs_Setup 1`)
      )

    # Prepare for plotting both absolute and percent differences in one go using faceting.
    p1_data <- mig_aggs_diff %>%
      dplyr::select(.data$time, .data$sex, value = .data$abs_diff) %>%
      dplyr::mutate(type = "Absolute Difference")
    p2_data <- mig_aggs_diff %>%
      dplyr::select(.data$time, .data$sex, value = .data$perc_diff) %>%
      dplyr::mutate(type = "Percent Difference")
    plot_data_long <- dplyr::bind_rows(p1_data, p2_data) %>% tidyr::drop_na(.data$value) # Combine and remove NAs.

    p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = .data$time, y = .data$value)) +
      ggplot2::geom_segment(ggplot2::aes(xend = .data$time, yend = 0)) +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_grid(type ~ sex, scales = "free_y") + # Free y-scale for abs vs. perc.
      ggplot2::labs(title = "Aggregate Emigration Comparison: Differences (Setup 2 - Setup 1)", y = "Difference Value", x = "Time") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })
  # TODO: Implement similar logic for Aggregate Emigration Differences and Net Migration Differences if required by UI.
  ##############################################################################


  ##############################################################################
  ################################ saSetupRun Tab ##############################
  ##############################################################################
  # This section handles the setup and execution of Sensitivity Analysis (SA).

  # --- UI Renderers for Sensitivity Analysis Setup ---
  # Dropdown to select the base system model setup for SA.
  output$sa_system_model_selector_ui <- renderUI({
    shiny::selectInput("sa_selected_sys_model_setup", "Select Base System Model Setup for SA:",
      choices = names(sysmod_list_list()),
      selected = if (length(names(sysmod_list_list())) > 0) names(sysmod_list_list())[1] else NULL
    )
  })

  # Checkbox group to select base data models for SA.
  output$sa_data_model_selector_ui <- renderUI({
    shiny::checkboxGroupInput("sa_selected_data_models_sens", "Select Base Data Models for SA:",
      choices = names(datamod_list())
    )
  })

  # Observer to update the 'sa_datamod_name_select' dropdown.
  # This dropdown allows choosing *which specific data model* (from those selected in checkboxes)
  # will have its parameter varied, if a data model parameter is chosen for SA.
  observe({
    req(input$sa_selected_data_models_sens) # Requires some data models to be checked.
    updateSelectInput(session, "sa_datamod_name_select",
      choices = c("None", input$sa_selected_data_models_sens), # "None" + names of checked data models.
      selected = "None"
    )
  })

  # Observer to update the 'sa_datamod_param_select' dropdown.
  # This lists parameters available for variation based on the *type* of the
  # data model selected in 'sa_datamod_name_select'.
  observe({
    req(input$sa_datamod_name_select == "None")
    updateSelectInput(session, "sa_datamod_param_select", choices = c("None"), selected = "None")
  })

  observe({
    req(input$sa_datamod_name_select, input$sa_datamod_name_select != "None")
    selected_dm_object <- datamod_list()[[input$sa_datamod_name_select]] # Get the actual data model object.
    req(selected_dm_object) # Ensure it exists.

    # Determine parameter choices based on the class of the selected data model.
    # The class names (e.g., "dpmaccount_datamod_exact") are assumed conventions.
    param_choices <- c("None") # Default.
    dm_class <- class(selected_dm_object)[[1]]

    if (dm_class == "dpmaccount_datamod_exact" || dm_class == "dpmaccount_datamod_poisson") {
      param_choices <- c("None", "Count Scaler" = "count_scaler", "Scale Ratio" = "scale_ratio", "Coverage Ratio" = "ratio")
    } else if (dm_class == "dpmaccount_datamod_norm" || dm_class == "dpmaccount_datamod_lognorm") {
      param_choices <- c("None", "Count Scaler" = "count_scaler", "Scale Ratio" = "scale_ratio", "Minimum SD" = "min_sd", "SD Scaler" = "sd_scaler", "Coverage Ratio" = "ratio", "Constant SD" = "sd_overide")
    } else if (dm_class == "dpmaccount_datamod_t") {
      param_choices <- c("None", "Count Scaler" = "count_scaler", "Scale Ratio" = "scale_ratio", "Coverage Ratio" = "ratio") # Potentially 'scale_df' or its components if applicable.
    } else if (dm_class == "dpmaccount_datamod_nbinom") {
      param_choices <- c("None", "Count Scaler" = "count_scaler", "Scale Ratio" = "scale_ratio", "Dispersion Scaler/Value" = "disp", "Coverage Ratio" = "ratio")
    }
    updateSelectInput(session, "sa_datamod_param_select", choices = param_choices, selected = "None")
  })

  sa_sysmod_param_select <- reactive({
    # req(input$sa_rate_select != "None", input$sa_modification_select != "None")
    if (input$sa_rate_select != "None" && input$sa_modification_select != "None"){
    rate <- input$sa_rate_select
    modification <- input$sa_modification_select

    if (rate == "None" || modification == "None") {
      return("None")
    } else {
      return(paste0(rate, "_", modification))
    }
    } else {
      return("None")
    }
  })

  # UI for specifying the range (min, max, steps) for the selected system model parameter.
  output$sa_sysmod_param_range_ui <- renderUI({
    req(sa_sysmod_param_select(), sa_sysmod_param_select() != "None") # Requires a sysmod parameter to be selected.

    # Set default range values based on the type of parameter.
    default_min <- 0
    default_max <- 1
    default_steps <- 3
    step_val <- 0.1
    param_label_prefix <- tools::toTitleCase(gsub("_", " ", sa_sysmod_param_select())) # Human-readable label.

    if (grepl("lower_rate_limit", sa_sysmod_param_select())) {
      default_min <- 1e-7
      default_max <- 1e-4
      default_steps <- 4
      step_val <- 1e-7
    } else if (grepl("rate_scale", sa_sysmod_param_select())) {
      default_min <- 0.5
      default_max <- 1.5
      default_steps <- 3
      step_val <- 0.1
    } else if (grepl("rate_noise", sa_sysmod_param_select())) {
      default_min <- 0
      default_max <- 0.1
      default_steps <- 3
      step_val <- 0.01
    } else if (grepl("dispersion", sa_sysmod_param_select())) {
      default_min <- 0.01
      default_max <- 0.15
      default_steps <- 4
      step_val <- 0.01 # Ensure dispersion is positive.
    }

    tagList(
      numericInput("sa_sys_param_min", paste(param_label_prefix, "Min:"), value = default_min, step = step_val),
      numericInput("sa_sys_param_max", paste(param_label_prefix, "Max:"), value = default_max, step = step_val),
      numericInput("sa_sys_param_steps", "Number of Steps (values to test):", value = default_steps, min = 2, step = 1)
    )
  })

  # UI for specifying the range for the selected data model parameter.
  output$sa_datamod_param_range_ui <- renderUI({
    # Requires a data model, its parameter, and that "None" isn't selected for these.
    req(
      input$sa_datamod_param_select, input$sa_datamod_param_select != "None",
      input$sa_datamod_name_select, input$sa_datamod_name_select != "None"
    )

    default_min <- 0
    default_max <- 1
    default_steps <- 3
    step_val <- 0.1
    param_label_prefix <- tools::toTitleCase(gsub("_", " ", input$sa_datamod_param_select))

    # Set default ranges based on common data model parameters.
    if (input$sa_datamod_param_select == "scale_ratio") {
      default_min <- 0
      default_max <- 0.5
      step_val <- 0.05
    } else if (input$sa_datamod_param_select %in% c("sd_scaler", "count_scaler")) {
      default_min <- 0.5
      default_max <- 1.5
      default_steps <- 3
      step_val <- 0.1
    } else if (input$sa_datamod_param_select == "min_sd") {
      default_min <- 0
      default_max <- 5
      default_steps <- 3
      step_val <- 0.5 # Example: min SD for population counts.
    } else if (input$sa_datamod_param_select == "ratio") { # Coverage ratio.
      default_min <- 0.9
      default_max <- 1.1
      default_steps <- 3
      step_val <- 0.05
    } else if (input$sa_datamod_param_select == "sd_overide") { # Fixed SD value.
      default_min <- 1
      default_max <- 10
      default_steps <- 3
      step_val <- 1 # Example range for fixed SD.
    } else if (input$sa_datamod_param_select == "disp") { # Dispersion for N-Binom.
      default_min <- 0.05
      default_max <- 0.5
      default_steps <- 5
      step_val <- 0.05
    }

    tagList(
      numericInput("sa_data_param_min_val", paste(param_label_prefix, "Min:"), value = default_min, step = step_val),
      numericInput("sa_data_param_max_val", paste(param_label_prefix, "Max:"), value = default_max, step = step_val),
      numericInput("sa_data_param_steps_val", "Number of Steps:", value = default_steps, min = 2, step = 1)
    )
  })

  # --- Observer for running the sensitivity analysis ---
  observeEvent(input$run_sensitivity_analysis, {
    # Essential requirements: global config, selected base system models, and base data models.
    req(
      global_config()$output_dir, global_config()$seed_value,
      input$sa_selected_sys_model_setup, input$sa_selected_data_models_sens
    )

    # Validate that at least one parameter (either system or data model) is selected for variation.
    if (sa_sysmod_param_select() == "None" && input$sa_datamod_param_select == "None") {
      shinyWidgets::sendSweetAlert(session, title = "No Parameter Selected", text = "Please select a system or data model parameter to vary for the sensitivity analysis.", type = "warning")
      return()
    }
    # If a data model parameter is selected, ensure a target data model is also selected.
    if (input$sa_datamod_param_select != "None" && input$sa_datamod_name_select == "None") {
      shinyWidgets::sendSweetAlert(session, title = "No Data Model Target", text = "A data model parameter is selected to vary, but no specific data model has been chosen as the target. Please select a target data model.", type = "warning")
      return()
    }

    # Get the original base system and data models. These will be duplicated and modified in each iteration.
    base_sys_model_setup_name <- input$sa_selected_sys_model_setup
    base_sys_models_orig <- sysmod_list_list()[[base_sys_model_setup_name]]
    base_data_models_orig <- datamod_list()[input$sa_selected_data_models_sens]

    # Initialize variables to store information about the parameter being varied and its values.
    param_values_to_test <- NULL
    varied_param_type <- NULL # "sys" or "data"
    varied_param_name_full_id <- NULL # e.g., "births_dispersion" or "pop_data_model$sd_scaler"
    sys_model_component_to_vary <- NULL # e.g., "births", "deaths" (if type is "sys")
    data_model_to_vary_name <- NULL # Name of the data model component (if type is "data")
    varied_param_name_short_id <- NULL # e.g., "dispersion", "sd_scaler"

    # Determine which parameter is being varied and get its range of values.
    if (sa_sysmod_param_select() != "None" && input$sa_datamod_param_select == "None") { # A system model parameter is selected.
      req(input$sa_sys_param_min, input$sa_sys_param_max, input$sa_sys_param_steps)
      if (input$sa_sys_param_min >= input$sa_sys_param_max || input$sa_sys_param_steps < 2) {
        shinyWidgets::sendSweetAlert(session, title = "Invalid Range", text = "System model parameter range (min >= max) or number of steps (< 2) is invalid.", type = "error")
        return()
      }
      param_values_to_test <- seq(from = input$sa_sys_param_min, to = input$sa_sys_param_max, length.out = as.integer(input$sa_sys_param_steps))
      varied_param_type <- "sys"
      # Parse the system model parameter string (e.g., "births_dispersion")
      split_param <- unlist(strsplit(sa_sysmod_param_select(), "_", fixed = TRUE))
      sys_model_component_to_vary <- split_param[1] # e.g., "births"
      varied_param_name_short_id <- paste(split_param[-1], collapse = "_") # e.g., "dispersion" or "lower_rate_limit"
      varied_param_name_full_id <- sa_sysmod_param_select()
      # Store info about the varied parameter for use in plotting.
      sensitivity_varied_param_info(list(name = varied_param_name_full_id, type = "System Model", component = sys_model_component_to_vary, short_name = varied_param_name_short_id))
    } else if (sa_sysmod_param_select() == "None" && input$sa_datamod_param_select != "None") { # A data model parameter is selected.
      req(input$sa_data_param_min_val, input$sa_data_param_max_val, input$sa_data_param_steps_val, input$sa_datamod_name_select)
      if (input$sa_data_param_min_val >= input$sa_data_param_max_val || input$sa_data_param_steps_val < 2) {
        shinyWidgets::sendSweetAlert(session, title = "Invalid Range", text = "Data model parameter range or steps are invalid.", type = "error")
        return()
      }
      param_values_to_test <- seq(from = input$sa_data_param_min_val, to = input$sa_data_param_max_val, length.out = as.integer(input$sa_data_param_steps_val))
      varied_param_type <- "data"
      data_model_to_vary_name <- input$sa_datamod_name_select
      varied_param_name_short_id <- input$sa_datamod_param_select # e.g., "sd_scaler"
      varied_param_name_full_id <- paste0(data_model_to_vary_name, "$", varied_param_name_short_id) # For clearer identification.
      sensitivity_varied_param_info(list(name = varied_param_name_full_id, type = "Data Model", component = data_model_to_vary_name, short_name = varied_param_name_short_id))
    }

    if (is.null(param_values_to_test)) { # Should not happen if logic above is correct.
      shinyWidgets::sendSweetAlert(session, title = "Setup Error", text = "Parameter values for sensitivity analysis could not be determined.", type = "error")
      return()
    }

    output$sensitivity_run_status <- renderText("Running sensitivity analysis... please wait.")
    results_collector_list <- list() # Re-initialize list to store results from each iteration.

    # Loop through each value of the parameter being varied.
    shiny::withProgress(message = "Running Sensitivity Analysis", value = 0, {
      n_iterations <- length(param_values_to_test)
      for (i in seq_along(param_values_to_test)) {
        current_param_value <- param_values_to_test[i]
        # Update progress bar.
        incProgress(1 / n_iterations, detail = paste("Iteration", i, "of", n_iterations, ": Testing", sensitivity_varied_param_info()$short_name, "=", signif(current_param_value, 4)))

        # Deep copy the base models to avoid modifying them in place across iterations.
        # `rlang::duplicate` with `shallow = FALSE` attempts a deep copy.
        current_iter_sys_models <- rlang::duplicate(base_sys_models_orig, shallow = FALSE)
        current_iter_data_models <- rlang::duplicate(base_data_models_orig, shallow = FALSE)

        # --- Modify the selected parameter in the copied models ---
        if (varied_param_type == "sys") {
          target_sm_component_obj <- current_iter_sys_models[[sys_model_component_to_vary]]
          if (is.null(target_sm_component_obj)) {
            warning(paste("System model component", sys_model_component_to_vary, "not found in SA loop for iter", i))
            next # Skip this iteration.
          }
          # Modify the specific part of the system model component.
          # This assumes system model components have 'mean' (data frame with rate column) and 'disp' (scalar or data frame).
          if (varied_param_name_short_id == "lower_rate_limit") {
            if ("mean" %in% names(target_sm_component_obj) && (is.data.frame(target_sm_component_obj$mean) || inherits(target_sm_component_obj$mean, "Counts"))) {
              rates_data <- as.data.frame(target_sm_component_obj$mean)
              rate_col_name <- if ("rate" %in% names(rates_data)) "rate" else if ("mean" %in% names(rates_data)) "mean" else NULL
              if (!is.null(rate_col_name)) rates_data[[rate_col_name]] <- pmax(rates_data[[rate_col_name]], current_param_value, na.rm = TRUE) else warning("Rate column not found for lower_rate_limit.")
              target_sm_component_obj$mean <- rates_data # Or convert back to Counts if necessary.
            } else {
              warning(paste("Cannot apply lower_rate_limit to mean of", sys_model_component_to_vary))
            }
          } else if (varied_param_name_short_id == "rate_scale") {
            if ("mean" %in% names(target_sm_component_obj) && (is.data.frame(target_sm_component_obj$mean) || inherits(target_sm_component_obj$mean, "Counts"))) {
              rates_data <- as.data.frame(target_sm_component_obj$mean)
              rate_col_name <- if ("rate" %in% names(rates_data)) "rate" else if ("mean" %in% names(rates_data)) "mean" else NULL
              # if (!is.null(rate_col_name)) rates_data[[rate_col_name]] <- rates_data[[rate_col_name]] * current_param_value else warning("Rate column not found for rate_scale.")
              if (!is.null(rate_col_name)) {
                if (input$sa_time_target == "all") {
                  if ("sex" %in% names(rates_data)) {
                    rates_data <- rates_data %>% mutate(across(all_of(rate_col_name), ~ ifelse(age %in% parse_values(input$sa_age_target) & sex %in% parse_values(input$sa_sex_target), .x * current_param_value, .x)))
                  } else {
                    rates_data <- rates_data %>% mutate(across(all_of(rate_col_name), ~ ifelse(age %in% parse_values(input$sa_age_target), .x * current_param_value, .x)))
                  }
                } else {
                  if ("sex" %in% names(rates_data)) {
                    rates_data <- rates_data %>% mutate(across(all_of(rate_col_name), ~ ifelse(age %in% parse_values(input$sa_age_target) & sex %in% parse_values(input$sa_sex_target) & time %in% parse_values(input$sa_time_target), .x * current_param_value, .x)))
                  } else {
                    rates_data <- rates_data %>% mutate(across(all_of(rate_col_name), ~ ifelse(age %in% parse_values(input$sa_age_target) & time %in% parse_values(input$sa_time_target), .x * current_param_value, .x)))
                  }
                }
              }
              target_sm_component_obj$mean <- rates_data
            } else {
              warning(paste("Cannot apply rate_scale to mean of", sys_model_component_to_vary))
            }
          } else if (varied_param_name_short_id == "rate_noise") {
            if ("mean" %in% names(target_sm_component_obj) && (is.data.frame(target_sm_component_obj$mean) || inherits(target_sm_component_obj$mean, "Counts"))) {
              rates_data <- as.data.frame(target_sm_component_obj$mean)
              rate_col_name <- if ("rate" %in% names(rates_data)) "rate" else if ("mean" %in% names(rates_data)) "mean" else NULL
              # if (!is.null(rate_col_name)) rates_data[[rate_col_name]] <- rates_data[[rate_col_name]] + (stats::rnorm(length(rates_data[[rate_col_name]]), mean = 0, sd = current_param_value) * rates_data[[rate_col_name]]) else warning("Rate column not found for rate_noise.")
              if (!is.null(rate_col_name)) {
                if (input$sa_time_target == "all") {
                  if ("sex" %in% names(rates_data)) {
                    rates_data <- rates_data %>% mutate(across(all_of(rate_col_name), ~ ifelse(age %in% parse_values(input$sa_age_target) & sex %in% parse_values(input$sa_sex_target), .x + (stats::rnorm(length(.x), mean = 0, sd = current_param_value) * .x), .x)))
                  } else {
                    rates_data <- rates_data %>% mutate(across(all_of(rate_col_name), ~ ifelse(age %in% parse_values(input$sa_age_target), .x + (stats::rnorm(length(.x), mean = 0, sd = current_param_value) * .x), .x)))
                  }
                } else {
                  if ("sex" %in% names(rates_data)) {
                    rates_data <- rates_data %>% mutate(across(all_of(rate_col_name), ~ ifelse(age %in% parse_values(input$sa_age_target) & sex %in% parse_values(input$sa_sex_target) & time %in% parse_values(input$sa_time_target), .x + (stats::rnorm(length(.x), mean = 0, sd = current_param_value) * .x), .x)))
                  } else {
                    rates_data <- rates_data %>% mutate(across(all_of(rate_col_name), ~ ifelse(age %in% parse_values(input$sa_age_target) & time %in% parse_values(input$sa_time_target), .x + (stats::rnorm(length(.x), mean = 0, sd = current_param_value) * .x), .x)))
                  }
                }
              }
              target_sm_component_obj$mean <- rates_data
            } else {
              warning(paste("Cannot apply rate_noise to mean of", sys_model_component_to_vary))
            }
          } else if (varied_param_name_short_id == "dispersion") {
            single_disp <- target_sm_component_obj$disp # Assuming disp is a direct scalar element.
            rates_data <- as.data.frame(target_sm_component_obj$mean)
            rate_col_name <- if ("rate" %in% names(rates_data)) "rate" else if ("mean" %in% names(rates_data)) "mean" else NULL
            if (!is.null(rate_col_name)) {
              if (input$sa_time_target == "all") {
                if ("sex" %in% names(rates_data)) {
                  rates_data <- rates_data %>%
                    mutate(across(all_of(rate_col_name), ~ ifelse(age %in% parse_values(input$sa_age_target) & sex %in% parse_values(input$sa_sex_target), current_param_value, single_disp))) %>%
                    rename(disp = rate_col_name)
                  print(head(rates_data))
                } else {
                  # rates_data <- rates_data %>%
                  #   mutate(across(all_of(rate_col_name), ~ ifelse(age %in% parse_values(input$sa_age_target), current_param_value, single_disp))) %>%
                  #   rename(disp = rate_col_name)
                  rates_data <- current_param_value
                }
              } else {
                if ("sex" %in% names(rates_data)) {
                  rates_data <- rates_data %>%
                    mutate(across(all_of(rate_col_name), ~ ifelse(age %in% parse_values(input$sa_age_target) & sex %in% parse_values(input$sa_sex_target) & time %in% parse_values(input$sa_time_target), current_param_value, single_disp))) %>%
                    rename(disp = rate_col_name)
                } else {
                  # rates_data <- rates_data %>%
                  #   mutate(across(all_of(rate_col_name), ~ ifelse(age %in% parse_values(input$sa_age_target) & time %in% parse_values(input$sa_time_target), current_param_value, single_disp))) %>%
                  #   rename(disp = rate_col_name)
                  rates_data <- current_param_value
                }
              }
            }
            target_sm_component_obj$disp <- rates_data
          }
          current_iter_sys_models[[sys_model_component_to_vary]] <- target_sm_component_obj # Put modified component back.
        } else if (varied_param_type == "data") {
          target_dm_obj <- current_iter_data_models[[data_model_to_vary_name]]
          if (is.null(target_dm_obj)) {
            warning(paste("Data model", data_model_to_vary_name, "not found in SA loop for iter", i))
            next
          }
          # Modify the specific part of the data model object.
          # This requires knowledge of the structure of data model objects (e.g., elements like 'scale_ratio', 'sd', 'data$count').
          # The exact modification depends on 'varied_param_name_short_id'.
          if (varied_param_name_short_id == "scale_ratio") {
            target_dm_obj$scale_ratio <- current_param_value
          } else if (varied_param_name_short_id == "min_sd" && "sd" %in% names(target_dm_obj) && is.data.frame(target_dm_obj$sd)) {
            target_dm_obj$sd <- target_dm_obj$sd %>% dplyr::mutate(sd = ifelse(.data$sd < current_param_value, current_param_value, .data$sd))
          } else if (varied_param_name_short_id == "sd_scaler" && "sd" %in% names(target_dm_obj) && is.data.frame(target_dm_obj$sd)) {
            # target_dm_obj$sd <- target_dm_obj$sd %>% dplyr::mutate(sd = .data$sd * current_param_value)
            sd_data <- as.data.frame(target_dm_obj$sd)
            if (input$sa_time_target == "all") {
              sd_data <- sd_data %>% mutate(across(all_of("sd"), ~ ifelse(age %in% parse_values(input$sa_age_target) & sex %in% parse_values(input$sa_sex_target), .x * current_param_value, .x)))
            } else {
              sd_data <- sd_data %>% mutate(across(all_of("sd"), ~ ifelse(age %in% parse_values(input$sa_age_target) & sex %in% parse_values(input$sa_sex_target) & time %in% parse_values(input$sa_time_target), .x * current_param_value, .x)))
            }
            target_dm_obj$sd <- sd_data
          } else if (varied_param_name_short_id == "sd_overide" && "sd" %in% names(target_dm_obj) && is.data.frame(target_dm_obj$sd)) {
            # target_dm_obj$sd <- target_dm_obj$sd %>% dplyr::mutate(sd = current_param_value)
            sd_data <- as.data.frame(target_dm_obj$sd)
            if (input$sa_time_target == "all") {
              sd_data <- sd_data %>% mutate(across(all_of("sd"), ~ ifelse(age %in% parse_values(input$sa_age_target) & sex %in% parse_values(input$sa_sex_target), current_param_value, .x)))
            } else {
              sd_data <- sd_data %>% mutate(across(all_of("sd"), ~ ifelse(age %in% parse_values(input$sa_age_target) & sex %in% parse_values(input$sa_sex_target) & time %in% parse_values(input$sa_time_target), current_param_value, .x)))
            }
            target_dm_obj$sd <- sd_data
          } else if (varied_param_name_short_id == "count_scaler" && "data" %in% names(target_dm_obj) && is.data.frame(target_dm_obj$data)) {
            # target_dm_obj$data <- target_dm_obj$data %>% dplyr::mutate(count = .data$count * current_param_value)
            count_data <- as.data.frame(target_dm_obj$data)
            if (input$sa_time_target == "all") {
              count_data <- count_data %>% mutate(across(all_of("count"), ~ ifelse(age %in% parse_values(input$sa_age_target) & sex %in% parse_values(input$sa_sex_target), .x * current_param_value, .x)))
            } else {
              count_data <- count_data %>% mutate(across(all_of("count"), ~ ifelse(age %in% parse_values(input$sa_age_target) & sex %in% parse_values(input$sa_sex_target) & time %in% parse_values(input$sa_time_target), .x * current_param_value, .x)))
            }
            target_dm_obj$data <- count_data
          } else if (varied_param_name_short_id == "ratio") { # For coverage ratio
            if ("ratio" %in% names(target_dm_obj)) {
              if (is.data.frame(target_dm_obj$ratio)) {
                # target_dm_obj$ratio <- target_dm_obj$ratio %>% dplyr::mutate(ratio = current_param_value)
                ratio_data <- as.data.frame(target_dm_obj$ratio)
                if (input$sa_time_target == "all") {
                  ratio_data <- ratio_data %>% mutate(across(all_of("ratio"), ~ ifelse(age %in% parse_values(input$sa_age_target) & sex %in% parse_values(input$sa_sex_target), current_param_value, .x)))
                } else {
                  ratio_data <- ratio_data %>% mutate(across(all_of("ratio"), ~ ifelse(age %in% parse_values(input$sa_age_target) & sex %in% parse_values(input$sa_sex_target) & time %in% parse_values(input$sa_time_target), current_param_value, .x)))
                }
                target_dm_obj$ratio <- ratio_data
              } else if (is.numeric(target_dm_obj$ratio)) target_dm_obj$ratio <- current_param_value # If scalar
            } else {
              warning("Data model does not have a 'ratio' component to modify.")
            }
          } else if (varied_param_name_short_id == "disp" && "disp" %in% names(target_dm_obj)) { # For N-Binom dispersion
            target_dm_obj$disp <- current_param_value # Assuming 'disp' is scalar or can be replaced by scalar.
          } else {
            warning(paste("Parameter '", varied_param_name_short_id, "' handling not implemented for data model '", data_model_to_vary_name, "' of class ", class(target_dm_obj)[1], ". Skipping modification for this iteration.", sep = ""))
          }
          current_iter_data_models[[data_model_to_vary_name]] <- target_dm_obj # Put modified data model back.
        }

        # --- Run the model with the modified parameters ---
        run_result_object <- NULL # Initialize.
        tryCatch(
          { # Estimate account with the modified models.
            run_result_object <- dpmaccount::estimate_account(
              datamods = current_iter_data_models,
              sysmods = current_iter_sys_models,
              seed_in = global_config()$seed_value
            )
          },
          error = function(e) { # Log error if estimation fails for this iteration.
            shiny::showNotification(paste("Sensitivity Analysis Error: Iteration", i, "failed during estimation -", e$message), type = "error", duration = 10)
            run_result_object <<- NULL # Ensure it's NULL on error.
          }
        )

        # --- Process and store results if estimation was successful ---
        if (!is.null(run_result_object)) {
          # Augment population and migration results.
          pop_res_df_iter <- tryCatch(dpmaccount::augment_population(run_result_object, collapse = "cohort"), error = function(e) NULL)
          mig_res_df_iter <- tryCatch(dpmaccount::augment_events(run_result_object, collapse = "age") %>% dplyr::mutate(age = .data$time - .data$cohort) %>% dplyr::select(-any_of("cohort")), error = function(e) NULL)

          param_info_current_iter <- sensitivity_varied_param_info() # Get the name of the varied parameter.
          # Add the current parameter value and run ID to the results dataframes.
          if (!is.null(pop_res_df_iter)) {
            pop_res_df_iter[[param_info_current_iter$name]] <- current_param_value # Add column with varied parameter value.
            pop_res_df_iter$run_id_sa <- i # Add run identifier.
            results_collector_list[[paste0("pop_run_", i)]] <- pop_res_df_iter # Store population results.
          }
          if (!is.null(mig_res_df_iter)) {
            mig_res_df_iter[[param_info_current_iter$name]] <- current_param_value
            mig_res_df_iter$run_id_sa <- i
            results_collector_list[[paste0("mig_run_", i)]] <- mig_res_df_iter # Store migration results.
          }
        }
      } # End of loop over parameter values.
    }) # End withProgress.

    sensitivity_results_list(results_collector_list) # Store all collected results from all iterations.
    output$sensitivity_run_status <- renderText(paste("Sensitivity analysis complete. Processed", length(results_collector_list) / 2, "successful iterations out of", length(param_values_to_test), "total parameter values tested."))

    # Update UI selectors for the sensitivity results plots.
    param_info_final_run <- sensitivity_varied_param_info()
    if (!is.null(param_info_final_run)) {
      # Update parameter display choices for plots.
      updateSelectInput(session, "sa_pop_plot_param_display", choices = param_info_final_run$name, selected = param_info_final_run$name)
      updateSelectInput(session, "sa_mig_plot_param_display", choices = param_info_final_run$name, selected = param_info_final_run$name)

      # Find the first available population result to get time choices.
      first_pop_res_sa <- NULL
      iter_names_sa <- names(results_collector_list)
      for (res_iter_name_sa in iter_names_sa) {
        if (startsWith(res_iter_name_sa, "pop_run_")) {
          first_pop_res_sa <- results_collector_list[[res_iter_name_sa]]
          break
        }
      }
      # Update time selectors for SA plots.
      if (!is.null(first_pop_res_sa) && "time" %in% colnames(first_pop_res_sa)) {
        unique_times_sa_pop <- sort(unique(first_pop_res_sa$time))
        unique_times_sa_mig <- unique_times_sa_pop[unique_times_sa_pop != min(unique_times_sa_pop, na.rm = TRUE)] # Exclude first time for migration.
        updateSelectInput(session, "sa_pop_time_select", choices = unique_times_sa_pop, selected = if (length(unique_times_sa_pop) > 0) unique_times_sa_pop[1] else NULL)
        updateSelectInput(session, "sa_mig_time_select", choices = unique_times_sa_mig, selected = if (length(unique_times_sa_mig) > 0) unique_times_sa_mig[1] else NULL)
      } else { # Fallback if no valid time data.
        updateSelectInput(session, "sa_pop_time_select", choices = c("NA"), selected = "NA")
        updateSelectInput(session, "sa_mig_time_select", choices = c("NA"), selected = "NA")
      }
    }
  }) # End observeEvent run_sensitivity_analysis.

  # --- Reactive expressions to combine SA results for plotting ---
  # Combines all population results from the sensitivity analysis list into a single dataframe.
  sa_combined_pop_results <- reactive({
    res_list_val <- sensitivity_results_list()
    if (length(res_list_val) == 0) {
      return(NULL)
    }
    # Filter for list elements starting with "pop_run_" and bind them.
    pop_dfs_list_sa <- Filter(Negate(is.null), lapply(names(res_list_val), function(n) if (startsWith(n, "pop_run_")) res_list_val[[n]] else NULL))
    if (length(pop_dfs_list_sa) == 0) {
      return(NULL)
    }
    dplyr::bind_rows(pop_dfs_list_sa)
  })

  # Combines all migration results.
  sa_combined_mig_results <- reactive({
    res_list_val <- sensitivity_results_list()
    if (length(res_list_val) == 0) {
      return(NULL)
    }
    mig_dfs_list_sa <- Filter(Negate(is.null), lapply(names(res_list_val), function(n) if (startsWith(n, "mig_run_")) res_list_val[[n]] else NULL))
    if (length(mig_dfs_list_sa) == 0) {
      return(NULL)
    }
    dplyr::bind_rows(mig_dfs_list_sa)
  })

  # Aggregate Comparison Plots (e.g., total population over time for each setup)
  output$SAcompAggPop <- shiny::renderUI({
    plotly::plotlyOutput("sa_comparing_agg_pop_plot")
  })
  output$sa_comparing_agg_pop_plot <- plotly::renderPlotly({
    req(sa_combined_pop_results())
    plot_data_sa_pop <- sa_combined_pop_results()
    # Aggregate total population by time, sex, and setup.
    varied_param_col_name <- sensitivity_varied_param_info()$name # Get the column name of the varied parameter.
    plot_data_sa_pop[[varied_param_col_name]] <- factor(round(plot_data_sa_pop[[varied_param_col_name]], 5))

    input_cols <- setdiff(colnames(plot_data_sa_pop), c("age", "sex", "time", "population", "population.fitted", "population.upper", "population.lower", "run_id_sa", varied_param_col_name))
    plot_data_sa_pop_inputs_long <- plot_data_sa_pop %>%
      tidyr::pivot_longer(cols = input_cols, names_to = "count_type", values_to = "count_value")
    inputs_agg <- plot_data_sa_pop_inputs_long %>%
      dplyr::group_by(.data$time, .data$sex, .data[[varied_param_col_name]], .data$count_type) %>%
      dplyr::summarise(total_count = sum(count_value, na.rm = TRUE), .groups = "drop")

    pop_aggs <- plot_data_sa_pop %>%
      dplyr::group_by(.data$time, .data$sex, .data[[varied_param_col_name]]) %>%
      dplyr::summarise(agg_pop = sum(.data$population.fitted, na.rm = TRUE), .groups = "drop")
    p <- ggplot2::ggplot(pop_aggs, ggplot2::aes(x = .data$time, y = .data$agg_pop, color = .data[[varied_param_col_name]])) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_point(data = inputs_agg %>% filter(.data$total_count != 0), aes(x = .data$time, y = .data$total_count, shape = .data$count_type), size = 2) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = "Aggregate Population Comparison Over Time", y = "Total Population", x = "Time") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })


  # Aggregate Comparison Plots (e.g., total immigration over time for each setup)
  output$SAcompAggImmig <- shiny::renderUI({
    plotly::plotlyOutput("sa_comparing_agg_immig_plot")
  })
  output$sa_comparing_agg_immig_plot <- plotly::renderPlotly({
    req(sa_combined_mig_results())
    plot_data_sa_mig <- sa_combined_mig_results()
    # Aggregate total immigration by time, sex, and setup.
    varied_param_col_name <- sensitivity_varied_param_info()$name # Get the column name of the varied parameter.
    plot_data_sa_mig[[varied_param_col_name]] <- factor(round(plot_data_sa_mig[[varied_param_col_name]], 5))

    ins_aggs <- plot_data_sa_mig %>%
      dplyr::group_by(.data$time, .data$sex, .data[[varied_param_col_name]]) %>%
      dplyr::summarise(agg_ins = sum(.data$ins.fitted, na.rm = TRUE), .groups = "drop")
    p <- ggplot2::ggplot(ins_aggs, ggplot2::aes(x = .data$time, y = .data$agg_ins, color = .data[[varied_param_col_name]])) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = "Aggregate Immigration Comparison Over Time", y = "Total Immigration", x = "Time") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })

  # Aggregate Comparison Plots (e.g., total immigration over time for each setup)
  output$SAcompAggEmig <- shiny::renderUI({
    plotly::plotlyOutput("sa_comparing_agg_emig_plot")
  })
  output$sa_comparing_agg_emig_plot <- plotly::renderPlotly({
    req(sa_combined_mig_results())
    plot_data_sa_mig <- sa_combined_mig_results()
    # Aggregate total immigration by time, sex, and setup.
    varied_param_col_name <- sensitivity_varied_param_info()$name # Get the column name of the varied parameter.
    plot_data_sa_mig[[varied_param_col_name]] <- factor(round(plot_data_sa_mig[[varied_param_col_name]], 5))

    outs_aggs <- plot_data_sa_mig %>%
      dplyr::group_by(.data$time, .data$sex, .data[[varied_param_col_name]]) %>%
      dplyr::summarise(agg_outs = sum(.data$outs.fitted, na.rm = TRUE), .groups = "drop")
    p <- ggplot2::ggplot(outs_aggs, ggplot2::aes(x = .data$time, y = .data$agg_outs, color = .data[[varied_param_col_name]])) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = "Aggregate Emigration Comparison Over Time", y = "Total Emigration", x = "Time") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })

  # Aggregate Comparison Plots (e.g., total immigration over time for each setup)
  output$SAcompAggNetMig <- shiny::renderUI({
    plotly::plotlyOutput("sa_comparing_agg_netmig_plot")
  })
  output$sa_comparing_agg_netmig_plot <- plotly::renderPlotly({
    req(sa_combined_mig_results())
    plot_data_sa_mig <- sa_combined_mig_results()
    # Aggregate net migration by time, sex, and setup.
    varied_param_col_name <- sensitivity_varied_param_info()$name # Get the column name of the varied parameter.
    plot_data_sa_mig[[varied_param_col_name]] <- factor(round(plot_data_sa_mig[[varied_param_col_name]], 5))

    net_aggs <- plot_data_sa_mig %>%
      dplyr::group_by(.data$time, .data$sex, .data[[varied_param_col_name]]) %>%
      dplyr::summarise(agg_net = sum(.data$ins.fitted - .data$outs.fitted, na.rm = TRUE), .groups = "drop")
    p <- ggplot2::ggplot(net_aggs, ggplot2::aes(x = .data$time, y = .data$agg_net, color = .data[[varied_param_col_name]])) +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2) +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$sex)) +
      ggplot2::labs(title = "Aggregate Net Migration Comparison Over Time", y = "Net Migration (Ins - Outs)", x = "Time") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p)
  })

  # --- Plotting Sensitivity Analysis Results ---
  # Plot for population estimates from SA.
  output$sa_population_plot_output <- plotly::renderPlotly({
    # Requires combined results, a selected time, and info about the varied parameter.
    req(sa_combined_pop_results(), input$sa_pop_time_select != "NA", sensitivity_varied_param_info())
    plot_data_sa_pop <- sa_combined_pop_results() %>% dplyr::filter(.data$time == as.numeric(input$sa_pop_time_select)) # Filter by selected time.
    varied_param_col_name <- sensitivity_varied_param_info()$name # Get the column name of the varied parameter.

    if (!varied_param_col_name %in% colnames(plot_data_sa_pop)) { # Check if column exists.
      shiny::showNotification(paste("Varied parameter column '", varied_param_col_name, "' not found in SA population results."), type = "error")
      return(NULL)
    }
    # Convert the varied parameter column to a factor for discrete coloring/grouping in ggplot.
    # Rounding helps group similar floating point values that might arise from seq().
    plot_data_sa_pop[[varied_param_col_name]] <- factor(round(plot_data_sa_pop[[varied_param_col_name]], 5))

    p_sa_pop <- ggplot2::ggplot(plot_data_sa_pop, ggplot2::aes(
      x = .data$age, y = .data$population.fitted,
      color = .data[[varied_param_col_name]], group = .data[[varied_param_col_name]]
    )) +
      ggplot2::geom_line(alpha = 0.8) + # Lines for fitted values.
      # Optional: Add ribbons for confidence intervals if they are not too cluttered.
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$population.lower, ymax = .data$population.upper, fill = .data[[varied_param_col_name]]), alpha = 0.1, linetype = "blank") +
      ggplot2::facet_wrap(~sex, scales = "free_y") + # Facet by sex.
      ggplot2::labs(
        title = paste("Population Sensitivity by", sensitivity_varied_param_info()$short_name, "@ Time", input$sa_pop_time_select),
        x = "Age", y = "Population Count", color = sensitivity_varied_param_info()$short_name, fill = sensitivity_varied_param_info()$short_name
      ) +
      ggplot2::theme_minimal() +
      ggplot2::guides(fill = "none") # Hide fill legend if ribbons are used and it's redundant.
    plotly::ggplotly(p_sa_pop)
  })

  # Plot for immigration estimates from SA.
  output$sa_immigration_plot_output <- plotly::renderPlotly({
    req(sa_combined_mig_results(), input$sa_mig_time_select != "NA", sensitivity_varied_param_info())
    plot_data_sa_mig <- sa_combined_mig_results() %>% dplyr::filter(.data$time == as.numeric(input$sa_mig_time_select))
    varied_param_col_name <- sensitivity_varied_param_info()$name
    if (!varied_param_col_name %in% colnames(plot_data_sa_mig)) {
      return(NULL)
    }
    plot_data_sa_mig[[varied_param_col_name]] <- factor(round(plot_data_sa_mig[[varied_param_col_name]], 5))

    p_sa_ins <- ggplot2::ggplot(plot_data_sa_mig, ggplot2::aes(
      x = .data$age, y = .data$ins.fitted,
      color = .data[[varied_param_col_name]], group = .data[[varied_param_col_name]]
    )) +
      ggplot2::geom_line(alpha = 0.8) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$ins.lower, ymax = .data$ins.upper, fill = .data[[varied_param_col_name]]), alpha = 0.1, linetype = "blank") +
      ggplot2::facet_wrap(~sex, scales = "free_y") +
      ggplot2::labs(
        title = paste("Immigration Sensitivity by", sensitivity_varied_param_info()$short_name, "@ Time", input$sa_mig_time_select),
        x = "Age", y = "Immigration Count", color = sensitivity_varied_param_info()$short_name
      ) +
      ggplot2::theme_minimal() +
      ggplot2::guides(fill = "none") # Hide fill legend if ribbons are used and it's redundant.
    plotly::ggplotly(p_sa_ins)
  })

  # Plot for emigration estimates from SA.
  output$sa_emigration_plot_output <- plotly::renderPlotly({
    req(sa_combined_mig_results(), input$sa_mig_time_select != "NA", sensitivity_varied_param_info())
    plot_data_sa_mig <- sa_combined_mig_results() %>% dplyr::filter(.data$time == as.numeric(input$sa_mig_time_select))
    varied_param_col_name <- sensitivity_varied_param_info()$name
    if (!varied_param_col_name %in% colnames(plot_data_sa_mig)) {
      return(NULL)
    }
    plot_data_sa_mig[[varied_param_col_name]] <- factor(round(plot_data_sa_mig[[varied_param_col_name]], 5))

    p_sa_outs <- ggplot2::ggplot(plot_data_sa_mig, ggplot2::aes(
      x = .data$age, y = .data$outs.fitted,
      color = .data[[varied_param_col_name]], group = .data[[varied_param_col_name]]
    )) +
      ggplot2::geom_line(alpha = 0.8) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$outs.lower, ymax = .data$outs.upper, fill = .data[[varied_param_col_name]]), alpha = 0.1, linetype = "blank") +
      ggplot2::facet_wrap(~sex, scales = "free_y") +
      ggplot2::labs(
        title = paste("Emigration Sensitivity by", sensitivity_varied_param_info()$short_name, "@ Time", input$sa_mig_time_select),
        x = "Age", y = "Emigration Count", color = sensitivity_varied_param_info()$short_name
      ) +
      ggplot2::theme_minimal() +
      ggplot2::guides(fill = "none") # Hide fill legend if ribbons are used and it's redundant.
    plotly::ggplotly(p_sa_outs)
  })
} # End of server function
