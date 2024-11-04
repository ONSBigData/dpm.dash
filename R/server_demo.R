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
server_dev <- function(input, output, session) {
  # Define the global reactive lists which the dashboard relies on
  datamod_list <- reactiveVal(list()) # All defined data models
  global_config <- reactiveVal(list()) # User-defined global config
  sysmod_list <- reactiveVal(list()) # All defined system models
  sysmod_list_list <- reactiveVal(list())
  selected_data_models <- reactiveVal(list()) # Checklist su

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
      seed_value = if (nzchar(input$global_seed_value)) input$global_seed_value else default_seed_value # Use default if empty
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
    req(global_config()$data_dir)
    models <- c("births", "deaths", "ins", "outs")
    new_sysmods <- lapply(models, function(model) {
      rates_file <- file.path(global_config()$data_dir, input[[paste0(model, "_rates_file")]])

      if (!file.exists(rates_file)) {
        file_error <<- TRUE # Set error flag to TRUE
        return(NULL) # Return NULL if file not found
      }

      rates_df <- utils::read.csv(rates_file)

      if (input[[paste0(model, "_disp_type")]] == "Single Value") {
        disp <- input[[paste0(model, "_disp_value")]]
      } else {
        disp_file <- file.path(global_config()$data_dir, input[[paste0(model, "_disp_file")]])
        if (!file.exists(disp_file)) {
          file_error <<- TRUE
          return(NULL)
        }
        disp <- utils::read.csv(disp_file)
      }

      lower_rates_limit <- input[[paste0(model, "_lower_rate_limit")]]

      rate_scaler <- input[[paste0(model, "_rate_scale")]]

      rate_overide <- input[[paste0(model, "_rate_overide")]]

      time_selection <- global_config()$time_selection

      create_system_model(model, rates_df, disp, time_selection, lower_rates_limit, rate_scaler, rate_overide)
    })

    if (file_error) { # Display a warning if any file errors occurred
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "File Error",
        text = "One or more files specified for the system models do not exist. Please check your file paths and try again.",
        type = "error"
      )
    }

    names(new_sysmods) <- models # Ensure the new models are named correctly
    sysmod_list(c(new_sysmods))

    currentList <- sysmod_list_list()
    newObjectList <- list()
    newObjectList[[input$sysmods_name]] <- sysmod_list()
    sysmod_list_list(c(currentList, newObjectList))

    # Update the dropdown choices when datamod_list changes
    observe({
      updateSelectInput(session, "delete_sysmod_list", choices = names(sysmod_list_list()))
    })

    # Observe event for deleting a data model
    shiny::observeEvent(input$delete_button_sysmod, {
      req(input$delete_sysmod_list)
      currentList <- sysmod_list_list()
      currentList[[input$delete_sysmod_list]] <- NULL
      sysmod_list_list(currentList)
    })

    output$loadedSystemModels <- renderUI({
      sysmod_list <- sysmod_list_list()
      if (length(sysmod_list) > 0) {
        model_names <- paste(names(sysmod_list), collapse = "<br>")

        HTML(paste("Loaded System Models: <br>", model_names))
      } else {
        HTML("No System Models loaded")
      }
    })

    output$modelSummaries <- shiny::renderUI({
      lapply(models, function(model) {
        shiny::verbatimTextOutput(outputId = paste0(model, "_summary"))
      })
    })

    output$modelPlots <- shiny::renderUI({
      lapply(models, function(model) {
        sysmod <- sysmod_list()[[model]]
        if (!is.null(sysmod)) {
          plotly::plotlyOutput(outputId = paste0(model, "_plot"))
        }
      })
    })

    lapply(models, function(model) {
      output[[paste0(model, "_summary")]] <- renderPrint({
        sysmod <- sysmod_list()[[model]]
        if (!is.null(sysmod)) {
          cat("Summary of rates data for", model, "\n")
          generate_summary(as.data.frame(sysmod$mean) %>% dplyr::rename(rate = .data$mean))
        }
      })

      output[[paste0(model, "_plot")]] <- plotly::renderPlotly({
        sysmod <- sysmod_list()[[model]]
        if (!is.null(sysmod)) {
          generate_plots(as.data.frame(sysmod$mean) %>% dplyr::rename(rate = .data$mean), model)
        }
      })
    })
  })

  # Observe event for creating the system models (all 4 at once)
  shiny::observeEvent(input$import_button_sysmod, {
    req(global_config()$data_dir, input$sysmod_list_file)

    sysmods_file <- input$sysmod_list_file

    # Check if the counts file exists
    if (!file.exists(paste0(global_config()$data_dir, "/", sysmods_file))) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "File Error",
        text = paste("The specified counts file does not exist:", input$counts_file),
        type = "error"
      )
      return() # Exit the function if the file doesn't exist
    }

    sysmod_list_imported <- readRDS(paste0(global_config()$data_dir, "/", sysmods_file))

    sysmod_list(sysmod_list_imported)

    currentList <- sysmod_list_list()
    newObjectList <- list()
    newObjectList[[as.character(stringr::str_remove_all(sysmods_file, ".RDS"))]] <- sysmod_list()
    sysmod_list_list(c(currentList, newObjectList))

    # Update the dropdown choices when datamod_list changes
    observe({
      updateSelectInput(session, "delete_sysmod_list", choices = names(sysmod_list_list()))
    })
  })


  # Observe event for creating the system models (all 4 at once)
  shiny::observeEvent(input$export_system_models, {
    file_error <- FALSE # Flag to track file errors
    req(global_config()$data_dir)
    models <- c("births", "deaths", "ins", "outs")
    new_sysmods <- lapply(models, function(model) {
      rates_file <- file.path(global_config()$data_dir, input[[paste0(model, "_rates_file")]])

      if (!file.exists(rates_file)) {
        file_error <<- TRUE # Set error flag to TRUE
        return(NULL) # Return NULL if file not found
      }

      rates_df <- utils::read.csv(rates_file)

      if (input[[paste0(model, "_disp_type")]] == "Single Value") {
        disp <- input[[paste0(model, "_disp_value")]]
      } else {
        disp_file <- file.path(global_config()$data_dir, input[[paste0(model, "_disp_file")]])
        if (!file.exists(disp_file)) {
          file_error <<- TRUE
          return(NULL)
        }
        disp <- utils::read.csv(disp_file)
      }

      lower_rates_limit <- input[[paste0(model, "_lower_rate_limit")]]

      rate_scaler <- input[[paste0(model, "_rate_scale")]]

      rate_overide <- input[[paste0(model, "_rate_overide")]]

      time_selection <- global_config()$time_selection

      create_system_model(model, rates_df, disp, time_selection, lower_rates_limit, rate_scaler, rate_overide)
    })

    if (file_error) { # Display a warning if any file errors occurred
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "File Error",
        text = "One or more files specified for the system models do not exist. Please check your file paths and try again.",
        type = "error"
      )
    }

    names(new_sysmods) <- models # Ensure the new models are named correctly
    saveRDS(c(new_sysmods), paste0(global_config()$data_dir, "/", input$sysmods_name, "_sysmods.RDS"))
  })

  # Observe event for the button to move to fitModel tab
  shiny::observeEvent(input$goGC, {
    shiny::updateTabsetPanel(session, "tabs", selected = "globalConfig")
  })


  ## dataModels Tab
  # Reactive to read the counts data (if file exists) for all data models
  mainData <- shiny::reactive({
    req(global_config()$data_dir, input$counts_file)
    counts_file_path <- file.path(global_config()$data_dir, input$counts_file)

    # Check if the counts file exists
    if (!file.exists(counts_file_path)) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "File Error",
        text = paste("The specified counts file does not exist:", input$counts_file),
        type = "error"
      )
      return() # Exit the function if the file doesn't exist
    }
    utils::read.csv(counts_file_path)
  })

  # Reactive to read the auxiliary data (if files exist) for different data models
  auxData <- shiny::reactive({
    extraData <- list()
    if (input$data_model == "Normal Data Model") {
      req(input$counts_uncertainty_file)
      counts_uncertainty_file_path <- file.path(global_config()$data_dir, input$counts_uncertainty_file)
      # Check if the uncertainty file exists
      if (!file.exists(counts_uncertainty_file_path)) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "File Error",
          text = paste("The specified counts file does not exist:", input$counts_uncertainty_file),
          type = "error"
        )
        return()
      }

      utils::read.csv(counts_uncertainty_file_path)

      if (nchar(input$ratio_file) > 0) {
        if (file.exists(file.path(global_config()$data_dir, input$ratio_file))) {
          extraData$ratio <- utils::read.csv(file.path(global_config()$data_dir, input$ratio_file))
        }
      } else {
        extraData$ratio <- input$ratio_value
      }

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
        scale_file_path <- file.path(global_config()$data_dir, input$scale_file)
        # Check if the t-dist scale file exists
        if (!file.exists(scale_file_path)) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "File Error",
            text = paste("The specified counts file does not exist:", input$scale_file),
            type = "error"
          )
          return() # Exit the function if the file doesn't exist
        }
        extraData$scale_df <- utils::read.csv(scale_file_path)
      }
      extraData$scale_ratio <- input$scale_ratio
      extraData$count_scaler <- input$count_scaler
    } else if (input$data_model == "Negative Binomial Data Model") {
      if (input$disp_type == "Single Value") {
        extraData$disp <- input$disp_value
      } else {
        req(input$disp_file)
        disp_file_path <- file.path(global_config()$data_dir, input$disp_file)
        # Check if the nbinom dispersion file exists
        if (!file.exists(disp_file_path)) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "File Error",
            text = paste("The specified counts file does not exist:", input$disp_file),
            type = "error"
          )
          return() # Exit the function if the file doesn't exist
        }
        extraData$disp <- utils::read.csv(disp_file_path)
      }
      extraData$scale_ratio <- input$scale_ratio

      extraData$count_scaler <- input$count_scaler

      if (nchar(input$ratio_file) > 0) {
        extraData$ratio <- utils::read.csv(file.path(global_config()$data_dir, input$ratio_file))
      } else {
        extraData$ratio <- input$ratio_value
      }
    } else if (input$data_model == "Poisson Data Model") {
      extraData$scale_ratio <- input$scale_ratio

      extraData$count_scaler <- input$count_scaler

      if (nchar(input$ratio_file) > 0) {
        extraData$ratio <- utils::read.csv(file.path(global_config()$data_dir, input$ratio_file))
      } else {
        extraData$ratio <- input$ratio_value
      }
    }
    extraData
  })

  # Render the additional auxiliary data inputs when selected
  output$additionalInputs <- shiny::renderUI({
    switch(input$data_model,
           "Normal Data Model" = tagList(
             shiny::textInput("counts_uncertainty_file", "Counts Uncertainty CSV Filename")
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

  # Render the additional auxiliary data inputs when selected
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
             shiny::textInput("age_selection", "Optional: Age Selection (comma separated or leave blank for all)", value = ""),
             shiny::textInput("time_selection", "Optional: Time Selection (comma separated or leave blank for all)", value = "")
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
               shiny::textInput("age_selection", "Optional: Age Selection (comma separated or leave blank for all)", value = ""),
               shiny::textInput("time_selection", "Optional: Time Selection (comma separated or leave blank for all)", value = "")
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
               shiny::textInput("age_selection", "Optional: Age Selection (comma separated or leave blank for all)", value = ""),
               shiny::textInput("time_selection", "Optional: Time Selection (comma separated or leave blank for all)", value = "")
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
               shiny::textInput("age_selection", "Optional: Age Selection (comma separated or leave blank for all)", value = ""),
               shiny::textInput("time_selection", "Optional: Time Selection (comma separated or leave blank for all)", value = "")
             ),
           NULL
    )
  })

  # Observe the create_data_model button to add new data model to the datamod_list
  shiny::observeEvent(input$create_births_data_model, {
    counts_file_path <- file.path(global_config()$data_dir, input[[paste0("births_counts_file")]])

    # Check if the counts file exists
    if (!file.exists(counts_file_path)) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "File Error",
        text = paste("The specified counts file does not exist:", input[[paste0("births_counts_file")]]),
        type = "error"
      )
      return() # Exit the function if the file doesn't exist
    }

    input_counts <- utils::read.csv(counts_file_path)

    newObject <- create_data_model(
      dm_name = "births",
      series_name = "births",
      dm_type = "Exact Data Model",
      counts_df = input_counts
    )
    currentList <- datamod_list()
    newObjectList <- list()
    newObjectList[[newObject$nm_data]] <- newObject
    datamod_list(c(currentList, newObjectList))
  })

  # Observe the create_data_model button to add new data model to the datamod_list
  shiny::observeEvent(input$create_deaths_data_model, {
    counts_file_path <- file.path(global_config()$data_dir, input[[paste0("deaths_counts_file")]])

    # Check if the counts file exists
    if (!file.exists(counts_file_path)) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "File Error",
        text = paste("The specified counts file does not exist:", input[[paste0("deaths_counts_file")]]),
        type = "error"
      )
      return() # Exit the function if the file doesn't exist
    }

    input_counts <- utils::read.csv(counts_file_path)

    newObject <- create_data_model(
      dm_name = "deaths",
      series_name = "deaths",
      dm_type = "Exact Data Model",
      counts_df = input_counts
    )
    currentList <- datamod_list()
    newObjectList <- list()
    newObjectList[[newObject$nm_data]] <- newObject
    datamod_list(c(currentList, newObjectList))
  })

  # Observe the create_data_model button to add new data model to the datamod_list
  shiny::observeEvent(input$create_data_model, {
    req(mainData(), auxData())

    age_select_str <- input$age_selection

    if (grepl(":", age_select_str)) {
      range_vals <- as.numeric(strsplit(age_select_str, ":")[[1]])
      age_subset_vals <- seq(range_vals[1], range_vals[2])
    } else if (grepl(",", age_select_str)) {
      age_subset_vals <- as.numeric(unlist(strsplit(age_select_str, ",")))
    } else if ((!grepl(",", age_select_str)) & (nchar(age_select_str) > 0)) {
      age_subset_vals <- as.numeric(age_select_str)
    } else {
      age_subset_vals <- NULL
    }

    time_select_str <- input$time_selection

    if (grepl(":", time_select_str)) {
      range_vals <- as.numeric(strsplit(time_select_str, ":")[[1]])
      time_subset_vals <- seq(range_vals[1], range_vals[2])
    } else if (grepl(",", time_select_str)) {
      time_subset_vals <- as.numeric(unlist(strsplit(time_select_str, ",")))
    } else if ((!grepl(",", time_select_str)) & (nchar(time_select_str) > 0)) {
      time_subset_vals <- as.numeric(time_select_str)
    } else {
      time_subset_vals <- NULL
    }

    newObject <- create_data_model(
      dm_name = input$dm_name,
      series_name = input$series_name,
      dm_type = input$data_model,
      counts_df = mainData(),
      time_select = time_subset_vals,
      age_select = age_subset_vals,
      uncertainty_df = auxData()$uncertainty,
      scale_df = auxData()$scale,
      disp = auxData()$disp,
      scale_ratio = auxData()$scale_ratio,
      ratio = auxData()$ratio,
      sd_scaler = auxData()$sd_scaler,
      sd_overide = auxData()$sd_overide,
      min_sd = auxData()$min_sd,
      count_scaler = auxData()$count_scaler
    )
    currentList <- datamod_list()
    newObjectList <- list()
    newObjectList[[newObject$nm_data]] <- newObject
    datamod_list(c(currentList, newObjectList))

    # Clear form inputs
    updateTextInput(session, "dm_name", value = "")
    updateTextInput(session, "series_name", value = "")
    updateTextInput(session, "counts_file", value = "")
    updateTextInput(session, "age_selection", value = "")
    updateTextInput(session, "time_selection", value = "")
    updateTextInput(session, "counts_uncertainty_file", value = "")
    updateRadioButtons(session, "scale_type", selected = "Single Value")
    updateNumericInput(session, "scale_value", value = 1)
    updateTextInput(session, "scale_file", value = "")
    updateRadioButtons(session, "disp_type", selected = "Single Value")
    updateNumericInput(session, "disp_value", value = 0.1)
    updateTextInput(session, "disp_file", value = "")
    updateNumericInput(session, "scale_ratio", value = 0)
    updateNumericInput(session, "sd_scaler", value = 1)
    updateNumericInput(session, "sd_overide", value = -1)
    updateNumericInput(session, "min_sd", value = 0)
  })

  # Update the dropdown choices when datamod_list changes
  observe({
    updateSelectInput(session, "delete_data_model", choices = names(datamod_list()))
  })

  # Observe event for deleting a data model
  shiny::observeEvent(input$delete_button, {
    req(input$delete_data_model)
    currentList <- datamod_list()
    currentList[[input$delete_data_model]] <- NULL
    datamod_list(currentList)
  })

  output$loadedDataModels <- renderDT({
    dataModels <- datamod_list()
    if (length(dataModels) > 0) {
      modelSummaries <- lapply(dataModels, function(dm) {
        c(
          dm_name = dm$nm_data,
          series_name = dm$nm_series,
          dm_type = class(dm)
        )
      })
      modelSummaryDF <- do.call(rbind, modelSummaries)
      datatable(modelSummaryDF)
    }
  })


  # Observe event for creating the system models (all 4 at once)
  shiny::observeEvent(input$import_button_datamod, {
    req(global_config()$data_dir, input$datamod_list_file)

    datamods_file <- input$datamod_list_file

    # Check if the counts file exists
    if (!file.exists(paste0(global_config()$data_dir, "/", datamods_file))) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "File Error",
        text = paste("The specified counts file does not exist:", input$datamods_file),
        type = "error"
      )
      return() # Exit the function if the file doesn't exist
    }

    datamods_list_imported <- readRDS(paste0(global_config()$data_dir, "/", datamods_file))

    currentList <- datamod_list()
    datamod_list(c(currentList, datamods_list_imported))

    # Update the dropdown choices when datamod_list changes
    observe({
      updateSelectInput(session, "delete_data_model", choices = names(datamod_list()))
    })
  })


  # Observe event for creating the system models (all 4 at once)
  shiny::observeEvent(input$export_data_models, {
    file_error <- FALSE # Flag to track file errors
    req(global_config()$data_dir)

    dataModels <- datamod_list()

    saveRDS(dataModels, paste0(global_config()$data_dir, "/", input$datamod_list_tag, "_datamods.RDS"))
  })


  # Observe event for the button to move to systemModels tab
  shiny::observeEvent(input$goSM, {
    shiny::updateTabsetPanel(session, "tabs", selected = "systemModels")
  })


  ## fitModel Tab
  # Checklist for selecting data models to include in the fitting
  output$system_model_checklist <- shiny::renderUI({
    if (length(sysmod_list_list()) > 0) {
      sysmod_list_names <- names(sysmod_list_list())
      radioButtons("selected_system_models", "Select System Models", choices = sysmod_list_names)
    } else {
      print("No System Models defined.")
    }
  })

  # Reactive to create the checklist-dependent datamod_list subset
  filtered_system_models <- shiny::reactive({
    req(input$selected_system_models) # Ensure at least one checkbox is selected
    sysmod_list_list()[[input$selected_system_models]]
  })

  # Checklist for selecting data models to include in the fitting
  output$data_model_checklist <- shiny::renderUI({
    data_models <- names(datamod_list())
    checkboxGroupInput("selected_data_models", "Select Data Models", choices = data_models)
  })

  # Reactive to create the checklist-dependent datamod_list subset
  filtered_data_models <- shiny::reactive({
    req(input$selected_data_models) # Ensure at least one checkbox is selected
    datamod_list()[input$selected_data_models]
  })

  # Observe event for the button to move to dataModels tab
  shiny::observeEvent(input$goDM, {
    shiny::updateTabsetPanel(session, "tabs", selected = "dataModels")
  })

  # Observe the 'fit_accout_model' button press to run accountTMB with loaded system models and selected data models
  shiny::observeEvent(input$fit_account_model, {
    req(filtered_data_models(), sysmod_list(), global_config()$output_dir, global_config()$seed_value)

    models <- c("births", "deaths", "ins", "outs")

    result <- accountTMB::estimate_account(datamods = filtered_data_models(), sysmods = filtered_system_models(), seed_in = global_config()$seed_value)

    output_file <- file.path(global_config()$output_dir, "fit_model_result.RDS")
    saveRDS(result, output_file)

    showModal(modalDialog(
      title = "Model fitted",
      paste("Model fitting completed. Results saved to", output_file),
      easyClose = TRUE,
      footer = NULL
    ))

    diag <- result %>% accountTMB::diagnostics()
    cohort_passes <- nrow(diag %>% dplyr::filter(.data$success == TRUE))
    cohort_total <- nrow(diag)

    output$cohortResults <- renderText({
      paste0(cohort_passes, " out of ", cohort_total, " cohorts fitted.")
    })

    output$cohortDiagnostics <- DT::renderDT({
      DT::datatable(diag %>% dplyr::filter(.data$success == FALSE))
    })

    pop_res <- result %>% accountTMB::augment_population(collapse = "cohort")
    mig_res <- result %>%
      accountTMB::augment_events(collapse = "age") %>%
      dplyr::mutate(age = .data$time - .data$cohort) %>%
      dplyr::select(-.data$cohort)

    showModal(modalDialog(
      title = "Population estimated",
      "Population estimation completed.",
      easyClose = TRUE,
      footer = NULL
    ))

    pop_res_sub <- shiny::reactive(pop_res %>% dplyr::select(-c("population")))
    mig_res_sub <- shiny::reactive(mig_res %>% dplyr::select(-c("ins", "outs")))

    output_file <- file.path(global_config()$output_dir, "population_estimates.csv")
    utils::write.csv(pop_res_sub(), output_file, row.names = FALSE)

    output_file <- file.path(global_config()$output_dir, "migration_estimates.csv")
    utils::write.csv(mig_res_sub(), output_file, row.names = FALSE)

    # Update time_select_pop choices dynamically
    shiny::observeEvent(pop_res_sub(), {
      choices <- unique(pop_res_sub()$time)
      shiny::updateSelectInput(session, "time_select_pop", choices = choices)
    })

    # Update time_select_mig choices dynamically
    shiny::observeEvent(mig_res_sub(), {
      choices <- unique(mig_res_sub()$time)
      shiny::updateSelectInput(session, "time_select_mig", choices = choices)
    })

    output$population_table <- DT::renderDT({
      DT::datatable(pop_res_sub())
    })

    output$migration_table <- DT::renderDT({
      DT::datatable(mig_res_sub())
    })


    ## popEstimates Tab
    output$popPlots <- shiny::renderUI({
      plotly::plotlyOutput(outputId = "population_estimates")
    })

    output$population_estimates <- renderPlotly({
      req(input$time_select_pop, input$sex_select_pop)

      if (nzchar(input$compare_select_pop) & (input$compare_select_pop %in% colnames(pop_res_sub()))) {
        p <- ggplot2::ggplot(
          pop_res_sub() %>%
            dplyr::filter(
              .data$time == input$time_select_pop,
              .data$sex == input$sex_select_pop
            ),
          ggplot2::aes(
            x = .data$age,
            ymin = .data$population.lower,
            y = .data$population.fitted,
            ymax = .data$population.upper
          )
        ) +
          ggplot2::geom_pointrange(
            fatten = 0.2,
            col = "darkorange"
          ) +
          ggplot2::geom_point(ggplot2::aes(y = .data[[input$compare_select_pop]]),
                              col = "darkblue",
                              size = 0.3
          ) +
          ggplot2::ylab("Count") +
          ggplot2::ggtitle("Population estimates")

        plotly::ggplotly(p) # Convert to interactive plot
      } else {
        p <- ggplot2::ggplot(
          pop_res_sub() %>%
            dplyr::filter(
              .data$time == input$time_select_pop,
              .data$sex == input$sex_select_pop
            ),
          ggplot2::aes(
            x = .data$age,
            ymin = .data$population.lower,
            y = .data$population.fitted,
            ymax = .data$population.upper
          )
        ) +
          ggplot2::geom_pointrange(
            fatten = 0.2,
            col = "darkorange"
          ) +
          ggplot2::ylab("Count") +
          ggplot2::ggtitle("Population estimates")

        plotly::ggplotly(p) # Convert to interactive plot
      }
    })


    ## migEstimates Tab
    output$immPlots <- shiny::renderUI({
      plotlyOutput(outputId = "immigration_estimates")
    })

    output$immigration_estimates <- renderPlotly({
      req(input$time_select_mig, input$sex_select_mig)

      if (nzchar(input$compare_select_ins) & (input$compare_select_ins %in% colnames(mig_res_sub()))) {
        p <- ggplot2::ggplot(
          mig_res_sub() %>%
            dplyr::filter(
              .data$time == input$time_select_mig,
              .data$sex == input$sex_select_mig
            ),
          ggplot2::aes(
            x = .data$age,
            ymin = .data$ins.lower,
            y = .data$ins.fitted,
            ymax = .data$ins.upper
          )
        ) +
          ggplot2::geom_pointrange(
            fatten = 0.2,
            col = "darkorange"
          ) +
          ggplot2::geom_point(ggplot2::aes(y = .data[[input$compare_select_ins]]),
                              col = "darkblue",
                              size = 0.3
          ) +
          ggplot2::ylab("Count") +
          ggplot2::ggtitle("Immigration estimates")

        plotly::ggplotly(p) # Convert to interactive plot
      } else {
        p <- ggplot2::ggplot(
          mig_res_sub() %>%
            dplyr::filter(
              .data$time == input$time_select_mig,
              .data$sex == input$sex_select_mig
            ),
          ggplot2::aes(
            x = .data$age,
            ymin = .data$ins.lower,
            y = .data$ins.fitted,
            ymax = .data$ins.upper
          )
        ) +
          ggplot2::geom_pointrange(
            fatten = 0.2,
            col = "darkorange"
          ) +
          ggplot2::ylab("Count") +
          ggplot2::ggtitle("Immigration estimates")

        plotly::ggplotly(p) # Convert to interactive plot
      }
    })

    output$emPlots <- shiny::renderUI({
      plotlyOutput(outputId = "emigration_estimates")
    })

    output$emigration_estimates <- renderPlotly({
      req(input$time_select_mig, input$sex_select_mig)

      if (nzchar(input$compare_select_outs) & (input$compare_select_outs %in% colnames(mig_res_sub()))) {
        p <- ggplot2::ggplot(
          mig_res_sub() %>%
            dplyr::filter(
              .data$time == input$time_select_mig,
              .data$sex == input$sex_select_mig
            ),
          ggplot2::aes(
            x = .data$age,
            ymin = .data$outs.lower,
            y = .data$outs.fitted,
            ymax = .data$outs.upper
          )
        ) +
          ggplot2::geom_pointrange(
            fatten = 0.2,
            col = "darkorange"
          ) +
          ggplot2::geom_point(ggplot2::aes(y = .data[[input$compare_select_outs]]),
                              col = "darkblue",
                              size = 0.3
          ) +
          ggplot2::ylab("") +
          ggplot2::ggtitle("Emigration estimates")

        plotly::ggplotly(p) # Convert to interactive plot
      } else {
        p <- ggplot2::ggplot(
          mig_res_sub() %>%
            dplyr::filter(
              .data$time == input$time_select_mig,
              .data$sex == input$sex_select_mig
            ),
          ggplot2::aes(
            x = .data$age,
            ymin = .data$outs.lower,
            y = .data$outs.fitted,
            ymax = .data$outs.upper
          )
        ) +
          ggplot2::geom_pointrange(
            fatten = 0.2,
            col = "darkorange"
          ) +
          ggplot2::ylab("") +
          ggplot2::ggtitle("Emigration estimates")

        plotly::ggplotly(p) # Convert to interactive plot
      }
    })
  })


  # Observe event for the button to move to fitModel tab
  shiny::observeEvent(input$goFM, {
    shiny::updateTabsetPanel(session, "tabs", selected = "fitModel")
  })
  # Observe event for the button to move to fitModel tab
  shiny::observeEvent(input$goPE, {
    shiny::updateTabsetPanel(session, "tabs", selected = "popEstimates")
  })
  # Observe event for the button to move to fitModel tab
  shiny::observeEvent(input$goME, {
    shiny::updateTabsetPanel(session, "tabs", selected = "migEstimates")
  })
  # Observe event for the button to move to fitModel tab
  shiny::observeEvent(input$goSC, {
    shiny::updateTabsetPanel(session, "tabs", selected = "setupComp")
  })

}
