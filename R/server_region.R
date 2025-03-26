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
server_region <- function(input, output, session) {
  # We set up a future plan, for paralell processing
  #future::plan(strategy = future::multisession(workers = future::availableCores() - 4))
  # Trying callr
  future::plan(strategy = future.callr::callr(workers = future::availableCores() - 4))
  # Define the global reactive lists which the dashboard relies on
  datamod_list <- reactiveVal(list()) # All defined data models
  global_config <- reactiveVal(list()) # User-defined global config
  sysmod_list <- reactiveVal(list()) # All defined system models
  sysmod_list_list <- reactiveVal(list())
  selected_data_models <- reactiveVal(list())

  r <- reactiveValues(create_system_models = 0)

  ## globalConfig Tab
  # Defaults for global_config
  default_output_dir <- here::here("output/") # Default output_dir in package
  default_data_dir <- here::here("data/") # Default data_dir in package
  default_seed_value <- numbers::nextPrime(as.integer(Sys.time())) # Generate a random prime number as the default seed value

  # Load modules
  # TODO: currently hybrid approach, change so that all content is loaded and
  # handled in shiny modules
  lapply(c("births","deaths","ins","outs"),
         \(model) mod_system_models_server(model,
                                           r,
                                           sys_mod_type = model,
                                           global_config = global_config,
                                           create_sys_models_button = input$create_system_models))

  # Observe global config save button press
  shiny::observeEvent(input$save_global_config, {
    current_region_selection <- if(nzchar(input$region_selection)){
      strsplit(input$region_selection, ',') |>
        unlist() |>
        trimws()}
    else {
      ""
    }

    global_config(list(
      data_dir = if (nzchar(input$global_data_dir)) input$global_data_dir else default_data_dir, # Use default if empty
      output_dir = if (nzchar(input$global_output_dir)) input$global_output_dir else default_output_dir, # Use default if empty
      time_selection = if (nzchar(input$global_time_selection)) as.integer(unlist(strsplit(input$global_time_selection, ","))) else NULL,
      seed_value = if (nzchar(input$global_seed_value)) input$global_seed_value else default_seed_value, # Use default if empty,
      region_selection = current_region_selection
    ))
    shiny::showModal(modalDialog(
      title = "Global Configuration Saved",
      "Global configuration parameters have been saved successfully.",
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # TODO: This observer makes the observer in mod_system_models work.
  # I have some idea why, although the below is
  # just forcing reactivity at this point.
  observe({
    r$create_system_models <- r$create_system_models + 1
  }) |>
    bindEvent(input$create_system_models)

  ## systemModels Tab
  # Observe event for creating the system models (all 4 at once)
  shiny::observeEvent(r[["create_system_models"]], {

    if (!all(as.logical(r$sys_mod_files_exist))) {
      # Display a warning if any file errors occurred
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "File Error",
        text = "One or more files specified for the system models do not exist. Please check your file paths and try again.",
        type = "error"
      )
    }
    req(global_config()$data_dir)
    req(all(as.logical(r$sys_mod_files_exist)))
    models <- c("births", "deaths", "ins", "outs")
    new_sysmods <- lapply(models, function(model) {
      rates_file <- r[["sysmods"]][[model]][["rates_file"]]
      rates_df <- utils::read.csv(rates_file)

      if (r[["sysmods"]][[model]][["disp_type"]] == "Single Value") {
        print("using single disp value")
        disp <- r[["sysmods"]][[model]][["disp_value"]]
      } else {
        print("using csv disp")
        disp_file <- file.path(global_config()$data_dir, r[["sysmods"]][[model]][["disp_file"]])
        disp <- utils::read.csv(disp_file)
      }

      lower_rates_limit <- input[[paste0(model, "_lower_rate_limit")]]

      rate_scaler <- input[[paste0(model, "_rate_scale")]]

      rate_overide <- input[[paste0(model, "_rate_overide")]]

      time_selection <- global_config()$time_selection

      # This is what we need to vary by Local authority
      # could just use create_system_model_region in a group map?
      create_system_model_region(model,
                                  rates_df,
                                  disp,
                                  global_config()$region_selection,
                                  time_selection,
                                  lower_rates_limit,
                                  rate_scaler,
                                  rate_overide)
    })



    names(new_sysmods) <- models # Ensure the new models are named correctly
    # Update the region preview
    updateSelectInput(session, "region_preview", choices = names(new_sysmods[[1]]))
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
        # System models that need to be subsetted by region
        sysmod <- sysmod_list()[[model]][[1]]
        if (!is.null(sysmod)) {
          plotly::plotlyOutput(outputId = paste0(model, "_plot"))
        }
      })
    })

    lapply(models, function(model) {
      output[[paste0(model, "_summary")]] <- renderPrint({
        sysmod <- sysmod_list()[[model]][[input$region_preview]]
        if (!is.null(sysmod)) {
          cat("Summary of rates data for", model, "\n")
          generate_summary(as.data.frame(sysmod$mean) %>% dplyr::rename(rate = .data$mean))
        }
      })

      output[[paste0(model, "_plot")]] <- plotly::renderPlotly({
        sysmod <- sysmod_list()[[model]][[input$region_preview]]
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
      rates_file <- r[["sysmods"]][[model]][["rates_file"]]
      rates_df <- utils::read.csv(rates_file)

      if (r[["sysmods"]][[model]][["disp_type"]] == "Single Value") {
        print("using single disp value")
        disp <- r[["sysmods"]][[model]][["disp_value"]]
      } else {
        print("using csv disp")
        disp_file <- file.path(global_config()$data_dir, r[["sysmods"]][[model]][["disp_file"]])
        disp <- utils::read.csv(disp_file)
      }

      lower_rates_limit <- input[[paste0(model, "_lower_rate_limit")]]

      rate_scaler <- input[[paste0(model, "_rate_scale")]]

      rate_overide <- input[[paste0(model, "_rate_overide")]]

      time_selection <- global_config()$time_selection

      # Again here, create it for system model region
      create_system_model_region(model,
                                 rates_df,
                                 disp,
                                 global_config()$region_selection,
                                 time_selection,
                                 lower_rates_limit,
                                 rate_scaler,
                                 rate_overide)
      #create_system_model(model, rates_df, disp, time_selection, lower_rates_limit, rate_scaler, rate_overide)
    })

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

    raw_counts <- utils::read.csv(counts_file_path)

    if ("region" %in% colnames(raw_counts)) {
      raw_counts <- raw_counts %>%
        dplyr::filter(region %in% global_config()$region_selection)
      return(raw_counts)
    } else {
      return(raw_counts)
    }
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

      raw_uncertainty <- utils::read.csv(counts_uncertainty_file_path)

      if ("region" %in% colnames(raw_uncertainty)) {
        raw_uncertainty <- raw_uncertainty %>%
          dplyr::filter(region %in% global_config()$region_selection)

        extraData$uncertainty <- raw_uncertainty
      } else {
        extraData$uncertainty <- raw_uncertainty
      }

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

    print("Creating births models")
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

    #  Output of this is named by region
    newObject <- purrr::map(
      unique(input_counts$region),
      \(unique_region) input_counts |>
        tibble::as_tibble() |>
        dplyr::filter(region == unique_region) |>
        dplyr::select(-region) %>%
        create_data_model(
          dm_name = "births",
          series_name = "births",
          dm_type = "Exact Data Model",
          counts_df = .
        )
    ) |>
      purrr::set_names(unique(input_counts$region))

    currentList <- datamod_list()
    currentList[["births"]] <- newObject
    datamod_list(currentList)
  })

  # Observe the create_data_model button to add new data model to the datamod_list
  shiny::observeEvent(input$create_deaths_data_model, {
    counts_file_path <- file.path(global_config()$data_dir, input[[paste0("deaths_counts_file")]])

    print("Creating deaths models")
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

    input_counts <- utils::read.csv(counts_file_path) |>
      dplyr::filter(region %in% global_config()$region_selection)

    newObject <- purrr::map(
      unique(input_counts$region),
      \(unique_region) input_counts |>
        dplyr::filter(region == unique_region) |>
        dplyr::select(-region) %>%
        create_data_model(
          dm_name = "deaths",
          series_name = "deaths",
          dm_type = "Exact Data Model",
          counts_df = .
        )
    ) |>
      purrr::set_names(unique(input_counts$region))

    currentList <- datamod_list()
    currentList[["deaths"]] <- newObject
    datamod_list(currentList)
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

    print("Setting up data model")
    print(paste0("dm_name; ", input$dm_name))
    print(paste0("series_name; ", input$series_name))

    newObject <- purrr::map(
      unique(mainData()$region),
      \(unique_region) create_data_model_region(
        dm_name = input$dm_name,
        series_name = input$series_name,
        dm_type = input$data_model,
        counts_df = mainData(),
        time_select = time_subset_vals,
        age_select = age_subset_vals,
        aux_data = auxData(),
        unique_region = unique_region
      )
    ) |>
      purrr::set_names(unique(mainData()$region))

    currentList <- datamod_list()
    currentList[[input$dm_name]] <- newObject
    datamod_list(currentList)

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
      #TODO change this so that it takes the first element of each list
      modelSummaries <- lapply(dataModels, function(dm) {
        # We take the first element here, all lists should be the same
        c(
          dm_name = dm$dm_name,
          series_name = dm$dm_series_name,
          dm_type = dm$dm_type
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

  # Create an observer to create the fit model,
  # lets say that this can be overwritten, and exported
  # should it be a list? ####
  fit_model <- shiny::reactive({
    shiny::req(
      filtered_data_models,
      filtered_system_models,
      global_config
    )

    models <- c("births", "deaths", "ins", "outs")

    #TODO: Come up with better var names
    dm <- filtered_data_models()
    sm <- filtered_system_models()
    run_config <- global_config()

    print("Fitting model")
    result <- furrr::future_map(
      run_config$region_selection,
      \(region){
        accountTMB::estimate_account(datamods = purrr::map(dm, purrr::pluck, region),
                                     sysmods = purrr::map(sm, purrr::pluck, region),
                                     seed_in = run_config$seed_value)
      },
      .progress = TRUE,
      .options = furrr::furrr_options(globals = c("dm", "sm", "run_config"),
                                      seed = run_config$seed_value)
    ) |>
      purrr::set_names(
        run_config$region_selection
      )

    output_file <- file.path(run_config$output_dir, "fit_model_result.RDS")
    print("Saving model")
    saveRDS(result, output_file)

    showModal(modalDialog(
      title = "Model fitted",
      paste("Model fitting completed. Results saved to", output_file),
      easyClose = TRUE,
      footer = NULL
    ))

    shiny::updateSelectInput(session,
                             "region_preview",
                             choices = names(result))

    return(result)
  }) |>
    shiny::bindEvent(input$fit_account_model)

  res_diag <- shiny::reactive({
    purrr::map(fit_model(),accountTMB::diagnostics)
  })

  output$cohortResults <- renderText({
    req(fit_model())

    cohort_passes <- purrr::map(
      res_diag(),
      \(x) x |>
        dplyr::filter(success == TRUE) |>
        nrow()) |>
      unlist() |>
      sum()

    cohort_total <- purrr::map(res_diag(), nrow) |>
      unlist() |>
      sum()


    paste0(
      cohort_passes,
           " out of ",
      cohort_total,
      " cohorts fitted.")
  })

  output$cohortDiagnostics <- DT::renderDT({
    req(fit_model())

    purrr::map(res_diag(),
               \(region_diag) region_diag |>
                  dplyr::filter(success == FALSE)) |>
      dplyr::bind_rows(.id = "Region") |>
      DT::datatable()
  })

  pop_res <- reactive({
    pop_res <- purrr::map(
      fit_model(),
      \(x) accountTMB::augment_population(x, collapse = "cohort") |> dplyr::select(-population),
      .progress = TRUE) |>
      dplyr::bind_rows(.id = "region")

    years <- pop_res |>
      dplyr::arrange(time) |>
      dplyr::pull(time) |>
      unique()

    updateSelectInput(session, "time_select_pop", choices = years)

    print("generated pop ests")
    output_file <- file.path(global_config()$output_dir, "population_estimates.csv")

    utils::write.csv(pop_res, output_file, row.names = FALSE)

    pop_res
  }) |>
    shiny::bindEvent(input$augment_pop)

  mig_res <- reactive({
    purrr::map(
      fit_model(),
      \(x) accountTMB::augment_events(x, collapse = "cohort"),
      .progress = TRUE)
  }) |>
    shiny::bindEvent(input$augment_mig)

  shiny::observe({
    dates <- unique(pop_res()$time)
    latest_date <- max(dates)
    shiny::updateSelectInput(session, "time_select_pop",
                             choices = dates,
                             selected = latest_date)
  }) |>
    shiny::bindEvent(pop_res)

  output$population_estimates <- renderPlotly({
    if (nzchar(input$compare_select_pop) & (input$compare_select_pop %in% colnames(pop_res()))) {
      p <- ggplot2::ggplot(
        pop_res() %>%
          dplyr::filter(
            time == input$time_select_pop,
            sex == input$sex_select_pop,
            region = input$region_preview
          ),
        ggplot2::aes(
          x = age,
          ymin = population.lower,
          y = population.fitted,
          ymax = population.upper
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
        pop_res() %>%
          dplyr::filter(
            sex == input$sex_select_pop,
            time == input$time_select_pop,
            region == input$region_preview
          ),
        ggplot2::aes(
          x = age,
          ymin = population.lower,
          y = population.fitted,
          ymax = population.upper,
          color = time
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


  mig_res_outputs <- observe({

  }) |>
    shiny::bindEvent(mig_res)


































  output$sys_model_checklist_1 <- shiny::renderUI({
    sysmods <- names(sysmod_list_list())
    shiny::checkboxGroupInput("selected_sys_models_1", "Select System Models", choices = sysmods)
  })

  output$sys_model_checklist_2 <- shiny::renderUI({
    sysmods <- names(sysmod_list_list())
    shiny::checkboxGroupInput("selected_sys_models_2", "Select System Models", choices = sysmods)
  })

  # Reactive Expression for Subset List
  filtered_sys_models_1 <- shiny::reactive({
    req(input$selected_sys_models_1) # Ensure at least one checkbox is selected
    sysmod_list_list()[[input$selected_sys_models_1]]
  })

  # Reactive Expression for Subset List
  filtered_sys_models_2 <- shiny::reactive({
    req(input$selected_sys_models_2) # Ensure at least one checkbox is selected
    sysmod_list_list()[[input$selected_sys_models_2]]
  })

  output$data_model_checklist_1 <- shiny::renderUI({
    data_models <- names(datamod_list())
    shiny::checkboxGroupInput("selected_data_models_1", "Select Data Models", choices = data_models)
  })

  output$data_model_checklist_2 <- shiny::renderUI({
    data_models <- names(datamod_list())
    shiny::checkboxGroupInput("selected_data_models_2", "Select Data Models", choices = data_models)
  })

  # Reactive Expression for Subset List
  filtered_data_models_1 <- shiny::reactive({
    req(input$selected_data_models_1) # Ensure at least one checkbox is selected
    datamod_list()[input$selected_data_models_1]
  })

  # Reactive Expression for Subset List
  filtered_data_models_2 <- shiny::reactive({
    req(input$selected_data_models_2) # Ensure at least one checkbox is selected
    datamod_list()[input$selected_data_models_2]
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


  shiny::observeEvent(input$compare_account_model, {
    req(filtered_data_models_1(), filtered_data_models_2(), sysmod_list(), global_config()$output_dir, global_config()$seed_value)

    models <- c("births", "deaths", "ins", "outs")

    result_1 <- purrr::map(
      global_config()$region_selection,
      \(region){
        accountTMB::estimate_account(datamods = purrr::map(filtered_data_models_1(), purrr::pluck, region),
                                     sysmods = purrr::map(filtered_sys_models_1(), purrr::pluck, region),
                                     seed_in = global_config()$seed_value)
      }
    ) |>
      purrr::set_names(
        global_config()$region_selection
      )

    result_2 <- purrr::map(
      global_config()$region_selection,
      \(region){
        accountTMB::estimate_account(datamods = purrr::map(filtered_data_models_2(), purrr::pluck, region),
                                     sysmods = purrr::map(filtered_sys_models_2(), purrr::pluck, region),
                                     seed_in = global_config()$seed_value)
      }
    ) |>
      purrr::set_names(
        global_config()$region_selection
      )

    showModal(modalDialog(
      title = "Model fitted",
      "Models fitting completed.",
      easyClose = TRUE,
      footer = NULL
    ))

    pop_comp_1 <- result_1 %>%
      accountTMB::augment_population(collapse = "cohort")
    mig_comp_1 <- result_1 %>%
      accountTMB::augment_events(collapse = "age") %>%
      dplyr::mutate(age = .data$time - .data$cohort)
    pop_comp_2 <- result_2 %>%
      accountTMB::augment_population(collapse = "cohort")
    mig_comp_2 <- result_2 %>%
      accountTMB::augment_events(collapse = "age") %>%
      dplyr::mutate(age = .data$time - .data$cohort)

    pop_res_1 <- pop_comp_1 %>%
      dplyr::mutate(setup = 1) %>%
      dplyr::select(c("age", "sex", "time", "population.lower", "population.fitted", "population.upper", "setup"))
    mig_res_1 <- mig_comp_1 %>%
      dplyr::mutate(setup = 1) %>%
      dplyr::select(c("age", "sex", "time", "ins.lower", "ins.fitted", "ins.upper", "outs.lower", "outs.fitted", "outs.upper", "setup"))

    pop_res_2 <- pop_comp_2 %>%
      dplyr::mutate(setup = 2) %>%
      dplyr::select(c("age", "sex", "time", "population.lower", "population.fitted", "population.upper", "setup"))
    mig_res_2 <- mig_comp_2 %>%
      dplyr::mutate(setup = 2) %>%
      dplyr::select(c("age", "sex", "time", "ins.lower", "ins.fitted", "ins.upper", "outs.lower", "outs.fitted", "outs.upper", "setup"))

    pop_res_comp <- shiny::reactive(rbind(pop_res_1, pop_res_2))
    mig_res_comp <- shiny::reactive(rbind(mig_res_1, mig_res_2))

    pop_comp_1_cols <- shiny::reactive(pop_comp_1)
    pop_comp_2_cols <- shiny::reactive(pop_comp_2)
    mig_comp_1_cols <- shiny::reactive(mig_comp_1)
    mig_comp_2_cols <- shiny::reactive(mig_comp_2)

    shiny::showModal(modalDialog(
      title = "Populations estimated",
      "Population estimations completed.",
      easyClose = TRUE,
      footer = NULL
    ))

    # Update selectInput choices dynamically
    shiny::observeEvent(pop_res_comp(), {
      endings_to_remove <- c(".fitted", ".lower", ".upper")
      time_choices <- unique(pop_res_comp()$time)
      comp_pop1_choices <- colnames(pop_comp_1_cols())[!(colnames(pop_comp_1_cols()) %in% c("age", "sex", "time", "setup", "population", "population.fitted", "population.lower", "population.upper"))]
      comp_pop2_choices <- colnames(pop_comp_2_cols())[!(colnames(pop_comp_2_cols()) %in% c("age", "sex", "time", "setup", "population", "population.fitted", "population.lower", "population.upper"))]
      comp_ins1_choices <- colnames(mig_comp_1_cols())[!(colnames(mig_comp_1_cols()) %in% c("age", "sex", "time", "setup", "ins", "ins.fitted", "ins.lower", "ins.upper", "outs", "births", "deaths", "outs.fitted", "cohort", "outs.lower", "outs.upper"))]
      comp_ins2_choices <- colnames(mig_comp_2_cols())[!(colnames(mig_comp_2_cols()) %in% c("age", "sex", "time", "setup", "ins", "ins.fitted", "ins.lower", "ins.upper", "outs", "births", "deaths", "outs.fitted", "cohort", "outs.lower", "outs.upper"))]
      comp_outs1_choices <- colnames(mig_comp_1_cols())[!(colnames(mig_comp_1_cols()) %in% c("age", "sex", "time", "setup", "ins", "ins.fitted", "ins.lower", "ins.upper", "outs", "births", "deaths", "outs.fitted", "cohort", "outs.lower", "outs.upper"))]
      comp_outs2_choices <- colnames(mig_comp_2_cols())[!(colnames(mig_comp_2_cols()) %in% c("age", "sex", "time", "setup", "ins", "ins.fitted", "ins.lower", "ins.upper", "outs", "births", "deaths", "outs.fitted", "cohort", "outs.lower", "outs.upper"))]


      shiny::updateSelectInput(session, "time_select_comp", choices = time_choices)
      shiny::updateSelectInput(session, "compare_select_pop_1", choices = comp_pop1_choices)
      shiny::updateSelectInput(session, "compare_select_ins_1", choices = comp_ins1_choices)
      shiny::updateSelectInput(session, "compare_select_outs_1", choices = comp_outs1_choices)
      shiny::updateSelectInput(session, "compare_select_pop_2", choices = comp_pop2_choices)
      shiny::updateSelectInput(session, "compare_select_ins_2", choices = comp_ins2_choices)
      shiny::updateSelectInput(session, "compare_select_outs_2", choices = comp_outs2_choices)
    })

    output$compPlots <- shiny::renderUI({
      plotly::plotlyOutput(outputId = "comparing_estimates")
    })

    output$comparing_estimates <- plotly::renderPlotly({
      req(input$time_select_comp, input$sex_select_comp, input$setup_select)

      if (input$setup_select == "Setup 1") {
        if (nzchar(input$compare_select_pop_1) & (input$compare_select_pop_1 %in% colnames(pop_comp_1_cols()))) {
          p <- ggplot2::ggplot(
            pop_res_comp() %>%
              dplyr::filter(
                .data$time == input$time_select_comp,
                .data$sex == input$sex_select_comp,
                .data$setup == 1
              ),
            ggplot2::aes(
              x = .data$age,
              ymin = .data$population.lower,
              y = .data$population.fitted,
              ymax = .data$population.upper,
              color = as.factor(.data$setup)
            )
          ) +
            ggplot2::geom_pointrange(
              fatten = 0.5,
              position = ggplot2::position_dodge(width = 0.5)
            ) +
            ggplot2::geom_point(
              data = pop_comp_1_cols() %>%
                dplyr::filter(
                  .data$time == input$time_select_comp,
                  .data$sex == input$sex_select_comp
                ), ggplot2::aes(y = .data[[input$compare_select_pop_1]]),
              col = "darkblue",
              size = 0.3
            ) +
            ggplot2::ylab("") +
            ggplot2::ggtitle("Population comparison")

          plotly::ggplotly(p) # Convert to interactive plot
        }
      } else if (input$setup_select == "Setup 2") {
        if (nzchar(input$compare_select_pop_2) & (input$compare_select_pop_2 %in% colnames(pop_comp_2_cols()))) {
          p <- ggplot2::ggplot(
            pop_res_comp() %>%
              dplyr::filter(
                .data$time == input$time_select_comp,
                .data$sex == input$sex_select_comp,
                .data$setup == 2
              ),
            ggplot2::aes(
              x = .data$age,
              ymin = .data$population.lower,
              y = .data$population.fitted,
              ymax = .data$population.upper,
              color = as.factor(.data$setup)
            )
          ) +
            ggplot2::geom_pointrange(
              fatten = 0.5,
              position = ggplot2::position_dodge(width = 0.5)
            ) +
            ggplot2::geom_point(
              data = pop_comp_2_cols() %>%
                dplyr::filter(
                  .data$time == input$time_select_comp,
                  .data$sex == input$sex_select_comp
                ), ggplot2::aes(y = .data[[input$compare_select_pop_2]]),
              col = "darkblue",
              size = 0.3
            ) +
            ggplot2::ylab("") +
            ggplot2::ggtitle("Population comparison")

          plotly::ggplotly(p) # Convert to interactive plot
        }
      } else if (input$setup_select == "Both") {
        if (nzchar(input$compare_select_pop_1) & (input$compare_select_pop_1 %in% colnames(pop_comp_1_cols())) &
            nzchar(input$compare_select_pop_2) & (input$compare_select_pop_2 %in% colnames(pop_comp_2_cols()))) {
          p <- ggplot2::ggplot(
            pop_res_comp() %>%
              dplyr::filter(
                .data$time == input$time_select_comp,
                .data$sex == input$sex_select_comp
              ),
            ggplot2::aes(
              x = .data$age,
              ymin = .data$population.lower,
              y = .data$population.fitted,
              ymax = .data$population.upper,
              color = as.factor(.data$setup)
            )
          ) +
            ggplot2::geom_pointrange(
              fatten = 0.5,
              position = ggplot2::position_dodge(width = 0.5)
            ) +
            ggplot2::geom_point(
              data = pop_comp_1_cols() %>%
                dplyr::filter(
                  .data$time == input$time_select_comp,
                  .data$sex == input$sex_select_comp
                ), ggplot2::aes(y = .data[[input$compare_select_pop_1]]),
              col = "darkgreen",
              size = 0.3
            ) +
            ggplot2::geom_point(
              data = pop_comp_2_cols() %>%
                dplyr::filter(
                  .data$time == input$time_select_comp,
                  .data$sex == input$sex_select_comp
                ), ggplot2::aes(y = .data[[input$compare_select_pop_2]]),
              col = "darkblue",
              size = 0.3
            ) +
            ggplot2::ylab("") +
            ggplot2::ggtitle("Population comparison")

          plotly::ggplotly(p) # Convert to interactive plot
        }
      }
    })

    output$compImmig <- shiny::renderUI({
      plotly::plotlyOutput(outputId = "comparing_ins")
    })


    output$comparing_ins <- plotly::renderPlotly({
      req(input$time_select_comp, input$sex_select_comp, input$setup_select)

      if (input$setup_select == "Setup 1") {
        if (nzchar(input$compare_select_ins_1) & (input$compare_select_ins_1 %in% colnames(mig_comp_1_cols())) &
            input$time_select_comp %in% unique(mig_res_comp()$time)) {
          p <- ggplot2::ggplot(
            mig_res_comp() %>%
              dplyr::filter(
                .data$time == input$time_select_comp,
                .data$sex == input$sex_select_comp,
                .data$setup == 1
              ),
            ggplot2::aes(
              x = .data$age,
              ymin = .data$ins.lower,
              y = .data$ins.fitted,
              ymax = .data$ins.upper,
              color = as.factor(.data$setup)
            )
          ) +
            ggplot2::geom_pointrange(
              fatten = 0.5,
              position = ggplot2::position_dodge(width = 0.5)
            ) +
            ggplot2::geom_point(
              data = mig_comp_1_cols() %>%
                dplyr::filter(
                  .data$time == input$time_select_comp,
                  .data$sex == input$sex_select_comp
                ), ggplot2::aes(y = .data[[input$compare_select_ins_1]]),
              col = "darkblue",
              size = 0.3
            ) +
            ggplot2::ylab("") +
            ggplot2::ggtitle("Immigration comparison")

          plotly::ggplotly(p) # Convert to interactive plot
        }
      } else if (input$setup_select == "Setup 2") {
        if (nzchar(input$compare_select_ins_2) & (input$compare_select_ins_2 %in% colnames(mig_comp_2_cols())) &
            input$time_select_comp %in% unique(mig_res_comp()$time)) {
          p <- ggplot2::ggplot(
            mig_res_comp() %>%
              dplyr::filter(
                .data$time == input$time_select_comp,
                .data$sex == input$sex_select_comp,
                .data$setup == 2
              ),
            ggplot2::aes(
              x = .data$age,
              ymin = .data$ins.lower,
              y = .data$ins.fitted,
              ymax = .data$ins.upper,
              color = as.factor(.data$setup)
            )
          ) +
            ggplot2::geom_pointrange(
              fatten = 0.5,
              position = ggplot2::position_dodge(width = 0.5)
            ) +
            ggplot2::geom_point(
              data = mig_comp_2_cols() %>%
                dplyr::filter(
                  .data$time == input$time_select_comp,
                  .data$sex == input$sex_select_comp
                ), ggplot2::aes(y = .data[[input$compare_select_ins_2]]),
              col = "darkblue",
              size = 0.3
            ) +
            ggplot2::ylab("") +
            ggplot2::ggtitle("Immigration comparison")

          plotly::ggplotly(p) # Convert to interactive plot
        }
      } else if (input$setup_select == "Both") {
        if (nzchar(input$compare_select_ins_1) & (input$compare_select_ins_1 %in% colnames(mig_comp_1_cols())) &
            nzchar(input$compare_select_ins_2) & (input$compare_select_ins_2 %in% colnames(mig_comp_2_cols())) &
            input$time_select_comp %in% unique(mig_res_comp()$time)) {
          p <- ggplot2::ggplot(
            mig_res_comp() %>%
              dplyr::filter(
                .data$time == input$time_select_comp,
                .data$sex == input$sex_select_comp
              ),
            ggplot2::aes(
              x = .data$age,
              ymin = .data$ins.lower,
              y = .data$ins.fitted,
              ymax = .data$ins.upper,
              color = as.factor(.data$setup)
            )
          ) +
            ggplot2::geom_pointrange(
              fatten = 0.5,
              position = ggplot2::position_dodge(width = 0.5)
            ) +
            ggplot2::geom_point(
              data = mig_comp_1_cols() %>%
                dplyr::filter(
                  .data$time == input$time_select_comp,
                  .data$sex == input$sex_select_comp
                ), ggplot2::aes(y = .data[[input$compare_select_ins_1]]),
              col = "darkgreen",
              size = 0.3
            ) +
            ggplot2::geom_point(
              data = mig_comp_2_cols() %>%
                dplyr::filter(
                  .data$time == input$time_select_comp,
                  .data$sex == input$sex_select_comp
                ), ggplot2::aes(y = .data[[input$compare_select_ins_2]]),
              col = "darkblue",
              size = 0.3
            ) +
            ggplot2::ylab("") +
            ggplot2::ggtitle("Immigration comparison")

          plotly::ggplotly(p) # Convert to interactive plot
        }
      }
    })


    output$compEmig <- shiny::renderUI({
      plotly::plotlyOutput(outputId = "comparing_outs")
    })

    output$comparing_outs <- plotly::renderPlotly({
      req(input$time_select_comp, input$sex_select_comp, input$setup_select)

      if (input$setup_select == "Setup 1") {
        if (nzchar(input$compare_select_outs_1) & (input$compare_select_outs_1 %in% colnames(mig_comp_1_cols())) &
            input$time_select_comp %in% unique(mig_res_comp()$time)) {
          p <- ggplot2::ggplot(
            mig_res_comp() %>%
              dplyr::filter(
                .data$time == input$time_select_comp,
                .data$sex == input$sex_select_comp,
                .data$setup == 1
              ),
            ggplot2::aes(
              x = .data$age,
              ymin = .data$outs.lower,
              y = .data$outs.fitted,
              ymax = .data$outs.upper,
              color = as.factor(.data$setup)
            )
          ) +
            ggplot2::geom_pointrange(
              fatten = 0.5,
              position = ggplot2::position_dodge(width = 0.5)
            ) +
            ggplot2::geom_point(
              data = mig_comp_1_cols() %>%
                dplyr::filter(
                  .data$time == input$time_select_comp,
                  .data$sex == input$sex_select_comp
                ), ggplot2::aes(y = .data[[input$compare_select_outs_1]]),
              col = "darkblue",
              size = 0.3
            ) +
            ggplot2::ylab("") +
            ggplot2::ggtitle("Emigration comparison")

          plotly::ggplotly(p) # Convert to interactive plot
        }
      } else if (input$setup_select == "Setup 2") {
        if (nzchar(input$compare_select_outs_2) & (input$compare_select_outs_2 %in% colnames(mig_comp_2_cols())) &
            input$time_select_comp %in% unique(mig_res_comp()$time)) {
          p <- ggplot2::ggplot(
            mig_res_comp() %>%
              dplyr::filter(
                .data$time == input$time_select_comp,
                .data$sex == input$sex_select_comp,
                .data$setup == 2
              ),
            ggplot2::aes(
              x = .data$age,
              ymin = .data$outs.lower,
              y = .data$outs.fitted,
              ymax = .data$outs.upper,
              color = as.factor(.data$setup)
            )
          ) +
            ggplot2::geom_point(
              data = mig_comp_2_cols() %>%
                dplyr::filter(
                  .data$time == input$time_select_comp,
                  .data$sex == input$sex_select_comp
                ), ggplot2::aes(y = .data[[input$compare_select_outs_2]]),
              col = "darkblue",
              size = 0.3
            ) +
            ggplot2::geom_pointrange(
              fatten = 0.5,
              position = ggplot2::position_dodge(width = 0.5)
            ) +
            ggplot2::ylab("") +
            ggplot2::ggtitle("Emigration comparison")

          plotly::ggplotly(p) # Convert to interactive plot
        }
      } else if (input$setup_select == "Both") {
        if (nzchar(input$compare_select_outs_1) & (input$compare_select_outs_1 %in% colnames(mig_comp_1_cols())) &
            nzchar(input$compare_select_outs_2) & (input$compare_select_outs_2 %in% colnames(mig_comp_2_cols())) &
            input$time_select_comp %in% unique(mig_res_comp()$time)) {
          p <- ggplot2::ggplot(
            mig_res_comp() %>%
              dplyr::filter(
                .data$time == input$time_select_comp,
                .data$sex == input$sex_select_comp
              ),
            ggplot2::aes(
              x = .data$age,
              ymin = .data$outs.lower,
              y = .data$outs.fitted,
              ymax = .data$outs.upper,
              color = as.factor(.data$setup)
            )
          ) +
            ggplot2::geom_pointrange(
              fatten = 0.5,
              position = ggplot2::position_dodge(width = 0.5)
            ) +
            ggplot2::geom_point(
              data = mig_comp_1_cols() %>%
                dplyr::filter(
                  .data$time == input$time_select_comp,
                  .data$sex == input$sex_select_comp
                ), ggplot2::aes(y = .data[[input$compare_select_outs_1]]),
              col = "darkgreen",
              size = 0.3
            ) +
            ggplot2::geom_point(
              data = mig_comp_2_cols() %>%
                dplyr::filter(
                  .data$time == input$time_select_comp,
                  .data$sex == input$sex_select_comp
                ), ggplot2::aes(y = .data[[input$compare_select_outs_2]]),
              col = "darkblue",
              size = 0.3
            ) +
            ggplot2::ylab("") +
            ggplot2::ggtitle("Emigration comparison")

          plotly::ggplotly(p) # Convert to interactive plot
        }
      }
    })


    output$compAggPop <- shiny::renderUI({
      plotly::plotlyOutput(outputId = "comparing_agg_pop")
    })
    output$comparing_agg_pop <- plotly::renderPlotly({
      pop_aggs <- pop_res_comp() %>%
        group_by(.data$time, .data$sex, .data$setup) %>%
        summarise(agg_pop = sum(.data$population.fitted))

      p <- ggplot2::ggplot(
        pop_aggs,
        ggplot2::aes(
          x = .data$time,
          y = .data$agg_pop,
          color = as.factor(.data$setup)
        )
      ) +
        ggplot2::geom_point(
          size = 2
        ) +
        ggplot2::ylab("") +
        ggplot2::ggtitle("Aggregate population comparison") +
        ggplot2::facet_grid(rows = vars(.data$sex))

      plotly::ggplotly(p)
    })


    output$compAggImmig <- shiny::renderUI({
      plotly::plotlyOutput(outputId = "comparing_agg_immig")
    })
    output$comparing_agg_immig <- plotly::renderPlotly({
      event_aggs <- mig_res_comp() %>%
        group_by(.data$time, .data$sex, .data$setup) %>%
        summarise(
          agg_ins = sum(.data$ins.fitted),
          agg_outs = sum(.data$outs.fitted)
        )

      p <- ggplot2::ggplot(
        event_aggs,
        ggplot2::aes(
          x = .data$time,
          y = .data$agg_ins,
          color = as.factor(.data$setup)
        )
      ) +
        ggplot2::geom_point(
          size = 2
        ) +
        ggplot2::ylab("") +
        ggplot2::ggtitle("Aggregate immigration comparison") +
        ggplot2::facet_grid(rows = vars(.data$sex))

      plotly::ggplotly(p)
    })

    output$compAggEmig <- shiny::renderUI({
      plotly::plotlyOutput(outputId = "comparing_agg_emig")
    })
    output$comparing_agg_emig <- plotly::renderPlotly({
      event_aggs <- mig_res_comp() %>%
        group_by(.data$time, .data$sex, .data$setup) %>%
        summarise(
          agg_ins = sum(.data$ins.fitted),
          agg_outs = sum(.data$outs.fitted)
        )
      p <- ggplot2::ggplot(
        event_aggs,
        ggplot2::aes(
          x = .data$time,
          y = .data$agg_outs,
          color = as.factor(.data$setup)
        )
      ) +
        ggplot2::geom_point(
          size = 2
        ) +
        ggplot2::ylab("") +
        ggplot2::ggtitle("Aggregate emigration comparison") +
        ggplot2::facet_grid(rows = vars(.data$sex))

      plotly::ggplotly(p)
    })

    output$compAggNetMig <- shiny::renderUI({
      plotly::plotlyOutput(outputId = "comparing_agg_netmig")
    })
    output$comparing_agg_netmig <- plotly::renderPlotly({
      event_aggs <- mig_res_comp() %>%
        group_by(.data$time, .data$sex, .data$setup) %>%
        summarise(
          agg_ins = sum(.data$ins.fitted),
          agg_outs = sum(.data$outs.fitted),
          agg_netmig = .data$agg_ins - .data$agg_outs
        )
      p <- ggplot2::ggplot(
        event_aggs,
        ggplot2::aes(
          x = .data$time,
          y = .data$agg_netmig,
          color = as.factor(.data$setup)
        )
      ) +
        ggplot2::geom_point(
          size = 2
        ) +
        ggplot2::ylab("") +
        ggplot2::ggtitle("Aggregate net-migration comparison") +
        ggplot2::facet_grid(rows = vars(.data$sex))

      plotly::ggplotly(p)
    })
  })

  # Validation observers ####
  # Creating an observer to check the filename
  shiny::observe(
    #check_headers(input$births_rates_files(), c("region","age","time","count","midint_pop_est","raw_rate","rate","ma"))
    {
      print("Changing filename")
      iv$validate()
    }
  ) |>
    shiny::bindEvent(input$births_rates_file,
                     input$deaths_rates_file,
                     input$ins_rates_file,
                     input$outs_rates_file,
                     ignoreInit = TRUE,
                     ignoreNULL = TRUE)

  # Create a validator bound to the rates files
  iv <- shinyvalidate::InputValidator$new()
  iv$add_rule("births_rates_file", ~check_headers(., c("region","age","time","count","midint_pop_est","raw_rate","rate","ma")), "Bad File")
  #iv$enable()

  # Create an observer to find the maximal region

}
