#' system_models UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_system_models_ui <- function(id, sys_mod_type) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      title = paste(sys_mod_type, "System Model"), width = 6, status = "primary",
      fluidRow(
        shinydashboard::box(
          title = "Required", width = 12,
          shiny::textOutput(ns("rates_check_text")),
          shiny::textInput(ns(paste0(sys_mod_type, "_rates_file")),
                           paste(sys_mod_type, "Rates CSV Filename"),
                           value = paste0("sm_", sys_mod_type, ".csv")),
          shiny::radioButtons(ns(paste0(sys_mod_type, "_disp_type")),
                              "Dispersion Input Type",
                              choices = c("Single Value", "CSV File")),
          shiny::conditionalPanel(
            condition = sprintf("input.%s_disp_type == 'Single Value'", sys_mod_type),
            ns = ns,
            numericInput(ns(paste0(sys_mod_type, "_disp_value")),
                         paste(sys_mod_type, "Dispersion Value"),
                         value = 0.05)
          ),
          shiny::conditionalPanel(
            condition = sprintf("input.%s_disp_type == 'CSV File'", sys_mod_type),
            ns = ns,
            shiny::textOutput(ns("disp_check_text")),
            shiny::textInput(ns(paste0(sys_mod_type, "_disp_file")),
                             paste(sys_mod_type, "Dispersion CSV Filename"))
          )
        )
      ),
      fluidRow(
        shinydashboard::box(
          title = "Optional", width = 12, collapsible = TRUE, collapsed = TRUE,
          numericInput(paste0(sys_mod_type, "_lower_rate_limit"),
                       paste("Optional: ", sys_mod_type, " Lower Rate Limit"),
                       value = 1e-6),
          sliderInput(paste0(sys_mod_type, "_rate_scale"),
                      paste("Optional: ", sys_mod_type, " Rate Scaler"),
                      value = 1, min = 0, max = 3, step = 0.1),
          numericInput(paste0(sys_mod_type, "_rate_overide"),
                       paste("Optional: ", sys_mod_type, " Rate Set"),
                       value = -1)
        )
      )
    )
  )
}

#' system_models Server Functions
#'
#' @noRd
mod_system_models_server <- function(id,
                                     r,
                                     sys_mod_type,
                                     global_config,
                                     create_sys_models_button){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    rates_check <- reactive({
      # we use file_test with -f operand here, because it checks if it is a
      # file, file.exists returns true on a directory
      file_test(
        '-f',
        paste0(
          global_config()$data_dir,
          '/',
          input[[paste0(sys_mod_type, "_rates_file")]]
        )
      )
    })
    output$rates_check_text <- renderText({
      if(rates_check() == TRUE){
        r[["sys_mod_files_exist"]][[sys_mod_type]] <- TRUE
        "File exists ✅"
      } else {
        r[["sys_mod_files_exist"]][[sys_mod_type]] <- FALSE
        "🚨 File doesn't exist 🚨"
      }
    })

    disp_check <- reactive({
      file_test(
        '-f',
        paste0(
          global_config()$data_dir,
          '/',
          input[[paste0(sys_mod_type, "_disp_file")]]
        )
      )
    })

    output$disp_check_text <- renderText({
      if(disp_check() == TRUE & input[[paste0(sys_mod_type, "_disp_type")]] == "CSV File"){
        r[["sys_mod_files_exist"]][[paste0(sys_mod_type, "_disp")]] <- TRUE
        r[["sysmods"]][[sys_mod_type]][["disp_file"]] <- input[[paste0(sys_mod_type, "_disp_file")]]
        "File exists ✅"
      } else if (input[[paste0(sys_mod_type, "_disp_type")]] == "CSV File"){
        r[["sys_mod_files_exist"]][[paste0(sys_mod_type, "_disp")]] <- FALSE
        "🚨 File doesn't exist 🚨️"
      }
    })

    observe({
      req(rates_check)
      if(rates_check() == TRUE){
        r[["sysmods"]][[sys_mod_type]][["rates_file"]] <- file.path(global_config()$data_dir,
                                                                    input[[paste0(sys_mod_type, "_rates_file")]])
      }

    },
    priority = 999) |>
      bindEvent(r$create_system_models)

    # Create observers which update reactive values based on inputs
    # This is so that they can be accessed in the parent module
    # TODO: I think this should only be temporary, and we should re-write entire
    # app using stratégie du petit r
    # (https://engineering-shiny.org/structuring-project.html)

    # TODO: handle control flow in this section so we do less weird referencing
    # in main server_region.R
    observe({
      r[["sysmods"]][[sys_mod_type]][["disp_type"]] <- input[[paste0(sys_mod_type, "_disp_type")]]
    }) |>
      bindEvent(input[[paste0(sys_mod_type, "_disp_type")]])

    observe({
      r[["sysmods"]][[sys_mod_type]][["disp_value"]] <- input[[paste0(sys_mod_type, "_disp_value")]]
    }) |>
      bindEvent(input[[paste0(sys_mod_type, "_disp_value")]])
    # I haven't added in reactive assignments for lower_rates_limit, rate_scaler
    # and rate_overide as they are assigned to the global namespace




  })
}
