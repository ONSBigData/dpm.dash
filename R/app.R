#' Launch the DPM Shiny Dashboard
#'
#' @description
#' This function initializes and launches the interactive DPM (Data Processing & Modeling) Shiny dashboard. It sources the necessary server (`server.R`) and UI (`ui.R`) components, then combines them to create and run the Shiny application.
#'
#' @details
#' The dashboard facilitates data exploration, analysis, and visualization using a variety of tools and techniques. It provides an intuitive interface for users to interact with their data and gain insights.
#'
#' Before running this function, ensure that:
#' - The `server.R` and `ui.R` files are located in a directory named "R" relative to your current working directory.
#' - All required packages for the dashboard (specified in `server.R` and `ui.R`) are installed.
#'
#' @return
#' This function does not return a value directly. It launches the Shiny app in your default web browser.
#'
#' @examples
#' \dontrun{
#' # Launch the DPM dashboard (after setting your working directory appropriately)
#' launch_dashboard()
#' }
#'
#' @export
launch_dashboard <- function() {

  requireNamespace("magrittr", quietly = TRUE)

  pkg_name <- "dpm.dash"
  app_dir <- system.file("app", package = pkg_name)

  if (app_dir == "") {
    stop("Could not find directory 'inst/app' in package ", pkg_name, ". Has the package been installed correctly?", call. = FALSE)
  }

  e <- new.env()

  ui_path <- system.file("app/server_dash.R", package = pkg_name)
  server_path <- system.file("app/ui_dash.R", package = pkg_name)

  if (ui_path == "" || server_path == "") {
    stop("Failed to find Shiny UI or Server files.", call. = FALSE)
  }

  source(ui_path, local = e)
  source(server_path, local = e)

  # source(system.file("app/functions/functions.R", package = pkg_name), local = e)
  # source(system.file("app/functions/utils.R", package = pkg_name), local = e)

  shiny::shinyApp(ui = e$ui_dash, server = e$server_dash)
}

#' Launch the DPM Multi-Step Shiny Dashboard
#'
#' @description
#' This function initializes and launches the interactive DPM (Data Processing & Modeling) Shiny dashboard. It sources the necessary server (`server.R`) and UI (`ui.R`) components, then combines them to create and run the Shiny application.
#'
#' @details
#' The dashboard facilitates data exploration, analysis, and visualization using a variety of tools and techniques. It provides an intuitive interface for users to interact with their data and gain insights.
#'
#' Before running this function, ensure that:
#' - The `server_dpm.R` and `ui_dpm.R` files are located in a directory named "R" relative to your current working directory.
#' - All required packages for the dashboard (specified in `server_dpm.R` and `ui_dpm.R`) are installed.
#'
#' @return
#' This function does not return a value directly. It launches the Shiny app in your default web browser.
#'
#' @examples
#' \dontrun{
#' # Launch the DPM dashboard (after setting your working directory appropriately)
#' launch_dpm_dash()
#' }
#'
#' @export
launch_dpm_dash <- function() {

  requireNamespace("magrittr", quietly = TRUE)

  pkg_name <- "dpm.dash"
  app_dir <- system.file("app", package = pkg_name)

  if (app_dir == "") {
    stop("Could not find directory 'inst/app' in package ", pkg_name, ". Has the package been installed correctly?", call. = FALSE)
  }

  e <- new.env()

  ui_path <- system.file("app/server_dpm.R", package = pkg_name)
  server_path <- system.file("app/ui_dpm.R", package = pkg_name)

  if (ui_path == "" || server_path == "") {
    stop("Failed to find Shiny UI or Server files.", call. = FALSE)
  }

  source(ui_path, local = e)
  source(server_path, local = e)

  # source(system.file("app/functions/functions.R", package = pkg_name), local = e)
  # source(system.file("app/functions/utils.R", package = pkg_name), local = e)

  shiny::shinyApp(ui = e$ui_dpm, server = e$server_dpm)

  # source(here::here("R/server_dpm.R"))
  # source(here::here("R/ui_dpm.R"))
  #
  # shinyApp(ui_dpm, server_dpm)
}


launch_dpm_region_dash <- function() {

  requireNamespace("magrittr", quietly = TRUE)

  pkg_name <- "dpm.dash"
  app_dir <- system.file("app", package = pkg_name)

  if (app_dir == "") {
    stop("Could not find directory 'inst/app' in package ", pkg_name, ". Has the package been installed correctly?", call. = FALSE)
  }

  e <- new.env()

  ui_path <- system.file("app/server_dpm_region.R", package = pkg_name)
  server_path <- system.file("app/ui_dpm_region.R", package = pkg_name)

  if (ui_path == "" || server_path == "") {
    stop("Failed to find Shiny UI or Server files.", call. = FALSE)
  }

  source(ui_path, local = e)
  source(server_path, local = e)

  source(system.file("app/functions/functions.R", package = pkg_name), local = e)
  source(system.file("app/functions/utils.R", package = pkg_name), local = e)

  shiny::shinyApp(ui = e$ui_dpm_region, server = e$server_dpm_region)

  # source(here::here("R/server_dpm_region.R"))
  # source(here::here("R/ui_dpm_region.R"))
  #
  # shinyApp(ui_dpm_region, server_dpm_region)
}
