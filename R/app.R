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
#' run_full_dash()
#' }
#'
#' @export
run_full_dash <- function() {
  source(here::here("R/server.R"))
  source(here::here("R/ui.R"))

  shinyApp(ui, server)
}

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
#' run_region_dash()
#' }
#'
#' @export
run_region_dash <- function() {
  source(here::here("R/server_region.R"))
  source(here::here("R/ui_region.R"))

  shinyApp(ui_region, server_region)
}


#' Launch the Demo DPM Shiny Dashboard
#'
#' @description
#' This function initializes and launches the interactive DPM (Data Processing & Modeling) Shiny dashboard. It sources the necessary server (`server.R`) and UI (`ui.R`) components, then combines them to create and run the Shiny application.
#'
#' @details
#' The dashboard facilitates data exploration, analysis, and visualization using a variety of tools and techniques. It provides an intuitive interface for users to interact with their data and gain insights.
#'
#' Before running this function, ensure that:
#' - The `server_demo.R` and `ui_demo.R` files are located in a directory named "R" relative to your current working directory.
#' - All required packages for the dashboard (specified in `server_demo.R` and `ui_demo.R`) are installed.
#'
#' @return
#' This function does not return a value directly. It launches the Shiny app in your default web browser.
#'
#' @examples
#' \dontrun{
#' # Launch the DPM dashboard (after setting your working directory appropriately)
#' run_demo_dash()
#' }
#'
#' @export
run_demo_dash <- function() {
  source(here::here("R/server_demo.R"))
  source(here::here("R/ui_demo.R"))

  shinyApp(ui_demo, server_demo)
}


#' Launch the developmental DPM Shiny Dashboard
#'
#' @description
#' This function initializes and launches the interactive DPM (Data Processing & Modeling) Shiny dashboard. It sources the necessary server (`server.R`) and UI (`ui.R`) components, then combines them to create and run the Shiny application.
#'
#' @details
#' The dashboard facilitates data exploration, analysis, and visualization using a variety of tools and techniques. It provides an intuitive interface for users to interact with their data and gain insights.
#'
#' Before running this function, ensure that:
#' - The `server_dev.R` and `ui_dev.R` files are located in a directory named "R" relative to your current working directory.
#' - All required packages for the dashboard (specified in `server_dev.R` and `ui_dev.R`) are installed.
#'
#' @return
#' This function does not return a value directly. It launches the Shiny app in your default web browser.
#'
#' @examples
#' \dontrun{
#' # Launch the DPM dashboard (after setting your working directory appropriately)
#' run_dev_dash()
#' }
#'
#' @export
run_dev_dash <- function() {
  source(here::here("R/server_dev.R"))
  source(here::here("R/ui_dev.R"))

  shinyApp(ui_dev, server_dev)
}
