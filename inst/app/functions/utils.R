#' Generate Summary Statistics for Data Frames Containing Rate Information
#'
#' @description
#' This function calculates summary statistics for data frames that include a column named `rate`. If the `rate` column is not found, it returns an informative message.
#'
#' @param df A data frame. The function expects this data frame to contain a column named `rate`, which is the variable for which summary statistics will be calculated.
#'
#' @return
#' If the `rate` column is found in `df`, the function returns a summary object containing common statistical measures (min, max, median, mean, 1st & 3rd quartiles) for the `rate` values. If the `rate` column is not found, it returns a character string "Column 'rate' not found in data."
#'
#' @details
#' This function provides a convenient way to quickly summarize the distribution of rate values within a data frame. It's particularly useful in exploratory data analysis or when you need a concise overview of the rate data.
#'
#' @examples
#' # Example usage with a data frame containing a 'rate' column:
#' time <- 1:10
#' rate <- c(0.1, 0.12, 0.11, 0.13, 0.15, 0.14, 0.16, 0.17, 0.18, 0.19)
#'
#' rate_data <- data.frame(
#'   time = time,
#'   rate = rate
#' )
#' summary_result <- generate_summary(rate_data)
#' print(summary_result)
#'
#' # Example usage with a data frame lacking a 'rate' column:
#' no_rate_data <- data.frame(
#'   time = 1:10,
#'   value = c(10, 12, 9, 11, 13, 12, 14, 15, 16, 18)
#' )
#' summary_result <- generate_summary(no_rate_data)
#' print(summary_result) # Outputs "Column 'rate' not found in data."
#' @export
generate_summary <- function(df) {
  if ("rate" %in% colnames(df)) {
    summary_stats <- summary(df)
  } else {
    summary_stats <- "Column 'rate' not found in data."
  }
  return(summary_stats)
}
# generate_summary <- function(df) {
#   if ("rate" %in% colnames(df)) {
#     if('sex' %in% colnames(df)){#}
#     summary_stats <- psych::describeBy(df, df$sex)
#     } else{
#       summary_stats <- psych::describe(df)
#     }
#   } else {
#     summary_stats <- "Column 'rate' not found in data."
#   }
#   return(summary_stats)
# }


#' Generate Rate-by-Age Plots with Facets for Time Periods
#'
#' @description
#' This function creates a faceted plot displaying the relationship between age and rate values for a given data frame and model name. Each facet represents a different time period.
#'
#' @param df A data frame. It must contain the following columns:
#'   * `age`: A numeric variable representing age groups.
#'   * `rate`: A numeric variable representing the rate values associated with each age group.
#'   * `time`: A factor or character variable indicating the time period to which each observation belongs.
#' @param model_name A character string specifying the name of the model or data source. This is used in the plot title.
#'
#' @return
#' A ggplot2 object representing the generated plot. The plot displays rate values on the y-axis against age groups on the x-axis, with separate facets for each unique value in the `time` column.
#'
#' @details
#' This function is useful for visualizing how rates vary across age groups and over time. The faceted structure allows for easy comparison of rate patterns across different time periods.
#'
#' @examples
#' # Example usage (assuming you have a data frame with 'age', 'rate', and 'time' columns):
#' age_rate_data <- data.frame(
#'   age = rep(20:60, each = 5),
#'   rate = rnorm(205, mean = 0.1, sd = 0.02),
#'   time = rep(c("2023", "2024", "2025", "2026", "2027"), 41)
#' )
#' my_plot <- generate_plots(age_rate_data, "Example Model")
#' print(my_plot)
#'
#' @export
generate_plots <- function(df, model_name) {
  if (!"rate" %in% colnames(df)) {
    stop(paste("Error: Column 'rate' not found in", model_name, "data frame"))
  }

  if ("sex" %in% colnames(df)) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$age, y = .data$rate, color = as.factor(.data$time))) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::ggtitle(paste(model_name, "Rates By Age")) +
      ggplot2::xlab("Age") +
      ggplot2::ylab("Rate") +
      ggplot2::facet_wrap(~sex)
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$age, y = .data$rate, color = as.factor(.data$time))) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::ggtitle(paste(model_name, "Rates By Age")) +
      ggplot2::xlab("Age") +
      ggplot2::ylab("Rate")
  }

  if (model_name == "deaths") {
    p <- p + ggplot2::scale_y_continuous(trans = "log10")
  }

  plotly_p <- plotly::ggplotly(p)
  return(plotly_p)
}
