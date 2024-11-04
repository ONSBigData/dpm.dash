#' Create a accountTMB System Model
#'
#' @description
#' This function creates a system model object (of class `accountTMB_sysmod`) for use in accountTMB, a package for accounting time series modeling. The system model describes the mean and dispersion of the rates over time.
#'
#' @param model_name A character string specifying the name of the system model.
#' @param rates_df A data frame containing information about the rates. It should have columns named `time` (representing time periods) and `rate` (representing the observed rates).
#' @param disp A numeric value or a vector of numeric values specifying the dispersion parameter(s) for the rates. If a single value is provided, it's assumed constant across time periods. If a vector is provided, it should have the same length as the number of time periods in `rates_df`.
#' @param time_selection (Optional) A vector of integers specifying which time periods to include in the system model. If `NULL` (default), all time periods from `rates_df` are included.
#' @param lower_rates_limit (Optional) A numerical lower limit to apply to the rate being used to create the system model
#' @param rate_scaler (Optional) A numeric scaler term to apply to the raw rate in the rates_df
#' @param rate_overide (Optional) A numeric rate to impute to all cells, default to -1 to not apply and use raw rate
#'
#' @return
#' An object of class `accountTMB_sysmod` representing the created system model. This object can be used in subsequent accountTMB analyses.
#'
#' @details
#' This function simplifies the creation of system models compatible with accountTMB. It allows you to model the mean and dispersion of rates, optionally focusing on specific time periods if desired.
#'
#' @examples
#' # Example usage (assuming you have an appropriate rates data frame):
#' my_rates_df <- data.frame(time = 1:5, rate = c(0.1, 0.12, 0.11, 0.13, 0.15), age = c(0, 0, 0, 0, 0))
#' my_sysmod <- create_system_model(model_name = "births", rates_df = my_rates_df, disp = 0.05)
#'
#' @export
create_system_model <- function(model_name, rates_df, disp, time_selection = NULL, lower_rates_limit = 0, rate_scaler = 1, rate_overide = -1) {
  if (!is.null(time_selection)) {
    rates_df <- rates_df[rates_df$time %in% time_selection, ]
  }

  sysmod_mean <- tibble::as_tibble(rates_df %>%
    dplyr::mutate(rate = ifelse(.data$rate < lower_rates_limit,
      lower_rates_limit,
      .data$rate * rate_scaler
    )) %>%
    dplyr::rename(mean = .data$rate))

  if (rate_overide > 0) {
    sysmod_mean <- tibble::as_tibble(sysmod_mean %>%
      dplyr::mutate(mean = rate_overide))
  }

  sysmod <- accountTMB::sysmod(
    mean = sysmod_mean,
    disp = disp,
    nm_series = model_name
  )

  return(sysmod)
}

#' Create a accountTMB System Model Region
#'
#' @description
#' This function creates a system model object (of class `accountTMB_sysmod`) for use in accountTMB, a package for accounting time series modeling. The system model describes the mean and dispersion of the rates over time.
#'
#' @param model_name A character string specifying the name of the system model.
#' @param rates_df A data frame containing information about the rates. It should have columns named `time` (representing time periods) and `rate` (representing the observed rates).
#' @param disp A numeric value or a vector of numeric values specifying the dispersion parameter(s) for the rates. If a single value is provided, it's assumed constant across time periods. If a vector is provided, it should have the same length as the number of time periods in `rates_df`.
#' @param time_selection (Optional) A vector of integers specifying which time periods to include in the system model. If `NULL` (default), all time periods from `rates_df` are included.
#' @param lower_rates_limit (Optional) A numerical lower limit to apply to the rate being used to create the system model
#' @param region_selection A character UK LA code region selection
#'
#' @return
#' An object of class `accountTMB_sysmod` representing the created system model. This object can be used in subsequent accountTMB analyses.
#'
#' @details
#' This function simplifies the creation of system models compatible with accountTMB. It allows you to model the mean and dispersion of rates, optionally focusing on specific time periods if desired.
#'
#' @examples
#' # Example usage (assuming you have an appropriate rates data frame):
#' my_rates_df <- data.frame(time = 1:5, rate = c(0.1, 0.12, 0.11, 0.13, 0.15), age = c(0, 0, 0, 0, 0))
#' my_sysmod <- create_system_model(model_name = "births", rates_df = my_rates_df, disp = 0.05)
#'
#' @export
create_system_model_region <- function(model_name, rates_df, disp, region_selection, time_selection = NULL, lower_rates_limit = 0) {
  if (!is.null(time_selection)) {
    rates_df <- rates_df[rates_df$time %in% time_selection, ]
  }

  sysmod <- accountTMB::sysmod(
    mean = tibble::as_tibble(rates_df %>%
      dplyr::filter(.data$region %in% region_selection) %>%
      dplyr::mutate(rate = ifelse(.data$rate < lower_rates_limit, lower_rates_limit, .data$rate)) %>%
      dplyr::rename(mean = .data$rate) %>%
      dplyr::select(-c(.data$region))),
    disp = disp,
    nm_series = model_name
  )

  return(sysmod)
}

#' Create a Custom accountTMB Data Model
#'
#' @description
#' This function creates a data model object (of class `accountTMB_datamod`) tailored for use in accountTMB, a package for accounting time series modeling. The function supports various data model types based on the underlying distribution assumptions.
#'
#' @param dm_name A character string specifying the name of the data model.
#' @param series_name A character string indicating the name of the time series within the data model.
#' @param dm_type A character string defining the type of data model to create. Valid options are:
#'   - "Exact Data Model"
#'   - "Normal Data Model"
#'   - "T-Dist Data Model"
#'   - "Negative Binomial Data Model"
#'   - "Poisson Data Model"
#' @param counts_df A data frame containing the count data for the time series. It should have columns corresponding to time periods and the observed counts.
#' @param uncertainty_df (Optional) A data frame containing standard deviation information for the "Normal Data Model." Only used if `dm_type` is "Normal Data Model."
#' @param scale_df (Optional) A data frame containing scale information for the "T-Dist Data Model." Only used if `dm_type` is "T-Dist Data Model."
#' @param disp (Optional) A numeric value specifying the dispersion parameter for the "Negative Binomial Data Model." Only used if `dm_type` is "Negative Binomial Data Model."
#' @param scale_ratio (Optional) A numeric value adjusting the scale of the data model (default is 0). Can be used to accommodate differences in units or magnitude.
#' @param ratio (Optional) A numeric coverage ratio to apply to the counts (can be a single value or a df)
#' @param sd_scaler (Optional) A numeric scale term to apply to the uncertainty (default is 1)
#' @param sd_overide (Optional) A numeric overide for the uncertainty, applies to all cells, default -1 to not apply
#' @param min_sd (Optional) A lower bound on the uncertainty, anything found lower than this is imputed to the minimum.
#' @param count_scaler (Optional) A numeric scale term to apply to the count (default is 1)
#' @param time_select (Optional) A subset selection of years to filter the data used in the datamodel to.
#' @param age_select (Optional) A numeric subset selection of ages to filter the data used in the datamodel to.
#' @return
#' An object of class `accountTMB_datamod` representing the created data model. This object can be used in subsequent accountTMB analyses.
#'
#' @details
#' This function streamlines the creation of data models compatible with accountTMB. It offers flexibility by supporting different distribution types to match your specific data and analysis needs.
#'
#' @examples
#' # Example usage (assuming you have appropriate data frames):
#' time <- rep(2011, 4)
#' count <- c(10, 15, 20, 22)
#' age <- rep(0, 4)
#' cohort <- c(2010, 2011, 2010, 2011)
#' sex <- c("Male", "Male", "Female", "Female")
#'
#' my_counts_df <- data.frame(
#'   time = time,
#'   age = age,
#'   cohort = cohort,
#'   sex = sex,
#'   count = count
#' )
#' my_datamod <- create_data_model(
#'   dm_name = "MyModel", series_name = "births",
#'   dm_type = "Exact Data Model", counts_df = my_counts_df
#' )
#'
#' @export
create_data_model <- function(dm_name, series_name, dm_type, counts_df,
                              time_select = NULL, age_select = NULL,
                              uncertainty_df = NULL, scale_df = NULL,
                              disp = NULL, scale_ratio = 0, ratio = 1,
                              sd_scaler = 1, sd_overide = -1, min_sd = 0,
                              count_scaler = 1) {
  # Default null time select is all available time
  if (is.null(time_select)) {
    time_select <- unique(counts_df$time)
  }
  # Default null age select is all available ages
  if (is.null(age_select)) {
    age_select <- unique(counts_df$age)
  }

  if (dm_type == "Exact Data Model") {
    datamod <- accountTMB::datamod_exact(
      data = tibble::as_tibble(counts_df %>%
        filter(
          .data$time %in% time_select,
          .data$age %in% age_select
        )),
      nm_series = series_name,
      nm_data = dm_name
    )
  } else if (dm_type == "Normal Data Model") {
    dm_counts <- tibble::as_tibble(counts_df %>%
      dplyr::filter(
        .data$time %in% time_select,
        .data$age %in% age_select
      ) %>%
      dplyr::mutate(count = .data$count * as.numeric(count_scaler)))

    dm_sd <- tibble::as_tibble(uncertainty_df %>%
      dplyr::filter(
        .data$time %in% time_select,
        .data$age %in% age_select
      ) %>%
      dplyr::mutate(sd = ifelse(
        .data$sd < min_sd,
        min_sd,
        .data$sd
      )) %>%
      dplyr::mutate(sd = .data$sd * as.numeric(sd_scaler)))

    if (sd_overide > 0) {
      dm_sd <- tibble::as_tibble(dm_sd %>%
        dplyr::mutate(sd = sd_overide))
    }

    datamod <- accountTMB::datamod_norm(
      data = dm_counts,
      ratio = ratio,
      sd = dm_sd,
      nm_series = series_name,
      nm_data = dm_name,
      scale_ratio = scale_ratio
    )
  } else if (dm_type == "T-Dist Data Model") {
    dm_counts <- tibble::as_tibble(counts_df %>%
      dplyr::filter(
        .data$time %in% time_select,
        .data$age %in% age_select
      ) %>%
      dplyr::mutate(count = .data$count * as.numeric(count_scaler)))

    datamod <- accountTMB::datamod_t(
      data = dm_counts,
      ratio = ratio,
      scale = scale_df,
      nm_series = series_name,
      nm_data = dm_name,
      scale_ratio = scale_ratio
    )
  } else if (dm_type == "Negative Binomial Data Model") {
    dm_counts <- tibble::as_tibble(counts_df %>%
      dplyr::filter(
        .data$time %in% time_select,
        .data$age %in% age_select
      ) %>%
      dplyr::mutate(count = .data$count * as.numeric(count_scaler)))

    datamod <- accountTMB::datamod_nbinom(
      data = dm_counts,
      ratio = ratio,
      disp = disp,
      nm_series = series_name,
      nm_data = dm_name,
      scale_ratio = scale_ratio
    )
  } else if (dm_type == "Poisson Data Model") {
    dm_counts <- tibble::as_tibble(counts_df %>%
      dplyr::filter(
        .data$time %in% time_select,
        .data$age %in% age_select
      ) %>%
      dplyr::mutate(count = .data$count * as.numeric(count_scaler)))

    datamod <- accountTMB::datamod_poisson(
      data = dm_counts,
      ratio = ratio,
      nm_series = series_name,
      nm_data = dm_name,
      scale_ratio = scale_ratio
    )
  }
  return(datamod)
}
