test_that("create_system_model creates a valid sysmod object", {
  # Test Data
  rates_data <- data.frame(
    time = 1:5,
    rate = c(0.1, 0.12, 0.11, 0.13, 0.15),
    age = c(0, 0, 0, 0, 0)
  )

  sysmod <- create_system_model("births", rates_data, disp = 0.05)

  # Check class
  expect_s3_class(sysmod, "accountTMB_sysmod")

  # Check structure
  expect_equal(names(sysmod), c("mean", "disp", "nm_series"))
  expect_s3_class(sysmod$mean, "tbl_df")
  expect_equal(names(sysmod$mean), c("time", "mean", "age"))
})

test_that("time_selection works correctly", {
  # Test Data
  rates_data <- data.frame(
    time = 1:5,
    rate = c(0.1, 0.12, 0.11, 0.13, 0.15),
    age = c(0, 0, 0, 0, 0)
  )
  time_selection <- c(2:4)
  sysmod_subset <- create_system_model("births", rates_data, disp = 0.05, time_selection = time_selection)

  # Check that only selected times are included
  expect_equal(sysmod_subset$mean$time, time_selection)
})

test_that("dispersion is handled correctly", {
  # Test Data
  rates_data <- data.frame(
    time = 1:5,
    rate = c(0.1, 0.12, 0.11, 0.13, 0.15),
    age = c(0, 0, 0, 0, 0)
  )
  # Constant dispersion
  sysmod_const_disp <- create_system_model("births", rates_data, disp = 0.1)
  expect_true(all(sysmod_const_disp$disp == 0.1))
})

# Additional tests
test_that("errors are raised for invalid inputs", {
  # Test Data
  rates_data <- data.frame(
    time = 1:5,
    rate = c(0.1, 0.12, 0.11, 0.13, 0.15),
    age = c(0, 0, 0, 0, 0)
  )
  # Missing model name
  expect_error(create_system_model(NULL, rates_data, disp = 0.05))

  # Invalid rates_df (missing columns)
  invalid_data <- data.frame(t = 1:5, r = c(0.1, 0.12, 0.11, 0.13, 0.15))
  expect_error(create_system_model("births", invalid_data, disp = 0.05))

  # Mismatched lengths (time_selection and disp)
  expect_error(create_system_model("births", rates_data, disp = c(0.05, 0.1), time_selection = 1:3))
})


# --- Test Suite ---

test_that("create_data_model creates correct datamod types", {
  # --- Test Data Setup ---

  # Sample counts data (consistent across tests)
  # Test Data
  counts_df <- data.frame(
    time = c(2011, 2011),
    count = c(25, 42),
    age = c(0, 0),
    sex = c('Male', 'Female')
  )

  flows_df <- data.frame(
    time = c(2011, 2011, 2011, 2011),
    count = c(10, 15, 20, 22),
    age = c(0, 0, 0, 0),
    cohort = c(2010, 2011, 2010, 2011),
    sex = c('Male','Male','Female','Female')
  )

  uncertainty_df <- data.frame(
    time = c(2011, 2011),
    sd = c(2.5, 4.2),
    age = c(0, 0),
    sex = c('Male', 'Female')
  )

  scale_df <- data.frame(
    time = c(2011, 2011),
    scale = c(2.5, 4.2),
    age = c(0, 0),
    sex = c('Male', 'Female')
  )

  # Exact Data Model
  exact_datamod <- create_data_model("ExactModel", "births", "Exact Data Model", flows_df)
  expect_s3_class(exact_datamod, "accountTMB_datamod")  # Check the base class

  # Normal Data Model
  normal_datamod <- create_data_model("NormalModel", "population", "Normal Data Model", counts_df, uncertainty_df = uncertainty_df)
  expect_s3_class(normal_datamod, "accountTMB_datamod_norm")

  # T-Dist Data Model
  tdist_datamod <- create_data_model("TDistModel", "population", "T-Dist Data Model", counts_df, scale_df = scale_df)
  expect_s3_class(tdist_datamod, "accountTMB_datamod_t")

  # Negative Binomial Data Model
  nb_datamod <- create_data_model("NBModel", "ins", "Negative Binomial Data Model", flows_df, disp = 0.8)
  expect_s3_class(nb_datamod, "accountTMB_datamod_nbinom")

  # Poisson Data Model
  poisson_datamod <- create_data_model("PoissonModel", "ins", "Poisson Data Model", flows_df)
  expect_s3_class(poisson_datamod, "accountTMB_datamod_poisson")
})

test_that("function handles invalid inputs gracefully", {
  # Invalid series
  expect_error(create_data_model("InvalidModel", "Series1", "Normal Data Model", counts_df, uncertainty_df))

  # Missing required data for a specific model
  expect_error(create_data_model("NormalModel", "population", "Normal Data Model", counts_df))

  # Mismatched lengths of input data frames
  invalid_df <- data.frame(time = 1:3, count = c(10, 15, 20))
  expect_error(create_data_model("NormalModel", "Series1", "Normal Data Model", invalid_df, uncertainty_df))
})
