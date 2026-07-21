# Helper functions ---------------------------------------------------------

helper_write_input_files <- function(data_dir, input_files) {
  for (file_name in input_files) {
    utils::write.csv(
      data.frame(RecordId = 1L),
      file.path(data_dir, file_name),
      row.names = FALSE
    )
  }
}

# Tests for haiicu_input_files() ------------------------------------------

testthat::test_that("haiicu_input_files lists expected raw inputs", {
  input_files <- haiicu_input_files()

  testthat::expect_type(input_files, "character")
  testthat::expect_named(input_files)
  testthat::expect_identical(input_files[["unit"]], "1.HAIICU.csv")
  testthat::expect_identical(input_files[["patient"]], "2.HAIICU$Pt.csv")
  testthat::expect_identical(
    input_files[["resistance_light"]],
    "4.HAIICULIGHT$Deno$Inf$Res.csv"
  )
})

# Tests for load_haiicu_inputs() ------------------------------------------

testthat::test_that("load_haiicu_inputs loads all expected input files", {
  withr::local_tempdir()
  data_dir <- tempdir()
  input_files <- c(
    unit = "1.HAIICU.csv",
    patient = "2.HAIICU$Pt.csv"
  )
  helper_write_input_files(data_dir, input_files)

  inputs <- load_haiicu_inputs(
    data_dir = data_dir,
    input_files = input_files,
    stringsAsFactors = FALSE
  )

  testthat::expect_named(inputs, c("unit", "patient"))
  testthat::expect_identical(inputs$unit$RecordId, 1L)
  testthat::expect_identical(inputs$patient$RecordId, 1L)
})

testthat::test_that("load_haiicu_inputs errors for missing required files", {
  withr::local_tempdir()
  data_dir <- file.path(tempdir(), "empty-inputs")
  dir.create(data_dir)
  input_files <- c(unit = "1.HAIICU.csv")

  testthat::expect_error(
    load_haiicu_inputs(data_dir = data_dir, input_files = input_files),
    "Missing required input files"
  )
})

testthat::test_that("load_haiicu_inputs skips optional missing files", {
  withr::local_tempdir()
  data_dir <- tempdir()
  input_files <- c(
    unit = "1.HAIICU.csv",
    resistance = "4.HAIICU$Pt$Inf$Res.csv"
  )
  helper_write_input_files(data_dir, input_files[["unit"]])

  inputs <- load_haiicu_inputs(
    data_dir = data_dir,
    input_files = input_files,
    optional_inputs = "resistance",
    stringsAsFactors = FALSE
  )

  testthat::expect_named(inputs, "unit")
})

testthat::test_that("load_haiicu_inputs errors for missing data directory", {
  missing_dir <- file.path(tempdir(), "missing-data-dir")

  testthat::expect_error(
    load_haiicu_inputs(data_dir = missing_dir),
    "Data directory not found"
  )
})