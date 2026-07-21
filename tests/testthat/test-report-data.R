# Helper functions ---------------------------------------------------------

helper_write_report_outputs <- function(output_dir, object_files) {
  for (object_name in names(object_files)) {
    object_value <- data.frame(
      object = object_name,
      value = seq_along(object_name)
    )
    saveRDS(object_value, file.path(output_dir, object_files[[object_name]]))
  }
}

# Tests for report_output_files() -----------------------------------------

testthat::test_that("report_output_files lists legacy report contract", {
  files <- report_output_files()

  testthat::expect_type(files, "character")
  testthat::expect_named(files)
  testthat::expect_contains(names(files), "country_unit_table")
  testthat::expect_contains(names(files), "eu_iap")
  testthat::expect_contains(names(files), "eu_cvcasbsi")
  testthat::expect_identical(files[["haiicu_unitall_uti"]], "haiicu_unitall_UTI.Rda")
})

# Tests for prepare_report_data() -----------------------------------------

testthat::test_that("prepare_report_data loads mapped output objects", {
  withr::local_tempdir()
  output_dir <- tempdir()
  object_files <- c(
    country_unit_table = "country_unit_table.Rda",
    InfOutc = "InfOutc.Rda"
  )
  helper_write_report_outputs(output_dir, object_files)

  report_data <- prepare_report_data(
    year = "2023",
    output_dir = output_dir,
    object_files = object_files
  )

  testthat::expect_named(
    report_data,
    c("country_unit_table", "InfOutc", "year", "output_dir")
  )
  testthat::expect_identical(report_data$year, "2023")
  testthat::expect_identical(report_data$country_unit_table$object, "country_unit_table")
  testthat::expect_identical(report_data$InfOutc$object, "InfOutc")
})

testthat::test_that("prepare_report_data errors for missing output directory", {
  missing_dir <- file.path(tempdir(), "missing-output-dir")

  testthat::expect_error(
    prepare_report_data(output_dir = missing_dir),
    "Output directory not found"
  )
})

# Tests for lookup helpers -------------------------------------------------

testthat::test_that("hicu_label_country_values labels known countries", {
  country_data <- data.frame(
    ReportingCountry = c("AT", "IT-SPIN-UTI", "XX")
  )

  labelled <- hicu_label_country_values(country_data)

  testthat::expect_identical(
    labelled$ReportingCountry,
    c("Austria", "Italy-SPIN-UTI", "XX")
  )
})

testthat::test_that("hicu_label_country_columns labels known country columns", {
  country_data <- data.frame(
    AT = 1L,
    DE = 2L,
    value = 3L,
    check.names = FALSE
  )

  labelled <- hicu_label_country_columns(country_data)

  testthat::expect_identical(names(labelled), c("Austria", "Germany", "value"))
})

testthat::test_that("hicu_sort_with_eu_last keeps EU row last", {
  country_data <- data.frame(
    ReportingCountry = c("EU/EEA", "Belgium", "Austria")
  )

  sorted <- hicu_sort_with_eu_last(country_data)

  testthat::expect_identical(
    sorted$ReportingCountry,
    c("Austria", "Belgium", "EU/EEA")
  )
})