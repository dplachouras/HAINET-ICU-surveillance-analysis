# Tests for prepare_device_exposures() ------------------------------------

testthat::test_that("prepare_device_exposures keeps positive matching records", {
  inputs <- helper_haiicu_inputs()

  cvc_exposures <- prepare_device_exposures(inputs$exposure)

  testthat::expect_identical(cvc_exposures$RecordId, c("EXP1", "EXP2"))
  testthat::expect_s3_class(cvc_exposures$DateExpStart, "Date")
  testthat::expect_identical(cvc_exposures$expdays, c(3, 2))
})

testthat::test_that("prepare_exposures derives exposure days from raw dates", {
  raw_exposure <- data.frame(
    RecordId = c("EXP1", "EXP2", "EXP3"),
    ParentId = c("P1", "P1", "P2"),
    ExpType = c("CVC", "CVC", "CVC"),
    DateExpStart = c("2023-01-01", "2023-01-06", "2023-02-04"),
    DateExpEnd = c("2023-01-03", "2023-01-07", "2023-02-02")
  )
  patient_unit <- data.frame(
    RecordId.x = c("P1", "P2"),
    UnitId = c("U1", "U2"),
    DateUnitAdmission = c("2023-01-02", "2023-02-01"),
    DateUnitDischarge = c("2023-01-05", "2023-02-06")
  )

  exposures <- prepare_exposures(raw_exposure, patient_unit = patient_unit)

  testthat::expect_identical(exposures$RecordId, c("EXP1", "EXP3"))
  testthat::expect_identical(exposures$DateExpStart, as.Date(c("2023-01-02", "2023-02-02")))
  testthat::expect_identical(exposures$DateExpEnd, as.Date(c("2023-01-03", "2023-02-04")))
  testthat::expect_identical(exposures$expdays, c(2, 3))
})

testthat::test_that("prepare_exposures drops records fully outside stays", {
  raw_exposure <- data.frame(
    RecordId = c("BEFORE", "OVERLAP", "AFTER"),
    ParentId = c("P1", "P1", "P1"),
    ExpType = "CVC",
    DateExpStart = c("2022-12-28", "2022-12-31", "2023-01-06"),
    DateExpEnd = c("2022-12-31", "2023-01-02", "2023-01-08")
  )
  patient_unit <- data.frame(
    RecordId.x = "P1",
    UnitId = "U1",
    DateUnitAdmission = "2023-01-01",
    DateUnitDischarge = "2023-01-05"
  )

  exposures <- prepare_exposures(raw_exposure, patient_unit = patient_unit)

  expected <- data.frame(
    Id = "P1",
    ParentId = "P1",
    RecordId = "OVERLAP",
    ExpType = "CVC",
    DateExpStart = as.Date("2023-01-01"),
    DateExpEnd = as.Date("2023-01-02"),
    UnitId = "U1",
    DateUnitAdmission = as.Date("2023-01-01"),
    DateUnitDischarge = as.Date("2023-01-05"),
    expdays = 2
  )

  testthat::expect_equal(exposures, expected)
})

# Tests for summarise_unit_exposure_days() --------------------------------

testthat::test_that("summarise_unit_exposure_days sums exposure days by ICU", {
  inputs <- helper_haiicu_inputs()

  exposure_days <- summarise_unit_exposure_days(inputs$exposure)

  expected <- data.frame(
    UnitId = "U1",
    unitexpdays = 5
  )

  testthat::expect_equal(exposure_days, expected)
})

testthat::test_that("summarise_unit_exposure_days aggregates clipped exposure days", {
  raw_exposure <- data.frame(
    RecordId = c("EXP1", "EXP2", "EXP3", "EXP4"),
    ParentId = c("P1", "P1", "P2", "P2"),
    ExpType = c("CVC", "CVC", "CVC", "INT"),
    DateExpStart = c("2023-01-01", "2023-01-04", "2023-02-01", "2023-02-01"),
    DateExpEnd = c("2023-01-02", "2023-01-08", "2023-02-03", "2023-02-04")
  )
  patient_unit <- data.frame(
    RecordId.x = c("P1", "P2"),
    UnitId = c("U1", "U2"),
    DateUnitAdmission = c("2023-01-01", "2023-02-02"),
    DateUnitDischarge = c("2023-01-05", "2023-02-05")
  )

  exposure_days <- summarise_unit_exposure_days(
    raw_exposure,
    patient_unit = patient_unit
  )

  expected <- data.frame(
    UnitId = c("U1", "U2"),
    unitexpdays = c(4, 2)
  )

  testthat::expect_equal(exposure_days, expected)
})

# Tests for build_device_exposure_outputs() -------------------------------

testthat::test_that("build_device_exposure_outputs returns unit and country tables", {
  inputs <- helper_haiicu_inputs()
  unit_aggregates <- data.frame(
    RecordId = c("U1", "U2"),
    ReportingCountry = c("AT", "IT-GiViTI"),
    NumPatDaysUnit2d = c(10, 20)
  )

  outputs <- build_device_exposure_outputs(unit_aggregates, inputs$exposure)

  expected_country <- data.frame(
    ReportingCountry = "AT",
    countryexpdays = 5,
    patientdays = 10,
    utilrate = 0.5
  )

  testthat::expect_contains(names(outputs), "unit_exposure")
  testthat::expect_contains(names(outputs), "country_exposure")
  testthat::expect_equal(outputs$country_exposure, expected_country)
})

# Tests for build_intubation_exposure_output() ----------------------------

testthat::test_that("build_intubation_exposure_output adds intubation percentages", {
  inputs <- helper_haiicu_inputs()
  incidence_input <- data.frame(
    RecordId = c("U1", "U2"),
    NumPatDaysUnit2d = c(10, 20)
  )

  output <- build_intubation_exposure_output(incidence_input, inputs$exposure)

  expected <- data.frame(
    RecordId = c("U1", "U2"),
    NumPatDaysUnit2d = c(10, 20),
    expdays = c(NA, 3),
    percintub = c(NA, 15)
  )

  testthat::expect_equal(output, expected)
})

# Tests for build_urinary_catheter_outputs() ------------------------------

testthat::test_that("build_urinary_catheter_outputs applies UC rules", {
  inputs <- helper_haiicu_inputs()
  unit_aggregates <- data.frame(
    RecordId = c("U1", "U2", "U3"),
    ReportingCountry = c("AT", "FR", "BE"),
    NumPatDaysUnit2d = c(20, 20, 20),
    UTI = c(2, 2, 2)
  )
  exposure <- data.frame(
    RecordId = c("UC1", "UC2", "UC3"),
    UnitId = c("U1", "U2", "U3"),
    ExpType = "UC",
    DateExpStart = "2023-01-01",
    DateExpEnd = "2023-01-12",
    expdays = c(12, 12, 12)
  )

  outputs <- build_urinary_catheter_outputs(unit_aggregates, exposure)

  expected_country <- data.frame(
    ReportingCountry = "AT",
    countryexpdays = 12,
    patientdays = 20,
    CAUTIn = 2,
    aggr = 166.7,
    meaninc = 166.67,
    cauti25pct = 166.67,
    cautimedian = 166.67,
    cauti75pct = 166.67,
    utilrate = 0.6
  )

  testthat::expect_identical(outputs$unit_exposure$ReportingCountry, "AT")
  testthat::expect_equal(outputs$country_exposure, expected_country)
})