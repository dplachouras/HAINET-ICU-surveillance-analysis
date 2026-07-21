# Tests for summarise_country_deno_7d() -----------------------------------

testthat::test_that("summarise_country_deno_7d returns country medians", {
  inputs <- helper_haiicu_inputs()
  units <- prepare_haiicu_units(inputs$unit, inputs$unit_light)

  country_deno_7d <- summarise_country_deno_7d(
    inputs$denominator,
    inputs$denominator_light,
    units
  )

  expected <- data.frame(
    ReportingCountry = c("AT", "BE", "IT-GiViTI"),
    UnitSize_median = c(10, 12, NA_real_),
    NumPatDays7Days_mean_median = c(70, 50, 60),
    NumRegNurseHours7Days_mean_median = c(120, 80, 100),
    NumNursingAssistHours7Days_mean_median = c(40, 20, 30),
    NumAlcoholHandRubLiters_median = c(5, 6, 4),
    NumPatientDaysPrevYear_median = c(1000, 800, 900)
  )

  testthat::expect_equal(country_deno_7d, expected)
})

# Tests for summarise_country_process_indicators() ------------------------

testthat::test_that("summarise_country_process_indicators cleans indicators", {
  inputs <- helper_haiicu_inputs()
  units <- prepare_haiicu_units(inputs$unit, inputs$unit_light)

  country_ind <- summarise_country_process_indicators(
    inputs$indicator,
    inputs$indicator_light,
    inputs$denominator,
    inputs$denominator_light,
    units
  )

  testthat::expect_identical(
    country_ind$ReportingCountry,
    c("AT", "BE", "IT-GiViTI")
  )
  testthat::expect_equal(
    country_ind$IndNumCompliant_ASTREV72H_mean,
    c(10, NaN, 5)
  )
  testthat::expect_equal(
    country_ind$IndNumObservations_ASTREV72H_mean,
    c(10, NaN, 10)
  )
  testthat::expect_equal(country_ind$IndPerc_ASTREV72H_mean, c(100, NaN, 50))
  testthat::expect_equal(country_ind$IndPerc_CVCSITDRES_mean, c(50, NaN, NaN))
  testthat::expect_equal(country_ind$IndPerc_INTPOSNSUP_mean, c(NaN, 50, NaN))
  testthat::expect_identical(country_ind$n_units, c(1L, 1L, 1L))
})