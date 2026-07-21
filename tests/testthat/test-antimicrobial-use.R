# Tests for prepare_antimicrobial_records() -------------------------------

testthat::test_that("prepare_antimicrobial_records clips treatment dates", {
  inputs <- helper_haiicu_inputs()

  records <- prepare_antimicrobial_records(
    inputs$antimicrobial,
    inputs$patient,
    prepare_haiicu_units(inputs$unit, inputs$unit_light)
  )

  testthat::expect_identical(records$treatmdays, c(3, 3, 3, 2))
  testthat::expect_identical(records$ReportingCountry, c("AT", "AT", "IT-GiViTI", "IT-GiViTI"))
})

# Tests for summarise_antimicrobial_use() ---------------------------------

testthat::test_that("summarise_antimicrobial_use returns country tables", {
  inputs <- helper_haiicu_inputs()
  units <- prepare_haiicu_units(inputs$unit, inputs$unit_light)
  unit_patient_days <- data.frame(
    UnitId = c("U1", "U2"),
    patdays = c(5, 6)
  )

  outputs <- summarise_antimicrobial_use(
    inputs$antimicrobial,
    inputs$patient,
    units,
    unit_patient_days
  )

  expected_table <- data.frame(
    ReportingCountry = c("AT", "IT-GiViTI"),
    N_ab_mean = c(2, 2),
    treatmdays_tot_mean = c(6, 5),
    carb_td_mean = c(60, 0),
    piptaz_td_mean = c(60, 0),
    ceph34_td_mean = c(0, 50),
    fq_td_mean = c(0, 0),
    glycop_td_mean = c(0, 33.33),
    polymyx_td_mean = c(0, 0)
  )
  expected_ind <- data.frame(
    ReportingCountry = c("AT", "IT-GiViTI"),
    emp = c(0.5, 0),
    dir = c(0.5, 0),
    proph = c(0, 1),
    selec = c(0, 0),
    other = c(0, 0)
  )

  testthat::expect_equal(outputs$country_ab_table, expected_table)
  testthat::expect_equal(outputs$country_ab_ind, expected_ind)
})

testthat::test_that("summarise_antimicrobial_use handles no valid treatment records", {
  inputs <- helper_haiicu_inputs()
  inputs$antimicrobial$DateAntimicrobialEnd <- NA
  units <- prepare_haiicu_units(inputs$unit, inputs$unit_light)

  outputs <- summarise_antimicrobial_use(
    inputs$antimicrobial,
    inputs$patient,
    units,
    data.frame(UnitId = c("U1", "U2"), patdays = c(5, 6))
  )

  testthat::expect_named(
    outputs,
    c("country_ab_table", "country_ab_ind", "unit_ab")
  )
  testthat::expect_equal(nrow(outputs$country_ab_table), 0)
  testthat::expect_equal(nrow(outputs$country_ab_ind), 0)
})