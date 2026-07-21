# Tests for prepare_haiicu_units() ----------------------------------------

testthat::test_that("prepare_haiicu_units recodes Italy and builds global IDs", {
  inputs <- helper_haiicu_inputs()

  units <- prepare_haiicu_units(inputs$unit, inputs$unit_light)

  testthat::expect_identical(as.character(units$ReportingCountry), c("AT", "IT-GiViTI", "BE"))
  testthat::expect_identical(units$UnitSize, c("10", "UNK", "12"))
  testthat::expect_true("UnitIdGlobal" %in% names(units))
  testthat::expect_true("HospitalIdGlobal" %in% names(units))
})

# Tests for prepare_patient_units() ---------------------------------------

testthat::test_that("prepare_patient_units filters and joins patients", {
  inputs <- helper_haiicu_inputs()

  patient_units <- prepare_patient_units(inputs$patient, inputs$unit)

  testthat::expect_identical(nrow(patient_units), 2L)
  testthat::expect_identical(patient_units$los, c(5, 6))
  testthat::expect_identical(patient_units$UnitId, c("U1", "U2"))
})

testthat::test_that("prepare_patient_units excludes invalid stays", {
  inputs <- helper_haiicu_inputs()
  invalid_patients <- inputs$patient[rep(1, 3), ]
  invalid_patients$RecordId <- c("P3", "P4", "P5")
  invalid_patients$ParentId <- c("U1", "U1", "U1")
  invalid_patients$DateUnitAdmission <- c(
    "2023-01-01", "2023-01-01", "2023-01-01"
  )
  invalid_patients$DateUnitDischarge <- c(
    "2023-01-02", NA, "2024-01-02"
  )
  inputs$patient <- rbind(inputs$patient, invalid_patients)

  patient_units <- prepare_patient_units(inputs$patient, inputs$unit)

  testthat::expect_identical(nrow(patient_units), 2L)
  testthat::expect_setequal(patient_units$RecordId.x, c("P1", "P2"))
})

# Tests for prepare_patient_infections() ----------------------------------

testthat::test_that("prepare_patient_infections adds flags and Italy PN LOS", {
  inputs <- helper_haiicu_inputs()
  inputs$infection$InfectionSite[[2]] <- "PN"

  duplicate_infection <- inputs$infection[1, ]
  duplicate_infection$RecordId <- "I3"
  duplicate_infection$InfectionSite <- "UTI"
  duplicate_infection$DateOfOnset <- "2023-01-04"
  inputs$infection <- rbind(inputs$infection, duplicate_infection)

  patient_units <- prepare_patient_units(inputs$patient, inputs$unit)
  patient_infections <- prepare_patient_infections(
    patient_units,
    inputs$infection
  )

  testthat::expect_setequal(patient_infections$InfectionId, c("I1", "I2", "I3"))
  testthat::expect_true(all(patient_infections$hasHai))
  testthat::expect_identical(sum(patient_infections$dupl_pat), 1L)

  italy_patient <- patient_infections[patient_infections$Id == "P2", ]
  testthat::expect_identical(italy_patient$ReportingCountry, "IT-GiViTI")
  testthat::expect_equal(as.numeric(italy_patient$losPN), 3)
})

testthat::test_that("aggregate_standard_unit_infections ignores duplicate LOS", {
  inputs <- helper_haiicu_inputs()
  duplicate_infection <- inputs$infection[1, ]
  duplicate_infection$RecordId <- "I3"
  duplicate_infection$InfectionSite <- "UTI"
  duplicate_infection$DateOfOnset <- "2023-01-04"
  inputs$infection <- rbind(inputs$infection, duplicate_infection)

  patient_units <- prepare_patient_units(inputs$patient, inputs$unit)
  patient_infections <- prepare_patient_infections(
    patient_units,
    inputs$infection
  )
  unit_infections <- aggregate_standard_unit_infections(
    patient_infections,
    inputs$unit
  )

  unit_one <- unit_infections[unit_infections$RecordId == "U1", ]
  testthat::expect_identical(unit_one$PN, 1L)
  testthat::expect_identical(unit_one$UTI, 1L)
  testthat::expect_equal(unit_one$lengthofstay, 5)
})

# Tests for build_core_workflow_outputs() ---------------------------------

testthat::test_that("build_core_workflow_outputs returns core output names", {
  outputs <- build_core_workflow_outputs(helper_haiicu_inputs())

  testthat::expect_contains(names(outputs), "haiicu_level1_all")
  testthat::expect_contains(names(outputs), "haiicu_pt_unit")
  testthat::expect_contains(names(outputs), "country_demogr")
  testthat::expect_contains(names(outputs), "haiicu_pt_inf_all")
  testthat::expect_contains(names(outputs), "haiicu_unit_expcvc")
  testthat::expect_contains(names(outputs), "haiicu_country_expcvc")
  testthat::expect_contains(names(outputs), "haiicuall_percintub")
  testthat::expect_contains(names(outputs), "haiicu_unit_expuc")
  testthat::expect_contains(names(outputs), "haiicu_country_expuc")
  testthat::expect_contains(names(outputs), "haiicuall2")
  testthat::expect_contains(names(outputs), "PNinc_country")
  testthat::expect_contains(names(outputs), "BSIinc_country")
  testthat::expect_contains(names(outputs), "UTIinc_country")
  testthat::expect_contains(names(outputs), "country_deno_7d")
  testthat::expect_contains(names(outputs), "country_ind")
  testthat::expect_contains(names(outputs), "prbsi_table")
  testthat::expect_contains(names(outputs), "bsidevadj_totcritable_bycountry")
  testthat::expect_contains(names(outputs), "bsidevadj_cri3table_bycountry")
  testthat::expect_contains(names(outputs), "bsidevadj_cvcasbsitable_bycountry")
  testthat::expect_true(all(c("PN_incdens", "BSI_incdens", "UTI_incdens") %in% names(outputs$haiicuall2)))
})