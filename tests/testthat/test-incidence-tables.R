# Tests for build_incidence_tables() ---------------------------------------

testthat::test_that("build_incidence_tables returns expected tables", {
  sample_data <- tibble::tibble(
    ReportingCountry = c("DE", "FR", "ES", "ES"),
    PN = c(10, 5, 4, 6),
    BSI = c(3, 2, 1, 1),
    CRI3 = c(1, 0, 1, 0),
    UTI = c(2, 3, 1, 2),
    NumPatDaysUnit2d = c(1000, 500, 400, 600)
  )

  out <- build_incidence_tables(sample_data)

  testthat::expect_type(out, "list")
  testthat::expect_named(
    out,
    c("PNinc_country", "BSIinc_country", "UTIinc_country", "PNinc_EU", "BSIinc_EU", "UTIinc_EU")
  )

  testthat::expect_true(all(c("ReportingCountry", "PNinc") %in% names(out$PNinc_country)))
  testthat::expect_true(all(c("ReportingCountry", "BSIinc") %in% names(out$BSIinc_country)))
  testthat::expect_true(all(c("ReportingCountry", "UTIinc") %in% names(out$UTIinc_country)))
})

testthat::test_that("build_incidence_tables applies default EU exclusions", {
  sample_data <- tibble::tibble(
    ReportingCountry = c("DE", "FR", "ES"),
    PN = c(10, 5, 5),
    BSI = c(5, 5, 5),
    CRI3 = c(0, 0, 0),
    UTI = c(10, 10, 10),
    NumPatDaysUnit2d = c(1000, 1000, 1000)
  )

  out <- build_incidence_tables(sample_data)

  expected_pn_eu <- round(1000 * (5 + 5) / (1000 + 1000), 2)
  expected_bsi_eu <- round(1000 * (5 + 5) / (1000 + 1000), 2)
  expected_uti_eu <- round(1000 * 10 / 1000, 2)

  testthat::expect_identical(as.numeric(out$PNinc_EU$PNinc), expected_pn_eu)
  testthat::expect_identical(as.numeric(out$BSIinc_EU$BSIinc), expected_bsi_eu)
  testthat::expect_identical(as.numeric(out$UTIinc_EU$UTIinc), expected_uti_eu)
})

testthat::test_that("build_incidence_tables calculates country rates", {
  sample_data <- data.frame(
    ReportingCountry = factor(c("AT", "AT", "BE")),
    PN = c(1, 3, 2),
    BSI = c(1, 1, 2),
    CRI3 = c(0, 1, 1),
    UTI = c(2, 0, 4),
    NumPatDaysUnit2d = c("100", "300", "200")
  )

  out <- build_incidence_tables(sample_data)

  expected_pn_country <- tibble::tibble(
    ReportingCountry = c("AT", "BE"),
    n_PN = c(4, 2),
    n_NumPtDays = c(400, 200),
    PNinc = c(10, 10),
    meanPNinc = c(10, 10),
    pct25 = c(10, 10),
    median = c(10, 10),
    pct75 = c(10, 10)
  )
  expected_bsi_country <- tibble::tibble(
    ReportingCountry = c("AT", "BE"),
    n_BSI = c(3, 3),
    n_NumPtDays = c(400, 200),
    BSIinc = c(7.5, 15),
    meanBSIinc = c(8.33, 15),
    pct25 = c(7.5, 15),
    median = c(8.33, 15),
    pct75 = c(9.17, 15)
  )
  expected_uti_country <- tibble::tibble(
    ReportingCountry = c("AT", "BE"),
    n_UTI = c(2, 4),
    n_NumPtDays = c(400, 200),
    UTIinc = c(5, 20),
    meanUTIinc = c(10, 20),
    pct25 = c(5, 20),
    median = c(10, 20),
    pct75 = c(15, 20)
  )

  testthat::expect_equal(out$PNinc_country, expected_pn_country)
  testthat::expect_equal(out$BSIinc_country, expected_bsi_country)
  testthat::expect_equal(out$UTIinc_country, expected_uti_country)
})

testthat::test_that("build_incidence_tables accepts custom EU exclusions", {
  sample_data <- tibble::tibble(
    ReportingCountry = c("AT", "BE", "DE"),
    PN = c(1, 2, 9),
    BSI = c(1, 2, 9),
    CRI3 = c(0, 1, 0),
    UTI = c(1, 2, 9),
    NumPatDaysUnit2d = c(100, 200, 900)
  )

  out <- build_incidence_tables(
    sample_data,
    eu_exclusions = list(PN = "AT", BSI = "BE", UTI = character())
  )

  testthat::expect_identical(as.numeric(out$PNinc_EU$n_PN), 11)
  testthat::expect_identical(as.numeric(out$PNinc_EU$n_NumPtDays), 1100)
  testthat::expect_identical(as.numeric(out$BSIinc_EU$n_BSI), 10)
  testthat::expect_identical(as.numeric(out$BSIinc_EU$n_NumPtDays), 1000)
  testthat::expect_identical(as.numeric(out$UTIinc_EU$n_UTI), 12)
  testthat::expect_identical(as.numeric(out$UTIinc_EU$n_NumPtDays), 1200)
})

testthat::test_that("build_incidence_tables errors on missing columns", {
  bad_data <- tibble::tibble(
    ReportingCountry = c("ES"),
    PN = c(1),
    NumPatDaysUnit2d = c(10)
  )

  testthat::expect_error(
    build_incidence_tables(bad_data),
    "Missing required columns"
  )
})

# Tests for build_prbsi_table() -------------------------------------------

testthat::test_that("build_prbsi_table returns country primary BSI rates", {
  unit_cvc_exposure <- data.frame(
    RecordId = c("U1", "U2", "U3"),
    ReportingCountry = c("AT", "AT", "IT-GiViTI"),
    unitexpdays = c(20, 10, 30),
    PRBSI = c(2, 1, 1),
    NumPatDaysUnit2d = c(100, 50, 100)
  )
  patient_unit <- data.frame(
    UnitId = c("U1", "U1", "U2", "U2", "U3")
  )

  out <- build_prbsi_table(
    unit_cvc_exposure,
    patient_unit,
    min_unit_admissions = 2
  )

  expected <- data.frame(
    ReportingCountry = "AT",
    cvcdays = 30,
    cvcuse = 200,
    pt_days = 150,
    n_prbsi = 3,
    aggrinc = 20,
    avgprbsirate = 20,
    prbsirate25pct = 20,
    prbsiratemedian = 20,
    prbisrate75pct = 20
  )

  testthat::expect_equal(out$prbsi_table, expected)
})

testthat::test_that("build_prbsi_table applies intubation eligibility", {
  unit_cvc_exposure <- data.frame(
    RecordId = c("U1", "U2"),
    ReportingCountry = c("AT", "AT"),
    unitexpdays = c(20, 10),
    PRBSI = c(2, 1),
    NumPatDaysUnit2d = c(100, 50)
  )
  patient_unit <- data.frame(UnitId = c("U1", "U2"))
  intubation_exposure <- data.frame(
    RecordId = c("U1", "U2"),
    expdays = c(20, 19)
  )

  out <- build_prbsi_table(
    unit_cvc_exposure,
    patient_unit,
    intubation_exposure = intubation_exposure,
    min_unit_admissions = 1,
    min_intubation_days = 20
  )

  testthat::expect_identical(out$unit_prbsi_table$RecordId, "U1")
  testthat::expect_equal(out$prbsi_table$n_prbsi, 2)
})

# Tests for build_device_bsi_tables() -------------------------------------

testthat::test_that("build_device_bsi_tables returns CRBSI and CRI3 tables", {
  unit_cvc_exposure <- data.frame(
    RecordId = c("U1", "U2"),
    ReportingCountry = c("AT", "IT-GiViTI"),
    unitexpdays = c(20, 10),
    NumPatDaysUnit2d = c(100, 50)
  )
  patient_infections <- data.frame(
    UnitId = c("U1", "U1", "U2"),
    InfectionSite = c("BSI", "CRI3-CVC", "BSI"),
    BSIOrigin = c("C-CVC", "C-CVC", "S-PUL")
  )
  patient_unit <- data.frame(
    UnitId = c("U1", "U1", "U2"),
    los = c(40, 60, 50)
  )

  out <- build_device_bsi_tables(
    unit_cvc_exposure,
    patient_infections,
    patient_unit,
    min_unit_admissions = 2
  )

  expected_tot <- data.frame(
    ReportingCountry = "AT",
    n_cvcdays = 20,
    cvcuse = 200,
    n_totcrbsi = 3,
    aggrinc = 150,
    avgcrbsirate = 100,
    crbsirate25pct = 100,
    crbsiratemedian = 100,
    crbisrate75pct = 100
  )
  expected_cri3 <- data.frame(
    ReportingCountry = "AT",
    cvcdays = 20,
    cvcuse = 200,
    n_CRI3 = 1,
    aggrinc = 50,
    avgcrbsirate = 50,
    crbsirate25pct = 50,
    crbsiratemedian = 50,
    crbisrate75pct = 50
  )

  testthat::expect_equal(out$bsidevadj_totcritable_bycountry, expected_tot)
  testthat::expect_equal(out$bsidevadj_cri3table_bycountry, expected_cri3)
})

testthat::test_that("build_device_bsi_tables returns CLABSI tables", {
  unit_cvc_exposure <- data.frame(
    RecordId = c("U1", "U2"),
    ReportingCountry = c("AT", "IT-GiViTI"),
    unitexpdays = c(20, 10),
    NumPatDaysUnit2d = c(100, 50)
  )
  patient_infections <- data.frame(
    UnitId = c("U1", "U2"),
    InfectionSite = c("BSI", "BSI"),
    BSIOrigin = c("C-CVC", "S-PUL")
  )
  patient_unit <- data.frame(
    UnitId = c("U1", "U1", "U2"),
    los = c(40, 60, 50)
  )
  cvc_counts <- data.frame(UnitId = "U1", CVCASBSI = 2)

  out <- build_device_bsi_tables(
    unit_cvc_exposure,
    patient_infections,
    patient_unit,
    cvc_associated_counts = cvc_counts,
    min_unit_admissions = 2
  )

  expected_country <- data.frame(
    ReportingCountry = c("AT", "EU/EEA"),
    n_cvcdays = c(20, 20),
    cvcuse = c(200, 200),
    n_clabsi = c(2, 2),
    aggrinc = c(100, 100),
    avgclabsirate = c(100, 100),
    clabsirate25pct = c(100, 100),
    clabsiratemedian = c(100, 100),
    clabsirate75pct = c(100, 100)
  )
  expected_clabsi <- data.frame(
    ReportingCountry = "AT",
    countrclabsiinc = 100
  )

  testthat::expect_equal(out$bsidevadj_cvcasbsitable_bycountry, expected_country)
  testthat::expect_equal(out$clabsi_bycountry, expected_clabsi)
})

testthat::test_that("build_device_bsi_tables handles no eligible units", {
  unit_cvc_exposure <- data.frame(
    RecordId = "U1",
    ReportingCountry = "AT",
    unitexpdays = 20,
    NumPatDaysUnit2d = 100
  )
  patient_infections <- data.frame(
    UnitId = "U1",
    InfectionSite = "BSI",
    BSIOrigin = "C-CVC"
  )
  patient_unit <- data.frame(UnitId = "U1", los = 100)

  out <- build_device_bsi_tables(
    unit_cvc_exposure,
    patient_infections,
    patient_unit,
    min_unit_admissions = 2
  )

  testthat::expect_identical(nrow(out$bsidevadj_totcritable_bycountry), 0L)
  testthat::expect_identical(nrow(out$bsidevadj_cri3table_bycountry), 0L)
  testthat::expect_identical(nrow(out$bsidevadj_cvcasbsitable_bycountry), 0L)
  testthat::expect_contains(names(out$CRBSItable), "avgcrbsirate")
})

testthat::test_that("build_device_bsi_tables keeps plain numeric quantiles", {
  unit_cvc_exposure <- data.frame(
    RecordId = c("U1", "U2"),
    ReportingCountry = c("AT", "AT"),
    unitexpdays = c(20, 10),
    NumPatDaysUnit2d = c(100, 100),
    CRI3 = c(1, 0)
  )
  patient_infections <- data.frame(
    UnitId = c("U1", "U2"),
    InfectionSite = c("CRI3-CVC", "BSI"),
    BSIOrigin = c("C-CVC", "C-CVC")
  )
  patient_unit <- data.frame(
    UnitId = c("U1", "U1", "U2", "U2"),
    lengthofstay = c(50, 50, 50, 50)
  )
  cvc_counts <- data.frame(
    UnitId = c("U1", "U2"),
    CVCASBSI = c(1, 1)
  )

  out <- build_device_bsi_tables(
    unit_cvc_exposure,
    patient_infections,
    patient_unit,
    cvc_associated_counts = cvc_counts,
    min_unit_admissions = 2
  )

  quantile_columns <- c(
    "crbsirate25pct", "crbsiratemedian", "crbisrate75pct"
  )
  clabsi_quantile_columns <- c(
    "clabsirate25pct", "clabsiratemedian", "clabsirate75pct"
  )

  testthat::expect_null(names(out$bsidevadj_totcritable_bycountry$crbsirate25pct))
  testthat::expect_null(names(out$bsidevadj_totcritable_bycountry$crbsiratemedian))
  testthat::expect_null(names(out$bsidevadj_totcritable_bycountry$crbisrate75pct))
  testthat::expect_null(names(out$bsidevadj_cri3table_bycountry$crbsirate25pct))
  testthat::expect_null(names(out$bsidevadj_cvcasbsitable_bycountry$clabsirate25pct))
  testthat::expect_true(all(
    vapply(
      out$CRBSItable[quantile_columns],
      is.numeric,
      logical(1)
    )
  ))
  testthat::expect_true(all(
    vapply(
      out$bsidevadj_cvcasbsitable_bycountry[clabsi_quantile_columns],
      is.numeric,
      logical(1)
    )
  ))
})
