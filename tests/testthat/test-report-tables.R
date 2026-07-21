# Tests for hicu_append_eu_row() ------------------------------------------

testthat::test_that("hicu_append_eu_row appends and aligns EU rows", {
  country_data <- data.frame(
    ReportingCountry = c("Austria", "Belgium"),
    n_PN = c(1L, 2L),
    PNinc = c(1.1, 2.2)
  )
  eu_data <- data.frame(
    PNinc = 1.5,
    n_PN = 3L
  )

  result <- hicu_append_eu_row(country_data, eu_data)

  expected <- data.frame(
    ReportingCountry = c("Austria", "Belgium", "EU/EEA"),
    n_PN = c(1L, 2L, 3L),
    PNinc = c(1.1, 2.2, 1.5)
  )

  testthat::expect_equal(result, expected)
})

# Tests for hicu_prepare_incidence_table() --------------------------------

testthat::test_that("hicu_prepare_incidence_table labels and appends EU rows", {
  country_data <- data.frame(
    ReportingCountry = c("BE", "AT"),
    PNinc = c(2.2, 1.1)
  )
  eu_data <- data.frame(
    PNinc = 1.5
  )

  result <- hicu_prepare_incidence_table(country_data, eu_data)

  expected <- data.frame(
    ReportingCountry = c("Austria", "Belgium", "EU/EEA"),
    PNinc = c(1.1, 2.2, 1.5)
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("hicu_prepare_incidence_table works without EU data", {
  country_data <- data.frame(
    ReportingCountry = c("BE", "AT"),
    PNinc = c(2.2, 1.1)
  )

  result <- hicu_prepare_incidence_table(country_data)

  testthat::expect_identical(result$ReportingCountry, c("Austria", "Belgium"))
})

# Tests for hicu_prepare_report_table() -----------------------------------

testthat::test_that("hicu_prepare_report_table labels and sorts report fields", {
  country_data <- data.frame(
    ReportingCountry = c("EU/EEA", "BE", "AT"),
    N = c(3L, 2L, 1L),
    PNinc = c(1.5, 2.2, 1.1)
  )

  result <- hicu_prepare_report_table(country_data)

  expected <- data.frame(
    `Country/Network` = c("Austria", "Belgium", "EU/EEA"),
    `ICUs (n)` = c(1L, 2L, 3L),
    Aggregated = c(1.1, 2.2, 1.5),
    check.names = FALSE
  )

  testthat::expect_equal(result, expected)
})

testthat::test_that("hicu_prepare_report_table labels country columns", {
  country_data <- data.frame(
    AT = 1L,
    BE = 2L,
    check.names = FALSE
  )

  result <- hicu_prepare_report_table(country_data)

  testthat::expect_identical(names(result), c("Austria", "Belgium"))
})

# Tests for hicu_prepare_report_tables() ----------------------------------

testthat::test_that("hicu_prepare_report_tables builds available tables", {
  report_data <- list(
    country_unit_table = data.frame(
      ReportingCountry = c("BE", "AT"),
      N = c(2L, 1L)
    ),
    PNinc_country = data.frame(
      ReportingCountry = "AT",
      PNinc = 1.1
    ),
    PNinc_EU = data.frame(PNinc = 1.5),
    PNtable_group_top10_pc = data.frame(
      Isolate = "Escherichia coli",
      totalpc = 12.3
    ),
    CRBSItable = data.frame(
      ReportingCountry = "AT",
      CRBSI = 1.1,
      avgcrbsirate = 2.2
    ),
    totcrbsitable = data.frame(
      ReportingCountry = "AT",
      totCRBSI = 2.2
    ),
    haiicu_unit_bsidevadj_totcritable = data.frame(
      ReportingCountry = "AT",
      NumPatDaysUnit2d = 100
    ),
    eu_cvcasbsi = data.frame(
      ReportingCountry = "EU/EEA",
      cvcasbsi = 3.3
    ),
    clabsi_bycountry = data.frame(
      ReportingCountry = "AT",
      countrclabsiinc = 4.4
    ),
    haiicu_unit_bsidevadj_cvcasbsi = data.frame(
      ReportingCountry = "AT",
      cvcasbsi = 5.5
    ),
    resist = data.frame(
      ReportingCountry = "AT",
      MRSA = 10,
      CRKP = 20
    ),
    country_ab_table = data.frame(
      ReportingCountry = "AT",
      N_pat = 10L
    )
  )

  result <- hicu_prepare_report_tables(report_data)

  testthat::expect_contains(names(result), "icu_characteristics")
  testthat::expect_contains(names(result), "pn_incidence")
  testthat::expect_contains(names(result), "pn_microorganisms")
  testthat::expect_contains(names(result), "catheter_related_bsi")
  testthat::expect_contains(names(result), "total_crbsi")
  testthat::expect_contains(names(result), "unit_total_crbsi")
  testthat::expect_contains(names(result), "eu_cvc_associated_bsi")
  testthat::expect_contains(names(result), "clabsi_by_country")
  testthat::expect_contains(names(result), "unit_cvc_associated_bsi")
  testthat::expect_contains(names(result), "antimicrobial_resistance")
  testthat::expect_contains(names(result), "antimicrobial_groups")
  testthat::expect_identical(
    result$icu_characteristics[["Country/Network"]],
    c("Austria", "Belgium")
  )
  testthat::expect_identical(
    result$pn_incidence[["Country/Network"]],
    c("Austria", "EU/EEA")
  )
  testthat::expect_identical(
    names(result$pn_incidence),
    c("Country/Network", "Aggregated")
  )
  testthat::expect_identical(
    names(result$catheter_related_bsi),
    c(
      "Country/Network",
      "CRBSI",
      "Mean"
    )
  )
  testthat::expect_identical(
    names(result$antimicrobial_resistance),
    c(
      "Country/Network",
      "S. aureus, meticillin resistance (MRSA, %)",
      "Klebsiella spp., carbapenem resistance (%)"
    )
  )
})

testthat::test_that("hicu_prepare_report_tables summarises raw AMR records", {
  report_data <- list(
    resist = data.frame(
      ReportingCountry = c("AT", "AT", "AT", "AT", "BE", "BE"),
      ResultIsolate = c("STAAUR", "STAAUR", "KLEPNE", "KLEPNE", "ACIBAU", "ACIBAU"),
      Isolate = c(
        "Staphylococcus aureus",
        "Staphylococcus aureus",
        "Klebsiellas",
        "Klebsiellas",
        "Acinetobacter spp.",
        "Acinetobacter spp."
      ),
      Antibiotic = c("OXA", "OXA", "C3G", "CAR", "CAR", "CAR"),
      SIR = c("R", "S", "R", "S", "R", "S")
    )
  )

  result <- hicu_prepare_report_tables(report_data)

  expected <- data.frame(
    `Country/Network` = c("Austria", "Belgium"),
    `S. aureus, meticillin resistance (MRSA, %)` = c(50, NA_real_),
    `Enterococcus spp., vancomycin resistance (%)` = c(NA_real_, NA_real_),
    `P. aeruginosa, ceftazidime resistance (%)` = c(NA_real_, NA_real_),
    `E. coli, third-generation cephalosporin resistance (%)` = c(NA_real_, NA_real_),
    `Klebsiella spp., third-generation cephalosporin resistance (%)` = c(100, NA_real_),
    `Enterobacter spp., third-generation cephalosporin resistance (%)` = c(NA_real_, NA_real_),
    `Klebsiella spp., carbapenem resistance (%)` = c(0, NA_real_),
    `E. coli, carbapenem resistance (%)` = c(NA_real_, NA_real_),
    `Enterobacter spp., carbapenem resistance (%)` = c(NA_real_, NA_real_),
    `P. aeruginosa, carbapenem resistance (%)` = c(NA_real_, NA_real_),
    `Acinetobacter baumannii, carbapenem resistance (%)` = c(NA_real_, 50),
    check.names = FALSE
  )

  testthat::expect_equal(result$antimicrobial_resistance, expected)
})

# Tests for hicu_prepare_report_summaries() -------------------------------

testthat::test_that("hicu_prepare_report_summaries builds narrative scalars", {
  report_data <- list(
    haiicu_level1_all = data.frame(
      ReportingCountry = c("AT", "BE"),
      HospitalIdGlobal = c("AT H1", "BE H2"),
      UnitIdGlobal = c("AT H1 ICU1", "BE H2 ICU2"),
      UnitSize = c("10", "20")
    ),
    haiicu_pt_unit = data.frame(
      RecordId = c("P1", "P2"),
      UnitId = c("U1", "U2")
    ),
    haiicu_pt_inf_all = data.frame(
      Id = c("P1", "P1", "P2"),
      InfectionSite = c("PN", "BSI", "UTI"),
      Intubation = c("Y", "N", "N"),
      hasHai = c(TRUE, TRUE, TRUE),
      dupl_pat = c(FALSE, TRUE, FALSE),
      lengthofstay = c(5, 5, 10)
    ),
    haiicuall2 = data.frame(
      BSI_incdens = c(1.5, 2.5),
      UTI_incdens = c(3.5, 4.5)
    ),
    PNinc_EU = data.frame(PNinc = 1.1),
    BSIinc_EU = data.frame(BSIinc = 2.2),
    UTIinc_EU = data.frame(UTIinc = 3.3),
    PNtable_group_top10_pc = data.frame(Isolate = c("A", "B", "C", "D", "E")),
    BSItable_group_top10_pc = data.frame(Isolate = c("F", "G")),
    UTItable_group_top10_pc = data.frame(Isolate = "H"),
    resist = data.frame(
      ReportingCountry = "AT",
      MRSA = 50,
      CRKP = 25
    )
  )

  result <- hicu_prepare_report_summaries(report_data)

  testthat::expect_equal(result$participation$country_count, 2)
  testthat::expect_equal(result$participation$hospital_count, 2)
  testthat::expect_equal(result$participation$icu_count, 2)
  testthat::expect_equal(result$participation$unit_size_median, 15)
  testthat::expect_identical(result$participation$patient_count, 2L)
  testthat::expect_identical(result$participation$patients_with_hai, 2L)
  testthat::expect_equal(result$participation$patients_with_hai_percent, 100)
  testthat::expect_equal(result$pneumonia$cases, 1)
  testthat::expect_equal(result$pneumonia$intubation_associated_percent, 100)
  testthat::expect_equal(result$pneumonia$patient_percent, 50)
  testthat::expect_equal(result$pneumonia$incidence, 66.67)
  testthat::expect_equal(result$bloodstream$mean_icu_incidence, 2)
  testthat::expect_equal(result$urinary_tract$mean_icu_incidence, 4)
  testthat::expect_identical(result$pneumonia$top_isolates, c("A", "B", "C", "D"))
  testthat::expect_identical(result$pneumonia$top_isolates_text, "A, B, C and D")
  testthat::expect_identical(result$bloodstream$top_isolates_text, "F and G")
  testthat::expect_identical(result$urinary_tract$top_isolates_text, "H")
  testthat::expect_identical(result$antimicrobial_resistance$indicator_count, 2L)
  testthat::expect_identical(
    result$antimicrobial_resistance$indicator_text,
    "S. aureus, meticillin resistance (MRSA, %) and Klebsiella spp., carbapenem resistance (%)"
  )
})