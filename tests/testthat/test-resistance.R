# Tests for prepare_resistance_records() ----------------------------------

testthat::test_that("prepare_resistance_records normalizes isolates and antibiotics", {
  records <- data.frame(
    ParentId = c("I1", "I2", "I3", "I4"),
    ReportingCountry = c("AT", "AT", "DE", "BE"),
    ResultIsolate = c("KLEPNE", "ENCFAE", "STAAUR", "PSEAER"),
    Antibiotic = c("CTX", "VAN", "OXA", "IPM"),
    SIR = c("R", "S", "R", "UNK")
  )

  prepared <- prepare_resistance_records(records)

  testthat::expect_identical(prepared$ReportingCountry, c("AT", "AT"))
  testthat::expect_identical(as.character(prepared$Isolate), c("Klebsiellas", "Enterococcus"))
  testthat::expect_identical(prepared$Antibiotic, c("C3G", "GLY"))
})

# Tests for summarise_resistance_indicators() -----------------------------

testthat::test_that("summarise_resistance_indicators calculates country rates", {
  records <- data.frame(
    ReportingCountry = c("AT", "AT", "AT", "AT", "AT", "BE", "BE"),
    ResultIsolate = c(
      "STAAUR", "STAAUR", "KLEPNE", "KLEPNE", "ESCCOL", "ACIBAU",
      "ACIBAU"
    ),
    Isolate = c(
      "Staphylococcus aureus", "Staphylococcus aureus", "Klebsiellas",
      "Klebsiellas", "Escherichia coli", "Acinetobacter spp.",
      "Acinetobacter spp."
    ),
    Antibiotic = c("OXA", "OXA", "C3G", "CAR", "CAR", "CAR", "CAR"),
    SIR = c("R", "S", "R", "S", "R", "R", "S")
  )

  summary <- summarise_resistance_indicators(records)

  expected <- data.frame(
    ReportingCountry = c("AT", "BE"),
    MRSA = c(50, NA_real_),
    VRE = c(NA_real_, NA_real_),
    CEFRPS = c(NA_real_, NA_real_),
    C3GREC = c(NA_real_, NA_real_),
    C3GRKP = c(100, NA_real_),
    C3GRENT = c(NA_real_, NA_real_),
    CRKP = c(0, NA_real_),
    CREC = c(100, NA_real_),
    CRENT = c(NA_real_, NA_real_),
    CRPS = c(NA_real_, NA_real_),
    CRAB = c(NA_real_, 50)
  )

  testthat::expect_equal(summary, expected)
})

testthat::test_that("summarise_resistance_indicators calculates EU rates", {
  records <- data.frame(
    ReportingCountry = c("AT", "BE"),
    ResultIsolate = c("PSEAER", "PSEAER"),
    Isolate = c("Pseudomonas aeruginosa", "Pseudomonas aeruginosa"),
    Antibiotic = c("CAR", "CAR"),
    SIR = c("R", "S")
  )

  summary <- summarise_resistance_indicators(records, by_country = FALSE)

  testthat::expect_identical(summary$CRPS, 50)
})