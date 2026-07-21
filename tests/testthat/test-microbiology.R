# Tests for recode_microorganism_isolate() --------------------------------

testthat::test_that("recode_microorganism_isolate maps known isolate codes", {
  codes <- c("PSEAER", "STAAUR", "KLEPNE", "ESCCOL", "_NOEXA")

  isolates <- recode_microorganism_isolate(codes, infection_group = "PN")

  testthat::expect_identical(
    isolates,
    c(
      "Pseudomonas aeruginosa",
      "Staphylococcus aureus",
      "Klebsiella spp.",
      "Escherichia coli",
      NA_character_
    )
  )
})

testthat::test_that("recode_microorganism_isolate applies BSI staphylococci rule", {
  isolates <- recode_microorganism_isolate(
    c("STAEPI", "STAAUR"),
    infection_group = "BSI"
  )

  testthat::expect_identical(
    isolates,
    c("Coagulase-negative staphylococci", "Staphylococcus aureus")
  )
})

# Tests for prepare_microorganism_records() -------------------------------

testthat::test_that("prepare_microorganism_records removes sentinel values", {
  records <- data.frame(
    ReportingCountry = c("AT", "AT", "BE"),
    ResultIsolate = c("PSEAER", "_NOEXA", "_STERI")
  )

  prepared <- prepare_microorganism_records(records, infection_group = "PN")

  expected <- data.frame(
    ReportingCountry = "AT",
    ResultIsolate = "PSEAER",
    Isolate = "Pseudomonas aeruginosa"
  )

  testthat::expect_equal(prepared, expected)
})

# Tests for build_microorganism_top_country_tables() ----------------------

testthat::test_that("build_microorganism_top_country_tables creates pc table", {
  records <- data.frame(
    ReportingCountry = c("AT", "AT", "BE", "BE", "BE"),
    ResultIsolate = c("PSEAER", "PSEAER", "STAAUR", "KLEPNE", "_NOEXA")
  )

  tables <- build_microorganism_top_country_tables(
    records,
    infection_group = "PN",
    n = 3
  )

  testthat::expect_contains(names(tables), "summary")
  testthat::expect_contains(names(tables), "totals")
  testthat::expect_contains(names(tables), "pc")
  testthat::expect_contains(tables$pc$Isolate, "Pseudomonas aeruginosa")
  testthat::expect_contains(names(tables$pc), "Austria")
  testthat::expect_contains(names(tables$pc), "Belgium")
})