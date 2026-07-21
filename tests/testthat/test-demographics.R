# Tests for summarise_country_demographics() ------------------------------

testthat::test_that("summarise_country_demographics calculates country metrics", {
  patient_unit <- data.frame(
    ReportingCountry = c("AT", "AT", "BE"),
    los = c(5, 7, 4),
    Gender = c("F", "M", "F"),
    Age = c(60, 70, 80),
    SapsII = c(20, 30, 40),
    PatientOrigin = c("HOSP", "COM", "HOSP"),
    Trauma = c("N", "Y", "N"),
    TypeOfAdmission = c("MED", "SSUR", "USUR"),
    Intubation = c("Y", "N", "Y"),
    UrinaryCatheter = c("Y", "N", "Y"),
    CVC = c("Y", "N", "Y"),
    ImpairedImmunity = c("N", "Y", "N"),
    AntimicrobialInUnit = c("Y", "N", "Y"),
    OutcomeUnit = c("A", "D", "D")
  )

  result <- summarise_country_demographics(patient_unit)

  expected <- data.frame(
    ReportingCountry = c("AT", "BE"),
    N_pat = c(2L, 1L),
    patdays = c(12, 4),
    avg_los = c(6, 4),
    Gender_F_pc = c(50, 100),
    Age_median = c(65, 80),
    SapsII_median = c(25, 40),
    Origin_HOSP_pc = c(50, 100),
    Trauma_pc = c(50, 0),
    TypeAdm_med = c(50, 0),
    TypeAdm_ssur = c(50, 0),
    TypeAdm_usur = c(0, 100),
    Intub_pc = c(50, 100),
    UrinCath_pc = c(50, 100),
    CVC_pc = c(50, 100),
    ImpImmun_pc = c(50, 0),
    AntimicrUnit_pc = c(50, 100),
    Outcome_D_pc = c(50, 100)
  )

  testthat::expect_equal(result, expected)
})