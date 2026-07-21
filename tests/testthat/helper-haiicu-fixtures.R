helper_haiicu_inputs <- function() {
  unit <- data.frame(
    RecordId = c("U1", "U2"),
    ReportingCountry = c("AT", "IT"),
    DataSource = c("AT", "IT-GiViTI"),
    HospitalId = c("H1", "H2"),
    UnitId = c("ICU1", "ICU2"),
    HospitalSize = c("S", "M"),
    HospitalType = c("PUB", "PUB"),
    UnitSize = c("10", "99"),
    UnitSpecialty = c("MIX", "MED"),
    UnitPercentIntub = c(20, 40),
    NumAlcoholHandRubLiters = c(5, 4),
    NumPatientDaysPrevYear = c(1000, 900)
  )

  unit_light <- data.frame(
    RecordId = "L1",
    ReportingCountry = "BE",
    DataSource = "BE",
    HospitalId = "H3",
    UnitId = "ICU3",
    HospitalSize = "L",
    HospitalType = "PUB",
    UnitSize = "12",
    UnitSpecialty = "SUR",
    UnitPercentIntub = 60,
    NumAlcoholHandRubLiters = 6,
    NumPatientDaysPrevYear = 800
  )

  patient <- data.frame(
    RecordId = c("P1", "P2"),
    ParentId = c("U1", "U2"),
    DateUnitAdmission = c("2023-01-01", "01/02/2023"),
    DateUnitDischarge = c("2023-01-05", "06/02/2023"),
    Gender = c("F", "M"),
    Age = c(60, 70),
    SapsII = c(20, 30),
    PatientOrigin = c("HOSP", "COM"),
    Trauma = c("N", "Y"),
    TypeOfAdmission = c("MED", "SSUR"),
    Intubation = c("Y", "N"),
    UrinaryCatheter = c("Y", "N"),
    CVC = c("Y", "N"),
    ImpairedImmunity = c("N", "Y"),
    AntimicrobialInUnit = c("Y", "N"),
    OutcomeUnit = c("A", "D")
  )

  infection <- data.frame(
    RecordId = c("I1", "I2"),
    ParentId = c("P1", "P2"),
    InfectionSite = c("PN", "BSI"),
    BSIOrigin = c("", "C-CVC"),
    DateOfOnset = c("2023-01-03", "03/02/2023")
  )

  denominator_light <- data.frame(
    RecordId = "DL1",
    ParentId = "L1",
    AuditStart = "2023-01-01",
    AuditEnd = "2023-01-08",
    PeriodStart = "2023-01-01",
    PeriodEnd = "2023-01-31",
    NumNursingAssistHours7Days = 20,
    NumPatDays7Days = 50,
    NumRegNurseHours7Days = 80,
    NumUnitAdmission2d = 8,
    NumPatDaysUnit = 10,
    NumPatDaysUnit2d = 10
  )

  infection_light <- data.frame(
    RecordId = c("LI1", "LI2"),
    ParentId = c("DL1", "DL1"),
    InfectionSite = c("UTI", "CRI3"),
    BSIOrigin = c("", "C-CVC")
  )

  exposure <- data.frame(
    Id = c("E1", "E2", "E3", "E4", "E5", "E6"),
    RecordId = c("EXP1", "EXP2", "EXP3", "EXP4", "EXP5", "EXP6"),
    UnitId = c("U1", "U1", "U2", "U2", "U1", "U2"),
    ExpType = c("CVC", "CVC", "CVC", "UC", "UC", "INT"),
    DateExpStart = c(
      "2023-01-01", "02/01/2023", "2023-02-01", "2023-02-01",
      "2023-01-01", "2023-02-02"
    ),
    DateExpEnd = c(
      "2023-01-03", "03/01/2023", "2023-02-04", "2023-02-02",
      "2023-01-12", "2023-02-04"
    ),
    DateUnitAdmission = c(
      "2023-01-01", "2023-01-01", "2023-02-01", "2023-02-01",
      "2023-01-01", "2023-02-01"
    ),
    DateUnitDischarge = c(
      "2023-01-05", "2023-01-05", "2023-02-06", "2023-02-06",
      "2023-01-12", "2023-02-06"
    ),
    expdays = c(3, 2, 0, 2, 12, 3)
  )

  antimicrobial <- data.frame(
    RecordId = c("AM1", "AM2", "AM3", "AM4"),
    ParentId = c("P1", "P1", "P2", "P2"),
    DateAntimicrobialStart = c("2023-01-01", "2023-01-03", "2023-02-01", "2023-02-05"),
    DateAntimicrobialEnd = c("2023-01-03", "2023-01-07", "2023-02-03", "2023-02-08"),
    ATCCode = c("J01DH02", "J01CR05", "J01DD04", "J01XA01"),
    AntimicrobialIndication = c("E", "M", "P", "UNK")
  )

  denominator <- data.frame(
    RecordId = c("DS1", "DS2", "DS3"),
    ParentId = c("U1", "U2", "U1"),
    AuditStart = c("2023-01-01", "2023-02-01", "N/A"),
    AuditEnd = c("2023-01-08", "2023-02-08", "2023-01-08"),
    PeriodStart = c("2023-01-01", "2023-02-01", "2023-01-01"),
    PeriodEnd = c("2023-01-31", "2023-02-28", "2023-01-31"),
    NumNursingAssistHours7Days = c(40, 30, 999),
    NumPatDays7Days = c(70, 60, 999),
    NumRegNurseHours7Days = c(120, 100, 999),
    NumUnitAdmission2d = c(10, 9, 999)
  )

  indicator <- data.frame(
    RecordId = c("I1", "I2", "I3", "I4", "I5"),
    ParentId = c("DS1", "DS1", "DS1", "DS2", "DS2"),
    IndicatorCode = c(
      "ASTREV72H", "CVCSITDRES", "INTCUFPRES", "ASTREV72H",
      "INTORDECON"
    ),
    IndNumCompliant = c("12", "4", "UNK", "5", "2"),
    IndNumObservations = c("10", "8", "8", "10", "0")
  )

  indicator_light <- data.frame(
    RecordId = "LI1",
    ParentId = "DL1",
    IndicatorCode = "INTPOSNSUP",
    IndNumCompliant = "3",
    IndNumObservations = "6"
  )

  list(
    unit = unit,
    unit_light = unit_light,
    patient = patient,
    infection = infection,
    denominator = denominator,
    denominator_light = denominator_light,
    indicator = indicator,
    indicator_light = indicator_light,
    infection_light = infection_light,
    exposure = exposure,
    antimicrobial = antimicrobial
  )
}