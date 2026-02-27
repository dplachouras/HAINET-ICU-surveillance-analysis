allunits<-readRDS("haiicu_level1.all.Rda")

reporting_median_date <- with(allunits, median(DateUsedForStatistics, na.rm = TRUE))
n_countries <- with(allunits, nlevels(ReportingCountry))
n_hospitals <- with(allunits, nlevels(HospitalIdGlobal))
n_icus <- with(allunits, nlevels(UnitIdGlobal))

reporting_countries_tbl <- with(allunits, addmargins(table(Subject, ReportingCountry), 2, FUN = sum))
icu_specialty_tbl <- with(allunits, table(Subject, UnitSpecialty))

icu_size_median <- with(allunits, median(as.numeric(UnitSize), na.rm = TRUE))
icu_size_min <- with(allunits, min(as.numeric(UnitSize), na.rm = TRUE))
icu_size_max <- with(allunits, max(as.numeric(UnitSize), na.rm = TRUE))
