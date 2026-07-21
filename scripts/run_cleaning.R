#!/usr/bin/env Rscript

if (requireNamespace("devtools", quietly = TRUE)) {
	devtools::load_all(quiet = TRUE)
} else {
	source("R/haiicu_functions.r")
	source("R/project_paths.R")
	source("R/inputs.R")
	source("R/incidence_tables.R")
	source("R/prepare_core.R")
	source("R/workflow.R")
}

year <- hicu_default_year()
run_haiicu_workflow(year = year)
