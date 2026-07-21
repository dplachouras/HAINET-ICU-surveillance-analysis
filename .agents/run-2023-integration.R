Sys.setenv(HAINET_YEAR = "2023")
devtools::load_all(quiet = TRUE)

result <- run_haiicu_workflow(year = "2023", write_outputs = FALSE)

summarize_object <- function(x) {
	if (is.data.frame(x)) {
		return(paste0(class(x)[1], "[", nrow(x), "x", ncol(x), "]"))
	}

	if (is.list(x)) {
		return(paste0(class(x)[1], "[len=", length(x), "]"))
	}

	paste0(class(x)[1], "[len=", length(x), "]")
}

cat("INTEGRATION_STATUS=PASS\n")
cat("YEAR=", result$year, "\n", sep = "")
cat("DATA_DIR=", result$data_dir, "\n", sep = "")
cat("OUTPUT_DIR=", result$output_dir, "\n", sep = "")
cat("WRITE_OUTPUTS=FALSE\n")
cat("OUTPUT_COUNT=", length(result), "\n", sep = "")
cat("OUTPUT_NAMES=", paste(names(result), collapse = ","), "\n", sep = "")
cat("OUTPUT_SUMMARY_BEGIN\n")
for (name in names(result)) {
	cat(name, ":", summarize_object(result[[name]]), "\n")
}
cat("OUTPUT_SUMMARY_END\n")