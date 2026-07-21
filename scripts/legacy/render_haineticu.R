if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
}

render_report(output_format = Sys.getenv("HAINET_OUTPUT_FORMAT", unset = "word"))