# Numeric helpers -------------------------------------------------------

prc <- function(x) {
  100 * round(x, digits = 3)
}

pct <- function(x, tot) {
  prc(sum(x, na.rm = TRUE) / tot)
}

unfactor <- function(x) {
  as.numeric(as.character(x))
}

p25 <- function(x) {
  stats::quantile(x, c(0.25), na.rm = TRUE)
}

p75 <- function(x) {
  stats::quantile(x, c(0.75), na.rm = TRUE)
}

# Package-safe helper aliases used by tests and newer functional wrappers.
hicu_prc <- function(x) {
  prc(x)
}

hicu_pct <- function(x, tot) {
  pct(x, tot)
}