isolated_lib <- 'C:/Users/dplachouras/AppData/Local/R/devtools-library/4.5'
if (!dir.exists(isolated_lib)) dir.create(isolated_lib, recursive = TRUE)
options(repos=c(CRAN='https://cloud.r-project.org'), pkgType='binary', install.packages.compile.from.source='never')
.libPaths(c(isolated_lib, .libPaths()))
install.packages(c('fs','pak','devtools'), lib=isolated_lib, dependencies=c('Depends','Imports','LinkingTo'), type='binary')
cat("\n=== LIBPATHS ===\n")
print(.libPaths())
cat("\n=== DETAILS ===\n")
for (pkg in c('fs', 'pak', 'devtools')) {
  v <- tryCatch(as.character(packageVersion(pkg)), error=function(e) "NOT INSTALLED")
  p <- tryCatch(system.file(package=pkg), error=function(e) "NOT FOUND")
  cat(sprintf("Package: %s, Version: %s, Path: %s\n", pkg, v, p))
}
cat("\n=== CHECK REQUIRE ===\n")
cat("requireNamespace('devtools', quietly=TRUE):", requireNamespace('devtools', quietly=TRUE), "\n")
