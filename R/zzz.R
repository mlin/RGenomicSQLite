.RGenomicSQLite_loaded <- FALSE

.onLoad <- function(libname, pkgname) {
  env <- getNamespace("RGenomicSQLite")
  if (!get(".RGenomicSQLite_loaded", envir = env, inherits = FALSE)) {
    load_libgenomicsqlite()
    assign(".RGenomicSQLite_loaded", TRUE, envir = env)
  }
}
