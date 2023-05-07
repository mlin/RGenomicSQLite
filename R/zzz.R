.RGenomicSQLite_loaded <- FALSE

.onLoad <- function(libname, pkgname) {
  env <- getNamespace("RGenomicSQLite")
  if (!get(".RGenomicSQLite_loaded", envir = env, inherits = FALSE)) {
    load_libgenomicsqlite()
    assign(".RGenomicSQLite_loaded", TRUE, envir = env)
  }
}

#' @noRd
#' @export
find_libgenomicsqlite <- function() {
  libgenomicsqlite <- Sys.getenv("LIBGENOMICSQLITE")
  if (nzchar(libgenomicsqlite)) {
    return(libgenomicsqlite)
  }
  sysinfo <- Sys.info()
  if (sysinfo["machine"] == "x86_64") {
    ext <- ""
    if (sysinfo["sysname"] == "Linux") {
      ext <- "so"
    } else if (sysinfo["sysname"] == "Darwin") {
      ext <- "dylib"
    }
    if (nzchar(ext)) {
      return(system.file(
        "extdata", paste("libgenomicsqlite", ext, sep = "."),
        package = "RGenomicSQLite", mustWork = TRUE
      ))
    }
  }
  return("libgenomicsqlite")
}

#' @noRd
#' @importFrom DBI dbConnect
#' @importFrom DBI dbGetQuery
#' @importFrom DBI dbDisconnect
#' @importFrom RSQLite SQLite
load_libgenomicsqlite <- function() {
  libgenomicsqlite_file <- find_libgenomicsqlite()
  memconn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", loadable.extensions = TRUE)
  on.exit(DBI::dbDisconnect(memconn), add = TRUE)
  DBI::dbGetQuery(
    memconn, "SELECT load_extension(:x)",
    params = list(x = libgenomicsqlite_file)
  )
  invisible()
}
