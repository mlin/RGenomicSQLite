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
#' @export
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

#' Open GenomicSQLite database connection
#'
#' @param dbname Database filename.
#' @param config_json GenomicSQLite configuration JSON.
#' @param ... Other options passed through to `DBI::dbConnect`
#'
#' @return `DBI` connection object
#' @export
#' @importFrom DBI dbClearResult
#' @importFrom DBI dbConnect
#' @importFrom DBI dbGetQuery
#' @importFrom DBI dbDisconnect
#' @importFrom DBI dbSendStatement
#' @importFrom RSQLite SQLite
#'
#' @examples
dbConnect <- function(dbname, config_json = "{}", ...) {
  # get connection string
  memconn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(memconn), add = TRUE)
  uri <- DBI::dbGetQuery(
    memconn, "SELECT genomicsqlite_uri(:dbname, :config_json)",
    params = list(dbname = dbname, config_json = config_json)
  )[, 1]

  # open connection
  dbc <- DBI::dbConnect(RSQLite::SQLite(), uri, ...)

  # generate & execute tuning SQL
  tuning_sql <- DBI::dbGetQuery(
    dbc, "SELECT genomicsqlite_tuning_sql(:config_json)",
    params = list(config_json = config_json)
  )[, 1]
  # need to semicolon-split the tuning SQL script
  # https://github.com/r-dbi/DBI/issues/273
  # https://github.com/r-dbi/RSQLite/issues/313
  for (stmt in trimws(unlist(strsplit(tuning_sql, ";", fixed = TRUE)))) {
    DBI::dbClearResult(DBI::dbSendStatement(dbc, stmt))
  }

  return(dbc)
}

#' Genomics Extension version
#'
#' @param dbc GenomicSQLite connection from `RGenomicSQLite::dbConnect()``
#'
#' @return Version string.
#' @export
#' @importFrom DBI dbGetQuery
#'
#' @examples
version <- function(dbc) {
  return(DBI::dbGetQuery(dbc, "SELECT genomicsqlite_version()")[, 1])
}
