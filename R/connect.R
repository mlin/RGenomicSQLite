#' Open GenomicSQLite database connection
#'
#' @param dbname Database filename.
#' @param ... Named arguments for the GenomicSQLite configuration JSON, or otherwise
#'            to be passed through to `DBI::dbConnect`.
#'
#' @return `DBI` connection object
#' @export
#' @importFrom DBI dbClearResult
#' @importFrom DBI dbConnect
#' @importFrom DBI dbGetQuery
#' @importFrom DBI dbDisconnect
#' @importFrom DBI dbSendStatement
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom RSQLite SQLite
#'
#' @examples
genomicsqliteConnect <- function(dbname, ...) {
  memconn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(memconn), add = TRUE)

  # partition the arguments into GenomicSQLite configuration settings & passthrough
  # arguments for DBI::dbConnect. The former match keys in the GenomicSQLite default
  # configuration JSON.
  args <- list(...)
  config <- list()
  passthrough_args <- list()
  config_defaults <- jsonlite::fromJSON(DBI::dbGetQuery(
    memconn, "SELECT genomicsqlite_default_config_json()"
  )[, 1])
  for (key in names(args)) {
    if (key %in% names(config_defaults)) {
      config[[key]] <- args[[key]]
    } else {
      passthrough_args[[key]] <- args[[key]]
    }
  }
  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE)

  # get connection string
  uri <- DBI::dbGetQuery(
    memconn, "SELECT genomicsqlite_uri(:dbname, :config_json)",
    params = list(dbname = dbname, config_json = config_json)
  )[, 1]

  # open connection
  dbc <- do.call(DBI::dbConnect, c(list(RSQLite::SQLite(), uri), passthrough_args))

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
