#' Genomics Extension version
#'
#' @param dbc GenomicSQLite connection from `RGenomicSQLite::dbConnect()``
#'
#' @return Version string.
#' @export
#' @importFrom DBI dbGetQuery
#'
#' @examples
genomicsqliteVersion <- function(dbc) {
  return(DBI::dbGetQuery(dbc, "SELECT genomicsqlite_version()")[, 1])
}
