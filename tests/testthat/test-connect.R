test_that("genomicsqliteConnect", {
  dbfn <- tempfile()
  on.exit(unlink(dbfn), add = TRUE)
  dbc <- genomicsqliteConnect(dbfn, inner_page_KiB = 2)
  v <- genomicsqliteVersion(dbc)
  expect_true(nzchar(v))
  message(paste("GenomicSQLite", v))
  expect_equal(DBI::dbGetQuery(dbc, "PRAGMA page_size")[, 1], 2048)
  DBI::dbExecute(dbc, "CREATE TABLE test(x INTEGER)")
  DBI::dbDisconnect(dbc)

  # mix of GenomicSQLite & dbConnect arguments
  dbc <- genomicsqliteConnect(dbfn, threads = 7, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(dbc), add = TRUE)
  expect_error(
    DBI::dbExecute(dbc, "CREATE TABLE test2(x INTEGER)"),
    "attempt to write a readonly database"
  )
})
