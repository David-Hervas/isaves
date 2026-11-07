test_that("a corrupt table stops with a clear error and keeps a backup", {
  local_store()
  e <- new.env()
  e$a <- 1
  save_quiet(envir = e)
  e$b <- 2
  save_quiet(envir = e)
  writeBin(as.raw(1:50), "ws_table.ref")
  corrupt <- readBin("ws_table.ref", "raw", 50)
  expect_error(ws_ref_table(), "cannot be read.*ws_table.ref.bak")
  expect_error(save_incremental(envir = e), "cannot be read")
  expect_identical(readBin("ws_table.ref", "raw", 50), corrupt)
  file.copy("ws_table.ref.bak", "ws_table.ref", overwrite = TRUE)
  expect_equal(ws_ref_table()$object, "a")
})

test_that("no temporary files are left behind", {
  local_store()
  e <- new.env()
  e$a <- 1
  save_quiet(envir = e)
  expect_length(list.files("rds", pattern = "isaves_tmp", all.files = TRUE), 0)
  expect_length(list.files(".", pattern = "isaves_tmp", all.files = TRUE), 0)
})

test_that("a failed write leaves the target untouched", {
  local_store()
  writeLines("before", "target")
  expect_error(isaves:::atomic_write(function(tmp) stop("interrupted"), "target"), "interrupted")
  expect_equal(readLines("target"), "before")
  expect_length(list.files(".", pattern = "isaves_tmp", all.files = TRUE), 0)
})

test_that("absolute and nested folders work", {
  dir <- local_store()
  other <- withr::local_tempdir()
  e <- new.env()
  e$a <- 1
  save_quiet(envir = e, rds.folder = file.path(other, "objects"))
  expect_length(list.files(file.path(other, "objects")), 1)
  e$b <- 2
  save_quiet(envir = e, metadata.file = "nested/dir/table.ref", rds.folder = "a/b/c")
  expect_length(list.files("a/b/c"), 2)
  expect_true(file.exists("nested/dir/table.ref"))
  restored <- new.env()
  load_quiet(envir = restored, metadata.file = "nested/dir/table.ref", rds.folder = "a/b/c/")
  expect_equal(objects(restored), c("a", "b"))
  absolute <- file.path(other, "table.ref")
  save_quiet(envir = e, metadata.file = absolute)
  expect_true(file.exists(absolute))
  expect_length(list.files(file.path(other, "rds")), 2)
})

test_that("the root is found from a subfolder", {
  dir <- local_store()
  e <- new.env()
  e$a <- 1
  save_quiet(envir = e)
  dir.create("sub/deeper", recursive = TRUE)
  withr::with_dir("sub/deeper", {
    expect_equal(normalizePath(ws_root(), "/"), normalizePath(dir, "/"))
    expect_equal(ws_ref_table()$object, "a")
    e$b <- 2
    save_quiet(envir = e)
  })
  expect_equal(ws_ref_table()$object, c("a", "b"))
  expect_false(file.exists("sub/deeper/ws_table.ref"))
})

test_that("the isaves.root option fixes the root", {
  local_store()
  fixed <- withr::local_tempdir()
  withr::local_options(list(isaves.root = fixed))
  expect_equal(normalizePath(ws_root(), "/"), normalizePath(fixed, "/"))
})

test_that("starting or changing store is announced", {
  dir <- local_store()
  e <- new.env()
  e$a <- 1
  expect_message(save_incremental(envir = e), "creating a new store")
  other <- withr::local_tempdir()
  file.create(file.path(other, "other.Rproj"))
  withr::with_dir(other, expect_message(ws_root(), "now using the store"))
  expect_message(ws_root(), "now using the store")
})

test_that("verify_ws detects problems and removes orphans", {
  local_store()
  e <- new.env()
  e$a <- 1
  e$b <- 2
  e$d <- 3
  save_quiet(envir = e)
  expect_message(report <- verify_ws(deep = TRUE), "consistent")
  expect_true(report$ok)

  table <- ws_ref_table()
  file_of <- function(name) file.path("rds", paste(table$hash[table$object == name], table$format[table$object == name], sep = "."))
  file.remove(file_of("a"))
  writeBin(as.raw(1:50), file_of("b"))
  orphan <- file.path("rds", paste0(strrep("ab", 16), ".qs2"))
  file.create(orphan)
  writeLines("not ours", file.path("rds", "notes.txt"))

  shallow <- suppressMessages(verify_ws())
  expect_false(shallow$ok)
  expect_equal(shallow$missing$object, "a")
  expect_equal(nrow(shallow$corrupt), 0)
  expect_equal(shallow$orphans, basename(orphan))

  deep <- suppressMessages(verify_ws(deep = TRUE))
  expect_equal(deep$corrupt$object, "b")

  suppressMessages(verify_ws(remove.orphans = TRUE))
  expect_false(file.exists(orphan))
  expect_true(file.exists(file.path("rds", "notes.txt")))
})

test_that("removed orphans do not count as a problem", {
  local_store()
  e <- new.env()
  e$a <- 1
  save_quiet(envir = e)
  orphan <- file.path("rds", paste0(strrep("ab", 16), ".qs2"))
  file.create(orphan)
  expect_false(suppressMessages(verify_ws())$ok)
  expect_message(report <- verify_ws(remove.orphans = TRUE), "consistent")
  expect_true(report$ok)
  expect_equal(report$orphans, basename(orphan))
  expect_false(file.exists(orphan))
})

test_that("the number of threads is limited", {
  withr::local_options(isaves.nthreads = NULL)
  expect_equal(isaves:::qs_threads(NA), 1L)
  expect_equal(isaves:::qs_threads(10), 1L)
  withr::local_envvar(`_R_CHECK_LIMIT_CORES_` = "TRUE")
  expect_lte(isaves:::qs_threads(), 2L)
  withr::local_options(isaves.nthreads = 3L)
  expect_equal(isaves:::qs_threads(), 3L)
})

test_that("verify_ws detects a corrupt file with a changed hash", {
  local_store()
  e <- new.env()
  e$a <- 1
  save_quiet(envir = e, format = "rds")
  table <- ws_ref_table()
  saveRDS("something else", file.path("rds", paste0(table$hash, ".rds")))
  expect_equal(suppressMessages(verify_ws())$ok, TRUE)
  expect_equal(suppressMessages(verify_ws(deep = TRUE))$corrupt$object, "a")
})

test_that("stores of previous versions keep working", {
  local_store()
  legacy_store(list(a = 1:3, b = "text", f = mean))
  table <- ws_ref_table()
  expect_equal(table$algo, rep("md5", 3))
  expect_equal(table$format, rep("rds", 3))

  restored <- new.env()
  load_quiet(envir = restored)
  expect_equal(objects(restored), c("a", "b", "f"))
  expect_equal(restored$a, 1:3)

  # Unchanged objects are recognized with the algorithm they were saved with
  expect_true(all(ws_status(envir = restored)$status == "unchanged"))
  save_quiet(envir = restored)
  expect_equal(nrow(ws_ref_table()), 3)

  restored$a <- 4:6
  save_quiet(envir = restored)
  table <- ws_ref_table()
  expect_equal(nrow(table), 4)
  expect_equal(table$algo[4], "spookyhash")
  expect_equal(table$format[4], "qs2")

  final <- new.env()
  load_quiet(envir = final)
  expect_equal(final$a, 4:6)
  expect_true(suppressMessages(verify_ws(deep = TRUE))$ok)
})

test_that("tables damaged by older versions can be used again", {
  local_store()
  legacy_store(list(a = 1, b = 2))
  table <- readRDS("ws_table.ref")
  saveRDS(rbind(table, table[NA_integer_, ]), "ws_table.ref")
  expect_message(result <- ws_ref_table(), "ignoring 1 invalid")
  expect_equal(result$object, c("a", "b"))
})

test_that("old tables with factors or without recent columns are read", {
  local_store()
  legacy_store(list(a = 1))
  table <- readRDS("ws_table.ref")
  table$object <- factor(table$object)
  table$user <- NULL
  table$annotation <- NULL
  table$custom <- "kept"
  saveRDS(table, "ws_table.ref")
  e <- new.env()
  load_quiet(envir = e)
  e$new_object <- 2
  save_quiet(envir = e)
  result <- ws_ref_table()
  expect_type(result$object, "character")
  expect_equal(result$custom, c("kept", NA))
})

test_that("columns added to old tables have their usual types", {
  local_store()
  legacy_store(list(a = 1))
  table <- ws_ref_table()
  expect_type(table$save_id, "integer")
  expect_type(table$user, "character")
  expect_type(table$algo, "character")
})

test_that("a store created with only rows and no snapshots supports snapshots from then on", {
  local_store()
  legacy_store(list(a = 1, b = 2))
  e <- new.env()
  e$a <- 10
  save_quiet(items = "a", envir = e, annotation = "first new")
  restored <- new.env()
  load_quiet(envir = restored, snapshot = "first new")
  expect_equal(restored$a, 10)
  expect_equal(restored$b, 2)
})

test_that("simultaneous saves do not lose rows", {
  skip_on_cran()
  dir <- local_store()
  lib <- dirname(system.file(package = "isaves"))
  # The workers need an installed copy, which devtools::test() does not provide
  skip_if_not(file.exists(file.path(lib, "isaves", "Meta", "package.rds")))
  writeLines(c(
    sprintf(".libPaths(c('%s', .libPaths()))", normalizePath(lib, "/")),
    "library(isaves)",
    "id <- commandArgs(TRUE)[1]",
    "while (!file.exists('go.flag')) Sys.sleep(0.01)",
    "e <- new.env()",
    "for (i in 1:5) { n <- paste0('o_', id, '_', i); assign(n, runif(50), envir = e); suppressMessages(save_incremental(items = n, envir = e)) }",
    "file.create(paste0('done_', id))"), "worker.R")
  rscript <- file.path(R.home("bin"), "Rscript")
  for(id in 1:4) system2(rscript, c("--vanilla", "worker.R", id), wait = FALSE, stdout = FALSE, stderr = FALSE)
  Sys.sleep(2)
  file.create("go.flag")
  start <- Sys.time()
  while(length(list.files(pattern = "^done_")) < 4 && difftime(Sys.time(), start, units = "secs") < 120) Sys.sleep(0.2)
  expect_length(list.files(pattern = "^done_"), 4)
  expect_equal(nrow(ws_ref_table()), 20)
  expect_length(list.files("rds"), 20)
})
