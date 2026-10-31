test_that("a snapshot restores the exact state of a save", {
  local_store()
  e <- new.env()
  e$keep <- 1
  e$temp <- 2
  save_quiet(envir = e, annotation = "start")
  rm("temp", envir = e)
  e$keep <- 3
  e$later <- 4
  save_quiet(envir = e)

  old <- new.env()
  load_quiet(envir = old, snapshot = "start")
  expect_equal(sort(objects(old)), c("keep", "temp"))
  expect_equal(old$keep, 1)

  latest <- new.env()
  load_quiet(envir = latest, snapshot = "latest")
  expect_equal(sort(objects(latest)), c("keep", "later"))
  expect_equal(latest$keep, 3)

  by_id <- new.env()
  load_quiet(envir = by_id, snapshot = 1)
  expect_equal(by_id$temp, 2)
})

test_that("a default load warns about objects removed from the workspace", {
  local_store()
  e <- new.env()
  e$keep <- 1
  e$temp <- 2
  save_quiet(envir = e)
  rm("temp", envir = e)
  save_quiet(envir = e)
  restored <- new.env()
  expect_message(load_incremental(envir = restored), "Not in the latest snapshot.*temp")
  expect_true("temp" %in% objects(restored))
})

test_that("an annotation is kept when no object has changed", {
  local_store()
  e <- new.env()
  e$x <- 1
  save_quiet(envir = e, annotation = "v1")
  e$x <- 2
  save_quiet(envir = e)
  save_quiet(envir = e, annotation = "v2")
  snapshots <- ws_snapshots()
  expect_equal(snapshots$annotation, c("v1", NA, "v2"))
  expect_equal(nrow(ws_ref_table()), 2)
  restored <- new.env()
  load_quiet(envir = restored, snapshot = "v2")
  expect_equal(restored$x, 2)
})

test_that("a save limited to some items keeps the rest of the snapshot", {
  local_store()
  e <- new.env()
  e$a <- 1
  e$b <- 2
  save_quiet(envir = e)
  e$a <- 10
  save_quiet(items = "a", envir = e)
  restored <- new.env()
  load_quiet(envir = restored, snapshot = "latest")
  expect_equal(restored$a, 10)
  expect_equal(restored$b, 2)
})

test_that("a subset can be combined with a snapshot", {
  local_store()
  e <- new.env()
  e$a <- 1
  e$b <- 2
  save_quiet(envir = e, annotation = "start")
  e$a <- 3
  save_quiet(envir = e)
  restored <- new.env()
  suppressMessages(load_incremental(object == "a", envir = restored, snapshot = "start"))
  expect_equal(objects(restored), "a")
  expect_equal(restored$a, 1)
})

test_that("unknown snapshots give an error", {
  local_store()
  expect_equal(nrow(ws_snapshots()), 0)
  e <- new.env()
  e$a <- 1
  save_quiet(envir = e)
  expect_error(load_incremental(snapshot = 99, envir = e), "Snapshot not found")
  expect_error(load_incremental(snapshot = "nope", envir = e), "Snapshot not found")
})

test_that("snapshots report versions that are gone", {
  local_store()
  e <- new.env()
  e$x <- 1
  e$y <- 5
  save_quiet(envir = e, annotation = "start")
  e$x <- 2
  save_quiet(envir = e)
  suppressMessages(prune_ws(keep = 1, remove = TRUE))
  snapshots <- ws_snapshots()
  expect_equal(snapshots$n_objects, c(2L, 2L))
  expect_equal(snapshots$n_missing, c(1L, 0L))
  restored <- new.env()
  expect_warning(load_quiet(envir = restored, snapshot = "start"), "incomplete.*x")
  expect_equal(objects(restored), "y")
})

test_that("ws_status classifies the objects without saving", {
  local_store()
  e <- new.env()
  e$same <- 1
  e$changed <- 2
  e$gone <- 3
  save_quiet(envir = e, annotation = "start")
  e$changed <- 20
  e$fresh <- 4
  rm("gone", envir = e)
  rows <- nrow(ws_ref_table())
  status <- ws_status(envir = e)
  expect_equal(status$status[match(c("same", "changed", "gone", "fresh"), status$object)],
               c("unchanged", "modified", "not_in_workspace", "new"))
  expect_equal(nrow(ws_ref_table()), rows)
  only <- ws_status(items = "same", envir = e)
  expect_equal(only$object, "same")
})

test_that("ws_status reports the class and size of the objects", {
  local_store()
  e <- new.env()
  e$same <- 1:10
  e$changed <- 1:10
  e$gone <- letters
  save_quiet(envir = e)
  e$changed <- data.frame(a = 1:100)
  e$fresh <- factor(letters)
  rm("gone", envir = e)
  status <- ws_status(envir = e)
  rownames(status) <- status$object
  expect_equal(names(status), c("object", "status", "class", "size", "last_saved", "annotation"))
  expect_equal(status[c("same", "changed", "gone", "fresh"), "class"], c("integer", "data.frame", "character", "factor"))
  expect_equal(status["same", "size"], as.numeric(object.size(e$same)))
  # The current object, not the one in the table
  expect_equal(status["changed", "size"], as.numeric(object.size(e$changed)))
  expect_equal(status["fresh", "size"], as.numeric(object.size(e$fresh)))
  expect_equal(status["gone", "size"], as.numeric(object.size(letters)))
  expect_type(status$size, "double")
})

test_that("ws_status gets the class and size of lazy objects without reading them", {
  local_store()
  e <- new.env()
  e$x <- 1:10
  save_quiet(envir = e)
  old_hash <- ws_ref_table()$hash
  e$x <- letters
  save_quiet(envir = e)

  lazy <- new.env()
  load_quiet(envir = lazy, lazyload = TRUE)
  current <- ws_status(envir = lazy)
  expect_equal(current$status, "unchanged")
  expect_equal(current$class, "character")
  expect_true(rlang::env_binding_are_lazy(lazy, "x"))

  # A lazy version that is not the latest one is described by its own row
  old <- new.env()
  suppressMessages(load_incremental(hash == old_hash, envir = old, lazyload = TRUE))
  modified <- ws_status(envir = old)
  expect_equal(modified$status, "modified")
  expect_equal(modified$class, "integer")
  expect_equal(modified$size, as.numeric(object.size(1:10)))
  expect_true(rlang::env_binding_are_lazy(old, "x"))
})

test_that("ws_status works without a store", {
  local_store()
  e <- new.env()
  e$a <- 1
  expect_equal(ws_status(envir = e)$status, "new")
  expect_false(file.exists("ws_table.ref"))
  expect_equal(nrow(ws_status(envir = new.env())), 0)
})

test_that("lazy loading stays lazy through status, save and a second load", {
  local_store()
  e <- new.env()
  e$big <- 1:3
  save_quiet(envir = e)
  lazy <- new.env()
  result <- load_quiet(envir = lazy, lazyload = TRUE)
  expect_equal(result$action, "lazy")
  expect_true(rlang::env_binding_are_lazy(lazy, "big"))

  expect_equal(ws_status(envir = lazy)$status, "unchanged")
  expect_true(rlang::env_binding_are_lazy(lazy, "big"))
  save_quiet(envir = lazy)
  expect_true(rlang::env_binding_are_lazy(lazy, "big"))
  expect_equal(load_quiet(envir = lazy, lazyload = TRUE)$action, "unchanged")
  expect_true(rlang::env_binding_are_lazy(lazy, "big"))
  expect_equal(nrow(ws_ref_table()), 1)

  expect_equal(lazy$big, 1:3)
  expect_false(rlang::env_binding_are_lazy(lazy, "big"))
})

test_that("lazy loading stays lazy in the global environment", {
  local_store()
  e <- new.env()
  e$isaves_test_big <- 1:3
  save_quiet(envir = e)
  withr::defer(rm("isaves_test_big", envir = .GlobalEnv))
  load_quiet(lazyload = TRUE)
  is_lazy <- function() rlang::env_binding_are_lazy(.GlobalEnv, "isaves_test_big")
  expect_true(is_lazy())
  status <- ws_status()
  expect_equal(status$status[status$object == "isaves_test_big"], "unchanged")
  expect_true(is_lazy())
  save_quiet(items = "isaves_test_big")
  expect_true(is_lazy())
  expect_equal(load_quiet(lazyload = TRUE)$action, "unchanged")
  expect_true(is_lazy())
  expect_equal(get("isaves_test_big", envir = .GlobalEnv), 1:3)
})

test_that("an old version loaded lazily is recorded without reading it", {
  local_store()
  e <- new.env()
  e$x <- 1
  save_quiet(envir = e)
  old_hash <- ws_ref_table()$hash
  e$x <- 2
  save_quiet(envir = e)
  lazy <- new.env()
  suppressMessages(load_incremental(hash == old_hash, envir = lazy, lazyload = TRUE))
  save_quiet(envir = lazy)
  expect_true(rlang::env_binding_are_lazy(lazy, "x"))
  table <- ws_ref_table()
  expect_equal(nrow(table), 3)
  expect_equal(table$hash[3], old_hash)
  expect_equal(table$class[3], "numeric")
})

test_that("lazy objects survive a change of working directory", {
  local_store()
  e <- new.env()
  e$a <- 1:3
  save_quiet(envir = e)
  lazy <- new.env()
  load_quiet(envir = lazy, lazyload = TRUE)
  withr::with_dir(tempdir(), expect_equal(lazy$a, 1:3))
})

test_that("promises created by the user are hashed normally", {
  local_store()
  e <- new.env()
  delayedAssign("mine", 40 + 2, assign.env = e)
  save_quiet(envir = e)
  restored <- new.env()
  load_quiet(envir = restored)
  expect_equal(restored$mine, 42)
})
