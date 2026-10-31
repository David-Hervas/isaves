annotated_store <- function(){
  e <- new.env()
  e$a <- 1
  save_quiet(envir = e, annotation = "v1")
  e$b <- 2
  save_quiet(envir = e)
  e$d <- 3
  save_quiet(envir = e)
  e
}

test_that("NA in a subset selects nothing in every function", {
  local_store()
  annotated_store()
  expect_equal(ws_ref_table(annotation == "v1")$object, "a")
  expect_equal(suppressMessages(purge_ws_table(annotation == "v1"))$object, "a")
  expect_equal(annotate_ws(annotation == "v1", "new")$object, "a")
  restored <- new.env()
  suppressMessages(load_incremental(annotation == "v1", envir = restored))
  expect_equal(objects(restored), "a")
})

test_that("purging with NA in the subset keeps the other rows", {
  local_store()
  annotated_store()
  suppressMessages(purge_ws_table(annotation == "v1", remove = TRUE))
  expect_equal(ws_ref_table()$object, c("b", "d"))
  expect_length(list.files("rds"), 2)
  restored <- new.env()
  load_quiet(envir = restored)
  expect_equal(objects(restored), c("b", "d"))
})

test_that("annotate_ws writes with NA in the subset", {
  local_store()
  annotated_store()
  annotate_ws(annotation == "v1", "v2", write = TRUE)
  table <- ws_ref_table()
  expect_equal(table$annotation, c("v2", NA, NA))
})

test_that("annotate_ws does not write by default and requires an annotation", {
  local_store()
  annotated_store()
  proposed <- annotate_ws(object == "b", "proposal")
  expect_equal(proposed$annotation, "proposal")
  expect_true(is.na(ws_ref_table(object == "b")$annotation))
  expect_error(annotate_ws(object == "a"), "annotation")
})

test_that("subset can use variables of the caller", {
  local_store()
  annotated_store()
  wanted <- c("a", "b")
  expect_equal(ws_ref_table(object %in% wanted)$object, wanted)
  inside <- function(name) ws_ref_table(object == name)$object
  expect_equal(inside("d"), "d")
  restored <- new.env()
  suppressMessages(load_incremental(object %in% wanted, envir = restored))
  expect_equal(objects(restored), wanted)
})

test_that("an invalid subset gives a clear error", {
  local_store()
  annotated_store()
  expect_error(ws_ref_table("a"), "logical")
  expect_error(ws_ref_table(c(TRUE, FALSE)), "one value per row")
})

test_that("functions report the absence of metadata", {
  local_store()
  expect_error(load_incremental(), "No metadata")
  expect_error(purge_ws_table(), "No metadata")
  expect_error(annotate_ws(annotation = "x"), "No metadata")
  expect_error(prune_ws(), "No metadata")
  expect_error(verify_ws(), "No metadata")
  expect_message(result <- ws_ref_table(), "No previous metadata")
  expect_null(result)
})

test_that("a dry-run purge changes nothing and says so", {
  local_store()
  annotated_store()
  expect_message(selected <- purge_ws_table(object == "a"), "would be purged")
  expect_equal(selected$object, "a")
  expect_equal(nrow(ws_ref_table()), 3)
  expect_length(list.files("rds"), 3)
})

test_that("purging everything needs to be explicit", {
  local_store()
  e <- annotated_store()
  expect_error(purge_ws_table(remove = TRUE), "subset = TRUE")
  expect_equal(nrow(ws_ref_table()), 3)
  suppressMessages(purge_ws_table(TRUE, remove = TRUE))
  expect_equal(nrow(ws_ref_table()), 0)
  expect_length(list.files("rds"), 0)
  # The store is still usable after that
  save_quiet(envir = e)
  expect_equal(nrow(ws_ref_table()), 3)
})

test_that("a file is kept while another row uses it", {
  local_store()
  e <- new.env()
  e$a <- 1
  e$b <- 1
  save_quiet(envir = e)
  suppressMessages(purge_ws_table(object == "a", remove = TRUE))
  expect_length(list.files("rds"), 1)
  restored <- new.env()
  load_quiet(envir = restored)
  expect_equal(restored$b, 1)
})

test_that("purging the latest version warns about the older one", {
  local_store()
  e <- new.env()
  e$x <- "v1"
  save_quiet(envir = e)
  e$x <- "v2"
  save_quiet(envir = e)
  latest <- ws_ref_table()$hash[2]
  expect_message(purge_ws_table(hash == latest, remove = TRUE), "older version is now the latest one for: x")
})

test_that("prune_ws keeps the most recent versions of each object", {
  local_store()
  e <- new.env()
  e$other <- "fixed"
  for(i in 1:4){
    e$x <- i
    save_quiet(envir = e)
  }
  expect_error(prune_ws(keep = 0), "at least 1")
  expect_message(selected <- prune_ws(keep = 2), "would be removed")
  expect_equal(nrow(selected), 2)
  expect_equal(nrow(ws_ref_table()), 5)
  suppressMessages(prune_ws(keep = 2, remove = TRUE))
  table <- ws_ref_table()
  expect_equal(sum(table$object == "x"), 2)
  expect_equal(sum(table$object == "other"), 1)
  expect_length(list.files("rds"), 3)
  restored <- new.env()
  load_quiet(envir = restored)
  expect_equal(restored$x, 4)
})
