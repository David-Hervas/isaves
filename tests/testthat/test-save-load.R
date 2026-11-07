test_that("objects named like functions or internal variables keep their value", {
  local_store()
  names <- c("c", "t", "table", "mean", "file", "date", "letters", "T", "names", "path", "items", "annotation",
             "metadata.file", "rds.folder", "hashes", "to_save", "state", "store", "ws", "metadata", "value", "i", "x")
  e <- new.env()
  for(name in names) assign(name, data.frame(mine = name), envir = e)
  save_quiet(envir = e)
  restored <- new.env()
  load_quiet(envir = restored)
  for(name in names){
    expect_identical(get(name, envir = restored, inherits = FALSE), data.frame(mine = name), info = name)
  }
})

test_that("identical objects are all restored and share one file", {
  local_store()
  e <- new.env()
  e$a <- 1:10
  e$b <- 1:10
  save_quiet(envir = e)
  expect_length(list.files("rds"), 1)
  restored <- new.env()
  load_quiet(envir = restored)
  expect_setequal(objects(restored), c("a", "b"))
  expect_equal(restored$b, 1:10)
})

test_that("a renamed object is recorded", {
  local_store()
  e <- new.env()
  e$a <- rnorm(5)
  save_quiet(envir = e)
  e$b <- e$a
  rm("a", envir = e)
  save_quiet(envir = e)
  restored <- new.env()
  load_quiet(envir = restored)
  expect_true("b" %in% objects(restored))
})

test_that("going back to a previous value is a new latest version", {
  local_store()
  e <- new.env()
  for(value in c(1, 2, 1)){
    e$x <- value
    save_quiet(envir = e)
  }
  expect_equal(nrow(ws_ref_table()), 3)
  expect_length(list.files("rds"), 2)
  restored <- new.env()
  load_quiet(envir = restored)
  expect_equal(restored$x, 1)
})

test_that("saving without changes adds nothing and returns no rows", {
  local_store()
  e <- new.env()
  e$x <- 1
  first <- save_quiet(envir = e)
  expect_equal(nrow(first), 1)
  expect_message(second <- save_incremental(envir = e), "No new objects")
  expect_equal(nrow(second), 0)
  expect_equal(nrow(ws_ref_table()), 1)
})

test_that("an empty environment can be saved", {
  local_store()
  expect_message(save_incremental(envir = new.env()), "No new objects")
})

test_that("save_incremental validates its arguments", {
  local_store()
  e <- new.env()
  e$a <- 1
  expect_error(save_incremental(items = "not_there", envir = e), "not found")
  # Must not find base::table through inheritance
  expect_error(save_incremental(items = "table", envir = e), "not found")
  expect_error(save_incremental(annotation = c("u", "v"), envir = e), "single value")
  expect_error(save_incremental(format = "csv", envir = e))
  save_quiet(annotation = NULL, envir = e)
  expect_true(is.na(ws_ref_table()$annotation))
  # An environment given as the first argument
  expect_error(save_incremental(e), "character vector")
  expect_error(save_incremental(items = NA_character_, envir = e), "character vector")
  expect_error(ws_status(e), "character vector")
})

test_that("repeated items are saved and shown once", {
  local_store()
  e <- new.env()
  e$a <- 1
  save_quiet(items = c("a", "a"), envir = e)
  expect_equal(nrow(ws_ref_table()), 1)
  expect_equal(ws_snapshots()$n_objects, 1)
  e$a <- 2
  status <- ws_status(items = c("a", "a"), envir = e)
  expect_equal(status$object, "a")
  expect_equal(status$status, "modified")
})

test_that("ws_status of an empty environment has the usual column types", {
  local_store()
  status <- ws_status(envir = new.env())
  expect_equal(nrow(status), 0)
  expect_type(status$status, "character")
})

test_that("a save records objects that another session removed while it was not locked", {
  local_store()
  e <- new.env()
  e$x <- "first"
  e$y <- 1
  save_quiet(envir = e)
  e$y <- 2
  lock <- isaves:::with_store_lock
  with_mocked_bindings(
    save_quiet(envir = e),
    with_store_lock = function(store, code){
      # The other session purges x after this one compared the workspace
      state <- isaves:::read_store(store)
      isaves:::remove_rows(store, state, state$table$object == "x")
      lock(store, code)
    })
  expect_equal(ws_ref_table(object == "x")$save_id, 2)
  restored <- new.env()
  load_quiet(envir = restored, snapshot = "latest")
  expect_equal(restored$x, "first")
  expect_true(suppressMessages(verify_ws(deep = TRUE))$ok)
})

test_that("local variables can be saved with envir", {
  local_store()
  saver <- function(){
    local_variable <- 1
    save_quiet(items = "local_variable", envir = environment())
  }
  saver()
  expect_equal(ws_ref_table()$object, "local_variable")
})

test_that("comments of several lines are kept complete", {
  local_store()
  e <- new.env()
  e$a <- 1
  comment(e$a) <- c("l1", "l2")
  save_quiet(envir = e)
  expect_equal(ws_ref_table()$comment, "l1\nl2")
})

test_that("both formats work and existing versions keep their file", {
  local_store()
  e <- new.env()
  e$r <- 1
  save_quiet(envir = e, format = "rds")
  e$q <- 2
  save_quiet(envir = e, format = "qs2")
  expect_setequal(tools::file_ext(list.files("rds")), c("rds", "qs2"))
  e$also_r <- 1
  save_quiet(envir = e, format = "qs2")
  expect_length(list.files("rds"), 2)
  restored <- new.env()
  load_quiet(envir = restored)
  expect_equal(mget(c("also_r", "q", "r"), envir = restored), list(also_r = 1, q = 2, r = 1))
})

test_that("different content under the same name is loaded with another name", {
  local_store()
  e <- new.env()
  e$x <- 1
  save_quiet(envir = e)
  e$x <- 2
  expect_message(result <- load_incremental(envir = e), "new name")
  expect_equal(e$x, 2)
  expect_equal(result$action, "renamed")
  expect_equal(get(result$name, envir = e), 1)
  # Loading again finds the version already there
  again <- load_quiet(envir = e)
  expect_equal(again$action, "unchanged")
  expect_length(objects(e), 2)
})

test_that("overwrite replaces and identical objects are not read again", {
  local_store()
  e <- new.env()
  e$x <- 1
  e$y <- 5
  save_quiet(envir = e)
  e$x <- 2
  table <- ws_ref_table()
  # A damaged file shows that y is not read: its object has the same content
  writeBin(as.raw(1:50), file.path("rds", paste(table$hash[table$object == "y"], table$format[table$object == "y"], sep = ".")))
  result <- load_quiet(envir = e, overwrite = TRUE)
  expect_equal(e$x, 1)
  expect_equal(e$y, 5)
  expect_equal(result$action[match(c("x", "y"), result$object)], c("loaded", "unchanged"))
})

test_that("envir is the environment checked for collisions", {
  local_store()
  e <- new.env()
  e$x <- 1
  save_quiet(envir = e)
  target <- new.env()
  target$x <- "valuable"
  load_quiet(envir = target)
  expect_equal(target$x, "valuable")
  empty <- new.env()
  assign("x", "in global", envir = .GlobalEnv)
  withr::defer(rm("x", envir = .GlobalEnv))
  load_quiet(envir = empty)
  expect_equal(objects(empty), "x")
})

test_that("a missing file fails before loading anything", {
  local_store()
  e <- new.env()
  e$a <- 1
  e$b <- 2
  e$d <- 3
  save_quiet(envir = e)
  table <- ws_ref_table()
  file.remove(file.path("rds", paste(table$hash[table$object == "b"], table$format[table$object == "b"], sep = ".")))
  restored <- new.env()
  expect_error(load_incremental(envir = restored), "files of these objects are missing: b")
  expect_length(objects(restored), 0)
})

test_that("functions do not create versions when they are compiled", {
  local_store()
  e <- new.env()
  for(keep_source in c(FALSE, TRUE)){
    e$f <- eval(parse(text = "function(x) { y <- x + 1; for (i in 1:3) y <- y * 2; y }", keep.source = keep_source), envir = globalenv())
    save_quiet(envir = e)
    rows <- nrow(ws_ref_table())
    for(i in 1:5) e$f(i)
    save_quiet(envir = e)
    expect_equal(nrow(ws_ref_table()), rows)
    result <- load_quiet(envir = e)
    expect_equal(result$action, "unchanged")
    expect_length(objects(e), 1)
  }
})

test_that("closures with state do create versions", {
  local_store()
  counter <- function(){
    n <- 0
    function(){
      n <<- n + 1
      n
    }
  }
  e <- new.env()
  e$count <- counter()
  save_quiet(envir = e)
  e$count()
  save_quiet(envir = e)
  expect_equal(nrow(ws_ref_table()), 2)
})

test_that("many kinds of objects survive the round trip with a stable hash", {
  local_store()
  e <- new.env()
  e$number <- rnorm(5)
  e$sequence <- 1:1000
  e$frame <- data.frame(a = 1:5, b = letters[1:5])
  e$model <- lm(mpg ~ wt, mtcars)
  e$a_list <- list(a = 1, b = "x")
  e$a_factor <- factor(letters)
  e$a_date <- Sys.Date()
  e$a_formula <- y ~ x
  e$nothing <- NULL
  save_quiet(envir = e)
  restored <- new.env()
  load_quiet(envir = restored)
  expect_setequal(objects(restored), objects(e))
  expect_equal(restored$frame, e$frame)
  expect_equal(coef(restored$model), coef(e$model))
  expect_null(restored$nothing)
  status <- ws_status(envir = restored)
  expect_true(all(status$status == "unchanged"))
})
