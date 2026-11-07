# isaves 0.3.1

* `load_incremental()`, `save_incremental()` and `ws_status()` use the calling environment by default (`envir = parent.frame()`) instead of the global environment. At the console this is the same.
* New vignette, `vignette("isaves")`.

## Bug fixes

* Restoring a snapshot of an object that later went back to the same value used the metadata of the later save, so `subset` and the name given to a renamed copy referred to the wrong save.
* `load_incremental(lazyload = FALSE)` left objects that were lazy-loaded and still unused as promises. They are now read into memory.
* A save could leave out an object that another R session purged or changed between the comparison of the workspace and the update of the table.
* `verify_ws(remove.orphans = TRUE)` now decides which files are orphans while the store is locked, and removed orphans no longer count as a problem.
* Using a lazy-loaded object whose file was removed gives a clear error.
* `items` must be a character vector of names, and repeated names are saved and shown once. `snapshot` must be a single value.
* `ws_status()` of an empty environment returns the usual column types, and columns added to tables of old versions have their usual types.
* Files larger than 1 MB use at most 2 threads when R CMD check limits the cores, and 1 thread if the number of cores is unknown.
* `digest (>= 0.6.25)` is required.

# isaves 0.3.0

Stores created by previous versions keep working and do not need to be migrated: their rows and `.rds` files are still read, and only new versions use the new hash and format.

## Bug fixes

* Objects named like a function of base R or like an internal variable (`table`, `c`, `file`, `path`, ...) were saved with the wrong content. Objects are now looked up only in `envir`.
* Objects identical to another stored object were not recorded, so they were lost when loading. A value that went back to a previous one was restored with the wrong version.
* A `subset` evaluating to `NA`, as `annotation == "x"` does with empty annotations, could destroy rows of the table in `purge_ws_table()` and caused errors elsewhere. `NA` now means not selected.
* Simultaneous saves from several R sessions lost rows of the table. Updates are now locked.
* The table and the object files are written atomically, and the table keeps a `.bak` copy. An unreadable table now gives an error instead of "No previous metadata".
* `load_incremental()` looks for name collisions in `envir` instead of always in the global environment.
* `subset` can use variables of the calling environment.
* `rds.folder` and `metadata.file` can be absolute or nested, and lazy-loaded objects survive a change of working directory.
* `load_incremental()` checks that all files exist and reads them before assigning anything.
* Clear errors when there is no table yet. `annotation = NULL` is accepted by `save_incremental()`, and annotations of length greater than 1 are rejected. Comments of several lines are kept complete.
* A dry run of `purge_ws_table()` no longer says "Purged objects". Purging everything now requires `subset = TRUE`, and purging the latest version of an object tells that an older one takes its place.
* Running a function no longer creates a new version of it or a renamed copy on load. This covers functions at the top level of the workspace, not functions inside lists or other objects.

## Performance

* Objects are hashed once with `spookyhash`, without duplicating them in memory.
* New objects are stored with `qs2` by default (see the `format` argument). Use `format = "rds"` or `options(isaves.format = "rds")` to keep using `.rds` files.
* The metadata of each save is built in one step, and loading no longer searches the table once per object.
* Objects already present with the same content are not read again.
* Lazy-loaded objects that have not been used are not read by `save_incremental()`, `ws_status()` or `load_incremental()`.

## New features

* Every save records a snapshot. `ws_snapshots()` lists them and `load_incremental(snapshot = )` restores the exact state of a save. Annotations are kept in the snapshot even if no object has changed.
* `ws_status()` compares the workspace with the store without saving, and shows the class and size of each object.
* The help of `ws_ref_table()` describes the columns of the table, including the new `algo`, `format` and `save_id`.
* `prune_ws()` keeps only the most recent versions of each object.
* `verify_ws()` looks for missing, corrupt and orphan files.
* `ws_root()` shows the folder in use. Relative paths are resolved from the project root, so a store is found from any subfolder. Starting a new store or changing to another one is announced.
* `save_incremental()` has an `envir` argument.
* `save_incremental()` and `load_incremental()` return what they did, invisibly, and use messages instead of `cat()`.

## Breaking changes

* `annotate_ws()` no longer has the unused `rds.folder` argument, and `annotation` is now required.
* `purge_ws_table()` and `annotate_ws()` return the selected rows instead of printing them.
* Versions renamed on load use the date without fractions of a second, as in `x_2026.10.31_12.45.49`.
