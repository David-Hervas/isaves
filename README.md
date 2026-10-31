# isaves

Incremental saves of the R workspace and selective load of objects.

`isaves` stores each object once, in a file named after its content, and keeps a table with every version of every object. Saving only writes what has changed, and loading can bring back a single object, an old version or the exact state of a past save.

## Installation

```r
devtools::install_github("David-Hervas/isaves")
```

## Workflow

```r
library(isaves)

data <- read.csv("data.csv")
save_incremental(annotation = "raw data")

data$group <- factor(data$group)
model <- lm(y ~ group, data)

ws_status()                                   # What would be saved?
save_incremental(annotation = "first model")

ws_ref_table()                                # Every stored version
ws_snapshots()                                # Every save
```

In a new session:

```r
load_incremental()                            # Latest version of every object
load_incremental(lazyload = TRUE)             # The same, read from disk when used
load_incremental(object == "model")           # Only some objects
load_incremental(snapshot = "raw data")       # The workspace as it was in that save
load_incremental(object == "data" & date < as.POSIXct("2026-10-01"))  # An old version
```

`load_incremental()` does not replace objects that have different content: the stored version is loaded with its date appended to the name, unless `overwrite = TRUE`.

A plain `load_incremental()` also brings back objects that were removed from the workspace, and tells which ones. Use `snapshot = "latest"` to leave them out.

## Maintenance

```r
annotate_ws(object == "model", "selected model", write = TRUE)

purge_ws_table(annotation == "draft")                 # Shows what would be removed
purge_ws_table(annotation == "draft", remove = TRUE)
prune_ws(keep = 3, remove = TRUE)                     # Keep 3 versions of each object

verify_ws(deep = TRUE)                                # Look for damaged or missing files
```

Removing versions can leave old snapshots incomplete. `ws_snapshots()` shows it in the `n_missing` column.

## Files

Relative paths are resolved from the root of the project, the nearest folder going up from the working directory that has the table, an RStudio project or a git repository. `ws_root()` shows it, and `options(isaves.root = "path")` fixes it.

| File | Content |
|---|---|
| `ws_table.ref` | Table with all the versions and snapshots |
| `ws_table.ref.bak` | The table before the last change |
| `ws_table.ref.lock` | Lock used while updating the table |
| `rds/` | One file per distinct content |

The names of the objects are only in the table. If it gets damaged, copy `ws_table.ref.bak` over it.

## Options

| Option | Default | Meaning |
|---|---|---|
| `isaves.format` | `"qs2"` | Format of new files: `"qs2"` or `"rds"` |
| `isaves.nthreads` | up to 8 | Threads used for files larger than 1 MB |
| `isaves.root` | not set | Fixed root folder |
| `isaves.lock.timeout` | `60000` | Milliseconds to wait for another session |

## Limitations

* Objects are compared by content, and functions are compared ignoring their compilation. A function stored inside a list or another object creates one extra version the first time it is compiled.
* Objects that share an environment (for example, two names for the same environment) are restored as independent copies.
