# Parallel build vs serial build on a few legacy datasets (development only).
# Reads results/ (read-only), writes only under results_new/<name>/.
#   Rscript sepsisfactorsdb/dev/smoke_parallel.R [dataset,dataset,...] [name] [workers] [chunk_fits]
args <- commandArgs(TRUE)
datasets <- if (length(args)) {
  strsplit(args[1], ",")[[1]]
} else {
  c(
    "GSE54514_DAY3",
    "GSE54514_DAY4",
    "GSE54514_DAY5"
  )
}
name <- if (length(args) > 1) args[2] else "parallel_smoke"
workers <- if (length(args) > 2) as.integer(args[3]) else 4L
chunk_fits <- if (length(args) > 3) as.integer(args[4]) else 100L
root <- normalizePath("/home/chase/projects/sepsis_factors")
out <- file.path(root, "results_new", name)
unlink(out, recursive = TRUE)
dir.create(out, recursive = TRUE)

# workers are separate R processes: give them the package from a temporary library
lib <- file.path(out, "lib")
dir.create(lib)
utils::install.packages(file.path(root, "sepsisfactorsdb"),
  lib = lib, repos = NULL, type = "source", quiet = TRUE,
  INSTALL_opts = c("--no-docs", "--no-test-load")
)
Sys.setenv(R_LIBS = lib)
.libPaths(c(lib, .libPaths()))
library(sepsisfactorsdb)

legacy_to_results(file.path(root, "results"), file.path(out, "results"),
  config_dir = file.path(root, "sepsisfactors", "inst"),
  datasets = datasets, artifact_mode = "symlink"
)
t_serial <- system.time(suppressMessages(
  ingest_results(
    file.path(out, "results"),
    file.path(out, "serial", "stability.sqlite"),
    artifact_mode = "symlink"
  )
))[["elapsed"]]
t_par <- system.time(progress <- suppressMessages(
  ingest_parallel(file.path(out, "results"), file.path(out, "parallel", "stability.sqlite"),
    controller = ingest_controller("local", workers = workers), artifact_mode = "symlink",
    chunk_fits = chunk_fits, store = file.path(out, "store"), work_dir = NULL
  )
))[["elapsed"]]
cat(
  sprintf(
    "serial %.1f s, parallel %.1f s (%d workers, chunk_fits %d)\n",
    t_serial,
    t_par,
    workers,
    chunk_fits
  )
)
print(as.data.frame(table(sub("_.*", "", progress$name))))

a <- file.path(out, "serial", "stability.sqlite")
b <- file.path(out, "parallel", "stability.sqlite")
ca <- DBI::dbConnect(RSQLite::SQLite(), a, flags = RSQLite::SQLITE_RO)
cb <- DBI::dbConnect(RSQLite::SQLite(), b, flags = RSQLite::SQLITE_RO)
tabs <- setdiff(
  DBI::dbGetQuery(
    ca,
    "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'"
  )$name,
  "meta"
)
same <- vapply(tabs, function(t) {
  cols <- DBI::dbGetQuery(ca, sprintf("PRAGMA table_info(%s)", t))$name
  sql <- sprintf(
    "SELECT * FROM %s ORDER BY %s",
    t,
    paste(DBI::dbQuoteIdentifier(ca, cols), collapse = ", ")
  )
  identical(DBI::dbGetQuery(ca, sql), DBI::dbGetQuery(cb, sql))
}, logical(1))
cat(
  "tables compared:",
  length(tabs),
  "; identical:",
  sum(same),
  "; differing:",
  paste(tabs[!same], collapse = ", "),
  "\n"
)
cat(
  "rows:",
  format(
    sum(vapply(tabs, function(t) DBI::dbGetQuery(ca, paste("SELECT COUNT(*) n FROM", t))$n, 1)),
    big.mark = ","
  ),
  "\n"
)
cat("foreign_key_check rows:", nrow(DBI::dbGetQuery(cb, "PRAGMA foreign_key_check")), "\n")
cat(
  "sizes MB serial / parallel:",
  round(file.size(a) / 1e6, 1),
  "/",
  round(file.size(b) / 1e6, 1),
  "\n"
)
