local_three <- function(env = parent.frame()) {
  path <- withr::local_tempdir("results-", .local_envir = env)
  example_results(file.path(path, "res"), n_datasets = 3L)
}

built <- function(progress, pattern = ".") {
  progress$name[progress$progress == "completed" & grepl(pattern, progress$name)]
}

test_that("a dataset above the work threshold is split into chunks of its fits", {
  root <- local_three()
  expect_null(plan_chunks(root, "TOY2", 400))
  chunks <- plan_chunks(root, "TOY1", 5)
  fits <- read_table(root, "TOY1", "fits")
  expect_gt(length(chunks), 1)
  expect_setequal(unlist(chunks), fits$fit_key)
  expect_false(anyDuplicated(unlist(chunks)) > 0)
  # the representative fits are cut in the order agreement rows are written in
  reps <- agreement_fits(transform(fits, fit_id = seq_len(nrow(fits))))$fit_key
  in_chunks <- unlist(lapply(chunks, function(k) k[k %in% reps]))
  expect_equal(in_chunks, reps)
})

test_that("the parallel build equals the serial one, ids included", {
  skip_on_cran()
  local_worker_library()
  root <- local_three()
  dir <- withr::local_tempdir()
  ingest_results(root, file.path(dir, "serial.sqlite"), artifact_mode = "symlink")
  progress <- suppressMessages(ingest_parallel(
    root, file.path(dir, "parallel.sqlite"),
    controller = ingest_controller("local", workers = 2),
    artifact_mode = "symlink", chunk_fits = 5, store = file.path(dir, "store")
  ))
  expect_gt(length(built(progress, "^chunk_")), 1)
  expect_equal(differing_tables(file.path(dir, "serial.sqlite"), file.path(dir, "parallel.sqlite")), character())
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, "parallel.sqlite"))
  withr::defer(DBI::dbDisconnect(con))
  expect_equal(nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check")), 0)
  expect_equal(DBI::dbGetQuery(con, "PRAGMA integrity_check")[[1]], "ok")
  expect_equal(db_schema_version(con), SCHEMA_VERSION)
  expect_gt(db_count(file.path(dir, "parallel.sqlite"), "projection_values"), 1)
})

test_that("a rerun rebuilds only the parts of the dataset that changed, and a deleted database", {
  skip_on_cran()
  local_worker_library()
  root <- local_three()
  dir <- withr::local_tempdir()
  db <- file.path(dir, "parallel.sqlite")
  run <- function() {
    suppressMessages(ingest_parallel(
      root, db,
      controller = ingest_controller("local", workers = 2),
      artifact_mode = "symlink", chunk_fits = 5, store = file.path(dir, "store")
    ))
  }
  first <- run()
  expect_true(all(first$progress == "completed"))
  expect_length(built(run()), 0)

  edit_table(root, "TOY2", "fit_params", function(df) df[-1, ])
  second <- run()
  expect_setequal(built(second), c("results_TOY2", "part_TOY2", "merged", "projection_index", "database"))
  serial <- file.path(dir, "serial.sqlite")
  ingest_results(root, serial, artifact_mode = "symlink")
  expect_equal(differing_tables(serial, db), character())

  unlink(db)
  third <- run()
  expect_equal(built(third), "database")
  expect_true(file.exists(db))
})

test_that("a failed part leaves the old database untouched", {
  skip_on_cran()
  local_worker_library()
  root <- local_three()
  dir <- withr::local_tempdir()
  db <- file.path(dir, "parallel.sqlite")
  run <- function() {
    suppressMessages(ingest_parallel(
      root, db,
      controller = ingest_controller("local", workers = 2),
      artifact_mode = "symlink", store = file.path(dir, "store")
    ))
  }
  run()
  before <- unname(tools::md5sum(db))
  edit_table(root, "TOY3", "fit_params", function(df) df[, c("fit_key", "name")])
  expect_error(run())
  expect_equal(unname(tools::md5sum(db)), before)
})

test_that("the SLURM controller is built from the config without submitting anything", {
  withr::local_dir(withr::local_tempdir())
  g <- ingest_controller("slurm", setup_lines = list("module load R/4.4"), partition = "short")
  expect_s3_class(g, "crew_class_controller_group")
  expect_equal(vapply(g$controllers, function(x) x$launcher$name, ""), c(default = "default", big = "big"), ignore_attr = TRUE)
  opts <- function(name) {
    o <- g$controllers[[name]]$launcher$options_cluster
    o[c("script_lines", "log_output", "memory_gigabytes_required", "cpus_per_task", "time_minutes", "partition")]
  }
  expect_snapshot({
    str(lapply(list(default = opts("default"), big = opts("big")), function(o) {
      o$script_lines[2] <- "cd <project>"
      o
    }))
  })
  expect_equal(g$controllers$default$launcher$workers, 200)
  expect_equal(g$controllers$big$launcher$workers, 20)
  expect_true(dir.exists("logs/crew/scripts"))
})

test_that("the local controller defaults to the cores minus two", {
  g <- ingest_controller("local")
  expect_equal(g$controllers$default$launcher$workers, max(1L, parallel::detectCores() - 2L))
  g2 <- ingest_controller("local", workers = 6)
  expect_equal(g2$controllers$big$launcher$workers, 3)
})

test_that("ingest.yml supplies defaults and use_ingest writes the files", {
  dir <- withr::local_tempdir()
  suppressMessages(use_ingest(dir))
  expect_setequal(list.files(dir), c("_targets.R", "ingest.yml", "run_ingest.sbatch"))
  expect_equal(read_ingest_config(file.path(dir, "ingest.yml"))$results_root, "results")
  writeLines(c("results_root: res", "db_path: out/db.sqlite", "controller:", "  type: slurm", "  slurm:", "    partition: p1"), file.path(dir, "ingest.yml"))
  cfg <- read_ingest_config(file.path(dir, "ingest.yml"))
  expect_equal(cfg$controller$type, "slurm")
  expect_equal(cfg$controller$slurm$partition, "p1")
  expect_equal(cfg$chunk_fits, 400)
  expect_equal(cfg$controller$slurm$big$memory_gigabytes_required, 32)
  expect_snapshot(cat(readLines(file.path(dir, "_targets.R")), sep = "\n"))
})

test_that("the parallel targets are laid out per dataset", {
  root <- local_three()
  tg <- ingest_parallel_targets(list(results_root = root, db_path = tempfile(), chunk_fits = 5L))
  names <- vapply(tg, function(t) t$settings$name, "")
  expect_contains(names, c("part_TOY1", "chunk_1_TOY1", "part_TOY3", "merged", "projection_index", "proj_TOY1", "proj_TOY2", "database"))
  expect_false("chunk_1_TOY2" %in% names)
})
