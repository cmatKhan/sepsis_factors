built_in_process <- function(root, dir, chunk_fits) {
  db <- file.path(dir, "parallel.sqlite")
  ids <- read_manifest(root)$datasets
  part_dir <- file.path(dir, "parts")
  work_dir <- file.path(dir, "work")
  parts <- character()
  units <- integer()
  for (i in seq_along(ids)) {
    chunks <- plan_chunks(root, ids[i], chunk_fits)
    load_part <- build_load_part(
      root, ids[i], part_dir, work_dir, db, "symlink",
      chunked = !is.null(chunks)
    )
    parts <- c(parts, load_part)
    for (k in seq_along(chunks)) {
      parts <- c(
        parts,
        build_chunk_part(root, ids[i], load_part, chunks[[k]], k, part_dir, work_dir, db)
      )
    }
    units <- c(units, rep(i, 1 + length(chunks)))
  }
  merged <- merge_dataset_parts(parts, units, root, db, "symlink", file.path(dir, "merge"))
  index <- projection_index(merged)
  proj <- lapply(unique(index$source), function(id) {
    build_projection_part(
      index[index$source == id, c("projection_id", "path"), drop = FALSE],
      id, part_dir, work_dir, db
    )
  })
  finish_database(merged, unlist(proj), db, file.path(dir, "merge"))
}

test_that("a dataset above the work threshold is split into chunks of its fits", {
  root <- local_results()
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

test_that("building the parts and merging them gives the serial database, ids included", {
  root <- local_results()
  dir <- withr::local_tempdir()
  serial <- file.path(dir, "serial.sqlite")
  ingest_results(root, serial, artifact_mode = "symlink")
  expect_gt(length(plan_chunks(root, "TOY1", 5)), 1)
  db <- built_in_process(root, dir, chunk_fits = 5)
  expect_equal(db, file.path(dir, "parallel.sqlite"))
  expect_equal(differing_tables(serial, db), character())
  expect_gt(db_count(db, "projection_values"), 0)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  withr::defer(DBI::dbDisconnect(con))
  expect_equal(nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check")), 0)
  expect_equal(db_schema_version(con), SCHEMA_VERSION)
})

test_that("the SLURM controller is built from the config without submitting anything", {
  withr::local_dir(withr::local_tempdir())
  g <- ingest_controller("slurm", setup_lines = list("module load R/4.4"), partition = "short")
  expect_s3_class(g, "crew_class_controller_group")
  expect_equal(
    vapply(g$controllers, function(x) x$launcher$name, ""),
    c(default = "default", big = "big"),
    ignore_attr = TRUE
  )
  opts <- function(name) {
    o <- g$controllers[[name]]$launcher$options_cluster
    o[c(
      "script_lines",
      "log_output",
      "memory_gigabytes_required",
      "cpus_per_task",
      "time_minutes",
      "partition"
    )]
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
  writeLines(
    c(
      "results_root: res",
      "db_path: out/db.sqlite",
      "controller:",
      "  type: slurm",
      "  slurm:",
      "    partition: p1"
    ),
    file.path(dir, "ingest.yml")
  )
  cfg <- read_ingest_config(file.path(dir, "ingest.yml"))
  expect_equal(cfg$controller$type, "slurm")
  expect_equal(cfg$controller$slurm$partition, "p1")
  expect_equal(cfg$chunk_fits, 400)
  expect_equal(cfg$controller$slurm$big$memory_gigabytes_required, 32)
  expect_snapshot(cat(readLines(file.path(dir, "_targets.R")), sep = "\n"))
})

test_that("the parallel targets are laid out per dataset", {
  root <- local_results()
  tg <- ingest_parallel_targets(list(results_root = root, db_path = tempfile(), chunk_fits = 5L))
  names <- vapply(tg, function(t) t$settings$name, "")
  expect_contains(
    names,
    c(
      "part_TOY1",
      "chunk_1_TOY1",
      "part_TOY2",
      "merged",
      "projection_index",
      "proj_TOY1",
      "database"
    )
  )
  expect_false("chunk_1_TOY2" %in% names)
})
