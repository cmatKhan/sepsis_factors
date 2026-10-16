# Smoke test of the schema 4 pipeline on one legacy dataset (development only).
# Reads results/ (read-only), writes only under results_new/smoke/.
#   Rscript sepsisfactorsdb/dev/smoke_schema4.R [dataset,dataset,...] [output name under results_new/]
args <- commandArgs(TRUE)
datasets <- if (length(args)) strsplit(args[1], ",")[[1]] else "GSE110487_T1"
dataset <- datasets[1]
name <- if (length(args) > 1) args[2] else "smoke"
root <- normalizePath("/home/chase/projects/sepsis_factors")
out <- file.path(root, "results_new", name)
suppressMessages(devtools::load_all(file.path(root, "sepsisfactorsdb"), quiet = TRUE))
unlink(out, recursive = TRUE)
t0 <- Sys.time()
legacy_to_results(
  file.path(root, "results"), file.path(out, "results"),
  config_dir = file.path(root, "sepsisfactors", "inst"),
  datasets = datasets, artifact_mode = "symlink"
)
t1 <- Sys.time()
res <- ingest_results(file.path(out, "results"), file.path(out, "stability.sqlite"), artifact_mode = "symlink")
t2 <- Sys.time()
cat("adapter seconds:", round(as.numeric(t1 - t0, units = "secs"), 1), "\n")
cat("ingest seconds:", round(as.numeric(t2 - t1, units = "secs"), 1), "\n")
for (d in datasets) {
  cat(d, "\n")
  print(res$derived[[d]])
}
print(res$derived$cross_dataset)
con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "stability.sqlite"))
for (t in c(
  "genes", "factor_top_genes", "fit_scores", "projection_values", "driver_genes", "fit_diag",
  "wgcna_dendrograms", "sample_metadata", "gene_variance", "fit_summary", "method_agreement",
  "factor_association", "composition_markers"
)) {
  cat(sprintf("%-20s %d\n", t, DBI::dbGetQuery(con, paste("SELECT COUNT(*) n FROM", t))$n))
}
cat("database MB:", round(file.size(file.path(out, "stability.sqlite")) / 1e6, 1), "\n")
print(nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check")))
