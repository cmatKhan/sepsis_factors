# Parity of the derived tables with the live functions of sepsisfactorsapp, on the
# smoke database (development only). Run dev/smoke_schema4.R first.
#   Rscript sepsisfactorsdb/dev/parity_schema4.R [dataset] [n_agreement_samples]
args <- commandArgs(TRUE)
dataset <- if (length(args)) args[1] else "GSE110487_T1"
n_sample <- if (length(args) > 1) as.integer(args[2]) else 150L
root <- "/home/chase/projects/sepsis_factors"
suppressMessages(devtools::load_all(file.path(root, "sepsisfactorsdb"), quiet = TRUE))
app <- new.env()
note_missing <- function(...) invisible(NULL)
assign("note_missing", note_missing, app)
for (f in c("db_helpers.R", "similarity.R", "comparison_helpers.R", "data_helpers.R")) {
  sys.source(file.path(root, "sepsisfactorsapp", "R", f), envir = app)
}
options(stability.db_dir = file.path(root, "results"))
old <- DBI::dbConnect(RSQLite::SQLite(), paste0(
  "file:",
  file.path(root, "results", "stability.sqlite"),
  "?immutable=1"
),
flags = RSQLite::SQLITE_RO, synchronous = NULL
)
new <- DBI::dbConnect(
  RSQLite::SQLite(),
  file.path(root, "results_new", "smoke", "stability.sqlite")
)
q <- function(con, sql, ...) {
  a <- list(...)
  if (length(a)) DBI::dbGetQuery(con, sql, params = a) else DBI::dbGetQuery(con, sql)
}
fits_old <- q(old, "SELECT fit_id, fit_key FROM fits WHERE dataset_id = ?", dataset)
fits_new <- q(new, "SELECT fit_id, fit_key FROM fits WHERE dataset_id = ?", dataset)
to_new <- function(old_id) {
  fits_new$fit_id[match(
    fits_old$fit_key[match(old_id, fits_old$fit_id)],
    fits_new$fit_key
  )]
}
to_old <- function(new_id) {
  fits_old$fit_id[match(
    fits_new$fit_key[match(new_id, fits_new$fit_id)],
    fits_old$fit_key
  )]
}
report <- function(name, d) {
  cat(
    sprintf("%-34s compared %5d, max abs diff %.3g, NA mismatches %d\n", name, d$n, d$max, d$na)
  )
}
cmp <- function(a, b) {
  both <- !is.na(a) & !is.na(b)
  list(
    n = length(a),
    max = if (any(both)) max(abs(a[both] - b[both])) else 0,
    na = sum(is.na(a) != is.na(b))
  )
}

## ---- fit_summary vs method_curve --------------------------------------------------------
val_sd <- app$dataset_val_sd(old, dataset)
summ <- q(new, "SELECT * FROM fit_summary")
tot <- list()
for (m in app$METHOD_ORDER) {
  for (metric in c("cosine", "pearson", "spearman")) {
    cv <- app$method_curve(old, dataset, m, metric, val_sd)
    if (nrow(cv) == 0) next
    nid <- to_new(cv$fit_id)
    s <- summ[match(nid, summ$fit_id), ]
    if (m == "pca") {
      k <- cv$n_factors
      d <- q(
        new,
        "SELECT idx, value FROM fit_diag WHERE fit_id = ? AND name = 'mse_by_rank'",
        nid[1]
      )
      r2 <- 1 - d$value[match(k, d$idx)] / val_sd^2
      tot[[paste(m, metric, "r2 (per rank, from fit_diag)")]] <- cmp(cv$in_sample_r2, r2)
      last <- which.max(k)
      tot[[paste(m, metric, "r2 (stored row)")]] <- cmp(cv$in_sample_r2[last], s$in_sample_r2[last])
      tot[[paste(m, metric, "stability")]] <- cmp(
        cv$stability[last],
        s[[paste0("stability_", metric)]][last]
      )
    } else {
      tot[[paste(m, metric, "r2")]] <- cmp(cv$in_sample_r2, s$in_sample_r2)
      tot[[paste(m, metric, "stability")]] <- cmp(cv$stability, s[[paste0("stability_", metric)]])
    }
  }
}
for (n in names(tot)) report(paste("fit_summary", n), tot[[n]])

## ---- factor_association vs association_scan ----------------------------------------------
meta <- app$dataset_sample_metadata(old, dataset)
assoc <- q(
  new,
  paste0(
    "SELECT a.*, x.label FROM factor_association a JOIN factors x ON x.fit_id = ",
    "a.fit_id AND x.factor_index = a.factor_index"
  )
)
reps <- q(
  old,
  paste0(
    "SELECT fit_id, method, n_factors FROM fits WHERE dataset_id = ? AND ",
    "representative = 1 AND status = 'ok'"
  ),
  dataset
)
d_stat <- d_p <- d_adj <- numeric()
n_cells <- 0L
n_miss <- 0L
for (i in seq_len(nrow(reps))) {
  fid <- reps$fit_id[i]
  S <- app$load_scores(old, fid)
  fac <- app$fit_factors(old, fid)
  if (reps$method[i] == "wgcna") {
    S <- S[, intersect(fac$label, colnames(S)), drop = FALSE]
  } else {
    S <- S[, seq_len(min(nrow(fac), ncol(S))), drop = FALSE]
    colnames(S) <- fac$label[seq_len(ncol(S))]
  }
  live <- app$association_scan(S, meta)
  got <- assoc[assoc$fit_id == to_new(fid), ]
  key_l <- paste(live$component, live$field)
  key_g <- paste(got$label, got$field)
  n_miss <- n_miss + sum(!key_l %in% key_g) + sum(!key_g %in% key_l)
  j <- match(key_l, key_g)
  ok <- !is.na(j)
  n_cells <- n_cells + sum(ok)
  d_stat <- c(d_stat, abs(live$statistic[ok] - got$statistic[j[ok]]))
  d_p <- c(d_p, abs(live$p_value[ok] - got$p_value[j[ok]]))
  d_adj <- c(d_adj, abs(live$padj[ok] - got$p_adj[j[ok]]))
}
cat(
  sprintf(
    "factor_association: %d fits, %d cells compared, cells present in only one side: %d\n",
    nrow(reps),
    n_cells,
    n_miss
  )
)
cat(
  sprintf(
    "  max abs diff statistic %.3g, p_value %.3g, p_adj %.3g\n",
    max(d_stat),
    max(d_p),
    max(d_adj)
  )
)

## ---- method_agreement vs method_pairwise_agreement -------------------------------------------
ag <- q(new, "SELECT m.*, a.method AS ma, b.method AS mb FROM method_agreement m
              JOIN fits a ON a.fit_id = m.fit_a JOIN fits b ON b.fit_id = m.fit_b")
set.seed(1)
pick <- ag[sample(nrow(ag), min(n_sample, nrow(ag))), ]
dj <- da <- numeric()
dm <- 0L
t_live <- 0
for (i in seq_len(nrow(pick))) {
  r <- pick[i, ]
  opt <- data.frame(
    method = c(r$ma, r$mb), fit_id = to_old(c(r$fit_a, r$fit_b)),
    n_factors = c(r$n_factors_a, r$n_factors_b), stringsAsFactors = FALSE
  )
  timing <- system.time(live <- app$method_pairwise_agreement(old, opt, r$top_n))
  t_live <- t_live + timing[["elapsed"]]
  dj <- c(dj, abs(live$gene_jaccard[1, 2] - r$gene_jaccard))
  if (!is.na(r$sample_ari) || !is.na(live$sample_ari[1, 2])) {
    da <- c(
      da,
      abs(live$sample_ari[1, 2] - r$sample_ari)
    )
  }
  pr <- live$pairs[[paste(r$ma, r$mb, sep = "|")]]
  if (!is.null(pr)) {
    m <- decode_i32(r$matches[[1]])
    na <- r$n_factors_a
    nb <- r$n_factors_b
    k <- length(m) - na - nb
    idx <- cbind(m[(na + nb + 1):(na + nb + k / 2)], m[(na + nb + k / 2 + 1):length(m)])
    jm <- matrix(decode_f32(r$jaccard_matrix[[1]]), na)
    if (max(abs(jm - pr$matrix), na.rm = TRUE) > 1e-6 || abs(mean(jm[idx]) - pr$mean) > 1e-6) {
      dm <- dm + 1L
    }
  }
}
cat(
  sprintf(
    "method_agreement: %d of %d rows sampled (live recompute %.1f s)\n",
    nrow(pick),
    nrow(ag),
    t_live
  )
)
cat(sprintf(
  paste0(
    "  max abs diff gene_jaccard %.3g, sample_ari %.3g (%d ARI compared); ",
    "matrix/matches disagreements: %d\n"
  ),
  max(dj), if (length(da)) max(da) else NA, length(da), dm
))
