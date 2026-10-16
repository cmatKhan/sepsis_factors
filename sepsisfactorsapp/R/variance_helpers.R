# Labels and pure helpers behind the variance pages (the queries are in db_variance.R).

VARIANCE_COLORS <- c(
  subject = "#2C5F8A", `subject-level` = "#8DB9DC", time = "#E07B00",
  `group x time` = "#F2B36B", technical = "#7B5EA7", group = "#4E9F6B",
  covariate = "#A9D6B5", residual = "#BDBDBD"
)
VARIANCE_LABELS <- c(
  subject = "patient", `subject-level` = "patient-level (group, sex, age)",
  time = "time", `group x time` = "group x time", technical = "technical",
  group = "group", covariate = "covariate", residual = "residual (within-person + noise)"
)
# The summary column each choice reads from variance_categories.
VARIANCE_GENESETS <- c(
  "Factor-analysis genes, variance-weighted" = "vw_factor_genes",
  "All modelled genes, variance-weighted" = "vw_all",
  "Top 8,000 by variance, variance-weighted" = "vw_top",
  "Typical gene (median per gene)" = "median"
)

time_course_parents <- function(design) {
  design$dataset_id[design$is_subset == 0 & !is.na(design$subject_col) & !is.na(design$time_col)]
}

# One row per dataset: between patients, time, technical, residual (summed categories).
headline_table <- function(long, summary) {
  part <- function(cats) {
    tapply(ifelse(long$category %in% cats, long$share, 0), long$dataset_id, sum)
  }
  out <- data.frame(
    dataset = names(part("subject")),
    between = as.numeric(part(c("subject", "subject-level"))),
    time = as.numeric(part(c("time", "group x time"))),
    technical = as.numeric(part("technical")),
    residual = as.numeric(part("residual")),
    stringsAsFactors = FALSE
  )
  s <- summary[match(out$dataset, summary$dataset_id), ]
  out$repeatability <- s$repeatability_median
  out$time_within <- s$time_within_median
  out$genes_time_gt10 <- s$share_time_gt10
  out$subjects_repeated <- s$n_subjects_repeated
  out$n_genes <- s$n_genes
  out$flags <- dataset_flags(s)
  out
}

dataset_flags <- function(s) {
  vapply(seq_len(nrow(s)), function(i) {
    f <- character(0)
    if (!is.na(s$sex_agreement[i]) && s$sex_agreement[i] < 0.9) {
      f <- c(f, sprintf("sex agreement %.2f", s$sex_agreement[i]))
    }
    if (!is.na(s$share_singular[i]) && s$share_singular[i] > 0.9) {
      f <- c(f, sprintf("%.0f%% singular fits", 100 * s$share_singular[i]))
    }
    if (!is.na(s$n_genes_failed[i]) && s$n_genes_failed[i] > 0) {
      f <- c(f, sprintf("%d genes failed", s$n_genes_failed[i]))
    }
    paste(f, collapse = "; ")
  }, character(1))
}

## ---- explaining a dataset's models ------------------------------------------------------

# One line per kind of term the variance model can contain (variance_terms.category); an
# unknown category is shown as its own name.
TERM_MEANINGS <- c(
  subject = "stable differences between people",
  `subject-level` = paste(
    "differences between people explained by a person-level variable (group, sex, age)"
  ),
  time = "the average change over time that every person shares",
  `group x time` = "time trajectories that differ between groups",
  group = "differences between groups",
  technical = "a technical axis (for example array controls or library size)",
  covariate = "a further covariate in the model",
  residual = paste(
    "everything else: change within a person that is not shared by time,",
    "plus measurement noise"
  ),
  derived = "a sum of the terms above"
)

term_meaning <- function(category) {
  out <- unname(TERM_MEANINGS[category])
  ifelse(is.na(out), category, out)
}

# The terms of a model formula as written in `dataset_design`, split on top-level `+` and
# marked random (`(1 | x)`) or fixed, with the underlying variable name.
formula_terms <- function(formula_text) {
  if (is.null(formula_text) || is.na(formula_text)) {
    return(NULL)
  }
  txt <- gsub("\\s+", " ", trimws(sub("^\\s*~", "", formula_text)))
  parts <- character(0)
  depth <- 0
  cur <- ""
  for (ch in strsplit(txt, "")[[1]]) {
    if (ch == "(") depth <- depth + 1
    if (ch == ")") depth <- depth - 1
    if (ch == "+" && depth == 0) {
      parts <- c(parts, trimws(cur))
      cur <- ""
    } else {
      cur <- paste0(cur, ch)
    }
  }
  parts <- c(parts, trimws(cur))
  parts <- parts[nzchar(parts) & parts != "0" & parts != "1"]
  random <- grepl("^\\(.*\\|.*\\)$", parts)
  data.frame(
    term = parts, random = random,
    variable = ifelse(random, trimws(sub("^\\(\\s*1\\s*\\|\\s*(.*)\\)$", "\\1", parts)), parts),
    stringsAsFactors = FALSE
  )
}

# Samples and subjects per group x time cell, in the design's time order (NULL when the
# dataset has no time or no samples). Empty cells (a group not sampled then) are kept.
cell_table <- function(samples, time_order = NULL) {
  s <- samples
  if (is.null(s) || nrow(s) == 0 || all(is.na(s$timepoint))) {
    return(NULL)
  }
  s$grp <- ifelse(is.na(s$grp), "all samples", s$grp)
  tp <- unique(s$timepoint)
  lv <- if (length(time_order)) c(intersect(time_order, tp), setdiff(tp, time_order)) else sort(tp)
  n <- table(factor(s$grp), factor(s$timepoint, levels = lv))
  nsub <- tapply(
    s$subject, list(factor(s$grp), factor(s$timepoint, levels = lv)),
    function(x) length(unique(x))
  )
  nsub[is.na(nsub)] <- 0
  out <- matrix(
    sprintf("%d / %d", as.integer(n), as.integer(nsub)), nrow(n), ncol(n),
    dimnames = dimnames(n)
  )
  out[n == 0] <- "-"
  as.data.frame.matrix(out)
}
