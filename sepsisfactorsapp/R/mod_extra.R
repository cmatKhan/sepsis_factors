# Remaining analyses: Biplots, Trajectory across time, Projection comparison, and Pattern
# drivers. `focus` and `state` as in mod_inspect.R.

component_choices <- function(db, focus) {
  fac <- fit_factors(db, focus$fit_id, focus$n_factors)
  setNames(seq_len(nrow(fac)), fac$label)
}

# What the pages lose when they read the database copy of the loadings, as the text of a
# reduced-data notice.
LOADINGS_REDUCED <- paste(
  "Gene arrows are chosen from the loadings stored for each factor (its top 50 genes per",
  "side); a gene outside a factor's stored genes counts as zero on it."
)
NO_METHOD_LOADINGS <- paste(
  "This method has no loadings, so it is not projected onto other datasets and has no",
  "trajectory. Choose another method in the sidebar."
)

# What the pattern-driver page lacks when it reads the database copy.
DRIVERS_REDUCED <- paste(
  "The volcano shows only the genes that are significant (adjusted p below 0.05) or shared;",
  "the non-significant background is not stored."
)

# The reduced-data notice of the biplot page, when its loadings are the database copy.
biplot_notice <- function(loadings) {
  if (from_database(loadings)) reduced_notice(LOADINGS_REDUCED)
}

# The reduced-data notice of the pattern-driver page: only the Welch volcano loses its
# background (the confidence-interval view shows shared genes either way).
driver_notice <- function(res, mode) {
  if (from_database(res) && identical(mode, "PV")) reduced_notice(DRIVERS_REDUCED)
}

## ---- Biplots -----------------------------------------------------------------------------------

# Gene biplot data: all sample scores plus the `top_n` genes with the largest combined
# loading on the two chosen components, scaled so the longest arrow reaches 80% of the
# score cloud's radius (loadings and scores live on different scales, so only direction
# and relative length carry meaning).
biplot_data <- function(L, S, cx, cy, top_n = 15) {
  scores <- data.frame(sample_id = rownames(S), x = S[, cx], y = S[, cy])
  lx <- L[, cx]
  ly <- L[, cy]
  mag <- sqrt(lx^2 + ly^2)
  keep <- order(-mag)[seq_len(min(top_n, sum(mag > 0)))]
  sr <- suppressWarnings(max(sqrt(scores$x^2 + scores$y^2), na.rm = TRUE))
  lr <- max(mag[keep])
  k <- if (is.finite(lr) && lr > 0) 0.8 * sr / lr else 1
  list(
    scores = scores,
    arrows = data.frame(gene = rownames(L)[keep], x = lx[keep] * k, y = ly[keep] * k)
  )
}

# Metadata biplot data: every usable sample-metadata field on the same two components.
# Numeric fields become a vector (Spearman correlation with each axis, scaled like the
# gene arrows); categorical fields become one centroid per level (levels with fewer than
# `min_n` samples are dropped; fields with fewer than 2 or more than `max_levels` usable
# levels are skipped).
biplot_metadata_data <- function(S, meta, cx, cy, id_col = "sample_id", min_n = 3, max_levels = 8) {
  shared <- intersect(rownames(S), meta[[id_col]])
  if (length(shared) < min_n) {
    return(NULL)
  }
  S <- S[shared, , drop = FALSE]
  meta <- meta[match(shared, meta[[id_col]]), , drop = FALSE]
  x <- S[, cx]
  y <- S[, cy]
  scores <- data.frame(sample_id = shared, x = x, y = y)
  vec <- list()
  cen <- list()
  for (field in setdiff(names(meta), id_col)) {
    v <- meta[[field]]
    if (is.numeric(v) && !is.factor(v)) {
      ok <- !is.na(v)
      if (sum(ok) < min_n) next
      dx <- suppressWarnings(stats::cor(v[ok], x[ok], method = "spearman"))
      dy <- suppressWarnings(stats::cor(v[ok], y[ok], method = "spearman"))
      if (!is.na(dx) && !is.na(dy)) {
        vec[[length(vec) + 1]] <- data.frame(field = field, dx = dx, dy = dy)
      }
    } else {
      f <- as.factor(v)
      tab <- table(f)
      levs <- names(tab)[tab >= min_n]
      if (length(levs) < 2 || length(levs) > max_levels) next
      for (lv in levs) {
        ok <- !is.na(f) & f == lv
        cen[[length(cen) + 1]] <- data.frame(
          field = field, level = lv, x = mean(x[ok]), y = mean(y[ok])
        )
      }
    }
  }
  vectors <- if (length(vec)) {
    do.call(rbind, vec)
  } else {
    data.frame(field = character(0), dx = numeric(0), dy = numeric(0))
  }
  centroids <- if (length(cen)) {
    do.call(rbind, cen)
  } else {
    data.frame(field = character(0), level = character(0), x = numeric(0), y = numeric(0))
  }
  if (nrow(vectors) > 0) {
    sr <- max(sqrt(scores$x^2 + scores$y^2))
    mm <- max(sqrt(vectors$dx^2 + vectors$dy^2))
    k <- if (is.finite(mm) && mm > 0) 0.8 * sr / mm else 1
    vectors$x <- vectors$dx * k
    vectors$y <- vectors$dy * k
  }
  list(scores = scores, vectors = vectors, centroids = centroids)
}

mod_biplot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3), fill = FALSE,
      selectInput(ns("x"), "x axis", choices = NULL),
      selectInput(ns("y"), "y axis", choices = NULL),
      sliderInput(ns("top_n"), "Genes shown", 5, 50, 15, step = 5), uiOutput(ns("note"))
    ),
    navset_card_tab(
      nav_panel(
        "Genes",
        uiOutput(ns("reduced")),
        p(
          class = "text-muted",
          "Samples at their scores on the two factors, with arrows for the genes that load",
          "most on them: direction and relative length matter, not absolute scale."
        ),
        plotOutput(ns("genes"), height = "560px")
      ),
      nav_panel(
        "Metadata",
        p(
          class = "text-muted",
          "The same sample cloud with every usable metadata field: numeric fields as a",
          "Spearman-correlation vector, categorical fields as a centroid per level."
        ),
        plotOutput(ns("meta"), height = "560px")
      )
    )
  )
}

mod_biplot_server <- function(id, state, focus, db) {
  moduleServer(id, function(input, output, session) {
    info <- state$info
    scores <- reactive(focus_scores(db, focus()))
    loadings <- reactive({
      f <- focus()
      if (f$has_loadings) load_loadings(db, f$fit_id, f$n_factors) else NULL
    })
    observe({
      ch <- component_choices(db, focus())
      cur <- isolate(input$x)
      updateSelectInput(
        session, "x",
        choices = ch, selected = if (!is.null(cur) && cur %in% ch) cur else ch[1]
      )
      updateSelectInput(session, "y", choices = ch, selected = if (length(ch) > 1) ch[2] else ch[1])
    })
    output$note <- renderUI(p(class = "text-muted mt-4", focus_label(info, focus())))
    output$reduced <- renderUI(biplot_notice(loadings()))
    axes <- reactive({
      req(input$x, input$y)
      validate(need(!is.null(scores()), "No scores for this fit."))
      c(as.integer(input$x), as.integer(input$y))
    })
    output$genes <- renderPlot({
      f <- focus()
      validate(need(
        f$has_loadings,
        paste(
          "This method has no loadings, so there are no gene arrows;",
          "the Metadata tab works on the module eigengenes."
        )
      ))
      ax <- axes()
      S <- scores()
      L <- loadings()
      validate(need(!is.null(L), "No loadings for this fit."))
      d <- biplot_data(L, S, ax[1], ax[2], input$top_n)
      d$arrows$gene <- label_genes(gene_labels(db, state$dataset()), d$arrows$gene)
      ggplot() +
        geom_point(data = d$scores, aes(x, y), alpha = 0.5, colour = "grey40") +
        geom_segment(
          data = d$arrows, aes(0, 0, xend = x, yend = y), colour = "#B2182B",
          arrow = arrow(length = unit(0.2, "cm"))
        ) +
        ggrepel::geom_text_repel(
          data = d$arrows, aes(x, y, label = gene), colour = "#B2182B", size = 3.5,
          max.overlaps = Inf
        ) +
        labs(x = colnames(S)[ax[1]], y = colnames(S)[ax[2]]) +
        theme_minimal(base_size = 12)
    })
    output$meta <- renderPlot({
      ax <- axes()
      S <- scores()
      m <- dataset_sample_metadata(db, state$dataset())
      validate(need(!is.null(m), "No sample metadata."))
      d <- biplot_metadata_data(S, m, ax[1], ax[2])
      validate(need(!is.null(d), "Too few samples shared with the metadata."))
      p <- ggplot() +
        geom_point(data = d$scores, aes(x, y), alpha = 0.4, colour = "grey60")
      if (nrow(d$vectors) > 0) {
        p <- p +
          geom_segment(
            data = d$vectors, aes(0, 0, xend = x, yend = y), colour = "#2166AC",
            arrow = arrow(length = unit(0.2, "cm"))
          ) +
          ggrepel::geom_text_repel(
            data = d$vectors, aes(x, y, label = field), colour = "#2166AC", size = 3.5,
            max.overlaps = Inf
          )
      }
      if (nrow(d$centroids) > 0) {
        p <- p +
          geom_point(data = d$centroids, aes(x, y, colour = field), size = 3) +
          ggrepel::geom_text_repel(
            data = d$centroids, aes(x, y, label = level, colour = field), size = 3.5,
            max.overlaps = Inf, show.legend = FALSE
          )
      }
      p +
        labs(x = colnames(S)[ax[1]], y = colnames(S)[ax[2]], colour = "categorical field") +
        theme_minimal(base_size = 12)
    })
  })
}

## ---- Trajectory across time ---------------------------------------------------------------------

# Metadata fields of the source dataset that are constant within every subject (so they
# can colour a subject's trajectory), one row per subject.
subject_level_fields <- function(db, dataset_id, max_levels = 8) {
  sc <- dataset_subject_col(db, dataset_id)
  m <- dataset_sample_metadata(db, dataset_id)
  if (is.null(m) || is.na(sc) || !sc %in% names(m)) {
    return(NULL)
  }
  subj <- as.character(m[[sc]])
  fields <- setdiff(names(m), c("sample_id", sc))
  constant_in_subject <- function(f) {
    all(vapply(split(m[[f]], subj), function(x) length(unique(x[!is.na(x)])) <= 1, logical(1)))
  }
  const <- vapply(fields, constant_in_subject, logical(1))
  usable <- function(f) {
    v <- m[[f]]
    is.numeric(v) || length(unique(stats::na.omit(v))) <= max_levels
  }
  keep <- Filter(usable, fields[const])
  if (length(keep) == 0) {
    return(NULL)
  }
  out <- data.frame(subject_id = unique(subj), stringsAsFactors = FALSE)
  for (f in keep) {
    out[[f]] <- m[[f]][match(out$subject_id, subj)]
  }
  out
}

mod_trajectory_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 6), fill = FALSE,
      selectInput(ns("component"), "Factor", choices = NULL),
      selectInput(ns("colour"), "Colour subjects by", choices = "none"), uiOutput(ns("note"))
    ),
    p(
      class = "text-muted",
      "The fit's basis projected (projectR) onto each sibling per-timepoint dataset of the",
      "family; each line is one subject's weight on the factor across the time course.",
      "Only available for datasets whose family has per-timepoint siblings."
    ),
    plotOutput(ns("traj"), height = "520px")
  )
}

mod_trajectory_server <- function(id, state, focus, db) {
  moduleServer(id, function(input, output, session) {
    info <- state$info
    observe({
      ch <- component_choices(db, focus())
      cur <- isolate(input$component)
      updateSelectInput(
        session, "component",
        choices = ch,
        selected = if (!is.null(cur) && cur %in% ch) cur else ch[1]
      )
    })
    fields <- reactive(subject_level_fields(db, state$dataset()))
    observe({
      f <- fields()
      updateSelectInput(
        session, "colour",
        choices = c("none", if (!is.null(f)) setdiff(names(f), "subject_id"))
      )
    })
    output$note <- renderUI({
      n <- nrow(within_projections(db, focus()$fit_id))
      p(
        class = "text-muted mt-4", focus_label(info, focus()), ": ", n,
        " within-family projection", if (n != 1) "s"
      )
    })
    output$traj <- renderPlot({
      f <- focus()
      req(input$component)
      validate(need(f$has_loadings, NO_METHOD_LOADINGS))
      validate(need(
        nrow(within_projections(db, f$fit_id)) > 0,
        paste(
          "This fit has no within-family projections",
          "(needs a dataset with per-timepoint siblings)."
        )
      ))
      label <- names(component_choices(db, f))[as.integer(input$component)]
      d <- trajectory_data(db, f$fit_id, label)
      validate(need(
        !is.null(d), "No usable projections: the timepoint datasets need a subject id column."
      ))
      lev <- unique(d$dataset_id)
      pos <- family_positions(db, lev)
      d$dataset_id <- factor(d$dataset_id, levels = lev[order_timepoints(lev, pos)])
      fl <- fields()
      col <- input$colour
      if (!is.null(fl) && !identical(col, "none") && col %in% names(fl)) {
        d$colour <- fl[[col]][match(d$subject_id, fl$subject_id)]
      }
      p <- ggplot(d, aes(dataset_id, weight, group = subject_id))
      if ("colour" %in% names(d)) {
        d$colour <- if (is.numeric(d$colour)) d$colour else factor(d$colour)
        p <- ggplot(d, aes(dataset_id, weight, group = subject_id, colour = colour)) +
          geom_line(alpha = 0.5) +
          geom_point(alpha = 0.5, size = 1.5) +
          labs(colour = col)
      } else {
        p <- p +
          geom_line(alpha = 0.3, colour = "#4C78A8") +
          geom_point(alpha = 0.4, colour = "#4C78A8", size = 1.5)
      }
      p +
        labs(
          x = NULL, y = paste(label, "projected weight"),
          subtitle = paste(
            length(unique(d$subject_id)), "subjects across", length(lev), "timepoints"
          )
        ) +
        theme_minimal(base_size = 12)
    })
  })
}

## ---- Projection comparison -----------------------------------------------------------

mod_projection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 6), fill = FALSE,
      selectInput(ns("target"), "Project onto dataset", choices = NULL),
      selectInput(ns("fit_b"), "Compare with (Fit B)", choices = NULL), uiOutput(ns("note"))
    ),
    navset_card_tab(
      nav_panel(
        paste0("R", GLYPH_SQ, " by method"),
        p(
          class = "text-muted",
          "How much of the target's variation (after removing each gene's mean and each",
          "sample's offset) each basis explains, as a fraction of the most any basis of the",
          "same size could explain (the target's own top principal components): 1 is as good",
          "as the target's own, 0 is no better than chance. Grey: every fit; coloured:",
          "representative fits. sPCA runs lower by construction: a sparse basis reconstructs",
          "new data worse."
        ),
        plotOutput(ns("r2"), height = "420px")
      ),
      nav_panel(
        "Sample-space alignment",
        p(
          class = "text-muted",
          "Correlation between Fit A's and Fit B's projected per-sample weights on the target,",
          "samples matched by id."
        ),
        plotOutput(ns("samples"), height = "480px")
      ),
      nav_panel(
        "Gene-space alignment",
        p(
          class = "text-muted",
          "Fit A's loadings projected (least squares, as projectR does) onto Fit B's basis on",
          "shared genes. Axes are reordered by best one-to-one match, since factor numbering",
          "is arbitrary for most methods."
        ),
        plotOutput(ns("genes"), height = "520px")
      )
    )
  )
}

mod_projection_server <- function(id, state, focus, db) {
  moduleServer(id, function(input, output, session) {
    info <- state$info
    observe({
      t <- projection_targets(db, state$dataset())
      cur <- isolate(input$target)
      updateSelectInput(
        session, "target",
        choices = t, selected = if (!is.null(cur) && cur %in% t) cur else t[1]
      )
    })
    pair <- reactive({
      req(input$target)
      d <- projections_for_pair(db, state$dataset(), input$target)
      validate(need(nrow(d) > 0, "No projections of this dataset's fits onto that target."))
      d
    })
    reps <- reactive({
      d <- pair()
      d <- d[d$representative == 1, , drop = FALSE]
      d <- d[order(match(d$method, info$method), d$rank), ]
      d$label <- paste(method_label(info, d$method), "rank", d$rank)
      d
    })
    observe({
      d <- reps()
      d <- d[d$source_fit_id != focus()$fit_id, , drop = FALSE]
      ch <- setNames(d$source_fit_id, d$label)
      cur <- isolate(input$fit_b)
      updateSelectInput(
        session, "fit_b",
        choices = ch, selected = if (!is.null(cur) && cur %in% ch) cur else ch[1]
      )
    })
    output$note <- renderUI({
      p(
        class = "mt-4", "Fit A: ", tags$strong(state$dataset(), " / ", focus_label(info, focus())),
        " (sidebar). Both fits are from this dataset."
      )
    })
    output$r2 <- renderPlot({
      d <- pair()
      d$method_name <- factor(
        method_label(info, d$method),
        levels = method_label(info, info$method)
      )
      ggplot(d, aes(method_name, relative_r2)) +
        geom_boxplot(outlier.shape = NA, fill = "grey90") +
        geom_jitter(
          data = d[d$representative != 1, ], width = 0.15, alpha = 0.3, colour = "grey40"
        ) +
        geom_jitter(
          data = d[d$representative == 1, ], aes(colour = method), width = 0.15, size = 2
        ) +
        scale_colour_manual(values = method_colors(info), guide = "none") +
        labs(
          x = NULL, y = paste0("relative transfer R", GLYPH_SQ),
          subtitle = paste(state$dataset(), GLYPH_ARROW, input$target)
        ) +
        theme_minimal(base_size = 12)
    })
    proj_of <- function(fit_id, n = NULL) {
      d <- pair()
      i <- which(d$source_fit_id == as.integer(fit_id))[1]
      load_projection(db, d$projection_id[i], d$path[i], n)
    }
    fit_a <- reactive({
      f <- focus()
      validate(need(f$has_loadings, NO_METHOD_LOADINGS))
      validate(need(
        f$fit_id %in% pair()$source_fit_id, "The sidebar fit has no projection onto this target."
      ))
      f
    })
    output$samples <- renderPlot({
      req(input$fit_b)
      fa <- fit_a()
      validate(need(fa$fit_id != as.integer(input$fit_b), "Pick two different fits."))
      proj_a <- proj_of(fa$fit_id, if (fa$method == "pca") fa$n_factors)
      proj_b <- proj_of(input$fit_b)
      validate(need(!is.null(proj_a) && !is.null(proj_b), "Could not read a projection."))
      sa <- t(proj_a)
      sb <- t(proj_b)
      shared <- intersect(rownames(sa), rownames(sb))
      validate(need(length(shared) >= 3, "Too few shared samples."))
      cm <- stats::cor(sa[shared, , drop = FALSE], sb[shared, , drop = FALSE])
      la <- focus_label(info, fa)
      lb <- reps()$label[reps()$source_fit_id == as.integer(input$fit_b)]
      heatmap_plot(cm, c(-1, 1), "correlation") +
        labs(
          x = paste(lb, "(B)"), y = paste(la, "(A)"),
          subtitle = paste(length(shared), "shared samples")
        )
    })
    output$genes <- renderPlot({
      req(input$fit_b)
      fa <- fit_a()
      validate(need(fa$fit_id != as.integer(input$fit_b), "Pick two different fits."))
      loadings_a <- load_loadings(db, fa$fit_id, if (fa$method == "pca") fa$n_factors)
      loadings_b <- load_loadings(db, as.integer(input$fit_b))
      validate(need(!is.null(loadings_a) && !is.null(loadings_b), "One fit has no loadings."))
      # a least-squares fit on the stored top genes alone would not be this analysis
      validate(need(
        !from_database(loadings_a) && !from_database(loadings_b),
        paste(
          "Not available without the full loadings: the database keeps only the top 50 genes",
          "per side of each factor.", REDUCED_HINT
        )
      ))
      pr <- loadings_projection(loadings_a, loadings_b)
      validate(need(!is.null(pr), "Too few shared genes."))
      P <- pr$projection
      idx <- greedy_match_abs(P)
      ord <- order(idx[, "a"])
      P <- P[
        c(idx[ord, "a"], setdiff(seq_len(nrow(P)), idx[, "a"])),
        c(idx[ord, "b"], setdiff(seq_len(ncol(P)), idx[, "b"])),
        drop = FALSE
      ]
      la <- focus_label(info, fa)
      lb <- reps()$label[reps()$source_fit_id == as.integer(input$fit_b)]
      heatmap_plot(P, c(-1, 1), "coefficient") +
        labs(
          x = paste(la, "(A, projected)"), y = paste(lb, "(B, basis)"),
          subtitle = paste(pr$n_genes, "shared genes")
        )
    })
  })
}

## ---- Pattern drivers ---------------------------------------------------------------

mod_drivers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 5, 2, 2), fill = FALSE,
      selectInput(ns("factor"), "Factor", choices = NULL),
      selectInput(ns("cmp"), "Comparison", choices = NULL),
      selectInput(
        ns("mode"), "Test",
        choices = c("Confidence intervals" = "CI", "Welch tests" = "PV")
      ),
      uiOutput(ns("note"))
    ),
    p(
      class = "text-muted",
      "projectR's projectionDriveR: which genes differ between two sample groups AND carry",
      "weight on the factor. Only the combinations computed by the pipeline are listed",
      "(factors of the representative fits, categorical fields with few levels, combinations",
      "with at least one shared significant gene)."
    ),
    uiOutput(ns("reduced")),
    layout_columns(
      col_widths = c(7, 5), plotOutput(ns("plot"), height = "520px"), DTOutput(ns("table"))
    )
  )
}

mod_drivers_server <- function(id, state, focus, db) {
  moduleServer(id, function(input, output, session) {
    info <- state$info
    drivers <- reactive({
      f <- focus()
      fit_drivers(db, f$fit_id, f$n_factors)
    })
    output$note <- renderUI(p(class = "text-muted mt-4", focus_label(info, focus())))
    observe({
      d <- drivers()
      fac <- unique(d[c("factor_index", "label")])
      cur <- isolate(input$factor)
      updateSelectInput(
        session, "factor",
        choices = setNames(fac$factor_index, fac$label),
        selected = if (!is.null(cur) && cur %in% fac$factor_index) cur else fac$factor_index[1]
      )
    })
    observe({
      d <- drivers()
      req(input$factor)
      d <- d[d$factor_index == as.numeric(input$factor) & d$mode == input$mode, ]
      if (nrow(d) == 0) {
        return(updateSelectInput(session, "cmp", choices = character(0)))
      }
      key <- paste(d$grouping_col, d$group1_level, d$group2_level, sep = "|")
      updateSelectInput(
        session, "cmp",
        choices = setNames(
          key, paste0(d$grouping_col, ": ", d$group1_level, " vs ", d$group2_level)
        )
      )
    })
    result <- reactive({
      d <- drivers()
      validate(need(
        nrow(d) > 0,
        if (!focus()$has_loadings) {
          "No pattern drivers are computed for a method without loadings. Choose another method."
        } else {
          "No pattern drivers stored for this fit."
        }
      ))
      req(input$factor, input$cmp)
      p <- strsplit(input$cmp, "|", fixed = TRUE)[[1]]
      r <- d[
        d$factor_index == as.numeric(input$factor) & d$mode == input$mode &
          d$grouping_col == p[1] & d$group1_level == p[2] & d$group2_level == p[3],
      ]
      validate(need(nrow(r) >= 1, "No stored result for this combination."))
      res <- load_driver(db, r[1, ])
      validate(need(!is.null(res), "No result stored for this combination."))
      list(res = res, row = r[1, ])
    })
    output$reduced <- renderUI({
      x <- tryCatch(result(), error = function(e) NULL)
      driver_notice(x$res, x$row$mode)
    })
    labels <- reactive(gene_labels(db, state$dataset()))
    output$plot <- renderPlot({
      x <- result()
      res <- x$res
      lab <- labels()
      sg <- res$gene[res$shared]
      if (input$mode == "CI") {
        validate(need(length(sg) > 0, "No shared significant genes."))
        ci <- res[res$shared, ]
        ci$mid <- (ci$low + ci$high) / 2
        ci <- head(ci[order(-abs(ci$mid)), ], 25)
        ci$g <- factor(label_genes(lab, ci$gene), levels = rev(label_genes(lab, ci$gene)))
        ggplot(ci, aes(y = g)) +
          geom_linerange(aes(xmin = low, xmax = high), colour = "#4C78A8", linewidth = 1) +
          geom_point(aes(mid), size = 2) +
          geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
          labs(
            x = "weighted mean difference (confidence interval)", y = NULL,
            subtitle = paste(
              x$row$group1_level, "vs", x$row$group2_level, "-", length(sg),
              "shared significant genes"
            )
          ) +
          theme_minimal(base_size = 12)
      } else {
        ms <- res[!is.na(res$mean_diff), ]
        ms$sig <- ms$shared
        ms$logp <- -log10(pmax(ms$padj, 1e-300))
        ggplot(ms, aes(mean_diff, logp, colour = sig)) +
          geom_point(alpha = 0.5) +
          ggrepel::geom_text_repel(
            data = ms[ms$sig, ], aes(label = label_genes(lab, gene)), size = 3.5,
            max.overlaps = Inf, show.legend = FALSE
          ) +
          scale_colour_manual(
            values = c(`TRUE` = "#B2182B", `FALSE` = "grey60"),
            labels = c(`TRUE` = "shared significant", `FALSE` = "other"), name = NULL
          ) +
          labs(x = "weighted mean difference", y = expression(-log[10] ~ adjusted ~ p)) +
          theme_minimal(base_size = 12) +
          theme(legend.position = "top")
      }
    })
    output$table <- renderDT({
      res <- result()$res
      lab <- labels()
      sg <- res[res$shared, ]
      validate(need(nrow(sg) > 0, "No shared significant genes."))
      d <- data.frame(Gene = label_genes(lab, sg$gene), Weight = round(sg$weight, 3))
      datatable(
        d[order(-abs(d$Weight)), ],
        rownames = FALSE, selection = "none",
        options = list(dom = "ftp", pageLength = 12)
      )
    })
  })
}
