# Explore fits: how one method behaves across its hyperparameter on the selected dataset
# -- stability across seeds, quality, and per-factor detail. `state` as in mod_compare.R.

mod_explore_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("blurb")),
    uiOutput(ns("tabs"))
  )
}

# The tabs of one method. The methods table says whether a method has loadings (WGCNA has
# modules) and its family (seed sweep or parameter grid). The cases it cannot say stay
# here: PCA is a single fit whose first n components stand for rank n, so it has only a
# variance-by-rank view, and ICASSO cluster quality exists only for ICA.
explore_panels <- function(ns, method, row) {
  if (row$has_loadings == 0) {
    return(list(
      nav_panel("Stability across powers", plotOutput(ns("ari"), height = "460px")),
      nav_panel("Modules and scale-free fit", plotOutput(ns("power_summary"), height = "420px")),
      nav_panel(
        "This power: modules",
        uiOutput(ns("power_note")),
        layout_columns(
          col_widths = c(6, 6), plotOutput(ns("sizes"), height = "380px"),
          plotOutput(ns("kme"), height = "380px")
        )
      ),
      nav_panel("This power: gene dendrogram", plotOutput(ns("dendro"), height = "560px")),
      nav_panel(
        "This power vs another",
        selectInput(ns("other"), "Compare with power", choices = NULL),
        plotOutput(ns("overlap"), height = "520px")
      )
    ))
  }
  if (method == "pca") {
    return(list(nav_panel("Variance by rank", plotOutput(ns("scree"), height = "420px"))))
  }
  grid <- row$family == "param_grid"
  c(
    list(
      nav_panel("Stability vs rank", plotOutput(ns("stab"), height = "420px")),
      nav_panel(
        "Quality vs rank",
        plotOutput(ns(if (grid) "is_curve" else "mse"), height = "420px")
      ),
      nav_panel(
        "One rank in detail",
        uiOutput(ns("rank_note")),
        layout_columns(
          col_widths = c(6, 6),
          plotOutput(ns("factor_stab"), height = "380px"),
          if (grid) NULL else plotOutput(ns("seedpair"), height = "380px")
        )
      )
    ),
    if (method == "ica") {
      list(nav_panel("ICASSO cluster quality", plotOutput(ns("iq"), height = "420px")))
    }
  )
}

mod_explore_server <- function(id, state, focus, db) {
  moduleServer(id, function(input, output, session) {
    info <- state$info
    method <- reactive(focus()$method)
    color <- reactive(method_colors(info)[[method()]])
    row <- reactive(info[info$method == method(), ])
    output$blurb <- renderUI({
      p(
        class = "text-muted",
        tags$strong(state$dataset(), " / ", method_label(info, method())), ": ", row()$blurb
      )
    })

    fits <- reactive(method_fits(db, state$dataset(), method()))

    output$tabs <- renderUI({
      do.call(navset_card_tab, explore_panels(session$ns, method(), row()))
    })

    output$scree <- renderPlot({
      d <- pca_scree(db, state$dataset())
      validate(need(!is.null(d), "No PCA diagnostics for this dataset."))
      long <- rbind(
        data.frame(rank = d$rank, panel = "reconstruction error (mse)", value = d$mse),
        data.frame(rank = d$rank, panel = "cumulative variance explained", value = d$pev)
      )
      ggplot(long, aes(rank, value)) +
        geom_line(colour = color()) +
        geom_point(colour = color()) +
        facet_wrap(~panel, scales = "free_y", ncol = 1) +
        labs(x = "rank", y = NULL) +
        theme_minimal(base_size = 12)
    })

    output$stab <- renderPlot({
      d <- same_rank_matches(db, state$dataset(), method(), state$metric())
      validate(need(nrow(d) > 0, "No same-rank fit pairs for this method."))
      nul <- same_rank_null(db, state$dataset(), method(), state$metric())
      d$rank <- factor(d$rank, levels = sort(unique(d$rank)))
      nul$rank <- factor(nul$rank, levels = levels(d$rank))
      ggplot(d, aes(rank, value, fill = kind)) +
        geom_boxplot(outlier.size = 0.6, position = position_dodge(preserve = "single")) +
        geom_point(
          data = nul, aes(rank, p95), inherit.aes = FALSE, shape = 95, size = 9, colour = "grey30"
        ) +
        scale_fill_manual(
          values = c("seeds only" = "#4C78A8", "with bootstrap" = "#F58518"), name = NULL
        ) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(
          x = "rank", y = paste("matched |", state$metric(), "|"),
          subtitle = paste(
            "Each value is one factor's best match in another fit of the same rank.",
            "Grey bars: chance level (95th percentile of unmatched pairs)."
          )
        ) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "top")
    })

    output$mse <- renderPlot({
      f <- fits()
      f <- f[f$status == "ok" & !is.na(f$mse) & (is.na(f$bootstrap) | f$bootstrap == 0), ]
      validate(need(nrow(f) > 0, "No reconstruction errors for this method."))
      rep <- f[f$representative == 1, ]
      rep <- rep[order(rep$rank), ]
      ggplot(f, aes(rank, mse)) +
        geom_point(alpha = 0.5, colour = "grey40") +
        geom_line(data = rep, colour = color()) +
        geom_point(data = rep, colour = color(), size = 3) +
        labs(
          x = "rank", y = "reconstruction error (mse)",
          subtitle = "Grey: every seed. Coloured: the representative (lowest-error) seed per rank."
        ) +
        theme_minimal(base_size = 12)
    })

    output$is_curve <- renderPlot({
      d <- spca_is_table(db, state$dataset())
      validate(need(!is.null(d), "No sPCA metrics for this dataset."))
      d$rank <- factor(d$rank)
      d$chosen <- d$is_selected == 1
      ggplot(d, aes(para, index_of_sparseness, colour = rank, group = rank)) +
        geom_line(alpha = 0.6) +
        geom_point(data = d[d$chosen, ], size = 3, shape = 21, fill = "white", stroke = 1.2) +
        scale_x_log10() +
        labs(
          x = "penalty (para, log scale)", y = "Index of Sparseness",
          subtitle = paste(
            "Open circles: the penalty chosen for each rank",
            "(maximum of the curve after refinement)."
          )
        ) +
        theme_minimal(base_size = 12)
    })

    output$rank_note <- renderUI({
      p(class = "text-muted", "Rank ", focus()$rank_key, " (change it in the sidebar).")
    })
    rank <- reactive(focus()$rank_key)
    rep_fit <- reactive({
      f <- fits()
      r <- rank()
      f$fit_id[f$status == "ok" & f$representative == 1 & !is.na(f$rank) & f$rank == r][1]
    })
    output$factor_stab <- renderPlot({
      d <- fit_factor_stability(db, rep_fit(), state$metric())
      validate(need(
        nrow(d) > 0 && any(!is.na(d$stability)), "No per-factor stability for this fit."
      ))
      d$f <- factor(d$label %||% d$factor_index, levels = d$label %||% d$factor_index)
      ggplot(d, aes(f, stability)) +
        geom_col(fill = color()) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(
          x = NULL, y = paste("stability |", state$metric()),
          subtitle = "Representative fit, factor by factor"
        ) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
    })
    output$seedpair <- renderPlot({
      m <- seed_pair_matrix(db, state$dataset(), method(), rank(), state$metric())
      validate(need(!is.null(m), "No seed pairs at this rank."))
      heatmap_plot(m, c(0, 1), "mean matched", digits = if (nrow(m) > 8) 1 else 2) +
        labs(subtitle = "Seed x seed: mean matched similarity of the two fits") +
        theme(axis.text.x = element_text(angle = 90))
    })

    output$iq <- renderPlot({
      d <- icasso_iq(db, state$dataset())
      validate(need(nrow(d) > 0, "No ICASSO clusters for this dataset."))
      d$rank <- factor(d$rank)
      ggplot(d, aes(rank, iq)) +
        geom_hline(yintercept = 0.7, linetype = 2, colour = "grey40") +
        geom_boxplot(outlier.shape = NA, fill = NA) +
        geom_jitter(width = 0.15, height = 0, alpha = 0.6, colour = color()) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(
          x = "rank (n.comp)", y = "cluster quality Iq",
          subtitle = paste(
            "One point per ICASSO cluster; Iq of 0.7 or more (dashed) is",
            "conventionally trustworthy."
          )
        ) +
        theme_minimal(base_size = 12)
    })

    output$ari <- renderPlot({
      m <- wgcna_ari_matrix(db, state$dataset())
      validate(need(!is.null(m), "No WGCNA fit pairs for this dataset."))
      heatmap_plot(m, c(0, 1), "ARI", digits = 1) +
        labs(
          x = "power", y = "power",
          subtitle = "Module assignment agreement between soft-threshold powers"
        )
    })
    output$power_summary <- renderPlot({
      d <- wgcna_power_summary(db, state$dataset())
      validate(need(nrow(d) > 0, "No WGCNA fits for this dataset."))
      long <- rbind(
        data.frame(power = d$power, panel = "modules", value = d$n_modules),
        data.frame(power = d$power, panel = "unassigned genes", value = d$n_unassigned),
        data.frame(
          power = d$power, panel = paste0("scale-free fit R", GLYPH_SQ), value = d$sft_r_sq
        )
      )
      ggplot(long, aes(power, value)) +
        geom_line(colour = color(), na.rm = TRUE) +
        geom_point(colour = color(), na.rm = TRUE) +
        facet_wrap(~panel, scales = "free_y", ncol = 1) +
        labs(x = "soft-threshold power", y = NULL) +
        theme_minimal(base_size = 12)
    })
    # modules: the sidebar power's network (focus fit)
    wfit <- reactive({
      f <- focus()
      req(!f$has_loadings)
      f$fit_id
    })
    gm <- reactive(wgcna_gene_modules(db, wfit())) |> bindCache(focus()$fit_id)
    pal <- reactive(module_palette(gm()$module))
    observe({
      req(!focus()$has_loadings)
      f <- wgcna_fits(db, state$dataset())
      updateSelectInput(session, "other", choices = setdiff(f$power, focus()$rank_key))
    })
    output$power_note <- renderUI({
      g <- gm()
      p(
        class = "text-muted", "Power ", focus()$rank_key, ": ", sum(g$module != 0), " of ", nrow(g),
        " genes in ", length(unique(g$module[g$module != 0])), " modules; ", sum(g$module == 0),
        " unassigned. Change the power in the sidebar."
      )
    })
    output$sizes <- renderPlot({
      g <- gm()
      n <- as.data.frame(table(module = g$module), stringsAsFactors = FALSE)
      n$module <- as.character(n$module)
      n$module <- factor(n$module, levels = n$module[order(-n$Freq)])
      ggplot(n, aes(.data$module, Freq, fill = .data$module)) +
        geom_col() +
        scale_fill_manual(values = pal(), guide = "none") +
        labs(x = "module (0 = unassigned)", y = "genes") +
        theme_minimal(base_size = 12)
    })
    output$kme <- renderPlot({
      s <- wgcna_kme_summary(db, wfit())
      validate(need(nrow(s) > 0, "No kME summary stored."))
      s$value <- ifelse(s$grp == "permutation", s$p95, s$mean)
      s$grp <- factor(
        s$grp,
        levels = c("member", "nonmember", "permutation"),
        labels = c("members (mean)", "non-members (mean)", "permutation null (p95)")
      )
      s$module <- factor(s$module)
      ggplot(s, aes(module, value, colour = grp)) +
        geom_point(size = 2.5) +
        scale_colour_manual(values = c("#B2182B", "#4C78A8", "grey50"), name = NULL) +
        labs(x = "module", y = "kME") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "top")
    })
    output$dendro <- renderPlot({
      hc <- wgcna_dendrogram(db, wfit(), state$dataset())
      validate(need(!is.null(hc), "No dendrogram stored for this fit."))
      g <- gm()
      mod <- g$module[match(hc$labels[hc$order], g$gene)]
      mod[is.na(mod)] <- 0
      cols <- pal()[as.character(mod)]
      n <- length(mod)
      op <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(op))
      graphics::layout(matrix(1:2, 2), heights = c(4, 1))
      graphics::par(mar = c(0, 4, 2, 1), xaxs = "i")
      plot(
        hc,
        labels = FALSE, hang = 0.03, main = "Gene dendrogram", xlab = "", sub = "",
        ylab = "height", xlim = c(0.5, n + 0.5)
      )
      graphics::par(mar = c(1, 4, 0, 1))
      plot(NA, xlim = c(0.5, n + 0.5), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
      graphics::rect(seq_len(n) - 0.5, 0, seq_len(n) + 0.5, 1, col = cols, border = NA)
    })
    output$overlap <- renderPlot({
      req(input$other)
      f <- wgcna_fits(db, state$dataset())
      m <- wgcna_module_overlap(db, wfit(), f$fit_id[f$power == as.numeric(input$other)])
      validate(need(!is.null(m), "No modules to compare."))
      heatmap_plot(m, c(0, 1), "Jaccard", digits = 1) +
        labs(x = paste("power", input$other), y = paste("power", focus()$rank_key))
    })
  })
}
