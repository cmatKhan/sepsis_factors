# A render that stops with validate()/need() is the app's "Not available" notice,
# which is the intended behaviour for data a dataset does not have; any other
# error is a failure.
render_ok <- function(expr) {
  tryCatch({ expr; "ok" },
    shiny.silent.error = function(e) "ok",
    error = function(e) conditionMessage(e))
}

# Drives the real app against STABILITY_DB: every page's outputs for the
# optimal fit of each method on two datasets render without error.
test_that("the app starts and its sidebar choices flow through", {
  skip_if_no_db()
  db <- local_real_db()
  testServer(app(Sys.getenv("STABILITY_DB")), {
    ds <- intersect(c("EARLI", "ANEMONES"), list_datasets(db))[1]
    session$setInputs(dataset = ds, metric = "cosine", rule = "stable", threshold = 0.9)
    session$flushReact()
    session$setInputs(methods = present())
    session$flushReact()
    expect_gt(nrow(optimal()), 0)
    session$setInputs(rule = "max_r2")
    expect_gt(nrow(optimal()), 0)
    session$setInputs(methods = character(0))
    session$flushReact()
  })
})

test_that("every output of the fit pages renders for each method's optimal fit", {
  skip_if_no_db()
  failures <- character()
  db <- local_real_db()
  testServer(app(Sys.getenv("STABILITY_DB")), {
    ids <- c(
      "explore-stab", "explore-mse", "explore-is_curve", "explore-scree",
      "explore-factor_stab", "explore-seedpair", "explore-ari",
      "explore-power_summary", "explore-iq", "xdata-heat", "xdata-nearest",
      "explore-sizes", "explore-kme", "explore-dendro", "explore-overlap",
      "factor-kme_hist", "factor-gs", "factor-genes_table", "factor-genes",
      "factor-scores", "meta-grid", "biplot-genes", "biplot-meta", "traj-traj",
      "drivers-plot"
    )
    for (ds in intersect(c("EARLI", "ANEMONES"), list_datasets(db))) {
      session$setInputs(dataset = ds, metric = "cosine", rule = "stable", threshold = 0.9)
      session$flushReact()
      session$setInputs(methods = present())
      session$flushReact()
      for (m in present()) {
        session$setInputs(fit_method = m)
        session$flushReact()
        session$setInputs(fit_rank = as.character(optimal()$rank_key[optimal()$method == m]))
        session$flushReact()
        session$setInputs(
          `explore-other` = "10", `factor-gs_field` = "sex", `xdata-kind` = "r2",
          `factor-top_n` = 20, `biplot-top_n` = 15, `factor-factor` = "1",
          `biplot-x` = "1", `biplot-y` = "2"
        )
        session$flushReact()
        png(tempfile(fileext = ".png"))
        for (id in ids) {
          # the Explore page does not show the seed x seed heatmap for sPCA
          if (m == "spca" && id == "explore-seedpair") next
          res <- render_ok(output[[id]])
          if (res != "ok") failures <<- c(failures, paste(ds, m, id, res))
        }
        dev.off()
      }
    }
  })
  expect_equal(failures, character())
})

test_that("the variance pages render for every dataset", {
  con <- local_real_db()
  failures <- character()
  outs <- c(
    "notices", "bins", "terms", "time_genes", "within_text", "traj_plot",
    "time_hist", "within_table", "sex_text", "sex_plot", "nl_text", "nl_plot",
    "trait_plot", "cor_plot", "design_text", "cell_text", "cell_grid",
    "terms_text", "terms_explained", "de_text", "de"
  )
  for (ds in list_datasets(con)) {
    testServer(mod_variance_server, args = list(state = list(dataset = reactive(ds)), con = con), {
      for (o in outs) {
        res <- render_ok(output[[o]])
        if (res != "ok") failures <<- c(failures, paste(ds, o, res))
      }
    })
  }
  expect_equal(failures, character())
})
