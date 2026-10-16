# Drives the real app against a real schema-4 database: opt-in, set STABILITY_DB. Every
# page's outputs for the optimal fit of each method on two datasets render without error.

# A render that stops with validate()/need() is the app's "Not available" notice, which is
# the intended behaviour for data a dataset does not have; any other error is a failure.
render_ok <- function(expr) {
  tryCatch(
    {
      expr
      "ok"
    },
    shiny.silent.error = function(e) "ok",
    error = function(e) conditionMessage(e)
  )
}

test_that("every output of the fit pages renders for each method's optimal fit", {
  db <- local_real_db()
  ids <- c(
    "explore-stab", "explore-mse", "explore-is_curve", "explore-scree",
    "explore-factor_stab", "explore-seedpair", "explore-ari", "explore-power_summary",
    "explore-iq", "xdata-heat", "xdata-nearest", "explore-sizes", "explore-kme",
    "explore-dendro", "explore-overlap", "factor-kme_hist", "factor-gs",
    "factor-genes_table", "factor-genes", "factor-scores", "meta-grid", "biplot-genes",
    "biplot-meta", "traj-traj", "drivers-plot"
  )
  failures <- character()
  testServer(app(Sys.getenv("STABILITY_DB")), {
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
        grDevices::png(tempfile(fileext = ".png"))
        results <- vapply(ids, function(id) render_ok(output[[id]]), character(1))
        grDevices::dev.off()
        bad <- results != "ok"
        failures <- c(failures, paste(ds, m, names(results)[bad], results[bad]))
      }
    }
  })
  expect_equal(failures, character())
})

test_that("the variance pages render for every dataset", {
  db <- local_real_db()
  outs <- c(
    "notices", "bins", "terms", "time_genes", "within_text", "traj_plot", "time_hist",
    "within_table", "sex_text", "sex_plot", "nl_text", "nl_plot", "trait_plot", "cor_plot",
    "design_text", "cell_text", "cell_grid", "terms_text", "terms_explained", "de_text", "de"
  )
  for (ds in list_datasets(db)) {
    testServer(
      mod_variance_server,
      args = list(state = list(dataset = reactive(ds)), db = db),
      {
        res <- vapply(outs, function(o) render_ok(output[[o]]), character(1))
        expect_equal(unname(res[res != "ok"]), character(), info = ds)
      }
    )
  }
})
