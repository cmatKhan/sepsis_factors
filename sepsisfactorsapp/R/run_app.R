# The Shiny app: `run_app()` opens the database the user names and serves the pages in
# mod_*.R. Pages: Overview and Variance by dataset (from the stored variancePartition
# results), Compare, Explore fits, Inspect fit and Across datasets. Data that is missing
# (a method not yet run, a table with no rows) is noted on the page and in the console
# log (notes.R), never an error.
#
# Layout: a sidebar holds the choices every page shares (dataset, methods, similarity
# metric, how the "optimal" fit is picked); the main panel is the Compare page, whose four
# tabs answer
#   1. what is the optimal hyperparameter for each method on this dataset
#   2. which method is best / most stable on this dataset
#   3. how similar are the optimal factors across methods
#   4. how similar are the factors across datasets (same method)
# Further pages: Explore fits (one method across its hyperparameter), Inspect fit (factor
# detail, enrichment, metadata, biplots, pattern drivers), and Across datasets
# (trajectory across time, projection comparison).

# The database layout this app reads (`PRAGMA user_version`), written by sepsisfactorsdb.
EXPECTED_SCHEMA_VERSION <- 4L

#' Run the latent-factor stability explorer
#'
#' Opens a stability database read-only and serves the app. The database alone is
#' enough. If a `stability_artifacts/` folder (written with the database by
#' sepsisfactorsdb) sits beside it, the pages use the full loadings, scores, projections
#' and pattern-driver results from it; without it they use the reduced copies stored in
#' the database and say where that limits what a page shows. Nothing is copied or
#' written.
#'
#' Get the database with [download_db()], then
#' `run_app("db_no_rose.sqlite")`.
#'
#' @param db_path Path to the SQLite file written by sepsisfactorsdb. Defaults to the
#'   environment variable `STABILITY_DB`.
#' @param ... Passed to [shiny::runApp()] (e.g. `port`, `launch.browser`).
#' @return Does not return while the app runs.
#' @export
run_app <- function(db_path = Sys.getenv("STABILITY_DB"), ...) {
  shiny::runApp(app(db_path), ...)
}

#' The app as a shiny app object
#'
#' @inheritParams run_app
#' @return A [shiny::shinyApp()].
#' @export
app <- function(db_path = Sys.getenv("STABILITY_DB")) {
  if (!nzchar(db_path)) {
    cli::cli_abort(c(
      "No database given.",
      "i" = "Pass {.arg db_path} or set the {.envvar STABILITY_DB} environment variable.",
      "i" = "{.fn download_db} fetches the published database."
    ))
  }
  if (!file.exists(db_path)) {
    cli::cli_abort("No stability database at {.file {db_path}}.")
  }
  db <- open_db(db_path)
  version <- db_schema_version(db)
  if (version != EXPECTED_SCHEMA_VERSION) {
    close_db(db)
    cli::cli_abort(c(
      paste0(
        "{.file {db_path}} has schema version {version}, ",
        "but this app reads version {EXPECTED_SCHEMA_VERSION}."
      ),
      "i" = "Use the version of sepsisfactorsapp that matches the database."
    ))
  }
  info <- methods_table(db)
  shiny::shinyApp(
    app_ui(list_datasets(db)),
    app_server(db, info),
    onStart = function() {
      shiny::onStop(function() close_db(db))
    }
  )
}

app_ui <- function(datasets) {
  page_navbar(
    id = "page",
    title = "Latent-factor stability",
    theme = bs_theme(bootswatch = "flatly", base_font_size = "1.05rem"),
    header = tags$style(paste(
      ".dataTables_wrapper {font-size: 1rem;}",
      "table.dataTable td, table.dataTable th {padding: 8px 12px;}"
    )),
    sidebar = sidebar(
      id = "main_sidebar",
      width = 300,
      selectInput("dataset", "Dataset", choices = datasets),
      selectInput("fit_method", "Method", choices = NULL),
      selectInput("fit_rank", "Rank", choices = NULL),
      radioButtons("metric", "Similarity metric", choices = STABILITY_METRICS, inline = TRUE),
      accordion(
        open = FALSE,
        accordion_panel(
          "Compare settings",
          helpText("Used only by the Compare page, which sets several methods side by side."),
          checkboxGroupInput("methods", "Methods to compare", choices = NULL),
          radioButtons("rule", "Optimal fit rule", choices = c(
            "Highest quality that is still stable" = "stable",
            "Highest in-sample quality" = "max_r2"
          )),
          conditionalPanel(
            "input.rule == 'stable'",
            sliderInput(
              "threshold", "Minimum stability",
              min = 0.5, max = 1, value = 0.9, step = 0.01
            )
          ),
          helpText(
            "Quality rises with rank, so the max-quality rule picks the largest rank.",
            "The stable rule picks the largest rank whose factors still reproduce across seeds."
          )
        )
      )
    ),
    nav_panel(
      "Overview",
      value = "overview", fillable = FALSE, mod_variance_overview_ui("overview")
    ),
    nav_panel("Variance by dataset", fillable = FALSE, mod_variance_ui("variance")),
    nav_panel(
      "Compare",
      uiOutput("coverage"),
      navset_card_tab(
        id = "compare_tab",
        nav_panel("1. Optimal hyperparameter", mod_hyperparam_ui("hyper")),
        nav_panel("2. Best method", mod_best_method_ui("best")),
        nav_panel("3. Across methods", mod_cross_method_ui("xmethod")),
        nav_panel("4. Across datasets", mod_cross_dataset_ui("xdata"))
      )
    ),
    nav_panel("Explore fits", mod_explore_ui("explore")),
    nav_menu(
      "Inspect fit",
      nav_panel("Factor detail", mod_factor_ui("factor")),
      nav_panel("Enrichment", mod_enrichment_ui("enrich")),
      nav_panel("Metadata", mod_metadata_ui("meta")),
      nav_panel("Biplots", mod_biplot_ui("biplot")),
      nav_panel("Pattern drivers", mod_drivers_ui("drivers"))
    ),
    nav_menu(
      "Across datasets",
      nav_panel("Trajectory across time", mod_trajectory_ui("traj")),
      nav_panel("Projection comparison", mod_projection_ui("proj"))
    )
  )
}

app_server <- function(db, info) {
  function(input, output, session) {
    present <- reactive(methods_for_dataset(db, info, input$dataset))

    observeEvent(present(), {
      p <- present()
      updateCheckboxGroupInput(
        session, "methods",
        choices = setNames(p, method_label(info, p)), selected = p, inline = TRUE
      )
    })

    curves <- reactive({
      req(input$dataset)
      dataset_curves(db, input$dataset, present(), input$metric)
    }) |> bindCache(input$dataset, input$metric)

    optimal <- reactive({
      cv <- curves()
      cv <- cv[cv$method %in% input$methods, , drop = FALSE]
      optimal_fits(cv, info, input$rule, input$threshold)
    })

    state <- list(
      dataset = reactive(input$dataset), metric = reactive(input$metric),
      methods = reactive(input$methods), curves = curves, optimal = optimal, info = info
    )

    # The fit every page looks at: the sidebar's method at a rank (default: that method's
    # optimal fit). Pages that need a second fit or dataset have their own selector.
    observe({
      p <- present()
      req(length(p) > 0)
      cur <- isolate(input$fit_method)
      updateSelectInput(
        session, "fit_method",
        choices = setNames(p, method_label(info, p)),
        selected = if (!is.null(cur) && cur %in% p) cur else p[1]
      )
    })
    observeEvent(list(input$fit_method, curves(), optimal()), {
      req(input$fit_method)
      cv <- curves()
      cv <- cv[cv$method == input$fit_method, , drop = FALSE]
      opt <- optimal()
      o <- opt$rank_key[opt$method == input$fit_method]
      by_power <- length(cv$rank_label) > 0 && startsWith(cv$rank_label[1], "power")
      updateSelectInput(
        session, "fit_rank",
        label = if (by_power) "Power" else "Rank",
        choices = setNames(cv$rank_key, cv$rank_label),
        selected = if (length(o) == 1) o else cv$rank_key[1]
      )
    })
    focus <- reactive({
      req(input$fit_method, input$fit_rank)
      cv <- curves()
      is_fit <- cv$method == input$fit_method & cv$rank_key == as.numeric(input$fit_rank)
      r <- cv[is_fit, , drop = FALSE]
      req(nrow(r) == 1)
      list(
        method = r$method, fit_id = r$fit_id, n_factors = r$n_factors,
        rank_label = r$rank_label, rank_key = r$rank_key,
        has_loadings = method_has_loadings(info, r$method)
      )
    })

    output$coverage <- renderUI({
      cov <- fit_coverage(db, input$dataset)
      miss <- setdiff(info$method, cov$method)
      if (length(miss) == 0) {
        return(NULL)
      }
      labs <- paste(method_label(info, miss), collapse = ", ")
      note_missing(paste("fits for", labs), paste("dataset", input$dataset))
      p(class = "text-muted small", "No fits for this dataset: ", labs)
    })

    mod_hyperparam_server("hyper", state)
    mod_best_method_server("best", state, db)
    mod_cross_method_server("xmethod", state, db)
    mod_cross_dataset_server("xdata", state, focus, db)
    mod_variance_overview_server("overview", db)
    mod_variance_server("variance", state, db)
    # the overview covers every dataset, so the sidebar is only offered on the other pages
    observeEvent(input$page, bslib::toggle_sidebar("main_sidebar", open = input$page != "overview"))
    mod_explore_server("explore", state, focus, db)
    mod_factor_server("factor", state, focus, db)
    mod_enrichment_server("enrich", state, focus, db)
    mod_metadata_server("meta", state, focus, db)
    mod_biplot_server("biplot", state, focus, db)
    mod_drivers_server("drivers", state, focus, db)
    mod_trajectory_server("traj", state, focus, db)
    mod_projection_server("proj", state, focus, db)
  }
}
