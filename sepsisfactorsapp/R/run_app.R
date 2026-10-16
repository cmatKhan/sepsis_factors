# The Shiny app: `run_app()` opens the database the user names and serves the
# pages in mod_*.R. Pages: Overview and Variance by dataset (from the stored
# variancePartition results), Compare, Explore fits, Inspect fit and Across
# datasets. Data that is missing (a method not yet run, a table from an older
# schema) is noted on the page and in the console log (notes.R), never an error.
#
# Layout: a sidebar holds the choices every page shares (dataset, methods,
# similarity metric, how the "optimal" fit is picked); the main panel is the
# Compare page, whose four tabs answer
#   1. what is the optimal hyperparameter for each method on this dataset
#   2. which method is best / most stable on this dataset
#   3. how similar are the optimal factors across methods
#   4. how similar are the factors across datasets (same method)
# Further pages: Explore fits (one method across its hyperparameter), Inspect fit
# (factor detail, enrichment, metadata, biplots, pattern drivers), and Across
# datasets (trajectory across time, projection comparison).

# The database layout this app reads (`PRAGMA user_version`), written by
# sepsisfactorsdb.
EXPECTED_SCHEMA_VERSION <- 3L

#' Run the latent-factor stability explorer
#'
#' Opens a stability database read-only and serves the app. The app reads the
#' database and its artifact files where they are: nothing is copied. Artifact
#' paths in the database are relative to the database's directory.
#'
#' @param db_path Path to the SQLite file written by sepsisfactorsdb. Defaults
#'   to the environment variable `STABILITY_DB`.
#' @param config_dir Optional directory holding `dataset_families.yml` (the
#'   timepoint order within a dataset family), `composition_markers.yml` (marker
#'   genes of the composition scores) and the per-dataset `*_config.yml` files
#'   (for links to the documentation). Defaults to the environment variable
#'   `SEPSISFACTORSAPP_CONFIG`. Without them, the app falls back to ordering
#'   timepoints by the number in the dataset id and omits the marker list and
#'   documentation links, and says so in the console log.
#' @param ... Passed to [shiny::runApp()] (e.g. `port`, `launch.browser`).
#' @return Does not return while the app runs.
#' @export
run_app <- function(
  db_path = Sys.getenv("STABILITY_DB"),
  config_dir = Sys.getenv("SEPSISFACTORSAPP_CONFIG"),
  ...
) {
  shiny::runApp(app(db_path, config_dir), ...)
}

#' The app as a shiny app object
#'
#' @inheritParams run_app
#' @return A [shiny::shinyApp()].
#' @export
app <- function(db_path = Sys.getenv("STABILITY_DB"), config_dir = Sys.getenv("SEPSISFACTORSAPP_CONFIG")) {
  if (!nzchar(db_path)) {
    cli::cli_abort(c(
      "No database given.",
      "i" = "Pass {.arg db_path} or set the {.envvar STABILITY_DB} environment variable."
    ))
  }
  if (!file.exists(db_path)) {
    cli::cli_abort("No stability database at {.file {db_path}}.")
  }
  if (nzchar(config_dir) && !dir.exists(config_dir)) {
    cli::cli_abort("{.arg config_dir} {.file {config_dir}} does not exist.")
  }
  # arrow-backed string columns in old metadata artifacts need it (data_helpers.R)
  requireNamespace("arrow", quietly = TRUE)

  con <- connect_read_only(db_path)
  version <- DBI::dbGetQuery(con, "PRAGMA user_version")[[1]]
  if (version != EXPECTED_SCHEMA_VERSION) {
    DBI::dbDisconnect(con)
    cli::cli_abort(c(
      "{.file {db_path}} has schema version {version}, but this app reads version {EXPECTED_SCHEMA_VERSION}.",
      "i" = "Use the version of sepsisfactorsapp that matches the database."
    ))
  }
  old <- options(
    stability.db_dir = normalizePath(dirname(db_path)),
    sepsisfactorsapp.config_dir = if (nzchar(config_dir)) normalizePath(config_dir)
  )
  shiny::shinyApp(
    app_ui(list_datasets(con)),
    app_server(con),
    onStart = function() {
      shiny::onStop(function() {
        DBI::dbDisconnect(con)
        options(old)
      })
    }
  )
}

# The config directory (NA when none was given) and a file in it (NA when absent).
config_dir <- function() {
  dir <- getOption("sepsisfactorsapp.config_dir")
  if (is.null(dir) || !nzchar(dir)) NA_character_ else dir
}

config_file <- function(name) {
  dir <- config_dir()
  path <- if (is.na(dir)) NA_character_ else file.path(dir, name)
  if (!is.na(path) && file.exists(path)) path else NA_character_
}

app_ui <- function(datasets) {
  page_navbar(
    id = "page",
    title = "Latent-factor stability",
    theme = bs_theme(bootswatch = "flatly", base_font_size = "1.05rem"),
    header = tags$style(".dataTables_wrapper {font-size: 1rem;} table.dataTable td, table.dataTable th {padding: 8px 12px;}"),
    sidebar = sidebar(
      id = "main_sidebar",
      width = 300,
      selectInput("dataset", "Dataset", choices = datasets),
      selectInput("fit_method", "Method", choices = NULL),
      selectInput("fit_rank", "Rank", choices = NULL),
      radioButtons("metric", "Similarity metric", choices = METRICS, inline = TRUE),
      accordion(
        open = FALSE,
        accordion_panel(
          "Compare settings",
          helpText("Used only by the Compare page, which sets several methods side by side."),
          checkboxGroupInput("methods", "Methods to compare", choices = NULL),
          radioButtons("rule", "Optimal fit rule", choices = c(
            "Highest quality that is still stable" = "stable",
            "Highest in-sample quality" = "max_r2")),
          conditionalPanel("input.rule == 'stable'",
            sliderInput("threshold", "Minimum stability", min = 0.5, max = 1, value = 0.9, step = 0.01)),
          helpText("Quality rises with rank, so the max-quality rule picks the largest rank.",
                   "The stable rule picks the largest rank whose factors still reproduce across seeds.")))),
    nav_panel("Overview", value = "overview", fillable = FALSE, mod_variance_overview_ui("overview")),
    nav_panel("Variance by dataset", fillable = FALSE, mod_variance_ui("variance")),
    nav_panel("Compare",
      uiOutput("coverage"),
      navset_card_tab(
        id = "compare_tab",
        nav_panel("1. Optimal hyperparameter", mod_hyperparam_ui("hyper")),
        nav_panel("2. Best method", mod_best_method_ui("best")),
        nav_panel("3. Across methods", mod_cross_method_ui("xmethod")),
        nav_panel("4. Across datasets", mod_cross_dataset_ui("xdata")))),
    nav_panel("Explore fits", mod_explore_ui("explore")),
    nav_menu("Inspect fit",
      nav_panel("Factor detail", mod_factor_ui("factor")),
      nav_panel("Enrichment", mod_enrichment_ui("enrich")),
      nav_panel("Metadata", mod_metadata_ui("meta")),
      nav_panel("Biplots", mod_biplot_ui("biplot")),
      nav_panel("Pattern drivers", mod_drivers_ui("drivers"))),
    nav_menu("Across datasets",
      nav_panel("Trajectory across time", mod_trajectory_ui("traj")),
      nav_panel("Projection comparison", mod_projection_ui("proj"))))
}

app_server <- function(con) function(input, output, session) {
  present <- reactive(methods_for_dataset(con, input$dataset))

  observeEvent(present(), {
    p <- present()
    updateCheckboxGroupInput(session, "methods", choices = setNames(p, method_label(p)), selected = p, inline = TRUE)
  })

  curves <- reactive({
    req(input$dataset)
    dataset_curves(con, input$dataset, present(), input$metric)
  }) |> bindCache(input$dataset, input$metric)

  optimal <- reactive({
    cv <- curves()
    cv <- cv[cv$method %in% input$methods, , drop = FALSE]
    optimal_fits(cv, input$rule, input$threshold)
  })

  state <- list(dataset = reactive(input$dataset), metric = reactive(input$metric),
                methods = reactive(input$methods), curves = curves, optimal = optimal)

  # The fit every page looks at: the sidebar's method at a rank (default: that
  # method's optimal fit). Pages that need a second fit or dataset have their
  # own selector for just that.
  observe({
    p <- present(); req(length(p) > 0)
    cur <- isolate(input$fit_method)
    updateSelectInput(session, "fit_method", choices = setNames(p, method_label(p)),
                      selected = if (!is.null(cur) && cur %in% p) cur else p[1])
  })
  observeEvent(list(input$fit_method, curves(), optimal()), {
    req(input$fit_method)
    cv <- curves(); cv <- cv[cv$method == input$fit_method, , drop = FALSE]
    opt <- optimal(); o <- opt$rank_key[opt$method == input$fit_method]
    updateSelectInput(session, "fit_rank", label = if (input$fit_method == "wgcna") "Power" else "Rank",
                      choices = setNames(cv$rank_key, cv$rank_label),
                      selected = if (length(o) == 1) o else cv$rank_key[1])
  })
  focus <- reactive({
    req(input$fit_method, input$fit_rank)
    cv <- curves()
    r <- cv[cv$method == input$fit_method & cv$rank_key == as.numeric(input$fit_rank), , drop = FALSE]
    req(nrow(r) == 1)
    list(method = r$method, fit_id = r$fit_id, n_factors = r$n_factors, rank_label = r$rank_label, rank_key = r$rank_key)
  })

  output$coverage <- renderUI({
    cov <- fit_coverage(con, input$dataset)
    miss <- setdiff(METHOD_ORDER, cov$method)
    if (length(miss) == 0) return(NULL)
    note_missing(paste("fits for", paste(method_label(miss), collapse = ", ")), paste("dataset", input$dataset))
    p(class = "text-muted small", "No fits for this dataset: ", paste(method_label(miss), collapse = ", "))
  })

  mod_hyperparam_server("hyper", state)
  mod_best_method_server("best", state, con)
  mod_cross_method_server("xmethod", state, con)
  mod_cross_dataset_server("xdata", state, focus, con)
  mod_variance_overview_server("overview", con)
  mod_variance_server("variance", state, con)
  # the overview covers every dataset, so the sidebar is only offered on the other pages
  observeEvent(input$page, bslib::toggle_sidebar("main_sidebar", open = input$page != "overview"))
  mod_explore_server("explore", state, focus, con)
  mod_factor_server("factor", state, focus, con)
  mod_enrichment_server("enrich", state, focus, con)
  mod_metadata_server("meta", state, focus, con)
  mod_biplot_server("biplot", state, focus, con)
  mod_drivers_server("drivers", state, focus, con)
  mod_trajectory_server("traj", state, focus, con)
  mod_projection_server("proj", state, focus, con)
}
