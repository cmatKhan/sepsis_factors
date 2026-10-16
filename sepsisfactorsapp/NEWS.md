# sepsisfactorsapp 0.0.0.9000

* Ported the Shiny app from the original repository at schema version 3.
  `run_app(db_path, config_dir)` replaces `app/app.R`; the database path is
  always given by the user (argument or `STABILITY_DB`), and the app errors on
  any other schema version.
* The config files the app read from the project's `config/` directory
  (`dataset_families.yml`, `composition_markers.yml`, `*_config.yml`) are read
  from `config_dir` instead, and are optional.
* Copied from `sepsisfactors` for now: the factor-matching helpers
  (`similarity.R`).
