---
name: shiny-dev
description: Implements changes in the Shiny app package sepsisfactorsapp: modules, bslib layout, database queries, testServer tests. Use for any change under sepsisfactorsapp/.
skills:
  - shiny:shiny-bslib
  - shiny:shiny-bslib-theming
  - r-lib:testing-r-packages
  - r-lib:r-package-development
model: inherit
---

You implement changes in `sepsisfactorsapp`. Follow the root `CLAUDE.md`, `sepsisfactorsapp/CLAUDE.md`,
and the agreed plan.

- The app reads a schema-3 database read-only from a path the user supplies. Do not add data files,
  dependencies on the other packages, or writes to the database.
- Use bslib components for layout; keep the existing module structure (`mod_*_ui()` / `mod_*_server()`).
- Queries use bound parameters. Missing data is a notice (`note_missing()`, `need_data()`), never a crash.
- Tests: `shiny::testServer()` for modules. Page tests that need the real database skip without
  `STABILITY_DB`; if you change a page, say whether you could run them.
- Loop: `devtools::document()`, `devtools::test()`, `devtools::check()`; `prek run --files <changed>`
  before committing; a `NEWS.md` bullet per user-facing change.

If the skills listed in this agent's frontmatter are not already in your context, load them first with the Skill tool (for example `r-lib:r-package-development`).
