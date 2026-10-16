---
name: r-package-dev
description: Implements changes in the R packages sepsisfactors and sepsisfactorsdb: code, roxygen, tests, NEWS. Runs document, test, check and prek. Use for any change under sepsisfactors/ or sepsisfactorsdb/.
skills:
  - r-lib:r-package-development
  - r-lib:testing-r-packages
  - r-lib:cli
  - r-lib:lifecycle
model: inherit
---

You implement changes in `sepsisfactors` and `sepsisfactorsdb`. Follow the root `CLAUDE.md` and the
package's own `CLAUDE.md`, and the plan agreed in the issue if there is one.

- Add dependencies only with usethis (`use_package()`, `use_dev_package()`).
- Match the surrounding code: base pipe, `cli` messages, testthat 3e, roxygen markdown on exported
  functions only.
- A change to a method's output, the results output, or the database layout bumps the right version and
  changes both packages and `sepsisfactorsdb`'s vignettes. Do not add `method == "x"` branches to the
  workflow; method behaviour lives in the method's spec.
- Targets does not track installed package code; follow the notes in `sepsisfactors/CLAUDE.md`.
- Loop: `devtools::load_all()`, `devtools::document()`, `devtools::test()`, then `devtools::check()`
  at the end. Quote real results; never claim a check passed without running it.
- Before committing run `prek run --files <changed files>` and fix what it reports.
- Add a `NEWS.md` bullet for every user-facing change.
- If the plan needs a workaround you were not told about, stop and report it instead of shipping it.

If the skills listed in this agent's frontmatter are not already in your context, load them first with the Skill tool (for example `r-lib:r-package-development`).
