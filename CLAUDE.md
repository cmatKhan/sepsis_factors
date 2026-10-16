# CLAUDE.md

Guidance for working anywhere in this repository. Each package has its own `CLAUDE.md` with the rules
specific to it; read that one too when you work in the package.

## The repository

Three R packages for latent-factor analysis of sepsis transcriptomic time courses. No package imports
another. The only coupling is two versioned contracts:

```
sepsisfactors --results output--> sepsisfactorsdb --SQLite database (schema 4)--> sepsisfactorsapp
```

| Package | Owns | Never does |
|---|---|---|
| `sepsisfactors` | All computation: datasets, preprocessing, fits, stability pairs, enrichment, variance partition, projections, drivers. Writes the results output. | Touch SQLite. |
| `sepsisfactorsdb` | Validates the results output and loads it into the SQLite database and artifact layout. Specifies both contracts in its vignettes. | Compute statistics (TEMPORARY exception: the derive step, see below). |
| `sepsisfactorsapp` | The Shiny app. Reads the database read-only, from a path the user supplies. | Copy data into the package or write to the database. |

- A change to the results output or to the database layout touches **both sides**: bump
  `RESULTS_OUTPUT_VERSION` (the contract), the affected `method_output_version` (one method's output)
  or `SCHEMA_VERSION` (the database), update `sepsisfactorsdb`'s vignettes, and say so in `NEWS.md`.
- The method set is closed (pca, spca, ica, nmf, cogaps, wgcna). Adding a method is a developer task
  across `sepsisfactors` and `sepsisfactorsdb`. Adding a dataset is a user task (`use_dataset()`).
- `sepsisfactorsapp` currently keeps copies of a few helpers from the other packages (listed in the root `TODO.md`,
  section 1). Do not "fix" the copies by adding a package dependency.

> **TEMPORARY exception (stage 1 of schema 4, 2026-10-16).** `sepsisfactorsdb` has a derive step
> (`R/derive*.R`) that computes reduced and precomputed tables from the full artifacts, and a legacy
> adapter (`R/legacy_adapter.R`) that reshapes the original project's output. Both go when `sepsisfactors`
> writes the contract itself; the list of pieces, the packages to remove and the contract catch-up are in
> section 7 of `TODO.md`. Do not add more statistics to `sepsisfactorsdb`.

## Working style

- **Ask before hacking anything.** Prefer the idioms of the packages' own vignettes (targets, crew,
  crew.cluster, shiny, bslib, DBI, ...). If something needs a workaround, say so and ask first. In an
  automated run you cannot ask interactively: state the workaround in the issue or PR and stop for
  discussion rather than shipping it.
- **No backward compatibility.** Delete legacy code rather than keep it runnable; correctness is shown
  by parity checks, not by preserving old paths.
- **Explain unfamiliar methods descriptively** (what the method does, what the choice changes), and give
  a recommendation rather than a survey of options.
- **Report outcomes faithfully**, including failing tests (with their output), skipped steps, and
  earlier statements that turned out wrong. Never report work as complete while jobs are still running.
- **Docs point to code, never the reverse.** No documentation paths in code strings or comments;
  each docs section names the scripts and functions behind it.
- **Commits.** In an interactive session, do not commit unless asked. In an automated run the workflow
  prompt governs: commit only to the `claude/issue-<n>` branch, never to `main`, never merge.

## Package conventions

- Dependencies are added with usethis only (`use_package()`, `use_dev_package()`), never by editing
  `DESCRIPTION` by hand.
- Base pipe `|>`; `\()` only for one-liners; roxygen2 with markdown; document only exported functions
  (internals get plain comments or `@noRd`); `devtools::document()` after any roxygen change.
- testthat 3e; `tests/testthat/test-<name>.R` mirrors `R/<name>.R`; `expect_snapshot()` for errors and
  warnings; `withr` for cleanup.
- Messages through `cli` (`cli_abort()`, `cli_warn()`, `cli_inform()`).
- No `library()`, `source()`, `setwd()` or working-directory-relative paths in package code; files that
  ship with a package are found with `system.file()`.
- Style and lint with styler and lintr, run by `prek` (see `.pre-commit-config.yaml`); run
  `prek run --files <changed files>` before committing.
- Check loop for a package: `devtools::load_all()`, `devtools::document()`, `devtools::test()`,
  `devtools::check()` with no errors or warnings. Do not leave the e2e tests of `sepsisfactors` skipped:
  they need the sibling `sepsisfactorsdb` directory, which exists in this repository.
- Use the R and Posit skills (`r-lib:*`, `shiny:*`) for package development, testing, cli messages and
  lifecycle; the agents in `.claude/agents/` preload them.

## Agents

| Agent | Use it for |
|---|---|
| `planner` | Turn an issue into a plan (read-only). Template: `.github/claude/plan-first.md`. |
| `r-package-dev` | Implement in `sepsisfactors` or `sepsisfactorsdb`. |
| `shiny-dev` | Implement in `sepsisfactorsapp`. |
| `docs-reviewer` | Check changed code against the documentation and recommended patterns of every package it uses (read-only). |
| `contract-reviewer` | Check that contract and schema changes bump versions and update both sides (read-only). |

Before opening a pull request, run `docs-reviewer` on the diff (and `contract-reviewer` when the
results output, a method's output, or the database layout changed). Fix Blocking and Required findings;
list the rest in the pull request description.

## Issues and pull requests

- **Plan first.** An issue labelled `plan` gets a plan comment (no code). Discuss and adjust it in the
  issue thread with `@claude`. Label `auto-fix` to implement (following the agreed plan if there is
  one) and open a pull request.
- A pull request body says what changed per package, what was verified (test and check results, quoted
  honestly), and links the issue with `Closes #<n>`.
- Reviewers request changes in review comments; address them with commits on the same branch.
- A pull request labelled `release` must bump the `Version` of each released package and add a
  `NEWS.md` heading for it. A pull request labelled `docs` publishes the documentation site when
  merged.
- The cluster work (SLURM runs of the pipeline, the from-scratch rerun) cannot happen in an automated
  run. Say what would need to run there and stop.
