# CLAUDE.md

Guidance for working in this package (the repository-wide rules are in the root `CLAUDE.md`). It is the computation half of a three-package split:
`sepsisfactors` (this one: fits and every analysis derived from the fits) writes a versioned results
output; `sepsisfactorsdb` loads it into a SQLite database; `sepsisfactorsapp` reads
that. No package imports another; the contracts are the results output (specified in
`vignette("results-output", package = "sepsisfactorsdb")`) and the database schema. Vignettes here
cover datasets, methods and the workflow; read the relevant one rather than relying on memory, and
keep it current when behavior changes.

## Working style

- **Ask before hacking anything.** Prefer the idioms of the targets, crew and crew.cluster
  vignettes; if something needs a workaround, say so and ask first.
- **No backward compatibility.** Delete legacy code rather than keep it runnable; correctness is
  shown by parity checks, not by preserving old paths.
- **Don't commit unless asked.**
- **Explain unfamiliar methods descriptively** (what the method does, what the choice changes), and
  give a recommendation rather than a survey of options.
- **Never report work as complete while its jobs are still running.** Give status, not a summary.
- **Report outcomes faithfully**, including when an earlier statement turned out wrong.
- **Docs point to code, never the reverse.** No doc paths in code strings or comments.
- Package conventions: dependencies only through usethis (`use_package()`, `use_dev_package()`);
  base pipe; roxygen2 markdown; testthat 3e; `cli` for messages; no `library()`/`source()`/`setwd`
  in package code; styler and lintr (via `prek`) after writing code.

## Method outputs are developer-level

The method set is closed (pca, spca, ica, nmf, cogaps, wgcna). Each method's output is specified and
versioned (`method_output_version` in its `<name>_ingest` spec, checked by `sepsisfactorsdb`).
Adding a method, or changing what one writes, means changing both packages and bumping a version.
Datasets are user-level: `use_dataset()`.

Behaviour that used to branch on method names is declared in each method's spec (`method_spec()`):
representative and optimal fits, pair kind, redundancy, markers, ICASSO, projection kind, factor
index. Do not add `method == "x"` branches to the workflow.

## Cluster runs

Automated (CI) runs cannot run anything on the cluster; say what would need to run and stop. Site
details (how R is put on the path on the user's cluster) live in the git-ignored `CLAUDE.local.md`.

- Put R on the compute nodes' path with `cluster.setup_lines` in `pipeline.yml`; no machine paths or
  module names in package code.
- **Long runs go through `sbatch run_pipeline.sbatch`**, never a background tool command (those stop
  at 2 hours). The main `tar_make()` process must outlive the run: workers connect back to it.
- Scripts that compute nodes read must live on shared storage, not `/tmp`.
- Run a subset by name (`sbatch run_pipeline.sbatch '<tidyselect>'`); check
  `tar_outdated(names = ...)` with the same selection first.
- **Log every job id** you submit (`logs/jobs/main_jobs.txt`, shared storage and git-ignored), with a
  note of what it is. Check usage with `seff <jobid>`; the `vp` controller's tasks record their own
  cgroup peak memory and CPU seconds in `$resources` (`job_resources()`), which `seff` misses.
- **Targets:** about 75% average memory and CPU use, never below 50%, erring high; size memory for
  the largest dataset (SHIP-TREND, 8,000 genes x 991 samples). **No worker caps just to limit
  submissions**; a `workers:` cap needs another reason, stated in a comment.
- Out-of-memory crashes are handled by each controller's `backup:`.

## Package code and targets

- The workflow's functions are package functions, and targets does not track an installed package's
  code: upgrading the package does not rerun anything. After a change to a computation,
  `tar_invalidate()` what should be recomputed (or opt in with `imports = "sepsisfactors"`).
- Functions that target commands call must be exported (`@keywords internal @export`), and every
  target that sets `packages` must go through `target_packages()`, which adds this package.
- Test the workflow in a separate process with the package *installed* (see
  `tests/testthat/helper-synthetic.R`): `devtools::load_all()` exports every internal object and hides
  commands that reference unexported ones.
- **WGCNA must be attached** and this package must not import `stats::cor` or `stats::hclust`:
  `blockwiseModules()` looks `cor()` up from the calling frame (see `run_wgcna_param_job()`).
- **File targets drop names** (`format = "file"` returns bare paths); recover ids from the path.
- **targets can't branch over an empty table**: job tables return `placeholder_jobs()` and branches
  check `has_job()`. `c(list(a, b))` doesn't flatten a list of patterns' branch lists; use
  `unlist(x, recursive = FALSE)`.
- Each branch costs the main process about a second, so batch tiny tasks (`batch_size`).
- Load a package before reading its S4 objects (`dim()` on a dream `MArrayLM2` read first returns
  `NULL` for the session): targets that read them set `packages = target_packages("variancePartition")`.
- Don't store once-per-session messages in a target value.
- Dry-run changes to the results writer on one small dataset, and load the result with
  `sepsisfactorsdb::ingest_results()` (the end-to-end test does this on synthetic data).

## Scientific decisions to keep

- **sPCA:** K 2-12; a scan at `K_scan` 5 over para 0.2-0.5, then every K at the scan's
  Index-of-Sparseness winner +/- 0.05 (Guerra-Urzola et al. 2021); `lambda` a single value (1e-6);
  `max_hours` 8 (then `killed_not_converged`).
- **WGCNA:** one block over all genes (`maxBlockSize = 50000`); power swept, not auto-picked; the
  per-gene connectivity and scale-free fit are stored per fit, the TOM is not.
- **PCA:** one fit per dataset at the maximum rank; first-n slicing happens downstream.
- **ICA:** seed sweep plus bootstrap resamples of subjects (all of a drawn person's samples), for
  ICASSO. Components are not nested across ranks.
- **Repeated measures:** time is 1-4% of variance in the top 8,000 genes, between-person 35-60%;
  fit the full datasets and all per-timepoint subsets as they are; resample by subject; treat
  within-family agreement as temporal reproducibility, not replication; describe every factor with a
  score-level variance partition. No within-person-centred fits.
- **Variance models:** variancePartition with ML, dream with REML (`ddf = "adaptive"`); categorical
  variables random, numeric covariates fixed and standardized; arrays use the deposited log values
  (no variance filter); RNA-seq filterByExpr + TMM + voomWithDreamWeights. No between-array
  normalization by default. ANEMONES' spike-in controls enter as a covariate (their first PC).
- **Gene ids across datasets:** Ensembl, through a one-to-one map built from each dataset's matrix.
- **Data quality is flagged downstream, not fixed in the data:** GSE54514's annotations don't match
  its arrays but it stays in; sex-discordant samples stay.
- **Stability storage:** matched pairs plus context, not every factor x factor cell.
- **Keep everything computed** in the results output, even what no reader uses yet.

## Open items

See `TODO.md` (git-ignored): the NMF/CoGAPS projection asymmetry question, `gains`, controller tuning
and the from-scratch rerun on the cluster, and the phase 2 checks.
