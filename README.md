# sepsis_factors

Latent-factor analysis of sepsis transcriptomic time courses, as three R packages. No package imports
another; the only coupling is two versioned contracts.

```
sepsisfactors --results output--> sepsisfactorsdb --SQLite database (schema 3)--> sepsisfactorsapp
   compute                            load                                            explore
```

## Which package do I want?

| I want to... | Go to |
|---|---|
| Run the analysis: fit PCA, sPCA, ICA, NMF, CoGAPS and WGCNA on the sepsis datasets, and compute stability, enrichment, variance and projections | [`sepsisfactors`](sepsisfactors/) |
| Add a dataset (a config and a preprocessing script) | [`sepsisfactors`](sepsisfactors/), `use_dataset()` |
| Request a new method | open an issue; methods are a closed set, implemented in `sepsisfactors` and `sepsisfactorsdb` |
| Load results into the SQLite database | [`sepsisfactorsdb`](sepsisfactorsdb/) |
| Understand the results output or the database layout | [`sepsisfactorsdb`](sepsisfactorsdb/) vignettes `results-output` and `database` |
| Explore results in the browser | [`sepsisfactorsapp`](sepsisfactorsapp/) |

## Install

```r
pak::pak("cmatKhan/sepsis_factors/sepsisfactors")
pak::pak("cmatKhan/sepsis_factors/sepsisfactorsdb")
pak::pak("cmatKhan/sepsis_factors/sepsisfactorsapp")
```

`sepsisfactors` needs Bioconductor packages and a fork of projectR; see its README. Documentation sites:
<https://cmatkhan.github.io/sepsis_factors/>.

## Setting up claude automation via github

Issues and pull requests in this repository are worked with Claude agents (`.claude/agents/`,
`.github/workflows/`). One-time setup by a repository admin:

```sh
# 1. Install the Claude GitHub app on the repository: https://github.com/apps/claude
# 2. Give the workflows an Anthropic API key (the GitHub workflows use the API, not a subscription).
gh secret set ANTHROPIC_API_KEY --repo cmatKhan/sepsis_factors
# 3. Create the labels the workflows react to.
gh label create plan     --color 0E8A16 --description "Claude replies with a plan, no code"
gh label create auto-fix --color D93F0B --description "Claude implements the issue and opens a PR"
gh label create release  --color 5319E7 --description "Merging this PR cuts a release (version bump required)"
gh label create docs     --color 1D76DB --description "Merging this PR publishes the documentation sites"
```

Then, in the repository settings:

- **Actions > General > Workflow permissions:** allow GitHub Actions to create and approve pull requests.
- **Pages:** deploy from the `gh-pages` branch (it is created by the first documentation run).

Locally, Claude Code uses your own login (subscription). Open the repository with `claude`; it offers to
install the Posit plugins (`r-lib`, `shiny`, `posit-dev`, `quarto`) named in `.claude/settings.json`.

### What each label or comment does

| Trigger | What happens |
|---|---|
| Issue labelled **`plan`** | The `planner` agent posts one comment: understanding, per-package changes, contract impact, tests, risks and open questions. Nothing is edited. |
| `@claude ...` comment on that issue | The plan is revised in the thread (comment only). Repeat until you are happy with it. |
| Issue labelled **`auto-fix`** | The agents implement the issue (following the agreed plan if there is one; without one they post a short plan in the pull request first), run the tests and checks, review their own diff against the packages' documentation, and open a pull request `claude/issue-<n>` that closes the issue. |
| Review comments on that pull request that start with `@claude` | Claude makes the requested changes on the same branch and replies with what it ran. |
| Pull request labelled **`release`** | Must bump the `Version` of the package(s) it releases and add a `NEWS.md` heading `# <package> <version>` (checked on the pull request). On merge, a GitHub release `<package>-v<version>` is created with that NEWS section. Merge by squash or merge commit. |
| Pull request labelled **`docs`**, merged | The pkgdown sites of the packages the pull request touched are built and published. You can also run the *docs* workflow by hand. It never runs on a plain push. |
| Any pull request | `prek` (styler, lintr) on the changed files, and `R CMD check` with the tests for the packages whose files changed. |

Costs and limits: each Claude run has a maximum number of turns and a job timeout; the API key is billed
per token. Only users with write access can trigger the agents. The agents never push to `main`, never
merge, and cannot run anything on a cluster: work that needs the SLURM runs is described in the issue
or pull request instead.

### Running the checks locally

```sh
prek run --all-files          # styler + lintr + whitespace; needs the R packages styler and lintr
prek install                  # run on every commit
```

The R hooks are local `language: system` hooks (see `.pre-commit-config.yaml`), because `prek` does not
implement `language: r` yet.
