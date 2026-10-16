# sepsisfactors

Latent-factor analysis of sepsis transcriptomes as a [targets](https://docs.ropensci.org/targets/)
workflow. It fits six methods (PCA, sparse PCA, ICA, NMF, CoGAPS and WGCNA) on every dataset, and
computes everything derived from the fits: stability across seeds and ranks, enrichment, repeated-
measures variance partitioning (variancePartition and dream), and projectR projections between
datasets. It writes a versioned *results output* (Parquet tables and `.rds` artifacts) that the
`sepsisfactorsdb` package loads into a database.

```
sepsisfactors --[results output]--> sepsisfactorsdb --[SQLite DB + artifacts]--> sepsisfactorsapp
```

The three packages do not import each other; the results output is the contract between the first
two (specified in `vignette("results-output", package = "sepsisfactorsdb")`).

## Use

```r
library(sepsisfactors)

# 1. get the raw data (HuggingFace collection; ROSE comes from dbGaP)
download_collection("/path/to/data_root")

# 2. set up a project, then edit pipeline.yml (datasets, data root, cluster settings)
use_pipeline("my_project")

# 3. in my_project/: build everything outdated
targets::tar_make()            # or: sbatch run_pipeline.sbatch
```

The datasets of the project are built into the package (`discover_datasets()`); add your own with
`use_dataset("MY_DATASET")`, which writes `datasets/MY_DATASET/config.yml` and `preprocess.R` to edit.
Methods are a closed set (`method_manifest()`): adding one is a developer task, because each method's
output is strictly specified and versioned.

## Installation

Needs R >= 4.4, Bioconductor packages (variancePartition, limma, edgeR, BiocParallel, fgsea), the
projectR fork `cmatKhan/projectR@feature-add_center_by_loadings`, and the method packages (elasticnet,
fastICA, NNLM, CoGAPS, WGCNA). Install from a local checkout with `devtools::install()`.
