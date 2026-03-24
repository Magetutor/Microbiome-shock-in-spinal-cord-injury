## 📚 Analysis & Figure Scripts

Welcome! This repository contains R scripts and R Markdown files used to generate
figures (main figures and supplementary figures) for the manuscript:
"Spinal Cord Injury Triggers Acute Microbiome Shock and System-Wide
Transcriptomic Reprogramming". The scripts live in the each folder described
below.

Our research reveals that SCI triggers a cascade of systemic dysfunctions,
impacting organs far beyond the initial lesion. To capture these complex,
temporal changes, we conducted a systematic investigation, and created an
open-access resource to accelerate mechanistic and translational research.

## 🗂️ Repository layout

- All plotting and analysis R scripts, organized by target figure:
  - `Figure1/` — Scripts to produce Figure 1 panels
  - `Figure2/` — Scripts to produce Figure 2 panels
  - `Supplementary Figure 1/` — Supplementary Figure 1 scripts
  - `Supplementary Figure 2/` — Supplementary Figure 2 scripts
  - `Supplementary Figure 3/` — Supplementary Figure 3 scripts
  - `Supplementary Figure 4/` — Supplementary Figure 4 scripts

Each script is typically a standalone R script or R Markdown (e.g. `A.R`, `B.R`,
`*_combined.Rmd`). Data used by the scripts are stored in the matching
`*_data/` subfolders (typically `.RData` and CSV files).

## ✨ What the scripts do

Typical workflow inside a script:
- Read input (e.g. `read.csv()`, `data.table::fread()`, `readxl::read_excel()`)
- Data wrangling with `tidyverse` (dplyr, tidyr, stringr)
- Statistical summaries or filtering (e.g. differential gene counts, enrichment)
- Visualization with `ggplot2`, `pheatmap`, and helpers like `cowplot`/`gridExtra`
- Save plots with `ggsave()` into subfolders (paths are defined inside each script)

## 🧩 Key R package dependencies

Install the common dependencies (adjust per-script as needed):

```r
install.packages(c(
  "tidyverse","ggplot2","cowplot","gridExtra","pheatmap",
  "data.table","ggpubr","RColorBrewer","reshape2","vegan",
  "ggprism","gghalves","circlize","export","xlsx","readxl",
  "htmlwidgets","webshot"
))

if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("clusterProfiler","GSVA","GSEABase"))
webshot::install_phantomjs()
```

Note: `clusterProfiler`, `GSVA`, and `GSEABase` are Bioconductor packages.

## ▶️ How to run a single script

From the repository root you can run an individual script with `Rscript`:

```powershell
# Example: run the Figure1 B script
Rscript ./Figure1/B.R

# Example: render an R Markdown
Rscript -e "rmarkdown::render('Figure1/Figure1_combined.Rmd')"
```

Or open scripts/Rmds in RStudio and run interactively (recommended for debugging).

## 🔍 Important notes & troubleshooting

- Paths: Scripts assume relative paths (e.g. `data/...` and `output/...`). Ensure
  your working directory is the repository root or update paths accordingly.
- R version: R >= 4.0 is recommended.
- Missing packages: Install required packages before running scripts. Bioconductor
  packages use `BiocManager`.
- Private packages: If a script uses a non-public package, obtain it or replace
  the functionality.
- Large data: Some scripts may load big tables — ensure sufficient memory and
  disk space.

## 🔁 Local additions (this update)

This repository has been updated to include the `Fig/` convenience folder that
contains combined R Markdown files and supporting data used to reproduce the
manuscript figures locally. Changes added here are intended to supplement the
existing repository structure without altering the original layout.

Key additions:
- `Fig/Figure1/Figure1_combined.Rmd` and `Fig/Figure1/Figure1_data/` — Rmd and
  data used for Figure 1 (includes `a多样性指数结果所有.csv`, `F_12h.RData`, etc.).
- `Fig/Figure2/Figure2_combined.Rmd` and `Fig/Figure2/Figure2_data/` — Figure 2
  rendering files and data (`A.RData`, `B.RData`, ...).
- `Fig/Supplementary Figure X/` — combined Rmd and `FigureS*_data/` for
  supplementary figures 1–4.

Notes for these additions:
- The `Fig` files are supplemental convenience materials; to render them, run
  the corresponding `*_combined.Rmd` with `rmarkdown::render()` or open in
  RStudio and Knit.
- If you plan to push large PDFs or binary outputs, consider `git lfs`.

## ✉️ Contact & contribution

If you want to contribute or need help reproducing a figure, please open an
issue or contact the authors: magic.mage0001@gmail.com

## 📄 License

This repository is licensed under the Creative Commons Attribution 4.0
International (CC BY 4.0). See the `LICENSE` file at the repository root.

