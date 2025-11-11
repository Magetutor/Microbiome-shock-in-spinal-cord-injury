## 📚 iMeta — Analysis & Figure Scripts

Welcome! This repository contains R scripts used to generate figures (main figures and supplementary figures) for the iMeta manuscript. The scripts live in the `scripts/` folder and read input files in `data/` and write figure outputs to `output/`.

## 🗂️ Repository layout

- `scripts/` — All plotting and analysis R scripts, organized by target figure:
  - `scripts/Figure1/` — Scripts to produce Figure 1 panels
  - `scripts/Figure2/` — Scripts to produce Figure 2 panels
  - `scripts/Supplementary Figure 2/` — Supplementary Figure 2 scripts
  - `scripts/Supplementary Figure 3/` — Supplementary Figure 3 scripts

Each script is a standalone R script (e.g. `A.R`, `B.R`, `C.R`), typically reading from `data/` and saving results to `output/` (as PDFs or images).

## ✨ What the scripts do

Typical workflow inside a script:
- Read input (e.g. `read.csv()`, `data.table::fread()`, `readxl::read_excel()`)
- Data wrangling with `tidyverse` (dplyr, tidyr, stringr)
- Statistical summaries or filtering (e.g. differential gene counts, enrichment)
- Visualization with `ggplot2`, `pheatmap`, and helpers like `cowplot`/`gridExtra`
- Save plots with `ggsave()` into `output/` subfolders (paths are defined inside each script)

## 🧩 Key R package dependencies

Scripts reference the following packages (install all that you need):

- tidyverse (ggplot2, dplyr, tidyr, readr, tibble, stringr)
- ggplot2
- cowplot, gridExtra
- pheatmap
- data.table
- ggpubr
- RColorBrewer
- reshape2
- vegan
- ggprism
- gground (custom or external — confirm availability)
- gghalves
- circlize
- export
- xlsx, readxl
- htmlwidgets, webshot
- clusterProfiler (Bioconductor)
- GSVA (Bioconductor)
- GSEABase (Bioconductor)

Note: `clusterProfiler`, `GSVA`, and `GSEABase` are Bioconductor packages. Some packages (e.g., `gground`) may be private/custom — confirm source or replace if unavailable.

## 🛠️ Install suggested packages (R)

Run the following in R (or RStudio) to install the common dependencies. This is a starting point; adjust to your needs.

```r
# CRAN packages
install.packages(c(
  "tidyverse","ggplot2","cowplot","gridExtra","pheatmap",
  "data.table","ggpubr","RColorBrewer","reshape2","vegan",
  "ggprism","gghalves","circlize","export","xlsx","readxl",
  "htmlwidgets","webshot"
))

# Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("clusterProfiler","GSVA","GSEABase"))

# phantomjs for webshot (if webshot is used to export widgets)
webshot::install_phantomjs()
```

If a package is not on CRAN (e.g. `gground`), contact the repository owner or replace the functions with available equivalents.

## ▶️ How to run a single script (PowerShell)

From the repository root you can run an individual script with `Rscript`:

```powershell
# Example: run the Figure1 B script
Rscript .\scripts\Figure1\B.R

# Example: run Figure2 A
Rscript .\scripts\Figure2\A.R
```

Alternatively open the script in RStudio and run interactively (recommended for debugging and inspecting intermediate objects).

## 🔍 Important notes & troubleshooting

- Paths: Scripts assume relative paths (e.g. `data/...` and `output/...`). Ensure your working directory is the repository root or update paths accordingly.
- R version: R >= 4.0 is recommended to avoid package compatibility issues.
- Missing packages: Install all required packages before running scripts. Bioconductor packages use `BiocManager`.
- Private packages: If a script uses a non-public package (e.g. `gground`), you must obtain or mock it.
- Large data: Some scripts may load big tables — ensure sufficient memory and disk space.

## ✅ Suggested next steps (optional enhancements)

- Add a `scripts/requirements.R` installer that runs the above install commands automatically.
- Add per-script `README` subsections listing exact input files required and example outputs.
- Add a small `Makefile` or `run_all.R` orchestration script to produce all figures in the correct order.

## ✉️ Contact & contribution

If you want to contribute or need help reproducing a figure, please open an issue or contact the authors (202421075@mail.sdu.edu.cn).

## 📄 License

This repository is licensed under the Creative Commons Attribution 4.0 International (CC BY 4.0).

You can find the full license text in the `LICENSE` file at the repository root.

Copyright (c) 2025 [Feng Lab] 

