## 📚 Analysis & Figure Scripts

Welcome! This repository contains R scripts used to generate figures (main figures and supplementary figures) for the manuscript. The scripts live in the each folder.

Our research reveals that SCI triggers a cascade of systemic dysfunctions, impacting organs far beyond the initial lesion. To capture these complex, temporal changes, we conducted a systematic investigation, leading to the creation of this open-access resource designed to accelerate future mechanistic and translational research in SCI.

## 🗂️ Repository layout

- All plotting and analysis R scripts, organized by target figure:
  - `Figure1/` — Scripts to produce Figure 1 panels
  - `Figure2/` — Scripts to produce Figure 2 panels
  - `Supplementary Figure 2/` — Supplementary Figure 2 scripts
  - `Supplementary Figure 3/` — Supplementary Figure 3 scripts

Each script is a standalone R script (e.g. `A.R`, `B.R`, `C.R`).

## ✨ What the scripts do

Typical workflow inside a script:
- Read input (e.g. `read.csv()`, `data.table::fread()`, `readxl::read_excel()`)
- Data wrangling with `tidyverse` (dplyr, tidyr, stringr)
- Statistical summaries or filtering (e.g. differential gene counts, enrichment)
- Visualization with `ggplot2`, `pheatmap`, and helpers like `cowplot`/`gridExtra`
- Save plots with `ggsave()` into subfolders (paths are defined inside each script)

## 🧩 Key R package dependencies

Scripts reference the following packages (install all that you need):

- tidyverse (ggplot2, dplyr, tidyr, readr, tibble, stringr)
- cowplot, gridExtra
- pheatmap
- data.table
- ggpubr
- RColorBrewer
- reshape2
- vegan
- ggprism
- gghalves
- circlize
- export
- xlsx, readxl
- htmlwidgets, webshot
- clusterProfiler (Bioconductor)
- GSVA (Bioconductor)
- GSEABase (Bioconductor)

Note: `clusterProfiler`, `GSVA`, and `GSEABase` are Bioconductor packages. 

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

## ▶️ How to run a single script 

From the repository root you can run an individual script with `Rscript`:

```powershell
# Example: run the Figure1 B script
Rscript ./Figure1/B.R

# Example: run Figure2 A
Rscript ./Figure2/A.R
```

Alternatively open the script in RStudio and run interactively (recommended for debugging and inspecting intermediate objects).

## 🔍 Important notes & troubleshooting

- Paths: Scripts assume relative paths (e.g. `data/...` and `output/...`). Ensure your working directory is the repository root or update paths accordingly.
- R version: R >= 4.0 is recommended to avoid package compatibility issues.
- Missing packages: Install all required packages before running scripts. Bioconductor packages use `BiocManager`.
- Private packages: If a script uses a non-public package (e.g. `gground`), you must obtain or mock it.
- Large data: Some scripts may load big tables — ensure sufficient memory and disk space.

## ✉️ Contact & contribution

If you want to contribute or need help reproducing a figure, please open an issue or contact the authors (202421075@mail.sdu.edu.cn).

## 📄 License

This repository is licensed under the Creative Commons Attribution 4.0 International (CC BY 4.0).

You can find the full license text in the `LICENSE` file at the repository root.

Copyright (c) 2025 [Feng Lab] 



