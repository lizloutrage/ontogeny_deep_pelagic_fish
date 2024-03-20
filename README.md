
# :fish: Ontogeny of deep-pelagic fishes in the Bay of Biscay Northeast Atlantic

<!-- badges: start -->

[![License:
GPL-2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
<!-- badges: end -->

The goal of the R project `ontogeny_deep_pelagic_fish` is to reproduce
the analyses carried out in the paper by Loutrage et al., 2024 (DOI :
10.1002/ece3.11129), *“Ontogenetic shift or not? Different foraging
trade-offs within the meso- to bathypelagic fish community”*

## Data:

All raw data are available in the online directory data.InDoRES platform
<https://data.indores.fr/> (any use must include the reference and DOI):

- DOI of the trawling data set: <https://doi.org/10.48579/PRO/O5QHTE>
- DOI of the isotopic data set: <https://doi.org/10.48579/PRO/QE2VWQ>

## Overview

Here is an overview of `ontogeny_deep_pelagic_fish` content:

- [`data/`](https://github.com/lizloutrage/ontogeny_deep_pelagic_fish/tree/main/data):
  contains all raw data required to perform analyses

- [`index.qmd`](https://github.com/lizloutrage/ontogeny_deep_pelagic_fish/blob/main/index.qmd):
  contains the final report to be knitted with the code to carry out the
  analysis

- [`figures/`](https://github.com/lizloutrage/ontogeny_deep_pelagic_fish/tree/main/figures):
  contains the figures in high resolution

## Workflow

The script is divided into 3 main parts:

- Linear relationships between individual size and depth (at community
  and specific levels)

- Linear relationships between $\delta$<sup>15</sup>N values and
  individual size (at community and specific level)

- Partition analysis of the variance in $\delta$<sup>15</sup>N values
  explained by size and depth and by these two variables combined for
  each species

## Code of Conduct

Please note that the `ontogeny_deep_pelagic_fish` project is released
with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
