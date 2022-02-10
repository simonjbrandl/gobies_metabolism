-   [Metabolic rates mirror morphological and behavioral differences in two sand-dwelling coral reef gobies](#metabolic-differentiation-facilitates-coexistence-in-two-coral-reef-fish-species)
    -   [Instructions](#instructions)
    -   [Details](#details)
    -   [Datasets](#datasets)
    -   [Supplemental info](#supplemental-info)

# Metabolic rates mirror morphological and behavioral differences in two sand-dwelling coral reef gobies

This repository contains the code and data to reproduce all tables and
figures presented in Brandl et al. “Metabolic rates mirror morphological
and behavioral differences in two sand-dwelling coral reef gobies”

## Instructions

All analyses for the current project were done in R, using the drake
pipeline [drake](https://github.com/ropensci/drake). You can use drake
to compile all models, figures, and tables. To do so, first install
`drake` from CRAN:

``` r
install.packages("drake")
```

Next you need to open an R session with working directory set to the
root of the project.

We use a number of packages, listed in `r/packages.R`. If needed,
missing packages should first be installed.

Then, to generate all figures, analyses, and tables, simply run:

``` r
drake::r_make()
```

All output will be automatically rendered inside the folder called
output.

## Details

The elements of this project include:

1.  data: this folder includes all the raw data necessary to run the
    script.
2.  output: this folder includesall outputs from the R-script.
3.  documents: this folder includes the manuscript, appendices, and
    cover letter.
4.  r: this folder contains three .R files, `packages.R`, `functions.R`,
    and `plan.R`.  
    `packages.R` contains all packages needed, `functions.R` contains
    all functions to reproduce the analyses, and `plan.R` provides the
    code that binds each step of the workflow.

## Datasets

1.  commdat = Community data: data were collected using enclosed
    clove-oil stations in both the lagoon and on the forereef around the
    island of Mo’orea, French Polynesia. Contains each caught individual
    of cryptobenthic fish (as rows), including the two target species.
    Columns specify each individuals corresponding sampling information
    (location, site, outcrop, lat & long, curved surface length, depth),
    identity, size, etc. All collections done in 2017, 2018, 2019,
    and 2020.

2.  respodat = Respirometry: these data are derived from intermittent
    respirometry, performed in the lab at CRIOBE. Each row is an
    individual fish and its associated metadata. SMR = standard
    metabolic rate in mg O2 per h. MaxMR = maximum metabolic rate in mg
    O2 per h. MeanTemp…C. = temperature in ºCelsius. Weight..kg. = mass
    in g.

3.  morpho = External morphology; gut.morpho = internal morphology: data
    derived from measurements performed on individuals collected from
    the field. Two different datasets for external and internal
    morphology. Each row is a unique individual with associated metadata
    and measurements for SL (mm), TL (mm), weight (g), girth (mm),
    vertical gape size (V_gape, mm), and horizontal gape size (H_gape,
    mm). Gut morpho is a smaller set but same setup, with the length of
    the gastrointestinal tract (GIT) in mm.

4.  DNA gut content metabarcoding, three distinct datasets:

    1.  gobies.meta = Metadata:  
        contains information on all sampled and processed individuals,
        with each individual havign a unique ID and its own row. IDs are
        critical to match with the two other datasets.
    2.  gobies.coi = COI primer data:  
        each row is an exact sequence variant (ESV) that was assigned to
        a prey taxon. Since this was part of a greater collection
        effort, there are many rows that are 0 across all columns.
        Columns 1:10 contain infromation about the sequence, while the
        remaining columns (G01 - G40) are individual fishes linked to
        the Extraction ID in the metadata. Values in the dataset are
        sequence abundances.
    3.  goby.23s = 23S primer data:  
        Similar as gobie.coi

5.  aquarium.trials = Feeding trials: data are observations from videos
    of sand gobies in varying configurations in aquaria. id = identity
    of the observed individual; species = species affiliation (F.
    neophytus or G. cauerensis); video = video ID; treatment =
    configuration tested; feed = number of bites taken over the duration
    of the observations (30 minutes); before_after = before or after
    generic food addition – only before data are used for this paper.
    All videos, labelled by their respective trial, are accessible under
    the following link:
    <https://www.youtube.com/playlist?list=PL9LfYSgK5itOtyll0jJr7Kszdf6olw6Ce>

## Supplemental info

The paper contains supplemental material.

-   Text S1: Details pertaining to the sequencing protocol and
    bioinformatic pipeline used to compile sequence data used in the
    paper.

-   Text S2: Short description of analytical methods and model summary
    outputs as obtained from the drake project, but in a separate word
    document.

-   Supplemental figures 1 and 2.

This paper was produced using the following software and associated
packages:

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value
    ##  version  R version 4.0.3 (2020-10-10)
    ##  os       macOS Big Sur 10.16
    ##  system   x86_64, darwin17.0
    ##  ui       X11
    ##  language (EN)
    ##  collate  en_US.UTF-8
    ##  ctype    en_US.UTF-8
    ##  tz       America/Chicago
    ##  date     2022-02-10
    ##  pandoc   2.11.2 @ /Applications/RStudio.app/Contents/MacOS/pandoc/ (via rmarkdown)
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package     * version date (UTC) lib source
    ##  brio          1.1.1   2021-01-20 [1] CRAN (R 4.0.2)
    ##  cachem        1.0.1   2021-01-21 [1] CRAN (R 4.0.2)
    ##  callr         3.7.0   2021-04-20 [1] CRAN (R 4.0.2)
    ##  cli           3.1.0   2021-10-27 [1] CRAN (R 4.0.2)
    ##  crayon        1.3.4   2017-09-16 [1] CRAN (R 4.0.2)
    ##  desc          1.4.0   2021-09-28 [1] CRAN (R 4.0.2)
    ##  devtools      2.4.3   2021-11-30 [1] CRAN (R 4.0.2)
    ##  digest        0.6.27  2020-10-24 [1] CRAN (R 4.0.2)
    ##  ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.0.2)
    ##  evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.1)
    ##  fastmap       1.1.0   2021-01-25 [1] CRAN (R 4.0.3)
    ##  fs            1.5.0   2020-07-31 [1] CRAN (R 4.0.2)
    ##  glue          1.4.2   2020-08-27 [1] CRAN (R 4.0.2)
    ##  htmltools     0.5.1.1 2021-01-22 [1] CRAN (R 4.0.2)
    ##  knitr         1.37    2021-12-16 [1] CRAN (R 4.0.3)
    ##  lifecycle     1.0.1   2021-09-24 [1] CRAN (R 4.0.2)
    ##  magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.0.2)
    ##  memoise       2.0.0   2021-01-26 [1] CRAN (R 4.0.3)
    ##  pkgbuild      1.2.0   2020-12-15 [1] CRAN (R 4.0.2)
    ##  pkgload       1.2.4   2021-11-30 [1] CRAN (R 4.0.2)
    ##  prettyunits   1.1.1   2020-01-24 [1] CRAN (R 4.0.2)
    ##  processx      3.5.2   2021-04-30 [1] CRAN (R 4.0.2)
    ##  ps            1.5.0   2020-12-05 [1] CRAN (R 4.0.2)
    ##  purrr         0.3.4   2020-04-17 [1] CRAN (R 4.0.2)
    ##  R6            2.5.0   2020-10-28 [1] CRAN (R 4.0.2)
    ##  remotes       2.4.2   2021-11-30 [1] CRAN (R 4.0.2)
    ##  rlang         0.4.12  2021-10-18 [1] CRAN (R 4.0.2)
    ##  rmarkdown     2.6     2020-12-14 [1] CRAN (R 4.0.2)
    ##  rprojroot     2.0.2   2020-11-15 [1] CRAN (R 4.0.2)
    ##  rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.0.2)
    ##  sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.0.2)
    ##  stringi       1.5.3   2020-09-09 [1] CRAN (R 4.0.2)
    ##  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.0.2)
    ##  testthat      3.1.2   2022-01-20 [1] CRAN (R 4.0.5)
    ##  usethis       2.1.5   2021-12-09 [1] CRAN (R 4.0.2)
    ##  withr         2.4.3   2021-11-30 [1] CRAN (R 4.0.2)
    ##  xfun          0.29    2021-12-14 [1] CRAN (R 4.0.2)
    ##  yaml          2.2.1   2020-02-01 [1] CRAN (R 4.0.2)
    ## 
    ##  [1] /Library/Frameworks/R.framework/Versions/4.0/Resources/library
    ## 
    ## ──────────────────────────────────────────────────────────────────────────────

All code written by Simon J. Brandl (<simonjbrandl@gmail.com> and
<https://github.com/simonjbrandl>). Please contact me for any issue or
question.
