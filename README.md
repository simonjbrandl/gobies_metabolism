-   [Metabolic differentiation facilitates coexistence in two coral reef
    fish
    species](#metabolic-differentiation-facilitates-coexistence-in-two-coral-reef-fish-species)
    -   [Instructions](#instructions)
    -   [Details](#details)
    -   [Datasets](#datasets)
    -   [Supplemental info](#supplemental-info)

Metabolic differentiation facilitates coexistence in two coral reef fish species
================================================================================

This repository contains the code and data to reproduce all tables and
figures presented in Brandl et al. “Metabolic differentiation
facilitates coexistence in two coral reef fish species.”

Instructions
------------

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

Details
-------

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

Datasets
--------

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
    vertical gape size (V\_gape, mm), and horizontal gape size (H\_gape,
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
    of the observations (30 minutes); before\_after = before or after
    generic food addition – only before data are used for this paper.
    All videos, labelled by their respective trial, are accessible under
    the following link:
    <a href="https://www.youtube.com/playlist?list=PL9LfYSgK5itOtyll0jJr7Kszdf6olw6Ce" class="uri">https://www.youtube.com/playlist?list=PL9LfYSgK5itOtyll0jJr7Kszdf6olw6Ce</a>

Supplemental info
-----------------

This paper was produced using the following software and associated
packages:

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value                       
    ##  version  R version 3.6.1 (2019-07-05)
    ##  os       macOS Mojave 10.14.6        
    ##  system   x86_64, darwin15.6.0        
    ##  ui       X11                         
    ##  language (EN)                        
    ##  collate  en_US.UTF-8                 
    ##  ctype    en_US.UTF-8                 
    ##  tz       Europe/Paris                
    ##  date     2020-05-05                  
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package     * version date       lib source        
    ##  assertthat    0.2.1   2019-03-21 [1] CRAN (R 3.6.0)
    ##  backports     1.1.5   2019-10-02 [1] CRAN (R 3.6.1)
    ##  callr         3.4.2   2020-02-12 [1] CRAN (R 3.6.0)
    ##  cli           2.0.2   2020-02-28 [1] CRAN (R 3.6.0)
    ##  crayon        1.3.4   2017-09-16 [1] CRAN (R 3.6.0)
    ##  desc          1.2.0   2018-05-01 [1] CRAN (R 3.6.0)
    ##  devtools      2.2.1   2019-09-24 [1] CRAN (R 3.6.1)
    ##  digest        0.6.25  2020-02-23 [1] CRAN (R 3.6.0)
    ##  ellipsis      0.3.0   2019-09-20 [1] CRAN (R 3.6.0)
    ##  evaluate      0.14    2019-05-28 [1] CRAN (R 3.6.0)
    ##  fansi         0.4.1   2020-01-08 [1] CRAN (R 3.6.0)
    ##  fs            1.3.1   2019-05-06 [1] CRAN (R 3.6.0)
    ##  glue          1.3.2   2020-03-12 [1] CRAN (R 3.6.0)
    ##  htmltools     0.4.0   2019-10-04 [1] CRAN (R 3.6.0)
    ##  knitr         1.27    2020-01-16 [1] CRAN (R 3.6.1)
    ##  magrittr      1.5     2014-11-22 [1] CRAN (R 3.6.0)
    ##  memoise       1.1.0   2017-04-21 [1] CRAN (R 3.6.0)
    ##  pkgbuild      1.0.6   2019-10-09 [1] CRAN (R 3.6.0)
    ##  pkgload       1.0.2   2018-10-29 [1] CRAN (R 3.6.0)
    ##  prettyunits   1.1.1   2020-01-24 [1] CRAN (R 3.6.0)
    ##  processx      3.4.2   2020-02-09 [1] CRAN (R 3.6.0)
    ##  ps            1.3.2   2020-02-13 [1] CRAN (R 3.6.0)
    ##  R6            2.4.1   2019-11-12 [1] CRAN (R 3.6.0)
    ##  Rcpp          1.0.4   2020-03-17 [1] CRAN (R 3.6.0)
    ##  remotes       2.1.1   2020-02-15 [1] CRAN (R 3.6.0)
    ##  rlang         0.4.5   2020-03-01 [1] CRAN (R 3.6.0)
    ##  rmarkdown     2.0     2019-12-12 [1] CRAN (R 3.6.0)
    ##  rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.6.0)
    ##  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.6.0)
    ##  stringi       1.4.6   2020-02-17 [1] CRAN (R 3.6.0)
    ##  stringr       1.4.0   2019-02-10 [1] CRAN (R 3.6.0)
    ##  testthat      2.3.2   2020-03-02 [1] CRAN (R 3.6.0)
    ##  usethis       1.5.1   2019-07-04 [1] CRAN (R 3.6.0)
    ##  withr         2.1.2   2018-03-15 [1] CRAN (R 3.6.0)
    ##  xfun          0.12    2020-01-13 [1] CRAN (R 3.6.0)
    ##  yaml          2.2.0   2018-07-25 [1] CRAN (R 3.6.0)
    ## 
    ## [1] /Library/Frameworks/R.framework/Versions/3.6/Resources/library

All code written by Simon J. Brandl
(<a href="mailto:simonjbrandl@gmail.com" class="email">simonjbrandl@gmail.com</a>
and
<a href="https://github.com/simonjbrandl" class="uri">https://github.com/simonjbrandl</a>).
Please contact me for any issue or question.
