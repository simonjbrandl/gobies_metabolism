-   [1. commdat = Community data: data were collected using enclosed
    clove-oil stations in both the lagoon and on the forereef around the
    island of Mo’orea, French Polynesia. Contains each caught individual
    of cryptobenthic fish (as rows), including the two target species.
    Columns specify each individuals corresponding sampling information
    (location, site, outcrop, lat & long, curved surface length, depth),
    identity, size, etc. All collections done in 2017, 2018, 2019,
    and 2020.](#commdat-community-data-data-were-collected-using-enclosed-clove-oil-stations-in-both-the-lagoon-and-on-the-forereef-around-the-island-of-moorea-french-polynesia.-contains-each-caught-individual-of-cryptobenthic-fish-as-rows-including-the-two-target-species.-columns-specify-each-individuals-corresponding-sampling-information-location-site-outcrop-lat-long-curved-surface-length-depth-identity-size-etc.-all-collections-done-in-2017-2018-2019-and-2020.)
-   [2. respodat = Respirometry: these data are derived from
    intermittent respirometry, performed in the lab at CRIOBE. Each row
    is an individual fish and its associated metadata. SMR = standard
    metabolic rate in mg O2 per h. MaxMR = maximum metabolic rate in mg
    O2 per h. MeanTemp…C. = temperature in ºCelsius. Weight..kg. = mass
    in g.](#respodat-respirometry-these-data-are-derived-from-intermittent-respirometry-performed-in-the-lab-at-criobe.-each-row-is-an-individual-fish-and-its-associated-metadata.-smr-standard-metabolic-rate-in-mg-o2-per-h.-maxmr-maximum-metabolic-rate-in-mg-o2-per-h.-meantempc.-temperature-in-ºcelsius.-weight..kg.-mass-in-g.)
-   [3. morpho = External morphology; gut.morpho = internal morphology:
    data derived from measurements performed on individuals collected
    from the field. Two different datasets for external and internal
    morphology. Each row is a unique individual with associated metadata
    and measurements for SL (mm), TL (mm), weight (g), girth (mm),
    vertical gape size (V\_gape, mm), and horizontal gape size (H\_gape,
    mm). Gut morpho is a smaller set but same setup, with the length of
    the gastrointestinal tract (GIT)
    in mm.](#morpho-external-morphology-gut.morpho-internal-morphology-data-derived-from-measurements-performed-on-individuals-collected-from-the-field.-two-different-datasets-for-external-and-internal-morphology.-each-row-is-a-unique-individual-with-associated-metadata-and-measurements-for-sl-mm-tl-mm-weight-g-girth-mm-vertical-gape-size-v_gape-mm-and-horizontal-gape-size-h_gape-mm.-gut-morpho-is-a-smaller-set-but-same-setup-with-the-length-of-the-gastrointestinal-tract-git-in-mm.)
-   [4. DNA gut content metabarcoding, three distinct
    datasets:](#dna-gut-content-metabarcoding-three-distinct-datasets)
-   [a) gobies.meta = Metadata](#a-gobies.meta-metadata)
-   [b) gobies.coi = COI primer data](#b-gobies.coi-coi-primer-data)
-   [c) goby.23s = 23S primer data](#c-goby.23s-23s-primer-data)
-   [gobies.meta contains information on all sampled and processed
    individuals, with each individual havign a unique ID and its own
    row. IDs are critical to match with the two other
    datasets.](#gobies.meta-contains-information-on-all-sampled-and-processed-individuals-with-each-individual-havign-a-unique-id-and-its-own-row.-ids-are-critical-to-match-with-the-two-other-datasets.)
-   [gobies coi and 23S: each row is an exact sequence variant (ESV)
    that was assigned to a prey taxon. Since this was part of a greater
    collection effort, there are many rows that are 0 across all
    columns. Columns 1:10 contain infromation about the sequence, while
    the remaining columns (G01 - G40) are individual fishes linked to
    the Extraction ID in the metadata. Values in the dataset are
    sequence
    abundances.](#gobies-coi-and-23s-each-row-is-an-exact-sequence-variant-esv-that-was-assigned-to-a-prey-taxon.-since-this-was-part-of-a-greater-collection-effort-there-are-many-rows-that-are-0-across-all-columns.-columns-110-contain-infromation-about-the-sequence-while-the-remaining-columns-g01---g40-are-individual-fishes-linked-to-the-extraction-id-in-the-metadata.-values-in-the-dataset-are-sequence-abundances.)
-   [5. aquarium.trials = Feeding trials: data are observations from
    videos of sand gobies in varying configurations in aquaria. id =
    identity of the observed individual; species = species affiliation
    (F. neophytus or G. cauerensis); video = video ID; treatment =
    configuration tested; feed = number of bites taken over the duration
    of the observations (30 minutes); before\_after = before or after
    generic food addition – only before data are used for this paper.
    All videos, labelled by their respective trial, are accessible under
    the following link:
    <a href="https://www.youtube.com/playlist?list=PL9LfYSgK5itOtyll0jJr7Kszdf6olw6Ce" class="uri">https://www.youtube.com/playlist?list=PL9LfYSgK5itOtyll0jJr7Kszdf6olw6Ce</a>](#aquarium.trials-feeding-trials-data-are-observations-from-videos-of-sand-gobies-in-varying-configurations-in-aquaria.-id-identity-of-the-observed-individual-species-species-affiliation-f.-neophytus-or-g.-cauerensis-video-video-id-treatment-configuration-tested-feed-number-of-bites-taken-over-the-duration-of-the-observations-30-minutes-before_after-before-or-after-generic-food-addition-only-before-data-are-used-for-this-paper.-all-videos-labelled-by-their-respective-trial-are-accessible-under-the-following-link-httpswww.youtube.complaylistlistpl9lfysgk5itotyll0jjr7kszdf6olw6ce)

The materials in this folder permit the reproduction of the results
presented in Brandl et al. “Metabolic differentiation facilitates
coexistence in two coral reef fish species.” The code is written in the
drake pipeline
(<a href="https://github.com/ropensci/drake" class="uri">https://github.com/ropensci/drake</a>).
The elements of the project (gobies\_metabolism.Rproj) include:

1.  data: this folder includes all the raw data necessary to run the
    script.
2.  output: this folder includesall outputs from the R-script.
3.  documents: this folder includes the manuscript, appendices, and
    cover letter.
4.  r: this folder contains three .R files, “packages.R”, “functions.R”,
    and “plan.R”. To run the code for the entire project, simply open
    all three R files along with the \_drake.R file. Make sure you have
    the drake package loaded in your environment and then run
    “r\_make()” in your R console. For any issues, please do not
    hesitate to contact me.

Datasets: a short description of each dataset is provided below.

1. commdat = Community data: data were collected using enclosed clove-oil stations in both the lagoon and on the forereef around the island of Mo’orea, French Polynesia. Contains each caught individual of cryptobenthic fish (as rows), including the two target species. Columns specify each individuals corresponding sampling information (location, site, outcrop, lat & long, curved surface length, depth), identity, size, etc. All collections done in 2017, 2018, 2019, and 2020.
==============================================================================================================================================================================================================================================================================================================================================================================================================================================================================================

2. respodat = Respirometry: these data are derived from intermittent respirometry, performed in the lab at CRIOBE. Each row is an individual fish and its associated metadata. SMR = standard metabolic rate in mg O2 per h. MaxMR = maximum metabolic rate in mg O2 per h. MeanTemp…C. = temperature in ºCelsius. Weight..kg. = mass in g.
===========================================================================================================================================================================================================================================================================================================================================

3. morpho = External morphology; gut.morpho = internal morphology: data derived from measurements performed on individuals collected from the field. Two different datasets for external and internal morphology. Each row is a unique individual with associated metadata and measurements for SL (mm), TL (mm), weight (g), girth (mm), vertical gape size (V\_gape, mm), and horizontal gape size (H\_gape, mm). Gut morpho is a smaller set but same setup, with the length of the gastrointestinal tract (GIT) in mm.
==========================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================

4. DNA gut content metabarcoding, three distinct datasets:
==========================================================

a) gobies.meta = Metadata
=========================

b) gobies.coi = COI primer data
===============================

c) goby.23s = 23S primer data
=============================

gobies.meta contains information on all sampled and processed individuals, with each individual havign a unique ID and its own row. IDs are critical to match with the two other datasets.
==========================================================================================================================================================================================

gobies coi and 23S: each row is an exact sequence variant (ESV) that was assigned to a prey taxon. Since this was part of a greater collection effort, there are many rows that are 0 across all columns. Columns 1:10 contain infromation about the sequence, while the remaining columns (G01 - G40) are individual fishes linked to the Extraction ID in the metadata. Values in the dataset are sequence abundances.
========================================================================================================================================================================================================================================================================================================================================================================================================================

5. aquarium.trials = Feeding trials: data are observations from videos of sand gobies in varying configurations in aquaria. id = identity of the observed individual; species = species affiliation (F. neophytus or G. cauerensis); video = video ID; treatment = configuration tested; feed = number of bites taken over the duration of the observations (30 minutes); before\_after = before or after generic food addition – only before data are used for this paper. All videos, labelled by their respective trial, are accessible under the following link: <a href="https://www.youtube.com/playlist?list=PL9LfYSgK5itOtyll0jJr7Kszdf6olw6Ce" class="uri">https://www.youtube.com/playlist?list=PL9LfYSgK5itOtyll0jJr7Kszdf6olw6Ce</a>
================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================

session\_info:

─ Session info
───────────────────────────────────────────────────────────────────────────────────────────
setting value  
version R version 3.6.1 (2019-07-05) os macOS Mojave 10.14.6  
system x86\_64, darwin15.6.0  
ui RStudio  
language (EN)  
collate en\_US.UTF-8  
ctype en\_US.UTF-8  
tz Europe/Paris  
date 2020-05-04

─ Packages
───────────────────────────────────────────────────────────────────────────────────────────────
package \* version date lib source  
assertthat 0.2.1 2019-03-21 \[1\] CRAN (R 3.6.0) backports 1.1.5
2019-10-02 \[1\] CRAN (R 3.6.1) base64url 1.4 2018-05-14 \[1\] CRAN (R
3.6.0) callr 3.4.2 2020-02-12 \[1\] CRAN (R 3.6.0) cli 2.0.2 2020-02-28
\[1\] CRAN (R 3.6.0) crayon 1.3.4 2017-09-16 \[1\] CRAN (R 3.6.0) desc
1.2.0 2018-05-01 \[1\] CRAN (R 3.6.0) devtools 2.2.1 2019-09-24 \[1\]
CRAN (R 3.6.1) digest 0.6.25 2020-02-23 \[1\] CRAN (R 3.6.0) drake \*
7.7.0 2019-10-15 \[1\] CRAN (R 3.6.0) ellipsis 0.3.0 2019-09-20 \[1\]
CRAN (R 3.6.0) fansi 0.4.1 2020-01-08 \[1\] CRAN (R 3.6.0) filelock
1.0.2 2018-10-05 \[1\] CRAN (R 3.6.0) fs 1.3.1 2019-05-06 \[1\] CRAN (R
3.6.0) glue 1.3.2 2020-03-12 \[1\] CRAN (R 3.6.0) igraph 1.2.5
2020-03-19 \[1\] CRAN (R 3.6.0) magrittr 1.5 2014-11-22 \[1\] CRAN (R
3.6.0) memoise 1.1.0 2017-04-21 \[1\] CRAN (R 3.6.0) packrat 0.5.0
2018-11-14 \[1\] CRAN (R 3.6.0) pkgbuild 1.0.6 2019-10-09 \[1\] CRAN (R
3.6.0) pkgconfig 2.0.3 2019-09-22 \[1\] CRAN (R 3.6.0) pkgload 1.0.2
2018-10-29 \[1\] CRAN (R 3.6.0) prettyunits 1.1.1 2020-01-24 \[1\] CRAN
(R 3.6.0) processx 3.4.2 2020-02-09 \[1\] CRAN (R 3.6.0) ps 1.3.2
2020-02-13 \[1\] CRAN (R 3.6.0) R6 2.4.1 2019-11-12 \[1\] CRAN (R 3.6.0)
Rcpp 1.0.4 2020-03-17 \[1\] CRAN (R 3.6.0) remotes 2.1.1 2020-02-15
\[1\] CRAN (R 3.6.0) rlang 0.4.5 2020-03-01 \[1\] CRAN (R 3.6.0)
rprojroot 1.3-2 2018-01-03 \[1\] CRAN (R 3.6.0) rstudioapi 0.11
2020-02-07 \[1\] CRAN (R 3.6.0) sessioninfo 1.1.1 2018-11-05 \[1\] CRAN
(R 3.6.0) storr 1.2.1 2018-10-18 \[1\] CRAN (R 3.6.0) testthat 2.3.2
2020-03-02 \[1\] CRAN (R 3.6.0) txtq 0.2.0 2019-10-15 \[1\] CRAN (R
3.6.0) usethis 1.5.1 2019-07-04 \[1\] CRAN (R 3.6.0) withr 2.1.2
2018-03-15 \[1\] CRAN (R 3.6.0)

\[1\] /Library/Frameworks/R.framework/Versions/3.6/Resources/library

All code written by Simon J. Brandl
(<a href="mailto:simonjbrandl@gmail.com" class="email">simonjbrandl@gmail.com</a>
and
<a href="https://github.com/simonjbrandl" class="uri">https://github.com/simonjbrandl</a>).
