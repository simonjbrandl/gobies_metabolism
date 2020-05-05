#load packages

list.of.packages <- c(
  "drake", 
  "tidyverse",
  "lazyeval",
  "magrittr",
  "tidybayes",
  "ggstance",
  "rstan",
  "brms",
  "ggrepel",
  "ggpubr",
  "vegan",
  "ggfortify",
  "bipartite",
  #"geomnet",
  "GGally",
  "fishualize",
  "iNEXT",
  "patchwork",
  "jSDM",
  "sjPlot",
  "rgdal",
  "broom",
  "devtools")

# install if needed
new.packages <- 
  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if (length(new.packages)){
  install.packages(new.packages)
}

# install latest version of geomnet if not installed yet
if (!"geomnet" %in% installed.packages()){
  devtools::install_github("sctyner/geomnet")}

# load all packages
lapply(list.of.packages, function(x){
         library(x, character.only = TRUE)})
