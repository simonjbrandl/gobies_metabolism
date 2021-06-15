#### SETUP & GENERAL FUNCTIONS ####

#set colors for paper
gobycols <- fish(2, option = "Pomacanthus_imperator")

# function: write summary table to html
write_sum_tab <- function(model, output){
  out <- paste0("output/tables/", output)
  tab_model(model, file = out)
}

empty_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}


#########################
#########################
#### 1. COOCCURRENCE ####
#########################
#########################

#################
#### 1A. MAP ####
#################

# function: create helper vector with all sites and metadata
helper_cooc <- function(data){
  data %>%
  distinct(Sitenumber, Depth, Dimension, Habitat, Lat, Long) %>%
  filter(Sitenumber != "BFAIL")  
}

# function: clean cooccurrence data
clean_cooc_dat <- function(raw, helper){
  raw %>% 
      filter(Sitenumber != "BFAIL") %>%
      mutate(Sitenumber= factor(Sitenumber)) %>%
      filter(Tax %in% c("FUSINEOP", "GNATCAUE")) %>%
      group_by(Sitenumber, Tax) %>%
      summarize(abundance = n(), biomass = sum(W)) %>%
      ungroup() %>% 
      mutate(Tax = factor(Tax)) %>%
      complete(Sitenumber, Tax, fill = list(abundance = 0, biomass = 0)) %>%
      left_join(helper) %>%
      mutate(pres = case_when(abundance > 0 ~ 1,
                              TRUE ~ 0)) %>%
      ungroup() %>%
      mutate(occupancy = case_when(abundance < 1 ~ "NONE",
                                   TRUE ~ as.character(Tax))) %>%
      distinct()
}

# function: load shapefile and extract/tidy/fortify data
tidy_shape <- function(data){
  data %>% tidy()
}

# function: turn into WGS and fortify
fortify_wgs_shape <- function(data){
    data.frame(Northing = data$long, Easting = data$lat) %>% 
    SpatialPoints(., proj4string=CRS("+proj=utm +zone=6 +south +datum=WGS84")) %>% 
    spTransform(., CRS("+proj=longlat +datum=WGS84")) %>%
    fortify() %>%
    mutate(group = data$group)
  
}

# function: plot map with circles 
create_moorea_map <- function(geo, raw){
  Fig1A <- ggplot() + geom_polygon(data = geo, aes( x = long, y = lat, group = group), 
               fill="grey46", alpha = 0.75) +
  theme_bw() +
  geom_jitter(data = raw, aes(x = Long, y = Lat, fill = occupancy, shape = Habitat, 
                                        size = log(abundance+1)), 
              alpha = 0.5, width = 0.003, height = 0.003) +
  scale_fill_manual(values = c(gobycols, "white"), 
                    labels = c(expression(italic("F. neophytus")), 
                               expression(italic("G. cauerensis")), "None")) +
  scale_shape_manual(values = c(21,24), labels = c("Lagoon", "Slope")) +
  theme(legend.position = "top",
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(breaks = (c(-17.5,-17.6))) +
  scale_x_continuous(breaks = c(-149.9, -149.8)) +
  guides(fill=guide_legend(override.aes=list(shape=c(22,22,22)))) 
  
  # ggsave("output/plots/Figure1a_Brandl_Gobies.png", Fig1A, width = 4, height = 5)
  return(Fig1A)
}

##################
#### 1B. JSDM ####
##################

# function to prepare data for jSDM
data_prep_jsdm <- function(raw, helper){
  raw %>% 
    filter(Sitenumber != "BFAIL") %>%
    mutate(Sitenumber= factor(Sitenumber)) %>%
    filter(Tax %in% c("FUSINEOP", "GNATCAUE")) %>%
    group_by(Sitenumber, Tax) %>%
    summarize(abundance = n(), biomass = sum(W)) %>%
    ungroup() %>% 
    mutate(Tax = factor(Tax)) %>%
    complete(Sitenumber, Tax, fill = list(abundance = 0, biomass = 0)) %>%
    left_join(helper) %>%
    # compute presence-absence
    mutate(pres = case_when(abundance > 0 ~ 1,
                            TRUE ~ 0)) %>%
    ungroup() %>%
    dplyr::select(Sitenumber, Depth, Dimension, Habitat, Tax, abundance, biomass, pres) %>%
    # long gormat
    pivot_wider(names_from = Tax, values_from = c(pres, abundance, biomass)) %>%
    select(Habitat, Depth, pres_FUSINEOP, pres_GNATCAUE) %>%
    arrange(Habitat)
}

# function: run joint species distribution model (jsdm)
#' @param gobies occurrences of the two goby species
#' @param env environmental variable in the model, here habitat
run_jsdm <- function(gobies, env){
  jSDM_gobies <- jSDM_probit_block (
    # Response variable 
    presence_site_sp = as.matrix(gobies), 
    # Explanatory variables 
    site_suitability = ~.,   
    site_data = env, 
    # Chains
    burnin=20000, mcmc=20000, thin=5,
    # Starting values
    alpha_start=0, beta_start=0,
    lambda_start=0, W_start=0,
    V_alpha_start=1, 
    # Priors
    shape=0.5, rate=0.0005,
    mu_beta=0, V_beta=1.0E6,
    mu_lambda=0, V_lambda=10, verbose=1)
}

# function: get predicted values from jSDM
get_predicted_thetas <- function(mod, spec, sit){
  jSDM::predict.jSDM(mod,
                     Id_species = spec, Id_sites = sit, type="quantile") 
}

# function 1: get estimates for first species into object
get_pred_sp1 <- function(data){
  as.data.frame(data[1])
}

# function 2: get estimates for second species, combine with first, add column from raw
#' @param data predictions from model for second species
#' @param comb predictions for first species
#' @param raw raw data habitat column
get_pred_sp2_comb <- function(data, comb, raw){
as.data.frame(data[2]) %>%
  bind_cols(comb) %>%
  add_column(
    habitat = raw$Habitat)
}

# function: plot jSDM predictions
plot_jsdm_pred_compl <- function(predictions){
  Fig1B <- ggplot(predictions, aes(x = pres_FUSINEOP.mean, y = pres_GNATCAUE.mean)) +
    geom_density_2d(color = "black", lty = 2) +
    geom_jitter(aes(fill = as.factor(habitat), shape = as.factor(habitat)), 
                width = 0.05, height = 0.05, size = 3, alpha = 0.75) +
    theme_bw() +
    theme(axis.text = element_text(color = "black", size = 10),
          legend.position = c(0.11,0.9),
          legend.title = element_blank(),
          legend.background = element_blank()) +
    scale_fill_fish(option = "Epinephelus_fasciatus", end = 0.8, discrete = T, direction = 1) +
    scale_shape_manual(values = c(21,24)) +
    scale_y_continuous(limits = c(0, 1), oob = scales::squish) +
    scale_x_continuous(limits = c(0, 1), oob = scales::squish) +
    ylab(expression(Predicted~probability~of~italic("G. cauerensis")~occurrence)) +
    xlab(expression(Predicted~probability~of~italic("F. neophytus")~occurrence)) +
    guides()
  
  # ggsave("output/plots/Figure1b_Brandl_Gobies.png", Fig1B, width = 4, height = 5)
  return(Fig1B)
}

# function: combine Fig 1A and 1B
comb_figs <- function(f1, f2){
  Figure1_Brandl_Gobies <- f1 | f2 +
    plot_annotation(tag_levels = 'A')

  ggsave("output/plots/Figure1_Brandl_Gobies.png", Figure1_Brandl_Gobies, width = 10, height = 5)
  
}

#######################
#######################
#### 2. PHYSIOLOGY ####
#######################
#######################

# function: clean respirometry data
clean_resp_dat <- function(data){
  data %>%
    drop_na(SMR) %>%
    #calculate aerobic scope 
    mutate(AerSco = MaxMR / SMR) %>%
    #rename temperature
    rename(TempC = MeanTemp...C.) %>%
    #get rid of the two trials where temperature fell below 26ºC
    filter(TempC >26) %>%
    #transform weight from kg to g
    mutate(W = W*1000)
}

# specify brms priors 
#' @param response specify response variable
#' @param dataset data to be used


get_smr_prior <- function(response, dataset){
  get_prior(paste0("log10(", response,") ~ log10(W) * Species + TempC"), dataset)
}

# run brms model phys_brms
#' @param response specify response variable
#' @param dataset data to be used
#' @param set_prior prior defined 

run_brms_smr <- function(response, dataset, set_prior){
  brm(paste0("log10(", response,") ~ log10(W) * Species + TempC"),
      data = dataset, prior = set_prior,
      control = list(adapt_delta = 0.999, max_treedepth = 15), iter = 5000)
}


# function: predict brms for phys -- to be used throughout so general
#' @param raw raw data the model was run on
#' @param ... variables in the model
#' @param mod brms model object
#' @param draws number of draws from posterior
#' @param backtrans logical backtransform predictions
#' @param transform type of backtransformation to apply

predict_from_brms_phys <- function(raw, ..., mod, draws, backtrans = F, transform = NA){
  
  if (backtrans & is.na(transform)){
    stop("Need to specify transformation")
  }
  
  prediction.new <- raw %>%
    group_by(Species) %>%
    data_grid(...) %>%
    add_fitted_draws(mod, n = draws) %>%
    as_tibble()
  
  if (backtrans){
    if (transform == "exp"){
      value <- exp(prediction.new$.value)
    } else if (transform == "e10"){
      value <- 10^prediction.new$.value
    }
    prediction.new <- prediction.new %>%
      mutate(pred.corr = value)
  }
  return(prediction.new)
}

# function: summarize predicted values for in text
predict_summary_phys <- function(prediction.new, var){
  prediction.new %>%
    group_by(Species) %>%
    summarise(var = list(enframe(quantile(pred.corr, probs=c(0.025,0.5,0.975))))) %>%
    unnest()
}

# function: plot predicted and raw values
# plot results to see fit
plot_smr_preds <- function(raw, response, prediction){
  Fig2A <- ggplot(raw, aes(x = log10(W), y = log10(SMR))) +
    geom_line(data = prediction, aes(x = log10(W), y = .value, group = paste(Species, .draw), 
                                     color = Species), alpha = 0.1) +
    geom_point(data = raw, aes(shape = Species, fill = Species), 
               color = "grey69", size = 2, alpha = 0.9) +
    theme_bw() +
    theme(legend.position = c(0.2, 0.9),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.text = element_text(color = "black", size = 10),
          legend.text.align = 0,
          axis.text = element_text(color = "black", size = 10)) +
    scale_fill_manual(values = gobycols, 
                      labels = c(expression(italic("Fusigobius neophytus")), 
                                 expression(italic("Gnatholepis cauerensis")))) +
    scale_color_manual(values = gobycols, 
                       labels = c(expression(italic("Fusigobius neophytus")), 
                                  expression(italic("Gnatholepis cauerensis")))) +
    scale_shape_manual(values = c(21, 23), 
                       labels = c(expression(italic("Fusigobius neophytus")), 
                                  expression(italic("Gnatholepis cauerensis")))) +
    xlab("log10 body mass (g)") +
    ylab(expression(log10~SMR~""~(mg~O[2]~h^{-1}))) + 
    scale_y_continuous(limits = c(-1.5, -0.5)) +
    add_fishape(family = "Gobiidae", option = "Fusigobius_neophytus", 
                scaled = TRUE, xmin = 0.4, xmax = 0.6, ymax = 1, ymin = 0.93, 
                fill = gobycols[1], xlim = c(-0.9, 0.3), ylim = c(-1.5, -0.50)) +
    add_fishape(family = "Gobiidae", option = "Gnatholepis_cauerensis", 
                scaled = TRUE, xmin = 0.4, xmax = 0.6, ymax = 0.92, ymin = 0.86, 
                fill = gobycols[2], xlim = c(-0.9, 0.3), ylim = c(-1.5, -0.50))
  
  return(Fig2A)
}

# function: create helper function for density plots
partial_density <- function(intervals){
  map(intervals, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    set_names(nm = map_chr(intervals, ~paste0(.x*100, "%"))) 
}

# function: calculate partial densities
get_partial_densities <- function(avg.predictions){
    data_frame("Fusigobius neophytus" = avg.predictions$pred.corr[1:(nrow(avg.predictions)/2)], 
                        "Gnatholepis cauerensis" = avg.predictions$pred.corr[(nrow(avg.predictions)/2+1):nrow(avg.predictions)]) %>%
    map(density) %>%
    map_dfr(~data_frame(y = .x$x, x = .x$y), .id = 'Species')
}


# function: calculate summaries for plotting
get_partial_density_sums <- function(avg.predictions, p.function){
  avg.predictions %>%
    group_by(Species) %>%
    summarize_at(vars(pred.corr), funs(!!!p.function)) %>%
    pivot_longer(cols = 2:4, names_to = "percentile", values_to = "value") %>%
    as.data.frame()
}

# function: plot density curves at mean weight with shading for intervals
plot_density_avg_weight_smr <- function(density, density.sum){
  Fig2B <- ggplot(density, aes(x = x, y = y, color = Species, group = Species, fill = Species)) +
  geom_hline(data = density.sum, aes(yintercept = value, color = Species), lwd = 0.5, lty = c(3,2,3,3,2,3)) +
  scale_fill_manual(values = gobycols) +
  scale_color_manual(values = gobycols) +
  theme_bw() + theme(legend.position = "none",
                     axis.text = element_text(color = "black", size = 10)) +
  geom_area(data = filter(density, Species == "Fusigobius neophytus" & y < density.sum[1,3]),
            alpha = 0.1, orientation = "y") +
  geom_area(data = filter(density, Species == "Fusigobius neophytus" & y > density.sum[1,3] & y < density.sum[3,3]),
            alpha = 0.75, orientation = "y") +
  geom_area(data = filter(density, Species == "Fusigobius neophytus" & y > density.sum[3,3]),
            alpha = 0.1, orientation = "y") +
  geom_area(data = filter(density, Species == "Gnatholepis cauerensis" & y < density.sum[4,3]),
            alpha = 0.1, orientation = "y") +
  geom_area(data = filter(density, Species == "Gnatholepis cauerensis" & y > density.sum[4,3] & y < density.sum[6,3]),
            alpha = 0.75, orientation = "y") +
  geom_area(data = filter(density, Species == "Gnatholepis cauerensis" & y > density.sum[6,3]),
            alpha = 0.1, orientation = "y") +
  ylab(expression(SMR~at~mean~mass~""~(mg~O[2]~h^{-1}))) +
  xlab("Predicted posterior density")
}


# now only need to change plotting function for MMR
# function: plot predicted and raw values
plot_mmr_preds <- function(raw, response, prediction){
  Fig2C <- ggplot(raw, aes(x = log10(W), y = log10(MaxMR))) +
    geom_line(data = prediction, aes(x = log10(W), y = .value, group = paste(Species, .draw), 
                                     color = Species), alpha = 0.1) +
    geom_point(data = raw, aes(shape = Species, fill = Species), 
               color = "grey69", size = 2, alpha = 0.9) +
    theme_bw() +
    theme(legend.position = c(0.2, 0.9),
          legend.title = element_blank(),
          legend.text = element_text(color = "black", size = 10),
          legend.background = element_blank(),
          legend.text.align = 0,
          axis.text = element_text(color = "black", size = 10)) +
    scale_fill_manual(values = gobycols, 
                      labels = c(expression(italic("Fusigobius neophytus")), expression(italic("Gnatholepis cauerensis")))) +
    scale_color_manual(values = gobycols, 
                       labels = c(expression(italic("Fusigobius neophytus")), expression(italic("Gnatholepis cauerensis")))) +
    scale_shape_manual(values = c(21, 23), 
                       labels = c(expression(italic("Fusigobius neophytus")), expression(italic("Gnatholepis cauerensis")))) +
    xlab("log10 body mass (g)") +
    ylab(expression(log10~MMR~""~(mg~O[2]~h^{-1}))) +
    scale_y_continuous(limits = c(-1.1, 0)) +
    add_fishape(family = "Gobiidae", 
                option = "Fusigobius_neophytus", scaled = TRUE, xmin = 0.4, xmax = 0.6, ymax = 1, ymin = 0.93, fill = gobycols[1], xlim = c(-0.9, 0.3), ylim = c(-1.1, 0)) +
    add_fishape(family = "Gobiidae",
                option = "Gnatholepis_cauerensis", scaled = TRUE, xmin = 0.4, xmax = 0.6, ymax = 0.92, ymin = 0.86, fill = gobycols[2], xlim = c(-0.9, 0.3), ylim = c(-1.1, 0))
  return(Fig2C)
}


# function: plot density curves at mean weight with shading for intervals
plot_density_avg_weight_mmr <- function(density, density.sum){
  Fig2D <- ggplot(density, aes(x = x, y = y, color = Species, group = Species, fill = Species)) +
    geom_hline(data = density.sum, aes(yintercept = value, color = Species), lwd = 0.5, lty = c(3,2,3,3,2,3)) +
    scale_fill_manual(values = gobycols) +
    scale_color_manual(values = gobycols) +
    theme_bw() + theme(legend.position = "none",
                       axis.text = element_text(color = "black", size = 10)) +
    geom_area(data = filter(density, Species == "Fusigobius neophytus" & y < density.sum[1,3]),
              alpha = 0.1, orientation = "y") +
    geom_area(data = filter(density, Species == "Fusigobius neophytus" & y > density.sum[1,3] & y < density.sum[3,3]),
              alpha = 0.75, orientation = "y") +
    geom_area(data = filter(density, Species == "Fusigobius neophytus" & y > density.sum[3,3]),
              alpha = 0.1, orientation = "y") +
    geom_area(data = filter(density, Species == "Gnatholepis cauerensis" & y < density.sum[4,3]),
              alpha = 0.1, orientation = "y") +
    geom_area(data = filter(density, Species == "Gnatholepis cauerensis" & y > density.sum[4,3] & y < density.sum[6,3]),
              alpha = 0.75, orientation = "y") +
    geom_area(data = filter(density, Species == "Gnatholepis cauerensis" & y > density.sum[6,3]),
              alpha = 0.1, orientation = "y") +
    ylab(expression(MMR~at~mean~mass~""~(mg~O[2]~h^{-1}))) +
    xlab("Predicted posterior density")
  
  return(Fig2D)
}

# function: combine Fig 2A, 2B, 2C and 2D
comb_figs2 <- function(f1, f2, f3, f4){
  Figure2_Brandl_Gobies <- (f1 | f2) / (f3 | f4) +
    plot_annotation(tag_levels = 'A')

  ggsave("output/plots/Figure2_Brandl_Gobies.png", Figure2_Brandl_Gobies, width = 11, height = 8)

}


#######################
#######################
#### 3. MORPHOLOGY ####
#######################
#######################

# clean external morphology data
clean_morphology <- function(data){
  data %>%
    drop_na(H_gape, V_gape)
}

# generic brms function for morphology models
#' @param response specify response variable
#' @param dataset data to be used
run_brms_morpho <- function(response, dataset){
  brm(paste0(response," ~ SL + Species"),
      data = dataset,
      control = list(adapt_delta = 0.999, max_treedepth = 15), iter = 5000)
}


# generic predict function for morphology
#' @param raw raw data the model was run on
#' @param ... variables in the model
#' @param mod brms model object
#' @param draws number of draws from posterior
#' @param backtrans logical backtransform predictions
#' @param transform type of backtransformation to apply

predict_from_brms_morph <- function(raw, ..., mod, draws, backtrans = F, transform = NA){
  
  if (backtrans & is.na(transform)){
    stop("Need to specify transformation")
  }
  
  prediction.new <- raw %>%
    group_by(Species) %>%
    data_grid(...) %>%
    add_fitted_draws(mod, n = draws) %>%
    as_tibble()
  
  if (backtrans){
    if (transform == "exp"){
      value <- exp(prediction.new$.value)
    } else if (transform == "e10"){
      value <- 10^prediction.new$.value
    }
    prediction.new <- prediction.new %>%
      mutate(pred.corr = value)
  }
  return(prediction.new)
}


# function: plot morphology data

# plot predicted and raw values
morpho_plot <- function(raw, yvar, predicted, ylableg){
  ggplot(raw, aes_string(x = "SL", y = paste0(yvar))) +
    geom_line(data = predicted, 
              aes(x = SL, y = .value, group = paste(Species, .draw), color = Species), alpha = 0.1) +
    geom_point(data = raw, aes(shape = Species, fill = Species), color = "black", size = 2, alpha = 0.9) +
    theme_bw() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          legend.background = element_blank(),
          axis.text = element_text(color = "black")) +
    scale_fill_manual(values = gobycols, 
                      labels = c(expression(italic("Fusigobius neophytus")), 
                                 expression(italic("Gnatholepis cauerensis")))) +
    scale_color_manual(values = gobycols, 
                       labels = c(expression(italic("Fusigobius neophytus")), 
                                  expression(italic("Gnatholepis cauerensis")))) +
    scale_shape_manual(values = c(21, 23), 
                       labels = c(expression(italic("Fusigobius neophytus")), 
                                  expression(italic("Gnatholepis cauerensis")))) +
    xlab("Standard length (mm)") +
    ylab(paste0(ylableg))
}

# function: combine Fig 2A, 2B, 2C and 2D
comb_figsS1 <- function(f1, f2, f3, f4){
  FigureS1_Brandl_Gobies <- (f1 | f2) / (f3 | f4) +
    plot_annotation(tag_levels = 'A')
  
  
  ggsave("output/plots/FigureS1_Brandl_Gobies.png", FigureS1_Brandl_Gobies, width = 10, height = 8)
}



#########################
#########################
#### 4. GUT CONTENTS ####
#########################
#########################

# function: extract necessary information from metadata
clean_meta <- function(metadata){
  metadata %>%
    select(Extraction_ID, Gen_Spe)
}

# function: remove empty rows and clean up self hits 
#' @param raw raw data of sequences in rows, sequence information and host specimens in columns
#' @param begin.col column number of first host fish specimen
#' @param end.col column number of last host fish specimen
#' @param column column by which to filter
#' @param filter.by vector of strings to filter against

clean_sequence_data <- function(raw, begin.col, end.col, column, filter.by){
  filter_criteria <- interp(~!y %in% x, .values=list(y = as.name(column), x = filter.by))
  raw %>%
    # sum ESVs/OTUs over columns that contain hosts
    mutate(rowsum = rowSums(.[begin.col:end.col])) %>%
    # remove all empty OTU rows by filtering
    filter(rowsum > 1) %>%
    select(-rowsum) %>%
    #remove self-hits
    filter_(filter_criteria) 
}


# function: turn sequences into wide format
#' @param raw raw data of sequences in rows, sequence information and host specimens in columns
#' @param id.pos column number of your seq_ID column
#' @param begin.col column number of first host fish specimen
#' @param end.col column number of last host fish specimen
#' @param compute logical whether to compute a metric (TRUE) or retain raw sequence reads (FALSE)
#' @param metric type of metric to compute, with options "pa" for presence-absence or "rra" for relative read abundance

widen_sequence_data <- function(raw, id.pos, begin.col, end.col, compute = F, metric = NA){
  
  if (compute & is.na(metric)){
    stop("Need to specify metric")
  }
  
  raw.long <- raw %>%
    # remove all but seq_ID and fish specimen columns
    dplyr::select(c(id.pos, begin.col:end.col)) %>%
    # bring to long format
    pivot_longer(names_to = "Extraction_ID", values_to = "value", -seq_ID)
  
  raw.wide <- raw.long %>%
    pivot_wider(id_cols = Extraction_ID, names_from = seq_ID, values_from = value, values_fill = list(value = 0))

  
  if (compute){
    if (metric == "pa"){
      pa <- raw.long %>%
        mutate(pres.abs = case_when(value > 0 ~ 1,
                                        TRUE ~ 0)) %>%
        pivot_wider(id_cols = Extraction_ID, names_from = seq_ID, values_from = pres.abs, values_fill = list(pres.abs = 0))
        return(pa)
        } else if (metric == "rra"){
          rra <- raw.long %>%
            group_by(Extraction_ID) %>%
            mutate(relreadabu = value/sum(value)) %>%
            replace_na(list(relreadabu = 0)) %>%
            ungroup() %>%
            pivot_wider(id_cols = Extraction_ID, names_from = seq_ID, values_from = relreadabu, values_fill = list(relreadabu = 0))
          return(rra)
        }
  }
  return(raw.wide)
}

# function: summarize to species level
sum_species_comp <- function(seqwide, meta){
  seqwide %>%
    pivot_longer(-Extraction_ID, names_to = "seq_ID", values_to = "rra") %>%
    left_join(meta) %>%
    group_by(Gen_Spe, seq_ID) %>%
    summarize(avg.rra = mean(rra)) %>%
    ungroup() %>%
    pivot_wider(id_cols = Gen_Spe, names_from = seq_ID, values_from = avg.rra, values_fill = list(avg.rra = 0))
  }

# function: run niche partitioning model through EcoSimR
run_niche_model <- function(data){
  EcoSimR::niche_null_model(speciesData=data,
                            algo="ra3", metric="pianka", 
                            suppressProg=TRUE,nReps=1000)
}


# function: bring into long format
lengthen_seq_dat <- function(data.wide, meta){
  data.wide %>%
    pivot_longer(-Extraction_ID, names_to = "seq_ID", values_to = "value") %>%
    filter(value > 0) %>%
    group_by(seq_ID) %>%
    mutate(total = sum(value)) %>%
    left_join(meta)
}


# function: prep COI
prepare_coi <- function(data.coi){
  data.coi %>%
    mutate(dataset = "coi") %>%
    unite("ID_dataset", c(seq_ID, dataset), sep = "_", remove = F)
}

# function: prep 23s
prepare_23s <- function(data.23s){
  data.23s %>%
    mutate(dataset = "23s") %>%
    unite("ID_dataset", c(seq_ID, dataset), sep = "_", remove = F)
}

# function: combine primers for network
combine_for_network <- function(data1, data2){
  bind_rows(data1, data2) %>%
    filter(total > 1) %>%
    pivot_wider(id_cols = Extraction_ID, names_from = ID_dataset, values_from = value, values_fill = list(value = 0)) %>%
    mutate(Extraction_ID = factor(Extraction_ID))
}

# function: clean modularity data 
clean_modularity <- function(module.data, network.data, meta.data){
  # select only columns that correspond to fish specimens (i.e. length of network data rows)
  module.data %>%
    select(1:34) %>%
    mutate(module = as.character(seq(1:nrow(module.data)))) %>%
    pivot_longer(cols = 1:34, names_to = "sequence", values_to = "value") %>%
    mutate(Extraction_ID = rep(network.data$Extraction_ID, nrow(module.data))) %>%
    filter(value > 0) %>%
    inner_join(meta.data) %>%
    distinct()
}

# function: combine modules and network
comb_module_network <- function(data1, data2, modularity){
  bind_rows(data1, data2) %>%
    filter(total > 1) %>%
    inner_join(modularity[1:4], by = "Extraction_ID") %>%
    mutate(colorvec = case_when(dataset == "23s" ~ "grey90",
                                TRUE ~ "grey10"))
}

# function: plot network tree
make_network_tree <- function(tree.data){
    ggplot(data = tree.data) +
    geom_net(layout.alg = "kamadakawai", aes(from_id = Extraction_ID, to_id = ID_dataset, colour = Gen_Spe, fill = Gen_Spe, shape = as.factor(module)),
             alpha = 0.8, lwd=5, labelon = F, repel = T, ealpha = 0.3, arrowsize = 0.25, linewidth = 0.4, singletons = F, directed = TRUE,
             vjust = 0.5, hjust = 0.5) +
    # labelcolour = cfnet2$lcolour) +
    theme_net() +
    scale_shape_manual(values = c(21:25), name = "Module") +
    scale_color_manual(values = gobycols, name = "Species", labels = c(expression(italic("Fusigobius neophytus")), expression(italic("Gnatholepis cauerensis")))) +
    scale_fill_manual(values = gobycols, name = "Species", labels = c(expression(italic("Fusigobius neophytus")), expression(italic("Gnatholepis cauerensis")))) +
    theme(legend.position = "top")
}

# function: turn wide sequences into rarefaction dataset
wide_to_rare <- function(seq.wide, meta){
  seq.wide %>%
  pivot_longer(names_to = "seq_ID", values_to = "seq_abu", -Extraction_ID) %>%
    left_join(meta) %>%
    group_by(Gen_Spe, seq_ID) %>%
    summarize(sum_seq_abu = sum(seq_abu)) %>%
    pivot_wider(names_from = Gen_Spe, values_from = sum_seq_abu)
}

# function: tibble to transposed list for rarefaction
tibble_to_list <- function(tibble){
  tib.df <- as.data.frame(tibble)
  rownames(tib.df) <- tib.df[,1]
  tib.df.rn <- tib.df[-1]
  
  # transpose
  tib.df.trans <- as.data.frame(t(tib.df.rn))
  # split dataframe into lists, with each species/population as an element
  tib.df.trans.list <- split(tib.df.trans, rownames(tib.df.trans))
  
  # transpose each element
  tib.df.list.t <- lapply(tib.df.trans.list, t)
}

# function: run rarefaction and fortify data
rarify_to_plot <- function(data, raw.tibble, metadata){
  rare.data <- iNEXT(data, q=0, 
        datatype="abundance", size=NULL, 
        endpoint=max(colSums(raw.tibble[-1])),
        knots=50, se=TRUE, conf=0.95, nboot=100)
  rare.data %>%
    fortify(., type = 1) %>%
    rename(Gen_Spe = site) %>%
    inner_join(metadata)
}

# function: plot rarefaction curves
plot_rarefaction_curves <- function(rarefaction.fort){
  pointvals <- rarefaction.fort %>%
    filter(method == "observed")
  linevals <- rarefaction.fort %>%
    filter(method != "observed")
  
  ggplot(rarefaction.fort, aes(x = x, y = y, color=Gen_Spe)) +
    geom_line(data=linevals, aes(linetype=method), lwd=0.5) + 
    geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,fill=Gen_Spe, color=NULL), alpha=0.2) + 
    geom_point(data = pointvals) +
    labs(x="Number of sequences", y="Number of ESVs") +
    theme_bw() + theme(plot.title = element_text(size = 10),
                       axis.text = element_text(color = "black", size = 10),
                       axis.title = element_text(color = "black", size = 10),
                       legend.title=element_text(size=10), 
                       legend.text=element_text(size=10),
                       legend.position = "top") +
    scale_color_manual(values = c(gobycols),name = "Species", 
                       labels = c(expression(italic("Fusigobius neophytus")), 
                                  expression(italic("Gnatholepis cauerensis"))))+
    scale_fill_manual(values = c(gobycols), name = "Species", 
                      labels = c(expression(italic("Fusigobius neophytus")), 
                                 expression(italic("Gnatholepis cauerensis"))))+
    theme(strip.text = element_text(face = "italic"),
          plot.title = element_text(size = 14, face = "bold"))
}

# function: combine Fig 2A, 2B, 2C and 2D
comb_figs3 <- function(f1, f2, f3){
  Figure3_Brandl_Gobies <- (f1 / f2) | f3 +
    plot_annotation(tag_levels = 'A')
  
  
  ggsave("output/plots/Figure3_Brandl_Gobies.pdf", Figure3_Brandl_Gobies, width = 14, height = 8, useDingbats = F)
}


# function: prey taxa for 23s data
plot_prey_rra <- function(tax, rra, metadata){
  taxonomy <- tax[1:8]
  
  preytax <- rra %>%
    pivot_longer(cols = 2:length(colnames(rra)), names_to = "seq_ID") %>%
    pivot_wider(names_from = Extraction_ID) %>%
    inner_join(taxonomy, by = "seq_ID") %>%
    dplyr::select(seq_ID, Phylum, Order, Family, Genus, Species, everything()) %>%
    mutate_each(funs(empty_na)) %>%
    # make sure all empty cells are equal
    replace_na(list(Phylum = "null",
                    Order = "null",
                    Family = "null",
                    Genus = "null",
                    Species = "null")) %>%
    # fill cells with respective closest levels
    # terrible code but works
    mutate(high_tax = case_when(Genus == "null" ~ "Unidentified",
                                Family == "null" ~ as.character(Genus),
                                Order == "null" ~ as.character(Family),
                                Phylum == "null" ~ as.character(Order),
                                Phylum != "null" ~ as.character(Phylum),
                                TRUE ~ as.character(Phylum))) %>%
    mutate(high_replace = case_when(high_tax == "Unidentified" & Phylum != "null" ~ as.character(Phylum),
                                    TRUE ~ as.character(high_tax))) %>%
    mutate(high_replace = case_when(high_tax == "Unidentified" & Order != "null" ~ as.character(Order),
                                    TRUE ~ as.character(high_replace))) %>%
    mutate(high_replace = case_when(high_tax == "Unidentified" & Family != "null" ~ as.character(Family),
                                    TRUE ~ as.character(high_replace))) %>%
    mutate(high_replace = case_when(high_tax == "Unidentified" & Genus != "null" ~ as.character(Genus),
                                    TRUE ~ as.character(high_replace))) %>%
    dplyr::select(high_tax, high_replace, everything()) %>%
    # assign lower level taxa to higher 
    mutate(highest = case_when(high_replace == "Bacillariaceae" ~ "Bacillariophyta",
                               high_replace == "Bangiaceae" ~ "Rhodophyta",
                               high_replace == "Bangiales" ~ "Rhodophyta",
                               high_replace == "Bigelowiella" ~ "Cercozoa",
                               high_replace == "Ceramiales" ~ "Rhodophyta",
                               high_replace == "Chattonellales" ~ "Ochrophyta",
                               high_replace == "Chlorellales" ~ "Chlorophyta",
                               high_replace == "Corallinales" ~ "Rhodophyta",
                               high_replace == "Cryptomonadales" ~ "Cryptophyta",
                               high_replace == "Cyanophoraceae" ~ "Glaucophyta",
                               high_replace == "Cyanoptyche" ~ "Glaucophyta",
                               high_replace == "Desmochloris" ~ "Chlorophyta",
                               high_replace == "Dictyotales" ~ "Ochrophyta",
                               high_replace == "Ectocarpales" ~ "Ochrophyta",
                               high_replace == "Erythropeltidales" ~ "Rhodophyta",
                               high_replace == "Euglenida" ~ "Euglenozoa",
                               high_replace == "Euglyphidae" ~ "Cercozoa",
                               high_replace == "Euglyphida" ~ "Cercozoa",
                               high_replace == "Eustigmatales" ~"Ochrophyta",
                               high_replace == "Eutreptia" ~ "Euglenozoa",
                               high_replace == "Fagales" ~ "Angiosperms",
                               high_replace == "Fucales" ~ "Ochrophyta",
                               high_replace == "Gigartinales" ~ "Rhodophyta",
                               high_replace == "Gleicheniaceae" ~ "Tracheophyta",
                               high_replace == "Halymeniales" ~ "Rhodophyta",
                               high_replace == "Isochrysidales" ~ "Haptophyta",
                               high_replace == "Liagoraceae" ~ "Rhodophyta",
                               high_replace == "Naviculales" ~ "Bacillariophyta",
                               high_replace == "Naviculaceae" ~ "Bacillariophyta",
                               high_replace == "Nemaliales" ~ "Rhodophyta",
                               high_replace == "Peridiniales" ~ "Miozoa",
                               high_replace == "Pinaceae" ~ "Tracheophyta",
                               high_replace == "Porphyridiales" ~ "Rhodophyta",
                               high_replace == "Rhodomelaceae" ~ "Rhodophyta",
                               high_replace == "Sargassaceae" ~ "Ochrophyta",
                               high_replace == "Sphacelariales" ~ "Ochrophyta",
                               high_replace == "Stylonematales" ~ "Rhodophyta",
                               high_replace == "Trebouxia" ~ "Chlorophyta",
                               high_replace == "Ulvales" ~ "Chlorophyta",
                               high_replace == "Vaucheriales" ~ "Ochrophyta",
                               TRUE ~ high_replace)) %>%
    # remove Angiosperms
    filter(highest != "Angiosperms")
  
  prey <- preytax %>%
    pivot_longer(cols = G01:G40, names_to = "Extraction_ID") %>%
    left_join(metadata, by = "Extraction_ID") %>%
    group_by(Extraction_ID, highest, Gen_Spe) %>%
    summarize(propcont = sum(value))
}

# function: plot prey taxa
plot_prey_taxa <- function(prey){
  FigureS2_Brandl_Gobies <- ggplot(prey, aes(x = highest, y = propcont)) +
    geom_boxplot(aes(fill = Gen_Spe), position = position_dodge(), notch = FALSE, outlier.size = 0, outlier.alpha = 0, alpha = 0.75) +
    geom_jitter(aes(fill = Gen_Spe, shape = Gen_Spe), position = position_jitterdodge(), alpha = 0.5) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.background = element_blank(),
          axis.text = element_text(color = "black")) +
    ylab("Mean relative read abundance of autotroph taxa") +
    scale_fill_manual(values = gobycols, 
                      labels = c(expression(italic("Fusigobius neophytus")), 
                                 expression(italic("Gnatholepis cauerensis")))) +
    scale_color_manual(values = gobycols, 
                       labels = c(expression(italic("Fusigobius neophytus")), 
                                  expression(italic("Gnatholepis cauerensis")))) +
    scale_shape_manual(values = c(21, 23), 
                       labels = c(expression(italic("Fusigobius neophytus")), 
                                  expression(italic("Gnatholepis cauerensis"))))
  
  ggsave("output/plots/FigureS2_Brandl_Gobies.png", FigureS2_Brandl_Gobies, width = 8, height = 6)
  
  return(FigureS2_Brandl_Gobies)
}


#####################
#####################
#### 5. BEHAVIOR ####
#####################
#####################

# function: clean aquarium trial data
clean_aq_dat <- function(data){
  data %>%
  # code treatment into a two-factor variable: mixed treatment vs. monospecific treatment
  mutate(monovsmix = case_when(treatment == "vff" ~ "mono",
                               treatment == "vgg" ~ "mono",
                               treatment == "vfg" ~ "mixed")) %>%
  #restrict dataset to first 30 minutes
  dplyr::filter(before_after == "before")
}

# function: run brms model with random effect
run_brms_mod1 <- function(data){
  brm(feed ~ (species * monovsmix) + (1|video),
      data = data, 
      family = negbinomial, 
      control = list(adapt_delta = 0.99))
}

# function: run brms model without random effect
run_brms_mod2 <- function(data){
  brm(feed ~ (species * monovsmix),
      data = data, 
      family = negbinomial, 
      control = list(adapt_delta = 0.99))
}

# function: predict brms -- to be used throughout so general
#' @param raw raw data the model was run on
#' @param ... variables in the model
#' @param mod brms model object
#' @param draws number of draws from posterior
#' @param backtrans logical backtransform predictions
#' @param transform type of backtransformation to apply

predict_from_brms <- function(raw, ..., mod, draws, backtrans = F, transform = NA){
  
  if (backtrans & is.na(transform)){
    stop("Need to specify transformation")
  }
  
  prediction.new <- raw %>%
  data_grid(...) %>%
  add_fitted_draws(mod, n = draws) %>%
  as_tibble()
  
  if (backtrans){
    if (transform == "exp"){
      value <- exp(prediction.new$.value)
    } else if (transform == "div30"){
      value <- prediction.new$.value/30
    }
    prediction.new <- prediction.new %>%
      mutate(pred.corr = value)
  }
  return(prediction.new)
}


# function: summarize predicted values for in text information
predict_summary <- function(prediction.new){
  prediction.new %>%
    group_by(species, monovsmix) %>%
    summarise(bites = list(enframe(quantile(pred.corr, probs=c(0.025,0.5,0.975))))) %>%
    unnest()
}

# function: plot predicted and raw values of behavior model
create_behavior_plot <- function(pred, raw){
  behavior.plot <- ggplot(pred, aes(y = pred.corr, x = rev(monovsmix), fill = species, color = species)) +
    geom_violin(trim = T, draw_quantiles = c(0.025,0.5,0.975), alpha = 0.75, position = position_dodge(width = 0.5)) +
    geom_jitter(data = raw, aes(x = rev(monovsmix), y = feed/30, group = species, fill = species, shape = species), 
                position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.5), alpha = 0.75, color = "black") +
    theme_bw() + theme(legend.position = "none",
                       legend.title = element_blank(),
                       panel.grid = element_blank(),
                       axis.text = element_text(color = "black", size = 10)) +
    ylab("Bites per minute (± CIs)") +
    xlab("") +
    scale_color_manual(values = c(gobycols)) +
    scale_fill_manual(values = c(gobycols)) +
    scale_shape_manual(values = c(21, 23)) +
    scale_y_log10(limits = c(0.01,100)) +
    scale_x_discrete(labels = c("Single-species", "Mixed-species")) +
    add_fishape(family = "Gobiidae",
                option = "Fusigobius_neophytus", 
                xmin = 0.75, xmax = 0.975, ymin = 1.25,
                fill = gobycols[1], alpha = 0.75) +
    add_fishape(family = "Gobiidae",
                option = "Fusigobius_neophytus", 
                xmin = 0.75, xmax = 0.975, ymin = 1.86,
                fill = gobycols[1], alpha = 0.75) +
    add_fishape(family = "Gobiidae",
                option = "Gnatholepis_cauerensis", 
                xmin = 1.025, xmax = 1.25, ymin = 1.25,
                fill = gobycols[2], alpha = 0.75) +
    add_fishape(family = "Gobiidae",
                option = "Gnatholepis_cauerensis", 
                xmin = 1.025, xmax = 1.25, ymin = 1.86,
                fill = gobycols[2], alpha = 0.75) +
    add_fishape(family = "Gobiidae",
                option = "Fusigobius_neophytus", 
                xmin = 1.75, xmax = 1.975, ymin = 1.25,
                fill = gobycols[1], alpha = 0.75) +
    add_fishape(family = "Gobiidae",
                option = "Gnatholepis_cauerensis", 
                xmin = 1.75, xmax = 1.975, ymin = 1.86,
                fill = gobycols[2], alpha = 0.75) +
    add_fishape(family = "Gobiidae",
                option = "Gnatholepis_cauerensis", 
                xmin = 2.025, xmax = 2.25, ymin = 1.25,
                fill = gobycols[2], alpha = 0.75) +
    add_fishape(family = "Gobiidae",
                option = "Fusigobius_neophytus", 
                xmin = 2.025, xmax = 2.25, ymin = 1.86,
                fill = gobycols[1], alpha = 0.75)
  ggsave("output/plots/Figure4_Brandl_Gobies.png", behavior.plot, width = 4, height = 5)
  return(behavior.plot)
}


  
