#### SETUP & GENERAL FUNCTIONS ####

#set colors for paper
gobycols <- fish(2, option = "Pomacanthus_imperator")

#########################
#### 1. COOCCURRENCE ####
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
  
  ggsave("output/plots/Figure1a_Brandl_Gobies.png", Fig1A, width = 4, height = 5)
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
  
  ggsave("output/plots/Figure1b_Brandl_Gobies.png", Fig1B, width = 4, height = 5)
  return(Fig1B)
}

# function: combine Fig 1A and 1B
comb_figs <- function(f1, f2){
  Figure1_Brandletal_Gobies <- f1 | f2 +
    plot_annotation(tag_levels = 'A')

  ggsave("output/plots/Figure1_Brandl_Gobies.png", Figure1_Brandletal_Gobies, width = 10, height = 5)
  
}


#######################
#### 2. PHYSIOLOGY ####
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
               color = "black", size = 2, alpha = 0.9) +
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
               color = "black", size = 2, alpha = 0.9) +
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
  Figure2_Brandletal_Gobies <- (f1 | f2) / (f3 | f4) +
    plot_annotation(tag_levels = 'A')
  
  return(Figure2_Brandletal_Gobies)

  ggsave("output/plots/Figure2_Brandl_Gobies.png", Figure2_Brandletal_Gobies, width = 11, height = 8)

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



#####################
#### 4. BEHAVIOR ####
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

# function: write summary table to html
write_sum_tab <- function(model, output){
  out <- paste0("output/tables/", output)
  tab_model(model, file = out)
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


  
