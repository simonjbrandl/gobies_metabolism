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
ggplot() +
  geom_polygon(data = geo, aes( x = long, y = lat, group = group), 
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
    habitat = raw[1])
}

# function: plot jSDM predictions
plot_jsdm_pred_compl <- function(predictions){
  pred.post.plot <- ggplot(predictions, aes(x = pres_FUSINEO.mean, y = pres_GNASTCAUE.mean)) +
    geom_density_2d(color = "black", lty = 2) +
    geom_jitter(aes(fill = habitat, shape = habitat), width = 0.05, height = 0.05, size = 3, alpha = 0.75) +
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
}


#### 4. BEHAVIOR ####

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
  return()
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


  
