#set colors for paper
gobycols <- fish(2, option = "Pomacanthus_imperator")

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


  
