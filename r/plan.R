plan <- drake_plan(
  
  #########################
  #### 1. COOCCURRENCE ####
  #########################
  
  #################
  #### 1A. MAP ####
  #################
  
  # load raw data for community composition
  commdat = read.csv(file = "data/moorea_communities_allyears.csv"),
  
  # create helper vector that contains all combinations of site x species
  site.helper = helper_cooc(commdat),
  
  # tidy up data for map
  cooc.map = clean_cooc_dat(commdat, site.helper),
  
  # load shapefile of Moorea
  moorea.shape = rgdal::readOGR("data/coastline.shx"),
  
  # tidy up shapefile
  moorea.tidy = tidy_shape(moorea.shape),
  
  # turn into WGS and fortify for ggplot
  moorea.wgs.fortified = fortify_wgs_shape(moorea.tidy),
  
  # plot map with abundances of the two gobies
  output.fig1a = create_moorea_map(moorea.wgs.fortified, cooc.map),
  
  ##################
  #### 1B. JSDM ####
  ##################
  
  # prepare data to be run in jSDM
  input.data.jsdm = data_prep_jsdm(commdat, site.helper),
  
  # run jSDM
  goby.jsdm = run_jsdm(input.data.jsdm[c(3,4)], input.data.jsdm[1]),
  
  # get predicted values 
  # prepare two helper vectors
  id_sites = 1:50,
  id_species = sample(colnames(input.data.jsdm[c(3,4)]), 2),
  goby.predicted.theta = get_predicted_thetas(goby.jsdm, id_species, id_sites),
  pred.fneo = get_pred_sp1(goby.predicted.theta),
  pred.complete = get_pred_sp2_comb(goby.predicted.theta, pred.fneo, input.data.jsdm),
  output.fig1b = plot_jsdm_pred_compl(pred.complete),

  
  #####################
  #### 4. BEHAVIOR ####
  #####################
  
  # load raw data for aquarium trials
  aquarium.trials = read.csv(file = "data/feeding_trials.csv"),
  
  # clean data from aquarium trials
  behav.data.clean = clean_aq_dat(aquarium.trials),
  
  # run brms model with "video" as a random effect
  behavior_brms_mod1 = run_brms_mod1(behav.data.clean),
  
  # run brms model without random effect
  behavior_brms_mod2 = run_brms_mod2(behav.data.clean),
  
  # compare the two models using LOO
  loo.res = loo(behavior_brms_mod1, behavior_brms_mod2),
  
  # write summary table of brms_mod2 as html
  output_supptab7 = write_sum_tab(behavior_brms_mod2, "SuppTable7.html"),
  
  # predict from brms_mod2 using generalizable function
  behavior.mod.pred = predict_from_brms(behav.data.clean, 
                                        species, monovsmix, 
                                        mod = behavior_brms_mod2, 
                                        draws = 1000, 
                                        backtrans = T, 
                                        transform = "div30"),
  
  # get summary of predictions for in-text infromation
  behavior.mod.pred.sum = predict_summary(behavior.mod.pred),
  
  # plot predictions and raw data
  output_fig4 = create_behavior_plot(behavior.mod.pred, behav.data.clean)
  
  # done!
)


