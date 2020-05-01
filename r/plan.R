plan <- drake_plan(
  
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


