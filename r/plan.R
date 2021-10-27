plan <- drake_plan(
  
  #########################
  #########################
  #### 1. COOCCURRENCE ####
  #########################
  #########################
  

  #### 1A. MAP ####

  
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
  

  #### 1B. JSDM ####

  
  # prepare data to be run in jSDM
  input.data.jsdm = data_prep_jsdm(commdat, site.helper),
  
  # run jSDM
  goby.jsdm = run_jsdm(input.data.jsdm[c(3,4)], input.data.jsdm[1]),
  
  # get predicted values 
  # prepare two helper vectors, id_sites and id_species
  id_sites = 1:50,
  id_species = sample(colnames(input.data.jsdm[c(3,4)]), 2),
  
  # predict from jsdm
  goby.predicted.theta = get_predicted_thetas(goby.jsdm, id_species, id_sites),
  
  # predictions for F. neophytus
  pred.fneo = get_pred_sp1(goby.predicted.theta),
  
  # prediction for G. cauerensis, combined with F. neophytus
  pred.complete = get_pred_sp2_comb(goby.predicted.theta, pred.fneo, input.data.jsdm),
  
  # figure of jSDM predicted values
  output.fig1b = plot_jsdm_pred_compl(as.data.frame(pred.complete)),
  
  # combine A and B
  ouput.fig1ab = comb_figs(output.fig1a, output.fig1b),
  

  #######################
  #######################
  #### 2. PHYSIOLOGY ####
  #######################
  #######################
  
  # load raw data for physiology
  respodat = read.csv(file = "data/respirometry_gnatholepis_fusigobius.csv"),
  
  # clean data from respirometry trials
  respdat.clean = clean_resp_dat(respodat),
  
  #### SMR ####
  
  # set prior
  smr.prior = get_smr_prior("SMR", respdat.clean),
  
  # run model
  smr.brms = run_brms_smr("SMR", respdat.clean, smr.prior),
  
  # write summary table of smr.brms as html
  output.supptab1 = write_sum_tab(smr.brms, "SuppTable1.doc"),
  
  # predict from brms_mod2 using generalizable function for mass = 1g
  smr.mod.pred.1g = predict_from_brms_phys(respdat.clean,
                                        W = rep(1, n = 1000), TempC = mean(respdat.clean$TempC),
                                        mod = smr.brms, 
                                        draws = 1000, 
                                        backtrans = T, transform = "e10"),
  
  # get summary of predictions for in-text infromation
  smr.pred.sum = predict_summary_phys(smr.mod.pred.1g, SMR),
  
  # predict for full size range
  smr.mod.pred.full = predict_from_brms_phys(respdat.clean,
                                             W = seq_range(W, n = 1000), TempC = mean(respdat.clean$TempC),
                                             mod = smr.brms, 
                                             draws = 1000, 
                                             backtrans = T, transform = "e10"),
  
  # plot predictions from SMR model (Fig 2A)
  output.fig2a = plot_smr_preds(respdat.clean, "SMR", smr.mod.pred.full),
  
  # predict for average size 
  smr.mod.pred.avg = predict_from_brms_phys(respdat.clean,
                                             W = mean(respdat.clean$W), TempC = mean(respdat.clean$TempC),
                                             mod = smr.brms, 
                                             draws = 1000, 
                                             backtrans = T, transform = "e10"),
  
  # functions to get partial densities
  p.funs = partial_density(intervals = c(0.025, 0.5, 0.975)),
  
  # calculate densities for both species
  density.calcs = get_partial_densities(smr.mod.pred.avg),
  
  # calculate summary values for lines in plot
  density.sums = get_partial_density_sums(smr.mod.pred.avg, p.funs),
  
  # plot density curves for average weight 
  output.fig2b = plot_density_avg_weight_smr(density.calcs, density.sums),
  
  
  #### MMR ####
  
  # set prior
  mmr.prior = get_smr_prior("MaxMR", respdat.clean),
  
  # run model
  mmr.brms = run_brms_smr("MaxMR", respdat.clean, mmr.prior),
  
  # write summary table of smr.brms as html
  output.supptab2 = write_sum_tab(mmr.brms, "SuppTable2.doc"),
  
  # predict from brms_mod2 using generalizable function for mass = 1g
  mmr.mod.pred.1g = predict_from_brms_phys(respdat.clean,
                                           W = rep(1, n = 1000), TempC = mean(respdat.clean$TempC),
                                           mod = mmr.brms, 
                                           draws = 1000, 
                                           backtrans = T, transform = "e10"),
  
  # get summary of predictions for in-text infromation
  mmr.pred.sum = predict_summary_phys(mmr.mod.pred.1g, MaxMR),
  
  # predict for full size range
  mmr.mod.pred.full = predict_from_brms_phys(respdat.clean,
                                             W = seq_range(W, n = 1000), TempC = mean(respdat.clean$TempC),
                                             mod = mmr.brms, 
                                             draws = 1000, 
                                             backtrans = T, transform = "e10"),
  
  # plot predictions from SMR model (Fig 2A)
  output.fig2c = plot_mmr_preds(respdat.clean, "MaxMR", mmr.mod.pred.full),
  
  # predict for average size 
  mmr.mod.pred.avg = predict_from_brms_phys(respdat.clean,
                                            W = mean(respdat.clean$W), TempC = mean(respdat.clean$TempC),
                                            mod = mmr.brms, 
                                            draws = 1000, 
                                            backtrans = T, transform = "e10"),
  
  
  # calculate densities for both species
  density.calcs.mmr = get_partial_densities(mmr.mod.pred.avg),
  
  # calculate summary values for lines in plot
  density.sums.mmr = get_partial_density_sums(mmr.mod.pred.avg, p.funs),
  
  # plot density curves for average weight 
  output.fig2d = plot_density_avg_weight_mmr(density.calcs.mmr, density.sums.mmr),
  
  # combine A and B
  output.fig2abcd = comb_figs2(output.fig2a, output.fig2b, output.fig2c, output.fig2d),
  
#### MMR ####
  
  # set prior
  aer.prior = get_smr_prior("AerSco", respdat.clean),
  
  # run model
  aer.brms = run_brms_smr("AerSco", respdat.clean, aer.prior),
  
  # predict from brms_mod2 using generalizable function for mass = 1g
  aer.mod.pred.1g = predict_from_brms_phys(respdat.clean,
                                           W = rep(1, n = 1000), TempC = mean(respdat.clean$TempC),
                                           mod = aer.brms, 
                                           draws = 1000, 
                                           backtrans = T, transform = "e10"),
  
  # get summary of predictions for in-text infromation
  aer.pred.sum = predict_summary_phys(aer.mod.pred.1g, MaxMR),
  
  # predict for full size range
  aer.mod.pred.full = predict_from_brms_phys(respdat.clean,
                                             W = seq_range(W, n = 1000), TempC = mean(respdat.clean$TempC),
                                             mod = aer.brms, 
                                             draws = 1000, 
                                             backtrans = T, transform = "e10"),
  
  
  #######################
  #######################
  #### 3. MORPHOLOGY ####
  #######################
  #######################
  
  #load raw data for external morphology and gut
  ext.morpho = read.csv(file ="data/morphology_mouth.csv"),
  gut.morpho = read.csv(file = "data/morphology_gut.csv"),
  
  # clean data
  ext.morpho.clean = clean_morphology(ext.morpho),
  
  # run hgape model
  hgape.brms = run_brms_morpho("H_gape", ext.morpho.clean),
  
  # predict hgape model
  hgape.pred = predict_from_brms_morph(ext.morpho.clean,
                                       SL = seq_range(SL, n = 1000),
                                       mod = hgape.brms, 
                                       draws = 1000, 
                                       backtrans = F),
  
  # write summary table as html
  output.supptab3 = write_sum_tab(hgape.brms, "SuppTable3.doc"),
  
  # run vgape model
  vgape.brms = run_brms_morpho("V_gape", ext.morpho.clean),
  
  # predict hgape model
  vgape.pred = predict_from_brms_morph(ext.morpho.clean,
                                       SL = seq_range(SL, n = 1000),
                                       mod = vgape.brms, 
                                       draws = 1000, 
                                       backtrans = F),
  
  # write summary table as html
  output.supptab4 = write_sum_tab(vgape.brms, "SuppTable4.doc"),
  
  # run girth model
  girth.brms = run_brms_morpho("Girth", ext.morpho.clean),
  
  # predict girth model
  girth.pred = predict_from_brms_morph(ext.morpho.clean,
                                       SL = seq_range(SL, n = 1000),
                                       mod = girth.brms, 
                                       draws = 1000, 
                                       backtrans = F),
  
  # write summary table as html
  output.supptab5 = write_sum_tab(girth.brms, "SuppTable5.doc"),
  
  # run gut model
  gut.brms = run_brms_morpho("GIT", gut.morpho),
  
  # predict hgape model
  gut.pred = predict_from_brms_morph(gut.morpho,
                                       SL = seq_range(SL, n = 1000),
                                       mod = gut.brms, 
                                       draws = 1000, 
                                       backtrans = F),
  # write summary table as html
  output.supptab6 = write_sum_tab(gut.brms, "SuppTable6.doc"),
  
  
  # plot horizontal gape
  output.figs1a = morpho_plot(ext.morpho.clean, "H_gape", hgape.pred, "Horizontal gape (mm)"),
  
  # plot vertical gape
  output.figs1b = morpho_plot(ext.morpho.clean, "V_gape", vgape.pred, "Vertical gape (mm)"),
  
  # plot horizontal gape
  output.figs1c = morpho_plot(ext.morpho.clean, "Girth", girth.pred, "Girth (mm)"),
  
  # plot horizontal gape
  output.figs1d = morpho_plot(gut.morpho, "GIT", gut.pred, "GIT length (mm)"),
  
  # combine figure for supplemental figure S1
  output.figS1abcd = comb_figsS1(output.figs1a, output.figs1b, output.figs1c, output.figs1d),
  
  #########################
  #########################
  #### 4. GUT COMTENTS ####
  #########################
  #########################
  
  # load datasets
  # Goby_Metadata.csv = metadata for extractions
  # COI_Gobies.csv = COI metabarcoding data
  # 23S_Gobies.csv = 23S metabarcoding data
  gobies.meta = read.csv(file = "data/Goby_Metadata.csv"),
  gobies.coi = read.csv(file = "data/COI_Gobies.csv"), 
  gobies.23s = read.csv(file = "data/23S_Gobies.csv"),
  
  # clean metadata
  clean.metadata.all = clean_meta(gobies.meta),
  
  # clean COI & 23 S data
  clean.coi.all = clean_sequence_data(gobies.coi, 11, 46, "genus", c("Gnatholepis", "Fusigobius")),
  clean.23s.all = clean_sequence_data(gobies.23s, 10, 45, "Genus", c("Gnatholepis", "Fusigobius")),
  
  
  # calculate presence/absence of sequences
  pa.coi = widen_sequence_data(clean.coi, id.pos = 1, begin.col = 11, end.col = 46, compute = T, metric = "pa"),
  pa.23s = widen_sequence_data(clean.23s, id.pos = 1, begin.col = 10, end.col = 45, compute = T, metric = "pa"),
  
  ##############################
  #### 3A. NICHES & NETWORK ####
  ##############################
  
  # calculate relative read abundance of sequences
  rra.coi = widen_sequence_data(clean.coi, id.pos = 1, begin.col = 11, end.col = 46, compute = T, metric = "rra"),
  rra.23s = widen_sequence_data(clean.23s, id.pos = 1, begin.col = 10, end.col = 45, compute = T, metric = "rra"),
  
  # caluclate species-level average of sequences
  rra.coi.species = sum_species_comp(rra.coi, clean.metadata),
  rra.23s.species = sum_species_comp(rra.23s, clean.metadata),
  
  # run niche model with Pianka index and ra3
  rra.coi.niches = run_niche_model(rra.coi.species),
  rra.23s.niches = run_niche_model(rra.23s.species),
  
  # bring PA data into long format for network
  pa.coi.long = lengthen_seq_dat(pa.coi, clean.metadata),
  pa.23s.long = lengthen_seq_dat(pa.23s, clean.metadata),
  
  # prepare the two datasets for merging
  pa.coi.prepped = prepare_coi(pa.coi.long),
  pa.23s.prepped = prepare_23s(pa.23s.long),
  
  # merge datasets for network analysis
  prim.comb.network = combine_for_network(pa.coi.prepped, pa.23s.prepped),
  
  # run network analysis & collect output
  # goby.network.modules = computeModules(prim.comb.network[-1], method = "Beckett", forceLPA = FALSE),
  # only run the line below if you have lots of time. Takes ~6hrs on a Macbook Pro 2.6 GHz Intel Core i7
  # output is the same as line above
  goby.network.modules = metaComputeModules(prim.comb.network[-1], method = "Beckett", N = 50, forceLPA = FALSE),
  
  modules = as.data.frame(goby.network.modules@modules[-1, -c(1,2) ]),

  # integrate module dataset
  module.membership = clean_modularity(modules, prim.comb.network, clean.metadata),
  output.modules = write.csv(module.membership, file = "output/data/module_membership.csv", row.names = FALSE),
  
  # combine modularity with network input
  network.tree.modules = comb_module_network(pa.coi.prepped, pa.23s.prepped, module.membership),
  
  # plot network tree
  output.fig3c = make_network_tree(network.tree.modules),
  
  
  #########################
  #### 3B. RAREFACTION ####
  #########################
  
  # get number of sequences wide
  nbseq.coi = widen_sequence_data(clean.coi, id.pos = 1, begin.col = 11, end.col = 46, compute = F),
  nbseq.23s = widen_sequence_data(clean.23s, id.pos = 1, begin.col = 10, end.col = 45, compute = F),
  
  # turn into rarefactor format 
  nbseq.genspe.coi = wide_to_rare(nbseq.coi, clean.metadata),
  nbseq.genspe.23s = wide_to_rare(nbseq.23s, clean.metadata),
  
  # turn into lists 
  nbseq.list.coi = tibble_to_list(nbseq.genspe.coi),
  nbseq.list.23s = tibble_to_list(nbseq.genspe.23s),
  
  # run rarefaction and fortify
  rarefaction.fort.coi = rarify_to_plot(nbseq.list.coi, nbseq.genspe.coi, clean.metadata),
  rarefaction.fort.23s = rarify_to_plot(nbseq.list.23s, nbseq.genspe.23s, clean.metadata),
  
  # create plot for coi rarefaction curves
  output.fig3a = plot_rarefaction_curves(rarefaction.fort.coi),
  output.fig3b = plot_rarefaction_curves(rarefaction.fort.23s),
  
  # Figure 3
  output.fig3abc = comb_figs3(output.fig3a, output.fig3b, output.fig3c),
  
  # create data for Figure S2
  prey.taxa = plot_prey_rra(gobies.23s, rra.23s, clean.metadata),
  
  # plot Figure S2
  output.figs2 = plot_prey_taxa(prey.taxa),
  
  #####################
  #####################
  #### 5. BEHAVIOR ####
  #####################
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
  output_supptab7 = write_sum_tab(behavior_brms_mod2, "SuppTable7.doc"),
  
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


