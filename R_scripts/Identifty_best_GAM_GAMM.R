
# Identify the best GAM or GAMM per variable per site ------------------------------------------------

# set the working directory to the folder containing the RDS files produced when running the script "Automated_Generalized_Additive_Model_GAM_GAMM.R"

# create the function to create figures to explore the model fit and results
GAM_figures = function(model_name, model){
  layout(matrix(1:16, ncol = 4))
  if (grepl('_GAM$', model_name) == TRUE) {
    # for GAM
    plot(model, seWithMean = TRUE, shift = coef(model)[1]) # , pages = 1, all.terms = TRUE # pages gets both graphs on one page, and all.terms displays partial effects along with the smooth plots
    plot(model, residuals = TRUE, pch = 1, cex = 1) # The residuals argument puts partial residuals on the plots. Partial residuals are the difference between the partial effect and the data, after all other partial effects have been accounted for.
    acf(residuals(model))
    pacf(residuals(model))
  } else {
    # for GAMM
    plot(model$gam, scale = 0, seWithMean = TRUE, shift = coef(model$gam)[1])#, all.terms = TRUE
    plot(model$gam, scale = 0, residuals=T, cex = 1)
    acf(residuals(model$lme, type = "normalized")) # need to look at the redisuals
    pacf(residuals(model$lme, type = "normalized"))
  }
  layout(1)
}

dev.off()

library("dplyr")
gam_results <- list.files(pattern = "Auk_soil_chem_int_") %>% #select the pattern for the file or multiple files that you wish to import (e.g. "Auk" for all files for the site Aukstaitija Integrated Monitoring Station). Use the symbol "|" for either or statement
  purrr::set_names() %>%
  purrr::map(readRDS) 


site_parameter = gam_results$Auk_soil_chem_int_soil_temp #select the name of a single file (e.g. "Auk_soil_chem_int_soil_temp" for the soil temperature gam/gamm results at Aukstaitija)
mgcv::gam.check(site_parameter[['mod0_GAM']])

# explore the anova comparison between the gam/gamm results
site_parameter[['GAM_anova_comparison']]

appraise(, mothod = "simulate")

# evaluate the model fit
GAM_figures('mod0_GAM', site_parameter[['mod0_GAM']]) 
GAM_figures('mod0_GAMM', site_parameter[['mod0_GAMM']])
GAM_figures('mod1', site_parameter[['mod1']])
GAM_figures('mod2', site_parameter[['mod2']])
GAM_figures('mod3', site_parameter[['mod3']])
GAM_figures('mod4', site_parameter[['mod4']])
GAM_figures('mod5', site_parameter[['mod5']])
GAM_figures('mod6', site_parameter[['mod6']])
GAM_figures('mod7', site_parameter[['mod7']])

# directly compare the results of certain models
anova(site_parameter[['mod0_GAMM']]$lme, site_parameter[['mod1']]$lme)
anova(site_parameter[['mod1']]$lme, site_parameter[['mod4']]$lme)

# get the summary results of each model
summary(site_parameter[['mod0_GAM']])
summary(site_parameter[['mod0_GAMM']]$gam)
summary(site_parameter[['mod1']]$gam) 
summary(site_parameter[['mod2']]$gam)
summary(site_parameter[['mod3']]$gam) 
summary(site_parameter[['mod4']]$gam) 
summary(site_parameter[['mod5']]$gam) 
summary(site_parameter[['mod6']]$gam) 
summary(site_parameter[['mod7']]$gam) 


### best models ####
# after identifying the best model, further explore the results using the "check" funciton in the mgcViz package

# if the best model is the GAM it should be writen as such: 
winner = gam_results$Auk_soil_chem_int_soil_temp[['mod0_GAM']]
# if the best model is a GAMM it should be writen as such: 
winner = gam_results$Auk_soil_chem_int_soil_temp[['mod2']]$gam


#https://mfasiolo.github.io/mgcViz/articles/mgcviz.html
library("mgcViz")
check(getViz(winner), a.qq = list(method = "tnorm",  a.cipoly = list(fill = "light blue")),  a.respoi = list(size = 0.5),  a.hist = list(bins = 10))
print(plot(getViz(winner), allTerms = T), pages = 1)
plot(winner, pages = 1, all.terms = TRUE, rug = TRUE, residuals = TRUE, pch = 1, cex = 1, shade = TRUE, seWithMean = TRUE, shift = coef(winner)[1])

# repeate this process for each variable at each site



### create a dataframe with the best models for each variable at a site ####
# for example:
best_models_Ali <- list()
best_models_Ali[["Ali_soil_chem_int_doc_mod4"]] = gam_results$Ali_soil_chem_int_doc[['mod4']] 
best_models_Ali[["Ali_soil_chem_int_n_nh4_mod1"]] = gam_results$Ali_soil_chem_int_n_nh4[['mod1']] 
best_models_Ali[["Ali_soil_chem_int_n_no3_mod1"]] = gam_results$Ali_soil_chem_int_n_no3[['mod1']] 
best_models_Ali[["Ali_soil_chem_int_soil_temp_mod4"]] = gam_results$Ali_soil_chem_int_soil_temp[['mod4']]
best_models_Ali[["Ali_soil_chem_swc_mod5"]] = gam_results$Ali_soil_chem_swc[['mod5']]
saveRDS(best_models_Ali, file = "best_models_Ali")

best_models_Auk <- list()
best_models_Auk[["Auk_soil_chem_int_n_nh4_mod0_GAMM"]] = gam_results$Auk_soil_chem_int_n_nh4[['mod0_GAMM']]
best_models_Auk[["Auk_soil_chem_int_n_no3_mod0_GAMM"]] = gam_results$Auk_soil_chem_int_n_no3[['mod0_GAMM']] 
best_models_Auk[["Auk_soil_chem_int_soil_temp_5cm_10cm_20cm_mod5"]] = gam_results$Auk_soil_chem_int_soil_temp_5cm_10cm_20cm[['mod5']] 
best_models_Auk[["Auk_soil_chem_int__soil_temp_undixieme_mod2"]] = gam_results$Auk_soil_chem_int__soil_temp_undixieme[['mod2']]
best_models_Auk[["Auk_soil_chem_int_soil_temp_mod2"]] = gam_results$Auk_soil_chem_int_soil_temp[['mod2']]
best_models_Auk[["Auk_soil_chem_int_swc_20251128_mod4"]] = gam_results$Auk_soil_chem_int_swc[['mod4']]
saveRDS(best_models_Auk, file = "best_models_Auk")

best_models_Bra <- list()
best_models_Bra[["Bra_soil_chem_int_doc_mod1"]] = gam_results$Bra_soil_chem_int_doc[['mod1']] 
best_models_Bra[["Bra_soil_chem_doc_10cm_mod2"]] = gam_results$Bra_soil_chem_doc_10cm[['mod2']]
best_models_Bra[["Bra_soil_chem_int_n_nh4_depth0cm_10cm_mod1"]] = gam_results$Bra_soil_chem_int_n_nh4_depth0cm_10cm[['mod1']] 
best_models_Bra[["Bra_soil_chem_n_nh4_0cm_mod2"]] = gam_results$Bra_soil_chem_n_nh4_0cm[['mod2']]
best_models_Bra[["Bra_soil_chem_n_nh4_50cm_mod2"]] = gam_results$Bra_soil_chem_n_nh4_50cm[['mod2']]
best_models_Bra[["Bra_soil_chem_int_n_nh4_depth50cm_70cm_mod1"]] = gam_results$Bra_soil_chem_int_n_nh4_depth50cm_70cm[['mod1']] 
best_models_Bra[["Bra_soil_chem_int_n_no3_depth0cm_10cm_mod1"]] = gam_results$Bra_soil_chem_int_n_no3_depth0cm_10cm[['mod1']]  
best_models_Bra[["Bra_soil_chem_n_no3_0cm_mod2"]] = gam_results$Bra_soil_chem_n_no3_0cm[['mod2']]
best_models_Bra[["Bra_soil_chem_int_n_no3_depth50cm_70cm_mod1"]] = gam_results$Bra_soil_chem_int_n_no3_depth50cm_70cm[['mod1']]  
best_models_Bra[["Bra_soil_chem_int_soil_temp_depth2cm_5cm_mod4"]] = gam_results$Bra_soil_chem_int_soil_temp_depth2cm_5cm[['mod4']]  
best_models_Bra[["Bra_soil_chem_int_soil_temp_depth2_5cm_12_5cm_30cm_60cm_mod1"]] = gam_results$Bra_soil_chem_int_soil_temp_depth2_5cm_12_5cm_30cm_60cm[['mod1']] 
best_models_Bra[["Bra_soil_chem_int_swc_mod3"]] = gam_results$Bra_soil_chem_int_swc[['mod3']]
saveRDS(best_models_Bra, file = "best_models_Bra")

best_models_Cai <- list()
best_models_Cai[["Cai_soil_chem_int_n_nh4_mod1"]] = gam_results$Cai_soil_chem_int_n_nh4[['mod1']]
best_models_Cai[["Cai_soil_chem_int_n_no3_mod1"]] = gam_results$Cai_soil_chem_int_n_no3[['mod1']] 
best_models_Cai[["Cai_soil_chem_n_no3_50cm_mod1"]] = gam_results$Cai_soil_chem_n_no3_50cm[['mod1']]
best_models_Cai[["Cai_soil_chem_int_soil_temp_mod4"]] = gam_results$Cai_soil_chem_int_soil_temp[['mod4']] 
best_models_Cai[["Cai_soil_chem_swc_mod1"]] = gam_results$Cai_soil_chem_swc[['mod1']] 
saveRDS(best_models_Cai, file = "best_models_Cai")

best_models_Col <- list()
best_models_Col[["Col_soil_chem_int_doc_mod1"]] = gam_results$Col_soil_chem_int_doc[['mod1']] 
best_models_Col[["Col_soil_chem_doc_0cm_mod2"]] = gam_results$Col_soil_chem_doc_0cm[['mod2']]
best_models_Col[["Col_soil_chem_doc_55cm_mod2"]] = gam_results$Col_soil_chem_doc_55cm[['mod2']]
best_models_Col[["Col_soil_chem_int_n_nh4_depth0cm_15cm_25cm_mod1"]] = gam_results$Col_soil_chem_int_n_nh4_depth0cm_15cm_25cm[['mod1']] 
best_models_Col[["Col_soil_chem_n_nh4_0cm_mod2"]] = gam_results$Col_soil_chem_n_nh4_0cm[['mod2']]
best_models_Col[["Col_soil_chem_int_n_nh4_depth55cm_90cm_mod1"]] = gam_results$Col_soil_chem_int_n_nh4_depth55cm_90cm[['mod1']]
best_models_Col[["Col_soil_chem_int_n_no3_0cm_15cm_25cm_mod1"]] = gam_results$Col_soil_chem_int_n_no3_0cm_15cm_25cm[['mod1']]
best_models_Col[["Col_soil_chem_int_n_no3_15cm_25cm_mod1"]] = gam_results$Col_soil_chem_int_n_no3_15cm_25cm[['mod1']]
best_models_Col[["Col_soil_chem_int_n_no3_55cm_90cm_mod1"]] = gam_results$Col_soil_chem_int_n_no3_55cm_90cm[['mod1']]
best_models_Col[["Col_soil_chem_int_soil_temp_mod5"]] = gam_results$Col_soil_chem_int_soil_temp[['mod5']]
best_models_Col[["Col_soil_chem_soil_temp_20cm_mod2"]] = gam_results$Col_soil_chem_soil_temp_20cm[['mod2']] 
best_models_Col[["Col_soil_chem_int_swc_mod4"]] = gam_results$Col_soil_chem_int_swc[['mod4']]
best_models_Col[["Col_soil_chem_swc_2cm_mod2"]] = gam_results$Col_soil_chem_swc_2cm[['mod2']]
saveRDS(best_models_Col, file = "best_models_Col")

best_models_Dav <- list()
best_models_Dav[["Dav_soil_chem_int_doc_mod0_GAM"]] = gam_results$Dav_soil_chem_int_doc[['mod0_GAM']]
best_models_Dav[["Dav_soil_chem_int_n_nh4_mod0_GAM"]] = gam_results$Dav_soil_chem_int_n_nh4[['mod0_GAM']]
best_models_Dav[["Dav_soil_chem_int_n_no3_depth0cm_15cm_50cm_mod0_GAM"]] = gam_results$Dav_soil_chem_int_n_no3_depth0cm_15cm_50cm[['mod0_GAM']] 
best_models_Dav[["Dav_soil_chem_n_no3_depth80cm_mod0_GAM"]] = gam_results$Dav_soil_chem_n_no3_depth80cm[['mod0_GAM']] 
best_models_Dav[["Dav_soil_chem_int_soil_temp_20250506_mod0_GAMM"]] = gam_results$Dav_soil_chem_int_soil_temp_20250506[['mod0_GAMM']] 
best_models_Dav[["Dav_soil_chem_int_swc_20250506_mod1"]] = gam_results$Dav_soil_chem_int_swc_20250506[['mod1']] 
saveRDS(best_models_Dav, file = "best_models_Dav_20250506")

best_models_Lae <- list()
best_models_Lae[["Lae_soil_chem_int_doc_mod0_GAM"]] = gam_results$Lae_soil_chem_int_doc[['mod0_GAM']] 
best_models_Lae[["Lae_soil_chem_doc_15cm_mod0_GAM"]] = gam_results$Lae_soil_chem_doc_15cm[['mod0_GAM']]
best_models_Lae[["Lae_soil_chem_doc_50cm_mod0_GAM"]] = gam_results$Lae_soil_chem_doc_50cm[['mod0_GAM']]
best_models_Lae[["Lae_soil_chem_doc_80cm_mod0_GAM"]] = gam_results$Lae_soil_chem_doc_80cm[['mod0_GAM']]
best_models_Lae[["Lae_soil_chem_int_n_nh4_mod0_GAM"]] = gam_results$Lae_soil_chem_int_n_nh4[['mod0_GAM']] 
best_models_Lae[["Lae_soil_chem_int_n_no3_mod0_GAM"]] = gam_results$Lae_soil_chem_int_n_no3[['mod0_GAM']] 
best_models_Lae[["Lae_soil_chem_int_soil_temp_20250506_mod1"]] = gam_results$Lae_soil_chem_int_soil_temp_20250506[['mod1']]  
best_models_Lae[["Lae_soil_chem_int_swc_20250506_mod1"]] = gam_results$Lae_soil_chem_int_swc_20250506[['mod1']]  
saveRDS(best_models_Lae, file = "best_models_Lae_20250506")

best_models_Moo <- list()
best_models_Moo[["Moo_soil_chem_int_doc_mod4"]] = gam_results$Moo_soil_chem_int_doc[['mod4']]
best_models_Moo[["Moo_soil_chem_int_n_nh4_mod1"]] = gam_results$Moo_soil_chem_int_n_nh4[['mod1']]
best_models_Moo[["Moo_soil_chem_int_n_no3_mod1"]] = gam_results$Moo_soil_chem_int_n_no3[['mod1']]
best_models_Moo[["Moo_soil_chem_int_soil_temp_mod4"]] = gam_results$Moo_soil_chem_int_soil_temp[['mod4']]
saveRDS(best_models_Moo, file = "best_models_Moo")

best_models_Wyt <- list()
best_models_Wyt[["Wyt_soil_chem_int_doc_mod4"]] = gam_results$Wyt_soil_chem_int_doc[['mod4']]
best_models_Wyt[["Wyt_soil_chem_int_n_nh4_mod4"]] = gam_results$Wyt_soil_chem_int_n_nh4[['mod4']]
best_models_Wyt[["Wyt_soil_chem_int_n_no3_mod1"]] = gam_results$Wyt_soil_chem_int_n_no3[['mod1']]
best_models_Wyt[["Wyt_soil_chem_int_soil_temp_mod4"]] = gam_results$Wyt_soil_chem_int_soil_temp[['mod4']]
best_models_Wyt[["Wyt_soil_chem_swc_mod2"]] = gam_results$Wyt_soil_chem_swc[['mod2']]
saveRDS(best_models_Wyt, file = "best_models_Wyt")

best_models_ZOE_IP123 <- list()
best_models_ZOE_IP123[["LTE_soil_chem_int_doc_5cm_6cm_10cm_IP2_mod4"]] = gam_results$LTE_soil_chem_int_doc_5cm_6cm_10cm_IP2[['mod4']] 
best_models_ZOE_IP123[["LTE_soil_chem_int_n_nh4_5cm_6cm_10cm_IP2_mod4"]] = gam_results$LTE_soil_chem_int_n_nh4_5cm_6cm_10cm_IP2[['mod4']] 
best_models_ZOE_IP123[["LTE_soil_chem_int_n_no3_5cm_6cm_10cm_IP2_mod4"]] = gam_results$LTE_soil_chem_int_n_no3_5cm_6cm_10cm_IP2[['mod4']]  
best_models_ZOE_IP123[["LTE_soil_chem_int_soil_temp_IP2_20250506_mod2"]] = gam_results$LTE_soil_chem_int_soil_temp_IP2_20250506[['mod2']] 
best_models_ZOE_IP123[["LTE_soil_chem_int_swc_5cm_12cm_IP2_mod2"]] = gam_results$LTE_soil_chem_int_swc_5cm_12cm_IP2[['mod2']]  

best_models_ZOE_IP123[["LTE_soil_chem_int_doc_6_75cm13cm15cm30cm32_5cm35cm45cm90_5_IP1_IP3_mod4"]] = gam_results$LTE_soil_chem_int_doc_6_75cm13cm15cm30cm32_5cm35cm45cm90_5_IP1_IP3[['mod4']]
best_models_ZOE_IP123[["LTE_soil_chem_int_n_nh4_6_75cm_13cm_15cm_30cm_IP1_IP3_mod4"]] = gam_results$LTE_soil_chem_int_n_nh4_6_75cm_13cm_15cm_30cm_IP1_IP3[['mod4']]
best_models_ZOE_IP123[["LTE_soil_chem_int_n_nh4_32_5cm_35cm_45cm_90_5_104_IP1_IP3_mod4"]] = gam_results$LTE_soil_chem_int_n_nh4_32_5cm_35cm_45cm_90_5_104_IP1_IP3[['mod4']]
best_models_ZOE_IP123[["LTE_soil_chem_int_n_no3_6_75cm_13cm_15cm_30cm_IP1_IP3_mod0_GAM"]] = gam_results$LTE_soil_chem_int_n_no3_6_75cm_13cm_15cm_30cm_IP1_IP3[['mod0_GAM']]
best_models_ZOE_IP123[["LTE_soil_chem_int_n_no3_32_5cm35cm45cm90_5_104_IP1_IP3_mod0_GAM"]] = gam_results$LTE_soil_chem_int_n_no3_32_5cm35cm45cm90_5_104_IP1_IP3[['mod0_GAM']]
best_models_ZOE_IP123[["LTE_soil_chem_int_soil_temp_IP1_IP3_20250506_mod2"]] = gam_results$LTE_soil_chem_int_soil_temp_IP1_IP3_20250506[['mod2']]
best_models_ZOE_IP123[["LTE_soil_chem_int_swc_5cm_15cm_40cm_58_IP1_IP3_mod1"]] = gam_results$LTE_soil_chem_int_swc_5cm_15cm_40cm_58_IP1_IP3[['mod1']]
saveRDS(best_models_ZOE_IP123, file = "best_models_ZOE_IP123")

# best models air temp, precip, bulk precip chemistry
best_models_precip_chem_bulk <- list()
best_models_precip_chem_bulk[["Ali_precip_chem_bulk_air_temperature_20251031_mod4"]] = gam_results$Ali_precip_chem_bulk_air_temperature[['mod4']]
best_models_precip_chem_bulk[["Ali_precip_chem_bulk_precipitation_20251031_mod0_GAMM"]] = gam_results$Ali_precip_chem_bulk_precipitation[['mod0_GAMM']]
best_models_precip_chem_bulk[["Ali_precip_chem_bulk_doc_bulk_20251031_mod0_GAM"]] = gam_results$Ali_precip_chem_bulk_doc_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Ali_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM"]] = gam_results$Ali_precip_chem_bulk_n_nh4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Ali_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM"]] = gam_results$Ali_precip_chem_bulk_n_no3_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Ali_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM"]] = gam_results$Ali_precip_chem_bulk_s_so4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Ali_precip_chem_bulk_ph_bulk_20251031_mod0_GAM"]] = gam_results$Ali_precip_chem_bulk_ph_bulk[['mod0_GAM']]

best_models_precip_chem_bulk[["Auk_precip_chem_bulk_air_temperature_20251128_mod4"]] = gam_results$Auk_precip_chem_bulk_air_temperature_20251128[['mod4']]
best_models_precip_chem_bulk[["Auk_precip_chem_bulk_precipitation_20251031_mod0_GAM"]] = gam_results$Auk_precip_chem_bulk_precipitation[['mod0_GAM']]
best_models_precip_chem_bulk[["Auk_precip_chem_bulk_n_nh4_bulk_20251128_mod0_GAM"]] = gam_results$Auk_precip_chem_bulk_n_nh4_bulk_20251128[['mod0_GAM']]
best_models_precip_chem_bulk[["Auk_precip_chem_bulk_n_no3_bulk_20251128_mod0_GAM"]] = gam_results$Auk_precip_chem_bulk_n_no3_bulk_20251128[['mod0_GAM']]
best_models_precip_chem_bulk[["Auk_precip_chem_bulk_s_so4_bulk_20251128_mod0_GAM"]] = gam_results$Auk_precip_chem_bulk_s_so4_bulk_20251128[['mod0_GAM']]
best_models_precip_chem_bulk[["Auk_precip_chem_bulk_ph_bulk_20251031_mod0_GAM"]] = gam_results$Auk_precip_chem_bulk_ph_bulk[['mod0_GAM']]

best_models_precip_chem_bulk[["Bra_precip_chem_bulk_air_temperature_20251031_mod1"]] = gam_results$Bra_precip_chem_bulk_air_temperature[['mod1']]
best_models_precip_chem_bulk[["Bra_precip_chem_bulk_precipitation_20251031_mod0_GAMM"]] = gam_results$Bra_precip_chem_bulk_precipitation[['mod0_GAMM']]
best_models_precip_chem_bulk[["Bra_precip_chem_bulk_doc_bulk_20251031_mod0_GAM"]] = gam_results$Bra_precip_chem_bulk_doc_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Bra_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM"]] = gam_results$Bra_precip_chem_bulk_n_nh4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Bra_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM"]] = gam_results$Bra_precip_chem_bulk_n_no3_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Bra_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM"]] = gam_results$Bra_precip_chem_bulk_s_so4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Bra_precip_chem_bulk_ph_bulk_20251031_mod0_GAMM"]] = gam_results$Bra_precip_chem_bulk_ph_bulk[['mod0_GAMM']]

best_models_precip_chem_bulk[["Cai_precip_chem_bulk_air_temperature_20251031_mod0_GAMM"]] = gam_results$Cai_precip_chem_bulk_air_temperature[['mod0_GAMM']]
best_models_precip_chem_bulk[["Cai_precip_chem_bulk_precipitation_20251031_mod0_GAMM"]] = gam_results$Cai_precip_chem_bulk_precipitation[['mod0_GAMM']]
best_models_precip_chem_bulk[["Cai_precip_chem_bulk_doc_bulk_20251031_mod0_GAM"]] = gam_results$Cai_precip_chem_bulk_doc_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Cai_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM"]] = gam_results$Cai_precip_chem_bulk_n_nh4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Cai_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM"]] = gam_results$Cai_precip_chem_bulk_n_no3_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Cai_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM"]] = gam_results$Cai_precip_chem_bulk_s_so4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Cai_precip_chem_bulk_ph_bulk_20251031_mod0_GAM"]] = gam_results$Cai_precip_chem_bulk_ph_bulk[['mod0_GAM']]

best_models_precip_chem_bulk[["Col_precip_chem_bulk_air_temperature_20251031_mod4"]] = gam_results$Col_precip_chem_bulk_air_temperature[['mod4']]
best_models_precip_chem_bulk[["Col_precip_chem_bulk_precipitation_20251031_mod0_GAMM"]] = gam_results$Col_precip_chem_bulk_precipitation[['mod0_GAMM']]
best_models_precip_chem_bulk[["Col_precip_chem_bulk_doc_bulk_20251031_mod0_GAM"]] = gam_results$Col_precip_chem_bulk_doc_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Col_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM"]] = gam_results$Col_precip_chem_bulk_n_nh4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Col_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM"]] = gam_results$Col_precip_chem_bulk_n_no3_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Col_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM"]] = gam_results$Col_precip_chem_bulk_s_so4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Col_precip_chem_bulk_ph_bulk_20251031_mod0_GAM"]] = gam_results$Col_precip_chem_bulk_ph_bulk[['mod0_GAM']]

best_models_precip_chem_bulk[["Dav_precip_chem_bulk_air_temperature_20251031_mod4"]] = gam_results$Dav_precip_chem_bulk_air_temperature[['mod4']]
best_models_precip_chem_bulk[["Dav_precip_chem_bulk_precipitation_20251031_mod0_GAM"]] = gam_results$Dav_precip_chem_bulk_precipitation[['mod0_GAM']]
best_models_precip_chem_bulk[["Dav_precip_chem_bulk_doc_bulk_20251031_mod0_GAM"]] = gam_results$Dav_precip_chem_bulk_doc_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Dav_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM"]] = gam_results$Dav_precip_chem_bulk_n_nh4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Dav_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM"]] = gam_results$Dav_precip_chem_bulk_n_no3_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Dav_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAMM"]] = gam_results$Dav_precip_chem_bulk_s_so4_bulk[['mod0_GAMM']]
best_models_precip_chem_bulk[["Dav_precip_chem_bulk_ph_bulk_20251031_mod0_GAM"]] = gam_results$Dav_precip_chem_bulk_ph_bulk[['mod0_GAM']]

best_models_precip_chem_bulk[["Lae_precip_chem_bulk_air_temperature_20251031_mod4"]] = gam_results$Lae_precip_chem_bulk_air_temperature[['mod4']]
best_models_precip_chem_bulk[["Lae_precip_chem_bulk_precipitation_20251031_mod0_GAM"]] = gam_results$Lae_precip_chem_bulk_precipitation[['mod0_GAM']]
best_models_precip_chem_bulk[["Lae_precip_chem_bulk_doc_bulk_20251031_mod0_GAM"]] = gam_results$Lae_precip_chem_bulk_doc_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Lae_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM"]] = gam_results$Lae_precip_chem_bulk_n_nh4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Lae_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM"]] = gam_results$Lae_precip_chem_bulk_n_no3_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Lae_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM"]] = gam_results$Lae_precip_chem_bulk_s_so4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Lae_precip_chem_bulk_ph_bulk_20251031_mod0_GAM"]] = gam_results$Lae_precip_chem_bulk_ph_bulk[['mod0_GAM']]

best_models_precip_chem_bulk[["Moo_precip_chem_bulk_air_temperature_20251031_mod1"]] = gam_results$Moo_precip_chem_bulk_air_temperature[['mod1']]
best_models_precip_chem_bulk[["Moo_precip_chem_bulk_precipitation_20251031_mod0_GAMM"]] = gam_results$Moo_precip_chem_bulk_precipitation[['mod0_GAMM']]
best_models_precip_chem_bulk[["Moo_precip_chem_bulk_doc_bulk_20251031_mod0_GAM"]] = gam_results$Moo_precip_chem_bulk_doc_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Moo_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM"]] = gam_results$Moo_precip_chem_bulk_n_nh4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Moo_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM"]] = gam_results$Moo_precip_chem_bulk_n_no3_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Moo_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM"]] = gam_results$Moo_precip_chem_bulk_s_so4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Moo_precip_chem_bulk_ph_bulk_20251031_mod0_GAM"]] = gam_results$Moo_precip_chem_bulk_ph_bulk[['mod0_GAM']]

best_models_precip_chem_bulk[["Wyt_precip_chem_bulk_air_temperature_20251031_mod1"]] = gam_results$Wyt_precip_chem_bulk_air_temperature[['mod1']]
best_models_precip_chem_bulk[["Wyt_precip_chem_bulk_precipitation_20251031_mod0_GAM"]] = gam_results$Wyt_precip_chem_bulk_precipitation[['mod0_GAM']]
best_models_precip_chem_bulk[["Wyt_precip_chem_bulk_doc_bulk_20251031_mod0_GAMM"]] = gam_results$Wyt_precip_chem_bulk_doc_bulk[['mod0_GAMM']]
best_models_precip_chem_bulk[["Wyt_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM"]] = gam_results$Wyt_precip_chem_bulk_n_nh4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Wyt_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM"]] = gam_results$Wyt_precip_chem_bulk_n_no3_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Wyt_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM"]] = gam_results$Wyt_precip_chem_bulk_s_so4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["Wyt_precip_chem_bulk_ph_bulk_20251031_mod0_GAM"]] = gam_results$Wyt_precip_chem_bulk_ph_bulk[['mod0_GAM']]

best_models_precip_chem_bulk[["LTE_precip_chem_bulk_air_temperature_20251031_mod4"]] = gam_results$LTE_precip_chem_bulk_air_temperature[['mod4']]
best_models_precip_chem_bulk[["LTE_precip_chem_bulk_precipitation_20251031_mod0_GAM"]] = gam_results$LTE_precip_chem_bulk_precipitation[['mod0_GAM']]
best_models_precip_chem_bulk[["LTE_precip_chem_bulk_doc_bulk_20251031_mod0_GAM"]] = gam_results$LTE_precip_chem_bulk_doc_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["LTE_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM"]] = gam_results$LTE_precip_chem_bulk_n_nh4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["LTE_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM"]] = gam_results$LTE_precip_chem_bulk_n_no3_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["LTE_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM"]] = gam_results$LTE_precip_chem_bulk_s_so4_bulk[['mod0_GAM']]
best_models_precip_chem_bulk[["LTE_precip_chem_bulk_ph_bulk_20251031_mod0_GAMM"]] = gam_results$LTE_precip_chem_bulk_ph_bulk[['mod0_GAMM']]
saveRDS(best_models_precip_chem_bulk, file = "best_models_precip_chem_bulk")

