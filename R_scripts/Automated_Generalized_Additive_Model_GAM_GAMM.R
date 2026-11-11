# In order to run GAM and GAMMs on the data for one variable at one site measured at multiple soil depths with increasing complexity
# at the end, the models are compared with ANOVA, and everything is bundled into a list and saved as an RDS file


library(dplyr)

soil_chem_filled = read.csv("soil_chem_filled_na_seadec_20250506.csv", sep = ",", fileEncoding = "latin1")  %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", #There tends to be issues with the accent with Zöbelboden, this makes sure the name is correct
    TRUE ~ site_name
  )) %>%
  #mutate(depth_cm = as.factor(depth_cm)) %>% 
  filter((site_name == "Laegeren" & depth_cm %in% c(5, 10, 20, 30))) # select the site and the depths to analyse


### GAM automated soil chem ####
library(mgcv)
run_GAMs_soil_chem = function(df, target_site, target_parameter, number_of_months, number_of_years) { #, supp_obs
  # GAM
  mod0_GAM = df %>% 
    distinct(.keep_all = TRUE) %>%
    filter(site_name == target_site, parameter_code == target_parameter) %>%
    droplevels() %>%
    dplyr::arrange(date) %>% 
    mutate(time = as.integer(as.Date(.$date, format = "%Y-%m-%d"))) %>% 
    bam(value_filled ~ s(month, bs = "cc", k = number_of_months) + s(time, by = as.factor(depth_cm)) + depth_cm, 
        data = .,
        method= 'fREML',
        knots = list(month = c(0.5, 12.5))) #+ s(replicate, bs = 're')
  
  
  # see that the lines depass the blue line, 
  # correlation means we have to move from gam to gamm
  
  # gAMM
  mod0_GAMM = df %>% 
    distinct(.keep_all = TRUE) %>%
    filter(site_name == target_site, parameter_code == target_parameter) %>%
    droplevels() %>%
    dplyr::arrange(date) %>% 
    mutate(time = as.integer(as.Date(.$date, format = "%Y-%m-%d"))) %>% 
    gamm(value_filled ~ s(month, bs = "cc", k = number_of_months)  + s(time, by = as.factor(depth_cm), bs="cr", k = number_of_years) + depth_cm,
         method="REML", # how p-value are calculationed
         knots = list(month = c(0.5, 12.5)),
         nthreads = c(4,1),
         discrete = TRUE, 
         data = .)
  
  mod1 = df %>% 
    distinct(.keep_all = TRUE) %>%
    filter(site_name == target_site, parameter_code == target_parameter) %>%
    droplevels() %>%
    dplyr::arrange(date) %>% 
    mutate(time = as.integer(as.Date(.$date, format = "%Y-%m-%d"))) %>% 
    gamm(value_filled ~ s(month, bs = "cc", k = number_of_months)  + s(time, by = as.factor(depth_cm), bs="cr", k = number_of_years) + depth_cm,
         correlation = corAR1(form = ~1),# corAR1 stuff means the order of the data is used for modeling, take the order as they appear, not assuming any nesting here
         method="REML", # how p-value are calculationed
         knots = list(month = c(0.5, 12.5)),
         nthreads = c(4,1),
         discrete = TRUE, 
         data = .)
  
  # now we have two options, either increase the number of lags or have nested
  # nesting could solve immediatly the problem 
  mod2 = df %>% 
    distinct(.keep_all = TRUE) %>%
    filter(site_name == target_site, parameter_code == target_parameter) %>%
    droplevels() %>%
    dplyr::arrange(date) %>% 
    mutate(time = as.integer(as.Date(.$date, format = "%Y-%m-%d"))) %>% 
    gamm(value_filled ~ s(month, bs = "cc", k = number_of_months) + s(time, by = as.factor(depth_cm), bs="cr", k = number_of_years) + depth_cm,
         correlation = corAR1(form = ~1|month),# corAR1 stuff means the order of the data is used for modeling, take the order as they appear, not assuming any nesting here
         method="REML", # how p-value are calculationed
         knots = list(month = c(0.5, 12.5)),
         nthreads = c(4,1),
         discrete = TRUE, 
         data = .)
  
  # sometimes it doesn't change anything

  
  #https://stackoverflow.com/questions/14059657/how-to-skip-an-error-in-a-loop
  #https://stackoverflow.com/questions/14748557/skipping-error-in-for-loop
  try(
    {    
  # increase p-component
  mod3 = df %>% 
    distinct(.keep_all = TRUE) %>%
    filter(site_name == target_site, parameter_code == target_parameter) %>%
    droplevels() %>%
    dplyr::arrange(date) %>% 
    mutate(time = as.integer(as.Date(.$date, format = "%Y-%m-%d"))) %>% 
    gamm(value_filled ~ s(month, bs = "cc", k = number_of_months)  + s(time, by = as.factor(depth_cm), bs="cr", k = number_of_years) + depth_cm,
          correlation = corARMA(form = ~ 1|month, p = 1),# corAR1 stuff means the order of the data is used for modeling, take the order as they appear, not assuming any nesting here
         method="REML", # how p-value are calculationed
         knots = list(month = c(0.5, 12.5)),
         nthreads = c(4,1),
         discrete = TRUE, 
         data = .)
  # doesn't work so we need to increase complexity, need to play with the p-component
    }
  )


  try(
    {  
      # increase p-component
      mod4 = df %>% 
        distinct(.keep_all = TRUE) %>%
        filter(site_name == target_site, parameter_code == target_parameter) %>%
        droplevels() %>%
        dplyr::arrange(date) %>% 
        mutate(time = as.integer(as.Date(.$date, format = "%Y-%m-%d"))) %>% 
        gamm(value_filled ~ s(month, bs = "cc", k = number_of_months) + s(time, by = as.factor(depth_cm), bs="cr", k = number_of_years) + depth_cm,
             correlation = corARMA(form = ~ 1|month, p = 2),# corAR1 stuff means the order of the data is used for modeling, take the order as they appear, not assuming any nesting here
             method="REML", # how p-value are calculationed
             knots = list(month = c(0.5, 12.5)),
             nthreads = c(4,1),
             discrete = TRUE, 
             data = .)
      # with pacf() 0.1 it is passable
    }
  )
  
  
  try(
    {
      mod5 <- df %>%
        distinct(.keep_all = TRUE) %>%
        filter(site_name == target_site, parameter_code == target_parameter) %>%
        droplevels() %>%
        dplyr::arrange(date) %>%
        mutate(time = as.integer(as.Date(.$date, format = "%Y-%m-%d"))) %>%
        gamm(value_filled ~ s(month, bs = "cc", k = number_of_months) + s(time, by = depth_cm, bs="cr", k = number_of_years) + depth_cm,
             correlation = corARMA(form = ~ 1|month, p = 3),# corAR1 stuff means the order of the data is used for modeling, take the order as they appear, not assuming any nesting here
             method="REML",
             data = .)
    }
  )
  
  try(
    {
      mod6 = df %>% 
        distinct(.keep_all = TRUE) %>%
        filter(site_name == target_site, parameter_code == target_parameter) %>%
        droplevels() %>%
        dplyr::arrange(date) %>% 
        mutate(time = as.integer(as.Date(.$date, format = "%Y-%m-%d"))) %>% 
        gamm(value_filled ~ s(month, bs = "cc", k = number_of_months) + s(time, by = as.factor(depth_cm), bs="cr", k = number_of_years) + depth_cm,
             correlation = corARMA(form = ~ 1|year, p = 4),# corAR1 stuff means the order of the data is used for modeling, take the order as they appear, not assuming any nesting here
             method="REML", # how p-value are calculationed
             knots = list(month = c(0.5, 12.5)),
             nthreads = c(4,1),
             discrete = TRUE, 
             data = .)
    }
  )
  
  try(
    {
      mod7 = df %>% 
        distinct(.keep_all = TRUE) %>%
        filter(site_name == target_site, parameter_code == target_parameter) %>%
        droplevels() %>%
        dplyr::arrange(date) %>% 
        mutate(time = as.integer(as.Date(.$date, format = "%Y-%m-%d"))) %>% 
        gamm(value_filled ~ s(month, bs = "cc", k = number_of_months) + s(time, by = as.factor(depth_cm), bs="cr", k = number_of_years) + depth_cm,
             correlation = corARMA(form = ~ 1|year, p = 5),# corAR1 stuff means the order of the data is used for modeling, take the order as they appear, not assuming any nesting here
             method="REML", # how p-value are calculationed
             knots = list(month = c(0.5, 12.5)),
             nthreads = c(4,1),
             discrete = TRUE, 
             data = .)
    }
  )
  
  # compare the models with ANOVA
  GAM_anova_comparison <- if(exists("mod7") & exists("mod6") & exists("mod5") & exists("mod4")){
    anova(mod0_GAMM$lme, mod1$lme, mod2$lme, mod3$lme, mod4$lme, mod5$lme, mod6$lme, mod7$lme)
  } else if ( exists("mod6") & exists("mod5") & exists("mod4")) {
    anova(mod0_GAMM$lme, mod1$lme, mod2$lme, mod3$lme, mod4$lme, mod5$lme, mod6$lme)
  } else if ( exists("mod5") & !exists("mod4")) {
    anova(mod0_GAMM$lme, mod1$lme, mod2$lme, mod3$lme, mod5$lme)
  } else if ( exists("mod5") & exists("mod4")) {
    anova(mod0_GAMM$lme, mod1$lme, mod2$lme, mod3$lme, mod4$lme, mod5$lme)
  } else if ( exists("mod4")) {
    anova(mod0_GAMM$lme, mod1$lme, mod2$lme, mod3$lme, mod4$lme)
  } else if ( exists("mod3")) {
    anova(mod0_GAMM$lme, mod1$lme, mod2$lme, mod3$lme)
  } else if ( exists("mod2")) {
    anova(mod0_GAMM$lme, mod1$lme, mod2$lme)
  } else ( anova(mod0_GAMM$lme, mod1$lme))  
  
  # create a list with the models  
  GAMlist <- if(exists("mod7") & exists("mod6") & exists("mod5") & exists("mod4")){
    list(mod0_GAM, mod0_GAMM, mod1, mod2, mod3, mod4, mod5, mod6, mod7,  GAM_anova_comparison)
  } else if ( exists("mod6") & exists("mod5") & exists("mod4")) {
    list(mod0_GAM, mod0_GAMM, mod1, mod2, mod3, mod4, mod5, mod6, GAM_anova_comparison)
  } else if ( exists("mod5") & !exists("mod4")) {
    list(mod0_GAM, mod0_GAMM, mod1, mod2, mod3, mod5, GAM_anova_comparison)
  } else if ( exists("mod5") & exists("mod4")) {
    list(mod0_GAM, mod0_GAMM, mod1, mod2, mod3, mod4, mod5, GAM_anova_comparison)
  } else if ( exists("mod4")) {
    list(mod0_GAM, mod0_GAMM, mod1, mod2, mod3, mod4, GAM_anova_comparison)
  } else if ( exists("mod3")) {
    list(mod0_GAM, mod0_GAMM, mod1, mod2, mod3, GAM_anova_comparison)
  } else if ( exists("mod2")) {
    list(mod0_GAM, mod0_GAMM, mod1, mod2, GAM_anova_comparison)
  } else ( list(mod0_GAM, mod0_GAMM, mod1, GAM_anova_comparison)) 
  
  # add the names  
  names(GAMlist) <- if(exists("mod7") & exists("mod6") & exists("mod5") & exists("mod4")){
    c("mod0_GAM", "mod0_GAMM", "mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7", "GAM_anova_comparison")
  } else if ( exists("mod6") & exists("mod5") & exists("mod4")) {
    c("mod0_GAM", "mod0_GAMM", "mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "GAM_anova_comparison")
  } else if ( exists("mod5") & !exists("mod4")) {
    c("mod0_GAM", "mod0_GAMM", "mod1", "mod2", "mod3", "mod5", "GAM_anova_comparison")
  } else if ( exists("mod5") & exists("mod4")) {
    c("mod0_GAM", "mod0_GAMM", "mod1", "mod2", "mod3", "mod4", "mod5", "GAM_anova_comparison")
  } else if ( exists("mod4")) {
    c("mod0_GAM", "mod0_GAMM", "mod1", "mod2", "mod3", "mod4", "GAM_anova_comparison")
  } else if ( exists("mod3")) {
    c("mod0_GAM", "mod0_GAMM", "mod1", "mod2", "mod3", "GAM_anova_comparison")
  } else if ( exists("mod2")) {
    c("mod0_GAM", "mod0_GAMM", "mod1", "mod2", "GAM_anova_comparison")
  } else ( c("mod0_GAM", "mod0_GAMM", "mod1", "GAM_anova_comparison")) 
  
  try(expr, silent = FALSE,
      outFile = getOption("try.outFile", default = stderr()))
  
  GAMlist_name <- paste(substr(target_site, 1, 3), "soil_chem_int", target_parameter, sep = "_")
  #assign(GAMlist_name, GAMlist, envir = .GlobalEnv)
  saveRDS(GAMlist, file = GAMlist_name)
  
}


run_GAMs_soil_chem(soil_chem_filled, "Laegeren", "swc", 12, 19) #dataframe, site_name, parameter_code, number of months, number of years




