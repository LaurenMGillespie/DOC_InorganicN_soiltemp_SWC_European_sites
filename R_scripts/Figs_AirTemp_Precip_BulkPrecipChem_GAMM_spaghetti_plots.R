# Import data ------------------------------------------------------------------------------------------------
# The cleaned, gap-filled time series can be found in the following open access B2Share repository: 
# Gillespie, L. M., Andrews, C., Augustaitis, A., Baužienė, I., Benham, S. E., Buchmann, N., et al. (2026). Long-term time series 
# data of dissolved organic carbon, inorganic nitrogen, temperature, and water content in ten European sites [Data set]. 
# https://doi.org/10.23728/b2share.nzw7b-5f570


library("dplyr")

soil_chem_filled = read.csv("Gillespie_et_all_time_series_DOC_IN_soiltemp_SWC_European_sites.csv", sep = ",", fileEncoding = "latin1")  %>% 
  mutate(depth_range = case_when( #if the variable depth_range is not already in the dataframe
    depth_cm <= 10 ~ "0-10 cm",
    depth_cm >= 11 & depth_cm <= 30 ~ "11-30 cm",
    depth_cm >= 31 & depth_cm <= 60 ~ "31-60 cm",
    depth_cm >= 61  ~ ">60 cm",
  )) %>%
  filter(!((is.na(date))| #some data needs to be removed
             #(parameter_code %in% c("precipitation", "air_temperature", "surface_air_temperature"))|
             (grepl("Auk|Cai", site_name)& parameter_code == "doc")|
             (grepl("Moo", site_name)& parameter_code == "swc")|
             (grepl("Zöbelboden", site_name) & parameter_code %in% c("doc", "n_nh4", "n_no3") & code_plot_agg == "IP1_IP3" & depth_cm == 5)|
             (grepl("Zöbelboden", site_name) & parameter_code == "swc" & code_plot_agg == "IP2" & as.Date(date) > "2013-06-17")|
             (grepl("Zöbelboden", site_name) & parameter_code == "swc" & code_plot_agg == "IP1_IP3" & depth_cm == 12)))

# create color palette
cols <- c("0-10 cm" = "#E41A1C", "11-30 cm" = "#377EB8", "31-60 cm" = "#4DAF4A", ">60 cm" = "#984EA3")

# create GAMM spaghetti plots in R with ggplot ---------------------------------------------------------------
#https://drmowinckels.io/blog/2018/gamm-spaghetti-plots-in-r-with-ggplot/


# different code structure if the variable was only measured at one depth vs >1 depths

library("itsadug")
library("mgcv")
library("nlme")
library("ggplot2")
library("patchwork")



### Precip Airtemp PrecipChem ####

best_models_precip_chem_bulk <- list.files(pattern = "best_models_precip_chem_bulk") %>%
  purrr::set_names() %>%
  purrr::map(readRDS) 


airtemp_precip_layout <- "
  1
  2
  3
  4
  5
  6
  7
"


#### Figures Ali ####
##### Ali_air_temp ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Alice Holt", parameter_code == "air_temperature")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Ali_precip_chem_bulk_air_temperature_20251031_mod4$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Ali_air_temp = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Air temperature (°C)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1994-07-06','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Ali_precipitation ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Alice Holt", parameter_code == "precipitation")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Ali_precip_chem_bulk_precipitation_20251031_mod0_GAMM$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Ali_precip = ggplot() + 
  geom_bar(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), stat="identity",color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Precipitation (mm)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1994-07-06','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Ali_doc_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Alice Holt", parameter_code == "doc_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Ali_precip_chem_bulk_doc_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Ali_doc_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1994-07-06','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Ali_n_nh4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Alice Holt", parameter_code == "n_nh4_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Ali_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Ali_n_nh4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1994-07-06','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Ali_n_no3_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Alice Holt", parameter_code == "n_no3_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Ali_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Ali_n_no3_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1994-07-06','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Ali_s_so4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Alice Holt", parameter_code == "s_so4_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Ali_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Ali_s_so4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("SO"[4]^{"2−"}~"-S (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1994-07-06','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Ali_ph_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Alice Holt", parameter_code == "ph_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Ali_precip_chem_bulk_ph_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Ali_ph_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("pH"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1994-07-06','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### fig combined ####
Ali_air_temp + Ali_precip + Ali_doc_bulk + Ali_n_nh4_bulk + Ali_n_no3_bulk +
  Ali_s_so4_bulk + Ali_ph_bulk +
  
  plot_layout(design = airtemp_precip_layout) +
  plot_annotation(title = 'Alice Holt - United Kingdom - Bulk deposition', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 15)))

ggsave("GAM_Ali_soiltemp_precip.png", width = 25, height = 50, units = "cm")



#### Figures Auk ####
##### Auk_air_temp ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Aukstaitija Integrated Monitoring Station", parameter_code == "air_temperature")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251128$Auk_precip_chem_bulk_air_temperature_20251128_mod4$gam,
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Auk_air_temp = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Air temperature (°C)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-05-01','2024-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Auk_precipitation ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Aukstaitija Integrated Monitoring Station", parameter_code == "precipitation")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251128$Auk_precip_chem_bulk_precipitation_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Auk_precip = ggplot() + 
  geom_bar(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), stat="identity",color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Precipitation (mm)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-05-01','2024-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Auk_n_nh4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Aukstaitija Integrated Monitoring Station", parameter_code == "n_nh4_bulk",
           analysis_description =="data at yearly resolution sent by Algirdas Augustaitis on 20251128" )

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251128$Auk_precip_chem_bulk_n_nh4_bulk_20251128_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Auk_n_nh4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-05-01','2024-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Auk_n_no3_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Aukstaitija Integrated Monitoring Station", parameter_code == "n_no3_bulk",
         analysis_description =="data at yearly resolution sent by Algirdas Augustaitis on 20251128" )

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251128$Auk_precip_chem_bulk_n_no3_bulk_20251128_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Auk_n_no3_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-05-01','2024-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Auk_s_so4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Aukstaitija Integrated Monitoring Station", parameter_code == "s_so4_bulk",
         analysis_description =="data at yearly resolution sent by Algirdas Augustaitis on 20251128" )

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251128$Auk_precip_chem_bulk_s_so4_bulk_20251128_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Auk_s_so4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("SO"[4]^{"2−"}~"-S (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-05-01','2024-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Auk_ph_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Aukstaitija Integrated Monitoring Station", parameter_code == "ph_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251128$Auk_precip_chem_bulk_ph_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Auk_ph_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("pH"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-05-01','2024-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### fig combined ####
Auk_airtemp_precip_layout <- "
  1
  2
  3
  4
  5
  6
"
Auk_air_temp + Auk_precip +  Auk_n_nh4_bulk + Auk_n_no3_bulk +
  Auk_s_so4_bulk + Auk_ph_bulk +
  
  plot_layout(design = Auk_airtemp_precip_layout) +
  plot_annotation(title = 'Aukstaitija Integrated Monitoring Station - Lithuania - Bulk deposition', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 15)))

ggsave("GAM_Auk_soiltemp_precip.png", width = 25, height = 40, units = "cm")



#### Figures Bra ####

##### Bra_air_temp ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "air_temperature")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Bra_precip_chem_bulk_air_temperature_20251031_mod1$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Bra_air_temp = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Air temperature (°C)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-01-15','2016-12-20'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Bra_precipitation ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "precipitation")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Bra_precip_chem_bulk_precipitation_20251031_mod0_GAMM$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Bra_precip = ggplot() + 
  geom_bar(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), stat="identity",color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Precipitation (mm)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-01-15','2016-12-20'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Bra_doc_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "doc_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Bra_precip_chem_bulk_doc_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(time < 10682))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))


Bra_doc_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-01-15','2016-12-20'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Bra_n_nh4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "n_nh4_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Bra_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Bra_n_nh4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-01-15','2016-12-20'))) +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Bra_n_no3_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "n_no3_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Bra_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Bra_n_no3_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-01-15','2016-12-20'))) +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Bra_s_so4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "s_so4_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Bra_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Bra_s_so4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("SO"[4]^{"2−"}~"-S (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-01-15','2016-12-20'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Bra_ph_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "ph_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Bra_precip_chem_bulk_ph_bulk_20251031_mod0_GAMM$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Bra_ph_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-01-15','2016-12-20'))) +
  ylab(expression("pH"))+
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### fig combined ####
Bra_air_temp + Bra_precip + Bra_doc_bulk + Bra_n_nh4_bulk + Bra_n_no3_bulk +
  Bra_s_so4_bulk + Bra_ph_bulk +
  
  plot_layout(design = airtemp_precip_layout) +
  plot_annotation(title = 'Brasschaat - De Inslag - Belgium - Bulk deposition', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 15)))

ggsave("GAM_Bra_soiltemp_precip.png", width = 25, height = 50, units = "cm")


#### Figures Cai ####
##### Cai_air_temp ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Cairngorms", parameter_code == "air_temperature")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Cai_precip_chem_bulk_air_temperature_20251031_mod0_GAMM$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Cai_air_temp = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Air temperature (°C)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1999-06-28','2015-12-29'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Cai_precipitation ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Cairngorms", parameter_code == "precipitation")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Cai_precip_chem_bulk_precipitation_20251031_mod0_GAMM$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Cai_precip = ggplot() + 
  geom_bar(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), stat="identity",color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Precipitation (mm)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1999-06-28','2015-12-29'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Cai_doc_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Cairngorms", parameter_code == "doc_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Cai_precip_chem_bulk_doc_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Cai_doc_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1999-06-28','2015-12-29'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Cai_n_nh4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Cairngorms", parameter_code == "n_nh4_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Cai_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Cai_n_nh4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1999-06-28','2015-12-29'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Cai_n_no3_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Cairngorms", parameter_code == "n_no3_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Cai_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Cai_n_no3_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1999-06-28','2015-12-29'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Cai_s_so4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Cairngorms", parameter_code == "s_so4_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Cai_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Cai_s_so4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("SO"[4]^{"2−"}~"-S (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1999-06-28','2015-12-29'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Cai_ph_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Cairngorms", parameter_code == "ph_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Cai_precip_chem_bulk_ph_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Cai_ph_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("pH"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1999-06-28','2015-12-29'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### fig combined ####
Cai_air_temp + Cai_precip + Cai_doc_bulk + Cai_n_nh4_bulk + Cai_n_no3_bulk +
  Cai_s_so4_bulk + Cai_ph_bulk +
  
  plot_layout(design = airtemp_precip_layout) +
  plot_annotation(title = 'Cairngorms - United Kingdom - Bulk deposition', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 15)))

ggsave("GAM_Cai_soiltemp_precip.png", width = 25, height = 50, units = "cm")



#### Figures Col ####

##### Col_air_temp ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "air_temperature")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Col_precip_chem_bulk_air_temperature_20251031_mod4$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Col_air_temp = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Air temperature (°C)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-01-01','2017-01-03'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Col_precipitation ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "precipitation")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Col_precip_chem_bulk_precipitation_20251031_mod0_GAMM$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Col_precip = ggplot() + 
  geom_bar(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), stat="identity",color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Precipitation (mm)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-01-01','2017-01-03'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Col_doc_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "doc_bulk")
pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Col_precip_chem_bulk_doc_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Col_doc_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-01-01','2017-01-03'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Col_n_nh4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "n_nh4_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Col_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Col_n_nh4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-01-01','2017-01-03'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Col_n_no3_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "n_no3_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Col_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Col_n_no3_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-01-01','2017-01-03'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Col_s_so4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "s_so4_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Col_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Col_s_so4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("SO"[4]^{"2−"}~"-S (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-01-01','2017-01-03'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Col_ph_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "ph_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Col_precip_chem_bulk_ph_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Col_ph_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("pH"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-01-01','2017-01-03'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### fig combined ####
Col_air_temp + Col_precip + Col_doc_bulk + Col_n_nh4_bulk + Col_n_no3_bulk +
  Col_s_so4_bulk + Col_ph_bulk +
  
  plot_layout(design = airtemp_precip_layout) +
  plot_annotation(title = 'Collelongo-Selva Piana - Italy - Bulk deposition', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 15)))

ggsave("GAM_Col_soiltemp_precip.png", width = 25, height = 50, units = "cm")


#### Figures Dav ####

##### Dav_air_temp ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Davos Seehornwald", parameter_code == "air_temperature")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Dav_precip_chem_bulk_air_temperature_20251031_mod4$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Dav_air_temp = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Air temperature (°C)"))+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2009-01-13','2017-01-03'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Dav_precipitation ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Davos Seehornwald", parameter_code == "precipitation")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Dav_precip_chem_bulk_precipitation_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Dav_precip = ggplot() + 
  geom_bar(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), stat="identity",color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Precipitation (mm)"))+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2009-01-13','2017-01-03'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Dav_doc_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Davos Seehornwald", parameter_code == "doc_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Dav_precip_chem_bulk_doc_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Dav_doc_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2009-01-13','2017-01-03'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Dav_n_nh4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Davos Seehornwald", parameter_code == "n_nh4_bulk" & value >0)

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Dav_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Dav_n_nh4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2009-01-13','2017-01-03'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Dav_n_no3_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Davos Seehornwald", parameter_code == "n_no3_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Dav_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Dav_n_no3_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2009-01-13','2017-01-03'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Dav_s_so4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Davos Seehornwald", parameter_code == "s_so4_bulk" & value >0)

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Dav_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAMM$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Dav_s_so4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("SO"[4]^{"2−"}~"-S (mg/l)"))+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2009-01-13','2017-01-03'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Dav_ph_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Davos Seehornwald", parameter_code == "ph_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Dav_precip_chem_bulk_ph_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Dav_ph_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("pH"))+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2009-01-13','2017-01-03'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### fig combined ####
Dav_air_temp + Dav_precip + Dav_doc_bulk + Dav_n_nh4_bulk + Dav_n_no3_bulk +
  Dav_s_so4_bulk + Dav_ph_bulk +
  
  plot_layout(design = airtemp_precip_layout) +
  plot_annotation(title = 'Davos Seehornwald - Switzerland - Bulk deposition', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 15)))

ggsave("GAM_Dav_soiltemp_precip.png", width = 25, height = 50, units = "cm")




#### Figures Lae ####

##### Lae_air_temp ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Laegeren", parameter_code == "air_temperature")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Lae_precip_chem_bulk_air_temperature_20251031_mod4$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Lae_air_temp = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Air temperature (°C)"))+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2013-01-01','2017-01-04'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Lae_precipitation ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Laegeren", parameter_code == "precipitation")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Lae_precip_chem_bulk_precipitation_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Lae_precip = ggplot() + 
  geom_bar(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), stat="identity",color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Precipitation (mm)"))+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2013-01-01','2017-01-04'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Lae_doc_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Laegeren", parameter_code == "doc_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Lae_precip_chem_bulk_doc_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Lae_doc_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2013-01-01','2017-01-04'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Lae_n_nh4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Laegeren", parameter_code == "n_nh4_bulk" & value >0)

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Lae_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Lae_n_nh4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2013-01-01','2017-01-04'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Lae_n_no3_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Laegeren", parameter_code == "n_no3_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Lae_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Lae_n_no3_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2013-01-01','2017-01-04'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Lae_s_so4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Laegeren", parameter_code == "s_so4_bulk" & value >0)

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Lae_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Lae_s_so4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("SO"[4]^{"2−"}~"-S (mg/l)"))+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2013-01-01','2017-01-04'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Lae_ph_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Laegeren", parameter_code == "ph_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Lae_precip_chem_bulk_ph_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Lae_ph_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("pH"))+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2013-01-01','2017-01-04'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### fig combined ####
Lae_air_temp + Lae_precip + Lae_doc_bulk + Lae_n_nh4_bulk + Lae_n_no3_bulk +
  Lae_s_so4_bulk + Lae_ph_bulk +
  
  plot_layout(design = airtemp_precip_layout) +
  plot_annotation(title = 'Lägeren - Switzerland - Bulk deposition', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 15)))

ggsave("GAM_Lae_soiltemp_precip.png", width = 25, height = 50, units = "cm")



#### Figures Moo ####

##### Moo_air_temp ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Moor House Upper Teesdale", parameter_code == "air_temperature")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Moo_precip_chem_bulk_air_temperature_20251031_mod1$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Moo_air_temp = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Air temperature (°C)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1991-05-28','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Moo_precipitation ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Moor House Upper Teesdale", parameter_code == "precipitation")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Moo_precip_chem_bulk_precipitation_20251031_mod0_GAMM$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Moo_precip = ggplot() + 
  geom_bar(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), stat="identity",color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Precipitation (mm)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1991-05-28','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Moo_doc_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Moor House Upper Teesdale", parameter_code == "doc_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Moo_precip_chem_bulk_doc_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Moo_doc_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1991-05-28','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Moo_n_nh4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Moor House Upper Teesdale", parameter_code == "n_nh4_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Moo_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Moo_n_nh4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1991-05-28','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Moo_n_no3_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Moor House Upper Teesdale", parameter_code == "n_no3_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Moo_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Moo_n_no3_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1991-05-28','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Moo_s_so4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Moor House Upper Teesdale", parameter_code == "s_so4_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Moo_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Moo_s_so4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("SO"[4]^{"2−"}~"-S (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1991-05-28','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Moo_ph_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Moor House Upper Teesdale", parameter_code == "ph_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Moo_precip_chem_bulk_ph_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Moo_ph_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("pH"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1991-05-28','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### fig combined ####
Moo_air_temp + Moo_precip + Moo_doc_bulk + Moo_n_nh4_bulk + Moo_n_no3_bulk +
  Moo_s_so4_bulk + Moo_ph_bulk +
  
  plot_layout(design = airtemp_precip_layout) +
  plot_annotation(title = 'Moor House Upper Teesdale - United Kingdom - Bulk deposition', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 15)))

ggsave("GAM_Moo_soiltemp_precip.png", width = 25, height = 50, units = "cm")



#### Figures Wyt ####

##### Wyt_air_temp ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Wytham", parameter_code == "air_temperature")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Wyt_precip_chem_bulk_air_temperature_20251031_mod1$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Wyt_air_temp = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Air temperature (°C)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1992-03-13','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Wyt_precipitation ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Wytham", parameter_code == "precipitation")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Wyt_precip_chem_bulk_precipitation_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Wyt_precip = ggplot() + 
  geom_bar(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), stat="identity",color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Precipitation (mm)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1992-03-13','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Wyt_doc_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Wytham", parameter_code == "doc_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Wyt_precip_chem_bulk_doc_bulk_20251031_mod0_GAMM$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Wyt_doc_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1992-03-13','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Wyt_n_nh4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Wytham", parameter_code == "n_nh4_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Wyt_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Wyt_n_nh4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1992-03-13','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Wyt_n_no3_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Wytham", parameter_code == "n_no3_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Wyt_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Wyt_n_no3_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1992-03-13','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### Wyt_s_so4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Wytham", parameter_code == "s_so4_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Wyt_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Wyt_s_so4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("SO"[4]^{"2−"}~"-S (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1992-03-13','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### Wyt_ph_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "Wytham", parameter_code == "ph_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$Wyt_precip_chem_bulk_ph_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


Wyt_ph_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("pH"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1992-03-13','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### fig combined ####
Wyt_air_temp + Wyt_precip + Wyt_doc_bulk + Wyt_n_nh4_bulk + Wyt_n_no3_bulk +
  Wyt_s_so4_bulk + Wyt_ph_bulk +
  
  plot_layout(design = airtemp_precip_layout) +
  plot_annotation(title = 'Wytham - United Kingdom - Bulk deposition', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 15)))

ggsave("GAM_Wyt_soiltemp_precip.png", width = 25, height = 50, units = "cm")




#### Figures Zoe ####

##### LTE_air_temp ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "LTER Zöbelboden", parameter_code == "air_temperature")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$LTE_precip_chem_bulk_air_temperature_20251031_mod4$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


LTE_air_temp = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Air temperature (°C)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-10-01','2018-12-01'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### LTE_precipitation ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "LTER Zöbelboden", parameter_code == "precipitation")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$LTE_precip_chem_bulk_precipitation_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


LTE_precip = ggplot() + 
  geom_bar(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), stat="identity",color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("Precipitation (mm)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-10-01','2018-12-01'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### LTE_doc_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "LTER Zöbelboden", parameter_code == "doc_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$LTE_precip_chem_bulk_doc_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


LTE_doc_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-10-01','2018-12-01'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### LTE_n_nh4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "LTER Zöbelboden", parameter_code == "n_nh4_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$LTE_precip_chem_bulk_n_nh4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


LTE_n_nh4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-10-01','2018-12-01'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### LTE_n_no3_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "LTER Zöbelboden", parameter_code == "n_no3_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$LTE_precip_chem_bulk_n_no3_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


LTE_n_no3_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-10-01','2018-12-01'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### LTE_s_so4_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "LTER Zöbelboden", parameter_code == "s_so4_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$LTE_precip_chem_bulk_s_so4_bulk_20251031_mod0_GAM, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


LTE_s_so4_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("SO"[4]^{"2−"}~"-S (mg/l)"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-10-01','2018-12-01'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))


##### LTE_ph_bulk ####
dat = soil_chem_filled_prec_at_pc %>% 
  filter(site_name == "LTER Zöbelboden", parameter_code == "ph_bulk")

pred <- itsadug::get_predictions(best_models_precip_chem_bulk$best_models_precip_chem_bulk_20251031$LTE_precip_chem_bulk_ph_bulk_20251031_mod0_GAMM$gam, 
                                 cond=list(time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


LTE_ph_bulk = ggplot() + 
  geom_line(data = dat, alpha=.7, aes(x=as.Date(time), y = value), color = "black") + 
  geom_point(data = dat,alpha=1.0, aes(x=as.Date(time), y = value), color = "black", fill = "black") +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='#66CD00') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='#66CD00') +
  ylab(expression("pH"))+
  scale_x_date(date_breaks = "2 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-10-01','2018-12-01'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(color = 'black'))

##### fig combined ####
LTE_air_temp + LTE_precip + LTE_doc_bulk + LTE_n_nh4_bulk + LTE_n_no3_bulk +
  LTE_s_so4_bulk + LTE_ph_bulk +
  
  plot_layout(design = airtemp_precip_layout) +
  plot_annotation(title = 'Zöbelboden - Austria - Bulk deposition', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 15)))

ggsave("GAM_LTE_soiltemp_precip.png", width = 25, height = 50, units = "cm")


