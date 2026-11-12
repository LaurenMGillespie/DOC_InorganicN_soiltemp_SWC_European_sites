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



### Identify the start and end dates for each site and variable ----------------------------------------------

period_coverage_GAM = soil_chem_filled %>%
  dplyr::select(site_name, parameter_code, depth_cm, depth_range, code_plot_agg, date) %>%
  mutate(time = as.integer(as.Date(.$date, format = "%Y-%m-%d"))) %>% 
  filter(!is.na(date), 
         !(grepl("Auk", site_name)& parameter_code == "doc"),
         !(grepl("Moo", site_name)& parameter_code == "swc"),
         !(grepl("Zöbelboden", site_name) & parameter_code %in% c("doc", "n_nh4", "n_no3") & code_plot_agg == "IP1_IP3" & depth_cm == 5)) %>%
  mutate(site_code_2 = case_when(
    site_name == "Alice Holt" ~ "ALI",
    site_name == "Aukstaitija Integrated Monitoring Station" ~ "AUK",
    site_name == "Brasschaat - De Inslag" ~ "BRA",
    site_name == "Cairngorms" ~ "CAI",
    site_name == "Collelongo-Selva Piana ABR1" ~ "COL",
    site_name == "Davos Seehornwald" ~ "DAV",
    site_name == "LTER Zöbelboden" ~ "ZOE",
    site_name == "Laegeren" ~ "LAE",
    site_name == "Moor House Upper Teesdale" ~ "MOO",
    site_name == "Wytham" ~ "WYT"
  ),
  code_plot_agg = case_when(
    is.na(code_plot_agg) ~ " ",
    TRUE ~ code_plot_agg
  ),
  parameter_code = case_when(
    parameter_code == "doc" ~ "DOC",
    parameter_code == "n_nh4" ~ "NH4+ -N",
    parameter_code == "n_no3" ~ "NO3- -N",
    parameter_code == "soil_temp" ~ "Soil temp.",
    parameter_code == "swc" ~ "SWC",
    TRUE ~ parameter_code)) %>% 
  group_by(site_name, site_code_2, code_plot_agg, parameter_code, depth_cm) %>%
  summarize(
    start = min(date),
    start_time = min(time),
    end = max(date),
    end_time = max(time),
    total_years = round(as.numeric(gsub(" days", "", difftime(end ,start , units = c("days"))/365)), digits = 1), 
    total_years_label = paste(round(as.numeric(gsub(" days", "", difftime(end ,start , units = c("days"))/365)), digits = 1), "years", sep = " ") 
  ) %>%
  ungroup() %>%   
  mutate(mid_date = as.Date(start) + floor((as.Date(end)-as.Date(start))/2)) %>% View()




# create GAMM spaghetti plots in R with ggplot ---------------------------------------------------------------
#https://drmowinckels.io/blog/2018/gamm-spaghetti-plots-in-r-with-ggplot/


# different code structure if the variable was only measured at one depth vs >1 depths

library("itsadug")
library("mgcv")
library("nlme")
library("ggplot2")
library("patchwork")


### Figures Ali ####
Ali_results <- list.files(pattern = "best_models_Ali") %>%
  purrr::set_names() %>%
  purrr::map(readRDS) 


#### doc ####
dat = soil_chem_filled %>% 
  filter(site_name == "Alice Holt", parameter_code == "doc")

pred <- itsadug::get_predictions(Ali_results$best_models_Ali_20250506$Ali_soil_chem_int_doc_20250506_mod4$gam, 
                                 cond=list(depth_cm= c(10, 50), #levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Ali_doc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-05-29','2021-12-29'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### n_nh4 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Alice Holt", parameter_code == "n_nh4")

pred <- itsadug::get_predictions(Ali_results$best_models_Ali_20250506$Ali_soil_chem_int_n_nh4_20250506_mod1$gam, 
                                 cond=list(depth_cm= c(10, 50), #levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  #filter(!(depth_cm ==10 & time > 16580))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Ali_n_nh4 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-05-29','2021-12-29'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### n_no3 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Alice Holt", parameter_code == "n_no3")

pred <- itsadug::get_predictions(Ali_results$best_models_Ali_20250506$Ali_soil_chem_int_n_no3_20250506_mod1$gam, 
                                 cond=list(depth_cm= c(10, 50), #levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Ali_n_no3 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-05-29','2021-12-29'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### soil_temp ####
dat = soil_chem_filled %>% 
  filter(site_name == "Alice Holt", parameter_code == "soil_temp")

pred <- itsadug::get_predictions(Ali_results$best_models_Ali_20250506$Ali_soil_chem_int_soil_temp_mod4$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Ali_soil_temp = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("Soil temp. (°C)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-05-29','2021-12-29'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))

#### swc ####
dat = soil_chem_filled %>% 
  filter(site_name == "Alice Holt", parameter_code == "swc")

pred <- itsadug::get_predictions(Ali_results$best_models_Ali_20250506$Ali_soil_chem_swc_mod5$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Ali_swc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("SWC (%)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-05-29','2021-12-29'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))

#### fig combined ####
Ali_layout <- "
  11
  2#
  33
  44
  55
"
Ali_soil_temp + Ali_swc + Ali_doc + Ali_n_nh4 + Ali_n_no3 + 
  plot_layout(design = Ali_layout) +
  plot_annotation(title = 'Alice Holt - United Kingdom', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 18)))

ggsave("Fig_ALI.png", width = 15, height = 25, units = "cm")


### Figures Auk ####
Auk_results <- list.files(pattern = "best_models_Auk_20251128") %>% #best_models_Auk
  purrr::set_names() %>%
  purrr::map(readRDS) 

#### n_nh4 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Aukstaitija Integrated Monitoring Station", parameter_code == "n_nh4")

pred <- itsadug::get_predictions(Auk_results$best_models_Auk_20251128$Auk_soil_chem_int_n_nh4_20251128_mod0_GAMM$gam, 
                                 cond=list(depth_cm= c(20, 40), #levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!( depth_cm ==40 & time < 9070))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Auk_n_nh4 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled_AUK, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "6 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-07-01','2024-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))

#### n_no3 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Aukstaitija Integrated Monitoring Station", parameter_code == "n_no3")

pred <- itsadug::get_predictions(Auk_results$best_models_Auk_20251128$Auk_soil_chem_int_n_no3_20251128_mod0_GAMM$gam, 
                                 cond=list(depth_cm= c(20, 40), # levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Auk_n_no3 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled_AUK, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "6 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-07-01','2024-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))

#### soil_temp ####
dat = soil_chem_filled %>% 
  filter(site_name == "Aukstaitija Integrated Monitoring Station", parameter_code == "soil_temp")

pred <- itsadug::get_predictions(Auk_results$best_models_Auk$Auk_soil_chem_int_soil_temp_20251128_mod2$gam, 
                                 cond=list(depth_cm= c(5, 10, 20), # levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm ==5 & time < 10596 | depth_cm ==20 & time < 10596))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Auk_soil_temp = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled_AUK, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("Soil temp. (°C)")) +
  scale_x_date(date_breaks = "6 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-07-01','2024-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### swc ####
dat = soil_chem_filled %>% 
  filter(site_name == "Aukstaitija Integrated Monitoring Station", parameter_code == "swc")

pred <- itsadug::get_predictions(Auk_results$best_models_Auk_20251128$Auk_soil_chem_int_swc_20251128_mod4$gam, 
                                 cond=list(depth_cm= c(20, 40), # levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Auk_swc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled_AUK, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("SWC (%)"))+
  scale_x_date(date_breaks = "6 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-07-01','2024-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))

#### fig combined ####
Auk_layout <- "
  111
  22#
  33#
  44#
"

Auk_soil_temp + Auk_swc + Auk_n_nh4 + Auk_n_no3 + #Auk_swc +
  plot_layout(design = Auk_layout) +
  plot_annotation(title = 'Aukstaitija - Lithuania', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 18)))

ggsave("Fig_AUK.png", width = 20, height = 20, units = "cm")


### Figures Bra ####
Bra_results <- list.files(pattern = "best_models_Bra") %>%
  purrr::set_names() %>%
  purrr::map(readRDS) 


#### doc ####
dat = soil_chem_filled %>% 
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "doc")

pred <- itsadug::get_predictions(Bra_results$best_models_Bra$Bra_soil_chem_int_doc_mod1$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm ==10 & time < 10836))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Bra_doc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1992-03-06','2020-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))

#### n_nh4 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "n_nh4", depth_cm %in% c(0, 10))

pred_0_10 <- itsadug::get_predictions(Bra_results$best_models_Bra$Bra_soil_chem_int_n_nh4_depth0cm_10cm_mod1$gam, 
                                 cond=list(depth_cm= c(0, 10), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred_0_10 = pred_0_10 %>% mutate(y=1) %>%
  filter(!(depth_cm ==0 & time < 8784))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "n_nh4", depth_cm %in% c(50, 70))
pred_50_70 <- itsadug::get_predictions(Bra_results$best_models_Bra$Bra_soil_chem_int_n_nh4_depth50cm_70cm_mod1$gam, 
                                      cond=list(depth_cm= c(50, 70), 
                                                time = seq(min(dat$time, na.rm=T),
                                                           max(dat$time, na.rm=T), 
                                                           length = nrow(dat)),
                                                month = 0,
                                                se=T))

pred_50_70 = pred_50_70 %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

pred = bind_rows(pred_0_10, pred_50_70)

dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "n_nh4")

Bra_n_nh4 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1992-03-06','2020-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### n_no3 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "n_no3", depth_cm %in% c(0, 10))

pred_0_10 <- itsadug::get_predictions(Bra_results$best_models_Bra$Bra_soil_chem_int_n_no3_depth0cm_10cm_mod1$gam, 
                                      cond=list(depth_cm= c(0, 10), 
                                                time = seq(min(dat$time, na.rm=T),
                                                           max(dat$time, na.rm=T), 
                                                           length = nrow(dat)),
                                                month = 0,
                                                se=T))

pred_0_10 = pred_0_10 %>% mutate(y=1) %>%
  filter(!(depth_cm ==0 & time < 8554))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "n_no3", depth_cm %in% c(50, 70))
pred_50_70 <- itsadug::get_predictions(Bra_results$best_models_Bra$Bra_soil_chem_int_n_no3_depth50cm_70cm_mod1$gam, 
                                       cond=list(depth_cm= c(50, 70), 
                                                 time = seq(min(dat$time, na.rm=T),
                                                            max(dat$time, na.rm=T), 
                                                            length = nrow(dat)),
                                                 month = 0,
                                                 se=T))

pred_50_70 = pred_50_70 %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

pred = bind_rows(pred_0_10, pred_50_70)

dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "n_no3")

Bra_n_no3 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1992-03-06','2020-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### soil_temp ####
dat = soil_chem_filled %>% 
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "soil_temp", depth_cm %in% c(2, 5))

pred_2_5 <- itsadug::get_predictions(Bra_results$best_models_Bra$Bra_soil_chem_int_soil_temp_depth2cm_5cm_mod4$gam, 
                                      cond=list(depth_cm= c(2, 5), 
                                                time = seq(min(dat$time, na.rm=T),
                                                           max(dat$time, na.rm=T), 
                                                           length = nrow(dat)),
                                                month = 0,
                                                se=T))

pred_2_5 = pred_2_5 %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

dat = soil_chem_filled %>% 
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "soil_temp", depth_cm %in% c(2.5, 12.5, 30, 60))

pred_2_5_12_5_30_60 <- itsadug::get_predictions(Bra_results$best_models_Bra$Bra_soil_chem_int_soil_temp_depth2_5cm_12_5cm_30cm_60cm_mod1$gam, 
                                       cond=list(depth_cm= c(2.5, 12.5, 30, 60), 
                                                 time = seq(min(dat$time, na.rm=T),
                                                            max(dat$time, na.rm=T), 
                                                            length = nrow(dat)),
                                                 month = 0,
                                                 se=T))

pred_2_5_12_5_30_60 = pred_2_5_12_5_30_60 %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

pred = bind_rows(pred_2_5, pred_2_5_12_5_30_60)

dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "soil_temp")

Bra_soil_temp = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_color_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("Soil temp. (°C)"))+
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1992-03-06','2020-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### swc ####
dat = soil_chem_filled %>% 
  filter(site_name == "Brasschaat - De Inslag", parameter_code == "swc")

pred <- itsadug::get_predictions(Bra_results$best_models_Bra$Bra_soil_chem_int_swc_mod3$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Bra_swc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("SWC (%)"))+
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1992-03-06','2020-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))

#### fig combined ####
Bra_layout <- "
  111111
  2222##
  3333##
  4444##
  5555##
"

Bra_soil_temp + Bra_swc + Bra_doc + Bra_n_nh4 + Bra_n_no3 + 
  plot_layout(design = Bra_layout) +
  plot_annotation(title = 'Brasschaat - De Inslag - Belgium', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 18)))

ggsave("Fig_BRA.png", width = 40, height = 25, units = "cm")


### Figures Cai ####
Cai_results <- list.files(pattern = "best_models_Cai") %>%
  purrr::set_names() %>%
  purrr::map(readRDS) 


#### n_nh4 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Cairngorms", parameter_code == "n_nh4")

pred <- itsadug::get_predictions(Cai_results$best_models_Cai$Cai_soil_chem_int_n_nh4_mod1$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Cai_n_nh4 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1999-06-28','2015-12-23'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### n_no3 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Cairngorms", parameter_code == "n_no3")

pred <- itsadug::get_predictions(Cai_results$best_models_Cai$Cai_soil_chem_int_n_no3_mod1$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm == 50 & time > 15476))  %>%
  
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Cai_n_no3 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1999-06-28','2015-12-23'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### soil_temp ####
dat = soil_chem_filled %>% 
  filter(site_name == "Cairngorms", parameter_code == "soil_temp")

pred <- itsadug::get_predictions(Cai_results$best_models_Cai$Cai_soil_chem_int_soil_temp_mod4$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Cai_soil_temp = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("Soil temp. (°C)"))+
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1999-06-28','2015-12-23'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### swc ####
dat = soil_chem_filled %>% 
  filter(site_name == "Cairngorms", parameter_code == "swc")

pred <- itsadug::get_predictions(Cai_results$best_models_Cai$Cai_soil_chem_swc_mod1$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Cai_swc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("SWC (%)"))+
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1999-06-28','2015-12-23'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))

#### fig combined ####
Cai_layout <- "
  11
  2#
  33
  44
"

Cai_soil_temp + Cai_swc + Cai_n_nh4 + Cai_n_no3 + #Cai_swc +
  plot_layout(design = Cai_layout) +
  plot_annotation(title = 'Cairngorms - United Kingdom', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 18)))

ggsave("Fig_CAI.png", width = 15, height = 20, units = "cm")


### Figures Col ####
Col_results <- list.files(pattern = "best_models_Col") %>%
  purrr::set_names() %>%
  purrr::map(readRDS) 


#### doc ####
dat = soil_chem_filled %>% 
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "doc")

pred <- itsadug::get_predictions(Col_results$best_models_Col$Col_soil_chem_int_doc_mod1$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm ==0 & time > 16393 | depth_cm ==55 & time < 10891 | depth_cm ==55 & time > 14973))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Col_doc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))

#### n_nh4 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "n_nh4", depth_cm %in% c(0, 15, 25))

pred_0_15_25 <- itsadug::get_predictions(Col_results$best_models_Col$Col_soil_chem_int_n_nh4_depth0cm_15cm_25cm_mod1$gam, 
                                      cond=list(depth_cm= c(0, 15, 25), 
                                                time = seq(min(dat$time, na.rm=T),
                                                           max(dat$time, na.rm=T), 
                                                           length = nrow(dat)),
                                                month = 0,
                                                se=T))

pred_0_15_25 = pred_0_15_25 %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "n_nh4", depth_cm %in% c(55, 90))
pred_55_90 <- itsadug::get_predictions(Col_results$best_models_Col$Col_soil_chem_int_n_nh4_depth55cm_90cm_mod1$gam, 
                                       cond=list(depth_cm= c(55, 90), 
                                                 time = seq(min(dat$time, na.rm=T),
                                                            max(dat$time, na.rm=T), 
                                                            length = nrow(dat)),
                                                 month = 0,
                                                 se=T))

pred_55_90 = pred_55_90 %>% mutate(y=1) %>%
  filter(!(depth_cm ==0 & time < 10891))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

pred = bind_rows(pred_0_15_25, pred_55_90)

dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "n_nh4")

Col_n_nh4 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-02-23','2018-09-13'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))

#### n_no3 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "n_no3", depth_cm %in% c(0))

pred_0 <- itsadug::get_predictions(Col_results$best_models_Col$Col_soil_chem_int_n_no3_0cm_15cm_25cm_mod1$gam, 
                                         cond=list(depth_cm= c(0), 
                                                   time = seq(min(dat$time, na.rm=T),
                                                              max(dat$time, na.rm=T), 
                                                              length = nrow(dat)),
                                                   month = 0,
                                                   se=T))

pred_0 = pred_0 %>% mutate(y=1) %>%
  filter(!(depth_cm %in% c(15, 25))) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

dat = soil_chem_filled %>% 
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "n_no3", depth_cm %in% c(15, 25))

pred_15_25 <- itsadug::get_predictions(Col_results$best_models_Col$Col_soil_chem_int_n_no3_15cm_25cm_mod1$gam, 
                                         cond=list(depth_cm= c(15, 25), 
                                                   time = seq(min(dat$time, na.rm=T),
                                                              max(dat$time, na.rm=T), 
                                                              length = nrow(dat)),
                                                   month = 0,
                                                   se=T))

pred_15_25 = pred_15_25 %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))


dat = soil_chem_filled %>% 
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "n_no3", depth_cm %in% c(55, 90))
pred_55_90 <- itsadug::get_predictions(Col_results$best_models_Col$Col_soil_chem_int_n_no3_55cm_90cm_mod1$gam, 
                                       cond=list(depth_cm= c(55, 90), 
                                                 time = seq(min(dat$time, na.rm=T),
                                                            max(dat$time, na.rm=T), 
                                                            length = nrow(dat)),
                                                 month = 0,
                                                 se=T))

pred_55_90 = pred_55_90 %>% mutate(y=1) %>%
  filter(!(depth_cm ==0 & time < 10891))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

pred = bind_rows(pred_0, pred_15_25, pred_55_90)

dat = soil_chem_filled %>% 
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "n_no3")

Col_n_no3 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-02-23','2018-09-13'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### soil_temp ####
dat = soil_chem_filled %>% 
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "soil_temp")

pred <- itsadug::get_predictions(Col_results$best_models_Col$Col_soil_chem_int_soil_temp_mod5$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm == 20 & time < 10227 | depth_cm == 20 & time > 14212))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Col_soil_temp = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("Soil temp. (°C)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-02-23','2018-09-13'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))



#### swc ####
dat = soil_chem_filled %>% 
  filter(site_name == "Collelongo-Selva Piana ABR1", parameter_code == "swc")

pred <- itsadug::get_predictions(Col_results$best_models_Col$Col_soil_chem_int_swc_mod4$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm == 2 & time < 14595))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Col_swc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("SWC (%)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1996-02-23','2018-09-13'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))

#### fig combined ####
Col_layout <- "
  111##
  2222#
  33333
  44444
  55555
"

Col_soil_temp + Col_swc + Col_doc + Col_n_nh4 + Col_n_no3 + 
  plot_layout(design = Col_layout) +
  plot_annotation(title = 'Collelongo-Selva Piana - Italy', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 18)))

ggsave("Fig_COL.png", width = 35, height = 25, units = "cm")


### Figures Dav ####
Dav_results <- list.files(pattern = "best_models_Dav") %>%
  purrr::set_names() %>%
  purrr::map(readRDS) 

#### doc ####
dat = soil_chem_filled %>% 
  filter(site_name == "Davos Seehornwald", parameter_code == "doc")

pred <- itsadug::get_predictions(Dav_results$best_models_Dav_20250506$Dav_soil_chem_int_doc_mod0_GAM, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Dav_doc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2006-01-30','2020-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))



#### n_nh4 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Davos Seehornwald", parameter_code == "n_nh4")

pred <- itsadug::get_predictions(Dav_results$best_models_Dav_20250506$Dav_soil_chem_int_n_nh4_mod0_GAM, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Dav_n_nh4 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2006-01-30','2020-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### n_no3 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Davos Seehornwald", parameter_code == "n_no3", depth_cm %in% c(0, 15, 50))

pred_0_15_50 <- itsadug::get_predictions(Dav_results$best_models_Dav_20250506$Dav_soil_chem_int_n_no3_depth0cm_15cm_50cm_mod0_GAM, 
                                      cond=list(depth_cm= c(0, 15, 50), 
                                                time = seq(min(dat$time, na.rm=T),
                                                           max(dat$time, na.rm=T), 
                                                           length = nrow(dat)),
                                                month = 0,
                                                se=T))

pred_0_15_50 = pred_0_15_50 %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

dat = soil_chem_filled %>% 
  filter(site_name == "Davos Seehornwald", parameter_code == "n_no3", depth_cm %in% c(80))

pred_80 <- itsadug::get_predictions(Dav_results$best_models_Dav$Dav_soil_chem_n_no3_depth80cm_mod0_GAM, 
                                       cond=list(depth_cm= c(80), 
                                                 time = seq(min(dat$time, na.rm=T),
                                                            max(dat$time, na.rm=T), 
                                                            length = nrow(dat)),
                                                 month = 0,
                                                 se=T))

pred_80 = pred_80 %>% mutate(y=1) %>%
  mutate(depth_cm = 80) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

pred = bind_rows(pred_0_15_50, pred_80)

dat = soil_chem_filled %>% 
  filter(site_name == "Davos Seehornwald", parameter_code == "n_no3")

Dav_n_no3 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2006-01-30','2020-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))



#### soil_temp ####
dat = soil_chem_filled %>% 
  filter(site_name == "Davos Seehornwald", parameter_code == "soil_temp")

pred <- itsadug::get_predictions(Dav_results$best_models_Dav_20250506$Dav_soil_chem_int_soil_temp_20250506_mod0_GAMM$gam, 
                                 cond=list(depth_cm= c(2.6, 5.2, 10, 20, 30, 40), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm == 5.2 & time < 16786 | depth_cm == 10 & time < 16786 | depth_cm == 20 & time < 16786 | depth_cm == 30 & time < 16786| depth_cm == 40 & time < 16786))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Dav_soil_temp = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("Soil temp. (°C)"))+
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2006-01-30','2020-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))



#### swc ####
dat = soil_chem_filled %>% 
  filter(site_name == "Davos Seehornwald", parameter_code == "swc")

pred <- itsadug::get_predictions(Dav_results$best_models_Dav_20250506$Dav_soil_chem_int_swc_20250506_mod1$gam, 
                                 cond=list(depth_cm= c(5, 10, 20, 30, 40), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm == 10 & time < 16786 | depth_cm == 20 & time < 16786 | depth_cm == 30 & time < 16786 | depth_cm == 40 & time < 16786))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Dav_swc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("SWC (%)"))+
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2006-01-30','2020-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### fig combined ####
Dav_layout <- "
  111111
  22222#
  3333##
  4444##
  5555##
"

Dav_soil_temp + Dav_swc + Dav_doc + Dav_n_nh4 + Dav_n_no3 + 
  plot_layout(design = Dav_layout) +
  plot_annotation(title = 'Davos Seehornwald - Switzerland', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 18)))

ggsave("Fig_DAV.png", width = 30, height = 25, units = "cm")



### Figures Lae ####
Lae_results <- list.files(pattern = "best_models_Lae") %>%
  purrr::set_names() %>%
  purrr::map(readRDS) 

#### doc ####
dat = soil_chem_filled %>% 
  filter(site_name == "Laegeren", parameter_code == "doc")

pred <- itsadug::get_predictions(Lae_results$best_models_Lae_20250506$Lae_soil_chem_int_doc_mod0_GAM, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm == 15 & time > 17645 | depth_cm == 50 & time > 17700 | depth_cm == 80 & time > 17729))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Lae_doc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2004-08-24','2023-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### n_nh4 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Laegeren", parameter_code == "n_nh4")

pred <- itsadug::get_predictions(Lae_results$best_models_Lae_20250506$Lae_soil_chem_int_n_nh4_mod0_GAM, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Lae_n_nh4 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2004-08-24','2023-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### n_no3 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Laegeren", parameter_code == "n_no3")

pred <- itsadug::get_predictions(Lae_results$best_models_Lae_20250506$Lae_soil_chem_int_n_no3_mod0_GAM, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Lae_n_no3 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2004-08-24','2023-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### soil_temp ####
dat = soil_chem_filled %>% 
  filter(site_name == "Laegeren", parameter_code == "soil_temp")

pred <- itsadug::get_predictions(Lae_results$best_models_Lae_20250506$Lae_soil_chem_int_soil_temp_20250506_mod1$gam, 
                                 cond=list(depth_cm= c(2.6, 5.2, 10, 20, 30), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))


pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm == 20 & time > 14688 | depth_cm == 30 & time > 14688))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Lae_soil_temp = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("Soil temp. (°C)"))+
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2004-08-24','2023-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### swc ####
dat = soil_chem_filled %>% 
  filter(site_name == "Laegeren", parameter_code == "swc")

pred <- itsadug::get_predictions(Lae_results$best_models_Lae_20250506$Lae_soil_chem_int_swc_20250506_mod1$gam, 
                                 cond=list(depth_cm= c(5, 10, 20, 30), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  #filter(!(depth_cm == 20 & time > 16084))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Lae_swc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("SWC (%)"))+
  scale_x_date(date_breaks = "3 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('2004-08-24','2023-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### fig combined ####
Lae_layout <- "
  11111
  2222#
  3333#
  4444#
  5555#
"

Lae_soil_temp + Lae_swc + Lae_doc + Lae_n_nh4 + Lae_n_no3 + 
  plot_layout(design = Lae_layout) +
  plot_annotation(title = 'Lägeren - Switzerland', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 18)))

ggsave("Fig_LAE.png", width = 35, height = 25, units = "cm")


### Figures Moo ####
Moo_results <- list.files(pattern = "best_models_Moo") %>%
  purrr::set_names() %>%
  purrr::map(readRDS) 

#### doc ####
dat = soil_chem_filled %>% 
  filter(site_name == "Moor House Upper Teesdale", parameter_code == "doc")

pred <- itsadug::get_predictions(Moo_results$best_models_Moo$Moo_soil_chem_int_doc_mod4$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Moo_doc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1991-05-28','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### n_nh4 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Moor House Upper Teesdale", parameter_code == "n_nh4")

pred <- itsadug::get_predictions(Moo_results$best_models_Moo$Moo_soil_chem_int_n_nh4_mod1$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Moo_n_nh4 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1991-05-28','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### n_no3 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Moor House Upper Teesdale", parameter_code == "n_no3")

pred <- itsadug::get_predictions(Moo_results$best_models_Moo$Moo_soil_chem_int_n_no3_mod1$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Moo_n_no3 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1991-05-28','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))



#### soil_temp ####
dat = soil_chem_filled %>% 
  filter(site_name == "Moor House Upper Teesdale", parameter_code == "soil_temp")

pred <- itsadug::get_predictions(Moo_results$best_models_Moo$Moo_soil_chem_int_soil_temp_mod4$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Moo_soil_temp = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("Soil temp. (°C)"))+
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1991-05-28','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### fig combined ####
Moo_layout <- "
  11
  22
  33
  44
  
"

Moo_soil_temp + Moo_doc + Moo_n_nh4 + Moo_n_no3 + #Moo_soil_temp +
  plot_layout(design = Moo_layout) +
  plot_annotation(title = 'Moor House - Upper Teesdale - United Kingdom', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 18)))

ggsave("Fig_MOO.png", width = 15, height = 20, units = "cm")


### Figures Wyt ####
Wyt_results <- list.files(pattern = "best_models_Wyt") %>%
  purrr::set_names() %>%
  purrr::map(readRDS) 

#### doc ####
dat = soil_chem_filled %>% 
  filter(site_name == "Wytham", parameter_code == "doc")

pred <- itsadug::get_predictions(Wyt_results$best_models_Wyt$Wyt_soil_chem_int_doc_mod4$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Wyt_doc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-06-09','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### n_nh4 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Wytham", parameter_code == "n_nh4")

pred <- itsadug::get_predictions(Wyt_results$best_models_Wyt$Wyt_soil_chem_int_n_nh4_mod4$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Wyt_n_nh4 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-06-09','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### n_no3 ####
dat = soil_chem_filled %>% 
  filter(site_name == "Wytham", parameter_code == "n_no3")

pred <- itsadug::get_predictions(Wyt_results$best_models_Wyt$Wyt_soil_chem_int_n_no3_mod1$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Wyt_n_no3 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-06-09','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### soil_temp ####
dat = soil_chem_filled %>% 
  filter(site_name == "Wytham", parameter_code == "soil_temp")

pred <- itsadug::get_predictions(Wyt_results$best_models_Wyt$Wyt_soil_chem_int_soil_temp_mod4$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Wyt_soil_temp = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("Soil temp. (°C)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-06-09','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### swc ####
dat = soil_chem_filled %>% 
  filter(site_name == "Wytham", parameter_code == "swc")

pred <- itsadug::get_predictions(Wyt_results$best_models_Wyt$Wyt_soil_chem_swc_mod2$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Wyt_swc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("SWC (%)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-06-09','2015-12-31'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))

#### fig combined ####
Wyt_layout <- "
  11
  2#
  33
  44
  55
"

Wyt_soil_temp + Wyt_swc + Wyt_doc + Wyt_n_nh4 + Wyt_n_no3 + 
  plot_layout(design = Wyt_layout) +
  plot_annotation(title = 'Wytham - United Kingdom', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 18)))


ggsave("Fig_WYT.png", width = 15, height = 25, units = "cm")






### Figures Zoe IP2 ####
Zoe_results_IP123 <- list.files(pattern = "best_models_ZOE_IP123") %>%
  purrr::set_names() %>%
  purrr::map(readRDS) 

#### doc ####
dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "LTER Zöbelboden", parameter_code == "doc",  code_plot_agg =="IP2", depth_cm %in% c(5, 6, 10))

pred <- itsadug::get_predictions(Zoe_results_IP123$best_models_ZOE_IP123_20250506$LTE_soil_chem_int_doc_5cm_6cm_10cm_IP2_mod4$gam, 
                                 cond=list(depth_cm= c(5, 6, 10), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm == 6 & time > 17714 | depth_cm == 10 & time > 17867))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Zoe_IP2_doc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1998-11-01','2019-12-01'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### n_nh4 ####
dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "LTER Zöbelboden", parameter_code == "n_nh4",  code_plot_agg =="IP2", depth_cm %in% c(5, 6, 10))

pred <- itsadug::get_predictions(Zoe_results_IP123$best_models_ZOE_IP123_20250506$LTE_soil_chem_int_n_nh4_5cm_6cm_10cm_IP2_mod4$gam, 
                                      cond=list(depth_cm= c(5, 6, 10), 
                                                time = seq(min(dat$time, na.rm=T),
                                                           max(dat$time, na.rm=T), 
                                                           length = nrow(dat)),
                                                month = 0,
                                                se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm == 6 & time > 17867 | depth_cm == 10 & time > 17867))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))


Zoe_IP2_n_nh4 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1998-11-01','2019-12-01'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))



#### n_no3 ####
dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "LTER Zöbelboden", parameter_code == "n_no3",  code_plot_agg =="IP2", depth_cm %in% c(5, 6, 10)) 

pred <- itsadug::get_predictions(Zoe_results_IP123$best_models_ZOE_IP123_20250506$LTE_soil_chem_int_n_no3_5cm_6cm_10cm_IP2_mod4$gam, 
                                      cond=list(depth_cm= c(5, 6, 10), 
                                                time = seq(min(dat$time, na.rm=T),
                                                           max(dat$time, na.rm=T), 
                                                           length = nrow(dat)),
                                                month = 0,
                                                se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm == 10 & time > 17867 | depth_cm == 6 & time > 17867 ))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Zoe_IP2_n_no3 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1998-11-01','2019-12-01'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### soil_temp ####
dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "LTER Zöbelboden", parameter_code == "soil_temp",  code_plot_agg =="IP2")

pred <- itsadug::get_predictions(Zoe_results_IP123$best_models_ZOE_IP123_20250506$LTE_soil_chem_int_soil_temp_IP1_IP3_20250506_mod2$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Zoe_IP2_soil_temp = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("Soil temp. (°C)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1998-11-01','2019-12-01'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))



#### swc ####
dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "LTER Zöbelboden", parameter_code == "swc",  code_plot_agg =="IP2", depth_cm %in% c(5, 12))%>%
  filter(!(depth_cm == 5 & time > 15872 | depth_cm == 12 & time > 15872) )

pred <- itsadug::get_predictions(Zoe_results_IP123$best_models_ZOE_IP123_20250506$LTE_soil_chem_int_swc_5cm_12cm_IP2_mod2$gam, 
                                 cond=list(depth_cm= c(5, 12), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm == 12 & time > 15872 |  depth_cm == 5 & time > 15872))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))


Zoe_IP2_swc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("SWC (%)"))+
  scale_x_date(date_breaks = "4 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1998-11-01','2019-12-01'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))



#### fig combined ####
Zoe_IP2_layout <- "
  1##
  22#
  333
  444
  555
"

Zoe_IP2_soil_temp + Zoe_IP2_swc + Zoe_IP2_doc + Zoe_IP2_n_nh4 + Zoe_IP2_n_no3 + 
  plot_layout(design = Zoe_IP2_layout) +
  plot_annotation(title = 'Zöbelboden - Austria - IP2', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 18)))


ggsave("Fig_ZOE_IP2.png", width = 20, height = 25, units = "cm")




### Figures Zoe IP1_IP3 ####

#### doc ####
dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "LTER Zöbelboden", parameter_code == "doc",  code_plot_agg =="IP1_IP3", depth_cm %in% c(6.75, 13, 15, 30, 32.5, 45, 90.5))

pred <- itsadug::get_predictions(Zoe_results_IP123$best_models_ZOE_IP123_20250506$LTE_soil_chem_int_doc_6_75cm13cm15cm30cm32_5cm35cm45cm90_5_IP1_IP3_mod4$gam, 
                                 cond=list(depth_cm= c(6.75, 13, 15, 30, 32.5, 45, 90.5), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm == 6.75 & time < 14092 | depth_cm == 13 & time < 11688 | depth_cm == 13 & time > 14488 | 
             depth_cm == 15 & time > 10531 | depth_cm == 30 & time > 14488| depth_cm == 32.5 & time < 14092 |
             depth_cm == 45 & time > 10531| depth_cm == 90.5 & time < 14092))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))


Zoe_IP1_IP3_doc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("DOC (mg/l)"))+
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-08-01','2019-06-25'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### n_nh4 ####
dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "LTER Zöbelboden", parameter_code == "n_nh4",  code_plot_agg =="IP1_IP3", depth_cm %in% c(6.75, 13, 15, 30))

pred_6_75cm_13cm_15cm_30 <- itsadug::get_predictions(Zoe_results_IP123$best_models_ZOE_IP123_20250506$LTE_soil_chem_int_n_nh4_6_75cm_13cm_15cm_30cm_IP1_IP3_mod4$gam, 
                                      cond=list(depth_cm= c(6.75, 13, 15, 30), 
                                                time = seq(min(dat$time, na.rm=T),
                                                           max(dat$time, na.rm=T), 
                                                           length = nrow(dat)),
                                                month = 0,
                                                se=T))

pred_6_75cm_13cm_15cm_30 = pred_6_75cm_13cm_15cm_30 %>% mutate(y=1) %>%
  filter(!(depth_cm == 6.75 & time < 14092 | depth_cm == 13 & time < 10531 | depth_cm == 13 & time > 14488 |
             depth_cm == 15 & time > 10531 | depth_cm == 30 & time > 14488 ))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

dat = soil_chem_filled %>% 
  #filter(!depth_cm == 35) %>%
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "LTER Zöbelboden", parameter_code == "n_nh4", depth_cm %in% c(32.5, 35, 45, 90.5, 104))
pred_32_5cm_35cm_45cm_90_5cm_104cm <- itsadug::get_predictions(Zoe_results_IP123$best_models_ZOE_IP123_20250506$LTE_soil_chem_int_n_nh4_32_5cm_35cm_45cm_90_5_104_IP1_IP3_mod4$gam, 
                                             cond=list(depth_cm= c(32.5, 35, 45, 90.5, 104), 
                                                       time = seq(min(dat$time, na.rm=T),
                                                                  max(dat$time, na.rm=T), 
                                                                  length = nrow(dat)),
                                                       month = 0,
                                                       se=T))

pred_32_5cm_35cm_45cm_90_5cm_104cm = pred_32_5cm_35cm_45cm_90_5cm_104cm %>% mutate(y=1) %>%
  filter(!(depth_cm == 32.5 & time < 14092 | depth_cm == 35 & time < 10531 | depth_cm == 35 & time > 14245 | depth_cm == 45 & time > 10531 | depth_cm == 90.5 & time < 14092 | depth_cm == 104 & time < 10682 | depth_cm == 104 & time > 14245))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))


pred = bind_rows(pred_6_75cm_13cm_15cm_30, pred_32_5cm_35cm_45cm_90_5cm_104cm)

dat = soil_chem_filled %>% 
  #filter(!depth_cm == 35) %>%
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "LTER Zöbelboden", parameter_code == "n_nh4", depth_cm %in% c(6.75, 13, 15, 30, 32.5, 35, 45, 90.5, 104))

Zoe_IP1_IP3_n_nh4 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NH"[4]^{"+"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-08-01','2019-06-25'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))



#### n_no3 ####
dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "LTER Zöbelboden", parameter_code == "n_no3",  code_plot_agg =="IP1_IP3", depth_cm %in% c(6.75, 13, 15, 30)) 

pred_6_75cm_13cm_15cm_30cm <- itsadug::get_predictions(Zoe_results_IP123$best_models_ZOE_IP123_20250506$LTE_soil_chem_int_n_no3_6_75cm_13cm_15cm_30cm_IP1_IP3_mod0_GAM, 
                                      cond=list(depth_cm= c(6.75, 13, 15, 30), 
                                                time = seq(min(dat$time, na.rm=T),
                                                           max(dat$time, na.rm=T), 
                                                           length = nrow(dat)),
                                                month = 0,
                                                se=T))

pred_6_75cm_13cm_15cm_30cm = pred_6_75cm_13cm_15cm_30cm %>% mutate(y=1) %>%
  filter(!(depth_cm == 6.75 & time < 14092 | depth_cm == 13 & time < 10531 | depth_cm == 13 & time > 14488 |
             depth_cm == 15 & time > 10531 | depth_cm == 30 & time > 14488 ))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))


dat = soil_chem_filled %>% 
  filter(!depth_cm == 35) %>%
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "LTER Zöbelboden", parameter_code == "n_no3", depth_cm %in% c(32.5, 35, 45, 90.5, 104))

pred_32_5cm35cm45cm90_5_104 <- itsadug::get_predictions(Zoe_results_IP123$best_models_ZOE_IP123_20250506$LTE_soil_chem_int_n_no3_32_5cm35cm45cm90_5_104_IP1_IP3_mod0_GAM, 
                                             cond=list(depth_cm= c(32.5, 35, 45, 90.5, 104), 
                                                       time = seq(min(dat$time, na.rm=T),
                                                                  max(dat$time, na.rm=T), 
                                                                  length = nrow(dat)),
                                                       month = 0,
                                                       se=T))

pred_32_5cm35cm45cm90_5_104 = pred_32_5cm35cm45cm90_5_104 %>% mutate(y=1) %>%
  filter(!(depth_cm == 32.5 & time < 14092 | 
             depth_cm == 35 & time < 10531 | depth_cm == 35 & time > 14488 | depth_cm == 45 & time > 10531 |
             depth_cm == 90.5 & time < 14092 | depth_cm == 104 & time < 10712 | depth_cm == 104 & time > 14488))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

pred = bind_rows(pred_6_75cm_13cm_15cm_30cm, pred_32_5cm35cm45cm90_5_104)

dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "LTER Zöbelboden", parameter_code == "n_no3",  code_plot_agg =="IP1_IP3", depth_cm %in% c(6.75, 13, 15, 30, 32.5, 35, 45, 90.5, 104)) 

Zoe_IP1_IP3_n_no3 = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("NO"[3]^{"−"}~"-N (mg/l)"))+
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-08-01','2019-06-25'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))


#### soil_temp ####
dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "LTER Zöbelboden", parameter_code == "soil_temp",  code_plot_agg =="IP1_IP3")

pred <- itsadug::get_predictions(Zoe_results_IP123$best_models_ZOE_IP123_20250506$LTE_soil_chem_int_soil_temp_IP1_IP3_20250506_mod2$gam, 
                                 cond=list(depth_cm= levels(factor(dat$depth_cm)), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))

Zoe_IP1_IP3_soil_temp = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("Soil temp. (°C)"))+
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-08-01','2019-06-25'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))



#### swc ####
dat = soil_chem_filled %>% 
  dplyr::mutate(site_name = case_when( 
    grepl("belboden", site_name) ~ "LTER Zöbelboden", 
    TRUE ~ site_name
  )) %>%
  filter(site_name == "LTER Zöbelboden", parameter_code == "swc",  code_plot_agg =="IP1_IP3", depth_cm %in% c(5, 15, 40, 58))

pred <- itsadug::get_predictions(Zoe_results_IP123$best_models_ZOE_IP123_20250506$LTE_soil_chem_int_swc_5cm_15cm_40cm_58_IP1_IP3_mod1$gam, 
                                 cond=list(depth_cm= c(5, 15, 40, 58), 
                                           time = seq(min(dat$time, na.rm=T),
                                                      max(dat$time, na.rm=T), 
                                                      length = nrow(dat)),
                                           month = 0,
                                           se=T))

pred = pred %>% mutate(y=1) %>%
  filter(!(depth_cm == 15 & time < 14076 | depth_cm == 40 & time < 14076 | depth_cm == 58 & time > 15464 ))  %>%
  mutate(fit = case_when(
    fit < 0 ~ 0,
    TRUE ~ fit
  ))


Zoe_IP1_IP3_swc = ggplot() + 
  geom_line(data = dat, alpha=.3, group = as.factor(dat$depth_cm), aes(x=as.Date(time), y = value_filled, color = depth_range)) + 
  geom_point(data = dat,alpha=.3, aes(x=as.Date(time), y = value_filled, color = depth_range, fill = depth_range)) +
  scale_colour_manual(values = cols) +
  geom_ribbon(data = pred, alpha=.4, aes(x = as.Date(time), ymin = fit-CI, ymax = fit+CI), show.legend = F, fill='black') +
  geom_line(data = pred, aes(x = as.Date(time), y=fit), show.legend = F, color='black') +
  facet_grid(. ~ depth_cm) +
  ylab(expression("SWC (%)"))+
  scale_x_date(date_breaks = "5 year", date_labels =  "%Y", expand = c(0.02, 0.02),
               limits = as.Date(c('1993-08-01','2019-06-25'))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = 10),  
        axis.title.x = element_blank(),
        strip.text = element_text(colour = 'black'))



#### fig combined ####
Zoe_IP1_IP3_layout <- "
  1########
  2222#####
  3333333##
  444444444
  555555555
"

Zoe_IP1_IP3_soil_temp + Zoe_IP1_IP3_swc + Zoe_IP1_IP3_doc + Zoe_IP1_IP3_n_nh4 + Zoe_IP1_IP3_n_no3 + 
  plot_layout(design = Zoe_IP1_IP3_layout) +
  plot_annotation(title = 'Zöbelboden - Austria - IP1 & IP3', tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 18)))

ggsave("Fig_ZOE_IP1_3.png", width = 60, height = 25, units = "cm")


