require(chillR)
require(tidyverse)
Bonn_temps <- read_tab("data/Bonn_temps.csv")

location = c(7.1, 50.8)

area <- c(52, 6, 50, 8)


download_cmip6_ecmwfr(
  scenarios = 'ssp126',
  area =  area,
  user = 'write user id here',
  key = 'write key here',
  model = 'default',
  frequency = 'monthly',
  variable = c('Tmin', 'Tmax'),
  year_start = 2015,
  year_end = 2100)



download_cmip6_ecmwfr(
  scenarios = c("ssp126", "ssp245", "ssp370", "ssp585"),
  area = area,
  user = 'write user id here'
  key = 'write key here',
  model = 'default',
  frequency = 'monthly',
  variable = c('Tmin', 'Tmax'),
  year_start = 2015,
  year_end = 2100)


download_baseline_cmip6_ecmwfr(
  area = area,
  user = 'write user id here'
  key = 'write key here',
  model = 'match_downloaded',
  frequency = 'monthly',
  variable = c('Tmin', 'Tmax'),
  year_start = 1986,
  year_end = 2014,
  month = 1:12)

station <- data.frame(
  station_name = c("Bonn"),
  longitude = c(7.1),
  latitude = c(50.8))

extracted <- extract_cmip6_data(stations = station)

library(ncdf4)
library(PCICt)

extracted <- extract_cmip6_data(stations = station)


head(extracted$`ssp126_AWI-CM-1-1-MR`)



change_scenarios <- 
  gen_rel_change_scenario(extracted,
                          scenarios = c(2050, 2085),
                          reference_period = c(1986:2014),
                          future_window_width = 30)
head(change_scenarios)


write.csv(change_scenarios, "data/all_change_scenarios.csv", row.names = FALSE)

change_scenarios <- read.csv("data/all_change_scenarios.csv")


scen_list <- convert_scen_information(change_scenarios)

scen_frame <- convert_scen_information(scen_list)

scen_list$Bonn$ssp126$`ACCESS-CM2`$'2050'




temps_1996 <- temperature_scenario_from_records(Bonn_temps,
                                                1996)
temps_2000 <- temperature_scenario_from_records(Bonn_temps,
                                                2000)
temps_1996
temps_2000


base <- temperature_scenario_baseline_adjustment(temps_1996,
                                                 temps_2000)

base


scen_list <- convert_scen_information(change_scenarios,
                                      give_structure = FALSE)

adjusted_list <- temperature_scenario_baseline_adjustment(base,
                                                          scen_list,
                   temperature_check_args = 
                     list(scenario_check_thresholds = c(-5, 15)))


temps <- temperature_generation(Bonn_temps,
                                years = c(1973, 2019),
                                sim_years = c(2001, 2100),
                                adjusted_list,  
                                temperature_check_args = 
                                  list( scenario_check_thresholds = c(-5, 15)))

save_temperature_scenarios(temps,
                           "data/future_climate","Bonn_future")

temps <- load_temperature_scenarios("data/future_climate","Bonn_future_")

# now we have temperature scenarios

frost_model <- function(x)
  step_model(x,
             data.frame(
               lower = c(-1000, 0),
               upper = c(0, 1000),
               weight = c(1, 0)))

models <- list(Chill_Portions = Dynamic_Model,
               GDH = GDH,
               Frost_H = frost_model)



chill_future_scenario_list <- 
  tempResponse_daily_list(temps,
                          latitude = 50.8,
                          Start_JDay = 305,
                          End_JDay = 59,
                          models = models)

chill_future_scenario_list <- 
  lapply(chill_future_scenario_list,
         function(x) x %>%
           filter(Perc_complete == 100))

save_temperature_scenarios(chill_future_scenario_list,
                           "data/future_climate",
                           "Bonn_futurechill")

# this would be for spring frost, but we're not running this
chill_future_scenario_list_frost <- 
  tempResponse_daily_list(temps,
                          latitude = 50.8,
                          Start_JDay = 75,
                          End_JDay = 120,
                          models = list(GDH = GDH,
                                        Frost_H = frost_model))

chill_future_scenario_list_frost <- 
  lapply(chill_future_scenario_list_frost,
         function(x) x %>%
           filter(Perc_complete == 100))


save_temperature_scenarios(chill_future_scenario_list_frost,
                           "data/future_climate",
                           "Bonn_futurefrost")

# now we should all the agroclimatic metrics

# THIS IS WHERE WE LEFT OFF ON 19th NOVEMBER


# simulate for the observed record

Bonn_temps <- read_tab("data/Bonn_temps.csv")

observed_chill <- tempResponse_daily_list(Bonn_temps,
                                          latitude = 50.8,
                                          Start_JDay = 305,
                                          End_JDay = 59,
                                          models = models)

write.csv(observed_chill, "data/Bonn_observed_chill_305_59.csv")

# simulate chill for historic scenarios

hist_temps <- load_temperature_scenarios("data","all_past_temps")



chill_hist_scenario_list <- tempResponse_daily_list(hist_temps,
                                          latitude = 50.8,
                                          Start_JDay = 305,
                                          End_JDay = 59,
                                          models = models)

chill_hist_scenario_list <- lapply(chill_hist_scenario_list,
                                     function(x) x %>%
                                       filter(Perc_complete == 100))

save_temperature_scenarios(chill_hist_scenario_list,
                           "data/future_climate",
                           "Bonn_hist_chill_305_59")

chill_future_scenario_list <- load_temperature_scenarios("data/future_climate","Bonn_futurechill")
chill_hist_scenario_list<-load_temperature_scenarios("data","Bonn_hist_chill_305_59")
observed_chill <- read_tab("data/Bonn_observed_chill_305_59.csv")


# prepare for plotting

chills <- make_climate_scenario(
  metric_summary = chill_hist_scenario_list,
  caption = "Historical",
  historic_data = observed_chill,
  time_series = TRUE)

plot_climate_scenarios(
  climate_scenario_list = chills,
  metric = "Chill_Portions",
  metric_label = "Chill (Chill Portions)")



list_ssp <- 
  strsplit(names(chill_future_scenario_list),
           '\\.') %>%
  map(2) %>%
  unlist()

list_gcm <-
  strsplit(names(chill_future_scenario_list), '\\.') %>%
  map(3) %>%
  unlist()

list_time <-
  strsplit(names(chill_future_scenario_list), '\\.') %>%
  map(4) %>%
  unlist()

SSPs <- c("ssp126", "ssp245", "ssp585")
Times <- c(2050, 2085)


for(SSP in SSPs)
  for(Time in Times)
    {
    
    # find all scenarios for the ssp and time
    chill <- chill_future_scenario_list[list_ssp == SSP &
                                          list_time == Time]
    names(chill) <- list_gcm[list_ssp == SSP &
                               list_time == Time]
    if(SSP == "ssp126") SSPcaption <- "SSP1"
    if(SSP == "ssp245") SSPcaption <- "SSP2"
    if(SSP == "ssp585") SSPcaption <- "SSP5"    
    chills <- chill %>% 
      make_climate_scenario(
        caption = c(SSPcaption,
                    Time),
        add_to = chills)
}



info_chill <-
  plot_climate_scenarios(
    climate_scenario_list = chills,
    metric = "Chill_Portions",
    metric_label = "Chill (Chill Portions)",
    texcex = 1)

info_heat <-
  plot_climate_scenarios(
    climate_scenario_list = chills,
    metric = "GDH",
    metric_label = "Heat (Growing Degree Hours)",
    texcex = 1)

info_frost <- 
  plot_climate_scenarios(  
    climate_scenario_list=chills,
    metric="Frost_H",
    metric_label="Frost incidence (hours)",
    texcex=1)



info_chill[[2]]

