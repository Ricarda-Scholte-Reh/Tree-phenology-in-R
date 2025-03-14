rm(list=ls())

require(chillR)
require(tidyverse)


# Here's the call from the earlier lesson. We don't have to run this again.
Temp <- temperature_generation(KA_weather,
                               years = c(1998, 2005),
                               sim_years = c(2001, 2100))

# Now we make a temperature scenario that raises all temperatures by 2°C

change_scenario <- data.frame(Tmin = rep(2, 12),
                              Tmax = rep(2, 12))

change_scenario


Temp_2 <- temperature_generation(KA_weather,
                                 years = c(1998, 2005),
                                 sim_years = c(2001, 2100),
                                 temperature_scenario = change_scenario)

# As before, we'll make a data.frame that contains all
# our data, so we can take a look at it.

Temperature_scenarios <- KA_weather %>%
  filter(Year %in% 1998:2005) %>%
  cbind(Data_source = "observed") %>%
  rbind(Temp[[1]] %>%
          select(c(Year, Month, Day, Tmin, Tmax)) %>%
          cbind(Data_source = "simulated")
        ) %>%
  rbind(Temp_2[[1]] %>%
          select(c(Year, Month, Day, Tmin, Tmax)) %>%
          cbind(Data_source = "Warming_2C")
        ) %>%
  mutate(Date = as.Date(ISOdate(2000,
                                Month,
                                Day)))


ggplot(data = Temperature_scenarios, 
       aes(Date, Tmin)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
  theme_bw(base_size = 15) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")

ggplot(data = Temperature_scenarios,
       aes(Date,Tmax)) +
  geom_smooth(aes(colour = factor(Year))) +
  facet_wrap(vars(Data_source)) +
  theme_bw(base_size = 15) +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b")


# download weather station list for the vicinity of Bonn
station_list <- handle_gsod(action = "list_stations",
                            location=c(7.1,50.8))

# download weather data for Cologne/Bonn airport and convert it to chillR format
Bonn_weather <- handle_gsod(action = "download_weather",
                            location = station_list$chillR_code[1],
                            time_interval = c(1973,2019)) %>%
  handle_gsod()

# check record for missing data
fix_weather(Bonn_weather$`KOLN BONN`)$QC

# (incidentally almost all gaps are for years covered by the KA_weather dataset)
Bonn_patched <- patch_daily_temperatures(
  weather = Bonn_weather$`KOLN BONN`,
  patch_weather = list(KA_weather))

fix_weather(Bonn_patched)$QC

# There are still 4 days missing here, out of 47 years -
# let's simply interpolate these gaps now

Bonn<-fix_weather(Bonn_patched)

Bonn_temps<-Bonn$weather


scenario_1980 <- temperature_scenario_from_records(weather = Bonn_temps,
                                                   year = 1980)

scenario_1980$'1980'$data
scenario_1980
str(scenario_1980)


temps_1980 <- temperature_generation(weather = Bonn_temps,
                                     years = c(1973, 2019),
                                     sim_years = c(2001, 2100),
                                     temperature_scenario = scenario_1980)

scenario_1996 <- temperature_scenario_from_records(weather = Bonn_temps,
                                                   year = 1996)
scenario_1996$'1996'$data
scenario_1996
str(scenario_1996)

relative_scenario <- temperature_scenario_baseline_adjustment(
  baseline = scenario_1996,
  temperature_scenario = scenario_1980)



temps_1980 <- temperature_generation(weather = Bonn_temps,
                                     years = c(1973, 2019),
                                     sim_years = c(2001, 2100),
                                     temperature_scenario = relative_scenario)




all_past_scenarios <- temperature_scenario_from_records(
  weather = Bonn_temps,
  year = c(1980,
           1990,
           2000,
           2010))

adjusted_scenarios <- temperature_scenario_baseline_adjustment(
  baseline = scenario_1996,
  temperature_scenario = all_past_scenarios)

```{r, eval=FALSE}

all_past_scenario_temps <- temperature_generation(
  weather = Bonn_temps,
  years = c(1973,2019),
  sim_years = c(2001,2100),
  temperature_scenario = adjusted_scenarios)

```
```{r, eval=FALSE, echo=FALSE}

save_temperature_scenarios(all_past_scenario_temps,"data","all_past_temps")
```

```{r, echo=FALSE}

all_past_scenario_temps <- load_temperature_scenarios("data","all_past_temps")
```

frost_model <- function(x)
  step_model(x,
             data.frame(
               lower=c(-1000, 0),
               upper=c(0, 1000),
               weight=c(1, 0)))

models <- list(Chill_Portions = Dynamic_Model,
               GDH = GDH,
               Frost_H = frost_model)






chill_hist_scenario_list <- tempResponse_daily_list(all_past_scenario_temps,
                                                    latitude = 50.9,
                                                    Start_JDay = 305,
                                                    End_JDay = 59,
                                                    models = models)



scenarios <- names(chill_hist_scenario_list)[1:4]


all_scenarios <- chill_hist_scenario_list[[scenarios[1]]] %>%
  mutate(scenario = as.numeric(scenarios[1]))


for (sc in scenarios[2:4])
 all_scenarios <- all_scenarios %>%
  rbind(chill_hist_scenario_list[[sc]] %>%
          cbind(
            scenario=as.numeric(sc))
        ) %>%
  filter(Perc_complete == 100)

# Let's compute the actual 'observed' chill for comparison
actual_chill <- tempResponse_daily_list(Bonn_temps,
                                        latitude = 50.9,
                                        Start_JDay = 305,
                                        End_JDay = 59,
                                        models)[[1]] %>%
  filter(Perc_complete == 100)

ggplot(data = all_scenarios,
       aes(scenario,
           Chill_Portions,
           fill = factor(scenario))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  ylab("Chill accumulation (Chill Portions)") +
  xlab("Scenario year") +
  theme_bw(base_size = 10) +
  ylim(c(0,90)) +
  geom_point(data = actual_chill,
             aes(End_year,
                 Chill_Portions,
                 fill = "blue"),
             col = "blue",
             show.legend = FALSE) +
  scale_fill_discrete(name = "Scenario",
                      breaks = unique(all_scenarios$scenario)) 


temperature_means <- 
  data.frame(Year = min(Bonn_temps$Year):max(Bonn_temps$Year),
             Tmin = aggregate(Bonn_temps$Tmin,
                              FUN = "mean",
                              by = list(Bonn_temps$Year))[,2],
             Tmax=aggregate(Bonn_temps$Tmax,
                            FUN = "mean",
                            by = list(Bonn_temps$Year))[,2]) %>%
  mutate(runn_mean_Tmin = runn_mean(Tmin,15),
         runn_mean_Tmax = runn_mean(Tmax,15))


Tmin_regression <- lm(Tmin ~ Year,
                      temperature_means)

Tmax_regression <- lm(Tmax ~ Year,
                      temperature_means)

temperature_means <- temperature_means %>%
  mutate(regression_Tmin = Tmin_regression$coefficients[1]+
           Tmin_regression$coefficients[2]*temperature_means$Year,
         regression_Tmax = Tmax_regression$coefficients[1]+
           Tmax_regression$coefficients[2]*temperature_means$Year
  )


ggplot(temperature_means,
       aes(Year,
           Tmin)) + 
  geom_point() + 
  geom_line(data = temperature_means,
            aes(Year,
                runn_mean_Tmin),
            lwd = 2,
            col = "blue") + 
  geom_line(data = temperature_means,
            aes(Year,
                regression_Tmin),
            lwd = 2,
            col = "red") +
  theme_bw(base_size = 15) +
  ylab("Mean monthly minimum temperature (°C)")

ggplot(temperature_means,
       aes(Year,
           Tmax)) + 
  geom_point() + 
  geom_line(data = temperature_means,
            aes(Year,
                runn_mean_Tmax),
            lwd = 2,
            col = "blue") + 
  geom_line(data = temperature_means,
            aes(Year, 
                regression_Tmax),
            lwd = 2,
            col = "red") +
  theme_bw(base_size = 15) +
  ylab("Mean monthly maximum temperature (°C)")


save_temperature_scenarios(chill_hist_scenario_list, "data","Bonn_hist_chill_305_59")
write.csv(actual_chill,"data/Bonn_observed_chill_305_59.csv", row.names = FALSE)
write.csv(Bonn_temps,"data/Bonn_temps.csv")
