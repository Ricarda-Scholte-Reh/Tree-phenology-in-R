library(chillR)
library(tidyverse)


#filling small gaps

#sometimes there are gaps in your data and you do not realize, because whole 
#entry is missing
#--> chillR::make_all_day_table() ensures that at least missing days 
#are indicated
weather <- KA_weather %>% make_all_day_table()

#fill gaps in minimum temperature observations
Tmin_int <- interpolate_gaps(weather[,"Tmin"])

#add to the original observation
weather <- weather %>% mutate(Tmin = Tmin_int$interp,
                              Tmin_interpolated = Tmin_int$missing)

#do the same for maximum temperature
Tmax_int <- interpolate_gaps(weather[,"Tmax"])

weather <- weather %>% mutate(Tmax = Tmax_int$interp,
                              Tmax_interpolated = Tmax_int$missing)



#---------------------------------#
#how to do the linear interpolation
#---------------------------------#

# add an extra day to the KA_weather dataset that is not connected to the days that are already there.
# this creates a large gap, which we can then interpolate
tail(KA_weather) 

#we add a pretty large gap to KA_weather
KA_weather_gap <- rbind(KA_weather, c(Year = 2011,
                                      Month = 3,
                                      Day = 3,
                                      Tmax = 26,
                                      Tmin = 14)) 

p1 <- KA_weather_gap %>% 
  mutate(Date = lubridate::ymd(paste(Year, Month, Day))) %>% 
  ggplot(aes(x = Date)) +
  geom_point(aes(y = Tmin)) +
  xlim(c(lubridate::ymd(paste(2010, 01,01)), lubridate::ymd(paste(2011, 3,3))))



# fill in the gaps between Julian date 300 (late October) and 100 (early April), only returning data between 2000 and 2011
fixed_winter_days <- KA_weather_gap %>% fix_weather(start_year = 2000,
                                                    end_year = 2011,
                                                    start_date = 300,
                                                    end_date = 100)

fixed_winter_days$weather %>% 
  mutate(Date = lubridate::ymd(paste(Year, Month, Day))) %>% 
  ggplot(aes(x = Date)) +
  geom_point(aes(y = Tmin, col = no_Tmin)) +
  xlim(c(lubridate::ymd(paste(2010, 01,01)), lubridate::ymd(paste(2011, 3,3))))

# fill in all gaps
fixed_all_days <- KA_weather_gap %>% fix_weather()


#you can get overview on how much data was fixed
fixed_winter_days$QC

fixed_all_days$QC


#---------------------------------#
#evaluate the linear interpolation
#---------------------------------#

gap_weather <- KA_weather[200:305,]
gap_weather[,"Tmin_observed"] <- gap_weather$Tmin

#remove some observations, replace with NA
gap_weather$Tmin[c(2,4:5,7:9,11:14,16:20,22:27,29:35,
                   37:44,46:54,56:65,67:77,79:90,92:104)] <- NA
fixed_gaps <- fix_weather(gap_weather)$weather

ggplot(data=fixed_gaps,
       aes(DATE,Tmin_observed)) +
  geom_line(lwd=1.3) +
  xlab("Date") +
  ylab("Daily minimum temperature (°C)") +
  geom_line(data=fixed_gaps,
            aes(DATE,Tmin),
            col="red",
            lwd=1.3)

fixed_gaps[,"error"] <- abs(fixed_gaps$Tmin - fixed_gaps$Tmin_observed)

ggplot(data=fixed_gaps,
       aes(DATE,error)) +
  geom_line(lwd=1.3) +
  xlab("Date") +
  ylab("Error introduced by interpolation (°C)") +
  geom_point(data=fixed_gaps[which(!fixed_gaps$no_Tmin),],
             aes(DATE,error),
             col="red",
             cex=3)

#we could also do a predicted vs observed plot
fixed_gaps %>% 
  filter(no_Tmin == TRUE) %>% 
  ggplot(aes(x = Tmin_observed,y = Tmin)) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  geom_point() +
  xlab("Observed Tmin") +
  ylab("Predicted Tmin (Linear interpolation)")




#---------------------------------------------#
#Patch weather observations of other stations
#---------------------------------------------#

Bonn <- read.csv("data/Bonn_chillR_weather.csv")

Bonn_QC <- fix_weather(Bonn)$QC
Bonn_QC

#visual inspection on missing data with naniar package
Bonn %>% 
  select(Tmin, Tmax, Prec) %>% 
  naniar::vis_miss()

#function to summarize gap length
#quickly coded, because I was curious
get_gap_length <- function(x, summary = 'median'){
  #in case there is no gap
  if(sum(is.na(x)) == 0){
    return(0)
  }
  #rle records the sequence length of each value
  rle_results <- rle(is.na(x) == TRUE)
  #we are only interested in the true values
  gap_length <- rle_results$lengths[rle_results$values == TRUE]
  if(summary == 'median') {
    return(median(gap_length))
  } else if(summary == 'min'){
    return(min(gap_length))
  } else if(summary == 'max'){
    return(max(gap_length))
  } else {
    error('wrong input for summary argument')
  }
}

#get information on the gap length per year
#example how to use group_by and summarise functions
gap_summary <- Bonn %>% 
  group_by(Year) %>% 
  summarise(n_gap = sum(is.na(Tmin)),
            median_gap_length = get_gap_length(Tmin),
            max_gap_length = get_gap_length(Tmin, summary = 'max'))




#------------------------#
#patch with auxiliary weather obserbations
#------------------------#




station_list <- handle_gsod(action="list_stations",
                            location=c(7.10,50.73),
                            time_interval=c(1990,2020))

station_list

#download stations number 2, 3, and 6
#--> these will be used to patch the gaps
patch_weather<-
      handle_gsod(action = "download_weather",
                  location = as.character(station_list$chillR_code[c(2,3,6)]),
                  time_interval = c(1990,2020)) %>%
  handle_gsod()

#simply out of curiosity, check the downloaded stations
purrr::map2(patch_weather, names(patch_weather), function(x, xname){
  x %>% 
    dplyr::select(Tmin, Tmax, Prec) %>% 
    naniar::vis_miss() +
    ggtitle(xname)
}) 




#use the function to patch the gaps
#the algorim cycles through the auxiliary stations in the same order as supplied
patched <- patch_daily_temperatures(weather = Bonn,
                                    patch_weather = patch_weather)


patched$statistics[[1]]

patched$statistics[[2]]

patched$statistics[[3]]


#set minimum quality criteria
patched <- patch_daily_temperatures(weather = Bonn,
                                    patch_weather = patch_weather,
                                    max_mean_bias = 1,
                                    max_stdev_bias = 2)

patched$statistics[[1]]

patched$statistics[[2]]

patched$statistics[[3]]


#check where gaps remain
post_patch_stats <- fix_weather(patched)$QC

post_patch_stats
#only one gap remains,
#--> linear interpolation will do not much harm here
Bonn_weather<-fix_weather(patched)




patched_monthly <- patch_daily_temps(weather = Bonn,
                                     patch_weather = patch_weather,
                                     max_mean_bias = 1,
                                     max_stdev_bias = 2,
                                     time_interval = "month")


patched_monthly$statistics$Tmin$NORVENICH
#biases vary on a monthly basis, smaller differences in winter


#we can do also the bias correction every two weeks
patched_2weeks <- patch_daily_temps(weather = Bonn,
                                    patch_weather = patch_weather,
                                    max_mean_bias = 1,
                                    max_stdev_bias = 2,
                                    time_interval = "2 weeks")


#-----------------------#
#evaluate the effect of bias correction interval
#-----------------------#


Gaps <- sample(seq(1:nrow(Bonn)),
               size = 5000,
               replace = FALSE)

Bonn_gaps <- Bonn %>% mutate(obs_Tmin=Tmin,
                             obs_Tmax=Tmax)
Bonn_gaps$Tmin[Gaps] <- NA
Bonn_gaps$Tmax[Gaps] <- NA

patch_annual <- patch_daily_temps(weather = Bonn_gaps,
                                  patch_weather = patch_weather,
                                  max_mean_bias = 1,
                                  max_stdev_bias = 2,
                                  time_interval = "year")
patch_month <- patch_daily_temps(weather = Bonn_gaps,
                                 patch_weather = patch_weather,
                                 max_mean_bias = 1,
                                 max_stdev_bias = 2,
                                 time_interval = "month")
patch_2weeks <- patch_daily_temps(weather = Bonn_gaps,
                                  patch_weather = patch_weather,
                                  max_mean_bias = 1,
                                  max_stdev_bias = 2,
                                  time_interval = "2 weeks")

Bonn_gaps[,"Tmin_annual"] <- Bonn_gaps$obs_Tmin - patch_annual$weather$Tmin
Bonn_gaps[,"Tmax_annual"] <- Bonn_gaps$obs_Tmax - patch_annual$weather$Tmax
Bonn_gaps[,"Tmin_month"] <- Bonn_gaps$obs_Tmin - patch_month$weather$Tmin
Bonn_gaps[,"Tmax_month"] <- Bonn_gaps$obs_Tmax - patch_month$weather$Tmax
Bonn_gaps[,"Tmin_2weeks"] <- Bonn_gaps$obs_Tmin - patch_2weeks$weather$Tmin
Bonn_gaps[,"Tmax_2weeks"] <- Bonn_gaps$obs_Tmax - patch_2weeks$weather$Tmax

Interval_eval <- Bonn_gaps %>%
  filter(is.na(Tmin)) %>%
  pivot_longer(Tmin_annual:Tmax_2weeks) %>%
  mutate(Type=factor(name,
                     levels = c("Tmin_annual",
                                "Tmin_month",
                                "Tmin_2weeks",
                                "Tmax_annual",
                                "Tmax_month",
                                "Tmax_2weeks")) )

ggplot(Interval_eval,
       aes(Type,value)) +
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  xlab("Variable and bias evaluation interval") +
  ylab("Prediction error")



error_eval <-
  data.frame(Variable = c(rep("Tmin",3),rep("Tmax",3)),
             Interval = rep(c("Year","Month","Two weeks"),2),
             Error = c(
             mean(abs(Bonn_gaps$Tmin_annual[is.na(Bonn_gaps$Tmin)]),na.rm=TRUE),
             mean(abs(Bonn_gaps$Tmin_month[is.na(Bonn_gaps$Tmin)]),na.rm=TRUE),
             mean(abs(Bonn_gaps$Tmin_2weeks[is.na(Bonn_gaps$Tmin)]),na.rm=TRUE),
             mean(abs(Bonn_gaps$Tmax_annual[is.na(Bonn_gaps$Tmin)]),na.rm=TRUE),
             mean(abs(Bonn_gaps$Tmax_month[is.na(Bonn_gaps$Tmin)]),na.rm=TRUE),
             mean(abs(Bonn_gaps$Tmax_2weeks[is.na(Bonn_gaps$Tmin)]),na.rm=TRUE))
             )


error_eval



monthly_bias_fixed <- fix_weather(patched_monthly)

write.csv(monthly_bias_fixed$weather,
           "data/Bonn_weather.csv")




#------------------------#
#patch hourly data
#------------------------#


Winters_hours_gaps[,"DATE"] <- ISOdate(Winters_hours_gaps$Year,
                                       Winters_hours_gaps$Month,
                                       Winters_hours_gaps$Day,
                                       Winters_hours_gaps$Hour)
Winters_hours_gaps[,"interpolated"] <-
  interpolate_gaps(Winters_hours_gaps$Temp_gaps)$interp

ggplot(data = Winters_hours_gaps[50:300,],
       aes(DATE,Temp)) +
  geom_line(lwd = 1.3) +
  ylab("Temperature (°C)") +
  xlab("Date") +
  geom_line(data = Winters_hours_gaps[50:300,],
            aes(DATE,interpolated),
            col = "red",lwd = 1.3) +
  theme_bw(base_size = 10)  

#quite poor gap filling using linear interpolation





# stations <- handle_cimis("list_stations",
#                          location = c(-122,38.5))
# downloaded_winters <- handle_cimis("download_weather",
#                                    stations$chillR_code[2],
#                                    time_interval = c(2008,2008))
# winters_daily <- handle_cimis(downloaded_winters)$weather
#download didnt work really well, downloaded from last year

#took from last year's folder
winters_daily <- read.csv("data/winters_daily.csv")

head(winters_daily)

to_interp <- Winters_hours_gaps
to_interp[,"Temp_recorded"] <- to_interp[,"Temp"]
to_interp[,"Temp"] <- to_interp[,"Temp_gaps"]
interp <- interpolate_gaps_hourly(hourtemps = to_interp,
                                  latitude = 38.5,
                                  daily_temps = list(Winters=winters_daily))


interp$daily_patch_report
interp$weather

interp$weather[30:45,c(1:4,7,10)]

inter <- interp$weather
inter[,"DATE"] <- ISOdate(inter$Year,
                          inter$Month,
                          inter$Day,
                          inter$Hour)

ggplot(data = inter[50:300,],
       aes(DATE,Temp_recorded)) +
  geom_line(lwd = 1.3,
            col = "gray") +
  ylab("Temperature (°C)") +
  xlab("Date") +
  geom_line(data = inter[50:300,],
            aes(DATE,Temp),
            lwd = 1.3,
            col = "red") +
  geom_line(data = inter[50:300,],
            aes(DATE,Temp_gaps),
            lwd = 1.3) +
  theme_bw(base_size = 20)
#red is interpolated, grey is actually observed
#black is unchanged temperature




#-----------------#
#compare different options to patch hourly data
#-----------------#


#quick explanation what interquartile range is
require(stats)
y <- rnorm(100)
IQ <- quantile(y)[4] - quantile(y)[2]



inter <- interp$weather
inter[,"DATE"] <- ISOdate(inter$Year,
                          inter$Month,
                          inter$Day,
                          inter$Hour)

#option 1: temperature curve from daily records of nearby station

#make daily data hourly
winters_hours <- stack_hourly_temps(fix_weather(winters_daily),
                                    latitude = 38)$hourtemps

#select the right period
start_hour_winters <- which(winters_hours$Year == head(inter$Year,1)&
                              winters_hours$Month == head(inter$Month,1)&
                              winters_hours$Day == head(inter$Day,1)&
                              winters_hours$Hour == head(inter$Hour,1))

end_hour_winters <- which(winters_hours$Year == tail(inter$Year,1)&
                            winters_hours$Month == tail(inter$Month,1)&
                            winters_hours$Day == tail(inter$Day,1)&
                            winters_hours$Hour == tail(inter$Hour,1))

#subset the data.frame
option1 <- winters_hours$Temp[start_hour_winters:end_hour_winters]



#option 2: idealized temperature curve from daily records of same location
#idealized temperature curve from hourly records

#pretend that our station actually only recorded daily data
orchard_extremes <- make_all_day_table(inter,
                                       timestep = "day",
                                       input_timestep = "hour")

#make it hourly
orchard_hours <- stack_hourly_temps(orchard_extremes,
                                    latitude = 38)$hourtemps

#only take the relevant period
start_hour_orchard <- which(orchard_hours$Year == head(inter$Year,1)&
                              orchard_hours$Month == head(inter$Month,1)&
                              orchard_hours$Day == head(inter$Day,1)&
                              orchard_hours$Hour == head(inter$Hour,1))

end_hour_orchard <- which(orchard_hours$Year == tail(inter$Year,1)&
                            orchard_hours$Month == tail(inter$Month,1)&
                            orchard_hours$Day == tail(inter$Day,1)&
                            orchard_hours$Hour == tail(inter$Hour,1))

option2 <- orchard_hours$Temp[start_hour_orchard:end_hour_orchard]



#linear interpolation of hourly records

option3 <- interpolate_gaps(inter$Temp_gaps)$interp



#eikes function combining three methods
#eikes interpolate gap function
option4 <- inter$Temp

#our actual observations
observed <- inter$Temp_recorded




#this is where we will store the model performance data
eval_table <-
  eval_table_gaps <-
  data.frame(Option = 1:4,
             Input_data = c("daily","daily","hourly","hourly"),
             Interpolation_method = c("from proxy","local extremes",
                                      "linear","hourly interpolation"),
             RMSEP = NA,
             RPIQ = NA)

#extract only the data points where we removed data
observed_gaps <- observed[which(is.na(inter$Temp_gaps))]
option1_gaps <- option1[which(is.na(inter$Temp_gaps))]
option2_gaps <- option2[which(is.na(inter$Temp_gaps))]
option3_gaps <- option3[which(is.na(inter$Temp_gaps))]
option4_gaps <- option4[which(is.na(inter$Temp_gaps))]


eval_table_gaps[,"RMSEP"] <- round(c(RMSEP(option1_gaps, observed_gaps),
                                     RMSEP(option2_gaps, observed_gaps),
                                     RMSEP(option3_gaps, observed_gaps),
                                     RMSEP(option4_gaps, observed_gaps)),
                                   1)

eval_table_gaps[,"RPIQ"] <- round(c(RPIQ(option1_gaps, observed_gaps),
                                    RPIQ(option2_gaps, observed_gaps),
                                    RPIQ(option3_gaps, observed_gaps),
                                    RPIQ(option4_gaps, observed_gaps)),
                                  1)
eval_table_gaps




eval_table <-
  data.frame(Option = 1:4,
             Input_data = c("daily","daily","hourly","hourly"),
             Interpolation_method = c("from proxy","local extremes",
                                      "linear","hourly interpolation"),
             RMSEP = NA,
             RPIQ = NA)

eval_table[,"RMSEP"] <- round(c(RMSEP(option1, observed),
                                RMSEP(option2, observed),
                                RMSEP(option3, observed),
                                RMSEP(option4, observed)),
                              1)

eval_table[,"RPIQ"] <- round(c(RPIQ(option1, observed),
                               RPIQ(option2, observed),
                               RPIQ(option3, observed),
                               RPIQ(option4, observed)),
                             1)


eval_table





all_chill <- data.frame(DATE = inter$DATE,
                        "Obs" = Dynamic_Model(observed),
                        "Opt1" = Dynamic_Model(option1),
                        "Opt2" = Dynamic_Model(option2),
                        "Opt3" = Dynamic_Model(option3),
                        "Opt4" = Dynamic_Model(option4))

all_chill <- pivot_longer(all_chill, Obs:Opt4)

all_chill[which(all_chill$name == "Obs"),"Method"] <- 
  "Observed temperatures"
all_chill[which(all_chill$name == "Opt1"),"Method"] <- 
  "Option 1 - idealized record from proxy data"
all_chill[which(all_chill$name == "Opt2"),"Method"] <-
  "Option 2 - idealized record from daily orchard data"
all_chill[which(all_chill$name == "Opt3"),"Method"] <-
  "Option 3 - linear interpolation of hourly data"
all_chill[which(all_chill$name == "Opt4"),"Method"] <-
  "Option 4 - use of interpolate_gaps_hourly"



ggplot(data=all_chill,
       aes(DATE,
           value,
           colour=Method)) +
  geom_line(lwd = 1.3) +
  ylab("Chill accumulation (Chill Portions)") +
  xlab("Date") +
  theme_bw(base_size = 15) +
  theme(legend.position = c(0.4, 0.85))



all_heat <- data.frame(DATE = inter$DATE,
                       "Obs" = GDH(observed),
                       "Opt1" = GDH(option1),
                       "Opt2" = GDH(option2),
                       "Opt3" = GDH(option3),
                       "Opt4" = GDH(option4))

all_heat <- pivot_longer(all_heat, Obs:Opt4)

all_heat[which(all_heat$name == "Obs"),"Method"] <-
  "Observed temperatures"
all_heat[which(all_heat$name == "Opt1"),"Method"] <-
  "Option 1 - idealized record from proxy data"
all_heat[which(all_heat$name == "Opt2"),"Method"] <-
  "Option 2 - idealized record from daily orchard data"
all_heat[which(all_heat$name == "Opt3"),"Method"] <-
  "Option 3 - linear interpolation of hourly data"
all_heat[which(all_heat$name == "Opt4"),"Method"] <-
  "Option 4 - use of interpolate_gaps_hourly"


ggplot(data = all_heat,
       aes(DATE,
           value,
           colour=Method)) +
  geom_line(lwd = 1.3) +
  ylab("Heat accumulation (Growing Degree Hours)") +
  xlab("Date") +
  theme_bw(base_size = 15) +
  theme(legend.position = c(0.4, 0.85))



chill_heat_eval <- rbind(data.frame(Option = 0,
                                    Input_data = "observed",
                                    Interpolation_method = "none"),
                         eval_table[,1:3])

chill_heat_eval[,"Chill Portions"] <-
  round(c(max(all_chill$value[which(all_chill$name == "Obs")]),
          max(all_chill$value[which(all_chill$name == "Opt1")]),
          max(all_chill$value[which(all_chill$name == "Opt2")]),
          max(all_chill$value[which(all_chill$name == "Opt3")]),
          max(all_chill$value[which(all_chill$name == "Opt4")])),
        1)
chill_heat_eval[,"Growing Degree Hours"] <-
  round(c(max(all_heat$value[which(all_heat$name == "Obs")]),
          max(all_heat$value[which(all_heat$name == "Opt1")]),
          max(all_heat$value[which(all_heat$name == "Opt2")]),
          max(all_heat$value[which(all_heat$name == "Opt3")]),
          max(all_heat$value[which(all_heat$name == "Opt4")])),
        0)

chill_heat_eval

