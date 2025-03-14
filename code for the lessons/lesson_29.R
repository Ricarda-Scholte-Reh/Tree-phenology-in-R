require(ggplot2)
dat<-data.frame(x=c(1,2,3,4),y=c(2.3,2.5,2.7,2.7))

ggplot(dat,aes(x=x,y=y)) +
  geom_smooth(method="lm",fullrange = TRUE) +
  geom_smooth(method="lm",fullrange = FALSE,col="dark green") +
  geom_point() +
  xlim(c(0,10)) +
  geom_vline(xintercept = 8, col="red") +
  theme_bw(base_size = 15)


require(chillR)
library(tidyverse)
past_weather <- read_tab("D:/! Ricardas Daten/Studium/Master/03. Semester (WiSe 24-25)/Tree phenology analysis in R/Logbook chapters/data ch29-35/TMaxTMin1958-2019_patched.csv")
past_weather$SSP_Time <- "Past"

future_temps <- load_temperature_scenarios("data/future_climate",
                                    "Bonn_future_")

SSPs <- c("ssp126", "ssp245", "ssp370", "ssp585")
Times <- c(2050, 2085)

list_ssp <- 
  strsplit(names(future_temps), '\\.') %>%
  map(2) %>%
  unlist()

list_gcm <-
  strsplit(names(future_temps), '\\.') %>%
  map(3) %>%
  unlist()

list_time <-
  strsplit(names(future_temps), '\\.') %>%
  map(4) %>%
  unlist()

for(SSP in SSPs)
  for(Time in Times)
   {Temps <- future_temps[list_ssp == SSP & list_time == Time]
    names(Temps) <- list_gcm[list_ssp == SSP & list_time == Time]
    
    for(gcm in names(Temps))
      Temps[[gcm]] <- Temps[[gcm]] %>% 
        mutate(GCM = gcm,
               SSP = SSP,
               Time = Time)
    
    Temps <- do.call("rbind", Temps)
    
    if(SSP == SSPs[1] & Time == Times[1])
      results <- Temps else
        results <- rbind(results,
                         Temps)
    }

results$SSP[results$SSP == "ssp126"] <- "SSP1"
results$SSP[results$SSP == "ssp245"] <- "SSP2"
results$SSP[results$SSP == "ssp370"] <- "SSP3"
results$SSP[results$SSP == "ssp585"] <- "SSP5"

results$SSP_Time<-paste(results$SSP,results$Time)

future_months<-
  aggregate(results[,c("Tmin","Tmax")],
            by=list(results$SSP_Time,
                    results$Year,
                    results$Month),
            FUN=mean)
colnames(future_months)[1:3]<-c("SSP_Time","Year","Month")

past_months<-
  aggregate(past_weather[,c("Tmin","Tmax")],
            by=list(past_weather$SSP_Time,
                    past_weather$Year,
                    past_weather$Month),
            FUN=mean)
colnames(past_months)[1:3]<-c("SSP_Time","Year","Month")

all_months<-rbind(past_months,future_months)

all_months$month_name<-factor(all_months$Month,
                                       levels=c(6:12,1:5),
                                       labels=month.name[c(6:12,1:5)])

library(tidyverse)

# Calculate the hulls for each group
hull_temps <- all_months %>%
  group_by(SSP_Time,month_name) %>%
  slice(chull(Tmin, Tmax))

ggplot(hull_temps, aes(Tmin, Tmax, fill = factor(SSP_Time))) +
  geom_polygon() +
  facet_wrap(vars(month_name)) +
  scale_fill_manual(name="Scenario",
                    breaks=c("Past",
                             "SSP1 2050",
                             "SSP1 2085",
                             "SSP2 2050",
                             "SSP2 2085",                            
                             "SSP3 2050",
                             "SSP3 2085",
                             "SSP5 2050",
                             "SSP5 2085"),
                    values=c("black",
                             alpha("light green",0.4),
                             alpha("dark green",0.4),
                             alpha("coral",0.4),
                             alpha("dark red",0.4),
                             alpha("yellow",0.4),
                             alpha("orange",0.4),                            
                             alpha("light blue",0.4),
                             alpha("dark blue",0.4))) +
  theme_bw(base_size = 15)



ggplot(hull_temps[which(hull_temps$Month %in% c(10,11,12,1,2,3)),],
       aes(Tmin, Tmax, fill = factor(SSP_Time))) +
  geom_polygon() +
  facet_wrap(vars(month_name)) +
  scale_fill_manual(name="Scenario",
                    breaks=c("Past",
                             "SSP1 2050",
                             "SSP1 2085",
                             "SSP2 2050",
                             "SSP2 2085",                            
                             "SSP3 2050",
                             "SSP3 2085",
                             "SSP5 2050",
                             "SSP5 2085"),
                    values=c("black",
                             alpha("light green",0.4),
                             alpha("dark green",0.4),
                             alpha("coral",0.4),
                             alpha("dark red",0.4),
                             alpha("yellow",0.4),
                             alpha("orange",0.4),                            
                             alpha("light blue",0.4),
                             alpha("dark blue",0.4))) +
  theme_bw(base_size = 15)




enhanced <- read_tab("data/final_weather_data_S1_S2.csv")
enhanced$Year <- enhanced$Treatment
enhanced$SSP_Time <- "Past enhanced"


enhanced_months<-aggregate(enhanced[,c("Tmin","Tmax")],
                           by=list(enhanced$SSP_Time,
                                   enhanced$Year,
                                   enhanced$Month),FUN=mean)
colnames(enhanced_months)[1:3]<-c("SSP_Time","Year","Month")

all_months_enhanced<-rbind(enhanced_months,future_months)

all_months_enhanced$month_name<-factor(all_months_enhanced$Month,
                                       levels=c(6:12,1:5),
                                       labels=month.name[c(6:12,1:5)])

# Calculate the hulls for each group
hull_temps_enhanced <- all_months_enhanced %>%
  group_by(SSP_Time,month_name) %>%
  slice(chull(Tmin, Tmax))

ggplot(hull_temps_enhanced[which(hull_temps_enhanced$Month %in% c(10,11,12,1,2,3)),],
       aes(Tmin, Tmax, fill = factor(SSP_Time))) +
  geom_polygon() +
  facet_wrap(vars(month_name)) +
  scale_fill_manual(name="Scenario",
                    breaks=c("Past enhanced",
                             "SSP1 2050",
                             "SSP1 2085",
                             "SSP2 2050",
                             "SSP2 2085",                            
                             "SSP3 2050",
                             "SSP3 2085",
                             "SSP5 2050",
                             "SSP5 2085"),
                    values=c("black",
                             alpha("light green",0.4),
                             alpha("dark green",0.4),
                             alpha("coral",0.4),
                             alpha("dark red",0.4),
                             alpha("yellow",0.4),
                             alpha("orange",0.4),                            
                             alpha("light blue",0.4),
                             alpha("dark blue",0.4))) +
  theme_bw(base_size = 15)


past_months$SSP_Time<-"Past combined"
enhanced_months$SSP_Time<-"Past combined"

all_months_both<-rbind(enhanced_months,
                       past_months,
                       future_months)

all_months_both$month_name<-factor(all_months_both$Month,
                                   levels=c(6:12,1:5),
                                   labels=month.name[c(6:12,1:5)])

hull_temps_both <- all_months_both %>%
  group_by(SSP_Time,month_name) %>%
  slice(chull(Tmin, Tmax))

ggplot(hull_temps_both[which(hull_temps_both$Month %in% c(10,11,12,1,2,3)),],
       aes(Tmin, Tmax, fill = factor(SSP_Time))) +
  geom_polygon() +
  facet_wrap(vars(month_name)) +
  scale_fill_manual(name="Scenario",
                    breaks=c("Past combined",
                             "SSP1 2050",
                             "SSP1 2085",
                             "SSP2 2050",
                             "SSP2 2085",                            
                             "SSP3 2050",
                             "SSP3 2085",
                             "SSP5 2050",
                             "SSP5 2085"),
                    values=c("black",
                             alpha("light green",0.4),
                             alpha("dark green",0.4),
                             alpha("coral",0.4),
                             alpha("dark red",0.4),
                             alpha("yellow",0.4),
                             alpha("orange",0.4),                            
                             alpha("light blue",0.4),
                             alpha("dark blue",0.4))) +
  theme_bw(base_size = 15)


