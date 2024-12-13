require(chillR)
require(ggplot2)
library(tidyverse)

library(downloadthis)

# Alex <- read.csv("data/Alexander_Lucas_bloom_1958_2019.csv")

# this is the download button
# Alex %>% download_this(
#   output_name = "Alexander_Lucas_bloom_1958_2019.csv",
#   output_extension = ".csv",
#   button_label = "Download phenology data",
#   button_type = "warning",
#   has_icon = TRUE,
#   icon = "fa fa-save"
# )



Alex <- read_tab("data/Alexander_Lucas_bloom_1958_2019.csv")

Alex <- pivot_longer(Alex,
                     cols = c(First_bloom:Last_bloom),
                     names_to = "Stage",
                     values_to = "YEARMODA")

Alex_first <- Alex %>%
  mutate(Year = as.numeric(substr(YEARMODA, 1, 4)),
         Month = as.numeric(substr(YEARMODA, 5, 6)),
         Day = as.numeric(substr(YEARMODA, 7, 8))) %>%
  make_JDay() %>%
  filter(Stage == "First_bloom")


ggplot(Alex_first,
       aes(Pheno_year,
           JDay)) +
  geom_point() +
  ylab("First bloom date (day of the year)") +
  xlab ("Year") +
  theme_bw(base_size = 15)

library(Kendall)
Kendall(x = Alex_first$Pheno_year,
        y = Alex_first$JDay)

x <- Alex_first$Pheno_year
y <- Alex_first$JDay

summary(lm(y ~ x))

ggplot(Alex_first,
       aes(Year,
           JDay)) +
  geom_point() +
  geom_smooth(method = 'lm',
              formula = y ~ x) +
  ylab("First bloom date (day of the year)") +
  xlab ("Year") +
  theme_bw(base_size = 15)


summary(lm(y ~ poly(x, 25)))

ggplot(Alex_first,
       aes(Year,
           JDay)) +
  geom_point() +
  geom_smooth(method='lm',
              formula = y ~ poly(x, 25)) +
  ylab("First bloom date (day of the year)") +
  xlab ("Year") +
  theme_bw(base_size = 15)

temperature <- read_tab("data/TMaxTMin1958-2019_patched.csv")

Tmin <- temperature %>%
  group_by(Year) %>%
  summarise(Tmin = mean(Tmin))

Tmax <- temperature %>%
  group_by(Year) %>% 
  summarise(Tmax = mean(Tmax))

Annual_means <- Tmin %>%
  cbind(Tmax[,2]) %>%
  mutate(Tmean = (Tmin + Tmax)/2)

Annual_means <- merge(Annual_means,
                      Alex_first)

Annual_means_longer <- Annual_means[,c(1:4,10)] %>%
  pivot_longer(cols = c(Tmin:Tmean),
               names_to = "Variable",
               values_to = "Temp")

ggplot(Annual_means_longer,
       aes(x = Temp,
           y = JDay)) + 
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x) + 
  facet_wrap("Variable")

summary(lm(Annual_means$JDay ~ Annual_means$Tmin))
summary(lm(Annual_means$JDay ~ Annual_means$Tmax))
summary(lm(Annual_means$JDay ~ Annual_means$Tmean))


temps_JDays <-
  make_JDay(temperature)

corr_temp_pheno <- function(start_JDay, # the start JDay of the period
                            end_JDay, # the start JDay of the period
                            temps_JDay = temps_JDays, # the temperature dataset
                            bloom = Alex_first) # a data.frame with bloom dates
{
  temps_JDay <- temps_JDay %>%
    mutate(Season = Year)
  
  if(start_JDay > end_JDay)
    temps_JDay$Season[temps_JDay$JDay >= start_JDay]<-
      temps_JDay$Year[temps_JDay$JDay >= start_JDay]+1
  
  if(start_JDay > end_JDay)
    sub_temps <- subset(temps_JDay,
                        JDay <= end_JDay | JDay >= start_JDay)
  
  if(start_JDay <= end_JDay) 
    sub_temps <- subset(temps_JDay,
                        JDay <= end_JDay & JDay >= start_JDay)
  
  mean_temps <- sub_temps %>%
    group_by(Season) %>%
    summarise(Tmin = mean(Tmin),
              Tmax = mean(Tmax)) %>%
    mutate(Tmean = (Tmin + Tmax)/2)
  
  colnames(mean_temps)[1] <- c("Pheno_year")
  
  temps_bloom <- merge(mean_temps,
                       bloom[c("Pheno_year",
                               "JDay")])
  
  # Let's just extract the slopes of the regression model for now
  slope_Tmin <- summary(lm(temps_bloom$JDay~temps_bloom$Tmin))$coefficients[2,1]
  slope_Tmean <- summary(lm(temps_bloom$JDay~temps_bloom$Tmean))$coefficients[2,1]
  slope_Tmax <- summary(lm(temps_bloom$JDay~temps_bloom$Tmax))$coefficients[2,1]
  
  c(start_JDay = start_JDay,
    end_JDay = end_JDay,
    length = length(unique(sub_temps$JDay)),
    slope_Tmin = slope_Tmin,
    slope_Tmean = slope_Tmean,
    slope_Tmax = slope_Tmax)
}

corr_temp_pheno(start_JDay = 305,
                end_JDay = 29,
                temps_JDay = temps_JDays,
                bloom = Alex_first)

corr_temp_pheno(start_JDay = 305,
                end_JDay = 45,
                temps_JDay = temps_JDays,
                bloom = Alex_first)


library(colorRamps) # for the color scheme we'll use in the plot

stJDs <- seq(from = 1,
             to = 366,
             by = 10)

eJDs <- seq(from = 1,
            to = 366,
            by = 10)

for(stJD in stJDs)
  for(eJD in eJDs)
    {correlations <- corr_temp_pheno(stJD,
                                     eJD)
    
    if(stJD == stJDs[1] & eJD == eJDs[1])
      corrs <- correlations else
        corrs <- rbind(corrs, correlations)
}


slopes <- as.data.frame(corrs) %>%
  rename(Tmin = slope_Tmin,
         Tmax = slope_Tmax,
         Tmean = slope_Tmean) %>%
  pivot_longer(cols = c(Tmin : Tmax),
               values_to = "Slope",
               names_to = "Variable")

ggplot(data = slopes,
       aes(x = start_JDay,
           y = length,
           fill = Slope)) +
  geom_tile() +
  facet_wrap(vars(Variable)) +
  scale_fill_gradientn(colours = matlab.like(15)) +
  ylab("Interval duration (days)") + 
  xlab("Start date of temperature summary interval (Day of year)") +
  theme_bw(base_size = 15)

