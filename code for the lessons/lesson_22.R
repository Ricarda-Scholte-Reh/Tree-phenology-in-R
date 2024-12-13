library(chillR)
library(dormancyR)
library(patchwork)
library(tidyverse)


hourly_models <- 
  list(
    Chilling_units = chilling_units,
    Low_chill = low_chill_model,
    Modified_Utah = modified_utah_model,
    North_Carolina = north_carolina_model,
    Positive_Utah = positive_utah_model,
    Chilling_Hours = Chilling_Hours,
    Utah_Chill_Units = Utah_Model,
    Chill_Portions = Dynamic_Model)

daily_models <-
  list(
    Rate_of_Chill = rate_of_chill, 
    Exponential_Chill = exponential_chill,
    Triangular_Chill_Haninnen = triangular_chill_1,
    Triangular_Chill_Legave = triangular_chill_2)

metrics <- c(names(daily_models),
             names(hourly_models))

model_labels <- c("Rate of Chill",
                  "Exponential Chill",
                  "Triangular Chill (Häninnen)",
                  "Triangular Chill (Legave)",
                  "Chilling Units",
                  "Low-Chill Chill Units",
                  "Modified Utah Chill Units",
                  "North Carolina Chill Units",
                  "Positive Utah Chill Units",
                  "Chilling Hours",
                  "Utah Chill Units",
                  "Chill Portions")



for(T in -20:30)
 {
  hourly <- sapply( hourly_models,
                    function(x)
                      x(rep(T,1000))
                    )[1000,]
 
  temp_frame <- data.frame(Tmin = rep(T,1000),
                           Tmax = rep(T,1000),
                           Tmean = rep(T,1000))
  
  daily <- sapply( daily_models,
                   function(x) 
                     x(temp_frame)
                   )[1000,]
 
  if(T == -20)
    sensitivity <- c(T = T,
                     daily,
                     hourly) else   
      sensitivity <- rbind(sensitivity,
                           c(T = T,
                             daily,
                             hourly))
  }

sensitivity_normal <- 
  as.data.frame(cbind(sensitivity[,1],
                      sapply(2:ncol(sensitivity),
                             function(x)
                               sensitivity[,x]/max(sensitivity[,x]))))

colnames(sensitivity_normal) <- colnames(sensitivity)

sensitivity_gg <- 
  sensitivity_normal %>%
  pivot_longer(Rate_of_Chill:Chill_Portions)
  
 # melt(sensitivity_normal,id.vars="T")
sensitivity_gg$value[sensitivity_gg$value<=0.001] <- NA


chill<-
  ggplot(sensitivity_gg,
         aes(x = T,
             y = factor(name),
             size = value)) +
  geom_point(col = "light blue") +
  # scale_y_discrete(labels = model_labels) +
  ylab("Chill model") +
  xlab("Temperature (assumed constant, °C)") +
  xlim(c(-30, 40)) +
  theme_bw(base_size = 15) +
  labs(size = "Chill \nWeight")

chill

KA_temps <- read_tab("D:/! Ricardas Daten/Studium/Master/03. Semester (WiSe 24-25)/Tree phenology analysis in R/code for the lessons/TMaxTMin1958-2019_patched.csv") %>%
  make_JDay() %>%
  filter(JDay > 305 | JDay < 90) %>%
  stack_hourly_temps(latitude = 50.6)


hh_KA <- hist(KA_temps$hourtemps$Temp,
              breaks = c(-30:30),
              plot=FALSE)

hh_KA_df <- data.frame(
  T = hh_KA$mids,
  name = "Klein-Altendorf, Germany",
  value = hh_KA$counts / max(hh_KA$counts))

hh_KA_df$value[hh_KA_df$value == 0] <- NA


Beijing_temps <- read_tab("D:/! Ricardas Daten/Studium/Master/03. Semester (WiSe 24-25)/Tree phenology analysis in R/code for the lessons/Beijing_weather.csv") %>%
  make_JDay() %>%
  filter(JDay > 305 | JDay < 90) %>%
  stack_hourly_temps(latitude = 39.9)

hh_Beijing <- hist(Beijing_temps$hourtemps$Temp,
                   breaks = c(-30:30),
                   plot=FALSE)

hh_Beijing_df<-data.frame(
  T = hh_Beijing$mids,
  name = "Beijing, China",
  value = hh_Beijing$counts / max(hh_Beijing$counts))

hh_Beijing_df$value[hh_Beijing_df$value==0]<-NA


Davis_temps <- read_tab("D:/! Ricardas Daten/Studium/Master/03. Semester (WiSe 24-25)/Tree phenology analysis in R/code for the lessons/Davis_weather.csv") %>%
  make_JDay() %>%
  filter(JDay > 305 | JDay < 90) %>%
  stack_hourly_temps(latitude = 38.5)


hh_Davis <- hist(Davis_temps$hourtemps$Temp,
              breaks = c(-30:40),
              plot=FALSE)

hh_Davis_df <- data.frame(
  T = hh_Davis$mids,
  name = "Davis, California",
  value = hh_Davis$counts / max(hh_Davis$counts))

hh_Davis_df$value[hh_Davis_df$value == 0] <- NA


hh_df<-rbind(hh_KA_df,
             hh_Beijing_df,
             hh_Davis_df)

locations<-
  ggplot(data = hh_df,
         aes(x = T,
             y = name,
             size = value)) +
  geom_point(col = "coral2") +
  ylab("Location") +
  xlab("Temperature (between November and March, °C)") + 
  xlim(c(-30, 40)) +
  theme_bw(base_size = 15) +
  labs(size = "Relative \nfrequency")


locations



  plot <- (chill +
             locations +
             plot_layout(guides = "collect",
                         heights = c(1, 0.4))
           ) & theme(legend.position = "right",
                     legend.text = element_text(size = 10),
                     legend.title = element_text(size = 12))

plot


chill <-
  ggplot(sensitivity_gg %>%
           filter(name == "Chill_Portions"),
         aes(x = T,
             y = factor(name),
             size=value)) +
  geom_point(col = "light blue") +
  scale_y_discrete(labels = "Chill Portions") +
  ylab("Chill model") +
  xlab("Temperature (assumed constant, °C)") +
  xlim(c(-30, 40)) +
  theme_bw(base_size = 15) +
  labs(size = "Chill \nWeight")

  plot<- (chill +
            locations +
            plot_layout(guides = "collect",
                        heights = c(0.5,1))
        ) & theme(legend.position = "right",
                  legend.text = element_text(size = 10),
                  legend.title = element_text(size = 12))

plot

