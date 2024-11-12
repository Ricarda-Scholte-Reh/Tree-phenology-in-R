library(chillR)
library(ggplot2)
library(tidyverse)

Temperatures<-read_tab("data/Bonn_chillR_weather.csv")

head(Temperatures)

write.csv(Temperatures, file="data/Temperatures.csv", row.names = FALSE)

Temperatures <- read_tab("data/Temperatures.csv")

head(Temperatures)


test_list <- list(Number = 1,
                  String = "Thanks for using chillR!",
                  DataFrame = data.frame(a = c(1,2,3),
                                         b = c(3,2,1),
                                         c = c(5,4,3)))

save_temperature_scenarios(test_list,
                           path = "data",
                           prefix = "test_list")


test_list <- load_temperature_scenarios(path = "data",
                                        prefix = "test_list")

