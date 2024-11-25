### Lesson 16

library(chillR)
library(dplyr)
library(ecmwfr)

# packageVersion("chillR")
# install.packages("chillR", type = "source")
# Sys.info()["machine"]

# "ssp126" already downloaded
download_cmip6_ecmwfr(
  scenarios = c("ssp126", "ssp245", "ssp370", "ssp585"),
  area = area,
  key = '9d522e37-4b90-46d1-8701-7deadd45032b',
  model = 'default',
  frequency = 'monthly',
  variable = c('Tmin', 'Tmax'),
  year_start = 2015,
  year_end = 2100)


