library(chillR)

station_list <- handle_gsod(action = "list_stations",
                            location = c(7.10, 50.73), #first longitude then latitude
                            time_interval = c(1990, 2020))
station_list

# Nationalpark Zentrum Eifel 50°35'09.4"N 6°26'53.9"E
station_list_Eifel <- handle_gsod(action = "list_stations",
                                 location = c(long = 6.2653, lat = 50.3509), #first longitude then latitude
                                 time_interval = c(1990, 2020))
station_list_Eifel
# Temp ist in Fahrenheit und Niederschlag in Inch

# Prec = Prescabitation (?) etwa so ausgesprochen


