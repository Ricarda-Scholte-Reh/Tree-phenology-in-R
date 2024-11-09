require(chillR)
#require(kableExtra)

?daylength



daylength(latitude = 51,
          JDay = 300)

aa <- daylength(latitude = 51,
                JDay = 300)

aa$Daylength

Days <- daylength(latitude = 50.4,
                  JDay = 1:365)

Days_df <-
  data.frame(
    JDay = 1:365,
    Sunrise = Days$Sunrise,
    Sunset = Days$Sunset,
    Daylength = Days$Daylength
  )

plot(Days_df$Sunrise ~ Days_df$JDay)

library(ggplot2)

ggplot(Days_df,
       aes(JDay, 
           Sunset)) +
  geom_line(lwd = 2,
            col = "blue",
            lty = 2)

ggplot(Days_df,
       aes(JDay, 
           Sunrise)) +
  geom_line(lwd = 2,
            col = "blue",
            lty = 2)




ggplot(Days_df, aes(JDay, Sunset)) +
  geom_line(lwd = 2, col = "red", lty = 2)
ggplot(Days_df, aes(JDay, Sunrise)) +
  geom_line(lwd = 2, col = "red", lty = 2)
ggplot(Days_df, aes(JDay, Daylength)) +
  geom_line(lwd = 2, col = "red", lty = 2)

library(tidyr)

Days_df_longer <- pivot_longer(Days_df,
                               cols = c(Sunrise:Daylength))

ggplot(Days_df_longer, aes(JDay, value)) +
  geom_line(lwd = 1.5) +
  facet_grid(cols = vars(name)) +
  ylab("Time of Day / Daylength (Hours)") +
  theme_bw(base_size = 10) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


?stack_hourly_temps


KA_weather[1:10,]


KA_hourly <- stack_hourly_temps(KA_weather, 
                                latitude = 50.4,
                                keep_sunrise_sunset = TRUE)

KA_hourly$hourtemps[100:120, ]
# add a plot of hourly temperatures


KA_hourly$hourtemps[, "DATE"] <-
  ISOdate(
    KA_hourly$hourtemps$Year,
    KA_hourly$hourtemps$Month,
    KA_hourly$hourtemps$Day,
    KA_hourly$hourtemps$Hour
  )

ggplot(KA_hourly$hourtemps[20:100, ],
       aes(DATE,
           Temp)) +
  geom_line(lwd = 1.5) +
  xlab("Date") +
  ylab("Temperature (°C)") +
  theme_bw(base_size = 10)



empi_curve <-
  Empirical_daily_temperature_curve(Winters_hours_gaps)


empi_curve[1:48, ]

ggplot(data = empi_curve[1:96, ],
       aes(Hour,
           Prediction_coefficient)) +
  geom_line(lwd = 1.3, 
            col = "red") + 
  facet_grid(rows = vars(Month)) + 
  xlab("Hour of the day") +
  ylab("Prediction coefficient") +
  theme_bw(base_size = 10)



coeffs <- Empirical_daily_temperature_curve(Winters_hours_gaps)

day_to_day<-make_all_day_table(
  KA_weather[c(1:10,20:30),],timestep="day")

Winters_daily <-
  make_all_day_table(Winters_hours_gaps, input_timestep = "hour")

Winters_hours <- Empirical_hourly_temperatures(Winters_daily, coeffs)

## stopping here


require(reshape2)

Winters_hours <- Winters_hours[, c("Year", "Month", "Day", "Hour", "Temp")]
colnames(Winters_hours)[ncol(Winters_hours)] <- "Temp_empirical"
Winters_ideal <-
  stack_hourly_temps(Winters_daily, latitude = 38.5)$hourtemps
Winters_ideal <- Winters_ideal[, c("Year", "Month", "Day", "Hour", "Temp")]
colnames(Winters_ideal)[ncol(Winters_ideal)] <- "Temp_ideal"



Winters_triangle <- Winters_daily
Winters_triangle[, "Hour"] <- 0
Winters_triangle$Hour[nrow(Winters_triangle)] <- 23
Winters_triangle[, "Temp"] <- 0
Winters_triangle <-
  make_all_day_table(Winters_triangle, timestep = "hour")
colnames(Winters_triangle)[ncol(Winters_triangle)] <-
  "Temp_triangular"

# with the following loop, we fill in the daily Tmin and Tmax values for every
# hour of the dataset

for (i in 2:nrow(Winters_triangle))
{
  if (is.na(Winters_triangle$Tmin[i]))
    Winters_triangle$Tmin[i] <- Winters_triangle$Tmin[i - 1]
  if (is.na(Winters_triangle$Tmax[i]))
    Winters_triangle$Tmax[i] <- Winters_triangle$Tmax[i - 1]
}
Winters_triangle$Temp_triangular <- NA

# now we assign the daily Tmin value to the 6th hour of every day

Winters_triangle$Temp_triangular[which(Winters_triangle$Hour == 6)] <-
  Winters_triangle$Tmin[which(Winters_triangle$Hour == 6)]

# we also assign the daily Tmax value to the 18th hour of every day

Winters_triangle$Temp_triangular[which(Winters_triangle$Hour == 18)] <-
  Winters_triangle$Tmax[which(Winters_triangle$Hour == 18)]

# in the following step, we use the chillR function "interpolate_gaps"
# to fill in all the gaps in the hourly record with straight lines

Winters_triangle$Temp_triangular <-
  interpolate_gaps(Winters_triangle$Temp_triangular)$interp
Winters_triangle <-
  Winters_triangle[, c("Year", "Month", "Day", "Hour", "Temp_triangular")]


 
Winters_temps <-
  merge(Winters_hours_gaps,
        Winters_hours,
        by = c("Year", "Month", "Day", "Hour"))
Winters_temps <-
  merge(Winters_temps,
        Winters_triangle,
        by = c("Year", "Month", "Day", "Hour"))
Winters_temps <-
  merge(Winters_temps,
        Winters_ideal,
        by = c("Year", "Month", "Day", "Hour"))


Winters_temps[, "DATE"] <-
  ISOdate(Winters_temps$Year,
          Winters_temps$Month,
          Winters_temps$Day,
          Winters_temps$Hour)


Winters_temps_to_plot <-
  Winters_temps[, c("DATE",
                    "Temp",
                    "Temp_empirical",
                    "Temp_triangular",
                    "Temp_ideal")]
Winters_temps_to_plot <- Winters_temps_to_plot[100:200, ]
Winters_temps_to_plot <- melt(Winters_temps_to_plot, id = c("DATE"))
colnames(Winters_temps_to_plot) <- c("DATE", "Method", "Temperature")


ggplot(data = Winters_temps_to_plot, aes(DATE, Temperature, colour = Method)) +
  geom_line(lwd = 1.3) + ylab("Temperature (°C)") + xlab("Date")


# here's the RMSE for the triangular method:
RMSEP(Winters_temps$Temp_triangular, Winters_temps$Temp)
# here's the RMSE for the idealized-curve method:
RMSEP(Winters_temps$Temp_ideal, Winters_temps$Temp)
# and here's the RMSE for the empirical-curve method:
RMSEP(Winters_temps$Temp_empirical, Winters_temps$Temp)

