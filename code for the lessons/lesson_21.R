library(chillR)
library(tidyverse)


Alex_first <- read_tab("D:/! Ricardas Daten/Studium/Master/03. Semester (WiSe 24-25)/Tree phenology analysis in R/Logbook chapters/Alexander_Lucas_bloom_1958_2019.csv") %>%
  select(Pheno_year, First_bloom) %>%
  mutate(Year = as.numeric(substr(First_bloom, 1, 4)),
         Month = as.numeric(substr(First_bloom, 5, 6)),
         Day = as.numeric(substr(First_bloom, 7, 8))) %>%
  make_JDay() %>%
  select(Pheno_year, JDay) %>%
  rename(Year = Pheno_year,
         pheno = JDay)


head(Alex_first)


KA_temps <- read_tab("D:/! Ricardas Daten/Studium/Master/03. Semester (WiSe 24-25)/Tree phenology analysis in R/Logbook chapters/TMaxTMin1958-2019_patched.csv") %>%
  make_JDay()

head(KA_temps)


PLS_results <- PLS_pheno(KA_temps,
                         Alex_first)


head(PLS_results$PLS_summary)

plot_PLS(PLS_results,
         PLS_results_path = "PLS_out")



library(ggplot2)

PLS_gg <- PLS_results$PLS_summary %>%
  mutate(Month = trunc(Date / 100),
         Day = Date - Month * 100,
         Date = NULL) 

PLS_gg$Date <- ISOdate(2002, 
                       PLS_gg$Month, 
                       PLS_gg$Day)

PLS_gg$Date[PLS_gg$JDay <= 0] <-
  ISOdate(2001, 
          PLS_gg$Month[PLS_gg$JDay <= 0], 
          PLS_gg$Day[PLS_gg$JDay <= 0])

PLS_gg <- PLS_gg %>%
  mutate(VIP_importance = VIP >= 0.8,
         VIP_Coeff = factor(sign(Coef) * VIP_importance))


VIP_plot<- ggplot(PLS_gg,
                  aes(x = Date,
                      y = VIP)) +
  geom_bar(stat='identity',
           aes(fill = VIP_importance))

VIP_plot <- VIP_plot +
  scale_fill_manual(name="VIP", 
                    labels = c("<0.8",
                               ">0.8"), 
                    values = c("FALSE"="grey",
                               "TRUE"="blue")) +
  theme_bw(base_size=15) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank() )

VIP_plot

coeff_plot <- ggplot(PLS_gg,
                     aes(x = Date,
                         y = Coef)) +
  geom_bar(stat ='identity',
           aes(fill = VIP_Coeff)) +
  scale_fill_manual(name = "Effect direction", 
                    labels = c("Advancing",
                               "Unimportant",
                               "Delaying"), 
                    values = c("-1" = "red", 
                               "0" = "grey",
                               "1" = "dark green")) +
  theme_bw(base_size = 15) +
  ylab("PLS coefficient") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank() )

coeff_plot

temp_plot <- ggplot(PLS_gg) +
  geom_ribbon(aes(x = Date,
                  ymin = Tmean - Tstdev,
                  ymax = Tmean + Tstdev),
              fill = "grey") +
  geom_ribbon(aes(x = Date,
                  ymin = Tmean - Tstdev * (VIP_Coeff == -1),
                  ymax = Tmean + Tstdev * (VIP_Coeff == -1)),
              fill = "red") +
  geom_ribbon(aes(x = Date,
                  ymin = Tmean - Tstdev * (VIP_Coeff == 1),
                  ymax = Tmean + Tstdev * (VIP_Coeff == 1)),
              fill = "dark green") +
  geom_line(aes(x = Date,
                y = Tmean)) +
  theme_bw(base_size = 15) +
  ylab(expression(paste(T[mean]," (°C)")))

temp_plot

library(patchwork)

plot<- (VIP_plot +
          coeff_plot +
          temp_plot +
          plot_layout(ncol = 1,
            guides = "collect")
        ) & theme(legend.position = "right",
                  legend.text = element_text(size = 8),
                  legend.title = element_text(size = 10),
                  axis.title.x = element_blank())

plot


ggplot_PLS <- function(PLS_results)
{
  library(ggplot2)
  PLS_gg <- PLS_results$PLS_summary %>%
    mutate(Month = trunc(Date / 100),
           Day = Date - Month * 100,
           Date = NULL) 
  
  PLS_gg$Date <- ISOdate(2002, 
                         PLS_gg$Month, 
                         PLS_gg$Day)
  
  PLS_gg$Date[PLS_gg$JDay <= 0] <-
    ISOdate(2001, 
            PLS_gg$Month[PLS_gg$JDay <= 0], 
            PLS_gg$Day[PLS_gg$JDay <= 0])
  
  PLS_gg <- PLS_gg %>%
    mutate(VIP_importance = VIP >= 0.8,
           VIP_Coeff = factor(sign(Coef) * VIP_importance))
  
  VIP_plot<- ggplot(PLS_gg,aes(x=Date,y=VIP)) +
    geom_bar(stat='identity',aes(fill=VIP>0.8))
  
  VIP_plot <- VIP_plot +
    scale_fill_manual(name="VIP", 
                      labels = c("<0.8", ">0.8"), 
                      values = c("FALSE"="grey", "TRUE"="blue")) +
    theme_bw(base_size=15) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank())
  
  coeff_plot <- ggplot(PLS_gg,
                       aes(x = Date,
                           y = Coef)) +
    geom_bar(stat ='identity',
             aes(fill = VIP_Coeff)) +
    scale_fill_manual(name = "Effect direction", 
                      labels = c("Advancing",
                                 "Unimportant",
                                 "Delaying"), 
                      values = c("-1" = "red", 
                                 "0" = "grey",
                                 "1" = "dark green")) +
    theme_bw(base_size = 15) +
    ylab("PLS coefficient") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank())
  
  temp_plot <- ggplot(PLS_gg) +
    geom_ribbon(aes(x = Date,
                    ymin = Tmean - Tstdev,
                    ymax = Tmean + Tstdev),
                fill = "grey") +
    geom_ribbon(aes(x = Date,
                    ymin = Tmean - Tstdev * (VIP_Coeff == -1),
                    ymax = Tmean + Tstdev * (VIP_Coeff == -1)),
                fill = "red") +
    geom_ribbon(aes(x = Date,
                    ymin = Tmean - Tstdev * (VIP_Coeff == 1),
                    ymax = Tmean + Tstdev * (VIP_Coeff == 1)),
                fill = "dark green") +
    geom_line(aes(x = Date,
                  y = Tmean)) +
    theme_bw(base_size = 15) +
    ylab(expression(paste(T[mean]," (°C)")))
  
  library(patchwork)
  
  plot<- (VIP_plot +
            coeff_plot +
            temp_plot +
            plot_layout(ncol=1,
                        guides = "collect")
          ) & theme(legend.position = "right",
                    legend.text = element_text(size = 8),
                    legend.title = element_text(size = 10),
                    axis.title.x = element_blank())
  
  plot}

ggplot_PLS(PLS_results)

