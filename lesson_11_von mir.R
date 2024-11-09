# To check if the data has gaps:
is.na(Bonn$Tmin) %>% sum()


# wenn nur eine Funktion eines packages benutzt werden soll kann man mit folgender notation darauf zugreifen
# package::function
naniar::vis_mis() # Beispiel










