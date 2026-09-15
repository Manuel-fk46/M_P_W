library(tidyverse)
library(readxl)
library(dplyr)
library(purrr)
library(writexl)


r <- read_csv("Module air /Sensor 3 /MOD-PM-01068-54ea68d82b6c40df8e90720d44bb2d9b.csv")

r <- r |> 
  select(`Local Date Time`,`PM 2.5`,`PM 10`) |> 
  rename( date =`Local Date Time`, pm2.5 = `PM 2.5`, pm10 = `PM 10`)


path <- "Module air /Sensor 3 " 


files <- list.files(path,pattern = "\\.csv$",full.names = TRUE)

d <- files %>% 
  lapply(read_csv) %>% 
  bind_rows()
  
  
write_delim(d,"Ramp sensor 1.txt") 

write.csv(d,"ModuleAir_sensor_3.csv") 

r$date <- lubridate::mdy_hm(r$date)


ModuleAir_sensor_3 <- read_csv("ModuleAir_sensor_3.csv") 

d <- d |> 
  select(Timestamp,`PM2.5 (µg/m³)`,PM10 (µg/m³)) |> 
  rename(date = Timestamp, pm2.5 = `PM2.5 (µg/m³)`, pm10 = `PM10 (µg/m³)`)


pollutants <- colnames(r)[colnames(r) != "date"]


timePlot(r,pollutant = pollutants,
         group = T,
         lty = 1,
         lwd = 2,
         avg.time = "5 min")



X1 <- read_csv("~/Downloads/Ramp_2026-01_to_2026-06-16/sensor_1/20250101-20250701 RAMP2174.csv")
X2 <- read_csv("~/Downloads/Ramp_2026-01_to_2026-06-16/sensor_1/20250701-20260101 RAMP2174.csv")
X3 <- read_csv("~/Downloads/Ramp_2026-01_to_2026-06-16/sensor_1/20260101-20260616 RAMP2174.csv")

c-1
a-2
d-3

a <- rbind(X1,X2,X3)

tail(d, n = 10)
tail(a,n = 10)



