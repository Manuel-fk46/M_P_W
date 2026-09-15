ref <- read_csv("21 - 26 FEMT640.csv")
ModuleAir_sensor_1 <- read_csv("ModuleAir Sensors combined/ModuleAir_sensor_1.csv")
ModuleAir_sensor_2 <- read_csv("ModuleAir Sensors combined/ModuleAir_sensor_2.csv")
ModuleAir_sensor_3 <- read_csv("ModuleAir Sensors combined/ModuleAir_sensor_3.csv")

sum(duplicated(ref$date))

head(comb_hour)
tail(comb_hour)

comb_hour <- timeAverage(comb,avg.time = "hour")

timePlot(selectByDate(year = 2025,month = 12,ref),pollutant = "PM2.5")

Combined_Ramp <- read_csv("Combined Ramp.csv")


mini2.5 <- 0.1 
maxi2.5 <- 500
mini10 <- 0.1
maxi10 <- 1000
miniRH <- 30
maxiRH <- 100
miniT <- 20
maxiT <- 50
startdate <- ymd_hms("2025-03-01 00:00:00")
enddate <-  ymd_hms("2026-03-01 23:59:00") 

ModuleAir_sensor_1 <- ModuleAir_sensor_1 |> 
  select(timestamp_local,pm25,pm10,sample_rh,sample_temp) |> 
  rename(date = timestamp_local) 

ModuleAir_sensor_2 <- ModuleAir_sensor_2 |> 
  select(timestamp_local,pm25,pm10,sample_rh,sample_temp) |> 
  rename(date = timestamp_local)

ModuleAir_sensor_3 <- ModuleAir_sensor_3 |> 
  select(timestamp_local,pm25,pm10,sample_rh,sample_temp) |> 
  rename(date = timestamp_local)


ModuleAir_sensor_1 <- ModuleAir_sensor_1[ModuleAir_sensor_1$date >= startdate & ModuleAir_sensor_1$date <= enddate, ]
ModuleAir_sensor_2 <- ModuleAir_sensor_2[ModuleAir_sensor_2$date >= startdate & ModuleAir_sensor_2$date <= enddate, ]
ModuleAir_sensor_3 <- ModuleAir_sensor_3[ModuleAir_sensor_3$date >= startdate & ModuleAir_sensor_3$date <= enddate, ]

ref <- ref[ref$date >= startdate & ref$date <= enddate,]

colnames(ref) <- c("date","pm10_ref","pm2.5_ref")

sensorlist <- list(Combined_Ramp,ref)

comb <- reduce(sensorlist,full_join,by = "date")


ModuleAir_sensor_1 <- ModuleAir_sensor_1 |>
  select(date,pm25, pm10, sample_rh, sample_temp) |> 
  filter(between(sample_rh, miniRH, maxiRH))

ModuleAir_sensor_2 <- ModuleAir_sensor_2 |>
  select(date,pm25, pm10, sample_rh, sample_temp) |> 
  filter(between(sample_rh, miniRH, maxiRH))

ModuleAir_sensor_3 <- ModuleAir_sensor_3 |>
  select(date,pm25, pm10, sample_rh, sample_temp) |> 
  filter(between(sample_rh, miniRH, maxiRH))



ModuleAir_sensor_1 <- ModuleAir_sensor_1 |> 
  select(date,pm25, pm10, sample_rh, sample_temp) |>
  distinct(date, .keep_all = TRUE)

ModuleAir_sensor_2 <- ModuleAir_sensor_2 |> 
  select(date,pm25, pm10, sample_rh, sample_temp) |> 
  distinct(date, .keep_all = TRUE)

ModuleAir_sensor_3 <- ModuleAir_sensor_3 |> 
  select(date,pm25, pm10, sample_rh, sample_temp) |> 
  distinct(date, .keep_all = TRUE)

write.csv(comb_hour, "ModuleAir_c.csv")

sensorlist <- list(ModuleAir_sensor_1,ModuleAir_sensor_2, ModuleAir_sensor_3)

combined <- reduce(sensorlist,full_join,by = "date")

#colnames(ModuleAir_sensor_3)[1] <- "date"

comb_hour <- timeAverage(combined,avg.time = "hour")

pollutants <- colnames(ModuleAir_sensor_1)[colnames(ModuleAir_sensor_1) != "date"]

sum(duplicated(ModuleAir_sensor_1$date))


timePlot(ModuleAir_sensor_3, pollutant = pollutants,
         group = FALSE,
         lwd = 1, 
         lty = 1)


sensor_1 <- Ramp_sensor_1
sensor_2 <- Ramp_sensor_2
sensor_3 <- Ramp_sensor_3



Ramp_sensor_1 <- read_csv("Ramp Sensors Combined /Ramp_sensor_1.csv")
Ramp_sensor_2 <- read_csv("Ramp Sensors Combined /Ramp_sensor_2.csv")
Ramp_sensor_3 <- read_csv("Ramp Sensors Combined /Ramp_sensor_3.csv")

Ramp_sensor_1 <- Ramp_sensor_1 |> 
  select(`UTC Date Time`,`PM 2.5`,`PM 10`,Humidity,Temperature) |> 
  rename(date = `UTC Date Time`) 

Ramp_sensor_2 <- Ramp_sensor_2 |> 
  select(`UTC Date Time`,`PM 2.5`,`PM 10`,Humidity,Temperature) |> 
  rename(date = `UTC Date Time`)

Ramp_sensor_3 <- Ramp_sensor_3 |> 
  select(`UTC Date Time`,`PM 2.5`,`PM 10`,Humidity,Temperature) |> 
  rename(date = `UTC Date Time`)


Ramp_sensor_1$date <- lubridate::mdy_hm(Ramp_sensor_1$date)
Ramp_sensor_2$date <- lubridate::mdy_hm(Ramp_sensor_2$date)
Ramp_sensor_3$date <- lubridate::mdy_hm(Ramp_sensor_3$date)


Ramp_sensor_1 <- Ramp_sensor_1[Ramp_sensor_1$date >= startdate & Ramp_sensor_1$date <= enddate, ]
Ramp_sensor_2 <- Ramp_sensor_2[Ramp_sensor_2$date >= startdate & Ramp_sensor_2$date <= enddate, ]
Ramp_sensor_3 <- Ramp_sensor_3[Ramp_sensor_3$date >= startdate & Ramp_sensor_3$date <= enddate, ]


Ramp_sensor_1 <- Ramp_sensor_1 |>
  select(date,`PM 2.5`,`PM 10`,Humidity,Temperature) |> 
  filter(between(Temperature, miniT, maxiT))

Ramp_sensor_2 <- Ramp_sensor_2 |>
  select(date,`PM 2.5`,`PM 10`,Humidity,Temperature) |> 
  filter(between(Temperature, miniT, maxiT))

Ramp_sensor_3 <- Ramp_sensor_3 |>
  select(date,`PM 2.5`,`PM 10`,Humidity,Temperature) |> 
  filter(between(Temperature, miniT, maxiT))



Ramp_sensor_1 <- Ramp_sensor_1 |> 
  select(date,`PM 2.5`,`PM 10`,Humidity,Temperature) |>
  distinct(date, .keep_all = TRUE)

Ramp_sensor_2 <- Ramp_sensor_2 |> 
  select(date,`PM 2.5`,`PM 10`,Humidity,Temperature) |> 
  distinct(date, .keep_all = TRUE)

Ramp_sensor_3 <- Ramp_sensor_3 |> 
  select(date,`PM 2.5`,`PM 10`,Humidity,Temperature) |> 
  distinct(date, .keep_all = TRUE)



timePlot(Ramp_sensor_3, pollutant = pollutants,
         group = FALSE,
         lwd = 1, 
         lty = 1)








S2 <- timeAverage(ModuleAir_sensor_2, avg.time = "hour") 

S3 <- timeAverage(ModuleAir_sensor_3, avg.time = "hour")



write_csv(combined, "combined ModuleAir.csv")

combined <- Reduce(function(x, y) merge(x, y, all = T, by = c("date")),
                   list(Ramp_sensor_1,Ramp_sensor_2,Ramp_sensor_3)) 


comb_hour <- comb_hour |> 
  select(date,pm25.x,pm10.x,sample_rh.x,sample_temp.x,pm25.y,pm10.y,sample_rh.y,sample_temp.y,
        pm25,pm10,sample_rh,sample_temp) |> 
  mutate(avgpm2.5 = rowMeans(across(c(pm25.x, pm25.y,pm25)),na.rm = TRUE)) |> 
  mutate(avgpm10 = rowMeans(across(c(pm10.x, pm10.y, pm10)), na.rm = TRUE)) |> 
  mutate(avgrh = rowMeans(across(c(sample_rh.x,sample_rh.y,sample_rh)), na.rm = TRUE)) |> 
  mutate(avgtemp = rowMeans(across(c(sample_temp.x, sample_temp.y, sample_temp)), na.rm = TRUE))
  


colnames(comb_hour) <- c("date","pm2.5_1","pm10_1","humidity_1","temperature_1","pm2.5_2",
                        "pm10_2","humidity_2","temperature_2","pm2.5_3","pm10_3","humidity_3",
                        "temperature_3","avgpm2.5","avgpm10","avgRh","avgtemp")      



combined <- timeAverage(combined, avg.time = "hour")


summary(combined)


which.max(ModuleAir_sensor_2$pm10)

ModuleAir_sensor_2[381402,3]


write_csv(comb_hour,"Combined Ramp.csv")












