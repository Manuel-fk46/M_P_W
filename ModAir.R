
s1<- read_csv("Module air / Sensor 1  /MOD-PM-01070-3eba1ee81b914b13b4daf69bc56f3488.csv")

s1a<- read_csv("Module air / Sensor 1  /MOD-PM-01070-52dc0c91f6f84bf9977cc8318d7659c8.csv")

s1b<- read_csv("Module air / Sensor 1  /MOD-PM-01070-67d415ee1ddf4b908df82a2bf54b21dc.csv")

s1c<- read_csv("Module air / Sensor 1  /MOD-PM-01070-a9aa9e624cfa4ebb9096b897803a35e1.csv")



s2  <- read_csv("Module air /Sensor 2/MOD-PM-01069-0ec0a6ddcca446ecb994cb8759104364.csv")
s2a <- read_csv("Module air /Sensor 2/MOD-PM-01069-658d6b57f8f24a838a6410c106894e51.csv")
s2b <- read_csv("Module air /Sensor 2/MOD-PM-01069-4466e138153c433ea6ccd2e77d947fed.csv")
s2c <- read_csv("Module air /Sensor 2/MOD-PM-01069-fda398096b4b468c9b1cab563b81c67c.csv")
 
s3 <- read_csv("Module air /Sensor 3 /MOD-PM-01068-54ea68d82b6c40df8e90720d44bb2d9b.csv")
s3a<- read_csv("Module air /Sensor 3 /MOD-PM-01068-1645c1ce2aeb487eab144a40278c000e.csv")
s3b<- read_csv("Module air /Sensor 3 /MOD-PM-01068-406582e6348d47ba828c554e7dffbdea.csv")
s3c<- read_csv("Module air /Sensor 3 /MOD-PM-01068-b09b12593f6845c1a04ca5bcafe9d75e.csv")

s3d <- rbind(s3,s3a,s3b,s3c)

s3da <- s3d |> 
  select(timestamp_local,pm25,pm10) |> 
  rename(date = timestamp_local) 


mini2.5 <- 0.1 
maxi2.5 <- 500
mini10 <- 0.1
maxi10 <- 1000
miniRH <- 30
maxiRH <- 100
miniT <- 20
maxiT <- 50
startdate <- ymd_hms("2023-11-06 00:00:00")
enddate <-  ymd_hms("2024-11-06 23:59:00") 


S1 <- read_csv("S1.csv")
S2 <- read_csv("S2.csv")
S3 <- read_csv("S3.csv")


S1 <- S1 |> 
  select(timestamp_local,pm25,pm10,sample_rh,sample_temp) |> 
  rename(date = timestamp_local, pm2.5 = pm25, pm10 = pm10, rh = sample_rh, temp = sample_temp)

S2 <- S2 |> 
  select(timestamp_local,pm25,pm10,sample_rh,sample_temp) |> 
  rename(date = timestamp_local, pm2.5 = pm25, pm10 = pm10, rh = sample_rh, temp = sample_temp)


S3 <- S3 |> 
  select(timestamp_local,pm25,pm10,sample_rh,sample_temp) |> 
  rename(date = timestamp_local, pm2.5 = pm25, pm10 = pm10, rh = sample_rh, temp = sample_temp)



S01 <- S1[S1$date >= startdate & S1$date <= enddate, ]
S02 <- S2[S2$date >= startdate & S2$date <= enddate, ]
S03 <- S3[S3$date >= startdate & S3$date <= enddate, ]


a1 <- S2 |> 
  select(date,pm2.5, pm10, rh, temp) |> 
  filter(pm2.5 > 500)

sum(duplicated(S01$date))

dup1 <-S01[!duplicated(S01$date), ]
dup2 <-S02[!duplicated(S02$date), ]
dup3 <-S03[!duplicated(S03$date), ]

dup <- timeAverage(dup,avg.time = "hour")

timePlot(selectByDate(year = 2023,month = 11, S3),pollutant = "pm2.5",
          avg.time = "5 min")

timePlot(dup01, pollutant = c("rh","temp"),group = T)


dup01 <- dup1 |> 
  select(date, pm2.5, pm10,rh,temp) |> 
  filter(temp > 50)

dup02 <- dup2 |> 
  select(date, pm2.5, pm10,rh,temp) |> 
  filter(temp > 50)

dup03 <- dup3 |> 
  select(date, pm2.5, pm10,rh,temp) |> 
  filter(temp > 50)



which.min(dup03$rh)


dup01[1417,4]
dup02[3180,4]
dup03[2319,4]


duplist <- list(dup1, dup2, dup3)

dup <- reduce(duplist, full_join, by = "date")



avg01 <- timeAverage(dup1, avg.time = "hour")
avg02 <- timeAverage(dup2, avg.time = "hour")
avg03 <- timeAverage(dup3, avg.time = "hour")


mod1 <- avg01 |>
  select(date,pm2.5,pm10,pm10,rh,temp) |> 
  filter(between(pm2.5, mini2.5, maxi2.5)) |> 
  filter(between(pm10, mini10, maxi10)) |> 
  filter(between(rh, miniRH, maxiRH)) |> 
  filter(between(temp, miniT, maxiT))

mod2 <- avg02 |>
  select(date,pm2.5,pm10,pm10,rh,temp) |> 
  filter(between(pm2.5, mini2.5, maxi2.5)) |> 
  filter(between(pm10, mini10, maxi10)) |> 
  filter(between(rh, miniRH, maxiRH)) |> 
  filter(between(temp, miniT, maxiT))

mod3 <- avg03 |>
  select(date,pm2.5,pm10,pm10,rh,temp) |> 
  filter(between(pm2.5, mini2.5, maxi2.5)) |> 
  filter(between(pm10, mini10, maxi10)) |> 
  filter(between(rh, miniRH, maxiRH)) |> 
  filter(between(temp, miniT, maxiT))

which.min(mod3$rh)
mod1[2788,4]

pollutants <- colnames(mod1)[colnames(mod1) !="date"]

sensorlist <- list(mod1,mod2,mod3)
combined <- reduce(sensorlist,full_join,by = "date")




 