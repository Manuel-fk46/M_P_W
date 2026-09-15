library(readr)
library(tidyverse)
library(openair)
library(readxl)

S1 <- read_excel("AQMESH_S4.xlsx")
S2 <- read_excel("AQMESH_S5.xlsx")
S3 <- read_excel("AQMESH_S6.xlsx")

S1 <- S1 |> 
  select(Project.Time..Interval.start...UTC0.,NOx.Scaled.ugm3,NOx.PreScaled.ugm3,NO2.Scaled.ugm3,NO2.PreScaled.ugm3,NO.Scaled.ugm3,NO.PreScaled.ugm3)

S2 <- S2 |> 
  select(Project.Time..Interval.start...UTC0.,NOx.Scaled.ugm3,NOx.PreScaled.ugm3,NO2.Scaled.ugm3,NO2.PreScaled.ugm3,NO.Scaled.ugm3,NO.PreScaled.ugm3)

S3 <- S3 |> 
  select(Project.Time..Interval.start...UTC0.,NOx.Scaled.ugm3,NOx.PreScaled.ugm3,NO2.Scaled.ugm3,NO2.PreScaled.ugm3,NO.Scaled.ugm3,NO.PreScaled.ugm3)


summary(S1)
summary(S2)
summary(S3)

S1$Project.Time..Interval.start...UTC0. <- as.POSIXct(S1$Project.Time..Interval.start...UTC0., tz = "UTC", format = c("%Y-%m-%d %H:%M %OS"))

?as.POSIXct()

S1
