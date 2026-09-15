library(readr)
library(tidyverse)
library(openair)
library(FSA)
E_d <- read_csv("Ecotech_data.csv", 
                         col_types = cols_only(Date_time = col_guess(), 
                                               `NO Conc` = col_guess(), `NOx Conc` = col_guess(), 
                                               `NO2 Conc` = col_guess()))


E_d$Date_time <- lubridate::dmy_hm(E_d$Date_time)

colnames(E_d) <- c("date", "NO", "NOx", "NO2")

head(E_d)
tail(E_d)

e.day <- timeAverage(E_d,avg.time = "day")


jpeg("Daily Oxides of Nitrogen.jpeg", units = "cm", width = 25, height = 15, res = 300)
timePlot(e.day, pollutant = c("NO", "NOx", "NO2"),
         stack=FALSE,group = T,date.breaks = 10,par.settings = list(fontsize = list(text = 24, family = "Serif")),
         y.relation = "free",lwd=3,lty=1,avg.time="day",
         date.format = "%b %d,%Y",scales = list(x= list (rot = 50)),
         ylab="OXIDES OF NITROGEN(ppb)",ylim=c(0,40),key.columns = 3,key.font=2,ci=TRUE,
         #xlim = as.POSIXct(c("2025-11-02", "2025-09-22")),
         cols = c( "blue","goldenrod","black"),key.position = "inside")
dev.off()


colnames(e.day)
which.max(e.min$NO2)
e.min[4771,4]


intr_wet <- e.min %>% tidyr::gather(Site, e.min, NO:NO2, na.rm = TRUE)
sum_wet <- Summarize(e.min ~ Site, 
                     data = intr_wet, na.rm = TRUE)

#summary <- sum_dry[c(2:nrow(sum_wet), 1), ] # move fem to the last row

sum_dry[4,1]<-"FEM T640"


