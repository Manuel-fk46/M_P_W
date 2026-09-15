library(data.table)
library(tidyverse)
library(openair)
library(ggpubr)
library(readxl)
library(writexl)
library(knitr)
library(xts)
library(tidyr)
library(dplyr)
library(tdr)
library(caret)
library(ggpmisc)
library(wesanderson)
library(grafify)
library(latticeExtra)
library(psych)
library(FSA)
library(corrplot)



air.G <- read_csv("gsm_97f4C.csv", col_types = cols_only(`UTC Date/Time` = col_guess(), 
                                                             `PM2.5 (μg/m³) corrected` = col_guess(), 
                                                             `PM10 (μg/m³)` = col_guess()))

ref <- read_csv("T640(1).csv")


colnames(ref) <- c("date", "PM10_fem", "PM2.5_fem")
colnames(air.G) <- c("date", "PM2.5_01", "PM10_01")

start <- ymd_hm("2026-04-07 18:00")
end <- ymd_hm("2026-04-14 10:00") 

ref$date <- ref[ref$date >= start & ref$date <= end, ]
air.G$date <- air.G[air.G$date >= start & air.G$date <= end, ]


ref<- timeAverage(ref,avg.time = "5 min")

air.G <- timeAverage(air.G, avg.time = "5 min")

combined <- Reduce(function(x, y) merge(x, y, all = T, by = c("date")),list(air.G,ref)) 

comb_day <- combined %>% 
  select(date,PM2.5_fem,PM2.5_01)

comb_day <- timeAverage(comb_day,avg.time = "5 min")


colnames(comb_day) <- c("date", "T640", "Sensor_001")
  
timePlot(comb_day, pollutant = pollutants,
         stack=FALSE,group = T,date.breaks = 10,par.settings = list(fontsize = list(text = 24, family = "Serif")),
         y.relation = "free",lwd=3,lty=1,avg.time="day",
         date.format = "%b %d,%Y",scales = list(x= list (rot = 50)),
         ylab="PM10(ug/m3)",ylim=c(0,150),key.columns = 2,key.font=2,ci=TRUE,
         xlim = as.POSIXct(c("2026-04-07", "2026-04-14")),
         cols = c( "red","blue"),key.position = "inside")



intr_dry <- comb_day %>% tidyr::gather(Site, comb_day, Sensor_001:T640, na.rm = TRUE)
sum_dry <- Summarize(comb_day ~ Site, 
                     data = intr_dry, na.rm = TRUE)

sum_dry[2,1]<-"FEM T640"

intnew <- sum_dry %>%
  select(Site, mean, median, sd)

colnames(intnew) <- c("Site","Mean ± SD", "Median", "sd") # change columns names

inta <- intnew %>%
  tidyr::gather(metric, mean, `Mean ± SD`:Median,-Site, na.rm = TRUE) 
inta$sd[3:4] <- NA
#View(inta)


ggplot(inta, aes(x =  fct_inorder(Site), y = mean, fill = metric)) +
  geom_bar(stat="identity",position=position_dodge(0.9),  color="black", linewidth=0.9) +
  geom_errorbar(data = inta, aes(ymin = mean - sd, ymax = mean + sd),
                position = position_dodge(width = 0.9), width = 0.2, size=0.9)+
  #scale_fill_manual(values=wes_palette(name="Rushmore1"))+
  #scale_fill_brewer(palette ="Set1")+
  #scale_fill_manual(values = c("chartreuse4","darkgoldenrod3","blue","deeppink3","darkorchid3"))+
  #scale_fill_manual(values = c("red", "white", "darkgoldenrod1","#56B4E9","darkgreen"))+
  scale_fill_grafify(palette = "okabe_ito")+ #okabe_ito,  vibrant, bright, safe, fishy, muted , kelly
  theme_test()+
  theme(text = element_text(family = "serif", face="bold"),
        axis.ticks = element_line(size = 1.6), 
        axis.ticks.length  = unit(0.2, "cm"),
        panel.border = element_rect(color = "black", size = 1.5),
        plot.margin = margin(0.7,0.7,0.7,0.7, "cm"),
        axis.title.y = element_text(face = "bold",margin = unit(c(0, 1, 0, 0), "mm"), color = "black", size = 20),
        axis.text.y = element_text(size = 18,face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(color = "black",hjust = 1, size= 30, face = "bold"),
        plot.background = element_rect(fill="white",color = "white", size = 1.5),
        axis.text.x= element_text(vjust = 0.5, size = 18,face="bold",angle = 0, colour = "black"),
        legend.position = c(0.30, 0.94),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key.height = unit(0.7, "cm"),
        legend.key.width  = unit(1.4, "cm"),
        legend.text = element_text(size = 18))+
  #scale_y_continuous(breaks = seq(0,300,20))+
  scale_y_continuous(breaks = seq(0,120,30),expand = c(0, 0), limits = c(0,120))+
  ylab(expression(bold(PM[2.5]~~(mu*g/m^3))))



aa <- comb_day %>% 
  select(T640,Sensor_001)

aa <- na.omit(comb_day)

cor(aa$Sensor_001,aa$T640, method = "pearson")^2


openair::corPlot(comb_day,pollutants = c("FEM T640", "Sensor_001"),
                 type = "",
                 method = "pearson",
                 use = "pairwise.complete.obs",
                 cluster = TRUE,
                 plot = TRUE,
                 auto.text = TRUE)
                 



