install.packages("tidyverse")
install.packages("openair")
install.packages("psych")
install.packages("ggpubr")
install.packages("data.table")
install.packages("grafify")
install.packages("writexl")
install.packages("readxl")
install.packages("tdr")
install.packages("caret")
install.packages("ggpmisc")
install.packages("wesanderson")
install.packages("latticeExtra")
install.packages("grafify")
install.packages("FSA")
library(writexl)
library(tidyverse)
library(openair)
library(ggpubr)
library(readr)
library(readxl)
library(caret)
library(latticeExtra)
library(tdr)
library(ggpmisc)
library(data.table)
library(psych)
library(wesanderson)
library(grafify)
library(FSA)

Ref_w <- read_excel("Reference Wet.xlsx")
Ref_d <- read_csv("Reference_dry.csv")
S1 <- read_csv("Sensor 1.csv")
S2 <- read_csv("Sensor 2.csv")
S3 <- read_csv("Sensor 3.csv")

str(Ref_d)
str(Ref_w)

head(Ref_w)
head(Ref_d)
tail(Ref_w)
tail(Ref_d)

colnames(Ref_w) <- c("date","pm2.5_w","pm10_w")
colnames(Ref_d) <- c("date","pm10_d","pm2.5_d")
colnames(S1) <- c("date","pm10_1","pm2.5_1","pm1_1")
colnames(S2) <- c("date","pm2.5_2","pm10_2","pm1_2")
colnames(S3) <- c("date","pm10_3","pm2.5_3","pm1_3")

?as.POSIXct()

S1$date <- as.POSIXct(S1$date, tz = "UTC", format = c("%m/%d/%Y %H:%M")) 
S2$date <- as.POSIXct(S2$date, tz = "UTC", format = c("%m/%d/%Y %H:%M"))
S3$date <- as.POSIXct(S3$date, tz = "UTC", format = c("%m/%d/%Y %H:%M"))

timePlot(Ref_w, pollutant = c("pm2.5_w","pm10_w"),
         group = F,
         lty = 1,
         lwd = 2,
         ylim = c(0,500))

which.max(Ref_d$pm10_d)

Ref_d[21993,2] <- NA

Wet_start <- as.POSIXct("2025-09-22 00:00:00", tz = "UTC")
Wet_end <- as.POSIXct("2025-11-02 23:59:00", tz = "UTC")
Dry_start <- as.POSIXct("2025-12-08 00:00:00", tz = "UTC")
Dry_end <- as.POSIXct("2026-01-18 23:59:00", tz = "UTC")

Ref_dry <- Ref_d[Ref_d$date >= Dry_start & Ref_d$date <= Dry_end, ]
Ref_wet <- Ref_w[Ref_w$date >= Wet_start & Ref_w$date <= Wet_end, ]

#DRY
Ref_dry <- timeAverage(Ref_dry, avg.time = "hour")
S01 <- S1[ S1$date >= Dry_start & S1$date <= Dry_end, ]
S02 <- S2[S2$date >= Dry_start & S2$date <= Dry_end, ]
S03 <- S3[S3$date >= Dry_start & S3$date <= Dry_end, ]


Combined <- Reduce(function(x, y) merge(x, y, all = T, by = c("date")),list(Ref_dry,S01,S02,S03)) 

Combined <- Combined %>% 
  select(date,pm2.5_d,pm10_d,pm2.5_1,pm10_1,pm2.5_2,pm10_2,pm2.5_3,pm10_3)

comb_2.5 <- Combined %>% 
  select(date,pm2.5_d,pm2.5_1,pm2.5_2,pm2.5_3)
comb_10 <- Combined %>% 
  select(date,pm10_d,pm10_1,pm10_2,pm10_3)

colnames(comb_2.5) <- c("date", "FEM T640", "Sensor_001","Sensor_002","Sensor_003")

comb_day <- timeAverage(comb_10,avg.time = "day")
#DRY 
#TIMESERIES PLOT FOR DRY
jpeg("ENVIRA TIMESERIES PLOT FOR PM10_DRY.jpeg", units = "cm", width = 25, height = 15, res = 300)
timePlot(comb_day, pollutant = c("FEM T640","Sensor_001","Sensor_002","Sensor_003"),
         stack=FALSE,group = T,date.breaks = 10,par.settings = list(fontsize = list(text = 24, family = "Serif")),
         y.relation = "free",lwd=3,lty=1,avg.time="day",
         date.format = "%b %d,%Y",scales = list(x= list (rot = 50)),
         ylab="PM10(ug/m3)",ylim=c(0,200),key.columns = 2,key.font=2,ci=TRUE,
         xlim = as.POSIXct(c("2025-12-08", "2026-01-18")),
         cols = c( "red","blue","goldenrod","black"),key.position = "inside")
dev.off()


#SCATTER PLOT
a<- ggplot(com)
a <-ggplot(comb_10, aes(Y= Sensor_001, X= `FEM T640`))+
  stat_poly_eq(use_label("eq"), size=6 ,family = "serif")+
  stat_poly_eq(label.y = 0.9, size=6  ,family = "serif", vjust=0.7 )+
  geom_point(fill="green", size=4.5,colour="black",pch=21, stroke=1.5) +
  # geom_text(x = 150, y = 230,  color="gray20",size=5 ,family = "serif",label = paste0("MAE=", round(mae, 3)))+
  geom_abline(slope=1, intercept=0, color="black", size=0.9)+
  theme_test()+
  theme(text = element_text(family = "serif",size = 20),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length  = unit(0.2, "cm"),
        panel.border = element_rect(color = "black", size = 1.5),
        plot.margin = margin(0.6,0.6,0.2,0.2, "cm"),
        axis.title.y = element_text(margin = unit(c(0, 1, 0, 0), "mm"),face = "bold", color = "black"),
        axis.text.y = element_text(size = 19, colour = "black", margin = unit(c(1, 1,1,1), "mm")),
        axis.title.x = element_text(face = "bold", size = 19,margin = unit(c(1,0,0,0), "mm")),
        legend.title = element_blank(),
        plot.title = element_text(color = "black",hjust = 0, size= 30, face = "bold"),
        plot.background = element_rect(fill="white",color = "white", size = 1.5),
        axis.text.x= element_text(vjust = 0.5,size = 19, angle = 0, colour = "black"),
        strip.text.x = element_text(size = 25, face = "bold"),
        strip.background = element_rect(fill = "orange", linewidth = 1.3),
        legend.direction = "horizontal",
        legend.background = element_blank(),   
        legend.key.height = unit(0.6, "cm"),
        legend.key.width  = unit(1.1, "cm"),
        legend.text = element_text(size = 15))+
  scale_y_continuous(breaks = seq(0,200,50),expand = c(0, 0), limits = c(0, 200))+
  scale_x_continuous(breaks = seq(0,200,50),expand = c(0, 0), limits = c(0, 200))+
  xlab(expression(bold(FEM~(T640)~PM[10]~(mu*g/m^3))))+
  ylab(expression(bold(Sensor_001~PM[10]~(mu*g/m^3))))

b <-ggplot(comb_10,aes(y= Sensor_002, x= `FEM T640`))+
  stat_poly_eq(use_label("eq"), size=6 ,family = "serif")+
  stat_poly_eq(label.y = 0.9, size=6  ,family = "serif", vjust=0.7 )+
  geom_point(fill="green", size=4.5,colour="black",pch=21, stroke=1.5) +
  # geom_text(x = 150, y = 230,  color="gray20",size=5 ,family = "serif",label = paste0("MAE=", round(mae, 3)))+
  geom_abline(slope=1, intercept=0, color="black", size=0.9)+
  theme_test()+
  theme(text = element_text(family = "serif",size = 20),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length  = unit(0.2, "cm"),
        panel.border = element_rect(color = "black", size = 1.5),
        plot.margin = margin(0.6,0.6,0.2,0.2, "cm"),
        axis.title.y = element_text(margin = unit(c(0, 1, 0, 0), "mm"),face = "bold", color = "black"),
        axis.text.y = element_text(size = 19, colour = "black", margin = unit(c(1, 1,1,1), "mm")),
        axis.title.x = element_text(face = "bold", size = 19,margin = unit(c(1,0,0,0), "mm")),
        legend.title = element_blank(),
        plot.title = element_text(color = "black",hjust = 0, size= 30, face = "bold"),
        plot.background = element_rect(fill="white",color = "white", size = 1.5),
        axis.text.x= element_text(vjust = 0.5,size = 19, angle = 0, colour = "black"),
        strip.text.x = element_text(size = 25, face = "bold"),
        strip.background = element_rect(fill = "orange", linewidth = 1.3),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key.height = unit(0.6, "cm"),
        legend.key.width  = unit(1.1, "cm"),
        legend.text = element_text(size = 15))+
  scale_y_continuous(breaks = seq(0,200,50),expand = c(0, 0), limits = c(0, 200))+
  scale_x_continuous(breaks = seq(0,200,50),expand = c(0, 0), limits = c(0, 200))+
  xlab(expression(bold(FEM~(T640)~PM[10]~(mu*g/m^3))))+
  ylab(expression(bold(Sensor_002~PM[10]~(mu*g/m^3))))


c <-ggplot(comb_10, aes(y=Sensor_003,x= `FEM T640`))+
  stat_poly_eq(use_label("eq"), size=6 ,family = "serif")+
  stat_poly_eq(label.y = 0.9, size=6  ,family = "serif", vjust=0.7 )+
  geom_point(fill="green", size=4.5,colour="black",pch=21, stroke=1.5) +
  # geom_text(x = 150, y = 230,  color="gray20",size=5 ,family = "serif",label = paste0("MAE=", round(mae, 3)))+
  geom_abline(slope=1, intercept=0, color="black", size=0.9)+
  theme_test()+
  theme(text = element_text(family = "serif",size = 20),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length  = unit(0.2, "cm"),
        panel.border = element_rect(color = "black", size = 1.5),
        plot.margin = margin(0.6,0.6,0.2,0.2, "cm"),
        axis.title.y = element_text(margin = unit(c(0, 1, 0, 0), "mm"),face = "bold", color = "black"),
        axis.text.y = element_text(size = 19, colour = "black", margin = unit(c(1, 1,1,1), "mm")),
        axis.title.x = element_text(face = "bold", size = 19,margin = unit(c(1,0,0,0), "mm")),
        legend.title = element_blank(),
        plot.title = element_text(color = "black",hjust = 0, size= 30, face = "bold"),
        plot.background = element_rect(fill="white",color = "white", size = 1.5),
        axis.text.x= element_text(vjust = 0.5,size = 19, angle = 0, colour = "black"),
        strip.text.x = element_text(size = 25, face = "bold"),
        strip.background = element_rect(fill = "orange", linewidth = 1.3),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key.height = unit(0.6, "cm"),
        legend.key.width  = unit(1.1, "cm"),
        legend.text = element_text(size = 15))+
  scale_y_continuous(breaks = seq(0,200,50),expand = c(0, 0), limits = c(0, 200))+
  scale_x_continuous(breaks = seq(0,200,50),expand = c(0, 0), limits = c(0, 200))+
  xlab(expression(bold(FEM~(T640)~PM[10]~(mu*g/m^3))))+
  ylab(expression(bold(Sensor_003~PM[10]~(mu*g/m^3))))



jpeg("ENVIRA SCATTER PLOT FOR PM10.DRY.jpeg",units="cm", width=40, height=15, res=300)
sc <- ggarrange(a,b,c, ncol =  3, nrow = 1)
dev.off()



jpeg("ENVIRA SCATTER PLOT FOR PM10_wet.jpeg",units="cm", width=40, height=15, res=300)
ggarrange(a,b,c, ncol =  3, nrow = 1)
dev.off()






