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
library(grafify)


#library(readr)
Reference_dry <- read_csv("Reference_dry.csv")
#View(Reference_dry)

#summary(Reference_Wet)
#library(readxl)
Reference_Wet <- read_excel("Reference Wet.xlsx")
#View(Reference_Wet)

#library(readr)
Sensor_1 <- read_csv("Sensor 1.csv")
#View(Sensor_1)

#library(readr)
Sensor_2 <- read_csv("Sensor 2.csv")
#View(Sensor_2)

#library(readr)
Sensor_3 <- read_csv("Sensor 3.csv")
#View(Sensor_3)


Fem_dry<-Reference_dry %>%
  select(`Timestamp (UTC+0)`,PM2.5,PM10) %>% 
  rename(date=`Timestamp (UTC+0)`,pm2.5_dry=PM2.5,pm10_dry=PM10) 

Fem_wet<-Reference_Wet %>% 
  select(date,`PM2.5 Conc`,`PM10 Conc`) %>% 
  rename(date=date,pm2.5_wet=`PM2.5 Conc`,pm10_wet=`PM10 Conc`)

Sensor_1<-Sensor_1 %>% 
  select(DATE,PM2.5,PM10,PM1) %>% 
  rename(date=DATE,pm2.5_01=PM2.5,pm1_01=PM1,pm10_01=PM10) 

Sensor_2<-Sensor_2 %>% 
  select(DATE,PM1,PM10,PM2.5) %>% 
  rename(date=DATE,pm2.5_02=PM2.5,pm1_02=PM1,pm10_02=PM10) 

Sensor_3<-Sensor_3 %>% 
  select(DATE,PM10,PM1,PM2.5) %>% 
  rename(date=DATE,pm2.5_03=PM2.5,pm1_03=PM1,pm10_03=PM10)


T640_dry<-Fem_dry %>% 
  select(date,pm2.5_dry) 

T640_wet<-Fem_wet %>% 
  select(date,pm2.5_wet)

Sen_01<-Sensor_1 %>% select(date,pm2.5_01)

Sen_02<-Sensor_2 %>% select(date,pm2.5_02) 

Sen_03<-Sensor_3 %>% select(date,pm2.5_03) %>% 


#T640_dry$date<-lubridate::ymd_hms(Fem_dry$date)
#T640_wet$date<-lubridate::ymd_hms(Fem_wet$date)
Sen_01$date<-lubridate::mdy_hm(Sensor_1$date)
Sen_02$date<-lubridate::mdy_hm(Sensor_2$date)
Sen_03$date<-lubridate::mdy_hm(Sensor_3$date)

which.max(Fem_dry$pm2.5_dry)
which.max(Fem_dry$pm10_dry)

Fem_dry[21993,2]<-NA 
Fem_dry[21993,3]<-NA 


colnames(Fem_dry)

Dry_Start_date<-ymd_hms("2025-12-08 00:00:00")

Dry_end_date<-ymd_hms("2026-01-18 23:59:00")

Wet_start_date<-ymd_hms("2025-09-22 00:00:00")

wet_end_date<-ymd_hms("2025-11-02 23:59:00")


#DRY
Filtered_dry_fem<-T640_dry %>% 
  select(date,pm2.5_dry) %>% 
  filter(date>=Dry_Start_date,date<=Dry_end_date)

fem_hourly_dry<-timeAverage(Filtered_dry_fem, avg.time = "hour")

filtered_01<-Sen_01 %>% select(date,pm2.5_01) %>% filter(date>=Dry_Start_date,date<=Dry_end_date)

filtered_02<-Sen_02 %>% select(date,pm2.5_02) %>% filter(date>=Dry_Start_date,date<=Dry_end_date)

filtered_03<-Sen_03 %>% select(date,pm2.5_03) %>% filter(date>=Dry_Start_date,date<=Dry_end_date)


#WET
Filtered_wet_fem<-T640_wet %>% 
  select(date,pm2.5_wet) %>% 
  filter(date>=Wet_start_date,date<=wet_end_date)

fem_hourly_wet <- timeAverage(Filtered_wet_fem,avg.time = "hour")

filtered_01<-Sen_01 %>% select(date,pm2.5_01) %>% filter(date>=Wet_start_date,date<=wet_end_date)

filtered_02<-Sen_02 %>% select(date,pm2.5_02) %>% filter(date>=Wet_start_date,date<=wet_end_date)

filtered_03<-Sen_03 %>% select(date,pm2.5_03) %>% filter(date>=Wet_start_date,date<=wet_end_date)




combined<-Reduce(function(x,y) merge(x,y,all=T,by=c("date")),list(fem_hourly_dry,filtered_01,filtered_02,filtered_03))
#combined<-na.omit(combined)
colnames(combined)<- c("date","FEM T640","Sensor_001","Sensor_002","Sensor_003")

comb_day<-timeAverage(combined,avg.time = "day")
#comb_day<-na.omit(comb_day)


#TIMESERIES PLOT 
#DRY
jpeg("ENVIRA_TIMESERIES_PLOT_FOR_PM2.5_DRY.jpeg",units="cm", width=25, height=15, res=270)
timePlot(comb_day, pollutant = c("FEM T640","Sensor_001","Sensor_002","Sensor_003"),
         stack=FALSE,group = T,date.breaks = 10,par.settings = list(fontsize = list(text = 24, family = "Serif")),
         y.relation = "free",lwd=3,lty=1,avg.time="day",
         date.format = "%b %d,%Y",scales = list(x= list (rot = 50)),
         ylab="PM2.5(ug/m3)",ylim=c(0,80),key.columns = 2,key.font=2,ci=TRUE,
         xlim = as.POSIXct(c("2025-12-08", "2026-01-18")),
         cols = c( "red","blue","goldenrod","black"),key.position = "inside")
dev.off()


?cols()


#WET
jpeg("ENVIRA_TIMESERIES_PLOT_FOR_PM1_WET.jpeg",units="cm", width=25, height=15, res=270)
timePlot(comb_day, pollutant = c("Sensor_001","Sensor_002","Sensor_003"),
         stack=FALSE,group = T,date.breaks = 10,par.settings = list(fontsize = list(text = 18, family = "Serif")),
         y.relation = "free",lwd=3,lty=1,avg.time="day",
         date.format = "%b %d,%Y",scales = list(x= list (rot = 50)),
         ylab="PM1(ug/m3)",ylim=c(0,10),key.columns = 2,key.font=2,ci=TRUE,
         xlim = as.POSIXct(c("2025-09-22", "2025-11-02")),
         cols = c( "blue","goldenrod","black"),key.position = "inside")
dev.off()

"maroon"
"green"
colnames(combined)

library(grafify)
library(FSA)
#summary(combined)


#bar_pm2.5_dry<- combined %>%
  #select(date,T640,Sensor_001,Sensor_002,Sensor_003)

#bar_pm2.5_wet<-combined %>% 
  #select(date,T640,Sensor_001,Sensor_002,Sensor_003)

#bar_pm10_dry <- combined %>%
  #select(date,T640,Sensor_001,Sensor_002,Sensor_003)

#bar_pm10_wet <- combined %>%
  #select(date,T640,Sensor_001,Sensor_002,Sensor_003)

#bar_pm1_dry <- combined %>%
  #select(date,Sensor_001,Sensor_002,Sensor_003)

#bar_pm1_wet <- combined %>%
  #select(date,Sensor_001,Sensor_002,Sensor_003)

intr_dry <- combined %>% tidyr::gather(Site, combined, Sensor_003:T640, na.rm = TRUE)
sum_dry <- Summarize(combined ~ Site, 
                     data = intr_dry, na.rm = TRUE)

intr_wet <- combined %>% tidyr::gather(Site, combined, Sensor_003:T640, na.rm = TRUE)
sum_wet <- Summarize(combined ~ Site, 
                   data = intr_wet, na.rm = TRUE)

#summary <- sum_dry[c(2:nrow(sum_wet), 1), ] # move fem to the last row

sum_dry[4,1]<-"FEM T640"

sum_wet[4,1]<-"FEM T640"


intnew <- sum_dry %>%
  select(Site, mean, median, sd)

intnew <- sum_wet %>%
  select(Site, mean, median, sd)

colnames(intnew) <- c("Site","Mean ± SD", "Median", "sd") # change columns names

inta <- intnew %>%
  tidyr::gather(metric, mean, `Mean ± SD`:Median,-Site, na.rm = TRUE) 
inta$sd[3:4] <- NA
#View(inta)

intnew <- sum_wet %>%
  select(Site, mean, median, sd)


colnames(intnew) <- c("Site","Mean ± SD", "Median", "sd") # change columns names

inta <- intnew %>%
  tidyr::gather(metric, mean, `Mean ± SD`:Median,-Site, na.rm = TRUE) 
inta$sd[5:8] <- NA
#View(inta)


 

#DRY BAR PLOT
jpeg(" ENVIRA BAR PLOT FOR PM2.5_DRY.jpeg",units="cm", width=23, height=17, res=300)
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
  scale_y_continuous(breaks = seq(0,60,10),expand = c(0, 0), limits = c(0,60))+
  ylab(expression(bold(PM[2.5]~~(mu*g/m^3))))
dev.off()

#WET BAR PLOT 
jpeg(" ENVIRA BAR PLOT FOR PM1_WET.jpeg",units="cm", width=23, height=17, res=300)
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
  scale_y_continuous(breaks = seq(0,10,5),expand = c(0, 0), limits = c(0, 10))+
  ylab(expression(bold(PM[1]~~(mu*g/m^3))))
dev.off()



#SCATTERPLOT 
a<-ggplot(combined,aes(y=Sensor_001,x=`FEM T640`))+
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
  scale_y_continuous(breaks = seq(0,120,20),expand = c(0, 0), limits = c(0, 120))+
  scale_x_continuous(breaks = seq(0,120,20),expand = c(0, 0), limits = c(0, 120))+
  xlab(expression(bold(FEM~(T640)~PM[2.5]~(mu*g/m^3))))+
  ylab(expression(bold(Sensor_001~PM[2.5]~(mu*g/m^3))))

b<-ggplot(combined,aes(y=Sensor_002,x=`FEM T640`))+
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
  scale_y_continuous(breaks = seq(0,120,20),expand = c(0, 0), limits = c(0, 120))+
  scale_x_continuous(breaks = seq(0,120,20),expand = c(0, 0), limits = c(0, 120))+
  xlab(expression(bold(FEM~(T640)~PM[2.5]~(mu*g/m^3))))+
  ylab(expression(bold(Sensor_002~PM[2.5]~(mu*g/m^3))))

c<-ggplot(combined,aes(y=Sensor_003,x=`FEM T640`))+
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
  scale_y_continuous(breaks = seq(0,120,20),expand = c(0, 0), limits = c(0, 120))+
  scale_x_continuous(breaks = seq(0,120,20),expand = c(0, 0), limits = c(0, 120))+
  xlab(expression(bold(FEM~(T640)~PM[2.5]~(mu*g/m^3))))+
  ylab(expression(bold(Sensor_003~PM[2.5]~(mu*g/m^3))))

  

jpeg("ENVIRA SCATTER PLOT FOR PM2.5_DRY.jpeg",units="cm", width=40, height=15, res=300)
ggarrange(a,b,c, ncol =  3, nrow = 1)
dev.off()


jpeg("ENVIRA SCATTER PLOT FOR PM2.5_wet.jpeg",units="cm", width=40, height=15, res=300)
ggarrange(a,b,c, ncol =  3, nrow = 1)
dev.off()

#pearsons correlation  

aa <- combined %>%
  select(`FEM T640`, Sensor_003)
aa <- na.omit(aa)

summary(aa)

timePlot(combined, pollutant = c("pm2.5_wet", "pm2.5_01"))
#Pearsons Correlation
cor(aa$Sensor_001, aa$`FEM T640`,method = "pearson")^2
cor(aa$Sensor_002, aa$`FEM T640`,method = "pearson")^2
cor(aa$Sensor_003, aa$`FEM T640`,method = "pearson")^2


#mae
mean(abs(aa$Sensor_001 - aa$`FEM T640`))
mean(abs(aa$Sensor_002 - aa$`FEM T640`))
mean(abs(aa$Sensor_003 - aa$`FEM T640`))

#sqrt(mean((a1$Sensor_002 - a1$`FEM(T640)`)^2))/mean(a1$`FEM(T640)`)

#mbe
mean(aa$Sensor_001 - aa$`FEM T640`)
mean(aa$Sensor_002 - aa$`FEM T640`)
mean(aa$Sensor_003 - aa$`FEM T640`)

#rmse
sqrt(mean((aa$Sensor_001 - aa$`FEM T640`)^2))
sqrt(mean((aa$Sensor_002 - aa$`FEM T640`)^2))
sqrt(mean((aa$Sensor_003 - aa$`FEM T640`)^2))



which.min(bar_pm2.5_dry$T640)
which.min(bar_pm2.5_dry$Sensor_001)
which.min(bar_pm2.5_dry$Sensor_002)
which.min(bar_pm2.5_dry$Sensor_003)
which.max(bar_pm2.5_dry$T640)
which.max(bar_pm2.5_dry$Sensor_001)
which.max(bar_pm2.5_dry$Sensor_002)
which.max(bar_pm2.5_dry$Sensor_003)

which.max(comb_day$pm2.5_wet)
which.max(comb_day$pm2.5_01)
which.max(comb_day$pm2.5_02)
which.max(comb_day$pm2.5_03)

which.min(comb_day$pm2.5_wet)
which.min(comb_day$pm2.5_01)
which.min(comb_day$pm2.5_02)
which.min(comb_day$pm2.5_03)

summary(bar_pm10_dry)
summary(bar_pm10_wet)
summary(bar_pm2.5_dry)
summary(bar_pm2.5_wet)
summary(hourly_dry)
summary(hourly_wet)
summary(Filtered_01)
summary(filtered_02)
summary(filtered_03)
summary(combined)


colnames(comb_day)

jpeg("ENVIRA DUAL AXIS PLOT FOR PM10 DRY .jpeg",units="cm", width=25, height=20, res=270)

ggplot(comb_day, aes(x = date)) +
  geom_line(aes(y = T640/5, colour = "T640"), lwd = 1) +
  geom_line(aes(y = Sensor_001, colour = "Sensor_001"), lwd = 1) +
  geom_line(aes(y = Sensor_002, colour = "Sensor_002"), lwd = 1) +
  geom_line(aes(y = Sensor_003, colour = "Sensor_003"), lwd = 1) +
  
  scale_colour_manual(
    values = c("T640" = "#FF0000",
               "Sensor_001" = "#000000", 
               "Sensor_002" = "#FFFF00", 
               "Sensor_003" = "#00BFFF")) +
  
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    expand = c(0, 0),
    limits = c(0, 100),
    name = expression(bold(Envira~~PM[10]~~(mu*g/m^3))),
    sec.axis = sec_axis(~ . *5, breaks = seq(0, 500, 100), 
                        name = expression(bold((Fem(T640))~~PM[10]~~(mu*g/m^3))))) +
  
  scale_x_datetime(date_breaks = "week", date_labels = "%Y-%m-%d", 
                   expand = c(0, 0)) +
  #scale_x_discrete( breaks = c("2025-02-17 00:09:00", "2025-02-18 12:00:00", "2025-02-19 23:58:00")) +
  
  theme_bw() +
  theme(text = element_text(family = "serif", face = "bold"),
        axis.ticks = element_line(size = 0.3), axis.ticks.length = unit(0.2, "cm"),
        panel.border = element_rect(color = "black", size = 1.5), 
        axis.title.y.right = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold", margin = unit(c(0,1,0,0), "mm"), color = "black", size = 14),
        axis.text.y = element_text(face = "bold", margin = unit(c(1,0,0,0), "mm"), color = "black", size = 12),
        axis.title.x = element_blank(), legend.title = element_blank(),
        plot.title = element_text(color = "black", hjust = 1, size = 16, face = "bold"),
        plot.background = element_rect(fill = "white", color = "white", size = 1.5),
        axis.text.x = element_text(vjust = 0.5, size = 12, face = "bold", angle = 60, colour = "black"),
        legend.position = c(0.7, 0.9), legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key.height = unit(0.7, "cm"), legend.key.width = unit(1.4, "cm"),
        legend.spacing.x = unit(1, "mm"), legend.key.spacing.y = unit(1, "cm"),
        legend.text = element_text(size = 20)) +
  guides(color = guide_legend(ncol = 2))
dev.off()


#intra_unit_measurement_variability
S1_mean<-mean(combined$Sensor_001, na.rm= TRUE)
S2_mean<-mean(combined$Sensor_002, na.rm= TRUE)
S3_mean<-mean(combined$Sensor_003, na.rm= TRUE)

sensor_means<- c(S1_mean,S2_mean,S3_mean)
Intra_unit_variability<- sd(sensor_means)
Relative_Variability<- (Intra_unit_variability/ mean(sensor_means)) * 100


