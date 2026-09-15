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
library(readxl)

Ref_d <- read_csv("Reference_dry.csv")

zwin.c <- read_excel("Evaluated Sensors /Zwinsoft/Micro_sensor_challenge_data_for_Accra_Ghana-2026.xlsx", 
                     sheet = "Zwins_08")

S1 <- read_csv("Evaluated Sensors /Aurrasure /Dry.A_01.csv")
S2 <- read_csv("Evaluated Sensors /Aurrasure /Dry.A_02.csv")
S3 <- read_csv("Evaluated Sensors /Aurrasure /Dry.A_03.csv")


Ref_d <- Ref_d |> 
  select(`Timestamp (UTC+0)`,PM10) |> 
  rename(date = `Timestamp (UTC+0)`, pm10.ref = PM10)

S1 <- S1 %>% 
  select(time,parameter_values_pm10) |> 
  rename(date = time, pm10.1 = parameter_values_pm10) 

S2 <- S2 %>%
  select(time,parameter_values_pm10) |> 
  rename(date = time, pm10.2 = parameter_values_pm10) 

S3 <- S3 %>% 
  select(time,parameter_values_pm10) |> 
  rename(date = time, pm10.3 = parameter_values_pm10) 


head(combined)
tail(combined)

colnames(Ref_d) <- c("date","pm10_d","pm2.5_d")
colnames(S1) <- c("date","pm10_1","pm2.5_1")
colnames(S2) <- c("date","pm2.5_2","pm10_2")
colnames(S3) <- c("date","pm10_3","pm2.5_3")

colnames(zwin.c) <- c("date", "pm2.5_03", "pm2.5_01", "pm2.5_02", "pm2.5_w")

S1$date <- lubridate::mdy_hm(S1$date)

S3 <- S3 |> 
  distinct(date,.keep_all = TRUE)

zwin.c <- zwin.c %>%
  select(date,pm2.5_w,pm2.5_01,pm2.5_02,pm2.5_03)

combined <- zwin.c 

sum(duplicated(S3))

colnames(combined) <- c("date", "FEM T640","Sensor_001","Sensor_002","Sensor_003")

combined$`FEM T640` <- as.numeric(combined$`FEM T640`)
combined$Sensor_001 <- as.numeric(combined$Sensor_001) 
combined$Sensor_002 <- as.numeric(combined$Sensor_002) 
combined$Sensor_003 <- as.numeric(combined$Sensor_003)

combined <- combined[combined$date >= Dry_start & combined$date <= Dry_end, ]


?as.POSIXct()

S1$date <- as.POSIXct(S1$date, tz = "UTC", format = c("%m/%d/%Y %H:%M")) 
S2$date <- as.POSIXct(S2$date, tz = "UTC", format = c("%m/%d/%Y %H:%M"))
S3$date <- as.POSIXct(S3$date, tz = "UTC", format = c("%m/%d/%Y %H:%M"))

timePlot(S1, pollutant = c("pm10_1", "pm2.5_1"),
         group = F,
         lty = 1,
         lwd = 2)

which.max(Ref_d$pm2.5_d)

Ref_d[21993,2] 

Dry_start <- as.POSIXct("2025-12-08 00:00:00", tz = "UTC")
Dry_end <- as.POSIXct("2026-01-18 23:59:00", tz = "UTC")

#DRY
Ref_d <- Ref_d[Ref_d$date >= Dry_start & Ref_d$date <= Dry_end, ]

Ref_d <- timeAverage(Ref_d,avg.time = "5 min")

S01 <- S1[S1$date >= Dry_start & S1$date <= Dry_end, ]
S02 <- S2[S2$date >= Dry_start & S2$date <= Dry_end, ]
S03 <- S3[S3$date >= Dry_start & S3$date <= Dry_end, ]

combined <- Reduce(function(x, y) merge(x, y, all = T, by = c("date")),list(S01,S02,S03)) 

colnames(combined) <- c("date","Sensor_001","Sensor_002","Sensor_003")

comb_day <- timeAverage(combined,avg.time = "day") 

which.max(comb_day$Sensor_003)

getwd()


jpeg("AURRASURE_TIMESERIES_PLOT_FOR_PM10_DRY.jpeg", units = "cm", width = 35, height = 20, res = 300)
ggplot(comb_day, aes(x = date)) +
  geom_line(aes(y = `FEM T640`, colour = "FEM T640"), lwd = 1) +
  geom_line(aes(y = Sensor_001, colour = "Sensor_001"), lwd = 1) +
  geom_line(aes(y = Sensor_002, colour = "Sensor_002"), lwd = 1) +
  geom_line(aes(y = Sensor_003, colour = "Sensor_003"), lwd = 1) +
  scale_colour_manual(
    values = c("FEM T640" = "red",
               "Sensor_001" = "blue",
               "Sensor_002" = "goldenrod",
               "Sensor_003" = "black")) +
  
  scale_y_continuous(breaks = seq(0, 250, 50), expand = c(0, 0), limits = c(0, 250),
                     name = expression(PM[10]~(mu*g/m^3))) +
  
  scale_x_datetime(date_breaks = "week", date_labels = "%b %d,%Y",
                   limits = as.POSIXct(c("2025-12-08", "2026-01-18")),
                   expand = c(0, 0)) +
  theme_test() +
  theme(text = element_text(family = "serif", size = 22),
        axis.ticks = element_line(size = 1.0),
        axis.ticks.length = unit(0.2, "cm"),
        panel.border = element_rect(color = "black", size = 1.5),
        plot.margin = margin(0.6, 0.6, 0.2, 0.2, "cm"),
        axis.title.y = element_text(face = "plain", color = "black",size = 22),
        axis.text.y = element_text(colour = "black",size = 20),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "white", color = "white", size = 1.5),
        axis.text.x = element_text(hjust = 1, vjust = 1, angle = 50, colour = "black",size = 18),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.94),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.text = element_text(size = 22),
        legend.key.height = unit(0.6, "cm"),
        legend.key.width = unit(1.1, "cm")) +
  guides(colour = guide_legend(ncol = 2))
dev.off()


#DRY 
#TIMESERIES PLOT FOR DRY
#jpeg("ZWINSOFT_TIMESERIES_PLOT_FOR_PM2.5_DRY.jpeg", units = "cm", width = 20, height = 15, res = 300)
#timePlot(comb_day, pollutant = c("FEM T640","Sensor_001","Sensor_002","Sensor_003"),
 #        stack=FALSE,group = T,date.breaks = 10,par.settings = list(fontsize = list(text = 18, family = "Serif")),
  #       y.relation = "free",lwd=3,lty=1,avg.time="day",
   #     ylab="PM2.5 (ug/m3)",ylim=c(0,45),key.columns = 2,key.font=2,ci=TRUE,
     #    xlim = as.POSIXct(c("2025-12-05", "2026-01-15")),
      #   cols = c( "red","blue","goldenrod","black"),key.position = "inside")
#dev.off()

comb_hour <- timeAverage(combined, avg.time = "hour")

comb_hour <- combined


#SCATTER PLOT
a <- ggplot(comb_hour, aes(y = Sensor_001, x =`FEM T640`))+
  #stat_poly_eq(use_label("eq"), size=6 ,family = "serif")+
  stat_poly_eq(label.y = 0.9, size=6  ,family = "serif", vjust=0.7 )+
  geom_point(fill="gray70", size=4.5,colour="black",pch=21, stroke=1.5) +
  # geom_text(x = 150, y = 230,  color="gray20",size=5 ,family = "serif",label = paste0("MAE=", round(mae, 3)))+
  geom_abline(slope=1, intercept=0, color="black", size=0.9)+
  theme_test()+
  theme(text = element_text(family = "serif",size = 20),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length  = unit(0.2, "cm"),
        panel.border = element_rect(color = "black", size = 1.5),
        plot.margin = margin(1.5,0.6,0.2,0.2, "cm"),
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
  scale_y_continuous(breaks = seq(0,300,50),expand = c(0, 0), limits = c(0,300))+
  scale_x_continuous(breaks = seq(0,300,50),expand = c(0, 0), limits = c(0,300))+
  xlab(expression(bold(FEM~(T640)~PM[10]~(mu*g/m^3))))+
  ylab(expression(bold(Sensor_001~PM[10]~(mu*g/m^3))))


b <- ggplot(comb_hour, aes(y = Sensor_002, x = `FEM T640`))+   
  #stat_poly_eq(use_label("eq"), size=6 ,family = "serif")+
  stat_poly_eq(label.y = 0.9, size=6  ,family = "serif", vjust=0.7 )+
  geom_point(fill="gray70", size=4.5,colour="black",pch=21, stroke=1.5) +
  # geom_text(x = 150, y = 230,  color="gray20",size=5 ,family = "serif",label = paste0("MAE=", round(mae, 3)))+
  geom_abline(slope=1, intercept=0, color="black", size=0.9)+
  theme_test()+
  theme(text = element_text(family = "serif",size = 20),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length  = unit(0.2, "cm"),
        panel.border = element_rect(color = "black", size = 1.5),
        plot.margin = margin(1.5,0.6,0.2,0.2, "cm"),
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
  scale_y_continuous(breaks = seq(0,300,50),expand = c(0, 0), limits = c(0, 300))+
  scale_x_continuous(breaks = seq(0,300,50),expand = c(0, 0), limits = c(0, 300))+
  xlab(expression(bold(FEM~(T640)~PM[10]~(mu*g/m^3))))+
  ylab(expression(bold(Sensor_002~PM[10]~(mu*g/m^3))))

c <- ggplot(comb_hour, aes(y = Sensor_003, x = `FEM T640`))+
  #stat_poly_eq(use_label("eq"), size=6 ,family = "serif")+
  stat_poly_eq(label.y = 0.9, size=6  ,family = "serif", vjust=0.7 )+
  geom_point(fill="gray70", size=4.5,colour="black",pch=21, stroke=1.5) +
  # geom_text(x = 150, y = 230,  color="gray20",size=5 ,family = "serif",label = paste0("MAE=", round(mae, 3)))+
  geom_abline(slope=1, intercept=0, color="black", size=0.9)+
  theme_test()+
  theme(text = element_text(family = "serif",size = 20),
        axis.ticks = element_line(size = 1.6),
        axis.ticks.length  = unit(0.2, "cm"),
        panel.border = element_rect(color = "black", size = 1.5),
        plot.margin = margin(1.5,0.6,0.2,0.2, "cm"),
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
  scale_y_continuous(breaks = seq(0,300,50),expand = c(0, 0), limits = c(0, 300))+
  scale_x_continuous(breaks = seq(0,300,50),expand = c(0, 0), limits = c(0, 300))+
  xlab(expression(bold(FEM~(T640)~PM[10]~(mu*g/m^3))))+
  ylab(expression(bold(Sensor_003~PM[10]~(mu*g/m^3))))

ggarrange(a,b,c, ncol =  3, nrow = 1)

a
b
c
sc

jpeg("AURRASURE SCATTER PLOT FOR PM10_DRY.jpeg",units="cm", width=40, height=15, res=250)
ggarrange(a,b,c, ncol =  3, nrow = 1)
dev.off()

comb_min <- combined

comb_min <- timeAverage(combined, avg.time = "5 min")

colnames(comb_min) <- c("date", "T640","Sensor_001","Sensor_002","Sensor_003")


intr_wet <- comb_min %>% tidyr::gather(Site, comb_min, Sensor_003:T640, na.rm = TRUE)
sum_wet <- Summarize(comb_min ~ Site, 
                     data = intr_wet, na.rm = TRUE)

#summary <- sum_dry[c(2:nrow(sum_wet), 1), ] # move fem to the last row

sum_wet[4,1]<-"FEM T640"


intnew <- sum_wet %>%
  select(Site, mean, median, sd)


colnames(intnew) <- c("Site","Mean ± SD", "Median", "sd") # change columns names

inta <- intnew %>%
  tidyr::gather(metric, mean, `Mean ± SD`:Median,-Site, na.rm = TRUE) 
inta$sd[5:8] <- NA
#View(inta)




#DRY BAR PLOT
jpeg("AURRASURE BAR PLOT FOR PM10_DRY.jpeg",units="cm", width=23, height=17, res=300)
ggplot(inta, aes(x =  fct_inorder(Site), y = mean, fill = metric)) +
  geom_bar(stat="identity",position=position_dodge(0.9),  color="black", linewidth=0.9) +
  geom_errorbar(data = inta, aes(ymin = mean - sd, ymax = mean + sd),
                position = position_dodge(width = 0.9), width = 0.2, size=0.9)+
  #scale_fill_manual(values=wes_palette(name="Rushmore1"))+
  #scale_fill_brewer(palette ="Set1")+
  #scale_fill_manual(values = c("chartreuse4","darkgoldenrod3","blue","deeppink3","darkorchid3"))+
  #scale_fill_manual(values = c("red", "white", "darkgoldenrod1","#56B4E9","darkgreen"))+
  scale_fill_grafify(palette = "bright")+ #okabe_ito,  vibrant, bright, safe, fishy, muted , kelly
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
  scale_y_continuous(breaks = seq(0,150,30),expand = c(0, 0), limits = c(0, 150))+
  ylab(expression(bold(PM[10]~~(mu*g/m^3))))
dev.off()


jpeg("AURRASURE DUAL AXIS PLOT FOR PM10_DRY.jpeg",units="cm", width=35, height=22, res=300)
ggplot(comb_day, aes(x = date)) +
  geom_line(aes(y = `FEM T640`/2, colour = "FEM T640"), lwd = 1) +
  geom_line(aes(y = Sensor_001, colour = "Sensor_001"), lwd = 1) +
  geom_line(aes(y = Sensor_002, colour = "Sensor_002"), lwd = 1) +
  geom_line(aes(y = Sensor_003, colour = "Sensor_003"), lwd = 1) +
  
  scale_colour_manual(
    values = c("FEM T640" = "#FF0000",
               "Sensor_001" = "blue", 
               "Sensor_002" = "goldenrod", 
               "Sensor_003" = "#000000")) +
  
  scale_y_continuous(
    breaks = seq(0, 120, 20),
    expand = c(0, 0),
    limits = c(0, 120),
    name = expression(bold(AURRASURE~~PM[10]~~(mu*g/m^3))),
    sec.axis = sec_axis(~ . *5, breaks = seq(0, 600, 100), 
                        name = expression(bold(FEM~(T640)~~PM[10]~~(mu*g/m^3))))) +
  
  scale_x_datetime(date_breaks = "week", date_labels = "%Y-%m-%d", 
                   expand = c(0, 0)) +
  #scale_x_discrete( breaks = c("2025-02-17 00:09:00", "2025-02-18 12:00:00", "2025-02-19 23:58:00")) +
  
  theme_test() +
  theme(text = element_text(family = "serif", face = "bold"),
        axis.ticks = element_line(size = 0.3), axis.ticks.length = unit(0.2, "cm"),
        panel.border = element_rect(color = "black", size = 1.5), 
        axis.title.y.right = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold", margin = unit(c(0,1,0,0), "mm"), color = "black", size = 20),
        axis.text.y = element_text(face = "bold", margin = unit(c(1,0,0,0), "mm"), color = "black", size = 18),
        axis.title.x = element_blank(), legend.title = element_blank(),
        plot.title = element_text(color = "black", hjust = 1, size = 16, face = "bold"),
        plot.background = element_rect(fill = "white", color = "white", size = 1.5),
        axis.text.x = element_text(vjust = 0.5, size = 20, face = "bold", angle = 60, colour = "black"),
        legend.position = c(0.2, 0.9), legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key.height = unit(0.7, "cm"), legend.key.width = unit(1.4, "cm"),
        legend.spacing.x = unit(1, "mm"), legend.key.spacing.y = unit(1, "cm"),
        legend.text = element_text(size = 22)) +
  guides(color = guide_legend(ncol = 2))
dev.off()

aa <-comb_hour %>% 
  select(`FEM T640`,Sensor_003)
aa <- na.omit(aa)

cor(aa$Sensor_001,aa$`FEM T640`, method = "pearson")^2
cor(aa$Sensor_002,aa$`FEM T640`, method = "pearson")^2
cor(aa$Sensor_003,aa$`FEM T640`, method = "pearson")^2

#mae
mean(abs(aa$Sensor_001 - aa$`FEM T640`))
mean(abs(aa$Sensor_002 - aa$`FEM T640`))
mean(abs(aa$Sensor_003 - aa$`FEM T640`))

#mbe
mean(aa$Sensor_001 - aa$`FEM T640`)
mean(aa$Sensor_002 - aa$`FEM T640`)
mean(aa$Sensor_003 - aa$`FEM T640`)

#rmse
sqrt(mean((aa$Sensor_001 - aa$`FEM T640`)^2))
sqrt(mean((aa$Sensor_002 - aa$`FEM T640`)^2))
sqrt(mean((aa$Sensor_003 - aa$`FEM T640`)^2))


S1_mean<-mean(combined$Sensor_001, na.rm= TRUE)
S2_mean<-mean(combined$Sensor_002, na.rm= TRUE)
S3_mean<-mean(combined$Sensor_003, na.rm= TRUE)

sensor_means<- c(S1_mean,S2_mean,S3_mean)
Intra_unit_variability<- sd(sensor_means)
Relative_Variability<- (Intra_unit_variability/ mean(sensor_means)) * 100



which.min(comb_day$Sensor_003)
comb_day[9,5]


which.max(comb_day$Sensor_002)
comb_day[40,4]


