rm(list = ls())

library(readxl)
library(readr)


int_1 <- read_excel("C:\\Users\\afris\\Downloads\\sensor1.xlsx")
int_2 <- read_excel("C:\\Users\\afris\\Downloads\\sensor2.xlsx")
int_3 <- read_excel("C:\\Users\\afris\\Downloads\\sensor3.xlsx")
int_4 <- read_excel("C:\\Users\\afris\\Downloads\\sensor4.xlsx")

FEM <- read_csv("C:\\Users\\afris\\Downloads\\Data export (13) (1).csv")

summary(int_4)


summary(FEM)
colnames(FEM)[1] <- "date" 
#removing the overlapping dates
s1 <- int_1[!duplicated(int_1$date), ]
s2 <- int_2[!duplicated(int_2$date), ]
s3 <- int_3[!duplicated(int_3$date), ]
s4 <- int_4[!duplicated(int_4$date), ]



summary(s1)

which.max(s3$pm25c)

s1[s1 < 1] <- NA
s1 <- na.omit(s1)

s2[s2 < 1] <- NA
s2 <- na.omit(s2)

s3[s3 < 1] <- NA
s3 <- na.omit(s3)

s4[s4 < 1] <- NA
s4 <- na.omit(s4)

summary(s4)

library(dplyr)
#to truncate the seconds out
s3$date <- lubridate::ymd_hms(s3$date)
s3$date <- format(s3$date, "%Y-%m-%d %H:%M:00")

s3 <- s3 %>%
  mutate(date1 = lubridate::ymd_hms(date))
s3 <- s3 %>% select(date1, everything())
s3 <- s3[,-2]
colnames(s3)[1] <- "date"


library(dplyr)

s1 <- s1 %>% 
  select(date,pm10a,pm25a)
s2 <- s2 %>% 
  select(date,pm10b,pm25b)
s3 <- s3 %>% 
  select(date,pm10c,pm25c)
s4 <- s4 %>% 
  select(date,pm10d,pm25d)




s1_true <- s1[s1$date >= start_date1 & s1$date <= end_date1,]
s2_true <- s2[s2$date >= start_date1 & s2$date <= end_date1,]
s3_true <- s3[s3$date >= start_date1 & s3$date <= end_date1,]
s4_true <- s4[s4$date >= start_date1 & s4$date <= end_date1,]
FEM_true <- FEM[FEM$date >= start_date1 & FEM$date <= end_date1,]


s1 <- data.frame(period.apply(b, endpoints(b, "min", 5), colMeans, na.rm=T))

b <- Reduce(function(x,y) merge(x,y, all=T, by=c("date")), list(s1, s2, s3, s4)) # nolint

library(xts)
b_5min <- data.frame(period.apply(b, endpoints(b, "min", 5), colMeans, na.rm=T))

library(data.table)
setDT(b_5min, keep.rownames = "date")
b_5min <- b_5min %>%
  mutate(date1 = lubridate::ymd_hms(date))


#move column 'column' to first position
b_5min <- b_5min%>% select(date1, everything())
b_5min = b_5min[,-2]
colnames(b_5min)[1] <- "date"

summary(b_5min)
library(openair)

timePlot(b, pollutant = c("pm25a","pm25b","pm25c", "pm25d"),
         stack=FALSE,group = F,date.breaks = 10,par.settings = list(fontsize = list(text = 18, family = "Serif")),
         y.relation = "free",lwd=3,lty=1,avg.time="day",
         date.format = "%b %d,%Y",scales = list(x= list (rot = 50)),
         ylab="PM2.5(ug/m3)",ylim=c(0,80),key.columns = 2,key.font=2,ci=TRUE,
         xlim = as.POSIXct(c("2024-10-25", "2025-01-15")),
         cols = c(  "#FC4E07","black", "#E7B800","#00AFBB", "violet"),key.position = "inside")


colnames(s1) <- c("date", "pm10a", "pm2.5a")
colnames(s2) <- c("date", "pm10b", "pm2.5b")
colnames(s3) <- c("date", "pm10c", "pm2.5c")
colnames(s4) <- c("date", "pm10d", "pm2.5d")

summary(FEM)

FEM_true[FEM_true < 1] <- NA
FEM_true <- na.omit(FEM_true)

summary(FEM)
summary(FEM)
colnames(FEM)[1] <- "date"




n <- Reduce(function(x,y) merge(x,y, all=T, by=c("date")), list(FEM_true, s1_true, s2_true, s3_true, s4_true)) # nolint

#selecting data for the first evaluation
start_date <- as.POSIXct("2024-11-18 00:00", tz = "UTC")
end_date <- as.POSIXct("2024-12-29 23:59", tz = "UTC")

#selecting date for the second evaluation
start_date1 <- as.POSIXct("2024-12-04 00:00", tz = "UTC")
end_date1 <- as.POSIXct("2025-01-14 23:59", tz = "UTC")





n1 <- n[n$date >= start_date & n$date <= end_date,]

n2 <- n[n$date >= start_date1 & n$date <= end_date1,]

correct_data <- length(na.omit(n2$date))

#s_2 <- s2[s2$date >= start_date &pm25a#s_2 <- s2[s2$date >= start_date & s2$date <= end_date,]
#s_3 <- s3[s3$date >= start_date & s3$date <= end_date,]
s_3 <- s3[s3$date >= start_date1 & s3$date <= end_date1,]


colnames(FEM) <- c("date", "PM10", "PM2.5")



library(xts)
n1_5min <- data.frame(period.apply(n1, endpoints(n1, "min", 5), colMeans, na.rm=T))

n1_hourly <- data.frame(period.apply(n1, endpoints(n1, "hour"), colMeans, na.rm=T))

n1_daily <- data.frame(period.apply(n1, endpoints(n1, "day"), colMeans, na.rm=T))

library(dplyr)

library(data.table)
setDT(n1_5min, keep.rownames = "date")
n1_5min <- n1_5min %>%
  mutate(date1 = lubridate::ymd_hms(date))


#move column 'column' to first position
n1_5min <- n1_5min%>% select(date1, everything())
n1_5min = n1_5min[,-2]
colnames(n1_5min)[1] <- "date"


#time series plot for the ZEFAN
n1_daily_pm25 <- n1_daily %>% 
  select(date, PM2.5, pm25a, pm25b, pm25c, pm25d)

n1_daily_pm10 <- n1_daily %>% 
  select(date, PM10, pm10a, pm10b, pm10c, pm10d) 

colnames(n1_daily_pm10) <- c("date", "FEM(T640)", "Sensor_001","Sensor_002","Sensor_003", "Sensor_004")


summary(n1_daily_pm10)

library(openair)
jpeg("zefan25.jpeg",units="cm", width=25, height=20, res=270)

timePlot(n1_daily_pm25 , pollutant = c("FEM(T640)", "Sensor_001","Sensor_002","Sensor_003"),
         stack=FALSE,group = T,date.breaks = 10,par.settings = list(fontsize = list(text = 18, family = "Serif")),
         y.relation = "free",lwd=3,lty=1,avg.time="day",
         date.format = "%b %d,%Y",scales = list(x= list (rot = 50)),
         ylab="PM2.5(ug/m3)",ylim=c(0,220),key.columns = 2,key.font=2,ci=TRUE,
         xlim = as.POSIXct(c("2024-11-18", "2024-12-29")),
         cols = c("#FF0000","#000000", "#FFFF00","#00BFFF"),key.position = "inside")

dev.off()


jpeg("zefan10.jpeg",units="cm", width=25, height=20, res=270)

timePlot(n1_daily_pm10 , pollutant = c("FEM(T640)", "Sensor_001","Sensor_002","Sensor_003"),
         stack=FALSE,group = T,date.breaks = 10,par.settings = list(fontsize = list(text = 18, family = "Serif")),
         y.relation = "free",lwd=3,lty=1,avg.time="day",
         date.format = "%b %d,%Y",scales = list(x= list (rot = 50)),
         ylab="PM10(ug/m3)",ylim=c(0,900),key.columns = 2,key.font=2,ci=TRUE,
         xlim = as.POSIXct(c("2024-11-18", "2024-12-29")),
         cols = c("#FF0000","#000000", "#FFFF00","#00BFFF"),key.position = "inside")

dev.off()
#ends here





#doing the scatter plot
summary(n1_hourly) 


library(ggplot2)
library(ggpmisc)

a <-ggplot(n1_hourly, aes(y = pm10a, x = PM10))+
  #stat_poly_eq(use_label("eq"), size=6 ,family = "serif")+
  stat_poly_eq(label.y = 0.9, size=7  ,family = "serif", vjust=0.4 )+
  geom_point(fill="cyan", size=4.5,colour="black",pch=21, stroke=1.5) +
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
  scale_y_continuous(breaks = seq(0,1300,200),expand = c(0, 0), limits = c(0, 1300))+
  scale_x_continuous(breaks = seq(0,1300,200),expand = c(0, 0), limits = c(0, 1300))+
  xlab(expression(bold(FEM~(T640)~PM[10]~(mu*g/m^3))))+
  ylab(expression(bold(Sensor_001~PM[10]~(mu*g/m^3))))

a
b
c
d

library(ggpubr)
jpeg("scatterplotZf10.jpeg",units="cm", width=40, height=15, res=300)
ggarrange(a, b, c, ncol = 3)

dev.off()

a1 <- na.omit(n1_hourly)
summary(a1)
cor(a1$pm25d, a1$PM2.5, method = "pearson")^2
sqrt(mean((a1$pm25d - a1$PM2.5)^2))
mean(abs(a1$pm25d - a1$PM2.5))
#sqrt(mean((a1$Sensor_002 - a1$`FEM(T640)`)^2))/mean(a1$`FEM(T640)`)
mean(a1$pm25d - a1$PM2.5)







#bar plots
library(grafify)
library(FSA)
summary(n1_5min)


bar_pm2.5 <- n1_5mi %>%
  select(date,PM2.5,pm25a,pm25b,pm25c)
n1_5min_10 <- n1_5min %>%
  select(date,PM10,pm10a,pm10b,pm10c)

intr <- n1_5min_25 %>% tidyr::gather(Site, PM2.5, pm25c:PM2.5, na.rm = TRUE)
abbd1 <- Summarize(PM2.5 ~ Site, 
                   data = intr, na.rm = TRUE)
??Summarize


summary <- abbd1[c(2:nrow(abbd1), 1), ] # move fem to the last row

intnew <- summary %>%                         
  select(Site, mean, median, sd)
colnames(intnew) <- c("Site","Mean ± SD", "Median", "sd") # change columns names

inta <- intnew %>%
  tidyr::gather(metric, mean, `Mean ± SD`:Median,-Site, na.rm = TRUE) 
inta$sd[5:8] <- NA
View(inta)

inta[1, 1] <- "Sensor_001"
inta[2, 1] <- "Sensor_002"
inta[3, 1] <- "Sensor_003"
inta[4, 1] <- "FEM (T640)"
inta[5, 1] <- "Sensor_001"
inta[6, 1] <- "Sensor_002"
inta[7, 1] <- "Sensor_003"
inta[8, 1] <- "FEM (T640)"

jpeg("PM10 ZF barchart.jpeg",units="cm", width=23, height=17, res=300)

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
  scale_y_continuous(breaks = seq(0,500,30),expand = c(0, 0), limits = c(0, 500))+
  ylab(expression(bold(PM[10]~~(mu*g/m^3))))

dev.off()


which.max(FEM$PM10)

summary(n2)

n2 <- n2[!duplicated(n2$date), ]

library(xts)
#working on the interval 2024-12-04 to 2025-01-14
n2_5min <- data.frame(period.apply(n2, endpoints(n2, "min", 5), colMeans, na.rm=T))

n2_hourly <- data.frame(period.apply(n2, endpoints(n2, "hour"), colMeans, na.rm=T))

n_daily <- data.frame(period.apply(n, endpoints(n, "day"), colMeans, na.rm=T))

library(data.table)
setDT(n_daily, keep.rownames = "date")
n_daily <- n_daily %>%
  mutate(date1 = lubridate::ymd_hms(date))

#move column 'column' to first position
n_daily <- n_daily%>% select(date1, everything())
n_daily = n_daily[,-2]
colnames(n_daily)[1] <- "date"



#time series plot for the ZEFAN
n_daily_pm25 <- n_daily %>% 
  select(date, PM2.5, pm25a, pm25b, pm25c, pm25d)

n_daily_pm10 <- n_daily %>% 
  select(date, PM10, pm10a, pm10b, pm10c, pm10d) 

colnames(n_daily_pm10) <- c("date", "FEM T640", "Sensor_001","Sensor_002","Sensor_003", "Sensor_004")


summary(n2_daily_pm10)

library(openair)
jpeg("2nd zefan25.jpeg",units="cm", width=25, height=20, res=270)

timePlot(n2_daily_pm25 , pollutant = c("FEM(T640)", "Sensor_001","Sensor_002","Sensor_003"),
         stack=FALSE,group = T,date.breaks = 10,par.settings = list(fontsize = list(text = 18, family = "Serif")),
         y.relation = "free",lwd=3,lty=1,avg.time="day",
         date.format = "%b %d,%Y",scales = list(x= list (rot = 50)),
         ylab="PM2.5(ug/m3)",ylim=c(0,220),key.columns = 2,key.font=2,ci=TRUE,
         xlim = as.POSIXct(c("2024-12-04", "2025-01-14")),
         cols = c("#FF0000","#000000", "#FFFF00","#00BFFF"),key.position = "inside")

dev.off()


jpeg("2nd zefan10.jpeg",units="cm", width=25, height=20, res=270)

timePlot(n_daily_pm10 , pollutant = c("FEM T640", "Sensor_001","Sensor_002","Sensor_003"),
         stack=FALSE,group = T,date.breaks = 10,par.settings = list(fontsize = list(text = 18, family = "Serif")),
         y.relation = "free",lwd=3,lty=1,avg.time="day",
         date.format = "%b %d,%Y",scales = list(x= list (rot = 50)),
         ylab="PM10 (ug/m3)",ylim=c(0,900),key.columns = 2,key.font=2,ci=TRUE,
         xlim = as.POSIXct(c("2024-12-04", "2025-01-14")),
         cols = c("#FF0000","#000000", "goldenrod","#00BFFF"),key.position = "inside")

dev.off()
#ends here

summary(n_daily_pm10)

library(ggplot2)

jpeg("dual axis plot for ZF.jpeg",units="cm", width=25, height=20, res=270)

ggplot(n_daily_pm10, aes(x = date)) +
  geom_line(aes(y = FEM_T640/5, colour = "FEM_T640"), lwd = 1) +
  geom_line(aes(y = Sensor_001, colour = "Sensor_001"), lwd = 1) +
  geom_line(aes(y = Sensor_002, colour = "Sensor_002"), lwd = 1) +
  geom_line(aes(y = Sensor_003, colour = "Sensor_003"), lwd = 1) +
  
  scale_colour_manual(
    values = c("FEM_T640" = "#FF0000",
               "Sensor_001" = "#000000", 
               "Sensor_002" = "#FFFF00", 
               "Sensor_003" = "#00BFFF")) +
  
  scale_y_continuous(
    breaks = seq(0, 200, 20),
    expand = c(0, 0),
    limits = c(0, 200),
    name = expression(bold(ZeFan~~PM[10]~~(mu*g/m^3))),
    sec.axis = sec_axis(~ . *5, breaks = seq(0, 1000, 100), 
                        name = expression(bold(FEM~(T640)~~PM[10]~~(mu*g/m^3))))) +
  
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







summary(n2_hourly)
library(ggpmisc)

a <-ggplot(n2_hourly_beta, aes(y = pm25a, x = PM2.5))+
  #stat_poly_eq(use_label("eq"), size=6 ,family = "serif")+
  stat_poly_eq(label.y = 0.9, size=7  ,family = "serif", vjust=0.4 )+
  geom_point(fill="cyan", size=4.5,colour="black",pch=21, stroke=1.5) +
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
  scale_y_continuous(breaks = seq(0,300,50),expand = c(0, 0), limits = c(0, 300))+
  scale_x_continuous(breaks = seq(0,300,50),expand = c(0, 0), limits = c(0, 300))+
  xlab(expression(bold(FEM~(T640)~PM[2.5]~(mu*g/m^3))))+
  ylab(expression(bold(Sensor_001~PM[2.5]~(mu*g/m^3))))

a
b
c
d

library(ggpubr)
jpeg("2nd scatterplotZf25.jpeg",units="cm", width=40, height=15, res=300)
ggarrange(a, c, ncol = 3)

dev.off()

a2 <- na.omit(n2_hourly_beta)

summary(a2)

r<-cor(a2$pm25c, a2$PM2.5, method = "pearson")^2
sqrt(mean((a2$pm25c - a2$PM2.5)^2))
mean(abs(a2$pm25c - a2$PM2.5))
#sqrt(mean((a1$Sensor_002 - a1$`FEM(T640)`)^2))/mean(a1$`FEM(T640)`)
mean(a2$pm25c - a2$PM2.5)



#bar plot
n2_5min_25 <- n2_5min %>%
  select(date,PM2.5,pm25a,pm25b,pm25c)
n2_5min_10 <- n2_5min %>%
  select(date,PM10,pm10a,pm10b,pm10c)

intr <- n2_5min_25 %>% tidyr::gather(Site, PM2.5, pm25c:PM2.5, na.rm = TRUE)
abbd1 <- Summarize(PM2.5 ~ Site, 
                   data = intr, na.rm = TRUE)
??Summarize


summary <- abbd1[c(2:nrow(abbd1), 1), ] # move fem to the last row

intnew <- summary %>%                         
  select(Site, mean, median, sd)
colnames(intnew) <- c("Site","Mean ± SD", "Median", "sd") # change columns names

inta <- intnew %>%
  tidyr::gather(metric, mean, `Mean ± SD`:Median,-Site, na.rm = TRUE) 
inta$sd[5:8] <- NA
View(inta)

inta[1, 1] <- "Sensor_001"
inta[2, 1] <- "Sensor_002"
inta[3, 1] <- "Sensor_003"
inta[4, 1] <- "FEM (T640)"
inta[5, 1] <- "Sensor_001"
inta[6, 1] <- "Sensor_002"
inta[7, 1] <- "Sensor_003"
inta[8, 1] <- "FEM (T640)"

jpeg(" 2nd PM25 ZF barchart.jpeg",units="cm", width=23, height=17, res=300)

ggplot(inta, aes(x =  fct_inorder(Site), y = mean, fill = metric)) +
  geom_bar(stat="identity",position=position_dodge(0.9),  color="black", linewidth=0.9) +
  geom_errorbar(data = inta, aes(ymin = mean - sd, ymax = mean + sd),
                position = position_dodge(width = 0.9), width = 0.2, size=0.9)+
  #scale_fill_manual(values=wes_palette(name="Rushmore1"))+
  #scale_fill_brewer(palette ="Set1")+
  #scale_fill_manual(values = c("chartreuse4","darkgoldenrod3","blue","deeppink3","darkorchid3"))+
  #scale_fill_manual(values = c("red", "white", "darkgoldenrod1","#56B4E9","darkgreen"))+
  scale_fill_grafify(palette = "kelly")+ #okabe_ito,  vibrant, bright, safe, fishy, muted , kelly
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
  scale_y_continuous(breaks = seq(0,150,30),expand = c(0, 0), limits = c(0, 130))+
  ylab(expression(bold(PM[2.5]~~(mu*g/m^3))))

dev.off()

library(ggplot2)


summary(int_1)


row <- 966
results <- n2_hourly %>%
  slice((row - 5):(row + 5))%>%
  view()

which.max(n2$pm25c)

which.max(n2_hourly$pm25c)
a = n2_hourly[c(960,961,962,963,964,965,966,967,968,969),]
a

summary(n2_hourly)

n2_hourly_beta <- n2_hourly[-c(966,967),]

summary(n2_hourly_beta)


#adding the weather parameters.

library(readr)

t_rh <- read_csv("Data export (43).csv")
summary(t_rh)

t_rh$date <- lubridate::mdy_hm(t_rh$date)

t_rh <-t_rh[ t_rh$date >= start_date1  & t_rh$date <= end_date1, ]


df <- Reduce(function(x,y) merge(x,y, all=T, by=c("date")), list(t_rh, n2_hourly_beta)) # nolint

library(xts)
df <- data.frame(period.apply(df, endpoints(df, "hour"), colMeans, na.rm=T))

library(tidyverse)
library(data.table)
setDT(df, keep.rownames = "date")
df <- df %>%
  mutate(date1 = lubridate::ymd_hms(date))

#move column 'column' to first position
df <- df%>% select(date1, everything())
df = df[,-2]
colnames(df)[1] <- "date"

write.csv(df, "zefan_ref_dry.csv", row.names = FALSE)
