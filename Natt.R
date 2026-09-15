ref <- read_csv("teledyne piera wet.csv")

s1 <- read_csv("p1.csv", col_types = cols_only(date = col_guess(), 
                                               `sen1 10` = col_guess(), `sen1 2.5` = col_guess()))

s2 <- read_csv("p2.csv", col_types = cols_only(date = col_guess(), 
                                               `sen2 2.5` = col_guess(), `sen2 10` = col_guess()))
s3 <- read_csv("p3.csv", col_types = cols_only(date = col_guess(), 
                                               `sen3 2.5` = col_guess(), `sen3 10` = col_guess()))

s4 <- read_csv("p4.csv", col_types = cols_only(date = col_guess(), 
                                               `sen4 2.5` = col_guess(), `sen4 10` = col_guess()))
s5 <- read_csv("p5.csv", col_types = cols_only(date = col_guess(), 
                                               `sen5 10` = col_guess(), `sen5 2.5` = col_guess()))


colnames(ref) <- c("date","pm10_fem","pm2.5_fem")
colnames(s1) <- c("date", "pm10_01", "pm2.5_01")
colnames(s2) <- c("date", "pm2.5_02", "pm10_02")
colnames(s3) <- c("date", "pm2.5_03", "pm10_03")
colnames(s4) <- c("date", "pm2.5_04", "pm10_04")
colnames(s5) <- c("date", "pm10_05", "pm2.5_05")


ref$date <- as.POSIXct(ref$date, tz = "UTC", format = c("%Y/%m/%d %H:%M"))
s1$date <- as.POSIXct(s1$date, tz = "UTC", format = c("%Y/%m/%d %H:%M"))
s2$date <- as.POSIXct(s2$date, tz = "UTC", format = c("%m/%d/%Y %H:%M"))
s3$date <- as.POSIXct(s3$date, tz = "UTC", format = c("%m/%d/%Y %H:%M"))
s4$date <- as.POSIXct(s4$date, tz = "UTC", format = c("%Y/%m/%d %H:%M"))
s5$date <- as.POSIXct(s5$date, tz = "UTC", format = c("%m/%d/%Y %H:%M"))

min_value <- 0.001
max_value <- 500.00

dry_start <- as.POSIXct("2025-12-08 00:00:00", tz = "UTC")
dry_end <- as.POSIXct("2026-01-18 23:59:00", tz = "UTC")
wet_start <- as.POSIXct("2025-04-08 00:00:00", tz = "UTC")
wet_end <- as.POSIXct("2025-05-19 23:59:00", tz = "UTC")

ref <- ref[ref$date >= wet_start & ref$date <= wet_end,]
s1 <- s1[s1$date >= wet_start & s1$date <= wet_end,]
s2 <- s2[s2$date >= wet_start & s2$date <= wet_end,]
s3 <- s3[s3$date >= wet_start & s3$date <= wet_end,]
s4 <- s4[s4$date >= wet_start & s4$date <= wet_end,]
s5 <- s5[s5$date >= wet_start & s5$date <= wet_end,]

#ref <- ref[ref$pm2.5_fem >= min_value & ref$pm2.5_fem <= max_value,]
s1 <- s1[s1$pm2.5_01 >= min_value & s1$pm2.5_01 <= max_value,]
s2 <- s2[s2$pm2.5_02 >= min_value & s2$pm2.5_02 <= max_value,]
s3 <- s3[s3$pm2.5_03 >= min_value & s3$pm2.5_03 <= max_value,]
s4 <- s4[s4$pm2.5_04 >= min_value & s4$pm2.5_04 <= max_value,]
s5 <- s5[s5$pm2.5_05 >= min_value & s5$pm2.5_05 <= max_value,]

 
wet_ref <- ref %>% 
  select(date,pm2.5_fem) 
s_01 <- s1 %>% 
  select(date,pm2.5_01) 
s_02 <- s2 %>% 
  select(date,pm2.5_02) 
s_03 <- s3 %>% 
  select(date,pm2.5_03)
s_04 <- s4 %>% 
  select(date,pm2.5_04) 
s_05 <- s5 %>% 
  select(date,pm2.5_05)


combined <- Reduce(function(x,y) merge(x,y,all=T,by=c("date")),list(wet_ref,s_01,s_02,s_03,s_04,s_05))
combined <-timeAverage(combined, avg.time = "hour")


a<-ggplot(combined,aes(y = pm2.5_01, x = pm2.5_fem))+
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
  scale_x_continuous(breaks = seq(0,200,50),expand = c(0, 0), limits = c(0, 200))
  #xlab(expression(bold(FEM~(T640)~PM[2.5]~(mu*g/m^3))))+
  #ylab(expression(bold(Sensor_001~PM[2.5]~(mu*g/m^3))))

b <- ggplot(combined,aes(y = pm2.5_02, x = pm2.5_fem))+
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
  scale_y_continuous(breaks = seq(0,100,20),expand = c(0, 0), limits = c(0, 100))+
  scale_x_continuous(breaks = seq(0,100,20),expand = c(0, 0), limits = c(0, 100))
  #xlab(expression(bold(FEM~(T640)~PM[2.5]~(mu*g/m^3))))+
  #ylab(expression(bold(Sensor_002~PM[2.5]~(mu*g/m^3))))


c <- ggplot(combined,aes(y = pm2.5_03, x = pm2.5_fem))+
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
  scale_y_continuous(breaks = seq(0,100,20),expand = c(0, 0), limits = c(0, 100))+
  scale_x_continuous(breaks = seq(0,100,20),expand = c(0, 0), limits = c(0, 100))
  #xlab(expression(bold(FEM~(T640)~PM[2.5]~(mu*g/m^3))))+
  #ylab(expression(bold(Sensor_003~PM[2.5]~(mu*g/m^3))))



d <- ggplot(combined,aes(y = pm2.5_04, x = pm2.5_fem))+
  #c<-ggplot(combined,aes(y=Sensor_003,x=`FEM T640`))+
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
  scale_y_continuous(breaks = seq(0,100,20),expand = c(0, 0), limits = c(0, 100))+
  scale_x_continuous(breaks = seq(0,100,20),expand = c(0, 0), limits = c(0, 100))
  #xlab(expression(bold(FEM~(T640)~PM[2.5]~(mu*g/m^3))))+
  #ylab(expression(bold(Sensor_003~PM[2.5]~(mu*g/m^3))))

e <- ggplot(combined,aes(y = pm2.5_05, x = pm2.5_fem))+
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
  scale_y_continuous(breaks = seq(0,100,20),expand = c(0, 0), limits = c(0, 100))+
  scale_x_continuous(breaks = seq(0,100,20),expand = c(0, 0), limits = c(0, 100))
  #xlab(expression(bold(FEM~(T640)~PM[2.5]~(mu*g/m^3))))+
  #ylab(expression(bold(Sensor_003~PM[2.5]~(mu*g/m^3))))


sc <- ggarrange(a, b, c, d, e, ncol = 5, nrow = 1 )

aa <- combined %>% 
  select(pm2.5_fem,pm2.5_05)
aa <- na.omit(aa)

cor(aa$pm2.5_05,aa$pm2.5_fem, method = "pearson")^2



