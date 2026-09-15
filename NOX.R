no_oxides <- read_excel("hourkunakwithoutzerosandecotech.xlsx")


#new
no <-no_oxides %>% 
  select(date,NOnew,NO_S1,NO_S2,NO_S3)

nox <- no_oxides %>% 
  select(date,NOxnew,NOx_S1,NOx_S2,NOx_S3)

no2 <- no_oxides %>% 
  select(date,NO2new,NO2_S1,NO2_S2,NO2_S3)

colnames(no) <- c("date", "Reference(S40)", "Sensor_001", "Sensor_002", "Sensor_003") 


tail(no)

jpeg("TIMESERIES PLOT FOR OLD NO2.jpeg", units = "cm", width = 25, height = 15, res = 300)
timePlot(no2, pollutant = c("Reference(S40)","Sensor_001","Sensor_002","Sensor_003"),
         stack=FALSE,group = T,date.breaks = 7,par.settings = list(fontsize = list(text = 22, family = "Serif")),
         y.relation = "free",lwd=3,lty=1,avg.time="hour",
         date.format = "%b %d,%Y",scales = list(x= list (rot = 50)),
         ylab="NO2(ppb)",ylim=c(0,50),key.columns = 2,key.font=2,ci=TRUE,
         xlim = as.POSIXct(c("2025-12-01", "2026-01-11")),
         cols = c( "red","blue","goldenrod","black"),key.position = "inside")
dev.off()



x <- ggplot(no2, aes(y=Sensor_001, x= `FEM T640`))+
  stat_poly_eq(use_label("eq"), size=6 ,family = "serif")+
  stat_poly_eq(label.y = 0.9, size=6  ,family = "serif", vjust=0.7 )+
  geom_point(fill="green", size=4.5,colour="black",pch=21, stroke=1.5) +
   #geom_text(x = 150, y = 230,  color="gray20",size=5 ,family = "serif",label = paste0("MAE=", round(mae, 3)))+
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
  scale_y_continuous(breaks = seq(0,50,10),expand = c(0, 0), limits = c(0, 50))+
  scale_x_continuous(breaks = seq(0,50,10),expand = c(0, 0), limits = c(0, 50))+
  xlab(expression(bold(Reference~(S40)~NO2~(ppb))))+
  ylab(expression(bold(Sensor_001~NO2~(ppb))))


y <-ggplot(no2,aes(y=Sensor_002, x=`FEM T640`))+
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
  scale_y_continuous(breaks = seq(0,50,10),expand = c(0, 0), limits = c(0, 50))+
  scale_x_continuous(breaks = seq(0,50,10),expand = c(0, 0), limits = c(0, 50))+
  xlab(expression(bold(Reference~(S40)~NO2~(ppb))))+
  ylab(expression(bold(Sensor_002~NO2~(ppb))))

z <- ggplot(no2,aes(y=Sensor_003, x=`FEM T640`))+
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
  scale_y_continuous(breaks = seq(0,50,10),expand = c(0, 0), limits = c(0,50))+
  scale_x_continuous(breaks = seq(0,50,10),expand = c(0, 0), limits = c(0,50))+
  xlab(expression(bold(Reference~(S40)~NO2~(ppb))))+
  ylab(expression(bold(Sensor_003~NO2~(ppb))))



jpeg("SCATTER PLOT FOR OLD NO2.jpeg",units="cm", width=40, height=15, res=300)
ggarrange(x,y,z, ncol =  3, nrow = 1)
dev.off()

ab <- nox %>% 
  select(date,NOxnew,NOx_S1)
ab <- na.omit(ab)

#NO
cor(ab$NO_S1,ab$NOnew, method = "pearson")^2
cor(ab$NO_S2,ab$NOnew, method = "pearson")^2
cor(ab$NO_S3,ab$NOnew, method = "pearson")^2

#NOX
cor(ab$NOx_S1,ab$NOxnew, method = "pearson")^2
cor(ab$NOx_S2,ab$NOxnew, method = "pearson")^2
cor(ab$NOx_S3,ab$NOxnew, method = "pearson")^2

#NO2
cor(ab$NO2_S1,ab$NOnew, method = "pearson")^2
cor(ab$NO2_S2,ab$NO2, method = "pearson")^2
cor(ab$NO2_S3,ab$NO2, method = "pearson")^2











