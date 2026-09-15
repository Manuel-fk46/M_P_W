n_oxides <- read_excel("hourkunakwithoutzerosandecotech.xlsx")

nox <- n_oxides %>% 
  select(date,NOx,NOx_S1,NOx_S2,NOx_S3)

nox <- timeAverage(nox,avg.time = "day")

colnames(nox) <- c("date", " Reference S40", "Sensor_001", "Sensor_002","Sensor_003")
 
jpeg("NOx OLD.jpeg", units = "cm", width = 25, height = 15, res = 300)
timePlot(nox, pollutant = c("Reference S40","Sensor_001","Sensor_002","Sensor_003"),
         stack=FALSE,group = T,date.breaks = 10,par.settings = list(fontsize = list(text = 20, family = "Serif")),
         y.relation = "free",lwd=3,lty=1,avg.time="day",
         date.format = "%b %d,%Y",scales = list(x= list (rot = 50)),
         ylab="NOx(ppb)",ylim=c(0, 25),key.columns = 2,key.font=2,ci=TRUE,
         xlim = as.POSIXct(c("2025-12-01", "2026-01-11")),
         cols = c( "red","blue","goldenrod","black"),key.position = "inside") 
dev.off()


intr_dry <- nox %>% tidyr::gather(Site, nox, Sensor_003:`TS40`, na.rm = TRUE)
sum_dry <- Summarize(nox ~ Site, 
                     data = intr_dry, na.rm = TRUE)



sum_dry[4,1]<-" Reference S40"

intnew <- sum_dry %>%
  select(Site, mean, median, sd)

colnames(intnew) <- c("Site","Mean ± SD", "Median", "sd") # change columns names
                 
inta <- intnew %>%
  tidyr::gather(metric, mean, `Mean ± SD`:Median,-Site, na.rm = TRUE) 
inta$sd[5:8] <- NA
#View(inta)


jpeg(" OLD NO BAR PLOT.jpeg",units="cm", width=23, height=17, res=300)
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
  scale_y_continuous(breaks = seq(-2,10,2),expand = c(0, 0), limits = c(-2, 10))+
  ylab(expression(bold(NO~(ppb))))
dev.off()



#SCATTER PLOT 
a <- ggplot(nox,aes(y= Sensor_001, x=` Reference S40`))+
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
  scale_y_continuous(breaks = seq(0,70,10),expand = c(0, 0), limits = c(0, 70))+
  scale_x_continuous(breaks = seq(0,70,10),expand = c(0, 0), limits = c(0, 70))+
  xlab(expression(bold(Reference~(S40)~NO[x]~(ppb))))+
  ylab(expression(bold(Sensor_001~NO[x]~(ppb))))

b <- ggplot(nox,aes(y= Sensor_002, x=` Reference S40`))+
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
  scale_y_continuous(breaks = seq(0,70,10),expand = c(0, 0), limits = c(0, 70))+
  scale_x_continuous(breaks = seq(0,70,10),expand = c(0, 0), limits = c(0, 70))+
  xlab(expression(bold(Reference~(S40)~NO[x]~(ppb))))+
  ylab(expression(bold(Sensor_002~NO[x]~(ppb))))

c <- ggplot(nox,aes(y= Sensor_003, x=` Reference S40`))+
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
  scale_y_continuous(breaks = seq(0,70,10),expand = c(0, 0), limits = c(0, 70))+
  scale_x_continuous(breaks = seq(0,70,10),expand = c(0, 0), limits = c(0, 70))+
  xlab(expression(bold(Reference~(S40)~NO[x]~(ppb))))+
  ylab(expression(bold(Sensor_003~NO[x]~(ppb))))

I 

jpeg("SCATTER PLOT FOR OLD NOx .jpeg",units="cm", width=40, height=15, res=300)
ggarrange(a,b,c, ncol =  3, nrow = 1)
dev.off()

ab

