library(tidyverse)
library(openair)
library(dplyr)
library(caret)
library(FSA)
library(grafify)
library(ggplot2)
library(wesanderson)



GSA <- read_csv("JOY NEWS/C5267C8902F3-16-Aug-2026-16-26-37.csv")

l <- read_csv("JOY NEWS/lapaz data/D7A91883E7E0_27_Aug_2026_13_53_27.csv")

colnames(l)

lapaz <- read_csv("lapaz.comb.csv")

l2 <- read_csv("JOY NEWS/lapaz data/D7A91883E7E0-16-Aug-2026-17-09-15.csv")

lapaz <- lapaz |> 
  select(date,PM1,PM2.5,PM10)


sum(duplicated(lapaz$date))

dup <- lapaz[!duplicated(lapaz$date),]

lapaz <- dup


GSA <- GSA |> 
  select(Date,`PM1, ug/m³`,`PM2.5, ug/m³`,`PM10, ug/m³`) |> 
  rename(date = Date, pm1_G = `PM1, ug/m³`, pm2.5_G = `PM2.5, ug/m³`, pm10_G = `PM10, ug/m³`)

Lapaz <- Lapaz |> 
  select(Date,`PM1, ug/m³`,`PM2.5, ug/m³`,`PM10, ug/m³`) |> 
  rename(date = Date, pm1_L = `PM1, ug/m³`, pm2.5_L = `PM2.5, ug/m³`, pm10_L = `PM10, ug/m³`)

pollutant_G <- colnames(GSA)[colnames(GSA) != "date"]

pollutant_L <- colnames(lapaz)[colnames(lapaz) != "date"]

lapaz <- timeAverage(lapaz, avg.time = "hour")

tv_G <- timeVariation(GSA,pollutant = pollutant_G,
         avg.time = "hour", ci = F,key.position = "right", fontsize = 18,
         lty = 1, lwd = 1, key.columns = 1,ylab = "PM concentrations",
         cols = c("black","green4","blue"))

tv_L <- timeVariation(lapaz,pollutant = pollutant_L,  
               ci = F, key.position = "right", fontsize = 18,
              lty = 1, lwd = 1, key.columns = 1,ylab = "PM concentrations",
              cols = c("black","green4","blue"))


tv_L$plot$hour

jpeg("JoyFM Lpz DIURNAL PLOT.jpeg",height = 2000, width =3000,res = 260)
tv_L$plot$hour + labs(title = "Lapaz - Diurnal")
dev.off()


jpeg("JoyFM GSA.jpeg",height = 5000, width = 8000,res = 700)
tv_G$plot$hour + labs(title = "Ghana Standard Authority(Shiashie Intersection) - Diurnal")
dev.off() 



colnames(GSA) <- c("date","PM1", "PM2.5","PM10")

colnames(Lapaz) <- c("date","PM1", "PM2.5","PM10")


?timeVariation()
?theme()



intr <- lapaz %>% tidyr::gather(Site, lapaz, PM10:PM1, na.rm = TRUE)


#boxplot
jpeg("Lapaz Boxplot.jpeg",height = 2000, width = 2500, res = 250)
ggplot(intr,aes(x = fct_inorder(Site),y = lapaz)) +
         stat_boxplot(geom = "errorbar", lwd = 1, width = 0.2, position = position_dodge(width = 0.5)) +
         geom_boxplot(aes(fill = Site), width= 0.5, lwd = 1, outlier.shape = NA, position = position_dodge(width = 0.5)) + 
         stat_summary(fun=mean, geom= 'point', shape=20, size=5, position = position_dodge(width = 0.5), show.legend = FALSE) +
         #geom_hline(aes(yintercept = 35, linetype= "PM1"),colour = "black", linewidth = 1) +
         scale_fill_grafify(palette = "kelly")+ #okabe_ito, vibrant, bright, safe, kelly, fishy, muted  
         theme_classic()+
         theme(text = element_text(family = "serif"),
               axis.ticks = element_line(size = 1.6),
               axis.ticks.length = unit(0.2, "cm"),
               axis.title.y = element_text(colour = "black", size = 15, face = "bold"),
               axis.text.y = element_text(colour = "black", size = 15, face = "bold"),
               plot.margin = margin(0.7,1.5,0.7,0.3, "cm"),
               axis.title.x = element_blank(),
               legend.title = element_blank(),
               legend.key.size = unit(2.0,"line"),
               plot.title = element_text(colour = "black", hjust = 0.5, size = 25, face = "bold",vjust = 0.1),
               legend.text = element_text(size = 15, face = "bold"),
               axis.text.x = element_text(colour = "black", size = 15, face = "bold", angle = 0, vjust = 0.5),
               legend.position = c(0.95,0.90))+
  scale_y_continuous(breaks = seq(0,80,20), expand = c(0, 0), limits = c(0, 80))+
  ylab(expression(bold("PM Concentrations"))) +
  scale_x_discrete(labels = c("PM1" = expression(bold(PM[1])),
                               "PM2.5" = expression(bold(PM[2.5])),
                               "PM10" = expression(bold(PM[10])))) +
  labs(title = expression(bold("lapaz")))
dev.off()



names.for.joynews.data <- colnames(l)

colnames(l2) <- c("date", "VOC", "AQS", "Temperature", "Humidity", "Pressure", "PM1", "PM2.5", "PM10", "Latitude", "Longitude")



?lapply


a <- rbind(l, l2)
write.csv(a,"lapaz.comb.csv")











