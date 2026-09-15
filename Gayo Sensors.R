library(tidyverse)
library(openair)
library(readr)
library(readxl)
library(ggpmisc)

path <- "JOY NEWS/lapaz data"

files <- list.files(path,pattern = "\\.csv$", full.names = T)

a <- files %>%  
  lapply(readr::read_csv) %>% 
  bind_rows()

l2 <- read_csv("JOY NEWS/lapaz data/D7A91883E7E0_27_Aug_2026_13_53_27.csv")

l <- read_csv("JOY NEWS/lapaz data/D7A91883E7E0-16-Aug-2026-17-09-15.csv")

write_tsv(a,"NextPM S3.txt")

getwd()

#gayo.1 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_1 sessions_2026072419423820260724-3926991-os6qb6/gayo-1_1968510__20260724-3926991-2pwsk5.csv", 
                                                   skip = 8)
#gayo.2 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_2 sessions_2026072419383520260724-3926991-vvgxcs/gayo-2_1968583__20260724-3926991-4ux5yx.csv", 
                                                   skip = 8)
#gayo.3<- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_3 sessions_2026072419381720260724-3926991-tfmcx9/gayo-3_1968584__20260724-3926991-6w46xw.csv", 
                                                   skip = 8)
#gayo.4 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_4 sessions_2026072419372620260724-3926991-z6ef7i/gayo-4_1968585__20260724-3926991-dnrdij.csv", 
                                                   skip = 8)
#gayo.5 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_5 sessions_2026072419370020260724-3926991-rai1ae/gayo-5_1968586__20260724-3926991-lqmpd5.csv", 
                                                   skip = 8)
#gayo.7 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_7 sessions_2026072419421220260724-3926991-u9hhum/gayo-7_1968521__20260724-3926991-nukuzy.csv", 
                                                   skip = 8)
#gayo.6 <- read_csv("Habitat Map Unzipped Files /gayo_6 sessions_2026073013012520260730-1523283-cs3bmn/gayo-6_1969233__20260730-1523283-coz3cc.csv", 
                                                   skip = 8)
#gayo.8 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_8 sessions_2026072419361820260724-3926991-9sezin/gayo-8_1968587__20260724-3926991-ypy5c4.csv", 
                                                   skip = 8)
#gayo.9 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_9 sessions_2026072419415720260724-3926991-jb26s7/gayo-9_1968528__20260724-3926991-9k3uf3.csv", 
                                                   skip = 8)
#gayo.10 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_10 sessions_2026072419413420260724-3926991-rf2mxd/gayo-10_1968531__20260724-3926991-afuphk.csv", 
                                                    skip = 8)
#gayo.11 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_11 sessions_2026072419235320260724-3926991-4dmnj9/gayo-11_1968689__20260724-3926991-fa5nsl.csv", 
                                                    skip = 8)
#gayo.12 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_12 sessions_2026072418164420260724-3926991-8tcxce/gayo_12_1968690__20260724-3926991-l80ck2.csv", 
                                                    skip = 8)
#gayo.13 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_13 sessions_2026072418161620260724-3926991-ry2wog/gayo_13_1968691__20260724-3926991-paoajz.csv", 
                                                    skip = 8)
#gayo.14 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_14 sessions_2026072418160320260724-3926991-ab8tvl/gayo_14_1968692__20260724-3926991-6cgydn.csv", 
                                                    skip = 8)
#gayo.15 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_15 sessions_2026072418154720260724-3926991-dpc9ve/gayo_15_1968693__20260724-3926991-177bh5.csv", 
                                                    skip = 8)
#gayo.16 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_16 sessions_2026072418153220260724-3926991-5o7zjz/gayo_16_1968694__20260724-3926991-31a3fd.csv", 
                                                    skip = 8)
#gayo.17 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_17 sessions_2026072418144620260724-3926991-ne6n5h/gayo_17_1968695__20260724-3926991-5jmpux.csv", 
                                                    skip = 8)
#gayo.18 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_18 sessions_2026072418141920260724-3926991-agn79x/gayo_18_1968696__20260724-3926991-9gqrkr.csv", 
                                                    skip = 8)
#gayo.19 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_19 sessions_2026072418134920260724-3926991-trj60g/gayo_19_1968697__20260724-3926991-ouagee.csv", 
                                                    skip = 8)
#gayo.20 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_20 sessions_2026072418112320260724-3926991-ze388n/gayo_20_1968698__20260724-3926991-6zlu4m.csv", 
                                                    skip = 8)
#gayo.22 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_22 sessions_2026072419351320260724-3926991-208po2/gayo-22_1968592__20260724-3926991-fpgyvj.csv", 
                                                    skip = 8)
#gayo.23 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_23 sessions_2026072419344720260724-3926991-vm1yjs/gayo-23_1968593__20260724-3926991-cs7m.csv", 
                                                  skip = 8)
#gayo.24 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_24 sessions_2026072419342620260724-3926991-two61j/gayo-24_1968594__20260724-3926991-3vvend.csv", 
                                                    skip = 8)
#gayo.25 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_25 sessions_2026072419283120260724-3926991-dx6xhw/gayo-25_1968601__20260724-3926991-pv56ma.csv", 
                                                    skip = 8)
#gayo.26 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_26 sessions_2026072419340220260724-3926991-g9isp0/gayo-26_1968596__20260724-3926991-sz890i.csv", 
                                                    skip = 8)
#gayo.27 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_27 sessions_2026072419334220260724-3926991-uqzfzb/gayo-27_1968597__20260724-3926991-6cn9o3.csv", 
                                                    skip = 8)
#gayo.28 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_28 sessions_2026072419332120260724-3926991-dkxutp/gayo-28_1968598__20260724-3926991-d6hilv.csv", 
                                                    skip = 8)
#gayo.29 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_29 sessions_2026072419330020260724-3926991-n6llug/gayo-29_1968599__20260724-3926991-vdtt0r.csv", 
                                                    skip = 8)
#gayo.30 <- read_csv("~/Desktop/Habitat Map Unzipped Files /gayo_30 sessions_2026072419291520260724-3926991-juelvw/gayo-30_1968600__20260724-3926991-m3jazp.csv", 
                                                    skip = 8)


ref <- read_csv("gayo_ref_clean.csv")

gayo.31 <- read_csv("HIMSELF/gayo-31_1971178__20260828-583192-wz22ze.csv", 
                    skip = 8)
gayo.32 <- read_csv("HIMSELF/gayo-32_1971171__20260828-583192-dzed6q.csv", 
                    skip = 8)
gayo.33 <- read_csv("HIMSELF/gayo-33_1971168__20260828-583192-z64i74.csv", 
                    skip = 8)
gayo.34 <- read_csv("HIMSELF/gayo_34_1971239__20260828-583192-b0m2o4.csv", 
                    skip = 8)
gayo.35 <- read_csv("HIMSELF/gayo_35_1971236__20260828-583192-14jbe.csv", 
                    skip = 8)
gayo.36 <- read_csv("HIMSELF/gayo-36_1971176__20260828-583192-ogbei1.csv", 
                    skip = 8)
gayo.37 <- read_csv("HIMSELF/gayo-37_1971223__20260828-583192-r6xr3z.csv", 
                    skip = 8)
gayo.38 <- read_csv("HIMSELF/gayo-38_1971182__20260828-583192-3rzri2.csv", 
                    skip = 8)
gayo.39 <- read_csv("HIMSELF/gayo-39_1971225__20260828-583192-um5i53.csv", 
                    skip = 8)

gayo.31 <- gayo.31 |> 
  select(Timestamp,`2:Measurement_Value`) |> 
  rename(date = Timestamp, pm2.5_31 = `2:Measurement_Value`)

gayo.32 <- gayo.32 |> 
  select(Timestamp,`2:Measurement_Value`) |> 
  rename(date = Timestamp, pm2.5_32 = `2:Measurement_Value`)

gayo.33 <- gayo.33 |> 
  select(Timestamp,`2:Measurement_Value`) |> 
  rename(date = Timestamp, pm2.5_33 = `2:Measurement_Value`)

gayo.34 <- gayo.34 |> 
  select(Timestamp,`2:Measurement_Value`) |> 
  rename(date = Timestamp, pm2.5_34 = `2:Measurement_Value`)

gayo.35 <- gayo.35 |> 
  select(Timestamp,`2:Measurement_Value`) |> 
  rename(date = Timestamp, pm2.5_35 = `2:Measurement_Value`)

gayo.36 <- gayo.36 |> 
  select(Timestamp,`2:Measurement_Value`) |> 
  rename(date = Timestamp, pm2.5_36 = `2:Measurement_Value`)

gayo.37 <- gayo.37 |> 
  select(Timestamp,`2:Measurement_Value`) |> 
  rename(date = Timestamp, pm2.5_37 = `2:Measurement_Value`)

gayo.38 <- gayo.38 |> 
  select(Timestamp,`2:Measurement_Value`) |> 
  rename(date = Timestamp, pm2.5_38 = `2:Measurement_Value`)

gayo.39 <- gayo.39 |> 
  select(Timestamp,`2:Measurement_Value`) |> 
  rename(date = Timestamp, pm2.5_39 = `2:Measurement_Value`)


ref <- ref |>
  select(date,fem_pm25)

start.date <- ymd_hms("2026-08-14 09:27:00")
end.date <- ymd_hms("2026-08-28 14:30:00")

ref <- ref[ref$date >= start.date & ref$date <= end.date, ]
gayo.31 <- gayo.31[gayo.31$date >= start.date & gayo.31$date <= end.date, ]
gayo.32 <- gayo.32[gayo.32$date >= start.date & gayo.32$date <= end.date, ]
gayo.33 <- gayo.33[gayo.33$date >= start.date & gayo.33$date <= end.date, ]
gayo.34 <- gayo.34[gayo.34$date >= start.date & gayo.34$date <= end.date, ]
gayo.35 <- gayo.35[gayo.35$date >= start.date & gayo.35$date <= end.date, ]
gayo.36 <- gayo.36[gayo.36$date >= start.date & gayo.36$date <= end.date, ]
gayo.37 <- gayo.37[gayo.37$date >= start.date & gayo.37$date <= end.date, ]
gayo.38 <- gayo.38[gayo.38$date >= start.date & gayo.38$date <= end.date, ]
gayo.39 <- gayo.39[gayo.39$date >= start.date & gayo.39$date <= end.date, ]


ref <- timeAverage(ref, avg.time = "5 min")
gayo.31 <- timeAverage(gayo.31, avg.time = "5 min")
gayo.32 <- timeAverage(gayo.32, avg.time = "1 min")
gayo.33 <- timeAverage(gayo.33, avg.time = "1 min")
gayo.34 <- timeAverage(gayo.34, avg.time = "1 min")
gayo.35 <- timeAverage(gayo.35, avg.time = "1 min")
gayo.36 <- timeAverage(gayo.36, avg.time = "1 min")
gayo.37 <- timeAverage(gayo.37, avg.time = "1 min")
gayo.38 <- timeAverage(gayo.38, avg.time = "1 min")
gayo.39 <- timeAverage(gayo.39, avg.time = "1 min")

sensorlist <- list(ref,gayo.31,gayo.32,gayo.33,gayo.34,
                   gayo.35,gayo.36,gayo.37,gayo.38,
                   gayo.39)

combined <- reduce(sensorlist,full_join,by = "date")

colnames(combined) <- c("date","FEM(T640)","GAYO_031","GAYO_032","GAYO_033","GAYO_034",
                        "GAYO_035","GAYO_036","GAYO_037","GAYO_038","GAYO_039")

pollutants <- colnames(combined)[colnames(combined) != "date"] 

timePlot(combined,pollutant = pollutants,group = TRUE)



comb_day <- timeAverage(combined,avg.time = "day") 

timePlot(combined, pollutant = pollutants,
         stack=FALSE,group = TRUE,date.breaks = 7,par.settings = list(fontsize = list(text = 24, family = "Serif")),
         y.relation = "free",lwd=1,lty=1,avg.time="hour",
         date.format = "%b %d,%Y",scales = list(x= list (rot = 50)),
         ylab="PM2.5 Concentration",ylim=c(0,40),key.columns = 2,key.font=2,ci=TRUE,
         xlim = as.POSIXct(c("2026-08-14","2026-08-28")),key.title = "Sensors",
         cols = c("red","yellow","green3","blue","goldenrod","gray",
                  "violet","black","maroon","orange","chocolate"),key.position = "right",
         title = expression(bold("Airbeam mini Sensors(31-40)")),fontsize = 18)



jpeg("AIR BEAM SENSORS(31-39) TIMESERIES PLOT.jpeg", units = "cm" ,width = 30, height = 20,res = 250 )
ggplot(comb_hour, aes(x = date)) +
  geom_line(aes(y = GAYO_031, colour = "GAYO_031"), lwd = 1) +
  geom_line(aes(y = GAYO_032, colour = "GAYO_032"), lwd = 1) +
  geom_line(aes(y = GAYO_033, colour = "GAYO_033"), lwd = 1) +
  geom_line(aes(y = GAYO_034, colour = "GAYO_034"), lwd = 1) +
  geom_line(aes(y = GAYO_035, colour = "GAYO_035"), lwd = 1) +
  geom_line(aes(y = GAYO_036, colour = "GAYO_036"), lwd = 1) +
  geom_line(aes(y = GAYO_037, colour = "GAYO_037"), lwd = 1) +
  geom_line(aes(y = GAYO_038, colour = "GAYO_038"), lwd = 1) +
  geom_line(aes(y = GAYO_039, colour = "GAYO_039"), lwd = 1) +
  geom_line(aes(y = `FEM(T640)`, colour = "FEM T640"), lwd = 1) +
  scale_colour_manual(
    values = c("GAYO_031" = "yellow", 
               "GAYO_032" = "turquoise",
               "GAYO_033" = "purple",
               "GAYO_034" = "goldenrod",
               "GAYO_035" = "grey50",
               "GAYO_036" = "violet",
               "GAYO_037" = "salmon",
               "GAYO_038" = "black",
               "GAYO_039" = "maroon",
               "FEM T640" = "#FF0000")) +
  
  scale_y_continuous(breaks = seq(0, 65, 10), expand = c(0, 0), limits = c(0, 65),
                    name = (expression(PM[2.5]~~(mu*g/m^3)))) +
  
  scale_x_datetime(date_breaks = "day", date_labels = "%b %d,%Y",
                   limits = as.POSIXct(c("2026-08-14", "2026-08-28")),
                   expand = c(0, 0)) +
  theme_test() +
  theme(text = element_text(family = "serif", size = 20),
        axis.ticks = element_line(size = 1.0),
        axis.ticks.length = unit(0.2, "cm"),
        panel.border = element_rect(color = "black", size = 1.5),
        plot.margin = margin(0.6, 0.6, 0.2, 0.0, "cm"),
        axis.title.y = element_text(face = "plain",size = 20, color = "black"),
        axis.text.y = element_text(colour = "black",size = 20),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "white", color = "white", size = 1.5),
        axis.text.x = element_text(hjust = 1, vjust = 1,size = 20, angle = 60, colour = "black"),
        legend.position = "inside",
        legend.position.inside = c(0.60, 0.90),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key.height = unit(0.6, "cm"),
        legend.key.width = unit(1.1, "cm")) +
  guides(colour = guide_legend(ncol = 5))
dev.off()



sum(duplicated(gayo.31)) 

 comb_min <- timeAverage(combined,avg.time = "5 min") 
 
 colnames(comb_min) <- c("date","T640","GAYO_031","GAYO_032","GAYO_033","GAYO_034",
                         "GAYO_035","GAYO_036","GAYO_037","GAYO_038","GAYO_039") 
 

intr <- comb_min |> 
  tidyr::gather(Site,comb_min, GAYO_039:T640, na.rm = TRUE)

intr.r <- Summarize(comb_min ~ Site, data = intr, na.rm = TRUE)

intr.r[10,1] <- "FEM(T640)"

intnew <- intr.r %>%
  select(Site, mean, median, sd)

colnames(intnew) <- c("Site","Mean ± SD", "Median", "sd") # change columns names


inta <- intnew |> 
  tidyr::gather(metric, mean, `Mean ± SD`:Median,-Site, na.rm = TRUE) 
inta$sd[11:20] <- NA

jpeg("BAR PLOT FOR AIR BEAM MINI SENSORS(31-40).jpeg",units="cm", width=40, height=30, res=300)
ggplot(inta, aes(x =  fct_inorder(Site), y = mean, fill = metric)) +
  geom_bar(stat="identity",position=position_dodge(0.9),  color="black", linewidth=0.9) +
  geom_errorbar(data = inta, aes(ymin = mean - sd, ymax = mean + sd),
                position = position_dodge(width = 0.9), width = 0.2, size=0.9)+
  #scale_fill_manual(values=wes_palette(name="Rushmore1"))+
  #scale_fill_brewer(palette ="Set1")+
  #scale_fill_manual(values = c("chartreuse4","darkgoldenrod3","blue","deeppink3","darkorchid3"))+
  #scale_fill_manual(values = c("red", "white", "darkgoldenrod1","#56B4E9","darkgreen"))+
  scale_fill_grafify(palette = "vibrant")+ #okabe_ito,  vibrant, bright, safe, fishy, muted , kelly
  theme_test()+
  theme(text = element_text(family = "serif", face="bold"),
        axis.ticks = element_line(size = 1.2), 
        axis.ticks.length  = unit(0.2, "cm"),
        panel.border = element_rect(color = "black", size = 1.5),
        plot.margin = margin(0.7,0.7,0.7,0.7, "cm"),
        axis.title.y = element_text(face = "bold",margin = unit(c(0, 1, 0, 0), "mm"), color = "black", size = 24),
        axis.text.y = element_text(size = 24,face="bold", colour = "black",),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(color = "black",hjust = 1, size= 30, face = "bold"),
        plot.background = element_rect(fill="white",color = "white", size = 1.5),
        axis.text.x= element_text(vjust = 1, hjust = 1.0, size = 24,face="bold",angle = 50, colour = "black"),
        legend.position = c(0.80, 0.91),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key.height = unit(0.7, "cm"),
        legend.key.width  = unit(1.4, "cm"),
        legend.text = element_text(size = 24))+
  #scale_y_continuous(breaks = seq(0,300,20))+
  scale_y_continuous(breaks = seq(0,35,5),expand = c(0, 0), limits = c(0,35))+
  ylab(expression(bold(PM[2.5]~~(mu*g/m^3))))
dev.off()

comb_hour <- timeAverage(combined, avg.time = "hour")

aa <- comb_hour %>% 
  select(`FEM(T640)`,Sensor_040)
aa <- na.omit(comb_hour)

cor(aa$Sensor_040,aa$`FEM(T640)`, method = "pearson")^2

i <- ggplot(comb_hour, aes(y = GAYO_039, x =`FEM(T640)`))+
  stat_correlation(use_label("R"), size = 10, family = "serif", label.y = 0.9, vjust = 0.7, method = "pearson")+
  #stat_poly_eq(use_label("eq"), size=6 ,family = "serif")+
  #stat_poly_eq(label.y = 0.9, size=10  ,family = "serif", vjust=0.7 )+
  geom_point(fill="green", size=7,colour="black",pch=21, stroke=1.5) +
  # geom_text(x = 150, y = 230,  color="gray20",size=5 ,family = "serif",label = paste0("MAE=", round(mae, 3)))+
  geom_abline(slope=1, intercept=0, color="black", size=0.9)+
  theme_test()+
  theme(text = element_text(family = "serif",size = 24),
        axis.ticks = element_line(size = 1.2),
        axis.ticks.length  = unit(0.2, "cm"),
        panel.border = element_rect(color = "black", size = 1.5),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        axis.title.y = element_text(margin = unit(c(0, 1, 0, 0), "mm"),face = "bold",size = 30, color = "black"),
        axis.text.y = element_text(size = 30, colour = "black", margin = unit(c(1, 1,1,1), "mm")),
        axis.title.x = element_text(face = "bold", size = 30,margin = unit(c(1,1,1,1), "mm")),
        legend.title = element_blank(),
        plot.title = element_text(color = "black",hjust = 0, size= 30, face = "bold"),
        plot.background = element_rect(fill="white",color = "white", size = 1.5),
        axis.text.x= element_text(vjust = 0.5,size = 30, angle = 0, colour = "black"),
        strip.text.x = element_text(size = 25, face = "bold"),
        strip.background = element_rect(fill = "orange", linewidth = 1.3),
        legend.direction = "horizontal",
        legend.background = element_blank(),   
        legend.key.height = unit(0.6, "cm"),
        legend.key.width  = unit(1.1, "cm"),
        legend.text = element_text(size = 15))+
  scale_y_continuous(breaks = seq(0,60,10),expand = c(0, 0), limits = c(0, 60))+
  scale_x_continuous(breaks = seq(0,60,10),expand = c(0, 0), limits = c(0,60))+
  xlab(expression(bold(FEM~(T640)~PM[2.5]~(mu*g/m^3))))+
  ylab(expression(bold(GAYO_039~PM[2.5]~(mu*g/m^3))))+
  coord_fixed()


sqrt(0.63)


#stat_cor(aes(label = after_stat(rr.label)),label.x = 0.05,label.y = 0.80,size = 6,family = "serif",parse = TRUE) +

a
b
c
d
e
f
g
h
i
j



jpeg("SCATTER PLOT FOR AIR BEAM MINI SENSORS(31-40).jpeg",units="cm", width=80, height=35, res=300)
ggarrange(a,b,c,d,e,f,g,h,i, ncol =  5, nrow = 2)
dev.off()

sqrt(63)

colnames(combined)

Agbogbloshie_Hour <- read_excel("~/Desktop/hertty/Agbogbloshie_Hour.xlsx")

Amasaman_hour <- 

Circle_Hour <- read_excel("~/Desktop/hertty/Circle_Hour.xlsx")

Graphic_Road_Hour <- read_excel("~/Desktop/hertty/Graphic_Road_Hour.xlsx")

kaneshie_Hour <- read_excel("~/Desktop/hertty/kaneshie_Hour.xlsx")

Kwashieman_Hour <- read_excel("~/Desktop/hertty/Kwashieman_Hour.xlsx")

Lapaz_Hour <- read_excel("~/Desktop/hertty/Lapaz_Hour.xlsx")

Legon_Hour <- read_csv("~/Desktop/hertty/Legon_Hour.csv")

Madina_Zongo_Hour <- read_excel("~/Desktop/hertty/Madina Zongo_Hour.xlsx")

Makola_Hour <- read_excel("~/Desktop/hertty/Makola_Hour.xlsx")

Nima_Mkt_Hour <- read_excel("~/Desktop/hertty/Nima_Mkt_Hour.xlsx")

Osu_Hour <- read_excel("~/Desktop/hertty/Osu_Hour.xlsx")

Tetteh_Quarshie_Hour <- read_excel("~/Desktop/hertty/Tetteh_Quarshie_Hour.xlsx")

Dansoman_Hour <- read_excel("~/Desktop/hertty/Dansoman_Hour.xlsx")

sqrt(0.64)

Dansoman_Hour <- Dansoman_Hour |> 
  select(date,PM25) |> 
  rename(pm2.5_dan = PM25)

Amasaman_hour$date <- lubridate::ymd_hms(Amasaman_hour$date)

Amasaman_hour <- timeAverage(Amasaman_hour, avg.time = "hour")

gayo.3 <- gayo.3 |>
  select(Timestamp,`2:Measurement_Value`) |> 
  rename(date = Timestamp, pm2.5_3 = `2:Measurement_Value`)    



sensor_list <- list(Agbogbloshie_Hour, Amasaman_hour, Circle_Hour, Graphic_Road_Hour,
                     kaneshie_Hour, Kwashieman_Hour, Lapaz_Hour, Legon_Hour, Madina_Zongo_Hour,
                     Makola_Hour, Nima_Mkt_Hour, Osu_Hour, Tetteh_Quarshie_Hour, Dansoman_Hour)


combined <- reduce(sensor_list, full_join, by = "date")



a <- timeAverage(combined,avg.time = "5 min") 

pollutants <- colnames(a)[colnames(a) != "date"]

jpeg("39 habitat map sensors correlation plot.jpeg", units = "cm" ,width = 25, height = 25,res = 250 )
corPlot(a, pollutants = pollutants,fontsize = 12,
        main = "39 Habitat map sensors pm2.5")
dev.off()



getwd()

jpeg("39 habitat map sensors timeseries plot (day).jpeg", units = "cm" ,width = 25, height = 20,res = 250 )
timePlot(a,pollutant = pollutants,fontsize = 12,
         group = T,avg.time = "day",
         lty = 1,
         lwd = 2,
         key.columns = 2,
         key.position = "right",
         ylab = "concentration",
         xlab = "date",
         main = "39 Habitat map sensors pm2.5, daily mean")
dev.off



?timePlot()
?ggplot

b <- a |> 
  select(date,pm2.5_1,pm2.5_2,pm2.5_3,pm2.5_4,pm2.5_5,pm2.5_7,pm2.5_8,pm2.5_9,pm2.5_10,pm2.5_11,
        pm2.5_12,pm2.5_13,pm2.5_14,pm2.5_15,pm2.5_16,pm2.5_17,pm2.5_18,pm2.5_19,pm2.5_20,pm2.5_22,
        pm2.5_23,pm2.5_24,pm2.5_25,pm2.5_26,pm2.5_27,pm2.5_28,pm2.5_29,pm2.5_29,pm2.5_30,pm2.5_31,
        pm2.5_32,pm2.5_33,pm2.5_34,pm2.5_35,pm2.5_36,pm2.5_37,pm2.5_38,pm2.5_39,pm2.5_40) |> 
  pivot_longer()

#6,21,32



A_AD <- A_AD |> 
  select(datetimeLocal,value)

colnames(A_AD) <- c("date","Ridge.Hospital")

b <- A_AD |> 
  select(date,Ridge.Hospital) |> 
  distinct(date,.keep_all = T)






stat_cor(method = "pearson", size = 8, family = "serif",label.x = 2, label.y = 55, digits = 2,aes(label = paste(..r.label.., sep = "~`,`~")))

#Agb$date <- lubridate::ymd_hms(Agb$date)




#A_AD <- A_AD |> 
 # select(date,site,value) |> 
  #pivot_wider(names_from = site,
   #           values_from = value)

#,
              #values_fn = mean) 

coday <- timeAverage(A_AD, avg.time = "day")

pollutants <- colnames(a)[colnames(a) != "date"]

pollutants <- colnames(combined)[colnames(combined) != "date"]

colnames(combined)
 
timePlot(combined, pollutant = pollutants,
         group = T,
         avg.time = "month",
         lty = 1,
         lwd = 2,
         key.position = "right")

colnames(combined)

jpeg("Lapaz inter.jpeg",units="cm", width=40, height=30, res=300)
calendarPlot(combined, pollutant = "pm2.5_lap", breaks = c(0,9,35,55,125,225,500), 
             cols = c("green","yellow","orange","red3","purple","maroon"),
             fontsize = 18, key.title = "US EPA AQI",
             labels = c("Good","Moderate", "Unhealthy For Sensitive Groups","Unhealthy", "Very Unhealthy", "Hazardous"),
             main = expression(bold("Lapaz inter PM"["2.5"])))
dev.off()


?calendarPlot()


getwd()

?calendarPlot()
calendarPlot(combined, pollutant = "")

calendarPlot(combined, pollutant = "Airport.Residential.Area_Tetteh.Quashie.Interchange", breaks = c(0,9,35,55,125,225,500), 
             cols = c("darkgreen","yellow","orange","red3","purple","maroon"), main = "Tetteh.Quashie.Interchange")

timeVariation(combined, pollutant = c("Asylum.down_Adabraka","Agbogbloshie","Airport.Residential.Area_Tetteh.Quashie.Interchange","Cantonments",
                                 "Flag.Staff.House.Basic.School","Kaneshie.Market","Makola_Accra.Central","Nima.Market","North.Ridge","Ridge.Hospital"),
        ylab="PM2.5",avg.time = "day",
         lty = 1,
         lwd = 2)[4]



colnames(l)
colnames(l2)

a <- rbind(l,l2)

write.csv(des,"GAYO DESCRIPTIVE STATS.csv")

str(comb_min)
describe(comb_min)
summary(comb_min)

library(ggpmisc)

stat_correlation(use_label("R"), size = 8, family = "serif", label.y = 0.9, vjust = 0.7, method = "pearson")



des <-describe(comb_min)
sum <- summary(comb_min)

aa <-comb_hour %>% 
  select(`FEM(T640)`,GAYO_031)
aa <- na.omit(aa)

cor(aa$Sensor_031,aa$`FEM T640`, method = "pearson")^2
cor(aa$Sensor_032,aa$`FEM T640`, method = "pearson")^2
cor(aa$Sensor_033,aa$`FEM T640`, method = "pearson")^2
cor(aa$Sensor_034,aa$`FEM T640`, method = "pearson")^2
cor(aa$Sensor_035,aa$`FEM T640`, method = "pearson")^2
cor(aa$Sensor_036,aa$`FEM T640`, method = "pearson")^2
cor(aa$Sensor_037,aa$`FEM T640`, method = "pearson")^2
cor(aa$Sensor_038,aa$`FEM T640`, method = "pearson")^2
cor(aa$Sensor_039,aa$`FEM T640`, method = "pearson")^2
cor(aa$Sensor_040,aa$`FEM T640`, method = "pearson")^2

#mae
mean(abs(aa$Sensor_031 - aa$`FEM T640`))
mean(abs(aa$Sensor_032 - aa$`FEM T640`))
mean(abs(aa$Sensor_033 - aa$`FEM T640`))
mean(abs(aa$Sensor_034 - aa$`FEM T640`))
mean(abs(aa$Sensor_035 - aa$`FEM T640`))
mean(abs(aa$Sensor_036 - aa$`FEM T640`))
mean(abs(aa$Sensor_037 - aa$`FEM T640`))
mean(abs(aa$Sensor_038 - aa$`FEM T640`))
mean(abs(aa$Sensor_039 - aa$`FEM T640`))
mean(abs(aa$Sensor_040 - aa$`FEM T640`))


#mbe
mean(aa$Sensor_031 - aa$`FEM T640`)
mean(aa$Sensor_032 - aa$`FEM T640`)
mean(aa$Sensor_033 - aa$`FEM T640`)
mean(aa$Sensor_034 - aa$`FEM T640`)
mean(aa$Sensor_035 - aa$`FEM T640`)
mean(aa$Sensor_036 - aa$`FEM T640`)
mean(aa$Sensor_037 - aa$`FEM T640`)
mean(aa$Sensor_038 - aa$`FEM T640`)
mean(aa$Sensor_039 - aa$`FEM T640`)
mean(aa$Sensor_040 - aa$`FEM T640`)

#rmse
sqrt(mean((aa$Sensor_031 - aa$`FEM T640`)^2))
sqrt(mean((aa$Sensor_032 - aa$`FEM T640`)^2))
sqrt(mean((aa$Sensor_033 - aa$`FEM T640`)^2))
sqrt(mean((aa$Sensor_034 - aa$`FEM T640`)^2))
sqrt(mean((aa$Sensor_035 - aa$`FEM T640`)^2))
sqrt(mean((aa$Sensor_036 - aa$`FEM T640`)^2))
sqrt(mean((aa$Sensor_037 - aa$`FEM T640`)^2))
sqrt(mean((aa$Sensor_038 - aa$`FEM T640`)^2))
sqrt(mean((aa$Sensor_039 - aa$`FEM T640`)^2))
sqrt(mean((aa$Sensor_040 - aa$`FEM T640`)^2))




library(dplyr)

# Select FEM T640 and all sensors
results <- lapply(names(aa)[-1], function(sensor) {
  
  data <- aa %>%
    select(`FEM(T640)`, all_of(sensor)) %>%
    na.omit()
  
  fem <- data$`FEM(T640)`
  sensor_data <- data[[sensor]]
  
  r <- cor(sensor_data, fem, method = "pearson")
  
  data.frame(
    Sensor = sensor,
    R = r,
    R2 = r^2,
    MAE = mean(abs(sensor_data - fem)),
    MBE = mean(sensor_data - fem),
    RMSE = sqrt(mean((sensor_data - fem)^2)),
    N = length(sensor_data)
  )
}) %>%
  bind_rows() %>%
  mutate(
    R = round(R, 2),
    R2 = round(R2, 2),
    MAE = round(MAE, 2),
    MBE = round(MBE, 2),
    RMSE = round(RMSE, 2)
  )

results


write_csv(results,"perf metrics.csv")



ab <- mean(combined$GAYO_031,na.rm = TRUE)
ac <- mean(combined$GAYO_032,na.rm = TRUE)
ad <- mean(combined$GAYO_033,na.rm = TRUE)
ae <- mean(combined$GAYO_034,na.rm = TRUE)
af <- mean(combined$GAYO_035,na.rm = TRUE)
ag <- mean(combined$GAYO_036,na.rm = TRUE)
ah <- mean(combined$GAYO_037,na.rm = TRUE)
ai <- mean(combined$GAYO_038,na.rm = TRUE)
aj <- mean(combined$GAYO_039,na.rm = TRUE)
ak <- mean(combined$GAYO_040,na.rm = TRUE)

comb_min <- comb_min |> 
  select(date,T640,GAYO_031:GAYO_040)


sensor_means <- colMeans(comb_hour %>% select(GAYO_031:GAYO_040),na.rm = TRUE)

overall_sensor_mean <- mean(sensor_means)

reference_mean <- mean(comb_hour$`FEM(T640)`,na.rm = TRUE)

difference <- overall_sensor_mean - reference_mean

absolute_difference <- abs(difference)




























