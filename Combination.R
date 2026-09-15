library(tidyverse)
library(readxl)
library(dplyr)
library(purrr)
library(writexl)

?lapply()

path <- "OpenAQ 01:06:2026-21:07:2026 /Ridge Hospital"

files <- list.files(path,pattern = "\\.csv$",full.names = TRUE)

d <- files %>% 
 lapply(read.csv) %>% 
  bind_rows()

write_xlsx(d,"Ridge Hospital.xlsx")  



getwd()


path <- "~/Downloads/AQMESH.S1"

files <- list.files(path,pattern = "\\.csv$",full.names = T)


 







