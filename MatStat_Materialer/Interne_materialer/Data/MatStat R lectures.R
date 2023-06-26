#1 ----
library(tidyverse)
#---- Readr;
library(readr)
ballon <- read_table2("C:/Users/Victor Z. Nygaard/Dropbox/Matematik - Københavns Universitet/3. År/3.3/3.3.A MatStat2020-2021/MatStat Materialer/Interne materialer/Data/ballon.txt", 
                      col_types = cols(Obs = col_integer(), 
                                       X4 = col_skip()))
View(ballon)

#-----
#Pure Rstudio;
#Gider ikke af en eller anden grund på Bærbar.


#---
theme_set(theme_bw()) #GGplot2 



































