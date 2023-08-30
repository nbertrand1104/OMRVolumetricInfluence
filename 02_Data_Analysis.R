#Script is part of the I. OMR Volumetric Influence line of evidence

#Purpose: Assemble data file for use in (file to be named later) from raw CAlSIM exported data

#Packages -----
library(tidyverse)
library(readr)

#load data files----

#Delta Outflow
DO_AllAlts <- read_csv("Data/DO_AllAlts.csv", 
                       col_types = cols(...1 = col_skip()))
#View(DO_AllAlts)

#Delta Exports
EXP_AllAlts <- read_csv("Data/EXP_AllAlts.csv", 
                        col_types = cols(...1 = col_skip()))
#View(EXP_AllAlts)

#North of Delta Inflow (NDOI)
NDOI_AllAlts <- read_csv("Data/NDOI_AllAlts.csv", 
                         col_types = cols(...1 = col_skip()))
#View(NDOI_AllAlts)

#water Year types (applies to all alternatives)
WYtype_All <- read_csv("Data/WYtype_All.csv", 
                       col_types = cols(...1 = col_skip()))
#View(WYtype_All)

#Combine data files----
DOtoNDOI_join <- left_join(DO_AllAlts, NDOI_AllAlts, join_by(Alt, Date)) 

#view(DOtoNDOI_join)

DOtoNDOItoEXP_join <- left_join(DOtoNDOI_join, EXP_AllAlts, join_by(Alt, Date)) 

#view(DOtoNDOItoEXP_join)

DOtoNDOItoEXP_join$Date <- as.POSIXct(DOtoNDOItoEXP_join$Date)

DOtoNDOItoEXP_join <- DOtoNDOItoEXP_join %>% mutate(year = year(Date))

DOtoNDOItoEXPtoWY_join <- left_join(DOtoNDOItoEXP_join, WYtype_All, 
                                    join_by(year == Year_WY))
#view(DOtoNDOItoEXPtoWY_join)

#Data Analysis----

#Rename object for ease
vol_influ <- DOtoNDOItoEXPtoWY_join

#View(vol_influ)

vol_influ_diff <- vol_influ %>% 
  mutate(totalEXP = JonesExp + BanksExpSWP + BanksExpCVP) %>% 
  mutate(NDOI_Minus_totalEXP_CFS = NDOI - totalEXP) %>% 
  mutate(NDOI_Minus_totalEXP_percent = (totalEXP/NDOI)*100) %>% 
  mutate(month = month(Date), month = factor(month, levels = c(10,11,12,1,2,3,4,5,6,7,8,9))) %>% 
  mutate(year = year(Date))
  

view(vol_influ_diff)

ggplot(vol_influ_diff, aes(x= month, y=NDOI_Minus_totalEXP_percent)) + 
  geom_point()+
  facet_wrap(~Alt)

ggplot(vol_influ_diff, aes(x= month, y=NDOI_Minus_totalEXP_percent, group = year)) + 
  geom_line()+
  facet_wrap(~Alt)



#filtered by alternative
Alt2v1woTUCP <- vol_influ_diff %>% filter(Alt == "ALT2 v1 072523 woTUCP")

ggplot(Alt2v1woTUCP, aes(x= month, y=NDOI_Minus_totalEXP_percent, group = year)) + 
  geom_line()+
  geom_point()+
  labs(title = "Alt2 v1 wo TUCP")+
  facet_wrap(~Wytype)
