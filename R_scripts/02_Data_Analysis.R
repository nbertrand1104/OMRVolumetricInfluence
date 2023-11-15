#02_Data_Analysis.R----

#master copy
# Nick Bertrand
# October 2023

#About----
#Script is part of the I. OMR Volumetric Influence line of evidence
#Purpose: This script takes in the files created in 01_Data_Assembly.R and uses them
#to construct the dataframe used in calculating the needs values to be 
#visualized in 03_Visualizations.R
#script is organized to use the Rstudio Table of contents features

## Clear work Space----
rm(list=ls())

#Set working directory ----
#set to location of root object to highest tier directory
root <- "C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRVolumetricInfluence"
setwd(root)

#these root object use directories within OMRVolumetricInfluence
data_root<-file.path(root,"Data")
code_root <- file.path(root,"R_scripts")
output_root <- file.path(root,"Output")

#Libraries ----
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyverse)
library(readr)
library(lubridate)
library(readxl)


#Import data----

##Delta Outflow----
DO_AllAlts <- read_csv(file.path(output_root, "DO_AllAlts.csv"), 
                       col_types = cols(...1 = col_skip()))
#view(DO_AllAlts)
##Delta Exports----
EXP_AllAlts <- read_csv(file.path(output_root, "EXP_AllAlts.csv"), 
                        col_types = cols(...1 = col_skip()))

##North of Delta Inflow (dinflow)----
dinflow_AllAlts <- read_csv(file.path(output_root, "dinflow_AllAlts.csv"), 
                            col_types = cols(...1 = col_skip()))

##water Year types (applies to all alternatives)----
WYtype_All <- read_csv(file.path(output_root,"WYtype_All.csv"), 
                       col_types = cols(...1 = col_skip()))
#view(WYtype_All)

##CalSim record with each month and year combination----
sheetNames <- excel_sheets(file.path(data_root, "Reclamation_2021LTO_SacR_SJR_OMR_Binning_rev01_20230929_result_Long.xlsx")) 
l_sheets <- list(sheetNames)

##Pull CalSim Bin Data (Flow bins for each model scenario by month and year)----
Bin_DataCS <- read_excel(file.path(data_root, "Reclamation_2021LTO_SacR_SJR_OMR_Binning_rev01_20230929_result_Long.xlsx"), 
                         sheet = sheetNames[1])  # sheet one is CalSim Data

##Pull in CalSim bin data with actual monthly OMR values
CS_monthlyOMR <- read_excel(file.path(data_root, "Reclamation_2021LTO_OMR_rev01_20231010.xlsx"),
                            skip = 11)

##CalSim monthly OMR wide to long----
CS_monthlyOMR$Month <- month(ymd(CS_monthlyOMR$Date))
CS_monthlyOMR$Year  <- year(ymd(CS_monthlyOMR$Date))

CS_monthlyOMRlong <- melt(setDT(CS_monthlyOMR), 
                          id.vars = c("Date", "Month", "Year"), 
                          variable.name = "Alt")
#view(CS_monthlyOMRlong)
##Correct alternative name formatting----
AlternativeNames <- read_csv(file.path(data_root, "AlternativeNames.csv"), 
                             col_types = cols(...1 = col_skip()))

#View(AlternativeNames)

##Correct Water Year Formatting----
WaterYear_short <- read_csv(file.path(data_root, "WaterTypeAbbreviations.csv"), 
                            col_types = cols(...1 = col_skip()))


#Filter data to December - June----

ind_DJ <- which(Bin_DataCS$Month < 7 | Bin_DataCS$Month == 12)
Bin_DataCS <- Bin_DataCS[ind_DJ,]

ind_DJ <- which(CS_monthlyOMRlong$Month < 7 | CS_monthlyOMRlong$Month == 12)
CS_monthlyOMRlong <- CS_monthlyOMRlong[ind_DJ, ]

# Subset DFs to Dec - June (decreases 12,000 to 7,000)  
DO_AllAlts$Month       <- month(ymd(DO_AllAlts$Date))
ind_DJ                 <- which(DO_AllAlts$Month < 7 | DO_AllAlts$Month == 12)
DO_AllAlts             <- DO_AllAlts[ind_DJ,]

EXP_AllAlts$Month      <- month(ymd(EXP_AllAlts$Date))
ind_DJ                 <- which(EXP_AllAlts$Month < 7 | EXP_AllAlts$Month == 12)
EXP_AllAlts            <- EXP_AllAlts[ind_DJ,]

dinflow_AllAlts$Month  <- month(ymd(dinflow_AllAlts$Date))
ind_DJ                 <- which(dinflow_AllAlts$Month < 7 | dinflow_AllAlts$Month == 12)
dinflow_AllAlts        <- dinflow_AllAlts[ind_DJ,]

rm(ind_DJ, l_sheets, sheetNames)




#Combine Data files----
##Joins----
DOtodinflow_join <- left_join(DO_AllAlts, dinflow_AllAlts, join_by(Alt, Date))
#view(DOtodinflow_join)
DOtodinflowtoEXP_join <- left_join(DOtodinflow_join, EXP_AllAlts, join_by(Alt, Date))
DOtodinflowtoEXP_join$Date <- as.POSIXct(DOtodinflowtoEXP_join$Date)
DOtodinflowtoEXP_join <- DOtodinflowtoEXP_join %>% mutate(year = year(Date), Month = month(Date))

vol_intermediate <- left_join(DOtodinflowtoEXP_join,Bin_DataCS, 
                        join_by(Month == Month, year == Year, Alt == Alt ))

Volumetric1 <- left_join(vol_intermediate, CS_monthlyOMRlong, 
                       join_by(Month == Month, year == Year, Alt == Alt))


Volumetric2<- left_join(Volumetric1, WYtype_All, 
                        join_by(year == Year_WY))
Volumetric3 <- left_join(Volumetric2,AlternativeNames, join_by(Alt == Alt)) %>% 
  left_join(.,WaterYear_short, by = "Wytype") 
#view(Volumetric3)

#factor order for inflow groups
inflow_order = c("lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi")
WaterYear_short_order = c("W","AN","BN","CD","D")

Volumetric4 <- Volumetric3 %>% mutate(FlowBin_f = factor(FlowBin, levels = inflow_order)) %>% 
  mutate(Wytype_f = factor(Wytype_short, levels = WaterYear_short_order))
#view(Volumetric4)

# remove value for Monthly OMR label
Volumetric4$MonthlyOMR <- Volumetric4$value
Volumetric <- subset(Volumetric4, select = -value)

##Cleanup----
rm(DOtodinflow_join, DOtodinflowtoEXP_join, vol_intermediate, 
   CS_monthlyOMR)


#Data Analysis----

#Rename object for ease
vol_influ <- Volumetric  
#view(vol_influ)
class(vol_influ$dinflow)
vol_influ$BinNum_f <- factor(vol_influ$BinNum) 
##Data Manipulation----
#Manipulates the data table and created variables for the visualizations
vol_influ_diff <- vol_influ %>% 
  mutate(totalEXP = JonesExp + BanksExpSWP + BanksExpCVP) %>% 
  mutate(dinflow_Minus_totalEXP_CFS = dinflow - totalEXP) %>% 
  mutate(dinflow_Minus_totalEXP_percent = (totalEXP/dinflow)*100) %>% 
  mutate(month = month(Date.x)) %>%   
  mutate(month_f = factor(month, levels = c(12,1,2,3,4,5,6))) %>%  
  mutate(year = year(Date.x))  

##View Object----
#View(vol_influ_diff)

#Write Output File----
#Create .csv files for use in 03_Visualizations.R

#write.csv(vol_influ_diff, file.path(output_root, "vol_influ_diff.csv")) 

