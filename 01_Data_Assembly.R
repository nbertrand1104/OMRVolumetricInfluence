#Script is part of the I. OMR Volumetric Influence line of evidence

#Purpose: Assemble data files from raw CAlSIM exported data. Each of the parameters is saved as a
#.csv file to be imported and analyzed in 02_Data_Analysis

#Packages -----

library(readxl)
library(tidyverse)

#Load Data ---------------

##User Data Entry----
#user will need to change the variable name and file path to run code,
#that should be all that is necessary to create the object for the calsim variable needed.
# I saved each variables variable name and file path below but commented out.

#NDOI----
#name the parameter being imported from the excel file
variable_name <- 'dinflow'
#enter the file path of the specific file to imported
file_path <- "Data/Reclamation_2021LTO_CS3_DeltaInflow_BA_2022MED_rev01_20230809_EXP1_EXP3_NAA_ALT2-v1-woutTUCP_ALT2-v1-wTUCP.xlsx"

#script executed on the datafile ----

#initialize read in listing
mysheets_fromexcel <- list()

mysheetlist <- excel_sheets(path=file_path)
i=1
for (i in 1:length(mysheetlist)){
  tempdf <- read_excel(path= file_path, sheet = mysheetlist[i])
  tempdf$sheetname <- mysheetlist[i]
  tempdf <- tempdf %>% 
    slice(7:nrow(tempdf)) %>% 
    rename("Alt" = sheetname, Date =`...1`) %>% 
    rename_with(~variable_name, CALSIM)
  mysheets_fromexcel[[i]] <- tempdf 
}

mysheets_fromexcel

variable_combined <- map_dfr(mysheets_fromexcel, bind_rows)  
view(variable_combined)
df_all <-as.data.frame(variable_combined)
assign((variable_name),df_all)

#Object viewing----
#to view dataframe enter the same name as used in "variable_name" below
view(NDOI)

#Write file----

#Save .csv file from object named from "variable_name"

write.csv(dinflow, "Data/dinflow_AllAlts.csv")

############################

#Delta Outflow----
#name the parameter being imported form the excel file
variable_name <- 'DeltaOutflow'
#enter the file path of the specific file to imported
file_path <- "Data/Reclamation_2021LTO_CS3_DO_BA_2022MED_rev01_20230809_EXP1_EXP3_NAA_ALT2-v1-woutTUCP_ALT2-v1-wTUCP.xlsx"

#script executed on the datafile ----

#initialize read in listing
mysheets_fromexcel <- list()

mysheetlist <- excel_sheets(path=file_path)
i=1
for (i in 1:length(mysheetlist)){
  tempdf <- read_excel(path= file_path,col_names = FALSE, col_types = c("date", "text"), sheet = mysheetlist[i])
  tempdf$sheetname <- mysheetlist[i]
  tempdf <- tempdf %>% 
    slice(13:nrow(tempdf)) %>% 
    rename("Alt" = sheetname, Date = "...1", DO = "...2" )# %>% 
  #rename_with(~variable_name, `CS3 EXP1 2022MED dynGWSW 071323`)
  tempdf$Date <-as.Date(tempdf$Date) 
  mysheets_fromexcel[[i]] <- tempdf 
}

colnames(mysheets_fromexcel[[1]])
mysheets_fromexcel[[1]]

variable_combined <- map_dfr(mysheets_fromexcel, bind_rows)  

DO <-as.data.frame(variable_combined)
#assign((variable_name),df_all)

#Object viewing----
#to view dataframe enter the same name as used in "variable_name" below
view(DO)

#Write file----

#Save .csv file from object named from "variable_name"

#write.csv(DO, "Data/DO_AllAlts.csv")

############################

#Delta Exports----
#I removed the Water Year type tab and saved it as a separate file because 
#its file structure was different from other sheets

#name the parameter being imported form the excel file
variable_name <- 'Exports'
#enter the file path of the specific file to imported
file_path <- "Data/Reclamation_2021LTO_CS3_Exports_WYT_BA_2022MED_rev01_20230809_EXP1_EXP3_NAA_ALT2-v1-woutTUCP_ALT2-v1-wTUCP.xlsx"

#script executed on the datafile ----

#initialize read in listing
mysheets_fromexcel <- list()

mysheetlist <- excel_sheets(path=file_path)
i=1
for (i in 1:length(mysheetlist)){
  tempdf <- read_excel(path= file_path,col_names = FALSE, col_types = c("date", "text", "text","text"), sheet = mysheetlist[i])
  tempdf$sheetname <- mysheetlist[i]
  tempdf <- tempdf %>% 
    slice(13:nrow(tempdf)) %>% 
    rename("Alt" = sheetname, Date = "...1", JonesExp = "...2",BanksExpSWP = "...3",BanksExpCVP = "...4")
  tempdf$Date <-as.Date(tempdf$Date) 
  mysheets_fromexcel[[i]] <- tempdf 
}

colnames(mysheets_fromexcel[[1]])
mysheets_fromexcel[[1]]

variable_combined <- map_dfr(mysheets_fromexcel, bind_rows)  

EXP <-as.data.frame(variable_combined)
#assign((variable_name),df_all)

#Object viewing----
#to view dataframe enter the same name as used in "variable_name" below
view(EXP)

#Write file----

#Save .csv file from object named from "variable_name"

#write.csv(EXP, "Data/EXP_AllAlts.csv")
################################################

#Water Year type
#this uses the file I created by extracting the water year type tab from the exports data file

#load data ----
Wateryears <- read_excel("Data/Reclamation_2021LTO_CS3_WYT_BA_2022MED_rev01_20230809_EXP1_EXP3_NAA_ALT2-v1-woutTUCP_ALT2-v1-wTUCP.xlsx", 
                                                                                                                 sheet = "WYT_Sac Valley Index", col_names = FALSE, 
                                                                                                                 col_types = c("numeric", "numeric", "numeric", 
                                                                                                                               "numeric", "numeric", "skip"), skip = 3)

#View(Wateryears)
#Data clean up and formatting----
#Select and renames columns 
WYtype_codes <- Wateryears %>% select("...1", "...2") %>% 
  rename(Year_WY = "...1", Code = "...2")

WaterYearCodes <- read_excel("Data/WaterYearCodes.xlsx")
#View(WaterYearCodes)

WYtype <- left_join(WYtype_codes,WaterYearCodes, by = "Code")

#view(WYtype)
#Write file----

#Save .csv file

write.csv(WYtype, "Data/WYtype_All.csv")


#Files are exported as .csv files to then be compiled in a separate script

