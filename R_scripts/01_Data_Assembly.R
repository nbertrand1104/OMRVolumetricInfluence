#01_Data_Assembly.R----

#master copy

#About----
#Script is part of the I. OMR Volumetric Influence line of evidence

#Purpose: Assemble data files from raw CAlSIM exported data. Each of the parameters is saved as a
#.csv file to be imported and analyzed in 02_Data_Analysis
#script is organized to use the Rstudio Table of contents features

#Packages -----

library(readxl)
library(tidyverse)

#User Data Entry----

#User will set root object to personal directory location

#Set working directory ----
#set to location of root object to highest tier directory
root <- "C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRVolumetricInfluence"
setwd(root)

#these root object use directories within OMRVolumetricInfluence
data_root<-file.path(root,"Data")
code_root <- file.path(root,"R_scripts")
output_root <- file.path(root,"Output")

#load data filesindividually in each section below

#Delta Inflow----
#name the parameter being imported from the excel file
variable_name <- 'dinflow'
#enter the file path of the specific file to imported

file_path <- file.path(data_root, "Reclamation_2021LTO_CS3 _DeltaInflow_2022MED_rev02_20230914_EXP1_EXP3_NAA_ALT2v1woutTUCP_ALT2v1wTUCP_ALT2v2_AT2v3_ALT1_ALT4_ALT3.xlsx")

##script executed on the datafile ----

#initialize read in listing
mysheets_fromexcel <- list()

mysheetlist <- excel_sheets(path=file_path)
i=1
for (i in 1:length(mysheetlist)){
  tempdf <- read_excel(path= file_path,col_names = FALSE, col_types = c("date", "text"), sheet = mysheetlist[i])
  tempdf$sheetname <- mysheetlist[i]
  tempdf <- tempdf %>% 
    slice(13:nrow(tempdf)) %>% 
    rename("Alt" = sheetname, Date = "...1", dinflow = "...2")
  tempdf$Date <-as.Date(tempdf$Date) 
  mysheets_fromexcel[[i]] <- tempdf 
}

mysheets_fromexcel[[1]]
variable_combined <- map_dfr(mysheets_fromexcel, bind_rows)  
#view(variable_combined)
df_all <-as.data.frame(variable_combined)
assign((variable_name),df_all)

##Object viewing----
#to view dataframe enter the same name as used in "variable_name" below
#view(dinflow)


##Write file----

#Save .csv file from object named from "variable_name"

write.csv(dinflow, file.path(output_root, "dinflow_AllAlts.csv")) 

#Delta Outflow----
#name the parameter being imported form the excel file
variable_name <- 'DeltaOutflow'
#enter the file path of the specific file to imported

file_path <- file.path(data_root,"Reclamation_2021LTO_CS3 _DO_2022MED_rev02_20230913_EXP1_EXP3_NAA_ALT2v1woutTUCP_ALT2v1wTUCP_ALT2v2_AT2v3_ALT1_ALT4_ALT3.xlsx")

##Script executed on the datafile ----

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

#view specific cells
#colnames(mysheets_fromexcel[[1]])
#mysheets_fromexcel[[1]]

variable_combined <- map_dfr(mysheets_fromexcel, bind_rows)  

DO <-as.data.frame(variable_combined)
#assign((variable_name),df_all)

##Object viewing----
#to view dataframe enter the same name as used in "variable_name" below

#view(DO)

##Write file----
#Save .csv file from object named from "variable_name"

write.csv(DO, file.path(output_root,"DO_AllAlts.csv"))



#Delta Exports----
#I removed the Water Year type tab and saved it as a separate file because 
#its file structure was different from other sheets

#name the parameter being imported form the excel file
variable_name <- 'Exports'
#enter the file path of the specific file to imported

file_path <- file.path(data_root,"Reclamation_2021LTO_CS3 _Exports_WYT_2022MED_rev02_20230913_EXP1_EXP3_NAA_ALT2v1woutTUCP_ALT2v1wTUCP_ALT2v2_AT2v3_ALT1_ALT4_ALT3_edit.xlsx") 

##Script executed on the datafile ----

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

##Object viewing----
#to view dataframe enter the same name as used in "variable_name" below
#view(EXP)

##Write file----

#Save .csv file from object named from "variable_name"

# write.csv(EXP, "Data/EXP_AllAlts.csv")
write.csv(EXP, file.path(output_root,"EXP_AllAlts.csv"))  

#Water Year type----
#this uses the file I created by extracting the water year type tab from the exports data file

##load data ----
Wateryears <- read_excel(file.path(data_root,"Reclamation_2021LTO_CS3 _Exports_WYT_2022MED_rev02_20230913_EXP1_EXP3_NAA_ALT2v1woutTUCP_ALT2v1wTUCP_ALT2v2_AT2v3_ALT1_ALT4.xlsx"),
            sheet = "WYT_Sac Valley Index", col_names = FALSE,
            col_types = c("numeric", "numeric", "numeric",
           "numeric", "numeric", "skip"), skip = 3)

#View(Wateryears)
##Data clean up and formatting----
#Select and renames columns 
WYtype_codes <- Wateryears %>% select("...1", "...2") %>% 
  rename(Year_WY = "...1", Code = "...2")

WaterYearCodes <- read_excel(file.path(data_root,"WaterYearCodes.xlsx")) # SCM
#View(WaterYearCodes)

WYtype <- left_join(WYtype_codes,WaterYearCodes, by = "Code")

#view(WYtype)
##Write file----

#Save .csv file

write.csv(WYtype, file.path(output_root,"WYtype_All.csv"))  


#Files are exported as .csv files to then be compiled in a separate script




