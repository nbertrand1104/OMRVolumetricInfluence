#04_Table.R----

#master copy
# Nick Bertrand
# October 2023

#About----
#Script is part of the I. OMR Volumetric Influence line of evidence
#Purpose: Generate the tables from dataframe created from the vol_influ_diff.csv that was saved to output
# in 02_Data_Analysis.R. The .csv files output from this script are used by WordFormattedTableEXport.Rmd. 
#which are copied into other word docs. 
#script is organized to use the Rstudio Table of contents features

#Table numbers here in no longer match word documents

# Set working directory ----
#set to location of root object to highest tier directory
root <- "C:/Users/nbertrand/Desktop/Bertrand/GitHub/OMRVolumetricInfluence"
setwd(root)

#these root object use directories within OMRVolumetricInfluence
data_root<-file.path(root,"Data")
code_root <- file.path(root,"R_scripts")
output_root <- file.path(root,"Output")

#Libraries ----
library(plotly)
library(ggplot2)
library(data.table)
library(tidyverse)
library(readr)
library(lubridate)
library(reticulate)

#Import Data ----
base_table <- read_csv(file.path(output_root, "vol_influ_diff.csv"), 
                       col_types = cols(...1 = col_skip()))
#view(base_table)

table_select <- base_table %>% select(Wytype,Date,DO,Alt,dinflow,
                             JonesExp, BanksExpSWP, BanksExpCVP,FlowBin, 
                             OMRBin, MonthlyOMR, totalEXP, 
                             totalEXP, dinflow_Minus_totalEXP_CFS, 
                             dinflow_Minus_totalEXP_percent,
                             month, month_f, Wytype, Alt_name,Wytype_f,FlowBin_f)
#View(table_select)

#Table For WY Type Means And Standard Deviations ----

tbl_1 <- table_select %>% 
  select(Alt_name, Wytype_f,month_f,dinflow_Minus_totalEXP_percent)%>%
  group_by(Alt_name,Wytype_f,month_f) %>% 
  mutate(percent_monthlymean = mean(dinflow_Minus_totalEXP_percent), 
         percent_monthlysd = sd(dinflow_Minus_totalEXP_percent)) %>% 
  ungroup()
#view(tbl_1)

tbl_1_piv_wider <-tbl_1 %>% 
  pivot_wider(
    id_cols = c("Wytype_f", "month_f"), 
    names_from = "Alt_name", 
    values_from = c("percent_monthlymean", "percent_monthlysd")) %>% 
    tidyr::unnest() 

tbl_1_distinct <- distinct(tbl_1_piv_wider)
#view(tbl_1_distinct)

options(digits = 2)      
tbl1_final <- tbl_1_distinct %>% 
  rename(`Water Year Type` = Wytype_f, 
         Month = month_f, 
         `EXP1 Monthly Mean %`=`percent_monthlymean_EXP1`,
         `EXP3 Monthly Mean %`  = `percent_monthlymean_EXP3`,
         `NAA Monthly Mean %`  = `percent_monthlymean_NAA`,
         `Alt1 Monthly Mean %`  = `percent_monthlymean_Alt1`,
         `Alt2wTUCPwoVA Monthly Mean %`  = `percent_monthlymean_Alt2wTUCPwoVA`,
         `Alt2woTUCPwoVA Monthly Mean %`  = `percent_monthlymean_Alt2woTUCPwoVA`,
         `Alt2woTUCPDeltaVA Monthly Mean %`  = `percent_monthlymean_Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA Monthly Mean %`  = `percent_monthlymean_Alt2woTUCPAllVA`,
         `Alt3 Monthly Mean %`  = `percent_monthlymean_Alt3`,
         `Alt4 Monthly Mean %`  = `percent_monthlymean_Alt4`,
         #space to separate standard deviation columns from the mean columns above
         `EXP1 SD`  = `percent_monthlysd_EXP1`,
         `EXP3 SD`  = `percent_monthlysd_EXP3`,
         `NAA SD`  = `percent_monthlysd_NAA`,
         `Alt1 SD`  = `percent_monthlysd_Alt1`,
         `Alt2wTUCPwoVA SD`  = `percent_monthlysd_Alt2wTUCPwoVA`,
         `Alt2woTUCPwoVA SD`  = `percent_monthlysd_Alt2woTUCPwoVA`,
         `Alt2woTUCPDeltaVA SD`  = `percent_monthlysd_Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA SD`  = `percent_monthlysd_Alt2woTUCPAllVA`,
         `Alt4 SD`  = `percent_monthlysd_Alt4`,
         `Alt3 SD`  = `percent_monthlysd_Alt3`) %>% 
  select(`Water Year Type`,
       `Month`,
       `NAA Monthly Mean %`,              
       `NAA SD`,                  
       `Alt1 Monthly Mean %`,             
       `Alt1 SD`,                 
       `Alt2woTUCPwoVA Monthly Mean %`,   
       `Alt2woTUCPwoVA SD`,       
       `Alt2woTUCPDeltaVA Monthly Mean %`,
       `Alt2woTUCPDeltaVA SD`,    
       `Alt2woTUCPAllVA Monthly Mean %`,  
       `Alt2woTUCPAllVA SD`,      
       `Alt2wTUCPwoVA Monthly Mean %`,    
       `Alt2wTUCPwoVA SD`,        
       `Alt3 Monthly Mean %`,             
       `Alt3 SD`,         
       `Alt4 Monthly Mean %`,             
       `Alt4 SD`,                 
       `EXP1 Monthly Mean %`,            
       `EXP1 SD`,                 
       `EXP3 Monthly Mean %`,             
       `EXP3 SD`) 
  
        
#View(tbl1_final)
##Writes table WY Type Means And Standard Deviations csv file to output----
#options(digits = 2)
write.csv(tbl1_final, file.path(output_root,"Table1WYTypeMonthlyMeanswSD.csv"))
 
#EIS Alternatives WY type Means and Percent Difference Table ----
#uses object created in previous section
EIS_tbl_1 <- tbl1_final %>% 
  select(`Water Year Type`,
         `Month`,
         `NAA Monthly Mean %`,              
         `Alt1 Monthly Mean %`,             
         `Alt2wTUCPwoVA Monthly Mean %`,   
         `Alt2woTUCPwoVA Monthly Mean %`,
         `Alt2woTUCPDeltaVA Monthly Mean %`,  
         `Alt2woTUCPAllVA Monthly Mean %`,    
         `Alt3 Monthly Mean %`,             
         `Alt4 Monthly Mean %`,
         `EXP1 Monthly Mean %`,            
         `EXP3 Monthly Mean %`) %>% 
  mutate(`Alt1 % Diff` = 100*((`Alt1 Monthly Mean %` - `NAA Monthly Mean %` )/
                                (`NAA Monthly Mean %`)),
         `Alt2wTUCPwoVA % Diff` = 100*((`Alt2wTUCPwoVA Monthly Mean %` - `NAA Monthly Mean %` )/
                                                                               (`NAA Monthly Mean %`)),
         `Alt2woTUCPwoVA % Diff` = 100*((`Alt2woTUCPwoVA Monthly Mean %` - `NAA Monthly Mean %` )/
                                         (`NAA Monthly Mean %`)),
         `Alt2woTUCPDeltaVA % Diff` = 100*((`Alt2woTUCPDeltaVA Monthly Mean %` - `NAA Monthly Mean %` )/
                                         (`NAA Monthly Mean %`)),
         `Alt2woTUCPAllVA % Diff` = 100*((`Alt2woTUCPAllVA Monthly Mean %` - `NAA Monthly Mean %` )/
                                         (`NAA Monthly Mean %`)),
         `Alt3 % Diff` = 100*((`Alt3 Monthly Mean %` - `NAA Monthly Mean %` )/
                                         (`NAA Monthly Mean %`)),
         `Alt4 % Diff` = 100*((`Alt4 Monthly Mean %` - `NAA Monthly Mean %` )/
                                         (`NAA Monthly Mean %`)),
         `EXP1 % Diff` = 100*((`EXP1 Monthly Mean %` - `NAA Monthly Mean %` )/
                                (`NAA Monthly Mean %`)),
         `EXP3 % Diff` = 100*((`EXP3 Monthly Mean %` - `NAA Monthly Mean %` )/
                                (`NAA Monthly Mean %`))) %>% 
  select(`Water Year Type`,
         `Month`,
         `NAA Monthly Mean %`,              
         `Alt1 Monthly Mean %`, 
         `Alt1 % Diff`,
         `Alt2wTUCPwoVA Monthly Mean %`,
         `Alt2wTUCPwoVA % Diff`,
         `Alt2woTUCPwoVA Monthly Mean %`,
         `Alt2woTUCPwoVA % Diff`,
         `Alt2woTUCPDeltaVA Monthly Mean %`,
         `Alt2woTUCPDeltaVA % Diff`,
         `Alt2woTUCPAllVA Monthly Mean %`,
         `Alt2woTUCPAllVA % Diff`,
         `Alt3 Monthly Mean %`,
         `Alt3 % Diff`,
         `Alt4 Monthly Mean %`,
         `Alt4 % Diff`,
         `EXP1 Monthly Mean %`,
         `EXP1 % Diff`,
         `EXP3 Monthly Mean %`,
         `EXP3 % Diff`)

#view(EIS_tbl_1)
##Writes table WY Type Means And % difference csv file to output----
options(digits = 1)
write.csv(EIS_tbl_1, file.path(output_root,"Table1WYTypeMonthlyMeansPercentDiff.csv"))


#EIS Table for Inflow Group Means ----

tbl_3 <- table_select %>%
  select(Alt_name, FlowBin, FlowBin_f, dinflow_Minus_totalEXP_percent) %>% 
  group_by(Alt_name, FlowBin_f) %>%
  summarise_at(vars(dinflow_Minus_totalEXP_percent), 
               list(`Mean % Delta Inflow Exported` = mean))

#view(tbl_3)  

tbl_3_piv_wider <- tbl_3 %>% 
  group_by(Alt_name) %>%
  mutate(rn = row_number()) %>% 
  pivot_wider(names_from = Alt_name, values_from = `Mean % Delta Inflow Exported`) %>%
  rename(`Inflow Group` = FlowBin_f) %>% 
  select(`Inflow Group`,
         `NAA`,                  
         `Alt1`,                 
         `Alt2woTUCPwoVA`,   
         `Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,
         `Alt2wTUCPwoVA`,    
         `Alt3`,        
         `Alt4`,      
         `EXP1`,            
         `EXP3`) 

#view(tbl_3_piv_wider)

options(digits = 2)      
tbl3_final <- tbl_3_piv_wider  

#View(tbl3_final)
##Writes Inflow group means csv file to output----

#options(digits = 1)
write.csv(tbl3_final, file.path(output_root,"Table3InflowGroupMeans.csv"))


#EIS Alternatives inflow group Table ----
#uses object created in previous section
EIS_tbl_3 <- tbl3_final %>% 
  select(`Inflow Group`,
         `NAA`,                  
         `Alt1`,                 
         `Alt2woTUCPwoVA`,   
         `Alt2woTUCPDeltaVA`,
         `Alt2woTUCPAllVA`,
         `Alt2wTUCPwoVA`,    
         `Alt3`,        
         `Alt4`) %>% 
  mutate(`Alt1 % Diff` = 100*((`Alt1` - `NAA` )/
                                (`NAA`)),
         `Alt2wTUCPwoVA % Diff` = 100*((`Alt2wTUCPwoVA` - `NAA` )/
                                         (`NAA`)),
         `Alt2woTUCPwoVA % Diff` = 100*((`Alt2woTUCPwoVA` - `NAA` )/
                                          (`NAA`)),
         `Alt2woTUCPDeltaVA % Diff` = 100*((`Alt2woTUCPDeltaVA` - `NAA` )/
                                             (`NAA`)),
         `Alt2woTUCPAllVA % Diff` = 100*((`Alt2woTUCPAllVA` - `NAA` )/
                                           (`NAA`)),
         `Alt3 % Diff` = 100*((`Alt3` - `NAA` )/
                                (`NAA`)),
         `Alt4 % Diff` = 100*((`Alt4` - `NAA` )/
                                (`NAA`))) %>% 
  select(`Inflow Group`,
         `NAA`,              
         `Alt1`, 
         `Alt1 % Diff`,
         `Alt2wTUCPwoVA`,
         `Alt2wTUCPwoVA % Diff`,
         `Alt2woTUCPwoVA`,
         `Alt2woTUCPwoVA % Diff`,
         `Alt2woTUCPDeltaVA`,
         `Alt2woTUCPDeltaVA % Diff`,
         `Alt2woTUCPAllVA`,
         `Alt2woTUCPAllVA % Diff`,
         `Alt3`,
         `Alt3 % Diff`,
         `Alt4`,
         `Alt4 % Diff`)

#view(EIS_tbl_3)
## Writes Inflow Group Means % difference----
write.csv(EIS_tbl_3, file.path(output_root,"Table4InflowGroupMeansPercentDiff.csv"))


#Summary Analysis Mean Min & Max ----
#Summary by water Year
summary_table_select_WY <- table_select %>%
  select(Wytype, Alt, dinflow, FlowBin, totalEXP, dinflow_Minus_totalEXP_percent, Alt_name,Wytype_f, FlowBin_f) %>% 
  group_by(Alt_name, Wytype_f) %>% 
  reframe(Mean = mean(dinflow_Minus_totalEXP_percent),
          Min = min(dinflow_Minus_totalEXP_percent),
          Max = max(dinflow_Minus_totalEXP_percent)) %>% 
  rename(Alternative = Alt_name, `Water Year` = Wytype_f)

#view(summary_table_select_WY)
##Writes Mean min max table to output----
write.csv(summary_table_select_WY, file.path(output_root,"Table5WYMeanMinMAx.csv"))


#summary by Inflow Group
summary_vol_influ_diff_flowbin <- table_select %>%
  select(Wytype, Alt, dinflow, FlowBin, totalEXP, dinflow_Minus_totalEXP_percent, Alt_name,Wytype_f, FlowBin_f) %>% 
  group_by(Alt_name, FlowBin) %>% 
  reframe(Mean = mean(dinflow_Minus_totalEXP_percent),
          Min = min(dinflow_Minus_totalEXP_percent),
          Max = max(dinflow_Minus_totalEXP_percent)) %>% 
  rename(Alternative = Alt_name, `Inflow Group` = FlowBin)


#view(summary_vol_influ_diff_flowbin)
##Writes Mean min max table to output----
write.csv(summary_vol_influ_diff_flowbin, file.path(output_root,"Table6InflowGroupMeanMinMAx.csv"))

#BA Tables----

#filters data to BA only alternative data
BA_included <- c("EXP1", "EXP3", "NAA", "Alt2woTUCPwoVA","Alt2woTUCPDeltaVA",
                 "Alt2woTUCPAllVA","Alt2wTUCPwoVA")
BA_Data <- table_select %>% 
  filter(Alt_name %in%BA_included)

##Summary by water Year----
summary_table_select_WY_BA <- BA_Data %>%
  select(Wytype, Alt, dinflow, FlowBin, totalEXP, dinflow_Minus_totalEXP_percent, Alt_name,Wytype_f, FlowBin_f) %>% 
  group_by(Alt_name, Wytype_f) %>% 
  reframe(Mean = mean(dinflow_Minus_totalEXP_percent),
          Min = min(dinflow_Minus_totalEXP_percent),
          Max = max(dinflow_Minus_totalEXP_percent)) %>% 
  rename(Alternative = Alt_name, `Water Year` = Wytype_f)

#view(summary_table_select_WY_BA)
###Writes table for BA summary----
write.csv(summary_table_select_WY_BA, file.path(output_root,"BATable1WYMeanMinMAx.csv"))


##summary by Inflow Group----
summary_vol_influ_diff_flowbin_BA <- BA_Data %>%
  select(Wytype, Alt, dinflow, FlowBin, totalEXP, dinflow_Minus_totalEXP_percent, Alt_name,Wytype_f, FlowBin_f) %>% 
  group_by(Alt_name, FlowBin) %>% 
  reframe(Mean = mean(dinflow_Minus_totalEXP_percent),
          Min = min(dinflow_Minus_totalEXP_percent),
          Max = max(dinflow_Minus_totalEXP_percent)) %>% 
  rename(Alternative = Alt_name, `Inflow Group` = FlowBin)


#view(summary_vol_influ_diff_flowbin_BA)

###Writes table for BA summary----
write.csv(summary_vol_influ_diff_flowbin_BA, file.path(output_root,"BATable2InflowGroupMeanMinMAx.csv"))

#Examining the distribution ----
#this script examines the distribution of observed values for data exploration only.

distribution_counts <- table_select %>%
  #unite(Alt_Flowbin, as.character("Alt"),as.character("FlowBin")) %>% 
  group_by(Alt,Flowbin) %>% 
  count()

distribution_counts <- table_select %>%
  group_by(Alt, FlowBin) %>% 
  count()

#view(distribution_counts)

p<-ggplot(data=distribution_counts, aes(x=FlowBin, y=n)) +
  geom_bar(stat="identity")+
  facet_wrap(~Alt)
p
