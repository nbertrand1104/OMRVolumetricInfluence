#03_Visualizations.R----

#master copy
# Nick Bertrand
# October 2023

#About----
#Script is part of the I. OMR Volumetric Influence line of evidence
#Purpose: Generate the figures from dataframes created from the vol_influ_diff.csv that was saved to output
# in 02_Data_Analysis.R 
#script is organized to use the Rstudio Table of contents features

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
library(plyr)

#Import Data ----
vol <- read_csv(file.path(output_root, "vol_influ_diff.csv"), 
                       col_types = cols(...1 = col_skip()))
#view(vol)

vol_select <- vol %>% select(Wytype,Wytype_f,Date,DO,Alt,dinflow,
                             JonesExp, BanksExpSWP, BanksExpCVP,FlowBin,
                             MonthlyOMR, totalEXP, 
                             totalEXP, dinflow_Minus_totalEXP_CFS, 
                             dinflow_Minus_totalEXP_percent,
                             month, month_f, Wytype, Alt_name, FlowBin_f)


#view(vol_select)

#EIS Plotly Visualizations----
#all plots are titled to reflect the grouping applied to the data in the figure.

#factor order for flow bin groups
inflow_order = c("lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi")
#reclamation color palette
my_palette=c('#003E51','#007396', '#C69214', '#DDCBA4','#FF671F', '#215732','#4C12A1','#9a3324', "#88CCEE","#AA4499")


##Boxplot with flowbin by Alt groups----

plot_ly(vol_select, x = ~dinflow_Minus_totalEXP_percent, y = ~interaction(FlowBin_f, Alt_name),colors=my_palette) %>%
  add_boxplot(color = ~Alt_name) %>%
  layout(title="Distribution of % Exported grouped by Inflow Group and Alternative", 
         xaxis = list(title = '% Delta Inflow Exported'), 
         yaxis = list(title = 'Flowbin & Alternative',
                      tickfont=list(size=4)))


##Boxplot with Alt by flowbin groups----  
plot_ly(vol_select, x = ~dinflow_Minus_totalEXP_percent, y = ~interaction(Alt_name, FlowBin_f),colors=my_palette) %>%
  add_boxplot(color = ~Alt) %>% 
  layout(title="Distribution of % Exported grouped by Alternative and Inflow Group", 
         xaxis = list(title = '% Delta Inflow Exported'), 
         yaxis = list(title = 'Alternative & Flowbin',
                      tickfont=list(size=4)))
  

##Boxplot with Alt by WaterYear groups---- 
plot_ly(vol_select, x = ~dinflow_Minus_totalEXP_percent, y = ~interaction(Alt_name, Wytype_f),colors=my_palette) %>%
  add_boxplot(color = ~Alt_name) %>%
  layout(title="Distribution of % Exported grouped by Alternative and Water Year Type", 
         xaxis = list(title = '% Delta Inflow Exported'), 
         yaxis = list(title = 'Alternative & Water Year Type',
                      tickfont=list(size=8)))

##Boxplot with WaterYear by Alt groups----  
plot_ly(vol_select, x = ~dinflow_Minus_totalEXP_percent, y = ~interaction(Wytype_f,Alt_name), colors=my_palette) %>%
  add_boxplot(color = ~Alt_name) %>%
  layout(title="Distribution of % Exported grouped by Water Year Type and Alternative", 
         xaxis = list(title = '% Delta Inflow Exported'), 
         yaxis = list(title = 'Water Year Type & Alternative',
                      tickfont=list(size=8)))



#EIS ggplot visualizations----
#density plots of data
#some filters applied to data to improve visualization

#ggplot pallet
my_palette=c('#003E51','#007396', '#C69214', '#DDCBA4','#FF671F', '#215732','#4C12A1','#9a3324', "#88CCEE","#AA4499")


#Demsity plot filtered for EXP3 and EXP1
#geom_vline kept to illustrate 0s in EXP3 and EXP1
filtered_EXP1andEXP3 <- vol_select %>% 
  filter(Alt != "EXP3 090623" & Alt != "EXP1 090623")

##Density Plot Grouped by WY type Filtered----
mu_Wytype_fil <- ddply(vol_select, .(Alt_name, Wytype_f), summarise, grp.mean=mean(dinflow_Minus_totalEXP_percent))

ggplot(filtered_EXP1andEXP3, aes(x=dinflow_Minus_totalEXP_percent, color=Alt_name)) +
  geom_density(size = 1.5)+
  geom_vline(data=mu_Wytype_fil, aes(xintercept=grp.mean, color=Alt_name),linetype="dashed")+
  labs(title = "Density plot of the % Delta Inflow grouped by Water Year Type")+
  theme(legend.title=element_blank())+
  xlab("% Inflow Exported")+
  facet_wrap(~Wytype_f)+
  scale_colour_manual(values = my_palette)


##Density Plot Grouped by Inflow Group (Flowbin)----
mu_flowbin_fil <- ddply(vol_select, .(Alt_name,FlowBin_f), summarise, grp.mean=mean(dinflow_Minus_totalEXP_percent))
ggplot(filtered_EXP1andEXP3, aes(x=dinflow_Minus_totalEXP_percent, color=Alt_name)) +
  geom_density(size = 1.5)+
  geom_vline(data=mu_flowbin_fil, aes(xintercept=grp.mean, color=Alt_name),linetype="dashed")+
  labs(title = "Densityplot of the % Delta Inflow exported by Inflow Group")+
  theme(legend.title=element_blank())+
  xlab("% Inflow Exported")+
  facet_wrap(~FlowBin_f)+
  scale_colour_manual(values = my_palette)

#BA ggplot Figures----
BA_palette=c('#007396','#C69214','#DDCBA4','#9a3324','#88CCEE','#AA4499')

#Filtered to just BA alts
BA_included <- c("EXP1", "EXP3", "NAA", "Alt2woTUCPwoVA","Alt2woTUCPDeltaVA",
                 "Alt2woTUCPAllVA")
#dropped "Alt2wTUCPwoVA" from list above
filtered_BA_Data <- vol_select %>% 
  filter(Alt_name %in% BA_included)

BA_included_noEXPs <- c("NAA", "Alt2woTUCPwoVA","Alt2woTUCPDeltaVA",
                 "Alt2woTUCPAllVA")

filtered_BA_Data_noEXPs <- vol_select %>% 
  filter(Alt_name %in% BA_included_noEXPs)

##BA Density plot grouped by WY Type----
mu_BA_Data_WY <- ddply(filtered_BA_Data, .(Alt_name,Wytype_f), summarise, grp.mean=mean(dinflow_Minus_totalEXP_percent))

ggplot(filtered_BA_Data_noEXPs, aes(x=dinflow_Minus_totalEXP_percent, color=Alt_name)) +
  geom_density(size = 1.5)+
  geom_vline(data=mu_BA_Data_WY, aes(xintercept=grp.mean, color=Alt_name),linetype="dashed")+
  labs(title = "Density plot of the % Delta Inflow grouped by Water Year Type")+
  theme(legend.title=element_blank())+
  xlab("% Inflow Exported")+
  facet_wrap(~Wytype_f)+
  scale_colour_manual(values = BA_palette)
##BA Density plot grouped by inflow grou (Flowbin)----
mu_BA_Data_flowbin <- ddply(filtered_BA_Data, .(Alt_name,FlowBin_f), summarise, grp.mean=mean(dinflow_Minus_totalEXP_percent))
ggplot(filtered_BA_Data_noEXPs, aes(x=dinflow_Minus_totalEXP_percent, color=Alt_name)) +
  geom_density(size = 1.5)+
  geom_vline(data=mu_BA_Data_flowbin, aes(xintercept=grp.mean, color=Alt_name),linetype="dashed")+
  labs(title = "Densityplot of the % Delta Inflow exported by Inflow Group")+
  theme(legend.title=element_blank())+
  xlab("% Inflow Exported")+
  facet_wrap(~FlowBin_f)+
  scale_colour_manual(values = BA_palette)

#BA Plotly Figures----
##BA Boxplot with flowbin by Alt groups----
#uses filtered_BA_Data object created in BA ggplot section

plot_ly(filtered_BA_Data, x = ~dinflow_Minus_totalEXP_percent, y = ~interaction(FlowBin_f, Alt_name),colors=BA_palette) %>%
  add_boxplot(color = ~Alt_name) %>%
  layout(title="Distribution of % Exported grouped by Inflow Group and Alternative", 
         xaxis = list(title = '% Delta Inflow Exported'), 
         yaxis = list(title = 'Flowbin & Alternative',
                      tickfont=list(size=4)))


##BA Boxplot with Alt by flowbin groups----  
plot_ly(filtered_BA_Data, x = ~dinflow_Minus_totalEXP_percent, y = ~interaction(Alt_name, FlowBin_f),colors=BA_palette) %>%
  add_boxplot(color = ~Alt) %>% 
  layout(title="Distribution of % Exported grouped by Alternative and Inflow Group", 
         xaxis = list(title = '% Delta Inflow Exported'), 
         yaxis = list(title = 'Alternative & Flowbin',
                      tickfont=list(size=4)))



#BA Plotly Figures----

##BA Boxplot with Alt by WaterYear groups---- 
plot_ly(filtered_BA_Data, x = ~dinflow_Minus_totalEXP_percent, y = ~interaction(Alt_name, Wytype_f),colors=BA_palette) %>%
  add_boxplot(color = ~Alt_name) %>%
  layout(title="Distribution of % Exported grouped by Alternative and Water Year Type", 
         xaxis = list(title = '% Delta Inflow Exported'), 
         yaxis = list(title = 'Alternative & Water Year Type',
                      tickfont=list(size=8)))

## BA Boxplot with WaterYear by Alt groups----  
plot_ly(filtered_BA_Data, x = ~dinflow_Minus_totalEXP_percent, y = ~interaction(Wytype_f,Alt_name), colors=BA_palette) %>%
  add_boxplot(color = ~Alt_name) %>%
  layout(title="Distribution of % Exported grouped by Water Year Type and Alternative", 
         xaxis = list(title = '% Delta Inflow Exported'), 
         yaxis = list(title = 'Water Year Type & Alternative',
                      tickfont=list(size=8)))

