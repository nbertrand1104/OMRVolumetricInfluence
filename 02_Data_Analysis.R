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

#North of Delta Inflow (dinflow)
dinflow_AllAlts <- read_csv("Data/dinflow_AllAlts.csv", 
                         col_types = cols(...1 = col_skip()))
#View(dinflow_AllAlts)

#water Year types (applies to all alternatives)
WYtype_All <- read_csv("Data/WYtype_All.csv", 
                       col_types = cols(...1 = col_skip()))
#View(WYtype_All)

#Combine data files----
DOtodinflow_join <- left_join(DO_AllAlts, dinflow_AllAlts, join_by(Alt, Date)) 

#view(DOtodinflow_join)

DOtodinflowtoEXP_join <- left_join(DOtodinflow_join, EXP_AllAlts, join_by(Alt, Date)) 

#view(DOtodinflowtoEXP_join)

DOtodinflowtoEXP_join$Date <- as.POSIXct(DOtodinflowtoEXP_join$Date)

DOtodinflowtoEXP_join <- DOtodinflowtoEXP_join %>% mutate(year = year(Date))

DOtodinflowtoEXPtoWY_join <- left_join(DOtodinflowtoEXP_join, WYtype_All, 
                                    join_by(year == Year_WY))
#view(DOtodinflowtoEXPtoWY_join)

#Data Analysis----

#Rename object for ease
vol_influ <- DOtodinflowtoEXPtoWY_join

View(vol_influ)
class(vol_influ$Dinflow)
#Manipulates the data table and created variables for the visualizations
vol_influ_diff <- vol_influ %>% 
  mutate(totalEXP = JonesExp + BanksExpSWP + BanksExpCVP) %>% 
  mutate(dinflow_Minus_totalEXP_CFS = dinflow - totalEXP) %>% 
  mutate(dinflow_Minus_totalEXP_percent = (totalEXP/dinflow)*100) %>% 
  mutate(month = month(Date)) %>%  
  mutate(month_f = factor(month, levels = c(10,11,12,1,2,3,4,5,6,7,8,9))) %>% 
  mutate(year = year(Date))
  
class(vol_influ_diff$month_f)
#view(vol_influ_diff)

#Figures----

#scatter plot 
ggplot(vol_influ_diff, aes(x= month_f, y=dinflow_Minus_totalEXP_percent)) + 
  geom_point()+
  labs(title = "Scatterplot all months in all alternatives")+
  facet_wrap(~Alt)

ggplot(vol_influ_diff, aes(x= month_f, y=dinflow_Minus_totalEXP_percent, group = year)) + 
  geom_line()+
  labs(title = "lineplot all months in all alternatives")+
  facet_wrap(~Alt)

#Violinplots by month by Water year type & alt
ggplot(vol_influ_diff, aes(x=month_f, y=dinflow_Minus_totalEXP_percent)) + 
  geom_violin(fill = "light blue")+
  geom_boxplot(width=0.15)+
  labs(title = "Violinplots by month by Water year type & alt")+
  facet_grid(Alt~Wytype)

#box plot of all alternatives by month
ggplot(vol_influ_diff, aes(x=month_f, y=dinflow_Minus_totalEXP_percent, fill = Alt)) + 
  geom_boxplot(width=0.5)+
  labs(title = "All Alternatives By Months")+
  xlab("Month")+
  ylab("Percent Exported from dinflow")+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette="Paired")



#filtered by alternative
Alt2v1woTUCP <- vol_influ_diff %>% filter(Alt == "ALT2 v1 090723 woTUCP")

ggplot(Alt2v1woTUCP, aes(x= month_f, y=dinflow_Minus_totalEXP_percent, group = year)) + 
  geom_line()+
  geom_point()+
  labs(title = "Alt2 v1 wo TUCP")+
  facet_grid(Wytype~ Alt)

#This plots the density curves of the monthly percentages across alternatives
library(plyr)
mu <- ddply(vol_influ_diff, "Alt", summarise, grp.mean=mean(dinflow_Minus_totalEXP_percent))

ggplot(vol_influ_diff, aes(x=dinflow_Minus_totalEXP_percent, color=Alt)) +
  geom_density()+
  #geom_vline(data=mu, aes(xintercept=grp.mean, color=Alt),linetype="dashed")+
  labs(title = "Densityplot of the % dinflow exported by month")+
  facet_wrap(~month_f)

filtered <- vol_influ_diff %>% filter(month_f == 6)
ggplot(filtered, aes(x=dinflow_Minus_totalEXP_percent, color=Alt)) +
  geom_density()+
  #geom_vline(data=mu, aes(xintercept=grp.mean, color=Alt),linetype="dashed")+
  labs(title = "Densityplot of the % dinflow exported by month")+
  facet_wrap(~month_f)

ggplot(vol_influ_diff, aes(x=month_f, y=dinflow_Minus_totalEXP_percent)) + 
  geom_violin(fill = "light blue")+
  geom_boxplot(width=0.15)+
  labs(title = "Violinplots by month by Water year type & alt")+
  facet_grid(Alt~Wytype)

#box plot of all alternatives by water year type
ggplot(vol_influ_diff, aes(x=month_f, y=dinflow_Minus_totalEXP_percent, fill = Alt)) + 
  geom_boxplot(width=1)+
  labs(title = "All Alternatives By Water Year Type")+
  xlab("Month")+
  ylab("Percent Exported from dinflow")+
  theme_bw()+
  theme(legend.position = "bottom")+
  facet_wrap(~Wytype)


ggplot(vol_influ_diff, aes(x=month_f, y=dinflow_Minus_totalEXP_percent, fill = Wytype)) + 
  geom_boxplot(width=1)+
  labs(title = "All Alternatives By Water Year Type")+
  xlab("Month")+
  ylab("Percent Exported from dinflow")+
  theme(legend.position = "bottom")+
  facet_wrap(~Alt)


ggplot(vol_influ_diff, aes(x=month_f, y=dinflow_Minus_totalEXP_percent, fill = Wytype)) + 
  geom_boxplot(width=1)+
  labs(title = "All Alternatives By Water Year Type")+
  theme(legend.position = "bottom")+
  facet_wrap(~Alt)

ggplot(vol_influ_diff, aes(x=Alt, y=dinflow_Minus_totalEXP_percent)) + 
  geom_boxplot(width=0.5)+
  labs(title = "boxplots by month by alt")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_grid(~month_f)


#filters to create a single figure for an alt
filtered2 <- vol_influ_diff %>% filter(Alt == "ALT2 v1 072523 wTUCP")
#view(filtered2)
ggplot(filtered2, aes(x=month_f, y=dinflow_Minus_totalEXP_percent, fill = Wytype)) + 
  geom_boxplot(width=1)+
  labs(title = "ALT2 v1 072523 wTUCP")+
  theme(legend.position = "bottom")

#Table Construction ----

#This section will work on formatting a table for export to the documents.

#View(vol_influ_diff)
colnames(vol_influ_diff)

#selects down to relevant columns
#generates a table of summary values
tbl <- vol_influ_diff %>% 
  select("Alt",                        
         "year", 
         "month",
         "Wytype", 
         "dinflow",
         "dinflow_Minus_totalEXP_percent") %>% 
  group_by(Alt, month) %>% 
  summarise(mean_dinflow = mean(dinflow),
            var_dinflow = var(dinflow),
            sd_dinflow = sd(dinflow), 
            .groups = "keep")
?summarize
view(tbl)

#this table is formatted for export as a csv for manipulation in excel

expt_tbl <- tbl %>% 
  pivot_wider(names_from = Alt,
              values_from = c("mean_dinflow",
                              "var_dinflow",
                              "sd_dinflow"))
  
view(expt_tbl)

#write_excel_csv(expt_tbl, "Data/tableforcopyandpaste.csv")
###############

#
comp_tbl <- vol_influ_diff %>% 
  select("Alt",                        
         "year", 
         "month",
         "Wytype", 
         "dinflow_Minus_totalEXP_percent") %>% 
  pivot_wider(names_from = Alt,
              values_from = c("dinflow_Minus_totalEXP_percent")) %>% 
  group_by(Wytype, month)

view(comp_tbl)

diff_tbl <- comp_tbl %>% group_by(Wytype,year, month) %>% 
  reframe(NAAminusEXP1 = `NAA 090723`-`EXP1 090623`,
            NAAminusEXP3 = `NAA 090723`-`EXP3 090623`,
            NAAminusEXP3 = `NAA 090723`-`EXP3 090623`,
            NAAminusALT1 = `NAA 090723`-`ALT1 090923`,
            NAAminusALT2v1wTUCP = `NAA 090723`-`ALT2 v1 090723 wTUCP`,
            NAAminusALT2v1woTUCP = `NAA 090723`-`ALT2 v1 090723 woTUCP`,
            NAAminusALT2v2noTUCP = `NAA 090723`-`ALT2 v2 090723 noTUCP`,
            NAAminusALT2v3noTUCP = `NAA 090723`-`ALT2 v3 090723 noTUCP`,
            NAAminusALT4 = `NAA 090723`-`ALT4 090823`) %>% 
  pivot_longer(NAAminusEXP1:NAAminusALT4) %>% 
  mutate(month_f = factor(month, levels = c(10,11,12,1,2,3,4,5,6,7,8,9))) 

view(diff_tbl)

colnames(comp_tbl)

ggplot(diff_tbl, aes(x=year, y=value, color = name)) + 
  geom_point()

#list of column headers----
# "Date",
# "DO",                         
# "Alt",                        
# "dinflow",                       
# "JonesExp",                   
# "BanksExpSWP" ,               
# "BanksExpCVP" ,               
# "year",                       
# "Code",                       
# "Wytype",                     
# "totalEXP",                   
# "dinflow_Minus_totalEXP_CFS",    
# "dinflow_Minus_totalEXP_percent",
# "month"
