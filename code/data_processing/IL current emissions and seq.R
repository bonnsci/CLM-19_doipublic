# What are the current emissions and C sequestration in IL?
# Data shared with AFT from Regrow DNDC analysis.
# 

# load packages
library(reshape2) # for dcast()
library(ggplot2)
library (tidyverse)
#library(dplyr) #is having conflict with 'filter' lap

# to update data, run "download data.R"

# load IL DNDC scenario output 
# (corresponds to "il_adoption_scenarios_final_outputs.csv")
scenil <- read.csv("data/scenarios/scenil.csv")
########### METADATA IN 3 ROWS per data column (column name -- unit -- description)
# column name       unit          description
# site_name         NA            String combination of one of the locations we simulated on. In the format of {x}_{y} where x refers to the region and y refers to the numeric id of that site. These will be of the format of h_{}, v_{}, a_{}, f_{}, IL-n_{}, IL-s_{} which represents the points for hops, vineyards, almonds, and forage, Illinois North, Illinois South respectively
# croP              NA            Name of the cropping system. Full list of unique names are: almonds, hops, grape, alfalfa, corn, corn-grain, corn-silage and soybean
# scenario code     integer       Scenario number that can be found in section 2.
# climate_scenario  NA            Either ‘rcp26’ or ‘rcp60’ that were defined in section 1.2
# year              integer       Calendar year in which results are for.
# dsoc              tonne C/ha    Calendar year change in soil organic carbon stocks (Dec. 31 - Jan. 1).
# n2o               tonne N/ha    Calendar year daily sum (Jan. 1 - Dec. 31) of nitrous oxide direct emissions in co2 equivalents.
# ch4               tonne C/ha    Calendar year daily sum (Jan. 1 - Dec. 31) of methane emissions in co2 equivalents.
# indirect_n2o      tonne N/ha    Calendar year daily sum (Jan. 1 - Dec. 31) of nitrous oxide indirect emissions in co2 equivalents.
# total_n2o         tonne N/ha    Sum of n2o and indirect_n2o
# ghg               tonne co2e/ha Sum of ghg_dsoc, ghg_ch4, ghg_total_n2o in co2 equivalents.
# ghg_dsoc          tonne co2e/ha Calendar year change in soil organic carbon stocks (Dec. 31 - Jan. 1) in co2 equivalents.
# ghg_ch4           tonne co2e/ha Calendar year daily sum (Jan. 1 - Dec. 31) of methane emissions in co2 equivalents.
# ghg_n2o           tonne co2e/ha Calendar year daily sum (Jan. 1 - Dec. 31) of nitrous oxide direct emissions in co2 equivalents.
# ghg_indirect_n2o  tonne co2e/ha Calendar year daily sum (Jan. 1 - Dec. 31) of nitrous oxide indirect emissions in co2 equivalents.
# ghg_total_n2o     tonne co2e/ha Sum of ghg_n2o and ghg_indirect_n2o in co2 equivalents 


# load IL unweighted simulation DNDC output 
# (corresponds to "yearly_outputs_post-weighting.csv")
unw <- read.csv("data/unw.csv")
########### METADATA IN 3 ROWS per data column (column name -- unit -- description)
# column name       unit          description
# site_name         NA            String combination of one of the locations we simulated on. In the format of {x}_{y} where x refers to the region and y refers to the numeric id of that site. These will be of the format of h_{}, v_{}, a_{}, f_{}, IL-n_{}, IL-s_{} which represents the points for hops, vineyards, almonds, and forage, Illinois North, Illinois South respectively
# crop_name         NA            Name of the cropping system. Full list of unique names are: almond, hops, grape, alfalfa, corn, corn-grain, corn-silage and soybean
# management        NA            Management code as associated with the simulations described in section 2.{x}.1
# climate_scenario  NA            Either ‘rcp26’ or ‘rcp60’ that were defined in section 1.2
# year              integer       Calendar year in which results are for.
# dsoc              tonne C/ha    Calendar year change in soil organic carbon stocks (Dec. 31 - Jan. 1).
# n2o               tonne N/ha    Calendar year daily sum (Jan. 1 - Dec. 31) of nitrous oxide direct emissions in co2 equivalents.
# ch4               tonne C/ha    Calendar year daily sum (Jan. 1 - Dec. 31) of methane emissions in co2 equivalents.
# indirect_n2o      tonne N/ha    Calendar year daily sum (Jan. 1 - Dec. 31) of nitrous oxide indirect emissions in co2 equivalents.
# total_n2o         tonne N/ha    Sum of n2o and indirect_n2o
# ghg               tonne co2e/ha Sum of ghg_dsoc, ghg_ch4, ghg_total_n2o in co2 equivalents.
# ghg_dsoc          tonne co2e/ha Calendar year change in soil organic carbon stocks (Dec. 31 - Jan. 1) in co2 equivalents.
# ghg_ch4           tonne co2e/ha Calendar year daily sum (Jan. 1 - Dec. 31) of methane emissions in co2 equivalents.
# ghg_n2o           tonne co2e/ha Calendar year daily sum (Jan. 1 - Dec. 31) of nitrous oxide direct emissions in co2 equivalents.
# ghg_indirect_n2o  tonne co2e/ha Calendar year daily sum (Jan. 1 - Dec. 31) of nitrous oxide indirect emissions in co2 equivalents.
# ghg_total_n2o     tonne co2e/ha Sum of ghg_n2o and ghg_indirect_n2o in co2 equivalents 


###current GHG emissions and C sequestration from corn-soybean agriculture in Illinois
##state level
# n2o tonne N/ha Calendar year daily sum (Jan. 1 - Dec. 31) of nitrous oxide direct emissions in co2 equivalents.
# ch4 tonne C/ha Calendar year daily sum (Jan. 1 - Dec. 31) of methane emissions in co2 equivalents.
# cseq == dsoc tonne C/ha Calendar year change in soil organic carbon stocks (Dec. 31 - Jan. 1).


scenil %>% filter (scenario.code == 1 & year ==2022) %>% #taking only scenario 1 and 2022 for analysis
  select(climate_scenario, dsoc, n2o, ch4) %>%
  gather(var, value, dsoc:ch4) %>%
  group_by(climate_scenario, var) %>%
  summarise_all(list(mean, sd)) %>% #fn1, fn2
  ggplot(aes(x=climate_scenario, y=fn1, group=climate_scenario)) + 
  geom_bar(stat="identity", position=position_dodge(), aes(fill=climate_scenario)) +
  geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2), linewidth=0.8, color="#000000",
                position=position_dodge(0.9), width=0.2) +
  facet_wrap(~var, scales = "free_y") +
  scale_fill_manual(values=c("#DDAA33", "#228833", "#66CCEE"), 
                    #labels=c("Corn", "Soybeans", "Winter Wheat"),
                    name="Climate Scenario") +
  #scale_y_continuous(breaks=seq(0,70,10)) +
  ylab("co2 equivalents (Tn/ha)") +
  xlab("") +
  theme_bw()+
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text=element_text(size=12),
    axis.title.x=element_text(size=12, face="bold"),
    axis.title.y=element_text(size=12, face="bold"),
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
    strip.text=element_text(size=12, face="bold"),
    legend.text=element_text(size=11),
    legend.title=element_text(size=12, face="bold"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
  )

ggsave("plots/ch4_n20_dsoc_2022sn1.png", width=12, height=6)     
  

##by sub-scenario
##year by year trends 
unw %>% filter (site_name == "IL-n_1" & year %in% (2010:2022) & climate_scenario == "rcp26") %>%  #taking only scenario 1, rcp26 and years 2010-2022 for analysis
  select(management, year, dsoc, n2o, ch4) %>%
  gather(var, value, dsoc:ch4) %>%
  ggplot(aes(as.numeric(year), value, color= management)) +
  geom_point () +
  facet_wrap(~var, scales = "free_y") +
  geom_line() +
  scale_x_continuous(breaks=seq(2010,2022,1))
  
ggsave("plots/temp1.png", width=12, height=6)     

  
  
###summary figure by management practice
  
unw %>% filter (site_name == "IL-n_1" & year %in% (2010:2022) & climate_scenario == "rcp26") %>%  #taking only scenario 1, rcp26 and years 2010-2022 for analysis
  select(management,dsoc, n2o, ch4) %>%
  gather(var, value, dsoc:ch4) %>%
  group_by(management, var) %>%
  summarise_all(list(mean, sd)) %>% #fn1, fn2
  ggplot(aes(x=management, y=fn1, group=management)) + 
  geom_bar(stat="identity", position=position_dodge(), aes(fill=management)) +
  geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2), linewidth=0.8, color="#000000",
                position=position_dodge(0.9), width=0.2) +
  facet_wrap(~var, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave("plots/temp2.png", width=12, height=6)     

##do anova analysis for management effect 


