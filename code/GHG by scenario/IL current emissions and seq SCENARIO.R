# What are the current emissions and C sequestration in IL?
# Data shared with AFT from Regrow DNDC analysis.
# 

# load packages
library(reshape2) # for dcast()
library(ggplot2)
library(dplyr)

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
unw <- read.csv("data/un-weighted_results.csv")
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
