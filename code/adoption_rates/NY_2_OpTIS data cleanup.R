# OpTIS adoption rate information for New York aggregated by counties 2015-2021.
# Data shared with AFT from Regrow analysis.
# Column "kpi_name" gives different types of land use areas, with "area" meaning the 
# analyzed acres of the county, tillage type/ cover crop / etc.
# units are ACRES
# this code cleans up the data by: 
## 1) comparing acres evaluated by OpTIS in 2017 with the harvested cropland acres reported in 2017 Ag Census
## 2) removing counties where <50% of harvested cropland was evaluated by OpTIS
## 3) removing counties in the lowest fifth percentile of harvested cropland (small ag counties)
## 4) reports excluded counties
## 5) calculates percent adoption for CT, CC, NT, RT by year and mean adoption for 2018-2021
## 6) saves cleaned up data in new .csv files

# Bonnie McGill - started July 2023



# load packages
library(reshape2) # for dcast()
library(tidyverse)


# to update data from google drive, run "download data.R"




# load the data  #### REDO THIS WITH UPDATED NY CORN AND SOYBEAN FOR ALL COUNTIES

datny <- read.csv("data/optis/datny.csv")


datny <- datny[,c(-1, -2, -4, -7, -10)]

# sum(is.na(datny$kpi_value))
# unique(datny$kpi_name)

# cast data into wide form for separate columns by kpi_name and kpi_value
datw <- dcast(datny, ...~kpi_name, value.var="kpi_value")
colnames(datw)[c(1,4:8)] <- c("county", "eval.acres", "ct.acres", "cc.acres", "nt.acres", "rt.acres") 

datw$county <- str_remove(datw$county, " County")

######### DATA CLEANING
# compare acres evaluated to harvested corn grain and soybean acres from NASS annual surveys


# ### IMPROVEMENT: add harvested forage crop acres 2015-2021 by county from NASS survey data
# ### only need to do once
# # prep data
# # note alfalfa ("HAYLAGE") and wheat data are not by county.
df <- list.files(path="data/NASS harvested acres", pattern=" NY ", full.names=T)
df <- df[1:2]  # note alfalfa ("HAYLAGE") and wheat data are not by county, so not helpful for assessing acreage evaluated as % of county crop acres.
df <- lapply(df, function(i){
        i <- read.csv(i)
        i <- i %>%
          select("Year", "County", "Data.Item", "Value") %>%
          filter(!County %in% c("OTHER COUNTIES", "OTHER (COMBINED) COUNTIES")) %>%
          rename("year"="Year", "county"="County", "crop_name"="Data.Item", "harv_ac"="Value")
        i$harv_ac <- as.numeric(gsub(",", "", i$harv_ac)) # remove commas from values
        i$crop_name <- gsub("CORN, GRAIN - ACRES HARVESTED", "Corn grain", i$crop_name)
        i$crop_name <- gsub("CORN, SILAGE - ACRES HARVESTED", "Corn silage", i$crop_name)
        i$crop_name <- gsub("SOYBEANS - ACRES HARVESTED", "Soybeans", i$crop_name)
        i$county <- tools::toTitleCase(tolower(i$county)) # get county names ready to merge with OpTIS spellings, which are correct
        i <- arrange(i, county)
      })

# unique(df[[1]]$crop_name)
# unique(df[[1]]$County)  ### 14 of 62 acres
# [1] "CAYUGA"      "CORTLAND"    "ONONDAGA"    "TOMPKINS"    "CATTARAUGUS" "CHAUTAUQUA"  "ERIE"       
# [8] "GENESEE"     "LIVINGSTON"  "ONTARIO"     "WAYNE"       "WYOMING"     "YATES"       "ALLEGANY"   
# unique(df[[2]]$County)  ### 15 of 62 acres
# [1] "CAYUGA"      "CORTLAND"    "ONONDAGA"    "TOMPKINS"    "ALLEGANY"    "CATTARAUGUS" "CHAUTAUQUA" 
# [8] "STEUBEN"     "ERIE"        "GENESEE"     "LIVINGSTON"  "ONTARIO"     "WAYNE"       "WYOMING"    
# [15] "YATES"      

acres <- bind_rows(df) # combine dfs into one df
rm(df)

write.csv(acres, "data/NASS harvested acres/NY_harvested_forages2015-2021.csv", row.names=F)



# read in NASS data prepped above
acres <- read.csv("data/NASS harvested acres/NY_harvested_forages2015-2021.csv")

acres <- acres %>%
  mutate(crop_name2=gsub("Corn grain", "Corn",
                         gsub("Corn silage", "Corn", x=crop_name))) %>%
  group_by(county, year, crop_name2) %>%
  mutate(harv_ac2 = sum(harv_ac)) %>%
  ungroup() %>%
  distinct(year, county, crop_name2, .keep_all=T) %>%
  select(year, county, harv_ac2, crop_name2) %>%
  rename(harv_ac = harv_ac2, crop_name = crop_name2)

# Optis data
NYac <- aggregate(eval.acres ~ crop_name + county + year, dat=datw, FUN="sum") %>%  # first sum acres across crops per year
  arrange(year,county)

# unique(NYac$crop_name)
# [1] "Alfalfa"      "Corn"         "Soybeans"     "Winter Wheat"

# remove alfalfa and wheat from NYac (NASS data below doesn't have these)
NYac <- NYac[NYac$crop_name %in% c("Corn", "Soybeans"),]

# join OpTIS And NASS data
NYac <- inner_join(NYac, acres) # acres does not have all counties, some counties do not report
# For the purposes of putting these two data together to check evaluation, inner_join makes sense
# why keep one obs if not in the other.

NYac$perc_eval <- round((100*NYac$eval.acres / NYac$harv_ac), digits=2)
# NAs created where we have OpTIS data but not harvest data

# look for outliers, remember the survey is not necessarily the truth either
min(na.omit(NYac$perc_eval)) # 50.07 --yay! >50% in all cases
hist(NYac$perc_eval)  
NYac[NYac$perc_eval>125,] %>% 
  arrange(crop_name, county, year)

# crop_name      county year eval.acres harv_ac perc_eval
# 1       Corn    Allegany 2015  12077.587    7000    172.54
# 2       Corn Cattaraugus 2019  21313.787   14700    144.99
# 3       Corn      Cayuga 2018  75529.514   43900    172.05
# 4       Corn      Cayuga 2019  71911.116   44000    163.43
# 5       Corn  Chautauqua 2019  23202.314   14500    160.02
# 6       Corn     Genesee 2017  46976.152   28600    164.25
# 7       Corn     Genesee 2021  44351.999   30900    143.53
# 8       Corn     Ontario 2018  53993.484   42200    127.95
# 9       Corn    Tompkins 2019  13807.748    7200    191.77
# 10      Corn       Yates 2015  34032.014   15900    214.04
# 11      Corn       Yates 2016  34980.596    5000    699.61
# 12      Corn       Yates 2019  38428.462    9500    404.51
# 13      Corn       Yates 2020  34299.195    8470    404.95
# 14      Corn       Yates 2021  32763.735   20570    159.28
# 15  Soybeans      Cayuga 2017  34558.314   27400    126.13
# 16  Soybeans       Yates 2015   6293.457    4500    139.85
# 17  Soybeans       Yates 2016   7721.888    5740    134.53
# 18  Soybeans       Yates 2017   5697.461    3700    153.99
# 19  Soybeans       Yates 2018  11727.632    4700    249.52
# 20  Soybeans       Yates 2019   6979.486    4300    162.31
# 21  Soybeans       Yates 2020  10679.466    4970    214.88
# 22  Soybeans       Yates 2021   9759.191    4430    220.30

# for now it seems we should ignore these data where perc_eval >125%

NYac <- NYac %>%
  filter(perc_eval <126)

hist(NYac$perc_eval)

# "Described the total acres evaluated and how the % was calculated." 
# % CC, NT, RT was calculated by dividing the number of acres with those practices 
# detected by the number of cropland acres evaluated by OpTIS per county-year

# what proportion of cropland acres did OpTIS evaluate per county?

se <- function(x) {
  sd(x)/sqrt(length(x))
}

mean(na.omit(NYac$perc_eval)) # 95.20
se(na.omit(NYac$perc_eval)) # 1.34
min(na.omit(NYac$perc_eval)) # 50.17
max(na.omit(NYac$perc_eval)) # 124.73


length(unique(NYac$county))

# number of counties represented in IL data
datw %>%
  # filter(state %in% "IL") %>%
  distinct(county) %>%
  n_distinct()
# 49 (48, 1 IS IN VT)
# there are 62 counties in NY


datw%>%
  distinct(county) %>%
  arrange(county)
# 14 left out are: 
# Bronx (NYC)
# Hamilton (Wilderness areas)
# Kings (Brooklyn)
# Monroe (along L. Ontario)
# Nassau (on Long Island)
# New York (Manhattan)
# Niagara (along L. Ontario)
# Orleans (along L. Ontario)
# Putnam (Hudson Valley)
# Queens (NYC)
# Richmond (Staten Island)
# Rockland (suburb)
# Wayne (along L. Ontario)
# Westchester (just N of NYC, suburb)
# EXTRA COUNTY RUTLAND IS IN VT






################# CALCULATE RATES

# calculate percentages
datw$perc_ct <- 100*(datw$ct.acres / datw$eval.acres)
datw$perc_cc <- 100*(datw$cc.acres / datw$eval.acres)
datw$perc_nt <- 100*(datw$nt.acres / datw$eval.acres)
datw$perc_rt <- 100*(datw$rt.acres / datw$eval.acres)

# remove data from county-years noted above
keep <- NYac %>%
  

datw <- inner_join(datw, NYac[,c(1,2,3,6)]) #  inner_join() only keeps observations from x that have a matching key in y.


# long form again, but leave out acreages, only need %s
datl <- melt(datw[, -c(6:11)], id=c("state", "county", "fips", "year", "crop_name", "perc_eval", "NS"), na.rm=T)
# using na.rm = TRUE because do not have all combinations of crop acres per cover crop or tillage acres, e.g. 
# we have soybean acres in Ulster County, NY in 2015, but we do not have soybean cover crop acres for that 
# county-year. So when we melt, we get a bunch of Value = NA rows.


# what are the average county adoption rates and number of acres evaluated? (are any small counties driving percents up?)
# which counties are hotspots?
# use this to make map of counties see "/code/plot_making/OpTIS_county_heatmap_IL.R"
means_county <- datl %>%
  filter(year>2017) %>%
  group_by(state, county, variable) %>%
  summarize(mean.perc = mean(value), 
            mean.eval = mean(perc_eval)) %>%
  arrange(variable, desc(mean.perc)) %>%
  ungroup() %>%
  group_by(variable) %>%
  mutate(rank = seq_along(variable))

# use this to make table of top 5
means_county_top5 <- means_county %>%
  filter(rank<6)

# > means_county_top5
# # A tibble: 20 × 6
# # Groups:   variable [4]
# state county     variable mean.perc mean.eval  rank
# <chr> <chr>      <fct>        <dbl>     <dbl> <int>
#   1 IL    Douglas    perc_ct       42.8      NA       1
# 2 IL    Coles      perc_ct       42.6      NA       2
# 3 IL    Sangamon   perc_ct       38.8      94.6     3
# 4 IL    Tazewell   perc_ct       35.1      95.8     4
# 5 IL    Christian  perc_ct       33.2      NA       5
# 6 IL    Monroe     perc_cc       28.9      78.4     1
# 7 IL    St. Clair  perc_cc       28.1      86.5     2
# 8 IL    White      perc_cc       27.4      89.7     3
# 9 IL    Hardin     perc_cc       22.9      NA       4
# 10 IL    Randolph   perc_cc       21.9      74.0     5
# 11 IL    Johnson    perc_nt       87.6      NA       1
# 12 IL    Massac     perc_nt       75.4      NA       2
# 13 IL    Williamson perc_nt       74.9      NA       3
# 14 IL    Hardin     perc_nt       74.8      NA       4
# 15 IL    Union      perc_nt       73.8      NA       5
# 16 IL    Marshall   perc_rt       54.2      NA       1
# 17 IL    Stark      perc_rt       54.1      NA       2
# 18 IL    DeKalb     perc_rt       53.9     100.      3
# 19 IL    Ogle       perc_rt       53.5      93.6     4
# 20 IL    Carroll    perc_rt       52.8      91.2     5

# top 5 by region 
means_countyNS <- datl %>%
  filter(year>2017) %>%
  group_by(NS, county, variable) %>%
  summarize(mean.perc = mean(value), 
            mean.eval = mean(perc_eval)) %>%
  arrange(variable, desc(mean.perc)) %>%
  ungroup() %>%
  group_by(NS, variable) %>%
  mutate(rank = seq_along(variable))

# use this to make table of top 5
means_county_top5NS <- means_countyNS %>%
  filter(rank<6) %>%
  mutate(mean.perc.rounded = round(mean.perc, 1)) %>%
  select(!c(mean.eval, mean.perc)) %>%
  arrange(variable, NS, rank)


# data to compare to 2018 IL transect data https://agr.illinois.gov/resources/landwater/illinois-soil-conservation-transect-survey-reports.html 
dat18 <- datl %>%
  filter(year==2018) %>%
  select(county, crop_name, variable, value) %>%
  arrange(variable, crop_name, county)
write.csv(dat18, "data/optis/datny_2018forcomparison.csv", row.names=F)



# write files
write.csv(NYac, "data/optis/datny_eval.csv", row.names=F)  # compares evaluated and harvested acres, no adoption info
write.csv(datw, "data/optis/datny_cleanwide.csv", row.names=F) # wide form with a column for each: perc_ct, perc_cc, etc.
write.csv(datl, "data/optis/datny_cleanlong.csv", row.names=F) # long form with a column for "variable" that gives perc_ct, perc_cc, etc.
write.csv(means_county, "data/optis/means_county_il.csv", row.names=F)  # mean adoption rates 2018-2021
write.csv(means_countyNS, "data/optis/means_county_ilNS.csv", row.names=F)  # mean adoption rates 2018-2021 ranked by North and South IL
write.csv(means_county_top5NS, "data/optis/means_county_iltop5NS.csv", row.names=F) # top 5 counties ranked by mean adoption % 2018-2021 in the N and in the S for each variable
