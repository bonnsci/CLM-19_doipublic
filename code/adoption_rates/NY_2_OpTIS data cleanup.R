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
# 
# 
# # ### IMPROVEMENT: add harvested forage crop acres 2015-2021 by county from NASS survey data
# # ### only need to do once
# # # prep data
# # # note alfalfa ("HAYLAGE") and wheat data are not by county.
# df <- list.files(path="data/NASS harvested acres", pattern=" NY ", full.names=T)
# df <- df[1:2]  # note alfalfa ("HAYLAGE") and wheat data are not by county, so not helpful for assessing acreage evaluated as % of county crop acres.
# df <- lapply(df, function(i){
#         i <- read.csv(i)
#         i <- i %>%
#           select("Year", "County", "Data.Item", "Value") %>%
#           filter(!County %in% c("OTHER COUNTIES", "OTHER (COMBINED) COUNTIES")) %>%
#           rename("year"="Year", "county"="County", "crop_name"="Data.Item", "harv_ac"="Value")
#         i$harv_ac <- as.numeric(gsub(",", "", i$harv_ac)) # remove commas from values
#         i$crop_name <- gsub("CORN, GRAIN - ACRES HARVESTED", "Corn grain", i$crop_name)
#         i$crop_name <- gsub("CORN, SILAGE - ACRES HARVESTED", "Corn silage", i$crop_name)
#         i$crop_name <- gsub("SOYBEANS - ACRES HARVESTED", "Soybeans", i$crop_name)
#         i$county <- tools::toTitleCase(tolower(i$county)) # get county names ready to merge with OpTIS spellings, which are correct
#         i <- arrange(i, county)
#       })
# 
# # unique(df[[1]]$crop_name)
# # unique(df[[1]]$County)  ### 14 of 62 acres
# # [1] "CAYUGA"      "CORTLAND"    "ONONDAGA"    "TOMPKINS"    "CATTARAUGUS" "CHAUTAUQUA"  "ERIE"       
# # [8] "GENESEE"     "LIVINGSTON"  "ONTARIO"     "WAYNE"       "WYOMING"     "YATES"       "ALLEGANY"   
# # unique(df[[2]]$County)  ### 15 of 62 acres
# # [1] "CAYUGA"      "CORTLAND"    "ONONDAGA"    "TOMPKINS"    "ALLEGANY"    "CATTARAUGUS" "CHAUTAUQUA" 
# # [8] "STEUBEN"     "ERIE"        "GENESEE"     "LIVINGSTON"  "ONTARIO"     "WAYNE"       "WYOMING"    
# # [15] "YATES"      
# 
# acres <- bind_rows(df) # combine dfs into one df
# rm(df)
# 
# write.csv(acres, "data/NASS harvested acres/NY_harvested_forages2015-2021.csv", row.names=F)



# read in NASS data prepped above
acres <- read.csv("data/NASS harvested acres/NY_harvested_forages2015-2021.csv")

acres <- acres %>%
  mutate(crop_name2=gsub("Corn grain", "Corn",
                         gsub("Corn silage", "Corn", x=crop_name))) %>%  # corn silage and corn grain indistinguishable to OpTIS
  group_by(county, year, crop_name2) %>%
  mutate(harv_ac2 = sum(harv_ac)) %>%  # sum corn silage and corn grain acres together
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
min(na.omit(NYac$perc_eval)) # 7.58 not good was hoping to have everything >50%
hist(NYac$perc_eval)   # have some <50, most are between 50-150, but still some as high as 700.
NYac[NYac$perc_eval>125,] %>% 
  arrange(crop_name, county, year)

# crop_name      county year eval.acres harv_ac perc_eval
# 1       Corn      Albany 2021  1612.0621    1230    131.06
# 2       Corn    Allegany 2015 12077.5871    7000    172.54
# 3       Corn      Broome 2015  2922.8562    2300    127.08
# 4       Corn      Broome 2019  4460.2804    3300    135.16
# 5       Corn Cattaraugus 2019 21313.7871   14700    144.99
# 6       Corn      Cayuga 2018 75529.5138   43900    172.05
# 7       Corn      Cayuga 2019 71911.1163   44000    163.43
# 8       Corn  Chautauqua 2019 23202.3139   14500    160.02
# 9       Corn     Chemung 2019  3678.3335    1700    216.37
# 10      Corn    Chenango 2019 10327.2264    7500    137.70
# 11      Corn     Clinton 2019 18180.4666   14000    129.86
# 12      Corn     Clinton 2020 17416.5267   12400    140.46
# 13      Corn     Clinton 2021 16368.4638   12600    129.91
# 14      Corn    Columbia 2015  7593.9455    4700    161.57
# 15      Corn    Columbia 2018 10004.4520    5100    196.17
# 16      Corn    Columbia 2019 10629.0008    6200    171.44
# 17      Corn    Dutchess 2018  4559.0267    2300    198.22
# 18      Corn    Franklin 2019 18272.0547   13900    131.45
# 19      Corn     Genesee 2017 46976.1520   28600    164.25
# 20      Corn     Genesee 2021 44351.9992   30900    143.53
# 21      Corn      Greene 2015   755.2576     200    377.63
# 22      Corn      Greene 2017   837.1254     400    209.28
# 23      Corn      Greene 2021  1153.5394     380    303.56
# 24      Corn    Herkimer 2019 14168.9498   10000    141.69
# 25      Corn       Lewis 2015 15459.5868    3200    483.11
# 26      Corn  Montgomery 2019 14515.2210   11000    131.96
# 27      Corn     Ontario 2018 53993.4843   42200    127.95
# 28      Corn      Orange 2018  5327.4984    2300    231.63
# 29      Corn      Orange 2019  4340.5980    2800    155.02
# 30      Corn      Oswego 2018 10824.0694    7100    152.45
# 31      Corn Schenectady 2016   415.4307     300    138.48
# 32      Corn   Schoharie 2018  6065.3197    4300    141.05
# 33      Corn    Schuyler 2015  5752.7054    2800    205.45
# 34      Corn    Schuyler 2017  7542.8171    4500    167.62
# 35      Corn    Schuyler 2018  8605.5639    6500    132.39
# 36      Corn    Schuyler 2021  7243.9538    4350    166.53
# 37      Corn      Seneca 2015 31407.8924   23000    136.56
# 38      Corn      Seneca 2016 31243.7357   21300    146.68
# 39      Corn      Seneca 2017 28115.3858   16500    170.40
# 40      Corn      Seneca 2018 35671.1007   23600    151.15
# 41      Corn      Seneca 2019 31734.3887   19700    161.09
# 42      Corn      Seneca 2020 31840.1764   24440    130.28
# 43      Corn     Steuben 2019 45703.5689   30400    150.34
# 44      Corn     Suffolk 2015  1007.9368     600    167.99
# 45      Corn     Suffolk 2020   614.9927     450    136.67
# 46      Corn    Tompkins 2019 13807.7483    7200    191.77
# 47      Corn      Ulster 2015  1298.4414     300    432.81
# 48      Corn      Ulster 2019  1976.6383    1200    164.72
# 49      Corn      Ulster 2020  1512.7346     690    219.24
# 50      Corn       Yates 2015 34032.0136   15900    214.04
# 51      Corn       Yates 2016 34980.5957    5000    699.61  #
# 52      Corn       Yates 2019 38428.4616    9500    404.51  #
# 53      Corn       Yates 2020 34299.1955    8470    404.95  #
# 54      Corn       Yates 2021 32763.7346   20570    159.28
# 55  Soybeans      Cayuga 2017 34558.3142   27400    126.13
# 56  Soybeans      Orange 2019  2591.7302    1900    136.41
# 57  Soybeans    Schuyler 2021  1242.3919     890    139.59
# 58  Soybeans      Seneca 2015 28999.9993   23000    126.09
# 59  Soybeans      Seneca 2016 31062.4524   23800    130.51
# 60  Soybeans      Seneca 2017 27137.0549   20000    135.69
# 61  Soybeans      Seneca 2018 32920.3554   24000    137.17
# 62  Soybeans      Seneca 2019 24614.2709   17200    143.11
# 63  Soybeans      Seneca 2020 33707.6210   25400    132.71
# 64  Soybeans      Seneca 2021 31493.7723   24600    128.02
# 65  Soybeans       Yates 2015  6293.4571    4500    139.85
# 66  Soybeans       Yates 2016  7721.8884    5740    134.53
# 67  Soybeans       Yates 2017  5697.4615    3700    153.99
# 68  Soybeans       Yates 2018 11727.6325    4700    249.52
# 69  Soybeans       Yates 2019  6979.4865    4300    162.31
# 70  Soybeans       Yates 2020 10679.4658    4970    214.88
# 71  Soybeans       Yates 2021  9759.1910    4430    220.30

# for now it seems we should ignore these data where perc_eval >125%

NYac <- NYac %>%
  filter(between(perc_eval, 50, 125))  # takes 450 obs to 364

hist(NYac$perc_eval)  # better

# "Described the total acres evaluated and how the % was calculated." 
# % CC, NT, RT was calculated by dividing the number of acres with those practices 
# detected by the number of cropland acres evaluated by OpTIS per county-year

# what proportion of cropland acres did OpTIS evaluate per county?

se <- function(x) {
  sd(x)/sqrt(length(x))
}

mean(na.omit(NYac$perc_eval)) # 85
se(na.omit(NYac$perc_eval)) # 1.03
min(na.omit(NYac$perc_eval)) # 50.05
max(na.omit(NYac$perc_eval)) # 124.73

length(unique(NYac$county))  #43

# number of counties represented in NY data
datw %>%
  # filter(state %in% "NY") %>%
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


datw2 <- inner_join(datw, NYac[,c(1,2,3,6)]) #  inner_join() only keeps observations from x that have a matching key in y.
# datw has 1186 obs, while datw2 only has 364
# datw2 only has corn and soybeans

# long form again, but leave out acreages, only need %s
datl <- melt(datw2, id=c("county","year", "crop_name", "perc_eval"), na.rm=T)  # [, -c(4:8)]
# using na.rm = TRUE because do not have all combinations of crop acres per cover crop or tillage acres, e.g. 
# we have soybean acres in Ulster County, NY in 2015, but we do not have soybean cover crop acres for that 
# county-year. So when we melt, we get a bunch of Value = NA rows.


# what are the average county adoption rates and number of acres evaluated? (are any small counties driving percents up?)
# which counties are hotspots?
# use this to make map of counties see "/code/plot_making/OpTIS_county_heatmap_IL.R"
means_county <- datl %>%
  filter(year>2017) %>%
  group_by(county, variable) %>%   # means across corn and soybean acres (not grouping by crop)
  summarize(mean.perc = mean(value), 
            mean.eval = mean(perc_eval)) %>%
  arrange(variable, desc(mean.perc)) %>%
  ungroup() %>%
  group_by(variable) %>%
  mutate(rank = seq_along(variable))

# use this to make table of top 5
means_county_top5 <- means_county %>%
  filter(rank<6)

means_county_top5
# # A tibble: 20 × 5
# # Groups:   variable [4]
# county     variable mean.perc mean.eval  rank
# <chr>      <fct>        <dbl>     <dbl> <int>
#   1 Broome     perc_ct       26.3      63.8     1
# 2 Chemung    perc_ct       25.1      66.5     2
# 3 Steuben    perc_ct       19.8      92.0     3
# 4 Sullivan   perc_ct       19.0      61.8     4
# 5 Chautauqua perc_ct       17.3      91.3     5

# 6 Suffolk    perc_cc       44.4     116.      1
# 7 Albany     perc_cc       40.0     125.      2
# 8 Delaware   perc_cc       26.9      69.3     3
# 9 Washington perc_cc       21.3      71.2     4
# 10 Rensselaer perc_cc       18.7      75.9     5

# 11 Seneca     perc_nt       43.7     125.      1
# 12 Oswego     perc_nt       43.0     104.      2
# 13 Schuyler   perc_nt       42.2      84.6     3
# 14 Cayuga     perc_nt       42.1     113.      4
# 15 Onondaga   perc_nt       40.0     112.      5

# 16 Herkimer   perc_rt       71.5      79.2     1
# 17 Jefferson  perc_rt       66.0      85.2     2
# 18 Sullivan   perc_rt       64.8      61.8     3
# 19 Oneida     perc_rt       64.8      79.0     4
# 20 Madison    perc_rt       63.7      77.5     5


# write files
write.csv(NYac, "data/optis/datny_eval.csv", row.names=F)  # compares evaluated and harvested acres, no adoption info
write.csv(datw, "data/optis/datny_cleanwide.csv", row.names=F) # wide form with a column for each: perc_ct, perc_cc, etc.
write.csv(datl, "data/optis/datny_cleanlong.csv", row.names=F) # long form with a column for "variable" that gives perc_ct, perc_cc, etc.
write.csv(means_county, "data/optis/means_county_ny.csv", row.names=F)  # mean adoption rates 2018-2021
write.csv(means_county_top5, "data/optis/means_county_top5ny.csv", row.names=F)  # mean adoption rates 2018-2021 top 5 counties

