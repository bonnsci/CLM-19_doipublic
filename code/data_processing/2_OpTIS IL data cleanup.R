# OpTIS adoption rate information for Illinois aggregated by counties 2015-2021.
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

# Bonnie McGill - started June 2023



# load packages
library(reshape2) # for dcast()
library(ggplot2)
library(tidyverse)


# to update data from google drive, run "download data.R"




# load the data
datil <- read.csv("data/optis/datil.csv")
# datny <- read.csv("data/optis/datny.csv")

# # combine the data into one df
# dat <- rbind(datil, datny)
# # clean up
# rm(datil)
# rm(datny)


datil <- datil[,c(-2, -10)]
colnames(datil)[1:3] <- c("state", "county", "fips")

sum(is.na(datil$kpi_value))

# fix one county spelling
datil$county <- ifelse(datil$county %in% "De Witt County", "DeWitt County", datil$county)

########## remove COOK County, any others? 7/10/23

# unique(dat$kpi_name)
# cast data into wide form for separate columns by kpi_name and kpi_value
datw <- dcast(datil, ...~kpi_name, value.var="kpi_value")
colnames(datw)[c(2,3,7:11)] <- c("county", "fips", "eval.acres", "ct.acres", "cc.acres", "nt.acres", "rt.acres") 
# $acres is acres analyzed by OpTIS

# add factor for northern or southern illinois
# according to map on https://en.wikipedia.org/wiki/Southern_Illinois#/media/File:Illinois_Counties_-_Little_Egypt.png
# Kris R. agreed with these counties as southern Illinois (including pink counties)
datw$NS <- ifelse(datw$county %in% c("Madison County", "Bond County", "Fayette County", "Effingham County", 
                                     "Jasper County", "Cumberland County", "Clark County", "Crawford County",
                                     "Monroe County", "St. Clair County", "Clinton County", "Washington County",
                                     "Marion County", "Jefferson County", "Clay County", "Wayne County", "Richland County",
                                     "Lawrence County", "Edwards County", "Wabash County", "Randolph County", "Perry County",
                                     "Franklin County", "Hamilton County", "White County", "Jackson County", "Williamson County",
                                     "Saline County", "Gallatin County", "Union County", "Johnson County", "Pope County",
                                     "Hardin County", "Alexander County", "Pulaski County", "Massac County"), 
                  "S", "N")

datw$county <- str_remove(datw$county, " County")

######### DATA CLEANING
# compare acres evaluated to harvested corn grain and soybean acres from NASS annual surveys
ILac <- aggregate(eval.acres ~ crop_name + county + year, dat=datw, FUN="sum") %>%  # first sum acres across crops per year
  arrange(year,county)



# #  SKIP add harvested cropland acres by IL county from 2017 ag census
# acres <- read.csv("C:/Users/BonnieMcGillPhD/American Farmland Trust/CLM-19 FFAR GHG - Documents/General/Results/NASS Quick Stats IL harvested crop acres by county 2017.csv", header=T)

### IMPROVEMENT: add harvested corn grain and soybean acres 2015-2021 by county from NASS survey data
### only need to do once 
# # prep corn and soy data
df <- list.files(path="data/NASS harvested acres", pattern=" IL ", full.names=T)
df <- lapply(df, function(i){
  i <- read.csv(i)
  i <- i %>%
    select("Year", "County", "Commodity", "Value") %>%
    filter(!County %in% c("OTHER COUNTIES", "OTHER (COMBINED) COUNTIES")) %>%
    rename("year"="Year", "county"="County", "crop_name"="Commodity", "harv_ac"="Value") 
  i$harv_ac <- as.numeric(gsub(",", "", i$harv_ac)) # remove commas from values
  i$crop_name <- gsub("CORN", "Corn", i$crop_name)
  i$crop_name <- gsub("SOYBEANS", "Soybeans", i$crop_name)
  i$county <- tools::toTitleCase(tolower(i$county)) # get county names ready to merge with OpTIS spellings, which are correct
  i$county <- ifelse(i$county %in% "De Witt", "DeWitt",
                             ifelse(i$county %in% "De Kalb", "DeKalb",
                                    ifelse(i$county %in% "Du Page", "DuPage",
                                           ifelse(i$county %in% "La Salle", "LaSalle",
                                                  ifelse(i$county %in% "Mcdonough", "McDonough",
                                                         ifelse(i$county %in% "Mchenry", "McHenry",
                                                                ifelse(i$county %in% "Mclean", "McLean",
                                                                       ifelse(i$county %in% "St Clair", "St. Clair",
                                                                              ifelse(i$county %in% "will", "Will", i$county)))))))))
  i <- arrange(i, county)
})

acres <- bind_rows(df) # combine corn and soybean dfs into one df
rm(df)

write.csv(acres, "data/IL_harvested_cornsoy2015-2021.csv", row.names=F)



# read in NASS data prepped above
acres <- read.csv("data/IL_harvested_cornsoy2015-2021.csv")
ILac <- left_join(ILac, acres)
ILac$perc_eval <- round((100*ILac$eval.acres / ILac$harv_ac), digits=2)
# NAs created where we have OpTIS data but not harvest data

# look for outliers, remember the Ag Census is not necessarily the truth either
min(na.omit(ILac$perc_eval)) # 53.2
hist(ILac$perc_eval)

# number of counties represented in IL data
datw %>%
  # filter(state %in% "IL") %>%
  distinct(county) %>%
  n_distinct()
# 101 # 102 counties in IL, Lake County is missing, it's a tiny county between
# Chicago and Wisconsin border, basically still Chicago outlying area. No ag.
# surprised cook county (Chicago) is even in dataset!


# "Described the total acres evaluated and how the % was calculated." 
# % CC, NT, RT was calculated by dividing the number of acres with those practices 
# detected by the number of cropland acres evaluated by OpTIS per county-year

# what proportion of cropland acres did OpTIS evaluate per county?


mean(na.omit(ILac$perc_eval)) # 91.53
min(na.omit(ILac$perc_eval)) # 53.2 - one county had >50% in 2017, but <50% in one year.
max(na.omit(ILac$perc_eval)) # 113.4 - 


################# CALCULATE RATES

# calculate percentages
datw$perc_ct <- 100*(datw$ct.acres / datw$eval.acres)
datw$perc_cc <- 100*(datw$cc.acres / datw$eval.acres)
datw$perc_nt <- 100*(datw$nt.acres / datw$eval.acres)
datw$perc_rt <- 100*(datw$rt.acres / datw$eval.acres)

datw <- inner_join(datw, ILac[,c(1,2,3,6)]) #  inner_join() only keeps observations from x that have a matching key in y.


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
write.csv(dat18, "data/optis/datil_2018forcomparison.csv", row.names=F)



# write files
write.csv(ILac, "data/optis/datil_eval.csv", row.names=F)  # compares evaluated and harvested acres, no adoption info
write.csv(datw, "data/optis/datil_cleanwide.csv", row.names=F) # wide form with a column for each: perc_ct, perc_cc, etc.
write.csv(datl, "data/optis/datil_cleanlong.csv", row.names=F) # long form with a column for "variable" that gives perc_ct, perc_cc, etc.
write.csv(means_county, "data/optis/means_county_il.csv", row.names=F)  # mean adoption rates 2018-2021
write.csv(means_countyNS, "data/optis/means_county_ilNS.csv", row.names=F)  # mean adoption rates 2018-2021 ranked by North and South IL
write.csv(means_county_top5NS, "data/optis/means_county_iltop5NS.csv", row.names=F) # top 5 counties ranked by mean adoption % 2018-2021 in the N and in the S for each variable
