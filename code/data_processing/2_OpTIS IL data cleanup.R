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
library(dplyr)


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


# unique(dat$kpi_name)
# cast data into wide form for separate columns by kpi_name and kpi_value
datw <- dcast(datil, ...~kpi_name, value.var="kpi_value")
colnames(datw)[c(2,3,7:11)] <- c("county", "fips", "eval.acres", "ct.acres", "cc.acres", "nt.acres", "rt.acres") 
# $acres is acres analyzed by OpTIS




######### DATA CLEANING
# compare acres evaluated to harvested cropland acres from Ag Census
ILac <- aggregate(eval.acres ~ county + year, dat=datw, FUN="sum") %>%  # first sum acres across crops per year
  arrange(year,county)

# #  add harvested cropland acres by IL county from 2017 ag census
# acres <- read.csv("C:/Users/BonnieMcGillPhD/American Farmland Trust/CLM-19 FFAR GHG - Documents/General/Results/NASS Quick Stats IL harvested crop acres by county 2017.csv", header=T)
# acres <- acres[,c("County", "Value")]
# colnames(acres) <- c("county", "harv_ac")
# acres$harv_ac <- gsub(',', '', acres$harv_ac)
# acres$harv_ac <- as.numeric(acres$harv_ac)
# # make county names in acres match OpTIS data
# acres$county <- tolower(acres$county)
# acres$county <- tools::toTitleCase(acres$county)
# acres$county <- paste0(acres$county, " County")
# # clean up county names to match OpTIS data (OpTIS spellings are correct)
# acres$county <- ifelse(acres$county %in% "De Witt County", "DeWitt County",
#                        ifelse(acres$county %in% "De Kalb County", "DeKalb County",
#                               ifelse(acres$county %in% "Du Page County", "DuPage County",
#                                      ifelse(acres$county %in% "La Salle County", "LaSalle County",
#                                             ifelse(acres$county %in% "Mcdonough County", "McDonough County",
#                                                    ifelse(acres$county %in% "Mchenry County", "McHenry County",
#                                                           ifelse(acres$county %in% "Mclean County", "McLean County",
#                                                                  ifelse(acres$county %in% "St Clair County", "St. Clair County",
#                                                                         ifelse(acres$county %in% "will County", "Will County", acres$county)))))))))
# acres <- arrange(acres, county)
# write.csv(acres, "data/IL_harvested_cropland_2017census.csv", row.names=F)
acres <- read.csv("data/IL_harvested_cropland_2017census.csv")
ILac <- left_join(ILac, acres)
ILac$perc_eval <- round((100*ILac$eval.acres / ILac$harv_ac), digits=2)

# look for outliers, remember the Ag Census is not necessarily the truth either
min(ILac$perc_eval)
hist(ILac$perc_eval)
small <- ILac[ILac$perc_eval < 50,] %>%
  arrange(county)
# small
#     county           year eval.acres   harv_ac    perc_eval
# 1   Alexander County 2019  14541.616   38737      37.5
# 2      Hardin County 2015   5234.984   13421      39.0
# 3      Hardin County 2016   5301.364   13421      39.5
# 4      Hardin County 2017   5551.443   13421      41.4
# 5      Hardin County 2018   5665.805   13421      42.2
# 6      Hardin County 2019   5009.524   13421      37.3
# 7      Hardin County 2020   5837.969   13421      43.5
# 8      Hardin County 2021   5711.928   13421      42.6
# 9     Jackson County 2019  73051.877  157102      46.5
# 10 Jo Daviess County 2015  88683.906  183158      48.4
# 11 Jo Daviess County 2016  88943.708  183158      48.6
# 12 Jo Daviess County 2017  91325.322  183158      49.9
# 13    Johnson County 2019  18442.644   37375      49.3
# 14      Union County 2015  34804.867   84872      41.0
# 15      Union County 2016  36957.190   84872      43.5
# 16      Union County 2017  37166.939   84872      43.8
# 17      Union County 2018  36717.970   84872      43.3
# 18      Union County 2019  22960.301   84872      27.1
# 19      Union County 2020  35842.927   84872      42.2
# 20      Union County 2021  37441.373   84872      44.1
 # Hardin Co. borders Kentucky and is mostly forested. In 2017 Census it was 13,421 acres
   # in 2012 census it was 9,318. OpTIS is evaluating about 5500 ac per year, which is only
   # about 40% of the county's cropland, each year it is low. Why is it low every year?
   # I would expect it to be low like some years, when its cloudy or something. Is it because
   # so much forest intermixed, it is hard to discern forest and fields?
   ###### exclude Hardin County all years?
   ###### Hardin Co. is fourth for highest %NT and %CC in state (should it be??)
   ###### should we eliminate counties with < X% evaluated acres?? 
   ###### what is the lower 10th percentile in terms of harvested acres?
   ###### Hardin in lower tenth and 5th percentile, gets excluded

pcutoff <- quantile(acres$harv_ac, probs=0.05)
#  37443.1 acres

 # Union Co. also shows up every year around 40% evaluated. In the ag census it has
   # 84,872 ac in 2017, and 59,743 acres in 2012, quite a fluctuation. OpTIS is evaluating about 
   # 37k acres. Union Co. is also in southern IL, bordering MO, and has lots of forest
   # land intermixed with ag fields.
   ####### (Union Co. is 2nd highest %NT county in the state (once we exclude lower 10% counties by acre)--should it be?)
   ####### Union Co. is excluded when we remove counties with <50% harvested acres evaluated in 2017.
 # Jo Daviess County is the NE corner of the state, bordering IA, WI. It is ag with pockets
   # of forest mixed in. In 2017 Ag Census says it had 183k acres and in 2012 172k. OpTIS is evaluating
   # about 89k in 2015 and 2016, very nearly 50%, in following years it gets up to 90k. 
 # Alexander Co. in 2019 is at 38% evaluated (14,500 ac). In the ag census in 2017 it was 38k ac
   # harvested, and 2012 it was 48k ac harvested.This is a small county in the very southern tip of IL
   # with a big patch of forest, and farm fields. Could be that not much of the county fields grow corn?
   # Ag census corn acres are 5,211 in 2017, 11,833 in 2012. 2017 soybeans were 31,000 ac, 32k in 2012.
   ### excluded with lower 10% cut
 # Jackson County in 2019 also low just that year but nearly half (46%)
 # Johnson County in 2019 also low just that year but nearly half (49%)


big <- ILac[ILac$perc_eval > 120,]
 # DuPage - looking at the Ag Census data it looks like the harvested cropland acres for DuPage county
   # were underestimated/reported in 2017. OpTIS evaluates about 4000 ac every year
   # and the 2012 census reported 5,643. (not 1,272 ac as in 2017). So DuPage is OK.
   # As you can tell by the acreage, DuPage is not a big farming county, it is mostly Chicago suburbs.
   ### excluded when cut lower 10th and 5th percentile
 # Putnam county in Census was 41,934 in 2017 and 46,212 in 2012. OpTIS is evaluating
   # about 54,000 ac.Putnam Co. altogether is about 110,000 ac. Roughly half of which is 
   # in ag, looking at google maps satellite. So 41k, 46k, and 54k, are all plausible.
   ### excluded when cut lower 10th percentile, not with 5th

# number of counties represented in IL data
# datw %>%
#   filter(state %in% "IL") %>%
#   distinct(county) %>%
#   n_distinct()
# 101 # 102 counties in IL, Lake County is missing, it's a tiny county between
# Chicago and Wisconsin border, basically still Chicago outlying area. No ag.
# surprised cook county (Chicago) is even in dataset!

# IGNORE
# remove lowest 5 percent of counties based on acreage (keep upper 90%)
# ILac_up95 <- ILac[ILac$harv_ac > pcutoff,]
# remove counties with < 50% acres evaluated

# first remove counties with < 50% in 2017 (corresponds to year of harvested cropland data)
ILac_ev50 <- ILac %>%
                filter(year==2017, perc_eval>50)
# then join these counties with ILac_up95
ILac_new <- inner_join(ILac, ILac_ev50[,c(1,4)])



# # counties excluded:

# IGNORE
# ILac_lo10 <- ILac[ILac$harv_ac <= pcutoff,] %>%
              # filter(year== 2015)
# > ILac_lo10
#    county        year eval.acres harv_ac    perc_eval
# 1    Cook County 2015   7761.593   10095      76.9
# 2  DuPage County 2015   3923.015    1272     308.4
# 3  Hardin County 2015   5234.984   13421      39.0
# 4 Johnson County 2015  19141.045   37375      51.2
# 5    Pope County 2015  17526.383   19823      88.4

              
ILac_less50 <- ILac[ILac$perc_eval <=50 & ILac$year==2017,]
# > ILac_less50
# county year eval.acres harv_ac perc_eval
# 237     Hardin County 2017   5551.443   13421      41.4
# 245 Jo Daviess County 2017  91325.322  183158      49.9
# 292      Union County 2017  37166.939   84872      43.8
# Lake county not in dataset to begin with
# 102 counties in IL - 4 = 98

# # IGNORE
# # Alexander, DuPage, Hardin, Johnson, Putnam now all excluded
# # Union still in, barely makes the cutoff.
# # ILac_lo10
# #    county            year eval.acres  harv_ac    perc_eval
# # 1   Alexander County 2015  42645.819   38737     110.1 (if use 5th percentile cutoff this is 4th in state for NT)
# # 2       Brown County 2015  59885.648   69625      86.0
# # 3     Calhoun County 2015  30301.075   54453      55.6
# # 4        Cook County 2015   7761.593   10095      76.9
# # 5      DuPage County 2015   3923.015    1272     308.4
# # 6      Hardin County 2015   5234.984   13421      39.0
# # 7     Johnson County 2015  19141.045   37375      51.2
# # 8        Pope County 2015  17526.383   19823      88.4
# # 9      Putnam County 2015  54470.438   41934     129.9
# # 10 Williamson County 2015  40985.528   61440      66.7 (if use 5th percentile cutoff this is 2nd in state for NT)
# rm(ILac_lo10)



# "Described the total acres evaluated and how the % was calculated." 
# % CC, NT, RT was calculated by dividing the number of acres with those practices 
# detected by the number of cropland acres evaluated by OpTIS per county-year

# what proportion of cropland acres did OpTIS evaluate per county?
# counties in the lowest 5% of cropland acres (according to ag census 2017) were excluded
# Counties with <50% of cropland acres evaluated in 2017 were excluded
ILac_new <- ILac_new %>%  
  arrange(perc_eval)
hist(ILac_new$perc_eval, breaks=15)
mean(ILac_new$perc_eval) # 87.7
min(ILac_new$perc_eval) # 37.5 - one county had >50% in 2017, but <50% in one year.
max(ILac_new$perc_eval) #314.7 - cropland acres reported in the census change from year to year, so it is
# possible that the acres evaluated were > number of acres reported in 2017, or that the number of acres reported
# differs from what the satellites detect.
# On average, OpTIS evaluated 86% of a county's harvested cropland acres (for those counties not excluded). 


# Explore top 5 for CC, NT, RT to see if small counties show up / see if plausible:
################# CALCULATE RATES

# calculate percentages
datw$perc_ct <- 100*(datw$ct.acres / datw$eval.acres)
datw$perc_cc <- 100*(datw$cc.acres / datw$eval.acres)
datw$perc_nt <- 100*(datw$nt.acres / datw$eval.acres)
datw$perc_rt <- 100*(datw$rt.acres / datw$eval.acres)

datw <- inner_join(datw, ILac_new[,c(1,2,5)]) #  inner_join() only keeps observations from x that have a matching key in y.


# long form again, but leave out acreages, only need %s
datl <- melt(datw[, -c(6:11)], id=c("state", "county", "fips", "year", "crop_name", "perc_eval"), na.rm=T)
# using na.rm = TRUE because do not have all combinations of crop acres per cover crop or tillage acres, e.g. 
# we have soybean acres in Ulster County, NY in 2015, but we do not have soybean cover crop acres for that 
# county-year. So when we melt, we get a bunch of Value = NA rows.

# (ignore alfalfa and winter wheat acre for cover crops)

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
# state county            variable mean.perc mean.eval  rank
# <chr> <chr>             <fct>        <dbl>     <dbl> <int>
#   1 IL    Douglas County    perc_ct       42.8      96.7     1
# 2 IL    Coles County      perc_ct       42.6     107.      2
# 3 IL    Sangamon County   perc_ct       38.8      74.3     3
# 4 IL    Tazewell County   perc_ct       35.1      92.8     4
# 5 IL    Christian County  perc_ct       33.2      95.9     5
# 6 IL    Monroe County     perc_cc       28.9      64.6     1
# 7 IL    St. Clair County  perc_cc       28.1      80.6     2
# 8 IL    White County      perc_cc       27.4      75.4     3
# 9 IL    Randolph County   perc_cc       21.9      59.3     4
# 10 IL    Edwards County    perc_cc       20.8      80.1     5
# 11 IL    Johnson County    perc_nt       87.6      56.2     1
# 12 IL    Massac County     perc_nt       75.4      58.6     2
# 13 IL    Williamson County perc_nt       74.9      70.7     3
# 14 IL    Pope County       perc_nt       72.6      95.5     4
# 15 IL    Pulaski County    perc_nt       71.1      65.3     5
# 16 IL    Marshall County   perc_rt       54.2      93.7     1
# 17 IL    Stark County      perc_rt       54.1      86.5     2
# 18 IL    DeKalb County     perc_rt       53.9      85.8     3
# 19 IL    Ogle County       perc_rt       53.5      97.0     4
# 20 IL    Carroll County    perc_rt       52.8      81.0     5





# write files
write.csv(ILac_new, "data/optis/datil_eval.csv", row.names=F)  # compares evaluated and harvested acres, no adoption info
write.csv(datw, "data/optis/datil_cleanwide.csv", row.names=F) # wide form with a column for each: perc_ct, perc_cc, etc.
write.csv(datl, "data/optis/datil_cleanlong.csv", row.names=F) # long form with a column for "variable" that gives perc_ct, perc_cc, etc.
write.csv(means_county, "data/optis/means_county_il.csv", row.names=F)  # mean adoption rates 2018-2021

