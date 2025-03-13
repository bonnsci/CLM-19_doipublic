# This script loads the 3 adoption datasets for making Table 2 and 3 and 
# Figs. 1, S2, and S3

# load packages

library(ggplot2)
library(tidyverse)

se <- function(x) sd(x, na.rm=T) / sqrt(length(x))



### load and prep optis data
#### PROPRIETARY COUNTY-LEVEL REGROW DATA NOT SHARED IN THIS REPO
### so this script will not work for external users

optny <- read.csv("data/adoption/datny_cleanlong.csv")
optny$state <- rep("New York", nrow(optny))
optny <- optny[,c(7, 1:3,5,6)]

optil <- read.csv("data/adoption/datil_cleanlong.csv")
optil <- optil[,c(1,2,4,5,8,9)]
optil$state <- rep("Illinois", nrow(optil))

opt <- bind_rows(optny, optil)
rm(optil, optny)
opt$source <- rep("OpTIS", nrow(opt))
opt$year <- ifelse(opt$variable=="perc_cc", opt$year-1, opt$year)   # to (somewhat) align optis crop year with ag census calendar year
# Doing this for cover crops but not for tillage see Table S1.
# i.e., crop year 2018 optis spring tillage, aligns with Census and Transect 2018 spring tillage, etc. ==DO NOT SUBTRACT 1 
# but crop year 2018 cover crop (fall 2017) aligns with Census calendar year fall 2017 planted-cover crop.  ==DO SUBTRACT 1


### load and prep census data
cens <- read.csv("data/adoption/CaRPE CC_NT_RT 2017_2022_NY and IL all counties.csv")
# anyone can download such data using CaRPE Tool https://farmland.org/project/the-carpe-tool/
# came from my copy in /CLM-19 FFAR GHG - General / Results / carpe adoption data

# note CaRPE refers to conventional tillage as intensive tillage, changing it to CT here:
colnames(cens) <- c("year", "state", "county", "perc_cc", "perc_nt", "perc_rt", "perc_ct")
cens$source <- rep("AgCensus", nrow(cens))
cens <- pivot_longer(cens, cols=c(perc_cc, perc_nt, perc_rt, perc_ct), names_to="variable", values_to="value")
# Census data are for all cropland, not by crop like the other data sources
cens$crop_name <- rep("Cropland", nrow(cens))

# fix county names to match optis
cens$county <- ifelse(cens$county == "Du Page", "DuPage",
                      ifelse(cens$county == "De Kalb", "DeKalb",
                             ifelse(cens$county == "De Witt", "DeWitt",
                                    ifelse(cens$county == "La Salle", "LaSalle",
                                           ifelse(cens$county == "Mcdonough", "McDonough",
                                                  ifelse(cens$county == "Mchenry", "McHenry",
                                                         ifelse(cens$county =="Mclean", "McLean",
                                                                ifelse(cens$county == "St Clair", "St. Clair", cens$county))))))))

# tillage transect data
till <- read.csv("data/adoption/Illinois tillage transect survey data 2015-2018_all counties.csv")
# anyone can download these data at https://agr.illinois.gov/resources/landwater/illinois-soil-conservation-transect-survey-reports.html
# came from spreadsheet in /CLM-19 FFAR GHG - General / Results / IL tillage transect data
# as.data.frame(names(till))
till <- filter(till, !County=="Total")
# note the "%" symbol doesn't come through into R but these aren't acres these are %s:
till <- till[,c(1,2,4,9,12,15,18)]
colnames(till) <- c("year", "crop_name", "county", "perc_nt", "perc_mt", "perc_rt", "perc_ct")
till$source <- rep("Transect", nrow(till))
till$state <- rep("Illinois", nrow(till))
till$perc_nt <- as.numeric(till$perc_nt)
till$perc_mt <- as.numeric(till$perc_mt)
till$perc_rt <- as.numeric(till$perc_rt)
till$perc_ct <- as.numeric(till$perc_ct)
# according to the definitions of residue cover, fields categorized as "mulch till" in 
# transect data would correspond to "no till" in OpTIS data
# sum no till and mulch till for new "no till"
till$perc_nt <- till$perc_nt + till$perc_mt
# remove mulch till
till$perc_mt <- NULL
till <- pivot_longer(till, cols=c(perc_nt, perc_rt, perc_ct), names_to="variable", values_to="value")
till$crop_name <- ifelse(till$crop_name=="corn", "Corn", "Soybeans")

till$county <- ifelse(till$county=="JoDaviess", "Jo Daviess",
                      ifelse(till$county== "St Clair", "St. Clair",
                             ifelse(till$county== "StClair", "St. Clair",
                                    ifelse(till$county=="RockIsland", "Rock Island", till$county))))

###  combine data

dat <- full_join(cens, opt) 
dat <- full_join(dat, till)

# rm(cens,opt,till)

dat$costate <- paste0(dat$county, " County, ", dat$state)

# assign region groups
dat$region <- ifelse(dat$state == "New York" & dat$county %in% c("Niagara", "Orleans", "Monroe", "Wayne",
                                                                 "Erie", "Genesee", "Wyoming", "Livingston", "Ontario", "Yates", "Seneca",
                                                                 "Chautauqua", "Cattaraugus", "Allegany", "Steuben", "Schuyler", "Chemung"),
                     "Western NY", 
                     ifelse(dat$state=="Illinois" & dat$county %in% c("Henderson", "Warren", "Knox", "Stark", "Marshall", "Woodford", "Livingston", "Ford", "Iroquois",
                                                                      "Hancock", "McDonough", "Fulton", "Peoria", "Tazewell", "McLean",
                                                                      "Adams", "Schuyler", "Brown", "Cass", "Mason", "Menard", "Logan", "DeWitt", "Piatt", "Champaign", "Vermilion",
                                                                      "Pike", "Scott", "Morgan", "Sangamon", "Macon", 
                                                                      "Calhoun", "Greene", "Jersey", "Macoupin", "Montgomery", "Christian", "Shelby", "Moultrie", "Douglas", "Coles", "Cumberland", "Edgar", "Clark"),
                            "Central IL",
                            ifelse(dat$state=="Illinois" & dat$county %in% c("Madison", "Bond", "Fayette", "Effingham", "Jasper", "Crawford", 
                                                                             "St. Clair", "Clinton", "Marion", "Clay", "Richland", "Lawrence",
                                                                             "Monroe", "Washington", "Jefferson", "Wayne", "Edwards", "Wabash",
                                                                             "Randolph", "Perry", "Franklin", "Hamilton", "White", 
                                                                             "Jackson", "Williamson", "Saline", "Gallatin",
                                                                             "Union", "Johnson", "Pope", "Hardin",
                                                                             "Alexander", "Pulaski", "Massac"),
                                   "Southern IL", "X")))

dat <- dat[dat$region != "X",]






# bring in climate data from Alison Thieme
clm <- read.csv("data/climate data not large/OpTIS_weather_v4/ThreeCountyWeatherOverviewV4.csv")
clm <- clm[,1:13]
# From Alison: # There is also a summary table which lists the county, year,  
# first day of a big freezing event (at least 3-day stretch of average temperatures below zero), ,
# Cumulative_GDD_1001.First.Big.Freeze is the GDD between Oct 1 and the first big 3 day freeze event
# cumulative growing degree days between October 1 and May 1 (4C), 
# cumulative frostdegree days between October 1 and May 1 (4C), 
# cumulative precipitation between October 1 and May 1 (mm),
# cum precip 10/1-11/30
# cum precip 3/1-4/30
# cum precip 10/1-11/1
# cum precip 3/1-4/1
# minimum daily average temperature (C), and the date of that minimum 
# average daily temperature.\

# The 3 locations representing the 3 regions are:
# Central IL (Logan County)
# Southern IL (Jefferson County)
# Western NY (Livingston County)

clm <- clm[clm$County %in% c( "Central IL (Logan County) 40.102 N, -89.357 W" ,
                              "Southern IL (Jefferson County) 38.292 N, -88.778 W",
                              "Western NY (Livingston County) 42.600 N, -77.797 W"),]

clm$region <- ifelse(clm$County=="Western NY (Livingston County) 42.600 N, -77.797 W", "Western NY",
                     ifelse(clm$County== "Central IL (Logan County) 40.102 N, -89.357 W", "Central IL", "Southern IL"))

clm$year <- as.numeric(str_sub(clm$Year, 6,10))



# add the fall and spring precip from same crop year but different calendar years
clm$cum_precip_mm_1001.1130.0301.0430 <-  clm$Cumulative_precip_mm_1001.1130 + clm$Cumulative_precip_mm_0301.0430



# to plot a secondary y-axis in ggplot, we have to scale the second variable to the first one.
# for cover crops we will be adding GDD data, what is the max?
gddmax <- max(clm$Cumulative_GDD_1001.First.Big.Freeze)
# for tillage we will be adding precip data, what is the max precip?
precipmax <- max(na.omit(clm$cum_precip_mm_1001.1130.0301.0430))
# for cover crop adoption data what is the max?
maxtest <- dat %>% dplyr::group_by(variable) %>%
  dplyr::summarize(max=max(na.omit(value)))

# scale_cc <- gddmax/ (maxtest %>% dplyr::filter(variable=="perc_cc") %>% pull)
scale_nt <- precipmax/ (maxtest %>% dplyr::filter(variable=="perc_nt") %>% pull)
scale_rt <- precipmax/ (maxtest %>% dplyr::filter(variable=="perc_rt") %>% pull)
scale_ct <- precipmax/ (maxtest %>% dplyr::filter(variable=="perc_ct") %>% pull)

# The tillage scales are very similar so let's get the mean and use that for those 3
scale_till <- mean(c(scale_nt, scale_rt, scale_ct))


# scale clm data by adoption data
# plotted the cc data and the max on the y-axis is 25 so using that instead:
clm$cum_gdd_scaled <- clm$Cumulative_GDD_1001.First.Big.Freeze/25
clm$cum_precip_tillscaled <- clm$cum_precip_mm_1001.1130.0301.0430/scale_till


# clm needs to have "variable" to facet like in the plot below
clmcc <- clm %>% select(year, region, cum_gdd_scaled) 
clmcc$variable <- rep("perc_cc", nrow(clmcc))
colnames(clmcc)[3] <- "clm.dat"
clmnt <- clm %>% select(year, region, cum_precip_tillscaled) 
clmnt$variable <- rep("perc_nt", nrow(clmnt))
colnames(clmnt)[3] <- "clm.dat"
clmrt <- clm %>% select(year, region, cum_precip_tillscaled) 
clmrt$variable <- rep("perc_rt", nrow(clmrt))
colnames(clmrt)[3] <- "clm.dat"
clmct <- clm %>% select(year, region, cum_precip_tillscaled) 
clmct$variable <- rep("perc_ct", nrow(clmct))
colnames(clmct)[3] <- "clm.dat"

clm2 <- bind_rows(clmcc, clmnt, clmrt, clmct)
rm(clmcc, clmnt, clmrt, clmct, maxtest, scale_nt, scale_rt, scale_ct, precipmax)