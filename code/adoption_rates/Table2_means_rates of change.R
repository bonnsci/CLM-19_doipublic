
# script started 6/13/2024 by Bonnie McGill


################################################
# FIRST RUN "0_Load_adopt_data.R" to load packages and data  
################################################


################ table 2 
######## part 1: 2017 regional mean practice adoption levels
means.17 <- dat%>%
  filter(year_1==2017) %>%
  group_by(source, variable, region, county) %>%  # to calculate mean of counties with a corn entry and a soybean entry in OpTIS and Transect data
  summarize(Mean = mean(na.omit(value))) %>%
  group_by(source, variable, region) %>%
  summarize(Mean2 = mean(Mean), SE=se(Mean), n=length(county))


######### part 2: RATE OF CHANGE BY REGION

# we have:
# AgCensus data for years 2017, 2022
# OpTIS data for 2015-2021
# Transect data for 2015, 2017, 2018

# values for Table 2 rate of change
# first step same as above but not filtered for 2017
region_sum <- dat %>% group_by(region, year_1, source, variable, county) %>%
  group_by(source, variable, region, county, year_1) %>%  # to calculate mean of counties with a corn entry and a soybean entry in OpTIS and Transect data
  summarize(Mean = mean(na.omit(value))) %>%
  group_by(source, variable, region, year_1) %>%  
  summarize(Mean2 = mean(Mean), n=length(county))  # mean by region

# mark which year is start and end, different for each dataset
region_sum$yrmark <- ifelse(region_sum$source!="AgCensus"  & region_sum$year_1==2014, "a",  # first year (minus 1) of Transect and OpTIS data
                            ifelse(region_sum$source=="OpTIS" & region_sum$year_1==2020, "b", # last year of  OpTIS data (minus 1)
                                   ifelse(region_sum$source=="Transect" & region_sum$year_1==2017, "b",  # last year of transect till data (minus 1)
                                          ifelse(region_sum$source=="AgCensus" & region_sum$year_1==2017, "a", 
                                                 ifelse(region_sum$source=="AgCensus" & region_sum$year_1==2022, "b", "X"
                                                 )))))

# region_sum$yrmark <- ifelse(region_sum$source!="AgCensus" & region_sum$variable!="perc_cc" & region_sum$year_1==2015, "a",  # first year of Transect and OpTIS TILLAGE data
#                         ifelse(region_sum$source=="OpTIS" & region_sum$variable!="perc_cc" & region_sum$year_1==2021, "b", # last year of  OpTIS TILLAGE data
#                             ifelse(region_sum$source=="Transect" & region_sum$year_1==2018, "b",  # last year of transect till data
#                                         
#                                   ifelse(region_sum$source=="OpTIS" & region_sum$variable=="perc_cc" & region_sum$year_1==2014, "a", # # first year of OpTIS cover crop data (subtracted a year to align 2018 crop year with AgCensus 2017 calendar year)
#                                   ifelse(region_sum$source=="OpTIS" & region_sum$variable=="perc_cc" & region_sum$year_1==2020, "b", # so last year of OpTIS cc data are a year before tillage too
#               
#                                           ifelse(region_sum$source=="AgCensus" & region_sum$year_1==2017, "a",
#                                                  ifelse(region_sum$source=="AgCensus" & region_sum$year_1==2022, "b", "X"
#                                                  )))))))

# remove in between years
region_sum <- region_sum[region_sum$yrmark != "X",]
# create temp dataset of year a data
region_suma <- region_sum[region_sum$yrmark =="a",c(-4, -6, -7)]  #remove year and yrmark columns
colnames(region_suma)[4] <- c("mean.a")
# create temp dataset of year b data
region_sumb <- region_sum[region_sum$yrmark =="b",c(-4,-6, -7)]
colnames(region_sumb)[4] <- c("mean.b")

# join together wide, column for mean.a and mean.b
region_sum <- full_join(region_suma, region_sumb)
rm(region_suma, region_sumb)

# add the denominator (different for each dataset)
region_sum$denom <- ifelse(region_sum$source=="OpTIS", 2021-2015,  # equivalent to 2020-2014 for cover crops
                           ifelse(region_sum$source=="AgCensus", 2022-2017, 2018-2015))  # else transect years

# calculate rate of change
region_sum$rateofchange <- (region_sum$mean.b - region_sum$mean.a)/region_sum$denom

# re-order variables in output
region_sum <- region_sum[,c("region", "source", "variable", "rateofchange", "mean.a", "mean.b", "denom")]


# how many years to reach 50% adoption?

region_sum2 <- left_join(region_sum, means.17)   #### Table 2

region_sum <- region_sum %>%
  mutate(yrs2_50p_lo = (50-Mean-SE)/rateofchange,
         yrs2_50p_hi = (50-Mean+SE)/rateofchange)

