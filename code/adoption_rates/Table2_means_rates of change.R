
# script started 6/13/2024 by Bonnie McGill


################################################
# FIRST RUN "0_Load_adopt_data.R" to load packages and data  
################################################


################ table 2 
######## part 1: 2017 regional mean practice adoption levels
means.17 <- dat%>%
  filter(year==2017) %>%
  group_by(source, variable, region) %>%
  summarize(Mean = mean(na.omit(value)), SE=se(na.omit(value)))


######### part 2: RATE OF CHANGE BY REGION

# what is the REGIONAL level % adoption AgCensus  ---values for Table 1 AgCensus
region_sum <- dat %>% group_by(region, year, source, variable) %>%
  summarize(mean = mean(na.omit(value)),
            se=se(na.omit(value))) 

region_sum$yrmark <- ifelse(region_sum$source!="AgCensus" & region_sum$variable!="perc_cc" & region_sum$year==2015, "a",  # first year of Transect and OpTIS TILLAGE data
                        ifelse(region_sum$source=="OpTIS" & region_sum$variable!="perc_cc" & region_sum$year==2021, "b", # last year of  OpTIS TILLAGE data
                            ifelse(region_sum$source=="Transect" & region_sum$year==2018, "b",  # last year of transect till data
                                        
                                  ifelse(region_sum$source=="OpTIS" & region_sum$variable=="perc_cc" & region_sum$year==2014, "a", # # first year of OpTIS cover crop data (subtracted a year to align 2018 crop year with AgCensus 2017 calendar year)
                                  ifelse(region_sum$source=="OpTIS" & region_sum$variable=="perc_cc" & region_sum$year==2020, "b", # so last year of OpTIS cc data are a year before tillage too
              
                                          ifelse(region_sum$source=="AgCensus" & region_sum$year==2017, "a",
                                                 ifelse(region_sum$source=="AgCensus" & region_sum$year==2022, "b", "X"
                                                 )))))))

region_sum <- region_sum[region_sum$yrmark != "X",]
region_suma <- region_sum[region_sum$yrmark =="a",c(-2, -6, -7)]  #remove year, se, and yrmark columns
region_sumb <- region_sum[region_sum$yrmark =="b",c(-2, -6, -7)]

colnames(region_suma)[4] <- c("mean.a")
colnames(region_sumb)[4] <- c("mean.b")


region_sum2 <- full_join(region_suma, region_sumb)
rm(region_suma, region_sumb)

region_sum2$denom <- ifelse(region_sum2$source=="OpTIS", 2021-2015,  # equivalent to 2020-2014 for cover crops
                            ifelse(region_sum2$source=="AgCensus", 2022-2017, 2018-2015))  # else transect years

region_sum2$rateofchange <- (region_sum2$mean.b - region_sum2$mean.a)/region_sum2$denom

region_sum2 <- region_sum2[,c("region", "source", "variable", "rateofchange", "mean.a", "mean.b", "denom")]


# how many years to reach 50% adoption?

region_sum2 <- left_join(region_sum2, means.17)   #### Table 2

region_sum2 <- region_sum2 %>%
  mutate(yrs2_50p_lo = (50-Mean-SE)/rateofchange,
         yrs2_50p_hi = (50-Mean+SE)/rateofchange)

