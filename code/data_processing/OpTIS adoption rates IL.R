# OpTIS adoption rate information for Illinois and New York aggregated by counties 2015-2021.
# Data shared with AFT from Regrow analysis.
# Column "kpi_name" gives different types of land use areas, with "area" meaning the 
# analyzed acres of the county, tillage type/ cover crop / etc.
# units are ACRES

# load packages
library(reshape2) # for dcast()
library(ggplot2)
library(dplyr)


# to update data, run "download data.R"




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
ILac$perc_eval <- round((100*ILac$eval.acres / ILac$harv_ac), digits=1)

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
   ###### should we eliminate counties with < X acres acres?? 
   ###### what is the lower 10th percentile in terms of harvested acres?
   ###### Hardin in lower tenth percentile, gets excluded

tenthp <- quantile(acres$harv_ac, probs=0.1)
#  70245.6 acres

 # Union Co. also shows up every year around 40% evaluated. In the ag census it has
   # 84,872 ac in 2017, and 59,743 acres in 2012, quite a fluctuation. OpTIS is evaluating about 
   # 37k acres. Union Co. is also in southern IL, bordering MO, and has lots of forest
   # land intermixed with ag fields.
   ####### (Union Co. is 2nd highest %NT county in the state (once we exclude lower 10% counties by acre)--should it be?)
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
   ### excluded when cut lower 10th percentile
 # Putnam county in Census was 41,934 in 2017 and 46,212 in 2012. OpTIS is evaluating
   # about 54,000 ac.Putnam Co. altogether is about 110,000 ac. Roughly half of which is 
   # in ag, looking at google maps satellite. So 41k, 46k, and 54k, are all plausible.
   ### excluded when cut lower 10th percentile

# number of counties represented in IL data
# datw %>%
#   filter(state %in% "IL") %>%
#   distinct(county) %>%
#   n_distinct()
# 101 # 102 counties in IL, Lake County is missing, it's a tiny county between
# Chicago and Wisconsin border, basically still Chicago outlying area. No ag.
# surprised cook county (Chicago) is even in dataset!


# remove lowest 10 percent of counties based on acreage (keep upper 90%)
ILac_up90 <- ILac[ILac$harv_ac > tenthp,]
# remove counties with < 50% acres evaluated
# first remove counties with < 50% in 2017 (corresponds to year of harvested cropland data)
ILac_ev50 <- ILac_up90 %>%
                filter(year==2017, perc_eval>50)
# then join these counties with ILac_up90
ILac_up90 <- inner_join(ILac_up90, ILac_ev50[,c(1,4)])

# clean up
rm(ILac_ev50, big, small, acres)                
  


# # counties excluded:
# ILac_lo10 <- ILac[ILac$harv_ac <= tenthp,] %>%
#               filter(year== 2015)
# # Alexander, DuPage, Hardin, Johnson, Putnam now all excluded
# # Union still in, barely makes the cutoff.
# # ILac_lo10
# #    county            year eval.acres  harv_ac    perc_eval
# # 1   Alexander County 2015  42645.819   38737     110.1
# # 2       Brown County 2015  59885.648   69625      86.0
# # 3     Calhoun County 2015  30301.075   54453      55.6
# # 4        Cook County 2015   7761.593   10095      76.9
# # 5      DuPage County 2015   3923.015    1272     308.4
# # 6      Hardin County 2015   5234.984   13421      39.0
# # 7     Johnson County 2015  19141.045   37375      51.2
# # 8        Pope County 2015  17526.383   19823      88.4
# # 9      Putnam County 2015  54470.438   41934     129.9
# # 10 Williamson County 2015  40985.528   61440      66.7
# rm(ILac_lo10)


# "Described the total acres evaluated and how the % was calculated." 
# % CC, NT, RT was calculated by dividing the number of acres with those practices 
# detected by the number of cropland acres evaluated by OpTIS per county-year

# what proportion of cropland acres did OpTIS evaluate per county?
# counties in the lowest 10% of cropland acres (according to ag census 2017) were excluded
# Counties with <50% of cropland acres evaluated in 2017 were excluded
ILac_up90 <- ILac_up90 %>%  
  arrange(perc_eval)
hist(ILac_up90$perc_eval, breaks=15)
mean(ILac_up90$perc_eval) # 85.6
min(ILac_up90$perc_eval) # 46.5 - one county had >50% in 2017, but <50% in one year.
max(ILac_up90$perc_eval) # 114.5 - cropland acres reported in the census change from year to year, so it is
 # possible that the acres evaluated were > number of acres reported in 2017, or that the number of acres reported
 # differs from what the satellites detect.
 # On average, OpTIS evaluated 86% of a county's harvested cropland acres (for those counties not excluded). 






################# CALCULATE RATES

# calculate percentages
datw$perc_ct <- round(100*(datw$ct.acres / datw$eval.acres), digits=1)
datw$perc_cc <- round(100*(datw$cc.acres / datw$eval.acres), digits=1)
datw$perc_nt <- round(100*(datw$nt.acres / datw$eval.acres), digits=1)
datw$perc_rt <- round(100*(datw$rt.acres / datw$eval.acres), digits=1)

datw <- inner_join(datw, ILac_up90[,c(1,2,5)]) #  inner_join() only keeps observations from x that have a matching key in y.


# long form again, but leave out acreages, only need %s
datl <- melt(datw[, -c(6:11)], id=c("state", "county", "fips", "year", "crop_name", "perc_eval"), na.rm=T)
# using na.rm = TRUE because do not have all combinations of crop acres per cover crop or tillage acres, e.g. 
# we have soybean acres in Ulster County, NY in 2015, but we do not have soybean cover crop acres for that 
# county-year. So when we melt, we get a bunch of Value = NA rows.

se <- function(x) sd(x) / sqrt(length(x))

# (ignore alfalfa and winter wheat acre for cover crops)

# what are the average county adoption rates and number of acres evaluated? (are any small counties driving percents up?)
# which counties are hotspots?
# use this to make map of counties see "/code/plot_making/OpTIS_county_heatmap_IL.R"
means_county <- datl %>%
                filter(year>2017) %>%
                group_by(state, county, variable) %>%
                summarize(mean.perc = round(mean(value), digits=1), 
                          mean.eval = round(mean(perc_eval), digits=1)) %>%
                arrange(variable, desc(mean.perc)) %>%
                ungroup() %>%
                group_by(variable) %>%
                mutate(rank = seq_along(variable))

# write.csv(means_county, "data/optis/IL_means_county.csv", row.names=F)

# use this to make table of top 5
means_county_top5 <- means_county %>%
                     filter(rank<6)

# do we mean small counties in terms of number of acres evaluated or small counties in terms of number of crop acres period?
# corn.acres <- datw[datw$state %in% "IL" & datw$crop_name %in% "Corn", c(1,2,7)]
# corn.acres <- corn.acres %>%
#               group_by(county) %>%
#               summarize(Mean.ac.eval = mean(acres))
# means_county <- left_join(means_county, corn.acres)
# cc <- means_county %>%
#   filter(variable %in% "perc_nt", state %in% "IL")

# what are the state average adoption rates by year and crop
means_cropyear <- datl %>% 
                group_by(state, year, crop_name, variable) %>%
                summarize(Mean = mean(value))
                          

# # what are the state average adoption rates by year (across all crops)
means_year <- datl%>%
              filter (! crop_name %in% "Alfalfa")  %>%
                  # because Alf is perennial no tillage, no covers
              filter(!(crop_name %in% "Winter Wheat" & variable == "perc_cc")) %>%
                  # because Alf is a perennial and winter wheat grows at same time as covers
              group_by(state, year, variable) %>%
              summarize(Mean = mean(value))

# what are the state average adoption rates across all available years (2015-2021) per crop?
means_crop <- datl%>%
                    filter (! crop_name %in% "Alfalfa")  %>%
                    # because Alf is perennial no tillage, no covers
                    filter(!(crop_name %in% "Winter Wheat" & variable == "perc_cc")) %>%
                    # because Alf is a perennial and winter wheat grows at same time as covers
                    group_by(state, crop_name, variable) %>%
                    summarize(Mean = mean(value))

# what are the state average adoption rates across MOST RECENT 4 YEARS 2018-2021?  
# note results not all that different between means_crop and means_crop_1821
# for now, will report most recent data:
means_crop_1821 <- datl%>%
              filter(year>2017) %>%
              filter (! crop_name %in% "Alfalfa")  %>%
              # because Alf is perennial no tillage, no covers
              filter(!(crop_name %in% "Winter Wheat" & variable == "perc_cc")) %>%
              # because Alf is a perennial and winter wheat grows at same time as covers
              group_by(state, crop_name, variable) %>%
              summarize(Mean = mean(value), se=se(value))

# what are the state average adoption rates across all years and crops
means_global <- datl%>%
                filter (! crop_name %in% "Alfalfa")  %>%
                  # because Alf is perennial no tillage, no covers
                filter(!(crop_name %in% "Winter Wheat" & variable == "perc_cc")) %>%
                  # because Alf is a perennial and winter wheat grows at same time as covers
                group_by(state, variable) %>%
                summarize(Mean = mean(value), se = se(value))


# plots for mean adoption rates

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(dat=means_crop_1821[means_crop_1821$state=="IL",], aes(x=variable, y=Mean, group=crop_name)) + 
    geom_bar(stat="identity", position=position_dodge(), aes(fill=crop_name)) +
    geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se), linewidth=0.8, color="#000000",
                  position=position_dodge(0.9), width=0.2) +
    scale_x_discrete(limits=c("perc_cc", "perc_nt", "perc_rt"), 
                       labels=c("Cover\nCrops", "No\nTill", "Reduced\nTill")) +  # ok to just leave out conventional till?
    scale_fill_manual(values=c("#DDAA33", "#228833", "#66CCEE"), 
                      labels=c("Corn", "Soybeans", "Winter Wheat"),
                      name="Crops") +
    scale_y_continuous(breaks=seq(0,70,10)) +
    ylab("Mean Percent Adoption 2018-2021") +
    xlab("Soil Health Practice") +
    geom_text(aes(y=Mean +5, 
                  label=paste0(format(round(Mean, digits=1), nsmall=1), "%")), 
                  position=position_dodge(0.9)) +
    # facet_grid(.~state) +
    theme(
        #legend.text.align=0,
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank() ,
        # axis.text.x=element_text(angle=-30, hjust=0, size=11),
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
       
ggsave("plots/IL_bar_means_crop_1821.png")    






# how does adoption rate change over time?

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=datw, aes(y=perc_cc, x=year, group=county)) +
  geom_line(aes(color=county), show.legend=F) + #, position="jitter", size=0.5) +
  facet_grid(rows=vars(crop_name), cols=vars(state))

# which counties have at least 5 years of consecutive measurements?
# work with a subset of data
subs <- datw[,c(1,2,5,4)]
# 
subs <- subs %>%
  group_by(state, county, crop_name) %>%
  mutate(con_yrs = cumsum(c(1, diff(year)>1))) %>%  # make col that is "1" for consecutive years, 2 if non-consecutive years
  group_by(state, county, crop_name, con_yrs) %>% # group by new column
  mutate(start = min(year), end = max(year), diff=end - start) # list first and last year of consecutive groups

# join back together with rest of data
datw2 <- left_join(datw, subs) %>%
  filter(diff>4) # only keep county-crop-years with at least 5 years consecutive data 2015-2021
# removes 102 county-crop-years
ggplot(data=datw2, aes(y=perc_cc, x=year, group=county)) +
  geom_line(aes(color=county), show.legend=F) + #, position="jitter", size=0.5) +
  facet_grid(rows=vars(crop_name), cols=vars(state))
unique(datw2$diff)

sum(is.na(datw$perc_cc))
sum(is.na(datw$acres))
sum(is.na(datw$cc.acres))
test <- datw[rowSums(is.na(datw))>0,]
# turns out that we create NAs for counties where certain kpi_names are not measured
# for that county that year that crop
# so need to do the subs code above for each kpi_name