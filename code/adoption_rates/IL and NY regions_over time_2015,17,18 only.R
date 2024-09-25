# script started 8/22/2024 by Bonnie McGill

# here we pull together optis, us census of agriculture (via carpe), and il tillage transect 
# survey data for these counties
library(ggplot2)
library(tidyverse)
library(maps)
library(sf)
# install.packages("ggnewscale")
# library(ggnewscale)
# install.packages("Rmisc") 
library(Rmisc)  # for summarySE

se <- function(x) sd(x, na.rm=T) / sqrt(length(x))


### to do
# change clm data for central and southern IL


### load and prep optis data
# prepped from other scripts in this repo
optny <- read.csv("data/optis/datny_cleanlong.csv")
optny$state <- rep("New York", nrow(optny))
optny <- optny[,c(7, 1:3,5,6)]

optil <- read.csv("data/optis/datil_cleanlong.csv")
optil <- optil[,c(1,2,4,5,8,9)]
optil$state <- rep("Illinois", nrow(optil))

opt <- bind_rows(optny, optil)
rm(optil, optny)
opt$source <- rep("OpTIS", nrow(opt))
opt$year <- ifelse(opt$variable=="perc_cc", opt$year-1, opt$year)   # to (somewhat) align optis crop year with ag census calendar year
# Doing this for cover crops but not for tillage see supplemental Fig. S1
# i.e., crop year 2018 optis spring tillage, aligns with Census and Transect 2018 spring tillage, etc. ==DO NOT SUBTRACT 1 
# but crop year 2018 cover crop (fall 2017) aligns with Census calendar year fall 2017 planted-cover crop.  ==DO SUBTRACT 1



### load and prep census data
# came from spreadsheet in /CLM-19 FFAR GHG - General / Results / carpe adoption data
cens <- read.csv("data/optis/CaRPE CC_NT_RT 2017_2022_NY and IL all counties.csv")
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
till <- read.csv("data/optis/Illinois tillage transect survey data 2015-2018_all counties.csv")
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

###  combine

dat <- full_join(cens, opt) 
dat <- full_join(dat, till)

rm(cens,opt,till)

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

# bring in climate data from Alison Thiem
clm <- read.csv("data/optis/OpTIS_weather_v4/ThreeCountyWeatherOverviewV4.csv")
clm <- clm[,1:13]
# From Alison: I’m attaching a folder containing the daily data for each of the 
# three highlighted lat/lon locations for 2014-2015, 2015-2016, 2016-2017, 2017-2018, 2018-2019, 
# 2019-2020, 2020-2021, 2021-2022. Each of these has the average temperature (AirTC_Avg, C), 
# minimum temperature (TMIN_C, C), maximum temperature (TMAX_C, C), growing degree days 
# (GDD_4, base temperature 4C), cumulative growing degree days since October 1 (cum_GDD_4_1001), 
# frost degree days (FDD_4, base temperature 4C), cumulative frost degree days since 
# October 1 (cum_FDD_4_1001), below zero days (FDD_Y_N), cumulative days below zero 
# (cum_days_below_0C), precipitation (PRCP, mm), and cumulative precipitation since 
# October 1 (cum_Precip_mm_1001, mm). 

# There is also a summary table which lists the county, year,   ########### LOADING IN THE SUMMARY TABLE HERE
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

scale_cc <- gddmax/ (maxtest %>% dplyr::filter(variable=="perc_cc") %>% pull)
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
rm(clmcc, clmnt, clmrt, clmct, maxtest, scale_nt, scale_rt, scale_ct)


# separate tillage and cc data - will have different y-axis ranges and background bar chart data
# make tillage dataset
dat.till <- dat[dat$variable != 'perc_cc',]
dat.till$variable <- factor(dat.till$variable, levels=c('perc_ct', 'perc_rt', 'perc_nt'))
levels(dat.till$variable) <- c('Conventional till', 'Reduced till', 'No-till')
# make CC dataset 
dat.cc <- dat[dat$variable == 'perc_cc',]


# COVER CROPS PLOT

windows(xpinch=200, ypinch=200, width=5, height=5)

# to plot these with different secondary axes I need to plot CC and tillage separately
# Rather than as facets, then plot them together using plot_layout()

dat.cc$year.cat <- as.factor(dat.cc$year)
clm2$year.cat <- as.factor(clm2$year)
dat.cc$crop_source <- paste0(dat.cc$crop_name, "_", dat.cc$source)
ccsum <- summarySE(dat.cc, measurevar="value", groupvars=c("year.cat", "source", "crop_name", "region", "crop_source"))
ccsum <- ccsum[ccsum$year.cat %in% c("2015", "2017", "2018"),]

ggplot() +
  #GDD bars in background
  geom_bar(data=clm2[clm2$variable=="perc_cc" & clm2$year %in% c(2015, 2017, 2018),], 
           aes(x=year.cat, y=clm.dat, fill=variable), 
           stat="identity", width=0.9, alpha=0.3) +
  scale_fill_manual(breaks = "perc_cc",
                     labels = "Fall GDD",
                     values="#DDCC77",
                     name = "") +
  # add points
  geom_pointrange(data= ccsum, aes(x=year.cat, y=value, ymin= value-ci, ymax=value+ci, color=crop_source, shape=crop_source), 
             size=0.5, alpha=0.7, stroke=1,
             position=position_dodge(width=0.4)) +

  scale_color_manual(breaks = c("Corn_OpTIS", "Soybeans_OpTIS", "Cropland_AgCensus"),
                     labels = c("OpTIS Corn", "OpTIS Soybeans", "AgCensus (Cropland)"),
                     values=c( "#999933", "#999933", "#332288"),
                     name = "Data Source") +
  scale_shape_manual(breaks = c("Corn_OpTIS", "Soybeans_OpTIS", "Cropland_AgCensus"),
                     labels = c("OpTIS Corn", "OpTIS Soybeans", "AgCensus (Cropland)"),
                     values = c(16, 1, 0),
                     name="Data Source") +
  # plot settings/appearance
  facet_grid(rows=vars(factor(variable,levels=c( "perc_cc"))), 
             cols=vars(factor(region)), 
             labeller = as_labeller(
               c("perc_cc" = "Cover crop",
                 "Central IL" = "CIL",
                 "Southern IL" = "SIL",
                 "Western NY" = "WNY"))) +
  scale_y_continuous(sec.axis = sec_axis(~.*25, name="Fall GDD"), 
                     name="% of cropland",
                     expand=expansion(mult=c(0,0.05))) +
  scale_x_discrete(name="Year") + # , 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text.y=element_text(size=9),
    axis.text.x=element_blank(),
    # axis.text.x=element_text(size=9, angle=-30, hjust=0.3, vjust=0.2),
    axis.title.x=element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y=element_text(size=12, face="bold", vjust=+3),
    axis.title.y.right=element_text(size=12, face="bold", vjust=+3),  # second y axis
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_blank(), #element_rect(color="grey50", fill=NA, linewidth=0.5),
    strip.text=element_text(size=10),
    strip.background = element_rect(fill="gray95"),
    legend.text=element_text(size=11),
    legend.title=element_text(size=10, face="bold"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size = unit(0.6, "cm")
  )	 

ggsave("plots/adoption/perc acres_by year_CC_2015,17,18_no x axis.png", width=8, height=2.3, dpi=300)
# ggsave("plots/adoption/perc acres_by year_CC_2015,17,18.png", width=10, height=3, dpi=300)




# TILLAGE PLOT

windows(xpinch=200, ypinch=200, width=5, height=5)

dat.till$year.cat <- as.factor(dat.till$year)
clm2$year.cat <- as.factor(clm2$year)
dat.till$crop_source <- paste0(dat.till$crop_name, "_", dat.till$source)
tillsum <- summarySE(dat.till, 
                     measurevar="value", 
                     groupvars=c("year.cat","variable", "source", "crop_name", "region", "crop_source"),
                     na.rm=TRUE)
clm2.till<- clm2[clm2$variable !="perc_cc" & clm2$year %in% c(2015, 2017, 2018),]
clm2.till$variable <- factor(clm2.till$variable, levels=c('perc_nt', 'perc_rt', 'perc_ct'))
levels(clm2.till$variable) <- c('No-till', 'Reduced till', 'Conventional till')
tillsum <- tillsum[tillsum$year %in% c(2015, 2017, 2018),]


# unique(tillsum$crop_source)  # "Corn_OpTIS"        "Soybeans_OpTIS"    "Corn_Transect"     "Soybeans_Transect" "Cropland_AgCensus"

ggplot() +
  # precip bars in background
  geom_bar(data=clm2.till, 
           aes(x=year.cat, y=clm.dat, fill=variable), 
           stat="identity", width=0.9, alpha=0.2) +
  scale_fill_manual(values=c("No-till"="#88CCEE","Reduced till"= "#88CCEE","Conventional till"="#88CCEE"),
                    breaks = "No-till",  # only show one key in legend
                    labels = "Crop year fall &\nspring precip",
                    name = "") +
  # add points
  geom_pointrange(data= tillsum, aes(x=year.cat, y=value, ymin= value-ci, ymax=value+ci, color=crop_source, shape=crop_source), 
                  size=0.5, alpha=0.7, stroke=1,
                  position=position_dodge(width=0.4)) +
  scale_color_manual(breaks = c("Corn_OpTIS", "Soybeans_OpTIS", "Cropland_AgCensus", "Corn_Transect", "Soybeans_Transect"),
                     labels = c("OpTIS Corn", "OpTIS Soybeans", "AgCensus (Cropland)", "Transect Corn", "Transect Soybeans"),
                     values=c( "#999933", "#999933", "#332288", "#AA4499", "#AA4499"),
                     name = "Data Source") +
  scale_shape_manual(breaks = c("Corn_OpTIS", "Soybeans_OpTIS", "Cropland_AgCensus", "Corn_Transect", "Soybeans_Transect"),
                     labels = c("OpTIS Corn", "OpTIS Soybeans", "AgCensus (Cropland)", "Transect Corn", "Transect Soybeans"),
                     values = c(16, 1, 0, 16, 1),
                     name="Data Source") +
  # plot settings/appearance
  facet_grid(rows=vars(factor(variable)), 
             cols=vars(factor(region)), 
             labeller = as_labeller(
               c("No-till"="No-till","Reduced till"= "Reduced till", "Conventional till"= "Conventional till",
                 "Central IL" = "CIL","Southern IL" = "SIL","Western NY" = "WNY"))) +
  scale_y_continuous(sec.axis = sec_axis(~.*scale_cc, name="Precip (mm)"), 
                     name="% of cropland",
                     expand=expansion(mult=c(0,0.05))) +
  scale_x_discrete(name="Year") + # , 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text.y=element_text(size=9),
    axis.text.x=element_text(size=9, angle=-30, hjust=0.3, vjust=0.2),
    axis.title.y=element_text(size=12, face="bold", vjust=+3),
    axis.title.y.right=element_text(size=12, face="bold", vjust=+3),  # second y axis
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_blank(), #element_rect(color="grey50", fill=NA, linewidth=0.5),
    strip.text.x=element_blank(),
    strip.background.x = element_blank(),
    strip.text.y =element_text(size=10),
    strip.background.y = element_rect(fill="gray95"),
    legend.text=element_text(size=11),
    legend.title=element_text(size=10, face="bold"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size = unit(0.6, "cm")
  )	 

ggsave("plots/adoption/perc acres_by year_tillage_2015,17,18_no region labels.png", width=8, height=6, dpi=300)




############ is precip a predictor of tillage? is gdd a predictor of cover crops?


# need to have clm and adoption data in one row
clm3 <- clm[, c("year", "region", 
                "cum_precip_mm_1001.1130.0301.0430", 
                "Cumulative_GDD_1001.First.Big.Freeze")]

colnames(clm3)[c(3,4)] <- c("cumprecip", "cumgdd")

datclm <- left_join(dat, clm3)

summary(lm.cc <- lm(value~cumgdd*region, dat=datclm[datclm$variable=="perc_cc",]))
summary(lm.till <- lm(value~cumprecip*region*variable, dat=datclm[datclm$variable !="perc_cc",]))



       
summary(lm.cc <- lm(value~cumgdd*region, dat=datclm[datclm$variable=="perc_cc" & datclm$source=="OpTIS",]))
summary(lm.till <- lm(value~cumprecip*region*variable, dat=datclm[datclm$variable !="perc_cc" & datclm$source=="OpTIS",]))
       

ggplot(dat=datclm[datclm$variable=="perc_cc" & datclm$source=="OpTIS",], 
       aes(x=cumgdd, y=value, color=region)) +
  geom_point()  +
  xlab("Fall GDD") +
  ylab("Cover crop adoption (%)")
  # geom_smooth()


ggplot(dat=datclm[datclm$variable!="perc_cc" & datclm$source=="OpTIS",], 
       aes(x=cumprecip, y=value, color=region)) +
  geom_point() + 
  facet_grid(rows=vars(variable)) +
  xlab("Fall and Spring precip (mm)") +
  ylab("Cover crop adoption (%)")


