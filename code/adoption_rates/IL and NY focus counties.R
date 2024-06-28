
# NY counties (fips) = Genesee (36037), Wyoming (36121), Livingston (36051) 
# IL counties (fips) = Ford (17053), Livingston (17105), Macoupin (17117), Montgomery (17135)

# here we pull together optis, us census of agriculture (via carpe), and il tillage transect 
# survey data for these counties

# script started 6/13/2024 by Bonnie McGill

library(ggplot2)
library(tidyverse)
library(patchwork) # for plot_layout()



###### 1) prepare the data

# load and prep optis data
# prepped from other scripts in this repo
optny <- read.csv("data/optis/datny_cleanlong.csv")
optny$state <- rep("New York", nrow(optny))
optny <- optny[,c(7, 1:3,5,6)]
optny <- filter(optny, county %in% c("Genesee", "Wyoming", "Livingston"))

optil <- read.csv("data/optis/datil_cleanlong.csv")
optil <- optil[,c(1,2,4,5,8,9)]
optil <- filter(optil, county %in% c("Ford", "Livingston", "Macoupin", "Montgomery"))
optil$state <- rep("Illinois", nrow(optil))

opt <- bind_rows(optny, optil)
rm(optil, optny)
opt$source <- rep("OpTIS", nrow(opt))
# unique(opt$crop_name)  #  "Soybeans" "Corn"

# census data
# came from spreadsheet in /CLM-19 FFAR GHG - General / Results / carpe adoption data
cens <- read.csv("data/optis/CaRPE CC_NT_RT 2017_2022.csv")
# note CaRPE refers to conventional tillage as intensive tillage, changing it to CT here:
colnames(cens) <- c("year", "state", "county", "perc_cc", "perc_nt", "perc_rt", "perc_ct")
cens$source <- rep("AgCensus", nrow(cens))
cens <- pivot_longer(cens, cols=c(perc_cc, perc_nt, perc_rt, perc_ct), names_to="variable", values_to="value")
# Census data are for all cropland, not by crop like the other data sources
cens$crop_name <- rep("Cropland", nrow(cens))
# # Want to plot census data with corn and soybean crops
# # make a copy of census label one as corn and one as soybeans so they get plotted in those facets
# cens$crop_name <- rep("Soybeans", nrow(cens))
# cens <- cens[,c(2,3,1,7,5,6,4)]
# censb <- cens
# censb$crop_name <- rep("Corn", nrow(censb))
# cens <- bind_rows(cens, censb)
# rm(censb)


# tillage transect data
till <- read.csv("data/optis/Illinois tillage transect survey data 2015-2018.csv")
# came from spreadsheet in /CLM-19 FFAR GHG - General / Results / IL tillage transect data
# as.data.frame(names(till))
# note the "%" symbol doesn't come through into R but these aren't acres these are %s:
till <- till[,c(1,2,4,9,12,15,18)]
colnames(till) <- c("year", "crop_name", "county", "perc_nt", "perc_mt", "perc_rt", "perc_ct")
till$source <- rep("Transect", nrow(till))
# according to the definitions of residue cover, fields categorized as "mulch till" in 
# transect data would correspond to "no till" in OpTIS data
# sum no till and mulch till for new "no till"
till$perc_nt <- till$perc_nt + till$perc_mt
# remove mulch till
till$perc_mt <- NULL
till <- pivot_longer(till, cols=c(perc_nt, perc_rt, perc_ct), names_to="variable", values_to="value")
till$state <- rep("Illinois", nrow(till))
till <- till[,c(7,3,1,2,5,6,4)]
unique(till$crop_name)
till$crop_name <- ifelse(till$crop_name=="corn", "Corn", "Soybeans")


dat <- bind_rows(cens,opt,till)
rm(cens,opt,till)

# dat$crop_source <- paste0(dat$crop_name, " ", dat$source)

dat$region <- ifelse(dat$state=="New York", "Western NY", 
                ifelse(dat$state=="Illinois" & dat$county %in% c("Ford", "Livingston"), "Northern IL",
                  ifelse(dat$state=="Illinois" & dat$county %in% c("Macoupin", "Montgomery"), "South Central IL", "X")))
unique(dat$region)  

dat$costate <- paste0(dat$county, " County, ", dat$state)


# bring in climate data from Alison Thiem
clm <- read.csv("data/optis/OpTIS_weather_v2/ThreeCountyWeatherOverviewV3.csv")
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

clm$cum_precip_mm_1001.1130.0301.0430 <- clm$Cumulative_precip_mm_0301.0430 + clm$Cumulative_precip_mm_1001.1130

# The 3 locations are 
# Wyoming County, NY 42.7, -78.2  (western New York)
# Livingston County, IL 40.9, -88.5 (northern IL)
# Macoupin County, IL 39.3, -89.9 (south central IL)

# GOING TO give clm data the same year as optis - so 2016-2017 is 2017
# extract characters 6-10 from Year
clm$year2 <- str_sub(clm$Year, 6,10)
clm$region <- ifelse(clm$County=="Wyoming, NY", "Western NY",
                     ifelse(clm$County=="Livingston, IL", "Northern IL", "South Central IL"))

# to plot a secondary y-axis in ggplot, we have to scale the second variable to the first one.
# for cover crops we will be adding GDD data, what is the max?
gddmax <- max(clm$Cumulative_GDD_1001.First.Big.Freeze)
# for tillage we will be adding precip data, what is the max precip?
precipmax <- max(clm$cum_precip_mm_1001.1130.0301.0430)
# for cover crop adoption data what is the max?
maxtest <- group_by(dat, variable) %>%
  summarize(max=max(value))

scale_cc <- gddmax/ (maxtest %>% filter(variable=="perc_cc") %>% pull)
scale_nt <- precipmax/ (maxtest %>% filter(variable=="perc_nt") %>% pull)
scale_rt <- precipmax/ (maxtest %>% filter(variable=="perc_rt") %>% pull)
scale_ct <- precipmax/ (maxtest %>% filter(variable=="perc_ct") %>% pull)

# The tillage scales are very similar so let's get the mean and use that for those 3
scale_till <- mean(c(scale_nt, scale_rt, scale_ct))

# scale clm data by adoption data
clm$cum_gdd_scaled <- clm$Cumulative_GDD_1001.First.Big.Freeze/scale_cc
clm$cum_precip_tillscaled <- clm$cum_precip_mm_1001.1130.0301.0430/scale_till


# clm needs to have "variable" to facet like in the plot below
clmcc <- clm %>% select(year2, region, cum_gdd_scaled) 
clmcc$variable <- rep("perc_cc", nrow(clmcc))
colnames(clmcc)[3] <- "clm.dat"
clmnt <- clm %>% select(year2, region, cum_precip_tillscaled) 
clmnt$variable <- rep("perc_nt", nrow(clmnt))
colnames(clmnt)[3] <- "clm.dat"
clmrt <- clm %>% select(year2, region, cum_precip_tillscaled) 
clmrt$variable <- rep("perc_rt", nrow(clmrt))
colnames(clmrt)[3] <- "clm.dat"
clmct <- clm %>% select(year2, region, cum_precip_tillscaled) 
clmct$variable <- rep("perc_ct", nrow(clmct))
colnames(clmct)[3] <- "clm.dat"

clm2 <- bind_rows(clmcc, clmnt, clmrt, clmct)
rm(clmcc, clmnt, clmrt, clmct, maxtest, scale_nt, scale_rt, scale_ct)
clm2$year2 <- as.numeric(clm2$year2)
clm2$cc_year <- ifelse(clm2$variable=="perc_cc", clm2$year2-1, clm2$year2)

###### 2b) make a tillage and CC plot that is the average of the counties per region


regiondat <- group_by(dat, region, year, crop_name, variable, source) %>%
  summarize(mean_value = mean(value))

# aligning years
# AgCensus 2017 cover crops correspond to Optis 2018 cover crops
# AgCensus 2017 tillage correspond to spring OptIS 2017 and fall 2018 tillage
# 2017 transect corresponds to 2017 AgCensus and 2017 optis
# do optis - 1 for covers
# no change for tillage years


regiondat$cc_year <- ifelse(regiondat$source=="OpTIS", regiondat$year-1, regiondat$year)

windows(xpinch=200, ypinch=200, width=5, height=5)

# to plot these with different secondary axes I need to plot them separately
# Rather than as facets, then plot them together using plot_layout()


pcc <- ggplot() +
  
  geom_bar(data=clm2[clm2$variable=="perc_cc",], 
           aes(x=cc_year, y=clm.dat), 
           stat="identity", fill="#DDCC77", width=1, alpha=0.4) +

  geom_point(data=regiondat[regiondat$variable=="perc_cc",], 
             aes(x=cc_year, y=mean_value, color=source, shape=crop_name),   
             size=1.8, alpha=1, stroke=1.4, show.legend=F) +  # For geom_jitter, add width=0.2
  scale_y_continuous(sec.axis = sec_axis(~.*scale_cc, name="Oct 1- first deep freeze GDD"), 
                     name="% of county acres",
                     expand=c(0,0)) +

  facet_grid(rows=vars(factor(variable,levels=c( "perc_cc"))), 
             cols=vars(factor(region)), 
             labeller = as_labeller(
               c("perc_cc" = "Cover crop",
                 "Northern IL" = "Northern Illinois",
                 "South Central IL" = "South Central Illinois",
                 "Western NY" = "Western New York")),
             scales="free")+
  scale_color_manual(breaks = c("AgCensus", "OpTIS", "Transect"),
                     values=c("#332288", "#999933", "#AA4499"),
                     name = "Data Source") +
  scale_shape_manual(breaks = c("Corn", "Soybeans", "Cropland"),
                     values = c(16, 1, 0),
                     name="AgCensus/Transect acreage type") +
  # scale_linetype_manual(values=c(1,2), name="OpTIS acreage type") +
  scale_x_continuous(name="Year", breaks=seq(2014,2022,1)) + # , 
                     # expand =expansion(add=c(0,1))) +
  # expand_limits(x=2014) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text.y=element_text(size=9),
    axis.text.x=element_text(size=9, angle=-30, hjust=0),
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

pcc 

ggsave("plots/adoption/perc acres_cc_2015-21.png", width=10, height=4, dpi=300)





ptill <- ggplot() +
  
  geom_bar(data=clm2[!clm2$variable=="perc_cc",], 
           aes(x=year2, y=clm.dat), 
           stat="identity", fill="#88CCEE", width=1, alpha=0.4) +
  
  geom_point(data=regiondat[!regiondat$variable=="perc_cc",], 
             aes(x=year, y=mean_value, color=source, shape=crop_name),   
             size=1.8, alpha=1, stroke=1.4) +  # For geom_jitter, add width=0.2
  scale_y_continuous(sec.axis = sec_axis(~.*scale_till,name="Oct, Nov, Mar, and Apr Precip (mm)"), 
                     name="% of county acres",
                     expand=c(0,0)) +
  
  facet_grid(rows=vars(factor(variable,levels=c( "perc_nt", "perc_rt", "perc_ct"))), 
             cols=vars(factor(region)), 
             labeller = as_labeller(
               c("perc_nt" = "No-till",
                 "perc_rt" = "Reduced tillage", "perc_ct" = "Conventional tillage",
                 "Northern IL" = "Northern Illinois",
                 "South Central IL" = "South Central Illinois",
                 "Western NY" = "Western New York")),
             scales="free")+
  scale_color_manual(breaks = c("AgCensus", "OpTIS", "Transect"),
                     values=c("#332288", "#999933", "#AA4499"),
                     name = "Data Source") +
  scale_shape_manual(breaks = c("Corn", "Soybeans", "Cropland"),
                     values = c(16, 1, 0),
                     name="AgCensus/Transect acreage type") +
  # scale_linetype_manual(values=c(1,2), name="OpTIS acreage type") +
  scale_x_continuous(name="Year", breaks=seq(2014,2022,1), 
                     expand =expansion(add=c(1,0))) +
  #   scale_x_continuous(name="Year", breaks=seq(2014,2022,1), expand =c(0,0))+
  # expand_limits(x=2014) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text.y=element_text(size=9),
    axis.text.x=element_text(size=9, angle=-30, hjust=0),
    axis.title.y=element_text(size=12, face="bold", vjust=+3),
    axis.title.y.right=element_text(size=12, face="bold", vjust=+3),  # second y axis
    axis.title.x=element_text(size=12, face="bold"),
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

ptill 

ggsave("plots/adoption/perc acres_3till_2015-21.png", width=10, height=7, dpi=300)





pcc + ptill + plot_layout(ncol=1, heights=c(1.2,3), guide='collect')



ggsave("plots/adoption/perc acres_4practices_2015-2021_2ndaxis.png", width=10, height=10, units="in")








ggplot() +
  
  geom_bar(data=clm2[clm2$variable=="perc_cc",], 
           aes(x=year2, y=clm.dat), 
           stat="identity", fill="#DDCC77", width=1, alpha=0.6) +
  
  geom_bar(data=clm2[!clm2$variable=="perc_cc",], 
           aes(x=year2, y=clm.dat), 
           stat="identity", fill="#88CCEE", width=1, alpha=0.6) +
  
  
  geom_point(data=regiondat, #[!regiondat$source=="OpTIS",], 
             aes(x=year, y=mean_value, color=source, shape=crop_name),   # Data are same between corn and soybean, Agcensus does not differentiate
             size=1.8, alpha=1, stroke=1.4) +  # For geom_jitter, add width=0.2
  # # cover crop lines use different year
  # geom_line(data=regiondat[regiondat$source=="OpTIS" & regiondat$variable=="perc_cc",], 
  #           aes(x=cc_year, y=mean_value, color=source, linetype=crop_name), linewidth=0.6, alpha=1) + 
  # # tillage lines use regular year
  # geom_line(data=regiondat[regiondat$source=="OpTIS" & !regiondat$variable=="perc_cc",], 
  #           aes(x=year, y=mean_value, color=source, linetype=crop_name), linewidth=0.6, alpha=1) + 
  scale_y_continuous(sec.axis = sec_axis(~.*scale_cc, name="Fall GDD"), name="% of county acres") +
  facet_grid(rows=vars(factor(variable,levels=c("perc_cc", "perc_nt", "perc_rt", "perc_ct"))), 
             cols=vars(factor(region)), 
             labeller = as_labeller(
               c("perc_cc" = "Cover Crop", "perc_nt" = "No-till",
                 "perc_rt" = "Reduced tillage", "perc_ct" = "Conventional tillage",
                 "Northern IL" = "Northern Illinois",
                 "South Central IL" = "South Central Illinois",
                 "Western NY" = "Western New York")),
             scales="free")+
  scale_color_manual(breaks = c("AgCensus", "OpTIS", "Transect"),
                     values=c("#332288", "#999933", "#AA4499"),
                     name = "Data Source") +
  scale_shape_manual(breaks = c("Corn", "Soybeans", "Cropland"),
                     values = c(16, 1, 0),
                     name="AgCensus/Transect acreage type") +
  scale_linetype_manual(values=c(1,2), name="OpTIS acreage type") +
  scale_x_continuous(name="Year", breaks=seq(2015,2022,1))+
  ylab("% of county acres") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text.y=element_text(size=9),
    axis.text.x=element_text(size=9, angle=-30, hjust=0),
    axis.title=element_text(size=12, face="bold"),
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
    strip.text=element_text(size=10),
    strip.background = element_rect(fill="gray95"),
    legend.text=element_text(size=11),
    legend.title=element_text(size=10, face="bold"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size = unit(0.6, "cm")
  )	 

ggsave("plots/adoption/perc acres_4practices_2015-21.png", width=10, height=7, dpi=300)









###### 2) make tillage plot for each county

windows(xpinch=200, ypinch=200, width=5, height=5)

fills <- c("perc_ct" = "#EE8866",
           "perc_rt" = "#BBCC33",
           "perc_nt" = "#77AADD")

fields <- c("Corn" = "Corn fields", 
            "Soybeans" = "Soybean fields", 
            "Western NY" = "Western NY",
            "Northern IL" = "Northern IL",
            "South Central IL" = "South Central IL")

ggplot() +
  geom_jitter(data=dat[!dat$variable=="perc_cc" & !dat$source=="OpTIS",], 
              aes(x=year, y=value, shape=source, color=variable), 
              size=1, width=0.15, alpha=0.8, stroke=1.2) + 
  geom_line(data=dat[!dat$variable=="perc_cc" & dat$source=="OpTIS",], 
            aes(x=year, y=value, color=variable, linetype=factor(line)), size=0.8) + 
  facet_grid(rows=vars(region), cols=vars(crop_name), labeller = as_labeller(fields)) +
  scale_color_manual(breaks = c("perc_ct", "perc_rt", "perc_nt"),
                     values = fills,
                     labels = c("Conventional", "Reduced", "No-till"),
                     name = "Tillage type") +
  scale_shape_manual(breaks = c("AgCensus", "Transect"),
                     values = c(16, 0),
                     name="Data source") +
  scale_x_continuous(name="Year", breaks=seq(2015,2022,1))+
  ylab("% of county acres") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text=element_text(size=9),
    axis.title=element_text(size=12, face="bold"),
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
    strip.text=element_text(size=10),
    strip.background = element_rect(fill="gray95"),
    legend.text=element_text(size=11),
    legend.title=element_text(size=10, face="bold"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size = unit(0.4, "cm")
  )	 

ggsave("plots/adoption/tillage_3sources_scatter.png", width=8, height=4.5, dpi=300)



