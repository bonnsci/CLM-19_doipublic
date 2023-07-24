# started 7/20/23

library(tidyverse)
library(reshape2)

# units are metric = Celsius and mm for precip


################
################
################ SPLIT UP DATA BY SITES ##### ONLY RE-RUN IF DATA ARE UPDATED
# # load data
# dat <- read.csv("data/large_data/clm_data_unpacked.csv")
# # takes a minute to load, big file: 667 MB

# unique(dat$model)
# # [1] "IPSL-CM5A-LR" - just the one model we're working with

# # extract Illinois climate data from 32 sites (16 north and 16 south)
# # site names
# sites_il <- grep("IL-", unique(dat$name), value=T)
# # subset data
# clm_il <- dat[dat$name %in% sites_il,]
# # save data
# write.csv(clm_il, "data/large_data/clm_il.csv", row.names=F)
# 
# # extract NY climate data from 16 sites
# # site names
# sites_ny <- grep("f_", unique(dat$name), value=T) # "f" for forage
# # subset data
# clm_ny <- dat[dat$name %in% sites_ny,]
# # save data
# write.csv(clm_ny, "data/large_data/clm_ny.csv", row.names=F)
# 
# # extract almond climate data from 16 sites
# # site names
# sites_alm <- grep("a_", unique(dat$name), value=T) # "a" for almond
# # subset data
# clm_alm <- dat[dat$name %in% sites_alm,]
# # save data
# write.csv(clm_alm, "data/large_data/clm_alm.csv", row.names=F)
# 
# # extract vineyard grape climate data from 16 sites
# # site names
# sites_vin <- grep("v_", unique(dat$name), value=T) # "v" for vineyard
# # subset data
# clm_vin <- dat[dat$name %in% sites_vin,]
# # save data
# write.csv(clm_vin, "data/large_data/clm_vin.csv", row.names=F)
# 
# # extract hops climate data from 16 sites
# # site names
# sites_hop <- grep("h_", unique(dat$name), value=T) # "h" for hops
# # subset data
# clm_hop <- dat[dat$name %in% sites_hop,]
# # save data
# write.csv(clm_hop, "data/large_data/clm_hop.csv", row.names=F)

################
################
################
############################ SET UP THE DATA


dat <- read.csv("data/large_data/clm_il.csv")
# can get rid of row numbers, model, #### double check the column numbers below
dat <- dat[,c(3,5:11)]

se <- function(x) {
  sd(x)/sqrt(length(x))
}

# average and se of min, avg, max temp and precip by scenario and site

# years are 2006-2085
# let's characterize by decade to start
# DNDC goes to 2070s

# add decade variable
dat$decade <- ifelse(dat$year <2011, "2000s", 
                     ifelse(dat$year <2021, "2010s",
                            ifelse(dat$year <2031, "2020s",
                                   ifelse(dat$year <2041, "2030s",
                                          ifelse(dat$year <2051, "2040s",
                                                 ifelse(dat$year <2061, "2050s",
                                                        ifelse(dat$year <2071, "2060s",
                                                               ifelse(dat$year <2081, "2070s", "2080s"))))))))



# convert doy to dates # Takes a sec to run
dat <- dat %>%
              mutate(date_ = as.Date(doy-1, origin=paste0(year, "-01-01")),  # subtract 1 b/c R uses a 0 base index
                     month = strftime(date_, "%m"),
                     day = strftime(date_, "%d")) 

# assign seasons: MAM, JJA, SON, DJF
# https://www.ncei.noaa.gov/news/meteorological-versus-astronomical-seasons#:~:text=The%20Meteorological%20Seasons,-Meteorologists%20and%20climatologists&text=Meteorological%20spring%20in%20the%20Northern,December%2C%20January%2C%20and%20February.

dat$season <- ifelse(dat$month %in% c("03", "04", "05"), "Spring", 
                     ifelse(dat$month %in% c("06", "07", "08"), "Summer",
                            ifelse(dat$month %in% c("09", "10", "11"), "Fall", "Winter")))
dat$season <- ordered(dat$season, levels=c("Winter", "Spring", "Summer", "Fall"))                          






############
############
############
#################################################### annual data & plots
dat.ann <- dat %>%
  group_by(name, scenario, decade, year) %>%
  summarize(meanann.min_temp = mean(min_temp),
            meanann.avg_temp = mean(avg_temp),
            meanann.max_temp = mean(max_temp),
            totann.precip = sum(precip)) %>%
  ungroup()
# the below message is correct, it grouped the **output** not the **data** like so:
# `summarise()` has grouped output by 'name', 'scenario'. You can override using the `.groups` argument.




# a few plots of annual data
# first melt temp data to long form
dat.ann.long <- melt(dat.ann, id.vars=c("name", "scenario", "decade", "year"), value.name="value")

# compare to IL climate normals 1991-2020
# study counties •	Champaign (3), Effingham (1), Greene (4), Henry (2), Kankakee (2), Knox (4), Livingston (3), Macoupin (2), Madison (3), McDonough (1), Montgomery (1), Sangamon (2), Schuyler (3), Stark (1) –selected by corn and soy acres
# Livingston kind of a central county for North IL // Montgomery central for the southern group of counties
# Get annual normals here: https://www.ncei.noaa.gov/access/us-climate-normals/#dataset=normals-annualseasonal&timeframe=30&station=USC00112500

# normals for Livingston Co. Station name: DWIGHT, IL (USC00112500) 
# ann-tavg 49.3 F = 9.61C
# ann-tmin 38.8 F = 3.78C
# ann-tmax 59.9 F = 15.5C
# ann-prcp-normal 37.69 (inches) = 957.33 mm
# normals for Montgomery Co. MORRISONVILLE, IL US (USC00115841)
# ann-tavg 53.6 F = 12.00C
# ann-tmin 43.4 F = 6.33C
# ann-tmax 63.8 F = 17.67C
# ann-prcp-normal 38.12 (inches) = 968.25 mm
# What was the drought in 2012 annual precip?
# get county data here: https://www.ncei.noaa.gov/cdo-web/
dat12 <- read.csv("data/climate data not large/IL_counties_2012_precip_temp.csv")
min(na.omit(dat12$PRCP))  #  554.8
mean(na.omit(dat12$PRCP)) # 778.0291
max(na.omit(dat12$TMAX)) # 20.3
mean(na.omit(dat12$TMAX)) #  18.73636
mean(na.omit(dat12$TAVG)) # 12.60455

windows(xpinch=200, ypinch=200, width=5, height=5)

# temp north
ggplot(data=dat.ann.long[dat.ann.long$variable %in% c("meanann.min_temp", 
                                                      "meanann.avg_temp", 
                                                      "meanann.max_temp") &
         dat.ann.long$name %in% c("IL-n_1", "IL-n_2", "IL-n_3", "IL-n_4", "IL-n_5") &
         dat.ann.long$year > 2022 & dat.ann.long$year <2073, ],  # can't look at all sites at once too many
       aes(x=year, y=value, color=variable)) + 
  geom_hline(yintercept=9.61, linewidth=0.3, color="blue") + # Livingston normal for avg temp
  geom_hline(yintercept=12.6, linewidth=0.3, color="red") + # avg TAVG in 2012
  geom_line(linewidth=0.5) +
  ylab("Temp (deg C)") +
  annotate("text", label="TAVG Normal 1991-2020", x=2030, y=8.5, hjust=0, color="blue", size=2) +
  annotate("text", label="TAVG 2012", x=2030, y=13.5, hjust=0, color="red", size=2) +
  facet_grid(rows=vars(name), cols=vars(scenario)) + 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank()  #,
    # axis.text.x=element_text(size=10),
    # axis.text.y=element_text(size=10),
    # axis.title.x=element_text(size=14, face="bold"),
    # axis.title.y=element_text(size=14, face="bold"),
    # panel.background = element_rect(fill = 'white') ,
    # panel.border=element_rect(color="grey50", fill=NA, size=0.5),
    # strip.text=element_text(size=12, face="bold"),
    # legend.text=element_text(size=11),
    # legend.title=element_text(size=12, face="bold"),
    # plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    # legend.key=element_rect(fill="white"),
    # legend.key.size = unit(0.4, "cm")
  )	 

ggsave("plots/climate/IL-N_temp_meanann_line.png")

# precip north
ggplot(data=dat.ann.long[dat.ann.long$variable %in% "totann.precip" &
                           dat.ann.long$name %in% c("IL-n_1", "IL-n_2", "IL-n_3", "IL-n_4", "IL-n_5") &  # can't look at all sites at once too many
       dat.ann.long$year > 2022 & dat.ann.long$year <2073, ], 
       aes(x=year, y=value)) + 
  geom_hline(yintercept=957, linewidth=0.3, color="blue") + # Livingston normal
  geom_hline(yintercept=778, linewidth=0.3, color="orange") + 
  geom_hline(yintercept=554, linewidth=0.3, color="red") + 
  geom_line(linewidth=0.3) + 
  ylab("Annual precip (mm)") +
  annotate("text", label="Normal 1991-2020", x=2020, y=1020, hjust=0, color="blue", size=2) +
  annotate("text", label="Avg 2012", x=2020, y=840, hjust=0, color="orange", size=2) +
  annotate("text", label="Min 2012", x=2020, y=620, hjust=0, color="red", size=2) +
  facet_grid(rows=vars(name), cols=vars(scenario)) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank()  #,
    # axis.text.x=element_text(size=10),
    # axis.text.y=element_text(size=10),
    # axis.title.x=element_text(size=14, face="bold"),
    # axis.title.y=element_text(size=14, face="bold"),
    # panel.background = element_rect(fill = 'white') ,
    # panel.border=element_rect(color="grey50", fill=NA, size=0.5),
    # strip.text=element_text(size=12, face="bold"),
    # legend.text=element_text(size=11),
    # legend.title=element_text(size=12, face="bold"),
    # plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    # legend.key=element_rect(fill="white"),
    # legend.key.size = unit(0.4, "cm")
  )	 

ggsave("plots/climate/IL-N_precip_anntot_line.png")

# temp south
ggplot(data=dat.ann.long[dat.ann.long$variable %in% c("meanann.min_temp", 
                                                      "meanann.avg_temp", 
                                                      "meanann.max_temp") &
                           dat.ann.long$name %in% c("IL-s_1", "IL-s_2", "IL-s_3", "IL-s_4", "IL-s_5") &  # can't look at all sites at once too many
                           dat.ann.long$year > 2022 & dat.ann.long$year <2073, ], 
       aes(x=year, y=value, color=variable)) + 
  geom_hline(yintercept=12, linewidth=0.3, color="blue") + # Montgomery normal for avg temp
  geom_hline(yintercept=12.6, linewidth=0.3, color="red") + # avg TAVG in 2012
  geom_line(linewidth=0.3) + 
  ylab("Temp (deg C)") +
  annotate("text", label="Normal 1991-2020", x=2020, y=11, hjust=0, color="blue", size=2) +
  annotate("text", label="TAVG 2012", x=2020, y=13.5, hjust=0, color="red", size=2) +
  facet_grid(rows=vars(name), cols=vars(scenario)) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank()  #,
    # axis.text.x=element_text(size=10),
    # axis.text.y=element_text(size=10),
    # axis.title.x=element_text(size=14, face="bold"),
    # axis.title.y=element_text(size=14, face="bold"),
    # panel.background = element_rect(fill = 'white') ,
    # panel.border=element_rect(color="grey50", fill=NA, size=0.5),
    # strip.text=element_text(size=12, face="bold"),
    # legend.text=element_text(size=11),
    # legend.title=element_text(size=12, face="bold"),
    # plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    # legend.key=element_rect(fill="white"),
    # legend.key.size = unit(0.4, "cm")
  )	 


ggsave("plots/climate/IL-S_temp_meanann_line.png")

# precip south
ggplot(data=dat.ann.long[dat.ann.long$variable %in% "totann.precip" &
                           dat.ann.long$name %in% c("IL-s_1", "IL-s_2", "IL-s_3", "IL-s_4", "IL-s_5") &
                           dat.ann.long$year > 2022 & dat.ann.long$year <2073, ], # can't look at all sites at once too many
       aes(x=year, y=value)) + 
  geom_hline(yintercept=968.25, linewidth=0.3, color="blue") + # Montgomery normal
  geom_hline(yintercept=778, linewidth=0.3, color="orange") +
  geom_hline(yintercept=554, linewidth=0.3, color="red") +
  annotate("text", label="Normal 1991-2020", x=2020, y=1020, hjust=0, color="blue", size=2) +
  annotate("text", label="Avg 2012", x=2020, y=840, hjust=0, color="orange", size=2) +
  annotate("text", label="Min 2012", x=2020, y=620, hjust=0, color="red", size=2) +
  geom_line(linewidth=0.3) +
  ylab("Annual precip (mm)") +
  facet_grid(rows=vars(name), cols=vars(scenario)) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank()  #,
    # axis.text.x=element_text(size=10),
    # axis.text.y=element_text(size=10),
    # axis.title.x=element_text(size=14, face="bold"),
    # axis.title.y=element_text(size=14, face="bold"),
    # panel.background = element_rect(fill = 'white') ,
    # panel.border=element_rect(color="grey50", fill=NA, size=0.5),
    # strip.text=element_text(size=12, face="bold"),
    # legend.text=element_text(size=11),
    # legend.title=element_text(size=12, face="bold"),
    # plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    # legend.key=element_rect(fill="white"),
    # legend.key.size = unit(0.4, "cm")
  )	 

ggsave("plots/climate/IL-S_precip_anntot_line.png")


# are the stations different? 
# are the RCPs different?
# eyeballing the above graphs it seems the stations show similar overall patterns year to year with slightly different y-intercepts
# averaging them could lose some granularity when comparing to DNDC output by site
# when analyzing DNDC data compared to weather should compare by site, since the site-specific data were used


# ideas for characterizing periods
# annual and monthly maxima of daily temp and rainfall
# 90th percentile daily precip events
# look at monthly normals for hottest, coldest, wettest, driest months
# focus in on those months.
# https://www.ncei.noaa.gov/access/us-climate-normals/#dataset=normals-monthly&timeframe=30&station=USC00112500
# hottest month of growing season, May-Sept: July
# coldest month of GS: May
# wettest month of GS: May
# driest month of GS: Sept
# Seasonal total rainfall
# number of rainy days per season
# Rainfall intensity (mm/rainy days)


###############
###############
###############
############################## SEASONAL PRECIP ANALYSES

# seasonal total rainfall
# number of rainy days per season
# Rainfall intensity (mm/rainy days)
# Seasonal normals for Dwight, IL
# winter = 5.55 in = 140.97
# spring = 10.94 in = 277.876
# summer = 12.18 in = 309.372
# fall = 9.02 in = 229.108

precip <- dat %>%
  group_by(name, scenario, year, season) %>%
  summarize(ssn.tot = sum(precip),
            rainyd = sum(precip>0),  # count the rows where precip >0
            intens = ssn.tot/rainyd) %>%
  ungroup()

# put data in long form for plotting
precipl <- melt(precip, id.vars = c("name", "scenario", "year", "season"), value.name="value")


hlinedat <- data.frame(scenario=c(rep("rcp26", 4), rep("rcp60", 4)),
                       season=rep(c("Winter", "Spring", "Summer", "Fall"),2),
                       y=rep(c(141, 278, 309, 229),2),
                       variable=rep("ssn.tot", 8)) # from seasonal normals commented in above

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=precipl[precipl$year>2020 & 
                      precipl$year<2073,], # & 
                      # precipl$name %in% c("IL-s_1"),], # &
                      # precipl$season %in% "Summer",], 
       aes(x=year, y=value, group =season)) +
  geom_point(aes(color=season), size=0.5, alpha=0.3, show.legend=F) +
  geom_hline(data=hlinedat, aes(yintercept=y), color="black", linewidth=0.4, linetype="dashed") +
  geom_smooth(method="lm", color="blue", linewidth=0.7) +
  scale_color_manual(values=c("#4477AA", "#228833", "#EE6677", "#AA3377")) +
  facet_grid(rows=vars(factor(variable, levels=c("ssn.tot", "rainyd", "intens"))), 
             cols=vars(scenario,factor(season, levels=c("Winter", "Spring", "Summer", "Fall"))), 
             scales="free_y",
             labeller = as_labeller(
               c(ssn.tot = "Total Precip (mm)", rainyd = "No. Wet Days", intens = "Precip. Intensity (mm/day)",
                 rcp26 = "RCP 2.6",rcp60= "RCP 6.0",
                 Winter = "Winter",Spring = "Spring",Summer = "Summer",Fall = "Fall")),
             switch="y") +
  labs(y=NULL) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank()  ,
    axis.text.x=element_text(size=6, angle=-40, vjust=-0.3),
    # axis.text.y=element_text(size=10),
    # axis.title.x=element_text(size=14, face="bold"),
    # axis.title.y=element_text(size=14, face="bold"),
    panel.background = element_rect(fill = 'gray95') ,
    strip.background = element_blank(),  
    strip.placement = "outside",
    # panel.border=element_rect(color="grey50", fill=NA, size=0.5),
    strip.text=element_text(size=8) # ,
    # legend.text=element_text(size=11),
    # legend.title=element_text(size=10) # ,
    # plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    # legend.key=element_rect(fill="white"),
    # legend.key.size = unit(0.4, "cm")
  )	 

ggsave("plots/climate/IL_precip_ssn.png")


# let's mean-center the year
center_scale <- function(x) {
  scale(x, scale=F)
}

precip$year.sc <- center_scale(precip$year)

plm1 <- lm(ssn.tot~ year.sc + scenario + factor(season, ordered=F) + year.sc:scenario:factor(season, ordered =F), dat=precip)
summary(plm1)
# DO I really need to show if something is changing significantly? Might be too detailed
# For reports, AGU talk etc.
# For now, have data ready to be manipulated as needed. Will come back.


# Ideas for next steps:
# Could calculate Palmer Drought Severity Index with ET
# R package for PDSI https://www.rdocumentation.org/packages/scPDSI/versions/0.1.3/topics/pdsi
# could find the 99%ile heavy storms for 2010-2020
# find number of days of precip per year exceed 99% in future
# similar for temp: hottest 99% temp for 2010-2020
# find number of days exceed 99% for future years.








############
############
############
################################# SEASONAL TEMPERATURE ANALYSES


### DWIGHT, IL seasonal normals
# max temps
# Winter 33.9F 1.06C
# Spring 60.1  15.61C
# Summer 82.2  27.89C
# Fall 63.2   17.33

# min temps
# Winter 17.0   -8.33
# Spring 37.6   3.11
# Summer 60.2   15.67
# Fall 40.3     4.61

# avg temps
# Winter 25.5  -3.61
# Spring 48.9  9.39
# Summer 71.2  21.78
# Fall 51.8   11



tempc <- dat %>%
  group_by(name, scenario, year, season) %>%
  summarize(ssn.avg = mean(avg_temp),
            ssn.min = min(min_temp),
            ssn.max = max(max_temp)) %>%
  ungroup()

# put data in long form for plotting
tempcl <- melt(tempc, id.vars = c("name", "scenario", "year", "season"), value.name="value")


hlinedat <- data.frame(scenario=rep(c(rep("rcp26", 4), rep("rcp60", 4)), 3),
                       season=rep(c("Winter", "Spring", "Summer", "Fall"),6),
                       y=c(rep(c(1.06,15.61,27.89,17.33),2), # from seasonal normals commented in above
                           rep(c(-8.33,3.11,15.67,4.61),2),
                           rep(c(-3.61,9.39,21.78,11),2)),
                       variable=c(rep("ssn.max", 8), rep("ssn.min", 8), rep("ssn.avg", 8))) 

windows(xpinch=200, ypinch=200, width=5, height=5)


ggplot(data=tempcl[tempcl$year>2020 & 
                     tempcl$year<2073,], # & 
       # tempcl$name %in% c("IL-s_1"),], # &
       # tempcl$season %in% "Summer",], 
       aes(x=year, y=value, group =season)) +
  geom_point(aes(color=season), size=0.5, alpha=0.3, show.legend=F) +
  geom_hline(data=hlinedat, aes(yintercept=y), color="black", linewidth=0.4, linetype="dashed") +
  geom_smooth(method="lm", color="blue", linewidth=0.7) +
  scale_color_manual(values=c("#4477AA", "#228833", "#EE6677", "#AA3377")) +
  facet_grid(rows=vars(factor(variable, levels=c("ssn.max", "ssn.min", "ssn.avg"))), 
             cols=vars(scenario,factor(season, levels=c("Winter", "Spring", "Summer", "Fall"))), 
             scales="free_y",
             labeller = as_labeller(
               c(ssn.max = "Maximum T deg C", ssn.min = "Minimum T deg C", ssn.avg = "Mean T deg C",
                 rcp26 = "RCP 2.6",rcp60= "RCP 6.0",
                 Winter = "Winter",Spring = "Spring",Summer = "Summer",Fall = "Fall")),
             switch="y") +
  labs(y=NULL) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank()  ,
    axis.text.x=element_text(size=6, angle=-40, vjust=-0.3),
    # axis.text.y=element_text(size=10),
    # axis.title.x=element_text(size=14, face="bold"),
    # axis.title.y=element_text(size=14, face="bold"),
    panel.background = element_rect(fill = 'gray95') ,
    strip.background = element_blank(),  
    strip.placement = "outside",
    # panel.border=element_rect(color="grey50", fill=NA, size=0.5),
    strip.text=element_text(size=8) # ,
    # legend.text=element_text(size=11),
    # legend.title=element_text(size=10) # ,
    # plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    # legend.key=element_rect(fill="white"),
    # legend.key.size = unit(0.4, "cm")
  )	 

ggsave("plots/climate/IL_temp_ssn.png")








############
############
############
############ to the decades <- <- DECADES DATA TOO COARSE CAN'T SEE ANYTHING
# dat.decade <- dat %>%
#   group_by(scenario, name, decade) %>%
#   summarize(mean.min_temp=mean(min_temp), 
#             mean.avg_temp=mean(avg_temp),
#             mean.max_temp=mean(max_temp),
#             tot.precip=sum(precip)) %>%
#   ungroup()
#   
# dat.decade.long <- melt(dat.decade, id.vars=c("name", "scenario", "decade"), value.name="value")
#
# # 
# dat.decade.long$decade <- ordered(dat.decade.long$decade, levels=c("2000s", "2010s", "2020s",
#                                                             "2030s", "2040s", "2050s",
#                                                             "2060s", "2070s", "2080s"))
# 
# windows(xpinch=200, ypinch=200, width=5, height=5)
# 
# # decade temp north
# ggplot(data=dat.decade.long[dat.decade.long$variable %in% "mean.avg_temp" &
#                            dat.decade.long$name %in% c("IL-n_1", "IL-n_2", "IL-n_3", "IL-n_4", "IL-n_5"), ],  # can't look at all sites at once too many
#        aes(x=decade, y=value, fill=variable)) + 
#   geom_bar(stat="identity") +
#   geom_hline(yintercept=9.61, linewidth=0.3, color="blue") + # Livingston normal for avg temp
#   facet_grid(rows=vars(name), cols=vars(scenario))
# 
# ggsave("plots/climate/IL-N_temp_decade-mean-avg_bar.png")
# 
# # precip north
# ggplot(data=dat.decade.long[dat.decade.long$variable %in% "tot.precip" &
#                            dat.decade.long$name %in% c("IL-n_1", "IL-n_2", "IL-n_3", "IL-n_4", "IL-n_5"), ],  # can't look at all sites at once too many
#        aes(x=decade, y=value)) + 
#   geom_bar(stat="identity") + 
#   geom_hline(yintercept=957*10, linewidth=1, color="blue") + # Livingston normal
#   facet_grid(rows=vars(name), cols=vars(scenario))
# 
# ggsave("plots/climate/IL-N_precip_decade-tot-precip_bar.png")




