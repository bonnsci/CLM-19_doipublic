
library(tidyverse) # has ggplot, dplyr, etc.
library(reshape2)

se <- function(x) sd(x) / sqrt(length(x))

# if you only need annual totals skip to ndatyr below

# # if you need daily estimates use this:
# ndat <- read.csv("data/large_data/daily N/IL_corn_day_soil_n.csv")
# N UNITS are in kg N / ha per day

# sum data by year
# ndatyr <- ndat %>%
#   group_by(site_name, crop_system_name, management_name, climate_scenario, Year) %>%
#   summarize(N2O.yr = sum(N2O.flux), NO3.yr = sum(NO3.leach))
# 
# # clean up
# rm(ndat)

# colnames(ndatyr)[c(3,5)] <- c("management", "year")

# write.csv(ndatyr, "data/water, nitrate, sediments/nitrate_IL_annualtotals.csv", row.names=F)

ndatyr <- read.csv("data/water, nitrate, sediments/nitrate_IL_annualtotals.csv")


ndatyr <- ndatyr[ndatyr$year>2021 & ndatyr$year<2073 & ndatyr$climate_scenario=="rcp60",]

ndatyr$till <- ifelse(grepl("ct-", ndatyr$management), "CT", 
                      ifelse(grepl("rt-", ndatyr$management), "RT", "NT"))
# # check
# unique(ndatyr$till)

# dummy for CC or NC
ndatyr$cc <- ifelse(grepl("-cc-", ndatyr$management), "CC", "NC")
# # check
# unique(ndatyr$cc)

# dummy for N treatment
ndatyr$nfert <- ifelse(grepl("-cn", ndatyr$management), "High N", 
                       ifelse(grepl("-fn", ndatyr$management), "Fall N","Recommended N"))
# # check
# unique(ndatyr$nfert)

# dummy for decade
ndatyr$decade <- ifelse(ndatyr$year <2031, "2020s",
                        ifelse(ndatyr$year>=2031 & ndatyr$year <2041, "2030s",
                               ifelse(ndatyr$year>=2041 & ndatyr$year <2051, "2040s",
                                      ifelse(ndatyr$year>=2051 & ndatyr$year <2061, "2050s",
                                             ifelse(ndatyr$year>=2061 & ndatyr$year <2071, "2060s", "2070s")))))
# unique(ndatyr$decade)

# first sum by site and crop name, then calculate mean across crop types per site, 
# Then mean and se across sites by treatments/management combinations

ndat_tmttot <- ndatyr %>%
  group_by(site_name, crop_system_name, till, cc, nfert) %>%
  summarize(N2O.tot = sum(N2O.yr), 
            NO3.tot = sum(NO3.yr),
            N.tot = sum(N2O.yr) + sum(NO3.yr)) %>%
  group_by(site_name, till, cc, nfert) %>%
  summarize(N2O.sitemean = mean(N2O.tot),
            NO3.sitemean = mean(NO3.tot),
            Ntot.sitemean = mean(N.tot)) %>%
  group_by(till, cc, nfert) %>%
  summarize(N2O.mean = mean(N2O.sitemean), # mean across sites in each treatment combo
            N2O.se = se(N2O.sitemean), # variability across sites in each treatment combo
            NO3.mean = mean(NO3.sitemean),
            NO3.se = se(NO3.sitemean),
            Ntot.mean = mean(Ntot.sitemean),
            Ntot.se = se(Ntot.sitemean))

ndat_tmtperyr <- ndatyr %>%
  group_by(site_name, crop_system_name, till, cc, nfert) %>%
  summarize(N2O.tot = sum(N2O.yr), 
            NO3.tot = sum(NO3.yr),
            N.tot = sum(N2O.yr) + sum(NO3.yr)) %>%
  group_by(site_name, till, cc, nfert) %>%
  summarize(N2O.sitemean = mean(N2O.tot),
            NO3.sitemean = mean(NO3.tot),
            Ntot.sitemean = mean(N.tot),
            N2O.sitemean.yr = N2O.sitemean/50,
            NO3.sitemean.yr = NO3.sitemean/50,
            Ntot.sitemean.yr = Ntot.sitemean/50) %>%
  group_by(till, cc, nfert) %>%
  summarize(N2O.mean = mean(N2O.sitemean.yr), # mean across sites in each treatment combo
            N2O.se = se(N2O.sitemean.yr), # variability across sites in each treatment combo
            NO3.mean = mean(NO3.sitemean.yr),
            NO3.se = se(NO3.sitemean.yr),
            Ntot.mean = mean(Ntot.sitemean.yr),
            Ntot.se = se(Ntot.sitemean.yr))

############################### 50 YEAR TOTAL N LOSSES PLOT
# prep data for plotting
# put data in long form for plotting, 1 column for N2O, NO3, and Ntots
ndat_tmttotlong <- melt(ndat_tmttot, id=c("till", "cc", "nfert"))
# separate means from se's
ndat_tmttotlong$mean.se <- ifelse(grepl("mean", ndat_tmttotlong$variable), "mean", "se")

# make new column for se values
dat.se <- ndat_tmttotlong[ndat_tmttotlong$mean.se=="se",1:5]
colnames(dat.se)[5] <- "se"
dat.se$variable <- gsub(".se", "", dat.se$variable)
dat.mean <- ndat_tmttotlong[ndat_tmttotlong$mean.se=="mean",1:5]
colnames(dat.mean)[5] <- "mean"
dat.mean$variable <- gsub(".mean", "", dat.mean$variable)
ntotlong <- left_join(dat.mean, dat.se) # , relationship="one-to-one")
rm(dat.se, dat.mean)

windows(xpinch=200, ypinch=200, width=5, height=5)


ggplot(data=ntotlong[!ntotlong$variable=="Ntot",], aes(x=nfert, y=mean, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, position=position_dodge(0.9)) +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT", "RT"))), 
             cols=vars(factor(cc, levels=c("CC", "NC"))), 
                       #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c(CC="Has Cover Crop", NC="No Cover Crop",
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
                 #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
  scale_fill_manual(values=c("#99DDFF", "#44AA99" )) +
  xlab("N management") +
  ylab("N loss (kg/ha) 2022 to 2072") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/IL_N losses 50 yr total bars.png", width=7, height=7, dpi=300)



############################### N LOSSES PER YEAR PLOT
# prep data for plotting
# put data in long form for plotting, 1 column for N2O, NO3, and Ntots
ndat_peryrlong <- melt(ndat_tmtperyr, id=c("till", "cc", "nfert"))
# separate means from se's
ndat_peryrlong$mean.se <- ifelse(grepl("mean", ndat_peryrlong$variable), "mean", "se")

# make new column for se values
dat.se <- ndat_peryrlong[ndat_peryrlong$mean.se=="se",1:5]
colnames(dat.se)[5] <- "se"
dat.se$variable <- gsub(".se", "", dat.se$variable)
dat.mean <- ndat_peryrlong[ndat_peryrlong$mean.se=="mean",1:5]
colnames(dat.mean)[5] <- "mean"
dat.mean$variable <- gsub(".mean", "", dat.mean$variable)
npyrlong <- left_join(dat.mean, dat.se) # , relationship="one-to-one")
rm(dat.se, dat.mean)

windows(xpinch=200, ypinch=200, width=5, height=5)


ggplot(data=npyrlong[!npyrlong$variable=="Ntot",], aes(x=nfert, y=mean, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, position=position_dodge(0.9)) +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT", "RT"))), 
             cols=vars(factor(cc, levels=c("CC", "NC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c(CC="Has Cover Crop", NC="No Cover Crop",
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
  scale_fill_manual(values=c("#99DDFF", "#44AA99" )) +
  xlab("N management") +
  ylab("Mean annual N loss (kg/ha) 2022 to 2072") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/IL_N losses mean annual bars.png", width=6, height=7, dpi=300)
