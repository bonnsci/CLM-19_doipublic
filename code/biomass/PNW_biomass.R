# Biomass data from Regrow DNDC simulations
# this script adds some dummy variables to the data
# summarizes corn grain biomass C data by mean, se, cv
# makes bar charts by decade and summarizing across all years
# runs ANOVA to look for significant differences in mean and cv between till*cc*N management across all years

library(tidyverse) # has ggplot, dplyr. etc.
library(broom) # for glance()
# install.packages("car")
library(car) # for qqp
# install.packages("multCompView") 
library(multcompView) # for tukey HSD letters

################### EXTRACT DATA BY SITE (CAN SKIP UNLESS NEED TO UPDATE DATA FROM GOOGLE DRIVE)

# bm <- read.csv("data/biomass/biomass.csv")
# From Regrow's doc "AFT Final Delivery Modeling Notes"
# This file includes maximum aboveground biomass of each crop grown in each management system. 
# Management_name maps directly to the file yearly_outputs_post-weighting.csv as described in 3.2.

# Column Name       Unit          Description
# site_name         NA            String combination of one of the locations we simulated on. In the format of {x}_{y} where x refers to the region and y refers to the numeric id of that site. These will be of the format of h_{}, v_{}, a_{}, f_{}, IL-n_{}, IL-s_{} which represents the points for hops, vineyards, grapes, and forage, Illinois North, Illinois South respectively
# management_name   NA            Management code as associated with the simulations described in section 2.{x}.1
# climate_scenario  NA            Either ‘rcp26’ or ‘rcp60’ that were defined in section 1.2
# crop_system_name  NA            Name of the entire system results are for. Will be one of the following:  alfalfa, grape-a, grape-c, corn, corn-grain, corn-silage, grape-a, grape-c, hops-a, hops-c, soybean. Note that for the vine and tree crops the -c refers to the crop row, while the -a refers to the alley row.
# crop_name         NA            Name of crop within the rotation of crop system name. 
# Year              integer       Calendar year in which results are for.
# Grain.C.kgC.ha    kg C /ha      Kilograms of Carbon per hectare of grain as part of the crop.
# Leaf.C.kgC.ha     kg C /ha      Kilograms of Carbon per hectare of leaf as part of the crop
# Stem.C.kgC.ha     kg C /ha      Kilograms of Carbon per hectare of stem as part of the crop.

# interesting, so we need to convert kg C to kg dry biomass
# can probably find common %C of dry grain biomass


###############
###############
###############
############### START HERE

#######################   
#######################   (1) set up the data
#######################   

bm <- read.csv("data/biomass/biomass_hops.csv")

# DO NOT need to find literature values to convert kg C grain to kg grain.
# since we'll only be looking at them RELATIVE TO EACH OTHER, not absolute amounts...
# let's look at the C results for now
# 
# min(bm$Year) # 2013 --2013-2021 start up years, start at 2022
# max(bm$Year) # 2073

bm <- bm[bm$Year>2020,]

unique(bm$site_name) # 16 sites
unique(bm$region_name) # PNW only
unique(bm$management_name) # [1] "ct-nc" "nt-nc" "rt-nc" "cn"    "on"    "ct-bc" "nt-bc" "rt-bc"  #rye cover only, conventional vs. organic N
# ct = conventional till  ---applies to alley
# nt = no till         ---applies to alley
# rt = reduced till    ---applies to alley
# -nc = no cover       ---applies to alley
# cn = conventional N  ---applies to crop row -- crop rows and alleys wer modeled separately
# so we will not be able to see the effect of cover crops and no till on grape biomass
# on = organic N (compost)      ---applies to crop row
# -bc = basic (rye) cover crop        ---applies to alley
unique(bm$crop_system_name) # "hops-a" "hops-c"
# -a is the alley and -c is the crop


unique(bm$crop_name) # "fallow"             "hops"               "rye, winter, cover"

# make dummy factor for CT, RT, NT
bm$till <- ifelse(grepl("ct-", bm$management_name), "CT", 
                    ifelse(grepl("rt-", bm$management_name), "RT", 
                           ifelse(grepl("nt-", bm$management_name), "NT", "NA")))
# # check
# unique(bm$till)


# dummy for CC or NC
bm$cc <- ifelse(grepl("-nc", bm$management_name), "NC", 
                   ifelse(grepl("-bc", bm$management_name), "CC", "NA"))  #barley, legume


# # check
# unique(bm$cc)

# dummy for N treatment
bm$nfert <- ifelse(grepl("cn", bm$management_name), "Conventional N", 
                     ifelse(grepl("on", bm$management_name), "Organic N", "NA"))

bm$system <- ifelse(grepl("-a", bm$crop_system_name), "alley", "crop")
# # check
# unique(bm$nfert)

# dummy for decade
bm$decade <- ifelse(bm$Year <2021, "2010s",
                      ifelse(bm$Year>=2021 & bm$Year <2031, "2020s",
                        ifelse(bm$Year>=2031 & bm$Year <2041, "2030s",
                          ifelse(bm$Year>=2041 & bm$Year <2051, "2040s",
                            ifelse(bm$Year>=2051 & bm$Year <2061, "2050s",
                              ifelse(bm$Year>=2061 & bm$Year <2071, "2060s", "2070s"))))))
# unique(bm$decade)


# # add in summer precip data -- only needed if doing regression analysis, not pursued at this time
# precip <- read.csv("data/climate data not large/precipIL.csv")
# 
# precip.summer <- precip[precip$season == "Summer",]
# precip.summer <- precip.summer[,c(1:3, 5:7)]
# colnames(precip.summer)[1:2] <- c("site_name", "climate_scenario")
# 
# bm <- left_join(bm, precip.summer, 
#                    relationship = "many-to-one", 
#                    by = join_by(site_name, climate_scenario, Year==year))


# unique(bm$management_name)


se <- function(x) sd(x) / sqrt(length(x))
cv <- function(x) sd(x) / mean(x)

# is the grain C biomass significantly different with High N vs Recommended N?
# let's look at biomass by decade
biomass_summary <- bm %>%
  group_by(climate_scenario,crop_name, cc, till, nfert, decade) %>%
  summarize(biomass_mean = mean(Grain.C.kgC.ha.), 
            biomass_se = se(Grain.C.kgC.ha.))

windows(xpinch=200, ypinch=200, width=5, height=5)

# convert kg/ha to lb/ac
biomass_summary$biomass_mean.lbac <- biomass_summary$biomass_mean*2.47105/2.20462
biomass_summary$biomass_se.lbac <- biomass_summary$biomass_se*2.47105/2.20462


ggplot(data=biomass_summary[biomass_summary$crop_name == "hops",],
       aes(x=decade, y=biomass_mean.lbac, fill=nfert)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288") +
  geom_errorbar(width=0.3, aes(ymin=biomass_mean.lbac - biomass_se.lbac, ymax=biomass_mean.lbac + biomass_se.lbac),  
                               position=position_dodge(0.9),
                                color="#332288") +
  facet_grid(cols=vars(factor(climate_scenario, levels=c("rcp26", "rcp60"))),
             # cols=vars(factor(cc, levels=c("CC", "NC"))),
             labeller = as_labeller(
               c(rcp26="RCP 2.6", rcp60="RCP 6.0"))) +
  xlab("Decade") +
  ylab("grape carbon (lb/ac)") +
  scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99" ), name="N management") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/biomass/PNW_hops_biomass_bars.png", width=8, height=5, dpi=300)



################ cover crop biomass in alleys
################ cover crop SOC low for barley, low for legume in no till, because low biomass?

bm_cover_sum <- bm %>%
  filter(crop_name ==  "rye, winter, cover") %>%
  group_by(climate_scenario,crop_name, till, cc, nfert, decade) %>%
  summarize(biomass_mean = mean(Grain.C.kgC.ha.+Leaf.C.kgC.ha.+Stem.C.kgC.ha.),  # all aboveground biomass for cover crops 
            biomass_se = se(Grain.C.kgC.ha.+Leaf.C.kgC.ha.+Stem.C.kgC.ha.))  


windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=bm_cover_sum[bm_cover_sum$climate_scenario=="rcp60",],  
       aes(x=decade, y=biomass_mean)) + #, fill=crop_name)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288") +
  geom_errorbar(width=0.3, aes(ymin=biomass_mean - biomass_se, ymax=biomass_mean + biomass_se),  
                position=position_dodge(0.9),
                color="#332288") +
  facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))),
             # cols=vars(factor(cc, levels=c("BarC", "LegC"))),
             labeller = as_labeller(
               c(CT="Conventional Till", RT="Reduced Till", NT="No Till")))+   
                 # BarC= "barley Cover", LegC="Legume Cover"))) +
  xlab("Decade") +
  ylab("ABVGD carbon (kg/ha)") +
  # scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99" ), name="N management") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/biomass/PNW_rye_coverbiomass_bars.png", width=4, height=5.5, dpi=300)


