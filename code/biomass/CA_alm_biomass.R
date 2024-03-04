# Biomass data from Regrow DNDC simulations
# this script adds some dummy variables to the data
# summarizes corn grain biomass C data by mean, se, cv
# makes bar charts by decade and summarizing across all years
# runs ANOVA to look for significant differences in mean and cv between till*cc*N management across all years

library(tidyverse) # has ggplot, dplyr, etc.
library(broom) # for glance()
# install.packages("car")
library(car) # for qqp
# install.packages("multcompView") 
library(multcompView) # for tukey HSD letters

################### EXTRACT DATA BY SITE (CAN SKIP UNLESS NEED TO UPDATE DATA FROM GOOGLE DRIVE)

# bm <- read.csv("data/biomass/biomass.csv")
# From Regrow's doc "AFT Final Delivery Modeling Notes"
# This file includes maximum aboveground biomass of each crop grown in each management system. 
# Management_name maps directly to the file yearly_outputs_post-weighting.csv as described in 3.2.

# Column Name       Unit          Description
# site_name         NA            String combination of one of the locations we simulated on. In the format of {x}_{y} where x refers to the region and y refers to the numeric id of that site. These will be of the format of h_{}, v_{}, a_{}, f_{}, IL-n_{}, IL-s_{} which represents the points for hops, vineyards, almonds, and forage, Illinois North, Illinois South respectively
# management_name   NA            Management code as associated with the simulations described in section 2.{x}.1
# climate_scenario  NA            Either ‘rcp26’ or ‘rcp60’ that were defined in section 1.2
# crop_system_name  NA            Name of the entire system results are for. Will be one of the following:  alfalfa, almond-a, almond-c, corn, corn-grain, corn-silage, grape-a, grape-c, hops-a, hops-c, soybean. Note that for the vine and tree crops the -c refers to the crop row, while the -a refers to the alley row.
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

bmalm <- read.csv("data/biomass/biomass_almond_20240220.csv")

# DO NOT need to find literature values to convert kg C grain to kg grain.
# since we'll only be looking at them RELATIVE TO EACH OTHER, not absolute amounts...
# let's look at the C results for now
# 
# min(bmalm$Year) # 2013 --2013-2021 start up years, start at 2022
# max(bmalm$Year) # 2073

bmalm <- bmalm[bmalm$Year>2020,]

unique(bmalm$site_name) # 16 sites
unique(bmalm$region_name) # CA only
unique(bmalm$management_name) # "ct-nc" "nt-nc" "rt-nc" "cn"    "rn"    "ct-lc" "rt-lc" "ct-bc" "nt-bc" "nt-lc" "rt-bc"
# ct = conventional till  ---applies to alley
# nt = no till         ---applies to alley
# rt = reduced till    ---applies to alley
# -nc = no cover       ---applies to alley
# cn = conventional N  ---applies to crop row -- crop rows and alleys wer modeled separately
# so we will not be able to see the effect of cover crops and no till on almond biomass
# rn = reduced N       ---applies to crop row
# -lc = legume cover crop (Faba bean)       ---applies to alley
# -bc = basic (Triticale) cover crop        ---applies to alley
unique(bmalm$crop_system_name) # "almond-a" "almond-c"
# -a is the alley and -c is the crop

unique(bmalm$crop_name) # "fallow"           "almond"           "faba_bean"        "triticale, cover"

# make dummy factor for CT, RT, NT
bmalm$till <- ifelse(grepl("ct-", bmalm$management_name), "CT", 
                    ifelse(grepl("rt-", bmalm$management_name), "RT", 
                           ifelse(grepl("nt-", bmalm$management_name), "NT", "NA")))
# # check
# unique(bmalm$till)

b <- bmalm[bmalm$management_name %in% c("ct-lc", "rt-lc", "nt-lc"),]

# # check
# unique(bmalm$till)

# dummy for CC or NC
bmalm$cc <- ifelse(grepl("-nc", bmalm$management_name), "NC", 
                   ifelse(grepl("-bc", bmalm$management_name), "TC", 
                          ifelse(grepl("-lc", bmalm$management_name), "LC", "NA")))  #triticale, legume


# # check
# unique(bmalm$cc)

# dummy for N treatment
bmalm$nfert <- ifelse(grepl("cn", bmalm$management_name), "Conventional N", 
                     ifelse(grepl("rn", bmalm$management_name), "Reduced N", "NA"))

bmalm$fieldpart <- ifelse(grepl("-a", bmalm$crop_system_name), "alley", "crop")
# # check
# unique(bmalm$nfert)

# dummy for decade
bmalm$decade <- ifelse(bmalm$Year <2021, "2010s",
                      ifelse(bmalm$Year>=2021 & bmalm$Year <2031, "2020s",
                        ifelse(bmalm$Year>=2031 & bmalm$Year <2041, "2030s",
                          ifelse(bmalm$Year>=2041 & bmalm$Year <2051, "2040s",
                            ifelse(bmalm$Year>=2051 & bmalm$Year <2061, "2050s",
                              ifelse(bmalm$Year>=2061 & bmalm$Year <2071, "2060s", "2070s"))))))
# unique(bmalm$decade)


# # add in summer precip data -- only needed if doing regression analysis, not pursued at this time
# precip <- read.csv("data/climate data not large/precipIL.csv")
# 
# precip.summer <- precip[precip$season == "Summer",]
# precip.summer <- precip.summer[,c(1:3, 5:7)]
# colnames(precip.summer)[1:2] <- c("site_name", "climate_scenario")
# 
# bmalm <- left_join(bmalm, precip.summer, 
#                    relationship = "many-to-one", 
#                    by = join_by(site_name, climate_scenario, Year==year))


# unique(bmalm$management_name)


se <- function(x) sd(x) / sqrt(length(x))
cv <- function(x) sd(x) / mean(x)

# is the grain C biomass significantly different with High N vs Recommended N?
# let's look at biomass by decade
biomass_summary <- bmalm %>%
  group_by(climate_scenario,crop_name, cc, till, nfert, decade) %>%
  summarize(biomass_mean = mean(Grain.C.kgC.ha.), 
            biomass_se = se(Grain.C.kgC.ha.))

windows(xpinch=200, ypinch=200, width=5, height=5)

# convert kg/ha to lb/ac
biomass_summary$biomass_mean.lbac <- biomass_summary$biomass_mean*2.47105/2.20462
biomass_summary$biomass_se.lbac <- biomass_summary$biomass_se*2.47105/2.20462


ggplot(data=biomass_summary[biomass_summary$crop_name == "almond",],
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
  ylab("Almond carbon (lb/ac)") +
  scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99" ), name="N management") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/biomass/CA_alm_biomass_bars.png", width=8, height=5, dpi=300)



################ cover crop biomass in alleys
################ cover crop SOC low for Triticale, low for legume in no till, because low biomass?

bm_cover_sum <- bmalm %>%
  filter(crop_name %in% c("faba_bean", "triticale, cover")) %>%
  group_by(climate_scenario,crop_name, till, cc, nfert, decade) %>%
  summarize(biomass_mean = mean(Grain.C.kgC.ha.+Leaf.C.kgC.ha.+Stem.C.kgC.ha.),  # all aboveground biomass for cover crops 
            biomass_se = se(Grain.C.kgC.ha.+Leaf.C.kgC.ha.+Stem.C.kgC.ha.))  


windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=bm_cover_sum[bm_cover_sum$climate_scenario=="rcp60",],  
       aes(x=decade, y=biomass_mean, fill=crop_name)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288") +
  geom_errorbar(width=0.3, aes(ymin=biomass_mean - biomass_se, ymax=biomass_mean + biomass_se),  
                position=position_dodge(0.9),
                color="#332288") +
  facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))),
             cols=vars(factor(cc, levels=c("TC", "LC"))),
             labeller = as_labeller(
               c(CT="Conventional Till", RT="Reduced Till", NT="No Till", 
                 TC= "Triticale Cover", LC="Legume Cover"))) +
  xlab("Decade") +
  ylab("ABVGD carbon (kg/ha)") +
  scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99" ), name="N management") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/biomass/CA_alm_coverbiomass_bars.png", width=8, height=5, dpi=300)


