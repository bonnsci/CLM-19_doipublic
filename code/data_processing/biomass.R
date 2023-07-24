# Biomass data from Regrow DNDC simulations

library(tidyverse) # has ggplot, dplyr, etc.



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

# save site data to separate csvs

# # # extract IL data
# # site names
# sites_IL <- grep("IL-", unique(bm$site_name), value=T) # 
# # subset data
# bmIL <- bm[bm$site_name %in% sites_IL,]
# # save data
# write.csv(bmIL, "data/biomass/biomass_IL.csv", row.names=F)
# 
# # # extract NY data
# # site names
# sites_NY <- grep("f_", unique(bm$site_name), value=T) # f for forage
# # subset data
# bmNY <- bm[bm$site_name %in% sites_NY,]  # has more data because more crops
# # save data
# write.csv(bmNY, "data/biomass/biomass_NY.csv", row.names=F)
# 
# # # extract almond data
# # site names
# sites_alm <- grep("a_", unique(bm$site_name), value=T) # 
# # subset data
# bmalm <- bm[bm$site_name %in% sites_alm,]
# # save data
# write.csv(bmalm, "data/biomass/biomass_almond.csv", row.names=F)
# 
# # # extract vineyard data
# # site names
# sites_vine <- grep("v_", unique(bm$site_name), value=T) # 
# # subset data
# bmvine <- bm[bm$site_name %in% sites_vine,]
# # save data
# write.csv(bmvine, "data/biomass/biomass_vineyard.csv", row.names=F)
# 
# # # extract IL data
# # site names
# sites_hops <- grep("h_", unique(bm$site_name), value=T) # 
# # subset data
# bmhops <- bm[bm$site_name %in% sites_hops,]
# # save data
# write.csv(bmhops, "data/biomass/biomass_hops.csv", row.names=F)
# 
# # clean up
# rm(bm, bmalm, bmhops, bmIL, bmNY, bmvine, sites_alm, sites_hops, sites_IL, sites_NY, sites_vine)



###############
###############
###############
############### START HERE

bmil <- read.csv("data/biomass/biomass_IL.csv")

# need to find literature values to convert kg C grain to kg grain.
# or maybe not since we'll only be looking at them RELATIVE TO EACH OTHER, not absolute amounts...
# let's look at the C results for now

min(bmil$Year) # 2013 --2013-2021 start up years, start at 2022
max(bmil$Year) # 2073

# looks like all the cover crop biomass is in leaf and stem, zero grain

# make dummy factor for CT, RT, NT
bmil$till <- ifelse(grepl("ct-", bmil$management_name), "CT", 
                    ifelse(grepl("rt-", bmil$management_name), "RT", "NT"))
# # check
# unique(bmil$till)

# dummy for CC or NC
bmil$cc <- ifelse(grepl("-cc-", bmil$management_name), "CC", "NC")
# # check
# unique(bmil$cc)


windows(xpinch=200, ypinch=200, width=5, height=5)

# recall current data only has corn every other year and soy the opposite years
ggplot(dat=bmil, 
  data=bmil[bmil$crop_name %in% c("corn, grain", "soybean") & bmil$Year>2021, ],  # & bmil$site_name=="IL-n_10",
  aes(x=Year, y=Grain.C.kgC.ha.)) +
  geom_point(aes(color=till), size=0.5, alpha=0.5) +
  # geom_hline(data=hlinedat, aes(yintercept=y), color="black", linewidth=0.4, linetype="dashed") +
  geom_smooth(method="lm", aes(color=till), linewidth=1, se=F) +
  # scale_color_manual(values=c("#4477AA", "#228833", "#EE6677", "#AA3377")) +
  facet_grid(rows=vars(factor(crop_name, levels=c("corn, grain", "soybean"))), 
             cols=vars(factor(climate_scenario, levels=c("rcp26", "rcp60")), factor(cc, levels=c("CC", "NC"))), 
             scales="free_y",
             labeller = as_labeller(
               c(CC="Has Cover Crop", NC="No Cover Crop",
                 rcp26 = "RCP 2.6",rcp60= "RCP 6.0",
                 "corn, grain" = "Corn for grain", soybean= "Soybean"))) +
             # switch="y") +
  labs(y="Grain kg C per ha") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank()  ,
    axis.text.x=element_text(size=10, angle=-40, vjust=-0.3),
    axis.text.y=element_text(size=10),
    # axis.title.x=element_text(size=14, face="bold"),
    # axis.title.y=element_text(size=14, face="bold"),
    panel.background = element_rect(fill = 'gray95') ,
    strip.background = element_blank(),  
    strip.placement = "outside",
    # panel.border=element_rect(color="grey50", fill=NA, size=0.5),
    strip.text=element_text(size=12) # ,
    # legend.text=element_text(size=11),
    # legend.title=element_text(size=10) # ,
    # plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    # legend.key=element_rect(fill="white"),
    # legend.key.size = unit(0.4, "cm")
  )	 

ggsave("plots/biomass/IL_cornsoybean.png", width=12, height=8, dpi=300)





# are the biomass slopes and intercepts significantly different?
# set base levels to no cover and conventional till
bmil$till <- relevel(factor(bmil$till), ref="CT")
bmil$cc <- relevel(factor(bmil$cc), ref="NC")

# center data at mean
center_scale <- function(x) {
  scale(x, scale=F)
}

bmil$year.sc <- center_scale(bmil$Year)
bmil$grain.sc <- center_scale(bmil$Grain.C.kgC.ha.)


blm1 <- lm(grain.sc ~ year.sc*climate_scenario*crop_name*cc*till,
           data=bmil[bmil$crop_name %in% c("corn, grain", "soybean") & bmil$Year>2021, ])
summary(blm1)

blm2 <- lm(grain.sc ~ year.sc+ cc + till + crop_name + year.sc:cc:till, 
           data=bmil[bmil$crop_name %in% c("corn, grain", "soybean") & bmil$Year>2021, ])
summary(blm2)
# base levels are CT a