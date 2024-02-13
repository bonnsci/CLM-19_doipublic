# This script analyzes NO3- loss data from DNDC model simulations for CLM-19 FFAR-001.
# started 2/1/2024 by BMM
# The script re-arranges the data, summarizes it, uses ANOVA and Tukey's HSD to generate
# letters for putting on bar charts.



library(tidyverse) # has ggplot, dplyr, etc.

library(reshape2)

se <- function(x) sd(x) / sqrt(length(x))


# if you only need annual totals skip to ndatyr below

# if you need daily estimates use this:
# data in the folder data/large_data/ are too big to share in github repo
# only saved to Bonnie's computer (backed up by onedrive)
# ndat <- read.csv("data/large_data/daily N/PNW_hops_day_soil_n.csv")
# beepr::beep(sound=8)
# # N UNITS are in kg N / ha per day
# 
# unique(ndat$crop_system_name) # [1] "hops-a" "hops-c"
# unique(ndat$management_name) #  "ct-bc" "on"    "rt-bc" "nt-bc" "cn"    "rt-nc" "nt-nc" "ct-nc"
# 
# # sum data by year
# ndatyr <- ndat %>%
#   group_by(site_name, crop_system_name, management_name, climate_scenario, Year) %>%
#   summarize(NO3.yr = sum(NO3.leach))
# #
# # # # clean up
# rm(ndat)
# #
# colnames(ndatyr)[c(2,3,5)] <- c("system", "management", "year")
# 
# write.csv(ndatyr, "data/water, nitrate, sediments/PNW_nitrate_annualtotals.csv", row.names=F)

ndatyr <- read.csv("data/water, nitrate, sediments/PNW_nitrate_annualtotals.csv")


ndatyr <- ndatyr[ndatyr$year>2021 & ndatyr$year<2073 & ndatyr$climate_scenario=="rcp60",]


ndatyr$till <- ifelse(grepl("ct-", ndatyr$management), "CT", 
                     ifelse(grepl("rt-", ndatyr$management), "RT", 
                            ifelse(grepl("nt-", ndatyr$management), "NT", "NA")))
# # check
# unique(ndatyr$till)

# dummy for CC or NC
ndatyr$cc <- ifelse(grepl("-nc", ndatyr$management), "NC", 
                   ifelse(grepl("-bc", ndatyr$management), "CC", "NA"))  #Rye


# # check
# unique(ndatyr$cc)

# dummy for N treatment
ndatyr$nfert <- ifelse(grepl("cn", ndatyr$management), "Conventional N", 
                      ifelse(grepl("on", ndatyr$management), "Organic N", "NA"))

ndatyr$system <- ifelse(grepl("-a", ndatyr$system), "alley", "crop")



# # check
# unique(ndatyr$nfert)
# unique(ndatyr$system)


# dummy for decade
ndatyr$decade <- ifelse(ndatyr$year <2031, "2020s",
                        ifelse(ndatyr$year>=2031 & ndatyr$year <2041, "2030s",
                               ifelse(ndatyr$year>=2041 & ndatyr$year <2051, "2040s",
                                      ifelse(ndatyr$year>=2051 & ndatyr$year <2061, "2050s",
                                             ifelse(ndatyr$year>=2061 & ndatyr$year <2071, "2060s", "2070s")))))
# unique(ndatyr$decade)



################ put alleys and crop rows together for per ha estimates

# alley weight = 0.55, crop weight = 0.45

# Want something like 1 ha for each combo:
# cn-ct-bc, cn-ct-lc, cn-ct-nc
# cn-nt-bc, cn-nt-lc, cn-nt-nc
# cn-rt-bc, cn-rt-lc, cn-rt,nc

# rn-ct-bc, rn-ct-lc, rn-ct-nc
# rn-nt-bc, rn-nt-lc, rn-nt-nc
# rn-rt-bc, rn-rt-lc, rn-rt,nc

# math: example for CN row and ct-bc alley:
# cn-ct-bc = 0.2(NO3- from CN crop row) + 0.8(NO3- from ct-bc alley)

# re-arrange data to do math
# split up data by system # i don't think rcast or pivot_wider works here but maybe I'm thinking about it wrong

ndatyr.a <- ndatyr[ndatyr$system=="alley",]
colnames(ndatyr.a)[6] <- "NO3.yr.alley"
ndatyr.a <- ndatyr.a[,c(1, 5:8,10)]  # don't need nfert data

ndatyr.c <- ndatyr[ndatyr$system=="crop",]
colnames(ndatyr.c)[6] <- "NO3.yr.crop"
ndatyr.c <- ndatyr.c[,c(1, 5,6,9,10)]  # don't need till or cc data

ndatyrw <- full_join(ndatyr.a, ndatyr.c, by=join_by(site_name, year, decade),
                     suffix=c(".x", ".y"),
                     multiple="all",
                     relationship="many-to-many")  # should not have any columns that end in ".x" or ".y"

# expect to have twice as many rows as ndatyr.a - basically need two copies of ndatyr.a - 
# one copy to combine with Conventional N and one copy to combine with Reduced N.

rm(ndatyr.a, ndatyr.c)

# do the math to combine alley and row nitrate losses
ndatyrw$NO3.yrtot <- (0.2*ndatyrw$NO3.yr.crop) + (0.8*ndatyrw$NO3.yr.alley)
ndatyrw$NO3.yrtot.lbac <- ndatyrw$NO3.yrtot * 2.47105/2.20462



################ calculate means across sites


# ANNUAL MEANS ACROSS ALL YEARS for rotation
ndat_50yr.site <- ndatyrw %>%  # for stats
  group_by(site_name, till, cc, nfert) %>%
  summarize(NO3.sitemean = mean(NO3.yrtot.lbac)) # mean annual N loss per site (mean across 50 years at each site)

ndat_50yr.global <- ndat_50yr.site %>%    # for plotting means
  group_by(till, cc, nfert) %>%  # drop sitename to get mean across sites
  summarize(NO3.mean = mean(NO3.sitemean),# mean of the site means in each treatment combo
             NO3.se = se(NO3.sitemean)) %>%# variability across sites in each treatment combo
  arrange(desc(NO3.mean))
            
            

# ANNUAL MEANS BY DECADE for rotation
ndat_dec.site <- ndatyrw %>%  # site means
  group_by(site_name, till, cc, nfert, decade) %>%
  summarize(NO3.sitemean = mean(NO3.yrtot.lbac))  # mean annual N loss per site per decade per treatment combo

ndat_dec.global <- ndat_dec.site %>%  # for plotting means  
  group_by(till, cc, nfert, decade) %>%  # drop sitename to get means across sites 
  summarize(NO3.mean = mean(NO3.sitemean), # mean across sites in each treatment combo
            NO3.se = se(NO3.sitemean)) # variability across sites in each treatment combo
            
            
rm(ndatyr, ndatyrw)


############################### NO3- losses average 2022-2072


### get letters for the bars



no3aov <- aov(NO3.sitemean ~ till:cc:nfert, data=ndat_50yr.site)  # means are already in lb/ac
no3lm <- lm(NO3.sitemean~ till:cc:nfert, data=ndat_50yr.site)

summary(no3lm)
# 
# Call:
#   lm(formula = NO3.sitemean ~ till:cc:nfert, data = ndat_50yr.site)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -24.755 -16.725  -2.438  17.486  32.908 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      37.6437     4.6087   8.168 5.35e-14 ***
#   tillCT:ccCC:nfertConventional N  -3.1716     6.5177  -0.487   0.6271    
# tillNT:ccCC:nfertConventional N  -8.7298     6.5177  -1.339   0.1821    
# tillRT:ccCC:nfertConventional N  -6.7851     6.5177  -1.041   0.2993    
# tillCT:ccNC:nfertConventional N   7.7047     6.5177   1.182   0.2387    
# tillNT:ccNC:nfertConventional N   4.2838     6.5177   0.657   0.5119    
# tillRT:ccNC:nfertConventional N   5.0230     6.5177   0.771   0.4419    
# tillCT:ccCC:nfertOrganic N       -8.1946     6.5177  -1.257   0.2103    
# tillNT:ccCC:nfertOrganic N      -13.7528     6.5177  -2.110   0.0362 *  
#   tillRT:ccCC:nfertOrganic N      -11.8081     6.5177  -1.812   0.0717 .  
# tillCT:ccNC:nfertOrganic N        2.6817     6.5177   0.411   0.6812    
# tillNT:ccNC:nfertOrganic N       -0.7392     6.5177  -0.113   0.9098    
# tillRT:ccNC:nfertOrganic N            NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 18.43 on 180 degrees of freedom
# Multiple R-squared:  0.1248,	Adjusted R-squared:  0.07133        # low R-squared
# F-statistic: 2.334 on 11 and 180 DF,  p-value: 0.01047            # p is significant but higher than other systems' same models
summary(no3aov)
# Df Sum Sq Mean Sq F value Pr(>F)  
# till:cc:nfert  11   8724   793.1   2.334 0.0105 *
#   Residuals     180  61171   339.8                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukout <- TukeyHSD(no3aov)
cld <- multcompView::multcompLetters4(no3aov, Tukout)
cld <- as.data.frame.list(cld$`till:cc:nfert`)
ndat_50yr.global$cld <- cld$Letters

#N treatment rates in lb/ac
# Conventional
(8*18)*2.20462/2.47105 # 128.5 lb/ac
# Organic
(2*72)*2.20462/2.47105 # 128.5 lb/ac

############ make graph

windows(xpinch=200, ypinch=200, width=5, height=5)


ggplot(data=ndat_50yr.global, 
       aes(x=nfert, y=NO3.mean, fill=nfert)) +   # fill=variable
  geom_bar(stat="identity", position=position_dodge(), show.legend=F) + # color="#332288", 
  geom_errorbar(aes(ymin=NO3.mean-NO3.se, ymax=NO3.mean+NO3.se), width=0.3, position=position_dodge(0.9)) +
  facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))), 
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c(NC="No Cover Crop",CC="Rye Cover", 
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
                 # "Fall N" = "Fall\nN", "High N" = "High\nN", "Recommended N"="Recm'd\nN"))) +
  scale_fill_manual(values=c("#20243d", "#C2e4ef")) +
  xlab("N management") +
  ylab("Mean annual nitrate loss (lb N per ac) 2022 to 2072") +
  # scale_x_discrete(breaks=c("Conventional N", "Organic N"),
  #                   labels=c("50 lb N\nper acre", "40 lb N\nper acre")) +
  geom_text(aes(x=nfert, y=NO3.mean+30, label=cld), size=5, fontface="bold") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/PNW_NO3 losses 22-72 mean bars lbac.png", width=7, height=6, dpi=300)




############################### N LOSSES PER decade PLOT


windows(xpinch=200, ypinch=200, width=5, height=5)


# pal6 <- c("#eaeccc", "#FEDa8B", "#FDb366", "#f67e4b","#dd3d2d", "#a50026")   
pal6blue <- c("#ffffff", "#ceced5", "#9f9fac", "#727284", "#48495f", "#20233c")
                   

ggplot(data=ndat_dec.global, aes(x=nfert, y=NO3.mean, fill=decade)) +
  geom_bar(stat="identity", position=position_dodge(), color="#20233d") +
  geom_errorbar(aes(ymin=NO3.mean-NO3.se, ymax=NO3.mean+NO3.se), width=0.3, position=position_dodge(0.9)) +
  facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))), 
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c(NC="No Cover Crop",CC="Rye Cover", 
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  # "Fall N" = "Fall\nN", "High N" = "High\nN", "Recommended N"="Recm'd\nN"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
  scale_fill_manual(values=pal6blue) +
  xlab("N management") +
  ylab("Mean annual nitrate loss (lb N per acre) 2022 to 2072") +
  # scale_x_discrete(breaks=c("Conventional N", "Reduced N"),
  #                  labels=c("50 lb N\nper acre", "40 lb N\nper acre")) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/PNW_NO3 losses decadal mean bars blue.png", width=7, height=5, dpi=300)

