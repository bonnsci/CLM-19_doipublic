# This script analyzes NO3- loss data from DNDC model simulations for CLM-19 FFAR-001.
# started 2/1/2024 by BMM
# The script re-arranges the data, summarizes it, uses ANOVA and Tukey's HSD to generate
# letters for putting on bar charts.



library(tidyverse) # has ggplot, dplyr, etc.

library(reshape2)

se <- function(x) sd(x) / sqrt(length(x))


# if you only need annual totals skip to ndatyr below

# # if you need daily estimates use this:
# # data in the folder data/large_data/ are too big to share in github repo
# # only saved to Bonnie's computer (backed up by onedrive)
# ndat <- read.csv("data/large_data/daily N/CA_almonds_day_soil_n.csv") 
# beepr::beep(sound=8)
# N UNITS are in kg N / ha per day

# unique(ndat$crop_system_name)
# unique(ndat$management_name)
# 
# # sum data by year
# ndatyr <- ndat %>%
#   group_by(site_name, crop_system_name, management_name, climate_scenario, Year) %>%
#   summarize(NO3.yr = sum(NO3.leach))
# 
# # # clean up
# rm(ndat)
# 
# colnames(ndatyr)[c(3,5)] <- c("management", "year")

# write.csv(ndatyr, "data/water, nitrate, sediments/CA_alm_nitrate_annualtotals.csv", row.names=F)

ndatyr <- read.csv("data/water, nitrate, sediments/CA_alm_nitrate_annualtotals.csv")


ndatyr <- ndatyr[ndatyr$year>2021 & ndatyr$year<2073 & ndatyr$climate_scenario=="rcp60",]


ndatyr$till <- ifelse(grepl("ct-", ndatyr$management), "CT", 
                     ifelse(grepl("rt-", ndatyr$management), "RT", 
                            ifelse(grepl("nt-", ndatyr$management), "NT", "NA")))
# # check
# unique(ndatyr$till)

# dummy for CC or NC
ndatyr$cc <- ifelse(grepl("-nc", ndatyr$management), "NC", 
                   ifelse(grepl("-bc", ndatyr$management), "TC", 
                          ifelse(grepl("-lc", ndatyr$management), "LC", "NA")))  #triticale, legume


# # check
# unique(ndatyr$cc)

# dummy for N treatment
ndatyr$nfert <- ifelse(grepl("cn", ndatyr$management), "Conventional N", 
                      ifelse(grepl("rn", ndatyr$management), "Reduced N", "NA"))

ndatyr$system <- ifelse(grepl("-a", ndatyr$crop_system), "alley", "crop")



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

# math:
# cn-ct-bc = 0.45(NO3- from CN crop row) + 0.55(NO3- from ct-bc alley)

# re-arrange data to do math
# split up data by system # i don't think rcast or pivot_wider works here but maybe I'm thinking about it wrong

ndatyr.a <- ndatyr[ndatyr$system=="alley",]
colnames(ndatyr.a)[6] <- "NO3.yr.alley"
ndatyr.a <- ndatyr.a[,c(1,4:8,11)]

ndatyr.c <- ndatyr[ndatyr$system=="crop",]
colnames(ndatyr.c)[6] <- "NO3.yr.crop"
ndatyr.c <- ndatyr.c[,c(1,4:6,9,11)]

ndatyrw <- full_join(ndatyr.a, ndatyr.c, by=join_by(site_name, climate_scenario, year, decade),
                     suffix=c(".x", ".y"),
                     multiple="all",
                     relationship="many-to-many")

# expect to have twice as many rows as ndatyr.a - basically need two copies of ndatyr.a - 
# one copy to combine with Conventional N and one copy to combine with Reduced N.

rm(ndatyr.a, ndatyr.c)

# do the math to combine alley and row nitrate losses
ndatyrw$NO3.yrtot <- (0.45*ndatyrw$NO3.yr.crop) + (0.55*ndatyrw$NO3.yr.alley)
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
# Call:
#   lm(formula = NO3.sitemean ~ till:cc:nfert, data = ndat_50yr.site)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -62.610 -15.526   5.726  11.605  80.503 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      24.4736     5.9952   4.082 5.88e-05 ***
#   tillCT:ccLC:nfertConventional N  75.6787     8.4785   8.926  < 2e-16 ***
#   tillNT:ccLC:nfertConventional N  23.3823     8.4785   2.758 0.006216 ** 
#   tillRT:ccLC:nfertConventional N  65.7276     8.4785   7.752 1.85e-13 ***
#   tillCT:ccNC:nfertConventional N  30.4502     8.4785   3.591 0.000391 ***
#   tillNT:ccNC:nfertConventional N  29.6326     8.4785   3.495 0.000554 ***
#   tillRT:ccNC:nfertConventional N  29.8465     8.4785   3.520 0.000506 ***
#   tillCT:ccTC:nfertConventional N  23.9815     8.4785   2.829 0.005027 ** 
#   tillNT:ccTC:nfertConventional N  23.3823     8.4785   2.758 0.006216 ** 
#   tillRT:ccTC:nfertConventional N  23.5621     8.4785   2.779 0.005835 ** 
#   tillCT:ccLC:nfertReduced N       52.1166     8.4785   6.147 2.83e-09 ***
#   tillNT:ccLC:nfertReduced N       -0.1798     8.4785  -0.021 0.983097    
# tillRT:ccLC:nfertReduced N       42.1655     8.4785   4.973 1.17e-06 ***
#   tillCT:ccNC:nfertReduced N        6.8881     8.4785   0.812 0.417265    
# tillNT:ccNC:nfertReduced N        6.0705     8.4785   0.716 0.474615    
# tillRT:ccNC:nfertReduced N        6.2844     8.4785   0.741 0.459205    
# tillCT:ccTC:nfertReduced N        0.4194     8.4785   0.049 0.960586    
# tillNT:ccTC:nfertReduced N       -0.1798     8.4785  -0.021 0.983097    
# tillRT:ccTC:nfertReduced N            NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 23.98 on 270 degrees of freedom
# Multiple R-squared:  0.4775,	Adjusted R-squared:  0.4446 
# F-statistic: 14.51 on 17 and 270 DF,  p-value: < 2.2e-16

summary(no3aov)
# Df Sum Sq Mean Sq F value Pr(>F)    
# till:cc:nfert  17 141879    8346   14.51 <2e-16 ***
#   Residuals     270 155271     575                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukout <- TukeyHSD(no3aov)
cld <- multcompView::multcompLetters4(no3aov, Tukout)
cld <- as.data.frame.list(cld$`till:cc:nfert`)
ndat_50yr.global$cld <- cld$Letters

#N treatment rates in lb/ac
# Conventional
(3*82.2)*2.20462/2.47105 # 220 lb/ac
# Reduced
(3*57.5)*2.20462/2.47105 # 154 lb/ac

############ make graph

windows(xpinch=200, ypinch=200, width=5, height=5)


ggplot(data=ndat_50yr.global, 
       aes(x=nfert, y=NO3.mean, fill=nfert)) +   # fill=variable
  geom_bar(stat="identity", position=position_dodge(), show.legend=F) + # color="#332288", 
  geom_errorbar(aes(ymin=NO3.mean-NO3.se, ymax=NO3.mean+NO3.se), width=0.3, position=position_dodge(0.9)) +
  facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))), 
             cols=vars(factor(cc, levels=c("NC", "TC", "LC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c(NC="No Cover Crop",TC="Triticale Cover", LC="Legume Cover", 
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
                 # "Fall N" = "Fall\nN", "High N" = "High\nN", "Recommended N"="Recm'd\nN"))) +
  scale_fill_manual(values=c("#20243d", "#C2e4ef")) +
  xlab("N management") +
  ylab("Mean annual nitrate loss (lb N per ac) 2022 to 2072") +
  scale_x_discrete(breaks=c("Conventional N", "Reduced N"),
                    labels=c("220 lb N\nper acre", "154 lb N\nper acre")) +
  geom_text(aes(x=nfert, y=NO3.mean+20, label=cld), size=5, fontface="bold") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/CA_alm_NO3 losses mean annual bars lbac.png", width=7, height=6, dpi=300)




############################### N LOSSES PER decade PLOT


windows(xpinch=200, ypinch=200, width=5, height=5)


pal6 <- c("#eaeccc", "#FEDa8B", "#FDb366", "#f67e4b","#dd3d2d", "#a50026")   
pal6blue <- c("#ffffff", "#ceced5", "#9f9fac", "#727284", "#48495f", "#20233c")
                   

ggplot(data=ndat_dec.global, aes(x=nfert, y=NO3.mean, fill=decade)) +
  geom_bar(stat="identity", position=position_dodge(), color="#20233d") +
  geom_errorbar(aes(ymin=NO3.mean-NO3.se, ymax=NO3.mean+NO3.se), width=0.3, position=position_dodge(0.9)) +
  facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))), 
             cols=vars(factor(cc, levels=c("NC", "TC", "LC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c(NC="No Cover Crop",TC="Triticale Cover", LC="Legume Cover", 
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  # "Fall N" = "Fall\nN", "High N" = "High\nN", "Recommended N"="Recm'd\nN"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
  scale_fill_manual(values=pal6blue) +
  xlab("N management") +
  ylab("Mean annual nitrate loss (lb N per acre) 2022 to 2072") +
  scale_x_discrete(breaks=c("Conventional N", "Reduced N"),
                   labels=c("220 lb N\nper acre", "154 lb N\nper acre")) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/CA_alm_NO3 losses mean annual bars blue.png", width=7, height=5, dpi=300)

