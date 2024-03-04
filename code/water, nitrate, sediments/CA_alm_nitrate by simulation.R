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
# ndat <- read.csv("data/large_data/daily N/CA_almonds_day_soil_n_20240220.csv")
# beepr::beep(sound=8)
# N UNITS are in kg N / ha per day

# unique(ndat$crop_system_name)  # "almond-a" "almond-c"
# unique(ndat$management_name)  #  "nt-nc" "cn"    "ct-nc" "rt-nc" "rt-bc" "ct-lc" "nt-lc" "nt-bc" "rt-lc" "ct-bc" "rn"  
# 
# # sum data by year
# ndatyr <- ndat %>%
#   group_by(site_name, crop_system_name, management_name, climate_scenario, Year) %>%
#   summarize(NO3.yr = sum(NO3.leach))
# 
# # # clean up
# rm(ndat)
# #
# colnames(ndatyr)[c(3,5)] <- c("management", "year")
# 
# write.csv(ndatyr, "data/water, nitrate, sediments/CA_alm_nitrate_annualtotals.csv", row.names=F)

ndatyr <- read.csv("data/water, nitrate, sediments/CA_alm_nitrate_annualtotals.csv")


ndatyr <- ndatyr[ndatyr$year>2021 & ndatyr$year<2073 & ndatyr$climate_scenario=="rcp60",]


ndatyr$till <- ifelse(grepl("ct-", ndatyr$management), "CT", 
                     ifelse(grepl("rt-", ndatyr$management), "RT", 
                            ifelse(grepl("nt-", ndatyr$management), "NT", "NA")))
# # check
# unique(ndatyr$till)  # NA is correct for row areas, till and cover crop only apply to alleys

# factor for cover crops
ndatyr$cc <- ifelse(grepl("-nc", ndatyr$management), "NC", 
                   ifelse(grepl("-bc", ndatyr$management), "TC",  # "basic cover" was triticale (but can be confused with bean cover)
                          ifelse(grepl("-lc", ndatyr$management), "LC", "NA")))  # legume cover (Faba bean)


# # check
# unique(ndatyr$cc)   # NA is correct for row areas, till and cover crop only apply to alleys

# dummy for N treatment
ndatyr$nfert <- ifelse(grepl("cn", ndatyr$management), "Conventional N", 
                      ifelse(grepl("rn", ndatyr$management), "Reduced N", "NA"))

ndatyr$system <- ifelse(grepl("-a", ndatyr$crop_system), "alley", "crop")



# # check
# unique(ndatyr$nfert)  # NA is correct for alleys
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
ndatyr.a <- ndatyr.a[,c(1,5:8,11)]  # don't need climate_scenario because filtered to only RCP60

ndatyr.c <- ndatyr[ndatyr$system=="crop",]
colnames(ndatyr.c)[6] <- "NO3.yr.crop"
ndatyr.c <- ndatyr.c[,c(1,5:6,9,11)]

ndatyrw <- full_join(ndatyr.a, ndatyr.c, by=join_by(site_name, year, decade),
                     suffix=c(".x", ".y"),
                     multiple="all",
                     relationship="many-to-many")

# expect to have twice as many rows as ndatyr.a - basically need two copies of ndatyr.a - 
# one copy to combine with Conventional N and one copy to combine with Reduced N.

rm(ndatyr.a, ndatyr.c)

# do the math to combine alley and row nitrate losses
ndatyrw$NO3.yrtot <- (0.45*ndatyrw$NO3.yr.crop) + (0.55*ndatyrw$NO3.yr.alley)
# convert kg N / ha*yr to lb N / ac*yr
ndatyrw$NO3.yrtot.lbac <- ndatyrw$NO3.yrtot * 1/(2.471*0.4536)



################ calculate means across sites


# Annual mean ACROSS ALL YEARS 
ndat.site <- ndatyrw[ndatyrw$till %in% c("CT", "NT"),] %>%  # for stats
  group_by(site_name, cc, nfert) %>%  # tillage does not have an effect on N losses so averaging across tillage
  summarize(NO3.sitemean = mean(NO3.yrtot.lbac)) # mean annual N loss per site (mean across 50 years at each site)

ndat_mean <- ndatyrw[ndatyrw$till %in% c("CT", "NT"),] %>%    # for plotting means
  group_by( cc, nfert) %>%  # drop sitename to get mean across sites, # tillage does not have an effect on N losses so averaging across tillage
  summarize(NO3.mean.lbac = mean(NO3.yrtot.lbac),# mean of the site means in each treatment combo
             NO3.se.lbac = se(NO3.yrtot.lbac)) %>%# variability across sites and years in each treatment combo
  arrange(desc(NO3.mean.lbac))
            
            

# # ANNUAL MEANS BY DECADE
# ndat_dec.site <- ndatyrw %>%  # site means
#   group_by(site_name, till, cc, nfert, decade) %>%
#   summarize(NO3.sitemean = mean(NO3.yrtot.lbac))  # mean annual N loss per site per decade per treatment combo
# 
# ndat_dec.global <- ndat_dec.site %>%  # for plotting means  
#   group_by(till, cc, nfert, decade) %>%  # drop sitename to get means across sites 
#   summarize(NO3.mean = mean(NO3.sitemean), # mean across sites in each treatment combo
#             NO3.se = se(NO3.sitemean)) # variability across sites in each treatment combo
            
            
rm(ndatyr, ndatyrw)


############################### NO3- losses average 2022-2072


### get letters for the bars



no3aov <- aov(NO3.sitemean ~ cc:nfert, data=ndat.site)  # & ndat.site$nfert=="Conventional N",])  #  till:cc:nfert
no3lm <- lm(NO3.sitemean~ cc:nfert, data=ndat.site)

summary(no3lm)
# Call:
#   lm(formula = NO3.sitemean ~ cc:nfert, data = ndat.site)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -35.854 -12.756   4.672   9.397  40.379 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                19.576      4.006   4.886 4.44e-06 ***
#   ccLC:nfertConventional N   39.330      5.666   6.942 5.80e-10 ***
#   ccNC:nfertConventional N   23.817      5.666   4.204 6.18e-05 ***
#   ccTC:nfertConventional N   18.755      5.666   3.310 0.001342 ** 
#   ccLC:nfertReduced N        20.575      5.666   3.632 0.000468 ***
#   ccNC:nfertReduced N         5.062      5.666   0.893 0.373987    
# ccTC:nfertReduced N            NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 16.02 on 90 degrees of freedom
# Multiple R-squared:  0.406,	Adjusted R-squared:  0.373 
# F-statistic:  12.3 on 5 and 90 DF,  p-value: 4.238e-09

# Call:
#   lm(formula = NO3.sitemean ~ till:cc, data = ndat.site[ndat.site$till %in% 
#                                                           c("CT", "NT") & ndat.site$nfert == "Conventional N", ])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -49.429 -12.919   5.688   9.305  64.080 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.809e+01  4.888e+00   7.792 1.09e-11 ***
#   tillCT:ccLC  4.163e+01  6.913e+00   6.021 3.70e-08 ***
#   tillNT:ccLC -1.316e-14  6.913e+00   0.000    1.000    
# tillCT:ccNC  5.626e+00  6.913e+00   0.814    0.418    
# tillNT:ccNC  4.975e+00  6.913e+00   0.720    0.474    
# tillCT:ccTC  4.769e-01  6.913e+00   0.069    0.945    
# tillNT:ccTC         NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 19.55 on 90 degrees of freedom
# Multiple R-squared:  0.3815,	Adjusted R-squared:  0.3471 
# F-statistic:  11.1 on 5 and 90 DF,  p-value: 2.399e-08

# Call:  ########## including RT and both NFerts
#   lm(formula = NO3.sitemean ~ till:cc:nfert, data = ndat.site)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -49.837 -12.358   4.558   9.238  64.080 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      19.4807     4.7721   4.082 5.88e-05 ***
#   tillCT:ccLC:nfertConventional N  60.2393     6.7488   8.926  < 2e-16 ***
#   tillNT:ccLC:nfertConventional N  18.6120     6.7488   2.758 0.006216 ** 
#   tillRT:ccLC:nfertConventional N  52.3183     6.7488   7.752 1.85e-13 ***
#   tillCT:ccNC:nfertConventional N  24.2380     6.7488   3.591 0.000391 ***
#   tillNT:ccNC:nfertConventional N  23.5872     6.7488   3.495 0.000554 ***
#   tillRT:ccNC:nfertConventional N  23.7575     6.7488   3.520 0.000506 ***
#   tillCT:ccTC:nfertConventional N  19.0890     6.7488   2.829 0.005027 ** 
#   tillNT:ccTC:nfertConventional N  18.6120     6.7488   2.758 0.006216 ** 
#   tillRT:ccTC:nfertConventional N  18.7551     6.7488   2.779 0.005835 ** 
#   tillCT:ccLC:nfertReduced N       41.4841     6.7488   6.147 2.83e-09 ***
#   tillNT:ccLC:nfertReduced N       -0.1431     6.7488  -0.021 0.983097    
# tillRT:ccLC:nfertReduced N       33.5632     6.7488   4.973 1.17e-06 ***
#   tillCT:ccNC:nfertReduced N        5.4828     6.7488   0.812 0.417265    
# tillNT:ccNC:nfertReduced N        4.8321     6.7488   0.716 0.474615    
# tillRT:ccNC:nfertReduced N        5.0023     6.7488   0.741 0.459205    
# tillCT:ccTC:nfertReduced N        0.3338     6.7488   0.049 0.960586    
# tillNT:ccTC:nfertReduced N       -0.1431     6.7488  -0.021 0.983097    
# tillRT:ccTC:nfertReduced N            NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 19.09 on 270 degrees of freedom
# Multiple R-squared:  0.4775,	Adjusted R-squared:  0.4446 
# F-statistic: 14.51 on 17 and 270 DF,  p-value: < 2.2e-16

#### Excluding RT
# Call:
#   lm(formula = NO3.sitemean ~ till:cc:nfert, data = ndat.site[ndat.site$till %in% 
#                                                                 c("CT", "NT"), ])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -49.429 -11.990   4.638   9.004  64.080 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     1.934e+01  4.407e+00   4.388 1.95e-05 ***
#   tillCT:ccLC:nfertConventional N 6.038e+01  6.233e+00   9.688  < 2e-16 ***
#   tillNT:ccLC:nfertConventional N 1.876e+01  6.233e+00   3.009 0.002996 ** 
#   tillCT:ccNC:nfertConventional N 2.438e+01  6.233e+00   3.912 0.000130 ***
#   tillNT:ccNC:nfertConventional N 2.373e+01  6.233e+00   3.807 0.000192 ***
#   tillCT:ccTC:nfertConventional N 1.923e+01  6.233e+00   3.086 0.002352 ** 
#   tillNT:ccTC:nfertConventional N 1.876e+01  6.233e+00   3.009 0.002996 ** 
#   tillCT:ccLC:nfertReduced N      4.163e+01  6.233e+00   6.679 2.90e-10 ***
#   tillNT:ccLC:nfertReduced N      1.713e-14  6.233e+00   0.000 1.000000    
# tillCT:ccNC:nfertReduced N      5.626e+00  6.233e+00   0.903 0.367917    
# tillNT:ccNC:nfertReduced N      4.975e+00  6.233e+00   0.798 0.425785    
# tillCT:ccTC:nfertReduced N      4.769e-01  6.233e+00   0.077 0.939089    
# tillNT:ccTC:nfertReduced N             NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 17.63 on 180 degrees of freedom
# Multiple R-squared:  0.5147,	Adjusted R-squared:  0.4851 
# F-statistic: 17.36 on 11 and 180 DF,  p-value: < 2.2e-16




summary(no3aov)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# cc:nfert     5  15798  3159.6    12.3 4.24e-09 ***
#   Residuals   90  23112   256.8                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#             Df Sum Sq Mean Sq F value  Pr(>F)    
# till:cc      5  21224    4245    11.1 2.4e-08 ***
#   Residuals   90  34412     382                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# including RT
# Df Sum Sq Mean Sq F value Pr(>F)
# till:cc:nfert  17 141879    8346   14.51 <2e-16 ***
#   Residuals     270 155271     575
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#excluding RT
# Df Sum Sq Mean Sq F value Pr(>F)    
# till:cc:nfert  11  59332    5394   17.36 <2e-16 ***
#   Residuals     180  55939     311                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukout <- TukeyHSD(no3aov)
cld <- multcompView::multcompLetters4(no3aov, Tukout)
cld <- as.data.frame.list(cld$`cc:nfert`)
# ndat_mean_ctnt <- filter(ndat_mean)   #, nfert=="Conventional N")
# ndat_mean_ctnt$cld <- cld$Letters
ndat_mean$cld <- cld$Letters
# > ndat_mean
# A tibble: 6 × 5
# # Groups:   cc [3]
# cc    nfert          NO3.mean.lbac NO3.se.lbac cld  
# <chr> <chr>                  <dbl>       <dbl> <chr>
#   1 LC    Conventional N          58.9       1.21  a    
# 2 NC    Conventional N          43.4       0.613 ab   
# 3 LC    Reduced N               40.2       1.05  bc   
# 4 TC    Conventional N          38.3       0.533 bc   
# 5 NC    Reduced N               24.6       0.376 cd   
# 6 TC    Reduced N               19.6       0.297 d    
# LC conv N - Reduced N 
(58.9-40.2)/58.2  # 32% or 18.7 units
# TC conv - reduced
(38.3-19.6)/38.3  # 49% or 18.7 units
# NC conv - reduced
(43.4 - 24.6)/43.4  # 43% or 18.8 units
mean(c(0.32, 0.49, 0.43))  # 0.41
mean(c(18.7, 18.7, 18.8))  # 18.7
18.7*0.6

# N fert comparisons: 
# A tibble: 12 × 6
# Groups:   till, cc [6]
# till  cc    nfert          NO3.mean.lbac NO3.se.lbac cld  
# <chr> <chr> <chr>                  <dbl>       <dbl> <chr>
#   1 CT    LC    Conventional N          79.7       2.07  a    
# 2 CT    LC    Reduced N               61.0       1.78  ab   
# 3 CT    NC    Conventional N          43.7       0.874 bc   
# 4 NT    NC    Conventional N          43.1       0.859 bc   
# 5 CT    TC    Conventional N          38.6       0.760 cd   
# 6 NT    LC    Conventional N          38.1       0.749 cd   
# 7 NT    TC    Conventional N          38.1       0.749 cd   
# 8 CT    NC    Reduced N               25.0       0.539 cd   
# 9 NT    NC    Reduced N               24.3       0.525 cd   
# 10 CT    TC    Reduced N               19.8       0.426 d    
# 11 NT    LC    Reduced N               19.3       0.413 d    
# 12 NT    TC    Reduced N               19.3       0.413 d    

# NC CT conv N - Reduced N 
(43.7-25.0)/43.7  # 0.43   # difference is 18.7
# NC NT conv N- Reduced N
(43.1-24.3)/43.1   # 0.44  # diff is 18.8
# TC CT conv - red
(38.6-19.8)/38.6   # 0.49  # diff is 18.8
# TC NT conv - red
(38.1 - 19.3)/38.1  # 0.49  # 18.8
# LC CT conv - red
(79.7 - 61)/79.7  # 0.23  # 18.7
# LC NT conv- red
(38.1-19.3)/38.1  # 0.49 <<--- double check these with Alex, they match TC NT
mean(c(0.43, 0.44, 0.49, 0.49, 0.23))  # 0.42
mean(c(18.7, 18.8, 18.8, 18.8, 18.7))  # 18.76

18.76*0.60



#N treatment rates in lb/ac
# Conventional
(3*82.2)*2.20462/2.47105 # 220 lb/ac
# Reduced
(3*57.5)*2.20462/2.47105 # 154 lb/ac

############ make graph

windows(xpinch=200, ypinch=200, width=5, height=5)


ggplot(data=ndat_mean, 
       aes(x=nfert, y=NO3.mean.lbac, fill=nfert)) +   # fill=variable
  geom_bar(stat="identity", position=position_dodge()) + # color="#332288", 
  geom_errorbar(aes(ymin = NO3.mean.lbac - NO3.se.lbac, 
                    ymax = NO3.mean.lbac + NO3.se.lbac), 
                width=0.3, position=position_dodge(0.9)) +
   facet_grid(# rows=vars(factor(till, levels=c("CT", "RT", "NT"))), 
             # cols=vars(factor(cc, levels=c("NC", "TC", "LC"))), 
     cols=vars(factor(cc, levels=c("NC", "TC", "LC"))), #,factor(till, levels=c("CT", "NT"))), 
             labeller = as_labeller(
               c(NC="No Cover Crop",TC="Triticale Cover", LC="Legume Cover"))) +
                #  "CT" = "Conventional Till", "NT" = "No Till"))) +
                 # "Fall N" = "Fall\nN", "High N" = "High\nN", "Recommended N"="Recm'd\nN"))) +
  scale_fill_manual(values=c("#C2e4ef", "#669947")) +
  xlab("N management") +
  ylab("Mean annual nitrate loss (lb N per ac) 2022 to 2072") +
  # scale_x_discrete(breaks=c("Conventional N", "Reduced N"),
  #                   labels=c("220 lb N\nper acre", "154 lb N\nper acre")) +
  # geom_text(aes(x=nfert, y=NO3.mean.lbac+5, label=cld), 
  #           position=position_dodge(width=1), size=5, fontface="bold") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/CA_alm_NO3 losses mean annual bars by nfert and cc, lbac_no letters.png", width=8, height=3.5, dpi=300)




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

