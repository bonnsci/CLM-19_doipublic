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
ndatyr <- ndat %>%
  group_by(site_name, crop_system_name, management_name, climate_scenario, Year) %>%
  summarize(NO3.yr = sum(NO3.leach))

# are nt-bc and nt-lc showing same values? 
# testy <- ndat %>%
#   group_by(site_name, crop_system_name, management_name, climate_scenario, Year) %>%
#   summarize(NO3.yr = sum(NO3.leach)) %>%
#   filter(climate_scenario=="rcp26",
#                   management_name %in% c("nt-bc", "nt-lc"),
#                   site_name=="a_1",
#                   Year >=2060) %>%
#   arrange(Year, management_name)
# print(testy, n=nrow(testy))
# 
# testy2 <- filter(ndatyr, climate_scenario=="rcp26",
#                  management_name %in% c("nt-bc", "nt-lc"),
#                  site_name=="a_1",
#                  Year >=2060) %>%
#   arrange(Year, management_name)
# testy2


# 
# # # clean up

# rm(ndat)
# #
# colnames(ndatyr)[c(3,5)] <- c("management", "year")
# 
# write.csv(ndatyr, "data/water, nitrate, sediments/CA_alm_nitrate_annualtotals.csv", row.names=F)


ndatyr <- read.csv("data/water, nitrate, sediments/CA_alm_nitrate_annualtotals.csv")

# testy3 <- filter(ndatyr, climate_scenario=="rcp26",
#                 management %in% c("nt-bc", "nt-lc"),
#                 site_name=="a_1",
#                 year >=2060) %>%
#   arrange(year, management)
# testy3


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

# factor for N treatment
ndatyr$nfert <- ifelse(grepl("cn", ndatyr$management), "Conventional N", 
                      ifelse(grepl("rn", ndatyr$management), "Reduced N", "NA"))

ndatyr$system <- ifelse(grepl("-a", ndatyr$crop_system), "alley", "crop")



# # check
# unique(ndatyr$nfert)  # NA is correct for alleys
# unique(ndatyr$system)


# factor for decade
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
  group_by(site_name,  cc, nfert) %>%  # tillage does not have an effect on N losses so averaging across tillage
  summarize(NO3.sitemean = mean(NO3.yrtot.lbac)) # mean annual N loss per site (mean across 50 years at each site)

ndat_mean <- ndatyrw[ndatyrw$till %in% c("CT", "NT"),] %>%    # for plotting means
  group_by(cc, nfert) %>%  # drop sitename to get mean across sites, # tillage does not have an effect on N losses so averaging across tillage
  summarize(NO3.mean.lbac = mean(NO3.yrtot.lbac),# mean of the site means in each treatment combo
             NO3.se.lbac = se(NO3.yrtot.lbac)) %>%# variability across sites and years in each treatment combo
  arrange(desc(NO3.mean.lbac))

# ndat_mean            
# till  cc    nfert          NO3.mean.lbac NO3.se.lbac
# <chr> <chr> <chr>                  <dbl>       <dbl>
#   1 CT    LC    Conventional N          79.4       2.06 
# 2 NT    LC    Conventional N          69.6       1.87 
# 3 CT    LC    Reduced N               60.8       1.78 
# 4 NT    LC    Reduced N               51.0       1.59 
# 5 CT    NC    Conventional N          43.4       0.868
# 6 NT    NC    Conventional N          42.7       0.854
# 7 CT    TC    Conventional N          38.2       0.754
# 8 NT    TC    Conventional N          37.7       0.743
# 9 CT    NC    Reduced N               24.8       0.535
# 10 NT    NC    Reduced N               24.1       0.521
# 11 CT    TC    Reduced N               19.6       0.422
# 12 NT    TC    Reduced N               19.2       0.409         


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
#   lm(formula = NO3.sitemean ~ cc:nfert, data = ndat.site)    ### cc:nfert results
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -50.905 -12.674   3.959  10.297  62.667 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                19.403      5.574   3.481 0.000772 ***
#   ccLC:nfertConventional N   55.100      7.882   6.991 4.63e-10 ***
#   ccNC:nfertConventional N   23.642      7.882   2.999 0.003497 ** 
#   ccTC:nfertConventional N   18.578      7.882   2.357 0.020593 *  
#   ccLC:nfertReduced N        36.522      7.882   4.634 1.21e-05 ***
#   ccNC:nfertReduced N         5.064      7.882   0.643 0.522182    
# ccTC:nfertReduced N            NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 22.29 on 90 degrees of freedom
# Multiple R-squared:  0.427,	Adjusted R-squared:  0.3952 
# F-statistic: 13.42 on 5 and 90 DF,  p-value: 8.995e-10



# Call:
#   lm(formula = NO3.sitemean ~ cc:till:nfert, data = ndat.site)          # cc:till:nfert results
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -54.085 -12.632   4.346  10.227  64.311 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      19.1626     5.5814   3.433  0.00074 ***
#   ccLC:tillCT:nfertConventional N  60.2370     7.8933   7.631 1.31e-12 ***
#   ccNC:tillCT:nfertConventional N  24.2083     7.8933   3.067  0.00250 ** 
#   ccTC:tillCT:nfertConventional N  19.0593     7.8933   2.415  0.01675 *  
#   ccLC:tillNT:nfertConventional N  50.4445     7.8933   6.391 1.37e-09 ***
#   ccNC:tillNT:nfertConventional N  23.5576     7.8933   2.985  0.00324 ** 
#   ccTC:tillNT:nfertConventional N  18.5780     7.8933   2.354  0.01967 *  
#   ccLC:tillCT:nfertReduced N       41.6590     7.8933   5.278 3.73e-07 ***
#   ccNC:tillCT:nfertReduced N        5.6303     7.8933   0.713  0.47658    
# ccTC:tillCT:nfertReduced N        0.4813     7.8933   0.061  0.95145    
# ccLC:tillNT:nfertReduced N       31.8665     7.8933   4.037 7.99e-05 ***
#   ccNC:tillNT:nfertReduced N        4.9795     7.8933   0.631  0.52894    
# ccTC:tillNT:nfertReduced N            NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 22.33 on 180 degrees of freedom
# Multiple R-squared:  0.432,	Adjusted R-squared:  0.3972 
# F-statistic: 12.44 on 11 and 180 DF,  p-value: < 2.2e-16




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

# Df Sum Sq Mean Sq F value Pr(>F)                           # cc:nfert results
# cc:nfert     5  33341    6668   13.42  9e-10 ***
#   Residuals   90  44732     497                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# formula = NO3.sitemean ~ cc:till:nfert, data = ndat.site)          # cc:till:nfert results
# Df Sum Sq Mean Sq F value Pr(>F)    
# cc:till:nfert  11  68226    6202   12.44 <2e-16 ***
#   Residuals     180  89718     498                   



Tukout <- TukeyHSD(no3aov)
cld <- multcompView::multcompLetters4(no3aov, Tukout)
cld <- as.data.frame.list(cld$`cc:nfert`)


ndat_mean$cld <- cld$Letters
ndat_mean
# # A tibble: 6 × 5                                               # cc:nfert results
# # Groups:   cc [3]
# cc    nfert          NO3.mean.lbac NO3.se.lbac cld  
# <chr> <chr>                  <dbl>       <dbl> <chr>
#   1 LC    Conventional N          74.5       1.40  a    
# 2 LC    Reduced N               55.9       1.20  ab   
# 3 NC    Conventional N          43.0       0.609 bc   
# 4 TC    Conventional N          38.0       0.529 bcd  
# 5 NC    Reduced N               24.5       0.373 cd   
# 6 TC    Reduced N               19.4       0.294 d  
# LC conv N - Reduced N 
(74.5-55.9)/74.5 # 25% or 18.6 units
# TC conv - reduced
(38.0-19.4)/38.0  # 49% or 18.6 units
# NC conv - reduced
(43.0 - 24.5)/43.0  # 43% or 18.5 units
mean(c(0.25, 0.49, 0.43))  # 0.39
mean(c(18.6, 18.6, 18.5))  # 18.6
18.6*0.6   # $0.60 average cost of N fert per lb N

# A tibble: 12 × 6                                                # cc:till:nfert results
# Groups:   till, cc [6]
# till  cc    nfert          NO3.mean.lbac NO3.se.lbac cld  
# <chr> <chr> <chr>                  <dbl>       <dbl> <chr>
#   1 CT    LC    Conventional N          79.4       2.06  a    
# 2 NT    LC    Conventional N          69.6       1.87  ab   
# 3 CT    LC    Reduced N               60.8       1.78  abc  
# 4 NT    LC    Reduced N               51.0       1.59  bc   
# 5 CT    NC    Conventional N          43.4       0.868 cd   
# 6 NT    NC    Conventional N          42.7       0.854 cd   
# 7 CT    TC    Conventional N          38.2       0.754 cd   
# 8 NT    TC    Conventional N          37.7       0.743 cd   
# 9 CT    NC    Reduced N               24.8       0.535 d    
# 10 NT    NC    Reduced N               24.1       0.521 d    
# 11 CT    TC    Reduced N               19.6       0.422 d    
# 12 NT    TC    Reduced N               19.2       0.409 d  
# LC CT - LC NT





#N treatment rates in lb/ac
# Conventional
(3*82.2)*2.20462/2.47105 # 220 lb/ac
# Reduced
(3*57.5)*2.20462/2.47105 # 154 lb/ac

############ make graph

windows(xpinch=200, ypinch=200, width=5, height=5)


ggplot(data=ndat_mean, 
       aes(x=nfert, y=NO3.mean.lbac, fill=nfert)) +   # fill=variable
  geom_bar(stat="identity", position=position_dodge(), width=0.7) + # color="#332288", 
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

