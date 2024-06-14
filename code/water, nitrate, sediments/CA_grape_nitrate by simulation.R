# This script analyzes NO3- loss data from DNDC model simulations for CLM-19 FFAR-001.
# started 2/1/2024 by BMM
# The script re-arranges the data, summarizes it, uses ANOVA and Tukey's HSD to generate
# letters for putting on bar charts.



library(tidyverse) # has ggplot, dplyr, etc.

library(reshape2)

se <- function(x) sd(x) / sqrt(length(x))


# if you only need annual totals skip to ndatyr below

# # if you need daily estimates use this:
# # # data in the folder data/large_data/ are too big to share in github repo
# # # only saved to Bonnie's computer (backed up by onedrive)
# ndat <- read.csv("data/large_data/daily N/CA_vineyards_day_soil_n20240220.csv")
# beepr::beep(sound=8)
# # N UNITS are in kg N / ha per day
# 
# # unique(ndat$crop_system_name)  # "grape-a" "grape-c"
# # unique(ndat$management_name)  #  "ct-bc" "cn"    "ct-lc" "ct-nc" "nt-nc" "rn"    "rt-lc" "rt-nc" "nt-lc" "rt-bc" "nt-bc"
# 
# # # sum data by year
# ndatyr <- ndat %>%
#   group_by(site_name, crop_system_name, management_name, climate_scenario, Year) %>%
#   summarize(NO3.yr = sum(NO3.leach))
# 
# 
# 
# # # # clean up
# rm(ndat)
# #
# colnames(ndatyr)[c(3,5)] <- c("management", "year")
# 
# write.csv(ndatyr, "data/water, nitrate, sediments/CA_grape_nitrate_annualtotals.csv", row.names=F)

ndatyr <- read.csv("data/water, nitrate, sediments/CA_grape_nitrate_annualtotals.csv")


ndatyr <- ndatyr[ndatyr$year>2021 & ndatyr$year<2073 & ndatyr$climate_scenario=="rcp60",]


ndatyr$till <- ifelse(grepl("ct-", ndatyr$management), "CT", 
                     ifelse(grepl("rt-", ndatyr$management), "RT", 
                            ifelse(grepl("nt-", ndatyr$management), "NT", "NA")))
# # check
# unique(ndatyr$till)  # NA is correct for row areas, till and cover crop only apply to alleys

# factor for cover crops
ndatyr$cc <- ifelse(grepl("-nc", ndatyr$management), "NC", 
                   ifelse(grepl("-bc", ndatyr$management), "BarC",  # "basic cover" was barley not to be confused with bean cover
                          ifelse(grepl("-lc", ndatyr$management), "LegC", "NA")))  # legume cover (Faba bean)


# # check
# unique(ndatyr$cc)   # NA is correct for row areas, till and cover crop only apply to alleys

# dummy for N treatment
ndatyr$nfert <- ifelse(grepl("cn", ndatyr$management), "Conventional N", 
                      ifelse(grepl("rn", ndatyr$management), "Reduced N", "NA"))

ndatyr$system <- ifelse(grepl("-a", ndatyr$crop_system_name), "alley", "crop")



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


# check that all levels present:
# check <- aggregate(year ~ climate_scenario + system + site_name + till + cc + nfert, data=ndatyr, FUN="length")
# looks correct!


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
ndatyrw$NO3.yrtot <- (0.15*ndatyrw$NO3.yr.crop) + (0.85*ndatyrw$NO3.yr.alley)
# convert kg N / ha*yr to lb N / ac*yr
ndatyrw$NO3.yrtot.lbac <- ndatyrw$NO3.yrtot * 1/(2.471*0.4536)



################ calculate means across sites


# Annual mean ACROSS ALL YEARS 
ndat.site <- ndatyrw[ndatyrw$till %in% c("CT", "NT") & ndatyrw$nfert=="Conventional N",] %>%  # for stats
  group_by(site_name, cc) %>%  # not significant effect of tillage and N level (N levels not very different)
  summarize(NO3.sitemean = mean(NO3.yrtot.lbac)) # mean annual N loss per site (mean across 50 years at each site)

ndat_mean <- ndatyrw[ndatyrw$till %in% c("CT", "NT") & ndatyrw$nfert=="Conventional N",] %>%    # for plotting means
  group_by(cc) %>%  # drop sitename to get mean across sites,  # not significant effect of tillage and N level (N levels not very different)
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
            
            
# rm(ndatyr, ndatyrw)


############################### NO3- losses average 2022-2072


### get letters for the bars



# no3aov <- aov(NO3.sitemean ~ cc:till:nfert, data=ndat.site)  # & ndat.site$nfert=="Conventional N",])  #  till:cc:nfert
# no3lm <- lm(NO3.sitemean~ cc:till:nfert, data=ndat.site)

no3aov <- aov(NO3.sitemean ~ cc, data=ndat.site)  # & ndat.site$nfert=="Conventional N",])  #  till:cc:nfert
no3lm <- lm(NO3.sitemean~ cc, data=ndat.site)

summary(no3lm)
# Call:
#   lm(formula = NO3.sitemean ~ cc, data = ndat.site)  # filtered ndat.site for CT, NT, and Conventional N
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -59.536  -5.263  -1.079   9.584  54.200 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   23.097      5.552   4.160 0.000141 ***
#   ccLegC       125.762      7.851  16.018  < 2e-16 ***
#   ccNC          27.573      7.851   3.512 0.001025 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 22.21 on 45 degrees of freedom
# Multiple R-squared:  0.863,	Adjusted R-squared:  0.8569 
# F-statistic: 141.8 on 2 and 45 DF,  p-value: < 2.2e-16

# Call:
#   lm(formula = NO3.sitemean ~ cc:till:nfert, data = ndat.site)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -64.157  -5.394  -1.660   9.311  56.835 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         44.164      5.575   7.922 2.34e-13 ***
#   ccBarC:tillCT:nfertConventional N  -17.331      7.884  -2.198  0.02921 *  
#   ccLegC:tillCT:nfertConventional N  111.435      7.884  14.134  < 2e-16 ***
#   ccNC:tillCT:nfertConventional N     12.233      7.884   1.552  0.12250    
# ccBarC:tillNT:nfertConventional N  -24.804      7.884  -3.146  0.00194 ** 
#   ccLegC:tillNT:nfertConventional N   97.954      7.884  12.424  < 2e-16 ***
#   ccNC:tillNT:nfertConventional N      0.778      7.884   0.099  0.92150    
# ccBarC:tillCT:nfertReduced N       -18.109      7.884  -2.297  0.02278 *  
#   ccLegC:tillCT:nfertReduced N       110.657      7.884  14.035  < 2e-16 ***
#   ccNC:tillCT:nfertReduced N          11.456      7.884   1.453  0.14797    
# ccBarC:tillNT:nfertReduced N       -25.582      7.884  -3.245  0.00140 ** 
#   ccLegC:tillNT:nfertReduced N        97.176      7.884  12.325  < 2e-16 ***
#   ccNC:tillNT:nfertReduced N              NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 22.3 on 180 degrees of freedom
# Multiple R-squared:  0.8633,	Adjusted R-squared:  0.8549 
# F-statistic: 103.3 on 11 and 180 DF,  p-value: < 2.2e-16



summary(no3aov)
# Df Sum Sq Mean Sq F value Pr(>F)         # filtered ndat.site for CT, NT, and Conventional N
# cc           2 139826   69913   141.8 <2e-16 ***
#   Residuals   45  22192     493                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Df Sum Sq Mean Sq F value Pr(>F)    
# cc:till:nfert  11 565236   51385   103.3 <2e-16 ***
#   Residuals     180  89511     497                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukout <- TukeyHSD(no3aov)
Tukout
# Tukey multiple comparisons of means
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = NO3.sitemean ~ cc, data = ndat.site)
# 
# $cc
# diff         lwr       upr     p adj
# LegC-BarC 125.76213  106.733537 144.79073 0.0000000
# NC-BarC    27.57315    8.544556  46.60174 0.0028911
# NC-LegC   -98.18898 -117.217575 -79.16039 0.0000000
cld <- multcompView::multcompLetters4(no3aov, Tukout)
cld <- as.data.frame.list(cld$cc)  # `cc:till:nfert`

ndat_mean$cld <- cld$Letters
ndat_mean
# # A tibble: 3 × 4
# cc    NO3.mean.lbac NO3.se.lbac cld  
# <chr>         <dbl>       <dbl> <chr>
#   1 LegC          149.        2.32  a    
# 2 NC             50.7       0.547 b    
# 3 BarC           23.1       0.457 c
# NC - BarC
(50.7-23.1)/50.7  # 54% or 27.6 units
27.6*0.6
# NC - LegC
(23.1 - 149)/23.1  # 545% or 125.9 units


# # A tibble: 12 × 6
# # Groups:   till, cc [6]
# till  cc    nfert          NO3.mean.lbac NO3.se.lbac cld  
# <chr> <chr> <chr>                  <dbl>       <dbl> <chr>
#   1 CT    LegC  Conventional N         156.        3.31  a    
# 2 CT    LegC  Reduced N              155.        3.30  a    
# 3 NT    LegC  Conventional N         142.        3.24  a    
# 4 NT    LegC  Reduced N              141.        3.23  a    
# 5 CT    NC    Conventional N          56.4       0.820 b    
# 6 CT    NC    Reduced N               55.6       0.810 b    
# 7 NT    NC    Conventional N          44.9       0.669 bc   
# 8 NT    NC    Reduced N               44.2       0.659 bcd  
# 9 CT    BarC  Conventional N          26.8       0.658 cd   
# 10 CT    BarC  Reduced N               26.1       0.650 cd   
# 11 NT    BarC  Conventional N          19.4       0.607 cd   
# 12 NT    BarC  Reduced N               18.6       0.601 d 



#N treatment rates in lb/ac
# Conventional
(2*25)*(2.20462)*(1/2.471) # 44.6 lb/ac
# Reduced
(2*20)*2.20462/2.47105 # 36 lb/ac

############ make graph

windows(xpinch=200, ypinch=200, width=5, height=5)


ggplot(data=ndat_mean, 
       aes(x=cc, y=NO3.mean.lbac)) +   # fill=variable
  geom_bar(stat="identity", position=position_dodge(), fill="#669947", width=0.7) + # color="#332288", 
  geom_errorbar(aes(ymin = NO3.mean.lbac - NO3.se.lbac, 
                    ymax = NO3.mean.lbac + NO3.se.lbac), 
                width=0.2, position=position_dodge(0.9)) +
  #  facet_grid(# rows=vars(factor(till, levels=c("CT", "RT", "NT"))), 
  #            # cols=vars(factor(cc, levels=c("NC", "TC", "LC"))), 
  #    cols=vars(factor(cc, levels=c("NC", "BarC", "LegC"))),  #factor(till, levels=c("CT", "NT"))), 
  #            labeller = as_labeller(
  #              c(NC="No Cover Crop",BarC="Barley Cover", LegC="Legume Cover"))) +
  #                #"CT" = "Conventional Till", "NT" = "No Till"))) +
  #                # "Fall N" = "Fall\nN", "High N" = "High\nN", "Recommended N"="Recm'd\nN"))) +
  # scale_fill_manual(values=c("#C2e4ef", "#669947")) +
  xlab("N management") +
  ylab("Mean annual nitrate loss (lb N per ac) 2022 to 2072") +
  scale_x_discrete(limits=c("NC", "BarC", "LegC"),
                    labels=c("No cover", "Barley cover", "Faba bean cover")) +
  # geom_text(aes(x=cc, y=NO3.mean.lbac+5, label=cld),
  #           position=position_dodge(width=1), size=5, fontface="bold") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/CA_grape_NO3 losses mean annual bars by cc lbac_no letters.png", width=4, height=3, dpi=300)




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

