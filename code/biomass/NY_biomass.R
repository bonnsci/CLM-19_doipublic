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

se <- function(x) sd(x) / sqrt(length(x))
cv <- function(x) sd(x) / mean(x)

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




#######################   
#######################   (1) set up the data  -- CAN SKIP to part 2
#######################   

# bm <- read.csv("data/biomass/biomass_NY.csv")
# bma <- read.csv("data/biomass/biomass_NYalf.csv")  # new data to replace alfalfa system data in above original file
# bma <- bma[,2:13]
# unique(bma$management_name) # "ct-cc" "ct-nc" "nt-cc" "nt-nc" "rt-cc" "rt-nc"  
# unique(bma$crop_system_name) # "alfalfa-act2" "alfalfa-act3" "alfalfa-act"  "alfalfa-cta"  "alfalfa-cta2"
# unique(bm$crop_system_name)# "alfalfa-act" "alfalfa-cta" "corn-grain"  "corn-soy"    "corn-silage" "soy-corn"    "alfalfa-tac"
# # the act/cta/tac - There were three versions of the alfalfa rotation system that 
# # were simulated such that alfalfa, corn, and triticale ("a, c, t") were grown across each 
# # calendar year (i.e. 
# # simulation "alfalfa-act" alfalfa is the first crop planted  
# #     years 2014-2016 in alfalfa, 2017 corn, 2017/2018 triticale (planted 11/2017, harvested 7/2018)
# # simulation "alfalfa-cta" corn is the first crop planted
# #     year 2013 is corn, 2013/2014 triticale, 2014-2017 alfalfa
# # simulation "alfalfa-cta2" - corn is first crop planted again
# #     year 2014 is corn, 2014/2015 tritical, 2015-2018 alfalfa
# # simulation "alfalfa-act2" - alfalfa first crop 
# #     year 2013-2014 alfalfa, 2015 corn, 2015-16 triticale (harvest july), 2016 (plant august)-2019 alfalfa
# # last simulation "alfalfa-act3"
# #     years 2013-2015 alfalfa, 2016 corn, 2016-17 triticale, 2017-2020 alfalfa
# 
# unique(bm$crop_name) # [1] "alfalfa"            "corn, grain"        "corn, silage"       "rye, winter, cover" "soybean"            "triticale_grain"   
# 
# # Remove alfalfa rotations from "bm", replace with new output
# bm <- filter(bm, !grepl("alfalfa-", crop_system_name))
# unique(bm$crop_system_name)
# bm <- rbind(bm, bma)
# unique(bm$crop_system_name) # "corn-grain"   "corn-soy"     "corn-silage"  "soy-corn"     
# #"alfalfa-act2" "alfalfa-act3" "alfalfa-act"  "alfalfa-cta"  "alfalfa-cta2"
# rm(bma)
# 
# # DO NOT need to find literature values to convert kg C grain to kg grain.
# # since we'll only be looking at them RELATIVE TO EACH OTHER, not absolute amounts...
# # let's look at the C results for now
# # 
# # min(bm$Year) # 2013 --2013-2021 start up years, start at 2022
# # max(bm$Year) # 2073
# 
# # looks like all the cover crop biomass is in leaf and stem, zero grain
# 
# # make factor for CT, RT, NT
# bm$till <- ifelse(grepl("ct-", bm$management_name), "CT", 
#                     ifelse(grepl("rt-", bm$management_name), "RT", "NT"))
# # # check
# # unique(bm$till)
# 
# # factor for CC or NC
# bm$cc <- ifelse(grepl("-cc", bm$management_name), "CC", "NC")
# # # check
# # unique(bm$cc)
# 
# # factor for N treatment # no N treatments in NY
# 
# bm <- bm[bm$Year>2021 & bm$Year<2073,]
# 
# # factor for decade
# bm$decade <- ifelse(bm$Year <2021, "2010s",
#                       ifelse(bm$Year>=2021 & bm$Year <2031, "2020s",
#                         ifelse(bm$Year>=2031 & bm$Year <2041, "2030s",
#                           ifelse(bm$Year>=2041 & bm$Year <2051, "2040s",
#                             ifelse(bm$Year>=2051 & bm$Year <2061, "2050s",
#                               ifelse(bm$Year>=2061 & bm$Year <2071, "2060s", "2070s"))))))
# 
# 
# 
# # unique(bm$decade)
# 
# # combine crop system with crop name (so we see difference between corn, grain in alfalfa and corn, grain in corn-soy system)
# unique(bm$crop_name) # "alfalfa"  "corn, grain"    "corn, silage"  "rye, winter, cover" "soybean"    "triticale_grain"   
# unique(bm$crop_system_name) # "corn-grain"   "corn-soy"     "corn-silage"  "soy-corn"     
# #"alfalfa-act2" "alfalfa-act3" "alfalfa-act"  "alfalfa-cta"  "alfalfa-cta2"
# 
# alfrots <- c("alfalfa-act2" ,"alfalfa-act3" ,"alfalfa-act" , "alfalfa-cta" , "alfalfa-cta2")
# 
# bm$crop_rot <- ifelse(bm$crop_name=="corn, grain" & bm$crop_system_name=="corn-grain", "corng_corng", 
#                       ifelse(bm$crop_name=="corn, grain" & bm$crop_system_name %in% c("corn-soy", "soy-corn"), "corng_cornsoy", 
#                              ifelse(bm$crop_name=="corn, grain" & bm$crop_system_name %in% alfrots, "corng_alf", 
#                                     ifelse(bm$crop_name=="corn, silage", "corns_corns", 
#                                            ifelse(bm$crop_name=="soybean", "soy_cornsoy",
#                                                   ifelse(bm$crop_name=="triticale_grain" , "tri_alf", 
#                                                          ifelse(bm$crop_name=="rye, winter, cover" & bm$crop_system_name=="corn-grain", "rye_corng",
#                                                                 ifelse(bm$crop_name=="rye, winter, cover" & bm$crop_system_name %in% c("corn-soy", "soy-corn"), "rye_cornsoy",
#                                                                        ifelse(bm$crop_name=="rye, winter, cover" & bm$crop_system_name=="corn-silage", "rye_corns",
#                                                                               ifelse(bm$crop_name=="rye, winter, cover" & bm$crop_system_name %in% alfrots, "rye_alf", 
#                                                                                      ifelse(bm$crop_name=="alfalfa", "alf_alf", "X")
#                                                          ))))))))))
# 
# # check
# unique(bm$crop_rot)
# rm(alfrots)
# 
# # is triticale data zero its first year, so potentially have a bunch of zeros skewing triticale yield data?
# hist(bm[bm$crop_name=="triticale_grain","Grain.C.kgC.ha."])
# 
# test <- filter(bm, 
#                site_name=="f_1" , 
#                between(Year, 2022,2023), 
#                crop_system_name == "alfalfa-act", 
#                climate_scenario=="rcp60", 
#                crop_name=="triticale_grain",
#                management_name=="ct-cc")
# 
# 
# ## YES, triticale has a bunch of grain = 0 for year planted and biomass entries we don't want.
# # don't want to just filter for where grain = 0 cause sometimes its a bad year and maybe it's 0.
# 
# # years and rotations we DON'T want triticale grain data entries: (basically the year of corn harvest in that rotation, which is the year triticale is planted in the fall)
# acttri <- seq(2022,2072, 5)
# ctatri <- seq(2023, 2072, 5)
# cta2tri <- seq(2024, 2072, 5)
# act2tri <- seq(2025, 2072, 5)
# act3tri <- seq(2021, 2072, 5)
# 
# # filter these data out:
# bm <- filter(bm, ! (crop_name=="triticale_grain" & crop_system_name == "alfalfa-act" & Year %in% acttri),
#              ! (crop_name=="triticale_grain" & crop_system_name == "alfalfa-cta" & Year %in% ctatri),
#              ! (crop_name=="triticale_grain" & crop_system_name == "alfalfa-cta2" & Year %in% cta2tri),
#              ! (crop_name=="triticale_grain" & crop_system_name == "alfalfa-act2" & Year %in% act2tri),
#              ! (crop_name=="triticale_grain" & crop_system_name == "alfalfa-act3" & Year %in% act3tri))
# 
# #check 
# ifelse(nrow(bm[bm$Grain.C.kgC.ha.==0 & bm$crop_name=="triticale_grain",])==0, print("CORRECT"), print("INCORRECT"))
# # NOT SURE why it is printing twice
# 
# bm$abvg <- bm$Grain.C.kgC.ha. + bm$Leaf.C.kgC.ha. + bm$Stem.C.kgC.ha.
# 
# 
# write.csv(bm, "data/biomass/NY_bm.csv", row.names = F)







#######################   
#######################   (2)  
#######################   









#######################   
#######################   (2a) is mean grain C / abvg C different among till*cc groups across all years across crops?
#######################   

bm <- read.csv("data/biomass/NY_bm.csv")

# calculate the treatment means, then the pdiff from the mean of those means
bmsum1 <- group_by(bm, cc, till, crop_name) %>%
  filter(!till=="RT", crop_name %in% c("corn, grain", "corn, silage", "soybean")) %>%
  summarize(mean_gr=mean(Grain.C.kgC.ha.), 
            se_gr=se(Grain.C.kgC.ha.),
            mean_abvg = mean(abvg),
            se_abvg = se(abvg)) %>%
  mutate(mean_harv = ifelse(crop_name %in% c("corn, grain", "soybean"), mean_gr, mean_abvg),
         se_harv = ifelse(crop_name %in% c("corn, grain", "soybean"), se_gr, se_abvg)) %>%
  arrange(desc(mean_harv))

meancorns <- mean(unlist(bmsum1[bmsum1$crop_name=="corn, silage", "mean_harv"]))
meancorng <- mean(unlist(bmsum1[bmsum1$crop_name=="corn, grain", "mean_harv"]))
meansoy<- mean(unlist(bmsum1[bmsum1$crop_name=="soybean", "mean_harv"]))

bmsum1 <- mutate(bmsum1, 
                     pdiff = ifelse(crop_name == "corn, silage", (mean_harv - meancorns)/mean_harv,
                                    ifelse(crop_name == "soybean", (mean_harv - meansoy)/mean_harv,
                                           ifelse(crop_name == "corn, grain", (mean_harv - meancorng)/mean_harv, NA))),
                     pdiff_se = se_harv/mean_harv)

bmsum2 <- group_by(bmsum1, cc, till) %>%
  reframe(pdiff_mean = mean(pdiff),
          pdiff_se = mean(pdiff_se),
          mean_harv = mean(mean_harv),
          se_harv = mean(se_harv))%>%
  arrange(desc(pdiff_mean))


            
  

# do ANOVA on means not pdiff
bm2 <- filter(bm, !till == "RT", crop_name %in% c("corn, grain", "corn, silage", "soybean"))


lm1 <- lm(Grain.C.kgC.ha. ~ till * cc, data = bm2)
summary(lm1)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1975.0  -866.5  -267.4   935.0  3094.8 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1881.42      11.65 161.511   <2e-16 ***
#   tillNT        305.77      16.47  18.561   <2e-16 ***
#   ccNC           21.48      16.47   1.304   0.1924    
# tillNT:ccNC   -66.19      23.30  -2.841   0.0045 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1052 on 32636 degrees of freedom
# Multiple R-squared:  0.01678,	Adjusted R-squared:  0.01669 
# F-statistic: 185.7 on 3 and 32636 DF,  p-value: < 2.2e-16


effect <- aov(Grain.C.kgC.ha. ~ till * cc, data = bm2)
summary(effect)
# > summary(effect)
# Df    Sum Sq   Mean Sq F value Pr(>F)    
# till            1 6.067e+08 606699601 547.918 <2e-16 ***
#   cc              1 1.102e+06   1101522   0.995 0.3186    
# till:cc         1 8.938e+06   8937679   8.072 0.0045 ** 
#   Residuals   32636 3.614e+10   1107282                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukout <- TukeyHSD(effect)
Tukout
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = Grain.C.kgC.ha. ~ till * cc, data = bm2)
# 
# $till
# diff      lwr      upr p adj
# NT-CT 272.6728 249.8414 295.5041     0
# 
# $cc
# diff       lwr      upr     p adj
# NC-CC -11.61854 -34.44992 11.21284 0.3185724
# 
# $`till:cc`
# diff        lwr         upr     p adj
# NT:CC-CT:CC  305.76815  263.44593  348.090370 0.0000000
# CT:NC-CT:CC   21.47683  -20.84539   63.799057 0.5605399
# NT:NC-CT:CC  261.05423  218.73201  303.376455 0.0000000
# CT:NC-NT:CC -284.29131 -326.61354 -241.969091 0.0000000
# NT:NC-NT:CC  -44.71391  -87.03614   -2.391693 0.0336075
# NT:NC-CT:NC  239.57740  197.25518  281.899620 0.0000000


# compact letter display
cld <- multcompLetters4(effect, Tukout)

cld <- as.data.frame.list(cld$`till:cc`)
bmsum2$cld <- cld$Letters

bmsum2$cctill <- paste0(bmsum2$till, "-", bmsum2$cc)

bmsum2

# cc    till  pdiff_mean pdiff_se mean_harv se_harv cld   cctill
# <chr> <chr>      <dbl>    <dbl>     <dbl>   <dbl> <chr> <chr> 
# 1 CC    NT        0.0451  0.00479     3603.    11.9 a     NT-CC 
# 2 NC    NT        0.0327  0.00487     3591.    12.1 b     NT-NC 
# 3 NC    CT       -0.0420  0.00507     3346.    12.0 c     CT-NC 
# 4 CC    CT       -0.0440  0.00497     3355.    11.2 c     CT-CC 
mean(c(3603, 3591)) # 3597
mean(c(3346, 3355)) # 3350.5
(3597-3350.5)/3350.5

(3603-3591)/3591 # 1%


windows(xpinch=200, ypinch=200, width=5, height=5)

pal2 <- c("#20243d", "#669947")
pal3 <- c("#20243d", "#669947", "#C2e4ef")
pal4 <- c("#c44f2d","#20243d", "#C2e4ef", "#669947")



ggplot(data=bmsum2, 
       aes(x=cctill, y=pdiff_mean, fill=cctill)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.7) +
  geom_errorbar(width=0.3, aes(ymin=pdiff_mean-pdiff_se, ymax=pdiff_mean+ pdiff_se),
                position=position_dodge(0.9),
                color="gray60") +
  # facet_grid( #rows=vars(factor(till, levels=c("CT", "NT"))),  #"RT"))), 
  #   cols=vars(# factor(crop_rot, levels=c("corng_corng", "corng_cornsoy", "corns_corns")), 
  #             factor(till, levels=c("CT", "NT"))),
  #   # factor(cc, levels=c("NC", "CC"))), 
  #   labeller = as_labeller(
  #     c( #NC="No Cover Crop", CC="Rye Cover Crop",  
  #       # corng_corng = "corng_corng", corng_cornsoy = "corng_cornsoy", corns_corns="corns_corns",
  #       "CT" = "Conventional Till", "NT" = "No Till"))) + #, "RT"="Reduced Till"))) +
  geom_hline(yintercept=0, color=pal4[2]) + 
  xlab("cover crop") +
  # ylab(expression(bold('2022-2072 mean corn grain biomass (kg C ha'^-1*')'))) + 
  scale_y_continuous(labels=scales::percent_format(accuracy=1),  # accuracy=1 indicates to show whole number, "0.1" would show one decimal place, etc.
                     breaks=seq(-0.04, 0.04, 0.02)) + 
  ylab(expression(bold('Percent difference from overall mean'))) + 
  # ylim(0,3800)+
  # geom_text(aes(label=cld, y=ifelse(pdiff_mean<0, pdiff_mean-0.01, pdiff_mean+0.006)), vjust=-0.5,
  #            color="gray20", size=4, fontface="bold") +
  scale_fill_manual(values=pal4, breaks=c("CT-NC", "CT-CC", "NT-NC", "NT-CC")) +
  scale_x_discrete(limits=c("CT-NC", "CT-CC", "NT-NC", "NT-CC")) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-30, hjust=0, size=11))

ggsave("plots/biomass/NY_pdiff_mean of corng,corns,soy_cc,till_no letters.png", width=5, height=6, dpi=300)




#######################   
#######################   (2a) is mean corn grain C different among till*cc groups across all years?
#######################   

# corndat <- bm[bm$crop_name=="corn, grain" & bm$climate_scenario=="rcp60",]

corngr <- bm[bm$crop_rot %in% c("corng_corng", "corng_cornsoy") & 
  bm$climate_scenario=="rcp60" &
  bm$till %in% c("CT", "NT"),]

lmgrain <- lm(Grain.C.kgC.ha.~till*cc*crop_rot, data=corngr)
summary(lmgrain)
# Call:
#   lm(formula = Grain.C.kgC.ha. ~ till * cc * crop_rot, data = corngr)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2368.09  -408.53    75.32   500.00  2059.51 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       3047.468     25.904 117.644  < 2e-16 ***
#   tillNT                             409.788     36.634  11.186  < 2e-16 ***
#   ccNC                               -21.884     36.634  -0.597    0.550    
# crop_rotcorng_cornsoy             -211.972     36.634  -5.786 7.53e-09 ***
#   tillNT:ccNC                        -41.391     51.808  -0.799    0.424    
# tillNT:crop_rotcorng_cornsoy        83.561     51.808   1.613    0.107    
# ccNC:crop_rotcorng_cornsoy           8.102     51.808   0.156    0.876    
# tillNT:ccNC:crop_rotcorng_cornsoy  -68.866     73.268  -0.940    0.347    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 740 on 6520 degrees of freedom
# Multiple R-squared:  0.08772,	Adjusted R-squared:  0.08674 
# F-statistic: 89.56 on 7 and 6520 DF,  p-value: < 2.2e-16

effect <- aov(Grain.C.kgC.ha.~till*cc*crop_rot, data=corngr)
summary(effect)
# Df    Sum Sq   Mean Sq F value  Pr(>F)    
# till                1 2.793e+08 279254206 510.002 < 2e-16 ***
#   cc                  1 5.071e+06   5071413   9.262 0.00235 ** 
#   crop_rot            1 5.487e+07  54867417 100.204 < 2e-16 ***
#   till:cc             1 2.346e+06   2345695   4.284 0.03851 *  
#   till:crop_rot       1 9.847e+05    984739   1.798 0.17995    
# cc:crop_rot         1 2.829e+05    282872   0.517 0.47232    
# till:cc:crop_rot    1 4.837e+05    483740   0.883 0.34729    
# Residuals        6520 3.570e+09    547556                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukout <- TukeyHSD(effect)
# put interaction output into a dataframe we can sort


# compact letter display
cld <- multcompLetters4(effect, Tukout)

# table with letters and 3rd quantile
corngrsum <- group_by(corngr, cc, till, crop_rot) %>%
  # mutate(abvg = Grain.C.kgC.ha. + Leaf.C.kgC.ha. + Stem.C.kgC.ha.) %>%
  summarize(meangr=mean(Grain.C.kgC.ha.), 
            # meansi=mean(abvg), 
            segr=se(Grain.C.kgC.ha.)) %>%
            # sesi=se(abvg)) %>%
  arrange(desc(meangr))

glob.meangr <- mean(corngrsum$meangr)


corngrsum$pdiff <- (corngrsum$meangr-glob.meangr)/corngrsum$meangr

cld <- as.data.frame.list(cld$`till:cc:crop_rot`)
corngrsum$cld <- cld$Letters

corngrsum
# # A tibble: 8 × 7
# # Groups:   cc, till [4]
# cc    till  crop_rot      meangr  segr   pdiff cld  
# <chr> <chr> <chr>          <dbl> <dbl>   <dbl> <chr>
#   1 CC    NT    corng_corng    3457.  28.4  0.0919 a    
# 2 NC    NT    corng_corng    3394.  27.9  0.0750 ab   
# 3 CC    NT    corng_cornsoy  3329.  26.1  0.0569 b    
# 4 NC    NT    corng_cornsoy  3205.  25.4  0.0204 c    
# 5 CC    CT    corng_corng    3047.  25.1 -0.0302 d    
# 6 NC    CT    corng_corng    3026.  25.9 -0.0376 d    
# 7 CC    CT    corng_cornsoy  2835.  23.0 -0.107  e    
# 8 NC    CT    corng_cornsoy  2822.  25.0 -0.113  e   

windows(xpinch=200, ypinch=200, width=5, height=5)

pal2 <- c("#20243d", "#669947")
pal3 <- c("#20243d", "#669947", "#C2e4ef")

ggplot(data=corngrsum, 
       aes(x=cc, y=pdiff, fill=crop_rot)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.7, show.legend=F) +
  # geom_errorbar(width=0.3, aes(ymin=mean-se, ymax=mean + se),  
  #               position=position_dodge(0.9),
  #               color="#332288") +
  facet_grid( #rows=vars(factor(till, levels=c("CT", "NT"))),  #"RT"))), 
             cols=vars(factor(crop_rot, levels=c("corng_corng", "corng_cornsoy", "corns_corns")), 
                       factor(till, levels=c("CT", "NT"))),
                       # factor(cc, levels=c("NC", "CC"))), 
             labeller = as_labeller(
               c( #NC="No Cover Crop", CC="Rye Cover Crop",  
                 corng_corng = "corng_corng", corng_cornsoy = "corng_cornsoy", corns_corns="corns_corns",
                "CT" = "Conventional Till", "NT" = "No Till"))) + #, "RT"="Reduced Till"))) +
  geom_hline(yintercept=0, color=pal2[1]) + 
  xlab("cover crop") +
  # ylab(expression(bold('2022-2072 mean corn grain biomass (kg C ha'^-1*')'))) + 
  scale_y_continuous(labels=scales::percent_format(accuracy=1), # accuracy=1 indicates to show whole number, "0.1" would show one decimal place, etc.
                     breaks=seq(-0.12, 0.12, 0.04), 
                     limits=c(-0.14, 0.12)) +
  ylab(expression(bold('Percent difference from overall mean'))) + 
  # ylim(0,3800)+
  # geom_text(aes(label=cld, y=ifelse(pdiff<0, pdiff-0.02, pdiff+0.01)), vjust=-0.5,
  #           color="gray20", size=4, fontface="bold") +
  scale_fill_manual(values=pal2) +
  scale_x_discrete(limits=c("NC", "CC")) + 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-10, hjust=0, size=11),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))
# # strip.background=element_rect(fill="lightblue", color="black", size=1) 

ggsave("plots/biomass/NY_corn,grain_biomass_pdiff_rotation,cc,till_no letters.png", width=8, height=4, dpi=300)















#######################   
#######################   (2b) is mean corn silage C different among till*cc groups across all years?
#######################   

# corndat <- bm[bm$crop_name=="corn, grain" & bm$climate_scenario=="rcp60",]

cornsi <- bm[bm$crop_rot=="corns_corns" & 
               bm$climate_scenario=="rcp60" &
               bm$till %in% c("CT", "NT"),]

cornsi$abvg <- cornsi$Grain.C.kgC.ha. + cornsi$Leaf.C.kgC.ha. + cornsi$Stem.C.kgC.ha.

lmsil <- lm(abvg~till*cc, data=cornsi)
summary(lmsil)
# Call:
#   lm(formula = abvg ~ till * cc, data = cornsi)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2495.21  -341.03    37.32   429.55  2477.51 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  7042.50      22.60 311.668  < 2e-16 ***
#   tillNT        321.69      31.96  10.067  < 2e-16 ***
#   ccNC          -75.45      31.96  -2.361 0.018281 *  
#   tillNT:ccNC   159.18      45.19   3.522 0.000434 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 645.5 on 3260 degrees of freedom
# Multiple R-squared:  0.09137,	Adjusted R-squared:  0.09053 
# F-statistic: 109.3 on 3 and 3260 DF,  p-value: < 2.2e-16

effectsi <- aov(abvg~till*cc, data=cornsi)
# summary(effect)
# Df    Sum Sq   Mean Sq F value   Pr(>F)    
# till           1 1.314e+08 131394505 315.368  < 2e-16 ***
#   cc             1 1.400e+04     13997   0.034 0.854583    
# till:cc        1 5.169e+06   5169177  12.407 0.000434 ***
#   Residuals   3260 1.358e+09    416639                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukoutsi <- TukeyHSD(effectsi)

# compact letter display
cldsi <- multcompLetters4(effectsi, Tukoutsi)

# summary table for letters
cornsisum <- group_by(cornsi, cc, till) %>%
  summarize(meansi=mean(abvg), 
            sesi=se(abvg)) %>%
  arrange(desc(meansi))

glob.meansi <- mean(cornsisum$meansi)

cornsisum$pdiff <- (cornsisum$meansi-glob.meansi)/cornsisum$meansi

cldsi <- as.data.frame.list(cldsi$`till:cc`)
cornsisum$cld <- cldsi$Letters

cornsisum
# cc    till  meansi  sesi   pdiff cld  
# <chr> <chr>  <dbl> <dbl>   <dbl> <chr>
#   1 NC    NT     7448.  23.4  0.0326 a    
# 2 CC    NT     7364.  22.1  0.0216 b    
# 3 CC    CT     7043.  20.2 -0.0231 c    
# 4 NC    CT     6967.  24.5 -0.0342 c  

(7364-6967)/6967

# windows(xpinch=200, ypinch=200, width=5, height=5)

pal2 <- c("#20243d", "#669947")
pal3 <- c("#20243d", "#669947", "#C2e4ef")

ggplot(data=cornsisum, 
       aes(x=cc, y=pdiff)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.7, fill="#1982be") +
  # geom_errorbar(width=0.3, aes(ymin=mean-se, ymax=mean + se),  
  #               position=position_dodge(0.9),
  #               color="#332288") +
  facet_grid( #rows=vars(factor(till, levels=c("CT", "NT"))),  #"RT"))), 
    cols=vars(
              factor(till, levels=c("CT", "NT"))),
    # factor(cc, levels=c("NC", "CC"))), 
    labeller = as_labeller(
      c( #NC="No Cover Crop", CC="Rye Cover Crop",  
        "CT" = "Conventional Till", "NT" = "No Till"))) + #, "RT"="Reduced Till"))) +
  geom_hline(yintercept=0, color=pal2[1]) + 
  xlab("cover crop") +
  # ylab(expression(bold('2022-2072 mean corn grain biomass (kg C ha'^-1*')'))) + 
  scale_y_continuous(labels=scales::percent_format(accuracy=1), # accuracy=1 indicates to show whole number, "0.1" would show one decimal place, etc.
                     breaks=seq(-0.04, 0.04, 0.02), 
                     limits=c(-0.04, 0.04)) +   # change to -0.045 for all letters to show
  ylab(expression(bold('Percent difference from overall mean'))) + 
  # ylim(0,3800)+
  geom_text(aes(label=cld, y=ifelse(pdiff<0, pdiff-0.008, pdiff+0.005)), vjust=-0.5,
           color="gray20", size=4, fontface="bold") +
  scale_fill_manual(values=pal2) +
  scale_x_discrete(limits=c("NC", "CC")) + 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-10, hjust=0, size=11),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))
# # strip.background=element_rect(fill="lightblue", color="black", size=1) 

ggsave("plots/biomass/NY_corn,silage_biomass_pdiff_rotation,cc,till_with letters.png", width=5, height=4, dpi=300)










#######################   
#######################   (2c) is mean alfalfa different among till*cc groups across all years?
#######################   

# corndat <- bm[bm$crop_name=="corn, grain" & bm$climate_scenario=="rcp60",]

alf <- bm[bm$crop_name=="alfalfa" & 
               bm$climate_scenario=="rcp60" &
               bm$till %in% c("CT", "NT"),]

alf$abvg <- alf$Grain.C.kgC.ha. + alf$Leaf.C.kgC.ha. + alf$Stem.C.kgC.ha.

lmalf <- lm(abvg~Year + till*cc, data=alf)
summary(lmalf)
# Call:
#   lm(formula = abvg ~ till * cc, data = alf)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2168.7  -937.7  -412.7   718.6  4809.6 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3408.228     22.786 149.578   <2e-16 ***
#   tillNT        48.337     32.224   1.500    0.134    
# ccNC         -27.068     32.224  -0.840    0.401    
# tillNT:ccNC   -6.246     45.571  -0.137    0.891    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1299 on 12988 degrees of freedom
# Multiple R-squared:  0.0004396,	Adjusted R-squared:  0.0002087 
# F-statistic: 1.904 on 3 and 12988 DF,  p-value: 0.1265

effectalf <- aov(abvg~till*cc, data=alf)
summary(effectalf)
# Df    Sum Sq Mean Sq F value Pr(>F)  
# till            1 6.640e+06 6640024   3.938 0.0472 *
#   cc              1 2.961e+06 2960574   1.756 0.1852  
# till:cc         1 3.168e+04   31679   0.019 0.8910  
# Residuals   12988 2.190e+10 1686305                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukoutalf <- TukeyHSD(effectalf)

# compact letter display
cldalf <- multcompLetters4(effectalf, Tukoutalf)

# summary table for letters
alfsum <- group_by(alf, cc, till) %>%
  summarize(meanalf=mean(abvg), 
            sealf=se(abvg)) %>%
  arrange(desc(meanalf))

glob.meanalf <- mean(alfsum$meanalf)

alfsum$pdiff <- (alfsum$meanalf-glob.meanalf)/alfsum$meanalf

cldalf <- as.data.frame.list(cldalf$`till:cc`)
alfsum$cld <- cldalf$Letters

alfsum
# # A tibble: 4 × 6
# # Groups:   cc [2]
# cc    till  meanalf sealf    pdiff cld  
# <chr> <chr>   <dbl> <dbl>    <dbl> <chr>
#   1 CC    NT      3457.  22.8  0.0114  a    
# 2 NC    NT      3423.  22.6  0.00174 a    
# 3 CC    CT      3408.  23.0 -0.00266 a    
# 4 NC    CT      3381.  22.8 -0.0107  a 

# don't need to make a graph to show no differences!






#######################   
#######################   (2d) is mean soybean C different among till*cc groups across all years?
#######################   

# corndat <- bm[bm$crop_name=="corn, grain" & bm$climate_scenario=="rcp60",]

soy <- bm[bm$crop_name=="soybean" & 
               bm$climate_scenario=="rcp60" &
               bm$till %in% c("CT", "NT"),]



lmsoy <- lm(Grain.C.kgC.ha.~till*cc, data=soy)
summary(lmsoy)
# Call:
#   lm(formula = Grain.C.kgC.ha. ~ till * cc, data = soy)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -652.4 -158.4   16.5  163.6  597.9 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  808.792      7.711 104.890  < 2e-16 ***
#   tillNT        55.742     10.905   5.112 3.38e-07 ***
#   ccNC          -5.321     10.905  -0.488    0.626    
# tillNT:ccNC   -8.653     15.422  -0.561    0.575    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 220.3 on 3260 degrees of freedom
# Multiple R-squared:  0.01402,	Adjusted R-squared:  0.01311 
# F-statistic: 15.45 on 3 and 3260 DF,  p-value: 5.61e-10

effectsoy <- aov(Grain.C.kgC.ha.~till*cc, data=soy)
summary(effectsoy)
# Df    Sum Sq Mean Sq F value   Pr(>F)    
# till           1   2157154 2157154  44.461 3.04e-11 ***
#   cc             1     75951   75951   1.565    0.211    
# till:cc        1     15273   15273   0.315    0.575    
# Residuals   3260 158166801   48517                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukoutsoy <- TukeyHSD(effectsoy)

# compact letter display
cldsoy <- multcompLetters4(effectsoy, Tukoutsoy)

# summary table for letters
soysum <- group_by(soy, cc, till) %>%
  summarize(mean=mean(Grain.C.kgC.ha.), 
            se=se(Grain.C.kgC.ha.)) %>%
  arrange(desc(mean))

glob.meansoy <- mean(soysum$mean)

soysum$pdiff <- (soysum$mean-glob.meansoy)/soysum$mean

cldsoy <- as.data.frame.list(cldsoy$`till:cc`)
soysum$cld <- cldsoy$Letters

soysum
# # A tibble: 4 × 6
# # Groups:   cc [2]
# cc    till   mean    se   pdiff cld  
# <chr> <chr> <dbl> <dbl>   <dbl> <chr>
#   1 CC    NT     865.  7.96  0.0378 a    
# 2 NC    NT     851.  7.80  0.0220 a    
# 3 CC    CT     809.  7.48 -0.0285 b    
# 4 NC    CT     803.  7.59 -0.0353 b  


# windows(xpinch=200, ypinch=200, width=5, height=5)
# 
# pal2 <- c("#20243d", "#669947")
# pal3 <- c("#20243d", "#669947", "#C2e4ef")

ggplot(data=soysum, 
       aes(x=cc, y=pdiff)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.7, fill="#c44f2d") +
  # geom_errorbar(width=0.3, aes(ymin=mean-se, ymax=mean + se),
  #               position=position_dodge(0.9),
  #               color="#332288") +
  facet_grid( #rows=vars(factor(till, levels=c("CT", "NT"))),  #"RT"))), 
    cols=vars(
      factor(till, levels=c("CT", "NT"))),
    # factor(cc, levels=c("NC", "CC"))), 
    labeller = as_labeller(
      c( #NC="No Cover Crop", CC="Rye Cover Crop",  
        "CT" = "Conventional Till", "NT" = "No Till"))) + #, "RT"="Reduced Till"))) +
  geom_hline(yintercept=0, color=pal2[1]) + 
  xlab("cover crop") +
  # ylab(expression(bold('2022-2072 mean corn grain biomass (kg C ha'^-1*')'))) + 
  scale_y_continuous(labels=scales::percent_format(accuracy=1), # accuracy=1 indicates to show whole number, "0.1" would show one decimal place, etc.
                     breaks=seq(-0.04, 0.04, 0.02),
                     limits=c(-0.04, 0.04)) +   # change to -0.045 for all letters to show
  ylab(expression(bold('Percent difference from overall mean'))) + 
  # ylim(0,3800)+
  # geom_text(aes(label=cld, y=ifelse(pdiff<0, pdiff-0.005, pdiff+0.002)), vjust=-0.5,
  #          color="gray20", size=4, fontface="bold") +
  # scale_fill_manual(values=pal2) +
  scale_x_discrete(limits=c("NC", "CC")) + 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-10, hjust=0, size=11),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))


ggsave("plots/biomass/NY_soy_biomass_pdiff_cc,till_no letters.png", width=5, height=4, dpi=300)






#######################   
#######################   (2e) is mean triticale C different among till*cc groups across all years?
#######################   

trit <- bm[bm$crop_name=="triticale_grain" & 
            bm$climate_scenario=="rcp60" &
            bm$till %in% c("CT", "NT"),]

# Triticale was harvested as a forage crop not a grain, 



lmtrit <- lm(abvg~till*cc, data=trit)
summary(lmtrit)
# Call:
#   lm(formula = abvg ~ till * cc, data = trit)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -6318  -1805    205   1704   5229 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  6624.46      76.79  86.270  < 2e-16 ***
#   tillNT       -322.73     108.59  -2.972  0.00298 ** 
#   ccNC         -238.71     108.59  -2.198  0.02801 *  
#   tillNT:ccNC    51.18     153.58   0.333  0.73894    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2193 on 3260 degrees of freedom
# Multiple R-squared:  0.006942,	Adjusted R-squared:  0.006028 
# F-statistic: 7.596 on 3 and 3260 DF,  p-value: 4.637e-05

# Notice how the model p-value and R2 (though still poor) improve when we include Year (probably true for other models too)
# Year is 
lmtrit.yr <- lm(abvg~cc + till + Year, data=trit)
summary(lmtrit.yr)
# Call:
#   lm(formula = abvg ~ cc + till + Year, data = trit)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6419.7 -1734.7   264.9  1739.3  4672.3 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -71148.877   5163.165 -13.780  < 2e-16 ***
#   ccNC          -213.117     74.249  -2.870  0.00413 ** 
#   tillNT        -297.138     74.249  -4.002 6.42e-05 ***
#   Year            37.988      2.522  15.062  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2121 on 3260 degrees of freedom
# Multiple R-squared:  0.07152,	Adjusted R-squared:  0.07066 
# F-statistic:  83.7 on 3 and 3260 DF,  p-value: < 2.2e-16


effecttrit <- aov(abvg~till*cc, data=trit)
summary(effecttrit)
# Df    Sum Sq  Mean Sq F value   Pr(>F)    
# till           1 7.205e+07 72045365  14.974 0.000111 ***
#   cc             1 3.706e+07 37061785   7.703 0.005545 ** 
#   till:cc        1 5.345e+05   534451   0.111 0.738940    
# Residuals   3260 1.569e+10  4811417                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukouttrit <- TukeyHSD(effecttrit)
Tukouttrit

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = abvg ~ till * cc, data = trit)
# 
# $till
# diff       lwr       upr     p adj
# NT-CT -297.1378 -447.6948 -146.5809 0.0001111
# 
# $cc
# diff       lwr       upr     p adj
# NC-CC -213.117 -363.6739 -62.56005 0.0055447
# 
# $`till:cc`
# diff       lwr         upr     p adj
# NT:CC-CT:CC -322.73010 -601.8558  -43.604355 0.0157743
# CT:NC-CT:CC -238.70927 -517.8350   40.416477 0.1239066
# NT:NC-CT:CC -510.25482 -789.3806 -231.129081 0.0000162
# CT:NC-NT:CC   84.02083 -195.1049  363.146575 0.8663632
# NT:NC-NT:CC -187.52473 -466.6505   91.601017 0.3098029
# NT:NC-CT:NC -271.54556 -550.6713    7.580185 0.0599959
# compact letter display
cldtrit <- multcompLetters4(effecttrit, Tukouttrit)

# summary table for letters
tritsum <- group_by(trit, cc, till) %>%
  summarize(mean=mean(abvg), 
            se=se(abvg)) %>%
  arrange(desc(mean))

glob.meantrit <- mean(tritsum$mean)

tritsum$pdiff <- (tritsum$mean-glob.meantrit)/tritsum$mean

cldtrit <- as.data.frame.list(cldtrit$`till:cc`)
tritsum$cld <- cldtrit$Letters

tritsum
# # A tibble: 4 × 6
# # Groups:   cc [2]
# cc    till   mean    se    pdiff cld
# <chr> <chr> <dbl> <dbl>    <dbl> <chr>
#   1 CC    CT    6624.  79.0  0.0404  a
# 2 NC    CT    6386.  78.9  0.00457 ab
# 3 CC    NT    6302.  74.3 -0.00870 b
# 4 NC    NT    6114.  74.8 -0.0396  b


windows(xpinch=200, ypinch=200, width=5, height=5)
# 
pal2 <- c("#20243d", "#669947")
# pal3 <- c("#20243d", "#669947", "#C2e4ef")

ggplot(data=tritsum, 
       aes(x=cc, y=pdiff)) +  # y= mean
  geom_bar(stat="identity", position=position_dodge(), width=0.7, fill="#c44f2d") +
  # geom_errorbar(width=0.3, aes(ymin=mean-se, ymax=mean + se),
  #               position=position_dodge(0.9),
  #               color="#332288") +
  facet_grid( #rows=vars(factor(till, levels=c("CT", "NT"))),  #"RT"))), 
    cols=vars(
      factor(till, levels=c("CT", "NT"))),
    # factor(cc, levels=c("NC", "CC"))), 
    labeller = as_labeller(
      c( #NC="No Cover Crop", CC="Rye Cover Crop",  
        "CT" = "Conventional Till", "NT" = "No Till"))) + #, "RT"="Reduced Till"))) +
  geom_hline(yintercept=0, color=pal2[1]) + 
  xlab("cover crop") +
  # ylab(expression(bold('2022-2072 mean corn grain biomass (kg C ha'^-1*')'))) + 
  # scale_y_continuous(labels=scales::percent_format(accuracy=1)) + # accuracy=1 indicates to show whole number, "0.1" would show one decimal place, etc.
                     # breaks=seq(-0.04, 0.04, 0.02),
                     # limits=c(-0.04, 0.04)) +   # change to -0.045 for all letters to show
  ylab(expression(bold('Grain yield (kg C / ha)'))) + 
  # ylim(0,3800)+
  geom_text(aes(label=cld, y=ifelse(pdiff<0, pdiff-0.005, pdiff+0.002)), vjust=-0.5,
           color="gray20", size=4, fontface="bold") +
  # geom_text(aes(label=cld, y=mean + 100), vjust=-0.5,
  #           color="gray20", size=4, fontface="bold") +
  # scale_fill_manual(values=pal2) +
  scale_x_discrete(limits=c("NC", "CC")) + 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-10, hjust=0, size=11),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))


ggsave("plots/biomass/NY_trit_biomass_pdiff_cc,till_with letters.png", width=5, height=4, dpi=300)




























#######################   
#######################   (3) is variability in corn grain C different among till*cc*Nfert groups across all years?
#######################   

# calculate corn grain CV for each site
corncv<- group_by(corngr, site_name, till, cc, crop_rot) %>%
  summarize(cv=cv(Grain.C.kgC.ha.)) %>%
  arrange(desc(cv))

summary(lm(cv~till*cc + crop_rot, data=corncv))

effectcv <- aov(cv~till*cc + crop_rot, data=corncv)
summary(effectcv)
Tukout <- TukeyHSD(effectcv)          # no patterns in corn grain variability




# # calculate corn silage CV for each site
cornscv<- group_by(cornsi, site_name, till, cc) %>%
  summarize(cv=cv(abvg)) %>%
  arrange(desc(cv))

summary(lm(cv~till*cc, data=cornscv))     # yes patterns in silage cv

# Call:
#   lm(formula = cv ~ till * cc, data = cornscv)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.019467 -0.008652 -0.001979  0.006259  0.042612 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.053525   0.003233  16.558  < 2e-16 ***
#   tillNT       0.021878   0.004572   4.786 1.15e-05 ***   # NT has a positive effect 
#   ccNC         0.002286   0.004572   0.500    0.619    
# tillNT:ccNC -0.003577   0.006465  -0.553    0.582    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.01293 on 60 degrees of freedom
# Multiple R-squared:  0.3936,	Adjusted R-squared:  0.3633 
# F-statistic: 12.98 on 3 and 60 DF,  p-value: 1.221e-06

effectcv.sil <- aov(cv~till*cc, data=cornscv)
summary(effectcv.sil)
# Df   Sum Sq  Mean Sq F value   Pr(>F)    
# till         1 0.006457 0.006457  38.619 5.37e-08 ***
#   cc           1 0.000004 0.000004   0.024    0.878    
# till:cc      1 0.000051 0.000051   0.306    0.582    
# Residuals   60 0.010032 0.000167                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukoutcv.sil <- TukeyHSD(effectcv.sil)          
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = cv ~ till * cc, data = cornscv)
# 
# $till
# diff        lwr        upr p adj
# NT-CT 0.02008898 0.01362274 0.02655521 1e-07
# 
# $cc
# diff          lwr         upr     p adj
# NC-CC 0.0004975619 -0.005968672 0.006963796 0.8781907
# 
# $`till:cc`
# diff          lwr          upr     p adj
# NT:CC-CT:CC  0.021877701  0.009797054  0.033958348 0.0000668
# CT:NC-CT:CC  0.002286286 -0.009794361  0.014366933 0.9587320
# NT:NC-CT:CC  0.020586539  0.008505891  0.032667186 0.0001809
# CT:NC-NT:CC -0.019591415 -0.031672062 -0.007510768 0.0003822
# NT:NC-NT:CC -0.001291162 -0.013371809  0.010789485 0.9920601
# NT:NC-CT:NC  0.018300253  0.006219605  0.030380900 0.0009804


# compact letter display
cldcv.sil <- multcompLetters4(effectcv.sil, Tukoutcv.sil)

# table with letters and 3rd quantile
cornsi.cvsum <- group_by(cornscv, cc, till) %>%
  summarize(mean=mean(cv), 
            se=se(cv)) %>%
  arrange(desc(mean))

cldcv.sil <- as.data.frame.list(cldcv.sil$`till:cc`)
cornsi.cvsum$cld <- cldcv.sil$Letters

cornsi.cvsum
# cc    till    mean      se cld  
# <chr> <chr>  <dbl>   <dbl> <chr>
#   1 CC    NT    0.0754 0.00188 a    # CV is higher with soil health practice , however the difference is quite small - 5% vs 7% CV
# 2 NC    NT    0.0741 0.00313 a    
# 3 NC    CT    0.0558 0.00471 b    
# 4 CC    CT    0.0535 0.00251 b    

# use these letters on this plot:

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=cornsi.cvsum,
       aes(x=cc, y=mean, fill=cc)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend=F, width=0.7) +
  geom_errorbar(width=0.3, aes(ymin=mean-se, ymax=mean + se),  
                position=position_dodge(0.9),
                color="#332288") +
  facet_grid(cols=vars(factor(till, levels=c("CT", "NT"))),  # "RT"))), 
             #cols=vars(factor(cc, levels=c("NC", "CC"))), 
             labeller = as_labeller(
               #c(NC="No Cover Crop", CC="Has Cover Crop", 
                 c("CT" = "Conventional Till", "NT" = "No Till"))) + #, "RT"="Reduced Till"))) +
  xlab("cover crop") +
  ylim(0,0.1) +
  ylab(expression(bold('2022-2072 CV corn silage biomass'))) + 
  geom_text(aes(label=cld, y=mean+(2*se)), vjust=-0.5,
            color="gray20", size=4, fontface="bold") +
  scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99" )) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-10, hjust=0, size=11),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))
# # strip.background=element_rect(fill="lightblue", color="black", size=1) 

ggsave("plots/biomass/NY_corn,silage_biomassCV_cc,till_with letters.png", width=5, height=3, dpi=300)


























#######################   
#######################   (4) is mean soy grain C different among till*cc*Nfert groups across all years?
#######################   

soydat <- bm[bm$crop_name=="soybean" & bm$climate_scenario=="rcp60",]

Neffect <- aov(Grain.C.kgC.ha.~till*cc*nfert, data=soydat)
summary(Neffect)
Tukout <- TukeyHSD(Neffect)
# put interaction output into a dataframe we can sort
Tukout <- as.data.frame(Tukout[7]) %>%
  rownames_to_column(., "term") %>%
  arrange(term)


# assumptions - following https://statsandr.com/blog/anova-in-r

# Independence - the observations (grain.c.kgc.ha. observations) are independent, one does
# not depend on the other, they're not dependent on some other variable like, 
# from one individual came multiple observations.

# Normality - not required with large enough sample size >30.  But we can test anyway. 
# the residuals (observed - mean for that group) should be normally distributed.
qqnorm(soydat$Grain.C.kgC.ha.)
qqline(soydat$Grain.C.kgC.ha.)
hist(soydat$Grain.C.kgC.ha.-mean(soydat$Grain.C.kgC.ha.))
# look ok

# Equality of variances 
ggplot(data=soydat, aes(x=management_name, y=Grain.C.kgC.ha.)) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))
# looks ok
leveneTest(Grain.C.kgC.ha. ~ cc*till*nfert, data=soydat)
# Levene's Test for Homogeneity of Variance (center = median)
#          Df F value  Pr(>F)  
# group    17  1.8054 0.02181 *
#       34542                  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# HOWEVER: it seems that our large sample size (>35k data points) might be affecting this test
# See this from https://www.theanalysisfactor.com/the-problem-with-tests-for-statistical-assumptions/
# It relies too much on p-values, and therefore, sample sizes. If the sample size is large, 
# Levene’s will have a smaller p-value than if the sample size is small, given the same variances.
# So it’s very likely that you’re ***overstating a problem*** with the assumption in large samples and understating 
# it in small samples. You can’t ignore the actual size difference in the variances when making this decision. 
# So sure, look at the p-value, but also look at the actual variances and how much bigger some are than others. 
# (In other words, actually look at the effect size, not just the p-value).
# The ANOVA is generally considered robust to violations of this assumption when sample sizes 
# across groups are equal. So even if Levene’s is significant, moderately different variances may not be a 
# problem in balanced data sets. Keppel (1992) suggests that a good rule of thumb is that if sample sizes are equal, 
# robustness should hold until the largest variance is more than 9 times the smallest variance.
# This robustness goes away the more unbalanced the samples are. So you need to use judgment here,
# taking into account both the imbalance and the actual difference in variances.
# *** emphasis added

# outliers
summary(soydat$Grain.C.kgC.ha.)
# > summary(soydat$Grain.C.kgC.ha.)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 17.22  419.89  637.90  649.91  877.61 1480.36 

# outliers via histograms
hist(soydat$Grain.C.kgC.ha., breaks=sqrt(nrow(soydat)))

ggplot(data=soydat, aes(x=Grain.C.kgC.ha.)) +
  geom_histogram() +
  facet_grid(rows=vars(nfert), cols=vars(till, cc))

#outliers via boxplots
boxplot(Grain.C.kgC.ha.~management_name, data=soydat) # looks ok


# outliers via z-scores
soydat$z_grainC <- scale(soydat$Grain.C.kgC.ha.)
hist(soydat$z_grainC)
summary(soydat$z_grainC)
# V1         
# Min.   :-2.0914  
# 1st Qu.:-0.7604  
# Median :-0.0397  
# Mean   : 0.0000  
# 3rd Qu.: 0.7527  
# Max.   : 2.7451  
# < -2 or >2 is considered rare
# < -3 or >3 is extremely rare
# < -3.29 or > -3.29 is used to detect outliers, where one out of 1000 observations will be outside
# this range if normal distribution.
# min. and max do not reach 3 or 3.29.

## outlier summary: boxplots and z-score suggest no extreme outliers. No justification for removing.

# re-run ANOVA with the above knowledge (assumptions met)

soydat$till <- factor(soydat$till)
soydat$cc <- factor(soydat$cc)
soydat$nfert <- factor(soydat$nfert)

Neffect <- aov(Grain.C.kgC.ha.~till*cc*nfert, data=soydat)
summary(Neffect)
# Df    Sum Sq Mean Sq F value   Pr(>F)    
# till              2 5.366e+06 2683154  29.358 1.82e-13 ***
# cc                1 3.326e+05  332550   3.639   0.0565 .  
# nfert             2 2.328e+03    1164   0.013   0.9873    
# till:cc           2 3.333e+04   16667   0.182   0.8333    
# till:nfert        4 5.440e+02     136   0.001   1.0000    
# cc:nfert          2 1.164e+05   58208   0.637   0.5289    
# till:cc:nfert     4 2.558e+03     639   0.007   0.9999    
# Residuals     34542 3.157e+09   91394                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Neffect <- aov(Grain.C.kgC.ha.~till, data=soydat)
summary(Neffect)

Tukout <- TukeyHSD(Neffect)

# compact letter display
cld <- multcompLetters4(Neffect, Tukout)   

# table with letters and 3rd quantile
soysum <- group_by(soydat, cc, till, nfert) %>%
  summarize(mean=mean(Grain.C.kgC.ha.), 
            se=se(Grain.C.kgC.ha.)) %>%
  arrange(desc(mean))

cld <- as.data.frame.list(cld$`till:cc:nfert`)
soysum$cld <- cld$Letters

# may need to summarize data now just across tillage treatments to go with the model output
# left off here 10/26/23

# soysum

# use these letters on this plot:

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=soysum,aes(x=nfert, y=mean, fill=nfert)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288", show.legend=F) +
  geom_errorbar(width=0.3, aes(ymin=mean-se, ymax=mean + se),  
                position=position_dodge(0.9),
                color="#332288") +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT", "RT"))), 
             cols=vars(factor(cc, levels=c("CC", "NC"))), 
             labeller = as_labeller(
               c(CC="Has Cover Crop", NC="No Cover Crop",
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  xlab("N management") +
  ylab("Mean soy grain (kg C/ha) 2022-2072") +
  ylim(0,750)+
  geom_text(aes(label=cld, y=mean+(2*se)), vjust=-0.5) +
  scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99" )) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/biomass/IL_soy_biomass_Neffect.png", width=6, height=8, dpi=300)





#######################   
#######################   (5) is variability in soy grain C different among till*cc*Nfert groups across all years?
#######################   

# calculate CV for each site
soycv<- group_by(soydat, site_name, till, cc, nfert) %>%
  summarize(cv=cv(Grain.C.kgC.ha.)) %>%
  arrange(desc(cv))


Neffect <- aov(cv~till*cc*nfert, data=soycv)
summary(Neffect)
Tukout <- TukeyHSD(Neffect)


# assumptions - following https://statsandr.com/blog/anova-in-r

# Independence - the observations (grain.c.kgc.ha. observations) are independent, one does
# not depend on the other, they're not dependent on some other variable like, 
# from one individual came multiple observations.

# Normality - not required with large enough sample size >30.  But we can test anyway. 
# the residuals (observed - mean for that group) should be normally distributed.
qqnorm(soycv$cv) # looks really good
qqline(soycv$cv)
hist(soycv$cv-mean(soycv$cv)) # looks ok

# Equality of variances - assumption not met
ggplot(data=soycv, aes(x=nfert, y=cv)) + 
  geom_boxplot() +
  facet_grid(rows=vars(till), cols=vars(cc)) 
# looks ok, some possible outliers in nt-nc-fn on the low end, and rt-cc-fn, also on low end
leveneTest(cv ~ cc*till*nfert, data=soycv) 
# > leveneTest(Grain.C.kgC.ha. ~ management_name, data=soydat)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group  17  1.4426  0.111  # Accept null hypothesis that variances do not differ
#       558 

# outliers
summary(soycv$cv)
# > summary(soydat$Grain.C.kgC.ha.)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 139.9  1945.6  2875.9  2915.8  3924.3  5794.6 

# outliers via histograms
hist(soycv$cv, breaks=sqrt(nrow(soycv)))

ggplot(data=soycv, aes(x=cv)) +
  geom_histogram() +
  facet_grid(rows=vars(nfert), cols=vars(till, cc))

#outliers via boxplots
boxplot(cv~till+cc+nfert, data=soycv) # no outliers plotted 

# outliers via z-scores
soycv$z_cv <- scale(soycv$cv)
hist(soycv$z_cv)
summary(soycv$z_cv)
# V1          
# Min.   :-2.92849  
# 1st Qu.:-0.67960  
# Median :-0.06132  
# Mean   : 0.00000  
# 3rd Qu.: 0.79708  
# Max.   : 1.96883  
# < -2 or >2 is considered rare
# < -3 or >3 is extremely rare
# < -3.29 or > -3.29 is used to detect outliers, where one out of 1000 observations will be outside
# this range if normal distribution.
# interquartile range is > -2 and <2. 
# max is in the safe zone
# min is getting close to extremely rare.

## outlier summary: boxplots, levenes test, and z-score suggest no extreme outliers. No justification for removing.

cvtest <- aov(cv~till*cc*nfert, data=soycv)
summary(cvtest)
# Df Sum Sq Mean Sq F value   Pr(>F)
# till            2 0.0756 0.03780  18.959 1.08e-08 ***
#   cc              1 0.0577 0.05772  28.950 1.09e-07 ***
#   nfert           2 0.0722 0.03608  18.098 2.42e-08 ***
#   till:cc         2 0.0010 0.00049   0.245    0.782    
# till:nfert      4 0.0150 0.00375   1.880    0.112    
# cc:nfert        2 0.0457 0.02284  11.454 1.33e-05 ***
#   till:cc:nfert   4 0.0036 0.00090   0.454    0.770    
# Residuals     558 1.1125 0.00199                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukoutcv <- TukeyHSD(cvtest)

# compact letter display
cldcv <- multcompLetters4(cvtest, Tukoutcv)

# table with letters and 3rd quantile
soycvsum <- group_by(soycv, cc, till, nfert) %>%
  summarize(mean=mean(cv), 
            se=se(cv)) %>%
  arrange(desc(mean))

cldcv <- as.data.frame.list(cldcv$`till:cc:nfert`)
soycvsum$cld <- cldcv$Letters

soycvsum

# use these letters on this plot:

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=soycvsum,aes(x=nfert, y=mean, fill=nfert)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288", show.legend=F) +
  geom_errorbar(width=0.3, aes(ymin=mean-se, ymax=mean + se),  
                position=position_dodge(0.9),
                color="#332288") +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT", "RT"))), 
             cols=vars(factor(cc, levels=c("CC", "NC"))), 
             labeller = as_labeller(
               c(CC="Has Cover Crop", NC="No Cover Crop",
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  xlab("N management") +
  ylab("CV for soy grain (kg C/ha) 2022-2072") +
  ylim(0,0.45)+
  geom_text(aes(label=cld, y=mean+(2*se)), vjust=-0.5) +
  scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99" )) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/biomass/IL_soy_biomass_cv.png", width=6, height=8, dpi=300)






############ decadal stuff



# is the grain C biomass significantly different with High N vs Recommended N?
# let's look at biomass by decade
# will need to do a separate graph for grains with different yield units:
# corn grain, soybeans, triticale - grain bu/ac
# alfalfa and silage in above ground biomass ton/ac

# 1 bu corn = 56 lb = 25.4 kg  # from  https://www.ers.usda.gov/webdocs/publications/41880/33132_ah697_002.pdf
# 1 bu soy = 60 lb = 27.2155 kg  # https://www.ers.usda.gov/webdocs/publications/41880/33132_ah697_002.pdf
# 1 bu triticale = 50 lb = 22.6796 kg  # https://www.rma.usda.gov/-/media/RMA/Handbooks/Privately-Developed-Products---20000/Triticale/2018-20310U-Triticale-Crop-Insurance-Standards.ashx
# 1 lb = 0.453592 kg
# 1 kg = 2.20462 lb
# 1 ha = 2.47105 ac
# t US ton = 907.185 kg
# unique(bm$crop_name) # [1] "corn, grain"        "corn, silage"       "rye, winter, cover" "soybean"            "alfalfa"           
# [6] "triticale_grain"





# # dataset for bushel crops
# bm_bu <- filter(bm, crop_name %in% c("corn, grain", "soybean", "triticale_grain")) %>%
#   # convert to bu/ac = kgC/ha * lb/kg * bu/lb * ha/ac (bu/lb is unique per crop see numbers in #comments above)
#   mutate(grain.buac = ifelse(crop_name=="corn, grain", Grain.C.kgC.ha.*(2.20462) *(1/56)*(1/2.47105),
#                              ifelse(crop_name=="soybean", Grain.C.kgC.ha.*(2.20462) *(1/60)*(1/2.47105),
#                                                    Grain.C.kgC.ha.*(2.20462) *(1/50)*(1/2.47105))))  # triticale
#   
# 
# bm_bu_decsum <- group_by(bm_bu, climate_scenario,crop_name, cc, till, decade, crop_rot) %>%
#   summarize(grain.buac_mean = mean(grain.buac), grain.buac_se = se(grain.buac))
# 
# 
# # dataset for tons of ABVGD biomass ### note the rye biomass would need to be added with the previous
# # year's rye biomass (planted in one calendar year, grows in year 1 and 2)
# bm_tn <- filter(bm, crop_name %in% c("corn, silage", "alfalfa", "rye, winter, cover")) %>%
#   mutate(bm.tac = (Grain.C.kgC.ha.+ Leaf.C.kgC.ha.+ Stem.C.kgC.ha.)*(1/907.185) *(1/2.47105))
# 
# 
# bm_tn_decsum <- group_by(bm_tn, climate_scenario,crop_name, cc, till, decade, crop_rot) %>%
#   summarize(bm.tac_mean = mean(bm.tac), bm.tac_se = se(bm.tac))

# corn-corn grain fert rates kg/ha to lb/ac
# (42+168)/(2.471*0.4536)  # 187 lb
# # corn silage
# 157/(2.471*0.4536) # 140 lb manure N
# 52.5/(2.471*0.4536) # 47 lb UAN-N
# # corn in corn-soy
# (26+144)/(2.471*0.4536)  # 151 lb
# # corn after alfalfa
# 25/(2.471*0.4536)  # 22 lb
# # triticale
# 36/(2.471*0.4536)  # 32 lb manure -N
# 20/(2.471*0.4536)  # 17 lb UAN -N


windows(xpinch=200, ypinch=200, width=5, height=5)


# pal6blue <- c("#ffffff", "#ceced5", "#9f9fac", "#727284", "#48495f", "#20233c")
# 
# 
# # bushel crop plot
# ggplot(data=bm_bu_decsum[bm_bu_decsum$climate_scenario == "rcp60" ,], 
#        aes(x=crop_rot, y=grain.buac_mean, fill=decade)) +
#   geom_bar(stat="identity", position=position_dodge(), color="#332288") +
#   geom_errorbar(width=0.3, aes(ymin=grain.buac_mean - grain.buac_se, ymax=grain.buac_mean + grain.buac_se),  
#                                position=position_dodge(0.9),
#                                 color="#332288") +
#   facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))), 
#              cols=vars(factor(cc, levels=c("NC", "CC"))), 
#              labeller = as_labeller(
#                c("NC"="No Cover Crop", "CC"="Rye Cover Crop", 
#                  "CT" = "Conventional Till", "RT"="Reduced Till","NT" = "No Till" ))) +
#   xlab("Crop") +
#   ylab("Grain carbon (bu per ac)") +
#   scale_fill_manual(values=pal6blue) +
#   theme(
#     panel.grid.minor=element_blank(), 
#     panel.grid.major=element_blank(),
#     panel.background = element_rect(fill = 'gray95'))
# 
# ggsave("plots/biomass/NY_bushelcrop_grainbiomass_decade_bars.png", width=10, height=8, dpi=300)
# 
# 
# # tons crops plot 
# ggplot(data=bm_tn_decsum[bm_tn_decsum$climate_scenario == "rcp60" ,], 
#        aes(x=crop_rot, y=bm.tac_mean, fill=decade)) +
#   geom_bar(stat="identity", position=position_dodge(), color="#332288") +
#   geom_errorbar(width=0.3, aes(ymin=bm.tac_mean - bm.tac_se, ymax=bm.tac_mean + bm.tac_se),  
#                 position=position_dodge(0.9),
#                 color="#332288") +
#   facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))), 
#              cols=vars(factor(cc, levels=c("NC", "CC"))), 
#              labeller = as_labeller(
#                c("NC"="No Cover Crop", "CC"="Rye Cover Crop", 
#                  "CT" = "Conventional Till", "RT"="Reduced Till","NT" = "No Till" ))) +
#   xlab("Crop") +
#   ylab("Aboveground biomass carbon (bu per ac)") +
#   scale_fill_manual(values=pal6blue) +
#   theme(
#     panel.grid.minor=element_blank(), 
#     panel.grid.major=element_blank(),
#     panel.background = element_rect(fill = 'gray95'))
# 
# ggsave("plots/biomass/NY_toncrop_ABVGDcarbon_decade_bars.png", width=10, height=8, dpi=300)






############ linear models / regressions to look at biomass change over time (not complete as of 10/4/2023)
############ what drives biomass over time, not a main research question, i.e., seems redundant with
############ just looking at how the model is built to predict biomass...

# set base levels to no cover and conventional till
bm$till <- relevel(factor(bm$till), ref="CT")
# bm$till <- ordered(bm$till, levels=c("CT", "RT", "NT"))
# bm$till <- factor(bm$till, ordered=F)
bm$cc <- relevel(factor(bm$cc), ref="NC")

# center data at mean
center_scale <- function(x) {
  scale(x, scale=F)
}

bm$year.sc <- center_scale(bm$Year)
bm$grain.sc <- center_scale(bm$Grain.C.kgC.ha.)


grmean <- mean(bm$Grain.C.kgC.ha.)
grsd <- sd(bm$Grain.C.kgC.ha.)
ssntotmean <- mean(bm$ssn.tot)
ssntotsd <- sd(bm$ssn.tot)
yrmean <- mean(bm$Year)
yrsd <- sd(bm$Year)
bm$grain.z <- (bm$Grain.C.kgC.ha.-grmean)/grsd
bm$ssn.tot.z <- (bm$ssn.tot-ssntotmean)/ssntotsd
bm$year.z <- (bm$Year - yrmean)/yrsd

grain <- bm[bm$crop_name %in% c("corn, grain", "soybean") & bm$Year>2021, ]



# check that data are normal
qqnorm(grain$Grain.C.kgC.ha.)  
qqline(grain$Grain.C.kgC.ha., distribution = qnorm)


# most of the explanatory power of the model comes from the two crops
blm0 <- lm(grain.z ~ year.z*crop_name, data=grain)
summary(blm0)
# R-squared is 0.6277, p< 2.2e-16
glance(blm0)
# AIC = 223463    # BIC 2.24e5  # when comparing these to other models, lower is better

# add in summer precip gain 0.15 in R2
blm0p <- lm(grain.z ~ year.z*crop_name + ssn.tot.z,  data=grain)
summary(blm0p)
# R-squared is 0.7756, p< 2.2e-16
glance(blm0p)
# AIC = 163956    # BIC 164014  

# add in cover crops
blm1p <- lm(grain.z ~ year.z*crop_name*cc + ssn.tot.z,  data=grain)
summary(blm1p)
# R-squared is 0.7759, p< 2.2e-16
glance(blm1p)
# AIC = 163836    # BIC 163933  


# add in tillage
blm2p <- lm(grain.z ~ year.z*crop_name*till + ssn.tot.z,  data=grain)
summary(blm2p)
# R-squared is 0.7769, p< 2.2e-16
glance(blm2p)
# AIC = 163324    # BIC 163460 


# add in site, a random effect


blm3p <- lmer(grain.z ~ year.z*crop_name + ssn.tot.z + (1|site_name),
              REML=F, # only one random effect, can use maximum likelihood not restricted ML
            data=grain)
summary(blm3p)
# R-squared is 0.79, p< 2.2e-16
glance(blm3p)
# AIC = 155646    # BIC 156004


# add in site, a random effect
blm3p <- lm(grain.z ~ year.z*crop_name + ssn.tot.z,  data=grain)
summary(blm3p)
# R-squared is 0.79, p< 2.2e-16
glance(blm3p)
# AIC = 155646    # BIC 156004





blm_fullp <- lm(grain.z ~ year.z*crop_name*climate_scenario*till*cc + ssn.tot.z, 
               data=bm[bm$crop_name %in% c("corn, grain", "soybean") & bm$Year>2021, ])
summary(blm_fullp)
# R-squared is 0.7784, p< 2.2e-16
glance(blm_fullp)
# AIC = 162567 ### BEST AIC SO FAR   # BIC 163050  # BEST BIC SO FAR

