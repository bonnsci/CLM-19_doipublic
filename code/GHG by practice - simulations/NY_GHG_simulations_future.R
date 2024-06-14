# Here we summarize future N2O emissions by DNDC simulations
# i.e., look for effects of till, cc, and Nfert on N2O emissions 2022-2072.

# packages
library(tidyverse) # has ggplot, dplyr, reshape2, etc.
library(reshape2) # for dcast / melt
library(MASS) # for boxcox

se <- function(x) sd(x) / sqrt(length(x))

# load data  ############ can skip 
# ghgdat <- read.csv("data/simulations/un-weighted_resultsNY.csv") # keep the "corn-grain"  "corn-silage" "corn-soy"    "soy-corn"   from this one
# ghgdat2 <- read.csv("data/simulations/un-weighted_resultsNYalf_20240220.csv") # new alfalfa rotation data -- removed irrigation and now have 5 rotations rather than 3 so all crops growing every year
# # as.data.frame(names(ghgdat))
# # names(ghgdat) - all are calendar year sums
# # 1         site_name
# # 2       region_name
# # 3         crop_name  #### this is actually the ROTATION And not the crop grown that year necessarily, see ghgdat$crop made below
# unique(ghgdat$crop_name) # "alfalfa-act" "alfalfa-cta" "alfalfa-tac" "corn-grain"  "corn-silage" "corn-soy"    "soy-corn"  
# unique(ghgdat2$crop_name) # "alfalfa-act"  "alfalfa-act2" "alfalfa-act3" "alfalfa-cta"  "alfalfa-cta2"
# # 4        management 
# unique(ghgdat$management) # "ct-cc" "ct-nc" "nt-cc" "nt-nc" "rt-cc" "rt-nc"
# # 5  climate_scenario
# # 6              year
# # 7              dsoc in tonne C/ha
# # 8               n2o DIRECT n2o em in tonne N2o/ha
# # 9               ch4 in tonne ch4/ha
# # 10              ghg sum of ghg_dsoc, ghg_ch4, ghg_total_n2o in tonne co2e /ha
# # 11     indirect_n2o in tonne n2o /ha
# # 12        total_n2o sum of n2o + indirect_n2o in tonne n2o/ha
# # 13         ghg_dsoc changes in soc stocks in tonne co2e/ha
# # 14          ghg_ch4 sum of ch4 em in co2e/ha
# # 15          ghg_n2o sum of DIRECT n2o em in tonne co2e/ha
# # 16 ghg_indirect_n2o sum of indirect n2o em in tonne co2e/ha
# # 17    ghg_total_n2o total n2o em in tonne co2e/ha
# 
# 
# 
# # combine the two datasets
# ghgdat <- ghgdat[ghgdat$crop_name %in% c("corn-grain" , "corn-silage" ,"corn-soy"  ,  "soy-corn"),]
# ghgdat2 <- ghgdat2[,-1]
# ghgdat <- rbind(ghgdat, ghgdat2)
# rm(ghgdat2)
# 
# 
# ghgdat <- ghgdat[ghgdat$year>2021 & ghgdat$year<2073& ghgdat$climate_scenario=="rcp60",c(1,3:6, 10, 13, 17)]
# 
# ghgdat$till <- ifelse(grepl("ct-", ghgdat$management), "CT", 
#                     ifelse(grepl("rt-", ghgdat$management), "RT", 
#                            ifelse(grepl("nt-", ghgdat$management), "NT", "X")))
# # # check
# # unique(ghgdat$till)
# 
# # factor for CC or NC
# ghgdat$cc <- ifelse(grepl("-cc", ghgdat$management), "CC", 
#                     ifelse(grepl("-nc", ghgdat$management), "NC", "X"))
# # # check
# # unique(ghgdat$cc)
# 
# 
# # for now, not making a factor for N treatment, because that is redundant with $crop (made below)
# # crop total N budgets:
# # corn grain monoculture 210 kg N/ha as UAN 
# # corn silage mono       210 kg N/ha (157.5 kgN as manure injected in June, 52.5 kg as UAN in July)
# # corn in corn-soy       170 kg N/ha as UAN (soybean credit)
# # soybean                  0 kg
# # alfalfa                  0 kg
# # corn in alf             25 kg N/ha as UAN
# # triticale in alf        56 kg N/ha (36 kg as dairy manure injected in Fall at planting; 20 kg as UAN in May)
# 
# # ghgdat$nfert <- ifelse(grepl("-cn", ghgdat$management), "High N", 
# #                      ifelse(grepl("-fn", ghgdat$management), "Fall N","Recommended N"))
# # # check
# # unique(ghgdat$nfert)
# 
# # factor for decade
# ghgdat$decade <- ifelse(ghgdat$year <2031, "2020s",
#                              ifelse(ghgdat$year>=2031 & ghgdat$year <2041, "2030s",
#                                     ifelse(ghgdat$year>=2041 & ghgdat$year <2051, "2040s",
#                                            ifelse(ghgdat$year>=2051 & ghgdat$year <2061, "2050s",
#                                                   ifelse(ghgdat$year>=2061 & ghgdat$year <2071, "2060s", "2070s")))))
# 
# 
# actcorn <- seq(2017,2072, 5)
# ctacorn <- seq(2013, 2072, 5)
# cta2corn <- seq(2014, 2072, 5)
# act2corn <- seq(2015, 2072, 5)
# act3corn <- seq(2016, 2072, 5)
# 
# actalf <- sort(c(seq(2014,2072, 5), seq(2015,2072,5), seq(2016,2072,5)))
# ctaalf <- sort(c(seq(2015,2072, 5), seq(2016,2072,5), seq(2017,2072,5)))
# cta2alf <- sort(c(seq(2016, 2072, 5), seq(2017, 2072, 5), seq(2018, 2072, 5)))
# act2alf <- sort(c(2013, 2014, seq(2017, 2072, 5), seq(2018, 2072, 5), seq(2019, 2072, 5)))
# act3alf <- sort(c(2013, 2014, 2015, seq(2018, 2072, 5), seq(2019, 2072, 5), seq(2020, 2072, 5)))
# 
# 
# acttri <- seq(2018,2072, 5)
# ctatri <- seq(2019, 2072, 5)
# cta2tri <- seq(2015, 2072, 5)
# act2tri <- seq(2016, 2072, 5)
# act3tri <- seq(2017, 2072, 5)
# 
# 
# 
# # label data for the crop they are (depends on the rotation and the year)
# ghgdat$crop <- ifelse(ghgdat$crop_name=="corn-soy" & ghgdat$year%%2 ==0, "soy cs",   # %%2 returns the remainder when divided by 2. if no remainder, then its an even number.
#                       ifelse(ghgdat$crop_name=="corn-soy" & !ghgdat$year%%2 ==0, "corn grain cs",  # cs= to indicate corn-soy rotation
#                              ifelse(ghgdat$crop_name=="soy-corn" & ghgdat$year%%2 ==0, "corn grain cs",
#                                     ifelse(ghgdat$crop_name=="soy-corn" & !ghgdat$year%%2 ==0, "soy cs", 
#                                            ifelse(ghgdat$crop_name=="corn-grain", "corn grain mono",
#                                                   ifelse(ghgdat$crop_name=="corn-silage", "corn silage mono",
#                                                         # corn in alfalfa rotation
#                                                           ifelse(ghgdat$crop_name=="alfalfa-act" & ghgdat$year %in% actcorn, "corn alf",
#                                                                 ifelse(ghgdat$crop_name=="alfalfa-cta" & ghgdat$year %in% ctacorn, "corn alf",
#                                                                        ifelse(ghgdat$crop_name=="alfalfa-cta2" & ghgdat$year %in% cta2corn, "corn alf",
#                                                                               ifelse(ghgdat$crop_name=="alfalfa-act2" & ghgdat$year %in% act2corn, "corn alf",
#                                                                                      ifelse(ghgdat$crop_name=="alfalfa-act3" & ghgdat$year %in% act3corn, "corn alf",
#                                                                              # alf in alf rotation
#                                                                                ifelse(ghgdat$crop_name=="alfalfa-act" & ghgdat$year %in% actalf, "alf",
#                                                                                      ifelse(ghgdat$crop_name=="alfalfa-cta" & ghgdat$year %in% ctaalf, "alf",
#                                                                                             ifelse(ghgdat$crop_name=="alfalfa-cta2" & ghgdat$year %in% cta2alf, "alf",
#                                                                                                    ifelse(ghgdat$crop_name=="alfalfa-act2" & ghgdat$year %in% act2alf, "alf",
#                                                                                                           ifelse(ghgdat$crop_name=="alfalfa-act3" & ghgdat$year %in% act3alf, "alf",
#                                                                                             # triticale in alf rotation
#                                                                                             ifelse(ghgdat$crop_name=="alfalfa-act" & ghgdat$year %in% acttri, "tri alf",
#                                                                                                    ifelse(ghgdat$crop_name=="alfalfa-cta" & ghgdat$year %in% ctatri, "tri alf",
#                                                                                                           ifelse(ghgdat$crop_name=="alfalfa-cta2" & ghgdat$year %in% cta2tri, "tri alf", 
#                                                                                                                  ifelse(ghgdat$crop_name=="alfalfa-act2" & ghgdat$year %in% act2tri, "tri alf", 
#                                                                                                                         ifelse(ghgdat$crop_name=="alfalfa-act3" & ghgdat$year %in% act3tri, "tri alf",   "X")))))))))))))))))))))
#                                              
# 
#  
# # check no Xs
# # unique(ghgdat$crop)
# 
# rm(act2alf, act2corn, act2tri, act3alf, act3corn, act3tri, actalf, actcorn, acttri, cta2alf, cta2corn, cta2tri, ctaalf, ctacorn, ctatri)
# 
# write.csv(ghgdat, "data/simulations/NY_ghgdat.csv", row.names=F)

ghgdat <- read.csv("data/simulations/NY_ghgdat.csv")

ghgdat$rot <- ifelse(ghgdat$crop_name %in% c("corn-soy", "soy-corn"), "corn soy",
                     ifelse(grepl("alfalfa-", ghgdat$crop_name), "alf", 
                            ifelse(ghgdat$crop_name == "corn-silage", "corn silage", 
                                   ifelse(ghgdat$crop_name == "corn-grain", "corn grain", "X"))))

# check no Xs
unique(ghgdat$rot)

# make stacked bar chart of n2o, soc, net by decade 
# with letters comparing the 2022-72 mean of each treatment group (cover*till*Nfert combo) for soc, n2o, net

# first, find the diffs among n2o bars, soc bars, net bars

ghgdat2 <- filter(ghgdat, !till=="RT", !rot=="alf")

########## just a side bar for answering REgrow question about averages
# ghgdatx <- filter(ghgdat, 
#                   rot== "corn grain",  # filter the data to just corn-grain monoculture
#                   management=="ct-nc", # just conventional till no cover crop
#                   climate_scenario=="rcp60") %>%  # RCP 6.0
#   group_by(site_name) %>%               # Group by site
#   summarize(dsoc_50yrtot_ac = sum(ghg_dsoc)/2.471,   # get the sum for each site across all the years
#             dsoc_mean_ac = mean(ghg_dsoc)/2.471)     # get the mean by site across all the years
# 
# ghgdatx
# 
# x <- mean(ghgdatx$dsoc_mean_ac)
# x
# x*50
# mean(ghgdatx$dsoc_50yrtot_ac)
################



lmn2o <- lm(ghg_total_n2o ~crop*till*cc, data=ghgdat2) 
lmsoc <- lm(ghg_dsoc ~crop*till*cc, data=ghgdat2)  
lmnet <- lm(ghg ~crop*till*cc, data=ghgdat2)  

summary(lmn2o)
# Call:
#   lm(formula = ghg_total_n2o ~ crop * till * cc, data = ghgdat2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.78419 -0.09574 -0.01469  0.07371  1.35415 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                       0.564901   0.007627  74.069  < 2e-16 ***
#   cropcorn grain mono               0.080050   0.010786   7.422 1.23e-13 ***
#   cropcorn silage mono             -0.088952   0.010786  -8.247  < 2e-16 ***
#   cropsoy cs                       -0.402514   0.010786 -37.319  < 2e-16 ***
#   tillNT                            0.321501   0.010786  29.808  < 2e-16 ***
#   ccNC                              0.054949   0.010786   5.095 3.54e-07 ***
#   cropcorn grain mono:tillNT        0.251979   0.015253  16.520  < 2e-16 ***
#   cropcorn silage mono:tillNT      -0.008212   0.015253  -0.538   0.5903    
#   cropsoy cs:tillNT                -0.178085   0.015253 -11.675  < 2e-16 ***
#   cropcorn grain mono:ccNC         -0.021297   0.015253  -1.396   0.1627    
#   cropcorn silage mono:ccNC         0.030698   0.015253   2.013   0.0442 *  
#   cropsoy cs:ccNC                  -0.010582   0.015253  -0.694   0.4878    
#   tillNT:ccNC                      -0.132091   0.015253  -8.660  < 2e-16 ***
#   cropcorn grain mono:tillNT:ccNC   0.032635   0.021571   1.513   0.1303    
#   cropcorn silage mono:tillNT:ccNC  0.207734   0.021571   9.630  < 2e-16 ***
#   cropsoy cs:tillNT:ccNC            0.110436   0.021571   5.120 3.11e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2179 on 13040 degrees of freedom
# Multiple R-squared:  0.6596,	Adjusted R-squared:  0.6592 
# F-statistic:  1685 on 15 and 13040 DF,  p-value: < 2.2e-16

summary(lmsoc)
# Call:
#   lm(formula = ghg_dsoc ~ crop * till * cc, data = ghgdat2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.85143 -0.36279 -0.00467  0.36064  2.71029 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                      -0.33715    0.01909 -17.664  < 2e-16 ***
#   cropcorn grain mono              -1.23623    0.02699 -45.799  < 2e-16 ***
#   cropcorn silage mono             -1.34766    0.02699 -49.928  < 2e-16 ***
#   cropsoy cs                       -1.58268    0.02699 -58.635  < 2e-16 ***
#   tillNT                           -0.53886    0.02699 -19.964  < 2e-16 ***
#   ccNC                              0.42268    0.02699  15.659  < 2e-16 ***
#   cropcorn grain mono:tillNT        0.90794    0.03817  23.785  < 2e-16 ***
#   cropcorn silage mono:tillNT      -0.34753    0.03817  -9.104  < 2e-16 ***
#   cropsoy cs:tillNT                 1.35101    0.03817  35.392  < 2e-16 ***
#   cropcorn grain mono:ccNC         -0.04232    0.03817  -1.109   0.2676    
#   cropcorn silage mono:ccNC         0.57541    0.03817  15.074  < 2e-16 ***
#   cropsoy cs:ccNC                   0.06852    0.03817   1.795   0.0727 .  
#   tillNT:ccNC                       0.17585    0.03817   4.607 4.13e-06 ***
#   cropcorn grain mono:tillNT:ccNC  -0.13428    0.05398  -2.487   0.0129 *  
#   cropcorn silage mono:tillNT:ccNC -0.29880    0.05398  -5.535 3.17e-08 ***
#   cropsoy cs:tillNT:ccNC           -0.24292    0.05398  -4.500 6.86e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5452 on 13040 degrees of freedom
# Multiple R-squared:  0.597,	Adjusted R-squared:  0.5966 
# F-statistic:  1288 on 15 and 13040 DF,  p-value: < 2.2e-16

summary(lmnet)
# Call:
#   lm(formula = ghg ~ crop * till * cc, data = ghgdat2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.43840 -0.39681  0.01063  0.40278  2.85261 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                       0.19877    0.02152   9.237  < 2e-16 ***
#   cropcorn grain mono              -1.15781    0.03043 -38.046  < 2e-16 ***
#   cropcorn silage mono             -1.43810    0.03043 -47.257  < 2e-16 ***
#   cropsoy cs                       -1.98656    0.03043 -65.280  < 2e-16 ***
#   tillNT                           -0.20334    0.03043  -6.682 2.45e-11 ***
#   ccNC                              0.47843    0.03043  15.721  < 2e-16 ***
#   cropcorn grain mono:tillNT        1.16371    0.04304  27.040  < 2e-16 ***
#   cropcorn silage mono:tillNT      -0.36948    0.04304  -8.585  < 2e-16 ***
#   cropsoy cs:tillNT                 1.17273    0.04304  27.249  < 2e-16 ***
#   cropcorn grain mono:ccNC         -0.06387    0.04304  -1.484   0.1378    
#   cropcorn silage mono:ccNC         0.60715    0.04304  14.108  < 2e-16 ***
#   cropsoy cs:ccNC                   0.05811    0.04304   1.350   0.1769    
#   tillNT:ccNC                       0.04073    0.04304   0.946   0.3439    
#   cropcorn grain mono:tillNT:ccNC  -0.10057    0.06086  -1.652   0.0985 .  
#   cropcorn silage mono:tillNT:ccNC -0.09486    0.06086  -1.559   0.1191    
#   cropsoy cs:tillNT:ccNC           -0.13215    0.06086  -2.171   0.0299 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6147 on 13040 degrees of freedom
# Multiple R-squared:  0.6025,	Adjusted R-squared:  0.6021 
# F-statistic:  1318 on 15 and 13040 DF,  p-value: < 2.2e-16


aovn2o <- aov(ghg_total_n2o ~crop*till*cc, data=ghgdat2)  
aovsoc <- aov(ghg_dsoc ~crop*till*cc, data=ghgdat2)  
aovnet <- aov(ghg ~crop*till*cc, data=ghgdat2)  

summary(aovn2o)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# crop             3  786.8   262.3 5525.43  < 2e-16 ***
#   till             1  325.4   325.4 6855.11  < 2e-16 ***
#   cc               1    3.4     3.4   72.45  < 2e-16 ***
#   crop:till        3   66.7    22.2  468.19  < 2e-16 ***
#   crop:cc          3   10.2     3.4   71.84  < 2e-16 ***
#   till:cc          1    1.6     1.6   33.88 6.01e-09 ***
#   crop:till:cc     3    5.2     1.7   36.75  < 2e-16 ***
#   Residuals    13040  618.9     0.0                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(aovsoc)
# Df Sum Sq Mean Sq  F value   Pr(>F)    
# crop             3   2984   994.5 3345.572  < 2e-16 ***
#   till             1     11    10.8   36.406 1.65e-09 ***
#   cc               1   1085  1084.9 3649.490  < 2e-16 ***
#   crop:till        3   1507   502.3 1689.834  < 2e-16 ***
#   crop:cc          3    146    48.7  163.689  < 2e-16 ***
#   till:cc          1      0     0.0    0.129     0.72    
# crop:till:cc     3     11     3.5   11.913 8.70e-08 ***
#   Residuals    13040   3876     0.3                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
summary(aovnet)
# Df Sum Sq Mean Sq  F value Pr(>F)    
# crop             3   4307  1435.7 3799.848 <2e-16 ***
#   till             1    234   234.1  619.605 <2e-16 ***
#   cc               1   1207  1207.4 3195.421 <2e-16 ***
#   crop:till        3   1489   496.5 1314.029 <2e-16 ***
#   crop:cc          3    227    75.8  200.492 <2e-16 ***
#   till:cc          1      1     1.4    3.659 0.0558 .  
# crop:till:cc     3      2     0.7    1.755 0.1535    
# Residuals    13040   4927     0.4                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukoutn2o <- TukeyHSD(aovn2o)
Tukoutsoc <- TukeyHSD(aovsoc)
Tukoutnet <- TukeyHSD(aovnet)


# compact letter display (cld)  ~ letters for bars
cldn2o<- multcompView::multcompLetters4(aovn2o, Tukoutn2o)
cldsoc<- multcompView::multcompLetters4(aovsoc, Tukoutsoc)
cldnet<- multcompView::multcompLetters4(aovnet, Tukoutnet)

# table with letters n2o
ghgn2osum <- group_by(ghgdat2, crop, cc, till) %>%
  summarize(mean.n2o=mean(ghg_total_n2o), 
            se.n2o=se(ghg_total_n2o)) %>%
  arrange(desc(mean.n2o))

cldn2o<- as.data.frame.list(cldn2o$`crop:till:cc`)
ghgn2osum$cldn2o <- cldn2o$Letters


# table with letters dsoc
ghgsocsum <- group_by(ghgdat2, crop, cc, till) %>%
  summarize(mean.soc=mean(ghg_dsoc), 
            se.soc=se(ghg_dsoc)) %>%
  arrange(desc(mean.soc))

cldsoc<- as.data.frame.list(cldsoc$`crop:till:cc`)
ghgsocsum$cldsoc <- cldsoc$Letters

# table with letters net
ghgnetsum <- group_by(ghgdat2, crop, cc, till) %>%
  summarize(mean.net=mean(ghg), 
            se.net=se(ghg)) %>%
  arrange(desc(mean.net))

cldnet<- as.data.frame.list(cldnet$`crop:till:cc`)
ghgnetsum$cldnet <- cldnet$Letters

# put the summaries together
ghgsummary <- left_join(ghgsocsum, ghgn2osum) %>%
  left_join(.,ghgnetsum)
# clean up
rm(ghgsocsum, ghgn2osum, ghgnetsum, cldn2o, cldnet, cldsoc, lmn2o, lmnet, lmsoc,
   Tukoutn2o, Tukoutnet, Tukoutsoc, aovn2o, aovnet, aovsoc)



# make plot with letters
windows(xpinch=200, ypinch=200, width=5, height=5)



####################### make a simpler version that is the 2022-2072 means


# convert em in tonnes / ha to tonnes /ac

ghgsummary <- mutate(ghgsummary, 
                     mean.n2o.ac=mean.n2o*2.47105,
                     se.n2o.ac=se.n2o*2.47105,
                     mean.soc.ac=mean.soc*2.47105,
                     se.soc.ac=se.soc*2.47105,
                     mean.net.ac=mean.net*2.47105,
                     se.net.ac=se.net*2.47105)


ggplot(data=ghgsummary, aes(x=crop)) +
  # n2o bars, error bars, letters
  geom_bar(aes(y=mean.n2o.ac), 
           stat="identity", position=position_dodge(), fill="#c44f2d", alpha=0.4) + #, color="gray20") +
  # geom_errorbar(aes(ymin= mean.n2o-se.n2o, ymax=mean.n2o+se.n2o),  
  #               width=0.3, position=position_dodge(0.9),color="#882255") +
  # geom_text(data=ghgsummary, aes(x=nfert, label=cldn2o, 
  #                                y=ifelse(mean.n2o<1.5, mean.n2o + 0.2*mean.n2o, mean.n2o+0.35*mean.n2o)), 
  #           vjust=-0.5, color="#882255", size=4, fontface="bold") +
  # 
  
  # dsoc bars, error bars, letters
  geom_bar(aes(y=mean.soc.ac),  
           stat="identity", position=position_dodge(), fill="#20243d", alpha=0.4) + #, color="gray20") +
  # geom_errorbar(aes(ymin= mean.soc-se.soc, ymax=mean.soc+se.soc),  
  #               width=0.3, position=position_dodge(0.9), color="#33bbee") +
  # geom_text(data=ghgsummary, aes(x=nfert, label=cldsoc, 
  #                                y=ifelse(mean.soc>-1, mean.soc + 0.5*mean.soc, mean.soc+0.35*mean.soc)), 
  #           vjust=-0.5, color="#0077BB", size=4, fontface="bold") +
  
  # zero-line
  geom_hline(yintercept=0, color="#20243d", linewidth=0.5) +
  
  # net points
  geom_point(aes(y=mean.net.ac),
             color="#20243d", size=1, position=position_dodge(0.9)) +
  # geom_errorbar(aes(ymin= mean.net-se.net, ymax=mean.net+se.net),  
  #               width=0.3, position=position_dodge(0.9), color="gray35") +
  # geom_text(data=ghgsummary, aes(label=cldnet, y=mean.net.ac-1),
  #           color="#20243d", size=4, fontface="bold.italic")+
  
  # make it pretty
  facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))),  #, "RT"
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             labeller = as_labeller(
               c("NC"="No Cover Crop", "CC"="Rye Cover Crop",
                 "CT" = "Conventional Till", "RT"= "Reduced Till", "NT" = "No Till"))) +  
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recomm. N"))) +
  # scale_fill_manual(values=c("#eaeccc", "#FEDA8B", "#FDB366", "#F67E4B", "#DD3D2D", "#A50026"),
  #                   name="Decade")+ #, name="N management") +
  
  ylab(expression('Mean annual emissions (tonnes CO'[2]*'e per acre)')) +
  # scale_y_continuous(breaks=seq(-6,5,1), limits=c(-6,5)) +
  
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-30, hjust=0, size=11))



ggsave("plots/ghgs/NY_mean em 2022-72 RCP60_no letters_20240312.png", width=10, height=8, dpi=300)






# Same graph as above but SOC only and average across N treatments for farmer report:

effectsoc <- aov(ghg_dsoc ~till*cc, data=ghgdat2)  # ghgdat$crop=="corn" &
summary(effectsoc)
# Df Sum Sq Mean Sq  F value   Pr(>F)    
# crop             3   2984   994.5 3345.572  < 2e-16 ***
#   till             1     11    10.8   36.406 1.65e-09 ***
#   cc               1   1085  1084.9 3649.490  < 2e-16 ***
#   crop:till        3   1507   502.3 1689.834  < 2e-16 ***
#   crop:cc          3    146    48.7  163.689  < 2e-16 ***
#   till:cc          1      0     0.0    0.129     0.72    
# crop:till:cc     3     11     3.5   11.913 8.70e-08 ***
#   Residuals    13040   3876     0.3                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukoutsoc <- TukeyHSD(effectsoc)
cldsoc<- multcompView::multcompLetters4(effectsoc, Tukoutsoc)
ghgsocsum <- ghgdat2 %>%
  group_by(cc, till) %>%
  summarize(mean.soc=mean(ghg_dsoc), 
            se.soc=se(ghg_dsoc)) %>%
  arrange(desc(mean.soc)) %>%
  ungroup() %>%
  mutate(mean.soc.ac = mean.soc/2.471,  #  metric t CO2e per ac
         se.soc.ac = se.soc/2.471,
         mean.soc.ac.assoc.USt = mean.soc.ac*(12/44)*1.10231,  # US t SOC per ac
         se.soc.ac.assoc.USt = se.soc.ac*(12/44)*1.10231)     # US t SOC per ac
         #dummy = rep("x", 16))

cldsoc<- as.data.frame.list(cldsoc$`till:cc`)
ghgsocsum$cldsoc <- cldsoc$Letters
ghgsocsum
                        # by cover and till only
# A tibble: 4 × 9
# cc    till  mean.soc se.soc mean.soc.ac se.soc.ac mean.soc.ac.assoc.USt se.soc.ac.assoc.USt cldsoc
# <chr> <chr>    <dbl>  <dbl>       <dbl>     <dbl>                 <dbl>               <dbl> <chr> 
#  1 NC    CT      -0.806 0.0132      -0.326   0.00533               -0.0980             0.00160 a     
#  2 NC    NT      -0.860 0.0135      -0.348   0.00548               -0.105              0.00165 b     
#  3 CC    CT      -1.38  0.0145      -0.558   0.00587               -0.168              0.00176 c     
#  4 CC    NT      -1.44  0.0153      -0.583   0.00618               -0.175              0.00186 d   

# "compared to CT without a CC, the system with CT and a rye CC gained _ more SOC per year"
(0.806-1.38)/0.806 # 0.71
# compared to a NT without a CC, the systems with NT and a rye cc gained _ more SOC per year
(0.860-1.44)/0.86  # 0.67
# without cover crops, no-till systems had _ % greater SOC rates than conventional tillage systems.
(0.806-0.860)/0.806  # 7%
# with cover crops, no-till systems had _ % greater SOC rates than conventional tillage systems.
(1.38-1.44)/1.38  # 4%

                        # by cover, till, and CROP
# # A tibble: 16 × 9
# cc    till  crop             mean.soc se.soc mean.soc.ac se.soc.ac dummy cldsoc
# <chr> <chr> <chr>               <dbl>  <dbl>       <dbl>     <dbl> <chr> <chr> 
#   1 NC    CT    corn grain cs      0.0855 0.0171      0.0346   0.00690 x     a     
# 2 NC    NT    corn grain cs     -0.277  0.0212     -0.112    0.00859 x     b     
# 3 CC    CT    corn grain cs     -0.337  0.0187     -0.136    0.00755 x     b     
# 4 NC    NT    soy cs            -0.684  0.0194     -0.277    0.00785 x     c     
# 5 NC    CT    corn silage mono  -0.687  0.0154     -0.278    0.00624 x     c     
# 6 NC    NT    corn grain mono   -0.782  0.0210     -0.317    0.00850 x     d     
# 7 CC    NT    corn grain cs     -0.876  0.0187     -0.355    0.00755 x     e     
# 8 CC    NT    soy cs            -1.11   0.0182     -0.448    0.00735 x     f     
# 9 NC    CT    corn grain mono   -1.19   0.0193     -0.483    0.00781 x     fg    
# 10 CC    NT    corn grain mono   -1.20   0.0210     -0.487    0.00851 x     g     
# 11 NC    CT    soy cs            -1.43   0.0151     -0.578    0.00612 x     h     
# 12 CC    CT    corn grain mono   -1.57   0.0203     -0.637    0.00822 x     i     
# 13 CC    CT    corn silage mono  -1.68   0.0219     -0.682    0.00887 x     j     
# 14 NC    NT    corn silage mono  -1.70   0.0187     -0.686    0.00755 x     j     
# 15 CC    CT    soy cs            -1.92   0.0166     -0.777    0.00671 x     k     
# 16 CC    NT    corn silage mono  -2.57   0.0212     -1.04     0.00856 x     l  

                        # by cover, till and ROTATION
# # A tibble: 12 × 10
# cc    till  rot         mean.soc se.soc mean.soc.ac se.soc.ac mean.soc.ac.assoc.USt se.soc.ac.assoc.USt cldsoc
# <chr> <chr> <chr>          <dbl>  <dbl>       <dbl>     <dbl>                 <dbl>               <dbl> <chr> 
#   1 NC    NT    corn soy      -0.481 0.0152      -0.194   0.00616               -0.0585             0.00185 a     
# 2 NC    CT    corn soy      -0.672 0.0219      -0.272   0.00888               -0.0817             0.00267 b     
# 3 NC    CT    corn silage   -0.687 0.0154      -0.278   0.00624               -0.0835             0.00187 bc    
# 4 NC    NT    corn grain    -0.782 0.0210      -0.317   0.00850               -0.0952             0.00256 c     
# 5 CC    NT    corn soy      -0.992 0.0133      -0.401   0.00539               -0.121              0.00162 d     
# 6 CC    CT    corn soy      -1.13  0.0232      -0.457   0.00940               -0.137              0.00283 e     
# 7 NC    CT    corn grain    -1.19  0.0193      -0.483   0.00781               -0.145              0.00235 e     
# 8 CC    NT    corn grain    -1.20  0.0210      -0.487   0.00851               -0.147              0.00256 e     
# 9 CC    CT    corn grain    -1.57  0.0203      -0.637   0.00822               -0.191              0.00247 f     
# 10 CC    CT    corn silage   -1.68  0.0219      -0.682   0.00887               -0.205              0.00267 g     
# 11 NC    NT    corn silage   -1.70  0.0187      -0.686   0.00755               -0.206              0.00227 g     
# 12 CC    NT    corn silage   -2.57  0.0212      -1.04    0.00856               -0.313              0.00257 h  

mean(c(0.429, 0.431)) # 0.43
mean(c(0.276, 0.275)) # 0.2755
(0.43-0.2755)/0.2755

# average effect of rye cover crop compared to CT NC
#corn grain mono
(0.483 - 0.637)/0.483 # 32%
# corn soy
(0.272-0.457)/0.272 # 68%
# silage
(0.278-0.682)/0.278 # 145%
mean(c(0.32, 0.68, 1.45))  # 0.8

# corn silage NT-CC compared to CT-CC or NT-NC
# NT-CC
1.04
# mean of CT-CC, NT-NC
mean(c(0.682, 0.686))  # 0.684
(1.04-0.684)/1.04 # 34%

# effect of rye cover crop in NT compared to NT NC
# NT-CC corn grain, corn soy, corn silage
mean(c(-0.487, -0.401, -1.04))  # -0.64
# NT-NC 
mean(c(-0.317, -0.194, -0.686))  # -0.399
(0.317-0.487)/0.317 # 53%
(0.194-0.401)/0.194 # 107%
(0.686-1.04)/0.686 # 52%


ghgsocsum$cctill <- paste0(ghgsocsum$till, "-", ghgsocsum$cc)

pal4 <- c("#c44f2d","#20243d", "#C2e4ef", "#669947")

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=ghgsocsum, aes(x=cctill, fill=cctill, y=mean.soc.ac*-1)) +  # Data are in terms of emissions, so -dSOC is negative emissions, 
  # dsoc bars, error bars, letters
  geom_bar(stat="identity", position=position_dodge(), width=0.7, show.legend=F) + #, color="gray20") +
  geom_errorbar(aes(ymin= (-1*mean.soc.ac)-se.soc.ac, ymax=(-1*mean.soc.ac)+se.soc.ac),  
                width=0.3, position=position_dodge(0.9), color="#20243d") +
  geom_text(aes(x=cctill, label=cldsoc, y= (mean.soc.ac*-1) + 0.05),
             vjust=-0.5, color="#0077BB", size=4, fontface="bold") +
  # facet_grid(cols=vars(factor(rot, levels=c("corn grain", "corn soy", "corn silage")))) + 
  # 
  # zero-line
  # geom_hline(yintercept=0, color="#20243d", linewidth=0.5) +
  scale_fill_manual(values=pal4, breaks=c("CT-NC", "CT-CC", "NT-NC", "NT-CC")) +
  scale_x_discrete(limits=c("CT-NC", "CT-CC", "NT-NC", "NT-CC")) +
  ylab(expression('Mean annual change in SOC (US tons CO'[2]*'e per acre)')) +
  
  # scale_y_continuous(breaks=seq(-6,5,1), limits=c(-6,5)) +
  
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/ghgs/NY_simulation mean annual SOC buildup_average 3 rots_with letters.png", width=4, height=3, dpi=300)






# mean annual emissions by decade by rotation

ghgdec <- ghgdat %>%
  group_by(crop, till, cc, decade) %>%  #  calculate means and se by decade
  summarize(net_mean=mean(ghg),
            net_se=se(ghg),
            ghgdsoc_mean = mean(ghg_dsoc),
            ghgdsoc_se=se(ghg_dsoc),
            ghgn2o_mean = mean(ghg_total_n2o),
            ghgn2o_se = se(ghg_total_n2o)
  ) %>%
  pivot_longer(cols=-c("crop", "till", "cc", "decade"),
               names_to=c("component", ".value"),
               names_sep="_")


       
ggplot() +
  # n2o bars, error bars, letters
  geom_bar(# data=ghgdec[ghgdec$crop == "corn" & ghgdec$var=="ghgn2o" & ghgdec$till %in% c("CT", "NT"),], 
           data=ghgdec[ghgdec$component == "ghgn2o",],# need separate call for bars for n2o, for soc, and points for net
           aes(x=crop, y=mean, group=decade), 
           stat="identity", position=position_dodge(), fill="#ee8866", alpha=0.7, color="#bb5566") + #, color="gray20") +
  geom_errorbar(data=ghgdec[ghgdec$component=="ghgn2o",], # ghgdec$crop == "corn" &
                aes(x=crop, ymin= mean-se, ymax=mean+se, group=decade),  
                width=0.3, position=position_dodge(0.9),color="#882255") +
  #geom_bar(data=ghgsummary, aes(x=nfert, y=mean.n2o), stat="identity", color=NA, fill=NA) +
  # geom_text(data=ghgsummary, aes(x=nfert, label=cldn2o, 
                                 # y=ifelse(mean.n2o<1.5, mean.n2o + 0.2*mean.n2o, mean.n2o+0.35*mean.n2o)), 
            # vjust=-0.5, color="#882255", size=4, fontface="bold") +
 
  
  # dsoc bars, error bars, letters
  geom_bar(data=ghgdec[ghgdec$component == "ghgdsoc",],  # need separate call for bars for n2o, for soc, and points for net

    aes(x=crop, y=mean, group=decade),  
    stat="identity", position=position_dodge(), fill="#99ddff", alpha=0.7, color="#33bbee") + #, color="gray20") +
  geom_errorbar(data=ghgdec[ghgdec$component=="ghgdsoc" ,], 
               aes(x=crop, ymin= mean-se, ymax=mean+se, group=decade),  
               width=0.3, position=position_dodge(0.9), color="#33bbee") +
  #geom_bar(data=ghgsummary, aes(x=nfert, y=mean.soc), stat="identity", color=NA, fill=NA) +
  # geom_text(data=ghgsummary, aes(x=nfert, label=cldsoc, 
                                 # y=ifelse(mean.soc>-1, mean.soc + 0.5*mean.soc, mean.soc+0.35*mean.soc)), 
            # vjust=-0.5, color="#0077BB", size=4, fontface="bold") +
  
  # zero-lin
  geom_hline(yintercept=0, color="#009988", linewidth=0.5) +
  
  # net points letters
  # geom_line(data=ghgdec[ghgdec$var == "net" &
  #                             ghgdec$till %in% c("CT", "NT"),],
  #            aes(x=nfert, y=mean),
  #            alpha=0.6, linewidth=1, position=position_dodge(0.9)) +  # color="gray25", 
  geom_point(data=ghgdec[ghgdec$component == "net",],
             aes(x=crop, y=mean, group=decade),
             color="gray25", size=0.5, position=position_dodge(0.9)) +
  geom_errorbar(data=ghgdec[ghgdec$component=="net" ,], 
              aes(x=crop, ymin= mean-se, ymax=mean+se, group=decade),  
              width=0.3, position=position_dodge(0.9), color="gray35") +
  # geom_text(data=ghgsummary, aes(x=nfert, label=cldnet, y=mean.net-0.3),
  #                                color="black", size=4, fontface="bold.italic")+

  # make it pretty
  facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))),  #, "RT"
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
                      # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             labeller = as_labeller(
               c("NC"="No Cover Crop", "CC"="Rye Cover Crop",
                 "CT" = "Conventional Till", "RT" = "Reduced Till", "NT" = "No Till"))) +  #, "RT"="Reduced Till"))) +
                 #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recomm. N"))) +
  # scale_fill_manual(values=c("#eaeccc", "#FEDA8B", "#FDB366", "#F67E4B", "#DD3D2D", "#A50026"),
  #                   name="Decade")+ #, name="N management") +
  xlab("Decade") +
  ylab(expression('Mean annual emissions (CO'[2]*'e ha'^-1*')')) +
  # scale_y_continuous(breaks=seq(-3,2,1), limits=c(-3,2.8)) +
 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-30, hjust=0, size=11))
  
  

ggsave("plots/ghgs/NY_simulation mean em by decade RCP60_no letters.png", width=6, height=5, dpi=300)






################ make a simpler version averaged across N treatments



# mean annual n2o emissions by decade

ghgdec2 <- ghgdat %>%
  group_by(till, cc, decade) %>%  # , crop # calculate means and se across all sites and 2 crops and 3 Nferts
  # showing corn-soy mean rather than just corn because 50 years of just corn data is like continuous corn rotation on a ha
  # which most ha are not
  summarize(net.mean=mean(ghg),
            net.se=se(ghg),
            ghgdsoc.mean = mean(ghg_dsoc),
            ghgdsoc.se=se(ghg_dsoc),
            ghgn2o.mean = mean(ghg_total_n2o),
            ghgn2o.se = se(ghg_total_n2o)
  ) %>%
  melt(id=c("till", "cc", "decade")) %>%  # , "crop", "nfert"
  rename(var1=variable) %>%
  mutate(mean.se = ifelse(grepl(pattern="mean", x=var1), "mean", "se"),
         var2 = ifelse(mean.se=="mean", gsub(pattern = ".mean", replacement="", x=var1), 
                       gsub(pattern = ".se", replacement="", x=var1))) %>%
  dplyr::select(-var1) %>%
  dcast(till + cc  + decade +var2 ~mean.se) # + crop 


# $value is in units of cumulative tco2e/ha


# make stacked bar chart of n2o, soc, net by decade 
# with letters comparing the 2022-72 mean of each treatment group (cover*till*Nfert combo) for soc, n2o, net

# first, find the diffs among n2o bars, soc bars, net bars

effectn2o <- aov(ghg_total_n2o ~till*cc, data=ghgdat[ghgdat$till %in% c("NT", "CT"),])  # ghgdat$crop=="corn" &
effectsoc <- aov(ghg_dsoc ~till*cc, data=ghgdat[ghgdat$till %in% c("NT", "CT"),])  # ghgdat$crop=="corn" &
effectnet <- aov(ghg ~till*cc, data=ghgdat[ghgdat$till %in% c("NT", "CT"),])  # ghgdat$crop=="corn" &

summary(effectn2o)
# Df Sum Sq Mean Sq F value Pr(>F)    
# till            1   3323    3323  9776.8 <2e-16 ***
# cc              1     37      37   108.1 <2e-16 ***
# till:cc         1    167     167   490.5 <2e-16 ***
# Residuals   39548  13443       0                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(effectsoc)
# Df Sum Sq Mean Sq   F value Pr(>F)    
# till            1     82      82   100.470 <2e-16 ***
# cc              1  15606   15606 19102.264 <2e-16 ***
# till:cc         1      2       2     2.194  0.139    
# Residuals   39548  32309       1                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(effectnet)
# Df Sum Sq Mean Sq F value Pr(>F)    
# till            1   2511    2511    1551 <2e-16 ***
# cc              1  14166   14166    8752 <2e-16 ***
# till:cc         1    209     209     129 <2e-16 ***
# Residuals   39548  64017       2                   

Tukoutn2o <- TukeyHSD(effectn2o)
# # put interaction output into a dataframe we can sort
# Tukout <- as.data.frame(Tukout[7]) %>%  # [7] is the 3-way interaction term
#   rownames_to_column(., "term") %>%
#   arrange(term)

Tukoutsoc <- TukeyHSD(effectsoc)
Tukoutnet <- TukeyHSD(effectnet)


# compact letter display (cld)  ~ letters for bars
cldn2o<- multcompView::multcompLetters4(effectn2o, Tukoutn2o)
cldsoc<- multcompView::multcompLetters4(effectsoc, Tukoutsoc)
cldnet<- multcompView::multcompLetters4(effectnet, Tukoutnet)

# table with letters n2o
ghgn2osum <- filter(ghgdat,till %in% c("NT", "CT")) %>%  # crop=="corn",
  group_by(cc, till) %>%
  summarize(mean.n2o=mean(ghg_total_n2o), 
            se.n2o=se(ghg_total_n2o)) %>%
  arrange(desc(mean.n2o))

cldn2o<- as.data.frame.list(cldn2o$`till:cc`)
ghgn2osum$cldn2o <- cldn2o$Letters


# table with letters dsoc
ghgsocsum <- filter(ghgdat,till %in% c("NT", "CT")) %>%  # crop=="corn",
  group_by(cc, till) %>%
  summarize(mean.soc=mean(ghg_dsoc), 
            se.soc=se(ghg_dsoc)) %>%
  arrange(desc(mean.soc))

cldsoc<- as.data.frame.list(cldsoc$`till:cc`)
ghgsocsum$cldsoc <- cldsoc$Letters

# table with letters net
ghgnetsum <- filter(ghgdat,till %in% c("NT", "CT")) %>%  # crop=="corn",
  group_by(cc, till) %>%
  summarize(mean.net=mean(ghg), 
            se.net=se(ghg)) %>%
  arrange(desc(mean.net))

cldnet<- as.data.frame.list(cldnet$`till:cc`)
ghgnetsum$cldnet <- cldnet$Letters

# put the summaries together
ghgsummary <- left_join(ghgsocsum, ghgn2osum) %>%
  left_join(.,ghgnetsum)
# clean up
rm(ghgsocsum, ghgn2osum, ghgnetsum, cldn2o, cldnet, cldsoc)

# dummy data for letter labels
cc <- c("NC","NC","CC","CC")
till <- c("CT", "NT", "CT", "NT")
xs <- rep(3.5, 4)
n2o.ys <- c(1.2, 1.8, 1.3, 2)
soc.ys <- c(-1, -1.1, -2.5, -2.5)
net.ys <- c(0.6, 1, -0.9, -0.1)
ghgsummary <- left_join(ghgsummary,data.frame(cc=cc, till=till, xs=xs, n2o.ys = n2o.ys, soc.ys=soc.ys, net.ys=net.ys) )
rm(cc, till, xs, n2o.ys, soc.ys, net.ys)

# make plot with letters
windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot() +
  # n2o bars, error bars, letters
  geom_bar(# data=ghgdec2[ghgdec2$crop == "corn" & ghgdec2$var=="ghgn2o" & ghgdec2$till %in% c("CT", "NT"),], 
    data=ghgdec2[ghgdec2$var == "ghgn2o" &  # need separate call for bars for n2o, for soc, and points for net
                     ghgdec2$till %in% c("CT", "NT"),], # for AGU only showing CT and NT not RT to simplify a bit
    aes(x=decade, y=mean), 
    stat="identity", position=position_dodge(), fill="#ee8866", alpha=0.4) + #, color="gray20") + color="#bb5566"
  geom_errorbar(data=ghgdec2[ghgdec2$var=="ghgn2o" & ghgdec2$till %in% c("CT", "NT"),], # ghgdec2$crop == "corn" &
                aes(x=decade, ymin= mean-se, ymax=mean+se),  
                width=0.3, position=position_dodge(0.9),color="#882255") +
  #geom_bar(data=ghgsummary, aes(x=decade, y=mean.n2o), stat="identity", color=NA, fill=NA) +
  geom_text(data=ghgsummary, aes(x=xs, label=cldn2o, y=n2o.ys),
            color="#882255", size=7, fontface="bold") +  #  vjust=-0.5,

  
  # dsoc bars, error bars, letters
  geom_bar(data=ghgdec2[ghgdec2$var == "ghgdsoc" &  # need separate call for bars for n2o, for soc, and points for net
                            ghgdec2$till %in% c("CT", "NT"),], # for AGU only showing CT and NT not RT to simplify a bit
           aes(x=decade, y=mean),  
           stat="identity", position=position_dodge(), fill="#99ddff", alpha=0.4) + #, color="gray20") + color="#33bbee"
  geom_errorbar(data=ghgdec2[ghgdec2$var=="ghgdsoc" & ghgdec2$till %in% c("CT", "NT"),], 
                aes(x=decade, ymin= mean-se, ymax=mean+se),  
                width=0.3, position=position_dodge(0.9), color="#0077BB") +
  #geom_bar(data=ghgsummary, aes(x=decade, y=mean.soc), stat="identity", color=NA, fill=NA) +
  geom_text(data=ghgsummary, aes(x=xs, label=cldsoc, y=soc.ys),
            vjust=-0.5, color="#0077BB", size=7, fontface="bold") +
  
  # zero-line
  geom_hline(yintercept=0, color="#009988", linewidth=0.5) +
  
  # net points letters
  geom_line(data=ghgdec2[ghgdec2$var == "net" &
                              ghgdec2$till %in% c("CT", "NT"),],
             aes(x=decade, y=mean, group=1),
             alpha=0.7, linewidth=1, position=position_dodge(0.9)) +  # color="gray25",
  geom_point(data=ghgdec2[ghgdec2$var == "net" &
                              ghgdec2$till %in% c("CT", "NT"),],
             aes(x=decade, y=mean),
             color="black", size=1, position=position_dodge(0.9)) +
  geom_errorbar(data=ghgdec2[ghgdec2$var=="net" & 
                                 ghgdec2$till %in% c("CT", "NT"),], 
                aes(x=decade, ymin= mean-se, ymax=mean+se),  
                width=0.3, position=position_dodge(0.9), color="gray35") +
  geom_text(data=ghgsummary, aes(x=xs, label=cldnet, y=net.ys),
            color="black", size=7, fontface="bold.italic")+
  
  # make it pretty
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT"))),  #, "RT"
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             labeller = as_labeller(
               c("NC"="No Cover Crop", "CC"="Has Cover Crop",
                 "CT" = "Conventional Till", "NT" = "No Till"))) +  #, "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recomm. N"))) +
  # scale_fill_manual(values=c("#eaeccc", "#FEDA8B", "#FDB366", "#F67E4B", "#DD3D2D", "#A50026"),
  #                   name="Decade")+ #, name="N management") +
  xlab("Decade") +
  ylab(expression(bold('Mean annual emissions (CO'[2]*'e ha'^-1*')'))) +
  #scale_y_continuous(breaks=seq(-3,2,1), limits=c(-3,2.8)) +
  
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray97'),
    axis.text.x=element_text(angle=-30, hjust=0.5, vjust=0, size=12, face="bold"),
    axis.text.y=element_text(size=12, face="bold"),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))



ggsave("plots/ghgs/IL_simulation mean (crop, site, decade, no nfert) annual em by decadeRCP60_with letters.png", width=7, height=8, dpi=300)


















######################### plot of 2022 "current" by practice - mean across crops
#########################
# plot of 2022 "current" by practice - mean across crops
# if you want crop specific it looks like this:
# ghgdatl <- ghgdat[,c(1:2,5:12)] %>%
#   group_by(crop_name, year, till, cc, nfert, decade) %>%
#   summarize(net.mean=mean(ghg),  # mean and se across sites
#             net.se=se(ghg),
#             ghgdsoc.mean = mean(ghg_dsoc),
#             ghgdsoc.se=se(ghg_dsoc),
#             ghgn2o.mean = mean(ghg_total_n2o),
#             ghgn2o.se = se(ghg_total_n2o)
#   ) %>%
#   melt(id=c("year",  "crop_name", "till", "cc", "nfert", "decade")) %>%
#   rename(var1=variable) %>%
#   mutate(mean.se = ifelse(grepl(pattern="mean", x=var1), "mean", "se"),
#          var2 = ifelse(mean.se=="mean", gsub(pattern = ".mean", replacement="", x=var1), 
#                        gsub(pattern = ".se", replacement="", x=var1))) %>%
#   dplyr::select(-var1) %>%
#   dcast(year + crop_name + till + cc + nfert + decade +var2 ~mean.se)

# mean across crops WITH NFERT LEVELS
ghgdatl_cropmean <- ghgdat[,c(1:2,5:12)] %>%
  group_by(year, till, cc, nfert, decade) %>%
  summarize(net.mean=mean(ghg),  # mean and se across sites
            net.se=se(ghg),
            ghgdsoc.mean = mean(ghg_dsoc),
            ghgdsoc.se=se(ghg_dsoc),
            ghgn2o.mean = mean(ghg_total_n2o),
            ghgn2o.se = se(ghg_total_n2o)
  ) %>%
  melt(id=c("year",  "till", "cc", "nfert", "decade")) %>%
  rename(var1=variable) %>%
  mutate(mean.se = ifelse(grepl(pattern="mean", x=var1), "mean", "se"),
         var2 = ifelse(mean.se=="mean", gsub(pattern = ".mean", replacement="", x=var1), 
                       gsub(pattern = ".se", replacement="", x=var1))) %>%
  dplyr::select(-var1) %>%  # select is used by other packages too and doesn't work here without dplyr::
  dcast(year + till + cc + nfert + decade +var2 ~mean.se)

# make the plot "current" / 2022 by practice WITH NFERT levels
ggplot(data=ghgdatl_cropmean[ghgdatl_cropmean$till %in% c("CT", "NT") & 
                               ghgdatl_cropmean$year == 2022 & 
                               ghgdatl_cropmean$var2 %in% c("ghgdsoc", "ghgn2o"),],
       aes(x=nfert, y=mean)) +
  geom_bar(aes(fill= var2), stat="identity", position= "stack", alpha=0.7, show.legend=F) +
  scale_fill_manual(values=c("#ee8866","#99ddff"), 
                    breaks=c("ghgn2o", "ghgdsoc"), 
                    name = "Source/Sink", 
                    labels=c(expression('Total N'[2]*'O emissions', "Change in SOC"))) + #, labels=c("ghg_dsoc", "ghg_tn2o")) +
  geom_hline(yintercept=0, color="#009988", linewidth=0.5) +
  geom_point(data=ghgdatl_cropmean[ghgdatl_cropmean$var2 == "net" &
                                     ghgdatl_cropmean$till %in% c("CT", "NT") & 
                                     ghgdatl_cropmean$year == 2022 ,], # 
             aes(x=nfert, y=mean),
             size = 0.5, color="gray25") +
  geom_errorbar(data=ghgdatl_cropmean[ghgdatl_cropmean$till %in% c("CT", "NT") & 
                                        ghgdatl_cropmean$year == 2022,], 
                aes(x=nfert, y=mean, ymin=mean-se, ymax=mean+se, color=var2),
                width = 0.2, show.legend=F) +
  labs(x="N management", 
       y=expression('2022 mean annual emissions (tonnes CO'[2]*'e ha'^'-1'*')')) +
  scale_color_manual(values=c("#0077BB", "#882255", "gray30"), breaks=c("ghgdsoc", "ghgn2o", "net")) +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT"))),   # set the order of facets here. setting the factor to ordered doesn't affect it here
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c("NC"="No Cover Crop","CC"="Has Cover Crop", 
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    legend.text.align=0,
    axis.text.x=element_text(angle=-20, hjust=0))

ggsave("plots/ghgs/IL_simulations net annual em 2022.png", width=6, height=4, dpi=300)


# are the net effects in the bars above significantly different?

# need mean of data across corn-soy
ghgdat_cropmean <- ghgdat[,c(1:2,5:12)] %>%
  group_by(site_name, year, till, cc, nfert, decade) %>%
  summarize(net.mean=mean(ghg),  # mean and se across sites
            ghgdsoc.mean = mean(ghg_dsoc),
            ghgn2o.mean = mean(ghg_total_n2o))

netlm <- lm(net.mean~till*cc*nfert, data=ghgdat_cropmean[ghgdat_cropmean$year==2022,])
summary(netlm)




# mean across crops WITHOUT NFERT LEVELS
ghgdatl_cropmean <- ghgdat[ghgdat$year==2022 & ghgdat$till %in% c("NT", "CT"),c(1:2,5:12)] %>%
  group_by(year, till, cc, decade) %>%
  summarize(net.mean=mean(ghg),  # mean and se across sites
            net.se=se(ghg),
            ghgdsoc.mean = mean(ghg_dsoc),
            ghgdsoc.se=se(ghg_dsoc),
            ghgn2o.mean = mean(ghg_total_n2o),
            ghgn2o.se = se(ghg_total_n2o)
  ) %>%
  melt(id=c("year",  "till", "cc", "decade")) %>%
  rename(var1=variable) %>%
  mutate(mean.se = ifelse(grepl(pattern="mean", x=var1), "mean", "se"),
         var2 = ifelse(mean.se=="mean", gsub(pattern = ".mean", replacement="", x=var1), 
                       gsub(pattern = ".se", replacement="", x=var1))) %>%
  dplyr::select(-var1) %>%  # select is used by other packages too and doesn't work here without dplyr::
  dcast(year + till + cc +decade +var2 ~mean.se)


# first, find the diffs among n2o bars, soc bars, net bars

effectn2o <- aov(ghg_total_n2o ~till*cc, data=ghgdat[ghgdat$till %in% c("NT", "CT") & ghgdat$year==2022,])  # ghgdat$crop=="corn" &
effectsoc <- aov(ghg_dsoc ~till*cc, data=ghgdat[ghgdat$till %in% c("NT", "CT") & ghgdat$year==2022,])  # ghgdat$crop=="corn" &
effectnet <- aov(ghg ~till*cc, data=ghgdat[ghgdat$till %in% c("NT", "CT") & ghgdat$year==2022,])  # ghgdat$crop=="corn" &

summary(effectn2o)
#               Df Sum Sq Mean Sq F value   Pr(>F)    
# till          1   0.44   0.442   5.173   0.0232 *  
# cc            1   3.34   3.338  39.105 6.68e-10 ***
# till:cc       1   0.26   0.259   3.040   0.0817 .  
# Residuals   764  65.21   0.085                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(effectsoc)
#               Df Sum Sq Mean Sq F value  Pr(>F)    
# till          1    5.6     5.6   9.971 0.00165 ** 
# cc            1  324.3   324.3 573.112 < 2e-16 ***
# till:cc       1    2.1     2.1   3.799 0.05166 .  
# Residuals   764  432.4     0.6                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(effectnet)
#               Df Sum Sq Mean Sq F value  Pr(>F)    
# till          1    8.7     8.7   9.497 0.00213 ** 
# cc            1  393.5   393.5 428.791 < 2e-16 ***
# till:cc       1    4.0     4.0   4.317 0.03807 *  
# Residuals   764  701.2     0.9                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukoutn2o <- TukeyHSD(effectn2o)
# # put interaction output into a dataframe we can sort
# Tukout <- as.data.frame(Tukout[7]) %>%  # [7] is the 3-way interaction term
#   rownames_to_column(., "term") %>%
#   arrange(term)

Tukoutsoc <- TukeyHSD(effectsoc)
Tukoutnet <- TukeyHSD(effectnet)


# compact letter display (cld)  ~ letters for bars
cldn2o<- multcompView::multcompLetters4(effectn2o, Tukoutn2o)
cldsoc<- multcompView::multcompLetters4(effectsoc, Tukoutsoc)
cldnet<- multcompView::multcompLetters4(effectnet, Tukoutnet)

# table with letters n2o
ghgn2osum <- filter(ghgdat,till %in% c("NT", "CT"), year==2022) %>%  # crop=="corn",
  group_by(cc, till) %>%
  summarize(mean.n2o=mean(ghg_total_n2o), 
            se.n2o=se(ghg_total_n2o)) %>%
  arrange(desc(mean.n2o))

cldn2o<- as.data.frame.list(cldn2o$`till:cc`)
ghgn2osum$cldn2o <- cldn2o$Letters


# table with letters dsoc
ghgsocsum <- filter(ghgdat,till %in% c("NT", "CT"), year==2022) %>%  # crop=="corn",
  group_by(cc, till) %>%
  summarize(mean.soc=mean(ghg_dsoc), 
            se.soc=se(ghg_dsoc)) %>%
  arrange(desc(mean.soc))

cldsoc<- as.data.frame.list(cldsoc$`till:cc`)
ghgsocsum$cldsoc <- cldsoc$Letters

# table with letters net
ghgnetsum <- filter(ghgdat,till %in% c("NT", "CT"), year==2022) %>%  # crop=="corn",
  group_by(cc, till) %>%
  summarize(mean.net=mean(ghg), 
            se.net=se(ghg)) %>%
  arrange(desc(mean.net))

cldnet<- as.data.frame.list(cldnet$`till:cc`)
ghgnetsum$cldnet <- cldnet$Letters

# put the summaries together
ghgsummary <- left_join(ghgsocsum, ghgn2osum) %>%
  left_join(.,ghgnetsum)
# clean up
rm(ghgsocsum, ghgn2osum, ghgnetsum, cldn2o, cldnet, cldsoc)
ghgsummary$year <- rep(2022, nrow(ghgsummary))

# dummy data for letter labels
# cc <- c("NC","NC","CC","CC")
# till <- c("CT", "NT", "CT", "NT")
# xs <- rep(3.5, 4)
# n2o.ys <- c(1.2, 1.8, 1.3, 2)
# soc.ys <- c(-1, -1.1, -2.5, -2.5)
# net.ys <- c(0.6, 1, -0.9, -0.1)
# ghgsummary <- left_join(ghgsummary,data.frame(cc=cc, till=till, xs=xs, n2o.ys = n2o.ys, soc.ys=soc.ys, net.ys=net.ys) )
# rm(cc, till, xs, n2o.ys, soc.ys, net.ys)



# make the plot "current" / 2022 by practice WITHOUT NFERT levels
ggplot() +
  geom_bar(data=ghgdatl_cropmean[ghgdatl_cropmean$var2 %in% c("ghgdsoc", "ghgn2o"),],
           aes(x=factor(year), y=mean, fill=var2),
           stat="identity", position= "stack", alpha=0.7, show.legend=F, width=0.6) +
  scale_fill_manual(values=c("#ee8866","#99ddff"), 
                    breaks=c("ghgn2o", "ghgdsoc"), 
                    name = "Source/Sink", 
                    labels=c(expression('Total N'[2]*'O emissions', "Change in SOC"))) + #, labels=c("ghg_dsoc", "ghg_tn2o")) +
  geom_hline(yintercept=0, color="#009988", linewidth=0.5) +
  geom_point(data=ghgdatl_cropmean[ghgdatl_cropmean$var2 == "net" ,], # 
             aes(x=factor(year), y=mean),
             size = 0.5, color="gray25") +
  geom_errorbar(data=ghgdatl_cropmean, 
                aes(x=factor(year), y=mean, ymin=mean-se, ymax=mean+se, color=var2),
                width = 0.2, show.legend=F) +
  geom_text(data=ghgsummary, aes(x=factor(year), label=cldnet, y=mean.net+0.4),
            color="black", size=6, fontface="bold.italic")+
  geom_text(data=ghgsummary, aes(x=factor(year), label=cldn2o, y=mean.n2o+0.4),
            color="#882255", size=6, fontface="bold", alpha=0.7)+
  geom_text(data=ghgsummary, aes(x=factor(year), label=cldsoc, y=mean.soc-0.25),
            color="#0077BB", size=6, fontface="bold", alpha=0.7)+
  labs(x="Year", 
       y=expression(bold('Mean annual emissions (tonnes CO'[2]*'e ha'^'-1'*')'))) +
  scale_color_manual(values=c("#0077BB", "#882255", "gray30"), breaks=c("ghgdsoc", "ghgn2o", "net")) +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT"))),   # set the order of facets here. setting the factor to ordered doesn't affect it here
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c("NC"="No Cover Crop","CC"="Has Cover Crop", 
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    legend.text.align=0,
    axis.text=element_text(size=12, face="bold"),
    axis.title=element_text(size=12, face="bold"))

ggsave("plots/ghgs/IL_simulations net annual em 2022 withletters.png", width=4, height=5, dpi=300)


















# are the group means significantly different?

n2odat <- ghgdecl[ghgdecl$variable=="n2o_tot",c(1:4,6)]

Neffect <- aov(value~till*cc*nfert, data=n2odat)
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
qqnorm(n2odat$value)
qqline(n2odat$value) # not the best
hist(n2odat$value-mean(n2odat$value))
# looks a bit right skewed
# use boxcox function to estimate transformation parameter using MLE.
boxcox(lm(n2odat$value~1))
# resulting lambda is about -0.25. If -0.5 do 1/sqrt(x), if 0, do log(x).
# let's try 1/sqrt(x) first.
# but negative values cannot do sqrt
# 
n2odat$value.tr <- sign(n2odat$value) * (abs(n2odat$value))^(1/3)
hist(n2odat$value.tr) #  better
qqnorm(n2odat$value.tr)
qqline(n2odat$value.tr) # much better
hist(n2odat$value.tr-mean(n2odat$value.tr))

# Equality of variances - OK
ggplot(data=n2odat, aes(x=nfert, y=value.tr)) + 
  geom_boxplot(outlier.size=0.5, outlier.alpha=0.5) +
  facet_grid(rows=vars(till), cols=vars(cc))
# looks ok, some possible outliers in all NT except nt-nc-fn
# maybe one (but close to IQR) in rt-cc-fn
# perhaps more variability in CT and less in NT

leveneTest(value.tr ~ cc*till*nfert, data=n2odat)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group  17  1.1495 0.3025
#       558  

# outliers
summary(n2odat$value.tr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.603   4.272   4.592   4.668   5.034   6.366 

# outliers via histograms
hist(n2odat$value.tr, breaks=sqrt(nrow(n2odat)))

ggplot(data=n2odat, aes(x=value.tr)) +
  geom_histogram() +
  facet_grid(rows=vars(nfert), cols=vars(till, cc))

#outliers via boxplots
b <- boxplot(value.tr~till+cc+nfert, data=n2odat) 
 ################# left off here trying to figure out what treatments have outliers.

# outliers via z-scores
n2odat$z_grainC <- scale(n2odat$value)
hist(n2odat$z_grainC)
summary(n2odat$z_grainC)
# Min.   :-2.31494  
# 1st Qu.:-0.80908  
# Median :-0.03325  
# Mean   : 0.00000  
# 3rd Qu.: 0.84105  
# Max.   : 2.40069  
# < -2 or >2 is considered rare
# < -3 or >3 is extremely rare
# < -3.29 or > -3.29 is used to detect outliers, where one out of 1000 observations will be outside
# this range if normal distribution.
# interquartile range is > -2 and <2. 
# min. and max do not reach 3 or 3.29.

## outlier summary: boxplots and z-score suggest no extreme outliers. No justification for removing.

# re-run ANOVA with the above knowledge (assumptions met)

n2odat$till <- factor(n2odat$till)
n2odat$cc <- factor(n2odat$cc)
n2odat$nfert <- factor(n2odat$nfert)

Neffect <- aov(value~till*cc*nfert, data=n2odat)
summary(Neffect)
# Df    Sum Sq   Mean Sq F value   Pr(>F)    
# till              2 1.637e+08 8.184e+07  61.284  < 2e-16 ***
#   cc                1 5.557e+07 5.557e+07  41.617 1.13e-10 ***
#   nfert             2 2.557e+09 1.279e+09 957.461  < 2e-16 ***
#   till:cc           2 1.742e+07 8.710e+06   6.522  0.00147 ** 
#   till:nfert        4 1.113e+08 2.781e+07  20.829  < 2e-16 ***
#   cc:nfert          2 6.444e+08 3.222e+08 241.278  < 2e-16 ***
#   till:cc:nfert     4 1.750e+07 4.375e+06   3.276  0.01079 *  
#   Residuals     34542 4.613e+10 1.335e+06                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukout <- TukeyHSD(Neffect)

# compact letter display
cld <- multcompLetters4(Neffect, Tukout)

# table with letters and 3rd quantile
cornsum <- group_by(n2odat, cc, till, nfert) %>%
  summarize(mean=mean(value), 
            se=se(value)) %>%
  arrange(desc(mean))

cld <- as.data.frame.list(cld$`till:cc:nfert`)
cornsum$cld <- cld$Letters

cornsum


















ggplot(data=sum_ghgdecl[sum_ghgdecl$variable %in% c("n2o_tot"),],
       aes(x=nfert, y=mean, fill=nfert)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288", show.legend=F) +
  geom_errorbar(width=0.3, aes(ymin= mean-se, ymax=mean+se),  
                position=position_dodge(0.9),
                color="#332288") +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT", "RT"))), 
             cols=vars(factor(cc, levels=c("CC", "NC"))),
             labeller = as_labeller(
               c(CC="Has Cover Crop", NC="No Cover Crop",
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  scale_x_discrete(breaks=c("n2o_20", "n2o_30", "n2o_40", "n2o_50", "n2o_60"),
                   labels=c("2020s", "2030s", "2040s", "2050s", "2060s")) +
  xlab("Decade") +
  ylab("Sum of N2O emissions (CO2e/ha) per decade") +
  scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99" ), name="N management") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))
