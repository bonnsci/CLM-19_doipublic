library(tidyverse) # has ggplot, dplyr, etc.
library(reshape2)

se <- function(x) sd(x) / sqrt(length(x))

# if you only need annual totals skip to ndatyr below

# # # if you need daily estimates use this:
# wdat <- read.csv("data/large_data/daily water, sediments/NY_forage_day_water.csv")
# wdatalf <- read.csv("data/large_data/daily water, sediments/NY_forage_day_water_alf20240304.csv")
# beepr::beep(sound=14)
# # UNITS: Transpiration, Evaporation, Water leaching, and runoff are mm/day.
# # Sediment yield is kg/ha.
# 
# # # sum data by year
# wdatyr <- wdat %>%
#   group_by(site_name, crop_system_name, management_name, climate_scenario, Year) %>%
#   summarize(trans.yr = sum(Transpiration.mm.),
#             evap.yr = sum(Evaporation.mm.),
#             leach.yr = sum(WaterLeaching),
#             run.yr = sum(Runoff),
#             sed.yr = sum(SedimentYield))
# # beepr::beep(sound=8)
# # #
# wdatyralf <- wdatalf %>%
#   group_by(site_name, crop_system_name, management_name, climate_scenario, Year) %>%
#   summarize(trans.yr = sum(Transpiration.mm.),
#             evap.yr = sum(Evaporation.mm.),
#             leach.yr = sum(WaterLeaching),
#             run.yr = sum(Runoff),
#             sed.yr = sum(SedimentYield))
# 
# beepr::beep(sound=8)
# 
# # new alfalfa rotation data to replace alfalfa rotation data in above
# 
# unique(wdatyr$crop_system_name)
# wdatyr <- filter(wdatyr, !grepl("alfalfa-", crop_system_name))
# unique(wdatyr$crop_system_name)
# nrow(wdatyr) # 46464
# wdatyr <- rbind(wdatyr, wdatyralf)
# nrow(wdatyr)  # 105024, which is > 81504, because we have 5 alfalfa rotations this time not just 3 so that
# # we have each crop growing each year
# 
# #
# # # # clean up
# rm(wdat, wdatalf, wdatyralf)
# 
# colnames(wdatyr)[c(3,5)] <- c("management", "year")
# 
# wdatyr <- wdatyr[wdatyr$year>2021 & wdatyr$year<2073 & wdatyr$climate_scenario=="rcp60",]
# 
# # make factor variables
# wdatyr$till <- ifelse(grepl("ct-", wdatyr$management), "CT",
#                       ifelse(grepl("rt-", wdatyr$management), "RT", "NT"))
# # # check
# # unique(wdatyr$till)
# 
# # factor  for CC or NC
# wdatyr$cc <- ifelse(grepl("-cc", wdatyr$management), "CC", "NC")
# # # check
# # unique(wdatyr$cc)
# 
# 
# # factor  for decade
# wdatyr$decade <- ifelse(wdatyr$year <2031, "2020s",
#                         ifelse(wdatyr$year>=2031 & wdatyr$year <2041, "2030s",
#                                ifelse(wdatyr$year>=2041 & wdatyr$year <2051, "2040s",
#                                       ifelse(wdatyr$year>=2051 & wdatyr$year <2061, "2050s",
#                                              ifelse(wdatyr$year>=2061 & wdatyr$year <2071, "2060s", "2070s")))))
# # unique(wdatyr$decade)
# 
# wdatyr$et.yr <- wdatyr$evap.yr + wdatyr$trans.yr
# 

# hist(wdatyr[grepl("alfalfa-", wdatyr$crop_system_name) & wdatyr$sed.yr>2000,"sed.yr"])
# wdatyr[grepl("alfalfa-", wdatyr$crop_system_name) & wdatyr$sed.yr>10000,"sed.yr"]
# hist(wdatyr$sed.yr)
# unique(wdatyr$crop_system_name)

# 
# # sequences of years for each crop (corn, alfalfa, triticale) in each of the 5 alfalfa rotations (act, cta, cta2, act2, act3)
# actcorn <- seq(2017,2072, 5)
# ctacorn <- seq(2013, 2072, 5)
# cta2corn <- seq(2014, 2072, 5)
# act2corn <- seq(2015, 2072, 5)
# act3corn <- seq(2016, 2072, 5)
# 
# actalf <- sort(c(seq(2014,2072, 5), seq(2015,2072,5), seq(2016,2072,5)))
# ctaalf <- sort(c(seq(2015,2072, 5), seq(2016,2072,5), seq(2017,2072,5)))
# cta2alf <- sort(c(seq(2016,2072, 5), seq(2017,2072,5), seq(2018,2072,5)))
# act2alf <- sort(c(seq(2017,2072, 5), seq(2018,2072,5), seq(2019,2072,5)))
# act3alf <- sort(c(seq(2018,2072, 5), seq(2019,2072,5), seq(2020,2072,5)))
# 
# acttri <- seq(2018,2072, 5)
# ctatri <- seq(2019, 2072, 5)
# cta2tri <- seq(2020, 2072, 5)
# act2tri <- seq(2021, 2072, 5)
# act3tri <- seq(2022, 2072, 5)
# 
# unique(wdatyr$crop_system_name)
# # label data for the crop they are (depends on the rotation and the year)
# # if we want to know NO3 loss from corn per year or soy per year, need to split up data by odd and even years.
# wdatyr$crop <- ifelse(wdatyr$crop_system_name=="corn-soy" & wdatyr$year%%2 ==0, "soy cs",   # %%2 returns the remainder when divided by 2. if no remainder, then its an even number.
#                       ifelse(wdatyr$crop_system_name=="corn-soy" & !wdatyr$year%%2 ==0, "corn grain cs",  # cs= to indicate corn-soy rotation
#                              ifelse(wdatyr$crop_system_name=="soy-corn" & wdatyr$year%%2 ==0, "corn grain cs",
#                                     ifelse(wdatyr$crop_system_name=="soy-corn" & !wdatyr$year%%2 ==0, "soy cs", 
#                                            ifelse(wdatyr$crop_system_name=="corn-grain", "corn grain mono",
#                                                   ifelse(wdatyr$crop_system_name=="corn-silage", "corn silage mono",
#                     # corn in alfalfa rotation
#                           ifelse(wdatyr$crop_system_name=="alfalfa-act" & wdatyr$year %in% actcorn, "corn alf",
#                           ifelse(wdatyr$crop_system_name=="alfalfa-cta" & wdatyr$year %in% ctacorn, "corn alf",
#                           ifelse(wdatyr$crop_system_name=="alfalfa-cta2" & wdatyr$year %in% cta2corn, "corn alf",
#                           ifelse(wdatyr$crop_system_name=="alfalfa-act2" & wdatyr$year %in% act2corn, "corn alf",
#                           ifelse(wdatyr$crop_system_name=="alfalfa-act3" & wdatyr$year %in% act3corn, "corn alf",
#                      # alf in alf rotation
#                           ifelse(wdatyr$crop_system_name=="alfalfa-act" & wdatyr$year %in% actalf, "alf",
#                           ifelse(wdatyr$crop_system_name=="alfalfa-cta" & wdatyr$year %in% ctaalf, "alf",
#                           ifelse(wdatyr$crop_system_name=="alfalfa-cta2" & wdatyr$year %in% cta2alf, "alf",
#                           ifelse(wdatyr$crop_system_name=="alfalfa-act2" & wdatyr$year %in% act2alf, "alf",
#                           ifelse(wdatyr$crop_system_name=="alfalfa-act3" & wdatyr$year %in% act3alf, "alf",
#                # triticale in alf rotation
#                           ifelse(wdatyr$crop_system_name=="alfalfa-act" & wdatyr$year %in% acttri, "tri alf",
#                           ifelse(wdatyr$crop_system_name=="alfalfa-cta" & wdatyr$year %in% ctatri, "tri alf",
#                           ifelse(wdatyr$crop_system_name=="alfalfa-cta2" & wdatyr$year %in% cta2tri, "tri alf",
#                           ifelse(wdatyr$crop_system_name=="alfalfa-act2" & wdatyr$year %in% act2tri, "tri alf",
#                           ifelse(wdatyr$crop_system_name=="alfalfa-act3" & wdatyr$year %in% act3tri, "tri alf",  "X")))))))))))))))))))))
# 
# 
# 
# # check no Xs
# # unique(wdatyr$crop)
# rm(actalf, actcorn,acttri,
#    ctaalf, ctacorn, ctatri, 
#    cta2alf, cta2corn, cta2tri,
#    act2alf, act2corn, act2tri,
#    act3alf, act3corn, act3tri)
# 
# 
# # add simpler rotation factor to lump corn-soy and soy-corn together, and all the alfalfas (4 total rotations)
wdatyr$rot <- ifelse(wdatyr$crop_system_name %in% c("corn-soy", "soy-corn"), "corn soy",
                     ifelse(grepl("alfalfa-", wdatyr$crop_system_name), "alf",
                            ifelse(wdatyr$crop_system_name == "corn-silage", "corn silage",
                                   ifelse(wdatyr$crop_system_name == "corn-grain", "corn grain", "X"))))
# 
# # check no Xs
# unique(wdatyr$rot)
# 
# write.csv(wdatyr, "data/water, nitrate, sediments/NY_wdatyr.csv", row.names=F)

wdatyr <- read.csv("data/water, nitrate, sediments/NY_wdatyr.csv")


# first sum by site and crop name across all years, then calculate mean across crop types per site, 
# Then mean and se across sites by treatments/management combinations
# 
# wdat_tmttot <- wdatyr %>%
#   group_by(site_name, crop_system_name, till, cc, nfert) %>%
#   summarize(et.tot = sum(et.yr), 
#             evap.tot = sum(evap.yr),
#             trans.tot = sum(trans.yr),
#             leach.tot = sum(leach.yr),
#             run.tot = sum(run.yr),
#             sed.tot = sum(sed.yr)) %>%
#   group_by(site_name, till, cc, nfert) %>%
#   summarize(et.sitemean = mean(et.tot),  # mean of corn-soy and soy-corn per site
#             evap.sitemean = mean(evap.tot),
#             trans.sitemean = mean(trans.tot),
#             leach.sitemean = mean(leach.tot),
#             run.sitemean = mean(run.tot),
#             sed.sitemean = mean(sed.tot)) %>%
#   group_by(till, cc) %>%
#   summarize(et.mean = mean(et.sitemean), # mean across sites and N treatments in each treatment combo
#             et.se = se(et.sitemean), # variability across sites and N treatments in each treatment combo
#             evap.mean = mean(evap.sitemean), # (N treatments don't really affect water and sediment losses, I checked)
#             evap.se = se(evap.sitemean),
#             trans.mean = mean(trans.sitemean),
#             trans.se = se(trans.sitemean),
#             leach.mean = mean(leach.sitemean),
#             leach.se = se(leach.sitemean),
#             run.mean = mean(run.sitemean),
#             run.se = se(run.sitemean),
#             sed.mean = mean(sed.sitemean),
#             sed.se = se(sed.sitemean))


# wdat_tmtperyr <- wdatyr %>%
#   group_by(site_name, till, cc) %>%
#   summarize( #et.sitemean = mean(et.yr),  # mean of corn-soy and soy-corn per site and n treatment
#             # evap.sitemean = mean(evap.yr),
#             # trans.sitemean = mean(trans.yr),
#             # leach.sitemean = mean(leach.yr),
#             # run.sitemean = mean(run.yr),
#             sed.sitemean = mean(sed.yr)) %>%
#   group_by(till, cc) %>%
#   summarize( # et.mean = mean(et.sitemean), # mean across sites and N treatments in each treatment combo
#             # et.se = se(et.sitemean), # variability across sites and N treatments in each treatment combo
#             # evap.mean = mean(evap.sitemean),
#             # evap.se = se(evap.sitemean),
#             # trans.mean = mean(trans.sitemean),
#             # trans.se = se(trans.sitemean),
#             # leach.mean = mean(leach.sitemean),
#             # leach.se = se(leach.sitemean),
#             # run.mean = mean(run.sitemean),
#             # run.se = se(run.sitemean),
#             sed.mean = mean(sed.sitemean),
#             sed.se = se(sed.sitemean))

# ############################### 50 YEAR TOTAL WATER AND SED LOSSES PLOT
# # prep data for plotting
# # put data in long form for plotting, 1 column evap, trans, et, leach, run, sed
# wdat_tmttotlong <- melt(wdat_tmttot, id=c("till", "cc"))
# # separate means from se's
# wdat_tmttotlong$mean.se <- ifelse(grepl("mean", wdat_tmttotlong$variable), "mean", "se")
# 
# # make new column for se values
# dat.se <- wdat_tmttotlong[wdat_tmttotlong$mean.se=="se",1:4]
# colnames(dat.se)[4] <- "se"
# dat.se$variable <- gsub(".se", "", dat.se$variable)
# dat.mean <- wdat_tmttotlong[wdat_tmttotlong$mean.se=="mean",1:4]
# colnames(dat.mean)[4] <- "mean"
# dat.mean$variable <- gsub(".mean", "", dat.mean$variable)
# wtotlong <- left_join(dat.mean, dat.se) # , relationship="one-to-one")
# rm(dat.se, dat.mean)
# 
# windows(xpinch=200, ypinch=200, width=5, height=5)
# 
# 
# wtotlong$variable <- factor(wtotlong$variable, levels=c("evap", "trans", "leach", "run", "sed", "et"))
# 
# # 50 YR TOTAL WATER LOSSES
# ggplot(data=wtotlong[!wtotlong$variable %in% c("sed", "et"),], aes(x=till, y=mean, fill=variable)) +
#   geom_bar(stat="identity", position=position_dodge(), color="#332288", width=0.8) +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, position=position_dodge(0.8), color="#332288") +
#   facet_grid(cols=vars(factor(cc, levels=c("CC", "NC"))), 
#              #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
#              labeller = as_labeller(
#                c(CC="Has Cover Crop", NC="No Cover Crop"))) +
#                 # "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
#   #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
#   scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99", "#AA4499")) +  # "#999933"
#   xlab("tillage") +
#   ylab("total mm 2022 to 2072") +
#   theme(
#     panel.grid.minor=element_blank(), 
#     panel.grid.major=element_blank(),
#     panel.background = element_rect(fill = 'gray95'))
# 
# ggsave("plots/water, nitrate, sediments/IL_water losses 50 yr total bars.png", width=6, height=2.5, dpi=300)
# 
# 
# # 50 YR TOTAL SEDIMENT LOSSES
# ggplot(data=wtotlong[wtotlong$variable %in% c("sed"),], aes(x=till, y=mean)) +
#   geom_bar(stat="identity", position=position_dodge(), color="#332288", width=0.8, fill="gray70") +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, position=position_dodge(0.8),color="#332288") +
#   facet_grid( #rows=vars(factor(till, levels=c("CT", "NT", "RT"))), 
#              cols=vars(factor(cc, levels=c("CC", "NC"))), 
#              #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
#              labeller = as_labeller(
#                c(CC="Has Cover Crop", NC="No Cover Crop"))) +
#                 # "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
#   #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
#   xlab("Tillage") +
#   ylab("total kg/ha 2022 to 2072") +
#   theme(
#     panel.grid.minor=element_blank(), 
#     panel.grid.major=element_blank(),
#     panel.background = element_rect(fill = 'gray95'))
# 
# ggsave("plots/water, nitrate, sediments/IL_sediments 50 yr total bars.png", width=6, height=2.5, dpi=300)
# 



############################### WATER, Sed LOSSES PER YEAR PLOT
# prep data for plotting
# put data in long form for plotting, 1 column for evap, trans, et, runoff, leach, sed
# wdat_peryrlong <- melt(wdat_tmtperyr, id=c("till", "cc"))
# # separate means from se's
# wdat_peryrlong$mean.se <- ifelse(grepl("mean", wdat_peryrlong$variable), "mean", "se")

# # make new column for se values
# dat.se <- wdat_peryrlong[wdat_peryrlong$mean.se=="se",1:4]
# colnames(dat.se)[4] <- "se"
# dat.se$variable <- gsub(".se", "", dat.se$variable)
# dat.mean <- wdat_peryrlong[wdat_peryrlong$mean.se=="mean",1:4]
# colnames(dat.mean)[4] <- "mean"
# dat.mean$variable <- gsub(".mean", "", dat.mean$variable)
# wpyrlong <- left_join(dat.mean, dat.se) # , relationship="one-to-one")
# rm(dat.se, dat.mean)

windows(xpinch=200, ypinch=200, width=5, height=5)


############# change to fill by tillage, facet by variable
unique(wdatyr$rot)
# # are the bars in the below plot significantly different from each other?
wdat_persite <- wdatyr %>%
  filter(!till=="RT", !rot=="alf") %>%   # 
  group_by(site_name, till, cc, rot, crop) %>%
  summarize( # et = mean(et.yr), # mean across all years and nferts and two cropping systems
            # evap = mean(evap.yr),
            # trans = mean(trans.yr),
            # leach = mean(leach.yr),
            # run = mean(run.yr),
            sed = mean(sed.yr)) # %>%
  # melt(., ids=c("site_name", "till", "cc"))


wdatyr2 <- wdatyr[!wdatyr$till == "RT" & !wdatyr$rot == "alf",]
lmsed_rot <- lm(sed.yr~ till + cc + rot + year, data=wdatyr2)
summary(lmsed_rot)
# Call:
#   lm(formula = sed.yr ~ till + cc + rot + year, data = wdatyr2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1255.8  -617.6  -395.5   270.5 13623.9 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -1.362e+04  1.294e+03 -10.521  < 2e-16 ***
#   tillNT         -1.910e+02  1.861e+01 -10.265  < 2e-16 ***
#   ccNC            1.573e+02  1.861e+01   8.454  < 2e-16 ***
#   rotcorn-silage  2.845e+02  2.632e+01  10.810  < 2e-16 ***
#   rotcorn-soy     7.058e+01  2.279e+01   3.097  0.00196 ** 
#   year            6.966e+00  6.321e-01  11.020  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1063 on 13050 degrees of freedom
# Multiple R-squared:  0.0319,	Adjusted R-squared:  0.03153 
# F-statistic: 85.99 on 5 and 13050 DF,  p-value: < 2.2e-16
######### it's really YEAR that is significant here, if you take year out
######### take out year and the other 3 factors are not significant

effectsed <- aov(sed.yr~ till*cc*rot, data=wdatyr2)
effectsed <- aov(sed.yr~ till + cc + rot + year, data=wdatyr2)
summary(effectsed)
tukoutsed <- TukeyHSD(effectsed)
cldsed <- multcompView::multcompLetters4(effectsed, tukoutsed)


lmsed_rot <- lm(sed~cc +till + rot, data=wdat_persite)
summary(lmsed_rot)
# Call:
#   lm(formula = sed ~ cc + till + rot, data = wdat_persite)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1052.4  -587.0  -394.2   500.7  4340.1 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      643.57     140.13   4.593 6.93e-06 ***
#   ccNC             157.33     114.42   1.375   0.1703    
# tillNT          -191.03     114.42  -1.670   0.0962 .  
# rotcorn-silage   284.48     161.81   1.758   0.0799 .  
# rotcorn-soy       70.58     140.13   0.504   0.6150    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 915.3 on 251 degrees of freedom
# Multiple R-squared:  0.03149,	Adjusted R-squared:  0.01606 
# F-statistic:  2.04 on 4 and 251 DF,  p-value: 0.08928


# Call:
#   lm(formula = sed ~ till * cc * rot, data = wdat_persite)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1137.1  -591.5  -395.8   525.3  4056.3 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                 753.405    230.498   3.269  0.00124 **
#   tillNT                     -327.663    325.973  -1.005  0.31580   
# ccNC                         80.422    325.973   0.247  0.80534   
# rotcorn-silage              -38.039    325.973  -0.117  0.90720   
# rotcorn-soy                  34.132    282.301   0.121  0.90386   
# tillNT:ccNC                 -12.281    460.995  -0.027  0.97877   
# tillNT:rotcorn-silage       366.109    460.995   0.794  0.42787   
# tillNT:rotcorn-soy           46.264    399.233   0.116  0.90784   
# ccNC:rotcorn-silage         201.652    460.995   0.437  0.66219   
# ccNC:rotcorn-soy              9.038    399.233   0.023  0.98196   
# tillNT:ccNC:rotcorn-silage  154.565    651.946   0.237  0.81279   
# tillNT:ccNC:rotcorn-soy      35.171    564.601   0.062  0.95038   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 922 on 244 degrees of freedom
# Multiple R-squared:  0.04477,	Adjusted R-squared:  0.001708 
# F-statistic:  1.04 on 11 and 244 DF,  p-value: 0.4119

lmsed_crop <- lm(sed~ till +cc +crop, data=wdat_persite)
summary(lmsed_crop)
# Call:
#   lm(formula = sed ~ till * cc * crop, data = wdat_persite)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1137.1  -595.7  -392.0   532.3  4056.3 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                       761.755    232.373   3.278   0.0012 **
#   tillNT                           -268.538    328.624  -0.817   0.4146   
# ccNC                               83.876    328.624   0.255   0.7988   
# cropcorn grain mono                -8.350    328.624  -0.025   0.9798   
# cropcorn silage mono              -46.389    328.624  -0.141   0.8879   
# cropsoy cs                         51.564    328.624   0.157   0.8754   
# tillNT:ccNC                        24.181    464.745   0.052   0.9585   
# tillNT:cropcorn grain mono        -59.125    464.745  -0.127   0.8989   
# tillNT:cropcorn silage mono       306.984    464.745   0.661   0.5095   
# tillNT:cropsoy cs                 -25.722    464.745  -0.055   0.9559   
# ccNC:cropcorn grain mono           -3.454    464.745  -0.007   0.9941   
# ccNC:cropcorn silage mono         198.198    464.745   0.426   0.6702   
# ccNC:cropsoy cs                    11.168    464.745   0.024   0.9808   
# tillNT:ccNC:cropcorn grain mono   -36.462    657.249  -0.055   0.9558   
# tillNT:ccNC:cropcorn silage mono  118.102    657.249   0.180   0.8575   
# tillNT:ccNC:cropsoy cs             -2.582    657.249  -0.004   0.9969   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 929.5 on 240 degrees of freedom
# Multiple R-squared:  0.04508,	Adjusted R-squared:  -0.0146 
# F-statistic: 0.7554 on 15 and 240 DF,  p-value: 0.7261


effectsed <- aov(sed~ till*cc*rot, data=wdat_persite)
summary(effectsed)
# Df    Sum Sq   Mean Sq F value Pr(>F)    
# till          1 2.198e+07  21977445   3.466 0.0632 .  
# cc            1 2.051e+07  20508922   3.234 0.0727 .  
# rot           3 7.421e+08 247380890  39.010 <2e-16 ***
#   till:cc       1 1.640e+04     16403   0.003 0.9595    
# till:rot      3 6.563e+06   2187559   0.345 0.7928    
# cc:rot        3 6.405e+06   2135016   0.337 0.7988    
# till:cc:rot   3 6.958e+04     23194   0.004 0.9997    
# Residuals   560 3.551e+09   6341423                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tukoutsed <- TukeyHSD(effectsed)
cldsed <- multcompView::multcompLetters4(effectsed, tukoutsed)



wdat_nosite <- group_by(wdatyr, till, cc, rot) %>%
  filter(till %in% c("CT", "NT"), !rot=="alf") %>%
  summarize(mean=mean(sed.yr),
            se=se(sed.yr)) %>%
  arrange(desc(mean))

cldsed <- as.data.frame.list(cldsed$`till:cc:rot`)
wdat_nosite$cld <- cldsed$Letters

# sediment yield is in kg/ha, convert to lb/ac
wdat_nosite$mean.lbac <- wdat_nosite$mean/(0.4536*2.471)
wdat_nosite$se.lbac <- wdat_nosite$se/(0.4536*2.471)

print(wdat_nosite, n=nrow(wdat_nosite))

# # A tibble: 12 × 8
# # Groups:   till, cc [4]
# till  cc    rot          mean    se cld   mean.lbac se.lbac
# <chr> <chr> <chr>       <dbl> <dbl> <chr>     <dbl>   <dbl>
#   1 NT    NC    corn silage 1178.  58.5 a         1051.    52.2
# 2 CT    NC    corn silage  997.  46.5 a          890.    41.5
# 3 CT    NC    corn soy     877.  29.2 a          782.    26.1
# 4 CT    NC    corn grain   834.  39.8 a          744.    35.5
# 5 CT    CC    corn soy     788.  26.5 a          703.    23.6
# 6 NT    CC    corn silage  754.  37.9 a          673.    33.8
# 7 CT    CC    corn grain   753.  36.2 a          672.    32.3
# 8 CT    CC    corn silage  715.  34.4 a          638.    30.7
# 9 NT    NC    corn soy     618.  23.1 a          552.    20.6
# 10 NT    CC    corn soy     506.  19.1 a          452.    17.0
# 11 NT    NC    corn grain   494.  26.5 a          441.    23.6
# 12 NT    CC    corn grain   426.  22.9 a          380.    20.4

# use these letters in the plot:

wdat_nosite$cctill <- paste0(wdat_nosite$till, "-", wdat_nosite$cc)

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=wdat_nosite, aes(x=cctill, y=mean.lbac, fill=cctill)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.6) + #  , fill="burlywood3") +
  geom_errorbar(aes(ymin=mean.lbac-se.lbac, ymax=mean.lbac+se.lbac), 
                width=0.3, position=position_dodge(0.8),color="gray60") +
  facet_grid(cols=vars(factor(rot, levels=c("corn grain", "corn soy", "corn silage")))) + 
               #vars(factor(cc, levels=c("NC", "CC")),factor(till, levels=c("CT", "NT"))), 
             # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             # labeller = as_labeller(
             #   c("NC"="No Cover Crop", "CC"="Has Cover Crop",
             #     "CT" = "Conventional Till", "NT" = "No Till"))) +
  scale_fill_manual(values=c("#c44f2d","#20243d", "#C2e4ef", "#669947")) +
  
  # scale_y_continuous(breaks=seq(0,300, 50), limits=c(0,300)) +
  scale_x_discrete(limits = c("CT-NC", "CT-CC", "NT-NC", "NT-CC")) +
  geom_text(aes(x=cctill, label=cld, y=mean.lbac), vjust=-2,
            position=position_dodge(0.9), color="gray20", size=4, fontface="bold") +
  xlab("Tillage") +
  ylab('2022-72 mean annual sediment loss (lb per ac)') +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(size=11, angle=-30, hjust=0), 
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(1,0.5,1,0.5), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))


ggsave("plots/water, nitrate, sediments/NY_sediments mean annual bars_rotations_no alf_RCP60_with letters.png", width=7, height=3.5, dpi=300)





# Same chart by crop rather than rotation
wdat_nosite_crop <- group_by(wdatyr, till, cc, crop) %>%
  summarize(mean=mean(sed.yr),
            se=se(sed.yr)) %>%
  mutate(mean.lbac = mean/(0.4536*2.471),
         se.lbac = se/(0.4536*2.471),
         cctill = paste0(till, "-", cc)) %>%
  arrange(desc(mean))

print(wdat_nosite_crop, n=nrow(wdat_nosite_crop))

# # A tibble: 36 × 7
# # Groups:   till, cc [4]
# till  cc    crop_system_name  mean    se mean.lbac se.lbac
# <chr> <chr> <chr>            <dbl> <dbl>     <dbl>   <dbl>
#   1 CT    NC    alfalfa-act      3705. 1012.     3305.    903.
# 2 CT    NC    alfalfa-act3     3682.  993.     3285.    886.
# 3 CT    NC    alfalfa-cta2     3494.  917.     3117.    818.
# 4 CT    NC    alfalfa-cta      3478.  878.     3103.    784.
# 5 CT    NC    alfalfa-act2     3462.  928.     3089.    828.
# 6 CT    CC    alfalfa-act      3210.  856.     2864.    764.
# 7 NT    NC    alfalfa-cta2     3154.  925.     2814.    826.
# 8 NT    NC    alfalfa-cta      3080.  866.     2748.    773.
# 9 CT    CC    alfalfa-act3     3057.  811.     2728.    723.
# 10 NT    NC    alfalfa-act      3044.  891.     2716.    795.
# 11 CT    CC    alfalfa-cta2     2969.  754.     2649.    673.
# 12 NT    NC    alfalfa-act3     2932.  845.     2616.    754.
# 13 CT    CC    alfalfa-cta      2930.  726.     2615.    648.
# 14 CT    CC    alfalfa-act2     2895.  767.     2583.    684.
# 15 NT    NC    alfalfa-act2     2867.  862.     2558.    769.
# 16 NT    CC    alfalfa-cta2     2556.  742.     2280.    662.
# 17 NT    CC    alfalfa-cta      2548.  710.     2274.    634.
# 18 NT    CC    alfalfa-act      2529.  719.     2256.    642.
# 19 NT    CC    alfalfa-act3     2351.  682.     2098.    608.
# 20 NT    CC    alfalfa-act2     2317.  682.     2067.    609.
# 21 NT    NC    corn-silage      1178.  370.     1051.    330.
# 22 CT    NC    corn-silage       997.  294.      890.    262.
# 23 CT    NC    corn-soy          877.  257.      782.    229.
# 24 CT    NC    soy-corn          877.  258.      782.    230.
# 25 CT    NC    corn-grain        834.  246.      744.    219.
# 26 CT    CC    soy-corn          789.  236.      704.    210.
# 27 CT    CC    corn-soy          786.  234.      701.    209.
# 28 NT    CC    corn-silage       754.  243.      673.    217.
# 29 CT    CC    corn-grain        753.  226.      672.    201.
# 30 CT    CC    corn-silage       715.  219.      638.    195.
# 31 NT    NC    corn-soy          621.  202.      554.    180.
# 32 NT    NC    soy-corn          616.  201.      550.    180.
# 33 NT    CC    soy-corn          508.  166.      453.    148.
# 34 NT    CC    corn-soy          505.  165.      450.    147.
# 35 NT    NC    corn-grain        494.  159.      441.    142.
# 36 NT    CC    corn-grain        426.  133.      380.    119.

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=wdat_nosite_crop, aes(x=cctill, y=mean.lbac, fill=cctill)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.6) + #  , fill="burlywood3") +
  geom_errorbar(aes(ymin=mean.lbac-se.lbac, ymax=mean.lbac+se.lbac), 
                width=0.3, position=position_dodge(0.8),color="gray60") +
  facet_grid(cols=vars(factor(crop_system_name))) + #, levels=c("corn_grain", "corn_soy", "corn_silage", "alf")))) + 
  #vars(factor(cc, levels=c("NC", "CC")),factor(till, levels=c("CT", "NT"))), 
  # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
  # labeller = as_labeller(
  #   c("NC"="No Cover Crop", "CC"="Has Cover Crop",
  #     "CT" = "Conventional Till", "NT" = "No Till"))) +
  scale_fill_manual(values=c("#c44f2d","#20243d", "#C2e4ef", "#669947")) +
  
  # scale_y_continuous(breaks=seq(0,300, 50), limits=c(0,300)) +
  scale_x_discrete(limits = c("CT-NC", "CT-CC", "NT-NC", "NT-CC")) +
  # labels = c("Conv.", "Reduced", "No")) +
  # geom_text(aes(x=cctill, label=cld, y=mean.lbac), vjust=-2,
            # position=position_dodge(0.9), color="gray20", size=4, fontface="bold") +
  xlab("Tillage") +
  ylab('2022-72 mean annual sediment loss (lb per ac)') +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(size=11, angle=-30, hjust=0), 
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(1,0.5,1,0.5), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))

ggsave("plots/water, nitrate, sediments/NY_sediments mean annual bars_crops_RCP60_no letters.png", width=10, height=3.5, dpi=300)






# precipitation and time and sediment yield

# load data
dat <- read.csv("data/large_data/clm_ny.csv")
# can get rid of row numbers, model, #### double check the column numbers below
# model is "IPSL-CM5A-LR"

dat <- dat[,c(3,5:11)]


# convert doy to dates so we can get monthly  means/totals # Takes a sec to run
dat <- dat %>%
  mutate(date_ = as.Date(doy-1, origin=paste0(year, "-01-01")),  # subtract 1 b/c R uses a 0 base index
         month = strftime(date_, "%m"),
         day = strftime(date_, "%d")) 
# beepr::beep(sound=8)

# SPEI works on monthly data

datmo <- dat %>%
  filter(year >2021, year <2073) %>%
  group_by(scenario, name, year, month) %>%
  summarize(tmed = mean(avg_temp),
            tmax = max(max_temp),
            tmin = min(min_temp), # mean monthly temp in C
            prcp = (sum(precip))) # total precip in mm

rm(dat)

datmo1 <- filter(datmo, name=="f_1", scenario=="rcp60")
datmo1$mmyyyy <- paste0(datmo1$month, "-01-", datmo1$year)
datmo1$mmyyyy <- as.POSIXct(datmo1$mmyyyy, format="%m-%d-%Y")

datmo1$season <- ifelse(datmo1$month %in% c("12", "01", "02"), "Winter",
                      ifelse(datmo1$month %in% c("03", "04", "05"), "Spring",
                             ifelse(datmo1$month %in% c("06", "07", "08"), "Summer", "Fall")))

ggplot(dat=datmo1, aes(x=mmyyyy, y=prcp)) +
  geom_point() +
  facet_grid(cols=vars(season))

# no clear answer here





# noticing that the different treatments seem to have different water balances, but getting same precip input.
# What do these results look like within one year (rather than average or total over many years)

# wdatyr.wbal <- wdatyr %>%
#   group_by(site_name, year, till, cc, nfert, crop_system_name) %>%
#   summarize(sum.etlr = trans.yr + evap.yr + leach.yr + run.yr)
# 
# # average across sites, N treatments
# wdatyr.mean <- wdatyr %>%
#   group_by(year, till, cc) %>%
#   summarize(trans.yrm = mean(trans.yr),
#             evap.yrm = mean(evap.yr),
#             leach.yrm = mean(leach.yr),
#             run.yrm = mean(run.yr),
#             sed.yrm = mean(sed.yr),
#             et.yrm = mean(et.yr))

# wdatyrlong <- melt(wdatyr.mean, id=c("year", "till", "cc"))
# 
# ggplot(data=wdatyrlong[wdatyrlong$year==2025 & !(wdatyrlong$variable %in% c("sed.yrm", "et.yrm")),], 
#        aes(x=till, y=value, fill=variable)) +
#   geom_bar(stat="identity", position=position_dodge(), color="#332288", width=0.8) +
#   #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, position=position_dodge(0.8), color="#332288") +
#   facet_grid(cols=vars(factor(cc, levels=c("CC", "NC"))), 
#              #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
#              labeller = as_labeller(
#                c(CC="Has Cover Crop", NC="No Cover Crop"))) +
#   # "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
#   #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
#   scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99", "#AA4499")) +  # "#999933"
#   xlab("tillage") +
#   ylab("total mm 2025") +
#   theme(
#     panel.grid.minor=element_blank(), 
#     panel.grid.major=element_blank(),
#     panel.background = element_rect(fill = 'gray95'))
