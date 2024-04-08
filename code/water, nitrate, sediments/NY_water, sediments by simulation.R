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
# write.csv(wdatyr, "data/water, nitrate, sediments/NY_wdatyr.csv", row.names=F)

wdatyr <- read.csv("data/water, nitrate, sediments/NY_wdatyr.csv")
hist(wdatyr[grepl("alfalfa-", wdatyr$crop_system_name) & wdatyr$sed.yr>2000,"sed.yr"])
wdatyr[grepl("alfalfa-", wdatyr$crop_system_name) & wdatyr$sed.yr>10000,"sed.yr"]
hist(wdatyr$sed.yr)
unique(wdatyr$crop_system_name)
# add simpler rotation factor to lump corn-soy and soy-corn together, and all the alfalfas (4 total rotations)
wdatyr$rot <- ifelse(wdatyr$crop_system_name %in% c("corn-soy", "soy-corn"), "corn_soy",
                     ifelse(grepl("alfalfa-", wdatyr$crop_system_name), "alf", 
                            ifelse(wdatyr$crop_system_name == "corn-silage", "corn_silage", 
                                   ifelse(wdatyr$crop_system_name == "corn-grain", "corn_grain", "X"))))


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


wdat_tmtperyr <- wdatyr %>%
  group_by(site_name, till, cc) %>%
  summarize( #et.sitemean = mean(et.yr),  # mean of corn-soy and soy-corn per site and n treatment
            # evap.sitemean = mean(evap.yr),
            # trans.sitemean = mean(trans.yr),
            # leach.sitemean = mean(leach.yr),
            # run.sitemean = mean(run.yr),
            sed.sitemean = mean(sed.yr)) %>%
  group_by(till, cc) %>%
  summarize( # et.mean = mean(et.sitemean), # mean across sites and N treatments in each treatment combo
            # et.se = se(et.sitemean), # variability across sites and N treatments in each treatment combo
            # evap.mean = mean(evap.sitemean),
            # evap.se = se(evap.sitemean),
            # trans.mean = mean(trans.sitemean),
            # trans.se = se(trans.sitemean),
            # leach.mean = mean(leach.sitemean),
            # leach.se = se(leach.sitemean),
            # run.mean = mean(run.sitemean),
            # run.se = se(run.sitemean),
            sed.mean = mean(sed.sitemean),
            sed.se = se(sed.sitemean))

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

# # are the bars in the below plot significantly different from each other?
wdat_persite <- wdatyr %>%
  filter(!till=="RT", !rot=="alf") %>%
  group_by(site_name, till, cc, rot, crop_system_name) %>%
  summarize( # et = mean(et.yr), # mean across all years and nferts and two cropping systems
            # evap = mean(evap.yr),
            # trans = mean(trans.yr),
            # leach = mean(leach.yr),
            # run = mean(run.yr),
            sed = mean(sed.yr)) # %>%
  # melt(., ids=c("site_name", "till", "cc"))
unique(wdatyr$rot)

wdatyr2 <- wdatyr[!wdatyr$till == "RT" & !wdatyr$rot == "alf",]
lmsed_rot <- lm(sed.yr~ till*cc*rot, data=wdatyr2)
summary(lmsed_rot)
effectsed <- aov(sed.yr~ till*cc*rot, data=wdatyr2)
summary(effectsed)
tukoutsed <- TukeyHSD(effectsed)
cldsed <- multcompView::multcompLetters4(effectsed, tukoutsed)


lmsed_rot <- lm(sed~ till*cc*rot, data=wdat_persite)
summary(lmsed_rot)
# Call:
#   lm(formula = sed ~ till * cc * rot, data = wdat_persite)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3465.0 -1455.2  -443.0   713.4 13034.7 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                 3012.247    281.545  10.699  < 2e-16 ***
#   tillNT                      -552.009    398.165  -1.386 0.166182    
# ccNC                         551.807    398.165   1.386 0.166337    
# rotcorn_grain              -2258.842    689.642  -3.275 0.001120 ** 
#   rotcorn_silage             -2296.881    689.642  -3.331 0.000924 ***
#   rotcorn_soy                -2224.710    526.723  -4.224 2.81e-05 ***
#   tillNT:ccNC                    3.266    563.091   0.006 0.995375    
# tillNT:rotcorn_grain         224.346    975.302   0.230 0.818154    
# tillNT:rotcorn_silage        590.456    975.302   0.605 0.545153    
# tillNT:rotcorn_soy           270.610    744.899   0.363 0.716529    
# ccNC:rotcorn_grain          -471.385    975.302  -0.483 0.629056    
# ccNC:rotcorn_silage         -269.733    975.302  -0.277 0.782217    
# ccNC:rotcorn_soy            -462.347    744.899  -0.621 0.535060    
# tillNT:ccNC:rotcorn_grain    -15.547   1379.285  -0.011 0.991011    
# tillNT:ccNC:rotcorn_silage   139.018   1379.285   0.101 0.919754    
# tillNT:ccNC:rotcorn_soy       19.624   1053.446   0.019 0.985144    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2518 on 560 degrees of freedom
# Multiple R-squared:  0.1834,	Adjusted R-squared:  0.1615 
# F-statistic: 8.386 on 15 and 560 DF,  p-value: < 2.2e-16

lmsed_crop <- lm(sed~ till*cc*crop_system_name, data=wdat_persite)
summary(lmsed_crop)
# Call:
#   lm(formula = sed ~ till * cc * crop_system_name, data = wdat_persite)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3495.0 -1452.9  -430.7   707.6 12893.9 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               3209.6168   640.7794   5.009 7.43e-07 ***
#   tillNT                                    -680.7179   906.1989  -0.751  0.45287    
# ccNC                                       495.1907   906.1989   0.546  0.58498    
# crop_system_namealfalfa-act2              -314.3471   906.1989  -0.347  0.72881    
# crop_system_namealfalfa-act3              -152.4769   906.1989  -0.168  0.86644    
# crop_system_namealfalfa-cta               -279.1190   906.1989  -0.308  0.75819    
# crop_system_namealfalfa-cta2              -240.9038   906.1989  -0.266  0.79046    
# crop_system_namecorn-grain               -2456.2117   906.1989  -2.710  0.00693 ** 
#   crop_system_namecorn-silage              -2494.2507   906.1989  -2.752  0.00611 ** 
#   crop_system_namecorn-soy                 -2423.3469   906.1989  -2.674  0.00772 ** 
#   crop_system_namesoy-corn                 -2420.8124   906.1989  -2.671  0.00778 ** 
#   tillNT:ccNC                                 19.6121  1281.5587   0.015  0.98780    
# tillNT:crop_system_namealfalfa-act2        102.3237  1281.5587   0.080  0.93639    
# tillNT:crop_system_namealfalfa-act3        -25.0211  1281.5587  -0.020  0.98443    
# tillNT:crop_system_namealfalfa-cta         298.6538  1281.5587   0.233  0.81582    
# tillNT:crop_system_namealfalfa-cta2        267.5855  1281.5587   0.209  0.83469    
# tillNT:crop_system_namecorn-grain          353.0547  1281.5587   0.275  0.78305    
# tillNT:crop_system_namecorn-silage         719.1642  1281.5587   0.561  0.57492    
# tillNT:crop_system_namecorn-soy            399.0345  1281.5587   0.311  0.75564    
# tillNT:crop_system_namesoy-corn            399.6029  1281.5587   0.312  0.75531    
# ccNC:crop_system_namealfalfa-act2           71.9740  1281.5587   0.056  0.95523    
# ccNC:crop_system_namealfalfa-act3          129.4081  1281.5587   0.101  0.91961    
# ccNC:crop_system_namealfalfa-cta            51.9204  1281.5587   0.041  0.96770    
# ccNC:crop_system_namealfalfa-cta2           29.7813  1281.5587   0.023  0.98147    
# ccNC:crop_system_namecorn-grain           -414.7687  1281.5587  -0.324  0.74633    
# ccNC:crop_system_namecorn-silage          -213.1165  1281.5587  -0.166  0.86799    
# ccNC:crop_system_namecorn-soy             -404.4250  1281.5587  -0.316  0.75245    
# ccNC:crop_system_namesoy-corn             -407.0358  1281.5587  -0.318  0.75090    
# tillNT:ccNC:crop_system_namealfalfa-act2   -36.4496  1812.3977  -0.020  0.98396    
# tillNT:ccNC:crop_system_namealfalfa-act3   -63.9388  1812.3977  -0.035  0.97187    
# tillNT:ccNC:crop_system_namealfalfa-cta    -34.9502  1812.3977  -0.019  0.98462    
# tillNT:ccNC:crop_system_namealfalfa-cta2    53.6062  1812.3977   0.030  0.97641    
# tillNT:ccNC:crop_system_namecorn-grain     -31.8935  1812.3977  -0.018  0.98597    
# tillNT:ccNC:crop_system_namecorn-silage    122.6711  1812.3977   0.068  0.94606    
# tillNT:ccNC:crop_system_namecorn-soy         5.8440  1812.3977   0.003  0.99743    
# tillNT:ccNC:crop_system_namesoy-corn         0.7114  1812.3977   0.000  0.99969    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2563 on 540 degrees of freedom
# Multiple R-squared:  0.1843,	Adjusted R-squared:  0.1314 
# F-statistic: 3.485 on 35 and 540 DF,  p-value: 3.501e-10


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

# # A tibble: 16 × 7
# # Groups:   till, cc [4]
# till  cc    rot          mean    se mean.lbac se.lbac
# <chr> <chr> <chr>       <dbl> <dbl>     <dbl>   <dbl>
#   1 CT    NC    alf         3564.  88.3     3180.    78.8
# 2 NT    NC    alf         3015.  81.2     2690.    72.4
# 3 CT    CC    alf         3012.  70.2     2687.    62.6
# 4 NT    CC    alf         2460.  63.4     2195.    56.6
# 5 NT    NC    corn_silage 1178.  58.5     1051.    52.2
# 6 CT    NC    corn_silage  997.  46.5      890.    41.5
# 7 CT    NC    corn_soy     877.  29.2      782.    26.1
# 8 CT    NC    corn_grain   834.  39.8      744.    35.5
# 9 CT    CC    corn_soy     788.  26.5      703.    23.6
# 10 NT    CC    corn_silage  754.  37.9      673.    33.8
# 11 CT    CC    corn_grain   753.  36.2      672.    32.3
# 12 CT    CC    corn_silage  715.  34.4      638.    30.7
# 13 NT    NC    corn_soy     618.  23.1      552.    20.6
# 14 NT    CC    corn_soy     506.  19.1      452.    17.0
# 15 NT    NC    corn_grain   494.  26.5      441.    23.6
# 16 NT    CC    corn_grain   426.  22.9      380.    20.4

# use these letters in the plot:

wdat_nosite$cctill <- paste0(wdat_nosite$till, "-", wdat_nosite$cc)

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=wdat_nosite, aes(x=cctill, y=mean.lbac, fill=cctill)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.6) + #  , fill="burlywood3") +
  geom_errorbar(aes(ymin=mean.lbac-se.lbac, ymax=mean.lbac+se.lbac), 
                width=0.3, position=position_dodge(0.8),color="gray60") +
  facet_grid(cols=vars(factor(rot, levels=c("corn_grain", "corn_soy", "corn_silage", "alf")))) + 
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
wdat_nosite_crop <- group_by(wdat_persite, till, cc, crop_system_name) %>%
  summarize(mean=mean(sed),
            se=se(sed)) %>%
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
