library(tidyverse) # has ggplot, dplyr, etc.
library(reshape2)

se <- function(x) sd(x) / sqrt(length(x))

# if you only need annual totals skip to wdatyr below (assuming this has been run and wdatyr is setup properly)

# # if you need daily estimates use this:
# wdat <- read.csv("data/large_data/daily water, sediments/CA_vineyards_day_water_20240220.csv")
# # wdat[wdat$crop_system_name=="grape-c" & wdat$site_name=="v_2" &  wdat$Year >2021,]  # wdat$climate_scenario=="rcp60" &
# # 
# check <- aggregate(Day ~ climate_scenario + crop_system_name  + site_name, dat=wdat[wdat$Year>2021,], FUN="length") %>%  #
#   arrange(climate_scenario, crop_system_name, site_name)
# 
# # 
# # # # 
# # # # # UNITS: Transpiration, Evaporation, Water leaching, and runoff are mm/day.
# # # # # Sediment yield is kg/ha.
# # # # 
# # # # # sum data by year  #### re-ran 3/14/24 with new data
# wdatyr <- wdat %>%
#   group_by(site_name, crop_system_name, management_name, climate_scenario, Year) %>%
#   summarize(trans.yr = sum(Transpiration.mm.), evap.yr = sum(Evaporation.mm.),
#             leach.yr = sum(WaterLeaching), run.yr = sum(Runoff), sed.yr = sum(SedimentYield))
# 
# # # # #
# # #
# write.csv(wdatyr, "data/water, nitrate, sediments/CA_grape_water,seds_annualtotals.csv", row.names=F)
# # # # 
# # # # clean up
# rm(wdat)

# wdatyr <- read.csv("data/water, nitrate, sediments/CA_grape_water,seds_annualtotals.csv")
# 
# colnames(wdatyr)[c(2,3,5)] <- c("crop", "management", "year")
# # # unique(wdatyr$crop) # "grape-a" "grape-c"  -a is alley, -c is crop, alley is 55% and crop is 45% in weighting values together to come up with one value for the system
# # # unique(wdatyr$management) # "ct-bc" "ct-lc" "ct-nc" "cn"    "nt-lc" "nt-bc" "rt-nc" "nt-nc" "rt-lc" "rt-bc" "rn"
# # # crop row: cn for conventional N and rn for reduced n
# # # alley area: ct- = conventional till, nt= no till, rt= reduced till
# #   # bc = barley cover (basic cover), nc = no cover, lc=legume cover
# #
# wdatyr <- wdatyr[wdatyr$year>2021 & wdatyr$year<2073 & wdatyr$climate_scenario=="rcp60",]
# # #
# wdatyr$till <- ifelse(grepl("ct-", wdatyr$management), "CT",
#                       ifelse(grepl("rt-", wdatyr$management), "RT",
#                              ifelse(grepl("nt-", wdatyr$management), "NT", "NA")))
# # # # check
# # # unique(wdatyr$till)  # there will be NA values for the "cn" and "rn" for crop areas
# 
# # # factor for CC or NC
# wdatyr$cc <- ifelse(grepl("-nc", wdatyr$management), "NC",
#                     ifelse(grepl("-bc", wdatyr$management), "BarC",
#                            ifelse(grepl("-lc", wdatyr$management), "LegC", "NA")))  #barley, legume
# 
# 
# # # # check
# # # unique(wdatyr$cc) # there will be NA values for the "cn" and "rn" for crop areas
# #
# # # factor for N treatment
# wdatyr$nfert <- ifelse(grepl("cn", wdatyr$management), "Conventional N",
#                        ifelse(grepl("rn", wdatyr$management), "Reduced N", "NA"))
# 
# # # check
# # # unique(wdatyr$nfert) # there will be NA values for the alley areas
# #
# wdatyr$system <- ifelse(grepl("-a", wdatyr$crop), "alley",
#                         ifelse(grepl("-c", wdatyr$crop), "crop", "NA"))
# # # unique(wdatyr$system) # should not have NAs
# #
# #
# # # factor for decade
# wdatyr$decade <- ifelse(wdatyr$year <2031, "2020s",
#                         ifelse(wdatyr$year>=2031 & wdatyr$year <2041, "2030s",
#                                ifelse(wdatyr$year>=2041 & wdatyr$year <2051, "2040s",
#                                       ifelse(wdatyr$year>=2051 & wdatyr$year <2061, "2050s",
#                                              ifelse(wdatyr$year>=2061 & wdatyr$year <2071, "2060s", "2070s")))))
# # # unique(wdatyr$decade)
# #
# wdatyr$et.yr <- wdatyr$evap.yr + wdatyr$trans.yr
# #
# # # do calculations for alley + crop row proportions to get one value for the whole system
# #
# wdatyr.a <- wdatyr[wdatyr$system=="alley",]
# colnames(wdatyr.a)[c(6:10, 16)] <- c("trans.yr.alley", "evap.yr.alley", "leach.yr.alley", "run.yr.alley", "sed.yr.alley", "et.yr.alley")
# wdatyr.a <- wdatyr.a[,c(1,5:12,15, 16)]
# 
# wdatyr.c <- wdatyr[wdatyr$system=="crop",]
# colnames(wdatyr.c)[c(6:10, 16)] <- c("trans.yr.crop", "evap.yr.crop", "leach.yr.crop", "run.yr.crop", "sed.yr.crop", "et.yr.crop")
# wdatyr.c <- wdatyr.c[,c(1,5:10,13,15, 16)]
# 
# wdatyrw <- full_join(wdatyr.a, wdatyr.c, by=join_by(site_name, decade, year),
#                      suffix=c(".x", ".y"),
#                      multiple="all",
#                      relationship="many-to-many")
# 
# #
# # # expect to have twice as many rows as wdatyr.a - basically need two copies of wdatyr.a -
# # # one copy to combine with Conventional N and one copy to combine with Reduced N.
# #
# #
# #
# rm(wdatyr.a, wdatyr.c)
# #
# # # do the math to combine alley and row nitrate losses to one system value
# wdatyrw$trans.yrtot <- (0.15*wdatyrw$trans.yr.crop) + (0.85*wdatyrw$trans.yr.alley)
# wdatyrw$evap.yrtot <- (0.15*wdatyrw$evap.yr.crop) + (0.85*wdatyrw$evap.yr.alley)
# wdatyrw$leach.yrtot <- (0.15*wdatyrw$leach.yr.crop) + (0.85*wdatyrw$leach.yr.alley)
# wdatyrw$run.yrtot <- (0.15*wdatyrw$run.yr.crop) + (0.85*wdatyrw$run.yr.alley)
# wdatyrw$sed.yrtot <- (0.15*wdatyrw$sed.yr.crop) + (0.85*wdatyrw$sed.yr.alley)
# wdatyrw$et.yrtot <- (0.15*wdatyrw$et.yr.crop) + (0.85*wdatyrw$et.yr.alley)
# #
# # # convert mm to inches (except sediments are in kg/ha  to lb/ac)
# wdatyrw$trans.yrtot.in <- wdatyrw$trans.yrtot/25.4
# wdatyrw$evap.yrtot.in <- wdatyrw$evap.yrtot/25.4
# wdatyrw$leach.yrtot.in <- wdatyrw$leach.yrtot/25.4
# wdatyrw$run.yrtot.in <- wdatyrw$run.yrtot/25.4
# wdatyrw$sed.yrtot.lbac <- wdatyrw$sed.yrtot/(0.4536*2.471)
# wdatyrw$et.yrtot.in <- wdatyrw$et.yrtot/25.4
# 
# wdatyrw$grtot.in <- wdatyrw$trans.yrtot.in + wdatyrw$evap.yrtot.in + wdatyrw$leach.yrtot.in + wdatyrw$run.yrtot.in
# #
# wdatyrw <- wdatyrw[,c(1,2,8:10, 17, 25:31)]
# 
# write.csv(wdatyrw, "data/water, nitrate, sediments/CA_grape_wdatyr.csv", row.names=F)

wdatyrw <- read.csv("data/water, nitrate, sediments/CA_grape_wdatyr.csv")



# are the water components different among tillage and cover crop treatment groups??
# probably a more efficient way to code this with fewer lines but didn't have time to think that through

effecttrans <- aov(trans.yrtot.in ~cc*till, data=wdatyrw[wdatyrw$till %in% c("CT", "NT") & wdatyrw$nfert=="Conventional N",])  # 
summary(effecttrans)
# Df Sum Sq Mean Sq  F value Pr(>F)    
# cc             2  80205   40102 7776.797 <2e-16 ***
#   till           1      0       0    0.034  0.853    
# cc:till        2     11       6    1.067  0.344    
# Residuals   4890  25216       5                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukouttrans <- TukeyHSD(effecttrans)
cldtrans<- multcompView::multcompLetters4(effecttrans, Tukouttrans)
cldtrans<- as.data.frame.list(cldtrans$`cc:till`) # $`till:cc`

effectevap <- aov(evap.yrtot.in ~cc*till, data=wdatyrw[wdatyrw$till %in% c("CT", "NT") & wdatyrw$nfert=="Conventional N",])  # 
summary(effectevap)
#               Df Sum Sq Mean Sq  F value  Pr(>F)    
# cc             2   5102  2551.1 2306.221 < 2e-16 ***
# till           1     11    11.4   10.321 0.00132 ** 
# cc:till        2     13     6.3    5.707 0.00334 ** 
# Residuals   4890   5409     1.1                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukoutevap <- TukeyHSD(effectevap)
cldevap<- multcompView::multcompLetters4(effectevap, Tukoutevap)
cldevap<- as.data.frame.list(cldevap$`cc:till`) # $`till:cc`

effectleach <- aov(leach.yrtot.in ~cc*till, data=wdatyrw[wdatyrw$till %in% c("CT", "NT") & wdatyrw$nfert=="Conventional N",])  # 
summary(effectleach)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# cc             2  28816   14408 138.934  < 2e-16 ***
#   till           1   1475    1475  14.221 0.000164 ***
#   cc:till        2    235     117   1.132 0.322357    
# Residuals   4890 507121     104                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukoutleach <- TukeyHSD(effectleach)
cldleach<- multcompView::multcompLetters4(effectleach, Tukoutleach)
cldleach<- as.data.frame.list(cldleach$`cc:till`)  # $`till:cc`

effectrun <- aov(run.yrtot.in ~cc*till, data=wdatyrw[wdatyrw$till %in% c("CT", "NT") & wdatyrw$nfert=="Conventional N",])  # 
summary(effectrun)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# cc             2   6923    3461  42.191  < 2e-16 ***
#   till           1   4118    4118  50.189 1.59e-12 ***
#   cc:till        2    287     144   1.749    0.174    
# Residuals   4890 401183      82                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukoutrun <- TukeyHSD(effectrun)
cldrun<- multcompView::multcompLetters4(effectrun, Tukoutrun)
cldrun<- as.data.frame.list(cldrun$`cc:till`)

effectsed <- aov(sed.yrtot.lbac ~cc*till, data=wdatyrw[wdatyrw$till %in% c("CT", "NT") & wdatyrw$nfert=="Conventional N",])  # 
summary(effectsed)
# Df    Sum Sq   Mean Sq F value Pr(>F)    
# cc             2 1.435e+10 7.174e+09  51.015 <2e-16 ***
#   till           1 2.548e+08 2.548e+08   1.812  0.178    
# cc:till        2 8.211e+07 4.105e+07   0.292  0.747    
# Residuals   4890 6.876e+11 1.406e+08                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukoutsed <- TukeyHSD(effectsed)
cldsed<- multcompView::multcompLetters4(effectsed, Tukoutsed)
cldsed<- as.data.frame.list(cldsed$`cc:till`)

effectgr <- aov(grtot.in ~cc*till, data=wdatyrw[wdatyrw$till %in% c("CT", "NT") & wdatyrw$nfert=="Conventional N",])  # 
summary(effectgr)
# Df  Sum Sq Mean Sq F value Pr(>F)
# cc             2    1593   796.6   2.060  0.128
# till           1     874   874.1   2.260  0.133
# cc:till        2      50    25.1   0.065  0.937
# Residuals   4890 1891163   386.7      
Tukoutgr <- TukeyHSD(effectgr)
cldgr<- multcompView::multcompLetters4(effectgr, Tukoutgr)
cldgr<- as.data.frame.list(cldgr$`cc:till`)


cldtrans$component <- rep("trans", 6)  # ,3) if cc only, ,3) if till*cc 6
cldevap$component <- rep("evap", 6)
cldrun$component <- rep("run", 6)
cldleach$component <- rep("leach", 6)
cldsed$component <- rep("sed", 6)
cldgr$component <- rep("grtot", 6)


cldtrans <- cldtrans[,c("Letters", "component")]
cldevap <- cldevap[,c("Letters", "component")]
cldrun <- cldrun[,c("Letters", "component")]
cldleach <- cldleach[,c("Letters", "component")]
cldsed <- cldsed[,c("Letters", "component")]
cldgr <- cldgr[,c("Letters", "component")]

cld <- rbind(cldtrans, cldevap, cldrun, cldleach, cldsed, cldgr)
cld <- group_by(cld, component) %>%
  arrange(component, Letters) 
cld$ID <- 1:nrow(cld)

# means across all years for plotting and joining to cld info

wmeaninch <- wdatyrw %>%
  filter(nfert=="Conventional N", !till=="RT") %>%
  group_by(till, cc, nfert) %>%  ### 
  summarize(et_mean = mean(et.yrtot.in), 
            evap_mean = mean(evap.yrtot.in),
            trans_mean = mean(trans.yrtot.in),
            leach_mean = mean(leach.yrtot.in),
            run_mean = mean(run.yrtot.in),
            sed_mean = mean(sed.yrtot.lbac),
            grtot_mean = mean(grtot.in),
            et_se = se(et.yrtot.in), 
            evap_se = se(evap.yrtot.in),
            trans_se = se(trans.yrtot.in),
            leach_se = se(leach.yrtot.in),
            run_se = se(run.yrtot.in),
            sed_se = se(sed.yrtot.lbac),
            grtot_se = se(grtot.in)) %>%
  pivot_longer(cols= -c("till", "cc", "nfert"), # i.e., don't pivot these columns  # till, 
               names_to=c("component", ".value"), # what we are calling the objects 
               # there should be as many new columns as there are unique tags
               # after the "_" separator (i.e., "_mean" and "_se")
               names_sep="_") %>%  # note this type of pivot_longer doesn't work if you use a "." separator!
  ungroup() %>%
  filter(!component=="et") %>%
  group_by(component) %>%
  arrange(desc(mean), .by_group=T) %>%
  ungroup() %>%
  mutate(ID=c(1:length(component))) %>%
  full_join(., cld)

# for stacked bar, want error bars, would need to make a new variable that is the sum of the variable mean + the means below it.


rm(cld, cldtrans, cldevap, cldrun, cldleach,cldsed, cldgr, Tukoutevap, Tukoutgr, Tukoutleach, Tukoutrun, Tukoutsed, Tukouttrans,
   effectevap, effectgr, effectleach, effectrun, effectsed, effecttrans)

print(wmeaninch, n=nrow(wmeaninch))
# # A tibble: 36 × 8
# till  cc    nfert          component     mean         se    ID Letters
# <chr> <chr> <chr>          <chr>        <dbl>      <dbl> <int> <chr>  
#   1 NT    NC    Conventional N evap         8.20    0.0471       1 a      
# 2 CT    NC    Conventional N evap         8.17    0.0468       2 a      
# 3 CT    BarC  Conventional N evap         5.57    0.0317       3 b      
# 4 NT    BarC  Conventional N evap         5.22    0.0286       4 c      
# 5 CT    LegC  Conventional N evap         4.54    0.0322       5 d      
# 6 NT    LegC  Conventional N evap         4.41    0.0308       6 d      
# 7 CT    NC    Conventional N grtot       37.9     0.709        7 a      
# 8 NT    NC    Conventional N grtot       37.0     0.701        8 ab     
# 9 CT    BarC  Conventional N grtot       36.7     0.689        9 ab     
# 10 CT    LegC  Conventional N grtot       36.0     0.675       10 ab     
# 11 NT    BarC  Conventional N grtot       35.0     0.649       11 b      
# 12 NT    LegC  Conventional N grtot       34.6     0.639       12 b      
# 13 NT    NC    Conventional N leach       18.5     0.382       13 a      
# 14 CT    NC    Conventional N leach       17.6     0.363       14 a      
# 15 NT    BarC  Conventional N leach       14.1     0.407       15 b      
# 16 CT    BarC  Conventional N leach       11.6     0.338       16 c      
# 17 NT    LegC  Conventional N leach        9.72    0.346       17 d      
# 18 CT    LegC  Conventional N leach        8.13    0.285       18 e      
# 19 CT    NC    Conventional N run         11.7     0.358       19 a      
# 20 NT    NC    Conventional N run          9.82    0.344       20 b      
# 21 CT    BarC  Conventional N run          9.39    0.325       21 b      
# 22 CT    LegC  Conventional N run          7.88    0.291       22 c      
# 23 NT    BarC  Conventional N run          5.72    0.228       23 d      
# 24 NT    LegC  Conventional N run          4.91    0.191       24 d      
# 25 CT    NC    Conventional N sed       8418.    591.          25 a      
# 26 NT    NC    Conventional N sed       7148.    566.          26 a      
# 27 CT    BarC  Conventional N sed       2504.    184.          27 b      
# 28 CT    LegC  Conventional N sed       2265.    161.          28 b      
# 29 NT    BarC  Conventional N sed       2049.    155.          29 b      
# 30 NT    LegC  Conventional N sed       1875.    132.          30 b      
# 31 NT    LegC  Conventional N trans       15.6     0.181       31 a      
# 32 CT    LegC  Conventional N trans       15.5     0.180       32 a      
# 33 CT    BarC  Conventional N trans       10.2     0.114       33 b      
# 34 NT    BarC  Conventional N trans        9.97    0.108       34 b      
# 35 CT    NC    Conventional N trans        0.409   0.000941    35 c      
# 36 NT    NC    Conventional N trans        0.409   0.000941    36 c      


# 
# wtotlong$variable <- factor(wtotlong$variable, levels=c("evap", "trans", "leach", "run", "sed", "et"))


windows(xpinch=200, ypinch=200, width=5, height=5)

# annual water
ggplot(data=wmeaninch[!wmeaninch$component %in% c("sed", "et", "grtot") & 
                        wmeaninch$nfert=="Conventional N",], 
       aes(x=till, y=mean, fill=component)) +
  geom_bar(stat="identity", width=0.7) +   # , color="#20243d"
  # geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, position=position_dodge(0.8), color="#332288") +
  facet_grid(cols=vars(factor(cc, levels=c("NC", "BarC", "LegC"))), # factor(till, levels=c("CT", "NT"))),  
             # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             labeller = as_labeller(
               c("NC"="No Cover Crop", "BarC" = "Barley Cover", "LegC"="Legume Cover",
                "CT" = "Conventional Till",  "NT" = "No Till"))) +  #, "RT"="Reduced Ti
  scale_fill_manual(values=c("#c44f2d", "#20243d", "#669947", "#C2e4ef")) +  # "#999933"
  xlab("tillage") +
  ylab("annual inches 2022 to 2072") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/CA_grape_water losses annual stacked bars by cover and till_no letters.png", width=6, height=4, dpi=300)

# fert rates
(82.2*3)/(0.4536*2.471)
560.4/(0.4536*2.471)





# PLOT FOR SEDIMENTS

sed <- wmeaninch[wmeaninch$component=="sed",]
sed$mean.tac <- sed$mean/2000
sed$se.tac <- sed$se/2000

# cc    nfert          component  mean    se    ID Letters mean.tac se.tac
# <chr> <chr>          <chr>     <dbl> <dbl> <int> <chr>      <dbl>  <dbl>
# 1 NC    Conventional N sed       4398. 149.     13 a          2.20  0.0743
# 2 TC    Conventional N sed       3627. 130.     14 b          1.81  0.0648
# 3 LC    Conventional N sed       1686.  64.0    15 c          0.843 0.0320

windows(xpinch=200, ypinch=200, width=5, height=5)


pal2 <- c("#20243d", "#669947")  #  "#C2e4ef",

ggplot(data=sed, aes(x=till, y=mean.tac)) +
  geom_bar(aes(fill=till), stat="identity", position=position_dodge(), width=0.7) +  # color="sienna4",  , fill="burlywood3"
  geom_errorbar(aes(ymin = mean.tac - se.tac, ymax = mean.tac + se.tac), 
                width=0.3, position = position_dodge(0.8), color="gray60", linewidth=1) + # color="sienna4"
  facet_grid(cols=vars(factor(cc, levels=c("NC", "BarC", "LegC"))),  # factor(till, levels=c("CT", "NT"))),  
             # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             labeller = as_labeller(
               c("NC"="No Cover Crop", "BarC" = "Barley Cover", "LegC"="Legume Cover"))) +
  #"CT" = "Conventional Till",  "NT" = "No Till"))) +  #, "RT"="Reduced Ti
  # scale_y_continuous(breaks=seq(0,300, 50), limits=c(0,300)) +
  # scale_x_discrete(breaks = c("CT", "RT", "NT"),
  #                  labels = c("Conv.", "Reduced", "No")) +

  scale_fill_manual(values=pal2) + 
  xlab("Tillage") +
  ylab('2022-72 mean annual sediment yield (tons per acre)') +
  # geom_text(aes(label=Letters),
  #            nudge_y = +0.5,
  #          color="#20243d", size=7, fontface="bold") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(size=11),  #angle=-10, hjust=0, 
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(1,0.5,1,0.5), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))
# # strip.background=element_rect(fill="lightblue", color="black", size=1) 

ggsave("plots/water, nitrate, sediments/CA_grape sediments mean annual bars cc,till only_no letters.png", width=8, height=5, dpi=300)

sed
# # A tibble: 6 × 10
# till  cc    nfert          component  mean    se    ID Letters mean.tac se.tac
# <chr> <chr> <chr>          <chr>     <dbl> <dbl> <int> <chr>      <dbl>  <dbl>
#   1 CT    NC    Conventional N sed       8315.  586.    25 a           4.16  0.293
# 2 NT    NC    Conventional N sed       7493.  569.    26 a           3.75  0.285
# 3 CT    BarC  Conventional N sed       4489.  318.    27 b           2.24  0.159
# 4 CT    LegC  Conventional N sed       4334.  304.    28 b           2.17  0.152
# 5 NT    BarC  Conventional N sed       4194.  300.    29 b           2.10  0.150
# 6 NT    LegC  Conventional N sed       4082.  287.    30 b           2.04  0.144
mean(c(2.24, 2.16, 2.1, 2.04))
# [1] 2.135
mean(c(4.16, 3.75))
# [1] 3.955
(3.955-2.135)/3.955
# [1] 0.460177



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
