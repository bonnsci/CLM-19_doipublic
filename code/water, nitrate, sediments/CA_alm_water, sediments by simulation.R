library(tidyverse) # has ggplot, dplyr, etc.
library(reshape2)

se <- function(x) sd(x) / sqrt(length(x))

# if you only need annual totals skip to wdatyr below

# # if you need daily estimates use this:
# wdat <- read.csv("data/large_data/daily water, sediments/CA_almonds_day_water_20240220.csv")
# 
# # UNITS: Transpiration, Evaporation, Water leaching, and runoff are mm/day.
# # Sediment yield is kg/ha.
# 
# # sum data by year  #### re-ran 3/14/24 with new data
# wdatyr <- wdat %>%
#   group_by(site_name, crop_system_name, management_name, climate_scenario, Year) %>%
#   summarize(trans.yr = sum(Transpiration.mm.), evap.yr = sum(Evaporation.mm.),
#             leach.yr = sum(WaterLeaching), run.yr = sum(Runoff), sed.yr = sum(SedimentYield))
# 
# # #
# 
# write.csv(wdatyr, "data/water, nitrate, sediments/CA_alm_water,seds_annualtotals.csv", row.names=F)
# 
# # clean up
# rm(wdat)



wdatyr <- read.csv("data/water, nitrate, sediments/CA_alm_water,seds_annualtotals.csv")

colnames(wdatyr)[c(2,3,5)] <- c("crop", "management", "year") 
# unique(wdatyr$crop) # "almond-a" "almond-c"  -a is alley, -c is crop, alley is 55% and crop is 45% in weighting values together to come up with one value for the system
# unique(wdatyr$management) # "ct-bc" "ct-lc" "ct-nc" "nt-bc" "nt-lc" "nt-nc" "rt-bc" "rt-lc" "rt-nc" "cn"    "rn" 
# crop row: cn for conventional N and rn for reduced n
# alley area: ct- = conventional till, nt= no till, rt= reduced till
  # bc = triticale cover (basic cover), nc = no cover, lc=legume cover

wdatyr <- wdatyr[wdatyr$year>2021 & wdatyr$year<2073 & wdatyr$climate_scenario=="rcp60",]
# 
wdatyr$till <- ifelse(grepl("ct-", wdatyr$management), "CT", 
                      ifelse(grepl("rt-", wdatyr$management), "RT", 
                             ifelse(grepl("nt-", wdatyr$management), "NT", "NA")))
# # check
# unique(wdatyr$till)  # there will be NA values for the "cn" and "rn" for crop areas

# dummy for CC or NC
wdatyr$cc <- ifelse(grepl("-nc", wdatyr$management), "NC", 
                    ifelse(grepl("-bc", wdatyr$management), "TC", 
                           ifelse(grepl("-lc", wdatyr$management), "LC", "NA")))  #triticale, legume


# # check
# unique(wdatyr$cc) # there will be NA values for the "cn" and "rn" for crop areas

# dummy for N treatment
wdatyr$nfert <- ifelse(grepl("cn", wdatyr$management), "Conventional N", 
                       ifelse(grepl("rn", wdatyr$management), "Reduced N", "NA"))

# check
# unique(wdatyr$nfert) # there will be NA values for the alley areas

wdatyr$system <- ifelse(grepl("-a", wdatyr$crop), "alley", "crop")
# unique(wdatyr$system)


# dummy for decade
wdatyr$decade <- ifelse(wdatyr$year <2031, "2020s",
                        ifelse(wdatyr$year>=2031 & wdatyr$year <2041, "2030s",
                               ifelse(wdatyr$year>=2041 & wdatyr$year <2051, "2040s",
                                      ifelse(wdatyr$year>=2051 & wdatyr$year <2061, "2050s",
                                             ifelse(wdatyr$year>=2061 & wdatyr$year <2071, "2060s", "2070s")))))
# unique(wdatyr$decade)

wdatyr$et.yr <- wdatyr$evap.yr + wdatyr$trans.yr

# do calculations for alley + crop row proportions to get one value for the whole system

wdatyr.a <- wdatyr[wdatyr$system=="alley",]
colnames(wdatyr.a)[c(6:10, 16)] <- c("trans.yr.alley", "evap.yr.alley", "leach.yr.alley", "run.yr.alley", "sed.yr.alley", "et.yr.alley")
wdatyr.a <- wdatyr.a[,c(1,5:12,15, 16)]

wdatyr.c <- wdatyr[wdatyr$system=="crop",]
colnames(wdatyr.c)[c(6:10, 16)] <- c("trans.yr.crop", "evap.yr.crop", "leach.yr.crop", "run.yr.crop", "sed.yr.crop", "et.yr.crop")
wdatyr.c <- wdatyr.c[,c(1,5:10,13,15, 16)]

wdatyrw <- full_join(wdatyr.a, wdatyr.c, by=join_by(site_name, decade, year),
                     suffix=c(".x", ".y"),
                     multiple="all",
                     relationship="many-to-many")

# expect to have twice as many rows as wdatyr.a - basically need two copies of wdatyr.a - 
# one copy to combine with Conventional N and one copy to combine with Reduced N.



rm(wdatyr.a, wdatyr.c)

# do the math to combine alley and row nitrate losses to one system value
wdatyrw$trans.yrtot <- (0.45*wdatyrw$trans.yr.crop) + (0.55*wdatyrw$trans.yr.alley)
wdatyrw$evap.yrtot <- (0.45*wdatyrw$evap.yr.crop) + (0.55*wdatyrw$evap.yr.alley)
wdatyrw$leach.yrtot <- (0.45*wdatyrw$leach.yr.crop) + (0.55*wdatyrw$leach.yr.alley)
wdatyrw$run.yrtot <- (0.45*wdatyrw$run.yr.crop) + (0.55*wdatyrw$run.yr.alley)
wdatyrw$sed.yrtot <- (0.45*wdatyrw$sed.yr.crop) + (0.55*wdatyrw$sed.yr.alley)
wdatyrw$et.yrtot <- (0.45*wdatyrw$et.yr.crop) + (0.55*wdatyrw$et.yr.alley)

# convert mm to inches (except sediments are in kg/ha  to lb/ac)
wdatyrw$trans.yrtot.in <- wdatyrw$trans.yrtot/25.4
wdatyrw$evap.yrtot.in <- wdatyrw$evap.yrtot/25.4
wdatyrw$leach.yrtot.in <- wdatyrw$leach.yrtot/25.4
wdatyrw$run.yrtot.in <- wdatyrw$run.yrtot/25.4
wdatyrw$sed.yrtot.lbac <- wdatyrw$sed.yrtot/(0.4536*2.471)
wdatyrw$et.yrtot.in <- wdatyrw$et.yrtot/25.4

wdatyrw$grtot.in <- wdatyrw$trans.yrtot.in + wdatyrw$evap.yrtot.in + wdatyrw$leach.yrtot.in + wdatyrw$run.yrtot.in

wdatyrw <- wdatyrw[,c(1,2,8:10, 17, 25:31)]

write.csv(wdatyrw, "data/water, nitrate, sediments/CA_alm_wdatyr.csv", row.names=F)


# means across all years,

wmeaninch <- wdatyrw %>%
  group_by(cc, nfert) %>%  ### till,  the results do not differ greatly between tillage treatments so I'm coming back here and averaging across tillage
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
  pivot_longer(cols= -c( "cc", "nfert"), # i.e., don't pivot these columns  # till, 
               names_to=c("component", ".value"), # what we are calling the objects 
               # there should be as many new columns as there are unique tags
               # after the "_" separator (i.e., "_mean" and "_se")
               names_sep="_")  # note this type of pivot_longer doesn't work if you use a "." separator!
                          

# for stacked bar, want error bars, would need to make a new variable that is the sum of the variable mean + the means below it.


# are the water components different among tillage and cover crop treatment groups??
# probably a more efficient way to code this with fewer lines but didn't have time to think that through

effecttrans <- aov(trans.yrtot.in ~cc, data=wdatyrw[wdatyrw$till %in% c("CT", "NT") & wdatyrw$nfert=="Conventional N",])  # 
summary(effecttrans)
# previous results when we include tillage: 
# till           1      2       2    0.413 0.5203    
# cc             2  33011   16506 3392.895 <2e-16 ***
#   till:cc        2     39      19    3.980 0.0187 *  
#   Residuals   4890  23789       5                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## cc only model: 
# Df Sum Sq Mean Sq F value Pr(>F)    
# cc             2  33011   16506    3389 <2e-16 ***
#   Residuals   4893  23830       5                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukouttrans <- TukeyHSD(effecttrans)
cldtrans<- multcompView::multcompLetters4(effecttrans, Tukouttrans)
cldtrans<- as.data.frame.list(cldtrans$cc) # $`till:cc`

effectevap <- aov(evap.yrtot.in ~cc, data=wdatyrw[wdatyrw$till %in% c("CT", "NT") & wdatyrw$nfert=="Conventional N",])  # 
summary(effectevap)
# cc*till results:
# Df Sum Sq Mean Sq  F value   Pr(>F)    
# till           1      0     0.0    0.000 0.996784    
# cc             2   4611  2305.3 1600.177  < 2e-16 ***
#   till:cc        2     27    13.3    9.206 0.000102 ***
#   Residuals   4890   7045     1.4                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# cc only
# Df Sum Sq Mean Sq F value Pr(>F)    
# cc             2   4611  2305.3    1595 <2e-16 ***
#   Residuals   4893   7071     1.4                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukoutevap <- TukeyHSD(effectevap)
cldevap<- multcompView::multcompLetters4(effectevap, Tukoutevap)
cldevap<- as.data.frame.list(cldevap$cc) # $`till:cc`

effectleach <- aov(leach.yrtot.in ~cc, data=wdatyrw[wdatyrw$till %in% c("CT", "NT") & wdatyrw$nfert=="Conventional N",])  # 
summary(effectleach)
# cc*till
# Df Sum Sq Mean Sq F value Pr(>F)    
# till           1     44      44   0.494  0.482    
# cc             2  11020    5510  62.258 <2e-16 ***
#   till:cc        2     34      17   0.190  0.827    
# Residuals   4890 432794      89                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# cc only
# Df Sum Sq Mean Sq F value Pr(>F)    
# cc             2  11020    5510   62.28 <2e-16 ***
#   Residuals   4893 432872      88                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukoutleach <- TukeyHSD(effectleach)
cldleach<- multcompView::multcompLetters4(effectleach, Tukoutleach)
cldleach<- as.data.frame.list(cldleach$cc)  # $`till:cc`

effectrun <- aov(run.yrtot.in ~cc, data=wdatyrw[wdatyrw$till %in% c("CT", "NT") & wdatyrw$nfert=="Conventional N",])  # 
summary(effectrun)
# cc*till results
# Df Sum Sq Mean Sq F value   Pr(>F)    
# till           1    118   118.0   4.385   0.0363 *  
#   cc             2   1104   551.9  20.509 1.35e-09 ***
#   till:cc        2    115    57.6   2.140   0.1178    
# Residuals   4890 131602    26.9                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# cc only results
# Df Sum Sq Mean Sq F value   Pr(>F)    
# cc             2   1104   551.9   20.48 1.38e-09 ***
#   Residuals   4893 131835    26.9                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukoutrun <- TukeyHSD(effectrun)
cldrun<- multcompView::multcompLetters4(effectrun, Tukoutrun)
cldrun<- as.data.frame.list(cldrun$cc)

effectsed <- aov(sed.yrtot.lbac ~cc, data=wdatyrw[wdatyrw$till %in% c("CT", "NT") & wdatyrw$nfert=="Conventional N",])  # 
summary(effectsed)
# till*cc output
# Df    Sum Sq Mean Sq F value Pr(>F)    
# till           1    121026  121026   1.792  0.181    
# cc             2  12420177 6210088  91.940 <2e-16 ***
#   till:cc        2     61570   30785   0.456  0.634    
# Residuals   4890 330295382   67545                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# cc only output
# Df    Sum Sq Mean Sq F value Pr(>F)    
# cc             2  12420177 6210088   91.94 <2e-16 ***
#   Residuals   4893 330477978   67541                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukoutsed <- TukeyHSD(effectsed)
cldsed<- multcompView::multcompLetters4(effectsed, Tukoutsed)
cldsed<- as.data.frame.list(cldsed$cc)

effectgr <- aov(grtot.in ~cc, data=wdatyrw[wdatyrw$till %in% c("CT", "NT") & wdatyrw$nfert=="Conventional N",])  # 
summary(effectgr)
# till*cc output
# Df  Sum Sq Mean Sq F value Pr(>F)
# till           1      32   32.21   0.150  0.698
# cc             2     358  178.95   0.835  0.434
# till:cc        2      29   14.48   0.068  0.935
# Residuals   4890 1047659  214.25   

# cc only output
# Df  Sum Sq Mean Sq F value Pr(>F)
# cc             2     358   178.9   0.836  0.434
# Residuals   4893 1047720   214.1     
Tukoutgr <- TukeyHSD(effectgr)
cldgr<- multcompView::multcompLetters4(effectgr, Tukoutgr)
cldgr<- as.data.frame.list(cldgr$cc)


cldtrans$component <- rep("trans", 3)  # ,3) if cc only, ,3) if till*cc
cldevap$component <- rep("evap", 3)
cldrun$component <- rep("run", 3)
cldleach$component <- rep("leach", 3)
cldsed$component <- rep("sed", 3)
cldgr$component <- rep("grtot", 3)

wmeaninch2 <- wmeaninch[wmeaninch$nfert=="Conventional N" & !wmeaninch$component=="et",] # & !wmeaninch$till=="RT",]

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

### use this table to talk about significant differences among treatments within each water component
wmeaninch2 <- group_by(wmeaninch2, component) %>%
  arrange(desc(mean), .by_group=T) %>%
  ungroup() %>%
  mutate(ID=c(1:length(component))) %>%
  full_join(., cld)

rm(cld, cldtrans, cldevap, cldrun, cldleach,cldsed, cldgr, Tukoutevap, Tukoutgr, Tukoutleach, Tukoutrun, Tukoutsed, Tukouttrans,
   effectevap, effectgr, effectleach, effectrun, effectsed, effecttrans)
# for aovs with cover crops only
# # A tibble: 18 × 7
# cc    nfert          component    mean        se    ID Letters
# <chr> <chr>          <chr>       <dbl>     <dbl> <int> <chr>  
#   1 TC    Conventional N evap         7.95   0.0250      1 a      
# 2 NC    Conventional N evap         7.40   0.0270      2 b      
# 3 LC    Conventional N evap         5.67   0.0204      3 c      
# 4 NC    Conventional N grtot       21.2    0.301       4 a      
# 5 TC    Conventional N grtot       21.0    0.299       5 a      
# 6 LC    Conventional N grtot       20.6    0.289       6 a      
# 7 NC    Conventional N leach        8.77   0.204       7 a      
# 8 TC    Conventional N leach        7.71   0.205       8 b      
# 9 LC    Conventional N leach        5.16   0.155       9 c      
# 10 NC    Conventional N run          3.21   0.114      10 a      
# 11 TC    Conventional N run          3.01   0.111      11 a      
# 12 LC    Conventional N run          2.17   0.0900     12 b      
# 13 NC    Conventional N sed       4398.   149.         13 a      
# 14 TC    Conventional N sed       3627.   130.         14 b      
# 15 LC    Conventional N sed       1686.    64.0        15 c      
# 16 LC    Conventional N trans        7.58   0.0767     16 a      
# 17 TC    Conventional N trans        2.38   0.00772    17 b      
# 18 NC    Conventional N trans        1.81   0.00466    18 c      

# print(wmeaninch2, n=nrow(wmeaninch2))
# # A tibble: 36 × 8
# till  cc    nfert          component   mean       se    ID Letters
# <chr> <chr> <chr>          <chr>      <dbl>    <dbl> <int> <chr>  
#   1 NT    TC    Conventional N evap        8.02  0.0427      1 a      
# 2 CT    TC    Conventional N evap        7.85  0.0438      2 b      
# 3 NT    NC    Conventional N evap        7.41  0.0469      3 c      
# 4 CT    NC    Conventional N evap        7.39  0.0466      4 c      
# 5 CT    LC    Conventional N evap        5.76  0.0369      5 d      
# 6 NT    LC    Conventional N evap        5.57  0.0334      6 e      
# 7 CT    NC    Conventional N grtot      21.2   0.521       7 a      
# 8 NT    NC    Conventional N grtot      21.2   0.521       8 a      
# 9 CT    TC    Conventional N grtot      21.1   0.519       9 a      
# 10 NT    TC    Conventional N grtot      21.0   0.515      10 a      
# 11 CT    LC    Conventional N grtot      20.7   0.508      11 a      
# 12 NT    LC    Conventional N grtot      20.4   0.490      12 a      
# 13 CT    NC    Conventional N leach       8.77  0.354      13 a      
# 14 NT    NC    Conventional N leach       8.76  0.354      14 a      
# 15 NT    TC    Conventional N leach       7.80  0.359      15 a      
# 16 CT    TC    Conventional N leach       7.62  0.353      16 a      
# 17 NT    LC    Conventional N leach       5.39  0.281      17 b      
# 18 CT    LC    Conventional N leach       4.99  0.260      18 b      
# 19 CT    NC    Conventional N run         3.21  0.197      19 a      
# 20 NT    NC    Conventional N run         3.20  0.198      20 ab     
# 21 CT    TC    Conventional N run         3.08  0.195      21 ab     
# 22 NT    TC    Conventional N run         2.89  0.190      22 ab     
# 23 CT    LC    Conventional N run         2.47  0.173      23 bc     
# 24 NT    LC    Conventional N run         1.74  0.126      24 c      
# 25 CT    NC    Conventional N sed       194.   11.4        25 a      
# 26 NT    NC    Conventional N sed       194.   11.4        26 a      
# 27 CT    TC    Conventional N sed       162.    9.84       27 ab     
# 28 NT    TC    Conventional N sed       152.    9.76       28 b      
# 29 CT    LC    Conventional N sed        82.6   5.23       29 c      
# 30 NT    LC    Conventional N sed        64.5   4.34       30 c      
# 31 NT    LC    Conventional N trans       7.66  0.134      31 a      
# 32 CT    LC    Conventional N trans       7.51  0.132      32 a      
# 33 CT    TC    Conventional N trans       2.53  0.0141     33 b      
# 34 NT    TC    Conventional N trans       2.26  0.0116     34 b      
# 35 CT    NC    Conventional N trans       1.81  0.00808    35 c      
# 36 NT    NC    Conventional N trans       1.81  0.00808    36 c 


# 
# wtotlong$variable <- factor(wtotlong$variable, levels=c("evap", "trans", "leach", "run", "sed", "et"))


windows(xpinch=200, ypinch=200, width=5, height=5)

# annual water
ggplot(data=wmeaninch[!wmeaninch$component %in% c("sed", "et", "grtot") & 
                        wmeaninch$nfert=="Conventional N",], 
       aes(x=nfert, y=mean, fill=component)) +
  geom_bar(stat="identity", width=0.7) +   # , color="#20243d"
  # geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, position=position_dodge(0.8), color="#332288") +
  facet_grid(cols=vars(factor(cc, levels=c("NC", "TC", "LC"))),  #  factor(till, levels=c("CT", "NT"))),  
             # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             labeller = as_labeller(
               c("NC"="No Cover Crop", "TC" = "Triticale Cover", "LC"="Legume Cover"))) +
                # "CT" = "Conventional Till",  "NT" = "No Till"))) +  #, "RT"="Reduced Ti
  scale_fill_manual(values=c("#c44f2d", "#20243d", "#669947", "#C2e4ef")) +  # "#999933"
  xlab("tillage") +
  ylab("annual inches 2022 to 2072") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/CA_almonds_water losses annual stacked bars by cover no tillage.png", width=6, height=4, dpi=300)

# fert rates
(82.2*3)/(0.4536*2.471)
560.4/(0.4536*2.471)





# PLOT FOR SEDIMENTS

sed <- wmeaninch2[wmeaninch2$component=="sed",]
sed$mean.tac <- sed$mean/2000
sed$se.tac <- sed$se/2000

windows(xpinch=200, ypinch=200, width=5, height=5)


ggplot(data=sed, aes(x=component, y=mean.tac)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.7, fill="burlywood3") +  # color="sienna4", 
  geom_errorbar(aes(ymin = mean.tac - se.tac, ymax = mean.tac + se.tac), 
                width=0.3, position = position_dodge(0.8), color="sienna4", linewidth=1) +
  facet_grid(cols=vars(factor(cc, levels=c("NC", "TC", "LC"))),  #  factor(till, levels=c("CT", "NT"))),  
             # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             labeller = as_labeller(
               c("NC"="No Cover Crop", "TC" = "Triticale Cover", "LC"="Legume Cover"))) +
  # "CT" = "Conventional Till",  "NT" = "No Till"))) +  #, "RT"="Reduced Ti
  # scale_y_continuous(breaks=seq(0,300, 50), limits=c(0,300)) +
  # scale_x_discrete(breaks = c("CT", "RT", "NT"),
  #                  labels = c("Conv.", "Reduced", "No")) +

  xlab("Tillage") +
  ylab('2022-72 mean annual sediment yield (tons per acre)') +
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

ggsave("plots/water, nitrate, sediments/CA_alm sediments mean annual bars cc only.png", width=5, height=4.5, dpi=300)






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
