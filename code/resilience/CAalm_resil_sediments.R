
# this script explores ways to quantify resilience in the CLM-19 dataset.
# following ideas in the CLM literature / resilience folder. https://americanfarmlandtrust.sharepoint.com/:f:/s/ClimatePrograms/EuOX-vSqLoZHmB8X1WTkIO4B4ZsmvAwlH2U85dBLcLcoRQ?e=jrJY4o
# started 10/31/2023 BMM

# esp. Scheffer et al. 2015
# we'll start with the idea of ranking resilience. I'm thinking different SHMS
# will have different resiliencies to climate extremes, i.e. with CC and NT, more resilient
# biomass C in SOC accumulation in the face of drought / extreme wet, etc.

# load packages
library(tidyverse)
library(MASS)  # for boxcox
# install.packages("ggh4x") 
library(ggh4x) # for strip_themed() and facet_grid2() in ggplot
# install.packages("AICcmodavg")
library(AICcmodavg) # for aictab()
library(multcompView) # for tukey HSD letters
# install.packages("ggpattern")
library(ggpattern)

se <- function(x) sd(x) / sqrt(length(x))
cv <- function(x) sd(x) / mean(x)


spei <- read.csv("data/climate data not large/spei_alm_formerge.csv")
# from script spei_CAalm.R





######################## plain sediment yield (not sediment yield probabilities) by season

# need monthly totals to match with growing season spei
# so we need to go back to daily data rather than annual sum

# # if you need daily estimates use this:
# wdat <- read.csv("data/large_data/daily water, sediments/CA_almonds_day_water_20240220.csv")
# # beepr::beep(sound=8)
# # # # UNITS: Transpiration, Evaporation, Water leaching, and runoff are mm/day.
# # # # Sediment yield is kg/ha.
# # #
# # #
# wdat <- wdat[,c(1:7, 12)]
# 
# wdatssn.site <- wdat %>%
#   mutate(month = strftime(as.Date(date), "%m"),
#          dayofmo = strftime(as.Date(date), "%d")) %>%
#   filter(climate_scenario == "rcp60") %>%
#   mutate(season =  ifelse(month %in% c("12", "01", "02"), "Winter",
#                           ifelse(month %in% c("03", "04", "05"), "Spring",
#                                  ifelse(month %in% c("06", "07", "08"), "Summer", "Fall"))),
#        till = ifelse(grepl("ct-", management_name), "CT",
#                         ifelse(grepl("rt-", management_name), "RT",
#                                ifelse(grepl("nt-", management_name), "NT", "NA"))),
#        cc = ifelse(grepl("-nc", management_name), "NC",
#                    ifelse(grepl("-bc", management_name), "TC",
#                           ifelse(grepl("-lc", management_name), "LC","NA"))),
#        nfert = ifelse(grepl("cn", management_name), "Conventional N",
#                        ifelse(grepl("rn", management_name), "Reduced N","NA")),
#        area = ifelse(grepl("-a", crop_system_name), "alley", "row")) %>%
#   group_by(site_name, area, Year, season, till, cc, nfert) %>%
#   summarize(sed.ssncrop = sum(SedimentYield)) #%>% # sum sediments for season at each site for each cropping area (row, alley)
# #   # group_by(site_name, management_name, Year, season, till, cc, nfert) %>%  # now take the mean of the two crop rotation types at each site for each season
# #   # summarize(sed.ssn = mean(sed.ssncrop))
# # beepr::beep(sound=14)
# # 
# # 
# wdatssn.site.a <- wdatssn.site[wdatssn.site$area=="alley",]
# colnames(wdatssn.site.a)[8] <- c("sed.ssn.alley")
# wdatssn.site.a <- wdatssn.site.a[,c(1,3:6,8)]
# 
# wdatssn.site.r <- wdatssn.site[wdatssn.site$area=="row",]
# colnames(wdatssn.site.r)[8] <- c("sed.ssn.row")
# wdatssn.site.r <- wdatssn.site.r[,c(1,3:4,7,8)]
# 
# wdatssn.sitew <- full_join(wdatssn.site.a, wdatssn.site.r, by=join_by(site_name, Year, season),
#                      suffix=c(".x", ".y"),
#                      multiple="all",
#                      relationship="many-to-many")
# # note should have 2x rows as in wdatssn.site.a
# rm(wdatssn.site.r, wdatssn.site.a, wdatssn.site)
# 
# wdatssn.sitew$sed.ssntot <- (0.45*wdatssn.sitew$sed.ssn.row) + (0.55*wdatssn.sitew$sed.ssn.alley)
# 
# 
# #
# write.csv(wdatssn.sitew, "data/water, nitrate, sediments/seds_CA_alm_ssntotals.csv", row.names=F)
# 
# #
# # clean up
# # rm(wdat, wdatssn.sitew)
# 
wdatssn.site <- read.csv("data/water, nitrate, sediments/seds_CA_alm_ssntotals.csv")
wdatssn.site <- wdatssn.site[wdatssn.site$Year >2022 & wdatssn.site$Year <2073,]  # spei data doesn't start til 2023

# get spei ready to join to sediments
spei <- spei[spei$rcp=="rcp60",]
spei <- spei[c(1,5,8,9,11)]
# because we simplified the data from monthly to seasonal, we have duplicate rows (Dec., Jan. same season, have same spcat values)
spei <- distinct(spei) %>% # nrow should be 1/3 of what it was originally
           drop_na(spcat12mossn)

sum(is.na(spei))
sum(is.na(wdatssn.site))
unique(spei$year)

# merge spei and sediments
spsed <- left_join(wdatssn.site, spei, join_by(site_name==site, Year==year, season==season), 
                   relationship="many-to-one") 
sum(is.na(spsed))


# check
unique(spsed$spcat12mossn)

# print(aggregate(year~spcat12mossn + season + site, data=spei, FUN="length") %>%
#   group_by(spcat12mossn, season) %>%
#   summarize(mean(year)) %>%
#     arrange(season, spcat12mossn), n=42)
# mostly only 1 or no cases of exceptional or extreme drought or wet
# A tibble: 42 × 3
# Groups:   spcat12mossn [11]
# spcat12mossn        season `mean(year)`
# <chr>               <chr>         <dbl>
#   1 Abnormally Dry      Fall           6   
# 2 Abnormally Wet      Fall           4.38
# 3 Exceptional Drought Fall           1   
# 4 Exceptionally Wet   Fall           1   
# 5 Extreme Drought     Fall           1.33
# 6 Extremely Wet       Fall           2   
# 7 Moderate Drought    Fall           8.38
# 8 Moderately Wet      Fall           8.69
# 9 Normal              Fall          13.1 
# 10 Severe Drought      Fall           3.31
# 11 Severely Wet        Fall           2.62
# 12 Abnormally Dry      Spring         5.5 
# 13 Abnormally Wet      Spring         3.62
# 14 Exceptional Drought Spring         1   
# 15 Exceptionally Wet   Spring         1   
# 16 Extreme Drought     Spring         1.5 
# 17 Extremely Wet       Spring         1   
# 18 Moderate Drought    Spring         9.56
# 19 Moderately Wet      Spring         9.94
# 20 Normal              Spring        14.4 
# 21 Severe Drought      Spring         1.64
# 22 Severely Wet        Spring         2.5 
# 23 Abnormally Dry      Summer         6.31
# 24 Abnormally Wet      Summer         4.75
# 25 Exceptional Drought Summer         1   
# 26 Exceptionally Wet   Summer         1   
# 27 Extreme Drought     Summer         1.45
# 28 Extremely Wet       Summer         2.31
# 29 Moderate Drought    Summer         8.62
# 30 Moderately Wet      Summer         8.25
# 31 Normal              Summer        12.6 
# 32 Severe Drought      Summer         3   
# 33 Severely Wet        Summer         2.75
# 34 Abnormally Dry      Winter         4.44
# 35 Abnormally Wet      Winter         6.56
# 36 Extreme Drought     Winter         1.08
# 37 Extremely Wet       Winter         1   
# 38 Moderate Drought    Winter         5.81
# 39 Moderately Wet      Winter         5.38
# 40 Normal              Winter        21.9 
# 41 Severe Drought      Winter         2.23
# 42 Severely Wet        Winter         2.56

# test that data make same chart as in "CA_alm_water, sediments by simulation.R"
# doing this because chart below with sediments by SPEI cat for summer show strange results --
# low sediments in wet summmers.
check <- group_by(spsed, site_name, Year, till, cc, nfert) %>%
  summarize(sed.yr.tot = sum(sed.ssntot)) %>%
  group_by(cc) %>%
  summarize(sed.yr.tot = mean(sed.yr.tot)) %>% # mean annual sediments for 3 cover crops averaged across tillage, nfert, site, year
  mutate(sed.yr.tot.tac = sed.yr.tot/(2.471*907.1847))
# yes these values match the chart.
# plotting the below chart for Spring the data look more as expected. So perhaps
# the spei and sediment data are off for Summer because an abnormally wet summer
# is not actually all that wet compared to an abnormally wet spring?



# make a subset for the report
spsedrep <- filter(spsed, season== "Spring", 
                    spcat12mossn %in% c("Abnormally Dry", "Abnormally Wet", "Moderate Drought", "Normal", "Moderately Wet"),
                   !till=="RT") %>%
  drop_na(spcat12mossn)


  
# test for sig. differences

sedlm <- lm(sed.ssntot~cc:till:spcat12mossn, data=spsedrep)
summary(sedlm)

# Call:
#   lm(formula = sed.ssntot ~ cc:till:spcat12mossn, data = spsedrep)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1316.4  -675.2  -384.6    35.8 11573.0 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                839.66      66.31  12.663  < 2e-16 ***
#   ccLC:tillCT:spcat12mossnAbnormally Dry    -671.43     126.25  -5.318 1.08e-07 ***
#   ccNC:tillCT:spcat12mossnAbnormally Dry    -359.57     126.25  -2.848 0.004410 ** 
#   ccTC:tillCT:spcat12mossnAbnormally Dry    -443.57     126.25  -3.513 0.000445 ***
#   ccLC:tillNT:spcat12mossnAbnormally Dry    -713.34     126.25  -5.650 1.66e-08 ***
#   ccNC:tillNT:spcat12mossnAbnormally Dry    -363.42     126.25  -2.879 0.004005 ** 
#   ccTC:tillNT:spcat12mossnAbnormally Dry    -491.54     126.25  -3.893 9.97e-05 ***
#   ccLC:tillCT:spcat12mossnAbnormally Wet    -312.40     148.02  -2.111 0.034839 *  
#   ccNC:tillCT:spcat12mossnAbnormally Wet     341.28     148.02   2.306 0.021155 *  
#   ccTC:tillCT:spcat12mossnAbnormally Wet     192.14     148.02   1.298 0.194288    
# ccLC:tillNT:spcat12mossnAbnormally Wet    -508.88     148.02  -3.438 0.000589 ***
#   ccNC:tillNT:spcat12mossnAbnormally Wet     334.75     148.02   2.262 0.023752 *  
#   ccTC:tillNT:spcat12mossnAbnormally Wet      58.63     148.02   0.396 0.692056    
# ccLC:tillCT:spcat12mossnModerate Drought  -577.41     105.05  -5.497 3.99e-08 ***
#   ccNC:tillCT:spcat12mossnModerate Drought  -277.69     105.05  -2.643 0.008224 ** 
#   ccTC:tillCT:spcat12mossnModerate Drought  -387.49     105.05  -3.689 0.000227 ***
#   ccLC:tillNT:spcat12mossnModerate Drought  -689.47     105.05  -6.563 5.58e-11 ***
#   ccNC:tillNT:spcat12mossnModerate Drought  -281.20     105.05  -2.677 0.007448 ** 
#   ccTC:tillNT:spcat12mossnModerate Drought  -432.43     105.05  -4.116 3.89e-05 ***
#   ccLC:tillCT:spcat12mossnModerately Wet    -164.48     103.85  -1.584 0.113288    
# ccNC:tillCT:spcat12mossnModerately Wet     476.76     103.85   4.591 4.48e-06 ***
#   ccTC:tillCT:spcat12mossnModerately Wet     365.49     103.85   3.519 0.000435 ***
#   ccLC:tillNT:spcat12mossnModerately Wet    -288.94     103.85  -2.782 0.005410 ** 
#   ccNC:tillNT:spcat12mossnModerately Wet     468.33     103.85   4.510 6.58e-06 ***
#   ccTC:tillNT:spcat12mossnModerately Wet     226.25     103.85   2.179 0.029390 *  
#   ccLC:tillCT:spcat12mossnNormal            -290.89      93.78  -3.102 0.001929 ** 
#   ccNC:tillCT:spcat12mossnNormal             241.71      93.78   2.578 0.009968 ** 
#   ccTC:tillCT:spcat12mossnNormal             114.04      93.78   1.216 0.223999    
# ccLC:tillNT:spcat12mossnNormal            -489.82      93.78  -5.223 1.80e-07 ***
#   ccNC:tillNT:spcat12mossnNormal             234.94      93.78   2.505 0.012255 *  
#   ccTC:tillNT:spcat12mossnNormal                 NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1425 on 8238 degrees of freedom
# Multiple R-squared:  0.05996,	Adjusted R-squared:  0.05665 
# F-statistic: 18.12 on 29 and 8238 DF,  p-value: < 2.2e-16

sedaov <- aov(sed.ssntot~cc:till:spcat12mossn, data=spsedrep)
summary(sedaov)
# Df    Sum Sq  Mean Sq F value Pr(>F)    
# cc:till:spcat12mossn   29 1.067e+09 36805436   18.12 <2e-16 ***
#   Residuals            8238 1.673e+10  2031429                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukout <- TukeyHSD(sedaov)
cld <- multcompLetters4(sedaov, Tukout)
# table with letters
cld <- as.data.frame.list(cld$`cc:till:spcat12mossn`)

# get means and SEs summary  #season, nfert, and management-name are now moot
spsedrep.sum <-  
  dplyr::select(spsedrep, !c(sed.ssn.alley, -sed.ssn.row)) %>%
  group_by(spcat12mossn, till, cc) %>%  # get the mean sed loss across all sites, across all years of each spcat
  summarize(sed.ssnmn = mean(sed.ssntot),
            sed.ssnse = se(sed.ssntot))  %>%  
  arrange(desc(sed.ssnmn))

spsedrep.sum$cld <- cld$Letters

# convert kg/ha to lb per acre
spsedrep.sum$sed.ssnmn.tac <- spsedrep.sum$sed.ssnmn/(2.471*907.1847)
spsedrep.sum$sed.ssnse.tac <- spsedrep.sum$sed.ssnse/(2.471*907.1847)

print(arrange(spsedrep.sum, spcat12mossn, till, cc), n=nrow(spsedrep.sum))
# A tibble: 30 × 8
# Groups:   spcat12mossn, till [10]
# spcat12mossn     till  cc    sed.ssnmn sed.ssnse cld      sed.ssnmn.tac sed.ssnse.tac
# <chr>            <chr> <chr>     <dbl>     <dbl> <chr>            <dbl>         <dbl>
#   1 Abnormally Dry   CT    LC         168.      35.3 ij              0.0750        0.0157
# 2 Abnormally Dry   CT    NC         480.      78.6 fghij           0.214         0.0350
# 3 Abnormally Dry   CT    TC         396.      68.1 fghij           0.177         0.0304
# 4 Abnormally Dry   NT    LC         126.      24.8 ij              0.0564        0.0111
# 5 Abnormally Dry   NT    NC         476.      78.5 fghij           0.212         0.0350
# 6 Abnormally Dry   NT    TC         348.      62.8 ghij            0.155         0.0280
# 7 Abnormally Wet   CT    LC         527.      89.6 cdefghij        0.235         0.0400
# 8 Abnormally Wet   CT    NC        1181.     166.  abcd            0.527         0.0742
# 9 Abnormally Wet   CT    TC        1032.     150.  abcdef          0.460         0.0670
# 10 Abnormally Wet   NT    LC         331.      55.7 fghij           0.148         0.0248
# 11 Abnormally Wet   NT    NC        1174.     167.  abcd            0.524         0.0744
# 12 Abnormally Wet   NT    TC         898.     138.  abcdefg         0.401         0.0615
# 13 Moderate Drought CT    LC         262.      46.2 hij             0.117         0.0206
# 14 Moderate Drought CT    NC         562.      80.0 efghij          0.251         0.0357
# 15 Moderate Drought CT    TC         452.      61.6 fghij           0.202         0.0275
# 16 Moderate Drought NT    LC         150.      26.2 j               0.0670        0.0117
# 17 Moderate Drought NT    NC         558.      79.7 fghij           0.249         0.0355
# 18 Moderate Drought NT    TC         407.      55.8 ghij            0.182         0.0249
# 19 Moderately Wet   CT    LC         675.      73.8 defgh           0.301         0.0329
# 20 Moderately Wet   CT    NC        1316.     119.  a               0.587         0.0530
# 21 Moderately Wet   CT    TC        1205.     114.  ab              0.538         0.0509
# 22 Moderately Wet   NT    LC         551.      64.4 fghij           0.246         0.0288
# 23 Moderately Wet   NT    NC        1308.     119.  a               0.583         0.0530
# 24 Moderately Wet   NT    TC        1066.     104.  abcd            0.476         0.0464
# 25 Normal           CT    LC         549.      49.9 fghi            0.245         0.0222
# 26 Normal           CT    NC        1081.      86.4 abc             0.482         0.0385
# 27 Normal           CT    TC         954.      77.5 abcde           0.425         0.0346
# 28 Normal           NT    LC         350.      31.6 ghij            0.156         0.0141
# 29 Normal           NT    NC        1075.      86.3 abc             0.479         0.0385
# 30 Normal           NT    TC         840.      71.2 bcdef           0.375         0.0317
# moderately wet summer: CT NC - CT LC
(0.587-0.301)/0.587 # 49%
# mod wet: NT NC - NT LC
(0.583 - 0.246)/0.583 # 57%
mean(c(0.49, 0.57))  #0.53

####################################  sed ~ cc*till*spei plot
windows(xpinch=200, ypinch=200, width=5, height=5)


#### left off making this graph and wondering why the lowest sediment losses are
#### in abnormally wet years, then moderately wet years. That's when you'd expect to see
#### the greatest sediment losses.

# pal2 <- c("#6f3112","#dfb1a3")
pal2 <- c( "#669947", "#004a23")
pal5 <- c( "#f67e4b","#FEDa8B", "#eaeccc", "#C2e4ef","#6ea6cd")

ggplot(data=spsedrep.sum, aes(x=till, 
                           y=sed.ssnmn.tac, fill=till)) + # 
  geom_bar(stat="identity", position=position_dodge(), width=0.7) + 
  # geom_col_pattern(aes(pattern_density=till, fill=cc), pattern="stripe", alpha=0.7,
                   # pattern_fill="white", pattern_colour="white", color="white", #width=0.7,
                   # show.legend=F) +
  # scale_pattern_density_manual(values=c(0.05, 0)) +
  scale_fill_manual(values=pal2) +
  geom_errorbar(aes( ymin=sed.ssnmn.tac-sed.ssnse.tac, 
                     ymax=sed.ssnmn.tac + sed.ssnse.tac), 
                width=0.2, linewidth=0.8, color="#20243d",
                position=position_dodge(0.7)) +
  facet_grid2(rows=vars(factor(spcat12mossn, levels=c( "Moderate Drought", "Abnormally Dry", "Normal", "Abnormally Wet", "Moderately Wet"))), # rows=vars(factor(till, levels=c("CT", "NT")), factor(cc, levels=c("NC", "CC"))),
              cols=vars(factor(cc, levels=c("NC", "TC", "LC"))),
              labeller = as_labeller(
                c("Moderate Drought" = "Moderate\nDrought", 
                   "Abnormally Dry" = "Abnormally\nDry", 
                  "Normal" = "Normal" ,
                   "Abnormally Wet" = "Abnormally\nWet" , 
                  "Moderately Wet" = "Moderately\nWet",
               "NC"= "No cover","TC" = "Triticale cover", "LC" = "Legume cover")),
              # "CT" = "Conventional Till", "NT" = "No Till")), 
              strip = strip_themed(background_y = elem_list_rect(fill = pal5), #,  # cool stuff from ggh4x package!
                                   text_y = elem_list_text(col="black"))) +          #color= c("white", rep("black", 5), "white")))) +
  ylab("2022-2072 mean sediment yield (pounds per acre)") +
  xlab("Management") +
  geom_text(aes(y=sed.ssnmn.tac + 0.3, label=cld), size=5) + 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    # axis.text.x=element_blank(),
    # axis.ticks.x = element_blank(),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))

ggsave("plots/resilience/sediments/CA_alm_sed mean summer SPEI ccxtill.png", width=6, height=6, dpi=300) 














######################## sediment yield probabilities


norm20 <- speised[speised$year>2022 & speised$year<2030 & 
                    speised$spcat12mossn =="Normal",] # calculate the median using the seasonally determined SPEI category

norm20med <- median(norm20$sed.yr)
rm(norm20)


pal5 <- c( "#f67e4b","#FEDa8B", "#eaeccc", "#C2e4ef","#6ea6cd")

windows(xpinch=200, ypinch=200, width=5, height=5)



ggplot(data=speised[speised$crop=="corn" & speised$season == "Summer" &
                      !is.na(speised$spcat12mossn) &  # for now just compare extremes: CT+NC and NT+CC :
                      speised$management %in% c("ct-nc-cn", "ct-nc-sn","nt-cc-cn", "nt-cc-sn") &
                      #speised$spcat12mossn == "Abnormally Wet",],
                      !speised$spcat12mossn %in% c("Exceptional Drought", "Extreme Drought", "Severe Drought",
                                                   "Exceptionally Wet", "Extremely Wet", "Severely Wet"),],
       aes(x=sed.yr)) +
  geom_density(aes(y=after_stat(density)), # sets the y-axis units
               linewidth=1.3,    
               #group=interaction(spcat12mossn, cc, till)),
               trim=T) +   # make sure calculating the density per facet, not whole dataset at once
  labs(x="Sediment yield kg/ha", y="Frequency (%) 2022 to 2072") +
  scale_y_continuous(labels=scales::percent_format()) +
  geom_vline(xintercept=norm20med, color="#BBcc33", linewidth=1.3) +  # 
  facet_grid2(cols=vars(spcat12mossn), rows=vars(factor(cc, levels=c("NC", "CC")), 
                                                 factor(till, levels=c("CT", "NT"))), 
              # scales="free_y", 
              labeller = as_labeller(
                c("Moderate Drought" = "Moderate\nDrought",
                  "Abnormally Dry" = "Abnormally\nDry", "Normal" = "Normal",
                  "Abnormally Wet" = "Abnormally\nWet", 
                  "Moderately Wet" = "Moderately\nWet",
                  "NC"= "No cover crops","CT" = "Conventional till","CC" = "Has cover crops", 
                  "NT" = "No-till")),
              strip = strip_themed(background_x = elem_list_rect(fill = pal5),  # cool stuff from ggh4x package!
                                   text_x=elem_list_text(color="black"))) +
  #text_y = elem_list_text(color=c("white", rep("black", 9), "white")))) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-30, hjust=0.5, vjust=0, size=11),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=12))

# ggsave("plots/resilience/sediments/IL_corngrainc_bydrought_hist.png", width=4, height=10, dpi=300)
ggsave("plots/resilience/sediments/IL_sed_bydrought_density Summer.png", width=4, height=10, dpi=300)
# ggsave("plots/resilience/sediments/IL_sed_bydrought_density Summer_ABN WET only.png", width=3, height=4, dpi=300)




# Quantify areas under curves by site, SPEI, and treatments

lsd <- filter(speised, season== "Summer", crop=="corn", 
              !spcat12mossn %in% c("Exceptional Drought", "Extreme Drought", "Severe Drought",
                                   "Exceptionally Wet", "Extremely Wet", "Severely Wet")) %>%
  group_split(site, spcat12mossn, till, cc, nfert) # crop

# ecdf = empirical cumulative distribution function
lecdf <- lapply(lsd, function(i){
  d_fun <- ecdf(i$sed.yr)
  sedmed <- 1 - d_fun(norm20med) # 1- gives the area under the curve to the RIGHT of the value
  out <- i[1, c("site", "till", "cc", "nfert", "spcat12mossn")]
  out$sedmed <- sedmed
  out
})

lecdf <- lecdf %>%
  bind_rows(lecdf) %>%
  mutate(cctill=paste0(cc, ".", till))

unique(lecdf$spcat12mossn)

rm(lsd)

# are the areas under the curves different?


################## models: are the areas under the curves different?


lecdf$spcat12mossn <- factor(lecdf$spcat12mossn,
                             levels=c("Moderate Drought", 
                                      "Abnormally Dry", "Normal" ,
                                      "Abnormally Wet" , "Moderately Wet"),
                             ordered=F)


##### letters for nt-cc, ct-nc x spei plot

lecdf_simp <-  filter(lecdf,  #(till %in% c("CT", "NT"))) 
                    cctill %in% c("CC.NT", "NC.CT", "CC.CT", "NC.NT"),
                    spcat12mossn %in% c("Moderate Drought", "Normal", "Moderately Wet"),
                    nfert %in% c("Recommended N")) %>%
               drop_na(spcat12mossn) 
                    

sedaov_simp <- aov(sedmed~cctill*spcat12mossn,   # since we only have CC+NT vs NC+CT we've made till and cc redundant
                   data=lecdf_simp)

summary(sedaov_simp)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# cctill                3  42.08  14.028 210.790  < 2e-16 ***
#   spcat12mossn          2   3.74   1.869  28.078 1.73e-12 ***
#   cctill:spcat12mossn   6   1.54   0.256   3.853 0.000854 ***
#   Residuals           756  50.31   0.067                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukout_simp <- TukeyHSD(sedaov_simp)
cld_simp <- multcompLetters4(sedaov_simp, Tukout_simp)
# table with letters and 3rd quantile
lecdf_sum<-   filter(lecdf,   # till %in% c("CT", "NT")) %>%
      cctill %in% c("CC.NT", "NC.CT", "CC.CT", "NC.NT"),
      spcat12mossn %in% c("Moderate Drought", "Normal", "Moderately Wet"),
      nfert %in% c("Recommended N")) %>%
  drop_na(spcat12mossn) %>%
  group_by( cctill, cc, till, spcat12mossn) %>%  # 
  summarize(mean=mean(sedmed), 
            se=se(sedmed)) %>%
  arrange(desc(mean))

cld_lecdf <- as.data.frame.list(cld_simp$`cctill:spcat12mossn`)
lecdf_sum$cld <- cld_lecdf$Letters


# 
####################################  sed ~ cc*till*spei plot
windows(xpinch=200, ypinch=200, width=5, height=5)

lecdf_sum$yr <- rep("X", nrow(lecdf_sum))

# pal2 <- c("#6f3112","#dfb1a3")
pal2 <- c("#004a23", "#669947")

ggplot(data=lecdf_sum, aes(x=factor(cctill, levels=c("NC.CT", "NC.NT", "CC.CT", "CC.NT")), 
                           y=mean)) +  # yr is a dummy var
  geom_col_pattern(aes(pattern_density=till, fill=cc), pattern="stripe", alpha=0.7,
                   pattern_fill="white", pattern_colour="white", color="white", #width=0.7,
                   show.legend=F) +
  scale_pattern_density_manual(values=c(0.05, 0)) +
  scale_fill_manual(values=pal2) +
  geom_errorbar(aes( ymin=mean-se, ymax=mean+se), width=0.2, size=0.8, color="#20243d") +
  facet_grid2(rows=vars(spcat12mossn), # rows=vars(factor(till, levels=c("CT", "NT")), factor(cc, levels=c("NC", "CC"))),
              labeller = as_labeller(
                c("Moderate Drought" = "Moderate\nDrought", 
                  # "Abnormally Dry" = "Abnormally\nDry", 
                  "Normal" = "Normal" ,
                  # "Abnormally Wet" = "Abnormally\nWet" , 
                  "Moderately Wet" = "Moderately\nWet")),
                  # "NC"= "Without cover crops","CC" = "With cover crops",
                  # "CT" = "Conventional Till", "NT" = "No Till")), 
              strip = strip_themed(background_y = elem_list_rect(fill = pal5[c(2:4)]), #,  # cool stuff from ggh4x package!
                                   text_y = elem_list_text(col="black"))) +          #color= c("white", rep("black", 5), "white")))) +
  ylab("Mean probability sediment yield\n> median in Normal year in 2020s") +
  xlab("Management") +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  geom_text(aes(y=mean +0.15, label=cld), size=5) + 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    # axis.text.x=element_blank(),
    # axis.ticks.x = element_blank(),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))

ggsave("plots/resilience/sediments/IL_sed prob summer SPEI ccxtill withletters simpler.png", width=4.5, height=6, dpi=300) 






















# assumptions - following https://statsandr.com/blog/anova-in-r

# Independence - the observations (grain.c.kgc.ha. observations) are independent, one does
# not depend on the other, they're not dependent on some other variable like, 
# from one individual came multiple observations.

# Normality - not required with large enough sample size >30.  But we can test anyway. 
# the residuals (observed - mean for that group) should be normally distributed.
qqnorm(corndf$grmed)
qqline(corndf$grmed)
hist(corndf$grmed-mean(corndf$grmed))
hist(corndf$grmed)

# Equality of variances 
ggplot(data=corndf, aes(x=interaction(till, cc, spcat12mo), y=grmed)) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))
# an interesting and not so pretty graph
car::leveneTest(grmed ~ cc*till*spcat12mo, data=corndf)
# Levene's Test for Homogeneity of Variance (center = median)
#          Df F value    Pr(>F)    
# group    65    15.2 < 2.2e-16 ***
#       12606                      
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
summary(corndf$grmed)
# > summary(corndat$Grain.C.kgC.ha.)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.3636  0.5625  0.5469  0.7692  1.0000 

# outliers via histograms
hist(corndf$grmed, breaks=sqrt(nrow(corndf)))

ggplot(data=corndf, aes(x=grmed)) +
  geom_histogram() +
  facet_grid(rows=vars(spcat12mo), cols=vars(till, cc))

#outliers via boxplots
boxplot(grmed~till*spcat12mo, data=corndf) # 

# outliers via z-scores (not sure if this works for data that are already between 0-1)
corndf$grmedz <- scale(corndf$grmed)
hist(corndf$grmedz)
summary(corndf$grmedz)
# V1          
# Min.   :-1.99416  
# 1st Qu.:-0.66826  
# Median : 0.05684  
# Mean   : 0.00000  
# 3rd Qu.: 0.81063  
# Max.   : 1.65206
# < -2 or >2 is considered rare
# < -3 or >3 is extremely rare
# < -3.29 or > -3.29 is used to detect outliers, where one out of 1000 observations will be outside
# we're good here

## outlier summary: boxplots and z-score suggest no extreme outliers. No justification for removing.

# re-run ANOVA - are we accounting for the 0-1 distribution properly?

########### left off here


cornaov_full <- aov(grmed~till*cc*nfert*spcat12mo, data=corndf)
summary(cornaov_full)

# put interaction output into a dataframe we can sort
Tukout_full <- as.data.frame(Tukout_full[5]) %>%  # 5 is the list item for the interaction that looks significant from the summary above
  rownames_to_column(., "term") %>%
  arrange(term)