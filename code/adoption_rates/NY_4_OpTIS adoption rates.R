# OpTIS adoption rate information for Illinois and New York aggregated by counties 2015-2021.
# Data shared with AFT from Regrow analysis.
# Column "kpi_name" gives different types of land use areas, with "area" meaning the 
# analyzed acres of the county, tillage type/ cover crop / etc.
# units are ACRES

# load packages
library(reshape2) # for dcast()
library(ggplot2)
library(dplyr)


# to update data, run "download data.R"


# standard error
se <- function(x){sd(x)/(sqrt(length(x)))}

# load the data
datny <- read.csv("data/optis/datny_cleanlong.csv")




# what are the state average adoption rates by year and crop
means_cropyear <- datny %>% 
                group_by(year, crop_name, variable) %>%
                summarize(Mean = mean(value), se=se(value))
                          

# # what are the state average adoption rates by year (across all crops)
means_year <- datny%>%
              group_by(year, variable) %>%
              summarize(Mean = mean(value))

# what are the state average adoption rates across all available years (2015-2021) per crop?
means_crop <- datny%>%
                    group_by(crop_name, variable) %>%
                    summarize(Mean = mean(value), se=se(value))

# what are the state average adoption rates across MOST RECENT 4 YEARS 2018-2021?  
# note results not all that different between means_crop and means_crop_1821, means_crop_1821 a little higher in some cases
# for now, will report most recent data:
means_crop_1821 <- datny%>%
              filter(year>2017) %>%
              group_by(crop_name, variable) %>%
              summarize(Mean = mean(value), se=se(value))

means_1821 <- datny%>%  # across all acrops
  filter(year>2017) %>%
  group_by(variable) %>%
  summarize(Mean = mean(value), se=se(value))

means_1821[means_1821$variable=="perc_nt", "Mean"] + means_1821[means_1821$variable=="perc_rt", "Mean"] #77.6
(means_1821[means_1821$variable=="perc_nt", "se"] + means_1821[means_1821$variable=="perc_rt", "se"])/2 # 1.14


# what are the state average adoption rates across all years and crops
means_global <- datny%>%
                group_by(variable) %>%
                summarize(Mean = mean(value), se = se(value))


# plots for mean adoption rates

windows(xpinch=200, ypinch=200, width=5, height=5)


# State mean change over time
# facet by crop

pal4 <- c("#999933", "#332288", "#88ccee",  "#cc6677")


ggplot(data=means_cropyear, aes(x=year, y=Mean, group=variable)) +
  geom_line(aes(color=variable), linewidth=0.4) +
  geom_point(aes(color=variable), size=0.5) +
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se, color=variable), width=0.1, linewidth=0.6) +
  scale_color_manual(values=pal4, 
                     name="Practice",
                     breaks=c("perc_cc", "perc_nt", "perc_rt", "perc_ct"),
                     labels=c("Cover Crops", "No Till", "Reduced Till", "Conventional Till")) +
  facet_grid(rows=vars(crop_name)) + 
  scale_x_continuous(breaks=seq(2015,2021, 1), name="Year") +
  ylab("State Mean Percent Adoption") +
  theme(
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank() ,
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=14, face="bold"),
        axis.title.y=element_text(size=14, face="bold"),
        panel.background = element_rect(fill = 'white') ,
        panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
        strip.text=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        legend.title=element_text(size=12, face="bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.key=element_rect(fill="white"),
        legend.key.size = unit(0.4, "cm")
  )	 

ggsave("plots/state_rates_15-21_NY.png", width=6, height=4, dpi=300)


library(lme4)
# rate of change - all practices together
summary(lm(value~year*variable + county + crop_name, data=datny))
mem <- lmer(value~year*variable + crop_name + (1|county), data=datny)
isSingular(mem)
summary(lm(value~year, data=datny))
summary(lm(value~year*variable, data=datny))
summary(lm(value~year + crop_name, data=datny))
summary(lm(value~year + county, data=datny))
summary(lm(value~year*variable + crop_name, data=datny)) # no improvement with crop_name
summary(lm(value~year*variable + county, data=datny)) # no improvement with county

# rate of change - cover crops
summary(lm(value~year, data=datny[datny$variable %in% "perc_cc",])) # R2 only 0.04, p is sig., slope=0.71
summary(lm(value~year + county, data=datny[datny$variable %in% "perc_cc",])) # improves R2 to 0.45, slope=0.49
# Call:
#   lm(formula = value ~ year + county, data = datny[datny$variable %in% 
#                                                      "perc_cc", ])
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -12.4227  -3.0766  -0.6906   2.4074  17.7418 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -941.9543   295.1986  -3.191 0.001574 ** 
#   year                 0.4861     0.1461   3.327 0.000991 ***
#   countyAllegany     -35.2726     5.5853  -6.315 1.01e-09 ***
#   countyBroome       -30.8253     5.6398  -5.466 9.95e-08 ***
#   countyCattaraugus  -34.4396     5.4400  -6.331 9.25e-10 ***
#   countyCayuga       -29.1709     5.4582  -5.344 1.84e-07 ***
#   countyChautauqua   -32.8586     5.4809  -5.995 6.03e-09 ***
#   countyChemung      -31.2783     5.5417  -5.644 3.95e-08 ***
#   countyChenango     -29.8164     5.7212  -5.212 3.56e-07 ***
#   countyClinton      -36.7966     6.0539  -6.078 3.82e-09 ***
#   countyColumbia     -28.0567     5.4781  -5.122 5.53e-07 ***
#   countyCortland     -31.7973     5.4809  -5.802 1.72e-08 ***
#   countyDelaware     -12.9795     6.0258  -2.154 0.032067 *  
#   countyDutchess     -28.9079     5.6407  -5.125 5.45e-07 ***
#   countyErie         -33.2876     5.4094  -6.154 2.51e-09 ***
#   countyFranklin     -36.3657     6.0258  -6.035 4.85e-09 ***
#   countyFulton       -36.7273     6.0327  -6.088 3.62e-09 ***
#   countyGenesee      -33.3265     5.4575  -6.107 3.26e-09 ***
#   countyHerkimer     -35.4562     5.5374  -6.403 6.12e-10 ***
#   countyJefferson    -30.7542     5.4094  -5.685 3.18e-08 ***
#   countyLewis        -32.0727     5.5068  -5.824 1.52e-08 ***
#   countyLivingston   -35.8876     5.4094  -6.634 1.60e-10 ***
#   countyMadison      -33.0956     5.4094  -6.118 3.06e-09 ***
#   countyMontgomery   -32.5864     5.5393  -5.883 1.11e-08 ***
#   countyOneida       -33.9192     5.4400  -6.235 1.59e-09 ***
#   countyOnondaga     -29.1148     5.4094  -5.382 1.52e-07 ***
#   countyOntario      -34.5744     5.4232  -6.375 7.18e-10 ***
#   countyOrange       -31.1571     5.5379  -5.626 4.34e-08 ***
#   countyOswego       -33.4570     5.5400  -6.039 4.74e-09 ***
#   countyOtsego       -36.4511     5.5386  -6.581 2.18e-10 ***
#   countyRensselaer   -24.0225     5.5812  -4.304 2.29e-05 ***
#   countySaratoga     -25.4981     5.5863  -4.564 7.41e-06 ***
#   countySchoharie    -31.3813     5.7255  -5.481 9.20e-08 ***
#   countySchuyler     -37.8028     6.3916  -5.914 9.36e-09 ***
#   countySeneca       -37.2775     7.3813  -5.050 7.81e-07 ***
#   countySteuben      -36.1421     5.4569  -6.623 1.70e-10 ***
#   countySuffolk        4.3744     6.3912   0.684 0.494237    
# countySullivan     -24.6540     6.0327  -4.087 5.67e-05 ***
#   countyTioga        -32.5996     5.5874  -5.834 1.44e-08 ***
#   countyTompkins     -31.7821     5.4239  -5.860 1.26e-08 ***
#   countyUlster       -32.5760     6.3949  -5.094 6.32e-07 ***
#   countyWashington   -22.0530     5.6407  -3.910 0.000115 ***
#   countyWyoming      -33.3143     5.4094  -6.159 2.44e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.218 on 290 degrees of freedom
# Multiple R-squared:  0.5229,	Adjusted R-squared:  0.4538 
# F-statistic: 7.567 on 42 and 290 DF,  p-value: < 2.2e-16

# slope means that cc adoption is increasing by 0.5% per year (percent data are in %, not decimal, so 0.5 is not 50% it's 0.5%). trend is significant (i.e. not zero)
# 0.48% +/-0.14
# which looks about right given the plot above.
# at this rate we will reach 50% adoption in year ___?
# according to means_1821 NY cover crop adoption is at 8.6% (+/- 0.6).
# increasing by 0.5% per year
(50-8.6)/0.5  # 82.8 years to get to 50%.
2024 + 83 # 2066

# rate of change - no till
# summary(lm(value~year, data=datny[datny$variable %in% "perc_nt",])) # R2 only 0.02, p is sig., slope=-1.53
summary(lm(value~year + county, data=datny[datny$variable %in% "perc_nt",])) # improves R2 to 0.5, slope=-1.53

# Call:
#   lm(formula = value ~ year + county, data = datny[datny$variable %in% 
#                                                      "perc_nt", ])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -33.511  -6.999  -0.606   7.260  38.771 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       1646.9891   703.7547   2.340 0.019992 *  
#   year                -0.8051     0.3487  -2.309 0.021715 *  
#   countyBroome         1.1276     6.2622   0.180 0.857237    
# countyCattaraugus   -0.3889     5.1902  -0.075 0.940319    
# countyCayuga        17.8050     5.2960   3.362 0.000885 ***
#   countyChautauqua     3.0314     5.2959   0.572 0.567521    
# countyChemung        2.3739     5.5697   0.426 0.670288    
# countyChenango       0.5856     6.6396   0.088 0.929785    
# countyCortland       3.6937     5.2960   0.697 0.486118    
# countyDelaware      -4.5468     7.9975  -0.569 0.570144    
# countyErie          12.6201     5.0186   2.515 0.012491 *  
#   countyFranklin     -19.3569     9.4259  -2.054 0.040972 *  
#   countyFulton       -15.9407     6.6499  -2.397 0.017198 *  
#   countyGenesee        6.9850     5.5714   1.254 0.211012    
# countyHerkimer       0.2275     5.2959   0.043 0.965760    
# countyJefferson     -4.6339     5.0186  -0.923 0.356644    
# countyLewis         -3.6812     5.1900  -0.709 0.478756    
# countyLivingston    11.0066     5.1904   2.121 0.034864 *  
#   countyMadison       -4.3646     5.0186  -0.870 0.385233    
# countyMontgomery   -12.6397     5.7511  -2.198 0.028805 *  
#   countyOneida        -4.0162     5.0186  -0.800 0.424260    
# countyOnondaga      17.1861     5.0186   3.424 0.000711 ***
#   countyOntario       11.5565     5.0984   2.267 0.024194 *  
#   countyOrange       -14.3207     5.7538  -2.489 0.013412 *  
#   countyOswego        16.8290     5.5726   3.020 0.002768 ** 
#   countyOtsego         6.8866     5.4216   1.270 0.205090    
# countySchoharie    -19.3436     6.6396  -2.913 0.003873 ** 
#   countySchuyler      21.0471     9.4014   2.239 0.025983 *  
#   countySeneca        23.6785    12.7525   1.857 0.064424 .  
# countySteuben        0.3441     5.0986   0.067 0.946247    
# countySullivan      -8.0788     7.1743  -1.126 0.261125    
# countyTioga          9.4260     5.5702   1.692 0.091750 .  
# countyTompkins      11.8265     5.0986   2.320 0.021106 *  
#   countyWyoming       15.1043     5.1904   2.910 0.003913 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 12.12 on 272 degrees of freedom
# Multiple R-squared:  0.4188,	Adjusted R-squared:  0.3483 
# F-statistic:  5.94 on 33 and 272 DF,  p-value: < 2.2e-16



# slope means that nt adoption is DECREASING on average by 0.80% (+/- 0.3) per year.  trend is significant (i.e. not zero)


# rate of change - reduced till
# summary(lm(value~year, data=datny[datny$variable %in% "perc_rt",])) # R2 only 0.02, p is sig., slope=1.21
summary(lm(value~year + county, data=datny[datny$variable %in% "perc_rt",])) # improves R2 to 0.49, slope=1.7

# Call:
#   lm(formula = value ~ year + county, data = datny[datny$variable %in% 
#                                                      "perc_rt", ])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -41.003  -6.992   0.367   6.442  41.410 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -3376.4233   740.0735  -4.562 7.64e-06 ***
#   year                  1.6997     0.3667   4.635 5.53e-06 ***
#   countyBroome         -6.1443     6.6091  -0.930  0.35336    
# countyCattaraugus    -3.6518     5.4776  -0.667  0.50554    
# countyCayuga         -4.9026     5.5894  -0.877  0.38119    
# countyChautauqua     -1.0075     5.5893  -0.180  0.85708    
# countyChemung         2.0080     5.8782   0.342  0.73291    
# countyChenango       -1.2711     7.0074  -0.181  0.85619    
# countyCortland       -0.2168     5.5893  -0.039  0.96908    
# countyDelaware        4.8990     8.4403   0.580  0.56210    
# countyErie          -15.4210     5.2965  -2.912  0.00389 ** 
#   countyFranklin      -52.2401     8.4605  -6.175 2.38e-09 ***
#   countyFulton        -31.1836     6.6077  -4.719 3.78e-06 ***
#   countyGenesee        -2.5290     5.8799  -0.430  0.66745    
# countyHerkimer       11.0095     5.5893   1.970  0.04987 *  
#   countyJefferson      11.2028     5.2965   2.115  0.03532 *  
#   countyLewis          10.7820     5.5916   1.928  0.05486 .  
# countyLivingston     -5.2799     5.4779  -0.964  0.33597    
# countyMadison         1.2139     5.2965   0.229  0.81889    
# countyMontgomery    -17.6275     6.0696  -2.904  0.00398 ** 
#   countyOneida          2.4093     5.2965   0.455  0.64956    
# countyOnondaga       -4.3035     5.2965  -0.813  0.41721    
# countyOntario        -6.7629     5.3807  -1.257  0.20987    
# countyOrange        -46.7001     5.8789  -7.944 5.11e-14 ***
#   countyOswego         -6.1544     5.8812  -1.046  0.29628    
# countyOtsego          1.4047     5.7218   0.245  0.80626    
# countySchoharie     -44.2886     7.0074  -6.320 1.05e-09 ***
#   countySchuyler      -14.9361     9.9220  -1.505  0.13338    
# countySeneca        -19.5591    13.4585  -1.453  0.14729    
# countySteuben        -3.9124     5.3810  -0.727  0.46781    
# countySullivan       12.4616     7.5716   1.646  0.10095    
# countyTioga          -7.1891     5.8787  -1.223  0.22242    
# countyTompkins       -2.2861     5.3810  -0.425  0.67128    
# countyWyoming        -1.0618     5.4779  -0.194  0.84645    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 12.79 on 274 degrees of freedom
# Multiple R-squared:  0.5478,	Adjusted R-squared:  0.4933 
# F-statistic: 10.06 on 33 and 274 DF,  p-value: < 2.2e-16

# slope means that rt adoption is increasing on average by 1.7% (+/-0.4) per year.  trend is significant (i.e. not zero)
# which looks about right given the plot above.
# at this rate we will reach 50% adoption in year ___?

# according to means_1821 rt adoption is at 51.7 (+/- 1.2)
# increasing by 1.7% per year


# rate of change - conventional till
summary(lm(value~year + county, data=datny[datny$variable %in% "perc_ct",])) # improves R2 to 0.30, slope=1.12

# Call:
#   lm(formula = value ~ year + county, data = datny[datny$variable %in% 
#                                                      "perc_ct", ])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -15.033  -3.827  -1.008   2.903  25.249 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -2251.9374   392.1329  -5.743 2.60e-08 ***
#   year                  1.1228     0.1943   5.777 2.17e-08 ***
#   countyBroome          7.3783     3.6840   2.003  0.04624 *  
#   countyCattaraugus    -1.0363     3.1455  -0.329  0.74209    
# countyCayuga         -6.9427     3.1980  -2.171  0.03084 *  
#   countyChautauqua      1.3824     3.1983   0.432  0.66595    
# countyChemung         4.4590     3.3350   1.337  0.18238    
# countyChenango       -6.3920     3.8747  -1.650  0.10022    
# countyCortland       -2.7889     3.1988  -0.872  0.38410    
# countyDelaware       -1.5170     4.5794  -0.331  0.74071    
# countyErie           -6.8526     3.0619  -2.238  0.02607 *  
#   countyFranklin      -14.7930     4.5919  -3.222  0.00144 ** 
#   countyFulton        -10.0340     5.3043  -1.892  0.05965 .  
# countyGenesee        -2.3228     3.3368  -0.696  0.48697    
# countyHerkimer       -6.8874     3.2618  -2.112  0.03568 *  
#   countyJefferson       1.6784     3.0619   0.548  0.58405    
# countyLewis          -1.2061     3.2016  -0.377  0.70668    
# countyLivingston     -2.6337     3.1476  -0.837  0.40350    
# countyMadison        -4.9914     3.0619  -1.630  0.10428    
# countyMontgomery    -10.0835     3.4261  -2.943  0.00354 ** 
#   countyOneida         -4.2883     3.0619  -1.401  0.16254    
# countyOnondaga       -6.9712     3.0619  -2.277  0.02362 *  
#   countyOntario        -4.3600     3.1009  -1.406  0.16091    
# countyOrange        -12.3236     4.1479  -2.971  0.00325 ** 
#   countyOswego         -6.4035     3.3379  -1.918  0.05616 .  
# countyOtsego         -7.2654     3.2618  -2.227  0.02678 *  
#   countySchoharie     -11.3261     4.1479  -2.731  0.00676 ** 
#   countySchuyler        0.1892     5.3138   0.036  0.97162    
# countySeneca         -3.4476     7.0987  -0.486  0.62761    
# countySteuben         1.7405     3.1006   0.561  0.57505    
# countySullivan        2.1254     4.1500   0.512  0.60899    
# countyTioga           1.0671     3.3328   0.320  0.74910    
# countyTompkins       -3.0532     3.1006  -0.985  0.32569    
# countyWyoming        -9.7692     3.1993  -3.054  0.00250 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.613 on 259 degrees of freedom
# Multiple R-squared:  0.3783,	Adjusted R-squared:  0.2991 
# F-statistic: 4.777 on 33 and 259 DF,  p-value: 1.785e-13



# slope means that ct adoption is increasing on average by 1.1% (+/-0.2) per year.  trend is significant (i.e. not zero)
# which looks about right given the plot above.
# at this rate we will reach 50% adoption in year ___?
# according to means_global NY conventional till adoption is at 15.3% (+/-0.36).
# According to means_1821 conventional till adoption is at 17.0% (+/- 0.50)
# increasing by 0.78% per year
(50-17)/0.78  # 42 years to get to 50% adoption
2023+42 # 2065














# which counties have at least 5 years of consecutive measurements?
# work with a subset of data
subs <- datw[,c(1,2,5,4)]
# 
subs <- subs %>%
  group_by(state, county, crop_name) %>%
  mutate(con_yrs = cumsum(c(1, diff(year)>1))) %>%  # make col that is "1" for consecutive years, 2 if non-consecutive years
  group_by(state, county, crop_name, con_yrs) %>% # group by new column
  mutate(start = min(year), end = max(year), diff=end - start) # list first and last year of consecutive groups

# join back together with rest of data
datw2 <- left_join(datw, subs) %>%
  filter(diff>4) # only keep county-crop-years with at least 5 years consecutive data 2015-2021
# removes 102 county-crop-years
ggplot(data=datw2, aes(y=perc_cc, x=year, group=county)) +
  geom_line(aes(color=county), show.legend=F) + #, position="jitter", size=0.5) +
  facet_grid(rows=vars(crop_name), cols=vars(state))
unique(datw2$diff)

sum(is.na(datw$perc_cc))
sum(is.na(datw$acres))
sum(is.na(datw$cc.acres))
test <- datw[rowSums(is.na(datw))>0,]
# turns out that we create NAs for counties where certain kpi_names are not measured
# for that county that year that crop
# so need to do the subs code above for each kpi_name