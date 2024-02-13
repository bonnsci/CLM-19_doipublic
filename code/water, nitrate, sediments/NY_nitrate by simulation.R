
library(tidyverse) # has ggplot, dplyr, etc.

library(reshape2)

se <- function(x) sd(x) / sqrt(length(x))

# note about the data
# crop_system_name - is soy-corn or corn-soy.
# For the crop name "corn-soy" that means corn was planted first and corn is 
# present in every odd year and soy is planted every even year, whereas in the 
# "soy-corn" simulations, soy was planted in odd years and corn was planted in 
# even years. 



# if you only need annual totals skip to ndatyr below

# # if you need daily estimates use this:
# # data in the folder data/large_data/ are too big to share in github repo
# # only saved to Bonnie's computer (backed up by onedrive)
# ndat <- read.csv("data/large_data/daily N/NY_forage_day_soil_n.csv")
# beepr::beep(sound=8)
# # N UNITS are in kg N / ha per day
# 
# # sum data by year
# ndatyr <- ndat %>%
#   group_by(site_name, crop_system_name, management_name, climate_scenario, Year) %>%
#   summarize(N2O.yr = sum(N2O.flux), 
#             NO3.yr = sum(NO3.leach), 
#             Nleachtot = sum(NO3.leach) + sum(Urea.leach) + sum(DON.leach))
# #
# # # clean up
# rm(ndat)
# 
# colnames(ndatyr)[c(1,3,5)] <- c("site", "management", "year")
# 
# write.csv(ndatyr, "data/water, nitrate, sediments/NY_nitrate_annualtotals.csv", row.names=F)

ndatyr <- read.csv("data/water, nitrate, sediments/NY_nitrate_annualtotals.csv")


ndatyr <- ndatyr[ndatyr$year>2021 & ndatyr$year<2073 & ndatyr$climate_scenario=="rcp60",]

ndatyr$till <- ifelse(grepl("ct-", ndatyr$management), "CT", 
                      ifelse(grepl("rt-", ndatyr$management), "RT", "NT"))
# # check
# unique(ndatyr$till)

# factor for CC or NC
ndatyr$cc <- ifelse(grepl("-cc", ndatyr$management), "CC", "NC")
# # check
# unique(ndatyr$cc)

# factor for N treatment -- for NY this is redundant with crop


# factor for decade
ndatyr$decade <- ifelse(ndatyr$year <2031, "2020s",
                        ifelse(ndatyr$year>=2031 & ndatyr$year <2041, "2030s",
                               ifelse(ndatyr$year>=2041 & ndatyr$year <2051, "2040s",
                                      ifelse(ndatyr$year>=2051 & ndatyr$year <2061, "2050s",
                                             ifelse(ndatyr$year>=2061 & ndatyr$year <2071, "2060s", "2070s")))))
# unique(ndatyr$decade)


# because crop_system_name - is soy-corn or corn-soy.
# For the crop name "corn-soy" that means corn was planted first and corn is 
# present in every odd year and soy is planted every even year, whereas in the 
# "soy-corn" simulations, soy was planted in odd years and corn was planted in 
# even years. 
# if we want to know NO3 loss from corn per year or soy per year, need to split up data by odd and even years.



actcorn <- seq(2017,2072, 5)
ctacorn <- seq(2013, 2072, 5)
taccorn <- seq(2018, 2072, 5)

actalf <- sort(c(seq(2014,2072, 5), seq(2015,2072,5), seq(2016,2072,5)))
ctaalf <- sort(c(seq(2015,2072, 5), seq(2016,2072,5), seq(2017,2072,5)))
tacalf <- sort(c(seq(2015,2072, 5), seq(2016,2072,5), seq(2017,2072,5)))

acttri <- seq(2018,2072, 5)
ctatri <- seq(2019, 2072, 5)
tactri <- seq(2019, 2072, 5)


# label data for the crop they are (depends on the rotation and the year)
ndatyr$crop <- ifelse(ndatyr$crop=="corn-soy" & ndatyr$year%%2 ==0, "soy cs",   # %%2 returns the remainder when divided by 2. if no remainder, then its an even number.
                      ifelse(ndatyr$crop=="corn-soy" & !ndatyr$year%%2 ==0, "corn grain cs",  # cs= to indicate corn-soy rotation
                             ifelse(ndatyr$crop=="soy-corn" & ndatyr$year%%2 ==0, "corn grain cs",
                                    ifelse(ndatyr$crop=="soy-corn" & !ndatyr$year%%2 ==0, "soy cs", 
                                           ifelse(ndatyr$crop=="corn-grain", "corn grain mono",
                                                  ifelse(ndatyr$crop=="corn-silage", "corn silage mono",
                                                         # corn in alfalfa rotation
                                                         ifelse(ndatyr$crop=="alfalfa-act" & ndatyr$year %in% actcorn, "corn alf",
                                                                ifelse(ndatyr$crop=="alfalfa-cta" & ndatyr$year %in% ctacorn, "corn alf",
                                                                       ifelse(ndatyr$crop=="alfalfa-tac" & ndatyr$year %in% taccorn, "corn alf",
                                                                              # alf in alf rotation
                                                                              ifelse(ndatyr$crop=="alfalfa-act" & ndatyr$year %in% actalf, "alf",
                                                                                     ifelse(ndatyr$crop=="alfalfa-cta" & ndatyr$year %in% ctaalf, "alf",
                                                                                            ifelse(ndatyr$crop=="alfalfa-tac" & ndatyr$year %in% tacalf, "alf",
                                                                                                   # triticale in alf rotation
                                                                                                   ifelse(ndatyr$crop=="alfalfa-act" & ndatyr$year %in% acttri, "tri alf",
                                                                                                          ifelse(ndatyr$crop=="alfalfa-cta" & ndatyr$year %in% ctatri, "tri alf",
                                                                                                                 ifelse(ndatyr$crop=="alfalfa-tac" & ndatyr$year %in% tactri, "tri alf", "X")))))))))))))))



# check no Xs
# unique(ndatyr$crop)
rm(actalf, actcorn,acttri,ctaalf, ctacorn, ctatri, tacalf, taccorn, tactri)












# TOTALS BY DECADE - doesn't really work b/c 2020s and 2070s have fewer years
# could do by 10 year intervals in the data if necessary
# but I think annual means are where its at anyway

# ANNUAL MEANS BY DECADE by crop
ndat_dec <- ndatyr %>%
  group_by(crop, till, cc, decade) %>%  # drop sitename and crop system name to get means across sites and rotations
  summarize(NO3_mean = mean(NO3.yr),
            NO3_se = se(NO3.yr)) %>%
  pivot_longer(cols=-c("crop", "till", "cc", "decade"),
             names_to=c("component", ".value"),
             names_sep="_")  %>%
  arrange(desc(mean))






############################### N LOSSES 50 year average


### get letters for the bars

aovno3 <- aov(NO3.yr ~ till:cc:crop, data=ndatyr)
lmno3 <- lm(NO3.yr~ till:cc:crop, data=ndatyr)
summary(lmno3)
# Call:
#   lm(formula = NO3.yr ~ till:cc:crop, data = ndatyr)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -44.091  -8.374  -0.740   7.492  85.502 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       16.7741     0.6703  25.026  < 2e-16 ***
#   tillCT:ccCC:cropalf               16.1989     0.7718  20.988  < 2e-16 ***
#   tillNT:ccCC:cropalf               28.4735     0.7718  36.891  < 2e-16 ***
#   tillRT:ccCC:cropalf               22.0018     0.7718  28.506  < 2e-16 ***
#   tillCT:ccNC:cropalf               20.6424     0.7718  26.745  < 2e-16 ***
#   tillNT:ccNC:cropalf               33.2221     0.7718  43.043  < 2e-16 ***
#   tillRT:ccNC:cropalf               27.0874     0.7718  35.095  < 2e-16 ***
#   tillCT:ccCC:cropcorn alf          20.5450     0.9402  21.852  < 2e-16 ***
#   tillNT:ccCC:cropcorn alf           7.8515     0.9402   8.351  < 2e-16 ***
#   tillRT:ccCC:cropcorn alf          15.9442     0.9402  16.958  < 2e-16 ***
#   tillCT:ccNC:cropcorn alf          48.1280     0.9402  51.189  < 2e-16 ***
#   tillNT:ccNC:cropcorn alf          38.5268     0.9402  40.977  < 2e-16 ***
#   tillRT:ccNC:cropcorn alf          44.9876     0.9402  47.849  < 2e-16 ***
#   tillCT:ccCC:cropcorn grain cs     50.6925     0.8447  60.013  < 2e-16 ***
#   tillNT:ccCC:cropcorn grain cs     44.1485     0.8447  52.266  < 2e-16 ***
#   tillRT:ccCC:cropcorn grain cs     47.7365     0.8447  56.514  < 2e-16 ***
#   tillCT:ccNC:cropcorn grain cs     64.1294     0.8447  75.921  < 2e-16 ***
#   tillNT:ccNC:cropcorn grain cs     58.1279     0.8447  68.816  < 2e-16 ***
#   tillRT:ccNC:cropcorn grain cs     61.0923     0.8447  72.325  < 2e-16 ***
#   tillCT:ccCC:cropcorn grain mono   54.0491     0.8447  63.987  < 2e-16 ***
#   tillNT:ccCC:cropcorn grain mono   56.2735     0.8447  66.621  < 2e-16 ***
#   tillRT:ccCC:cropcorn grain mono   53.8745     0.8447  63.780  < 2e-16 ***
#   tillCT:ccNC:cropcorn grain mono   62.9940     0.8447  74.577  < 2e-16 ***
#   tillNT:ccNC:cropcorn grain mono   67.1178     0.8447  79.459  < 2e-16 ***
#   tillRT:ccNC:cropcorn grain mono   63.5414     0.8447  75.225  < 2e-16 ***
#   tillCT:ccCC:cropcorn silage mono  38.0402     0.8447  45.035  < 2e-16 ***
#   tillNT:ccCC:cropcorn silage mono   4.0922     0.8447   4.845 1.27e-06 ***
#   tillRT:ccCC:cropcorn silage mono  29.3340     0.8447  34.728  < 2e-16 ***
#   tillCT:ccNC:cropcorn silage mono  65.4835     0.8447  77.524  < 2e-16 ***
#   tillNT:ccNC:cropcorn silage mono  28.8184     0.8447  34.117  < 2e-16 ***
#   tillRT:ccNC:cropcorn silage mono  58.7421     0.8447  69.543  < 2e-16 ***
#   tillCT:ccCC:cropsoy cs            -0.9400     0.8447  -1.113   0.2658    
# tillNT:ccCC:cropsoy cs            -4.1371     0.8447  -4.898 9.74e-07 ***
#   tillRT:ccCC:cropsoy cs            -1.6751     0.8447  -1.983   0.0474 *  
#   tillCT:ccNC:cropsoy cs             6.6335     0.8447   7.853 4.18e-15 ***
#   tillNT:ccNC:cropsoy cs             6.9427     0.8447   8.219  < 2e-16 ***
#   tillRT:ccNC:cropsoy cs             7.2295     0.8447   8.559  < 2e-16 ***
#   tillCT:ccCC:croptri alf           12.1101     0.9479  12.776  < 2e-16 ***
#   tillNT:ccCC:croptri alf           -6.0374     0.9479  -6.369 1.92e-10 ***
#   tillRT:ccCC:croptri alf            3.9778     0.9479   4.197 2.72e-05 ***
#   tillCT:ccNC:croptri alf            6.5373     0.9479   6.897 5.41e-12 ***
#   tillNT:ccNC:croptri alf           -7.2168     0.9479  -7.614 2.73e-14 ***
#   tillRT:ccNC:croptri alf                NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 14.68 on 34230 degrees of freedom
# Multiple R-squared:  0.7022,	Adjusted R-squared:  0.7018 
# F-statistic:  1968 on 41 and 34230 DF,  p-value: < 2.2e-16

summary(aovno3)
# Df   Sum Sq Mean Sq F value Pr(>F)
# till:cc:nfert    11 13587543 1235231    2143 <2e-16 ***
#   Residuals     39156 22573096     576
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukout <- TukeyHSD(aovno3)
cld <- multcompView::multcompLetters4(aovno3, Tukout)


# ANNUAL MEANS ACROSS ALL YEARS 
ndat_gmean <- ndatyr %>%
  group_by(crop, till, cc) %>%
  summarize(NO3_mean = mean(NO3.yr),
            NO3_se = se(NO3.yr)) %>%
  pivot_longer(cols=-c("crop", "till", "cc"),
               names_to=c("component", ".value"),
               names_sep="_") %>%
  arrange(desc(mean))


cld <- as.data.frame.list(cld$`till:cc:crop`)
ndat_gmean$cld <- cld$Letters


windows(xpinch=200, ypinch=200, width=5, height=5)

# convert kg/ha to lb/ac
ndat_gmean$meanlbac <- ndat_gmean$mean*2.20462/2.47105
ndat_gmean$selbac <- ndat_gmean$se*2.20462/2.47105




ggplot(data=ndat_gmean, 
       aes(x=crop, y=meanlbac, fill=crop)) +   # fill=variable
  geom_bar(stat="identity", position=position_dodge(), show.legend=F) + # color="#332288", 
  geom_errorbar(aes(ymin=meanlbac-selbac, ymax=meanlbac+selbac), width=0.3, position=position_dodge(0.9)) +
  facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))), 
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c(NC="No Cover Crop",CC="Rye Cover Crop", 
                 "CT" = "Conventional Till", "RT"="Reduced Till","NT" = "No Till"))) +
                 # "Fall N" = "Fall\nN", "High N" = "High\nN", "Recommended N"="Recm'd\nN"))) +
  # scale_fill_manual(values=c("#c44f2d","#20243d", "#C2e4ef")) +
  xlab("Crop") +
  ylab("Mean annual nitrate loss (lb N per ac) 2022 to 2072") +
  scale_x_discrete(breaks=c("alf", "soy cs", "tri alf", "corn grain mono", "corn silage mono",
                            "corn grain cs", "corn alf"),
                   labels=c("alfalfa", "soy", "triticale", "corn grain\nmonoculture", 
                            "corn silage\nmonoculture", "corn grain\nc-s rotation", "corn grain\nalf rotation")) +
  geom_text(aes(x=crop, y=meanlbac+10, label=cld), size=4, fontface="bold") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-30, hjust=0, size=11))

ggsave("plots/water, nitrate, sediments/NY_NO3 losses 22-72 mean bars lbac.png", width=8, height=5, dpi=300)




############################### N LOSSES PER decade PLOT


windows(xpinch=200, ypinch=200, width=5, height=5)


# convert kg/ha to lb/ac
ndat_dec$meanlbac <- ndat_dec$mean*2.20462/2.47105
ndat_dec$selbac <- ndat_dec$se*2.20462/2.47105


pal6 <- c("#eaeccc", "#FEDa8B", "#FDb366", "#f67e4b","#dd3d2d", "#a50026")   
pal6blue <- c("#ffffff", "#ceced5", "#9f9fac", "#727284", "#48495f", "#20233c")
                   

ggplot(data=ndat_dec, aes(x=crop, y=meanlbac, fill=decade)) +
  geom_bar(stat="identity", position=position_dodge(), color="#20233d") +
  geom_errorbar(aes(ymin=meanlbac-selbac, ymax=meanlbac+selbac), width=0.3, position=position_dodge(0.9)) +
  facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))), 
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c(NC="No Cover Crop", CC="Rye Cover Crop", 
                 "CT" = "Conventional Till", "Reduced Till", "NT" = "No Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
  scale_fill_manual(values=pal6blue) +
  xlab("crop") +
  ylab("Mean annual nitrate loss (lb N per ac) 2022 to 2072") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-30, hjust=0, size=11))

ggsave("plots/water, nitrate, sediments/NY_NO3 losses decadal mean bars blue lbac.png", width=7, height=5, dpi=300)



############################### N LOSSES PER decade per crop PLOT
# prep data for plotting
# put data in long form for plotting, 1 column for N2O, NO3, and Ntots
ndat_dec_croplong <- melt(ndat_dec_crop, id=c("crop", "till", "cc", "nfert", "decade"))
# separate means from se's
ndat_dec_croplong$mean.se <- ifelse(grepl("mean", ndat_dec_croplong$variable), "mean", "se")

# make new column for se values
dat.se <- ndat_dec_croplong[ndat_dec_croplong$mean.se=="se",1:7]
colnames(dat.se)[7] <- "se"
dat.se$variable <- gsub(".se", "", dat.se$variable)
dat.mean <- ndat_dec_croplong[ndat_dec_croplong$mean.se=="mean",1:7]
colnames(dat.mean)[7] <- "mean"
dat.mean$variable <- gsub(".mean", "", dat.mean$variable)
ndeccroplong <- left_join(dat.mean, dat.se) # , relationship="one-to-one")
rm(dat.se, dat.mean)

windows(xpinch=200, ypinch=200, width=5, height=5)


pal6 <- c("#eaeccc", "#FEDa8B", "#FDb366", "#f67e4b","#dd3d2d", "#a50026")                  


ggplot(data=ndeccroplong[ndeccroplong$variable=="NO3" & ndeccroplong$crop == "corn",], aes(x=nfert, y=mean, fill=decade)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, position=position_dodge(0.9)) +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT", "RT"))), 
             cols=vars(factor(cc, levels=c("CC", "NC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c(CC="Has Cover Crop", NC="No Cover Crop",
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
  scale_fill_manual(values=pal6) +
  xlab("N management") +
  ylab("Corn mean annual nitrate loss (kg N per ha) 2022 to 2072") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/IL_N losses corn mean annual bars.png", width=6, height=7, dpi=300)




# we don't really care if the decades are significnatly different from one another within a treatment.
# we want to know if the treatments are different.
# so let's run stats on ndatyr without a time term

#######################   
#######################   is mean no3 loss different among till*cc*Nfert groups across all years in corn?
#######################   

ndatyrcorn <- ndatyr[ndatyr$crop=="corn",]

effect <- aov(NO3.yr ~till*cc*nfert, data=ndatyrcorn)

summary(effect)
Tukout <- TukeyHSD(effect)
# put interaction output into a dataframe we can sort
Tukout <- as.data.frame(Tukout[7]) %>%  # [7] is the 3-way interaction term
  rownames_to_column(., "term") %>%
  arrange(term)


# assumptions - following https://statsandr.com/blog/anova-in-r

# Independence - the observations (grain.c.kgc.ha. observations) are independent, one does
# not depend on the other, they're not dependent on some other variable like, 
# from one individual came multiple observations.

# Normality - not required with large enough sample size >30.  But we can test anyway. 
# the residuals (observed - mean for that group) should be normally distributed.
qqnorm(ndatyrcorn$NO3.yr)
qqline(ndatyrcorn$NO3.yr)  # not amazing
hist(ndatyrcorn$NO3.yr-mean(ndatyrcorn$NO3.yr))  # right skewed

MASS::boxcox(NO3.yr~1, data=ndatyrcorn) # lambda is about 0.5
qqnorm(sqrt(ndatyrcorn$NO3.yr))
qqline(sqrt(ndatyrcorn$NO3.yr))  # MUCH BETTER


# Equality of variances 
ggplot(data=ndatyrcorn, aes(x=management, y=NO3.yr)) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))
# looks like a lot of potential outliers
car::leveneTest(NO3.yr ~ cc*till*nfert, data=ndatyrcorn)
# Levene's Test for Homogeneity of Variance (center = median)
#          Df F value    Pr(>F)    
# group    17  71.284 < 2.2e-16 ***
#       29358                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# HOWEVER: it seems that our large sample size (>29k data points) might be affecting this test
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

# Equality of variances with sqrt()
ggplot(data=ndatyrcorn, aes(x=management, y=sqrt(NO3.yr))) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))
# looks like a lot of potential outliers
car::leveneTest(sqrt(NO3.yr) ~ cc*till*nfert, data=ndatyrcorn)
# Levene's Test for Homogeneity of Variance (center = median)
#          Df F value    Pr(>F)    
# group    17   6.922 < 2.2e-16 ***
#       29358                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# outliers
summary(ndatyrcorn$NO3.yr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    # max is nearly 3x 3rd quartile
# 2.843  37.072  56.217  59.099  78.021 209.284 

summary(sqrt(ndatyrcorn$NO3.yr))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   # max now <2x 3rd quartile
# 1.686   6.089   7.498   7.445   8.833  14.467 

# outliers via histograms
hist(ndatyrcorn$NO3.yr, breaks=sqrt(nrow(ndatyrcorn)))
hist(sqrt(ndatyrcorn$NO3.yr), breaks=sqrt(nrow(ndatyrcorn)))  # much better

ggplot(data=ndatyrcorn, aes(x=NO3.yr)) +
  geom_histogram() +
  facet_grid(rows=vars(nfert), cols=vars(till, cc))

ggplot(data=ndatyrcorn, aes(x=sqrt(NO3.yr))) +
  geom_histogram() +
  facet_grid(rows=vars(nfert), cols=vars(till, cc))

#outliers via boxplots
boxplot(NO3.yr~management, data=ndatyrcorn) # 
boxplot(sqrt(NO3.yr)~management, data=ndatyrcorn) # looks better but still a lot of outlier dots

# outliers via z-scores
ndatyr$NO3.yrz <- scale(ndatyr$NO3.yr)
hist(ndatyr$NO3.yrz)
summary(ndatyr$NO3.yrz)
# V1         
# Min.   :-1.5839  
# 1st Qu.:-0.7849  
# Median :-0.1558  
# Mean   : 0.0000  
# 3rd Qu.: 0.6350  
# Max.   : 5.3288  
# < -2 or >2 is considered rare
# < -3 or >3 is extremely rare
# < -3.29 or > -3.29 is used to detect outliers, where one out of 1000 observations will be outside
# this range if normal distribution.
# interquartile range is > -2 and <2. 
# min. does not reach -2 max does exceed 5!


ndatyr$NO3sqrt.yrz <- scale(sqrt(ndatyr$NO3.yr))
hist(ndatyr$NO3sqrt.yrz)
summary(ndatyr$NO3sqrt.yrz)
# V1          
# Min.   :-2.64268  
# 1st Qu.:-0.72126  
# Median : 0.01135  
# Mean   : 0.00000  
# 3rd Qu.: 0.72960  
# Max.   : 3.51435  
# < -2 or >2 is considered rare
# < -3 or >3 is extremely rare
# < -3.29 or > -3.29 is used to detect outliers, where one out of 1000 observations will be outside
# this range if normal distribution.
# interquartile range is > -2 and <2. 
# min. does not reach -3 max does exceed 3! but at least not 5 like above!

# assumption test summary: taking sqrt of NO3 might meet assumptions better.
# but with such a large sample size, it might not matter. try both.

# re-run ANOVA with the above knowledge 
# try sqrt(NO3)

effect <- aov(NO3.yr ~till*cc*nfert, data=ndatyrcorn)

summary(effect)
# > summary(effect)
# Df   Sum Sq Mean Sq   F value   Pr(>F)    
# till              2   190707   95354   228.307  < 2e-16 ***
# cc                1  8992320 8992320 21530.455  < 2e-16 ***
# nfert             2  2682054 1341027  3210.842  < 2e-16 ***
# till:cc           2      864     432     1.034  0.35561    
# till:nfert        4     6242    1561     3.736  0.00482 ** 
# cc:nfert          2    31957   15979    38.258  < 2e-16 ***
# till:cc:nfert     4    13236    3309     7.923 2.23e-06 ***
# Residuals     29358 12261540     418                       
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukout <- TukeyHSD(effect)
# # put interaction output into a dataframe we can sort
# Tukout <- as.data.frame(Tukout[7]) %>%  # [7] is the 3-way interaction term
#   rownames_to_column(., "term") %>%
#   arrange(term)

# alternate model :
effect.sqrt <-aov(sqrt(NO3.yr) ~till*cc*nfert, data=ndatyrcorn)

# > summary(effect.sqrt)
# Df Sum Sq Mean Sq   F value   Pr(>F)    
#   till              2    903     451   253.291  < 2e-16 ***
#   cc                1  41076   41076 23050.411  < 2e-16 ***
#   nfert             2  13178    6589  3697.551  < 2e-16 ***
#   till:cc           2     26      13     7.216 0.000736 ***
#   till:nfert        4     31       8     4.387 0.001516 ** 
#   cc:nfert          2     36      18    10.040 4.38e-05 ***
#   till:cc:nfert     4     63      16     8.842 3.94e-07 ***
#   Residuals     29358  52316       2                       
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukout.sqrt <- TukeyHSD(effect.sqrt)
# # put interaction output into a dataframe we can sort
# Tukout.sqrt <- as.data.frame(Tukout.sqrt[7]) %>%  # [7] is the 3-way interaction term
#   rownames_to_column(., "term") %>%
#   arrange(term)

# compact letter display
cld.sqrt <- multcompView::multcompLetters4(effect.sqrt, Tukout.sqrt)

# table with letters 
ndatsumcorn <- group_by(ndatyrcorn, cc, till, nfert) %>%
  summarize(mean=mean(sqrt(NO3.yr)), 
            se=se(sqrt(NO3.yr))) %>%
  arrange(desc(mean))

cld.sqrt <- as.data.frame.list(cld.sqrt$`till:cc:nfert`)
ndatsumcorn$cld <- cld.sqrt$Letters



# use these letters on this plot:

windows(xpinch=200, ypinch=200, width=5, height=5)


pal6 <- c("#eaeccc", "#FEDa8B", "#FDb366", "#f67e4b","#dd3d2d", "#a50026")  

ndeccroplong$till <- factor(ndeccroplong$till, levels=c("CT", "RT", "NT"), ordered=T)
ndatsumcorn$till <- factor(ndatsumcorn$till, levels=c("CT", "RT", "NT"), ordered=T)
ndeccroplong$cc <- factor(ndeccroplong$cc, levels=c("NC", "CC"), ordered=T)
ndatsumcorn$cc <- factor(ndatsumcorn$cc, levels=c("NC", "CC"), ordered=T)


ggplot() +
  geom_bar(data=ndeccroplong[ndeccroplong$variable=="NO3" & ndeccroplong$crop == "corn" & ndeccroplong$till %in% c("CT", "NT"),], 
           aes(x=nfert, y=mean, fill=decade),
           stat="identity", position=position_dodge(), 
           color="gray20") +
  geom_errorbar(data=ndeccroplong[ndeccroplong$variable=="NO3" & ndeccroplong$crop == "corn" & ndeccroplong$till %in% c("CT", "NT"),],
                aes(x=nfert, y=mean, ymin=mean-se, ymax=mean+se, group=decade), 
                width=0.3, position=position_dodge(0.9), color="gray20") +
  scale_fill_manual(values=pal6, name="Decade") +
  geom_bar(data=ndatsumcorn[ndatsumcorn$till %in% c("CT", "NT"),], aes(x=nfert, y=mean), 
           stat="identity", color=NA, fill=NA) +
  geom_text(data=ndatsumcorn[ndatsumcorn$till %in% c("CT", "NT"),], 
            aes(x=nfert, label=cld, y=mean^2 + 2*mean), vjust=-0.5, 
            color="gray20", size=4, fontface="bold") +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT"))),  #"RT", 
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c(CC="Has Cover Crop", NC="No Cover Crop",
                 "CT" = "Conventional Till", "NT" = "No Till"))) + # , "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 

  xlab("N management") +
  ylab(expression('Corn mean annual NO'[3]*''^-''*'-N loss (kg N ha'^-1*') 2022 to 2072')) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/IL_N losses corn mean annual bars with letters.png", width=6, height=4, dpi=300)


