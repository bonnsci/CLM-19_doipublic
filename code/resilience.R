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


# pull in data
# biomass 
bmil <- read.csv("data/biomass/biomass_IL.csv")
colnames(bmil)[9] <- "grainc"

# climate - SPEI
spei <- read.csv("data/climate data not large/speiIL.csv")

# let's focus on rcp 6.0 for now
bmil <- bmil[bmil$climate_scenario=="rcp60",]

spei <- spei[spei$rcp=="rcp60",]

# biomass is once per year, spei is monthly, could get annual (or water year) average
# spei or look at July SPEI (or other month or average of months). Let's start
# with july Spei

spei <- spei %>%
  separate_wider_delim(dat, delim=" ", 
                       names=c("month", "year"),  # split the ID into two columns
                       cols_remove=F)

bmilgr <- bmil[,c(2,4, 6:9),]

# make dummy factor for CT, RT, NT
bmilgr$till <- ifelse(grepl("ct-", bmilgr$management_name), "CT", 
                    ifelse(grepl("rt-", bmilgr$management_name), "RT", "NT"))
# # check
# unique(bmilgr$till)

# dummy for CC or NC
bmilgr$cc <- ifelse(grepl("-cc-", bmilgr$management_name), "CC", "NC")
# # check
# unique(bmilgr$cc)

# dummy for N treatment
bmilgr$nfert <- ifelse(grepl("-cn", bmilgr$management_name), "High N", 
                     ifelse(grepl("-fn", bmilgr$management_name), "Fall N","Recommended N"))
# # check
# unique(bmilgr$nfert)


# merge biomass and spei data
spei$year <- as.numeric(spei$year)

speibm <- inner_join(spei[,-2], bmilgr, join_by(site==site_name, year==Year), 
                    relationship="many-to-many")

# grainc z-score
speibm$grainz <- (speibm$grainc - mean(speibm$grainc))/sd(speibm$grainc)

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=speibm[speibm$crop_name %in% c("corn, grain") & speibm$month =="Jul",], 
       aes(x=fit.12mo, y=grainc)) +  #  x=abs(fit.1mo), x=fit.12mo, x=grainz
  geom_point(size=0.1, alpha=0.5) +
  scale_x_continuous(breaks=c(-2.0, -1.6, -1.3, -0.8, 1.30, 1.60, 2.0, 3.0),
                     labels=c(-2.0, -1.6, -1.3, -0.8, 1.30, 1.60, 2.0, 3.0)) +
  facet_grid(cols=vars(ns, till), rows=vars(cc, nfert))

# looks like corn grain c less resilient to extreme weather when: fall N, in the South. nothing obviously apparent between tillage or cover treatments

ggplot(data=speibm[speibm$crop_name %in% c("soybean") & speibm$month =="Jul",], 
       aes(x=fit.12mo, y=grainc)) +  #  x=abs(fit.1mo), x=fit.12mo
  geom_point(size=0.1, alpha=0.5) +
  facet_grid(cols=vars(ns, till), rows=vars(cc))

# why so many NA spcat12mo values?
# check that they are all in 2022

# ch <- speibm[is.na(speibm$spcat12mo),]
# unique(ch$year) # just 2022
# unique(ch$month) # Jan. - Nov. as expected
# sum(is.na(ch[ch$year==2022,"fit.12mo"]))  # 19008
# sum(is.na(ch[!ch$year==2022,"fit.12mo"]))  # 0

# following fig. 5c in Scheffer et al 2015 - inferring probabilistic resilience from massive data.
# Not sure we have "massive" data but probably enough to try this approach.
# I'm thinking histograms of crop yields at different drought and wetness categories
# across our "treatments".

# need to add ID variable for SPEI category for each row of data
# again classifications from: https://droughtmonitor.unl.edu/About/AbouttheData/DroughtClassification.aspx


speibm$spcat1mo <- ifelse(speibm$fit.1mo >= -0.4999999 & speibm$fit.1mo <=0.49999999, "Normal",
                          # drought conditions
                          ifelse(speibm$fit.1mo >= -0.7999999 & speibm$fit.1mo <= -0.50, "Abnormally Dry",  
                                 ifelse(speibm$fit.1mo >= -1.2999999 & speibm$fit.1mo <= -0.80, "Moderate Drought",
                                        ifelse(speibm$fit.1mo >= -1.59999999 & speibm$fit.1mo <= -1.30, "Severe Drought",
                                               ifelse(speibm$fit.1mo >= -1.9999999 & speibm$fit.1mo <= -1.60, "Extreme Drought",
                                                      ifelse(speibm$fit.1mo <= -2.0, "Exceptional Drought",
                           # wet conditions                           
                                            ifelse(speibm$fit.1mo <= 0.7999999 & speibm$fit.1mo >= 0.50, "Abnormally Wet",    
                                                  ifelse(speibm$fit.1mo <= 1.2999999 & speibm$fit.1mo >= 0.80, "Moderately Wet",
                                                        ifelse(speibm$fit.1mo <= 1.5999999 & speibm$fit.1mo >= 1.30, "Severely Wet",
                                                              ifelse(speibm$fit.1mo <= 1.999999 & speibm$fit.1mo >= 1.60, "Extremely Wet",
                                                                    ifelse(speibm$fit.1mo >= 2.0, "Exceptionally Wet", "X")))))))))))
                                                             
# Test
unique(speibm$spcat1mo)

speibm$spcat12mo <- ifelse(speibm$fit.12mo >= -0.4999999 & speibm$fit.12mo <=0.49999999, "Normal",
                          # drought conditions
                          ifelse(speibm$fit.12mo >= -0.7999999 & speibm$fit.12mo <= -0.50, "Abnormally Dry",  
                                 ifelse(speibm$fit.12mo >= -1.2999999 & speibm$fit.12mo <= -0.80, "Moderate Drought",
                                        ifelse(speibm$fit.12mo >= -1.59999999 & speibm$fit.12mo <= -1.30, "Severe Drought",
                                               ifelse(speibm$fit.12mo >= -1.9999999 & speibm$fit.12mo <= -1.60, "Extreme Drought",
                                                      ifelse(speibm$fit.12mo <= -2.0, "Exceptional Drought",
                                                             # wet conditions                           
                                                             ifelse(speibm$fit.12mo <= 0.7999999 & speibm$fit.12mo >= 0.50, "Abnormally Wet",    
                                                                    ifelse(speibm$fit.12mo <= 1.2999999 & speibm$fit.12mo >= 0.80, "Moderately Wet",
                                                                           ifelse(speibm$fit.12mo <= 1.5999999 & speibm$fit.12mo >= 1.30, "Severely Wet",
                                                                                  ifelse(speibm$fit.12mo <= 1.999999 & speibm$fit.12mo >= 1.60, "Extremely Wet",
                                                                                         ifelse(speibm$fit.12mo >= 2.0, "Exceptionally Wet", "X")))))))))))

# Test
unique(speibm$spcat12mo)
# NA values expected because first 11 months fit.12mo = NA. 

speibm$spcat12mo <- factor(speibm$spcat12mo,
           levels=c("Exceptional Drought","Extreme Drought","Severe Drought","Moderate Drought", "Abnormally Dry",
                    "Normal", "Abnormally Wet", "Moderately Wet", "Severely Wet", "Extremely Wet", "Exceptionally Wet"),
           ordered=T)



# Find cutoff values for plots
# Fig. 5C caption says probability of finding tree cover > 60% is low/high, etc. Is 60% meaningful? What is meaningful here?
# Perhaps median grainc in spei=normal in 2020s? 75 %ile, i.e., "better than average".

speibm$graincMg <- speibm$grainc/1000  # doing this makes the % frequency more like 10% rather than 0.01%

norm30corn <- speibm[speibm$year>2022 & speibm$year<2030 & 
                       speibm$spcat12mo =="Normal" &
                       speibm$crop_name == "corn, grain",]

norm30grainc_corn75p <- quantile(norm30corn$graincMg, prob=0.75)
norm30grainc_corn90p <- quantile(norm30corn$graincMg, prob=0.90)
norm30grainc_cornmed <- median(norm30corn$graincMg)

norm30soy<- speibm[speibm$year>2022 & speibm$year<2030 & 
                       speibm$spcat12mo =="Normal" &
                       speibm$crop_name == "soybean",]

norm30grainc_soy75p <- quantile(norm30soy$graincMg, prob=0.75)
norm30grainc_soy90p <- quantile(norm30soy$graincMg, prob=0.90)
norm30grainc_soymed <- median(norm30soy$graincMg)





# make plots like Fig. 5c

# A histogram is frequency. but a relative frequency AKA density gives % probability rather than just count.
# if N is large enough, then relative freq histogram starts to resemble the populations distribution,
# the area under the histogram =1, and related to probability.
# probability is the fraction of the area under the frequency distribution curve / density curve.

windows(xpinch=200, ypinch=200, width=5, height=5)


# set facet background color per spcat
pal11 <- c("#a50026", "#dd3d2d","#f67e4b","#FDb366","#FEDa8B", "#eaeccc", "#C2e4ef", "#98cae1","#6ea6cd", "#4a7bb7", "#364b9a")


ggplot(data=speibm[speibm$crop_name=="corn, grain" & 
                     !is.na(speibm$spcat12mo) &  # for now just compare extremes: CT+NC and NT+CC
                     speibm$management_name %in% c("ct-nc-cn", "ct-nc-sn","nt-cc-cn", "nt-cc-sn"),],
       aes(x=graincMg)) +
  # geom_histogram() + # histogram (counts)
  geom_density(aes(y=after_stat(density), # sets the y-axis units
                   group=interaction(spcat12mo, cc, till)),
               trim=T) +   # make sure calculating the density per facet, not whole dataset at once
  # labs(x="Corn grain biomass C (kg/ha)", y="Frequency (count) 2022 to 2072") +
  labs(x="Corn grain biomass C (Mg/ha)", y="Frequency (%) 2022 to 2072") +
  scale_y_continuous(labels=scales::percent_format()) +
  geom_vline(xintercept=norm30grainc_cornmed, color="#BBcc33") +  # 4000 chosen arbitrarily - is there something more meaningful?
  geom_vline(xintercept=norm30grainc_corn90p, color="#44bb99") + 
  facet_grid2(rows=vars(spcat12mo), cols=vars(cc, till), scales="free_y", 
             labeller = as_labeller(
               # c("Exceptional Drought"= "D4","Extreme Drought" = "D3","Severe Drought" = "D2","Moderate Drought" = "D1", "Abnormally Dry" = "D0",
               #   "Normal" = "Normal", "Abnormally Wet" = "W0", "Moderately Wet" = "W1", "Severely Wet" = "W2", "Extremely Wet" = "W3", "Exceptionally Wet" = "W4")))
              c("Exceptional Drought"= "Exceptional\nDrought","Extreme Drought" = "Extreme\nDrought",
                "Severe Drought" = "Severe\nDrought","Moderate Drought" = "Moderate\nDrought", 
                "Abnormally Dry" = "Abnormally\nDry", "Normal" = "Normal", 
                "Abnormally Wet" = "Abnormally\nWet", "Moderately Wet" = "Moderately\nWet", 
                "Severely Wet" = "Severely\nWet", "Extremely Wet" = "Extremely\nWet", 
                "Exceptionally Wet" = "Exceptionally\nWet",
                "CC" = "With cover crops", "NC"= "Without cover crops",
                "NT" = "No-till", "CT" = "Conventional till")),
              strip = strip_themed(background_y = elem_list_rect(fill = pal11),  # cool stuff from ggh4x package!
                                   text_y = elem_list_text(color=c("white", rep("black", 9), "white")))) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),  #,
    panel.background = element_rect(fill = 'gray96')
  )	 

# ggsave("plots/resilience/IL_corngrainc_bydrought_hist.png", width=4, height=10, dpi=300)
ggsave("plots/resilience/IL_corngrainc_bydrought_density.png", width=4, height=10, dpi=300)




# Quantify areas under curves by site, SPEI, and treatments

lsd <- speibm[speibm$crop_name %in% c("corn, grain", "soybean"),] %>%
  group_split(site, crop_name, spcat12mo, till, cc, nfert) # by site, so that we can compare means and variability of treatments across sites


lecdf <- lapply(lsd, function(i){
  d_fun <- ecdf(i$graincMg)
  cropmed <- ifelse(i$crop_name[1] == "corn, grain", norm30grainc_cornmed, norm30grainc_soymed)
  crop90p <- ifelse(i$crop_name[1] == "corn, grain", norm30grainc_corn90p, norm30grainc_soy90p)
  grmed <- 1 - d_fun(cropmed) # 1- gives the area under the curve to the RIGHT of the value
  gr90p <- 1 - d_fun(crop90p)
  out <- i[1, c("site", "crop_name", "till", "cc", "nfert", "spcat12mo")]
  out$grmed <- grmed
  out$gr90p <- gr90p
  out
})

lecdf <- lecdf %>%
  bind_rows(lecdf)

rm(lsd)

# are the areas under the curves different?

corndf <- lecdf[lecdf$crop_name=="corn, grain" & !is.na(lecdf$spcat12mo),]

cornaov <- aov(grmed~till*cc*nfert*spcat12mo, data=corndf)
summary(cornaov)
# Df Sum Sq Mean Sq  F value   Pr(>F)    
# till                        2    0.7    0.36    9.070 0.000116 ***
# cc                          1    3.4    3.43   85.803  < 2e-16 ***
# nfert                       2  100.8   50.39 1259.084  < 2e-16 ***
# spcat12mo                  10  288.6   28.86  721.153  < 2e-16 ***
# till:cc                     2    0.1    0.03    0.736 0.478825    
# till:nfert                  4    5.2    1.31   32.685  < 2e-16 ***
# cc:nfert                    2   20.6   10.28  256.911  < 2e-16 ***
# till:spcat12mo             20    2.1    0.10    2.575 0.000138 ***
# cc:spcat12mo               10    0.7    0.07    1.636 0.089931 .  
# nfert:spcat12mo            20   28.1    1.40   35.053  < 2e-16 ***
# till:cc:nfert               4    1.5    0.36    9.076 2.57e-07 ***
# till:cc:spcat12mo          20    0.5    0.02    0.565 0.938112    
# till:nfert:spcat12mo       40    0.4    0.01    0.248 1.000000    
# cc:nfert:spcat12mo         20    1.2    0.06    1.493 0.072330 .  
# till:cc:nfert:spcat12mo    40    0.1    0.00    0.076 1.000000    
# Residuals               12474  499.2    0.04                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukout <- TukeyHSD(cornaov)

# put interaction output into a dataframe we can sort
Tukout <- as.data.frame(Tukout[5]) %>%  # 5 is the list item for the interaction that looks significant from the summary above
  rownames_to_column(., "term") %>%
  arrange(term)


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

