
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


spei <- read.csv("data/climate data not large/speiNY_formerge.csv")
# from script spei_IL.R



################################## resilience: sediment yield

# sed <- read.csv("data/water, nitrate, sediments/NY_wdatyr.csv")
# 
# # unique(sed$climate_scenario)  # "rcp60"
# 
# 
# speised <- inner_join(spei[,-2], sed[-4], join_by(site==site_name, year==year), 
#                       relationship="many-to-many")

# windows(xpinch=200, ypinch=200, width=5, height=5)
# ggplot(data=speised, 
#        aes(x=fit.12mo, y=sed.yr)) +  #  x=abs(fit.1mo), x=fit.12mo
#   geom_point(size=0.1, alpha=0.5) +
#   facet_grid(cols=vars(factor(month)), rows=vars(factor(cc), factor(till)))



######################## plain sediment yield (not sediment yield probabilities) by season

# need monthly totals to match with growing season spei
# so we need to go back to daily data rather than annual sum

# # if you need daily estimates use this:
# wdat <- read.csv("data/large_data/daily water, sediments/NY_forage_day_water.csv")
# beepr::beep(sound=11)
# # UNITS: Transpiration, Evaporation, Water leaching, and runoff are mm/day.
# # Sediment yield is kg/ha.
# 
# 
# wdatssn.site <- wdat %>%
#   mutate(month = strftime(as.Date(date), "%m"),
#          dayofmo = strftime(as.Date(date), "%d")) %>%
#   filter(climate_scenario == "rcp60") %>%
#   mutate(season =  ifelse(month %in% c("12", "01", "02"), "Winter",
#                           ifelse(month %in% c("03", "04", "05"), "Spring",
#                                  ifelse(month %in% c("06", "07", "08"), "Summer", "Fall"))),
#        till = ifelse(grepl("ct-", management_name), "CT",
#                         ifelse(grepl("rt-", management_name), "RT", "NT")),
#        cc = ifelse(grepl("-cc", management_name), "CC", "NC")) %>%
#   group_by(site_name, crop_system_name, management_name, Year, season, till, cc) %>%
#   summarize(sed.ssncrop = mean(SedimentYield)) # mean sediments for season at each site for each crop rotation
# 
#     
# unique(wdat$crop_system_name)    
# 
# beepr::beep(sound=8)
# #
# write.csv(wdatssn.site, "data/water, nitrate, sediments/seds_NY_ssntotals.csv", row.names=F)

#
# clean up
# rm(wdat)

wdatssn.site <- read.csv("data/water, nitrate, sediments/seds_NY_ssntotals.csv")

# get spei ready to join to sediments
# spei <- spei[spei$rcp=="rcp60",]  # already filtered
spei <- distinct(spei[c(1,5,8,9,11)])


# merge spei and sediments
spsed <- inner_join(spei, wdatssn.site, join_by(site==site_name, year==Year, season==season), 
                     relationship="many-to-many")

# check
unique(spsed$spcat12mossn)


# how many seasons of each climate type do we have?
# testy <- filter(spsed, site=="f_1", management_name=="ct-cc", crop_system_name == "corn-grain")
# x <- aggregate(year~spcat12mossn, data=testy, FUN="length")
# sum(x$year)
# x
# spcat12mossn year
# 1       Abnormally Dry   26
# 2       Abnormally Wet   14
# 3  Exceptional Drought    4  # <10
# 4    Exceptionally Wet    2  # <10
# 5      Extreme Drought    4  # <10
# 6        Extremely Wet    7  # <10
# 7     Moderate Drought   16
# 8       Moderately Wet   18
# 9               Normal   90
# 10      Severe Drought    9  # <10
# 11        Severely Wet   10  # =10
# rm(testy, x)

# not enough data of the ones marked as <=10 above for statistical analysis.
spsed2 <- filter(spsed, spcat12mossn %in% c("Abnormally Dry", "Abnormally Wet", 
                                            "Moderate Drought", "Moderately Wet", 
                                            "Normal"))

spsed2$spcat12mossn <- factor(spsed2$spcat12mossn, ordered=F)
spsed2$cc <- factor(spsed2$cc, ordered=F)
spsed2$till <- factor(spsed2$till, ordered=F)

sedlm <- lm(sed.ssncrop~relevel(cc, ref="NC") + 
              relevel(till, ref="CT") + 
              relevel(spcat12mossn, ref="Normal")*year, 
            data=spsed2[spsed2$season=="Spring" & ! spsed2$till=="RT",])
summary(sedlm)

# for a system in a normal year with no cover, conventional till,  year sediments increase 0.04 (kg/ha)
# for a system in a normal year with cover crop, CT, sediments decrease by 0.68 kg/ha across all years
# for a system in a normal year with no cover and no till, sediments decrease by 0.47 kg/ha across all years
# for a system in a normal year with cover crop and no till, sediments decrease by 0.68+0.47=1.15 kg/ha across all years


0.68+0.47

# make a subset for the report
spsedrep <- mutate(spsed, cctill=paste0(cc, ".", till)) %>%
  filter(season== "Spring", 
                    cctill %in% c("CC.NT", "NC.CT", "CC.CT", "NC.NT"),
                    spcat12mossn %in% c("Moderate Drought", "Normal", "Moderately Wet"),
                    nfert %in% c("Recommended N")) %>%
  drop_na(spcat12mossn)



# test for sig. differences

sedlm <- lm(sed.ssn~cc:till:spcat12mossn, data=spsedrep)
summary(sedlm)
# Call:
#   lm(formula = sed.ssn ~ cc * till * spcat12mossn, data = spsedrep)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -150.09  -42.33  -18.08   27.78  400.89 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                              53.167      3.157  16.839  < 2e-16 ***
#   ccNC                                     59.331      4.465  13.288  < 2e-16 ***
#   tillNT                                  -31.040      4.465  -6.952 3.80e-12 ***
#   spcat12mossnModerately Wet               34.488      4.835   7.133 1.04e-12 ***
#   spcat12mossnNormal                       17.971      3.484   5.158 2.53e-07 ***
#   ccNC:tillNT                             -38.121      6.315  -6.037 1.62e-09 ***
#   ccNC:spcat12mossnModerately Wet           5.587      6.837   0.817   0.4138    
# ccNC:spcat12mossnNormal                  -3.703      4.927  -0.752   0.4524    
# tillNT:spcat12mossnModerately Wet       -13.458      6.837  -1.968   0.0491 *  
#   tillNT:spcat12mossnNormal                -9.272      4.927  -1.882   0.0599 .  
# ccNC:tillNT:spcat12mossnModerately Wet   -1.107      9.670  -0.114   0.9089    
# ccNC:tillNT:spcat12mossnNormal            5.413      6.968   0.777   0.4372    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 68.3 on 11856 degrees of freedom
# Multiple R-squared:  0.2234,	Adjusted R-squared:  0.2227 
# F-statistic: 310.1 on 11 and 11856 DF,  p-value: < 2.2e-16


sedaov <- aov(sed.ssn~cc:till:spcat12mossn, data=spsedrep)
summary(sedaov)
# > summary(sedaov)
# Df   Sum Sq Mean Sq F value Pr(>F)    
# cc:till:spcat12mossn    11 15913738 1446703   310.1 <2e-16 ***
#   Residuals            11856 55312548    4665                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukout <- TukeyHSD(sedaov)
cld <- multcompLetters4(sedaov, Tukout)
# table with letters
cld <- as.data.frame.list(cld$`cc:till:spcat12mossn`)

# get means and SEs summary  #season, nfert, and management-name are now moot
spsedrep.sum <-  group_by(spsedrep, spcat12mossn, cctill, till, cc) %>%  # get the mean sed loss across all sites, across all years of each spcat
  summarize(sed.ssnmn = mean(sed.ssn),
            sed.ssnse = se(sed.ssn))  %>%  
  arrange(desc(sed.ssnmn))

spsedrep.sum$cld <- cld$Letters

# convert kg/ha to lb per acre
spsedrep.sum$sed.ssnmn.lbac <- spsedrep.sum$sed.ssnmn*2.20462/2.47105
spsedrep.sum$sed.ssnse.lbac <- spsedrep.sum$sed.ssnse*2.20462/2.47105

####################################  sed ~ cc*till*spei plot
windows(xpinch=200, ypinch=200, width=5, height=5)


# pal2 <- c("#6f3112","#dfb1a3")
pal2 <- c("#004a23", "#669947")
pal5 <- c( "#f67e4b","#FEDa8B", "#eaeccc", "#C2e4ef","#6ea6cd")

ggplot(data=spsedrep.sum, aes(x=factor(cctill, levels=c("NC.CT", "NC.NT", "CC.CT", "CC.NT")), 
                           y=sed.ssnmn.lbac)) + # 
  geom_col_pattern(aes(pattern_density=till, fill=cc), pattern="stripe", alpha=0.7,
                   pattern_fill="white", pattern_colour="white", color="white", #width=0.7,
                   show.legend=F) +
  scale_pattern_density_manual(values=c(0.05, 0)) +
  scale_fill_manual(values=pal2) +
  geom_errorbar(aes( ymin=sed.ssnmn.lbac-sed.ssnse.lbac, ymax=sed.ssnmn.lbac + sed.ssnse.lbac), width=0.2, linewidth=0.8, color="#20243d") +
  facet_grid2(rows=vars(factor(spcat12mossn, levels=c("Moderate Drought", "Normal", "Moderately Wet"))), # rows=vars(factor(till, levels=c("CT", "NT")), factor(cc, levels=c("NC", "CC"))),
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
  ylab("2022-2072 mean sediment yield (pounds per acre)") +
  xlab("Management") +
  geom_text(aes(y=sed.ssnmn.lbac + 20, label=cld), size=5) + 
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

ggsave("plots/resilience/sediments/IL_sed mean spring SPEI ccxtill withletters simpler.png", width=4.5, height=6, dpi=300) 














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