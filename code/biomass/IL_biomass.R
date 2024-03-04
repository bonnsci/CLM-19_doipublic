# Biomass data from Regrow DNDC simulations
# this script adds some dummy variables to the data
# summarizes corn grain biomass C data by mean, se, cv
# makes bar charts by decade and summarizing across all years
# runs ANOVA to look for significant differences in mean and cv between till*cc*N management across all years

library(tidyverse) # has ggplot, dplyr, etc.
library(broom) # for glance()
# install.packages("car")
library(car) # for qqp
# install.packages("multcompView") 
library(multcompView) # for tukey HSD letters

################### EXTRACT DATA BY SITE (CAN SKIP UNLESS NEED TO UPDATE DATA FROM GOOGLE DRIVE)

# bm <- read.csv("data/biomass/biomass.csv")
# From Regrow's doc "AFT Final Delivery Modeling Notes"
# This file includes maximum aboveground biomass of each crop grown in each management system. 
# Management_name maps directly to the file yearly_outputs_post-weighting.csv as described in 3.2.

# Column Name       Unit          Description
# site_name         NA            String combination of one of the locations we simulated on. In the format of {x}_{y} where x refers to the region and y refers to the numeric id of that site. These will be of the format of h_{}, v_{}, a_{}, f_{}, IL-n_{}, IL-s_{} which represents the points for hops, vineyards, almonds, and forage, Illinois North, Illinois South respectively
# management_name   NA            Management code as associated with the simulations described in section 2.{x}.1
# climate_scenario  NA            Either ‘rcp26’ or ‘rcp60’ that were defined in section 1.2
# crop_system_name  NA            Name of the entire system results are for. Will be one of the following:  alfalfa, almond-a, almond-c, corn, corn-grain, corn-silage, grape-a, grape-c, hops-a, hops-c, soybean. Note that for the vine and tree crops the -c refers to the crop row, while the -a refers to the alley row.
# crop_name         NA            Name of crop within the rotation of crop system name. 
# Year              integer       Calendar year in which results are for.
# Grain.C.kgC.ha    kg C /ha      Kilograms of Carbon per hectare of grain as part of the crop.
# Leaf.C.kgC.ha     kg C /ha      Kilograms of Carbon per hectare of leaf as part of the crop
# Stem.C.kgC.ha     kg C /ha      Kilograms of Carbon per hectare of stem as part of the crop.

# interesting, so we need to convert kg C to kg dry biomass
# can probably find common %C of dry grain biomass


###############
###############
###############
############### START HERE

#######################   
#######################   (1) set up the data
#######################   

bmil <- read.csv("data/biomass/biomass_IL.csv")

# DO NOT need to find literature values to convert kg C grain to kg grain.
# since we'll only be looking at them RELATIVE TO EACH OTHER, not absolute amounts...
# let's look at the C results for now
# 
# min(bmil$Year) # 2013 --2013-2021 start up years, start at 2022
# max(bmil$Year) # 2073

# looks like all the cover crop biomass is in leaf and stem, zero grain

# make factor for CT, RT, NT
bmil$till <- ifelse(grepl("ct-", bmil$management_name), "CT", 
                    ifelse(grepl("rt-", bmil$management_name), "RT", "NT"))
# # check
# unique(bmil$till)

# factor for CC or NC
bmil$cc <- ifelse(grepl("-cc-", bmil$management_name), "CC", "NC")
# # check
# unique(bmil$cc)

# factor for N treatment
bmil$nfert <- ifelse(grepl("-cn", bmil$management_name), "High N", 
                     ifelse(grepl("-fn", bmil$management_name), "Fall N","Recommended N"))
# # check
# unique(bmil$nfert)

# dummy for decade
bmil$decade <- ifelse(bmil$Year <2021, "2010s",
                      ifelse(bmil$Year>=2021 & bmil$Year <2031, "2020s",
                        ifelse(bmil$Year>=2031 & bmil$Year <2041, "2030s",
                          ifelse(bmil$Year>=2041 & bmil$Year <2051, "2040s",
                            ifelse(bmil$Year>=2051 & bmil$Year <2061, "2050s",
                              ifelse(bmil$Year>=2061 & bmil$Year <2071, "2060s", "2070s"))))))
# unique(bmil$decade)


# # add in summer precip data -- only needed if doing regression analysis, not pursued at this time
# precip <- read.csv("data/climate data not large/precipIL.csv")
# 
# precip.summer <- precip[precip$season == "Summer",]
# precip.summer <- precip.summer[,c(1:3, 5:7)]
# colnames(precip.summer)[1:2] <- c("site_name", "climate_scenario")
# 
# bmil <- left_join(bmil, precip.summer, 
#                    relationship = "many-to-one", 
#                    by = join_by(site_name, climate_scenario, Year==year))


# unique(bmil$management_name)

windows(xpinch=200, ypinch=200, width=5, height=5)

se <- function(x) sd(x) / sqrt(length(x))
cv <- function(x) sd(x) / mean(x)

# is the grain C biomass significantly different with High N vs Recommended N?
# let's look at biomass by decade
biomass_summary <- bmil %>%
  group_by(climate_scenario,crop_name, cc, till, nfert, decade) %>%
  summarize(biomass_mean = mean(Grain.C.kgC.ha.), biomass_se = se(Grain.C.kgC.ha.))




windows(xpinch=200, ypinch=200, width=5, height=5)
ggplot(data=biomass_summary[#biomass_summary$till=="NT" & # just look at NT for simplicity sake for now
  biomass_summary$climate_scenario == "rcp60" &                            
  biomass_summary$crop_name %in% c("corn, grain"),], #exclude rye, soybeans
                             # biomass_summary$nfert %in% c("High N", "Recommended N"),],  # exclude fall N for now
       aes(x=decade, y=biomass_mean, fill=nfert)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288") +
  geom_errorbar(width=0.3, aes(ymin=biomass_mean - biomass_se, ymax=biomass_mean + biomass_se),  
                               position=position_dodge(0.9),
                                color="#332288") +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT", "RT"))), 
             cols=vars(factor(cc, levels=c("CC", "NC"))), 
             labeller = as_labeller(
               c(CC="Has Cover Crop", NC="No Cover Crop",
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  xlab("Decade") +
  ylab("Corn grain (kg/ha)") +
  scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99" ), name="N management") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

# ggsave("plots/biomass/IL_corn_biomass_bars.png", width=12, height=8, dpi=300)






#######################   
#######################   (2) is mean corn grain C different among till*cc*Nfert groups across all years?
#######################   

corndat <- bmil[bmil$crop_name=="corn, grain" & bmil$climate_scenario=="rcp60",]


Neffect <- aov(Grain.C.kgC.ha.~till*cc*nfert, data=corndat)
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
qqnorm(corndat$Grain.C.kgC.ha.)
qqline(corndat$Grain.C.kgC.ha.)
hist(corndat$Grain.C.kgC.ha.-mean(corndat$Grain.C.kgC.ha.))
# look ok

# Equality of variances 
ggplot(data=corndat, aes(x=management_name, y=Grain.C.kgC.ha.)) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))
# looks ok, some possible outliers in nt-cc-fn
leveneTest(Grain.C.kgC.ha. ~ cc*till*nfert, data=corndat)
# > leveneTest(Grain.C.kgC.ha. ~ management_name, data=corndat)
# Levene's Test for Homogeneity of Variance (center = median)
#          Df F value    Pr(>F)    
# group    17  24.503 < 2.2e-16 ***   # we REJECT the null hyp (no sig. diff. between variances of the samples)
#       34542                      
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
summary(corndat$Grain.C.kgC.ha.)
# > summary(corndat$Grain.C.kgC.ha.)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 139.9  1945.6  2875.9  2915.8  3924.3  5794.6 

# outliers via histograms
hist(corndat$Grain.C.kgC.ha., breaks=sqrt(nrow(corndat)))

ggplot(data=corndat, aes(x=Grain.C.kgC.ha.)) +
         geom_histogram() +
         facet_grid(rows=vars(nfert), cols=vars(till, cc))

#outliers via boxplots
boxplot(Grain.C.kgC.ha.~management_name, data=corndat) # some outliers plotted for nt-cc-fn, 
# but not far from interquartile range for nt-cc-fn, and not outside IQR for many other groups


# outliers via z-scores
corndat$z_grainC <- scale(corndat$Grain.C.kgC.ha.)
hist(corndat$z_grainC)
summary(corndat$z_grainC)
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

corndat$till <- factor(corndat$till)
corndat$cc <- factor(corndat$cc)
corndat$nfert <- factor(corndat$nfert)

Neffect <- aov(Grain.C.kgC.ha.~till*cc*nfert, data=corndat)
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
cornsum <- group_by(corndat, cc, till, nfert) %>%
  summarize(mean=mean(Grain.C.kgC.ha.), 
            se=se(Grain.C.kgC.ha.)) %>%
  arrange(desc(mean))

cld <- as.data.frame.list(cld$`till:cc:nfert`)
cornsum$cld <- cld$Letters

# cornsum

# use these letters on this plot:

windows(xpinch=200, ypinch=200, width=5, height=5)



ggplot(data=cornsum[cornsum$till %in% c("CT", "NT"),],aes(x=nfert, y=mean, fill=nfert)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288", show.legend=F) +
  geom_errorbar(width=0.3, aes(ymin=mean-se, ymax=mean + se),  
                position=position_dodge(0.9),
                color="#332288") +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT"))), 
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             labeller = as_labeller(
               c(NC="No Cover Crop", CC="Rye Cover Crop", 
                 "CT" = "Conventional Till", "NT" = "No Till"))) +
  xlab("N management") +
  ylab(expression(bold('2022-2072 mean corn grain biomass (kg C ha'^-1*')'))) + 
  scale_x_discrete(breaks=c("Fall N", "High N", "Recommended N"),
                   labels = c("Fall N", "High N", "Recomm. N")) +
  ylim(0,3800)+
  geom_text(aes(label=cld, y=mean+(2*se)), vjust=-0.5,
            color="gray20", size=4, fontface="bold") +
  scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99" )) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-10, hjust=0, size=11),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))
# # strip.background=element_rect(fill="lightblue", color="black", size=1) 

# ggsave("plots/biomass/IL_corn_biomass_Neffect.png", width=5, height=5, dpi=300)


# Same as graph above but shown as % difference from 2022 mean yield

mn <- mean(cornsum$mean)

cornsum$pdiff <- (cornsum$mean - mn)/cornsum$mean
cornsum$pdiff.se <- (cornsum$se)/cornsum$mean

cornsum
# # A tibble: 18 × 8
# # Groups:   cc, till [6]
# cc    till  nfert          mean    se cld     pdiff pdiff.se
# <fct> <fct> <fct>         <dbl> <dbl> <chr>   <dbl>    <dbl>
#   1 CC    NT    High N        3362.  27.7 a      0.133   0.00823
# 2 CC    NT    Recommended N 3361.  25.5 a      0.132   0.00759
# 3 NC    NT    High N        3164.  27.4 b      0.0785  0.00867
# 4 CC    RT    High N        3155.  27.3 bc     0.0757  0.00865
# 5 NC    NT    Recommended N 3141.  26.4 bc     0.0717  0.00841
# 6 CC    RT    Recommended N 3070.  23.7 bcd    0.0502  0.00772
# 7 CC    CT    High N        3062.  26.8 bcd    0.0477  0.00875
# 8 NC    RT    High N        3029.  27.4 cd     0.0372  0.00903
# 9 NC    CT    High N        3005.  27.3 d      0.0296  0.00909
# 10 NC    RT    Recommended N 2995.  26.6 d      0.0263  0.00887
# 11 NC    CT    Recommended N 2975.  26.7 d      0.0200  0.00899
# 12 CC    CT    Recommended N 2975.  23.8 d      0.0199  0.00798
# 13 NC    CT    Fall N        2772.  27.7 e     -0.0520  0.00998
# 14 NC    RT    Fall N        2764.  27.5 e     -0.0550  0.00994
# 15 NC    NT    Fall N        2759.  27.1 e     -0.0567  0.00982
# 16 CC    CT    Fall N        2336.  26.8 f     -0.248   0.0115 
# 17 CC    NT    Fall N        2284.  22.8 f     -0.277   0.0100 
# 18 CC    RT    Fall N        2277.  25.6 f     -0.280   0.0112 

# because it is percents we don't have to convert to lb / ac

pal3 <- c("#20243d","#C2e4ef", "#669947")   # )

ggplot(data=cornsum[cornsum$till %in% c("CT", "NT"),],aes(x=nfert, y=pdiff, fill=nfert)) +
  geom_bar(stat="identity", position=position_dodge(), show.legend=F) +
  geom_errorbar(width=0.3, aes(ymin=pdiff-pdiff.se, ymax=pdiff+pdiff.se),
                position=position_dodge(0.9),
                color="#20243d") +
  facet_grid( # rows=vars(factor(till, levels=c("CT", "NT"))), 
    cols=vars(factor(cc, levels=c("NC", "CC")),factor(till, levels=c("CT", "NT"))), 
    labeller = as_labeller(
      c(NC="No Cover Crop", CC="Rye Cover Crop", 
        "CT" = "Conventional Till", "NT" = "No Till"))) +
  geom_hline(yintercept=0, color="#20243d") +
  xlab("N management") +
  ylab(expression(bold('Corn yield % difference from overall average corn yield'))) + 
  scale_x_discrete(breaks=c("Fall N", "High N", "Recommended N"),
                   labels = c("Fall N", "High N", "Recomm. N")) +
  scale_y_continuous(labels=scales::percent_format()) +
  # ylim(0,3800)+
  geom_text(aes(label=cld, y=ifelse(pdiff>0, pdiff+0.02, pdiff-0.04)), 
            vjust=-0.5,
            color="gray20", size=4, fontface="bold") +
  scale_fill_manual(values=pal3) +  #c("#CC6677","#99DDFF", "#44AA99" )) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-10, hjust=0, size=11),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))
# # strip.background=element_rect(fill="lightblue", color="black", size=1) 

ggsave("plots/biomass/IL_corn_biomass_percent diff global mean_with letters.png", width=6, height=4, dpi=300)


# NC + CT + high N compared to CC + NT + recommended N
(cornsum$mean[cornsum$cc=="NC" & cornsum$till=="CT" & cornsum$nfert=="High N"] -
    cornsum$mean[cornsum$cc=="CC" & cornsum$till=="NT" & cornsum$nfert=="Recommended N"])/
  cornsum$mean[cornsum$cc=="NC" & cornsum$till=="CT" & cornsum$nfert=="High N"]








# same graph as above but expressed as % of Fall N

# re-arrange data to calculate % of fall N yield
falln <- filter(cornsum, nfert=="Fall N") %>%
  rename("mean.falln" = "mean", "se.falln" = "se") %>%
  select(cc, till, mean.falln, se.falln)

springn <- filter(cornsum, !nfert=="Fall N")

cornsum2 <- left_join(springn, falln)


pal2 <- c("#004a23", "#669947")
# pal2 <- c("#20243d", "#669947")

# Calculate % difference from fall N. So fall N = 100%. how much bigger are the others?
cornsum2$mean.pfalln <- (cornsum2$mean/cornsum2$mean.falln)-1
cornsum2$se.pfalln <- (cornsum2$se/cornsum2$mean.falln)

ggplot(data=cornsum2[cornsum2$till %in% c("CT", "NT") & 
                       cornsum2$nfert %in% c("High N", "Recommended N"),],
       aes(x=nfert, y=mean.pfalln, fill=nfert)) +
  geom_bar(stat="identity", position=position_dodge(),  show.legend=F) +  # color="#20243d",
  geom_errorbar(width=0.3, aes(ymin=mean.pfalln-se.pfalln, ymax=mean.pfalln + se.pfalln),  
                position=position_dodge(0.9),
                color="#20243d") +
  facet_grid( # rows=vars(factor(till, levels=c("CT", "NT"))), 
             cols=vars(factor(cc, levels=c("NC", "CC")),factor(till, levels=c("CT", "NT"))), 
             labeller = as_labeller(
               c(NC="No Cover Crop", CC="Rye Cover Crop", 
                 "CT" = "Conventional Till", "NT" = "No Till"))) +
  xlab("N management") +
  scale_y_continuous(labels=scales::percent_format()) +
  ylab(expression(bold("Corn yield % difference from fall applied N"))) + 
  scale_x_discrete(breaks=c("High N", "Recommended N"),
                   labels = c("High N", "Recomm. N")) +
  # ylim(0,3800)+
  # geom_text(aes(label=cld, y=mean+(2*se)), vjust=-0.5,
            # color="gray20", size=4, fontface="bold") +
  scale_fill_manual(values=pal2) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-10, hjust=0, size=11),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=11, face="bold"),
    strip.text=element_text(face="bold", size=11))
# # strip.background=element_rect(fill="lightblue", color="black", size=1) 

ggsave("plots/biomass/IL_corn_biomass_percdiff from Fall N.png", width=6, height=3, dpi=300)




#######################   
#######################   (3) is variability in corn grain C different among till*cc*Nfert groups across all years?
#######################   

# calculate CV for each site
corncv<- group_by(corndat, site_name, till, cc, nfert) %>%
  summarize(cv=cv(Grain.C.kgC.ha.)) %>%
  arrange(desc(cv))


Neffect <- aov(cv~till*cc*nfert, data=corncv)
summary(Neffect)
Tukout <- TukeyHSD(Neffect)


# assumptions - following https://statsandr.com/blog/anova-in-r

# Independence - the observations (grain.c.kgc.ha. observations) are independent, one does
# not depend on the other, they're not dependent on some other variable like, 
# from one individual came multiple observations.

# Normality - not required with large enough sample size >30.  But we can test anyway. 
# the residuals (observed - mean for that group) should be normally distributed.
qqnorm(corncv$cv) # looks really good
qqline(corncv$cv)
hist(corncv$cv-mean(corncv$cv)) # looks ok

# Equality of variances - assumption not met
ggplot(data=corncv, aes(x=nfert, y=cv)) + 
  geom_boxplot() +
  facet_grid(rows=vars(till), cols=vars(cc)) 
# looks ok, some possible outliers in nt-nc-fn on the low end, and rt-cc-fn, also on low end
leveneTest(cv ~ cc*till*nfert, data=corncv) 
# > leveneTest(Grain.C.kgC.ha. ~ management_name, data=corndat)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group  17  1.4426  0.111  # Accept null hypothesis that variances do not differ
#       558 

# outliers
summary(corncv$cv)
# > summary(corndat$Grain.C.kgC.ha.)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 139.9  1945.6  2875.9  2915.8  3924.3  5794.6 

# outliers via histograms
hist(corncv$cv, breaks=sqrt(nrow(corncv)))

ggplot(data=corncv, aes(x=cv)) +
  geom_histogram() +
  facet_grid(rows=vars(nfert), cols=vars(till, cc))

#outliers via boxplots
boxplot(cv~till+cc+nfert, data=corncv) # no outliers plotted 

# outliers via z-scores
corncv$z_cv <- scale(corncv$cv)
hist(corncv$z_cv)
summary(corncv$z_cv)
# V1          
# Min.   :-2.92849  
# 1st Qu.:-0.67960  
# Median :-0.06132  
# Mean   : 0.00000  
# 3rd Qu.: 0.79708  
# Max.   : 1.96883  
# < -2 or >2 is considered rare
# < -3 or >3 is extremely rare
# < -3.29 or > -3.29 is used to detect outliers, where one out of 1000 observations will be outside
# this range if normal distribution.
# interquartile range is > -2 and <2. 
# max is in the safe zone
# min is getting close to extremely rare.

## outlier summary: boxplots, levenes test, and z-score suggest no extreme outliers. No justification for removing.

cvtest <- aov(cv~till*cc*nfert, data=corncv)
summary(cvtest)
# Df Sum Sq Mean Sq F value   Pr(>F)
# till            2 0.0756 0.03780  18.959 1.08e-08 ***
#   cc              1 0.0577 0.05772  28.950 1.09e-07 ***
#   nfert           2 0.0722 0.03608  18.098 2.42e-08 ***
#   till:cc         2 0.0010 0.00049   0.245    0.782    
# till:nfert      4 0.0150 0.00375   1.880    0.112    
# cc:nfert        2 0.0457 0.02284  11.454 1.33e-05 ***
#   till:cc:nfert   4 0.0036 0.00090   0.454    0.770    
# Residuals     558 1.1125 0.00199                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukoutcv <- TukeyHSD(cvtest)

# compact letter display
cldcv <- multcompLetters4(cvtest, Tukoutcv)

# table with letters and 3rd quantile
corncvsum <- group_by(corncv, cc, till, nfert) %>%
  summarize(mean=mean(cv), 
            se=se(cv)) %>%
  arrange(desc(mean))

cldcv <- as.data.frame.list(cldcv$`till:cc:nfert`)
corncvsum$cld <- cldcv$Letters

corncvsum

# use these letters on this plot:

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=corncvsum[corncvsum$till %in% c("CT", "NT"),],
       aes(x=nfert, y=mean, fill=nfert)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288", show.legend=F, alpha=0.7) +
  geom_errorbar(width=0.3, aes(ymin=mean-se, ymax=mean + se),  
                position=position_dodge(0.9),
                color="#332288") +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT"))),  # "RT"))), 
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             labeller = as_labeller(
               c(NC="No Cover Crop", CC="Has Cover Crop", 
                 "CT" = "Conventional Till", "NT" = "No Till"))) + #, "RT"="Reduced Till"))) +
  xlab("N management") +
  ylab(expression(bold('2022-2072 CV corn grain biomass'))) + 
  scale_x_discrete(breaks=c("Fall N", "High N", "Recommended N"),
                   labels = c("Fall N", "High N", "Recomm. N")) +
  ylim(0,0.45)+
  geom_text(aes(label=cld, y=mean+(2*se)), vjust=-0.5,
            color="gray20", size=4, fontface="bold") +
  scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99" )) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-10, hjust=0, size=11),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))
# # strip.background=element_rect(fill="lightblue", color="black", size=1) 

ggsave("plots/biomass/IL_corn_biomass_cv.png", width=5, height=5, dpi=300)







#######################   
#######################   (4) is mean soy grain C different among till*cc*Nfert groups across all years?
#######################   

soydat <- bmil[bmil$crop_name=="soybean" & bmil$climate_scenario=="rcp60",]

Neffect <- aov(Grain.C.kgC.ha.~till*cc*nfert, data=soydat)
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
qqnorm(soydat$Grain.C.kgC.ha.)
qqline(soydat$Grain.C.kgC.ha.)
hist(soydat$Grain.C.kgC.ha.-mean(soydat$Grain.C.kgC.ha.))
# look ok

# Equality of variances 
ggplot(data=soydat, aes(x=management_name, y=Grain.C.kgC.ha.)) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))
# looks ok
leveneTest(Grain.C.kgC.ha. ~ cc*till*nfert, data=soydat)
# Levene's Test for Homogeneity of Variance (center = median)
#          Df F value  Pr(>F)  
# group    17  1.8054 0.02181 *
#       34542                  
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
summary(soydat$Grain.C.kgC.ha.)
# > summary(soydat$Grain.C.kgC.ha.)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 17.22  419.89  637.90  649.91  877.61 1480.36 

# outliers via histograms
hist(soydat$Grain.C.kgC.ha., breaks=sqrt(nrow(soydat)))

ggplot(data=soydat, aes(x=Grain.C.kgC.ha.)) +
  geom_histogram() +
  facet_grid(rows=vars(nfert), cols=vars(till, cc))

#outliers via boxplots
boxplot(Grain.C.kgC.ha.~management_name, data=soydat) # looks ok


# outliers via z-scores
soydat$z_grainC <- scale(soydat$Grain.C.kgC.ha.)
hist(soydat$z_grainC)
summary(soydat$z_grainC)
# V1         
# Min.   :-2.0914  
# 1st Qu.:-0.7604  
# Median :-0.0397  
# Mean   : 0.0000  
# 3rd Qu.: 0.7527  
# Max.   : 2.7451  
# < -2 or >2 is considered rare
# < -3 or >3 is extremely rare
# < -3.29 or > -3.29 is used to detect outliers, where one out of 1000 observations will be outside
# this range if normal distribution.
# min. and max do not reach 3 or 3.29.

## outlier summary: boxplots and z-score suggest no extreme outliers. No justification for removing.

# re-run ANOVA with the above knowledge (assumptions met)

soydat$till <- factor(soydat$till)
soydat$cc <- factor(soydat$cc)
soydat$nfert <- factor(soydat$nfert)

Neffect <- aov(Grain.C.kgC.ha.~till*cc*nfert, data=soydat)
summary(Neffect)
# Df    Sum Sq Mean Sq F value   Pr(>F)    
# till              2 5.366e+06 2683154  29.358 1.82e-13 ***
# cc                1 3.326e+05  332550   3.639   0.0565 .  
# nfert             2 2.328e+03    1164   0.013   0.9873    
# till:cc           2 3.333e+04   16667   0.182   0.8333    
# till:nfert        4 5.440e+02     136   0.001   1.0000    
# cc:nfert          2 1.164e+05   58208   0.637   0.5289    
# till:cc:nfert     4 2.558e+03     639   0.007   0.9999    
# Residuals     34542 3.157e+09   91394                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Neffect <- aov(Grain.C.kgC.ha.~till, data=soydat)
summary(Neffect)

Tukout <- TukeyHSD(Neffect)

# compact letter display
cld <- multcompLetters4(Neffect, Tukout)   

# table with letters and 3rd quantile
soysum <- group_by(soydat, cc, till, nfert) %>%
  summarize(mean=mean(Grain.C.kgC.ha.), 
            se=se(Grain.C.kgC.ha.)) %>%
  arrange(desc(mean))

cld <- as.data.frame.list(cld$`till:cc:nfert`)
soysum$cld <- cld$Letters

# may need to summarize data now just across tillage treatments to go with the model output
# left off here 10/26/23

# soysum

# use these letters on this plot:

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=soysum,aes(x=nfert, y=mean, fill=nfert)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288", show.legend=F) +
  geom_errorbar(width=0.3, aes(ymin=mean-se, ymax=mean + se),  
                position=position_dodge(0.9),
                color="#332288") +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT", "RT"))), 
             cols=vars(factor(cc, levels=c("CC", "NC"))), 
             labeller = as_labeller(
               c(CC="Has Cover Crop", NC="No Cover Crop",
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  xlab("N management") +
  ylab("Mean soy grain (kg C/ha) 2022-2072") +
  ylim(0,750)+
  geom_text(aes(label=cld, y=mean+(2*se)), vjust=-0.5) +
  scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99" )) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/biomass/IL_soy_biomass_Neffect.png", width=6, height=8, dpi=300)





#######################   
#######################   (5) is variability in soy grain C different among till*cc*Nfert groups across all years?
#######################   

# calculate CV for each site
soycv<- group_by(soydat, site_name, till, cc, nfert) %>%
  summarize(cv=cv(Grain.C.kgC.ha.)) %>%
  arrange(desc(cv))


Neffect <- aov(cv~till*cc*nfert, data=soycv)
summary(Neffect)
Tukout <- TukeyHSD(Neffect)


# assumptions - following https://statsandr.com/blog/anova-in-r

# Independence - the observations (grain.c.kgc.ha. observations) are independent, one does
# not depend on the other, they're not dependent on some other variable like, 
# from one individual came multiple observations.

# Normality - not required with large enough sample size >30.  But we can test anyway. 
# the residuals (observed - mean for that group) should be normally distributed.
qqnorm(soycv$cv) # looks really good
qqline(soycv$cv)
hist(soycv$cv-mean(soycv$cv)) # looks ok

# Equality of variances - assumption not met
ggplot(data=soycv, aes(x=nfert, y=cv)) + 
  geom_boxplot() +
  facet_grid(rows=vars(till), cols=vars(cc)) 
# looks ok, some possible outliers in nt-nc-fn on the low end, and rt-cc-fn, also on low end
leveneTest(cv ~ cc*till*nfert, data=soycv) 
# > leveneTest(Grain.C.kgC.ha. ~ management_name, data=soydat)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group  17  1.4426  0.111  # Accept null hypothesis that variances do not differ
#       558 

# outliers
summary(soycv$cv)
# > summary(soydat$Grain.C.kgC.ha.)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 139.9  1945.6  2875.9  2915.8  3924.3  5794.6 

# outliers via histograms
hist(soycv$cv, breaks=sqrt(nrow(soycv)))

ggplot(data=soycv, aes(x=cv)) +
  geom_histogram() +
  facet_grid(rows=vars(nfert), cols=vars(till, cc))

#outliers via boxplots
boxplot(cv~till+cc+nfert, data=soycv) # no outliers plotted 

# outliers via z-scores
soycv$z_cv <- scale(soycv$cv)
hist(soycv$z_cv)
summary(soycv$z_cv)
# V1          
# Min.   :-2.92849  
# 1st Qu.:-0.67960  
# Median :-0.06132  
# Mean   : 0.00000  
# 3rd Qu.: 0.79708  
# Max.   : 1.96883  
# < -2 or >2 is considered rare
# < -3 or >3 is extremely rare
# < -3.29 or > -3.29 is used to detect outliers, where one out of 1000 observations will be outside
# this range if normal distribution.
# interquartile range is > -2 and <2. 
# max is in the safe zone
# min is getting close to extremely rare.

## outlier summary: boxplots, levenes test, and z-score suggest no extreme outliers. No justification for removing.

cvtest <- aov(cv~till*cc*nfert, data=soycv)
summary(cvtest)
# Df Sum Sq Mean Sq F value   Pr(>F)
# till            2 0.0756 0.03780  18.959 1.08e-08 ***
#   cc              1 0.0577 0.05772  28.950 1.09e-07 ***
#   nfert           2 0.0722 0.03608  18.098 2.42e-08 ***
#   till:cc         2 0.0010 0.00049   0.245    0.782    
# till:nfert      4 0.0150 0.00375   1.880    0.112    
# cc:nfert        2 0.0457 0.02284  11.454 1.33e-05 ***
#   till:cc:nfert   4 0.0036 0.00090   0.454    0.770    
# Residuals     558 1.1125 0.00199                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukoutcv <- TukeyHSD(cvtest)

# compact letter display
cldcv <- multcompLetters4(cvtest, Tukoutcv)

# table with letters and 3rd quantile
soycvsum <- group_by(soycv, cc, till, nfert) %>%
  summarize(mean=mean(cv), 
            se=se(cv)) %>%
  arrange(desc(mean))

cldcv <- as.data.frame.list(cldcv$`till:cc:nfert`)
soycvsum$cld <- cldcv$Letters

soycvsum

# use these letters on this plot:

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=soycvsum,aes(x=nfert, y=mean, fill=nfert)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288", show.legend=F) +
  geom_errorbar(width=0.3, aes(ymin=mean-se, ymax=mean + se),  
                position=position_dodge(0.9),
                color="#332288") +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT", "RT"))), 
             cols=vars(factor(cc, levels=c("CC", "NC"))), 
             labeller = as_labeller(
               c(CC="Has Cover Crop", NC="No Cover Crop",
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  xlab("N management") +
  ylab("CV for soy grain (kg C/ha) 2022-2072") +
  ylim(0,0.45)+
  geom_text(aes(label=cld, y=mean+(2*se)), vjust=-0.5) +
  scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99" )) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/biomass/IL_soy_biomass_cv.png", width=6, height=8, dpi=300)









############ linear models / regressions to look at biomass change over time (not complete as of 10/4/2023)
############ what drives biomass over time, not a main research question, i.e., seems redundant with
############ just looking at how the model is built to predict biomass...

# set base levels to no cover and conventional till
bmil$till <- relevel(factor(bmil$till), ref="CT")
# bmil$till <- ordered(bmil$till, levels=c("CT", "RT", "NT"))
# bmil$till <- factor(bmil$till, ordered=F)
bmil$cc <- relevel(factor(bmil$cc), ref="NC")

# center data at mean
center_scale <- function(x) {
  scale(x, scale=F)
}

bmil$year.sc <- center_scale(bmil$Year)
bmil$grain.sc <- center_scale(bmil$Grain.C.kgC.ha.)


grmean <- mean(bmil$Grain.C.kgC.ha.)
grsd <- sd(bmil$Grain.C.kgC.ha.)
ssntotmean <- mean(bmil$ssn.tot)
ssntotsd <- sd(bmil$ssn.tot)
yrmean <- mean(bmil$Year)
yrsd <- sd(bmil$Year)
bmil$grain.z <- (bmil$Grain.C.kgC.ha.-grmean)/grsd
bmil$ssn.tot.z <- (bmil$ssn.tot-ssntotmean)/ssntotsd
bmil$year.z <- (bmil$Year - yrmean)/yrsd

grain <- bmil[bmil$crop_name %in% c("corn, grain", "soybean") & bmil$Year>2021, ]



# check that data are normal
qqnorm(grain$Grain.C.kgC.ha.)  
qqline(grain$Grain.C.kgC.ha., distribution = qnorm)


# most of the explanatory power of the model comes from the two crops
blm0 <- lm(grain.z ~ year.z*crop_name, data=grain)
summary(blm0)
# R-squared is 0.6277, p< 2.2e-16
glance(blm0)
# AIC = 223463    # BIC 2.24e5  # when comparing these to other models, lower is better

# add in summer precip gain 0.15 in R2
blm0p <- lm(grain.z ~ year.z*crop_name + ssn.tot.z,  data=grain)
summary(blm0p)
# R-squared is 0.7756, p< 2.2e-16
glance(blm0p)
# AIC = 163956    # BIC 164014  

# add in cover crops
blm1p <- lm(grain.z ~ year.z*crop_name*cc + ssn.tot.z,  data=grain)
summary(blm1p)
# R-squared is 0.7759, p< 2.2e-16
glance(blm1p)
# AIC = 163836    # BIC 163933  


# add in tillage
blm2p <- lm(grain.z ~ year.z*crop_name*till + ssn.tot.z,  data=grain)
summary(blm2p)
# R-squared is 0.7769, p< 2.2e-16
glance(blm2p)
# AIC = 163324    # BIC 163460 


# add in site, a random effect


blm3p <- lmer(grain.z ~ year.z*crop_name + ssn.tot.z + (1|site_name),
              REML=F, # only one random effect, can use maximum likelihood not restricted ML
            data=grain)
summary(blm3p)
# R-squared is 0.79, p< 2.2e-16
glance(blm3p)
# AIC = 155646    # BIC 156004


# add in site, a random effect
blm3p <- lm(grain.z ~ year.z*crop_name + ssn.tot.z,  data=grain)
summary(blm3p)
# R-squared is 0.79, p< 2.2e-16
glance(blm3p)
# AIC = 155646    # BIC 156004





blm_fullp <- lm(grain.z ~ year.z*crop_name*climate_scenario*till*cc + ssn.tot.z, 
               data=bmil[bmil$crop_name %in% c("corn, grain", "soybean") & bmil$Year>2021, ])
summary(blm_fullp)
# R-squared is 0.7784, p< 2.2e-16
glance(blm_fullp)
# AIC = 162567 ### BEST AIC SO FAR   # BIC 163050  # BEST BIC SO FAR

