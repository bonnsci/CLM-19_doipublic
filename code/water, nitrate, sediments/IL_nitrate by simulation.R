
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
# ndat <- read.csv("data/large_data/daily N/IL_corn_day_soil_n.csv") 
# N UNITS are in kg N / ha per day

# sum data by year
# ndatyr <- ndat %>%
#   group_by(site_name, crop_system_name, management_name, climate_scenario, Year) %>%
#   summarize(N2O.yr = sum(N2O.flux), NO3.yr = sum(NO3.leach))
# 
# # clean up
# rm(ndat)

# colnames(ndatyr)[c(3,5)] <- c("management", "year")

# write.csv(ndatyr, "data/water, nitrate, sediments/nitrate_IL_annualtotals.csv", row.names=F)

ndatyr <- read.csv("data/water, nitrate, sediments/IL_nitrate_annualtotals.csv")


ndatyr <- ndatyr[ndatyr$year>2021 & ndatyr$year<2073 & ndatyr$climate_scenario=="rcp60",]

ndatyr$till <- ifelse(grepl("ct-", ndatyr$management), "CT", 
                      ifelse(grepl("rt-", ndatyr$management), "RT", "NT"))
# # check
# unique(ndatyr$till)

# dummy for CC or NC
ndatyr$cc <- ifelse(grepl("-cc-", ndatyr$management), "CC", "NC")
# # check
# unique(ndatyr$cc)

# dummy for N treatment
ndatyr$nfert <- ifelse(grepl("-cn", ndatyr$management), "High N", 
                       ifelse(grepl("-fn", ndatyr$management), "Fall N","Recommended N"))
# # check
# unique(ndatyr$nfert)

# dummy for decade
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

ndatyr$crop <- ifelse(ndatyr$crop_system_name=="corn-soy" & ndatyr$year%%2 ==0, "soy",   # %%2 returns the remainder when divided by 2. if no remainder, then its an even number.
                      ifelse(ndatyr$crop_system_name=="corn-soy" & !ndatyr$year%%2 ==0, "corn",
                             ifelse(ndatyr$crop_system_name=="soy-corn" & ndatyr$year%%2 ==0, "corn",
                                    ifelse(ndatyr$crop_system_name=="soy-corn" & !ndatyr$year%%2 ==0, "soy", "X"))))

# check no Xs
# unique(ndatyr$crop)


# calculate totals and annual means ACROSS ALL YEARS and by decade
# first sum by site and crop name, then calculate mean across crop types per site, 
# Then mean and se across sites by treatments/management combinations

# TOTALS ACROSS ALL YEARS for rotation
ndat_tmttot <- ndatyr %>%
  group_by(site_name, crop_system_name, till, cc, nfert) %>%
  summarize(N2O.tot = sum(N2O.yr),  # this is two totals one for corn and one for  soy per year
            NO3.tot = sum(NO3.yr),
            N.tot = sum(N2O.yr) + sum(NO3.yr)) %>%
  group_by(site_name, till, cc, nfert) %>%
  summarize(N2O.sitemean = mean(N2O.tot),  # so here we take the mean across corn-soybeans at each site
            NO3.sitemean = mean(NO3.tot),
            Ntot.sitemean = mean(N.tot)) %>%
  group_by(till, cc, nfert) %>%
  summarize(N2O.mean = mean(N2O.sitemean), # mean total across sites in each treatment combo
            N2O.se = se(N2O.sitemean), # variability across sites in each treatment combo
            NO3.mean = mean(NO3.sitemean),
            NO3.se = se(NO3.sitemean),
            Ntot.mean = mean(Ntot.sitemean),
            Ntot.se = se(Ntot.sitemean))

# ANNUAL MEANS ACROSS ALL YEARS for rotation
ndat_tmtperyr <- ndatyr %>%
  group_by(site_name, crop_system_name, till, cc, nfert) %>%
  summarize(N2O.sitemean = mean(N2O.yr),  # mean annual N loss per site
            NO3.sitemean = mean(NO3.yr)) %>%
  group_by(crop_system_name, till, cc, nfert) %>%  # drop sitename to get mean across sites
  summarize(N2O.mean = mean(N2O.sitemean), # mean of the means across sites in each treatment combo
            N2O.se = se(N2O.sitemean), # variability across sites in each treatment combo
            NO3.mean = mean(NO3.sitemean),
            NO3.se = se(NO3.sitemean))



# TOTALS BY DECADE - doesn't really work b/c 2020s and 2070s have fewer years
# could do by 10 year intervals in the data if necessary
# but I think annual means are where its at anyway

# ANNUAL MEANS BY DECADE for rotation
ndat_dec <- ndatyr %>%
  group_by(site_name, crop_system_name, till, cc, nfert, decade) %>%
  summarize(N2O.sitemean = mean(N2O.yr),  # mean annual N loss per site per decade, per corn-soy and soy-corn, 
            NO3.sitemean = mean(NO3.yr)) %>%
  group_by(till, cc, nfert, decade) %>%  # drop sitename and crop system name to get means across sites and rotations
  summarize(N2O.mean = mean(N2O.sitemean), # mean across sites in each treatment combo
            N2O.se = se(N2O.sitemean), # variability across sites in each treatment combo
            NO3.mean = mean(NO3.sitemean),
            NO3.se = se(NO3.sitemean))



# ANNUAL MEANS BY DECADE **by crop




ndat_dec_crop <- ndatyr %>%
  group_by(site_name, crop, till, cc, nfert, decade) %>%
  summarize(N2O.sitemean = mean(N2O.yr),  # mean annual N loss for decade per site
            NO3.sitemean = mean(NO3.yr)) %>%
  group_by(crop, till, cc, nfert, decade) %>%  # drop sitename to get mean across sites
  summarize(N2O.mean = mean(N2O.sitemean), # mean across sites in each treatment combo
            N2O.se = se(N2O.sitemean), # variability across sites in each treatment combo
            NO3.mean = mean(NO3.sitemean),
            NO3.se = se(NO3.sitemean))


############################### 50 YEAR TOTAL N LOSSES PLOT
# prep data for plotting
# put data in long form for plotting, 1 column for N2O, NO3, and Ntots
ndat_tmttotlong <- melt(ndat_tmttot, id=c("till", "cc", "nfert"))
# separate means from se's
ndat_tmttotlong$mean.se <- ifelse(grepl("mean", ndat_tmttotlong$variable), "mean", "se")

# make new column for se values
dat.se <- ndat_tmttotlong[ndat_tmttotlong$mean.se=="se",1:5]
colnames(dat.se)[5] <- "se"
dat.se$variable <- gsub(".se", "", dat.se$variable)
dat.mean <- ndat_tmttotlong[ndat_tmttotlong$mean.se=="mean",1:5]
colnames(dat.mean)[5] <- "mean"
dat.mean$variable <- gsub(".mean", "", dat.mean$variable)
ntotlong <- left_join(dat.mean, dat.se) # , relationship="one-to-one")
rm(dat.se, dat.mean)

windows(xpinch=200, ypinch=200, width=5, height=5)


ggplot(data=ntotlong[!ntotlong$variable=="Ntot",], aes(x=nfert, y=mean, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288", show.legend=F) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, position=position_dodge(0.9)) +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT", "RT"))), 
             cols=vars(factor(cc, levels=c("CC", "NC"))), 
                       #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c(CC="Has Cover Crop", NC="No Cover Crop",
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
                 #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
  scale_fill_manual(values=c("#99DDFF", "#44AA99" )) +
  xlab("N management") +
  ylab("N loss (kg/ha) 2022 to 2072") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))


ggsave("plots/water, nitrate, sediments/IL_N losses 50 yr total bars.png", width=7, height=7, dpi=300)


############################### N LOSSES PER YEAR PLOT
# prep data for plotting
# put data in long form for plotting, 1 column for N2O, NO3, and Ntots
ndat_peryrlong <- melt(ndat_tmtperyr, id=c("till", "cc", "nfert", "crop_system_name"))
ndat_peryrlong <- group_by(ndat_peryrlong, till, cc, nfert, variable) %>% # get the mean across crop systems corn-soy and soy-corn
  summarize(value=mean(value))
# separate means from se's
ndat_peryrlong$mean.se <- ifelse(grepl("mean", ndat_peryrlong$variable), "mean", "se")

# make new column for se values
dat.se <- ndat_peryrlong[ndat_peryrlong$mean.se=="se",1:5]
colnames(dat.se)[5] <- "se"
dat.se$variable <- gsub(".se", "", dat.se$variable)
dat.mean <- ndat_peryrlong[ndat_peryrlong$mean.se=="mean",1:5]
colnames(dat.mean)[5] <- "mean"
dat.mean$variable <- gsub(".mean", "", dat.mean$variable)
npyrlong <- left_join(dat.mean, dat.se) # , relationship="one-to-one")
rm(dat.se, dat.mean)


### get letters for the bars

no3aov <- aov(NO3.yr ~ till:cc:nfert, data=ndatyr[!ndatyr$till=="RT",])
no3lm <- lm(NO3.yr~ till:cc:nfert, data=ndatyr[!ndatyr$till=="RT",])
summary(no3lm)
# Call:
#   lm(formula = NO3.yr ~ till:cc:nfert, data = ndatyr[!ndatyr$till == 
#                                                        "RT", ])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -66.975 -16.103  -2.392  13.828 135.508 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     49.6688     0.4203 118.185  < 2e-16 ***
#   tillCT:ccCC:nfertFall N         -6.2360     0.5943 -10.492  < 2e-16 ***
#   tillNT:ccCC:nfertFall N        -10.1729     0.5943 -17.116  < 2e-16 ***
#   tillCT:ccNC:nfertFall N         24.1067     0.5943  40.560  < 2e-16 ***
#   tillNT:ccNC:nfertFall N         20.9573     0.5943  35.261  < 2e-16 ***
#   tillCT:ccCC:nfertHigh N        -14.4759     0.5943 -24.356  < 2e-16 ***
#   tillNT:ccCC:nfertHigh N        -18.2309     0.5943 -30.674  < 2e-16 ***
#   tillCT:ccNC:nfertHigh N         21.9818     0.5943  36.985  < 2e-16 ***
#   tillNT:ccNC:nfertHigh N         18.8954     0.5943  31.792  < 2e-16 ***
#   tillCT:ccCC:nfertRecommended N -27.1715     0.5943 -45.717  < 2e-16 ***
#   tillNT:ccCC:nfertRecommended N -29.7633     0.5943 -50.078  < 2e-16 ***
#   tillCT:ccNC:nfertRecommended N   3.9494     0.5943   6.645 3.07e-11 ***
#   tillNT:ccNC:nfertRecommended N       NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 24.01 on 39156 degrees of freedom
# Multiple R-squared:  0.3758,	Adjusted R-squared:  0.3756 
# F-statistic:  2143 on 11 and 39156 DF,  p-value: < 2.2e-16

summary(no3aov)
# Df   Sum Sq Mean Sq F value Pr(>F)    
# till:cc:nfert    11 13587543 1235231    2143 <2e-16 ***
#   Residuals     39156 22573096     576                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukout <- TukeyHSD(no3aov)
cld <- multcompView::multcompLetters4(no3aov, Tukout)


npyrlongnoRT <- filter(npyrlong, till %in% c("NT", "CT"), variable=="NO3") %>%
  arrange(desc(mean))

cld <- as.data.frame.list(cld$`till:cc:nfert`)
npyrlongnoRT$cld <- cld$Letters


windows(xpinch=200, ypinch=200, width=5, height=5)


npyrlongnoRT$meanlbac <- npyrlongnoRT$mean*2.20462/2.47105
npyrlongnoRT$selbac <- npyrlongnoRT$se*2.20462/2.47105

#N treatment rates in lb/ac
190*2.20462/2.47105 # 169.51 lb/ac
(49.4+197.6)*2.20462/2.47105 # 220.37 lb/ac


ggplot(data=npyrlongnoRT, 
       aes(x=nfert, y=meanlbac, fill=nfert)) +   # fill=variable
  geom_bar(stat="identity", position=position_dodge(), show.legend=F) + # color="#332288", 
  geom_errorbar(aes(ymin=meanlbac-selbac, ymax=meanlbac+selbac), 
                width=0.3, position=position_dodge(0.9), color="#20243d") +
  # facet_grid(rows=vars(factor(till, levels=c("CT", "NT"))),   2x2 facets
  #            cols=vars(factor(cc, levels=c("NC", "CC"))), 
  #            #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
  #            labeller = as_labeller(
  #              c(NC="No Cover Crop",CC="Has Cover Crop", 
  #                "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
                 # "Fall N" = "Fall\nN", "High N" = "High\nN", "Recommended N"="Recm'd\nN"))) +
  facet_grid(  # 1 x 4 facets
    cols=vars(factor(cc, levels=c("NC", "CC")),factor(till, levels=c("CT", "NT"))), 
    labeller = as_labeller(
      c(NC="No Cover Crop", CC="Rye Cover Crop", 
        "CT" = "Conventional Till", "NT" = "No Till"))) +
  geom_hline(yintercept=0, color="#20243d") +
  
  scale_fill_manual(values=c("#20243d", "#C2e4ef", "#669947")) +
  xlab("N management") +
  ylab("Mean annual N loss (lb per ac) 2022 to 2072") +
  # scale_x_discrete(breaks=c("Fall N", "High N", "Recommended N"),
  #                  labels=c("Fall\nN", "High\N", "Recm'd\nN")) +
  # geom_text(aes(x=nfert, y=meanlbac+10, label=cld), size=5, fontface="bold") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/IL_NO3 losses mean annual bars lbac 1x4 no letters.png", width=6, height=3, dpi=300)


# percent differences
# rye cover (with conventional till) reduced NO3- in Fall N by
cc.ct.fn <- (npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="NC" & npyrlongnoRT$nfert=="Fall N"]-
               npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="CC" & npyrlongnoRT$nfert=="Fall N"])/
  npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="NC" & npyrlongnoRT$nfert=="Fall N"]
#  0.4113
# rye cover (with conventional till) reduced NO3- in high N by
cc.ct.hn <- (npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="NC" & npyrlongnoRT$nfert=="High N"] - 
               npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="CC" & npyrlongnoRT$nfert=="High N"]) /
  npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="NC" & npyrlongnoRT$nfert=="High N"]
#  0.5088
# rye cover (with conventional till) reduced NO3- in recommended N by
cc.ct.rn <- (npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="NC" & npyrlongnoRT$nfert=="Recommended N"]-
               npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="CC" & npyrlongnoRT$nfert=="Recommended N"])/
  npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="NC" & npyrlongnoRT$nfert=="Recommended N"]
#  0.5804
mean(c(cc.ct.fn, cc.ct.hn, cc.ct.rn))  # 0.5002


# rye cover PLUS NO TIL reduced NO3- BY ANOTHER (compared to CC + CT above) in Fall N by
cc.nt.fn <- (npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="CC" & npyrlongnoRT$nfert=="Fall N"] -
               npyrlongnoRT$mean[npyrlongnoRT$till=="NT" & npyrlongnoRT$cc=="CC" & npyrlongnoRT$nfert=="Fall N"])/
  npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="CC" & npyrlongnoRT$nfert=="Fall N"]
#  0.0906
# rye cover PLUS NO TIL reduced NO3- BY ANOTHER (compared to CC + CT above) in high N by
cc.nt.hn <- (npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="CC" & npyrlongnoRT$nfert=="High N"] -
               npyrlongnoRT$mean[npyrlongnoRT$till=="NT" & npyrlongnoRT$cc=="CC" & npyrlongnoRT$nfert=="High N"])/
  npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="CC" & npyrlongnoRT$nfert=="High N"]
#  0.1067
# rye cover PLUS NO TIL reduced NO3- BY ANOTHER (compared to CC + CT above) recommended N by
cc.nt.rn <- (npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="CC" & npyrlongnoRT$nfert=="Recommended N"] -
               npyrlongnoRT$mean[npyrlongnoRT$till=="NT" & npyrlongnoRT$cc=="CC" & npyrlongnoRT$nfert=="Recommended N"])/
  npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="CC" & npyrlongnoRT$nfert=="Recommended N"]
#  0.1152
mean(c(cc.nt.fn, cc.nt.hn, cc.nt.rn))  # 0.104

# an 80 acre field with NC + CT + high N would lose 
loss.ctnchn80 <- npyrlongnoRT$mean[npyrlongnoRT$till=="CT" & npyrlongnoRT$cc=="NC" & npyrlongnoRT$nfert=="High N"] * 80
# 5732 lb
loss.ctnchn80dollars <- loss.ctnchn80*0.6
# $3439

# an 80 acre field with CC + NT + recommended N would lose 
loss.ntccrn80 <- npyrlongnoRT$mean[npyrlongnoRT$till=="NT" & npyrlongnoRT$cc=="CC" & npyrlongnoRT$nfert=="Recommended N"] * 80
# 1592.439 lb
loss.ntccrn80dollars <- loss.ntccrn80*0.6
# 955


### above plot but as difference from Fall N

falln <- filter(npyrlongnoRT, nfert=="Fall N") %>%
  rename("meanlbac.falln" = "meanlbac", "selbac.falln" = "selbac") %>%
  ungroup(nfert) %>%
  select(cc, till, meanlbac.falln, selbac.falln)

springn <- filter(npyrlongnoRT, !nfert=="Fall N") %>%
  select(cc, till, nfert, meanlbac, selbac)

npyrlongnoRT2 <- left_join(springn, falln)

rm(falln, springn)

npyrlongnoRT2$mean.falldiff <- npyrlongnoRT2$meanlbac - npyrlongnoRT2$meanlbac.falln

pal2 <- c("#004a23", "#669947")


ggplot(data=npyrlongnoRT2, aes(x=nfert, y=mean.falldiff, fill=nfert)) +
  geom_bar(stat="identity", position=position_dodge(),  show.legend=F) +  # color="#20243d",
  geom_errorbar(width=0.3, aes(ymin=mean.falldiff-selbac.falln, 
                               ymax=mean.falldiff + selbac.falln),  
                position=position_dodge(0.9),
                color="#20243d") +
  facet_grid( # rows=vars(factor(till, levels=c("CT", "NT"))), 
    cols=vars(factor(cc, levels=c("NC", "CC")),factor(till, levels=c("CT", "NT"))), 
    labeller = as_labeller(
      c(NC="No Cover Crop", CC="Rye Cover Crop", 
        "CT" = "Conventional Till", "NT" = "No Till"))) +
  xlab("N management") +
  ylab(expression(bold("Nitrate loss difference from fall applied N (lb N per ac)"))) + 
  scale_x_discrete(breaks=c("High N", "Recommended N"),
                   labels = c("High N", "Recomm. N")) +
  # ylim(0,3800)+
  # geom_text(aes(label=cld, y=mean+(2*se)), vjust=-0.5,
  # color="gray20", size=4, fontface="bold") +
  scale_fill_manual(values=pal2) +
  scale_y_continuous(expand=c(0,0)) +
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

ggsave("plots/water, nitrate, sediments/IL_NO3 loss_diff from Fall N.png", width=6, height=3, dpi=300)






############################### N LOSSES PER decade PLOT
# prep data for plotting
# put data in long form for plotting, 1 column for N2O, NO3, and Ntots
ndat_declong <- melt(ndat_dec, id=c("till", "cc", "nfert", "decade"))


# separate means from se's
ndat_declong$mean.se <- ifelse(grepl("mean", ndat_declong$variable), "mean", "se")

# make new column for se values
dat.se <- ndat_declong[ndat_declong$mean.se=="se",1:6]
colnames(dat.se)[6] <- "se"
dat.se$variable <- gsub(".se", "", dat.se$variable)
dat.mean <- ndat_declong[ndat_declong$mean.se=="mean",1:6]
colnames(dat.mean)[6] <- "mean"
dat.mean$variable <- gsub(".mean", "", dat.mean$variable)
ndeclong <- left_join(dat.mean, dat.se) # , relationship="one-to-one")
rm(dat.se, dat.mean)

windows(xpinch=200, ypinch=200, width=5, height=5)


pal6 <- c("#eaeccc", "#FEDa8B", "#FDb366", "#f67e4b","#dd3d2d", "#a50026")   
pal6blue <- c("#ffffff", "#ceced5", "#9f9fac", "#727284", "#48495f", "#20233c")
                   

ggplot(data=ndeclong[ndeclong$variable=="NO3" & ndeclong$till %in% c("NT", "CT"),], aes(x=nfert, y=mean, fill=decade)) +
  geom_bar(stat="identity", position=position_dodge(), color="#20233d") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, position=position_dodge(0.9)) +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT"))), 
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c(NC="No Cover Crop", CC="Has Cover Crop", 
                 "CT" = "Conventional Till", "NT" = "No Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
  scale_fill_manual(values=pal6blue) +
  xlab("N management") +
  ylab("Mean annual nitrate loss (kg N /ha) 2022 to 2072") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/IL_N losses mean annual bars blue.png", width=7, height=5, dpi=300)



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


