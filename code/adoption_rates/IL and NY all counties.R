
# NY counties (fips) = Genesee (36037), Wyoming (36121), Livingston (36051) 
# IL counties (fips) = Ford (17053), Livingston (17105), Macoupin (17117), Montgomery (17135)

# here we pull together optis, us census of agriculture (via carpe), and il tillage transect 
# survey data for these counties

# script started 6/13/2024 by Bonnie McGill

library(ggplot2)
library(tidyverse)

se <- function(x) sd(x) / sqrt(length(x))


###### 1) prepare the data

# load and prep optis data
# prepped from other scripts in this repo
optny <- read.csv("data/optis/datny_cleanlong.csv")
optny$state <- rep("New York", nrow(optny))
optny <- optny[,c(7, 1:3,5,6)]

optil <- read.csv("data/optis/datil_cleanlong.csv")
optil <- optil[,c(1,2,4,5,8,9)]
optil$state <- rep("Illinois", nrow(optil))

opt <- bind_rows(optny, optil)
rm(optil, optny)
opt$source <- rep("OpTIS", nrow(opt))
# unique(opt$crop_name)  #  "Soybeans" "Corn" 


# census data
# came from spreadsheet in /CLM-19 FFAR GHG - General / Results / carpe adoption data
cens <- read.csv("data/optis/CaRPE CC_NT_RT 2017_2022_NY and IL all counties.csv")
# note CaRPE refers to conventional tillage as intensive tillage, changing it to CT here:
colnames(cens) <- c("year", "state", "county", "perc_cc", "perc_nt", "perc_rt", "perc_ct")
cens$source <- rep("AgCensus", nrow(cens))
cens <- pivot_longer(cens, cols=c(perc_cc, perc_nt, perc_rt, perc_ct), names_to="variable", values_to="value")
# Census data are for all cropland, not by crop like the other data sources
# Want to plot census data with corn and soybean crops
# make a copy of census label one as corn and one as soybeans so they get plotted in those facets
cens$crop_name <- rep("Soybeans", nrow(cens))
cens <- cens[,c(2,3,1,7,5,6,4)]
censb <- cens
censb$crop_name <- rep("Corn", nrow(censb))
cens <- bind_rows(cens, censb)
rm(censb)

# tillage transect data
till <- read.csv("data/optis/Illinois tillage transect survey data 2015-2018_all counties.csv")
# came from spreadsheet in /CLM-19 FFAR GHG - General / Results / IL tillage transect data
# as.data.frame(names(till))
# note the "%" symbol doesn't come through into R but these aren't acres these are %s:
till <- till[,c(1,2,4,9,12,15,18)]
colnames(till) <- c("year", "crop_name", "county", "perc_nt", "perc_mt", "perc_rt", "perc_ct")
till$source <- rep("Transect", nrow(till))
till$perc_nt <- as.numeric(till$perc_nt)
till$perc_mt <- as.numeric(till$perc_mt)
till$perc_rt <- as.numeric(till$perc_rt)
till$perc_ct <- as.numeric(till$perc_ct)
# according to the definitions of residue cover, fields categorized as "mulch till" in 
# transect data would correspond to "no till" in OpTIS data
# sum no till and mulch till for new "no till"
till$perc_nt <- till$perc_nt + till$perc_mt
# remove mulch till
till$perc_mt <- NULL
till <- pivot_longer(till, cols=c(perc_nt, perc_rt, perc_ct), names_to="variable", values_to="value")
till$state <- rep("Illinois", nrow(till))
till <- till[,c(7,3,1,2,5,6,4)]
unique(till$crop_name)
till$crop_name <- ifelse(till$crop_name=="corn", "Corn", "Soybeans")


dat <- bind_rows(cens,opt,till)
rm(cens,opt,till)

# dat$crop_source <- paste0(dat$crop_name, " ", dat$source)


dat$costate <- paste0(dat$county, " County, ", dat$state)

# align optis data (crop year 2018) with agcensus calendar year
dat$cc_year <- ifelse(dat$source=="OpTIS" & dat$variable=="perc_cc", dat$year-1, dat$year)


###### 2) make 2017 density plot

windows(xpinch=200, ypinch=200, width=5, height=5)


ggplot(dat=dat[dat$cc_year==2017,], aes(x=value)) +
  geom_density(aes(color=source, fill=source), alpha=0.6) +  
  scale_color_manual(breaks = c("AgCensus", "OpTIS", "Transect"),
                      values=c("#88CCEE", "#999933", "#AA4499"),
                      name="Data source") +
  scale_fill_manual(breaks = c("AgCensus", "OpTIS", "Transect"),
                     values=c("#88CCEE", "#999933", "#AA4499"),
                     name="Data source") +
  facet_grid(rows=vars(factor(variable, levels=c("perc_cc", "perc_nt", "perc_rt", "perc_ct"))), 
             cols=vars(factor(state, levels=c("Illinois", "New York"))),
             labeller = as_labeller(
               c("perc_cc" = "Cover Crop", "perc_nt" = "No-till",
                 "perc_rt" = "Reduced tillage", "perc_ct" = "Conventional tillage",
                 "Illinois" = "Illinois counties", "New York" = "New York counties")),
             scales="free") +
  xlab("% of county acres in 2017") +
  ylab("Density") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text=element_text(size=9),
    axis.title=element_text(size=12, face="bold"),
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
    strip.text=element_text(size=10),
    strip.background = element_rect(fill="gray95"),
    legend.text=element_text(size=11),
    legend.title=element_text(size=12, face="bold"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size = unit(0.5, "cm")
  )	 

ggsave("plots/adoption/densities 2017_4 practices.png", width=8, height=7, dpi=300)




###### 3) Are these populations significantly different in 2017



ccny <- aov(value~source, data=dat[dat$cc_year==2017& dat$variable=="perc_cc" & dat$state=="New York",])
summary(ccny)
# Df Sum Sq Mean Sq F value  Pr(>F)   
# source        1    530   530.2   9.164 0.00288 **
#   Residuals   160   9257    57.9                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1     
Tukoutccny <- TukeyHSD(ccny)
# $source
# diff       lwr       upr     p adj
# OpTIS-AgCensus -4.0674 -6.720897 -1.413903 0.0028775


ntny <- aov(value~source, data=dat[dat$year==2017& dat$variable=="perc_nt" & dat$state=="New York",])
summary(ntny)
# Df Sum Sq Mean Sq F value Pr(>F)  
# source        1   1248    1248   5.548 0.0197 *
#   Residuals   160  35998     225     
Tukoutntny <- TukeyHSD(ntny)
Tukoutntny
# $source
# diff       lwr      upr    p adj
# OpTIS-AgCensus 6.156159 0.9946732 11.31764 0.019711

rtny <- aov(value~source, data=dat[dat$year==2017& dat$variable=="perc_rt" & dat$state=="New York",])
summary(rtny)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# source        1   3454    3454   11.74 0.000777 ***
#   Residuals   160  47068     294 
Tukoutrtny <- TukeyHSD(rtny)
Tukoutrtny
# $source
# diff      lwr      upr     p adj
# OpTIS-AgCensus 10.23998 4.337993 16.14197 0.0007773

ctny <- aov(value~source, data=dat[dat$year==2017& dat$variable=="perc_ct" & dat$state=="New York",])
summary(ctny)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source        1  39277   39277   197.2 <2e-16 ***
#   Residuals   158  31472     199    
Tukoutctny <- TukeyHSD(ctny)
Tukoutctny
# $source
# diff       lwr       upr p adj
# OpTIS-AgCensus -35.0893 -40.02471 -30.15389     0



ccil <- aov(value~source, data=dat[dat$year==2017& dat$variable=="perc_cc" & dat$state=="Illinois",])
summary(ccil)
# Df Sum Sq Mean Sq F value  Pr(>F)    
# source        1    365   364.8   37.05 2.7e-09 ***
#   Residuals   400   3938     9.8 
Tukoutccil <- TukeyHSD(ccil)
Tukoutccil
# $source
# diff      lwr      upr p adj
# OpTIS-AgCensus 1.905365 1.289963 2.520767     0

ntil <- aov(value~source, data=dat[dat$year==2017& dat$variable=="perc_nt" & dat$state=="Illinois",])
summary(ntil)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source        2  35345   17672   40.81 <2e-16 ***
#   Residuals   602 260708     433      
Tukoutntil <- TukeyHSD(ntil)
Tukoutntil
# $source
# diff        lwr        upr     p adj
# OpTIS-AgCensus    18.265003  13.411780 23.1182254 0.0000000
# Transect-AgCensus 12.421225   7.549656 17.2927947 0.0000000
# Transect-OpTIS    -5.843777 -10.727241 -0.9603137 0.0140791

rtil <- aov(value~source, data=dat[dat$year==2017& dat$variable=="perc_rt" & dat$state=="Illinois",])
summary(rtil)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# source        1   3454    3454   11.74 0.000777 ***
#   Residuals   160  47068     294 
Tukoutrtil <- TukeyHSD(rtil)
Tukoutrtil
# $source
# diff       lwr        upr   p adj
# OpTIS-AgCensus     -6.851633  -9.99817  -3.705096 1.3e-06
# Transect-AgCensus -19.360389 -22.51882 -16.201957 0.0e+00
# Transect-OpTIS    -12.508755 -15.67490  -9.342613 0.0e+00

ctil <- aov(value~source, data=dat[dat$year==2017& dat$variable=="perc_ct" & dat$state=="Illinois",])
summary(ctil)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source        2  39925   19962   53.94 <2e-16 ***
#   Residuals   597 220961     370    
Tukoutctil <- TukeyHSD(ctil)
Tukoutctil
# $source
# diff        lwr      upr    p adj
# OpTIS-AgCensus    -13.263129 -17.778397 -8.74786 0.000000
# Transect-AgCensus   6.431625   1.927914 10.93533 0.002426
# Transect-OpTIS     19.694753  15.151703 24.23780 0.000000



####### 4) how many counties have >90% of a practice?



dct <- filter(dat, year==2017, state=="Illinois", variable=="perc_ct", value>75) # 11 counties
drt <- filter(dat, year==2017, state=="Illinois", variable=="perc_rt", value>75)
dnt <- filter(dat, year==2017, state=="Illinois", variable=="perc_nt", value>75) # 8 counties


# what is the state level % adoption AgCensus
state_cc <- filter(dat, variable=="perc_cc",  source=="AgCensus") %>%
  group_by(state, year) %>%
  summarize(mean.cc = mean(value),
            se.cc=se(value))

state_till <- filter(dat, !variable=="perc_cc", source=="AgCensus") %>%
  group_by(state, year, variable) %>%
  summarize(mean.value = mean(na.omit(value)),
            se.value=se(na.omit(value)))

# Ag Census rate of change

# cc
(state_cc[state_cc$state=="Illinois" & state_cc$year==2022,"mean.cc"] -
    state_cc[state_cc$state=="Illinois" & state_cc$year==2017,"mean.cc"] ) /
  (2022-2017)   # 0.163

(state_cc[state_cc$state=="New York" & state_cc$year==2022,"mean.cc"] -
    state_cc[state_cc$state=="New York" & state_cc$year==2017,"mean.cc"] ) /
  (2022-2017)   # 0.437

# no till
(state_till[state_till$state=="Illinois" & state_till$year==2022 & state_till$variable=="perc_nt","mean.value"] -
    state_till[state_till$state=="Illinois" & state_till$year==2017 & state_till$variable=="perc_nt","mean.value"] ) /
  (2022-2017)  # 0.405

(state_till[state_till$state=="New York" & state_till$year==2022 & state_till$variable=="perc_nt","mean.value"] -
    state_till[state_till$state=="New York" & state_till$year==2017 & state_till$variable=="perc_nt","mean.value"] ) /
  (2022-2017)  # 0.680

# reduced till
(state_till[state_till$state=="Illinois" & state_till$year==2022 & state_till$variable=="perc_rt","mean.value"] -
    state_till[state_till$state=="Illinois" & state_till$year==2017 & state_till$variable=="perc_rt","mean.value"] ) /
  (2022-2017)  # -0.104

(state_till[state_till$state=="New York" & state_till$year==2022 & state_till$variable=="perc_rt","mean.value"] -
    state_till[state_till$state=="New York" & state_till$year==2017 & state_till$variable=="perc_rt","mean.value"] ) /
  (2022-2017)  # 0.513


# conventional till
(state_till[state_till$state=="Illinois" & state_till$year==2022 & state_till$variable=="perc_ct","mean.value"] -
    state_till[state_till$state=="Illinois" & state_till$year==2017 & state_till$variable=="perc_ct","mean.value"] ) /
  (2022-2017)  # -0.301

(state_till[state_till$state=="New York" & state_till$year==2022 & state_till$variable=="perc_ct","mean.value"] -
    state_till[state_till$state=="New York" & state_till$year==2017 & state_till$variable=="perc_ct","mean.value"] ) /
  (2022-2017)  # -1.194



state_cc_all <- filter(dat, variable=="perc_cc", cc_year==2017) %>%
  group_by(state, year, source) %>%
  summarize(mean.cc = mean(value),
            se.cc=se(value))

state_till_all <- filter(dat, !variable=="perc_cc", year==2017) %>%
  group_by(state, year, variable, source) %>%
  summarize(mean.value = mean(na.omit(value)),
            se.value=se(na.omit(value)))


# transect rate of change
state_till_transect <- filter(dat, !variable=="perc_cc", source=="Transect", year %in% c(2015, 2018)) %>%
  group_by(year, variable) %>%
  summarize(mean.value = mean(na.omit(value)),
            se.value=se(na.omit(value)))


# conventional till
(state_till_transect[state_till_transect$year==2018 & state_till_transect$variable=="perc_ct","mean.value"] -
    state_till_transect[state_till_transect$year==2015 & state_till_transect$variable=="perc_ct","mean.value"] ) /
  (2018-2015)  # 0.37

(state_till_transect[state_till_transect$year==2018 & state_till_transect$variable=="perc_rt","mean.value"] -
    state_till_transect[state_till_transect$year==2015 & state_till_transect$variable=="perc_rt","mean.value"] ) /
  (2018-2015)  # -0.09

(state_till_transect[state_till_transect$year==2018 & state_till_transect$variable=="perc_nt","mean.value"] -
    state_till_transect[state_till_transect$year==2015 & state_till_transect$variable=="perc_nt","mean.value"] ) /
  (2018-2015)  # -0.81
