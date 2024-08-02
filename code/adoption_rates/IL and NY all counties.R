
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
# y <- unique(opt[c("county", "state")])
# nrow(y[y$state=="New York",])
# nrow(y[y$state=="Illinois",])

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
# Census data are for all cropland, not by crop like the other data sources
cens$crop_name <- rep("Cropland", nrow(cens))

# fix county names to match optis
cens$county <- ifelse(cens$county == "Du Page", "DuPage",
                      ifelse(cens$county == "De Kalb", "DeKalb",
                             ifelse(cens$county == "De Witt", "DeWitt",
                                    ifelse(cens$county == "La Salle", "LaSalle",
                                           ifelse(cens$county == "Mcdonough", "McDonough",
                                                  ifelse(cens$county == "Mchenry", "McHenry",
                                                         ifelse(cens$county =="Mclean", "McLean",
                                                                ifelse(cens$county == "St Clair", "St. Clair", cens$county))))))))

# tillage transect data
till <- read.csv("data/optis/Illinois tillage transect survey data 2015-2018_all counties.csv")
# came from spreadsheet in /CLM-19 FFAR GHG - General / Results / IL tillage transect data
# as.data.frame(names(till))
till <- filter(till, !County=="Total")
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
unique(till$county)

till$county <- ifelse(till$county=="JoDaviess", "Jo Daviess",
                      ifelse(till$county== "St Clair", "St. Clair",
                             ifelse(till$county== "StClair", "St. Clair",
                             ifelse(till$county=="RockIsland", "Rock Island", till$county))))

dat <- bind_rows(cens,opt,till)
rm(cens,opt,till)

# x <- unique(dat[c("county", "state")])
# nrow(x[x$state=="Illinois",])
# nrow(x[x$state=="New York",])

# dat$crop_source <- paste0(dat$crop_name, " ", dat$source)


dat$costate <- paste0(dat$county, " County, ", dat$state)

# align optis data (crop year 2018) with agcensus calendar year
dat$cc_year <- ifelse(dat$source=="OpTIS" & dat$variable=="perc_cc", dat$year-1, dat$year)


# ###### 2) make 2017 density plot
# 
# windows(xpinch=200, ypinch=200, width=5, height=5)
# 
# 
# ggplot(dat=dat[dat$cc_year==2017,], aes(x=value)) +
#   geom_density(aes(color=source, fill=source), alpha=0.6) +  
#   scale_color_manual(breaks = c("AgCensus", "OpTIS", "Transect"),
#                       values=c("#88CCEE", "#999933", "#AA4499"),
#                       name="Data source") +
#   scale_fill_manual(breaks = c("AgCensus", "OpTIS", "Transect"),
#                      values=c("#88CCEE", "#999933", "#AA4499"),
#                      name="Data source") +
#   facet_grid(rows=vars(factor(variable, levels=c("perc_cc", "perc_nt", "perc_rt", "perc_ct"))), 
#              cols=vars(factor(state, levels=c("Illinois", "New York"))),
#              labeller = as_labeller(
#                c("perc_cc" = "Cover Crop", "perc_nt" = "No-till",
#                  "perc_rt" = "Reduced tillage", "perc_ct" = "Conventional tillage",
#                  "Illinois" = "Illinois counties", "New York" = "New York counties")),
#              scales="free") +
#   xlab("% of county acres in 2017") +
#   ylab("Density") +
#   theme(
#     panel.grid.minor=element_blank(), 
#     panel.grid.major=element_blank() ,
#     axis.text=element_text(size=9),
#     axis.title=element_text(size=12, face="bold"),
#     panel.background = element_rect(fill = 'white') ,
#     panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
#     strip.text=element_text(size=10),
#     strip.background = element_rect(fill="gray95"),
#     legend.text=element_text(size=11),
#     legend.title=element_text(size=12, face="bold"),
#     plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
#     legend.key=element_rect(fill="white"),
#     legend.key.size = unit(0.5, "cm")
#   )	 
# 
# ggsave("plots/adoption/densities 2017_4 practices.png", width=8, height=7, dpi=300)





########  3) are these regional populations significantly different in 2017


# assign region groups
dat$region <- ifelse(dat$state == "New York" & dat$county %in% c("Niagara", "Orleans", "Monroe", "Wayne",
                                                                 "Erie", "Genesee", "Wyoming", "Livingston", "Ontario", "Yates", "Seneca",
                                                                 "Chautauqua", "Cattaraugus", "Allegany", "Steuben", "Schuyler", "Chemung"),
                     "Western NY", 
                     ifelse(dat$state=="Illinois" & dat$county %in% c("Henderson", "Warren", "Knox", "Stark", "Marshall", "Woodford", "Livingston", "Ford", "Iroquois",
                                                                      "Hancock", "McDonough", "Fulton", "Peoria", "Tazewell", "McLean",
                                                                      "Adams", "Schuyler", "Brown", "Cass", "Mason", "Menard", "Logan", "DeWitt", "Piatt", "Champaign", "Vermilion",
                                                                      "Pike", "Scott", "Morgan", "Sangamon", "Macon", 
                                                                      "Calhoun", "Greene", "Jersey", "Macoupin", "Montgomery", "Christian", "Shelby", "Moultrie", "Douglas", "Coles", "Cumberland", "Edgar", "Clark"),
                            "Central IL",
                            ifelse(dat$state=="Illinois" & dat$county %in% c("Madison", "Bond", "Fayette", "Effingham", "Jasper", "Crawford", 
                                                                             "St. Clair", "Clinton", "Marion", "Clay", "Richland", "Lawrence",
                                                                             "Monroe", "Washington", "Jefferson", "Wayne", "Edwards", "Wabash",
                                                                             "Randolph", "Perry", "Franklin", "Hamilton", "White", 
                                                                             "Jackson", "Williamson", "Saline", "Gallatin",
                                                                             "Union", "Johnson", "Pope", "Hardin",
                                                                             "Alexander", "Pulaski", "Massac"),
                                   "Southern IL", "X")))


dat.reg <- dat[dat$region != "X",]



# table 1 2017 regional mean practice adoption levels
means.17 <- dat.reg%>%
  filter(year==2017) %>%
  group_by(source, variable, region) %>%
  summarize(Mean = mean(na.omit(value)), se=se(na.omit(value)))




# Sig. differences for table in Fig. 1

ccwny <- aov(value~source:crop_name, data=dat.reg[dat.reg$cc_year==2017& dat.reg$variable=="perc_cc" & dat.reg$region=="Western NY",])
summary(ccwny)
# Df Sum Sq Mean Sq F value  Pr(>F)   
# source:crop_name  2    561  280.52   7.288 0.00273 **
#   Residuals        29   1116   38.49                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukoutccwny <- TukeyHSD(ccwny)
Tukoutccwny  ########################### yes there are ways to specify which contrasts you want, but I didn't figure that out yet so just seeing them all
# $`source:crop_name`
# diff       lwr       upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA        NA        NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA        NA        NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA        NA        NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA        NA        NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA        NA        NA        NA
# AgCensus:Cropland-OpTIS:Corn          6.784225  -1.01243 14.580880 0.1165773  ####
# OpTIS:Cropland-OpTIS:Corn                   NA        NA        NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA        NA        NA        NA
# OpTIS:Soybeans-OpTIS:Corn            -3.279694 -13.24788  6.688491 0.9131721  XXXXX
# OpTIS:Cropland-AgCensus:Cropland            NA        NA        NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA        NA        NA        NA
# OpTIS:Soybeans-AgCensus:Cropland    -10.063919 -19.04505 -1.082785 0.0211630   ####
# AgCensus:Soybeans-OpTIS:Cropland            NA        NA        NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA        NA        NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA        NA        NA        NA


ntwny <- aov(value~source:crop_name, data=dat.reg[dat.reg$year==2017& dat.reg$variable=="perc_nt" & dat.reg$region=="Western NY",])
summary(ntwny)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# source:crop_name  2   2466  1233.2   9.308 0.000623 ***
#   Residuals        33   4372   132.5                
Tukoutntwny <- TukeyHSD(ntwny)
Tukoutntwny
# $`source:crop_name`
# diff         lwr       upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA          NA        NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA          NA        NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA          NA        NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA          NA        NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA          NA        NA        NA
# AgCensus:Cropland-OpTIS:Corn        -18.886681 -33.2328539 -4.540508 0.0044087  ###
# OpTIS:Cropland-OpTIS:Corn                   NA          NA        NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA          NA        NA        NA
# OpTIS:Soybeans-OpTIS:Corn            -5.217583 -21.2076632 10.772498 0.9189727  XXX
# OpTIS:Cropland-AgCensus:Cropland            NA          NA        NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA          NA        NA        NA
# OpTIS:Soybeans-AgCensus:Cropland     13.669098  -0.2001372 27.538334 0.0552772  ###
# AgCensus:Soybeans-OpTIS:Cropland            NA          NA        NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA          NA        NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA          NA        NA        NA

rtwny <- aov(value~source:crop_name, data=dat.reg[dat.reg$year==2017& dat.reg$variable=="perc_rt" & dat.reg$region=="Western NY",])
summary(rtwny)
#                   Df Sum Sq Mean Sq F value Pr(>F)  
# source:crop_name  2    675   337.4   3.375 0.0464 *
# Residuals        33   3299   100.0     
Tukoutrtwny <- TukeyHSD(rtwny)
Tukoutrtwny
# $`source:crop_name`
# diff        lwr       upr     p adj
# OpTIS:Corn-AgCensus:Corn                   NA         NA        NA        NA
# AgCensus:Cropland-AgCensus:Corn            NA         NA        NA        NA
# OpTIS:Cropland-AgCensus:Corn               NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Corn            NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Corn               NA         NA        NA        NA
# AgCensus:Cropland-OpTIS:Corn        -4.663744 -17.126469  7.798981 0.8647140
# OpTIS:Cropland-OpTIS:Corn                  NA         NA        NA        NA
# AgCensus:Soybeans-OpTIS:Corn               NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Corn            5.652655  -8.238156 19.543466 0.8188908
# OpTIS:Cropland-AgCensus:Cropland           NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Cropland        NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Cropland    10.316399  -1.732004 22.364802 0.1285667
# AgCensus:Soybeans-OpTIS:Cropland           NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Cropland              NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans           NA         NA        NA        NA

ctwny <- aov(value~source:crop_name, data=dat.reg[dat.reg$year==2017& dat.reg$variable=="perc_ct" & dat.reg$region=="Western NY",])
summary(ctwny)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# source:crop_name  2   9000    4500   55.11 3.03e-11 ***
#   Residuals        33   2695      82     
Tukoutctwny <- TukeyHSD(ctwny)
Tukoutctwny
# $`source:crop_name`
# diff        lwr       upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA         NA        NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA         NA        NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA         NA        NA        NA
# AgCensus:Cropland-OpTIS:Corn         33.634464  22.371657  44.89727 0.0000000
# OpTIS:Cropland-OpTIS:Corn                   NA         NA        NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Corn             3.982243  -8.571152  16.53564 0.9274444
# OpTIS:Cropland-AgCensus:Cropland            NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Cropland    -29.652221 -40.540596 -18.76385 0.0000000
# AgCensus:Soybeans-OpTIS:Cropland            NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA         NA        NA        NA



cccil <- aov(value~source:crop_name, data=dat.reg[dat.reg$year==2017& dat.reg$variable=="perc_cc" & dat.reg$region=="Central IL",])
summary(cccil)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# source:crop_name   2   83.5   41.73   7.841 0.000611 ***
#   Residuals        129  686.6    5.32                     
Tukoutcccil <- TukeyHSD(cccil)
Tukoutcccil
# $`source:crop_name`
# diff        lwr       upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA         NA        NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA         NA        NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA         NA        NA        NA
# AgCensus:Cropland-OpTIS:Corn        -1.1417089 -2.5646372 0.2812195 0.1931652
# OpTIS:Cropland-OpTIS:Corn                   NA         NA        NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Corn            0.7957463 -0.6271821 2.2186746 0.5883322
# OpTIS:Cropland-AgCensus:Cropland            NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Cropland     1.9374551  0.5145268 3.3603835 0.0018193
# AgCensus:Soybeans-OpTIS:Cropland            NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA         NA        NA        NA

ntcil <- aov(value~source:crop_name, data=dat.reg[dat.reg$year==2017& dat.reg$variable=="perc_nt" & dat.reg$region=="Central IL",])
summary(ntcil)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name   4  39955    9989   33.03 <2e-16 ***
#   Residuals        215  65021     302     

Tukoutntcil <- TukeyHSD(ntcil)
Tukoutntcil
# $`source:crop_name`
# diff        lwr       upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA         NA        NA        NA
# Transect:Corn-AgCensus:Corn                 NA         NA        NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA         NA        NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA         NA        NA        NA
# Transect:Cropland-AgCensus:Corn             NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA         NA        NA        NA
# Transect:Soybeans-AgCensus:Corn             NA         NA        NA        NA
# Transect:Corn-OpTIS:Corn            -14.772632 -26.391153 -3.154112 0.0029379   ###
# AgCensus:Cropland-OpTIS:Corn        -11.992904 -23.611424 -0.374383 0.0372144   ###
# OpTIS:Cropland-OpTIS:Corn                   NA         NA        NA        NA
# Transect:Cropland-OpTIS:Corn                NA         NA        NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Corn             0.750622 -10.867899 12.369143 0.9999999   XXX
# Transect:Soybeans-OpTIS:Corn         23.384186  11.765665 35.002707 0.0000001   XXX
# AgCensus:Cropland-Transect:Corn       2.779729  -8.838792 14.398249 0.9979593   ###
# OpTIS:Cropland-Transect:Corn                NA         NA        NA        NA
# Transect:Cropland-Transect:Corn             NA         NA        NA        NA
# AgCensus:Soybeans-Transect:Corn             NA         NA        NA        NA
# OpTIS:Soybeans-Transect:Corn         15.523254   3.904734 27.141775 0.0013464  XXX
# Transect:Soybeans-Transect:Corn      38.156818  26.538298 49.775339 0.0000000  XXX
# OpTIS:Cropland-AgCensus:Cropland            NA         NA        NA        NA
# Transect:Cropland-AgCensus:Cropland         NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Cropland     12.743526   1.125005 24.362046 0.0198741  ###
# Transect:Soybeans-AgCensus:Cropland  35.377090  23.758569 46.995610 0.0000000  ###
# Transect:Cropland-OpTIS:Cropland            NA         NA        NA        NA
# AgCensus:Soybeans-OpTIS:Cropland            NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA         NA        NA        NA
# Transect:Soybeans-OpTIS:Cropland            NA         NA        NA        NA
# AgCensus:Soybeans-Transect:Cropland         NA         NA        NA        NA
# OpTIS:Soybeans-Transect:Cropland            NA         NA        NA        NA
# Transect:Soybeans-Transect:Cropland         NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA         NA        NA        NA
# Transect:Soybeans-AgCensus:Soybeans         NA         NA        NA        NA
# Transect:Soybeans-OpTIS:Soybeans     22.633564  11.015043 34.252085 0.0000002  ###

rtcil <- aov(value~source:crop_name, data=dat.reg[dat.reg$year==2017& dat.reg$variable=="perc_rt" & dat.reg$region=="Central IL",])
summary(rtcil)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name   4  16740    4185   25.39 <2e-16 ***
#   Residuals        215  35439     165     
Tukoutrtcil <- TukeyHSD(rtcil)
Tukoutrtcil
# $`source:crop_name`
# diff         lwr        upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA          NA         NA        NA
# Transect:Corn-AgCensus:Corn                 NA          NA         NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA          NA         NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA          NA         NA        NA
# Transect:Cropland-AgCensus:Corn             NA          NA         NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA          NA         NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA          NA         NA        NA
# Transect:Soybeans-AgCensus:Corn             NA          NA         NA        NA
# Transect:Corn-OpTIS:Corn             -5.924488 -14.5020941   2.653119 0.4327840  ##
# AgCensus:Cropland-OpTIS:Corn          9.122733   0.5451267  17.700339 0.0276120  ###
# OpTIS:Cropland-OpTIS:Corn                   NA          NA         NA        NA
# Transect:Cropland-OpTIS:Corn                NA          NA         NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA          NA         NA        NA
# OpTIS:Soybeans-OpTIS:Corn            10.216822   1.6392154  18.794428 0.0073504  XX
# Transect:Soybeans-OpTIS:Corn        -12.565397 -21.1430031  -3.987791 0.0002554  XX
# AgCensus:Cropland-Transect:Corn      15.047221   6.4696145  23.624827 0.0000039  ###
# OpTIS:Cropland-Transect:Corn                NA          NA         NA        NA
# Transect:Cropland-Transect:Corn             NA          NA         NA        NA
# AgCensus:Soybeans-Transect:Corn             NA          NA         NA        NA
# OpTIS:Soybeans-Transect:Corn         16.141309   7.5637032  24.718916 0.0000005  XX
# Transect:Soybeans-Transect:Corn      -6.640909 -15.2185154   1.936697 0.2752830  XX
# OpTIS:Cropland-AgCensus:Cropland            NA          NA         NA        NA
# Transect:Cropland-AgCensus:Cropland         NA          NA         NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA          NA         NA        NA
# OpTIS:Soybeans-AgCensus:Cropland      1.094089  -7.4835176   9.671695 0.9999815 ##
# Transect:Soybeans-AgCensus:Cropland -21.688130 -30.2657362 -13.110524 0.0000000 ##
# Transect:Cropland-OpTIS:Cropland            NA          NA         NA        NA
# AgCensus:Soybeans-OpTIS:Cropland            NA          NA         NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA          NA         NA        NA
# Transect:Soybeans-OpTIS:Cropland            NA          NA         NA        NA
# AgCensus:Soybeans-Transect:Cropland         NA          NA         NA        NA
# OpTIS:Soybeans-Transect:Cropland            NA          NA         NA        NA
# Transect:Soybeans-Transect:Cropland         NA          NA         NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA          NA         NA        NA
# Transect:Soybeans-AgCensus:Soybeans         NA          NA         NA        NA
# Transect:Soybeans-OpTIS:Soybeans    -22.782219 -31.3598249 -14.204612 0.0000000 ##


ctcil <- aov(value~source:crop_name, data=dat.reg[dat.reg$year==2017& dat.reg$variable=="perc_ct" & dat.reg$region=="Central IL",])
summary(ctcil)
# Df Sum Sq Mean Sq F value  Pr(>F)    
# source:crop_name   4  31308    7827   20.87 1.5e-14 ***
#   Residuals        215  80631     375   
Tukoutctcil <- TukeyHSD(ctcil)
Tukoutctcil
# $`source:crop_name`
# diff        lwr         upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA         NA          NA        NA
# Transect:Corn-AgCensus:Corn                 NA         NA          NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA         NA          NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA         NA          NA        NA
# Transect:Cropland-AgCensus:Corn             NA         NA          NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA         NA          NA        NA
# Transect:Soybeans-AgCensus:Corn             NA         NA          NA        NA
# Transect:Corn-OpTIS:Corn             21.803517   8.865243  34.7417915 0.0000111  ##
# AgCensus:Cropland-OpTIS:Corn          3.990204  -8.948070  16.9284785 0.9884400  ##
# OpTIS:Cropland-OpTIS:Corn                   NA         NA          NA        NA
# Transect:Cropland-OpTIS:Corn                NA         NA          NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA         NA          NA        NA
# OpTIS:Soybeans-OpTIS:Corn           -11.429137 -24.367411   1.5091372 0.1311678  XX
# Transect:Soybeans-OpTIS:Corn         -9.694210 -22.632484   3.2440642 0.3185291 XX
# AgCensus:Cropland-Transect:Corn     -17.813313 -30.751587  -4.8750389 0.0008081 ##
# OpTIS:Cropland-Transect:Corn                NA         NA          NA        NA
# Transect:Cropland-Transect:Corn             NA         NA          NA        NA
# AgCensus:Soybeans-Transect:Corn             NA         NA          NA        NA
# OpTIS:Soybeans-Transect:Corn        -33.232654 -46.170928 -20.2943802 0.0000000  XX
# Transect:Soybeans-Transect:Corn     -31.497727 -44.436001 -18.5594532 0.0000000  XX
# OpTIS:Cropland-AgCensus:Cropland            NA         NA          NA        NA
# Transect:Cropland-AgCensus:Cropland         NA         NA          NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Cropland    -15.419341 -28.357615  -2.4810673 0.0072973 ##
# Transect:Soybeans-AgCensus:Cropland -13.684414 -26.622688  -0.7461402 0.0292280  ##
# Transect:Cropland-OpTIS:Cropland            NA         NA          NA        NA
# AgCensus:Soybeans-OpTIS:Cropland            NA         NA          NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA         NA          NA        NA
# Transect:Soybeans-OpTIS:Cropland            NA         NA          NA        NA
# AgCensus:Soybeans-Transect:Cropland         NA         NA          NA        NA
# OpTIS:Soybeans-Transect:Cropland            NA         NA          NA        NA
# Transect:Soybeans-Transect:Cropland         NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA         NA          NA        NA
# Transect:Soybeans-AgCensus:Soybeans         NA         NA          NA        NA 
# Transect:Soybeans-OpTIS:Soybeans      1.734927 -11.203347  14.6732011 0.9999727 ##



ccsil <- aov(value~source:crop_name, data=dat.reg[dat.reg$year==2017& dat.reg$variable=="perc_cc" & dat.reg$region=="Southern IL",])
summary(ccsil)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# source:crop_name  2  416.2  208.12   14.94 2.17e-06 ***
#   Residuals        98 1365.3   13.93  
Tukoutccsil <- TukeyHSD(ccsil)
Tukoutccsil
# $`source:crop_name`
# diff       lwr       upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA        NA        NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA        NA        NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA        NA        NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA        NA        NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA        NA        NA        NA
# AgCensus:Cropland-OpTIS:Corn        -4.4508286 -7.102167 -1.799490 0.0000595  ###
# OpTIS:Cropland-OpTIS:Corn                   NA        NA        NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA        NA        NA        NA
# OpTIS:Soybeans-OpTIS:Corn           -0.3231051 -2.974443  2.328233 0.9992458  XXX
# OpTIS:Cropland-AgCensus:Cropland            NA        NA        NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA        NA        NA        NA
# OpTIS:Soybeans-AgCensus:Cropland     4.1277234  1.496246  6.759201 0.0002107  ###
# AgCensus:Soybeans-OpTIS:Cropland            NA        NA        NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA        NA        NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA        NA        NA        NA

ntsil <- aov(value~source:crop_name, data=dat.reg[dat.reg$year==2017& dat.reg$variable=="perc_nt" & dat.reg$region=="Southern IL",])
summary(ntsil)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name   4  38749    9687      31 <2e-16 ***
#   Residuals        164  51247     312 
Tukoutntsil <- TukeyHSD(ntsil)
Tukoutntsil
# $`source:crop_name`
# diff        lwr         upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA         NA          NA        NA
# Transect:Corn-AgCensus:Corn                 NA         NA          NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA         NA          NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA         NA          NA        NA
# Transect:Cropland-AgCensus:Corn             NA         NA          NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA         NA          NA        NA
# Transect:Soybeans-AgCensus:Corn             NA         NA          NA        NA
# Transect:Corn-OpTIS:Corn            -37.180948 -50.760682 -23.6012133 0.0000000  ##
# AgCensus:Cropland-OpTIS:Corn        -24.673054 -38.151066 -11.1950415 0.0000015  ##
# OpTIS:Cropland-OpTIS:Corn                   NA         NA          NA        NA
# Transect:Cropland-OpTIS:Corn                NA         NA          NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA         NA          NA        NA
# OpTIS:Soybeans-OpTIS:Corn             3.064446 -10.413566  16.5424580 0.9985299 XX
# Transect:Soybeans-OpTIS:Corn         -9.767044 -23.245056   3.7109681 0.3615176 XX
# AgCensus:Cropland-Transect:Corn      12.507894  -1.071840  26.0876285 0.0973598 ##
# OpTIS:Cropland-Transect:Corn                NA         NA          NA        NA
# Transect:Cropland-Transect:Corn             NA         NA          NA        NA
# AgCensus:Soybeans-Transect:Corn             NA         NA          NA        NA
# OpTIS:Soybeans-Transect:Corn         40.245394  26.665659  53.8251280 0.0000000 XX
# Transect:Soybeans-Transect:Corn      27.413904  13.834169  40.9936381 0.0000001 XX
# OpTIS:Cropland-AgCensus:Cropland            NA         NA          NA        NA
# Transect:Cropland-AgCensus:Cropland         NA         NA          NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Cropland     27.737500  14.259487  41.2155116 0.0000000  ##
# Transect:Soybeans-AgCensus:Cropland  14.906010   1.427998  28.3840217 0.0183098 ##
# Transect:Cropland-OpTIS:Cropland            NA         NA          NA        NA
# AgCensus:Soybeans-OpTIS:Cropland            NA         NA          NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA         NA          NA        NA
# Transect:Soybeans-OpTIS:Cropland            NA         NA          NA        NA
# AgCensus:Soybeans-Transect:Cropland         NA         NA          NA        NA
# OpTIS:Soybeans-Transect:Cropland            NA         NA          NA        NA
# Transect:Soybeans-Transect:Cropland         NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA         NA          NA        NA
# Transect:Soybeans-AgCensus:Soybeans         NA         NA          NA        NA
# Transect:Soybeans-OpTIS:Soybeans    -12.831490 -26.309502   0.6465222 0.0756272 ##

rtsil <- aov(value~source:crop_name, data=dat.reg[dat.reg$year==2017& dat.reg$variable=="perc_rt" & dat.reg$region=="Southern IL",])
summary(rtsil)
#                   Df Sum Sq Mean Sq F value   Pr(>F)    
# source:crop_name   4   9644  2411.0   14.93 1.99e-10 ***
#   Residuals        164  26480   161.5   
Tukoutrtsil <- TukeyHSD(rtsil)
Tukoutrtsil
# $`source:crop_name`
# diff         lwr         upr     p adj
# OpTIS:Corn-AgCensus:Corn                     NA          NA          NA        NA
# Transect:Corn-AgCensus:Corn                  NA          NA          NA        NA
# AgCensus:Cropland-AgCensus:Corn              NA          NA          NA        NA
# OpTIS:Cropland-AgCensus:Corn                 NA          NA          NA        NA
# Transect:Cropland-AgCensus:Corn              NA          NA          NA        NA
# AgCensus:Soybeans-AgCensus:Corn              NA          NA          NA        NA
# OpTIS:Soybeans-AgCensus:Corn                 NA          NA          NA        NA
# Transect:Soybeans-AgCensus:Corn              NA          NA          NA        NA
# Transect:Corn-OpTIS:Corn             -8.6869283 -18.4483533   1.0744967 0.1240418  ##
# AgCensus:Cropland-OpTIS:Corn         10.2341471   0.5458424  19.9224517 0.0297490  ##
# OpTIS:Cropland-OpTIS:Corn                    NA          NA          NA        NA
# Transect:Cropland-OpTIS:Corn                 NA          NA          NA        NA
# AgCensus:Soybeans-OpTIS:Corn                 NA          NA          NA        NA
# OpTIS:Soybeans-OpTIS:Corn             0.2315333  -9.4567714   9.9198379 1.0000000  XX
# Transect:Soybeans-OpTIS:Corn        -11.0427215 -20.7310262  -1.3544168 0.0129598 XX
# AgCensus:Cropland-Transect:Corn      18.9210754   9.1596504  28.6825004 0.0000003 ##
# OpTIS:Cropland-Transect:Corn                 NA          NA          NA        NA
# Transect:Cropland-Transect:Corn              NA          NA          NA        NA
# AgCensus:Soybeans-Transect:Corn              NA          NA          NA        NA
# OpTIS:Soybeans-Transect:Corn          8.9184616  -0.8429634  18.6798866 0.1032619  XX
# Transect:Soybeans-Transect:Corn      -2.3557932 -12.1172182   7.4056318 0.9977535 XX
# OpTIS:Cropland-AgCensus:Cropland             NA          NA          NA        NA
# Transect:Cropland-AgCensus:Cropland          NA          NA          NA        NA
# AgCensus:Soybeans-AgCensus:Cropland          NA          NA          NA        NA
# OpTIS:Soybeans-AgCensus:Cropland    -10.0026138 -19.6909185  -0.3143091 0.0372369  ##
# Transect:Soybeans-AgCensus:Cropland -21.2768686 -30.9651733 -11.5885639 0.0000000 ##
# Transect:Cropland-OpTIS:Cropland             NA          NA          NA        NA
# AgCensus:Soybeans-OpTIS:Cropland             NA          NA          NA        NA
# OpTIS:Soybeans-OpTIS:Cropland                NA          NA          NA        NA
# Transect:Soybeans-OpTIS:Cropland             NA          NA          NA        NA
# AgCensus:Soybeans-Transect:Cropland          NA          NA          NA        NA
# OpTIS:Soybeans-Transect:Cropland             NA          NA          NA        NA
# Transect:Soybeans-Transect:Cropland          NA          NA          NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans             NA          NA          NA        NA
# Transect:Soybeans-AgCensus:Soybeans          NA          NA          NA        NA
# Transect:Soybeans-OpTIS:Soybeans    -11.2742548 -20.9625595  -1.5859501 0.0100852  ##

ctsil <- aov(value~source:crop_name, data=dat.reg[dat.reg$year==2017& dat.reg$variable=="perc_ct" & dat.reg$region=="Southern IL",])
summary(ctsil)
# source:crop_name   4  54635   13659   49.76 <2e-16 ***
#   Residuals        160  43918     274       
Tukoutctsil <- TukeyHSD(ctsil)
Tukoutctsil
# $`source:crop_name`
# diff        lwr        upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA         NA         NA        NA
# Transect:Corn-AgCensus:Corn                 NA         NA         NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA         NA         NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA         NA         NA        NA
# Transect:Cropland-AgCensus:Corn             NA         NA         NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA         NA         NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA         NA         NA        NA
# Transect:Soybeans-AgCensus:Corn             NA         NA         NA        NA
# Transect:Corn-OpTIS:Corn             47.276727  34.350522  60.202931 0.0000000  ##
# AgCensus:Cropland-OpTIS:Corn         15.844727   3.012447  28.677007 0.0046488  ##
# OpTIS:Cropland-OpTIS:Corn                   NA         NA         NA        NA
# Transect:Cropland-OpTIS:Corn                NA         NA         NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA         NA         NA        NA
# OpTIS:Soybeans-OpTIS:Corn            -4.771617 -17.796874   8.253640 0.9649795  XX
# Transect:Soybeans-OpTIS:Corn         19.265586   6.433306  32.097866 0.0001732  XX
# AgCensus:Cropland-Transect:Corn     -31.432000 -44.163725 -18.700274 0.0000000  ##
# OpTIS:Cropland-Transect:Corn                NA         NA         NA        NA
# Transect:Cropland-Transect:Corn             NA         NA         NA        NA
# AgCensus:Soybeans-Transect:Corn             NA         NA         NA        NA
# OpTIS:Soybeans-Transect:Corn        -52.048344 -64.974548 -39.122140 0.0000000  XX
# Transect:Soybeans-Transect:Corn     -28.011141 -40.742866 -15.279415 0.0000000  XX
# OpTIS:Cropland-AgCensus:Cropland            NA         NA         NA        NA
# Transect:Cropland-AgCensus:Cropland         NA         NA         NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA         NA         NA        NA
# OpTIS:Soybeans-AgCensus:Cropland    -20.616344 -33.448624  -7.784065 0.0000409  ##
# Transect:Soybeans-AgCensus:Cropland   3.420859  -9.215496  16.057214 0.9949914  ##
# Transect:Cropland-OpTIS:Cropland            NA         NA         NA        NA
# AgCensus:Soybeans-OpTIS:Cropland            NA         NA         NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA         NA         NA        NA
# Transect:Soybeans-OpTIS:Cropland            NA         NA         NA        NA
# AgCensus:Soybeans-Transect:Cropland         NA         NA         NA        NA
# OpTIS:Soybeans-Transect:Cropland            NA         NA         NA        NA
# Transect:Soybeans-Transect:Cropland         NA         NA         NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA         NA         NA        NA
# Transect:Soybeans-AgCensus:Soybeans         NA         NA         NA        NA
# Transect:Soybeans-OpTIS:Soybeans     24.037203  11.204924  36.869483 0.0000008  ##




############# RATE OF CHANGE BY REGION

# Ag Census rate of change 

# what is the REGIONAL level % adoption AgCensus  ---values for Table 1 AgCensus
region_sum <- filter(dat.reg) %>%
  group_by(region, year, source, variable) %>%
  summarize(mean = mean(value),
            se=se(value)) 

region_sum$yrmark <- ifelse(region_sum$source!="AgCensus" & region_sum$year==2015, "a",
                            ifelse(region_sum$source=="OpTIS" & region_sum$year==2021, "b",
                                   ifelse(region_sum$source=="Transect" & region_sum$year==2018, "b",
                                   ifelse(region_sum$source=="AgCensus" & region_sum$year==2017, "a",
                                          ifelse(region_sum$source=="AgCensus" & region_sum$year==2022, "b", "X"
                                                 )))))

region_sum <- region_sum[region_sum$yrmark != "X",]
region_suma <- region_sum[region_sum$yrmark =="a",c(-2, -6, -7)]  #remove year, se, and yrmark columns
region_sumb <- region_sum[region_sum$yrmark =="b",c(-2, -6, -7)]

colnames(region_suma)[4] <- c("mean.a")
colnames(region_sumb)[4] <- c("mean.b")


region_sum2 <- full_join(region_suma, region_sumb)
rm(region_suma, region_sumb)

region_sum2$denom <- ifelse(region_sum2$source=="OpTIS", 2021-2015,
                            ifelse(region_sum2$source=="AgCensus", 2022-2017, 2018-2015))

region_sum2$rateofchange <- (region_sum2$mean.b - region_sum2$mean.a)/region_sum2$denom

region_sum2 <- region_sum2[,c("region", "source", "variable", "rateofchange", "mean.a", "mean.b", "denom")]

# cc  #### rates of change AgCensus Table 1
(region_cc[region_cc$region=="Central IL" & region_cc$year==2022,"mean.cc"] -
    region_cc[region_cc$region=="Central IL" & region_cc$year==2017,"mean.cc"] ) /
  (2022-2017)   # 0.21

(region_cc[region_cc$region=="Southern IL" & region_cc$year==2022,"mean.cc"] -
    region_cc[region_cc$region=="Southern IL" & region_cc$year==2017,"mean.cc"] ) /
  (2022-2017)   # 0.13

(region_cc[region_cc$region=="Western NY" & region_cc$year==2022,"mean.cc"] -
    region_cc[region_cc$region=="Western NY" & region_cc$year==2017,"mean.cc"] ) /
  (2022-2017)   # 0.32

# no till
(region_till[region_till$region=="Central IL" & region_till$year==2022 & region_till$variable=="perc_nt","mean.value"] -
    region_till[region_till$region=="Central IL" & region_till$year==2017 & region_till$variable=="perc_nt","mean.value"] ) /
  (2022-2017)  # 0.21

(region_till[region_till$region=="Southern IL" & region_till$year==2022 & region_till$variable=="perc_nt","mean.value"] -
    region_till[region_till$region=="Southern IL" & region_till$year==2017 & region_till$variable=="perc_nt","mean.value"] ) /
  (2022-2017)  # 0.76

(region_till[region_till$region=="Western NY" & region_till$year==2022 & region_till$variable=="perc_nt","mean.value"] -
    region_till[region_till$region=="Western NY" & region_till$year==2017 & region_till$variable=="perc_nt","mean.value"] ) /
  (2022-2017)  # 0.27

# reduced till
(region_till[region_till$region=="Central IL" & region_till$year==2022 & region_till$variable=="perc_rt","mean.value"] -
    region_till[region_till$region=="Central IL" & region_till$year==2017 & region_till$variable=="perc_rt","mean.value"] ) /
  (2022-2017)  # 0.03

(region_till[region_till$region=="Southern IL" & region_till$year==2022 & region_till$variable=="perc_rt","mean.value"] -
    region_till[region_till$region=="Southern IL" & region_till$year==2017 & region_till$variable=="perc_rt","mean.value"] ) /
  (2022-2017)  # -0.42

(region_till[region_till$region=="Western NY" & region_till$year==2022 & region_till$variable=="perc_rt","mean.value"] -
    region_till[region_till$region=="Western NY" & region_till$year==2017 & region_till$variable=="perc_rt","mean.value"] ) /
  (2022-2017)  # 0.58


# conventional till
(region_till[region_till$region=="Central IL" & region_till$year==2022 & region_till$variable=="perc_ct","mean.value"] -
    region_till[region_till$region=="Central IL" & region_till$year==2017 & region_till$variable=="perc_ct","mean.value"] ) /
  (2022-2017)  # -0.25

(region_till[region_till$region=="Southern IL" & region_till$year==2022 & region_till$variable=="perc_ct","mean.value"] -
    region_till[region_till$region=="Southern IL" & region_till$year==2017 & region_till$variable=="perc_ct","mean.value"] ) /
  (2022-2017)  # -0.34

(region_till[region_till$region=="Western NY" & region_till$year==2022 & region_till$variable=="perc_ct","mean.value"] -
    region_till[region_till$region=="Western NY" & region_till$year==2017 & region_till$variable=="perc_ct","mean.value"] ) /
  (2022-2017)  # -0.85







# Ag Census rate of change BY STATE

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


# transect rate of change  ###### Table 1 Transect values
region_till_transect <- filter(dat, !variable=="perc_cc", source=="Transect", year %in% c(2015, 2017, 2018)) %>%
  group_by(year, variable, region) %>%
  summarize(mean.value = mean(na.omit(value)),
            se.value=se(na.omit(value)))

### Central IL
# conventional till
(region_till_transect[region_till_transect$year==2018 & 
                        region_till_transect$variable=="perc_ct" &
                        region_till_transect$region=="Central IL","mean.value"] -
    region_till_transect[region_till_transect$year==2015 & 
                           region_till_transect$variable=="perc_ct" &
                           region_till_transect$region=="Central IL","mean.value"] ) /
  (2018-2015)  # 0.61
# reduced till
(region_till_transect[region_till_transect$year==2018 & region_till_transect$variable=="perc_rt" &
                        region_till_transect$region=="Central IL","mean.value"] -
    region_till_transect[region_till_transect$year==2015 & region_till_transect$variable=="perc_rt" &
                         region_till_transect$region=="Central IL","mean.value"] ) /
  (2018-2015)  # 0.45
# no till
(region_till_transect[region_till_transect$year==2018 & region_till_transect$variable=="perc_nt" &
                      region_till_transect$region=="Central IL","mean.value"] -
    region_till_transect[region_till_transect$year==2015 & region_till_transect$variable=="perc_nt" &
    region_till_transect$region=="Central IL","mean.value"] ) /
  (2018-2015)  # -1.48


### Southern IL
# conventional till
(region_till_transect[region_till_transect$year==2018 & 
                        region_till_transect$variable=="perc_ct" &
                        region_till_transect$region=="Southern IL","mean.value"] -
    region_till_transect[region_till_transect$year==2015 & 
                           region_till_transect$variable=="perc_ct" &
                           region_till_transect$region=="Southern IL","mean.value"] ) /
  (2018-2015)  # 0.084
# reduced till
(region_till_transect[region_till_transect$year==2018 & region_till_transect$variable=="perc_rt" &
                        region_till_transect$region=="Southern IL","mean.value"] -
    region_till_transect[region_till_transect$year==2015 & region_till_transect$variable=="perc_rt" &
                           region_till_transect$region=="Southern IL","mean.value"] ) /
  (2018-2015)  # -0.486
# no till
(region_till_transect[region_till_transect$year==2018 & region_till_transect$variable=="perc_nt" &
                        region_till_transect$region=="Southern IL","mean.value"] -
    region_till_transect[region_till_transect$year==2015 & region_till_transect$variable=="perc_nt" &
                           region_till_transect$region=="Southern IL","mean.value"] ) /
  (2018-2015)  # 0.375





########### OpTIS 2017 adoption levels




