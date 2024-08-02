# script started 6/13/2024 by Bonnie McGill
# Modified by Dan Manter

# here we pull together optis, us census of agriculture (via carpe), and il tillage transect 
# survey data for these counties
library(ggplot2)
library(tidyverse)
library(maps)
library(sf)

se <- function(x) sd(x, na.rm=T) / sqrt(length(x))


### load and prep optis data
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


### load and prep census data
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
censb <- cens
censb$crop_name <- rep("Corn", nrow(censb))
cens <- bind_rows(cens, censb)
rm(censb)

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
till$state <- rep("Illinois", nrow(till))
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
till$crop_name <- ifelse(till$crop_name=="corn", "Corn", "Soybeans")

till$county <- ifelse(till$county=="JoDaviess", "Jo Daviess",
                      ifelse(till$county== "St Clair", "St. Clair",
                             ifelse(till$county== "StClair", "St. Clair",
                                    ifelse(till$county=="RockIsland", "Rock Island", till$county))))

### account for lag between OpTIS and AgCensus/Survey and combine
cens <- cens %>%   select(-source) # AgCensus
opt <- opt %>%   select(-source)  # OpTIS
opt$year <- opt$year - 1
till <- till %>%   select(-source) # Survey

dat <- cens %>%
  full_join(opt, by=c('year', 'state', 'county', 'variable', 'crop_name')) 
names(dat) <- c('year', 'state', 'county', 'variable', 'AgCensus', 'crop_name', 'OpTIS')

dat <- dat %>%
  full_join(till, by=c('state', 'county', 'year', 'crop_name', 'variable'))
names(dat) <- c('year', 'state', 'county', 'variable', 'AgCensus', 'crop_name', 'OpTIS', 'Transect')

dat.2017 <- dat %>%
  filter(year == 2017)

dat.2017 <- dat.2017[dat.2017$variable != 'perc_cc',]
dat.2017$variable <- factor(dat.2017$variable, levels=c('perc_ct', 'perc_rt', 'perc_nt'))
levels(dat.2017$variable) <- c('Conventional', 'Reduced', 'No-till')
dat.2017$county.lower <- tolower(dat.2017$county) # put county names in lower case to match with map data below
dat.2017$state.lower <- tolower(dat.2017$state)
### graphs comparing tillage estimates for  OpTIS and Transect - Illinois only

windows(xpinch=200, ypinch=200, width=5, height=5)

till3col <- c("#88CCEE", "#999933", "#AA4499")

ggplot(dat.2017, aes(x=Transect, y=OpTIS)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(aes(fill=variable), shape=21, size=3) + 
  #scale_fill_brewer(palette='Set1', labels=c('Corn', 'Soybeans'), name='Crop') +
  scale_fill_manual(values=till3col, name='Tillage Type') +
  # scale_fill_brewer(palette='Set1', name='Tillage Type') +
  #stat_smooth(method='lm', se=FALSE) +
  ggpubr::stat_cor(method='spearman',
                   aes(label=after_stat(r.label)),  # not showing p-value because this is not a subsample of a whole population, this is all the counties
                       r.accuracy=0.01,
                   #aes(label = paste(after_stat(rr.label), after_stat(p.label), sep="~`,`~")), makes it say R2 which technically is incorrect
                   label.y=100) +
  facet_grid(. ~ crop_name) + 
  xlim(0,100) + ylim(0,100) +
  labs(y="Adoption (%) from OpTIS", x="Adoption (%) from Transect") + 
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
        legend.key.spacing.x=unit(1, "cm"),
        legend.position = "top",
        legend.box.spacing = unit(0.05, "cm"),
        legend.text=element_text(size=14, margin=margin(l=0.05)),
        strip.text=element_text(size=14, face="bold"),
        legend.title=element_text(size=16),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12))
ggsave("plots/adoption/OpTIS_vs_Transect.scatter.png", height=4, width=7)


### graph residuals by crop
resid.2017 <- dat.2017 %>%
  mutate(resid = OpTIS - Transect)
         

resid.2017 %>%
  group_by(crop_name, variable) %>%
  summarize(mean = mean(resid, na.rm=T),
            sd = sd(resid, na.rm=T))


il <- map_data("county", "Illinois")
# fix spellings to match adoption data
il$subregion <- ifelse(il$subregion == "de kalb", "dekalb",
                       ifelse(il$subregion == "du page", "dupage",
                              ifelse(il$subregion == "de witt", "dewitt",
                                    ifelse(il$subregion == "la salle", "lasalle",
                                           ifelse(il$subregion == "st clair", "st. clair", il$subregion)) )))
mapnames <- data.frame(county=sort(unique(il$subregion)))   # 102 names in map
# countynames2 <- unique(resid.2017[c("county.lower", "state")])
# countynames2 <- countynames2[countynames2$state=="Illinois",]
# check <- left_join(mapnames, countynames2, join_by(county==county.lower))


il <- left_join(il, resid.2017, join_by(subregion==county.lower), relationship="many-to-many")
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)


# map.till <- ggplot() +
#   geom_polygon(data=il,
#                mapping=aes(x=long, y=lat, fill=resid, group=subregion),
#                color="#20243d", linewidth=0.2) +
#   #coord_sf(default_crs = sf::st_crs(4326)) + 
#   facet_grid(variable ~ crop_name, switch='y') +
#   scale_fill_gradient2(breaks=c(-Inf, -50, -25, 0, 25, 50, Inf),
#                        na.value = "gray80",
#                        name= "OpTIS (%) - \nTransect (%)") +
#   scale_color_manual(values=NA) +
#   theme_bw() +
#   ditch_the_axes +
#   theme(legend.title=element_text(size=10, face="bold"),
#         legend.text=element_text(size=10),
#         legend.key.size=unit(0.4, "cm"),
#         plot.margin = unit(c(1, 0, 1,0 ), "cm"))
# 
# map.till
# ggsave("plots/adoption/OpTIS_vs_Transect.map.png", height=10, width=4.6)


ggplot(resid.2017, aes(x=resid, fill=variable)) +
  #geom_histogram(aes(y = ..density..), position="identity", alpha=0.5, color='black') +
  geom_density(aes(color=variable), alpha=0.4) +
  scale_color_manual(values=till3col, name="Tillage type") +
  scale_fill_manual(values=till3col, name="Tillage type")+
  # scale_color_brewer(palette='Set1', name='Tillage Type') +
  # scale_fill_brewer(palette='Set1', name='Tillage Type') +
  facet_grid(. ~ crop_name) + 
  labs(y="Density", x="OpTIS (%) - Transect (%)") + 
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
        legend.key.spacing.x=unit(1, "cm"),
        legend.position = "bottom",
        legend.box.spacing = unit(0.2, "cm"),
        legend.text=element_text(size=14, margin=margin(l=5)),
        strip.text=element_text(size=14, face="bold"),
        legend.title=element_text(size=16),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12))
ggsave("plots/adoption/OpTIS_vs_Transect.histogram.png", height=4, width=7)

# are the residuals significantly different by tillage type?
till.tr <- aov(resid~variable*crop_name, data=resid.2017)
summary(till.tr)
# Df Sum Sq Mean Sq F value Pr(>F)    
#   variable             2 113228   56614 108.274 <2e-16 ***
#   crop_name            1     11      11   0.022  0.883    
#   variable:crop_name   2  88080   44040  84.226 <2e-16 ***
#   Residuals          585 305884     523                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 375 observations deleted due to missingness
TukeyHSD(till.tr)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = resid ~ variable * crop_name, data = resid.2017)
# 
# $variable
# diff       lwr        upr     p adj
# Reduced-Conventional  28.353159  22.93948  33.766842 0.0000000
# No-till-Conventional  -1.923678  -7.33736   3.490005 0.6814568
# No-till-Reduced      -30.276837 -35.69052 -24.863155 0.0000000
# 
# $crop_name
# diff       lwr     upr     p adj
# Soybeans-Corn 0.2779926 -3.416795 3.97278 0.8825738
# 
# $`variable:crop_name`
# diff        lwr         upr     p adj
# Reduced:Corn-Conventional:Corn           21.713484  12.373522  31.0534464 0.0000000
# No-till:Corn-Conventional:Corn           20.142308  10.802346  29.4822699 0.0000000
# Conventional:Soybeans-Conventional:Corn  10.510259   1.193913  19.8266058 0.0166478
# Reduced:Soybeans-Conventional:Corn       45.436026  36.119680  54.7523727 0.0000000
# No-till:Soybeans-Conventional:Corn      -13.256515 -22.572862  -3.9401690 0.0007616
# No-till:Corn-Reduced:Corn                -1.571176 -10.911139   7.7687856 0.9968102
# Conventional:Soybeans-Reduced:Corn      -11.203225 -20.519571  -1.8868786 0.0082028
# Reduced:Soybeans-Reduced:Corn            23.722542  14.406195  33.0388884 0.0000000
# No-till:Soybeans-Reduced:Corn           -34.970000 -44.286346 -25.6536534 0.0000000
# Conventional:Soybeans-No-till:Corn       -9.632049 -18.948395  -0.3157021 0.0379551
# Reduced:Soybeans-No-till:Corn            25.293718  15.977372  34.6100648 0.0000000
# No-till:Soybeans-No-till:Corn           -33.398823 -42.715170 -24.0824769 0.0000000
# Reduced:Soybeans-Conventional:Soybeans   34.925767  25.633096  44.2184377 0.0000000
# No-till:Soybeans-Conventional:Soybeans  -23.766775 -33.059446 -14.4741040 0.0000000
# No-till:Soybeans-Reduced:Soybeans       -58.692542 -67.985213 -49.3998710 0.0000000


### graphs comparing tillage estimates for OpTIS and AgCensus
dat.2017.mean <- dat.2017 %>%
  group_by(state.lower, state, county.lower, variable) %>%
  summarize(AgCensus = mean(AgCensus, na.rm=TRUE),
            OpTIS = mean(OpTIS, na.rm=TRUE),
            Transect = mean(Transect, na.rm=TRUE))

levels(dat.2017.mean$state) <- c("Illinois", "New York")

windows(xpinch=200, ypinch=200, width=5, height=5)


ggplot(dat.2017.mean, aes(x=AgCensus, y=OpTIS)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(aes(fill=variable), shape=21, size=3) + 
  #stat_smooth(method='lm', se=FALSE) +
  scale_fill_manual(values=till3col, name='Tillage Type') +
  # scale_fill_brewer(palette='Set1', name='Tillage Type') +
  ggpubr::stat_cor(method='spearman',
                   # aes(label = paste(after_stat(rr.label), after_stat(p.label), sep="~`,`~")),
                   aes(label=after_stat(r.label)),  # not showing p-value because this is not a subsample of a whole population, this is all the counties
                   r.accuracy=0.01,
                   label.y=100) +
  facet_grid(. ~ state) +  
  xlim(0,100) + ylim(0,100) +
  labs(y="Adoption (%) from OpTIS", x="Adoption (%) from AgCensus") + 
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
        legend.key.spacing.x=unit(1, "cm"),
        legend.position = "top",
        legend.box.spacing = unit(0.05, "cm"),
        legend.text=element_text(size=14, margin=margin(l=0.05)),
        strip.text=element_text(size=14, face="bold"),
        legend.title=element_text(size=16),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12))
ggsave("plots/adoption/OpTIS_vs_AgCensus.scatter.png", height=4, width=7)

### graph residuals by crop
resid.2017 <- dat.2017.mean %>%
  mutate(resid = OpTIS - AgCensus) %>%
  select(-Transect)


resid.2017 %>%
  group_by(state.lower, variable) %>%
  summarize(mean = mean(resid, na.rm=T),
            sd = sd(resid, na.rm=T))

all <- map_data("county", c("New York", "Illinois"))
all.county <- unique(all[c("subregion", "region")])
all.nycounty <- all.county[all.county$region=="new york",]
all$subregion <- ifelse(all$subregion == "de kalb", "dekalb",
                       ifelse(all$subregion == "du page", "dupage",
                              ifelse(all$subregion == "de witt", "dewitt",
                                     ifelse(all$subregion == "la salle", "lasalle",
                                            ifelse(all$subregion == "st clair", "st. clair", all$subregion)) )))

# unique(all[c("subregion", "region")])
# a <- unique(resid.2017[c("county.lower", "state.lower")])
# b <- a[a$state.lower=="new york",]
# colnames(b) <- c("subregion", "region")
# setdiff(all.nycounty, b)
# subregion   region
# 1     bronx new york
# 2    queens new york
# 3  richmond new york
# add above counties to resid dataset to prevents them from showing up in "NA" facet in map below
missing.counties <- data.frame(c(rep("new york", 3)),
                               c(rep("New York", 3)),
                               c("bronx", "queens", "richmond"),
                               c(rep("Conventional tillage", 3)),
                               c(NA, NA, NA),
                               c(NA, NA, NA),
                               c(NA, NA, NA))
names(missing.counties) <- c("state.lower", "state", "county.lower", "variable", "AgCensus", "OpTIS", "resid")
resid.2017 <- rbind(resid.2017, missing.counties)
resid.2017$variable <- factor(resid.2017$variable, 
                              levels=c('Conventional', 'Reduced', 'No-till'))


# join adoption and map data
all <- full_join(all, resid.2017, join_by(region==state.lower, subregion==county.lower), relationship='many-to-many')


# il <- all[all$region == 'illinois',]
# map.till <- ggplot() +
#   geom_polygon(data=all,
#                mapping=aes(x=long, y=lat, fill=resid, group=subregion),
#                color="#20243d", linewidth=0.2) +
#   #coord_sf(default_crs = sf::st_crs(4326)) + 
#   facet_grid(variable ~ state, scales='free', space='free', switch='y') +
#   scale_fill_gradient2(
#     na.value = "gray80",
#     breaks=c(-Inf, -50, -25, 0, 25, 50, Inf),
#     midpoint = 0,
#     name= "Difference (%pt)") +
#   scale_color_manual(values=NA) +
#   theme_bw() +
#   ditch_the_axes +
#   theme(legend.title=element_text(size=10, face="bold"),
#         legend.text=element_text(size=10),
#         legend.key.size=unit(0.4, "cm"),
#         plot.margin = unit(c(1, 0, 1,0 ), "cm"))
# 
# map.till
# ggsave("plots/adoption/OpTIS_vs_AgCensus.map.png", height=8, width=4.3)


ggplot(resid.2017, aes(x=resid, fill=variable)) +
  #geom_histogram(aes(y = ..density..), position="identity", alpha=0.5, color='black') +
  geom_density(aes(color=variable), alpha=0.4) +
  scale_color_manual(values=till3col, name="Tillage type") +
  scale_fill_manual(values=till3col, name="Tillage type")+
  # scale_color_brewer(palette='Set1', name='Tillage Type') +
  # scale_fill_brewer(palette='Set1', name='Tillage Type') +
  facet_grid(. ~ state) + #, labeller = as_labeller(c("illinois" = "Illinois", "new york" = "New York"))) + 
  #geom_vline(xintercept=0, color='black', lty=2) +
  labs(y="Density", x="OpTIS (%) - AgCensus (%)") + 
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
        legend.key.spacing.x=unit(1, "cm"),
        legend.position = "bottom",
        legend.box.spacing = unit(0.2, "cm"),
        legend.text=element_text(size=14, margin=margin(l=5)),
        strip.text=element_text(size=14, face="bold"),
        legend.title=element_text(size=16),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12))
ggsave("plots/adoption/OpTIS_vs_AgCensus.histogram.png", height=4, width=7)


# are the residuals significantly different by tillage type?

till.ce <- aov(resid~variable*state, data=resid.2017)
# summary(till.ce)
#                    Df Sum Sq Mean Sq F value Pr(>F)    
#   variable         2  14060    7030  40.799 <2e-16 ***
#   state            1    266     266   1.546  0.214    
#   variable:state   2  27181   13591  78.875 <2e-16 ***
#   Residuals      379  65304     172                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 101 observations deleted due to missingness
TukeyHSD(till.ce)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = resid ~ variable * state, data = resid.2017)
# 
# $variable
# diff       lwr       upr     p adj
# Reduced-Conventional 12.4018362  8.540934 16.262739 0.0000000
# No-till-Conventional 13.2607859  9.399883 17.121689 0.0000000
# No-till-Reduced       0.8589497 -2.986842  4.704741 0.8588886
# 
# $state
# diff       lwr      upr     p adj
# New York-Illinois -2.031593 -5.244445 1.181259 0.2145182
# 
# $`variable:state`
# diff         lwr        upr     p adj
# Reduced:Illinois-Conventional:Illinois        2.865868  -2.4250800   8.156816 0.6310444
# No-till:Illinois-Conventional:Illinois        4.543178  -0.7477699   9.834126 0.1390009
# Conventional:New York-Conventional:Illinois -31.834154 -40.1027859 -23.565521 0.0000000
# Reduced:New York-Conventional:Illinois       16.773634   8.7432866  24.803982 0.0000001
# No-till:New York-Conventional:Illinois       14.680641   6.6502934  22.710988 0.0000040
# No-till:Illinois-Reduced:Illinois             1.677310  -3.6136379   6.968258 0.9443404
# Conventional:New York-Reduced:Illinois      -34.700022 -42.9686539 -26.431389 0.0000000
# Reduced:New York-Reduced:Illinois            13.907766   5.8774186  21.938114 0.0000157
# No-till:New York-Reduced:Illinois            11.814773   3.7844254  19.845120 0.0004473
# Conventional:New York-No-till:Illinois      -36.377332 -44.6459641 -28.108699 0.0000000
# Reduced:New York-No-till:Illinois            12.230456   4.2001084  20.260803 0.0002390
# No-till:New York-No-till:Illinois            10.137463   2.1071152  18.167810 0.0045530
# Reduced:New York-Conventional:New York       48.607788  38.3675419  58.848033 0.0000000
# No-till:New York-Conventional:New York       46.514794  36.2745487  56.755040 0.0000000
# No-till:New York-Reduced:New York            -2.092993 -12.1418155   7.955829 0.9912297

### graphs comparing cover crops estimates for OpTIS and AgCensus
dat.2017 <- dat %>%
  filter(year == 2017)

dat.2017 <- dat.2017[dat.2017$variable == 'perc_cc',]

dat.2017.mean <- dat.2017 %>%
  group_by(state, county, variable) %>%
  summarize(AgCensus = mean(AgCensus, na.rm=TRUE),
            OpTIS = mean(OpTIS, na.rm=TRUE))

states2col <- c("#44AA99", "#332288")

ggplot(dat.2017.mean, aes(x=AgCensus, y=OpTIS, fill=state)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(shape=21, size=3) + 
  #stat_smooth(method='lm', se=FALSE) +
  scale_fill_manual(values=states2col, labels=c('Illinois', 'New York'), name='State') +
  scale_color_manual(values=states2col, labels=c('Illinois', 'New York'), name='State') +
  # scale_fill_brewer(palette='Set2', labels=c('Illinois', 'New York'), name='State') +
  ggpubr::stat_cor(method='spearman', 
                   aes(color=state,
                       label=after_stat(r.label)),  # not showing p-value because this is not a subsample of a whole population, this is all the counties
                   r.accuracy=0.01,
                   label.y=c(40,37)) +
  facet_grid(. ~ variable, labeller=as_labeller(c("perc_cc" = 'Cover Crop'))) + 
  xlim(0,40) + ylim(0,40) +
  labs(y="Adoption (%) from OpTIS", x="Adoption (%) from AgCensus") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
        legend.key.spacing.x=unit(1, "cm"),
        legend.position = c(0.8, 0.4),
        legend.box.spacing = unit(0.05, "cm"),
        legend.text=element_text(size=12, margin=margin(l=0.05)),
        strip.text=element_text(size=12, face="bold"),
        legend.title=element_text(size=13),
        axis.title=element_text(size=12),
        axis.text=element_text(size=11))
ggsave("plots/adoption/OpTIS_vs_AgCensus.scatter.cc.png", height=3, width=3)

resid.2017 <- dat.2017.mean %>%
  mutate(resid = OpTIS - AgCensus,
         county.lower = tolower(county),
         state.lower = tolower(state))

resid.2017 %>%
  group_by(state, variable) %>%
  summarize(mean = mean(resid, na.rm=T),
            sd = sd(resid, na.rm=T))



ggplot(resid.2017, aes(x=resid, fill=state)) +
  #geom_histogram(aes(y = ..density..), position="identity", alpha=0.5, color='black') +
  geom_density(aes(color=state), alpha=0.4) +
  scale_color_manual(values = states2col, labels=c('Illinois', 'New York'),  name='State') +
  scale_fill_manual(values=states2col, labels=c('Illinois', 'New York'), name='State') +
  # scale_color_brewer(palette='Set2', labels=c('Illinois', 'New York'),  name='State') +
  # scale_fill_brewer(palette='Set2', labels=c('Illinois', 'New York'), name='State') +
  facet_grid(. ~ variable, labeller = as_labeller(c("perc_cc" = "Cover Crop"))) + 
  #geom_vline(xintercept=0, color='black', lty=2) +
  labs(y="Density", x="OpTIS (%) - AgCensus (%)") + 
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
        legend.key.spacing.x=unit(1, "cm"),
        legend.position = c(0.78, 0.82),
        legend.box.spacing = unit(0.2, "cm"),
        legend.text=element_text(size=11, margin=margin(l=5)),
        strip.text=element_text(size=12, face="bold"),
        legend.title=element_text(size=12),
        axis.title=element_text(size=12),
        axis.text=element_text(size=11))
ggsave("plots/adoption/OpTIS_vs_AgCensus.histogram.cc.png", height=3, width=3.3)

# are the residuals significantly different by tillage type?

cc.ce <- aov(resid~state, data=resid.2017)
summary(cc.ce)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# state         1   1196  1195.7    30.1 2.04e-07 ***
#   Residuals   131   5204    39.7                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 28 observations deleted due to missingness
TukeyHSD(cc.ce)
Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = resid ~ state, data = resid.2017)
# 
# $state
#                     diff       lwr       upr p adj
# New York-Illinois -6.941947 -9.445089 -4.438805 2e-07



all <- map_data("county", c("New York", "Illinois"))
all.county <- unique(all[c("subregion", "region")])
all.nycounty <- all.county[all.county$region=="new york",]
all$subregion <- ifelse(all$subregion == "de kalb", "dekalb",
                        ifelse(all$subregion == "du page", "dupage",
                               ifelse(all$subregion == "de witt", "dewitt",
                                      ifelse(all$subregion == "la salle", "lasalle",
                                             ifelse(all$subregion == "st clair", "st. clair", all$subregion)) )))

unique(all[c("subregion", "region")])

a <- unique(resid.2017[c("county.lower", "state.lower")])
b <- a[a$state.lower=="new york",]
colnames(b) <- c("subregion", "region")
setdiff(all.nycounty, b)
# subregion   region
# 1     bronx new york
# 2    queens new york
# 3  richmond new york
# add above counties to resid dataset to prevents them from showing up in "NA" facet in map below
missing.counties <- data.frame(
                               c(rep("New York", 3)),
                               c("Bronx", "Queens", "Richmond"),
                               c(rep("perc_cc", 3)),
                               c(NA, NA, NA),
                               c(NA, NA, NA),
                               c(NA, NA, NA),
                               c("bronx", "queens", "richmond"),
                               c(rep("new york", 3)))
names(missing.counties) <- c("state", "county", "variable", "AgCensus", "OpTIS", "resid", "county.lower", "state.lower")
resid.2017 <- rbind(resid.2017, missing.counties)




all <- full_join(all, resid.2017, join_by(region==state.lower, subregion==county.lower), relationship='many-to-many')

#il <- all[all$region == 'illinois',]
map.cc <- ggplot() +
  geom_polygon(data=all,
               mapping=aes(x=long, y=lat, fill=resid, group=subregion),
               color="#20243d", linewidth=0.2) +
  #coord_sf(default_crs = sf::st_crs(4326)) + 
  facet_grid(variable ~ state, scales='free', space='free', switch='y') +
  scale_fill_gradient2(
    na.value = "gray80",
    #breaks=c(-Inf, -50, -25, 0, 25, 50, Inf),
    midpoint = 0,
    name= "Difference (%pt)") +
  scale_color_manual(values=NA) +
  theme_bw() +
  ditch_the_axes +
  theme(legend.title=element_text(size=10, face="bold"),
        legend.text=element_text(size=10),
        legend.key.size=unit(0.4, "cm"),
        plot.margin = unit(c(1, 0, 1,0 ), "cm"))

map.cc
ggsave("plots/adoption/OpTIS_vs_AgCensus.map.cc.png", height=4, width=5)





