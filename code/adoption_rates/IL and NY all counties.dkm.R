# script started 6/13/2024 by Bonnie McGill
# Modified by Dan Manter

# here we pull together optis, us census of agriculture (via carpe), and il tillage transect 
# survey data for these counties
library(ggplot2)
library(tidyverse)
library(maps)
library(sf)

se <- function(x) sd(x, na.rm=T) / sqrt(length(x))


## to do:
# Fix legend titles in maps for agcensus vs optis (tillage and cc)
# plot maps together
# plot plots together



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
                   aes(label = paste(after_stat(rr.label), after_stat(p.label), sep="~`,`~")),
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
                   aes(label = paste(after_stat(rr.label), after_stat(p.label), sep="~`,`~")),
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
                       label = paste(after_stat(rr.label), after_stat(p.label), sep="~`,`~")),
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
        legend.text=element_text(size=10, margin=margin(l=0.05)),
        strip.text=element_text(size=10, face="bold"),
        legend.title=element_text(size=11),
        axis.title=element_text(size=10),
        axis.text=element_text(size=8))
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
        legend.position = c(0.8, 0.5),
        legend.box.spacing = unit(0.2, "cm"),
        legend.text=element_text(size=10, margin=margin(l=5)),
        strip.text=element_text(size=10, face="bold"),
        legend.title=element_text(size=11),
        axis.title=element_text(size=10),
        axis.text=element_text(size=8))
ggsave("plots/adoption/OpTIS_vs_AgCensus.histogram.cc.png", height=3, width=3)


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





