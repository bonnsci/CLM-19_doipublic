# This makes county heat maps for average % adoption 2018-2021 from OpTIS for 
# cover crops, no till, reduced till
# excludes counties whose harvested cropland acres in 2017 census is in the 
# lowest 10th percentile for the state and counties for which OpTIS evaluated
# < 50% of 2017 harvested cropland acres.See "OpTIS adoption rates IL.R" for details.
# adoption on just corn (silage and grain) and soybean acres only
# 

library(tidyverse)
library(maps)
library(reshape2)
# install.packages("sf")
library(sf)
# library(gridExtra)  # for grid.arrange()
# install.packages("patchwork")
library(patchwork) # for plot_layout()

# based on https://stackoverflow.com/questions/51463037/plotting-a-heat-map-of-pennsylvania-counties-in-r

# load data created in "OpTIS adoption rates NY.R"
means_countyny <- read.csv("data/optis/means_county_ny.csv") %>%
  arrange(county)
means_countyny$state <- rep("NY", nrow(means_countyny))
means_countyny <- means_countyny[,c(6, 1:5)]

means_countyil <- read.csv("data/optis/means_county_il.csv") 

dat <- bind_rows(means_countyny, means_countyil)

rm(means_countyil, means_countyny)

dat$co_st <- paste0(dat$county, "-", dat$state)


# define colors for YlOrBr by Paul Tol https://personal.sron.nl/~pault/
# how many colors do we need? do they all work with 5?
# hist(means_county$mean.perc[means_county$variable %in% "perc_cc"]) # 0-30
# hist(means_county$mean.perc[means_county$variable %in% "perc_nt"]) # 10-80
# hist(means_county$mean.perc[means_county$variable %in% "perc_rt"]) # 15-55
# hist(means_county$mean.perc[means_county$variable %in% "perc_ct"]) # 0-50
# pal5 <- c("#ffffe5", "#fee391", "#fb9a29", "#cc4c02", "#662506", "#888888")
# pal6 <- c("#fee391", "#fec44f", "#fb9a29", "#ec7014", "#cc4c02", "#993404", "#888888")
# pal6b <- c(rev(pal6[1:6]), pal6[7])
# # scales::show_col(pal6b)
# pal7 <- c("#fff7bc", "#fee391", "#fec44f", "#fb9a29", "#ec7014", "#cc4c02", "#993404", "#888888")
# pal8 <- c("#fff7bc", "#fee391", "#fec44f", "#fb9a29", "#ec7014", "#cc4c02", "#993404", "#662506", "#888888")
# pal8b <- c(rev(pal8[1:8]), pal8[9])
pal3new <- c("#8c510a", "#bf812d", "#dfc27d")
pal6new <- c("#8c510a", "#bf812d", "#dfc27d", "#80cdc1", "#35978f", "#01665e")
pal5new <- c("#bf812d", "#dfc27d", "#80cdc1", "#35978f", "#01665e")
pal8new <- c("#8c510a", "#bf812d", "#dfc27d", "#f6e8c3","#c7eae5", "#80cdc1", "#35978f", "#01665e")
palcc <- c("#ffffcc", "#d9f0a3", "#addd8e", "#78c679","#41ab5d", "#238443", "#005a32") # From https://colorbrewer2.org/#type=sequential&scheme=YlGn&n=7
#palcc <- c("#FFFFE5", "#F7FCB9", "#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#004529")
palnt <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837") # From https://colorbrewer2.org/#type=sequential&scheme=YlGn&n=5
palct <- c("#fff7bc", "#fec44f", "#d95f0e")  # From https://colorbrewer2.org/#type=sequential&scheme=YlOrBr&n=3
palrt <- c("#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#005a32")  # From https://colorbrewer2.org/#type=sequential&scheme=YlGn&n=8


# divide percents into bins
# what's the max and min for each variable?
ranges <- do.call(data.frame, aggregate(dat, mean.perc~variable, 
                                        FUN=function (x) c(min=min(x), max=max(x))))

# fn for rounding to the upper 5
round_any <- function(x, accuracy, f=round){f(x/accuracy)*accuracy}

ranges$min_bin <-round_any(ranges$mean.perc.min, 10, f=floor)
ranges$max_bin <-round_any(ranges$mean.perc.max, 10, f=ceiling)
ranges$bins <- c(5, 10, 10, 10)

dat <- inner_join(dat, ranges[,c(1,4:6)]) %>%
  mutate(binlo = round_any(mean.perc, bins, f=floor), 
         binhi = round_any(mean.perc, bins, f=ceiling),
         mybin = paste0(binlo, "-", binhi),
         bincheck = binhi-binlo # check to see if any bins are e.g., 18-18
  ) %>%
  arrange(variable, mean.perc)

# do any counties mybin = 0? (i.e. "18-18")
sum(dat$bincheck==0) # should be 0

# cleanup bins dataset
dat <- dat[,c(1:4,7,13)]




# add "%" to bin category to show up in plots
dat$mybin <- ifelse(dat$mybin=="NA", dat$mybin, paste0(dat$mybin, "%"))

dat$mybin <- ordered(dat$mybin, levels=unique(dat$mybin))

# ID focus areas
dat$focus_area <- ifelse(dat$co_st %in% c("Ford-IL", "Livingston-IL", "Macoupin-IL", "Montgomery-IL",
                                           "Wyoming-NY", "Livingston-NY", "Genesee-NY"), "yes", "no")



# get county spatial data
mp <- map_data("county", c("New York", "Illinois"), proj="mercator")


# check spelling of counties matches between ny and dat
# and get ready to join with dat data
mp$subregion <- str_to_title(mp$subregion)

mapnames <- unique(mp[,c('subregion', 'region')]) # 164 names  (102 counties in IL, 62 in NY = 164)
mapnames$state <- ifelse(mapnames$region=="illinois", "IL", "NY")
mapnames$co_st <- paste0(mapnames$subregion, "-", mapnames$state)
datnames <- unique(dat$co_st)  # data have 142 names
sort(datnames)
# what is different - are they MISSING or SPELLED DIFFERENT?
difs <- setdiff(mapnames$co_st, datnames)
# mapnames have spaces, datnames do not

dat$county2 <- ifelse(dat$county=="DeKalb", "De Kalb",
                      ifelse(dat$county=="DeWitt", "De Witt",
                             ifelse(dat$county=="DuPage", "Du Page",
                                    ifelse(dat$county=="LaSalle", "La Salle",
                                           ifelse(dat$county=="McDonough", "Mcdonough",
                                                  ifelse(dat$county=="McHenry", "Mchenry",
                                                         ifelse(dat$county=="McLean", "Mclean",
                                                                ifelse(dat$county=="St. Clair", "St Clair", dat$county))))))))

dat$co_st <- paste0(dat$county2, "-", dat$state)
datnames <- unique(dat$co_st)  
sort(datnames)
# what is different - are they MISSING or SPELLED DIFFERENT?
difs <- setdiff(mapnames$co_st, datnames)
# now seems that it is counties that are missing

# make complete list of counties x 4 variable types
vars <- unique(dat$variable)
mapnames2 <- bind_rows(mapnames, mapnames, mapnames, mapnames)
varnames <- c(rep(vars[1], nrow(mapnames)),
              rep(vars[2], nrow(mapnames)),
              rep(vars[3], nrow(mapnames)),
              rep(vars[4], nrow(mapnames)))
complete <- bind_cols(mapnames2[,c(1,3,4)], variable=varnames)
colnames(complete)[1] <- "county2"


dat <- full_join(dat, complete)

# datnames <- unique(dat$co_st)  
# difs <- setdiff(mapnames$co_st, datnames)

rm(complete, vars, mapnames, datnames, mapnames2, varnames)



# join rates with spatial data:
mp$state <- ifelse(mp$region=="illinois", "IL", "NY")
mp <- left_join(mp, dat[,c(1,3:8)], join_by(subregion==county2, state==state), relationship="many-to-many")


# make a map
# nysf <- st_as_sf(ny, coords=c("long", "lat"), crs=4326)


ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)



windows(xpinch=200, ypinch=200, width=5, height=5)

# plot for cover crops   ########## Left off here, needs more troubleshooting

mpcc <- mp[mp$variable=="perc_cc",]
unique(mpcc$mybin)
levels(mpcc$mybin)
mpcc$mybin <- as.character(mpcc$mybin)
unique(mpcc$mybin)
mpcc$mybin <- ordered(mpcc$mybin, levels=c("0-5%", "5-10%", "10-15%", "15-20%", "20-25%", "25-30%", "30-35%", "35-40%", "40-45%"))
levels(mpcc$mybin)
unique(mpcc$mybin)

palcc <- c("#fff7fb", "#ece7f2", "#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858")

pcc <- ggplot() +
  geom_polygon(data=mpcc,
               mapping=aes(x=long, y=lat, fill=mybin, group=group),
               color="#20243d", linewidth=0.2) +
  geom_polygon(data=mpcc[mpcc$focus_area == "yes",],
               mapping=aes(x=long, y=lat, group=group),
               color="blue", linewidth=0.8, fill=NA) +
  scale_fill_manual(values=palcc,
                    na.value = "gray80",
                    name= "Cover crops",
                    drop=FALSE) +
 coord_sf(default_crs = sf::st_crs(4326)) +  # , # read x and y as latitude and longitude
  #          xlim = c(-92, -87), ylim=c(36.5, 43)) +
  theme_bw() +
  ditch_the_axes +
  theme(legend.title=element_text(size=10, face="bold"),
        legend.text=element_text(size=10),
        legend.key.size=unit(0.4, "cm"),
        plot.margin = unit(c(1, 0, 1,0 ), "cm"))

pcc

ggsave("plots/maps/county_heatmap_NYIL_cc_wfocuscounties.png")





# plot for no-till
mpnt <- mp[mp$variable=="perc_nt",]
unique(mpnt$mybin)
levels(mpnt$mybin)
mpnt$mybin <- as.character(mpnt$mybin)
unique(mpnt$mybin)
mpnt$mybin <- ordered(mpnt$mybin, levels=c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%"))
levels(mpnt$mybin)
unique(mpnt$mybin)

palnt <- c("#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529")

pnt <- ggplot() +
  geom_polygon(data=mpnt,
               mapping=aes(x=long, y=lat, fill=mybin, group=group),
               color="#20243d", linewidth=0.2) +
  geom_polygon(data=mpnt[mpnt$focus_area == "yes",],
               mapping=aes(x=long, y=lat, group=group),
               color="blue", linewidth=0.8, fill=NA) +
  scale_fill_manual(values=palnt,
                    na.value = "gray80",
                    name= "No-till",
                    drop=FALSE) +
  coord_sf(default_crs = sf::st_crs(4326)) +  # , # read x and y as latitude and longitude
  #          xlim = c(-92, -87), ylim=c(36.5, 43)) +
  theme_bw() +
  ditch_the_axes +
  theme(legend.title=element_text(size=10, face="bold"),
        legend.text=element_text(size=10),
        legend.key.size=unit(0.4, "cm"),
        plot.margin = unit(c(1, 0, 1,0 ), "cm"))

pnt

ggsave("plots/maps/county_heatmap_NYIL_nt_wfocuscounties.png")





# plot for reduced-till

mprt <- mp[mp$variable=="perc_rt",]
unique(mprt$mybin)
levels(mprt$mybin)
mprt$mybin <- as.character(mprt$mybin)
unique(mprt$mybin)
mprt$mybin <- ordered(mprt$mybin, levels=c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%"))
levels(mprt$mybin)
unique(mprt$mybin)

palrt <- c("#fcfbfb", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#4a1486")

prt <- ggplot() +
  geom_polygon(data=mprt,
               mapping=aes(x=long, y=lat, fill=mybin, group=group),
               color="#20243d", linewidth=0.2) +
  geom_polygon(data=mprt[mprt$focus_area == "yes",],
               mapping=aes(x=long, y=lat, group=group),
               color="blue", linewidth=0.8, fill=NA) +
  scale_fill_manual(values=palrt,
                    na.value = "gray80",
                    name= "Reduced tillage",
                    drop=FALSE) +
  coord_sf(default_crs = sf::st_crs(4326)) +  # , # read x and y as latitude and longitude
  #          xlim = c(-92, -87), ylim=c(36.5, 43)) +
  theme_bw() +
  ditch_the_axes +
  theme(legend.title=element_text(size=10, face="bold"),
        legend.text=element_text(size=10),
        legend.key.size=unit(0.4, "cm"),
        plot.margin = unit(c(1, 0, 1,0 ), "cm"))

prt

ggsave("plots/maps/county_heatmap_NYIL_rt_wfocuscounties.png")





# plot for conventional-till
mpct <- mp[mp$variable=="perc_ct",]
unique(mpct$mybin)
levels(mpct$mybin)
mpct$mybin <- as.character(mpct$mybin)
unique(mpct$mybin)
mpct$mybin <- ordered(mpct$mybin, levels=c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%"))
levels(mpct$mybin)
unique(mpct$mybin)

palct <- c("#ffffd4", "#fed983", "#fe9929", "#d95f0e", "#993404")

pct <- ggplot() +
  geom_polygon(data=mpct,
               mapping=aes(x=long, y=lat, fill=mybin, group=group),
               color="#20243d", linewidth=0.2) +
  geom_polygon(data=mpct[mpct$focus_area == "yes",],
               mapping=aes(x=long, y=lat, group=group),
               color="blue", linewidth=0.8, fill=NA) +
  scale_fill_manual(values=palct,
                    na.value = "gray80",
                    name= "Conventional tillage",
                    drop=FALSE) +
  coord_sf(default_crs = sf::st_crs(4326)) +  # , # read x and y as latitude and longitude
  #          xlim = c(-92, -87), ylim=c(36.5, 43)) +
  theme_bw() +
  ditch_the_axes +
  theme(legend.title=element_text(size=10, face="bold"),
        legend.text=element_text(size=10),
        legend.key.size=unit(0.4, "cm"),
        plot.margin = unit(c(1, 0, 1,0 ), "cm"))

pct

ggsave("plots/maps/county_heatmap_NYIL_ct_wfocuscounties.png")





# plot all 4 plots at once

# grid.arrange(pcc, pnt, prt, pct, nrow=2) # plot sizes vary because legend titles are different widths



pcc + pnt + prt + pct + plot_layout(ncol=1)
ggsave("plots/maps/county_heatmap_NYIL_4x2_focus counties.png", width=6, height=14, units="in")







# INCOMPLETE
# add rivers (following https://milospopovic.net/map-rivers-with-sf-and-ggplot2-in-r/)

filenames <- list.files("data/gis/NHD_H_Illinois_State_Shape_NAD83/Shape", pattern="*.shp", full.names=T)
## left off here trying to understand which file to use
## check out https://pubs.er.usgs.gov/publication/ofr20191096
## might also search for mapping NHD in R 

riv_list <- lapply(filenames, st_read)  
il_riv <- riv_list[[1]] |>
  sf::st_cast("MULTILINESTRING")

il_map + 
  ggplot() +
  geom_sf(data=il_riv, aes(x=geometry$X, y=geometry$Y), color="midnightblue", lwd=1) +
  coord_sf(default_crs = sf::st_crs(4326))
