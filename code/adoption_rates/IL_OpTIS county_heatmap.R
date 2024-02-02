# This makes county heat maps for average % adoption 2018-2021 from OpTIS for 
# cover crops, no till, reduced till
# excludes counties whose harvested cropland acres in 2017 census is in the 
# lowest 10th percentile for the state and counties for which OpTIS evaluated
# < 50% of 2017 harvested cropland acres.See "OpTIS adoption rates IL.R" for details.
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

# load data created in "OpTIS adoption rates IL.R"
means_county <- read.csv("data/optis/means_county_il.csv")
# meansw <- dcast(means_county[,1:4], ...~variable, value.var = "mean.perc")

# unique(ill$subregion)
means_county <- means_county %>%
  arrange(county)
# unique(means_county$county) 

# get map data to match county names
means_county$county <- str_remove(means_county$county, " County")



# define colors for YlOrBr by Paul Tol https://personal.sron.nl/~pault/
# how many colors do we need? do they all work with 5?
hist(means_county$mean.perc[means_county$variable %in% "perc_cc"]) # 0-30
hist(means_county$mean.perc[means_county$variable %in% "perc_nt"]) # 10-80
hist(means_county$mean.perc[means_county$variable %in% "perc_rt"]) # 15-55
hist(means_county$mean.perc[means_county$variable %in% "perc_ct"]) # 0-50
pal5 <- c("#ffffe5", "#fee391", "#fb9a29", "#cc4c02", "#662506", "#888888")
pal6 <- c("#fee391", "#fec44f", "#fb9a29", "#ec7014", "#cc4c02", "#993404", "#888888")
pal6b <- c(rev(pal6[1:6]), pal6[7])
# scales::show_col(pal6b)
pal7 <- c("#fff7bc", "#fee391", "#fec44f", "#fb9a29", "#ec7014", "#cc4c02", "#993404", "#888888")
pal8 <- c("#fff7bc", "#fee391", "#fec44f", "#fb9a29", "#ec7014", "#cc4c02", "#993404", "#662506", "#888888")
pal8b <- c(rev(pal8[1:8]), pal8[9])


# divide percents into bins
# what's the max and min for each variable?
ranges <- do.call(data.frame, aggregate(means_county, mean.perc~variable, FUN=function (x) c(min=min(x), max=max(x))))
colnames(ranges)
# fn for rounding to the upper 5
round_any <- function(x, accuracy, f=round){f(x/accuracy)*accuracy}

ranges$min_bin <-round_any(ranges$mean.perc.min, 10, f=floor)
ranges$max_bin <-round_any(ranges$mean.perc.max, 10, f=ceiling)
ranges$bins <- c(5, 10, 10, 10)

means_county <- inner_join(means_county, ranges[,c(1,4:6)]) %>%
  mutate(binlo = round_any(mean.perc, bins, f=floor), 
         binhi = round_any(mean.perc, bins, f=ceiling),
         mybin = paste0(binlo, "-", binhi),
         bincheck = binhi-binlo # check to see if any bins are e.g., 18-18
  ) %>%
  arrange(variable, mean.perc)

# do any counties mybin = 0? (i.e. "18-18")
sum(means_county$bincheck==0) # should be 0

# cleanup
means_county <- means_county[,c(2,3,4,6,12)]

# add NA entries for each variable for the excluded counties so they fill gray in the maps

# Cook, DuPage, Hardin, Johnson, Pope, Jo Daviess, Union
county2 <- c("Cook", "DuPage", "Hardin", "Johnson", "Pope", "Jo Daviess", "Union", "Lake")
variable2 <- c(unique(means_county$variable))
variable3 <- rep(variable2, length(county2))
county2 <- sort(rep(county2, length(variable2))) 

dat_na <- data.frame(county=county2, variable=variable3)

means_county <- full_join(means_county, dat_na)

means_county$mybin <- ordered(means_county$mybin, levels=unique(means_county$mybin))

# get county spatial data
ill <- map_data("county", "Illinois", proj="mercator")

# get ready to join with means_county data
ill$subregion <- str_to_title(ill$subregion)
ill$subregion <- ifelse(ill$subregion %in% "De Kalb", "DeKalb", 
                        ifelse(ill$subregion %in% "De Witt", "DeWitt", 
                               ifelse(ill$subregion %in% "Du Page", "DuPage", 
                                      ifelse(ill$subregion %in% "La Salle", "LaSalle",
                                             ifelse(ill$subregion %in% "Mcdonough", "McDonough",
                                                    ifelse(ill$subregion %in% "Mchenry", "McHenry",
                                                           ifelse(ill$subregion %in% "Mclean", "McLean",
                                                                  ifelse(ill$subregion %in% "St Clair", "St. Clair", 
                                                                         ill$subregion))))))))



# join rates with spatial data:
ill <- left_join(ill, means_county, join_by(subregion==county), relationship="many-to-many")

rm(dat_na)


# make a map
# illsf <- st_as_sf(ill, coords=c("long", "lat"), crs=4326)


ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)



windows(xpinch=200, ypinch=200, width=5, height=5)

# plot for cover crops
pcc <- ggplot() +
  geom_polygon(data=ill[ill$variable %in% "perc_cc",],
               mapping=aes(x=long, y=lat, fill=mybin, group=subregion),
               color="black", linewidth=0.2) +
 coord_sf(default_crs = sf::st_crs(4326)) +  # , # read x and y as latitude and longitude
  #          xlim = c(-92, -87), ylim=c(36.5, 43)) +
  scale_fill_manual(values=pal6b,
                    na.value = "#888888",
                    name= "Mean % Cover Crop\nAdoption 2018-2021") +
  scale_color_manual(values=NA) +
  theme_bw() +
  ditch_the_axes +
  theme(legend.title=element_text(size=10, face="bold"),
        legend.text=element_text(size=10),
        legend.key.size=unit(0.4, "cm"))

pcc

ggsave("plots/maps/county_heatmap_ILcc.png")





# plot for no-till
pnt <- ggplot() +
  geom_polygon(data=ill[ill$variable %in% "perc_nt",],
               mapping=aes(x=long, y=lat, fill=mybin, group=subregion),
               color="black", linewidth=0.2) +
  coord_sf(default_crs = sf::st_crs(4326)) +  # , # read x and y as latitude and longitude
  #          xlim = c(-92, -87), ylim=c(36.5, 43)) +
  scale_fill_manual(values=pal8b,
                    na.value = "#888888",
                    name= "Mean % No-Till\nAdoption 2018-2021") +
  scale_color_manual(values=NA) +
  theme_bw() +
  ditch_the_axes +
  theme(legend.title=element_text(size=10, face="bold"),
        legend.text=element_text(size=10),
        legend.key.size=unit(0.4, "cm"))

pnt

ggsave("plots/maps/county_heatmap_ILnt.png")




# plot for reduced-till
prt <- ggplot() +
  geom_polygon(data=ill[ill$variable %in% "perc_rt",],
               mapping=aes(x=long, y=lat, fill=mybin, group=subregion),
               color="black", linewidth=0.2) +
  coord_sf(default_crs = sf::st_crs(4326)) +  # , # read x and y as latitude and longitude
  #          xlim = c(-92, -87), ylim=c(36.5, 43)) +
  scale_fill_manual(values=pal6b,
                    na.value = "#888888",
                    name= "Mean % Reduced Till\nAdoption 2018-2021") +
  scale_color_manual(values=NA) +
  theme_bw() +
  ditch_the_axes +
  theme(legend.title=element_text(size=10, face="bold"),
        legend.text=element_text(size=10),
        legend.key.size=unit(0.4, "cm"))

prt

ggsave("plots/maps/county_heatmap_ILrt.png")



# plot for conventional-till
pct <- ggplot() +
  geom_polygon(data=ill[ill$variable %in% "perc_ct",],
               mapping=aes(x=long, y=lat, fill=mybin, group=subregion),
               color="black",linewidth=0.2) +
  coord_sf(default_crs = sf::st_crs(4326)) +  # , # read x and y as latitude and longitude
  #          xlim = c(-92, -87), ylim=c(36.5, 43)) +
  scale_fill_manual(values=pal5,
                    na.value = "#888888",
                    name= "Mean % Conventional Till\nAdoption 2018-2021")+
  scale_color_manual(values=NA) +
  theme_bw() +
  ditch_the_axes +
  theme(legend.title=element_text(size=10, face="bold"),
        legend.text=element_text(size=10),
        legend.key.size=unit(0.4, "cm"))


pct

ggsave("plots/maps/county_heatmap_ILct.png")



# plot all 4 plots at once

# grid.arrange(pcc, pnt, prt, pct, nrow=2) # plot sizes vary because legend titles are different widths

pcc + pnt + prt + pct + plot_layout(ncol=2)
ggsave("plots/maps/county_heatmap_ILall4.png", width=6.5, height=6.5, units="in")



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
