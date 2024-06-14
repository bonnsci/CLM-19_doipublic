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

# load data created in "OpTIS adoption rates IL.R"
means_county <- read.csv("data/optis/means_county_ny.csv") %>%
  arrange(county)

# get map data to match county names
means_county$county <- str_remove(means_county$county, " County")



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
# palcc <- c("#FFFFE5", "#F7FCB9", "#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#004529")
palnt <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837") # From https://colorbrewer2.org/#type=sequential&scheme=YlGn&n=5
palct <- c("#fff7bc", "#fec44f", "#d95f0e")  # From https://colorbrewer2.org/#type=sequential&scheme=YlOrBr&n=3
palrt <- c("#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#005a32")  # From https://colorbrewer2.org/#type=sequential&scheme=YlGn&n=8


# divide percents into bins
# what's the max and min for each variable?
ranges <- do.call(data.frame, aggregate(means_county, mean.perc~variable, 
                                        FUN=function (x) c(min=min(x), max=max(x))))

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

# cleanup bins dataset
means_county <- means_county[,c(1:4,11)]

# get county spatial data
ny <- map_data("county", "New York", proj="mercator")

# check spelling of counties matches between ny and means_county


# get ready to join with means_county data
ny$subregion <- str_to_title(ny$subregion)
mapnames <- sort(unique(ny$subregion))   # 62 names in map
datnames <- arrange(unique(means_county[,c('county', 'variable')]))  # data have 41 names

# what is different
# difs <- setdiff(mapnames, datnames)

# complete list of counties x 4 variable types
vars <- unique(means_county$variable)
mapnames2 <- rep(mapnames, 4)
varnames <- c(rep(vars[1], length(mapnames)),
              rep(vars[2], length(mapnames)),
              rep(vars[3], length(mapnames)),
              rep(vars[4], length(mapnames)))
complete <- data.frame(county=mapnames2, variable=varnames)

means_county <- full_join(means_county, complete)
rm(complete, vars, mapnames, datnames, mapnames2, varnames)

means_county$mybin <- ordered(means_county$mybin, levels=unique(means_county$mybin))
length(unique(means_county$county))  # 62


# join rates with spatial data:
ny <- left_join(ny, means_county, join_by(subregion==county), relationship="many-to-many")



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

# plot for cover crops
pcc <- ggplot() +
  geom_polygon(data=ny[ny$variable %in% "perc_cc",],
               mapping=aes(x=long, y=lat, fill=mybin, group=subregion),
               color="#20243d", linewidth=0.2) +
 coord_sf(default_crs = sf::st_crs(4326)) +  # , # read x and y as latitude and longitude
  #          xlim = c(-92, -87), ylim=c(36.5, 43)) +
  scale_fill_manual(values=palcc,
                    na.value = "gray80",
                    name= "Mean % Cover Crop\nAdoption 2018-2021") +
  scale_color_manual(values=NA) +
  theme_bw() +
  ditch_the_axes +
  theme(legend.title=element_text(size=10, face="bold"),
        legend.text=element_text(size=10),
        legend.key.size=unit(0.4, "cm"),
        plot.margin = unit(c(1, 0, 1,0 ), "cm"))

pcc

ggsave("plots/maps/county_heatmap_NYcc.png")





# plot for no-tny
pnt <- ggplot() +
  geom_polygon(data=ny[ny$variable %in% "perc_nt",],
               mapping=aes(x=long, y=lat, fill=mybin, group=subregion),
               color="#20243d", linewidth=0.2) +
  coord_sf(default_crs = sf::st_crs(4326)) +  # , # read x and y as latitude and longitude
  #          xlim = c(-92, -87), ylim=c(36.5, 43)) +
  scale_fill_manual(values=palnt,
                    na.value = "gray80",
                    name= "Mean % No-Till\nAdoption 2018-2021") +
  scale_color_manual(values=NA) +
  theme_bw() +
  ditch_the_axes +
  theme(legend.title=element_text(size=10, face="bold"),
        legend.text=element_text(size=10),
        legend.key.size=unit(0.4, "cm"),
        plot.margin = unit(c(1, 0, 0,0 ), "cm"))

pnt

ggsave("plots/maps/county_heatmap_NYnt.png")




# plot for reduced-till
prt <- ggplot() +
  geom_polygon(data=ny[ny$variable %in% "perc_rt",],
               mapping=aes(x=long, y=lat, fill=mybin, group=subregion),
               color="#20243d", linewidth=0.2) +
  coord_sf(default_crs = sf::st_crs(4326)) +  # , # read x and y as latitude and longitude
  #          xlim = c(-92, -87), ylim=c(36.5, 43)) +
  scale_fill_manual(values=palrt,
                    na.value = "gray80",
                    name= "Mean % Reduced Till\nAdoption 2018-2021") +
  scale_color_manual(values=NA) +
  theme_bw() +
  ditch_the_axes +
  theme(legend.title=element_text(size=10, face="bold"),
        legend.text=element_text(size=10),
        legend.key.size=unit(0.4, "cm"),
        plot.margin = unit(c(1, 0, 1,0 ), "cm"))

prt

ggsave("plots/maps/county_heatmap_NYrt.png")



# plot for conventional-till
pct <- ggplot() +
  geom_polygon(data=ny[ny$variable %in% "perc_ct",],
               mapping=aes(x=long, y=lat, fill=mybin, group=subregion),
               color="#20243d",linewidth=0.2) +
  coord_sf(default_crs = sf::st_crs(4326)) +  # , # read x and y as latitude and longitude
  #          xlim = c(-92, -87), ylim=c(36.5, 43)) +
  scale_fill_manual(values=palct,
                    na.value = "gray80",
                    name= "Mean % Conventional Till\nAdoption 2018-2021")+
  scale_color_manual(values=NA) +
  theme_bw() +
  ditch_the_axes +
  theme(legend.title=element_text(size=10, face="bold"),
        legend.text=element_text(size=10),
        legend.key.size=unit(0.4, "cm"),
        plot.margin = unit(c(1, 0, 0,0 ), "cm"))


pct

ggsave("plots/maps/county_heatmap_NYct.png")



# plot all 4 plots at once

# grid.arrange(pcc, pnt, prt, pct, nrow=2) # plot sizes vary because legend titles are different widths

pcc + pnt + prt + pct + plot_layout(ncol=2)
ggsave("plots/maps/county_heatmap_NYall4_2x2.png", width=9, height=8, units="in")

pcc + pnt + prt + pct + plot_layout(ncol=1)
ggsave("plots/maps/county_heatmap_NYall4_1x4.png", width=4, height=9, units="in")

pcc + pnt + pct + plot_layout(ncol=1)
ggsave("plots/maps/county_heatmap_NYCC,CT,NTonly.png", width=4, height=7, units="in")





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
