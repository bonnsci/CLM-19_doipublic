# This makes county heat maps for average % adoption 2018-2021 from OpTIS for 
# cover crops, no till, reduced till
# excludes counties whose harvested cropland acres in 2017 census is in the 
# lowest 10th percentile for the state and counties for which OpTIS evaluated
# < 50% of 2017 harvested cropland acres.See "OpTIS adoption rates IL.R" for details.
# 

library(tidyverse)
library(maps)

# based on https://stackoverflow.com/questions/51463037/plotting-a-heat-map-of-pennsylvania-counties-in-r

# load data created in "OpTIS adoption rates IL.R"
means_county <- read.csv("data/optis/IL_means_county.csv")

ill <- map_data("county", "Illinois")
# unique(ill$subregion)
means_county <- means_county %>%
  arrange(county)
# unique(means_county$county) 

# get map data to match county names

means_county$county <- str_remove(means_county$county, " County")

ill$subregion <- str_to_title(ill$subregion)
ill$subregion <- ifelse(ill$subregion %in% "De Kalb", "DeKalb", 
                        ifelse(ill$subregion %in% "De Witt", "DeWitt", 
                               ifelse(ill$subregion %in% "La Salle", "LaSalle",
                                      ifelse(ill$subregion %in% "Mcdonough", "McDonough",
                                             ifelse(ill$subregion %in% "Mchenry", "McHenry",
                                                    ifelse(ill$subregion %in% "Mclean", "McLean",
                                                           ifelse(ill$subregion %in% "St Clair", "St. Clair", 
                                                                  ill$subregion)))))))


# join rates with spatial data:
ill <- ill %>% left_join(means_county[,2:4], join_by(subregion==county))

# define colors
ppal <- c("#E8601c", "#f6c141", "#cae0ab", "#4eb265", "#5289c7")
10,14,15,17,18,26  c("#1965B0")
9,10,14,15,17,18,26
9,10,14,15,17,18,23,26 ("#882e72", "#1965b0", "#7bafde")
# from "smooth rainbow" by Paul Tol https://personal.sron.nl/~pault/
hist(means_county$mean.perc[means_county$variable %in% "perc_cc"]) # 6 bins 0-30
hist(means_county$mean.perc[means_county$variable %in% "perc_nt"]) # 7 bins 10-80
hist(means_county$mean.perc[means_county$variable %in% "perc_rt"]) # 8 bins 10-60
