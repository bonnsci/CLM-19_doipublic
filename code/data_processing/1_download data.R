# download data from Regrow shared google drive

library(readr)  # for unzip
library(dplyr)  # for bind_rows


# LOAD OpTIS data 
datil <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/OpTIS Adoption Acres/IL_counties_adoption_acreage.csv")
datny <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/OpTIS Adoption Acres/NY_counties_adoption_acreage.csv")

# SAVE OpTIS data to github project
write.csv(datil, "data/optis/datil.csv", row.names=F)
write.csv(datny, "data/optis/datny.csv", row.names=F)

# LOAD scenario data
scenil <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/il_adoption_scenarios_final_outputs.csv")
scena <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/almonds_adoption_scenarios_final_outputs.csv")
sceng <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/grape_adoption_scenarios_final_outputs.csv")
scenh <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/hops_adoption_scenarios_final_outputs.csv")
scenny <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/ny_adoption_scenarios_final_outputs.csv")

# SAVE scenario data
write.csv(scenil, "data/scenarios/scenil.csv", row.names=F)
write.csv(scena, "data/scenarios/scena.csv", row.names=F)
write.csv(sceng, "data/scenarios/sceng.csv", row.names=F)
write.csv(scenh, "data/scenarios/scenh.csv", row.names=F)
write.csv(scenny, "data/scenarios/scenny.csv", row.names=F)

# LOAD biomass
bm <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/max_biomass_summary.csv")

# LOAD soil metadata
soils <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/simulation_points_w_soils.csv")

# LOAD un-weighted DNDC results (all systems) ("post_weighting" in the file name refers to the vine and tree crops being weighted for % row that is tree/vine and % alley)
unw <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/yearly_outputs_post_weighting.csv")

# SAVE biomass, soil metadata, and un-weighted results
write.csv(bm, "data/bm.csv", row.names=F)
write.csv(soils, "data/soils.csv", row.names=F)
write.csv(unw, "data/unw.csv", row.names=F)

################ THE FOLLOWING WORKS WHEN WE'RE READY TO DEAL WITH BIG FILES - HASN'T BEEN COMPLETED
# Daily nitrogen (600 MB) - a zipped file that includes files for each simulation run. These include daily nitrogen loading and flux information for each management system.
unzip("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/Daily Nitrogen/day_soil_n_files-AFT.zip", exdir="/tempdir")
temp <- list.files("/tempdir", pattern="*.csv")
setwd("/tempdir")
dailyn <- lapply(temp, read.delim)  # this is 11.1 GB
# next step would be to convert this to one dataframe

# Daily water (666 MB) -  zipped file that includes files for each simulation run. These files include daily evaporation and transpiration rates that are lost through each management system.
unzip("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/Daily Water/day_water_files-AFT.zip", exdir="/tempdir")



###################################### WEATHER DATA
################################# dealing with nested JSON instructions here: https://medium.com/@Periscopic/cozy-collecting-part-2-5e717588e37b
# install.packages("jsonlite")
library(jsonlite)
library(tidyverse) # contains:
# library(tibble) # nicer dataframes
# library(purrr) # apply functions to lists
# library(dplyr) # data wrangling
# library(tidyr) # creating tidy data
# library(glue) # pass variables into strings
# install.packages("listviewer")
library(listviewer)
# install.packages("janitor")
library(janitor)



# weather data 
json_file <- "G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/aft_rcp_data.json"
dat <- fromJSON(txt=json_file)


weather <- tibble(place=dat) # necessary?
# jsonedit(weather)  # not working
# jsonedit(dat) # not working
weather <- weather %>%
  unnest_wider(place) %>%
  unnest(rcp_data) %>%
  unnest(rcp_data) 

names(weather$dndc) <- paste0(weather$name, "_", weather$scenario)  # worked

weather$dndc <- lapply(weather$dndc, function(i){  #worked
  i <- row_to_names(i, row_number=1)
  i <- data.frame(i)
  i
})

weather <- weather %>%
  unnest_longer(dndc) %>%
  unnest(dndc)

write.csv(weather, "data/large_data/clm_data_unpacked.csv")


##### testing code on subset to figure out how to unnest properly
##### this code works fyi
# test <- weather[1:10,1:4]
# names(test$dndc) <-  paste0(test$name, "_", test$scenario)
# y<- test$dndc[[1]]
# y <- row_to_names(y, row_number=1) # worked
# colnames(y) # 
# y <- data.frame(y)















unique(dat2$model)
# [1] "IPSL-CM5A-LR"


# extract Illinois climate data from 32 sites (16 north and 16 south)
il_lists <- grep("IL-", dat$name)
# subset data
clm_il <- dat[il_lists,]
# flatten list to one dataframe
clm_il2 <- bind_rows(clm_il, )

                     