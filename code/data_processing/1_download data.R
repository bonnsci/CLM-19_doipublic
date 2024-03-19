# download data from Regrow shared google drive

library(readr)  # for unzip
library(dplyr)  # for bind_rows
# install.packages("beepr")
library(beepr)


# LOAD OpTIS data 
datil <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/OpTIS Adoption Acres/IL_counties_adoption_acreage.csv")
datny <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/OpTIS Adoption Acres/NY_counties_adoption_acreage.csv")

# SAVE OpTIS data to github project
write.csv(datil, "data/optis/datil.csv", row.names=F)
write.csv(datny, "data/optis/datny.csv", row.names=F)

# LOAD scenario data # most recent download 8/2/23 on files dated 7/26/23 from Alex
scenil <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/il_adoption_scenarios_final_outputs.csv")
# scena <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/almonds_adoption_scenarios_final_outputs.csv")
# sceng <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/grape_adoption_scenarios_final_outputs.csv")
# scenh <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/hops_adoption_scenarios_final_outputs.csv")
# scenny <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/ny_adoption_scenarios_final_outputs.csv")

# new alfalfa, almonds, vineyards, hops results 3/12/2024
scenny.alf <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Alfalfa 2024-03-04/ny_adoption_scenarios_final_outputs-alfalfa-2024-03-04.csv")
scenny <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Alfalfa 2024-03-04/ny_adoption_scenarios_final_outputs.csv")
scena <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Almonds, Hops, Vineyards 2024-02-20/almonds_adoption_scenarios_final_outputs.csv")
sceng <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Almonds, Hops, Vineyards 2024-02-20/grape_adoption_scenarios_final_outputs.csv")
scenh <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Almonds, Hops, Vineyards 2024-02-20/hops_adoption_scenarios_final_outputs.csv")

# SAVE scenario data
write.csv(scenil, "data/scenarios/scenil.csv", row.names=F)
write.csv(scena, "data/scenarios/scena.csv", row.names=F)
write.csv(sceng, "data/scenarios/sceng.csv", row.names=F)
write.csv(scenh, "data/scenarios/scenh.csv", row.names=F)
# Save new data 3/12/2024
write.csv(scenny, "data/scenarios/scenny.csv", row.names=F)
write.csv(scenny.alf, "data/scenarios/scenny_alf.csv", row.names=F) 
write.csv(scena, "data/scenarios/scena_20240220.csv", row.names=F) 
write.csv(sceng, "data/scenarios/sceng_20240220.csv", row.names=F)
write.csv(scenh, "data/scenarios/scenh_20240220.csv", row.names=F)

rm(scenil, scena, sceng, scenh, scenny)

# LOAD biomass # most recent download 8/2/23 on files dated 7/26/23 from Alex
# bm <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/max_biomass_summary.csv")
# New almonds, grapes, hops biomass 3/12/2024
bm <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Almonds, Hops, Vineyards 2024-02-20/max_biomass_summary.csv")
# bm <- read.csv("data/biomass.csv")
bm <- bm %>%
  arrange(site_name, Year, crop_name)
# new alf biomass 3/12/2024
bmalf <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Alfalfa 2024-03-04/max_biomass_summary-alfalfa-2024-03-04.csv")
bmalf <- bmalf %>%
  arrange(site_name, Year, crop_name) 
write.csv(bmalf, "data/biomass/biomass_NYalf.csv")  # 3/12/2024

# FILE TOO BIG TO PUSH TO GITHUB
# save site data to separate csvs
# # extract IL data
# site names
sites_IL <- grep("IL-", unique(bm$site_name), value=T) #
# subset data
bmIL <- bm[bm$site_name %in% sites_IL,]
# save data
write.csv(bmIL, "data/biomass/biomass_IL.csv", row.names=F)

# # extract NY data
# site names
sites_NY <- grep("f_", unique(bm$site_name), value=T) # f for forage
# subset data
bmNY <- bm[bm$site_name %in% sites_NY,]  # has more data because more crops
# save data
write.csv(bmNY, "data/biomass/biomass_NY_20240220.csv", row.names=F)

# # extract almond data
# site names
sites_alm <- grep("a_", unique(bm$site_name), value=T) #
# subset data
bmalm <- bm[bm$site_name %in% sites_alm,]
# save data
write.csv(bmalm, "data/biomass/biomass_almond_20240220.csv", row.names=F)

# # extract vineyard data
# site names
sites_vine <- grep("v_", unique(bm$site_name), value=T) #
# subset data
bmvine <- bm[bm$site_name %in% sites_vine,]
# save data
write.csv(bmvine, "data/biomass/biomass_vineyard_20240220.csv", row.names=F)

# # extract IL data
# site names
sites_hops <- grep("h_", unique(bm$site_name), value=T) #
# subset data
bmhops <- bm[bm$site_name %in% sites_hops,]
# save data
write.csv(bmhops, "data/biomass/biomass_hops_20240220.csv", row.names=F)

# clean up
rm(bm, bmalm, bmhops, bmIL, bmNY, bmvine, sites_alm, sites_hops, sites_IL, sites_NY, sites_vine)



# LOAD soil metadata # shouldn't change
soils <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/simulation_points_w_soils.csv")

# LOAD un-weighted DNDC results (all systems) ("post_weighting" in the file name refers to the vine and tree crops being weighted for % row that is tree/vine and % alley)
# unw <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/yearly_outputs_post_weighting.csv")
# new data hops, almonds, grapes 3/12/2024
unw <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Almonds, Hops, Vineyards 2024-02-20/yearly_outputs_post_weighting.csv")

# new NY data
unwny <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Alfalfa 2024-03-04/yearly_outputs_post_weighting-alfalfa-2024-03-04.csv")


# SAVE biomass, soil metadata, and un-weighted results
write.csv(bm, "data/biomass.csv", row.names=F)
write.csv(soils, "data/soils.csv", row.names=F)
write.csv(unw, "data/large_data/un-weighted_results_20240220.csv", row.names=F)
# 3/12/2024
write.csv(unwny, "data/simulations/un-weighted_resultsNYalf_20240220.csv", row.names=F)

############### divide simulations data up, can be stitched back together locally
unw <- read.csv("data/large_data/un-weighted_results.csv")
il <- unw[unw$region_name=="IL",]
object.size(il)
write.csv(il, "data/simulations/un-weighted_resultsIL.csv", row.names=F)
ca <- unw[unw$region_name=="CA",]
object.size(ca)
write.csv(ca, "data/simulations/un-weighted_resultsCA_20240220.csv", row.names=F)
ny <- unw[unw$region_name=="NY",]
object.size(ny)
write.csv(ny, "data/simulations/un-weighted_resultsNY.csv", row.names=F)
pnw <- unw[unw$region_name=="PNW",]
object.size(pnw)
write.csv(pnw, "data/simulations/un-weighted_resultsPNW_20240220.csv", row.names=F)

rm(ny, pnw, il, ca, unw)

################ THE FOLLOWING DOWNLOADS 1-2 GB .csv's per file. will need to work on these files locally rather than through github

# daily N data
dayn_alm <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Almonds, Hops, Vineyards 2024-02-20/CA_almonds_day_soil_n.csv")
write.csv(dayn_alm, "data/large_data/daily N/CA_almonds_day_soil_n_20240220.csv", row.names=F)
rm(dayn_alm)
beep(sound=5)

dayn_vin <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Almonds, Hops, Vineyards 2024-02-20/CA_vineyards_day_soil_n.csv")
write.csv(dayn_vin, "data/large_data/daily N/CA_vineyards_day_soil_n20240220.csv", row.names=F)
rm(dayn_vin)
beep(sound=5)

dayn_il <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/Daily Nitrogen/IL_corn_day_soil_n.csv")
write.csv(dayn_il, "data/large_data/daily N/IL_corn_day_soil_n.csv", row.names=F)
rm(dayn_il)
beep(sound=5)

dayn_ny <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/Daily Nitrogen/NY_forage_day_soil_n.csv")
write.csv(dayn_ny, "data/large_data/daily N/NY_forage_day_soil_n.csv", row.names=F)
rm(dayn_ny)
beep(sound=5)

dayn_pnw <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Almonds, Hops, Vineyards 2024-02-20/PNW_hops_day_soil_n.csv")
write.csv(dayn_pnw, "data/large_data/daily N/PNW_hops_day_soil_n_20240220.csv", row.names=F)
rm(dayn_pnw)
beep(sound=5)

# new 3/12/2024 NY
dayn_nyalf <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Alfalfa 2024-03-04/NY_forage_day_soil_n-alfalfa-2024-03-04.csv")
write.csv(dayn_nyalf, "data/large_data/daily N/NY_forage_day_soil_n_alf20240304.csv", row.names=F)
rm(dayn_nyalf)
beep(sound=5)


# daily water data
dayw_alm <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Almonds, Hops, Vineyards 2024-02-20/CA_almonds_day_water.csv")
write.csv(dayw_alm, "data/large_data/daily water, sediments/CA_almonds_day_water_20240220.csv", row.names=F)
rm(dayw_alm)
beep(sound=5)

dayw_vin <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Almonds, Hops, Vineyards 2024-02-20/CA_vineyards_day_water.csv")
write.csv(dayw_vin, "data/large_data/daily water, sediments/CA_vineyards_day_water_20240220.csv", row.names=F)
rm(dayw_vin)
beep(sound=5)

dayw_il <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/Daily Water/IL_corn_day_water.csv")
write.csv(dayw_il, "data/large_data/daily water, sediments/IL_corn_day_water.csv", row.names=F)
rm(dayw_il)
beep(sound=5)

dayw_ny <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/Daily Water/NY_forage_day_water.csv")
write.csv(dayw_ny, "data/large_data/daily water, sediments/NY_forage_day_water.csv", row.names=F)
rm(dayw_ny)
beep(sound=5)

dayw_pnw <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Almonds, Hops, Vineyards 2024-02-20/PNW_hops_day_water.csv")
write.csv(dayw_pnw, "data/large_data/daily water, sediments/PNW_hops_day_water_20240220.csv", row.names=F)
rm(dayw_pnw)
beep(sound=5)

# new 3/12/2024
dayw_nyalf <- read.csv("G:/.shortcut-targets-by-id/1RzGGwXFnsKjXPH17gQl345pZJAOV72w8/American Farmland Trust/DNDC Results/Data/2024 Reruns (Almonds, Hops, Vineyards, NY Alfalfa)/Alfalfa 2024-03-04/NY_forage_day_water-alfalfa-2024-03-04.csv")
write.csv(dayw_nyalf, "data/large_data/daily water, sediments/NY_forage_day_water_alf20240304.csv", row.names=F)
rm(dayw_nyalf)
beep(sound=5)


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

write.csv(weather, "data/large_data/clm_data_unpacked.csv", row.names=F)


##### testing code on subset to figure out how to unnest properly
##### this code works fyi
# test <- weather[1:10,1:4]
# names(test$dndc) <-  paste0(test$name, "_", test$scenario)
# y<- test$dndc[[1]]
# y <- row_to_names(y, row_number=1) # worked
# colnames(y) # 
# y <- data.frame(y)

                     