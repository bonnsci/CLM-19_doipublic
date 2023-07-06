library(dplyr)

datil <- read.csv("data/optis/datil_cleanwide.csv") 
  

compare <- datil %>%
  arrange(year, county) %>%
  select(county, year, crop_name, perc_cc) %>%
  filter(year==2018)

# visually compare to data shown here: https://agr.illinois.gov/resources/landwater/illinois-soil-conservation-transect-survey-reports.html
# unfortunately in PDF form and not in .csv :(