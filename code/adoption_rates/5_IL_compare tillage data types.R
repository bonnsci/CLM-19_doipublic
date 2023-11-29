library(dplyr)

datil <- read.csv("data/optis/datil_cleanwide.csv") 
  

compare <- datil %>%
  arrange(year, county) %>%
  select(county, year, crop_name, perc_nt, perc_ct) %>%
  filter(year==2018)

compare$perc_nt <- format(round(compare$perc_nt, 3), nsmall=3)
compare$perc_ct <- format(round(compare$perc_ct, 3), nsmall=3)

# visually compare to data shown here: https://agr.illinois.gov/resources/landwater/illinois-soil-conservation-transect-survey-reports.html
# unfortunately in PDF form and not in .csv :(