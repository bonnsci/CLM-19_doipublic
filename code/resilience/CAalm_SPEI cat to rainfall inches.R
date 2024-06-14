library(tidyverse)


# load data
dat <- read.csv("data/large_data/clm_alm.csv")

# can get rid of row numbers, model, #### double check the column numbers below
# model is "IPSL-CM5A-LR"

dat <- dat[,c(3,5:11)]


# convert doy to dates so we can get monthly  means/totals # Takes a sec to run
dat <- dat %>%
  mutate(date_ = as.Date(doy-1, origin=paste0(year, "-01-01")),  # subtract 1 b/c R uses a 0 base index
         month = strftime(date_, "%b"),
         day = strftime(date_, "%d")) 

# SPEI works on monthly data

datmo <- dat %>%
  filter(year >2021, year <2073) %>%
  group_by(scenario, name, year, month) %>%
  summarize(tmed = mean(avg_temp),
            tmax = max(max_temp),
            tmin = min(min_temp), # mean monthly temp in C
            prcp = (sum(precip))) # total precip in mm

rm(dat)


spei <- read.csv("data/climate data not large/spei_alm_formerge.csv")



# What is a "moderately wet spring" in these data?

spei2 <- filter(spei, rcp=="rcp60", season=="Spring", spcat12mossn == "Moderately Wet", site=="a_1")

spei2 <- left_join(spei2, datmo, join_by(month==month, year==year, site==name, rcp==scenario))

agg <- aggregate(prcp~year, data=spei2, FUN="sum")

agg$prcp_in  <- agg$prcp*0.0394
min(agg$prcp_in)
max(agg$prcp_in)
