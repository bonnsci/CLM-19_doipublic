# calculate Standard Precipitation Evapotranspiration Index
# to identify climatic stressors to ag systems and quantify resilience
# to stressor

# using package "SPEI"
# following guide at https://cran.r-project.org/web/packages/SPEI/SPEI.pdf

# started 10/30/2023

# data are daily weather used for all simulations. 
# temps are in C, 
# precip is mm/day


library(tidyverse)
library(reshape2)
# install.packages("SPEI")
library(SPEI)
# install.packages("zoo")
library(zoo)

# load data
dat <- read.csv("data/large_data/clm_il.csv")
# can get rid of row numbers, model, #### double check the column numbers below
# model is "IPSL-CM5A-LR"

dat <- dat[,c(3,5:11)]


# convert doy to dates so we can get monthly  means/totals # Takes a sec to run
dat <- dat %>%
  mutate(date_ = as.Date(doy-1, origin=paste0(year, "-01-01")),  # subtract 1 b/c R uses a 0 base index
         month = strftime(date_, "%m"),
         day = strftime(date_, "%d")) 
beepr::beep(sound=8)

# SPEI works on monthly data

datmo <- dat %>%
  filter(year >2021, year <2073) %>%
  group_by(scenario, name, year, month) %>%
  summarize(tmed = mean(avg_temp),
            tmax = max(max_temp),
            tmin = min(min_temp), # mean monthly temp in C
            prcp = (sum(precip))) # total precip in mm

rm(dat)

# need latitudes of each site
soils <- read.csv("data/soils.csv") %>%
  filter(state=="IL") %>%
  select(site_name, long) %>%  # CSV has lat and long named in reverse
  rename(lat = long)

datmo <- left_join(datmo, soils, 
                   join_by(name == site_name), 
                   relationship= "many-to-one")


# compute PET and climatic water balance
# the PET and SPEI calculations seem to just want one latitude value, 
# not a column of latitude values, i.e., assume all data are for 1 site.
# so we shall calculate pet and spei per site then stitch the data back together
# split data by site*RCP

ldatmo <- split(datmo, ~ datmo$name + datmo$scenario)

ldatmo <- lapply(ldatmo, function(i){
  latx <- i$lat[1]
  i$pet <- hargreaves(Tmin = i$tmin,
                      Tmax = i$tmax,
                      lat = latx,
                      Pre = i$prcp,
                      verbose=F)
  i$bal <- i$prcp - i$pet
  i <- ts(i[,-c(1:4)], end = c(2072,12), frequency = 12)
  i
})

spei_1 <- lapply(ldatmo, function(i){
  out <- spei(i[, "bal"], 1, verbose=F)
})

spei_12 <- lapply(ldatmo, function(i){
  out <- spei(i[, "bal"], 12, verbose=F)
})

summary(spei_1[[1]])
spei_1[[1]]$fitted
spei_1[[1]]$coefficients

# Since plot.spei() returns a ggplot object, it is possible to add or tweak
# parts of the plot.

plot(spei(ldatmo[[1]][, "bal"], 12)) +
  ggtitle("SPEI1 at IL-n_1.rcp26") +
  scale_fill_manual(values = c("blue", "red")) + # classic SPEI look
  scale_color_manual(values = c("blue", "red")) + # classic SPEI look
  theme_classic() +
  theme(legend.position = "bottom")

names(spei_1[[1]]$fitted)
n <- as.data.frame(spei_1[[1]]$fitted)
n <- data.frame(fit = spei_1[[1]]$fitted, dat = as.yearmon(time(spei_1[[1]]$fitted)))
n$month <- as.Date(n$dat, format = "%m")
time(spei_1[[1]]$fitted)

as.yearmon(time(spei_1[[1]]$fitted))

# stitch the data back together
# extract dfs of the fitted values ####################left off here
spei1fit <- lapply(spei_1, function(i){
  fit = i$fitted 
  dat = as.yearmon(i$fitted)
  out <- data.frame(fit=fit, dat=dat)
  out
})
