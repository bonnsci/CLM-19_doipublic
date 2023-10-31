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
library(zoo)  # for as.yearmon()

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
  dplyr::select(site_name, long) %>%  # CSV has lat and long named in reverse
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


# calculate PET and water balance
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

# calculate monthly SPEI
# from ?spei
# An important advantage of the SPEI and the SPI is that they can be computed at 
# different time scales. This way it is possible to incorporate the influence of 
# the past values of the variable in the computation enabling the index to adapt 
# to the memory of the system under study. The magnitude of this memory is 
# controlled by parameter **scale**. For example, a value of six would imply that data 
# from the current month and of the past five months will be used for computing 
# the SPEI or SPI value for a given month. By default all past data will have 
# the same weight in computing the index, as it was originally proposed in the 
# references below. 

# one-month spei
spei_1 <- lapply(ldatmo, function(i){
  out <- spei(i[, "bal"], scale=1, verbose=F)
  out
})

# 12-month spei (still gives SPEI values per month)
spei_12 <- lapply(ldatmo, function(i){
  out <- spei(i[, "bal"], scale=12, verbose=F)
  out
})

# summary(spei_1[[1]])
# spei_1[[1]]$fitted
# spei_1[[1]]$coefficients

# # Since plot.spei() returns a ggplot object, it is possible to add or tweak
# # parts of the plot.
# 
plot(spei(ldatmo[[1]][, "bal"], 12)) +
  ggtitle("SPEI1 at IL-n_1.rcp26") +
  scale_fill_manual(values = c("blue", "red")) + # classic SPEI look
  scale_color_manual(values = c("blue", "red")) + # classic SPEI look
  theme_classic() +
  theme(legend.position = "bottom")



# stitch the data back together
# extract dfs of the fitted values 
spei1fit <- lapply(spei_1, function(i){
  fit = i$fitted 
  dat = as.yearmon(time(i$fitted))
  out <- data.frame(fit=fit, dat=dat)
  out
})

spei12fit <- lapply(spei_12, function(i){
  fit = i$fitted 
  dat = as.yearmon(time(i$fitted))
  out <- data.frame(fit=fit, dat=dat)
  out
})

# stitch together
spei1dat <- bind_rows(spei1fit, .id="ID") %>%  # add list name as ID column, e.g., "IL-n_1.rcp26"
  separate_wider_delim(ID, delim=".", 
                       names=c("site", "rcp"),  # split the ID into two columns
                       cols_remove=T)

spei12dat <- bind_rows(spei12fit, .id="ID") %>%  # add list name as ID column, e.g., "IL-n_1.rcp26"
  separate_wider_delim(ID, delim=".", 
                       names=c("site", "rcp"),  # split the ID into two columns
                       cols_remove=T)

rm(spei_1, spei_12, spei1fit, spei12fit)


spei1dat$ns <- ifelse(grepl("n_", spei1dat$site), "North", "South")
spei12dat$ns <- ifelse(grepl("n_", spei12dat$site), "North", "South")

colnames(spei1dat)[3] <- "fit.1mo"
colnames(spei12dat)[3] <- "fit.12mo"

speidat <- left_join(spei1dat, spei12dat, relationship="one-to-one")
# write.csv(speidat, "data/climate data not large/speiIL.csv", row.names=F)

##### MAKE SOME PLOTS

windows(xpinch=200, ypinch=200, width=5, height=5)

spei1sitemean <- spei1dat %>%
  group_by(rcp, ns, dat) %>%
  summarize(mean.fit = mean(fit),
            sd.fit = sd(fit))

spei12sitemean <- spei12dat %>%
  group_by(rcp, ns, dat) %>%
  summarize(mean.fit = mean(fit),
            sd.fit = sd(fit))


# Drought classifications using SPEI
# https://droughtmonitor.unl.edu/About/AbouttheData/DroughtClassification.aspx

cats <- data.frame(ymax=c(-2.0, -1.6, -1.3, -0.8, 1.30, 1.60, 2.0, 3.0),
                   ymin=c(-3.5, -2.0, -1.6, -1.3,  0.8, 1.3, 1.6, 2.0),
                   xmin=rep(min(spei12sitemean$dat), 8),
                   xmax=rep(max(spei12sitemean$dat), 8),
                   cat=factor(x=c("Exceptional Drought","Extreme Drought","Severe Drought","Moderate Drought",
                                  "Moderately Wet", "Severely Wet", "Extremely Wet", "Exceptionally Wet"),
                                levels=c("Exceptional Drought","Extreme Drought","Severe Drought","Moderate Drought",
                                         "Moderately Wet", "Severely Wet", "Extremely Wet", "Exceptionally Wet"),
                         ordered=T),
                   labx = rep(2073, 8))
cats$laby = cats$ymax-0.1

pal8 <- c("#dd3d2d","#f67e4b","#FDb366","#FEDa8B", "#C2e4ef", "#98cae1","#6ea6cd", "#4a7bb7")


ggplot() +
  geom_rect(data=cats,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=cat),
            alpha=0.5) +
  scale_fill_manual(values=pal8, name = "Extreme wet/dry\ncategory") +
  # geom_text(data=cats, aes(x=labx, y=laby, label=cat), hjust=0) + 
  coord_cartesian(xlim=c(2022, 2072), ylim=c(-3.5, 3), expand=F) +
  geom_vline(xintercept=seq(2030,2070,10), color="gray50", alpha=0.4, linewidth=0.5) +
  geom_hline(yintercept=0, linewidth=0.5, color="gray50", alpha=0.4) +
  # geom_line(data=spei12sitemean, aes(x=dat, y=mean.fit), 
  #          color="#004488", alpha=0.8) +
  
  # geom_line(data=spei12sitemean, aes(x=dat, y=mean.fit+sd.fit), 
  #           color="gray50", alpha=0.8) +
  # geom_line(data=spei12sitemean, aes(x=dat, y=mean.fit+sd.fit), 
  #           color="gray50", alpha=0.8) +
  
  geom_ribbon(data=spei12sitemean, aes(x=dat, ymax=mean.fit+sd.fit, ymin=mean.fit-sd.fit),
              fill="gray30", alpha=0.9, linetype=0) +
  scale_x_continuous(breaks=seq(2030,2070,10), labels=seq(2030,2070,10)) +
  
  # geom_errorbar(data=spei12sitemean,
  #               aes(x=dat, y=mean.fit, ymax = mean.fit+sd.fit, ymin = mean.fit - sd.fit), 
  #               linewidth=0, alpha=0.7, color="#DDAA33") +
  facet_grid(cols=vars(rcp), rows=vars(ns), 
             labeller = as_labeller(
               c(rcp26="RCP2.6", rcp60="RCP 6.0",
                 North="North IL", South="South IL"))) +
  labs(x="Month", y="Standard Precipitation-\nEvapotranspiration Index") +
  guides(fill=guide_legend(reverse=TRUE)) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),  #,
    panel.background = element_rect(fill = 'gray95')

  )	 

ggsave("plots/climate/IL_SPEI_12-month.png", width=8, height=3, dpi=300)
  







