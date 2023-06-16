# OpTIS adoption rate information for Illinois and New York aggregated by counties 2015-2021.
# Data shared with AFT from Regrow analysis.
# Column "kpi_name" gives different types of land use areas, with "area" meaning the 
# analyzed acres of the county, tillage type/ cover crop / etc.
# units are ACRES

# load packages
library(reshape2) # for dcast()
library(ggplot2)
library(dplyr)


# to update data, run "download data.R"




# load the data
datil <- read.csv("data/optis/datil.csv")
datny <- read.csv("data/optis/datny.csv")

# combine the data into one df
dat <- rbind(datil, datny)
# clean up
rm(datil)
rm(datny)

colnames(dat)[1] <- "state"
dat <- dat[,c(-2, -10)]

sum(is.na(dat$kpi_value))

# unique(dat$kpi_name)
# cast data into wide form for separate columns by kpi_name and kpi_value
datw <- dcast(dat, ...~kpi_name, value.var="kpi_value")
colnames(datw)[c(2,3,7:11)] <- c("county", "fips", "acres", "ct.acres", "cc.acres", "nt.acres", "rt.acres") 
# $acres is acres analyzed by OpTIS

# calculate percentages
datw$perc_ct <- 100*(datw$ct.acres / datw$acres)
datw$perc_cc <- 100*(datw$cc.acres / datw$acres)
datw$perc_nt <- 100*(datw$nt.acres / datw$acres)
datw$perc_rt <- 100*(datw$rt.acres / datw$acres)


# how does adoption rate change over time?

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=datw, aes(y=perc_cc, x=year, group=county)) +
  geom_line(aes(color=county), show.legend=F) + #, position="jitter", size=0.5) +
  facet_grid(rows=vars(crop_name), cols=vars(state))

# which counties have at least 5 years of consecutive measurements?
# work with a subset of data
subs <- datw[,c(1,2,5,4)]
# 
subs <- subs %>%
  group_by(state, county, crop_name) %>%
  mutate(con_yrs = cumsum(c(1, diff(year)>1))) %>%  # make col that is "1" for consecutive years, 2 if non-consecutive years
  group_by(state, county, crop_name, con_yrs) %>% # group by new column
  mutate(start = min(year), end = max(year), diff=end - start) # list first and last year of consecutive groups

# join back together with rest of data
datw2 <- left_join(datw, subs) %>%
  filter(diff>4) # only keep county-crop-years with at least 5 years consecutive data 2015-2021
# removes 102 county-crop-years
ggplot(data=datw2, aes(y=perc_cc, x=year, group=county)) +
  geom_line(aes(color=county), show.legend=F) + #, position="jitter", size=0.5) +
  facet_grid(rows=vars(crop_name), cols=vars(state))
unique(datw2$diff)

sum(is.na(datw$perc_cc))
sum(is.na(datw$acres))
sum(is.na(datw$cc.acres))
test <- datw[rowSums(is.na(datw))>0,]
# turns out that we create NAs for counties where certain kpi_names are not measured
# for that county that year that crop
# so need to do the subs code above for each kpi_name