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

# number of counties represented in IL data
datw %>%
  filter(state %in% "IL") %>%
  distinct(county) %>%
  n_distinct()
# 101 # 102 counties in IL, Lake County is missing, it's a tiny county between
# Chicago and Wisconsin border, basically still Chicago outlying area. No ag.
# surprised cook county (Chicago) is even in dataset!



# long form again, but leave out acreages, only need %s
datl <- melt(datw[, -c(6:11)], id=c("state", "county", "fips", "year", "crop_name"), na.rm=T)
# using na.rm = TRUE because do not have all combinations of crop acres per cover crop or tillage acres, e.g. 
# we have soybean acres in Ulster County, NY in 2015, but we do not have soybean cover crop acres for that 
# county-year. So when we melt, we get a bunch of Value = NA rows.

se <- function(x) sd(x) / sqrt(length(x))

# (ignore alfalfa and winter wheat acre for cover crops)

# what are the average county adoption rates and number of acres evaluated? (are any small counties driving percents up?)
# which counties are hotspots?
means_county <- datl %>%
                filter(year>2017) %>%
                group_by(state, county, variable) %>%
                summarize(Mean.perc = mean(value))

# do we mean small counties in terms of number of acres evaluated or small counties in terms of number of crop acres period?
# corn.acres <- datw[datw$state %in% "IL" & datw$crop_name %in% "Corn", c(1,2,7)]
# corn.acres <- corn.acres %>%
#               group_by(county) %>%
#               summarize(Mean.ac.eval = mean(acres))
# means_county <- left_join(means_county, corn.acres)
# cc <- means_county %>%
#   filter(variable %in% "perc_nt", state %in% "IL")

# what are the state average adoption rates by year and crop
means_cropyear <- datl %>% 
                group_by(state, year, crop_name, variable) %>%
                summarize(Mean = mean(value))
                          

# # what are the state average adoption rates by year (across all crops)
means_year <- datl%>%
              filter (! crop_name %in% "Alfalfa")  %>%
                  # because Alf is perennial no tillage, no covers
              filter(!(crop_name %in% "Winter Wheat" & variable == "perc_cc")) %>%
                  # because Alf is a perennial and winter wheat grows at same time as covers
              group_by(state, year, variable) %>%
              summarize(Mean = mean(value))

# what are the state average adoption rates across all available years (2015-2021) per crop?
means_crop <- datl%>%
                    filter (! crop_name %in% "Alfalfa")  %>%
                    # because Alf is perennial no tillage, no covers
                    filter(!(crop_name %in% "Winter Wheat" & variable == "perc_cc")) %>%
                    # because Alf is a perennial and winter wheat grows at same time as covers
                    group_by(state, crop_name, variable) %>%
                    summarize(Mean = mean(value))

# what are the state average adoption rates across MOST RECENT 4 YEARS 2018-2021?  
# note results not all that different between means_crop and means_crop_1821
# for now, will report most recent data:
means_crop_1821 <- datl%>%
              filter(year>2017) %>%
              filter (! crop_name %in% "Alfalfa")  %>%
              # because Alf is perennial no tillage, no covers
              filter(!(crop_name %in% "Winter Wheat" & variable == "perc_cc")) %>%
              # because Alf is a perennial and winter wheat grows at same time as covers
              group_by(state, crop_name, variable) %>%
              summarize(Mean = mean(value), se=se(value))

# what are the state average adoption rates across all years and crops
means_global <- datl%>%
                filter (! crop_name %in% "Alfalfa")  %>%
                  # because Alf is perennial no tillage, no covers
                filter(!(crop_name %in% "Winter Wheat" & variable == "perc_cc")) %>%
                  # because Alf is a perennial and winter wheat grows at same time as covers
                group_by(state, variable) %>%
                summarize(Mean = mean(value), se = se(value))


# plots for mean adoption rates

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(dat=means_crop_1821[means_crop_1821$state=="IL",], aes(x=variable, y=Mean, group=crop_name)) + 
    geom_bar(stat="identity", position=position_dodge(), aes(fill=crop_name)) +
    geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se), linewidth=0.8, color="#000000",
                  position=position_dodge(0.9), width=0.2) +
    scale_x_discrete(limits=c("perc_cc", "perc_nt", "perc_rt"), 
                       labels=c("Cover\nCrops", "No\nTill", "Reduced\nTill")) +  # ok to just leave out conventional till?
    scale_fill_manual(values=c("#DDAA33", "#228833", "#66CCEE"), 
                      labels=c("Corn", "Soybeans", "Winter Wheat"),
                      name="Crops") +
    scale_y_continuous(breaks=seq(0,70,10)) +
    ylab("Mean Percent Adoption 2018-2021") +
    xlab("Soil Health Practice") +
    geom_text(aes(y=Mean +5, 
                  label=paste0(format(round(Mean, digits=1), nsmall=1), "%")), 
                  position=position_dodge(0.9)) +
    # facet_grid(.~state) +
    theme(
        #legend.text.align=0,
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank() ,
        # axis.text.x=element_text(angle=-30, hjust=0, size=11),
        axis.text=element_text(size=12),
        axis.title.x=element_text(size=12, face="bold"),
        axis.title.y=element_text(size=12, face="bold"),
        panel.background = element_rect(fill = 'white') ,
        panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
        strip.text=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        legend.title=element_text(size=12, face="bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
    )
       
ggsave("plots/IL_bar_means_crop_1821.png")    






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