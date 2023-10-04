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


# standard error
se <- function(x){sd(x)/(sqrt(length(x)))}

# load the data
datil <- read.csv("data/optis/datil_cleanlong.csv")




# what are the state average adoption rates by year and crop
means_cropyear <- datil %>% 
                group_by(state, year, crop_name, variable) %>%
                summarize(Mean = mean(value), se=se(value))
                          

# # what are the state average adoption rates by year (across all crops)
means_year <- datil%>%
              group_by(state, year, variable) %>%
              summarize(Mean = mean(value))

# what are the state average adoption rates across all available years (2015-2021) per crop?
means_crop <- datil%>%
                    group_by(state, crop_name, variable) %>%
                    summarize(Mean = mean(value), se=se(value))

# what are the state average adoption rates across MOST RECENT 4 YEARS 2018-2021?  
# note results not all that different between means_crop and means_crop_1821
# for now, will report most recent data:
means_crop_1821 <- datil%>%
              filter(year>2017) %>%
              group_by(crop_name, variable) %>%
              summarize(Mean = mean(value), se=se(value))

# what are the state average adoption rates across all years and crops
means_global <- datil%>%
                group_by(variable) %>%
                summarize(Mean = mean(value), se = se(value))


# plots for mean adoption rates

windows(xpinch=200, ypinch=200, width=5, height=5)


# State mean change over time
# facet by crop

pal4 <- c("#999933", "#332288", "#88ccee",  "#cc6677")


ggplot(data=means_cropyear, aes(x=year, y=Mean, group=variable)) +
  geom_line(aes(color=variable), linewidth=0.4) +
  geom_point(aes(color=variable), size=0.5) +
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se, color=variable), width=0.1, linewidth=0.6) +
  scale_color_manual(values=pal4, 
                     name="Practice",
                     breaks=c("perc_cc", "perc_nt", "perc_rt", "perc_ct"),
                     labels=c("Cover Crops", "No Till", "Reduced Till", "Conventional Till")) +
  facet_grid(rows=vars(crop_name)) + 
  scale_x_continuous(breaks=seq(2015,2021, 1), name="Year") +
  ylab("State Mean Percent Adoption") +
  theme(
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank() ,
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=14, face="bold"),
        axis.title.y=element_text(size=14, face="bold"),
        panel.background = element_rect(fill = 'white') ,
        panel.border=element_rect(color="grey50", fill=NA, size=0.5),
        strip.text=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        legend.title=element_text(size=12, face="bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.key=element_rect(fill="white"),
        legend.key.size = unit(0.4, "cm")
  )	 

ggsave("plots/state_rates_15-21_IL.png")


library(lme4)
# rate of change - all practices together
summary(lm(value~year*variable + county + crop_name, data=datil))
mem <- lmer(value~year*variable + crop_name + (1|county), data=datil)
isSingular(mem)
summary(lm(value~year, data=datil))
summary(lm(value~year*variable, data=datil))
summary(lm(value~year + crop_name, data=datil))
summary(lm(value~year + county, data=datil))
summary(lm(value~year*variable + crop_name, data=datil)) # no improvement with crop_name
summary(lm(value~year*variable + county, data=datil)) # no improvement with county

# rate of change - cover crops
summary(lm(value~year, data=datil[datil$variable %in% "perc_cc",])) # R2 only 0.05, p is sig., slope=0.926
summary(lm(value~year + county, data=datil[datil$variable %in% "perc_cc",])) # improves R2 to 0.5, slope=0.942
# slope means that cc adoption is increasing by 0.9% per year. trend is significant (i.e. not zero)
# which looks about right given the plot above.
# at this rate we will reach 50% adoption in year ___?
# according to means_global IL cover crop adoption is at 6.5% (+/- 0.19).
# increasing by 0.94% per year
(50-6.5)/0.94  # 46 years. 
2023+46 # 2069

# rate of change - no till
summary(lm(value~year, data=datil[datil$variable %in% "perc_nt",])) # R2 only 0.02, p is sig., slope=-1.53
summary(lm(value~year + county, data=datil[datil$variable %in% "perc_nt",])) # improves R2 to 0.5, slope=-1.53
# slope means that nt adoption is DECREASING on average by 1.5% per year.  trend is significant (i.e. not zero)
# which looks about right given the plot above.
# at this rate we will reach 50% adoption in year ___?
# according to means_global IL no till adoption is at 43.5% (+/- 0.53).
# decreasing by 1.53% per year
43.5/1.53  # in 28 years no no-till 
2023+28 # 2051

# rate of change - reduced till
summary(lm(value~year, data=datil[datil$variable %in% "perc_rt",])) # R2 only 0.02, p is sig., slope=1.21
summary(lm(value~year + county, data=datil[datil$variable %in% "perc_rt",])) # improves R2 to 0.27, slope=1.21
# slope means that rt adoption is increasing on average by 1.2% per year.  trend is significant (i.e. not zero)
# which looks about right given the plot above.
# at this rate we will reach 50% adoption in year ___?
# according to means_global IL rt adoption is at 39.9% (+/- 0.41).
# increasing by 1.21% per year
(50-39.9)/1.21  # 8 years. 
2023+8 # 2031

# rate of change - conventional till
summary(lm(value~year, data=datil[datil$variable %in% "perc_ct",])) # R2 only 0.01, p is sig., slope=0.80
summary(lm(value~year + county, data=datil[datil$variable %in% "perc_ct",])) # improves R2 to 0.36, slope=0.80
# slope means that ct adoption is increasing on average by 0.8% per year.  trend is significant (i.e. not zero)
# which looks about right given the plot above.
# at this rate we will reach 50% adoption in year ___?
# according to means_global IL cover crop adoption is at 15.3% (+/-0.36).
# increasing by 0.80% per year
(50-15.3)/0.80  # 43 years. 
2023+43 # 2066














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