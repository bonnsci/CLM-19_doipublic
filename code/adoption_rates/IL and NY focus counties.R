
# NY counties (fips) = Genesee (36037), Wyoming (36121), Livingston (36051) 
# IL counties (fips) = Ford (17053), Livingston (17105), Macoupin (17117), Montgomery (17135)

# here we pull together optis, us census of agriculture (via carpe), and il tillage transect 
# survey data for these counties

# script started 6/13/2024 by Bonnie McGill

library(ggplot2)
# library(tidyr)
library(dplyr)



###### 1) prepare the data

# load and prep optis data
# prepped from other scripts in this repo
optny <- read.csv("data/optis/datny_cleanlong.csv")
optny$state <- rep("New York", nrow(optny))
optny <- optny[,c(7, 1:3,5,6)]
optny <- filter(optny, county %in% c("Genesee", "Wyoming", "Livingston"))

optil <- read.csv("data/optis/datil_cleanlong.csv")
optil <- optil[,c(1,2,4,5,8,9)]
optil <- filter(optil, county %in% c("Ford", "Livingston", "Macoupin", "Montgomery"))
optil$state <- rep("Illinois", nrow(optil))

opt <- bind_rows(optny, optil)
rm(optil, optny)
opt$source <- rep("OpTIS", nrow(opt))
# unique(opt$crop_name)  #  "Soybeans" "Corn"

# census data
# came from spreadsheet in /CLM-19 FFAR GHG - General / Results / carpe adoption data
cens <- read.csv("data/optis/CaRPE CC_NT_RT 2017_2022.csv")
# note CaRPE refers to conventional tillage as intensive tillage, changing it to CT here:
colnames(cens) <- c("year", "state", "county", "perc_cc", "perc_nt", "perc_rt", "perc_ct")
cens$source <- rep("AgCensus", nrow(cens))
cens <- pivot_longer(cens, cols=c(perc_cc, perc_nt, perc_rt, perc_ct), names_to="variable", values_to="value")
# Census data are for all cropland, not by crop like the other data sources
cens$crop_name <- rep("Cropland", nrow(cens))
# # Want to plot census data with corn and soybean crops
# # make a copy of census label one as corn and one as soybeans so they get plotted in those facets
# cens$crop_name <- rep("Soybeans", nrow(cens))
# cens <- cens[,c(2,3,1,7,5,6,4)]
# censb <- cens
# censb$crop_name <- rep("Corn", nrow(censb))
# cens <- bind_rows(cens, censb)
# rm(censb)


# tillage transect data
till <- read.csv("data/optis/Illinois tillage transect survey data 2015-2018.csv")
# came from spreadsheet in /CLM-19 FFAR GHG - General / Results / IL tillage transect data
# as.data.frame(names(till))
# note the "%" symbol doesn't come through into R but these aren't acres these are %s:
till <- till[,c(1,2,4,9,12,15,18)]
colnames(till) <- c("year", "crop_name", "county", "perc_nt", "perc_mt", "perc_rt", "perc_ct")
till$source <- rep("Transect", nrow(till))
# according to the definitions of residue cover, fields categorized as "mulch till" in 
# transect data would correspond to "no till" in OpTIS data
# sum no till and mulch till for new "no till"
till$perc_nt <- till$perc_nt + till$perc_mt
# remove mulch till
till$perc_mt <- NULL
till <- pivot_longer(till, cols=c(perc_nt, perc_rt, perc_ct), names_to="variable", values_to="value")
till$state <- rep("Illinois", nrow(till))
till <- till[,c(7,3,1,2,5,6,4)]
unique(till$crop_name)
till$crop_name <- ifelse(till$crop_name=="corn", "Corn", "Soybeans")


dat <- bind_rows(cens,opt,till)
rm(cens,opt,till)

# dat$crop_source <- paste0(dat$crop_name, " ", dat$source)

dat$region <- ifelse(dat$state=="New York", "Western NY", 
                ifelse(dat$state=="Illinois" & dat$county %in% c("Ford", "Livingston"), "Northern IL",
                  ifelse(dat$state=="Illinois" & dat$county %in% c("Macoupin", "Montgomery"), "South Central IL", "X")))
unique(dat$region)  

dat$costate <- paste0(dat$county, " County, ", dat$state)


# aligning years
# AgCensus 2017 cover crops correspond to Optis 2018 cover crops
# AgCensus 2017 tillage correspond to spring OptIS 2017 and fall 2018 tillage
# 2017 transect corresponds to 2017 AgCensus and 2017 optis
# do optis - 1 for covers
# no change for tillage years





# dat$line <- ifelse(dat$costate %in% c("Ford County, Illinois", "Macoupin County, Illinois", "Genesee County, New York"), 1, 
#                                       ifelse(dat$costate == "Wyoming County, New York", 3, 2))


###### 2) make tillage plot

windows(xpinch=200, ypinch=200, width=5, height=5)

fills <- c("perc_ct" = "#EE8866",
           "perc_rt" = "#BBCC33",
           "perc_nt" = "#77AADD")

fields <- c("Corn" = "Corn fields", 
               "Soybeans" = "Soybean fields", 
               "Western NY" = "Western NY",
               "Northern IL" = "Northern IL",
               "South Central IL" = "South Central IL")

ggplot() +
  geom_jitter(data=dat[!dat$variable=="perc_cc" & !dat$source=="OpTIS",], 
              aes(x=year, y=value, shape=source, color=variable), 
              size=1, width=0.15, alpha=0.8, stroke=1.2) + 
  geom_line(data=dat[!dat$variable=="perc_cc" & dat$source=="OpTIS",], 
            aes(x=year, y=value, color=variable, linetype=factor(line)), size=0.8) + 
  facet_grid(rows=vars(region), cols=vars(crop_name), labeller = as_labeller(fields)) +
  scale_color_manual(breaks = c("perc_ct", "perc_rt", "perc_nt"),
                    values = fills,
                    labels = c("Conventional", "Reduced", "No-till"),
                    name = "Tillage type") +
  scale_shape_manual(breaks = c("AgCensus", "Transect"),
                     values = c(16, 0),
                     name="Data source") +
  scale_x_continuous(name="Year", breaks=seq(2015,2022,1))+
  ylab("% of county acres") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text=element_text(size=9),
    axis.title=element_text(size=12, face="bold"),
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
    strip.text=element_text(size=10),
    strip.background = element_rect(fill="gray95"),
    legend.text=element_text(size=11),
    legend.title=element_text(size=10, face="bold"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size = unit(0.4, "cm")
  )	 

ggsave("plots/adoption/tillage_3sources_scatter.png", width=8, height=4.5, dpi=300)




###### 2b) make a tillage plot that is the average of the counties per region


regiondat <- group_by(dat, region, year, crop_name, variable, source) %>%
  summarize(mean_value = mean(value))

regiondat$cc_year <- ifelse(regiondat$source=="OpTIS", regiondat$year-1, regiondat$year)

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot() +
  geom_jitter(data=regiondat[!regiondat$source=="OpTIS",], 
              aes(x=year, y=mean_value, color=source, shape=crop_name),   # Data are same between corn and soybean, Agcensus does not differentiate
              size=1.8, width=0.2, alpha=1, stroke=1.4) + 
  # cover crop lines use different year
  geom_line(data=regiondat[regiondat$source=="OpTIS" & regiondat$variable=="perc_cc",], 
            aes(x=cc_year, y=mean_value, color=source, linetype=crop_name), size=0.6, alpha=0.9) + 
  # tillage lines use regular year
  geom_line(data=regiondat[regiondat$source=="OpTIS" & !regiondat$variable=="perc_cc",], 
            aes(x=year, y=mean_value, color=source, linetype=crop_name), size=0.6, alpha=0.9) + 
  facet_grid(rows=vars(factor(variable,levels=c("perc_cc", "perc_nt", "perc_rt", "perc_ct"))), 
             cols=vars(factor(region)), 
             labeller = as_labeller(
               c("perc_cc" = "Cover Crop", "perc_nt" = "No-till",
                 "perc_rt" = "Reduced tillage", "perc_ct" = "Conventional tillage",
                 "Northern IL" = "Northern Illinois",
                 "South Central IL" = "South Central Illinois",
                 "Western NY" = "Western New York")),
              scales="free")+
  scale_color_manual(breaks = c("AgCensus", "OpTIS", "Transect"),
                     values=c("#88CCEE", "#999933", "#AA4499"),
                     name = "Data Source") +
  scale_shape_manual(breaks = c("Corn", "Soybeans", "Cropland"),
                     values = c(16, 1, 0),
                     name="AgCensus/Transect acreage type") +
  scale_linetype_manual(values=c(1,2), name="OpTIS acreage type") +
  scale_x_continuous(name="Year", breaks=seq(2015,2022,1))+
  ylab("% of county acres") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text.y=element_text(size=9),
    axis.text.x=element_text(size=9, angle=-30, hjust=0),
    axis.title=element_text(size=12, face="bold"),
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
    strip.text=element_text(size=10),
    strip.background = element_rect(fill="gray95"),
    legend.text=element_text(size=11),
    legend.title=element_text(size=10, face="bold"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size = unit(0.6, "cm")
  )	 

ggsave("plots/adoption/perc acres_4practices_2015-22.png", width=10, height=7, dpi=300)



###### 3) make cover crop plot

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot() +
  geom_jitter(data=dat[dat$variable=="perc_cc" & !dat$source=="OpTIS",], 
              aes(x=year, y=value, shape=source), 
              size=1, width=0.15, alpha=0.8, color="navyblue") + 
  geom_line(data=dat[dat$variable=="perc_cc" & dat$source=="OpTIS",], 
            aes(x=year, y=value, linetype=factor(line)),
            color="navyblue") + 
  facet_grid(rows=vars(region), cols=vars(crop_name), labeller = as_labeller(fields)) +
  scale_shape_manual(breaks = c("AgCensus", "Transect"),
                     values = c(16, 0),
                     name="Data source") +
  scale_x_continuous(name="Year", breaks=seq(2015,2022,1))+
  ylab("% of county acres") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text=element_text(size=9),
    axis.title=element_text(size=12, face="bold"),
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
    strip.text=element_text(size=10),
    strip.background = element_rect(fill="gray95"),
    legend.text=element_text(size=11),
    legend.title=element_text(size=10, face="bold"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size = unit(0.4, "cm")
  )	 

ggsave("plots/adoption/covercrop_2sources_scatter.png", width=8, height=4.5, dpi=300)


###### 3b) make regional (county averaged) cover crop plot

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot() +
  geom_jitter(data=region[region$variable=="perc_cc" & !region$source=="OpTIS",], 
              aes(x=year, y=mean_value, shape=source, color=region), 
              size=1.2, width=0.15, alpha=0.8, show.legend=F) + 
  geom_line(data=region[region$variable=="perc_cc" & region$source=="OpTIS",], 
            aes(x=year, y=mean_value, color=region), size=0.8) + 
  facet_grid(cols=vars(crop_name), labeller = as_labeller(fields)) +
  scale_shape_manual(breaks = c("AgCensus", "Transect"),
                     values = c(16, 0),
                     name="Data source") +
  scale_color_manual(breaks = c("Northern IL", "South Central IL", "Western NY"),
                     values = c("#882255", "#999933", "#88CCEE"),
                     name = "Region") +
  scale_x_continuous(name="Year", breaks=seq(2015,2022,1))+
  ylab("% of county acres") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text=element_text(size=9),
    axis.title=element_text(size=12, face="bold"),
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
    strip.text=element_text(size=10),
    strip.background = element_rect(fill="gray95"),
    legend.text=element_text(size=11),
    legend.title=element_text(size=10, face="bold"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size = unit(0.4, "cm")
  )	 

ggsave("plots/adoption/covercrop_2sources_scatter_regionavg.png", width=8, height=4, dpi=300)

