
# NY counties (fips) = Genesee (36037), Wyoming (36121), Livingston (36051) 
# IL counties (fips) = Ford (17053), Livingston (17105), Macoupin (17117), Montgomery (17135)

# here we pull together optis, us census of agriculture (via carpe), and il tillage transect 
# survey data for these counties

# script started 6/13/2024 by Bonnie McGill

library(ggplot2)
library(tidyr)



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
# Want to plot census data with corn and soybean crops
# make a copy of census label one as corn and one as soybeans so they get plotted in those facets
cens$crop_name <- rep("Soybeans", nrow(cens))
cens <- cens[,c(2,3,1,7,5,6,4)]
censb <- cens
censb$crop_name <- rep("Corn", nrow(censb))
cens <- bind_rows(cens, censb)
rm(censb)

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



###### 2) make cover crop plot

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
              size=2, width=0.15, alpha=0.8) + 
  geom_line(data=dat[!dat$variable=="perc_cc" & dat$source=="OpTIS",], 
            aes(x=year, y=value, color=variable, linetype=county)) + 
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
