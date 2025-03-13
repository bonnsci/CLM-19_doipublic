# script started 8/22/2024 by Bonnie McGill


################################################
# FIRST RUN "0_Load_adopt_data.R" to load packages and data  
################################################

# Further prep data

# separate tillage and cc data - will have different y-axis ranges and background bar chart data
# make tillage dataset
dat.till <- dat[dat$variable != 'perc_cc',]
dat.till$variable <- factor(dat.till$variable, levels=c('perc_ct', 'perc_rt', 'perc_nt'))
levels(dat.till$variable) <- c('Conventional till', 'Reduced till', 'No-till')
# make CC dataset 
dat.cc <- dat[dat$variable == 'perc_cc',]


# COVER CROPS PLOT ##### Fig. 1a

windows(xpinch=200, ypinch=200, width=5, height=5)

# to plot these with different secondary axes I need to plot CC and tillage separately
# Rather than as facets, then plot them together using plot_layout()

dat.cc$year.cat <- as.factor(dat.cc$year)
clm2$year.cat <- as.factor(clm2$year)
dat.cc$crop_source <- paste0(dat.cc$crop_name, "_", dat.cc$source)
ccsum <- Rmisc::summarySE(dat.cc, measurevar="value", groupvars=c("year.cat", "source", "crop_name", "region", "crop_source"))
ccsum <- ccsum[ccsum$year.cat %in% c("2015", "2017", "2018"),]

ggplot() +
  #GDD bars in background
  geom_bar(data=clm2[clm2$variable=="perc_cc" & clm2$year %in% c(2015, 2017, 2018),], 
           aes(x=year.cat, y=clm.dat, fill=variable), 
           stat="identity", width=0.9, alpha=0.3) +
  scale_fill_manual(breaks = "perc_cc",
                     labels = "Fall GDD",
                     values="#DDCC77",
                     name = "") +
  # add points
  geom_pointrange(data= ccsum, aes(x=year.cat, y=value, ymin= value-ci, ymax=value+ci, color=crop_source, shape=crop_source), 
             size=0.5, alpha=0.7, stroke=1,
             position=position_dodge(width=0.4)) +

  scale_color_manual(breaks = c("Corn_OpTIS", "Soybeans_OpTIS", "Cropland_AgCensus"),
                     labels = c("OpTIS Corn", "OpTIS Soybeans", "AgCensus (Cropland)"),
                     values=c( "#999933", "#999933", "#332288"),
                     name = "Data Source") +
  scale_shape_manual(breaks = c("Corn_OpTIS", "Soybeans_OpTIS", "Cropland_AgCensus"),
                     labels = c("OpTIS Corn", "OpTIS Soybeans", "AgCensus (Cropland)"),
                     values = c(16, 1, 0),
                     name="Data Source") +
  # plot settings/appearance
  facet_grid(rows=vars(factor(variable,levels=c( "perc_cc"))), 
             cols=vars(factor(region)), 
             labeller = as_labeller(
               c("perc_cc" = "Cover crop",
                 "Central IL" = "CIL",
                 "Southern IL" = "SIL",
                 "Western NY" = "WNY"))) +
  scale_y_continuous(sec.axis = sec_axis(~.*25, name="Fall GDD"), 
                     name="% of cropland",
                     expand=expansion(mult=c(0,0.05))) +
  scale_x_discrete(name="Year") + # , 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text.y=element_text(size=9),
    axis.text.x=element_blank(),
    # axis.text.x=element_text(size=9, angle=-30, hjust=0.3, vjust=0.2),
    axis.title.x=element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y=element_text(size=12, face="bold", vjust=+3),
    axis.title.y.right=element_text(size=12, face="bold", vjust=+3),  # second y axis
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_blank(), #element_rect(color="grey50", fill=NA, linewidth=0.5),
    strip.text=element_text(size=10),
    strip.background = element_rect(fill="gray95"),
    legend.text=element_text(size=11),
    legend.title=element_text(size=10, face="bold"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size = unit(0.6, "cm")
  )	 

ggsave("plots/adoption/perc acres_by year_CC_2015,17,18_no x axis.png", width=8, height=2.3, dpi=300)
# ggsave("plots/adoption/perc acres_by year_CC_2015,17,18.png", width=10, height=3, dpi=300)




# TILLAGE PLOT  ##### Fig. 1b-d

windows(xpinch=200, ypinch=200, width=5, height=5)

dat.till$year.cat <- as.factor(dat.till$year)
clm2$year.cat <- as.factor(clm2$year)
dat.till$crop_source <- paste0(dat.till$crop_name, "_", dat.till$source)
tillsum <- Rmisc::summarySE(dat.till, 
                     measurevar="value", 
                     groupvars=c("year.cat","variable", "source", "crop_name", "region", "crop_source"),
                     na.rm=TRUE)
clm2.till<- clm2[clm2$variable !="perc_cc" & clm2$year %in% c(2015, 2017, 2018),]
clm2.till$variable <- factor(clm2.till$variable, levels=c('perc_nt', 'perc_rt', 'perc_ct'))
levels(clm2.till$variable) <- c('No-till', 'Reduced till', 'Conventional till')
tillsum <- tillsum[tillsum$year %in% c(2015, 2017, 2018),]


ggplot() +
  # precip bars in background
  geom_bar(data=clm2.till, 
           aes(x=year.cat, y=clm.dat, fill=variable), 
           stat="identity", width=0.9, alpha=0.2) +
  scale_fill_manual(values=c("No-till"="#88CCEE","Reduced till"= "#88CCEE","Conventional till"="#88CCEE"),
                    breaks = "No-till",  # only show one key in legend
                    labels = "Crop year fall &\nspring precip",
                    name = "") +
  # add points
  geom_pointrange(data= tillsum, aes(x=year.cat, y=value, ymin= value-ci, ymax=value+ci, color=crop_source, shape=crop_source), 
                  size=0.5, alpha=0.7, stroke=1,
                  position=position_dodge(width=0.4)) +
  scale_color_manual(breaks = c("Corn_OpTIS", "Soybeans_OpTIS", "Cropland_AgCensus", "Corn_Transect", "Soybeans_Transect"),
                     labels = c("OpTIS Corn", "OpTIS Soybeans", "AgCensus (Cropland)", "Transect Corn", "Transect Soybeans"),
                     values=c( "#999933", "#999933", "#332288", "#AA4499", "#AA4499"),
                     name = "Data Source") +
  scale_shape_manual(breaks = c("Corn_OpTIS", "Soybeans_OpTIS", "Cropland_AgCensus", "Corn_Transect", "Soybeans_Transect"),
                     labels = c("OpTIS Corn", "OpTIS Soybeans", "AgCensus (Cropland)", "Transect Corn", "Transect Soybeans"),
                     values = c(16, 1, 0, 16, 1),
                     name="Data Source") +
  # plot settings/appearance
  facet_grid(rows=vars(factor(variable)), 
             cols=vars(factor(region)), 
             labeller = as_labeller(
               c("No-till"="No-till","Reduced till"= "Reduced till", "Conventional till"= "Conventional till",
                 "Central IL" = "CIL","Southern IL" = "SIL","Western NY" = "WNY"))) +
  scale_y_continuous(sec.axis = sec_axis(~.*scale_till, name="Precip (mm)", breaks=c(0,200,400,600)), 
                     name="% of cropland",
                     expand=expansion(mult=c(0,0.05))) +
  scale_x_discrete(name="Year") + # , 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text.y=element_text(size=9),
    axis.text.x=element_text(size=9, angle=-30, hjust=0.3, vjust=0.2),
    axis.title.y=element_text(size=12, face="bold", vjust=+3),
    axis.title.y.right=element_text(size=12, face="bold", vjust=+3),  # second y axis
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_blank(), #element_rect(color="grey50", fill=NA, linewidth=0.5),
    strip.text.x=element_blank(),
    strip.background.x = element_blank(),
    strip.text.y =element_text(size=10),
    strip.background.y = element_rect(fill="gray95"),
    legend.text=element_text(size=11),
    legend.title=element_text(size=10, face="bold"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size = unit(0.6, "cm")
  )	 

ggsave("plots/adoption/perc acres_by year_tillage_2015,17,18_no region labels.png", width=8, height=6, dpi=300)


