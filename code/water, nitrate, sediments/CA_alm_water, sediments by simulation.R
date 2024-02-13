library(tidyverse) # has ggplot, dplyr, etc.
library(reshape2)

se <- function(x) sd(x) / sqrt(length(x))

# if you only need annual totals skip to wdatyr below

# # if you need daily estimates use this:
wdat <- read.csv("data/large_data/daily water, sediments/CA_almonds_day_water.csv")
# 
# # UNITS: Transpiration, Evaporation, Water leaching, and runoff are mm/day.
# # Sediment yield is kg/ha.
# 
# # # sum data by year
# wdatyr <- wdat %>%
#   group_by(site_name, crop_system_name, management_name, climate_scenario, Year) %>%
#   summarize(trans.yr = sum(Transpiration.mm.), evap.yr = sum(Evaporation.mm.),
#             leach.yr = sum(WaterLeaching), run.yr = sum(Runoff), sed.yr = sum(SedimentYield))
# 
# # # 
# 
# write.csv(wdatyr, "data/water, nitrate, sediments/CA_alm_water,seds_annualtotals.csv", row.names=F)
# 
# # clean up
# rm(wdat)



wdatyr <- read.csv("data/water, nitrate, sediments/CA_alm_water,seds_annualtotals.csv")

colnames(wdatyr)[c(2,3,5)] <- c("crop", "management", "year") 


wdatyr <- wdatyr[wdatyr$year>2021 & wdatyr$year<2073 & wdatyr$climate_scenario=="rcp60",]
# 
# # make dummy variables
# wdatyr$till <- ifelse(grepl("ct-", wdatyr$management), "CT", 
#                       ifelse(grepl("rt-", wdatyr$management), "RT", "NT"))
# # # check
# # unique(wdatyr$till)
# 
# # dummy for CC or NC
# wdatyr$cc <- ifelse(grepl("-cc-", wdatyr$management), "CC", "NC")
# # check
# unique(wdatyr$cc)

# dummy for N treatment
# wdatyr$nfert <- ifelse(grepl("-cn", wdatyr$management), "High N", 
#                        ifelse(grepl("-fn", wdatyr$management), "Fall N","Recommended N"))
# # check
# unique(wdatyr$nfert)

# dummy for decade
wdatyr$decade <- ifelse(wdatyr$year <2031, "2020s",
                        ifelse(wdatyr$year>=2031 & wdatyr$year <2041, "2030s",
                               ifelse(wdatyr$year>=2041 & wdatyr$year <2051, "2040s",
                                      ifelse(wdatyr$year>=2051 & wdatyr$year <2061, "2050s",
                                             ifelse(wdatyr$year>=2061 & wdatyr$year <2071, "2060s", "2070s")))))
# unique(wdatyr$decade)

wdatyr$et.yr <- wdatyr$evap.yr + wdatyr$trans.yr

# write.csv(wdatyr, "data/water, nitrate, sediments/wdatyr.csv", row.names=F)

# first site means across all years,

wdat_sitemean <- wdatyr %>%
  group_by(site_name, management) %>%
  summarize(et_mean = mean(et.yr), 
            evap_mean = mean(evap.yr),
            trans_mean = mean(trans.yr),
            leach_mean = mean(leach.yr),
            run_mean = mean(run.yr),
            sed_mean = mean(sed.yr),
            et_se = se(et.yr), 
            evap_se = se(evap.yr),
            trans_se = se(trans.yr),
            leach_se = se(leach.yr),
            run_se = se(run.yr),
            sed_se = se(sed.yr)) %>%
  pivot_longer(cols=-c("site_name", "management"), # i.e., don't pivot these columns
               names_to=c("component", ".value"), # what we are calling the objects 
               # there should be as many new columns as there are unique tags
               # after the "_" separator (i.e., "_mean" and "_se")
               names_sep="_")  # note this type of pivot_longer doesn't work if you use a "." separator!
                          

################################# left off here adapting the code for almonds 2/5/2024




  
wdat_globmean <- wdatyr %>%
  group_by(site_name, management) %>%
  summarize(et.mean = mean(et.yr), 
            evap.mean = mean(evap.yr),
            trans.mean = mean(trans.yr),
            leach.mean = mean(leach.yr),
            run.mean = mean(run.yr),
            sed.mean = mean(sed.yr),
            et.se = se(et.yr), 
            evap.se = se(evap.yr),
            trans.se = se(trans.yr),
            leach.se = se(leach.yr),
            run.se = se(run.yr),
            sed.se = se(sed.yr)) 
  
  
  

############################### 50 YEAR TOTAL WATER AND SED LOSSES PLOT
# prep data for plotting
# put data in long form for plotting, 1 column evap, trans, et, leach, run, sed
wdat_tmttotlong <- melt(wdat_tmttot, id=c("till", "cc"))
# separate means from se's
wdat_tmttotlong$mean.se <- ifelse(grepl("mean", wdat_tmttotlong$variable), "mean", "se")

# make new column for se values
dat.se <- wdat_tmttotlong[wdat_tmttotlong$mean.se=="se",1:4]
colnames(dat.se)[4] <- "se"
dat.se$variable <- gsub(".se", "", dat.se$variable)
dat.mean <- wdat_tmttotlong[wdat_tmttotlong$mean.se=="mean",1:4]
colnames(dat.mean)[4] <- "mean"
dat.mean$variable <- gsub(".mean", "", dat.mean$variable)
wtotlong <- left_join(dat.mean, dat.se) # , relationship="one-to-one")
rm(dat.se, dat.mean)

windows(xpinch=200, ypinch=200, width=5, height=5)


wtotlong$variable <- factor(wtotlong$variable, levels=c("evap", "trans", "leach", "run", "sed", "et"))

# 50 YR TOTAL WATER LOSSES
ggplot(data=wtotlong[!wtotlong$variable %in% c("sed", "et"),], aes(x=till, y=mean, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288", width=0.8) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, position=position_dodge(0.8), color="#332288") +
  facet_grid(cols=vars(factor(cc, levels=c("CC", "NC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c(CC="Has Cover Crop", NC="No Cover Crop"))) +
                # "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
  scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99", "#AA4499")) +  # "#999933"
  xlab("tillage") +
  ylab("total mm 2022 to 2072") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/CA_water losses 50 yr total bars.png", width=6, height=2.5, dpi=300)


# 50 YR TOTAL SEDIMENT LOSSES
ggplot(data=wtotlong[wtotlong$variable %in% c("sed"),], aes(x=till, y=mean)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288", width=0.8, fill="gray70") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, position=position_dodge(0.8),color="#332288") +
  facet_grid( #rows=vars(factor(till, levels=c("CT", "NT", "RT"))), 
             cols=vars(factor(cc, levels=c("CC", "NC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c(CC="Has Cover Crop", NC="No Cover Crop"))) +
                # "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
  xlab("Tillage") +
  ylab("total kg/ha 2022 to 2072") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/CA_sediments 50 yr total bars.png", width=6, height=2.5, dpi=300)




############################### WATER, Sed LOSSES PER YEAR PLOT
# prep data for plotting
# put data in long form for plotting, 1 column for evap, trans, et, runoff, leach, sed
wdat_peryrlong <- melt(wdat_tmtperyr, id=c("till", "cc"))
# separate means from se's
wdat_peryrlong$mean.se <- ifelse(grepl("mean", wdat_peryrlong$variable), "mean", "se")

# make new column for se values
dat.se <- wdat_peryrlong[wdat_peryrlong$mean.se=="se",1:4]
colnames(dat.se)[4] <- "se"
dat.se$variable <- gsub(".se", "", dat.se$variable)
dat.mean <- wdat_peryrlong[wdat_peryrlong$mean.se=="mean",1:4]
colnames(dat.mean)[4] <- "mean"
dat.mean$variable <- gsub(".mean", "", dat.mean$variable)
wpyrlong <- left_join(dat.mean, dat.se) # , relationship="one-to-one")
rm(dat.se, dat.mean)

windows(xpinch=200, ypinch=200, width=5, height=5)


############# change to fill by tillage, facet by variable

# # are the bars in the below plot significantly different from each other?
wdat_persite <- wdatyr %>%
  group_by(site_name, till, cc) %>%
  summarize(et = mean(et.yr), # mean across all years and nferts and two cropping systems
            evap = mean(evap.yr),
            trans = mean(trans.yr),
            leach = mean(leach.yr),
            run = mean(run.yr),
            sed = mean(sed.yr)) %>%
  melt(., ids=c("site_name", "till", "cc"))

wdat_persitecheck <- wdat_persite %>%
  group_by(till, cc, variable) %>%
  summarize(mean.val = mean(value))

effect <- aov(value~variable*till*cc, 
              data=wdat_persite[!wdat_persite$variable %in% c("et", "sed"), ])
summary(effect)

tukout <- TukeyHSD(effect)
cld <- multcompView::multcompLetters4(effect, tukout)

wdat_nosite <- group_by(wdat_persite, till, cc, variable) %>%
  filter(! variable %in% c("et", "sed"))%>%
  summarize(mean=mean(value),
            se=se(value)) %>%
  arrange(desc(mean))

cld <- as.data.frame.list(cld$`variable:till:cc`)
wdat_nosite$cld <- cld$Letters

# use these letters in the plot:

wpyrlong$till <- factor(wpyrlong$till, levels = c("CT", "RT", "NT"), ordered=T)
wdat_nosite$till <- factor(wdat_nosite$till, levels= c("CT", "RT", "NT"), ordered=T)

ggplot(data=wpyrlong[!wpyrlong$variable %in% c("sed", "et"),], aes(x=variable, y=mean, fill=till)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, position=position_dodge(0.9)) +
  facet_grid( #rows=vars(factor(till, levels=c("CT", "NT", "RT"))), 
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c(NC="No Cover Crop", CC="Has Cover Crop"))) +
                 #"CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
  scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99")) + #, "#AA4499")) +  # "#999933"
  geom_bar(data=wdat_nosite, 
           aes(x=variable, y=mean), stat="identity", 
           position=position_dodge(), color=NA, fill=NA) +
  geom_text(data=wdat_nosite, 
            aes(x=variable, label=cld, y=mean), vjust=-0.5,
            position=position_dodge(0.9), color="gray20", size=4, fontface="bold") +
  xlab("Water Loss") +
  ylab("Mean annual water loss\n(mm/yr) 2022 to 2072") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/water, nitrate, sediments/CA_water losses mean annual barsRCP60_withletters.png", width=8, height=3, dpi=300)



# are the bars below significantly different?
effectsed <- aov(value~till*cc, 
              data=wdat_persite[wdat_persite$variable == "sed", ])
summary(effectsed)

tukoutsed <- TukeyHSD(effectsed)
cldsed <- multcompView::multcompLetters4(effectsed, tukoutsed)

wdat_nositesed <- group_by(wdat_persite, till, cc, variable) %>%
  filter(variable=="sed") %>%
  summarize(mean=mean(value),
            se=se(value)) %>%
  arrange(desc(mean))

cldsed <- as.data.frame.list(cldsed$`till:cc`)
wdat_nositesed$cld <- cldsed$Letters


ggplot(data=wpyrlong[wpyrlong$variable %in% c("sed"),], aes(x=till, y=mean)) +
  geom_bar(stat="identity", position=position_dodge(), color="sienna4", width=0.8, fill="burlywood3") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, position=position_dodge(0.8),color="sienna4", linewidth=1) +
  facet_grid( #rows=vars(factor(till, levels=c("CT", "NT", "RT"))), 
    cols=vars(factor(cc, levels=c("NC", "CC"))), 
    #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
    labeller = as_labeller(
      c(NC="No Cover Crop", CC="Has Cover Crop"))) +
  # "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
  scale_y_continuous(breaks=seq(0,300, 50), limits=c(0,300)) +
  scale_x_discrete(breaks = c("CT", "RT", "NT"),
                   labels = c("Conv.", "Reduced", "No")) +
  geom_bar(data=wdat_nositesed, 
           aes(x=till, y=mean), stat="identity", 
           position=position_dodge(), color=NA, fill=NA) +
  geom_text(data=wdat_nositesed, 
            aes(x=till, label=cld, y=mean), vjust=-2,
            position=position_dodge(0.9), color="gray20", size=4, fontface="bold") +
  xlab("Tillage") +
  ylab(expression(bold('2022-72 mean annual sediment yield (kg ha'^-1*')'))) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(size=11),  #angle=-10, hjust=0, 
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(1,0.5,1,0.5), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))
# # strip.background=element_rect(fill="lightblue", color="black", size=1) 

ggsave("plots/water, nitrate, sediments/CA_sediments mean annual barsRCP60_withletters.png", width=5, height=4.5, dpi=300)






# noticing that the different treatments seem to have different water balances, but getting same precip input.
# What do these results look like within one year (rather than average or total over many years)

# wdatyr.wbal <- wdatyr %>%
#   group_by(site_name, year, till, cc, nfert, crop_system_name) %>%
#   summarize(sum.etlr = trans.yr + evap.yr + leach.yr + run.yr)
# 
# # average across sites, N treatments
# wdatyr.mean <- wdatyr %>%
#   group_by(year, till, cc) %>%
#   summarize(trans.yrm = mean(trans.yr),
#             evap.yrm = mean(evap.yr),
#             leach.yrm = mean(leach.yr),
#             run.yrm = mean(run.yr),
#             sed.yrm = mean(sed.yr),
#             et.yrm = mean(et.yr))

# wdatyrlong <- melt(wdatyr.mean, id=c("year", "till", "cc"))
# 
# ggplot(data=wdatyrlong[wdatyrlong$year==2025 & !(wdatyrlong$variable %in% c("sed.yrm", "et.yrm")),], 
#        aes(x=till, y=value, fill=variable)) +
#   geom_bar(stat="identity", position=position_dodge(), color="#332288", width=0.8) +
#   #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.3, position=position_dodge(0.8), color="#332288") +
#   facet_grid(cols=vars(factor(cc, levels=c("CC", "NC"))), 
#              #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
#              labeller = as_labeller(
#                c(CC="Has Cover Crop", NC="No Cover Crop"))) +
#   # "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
#   #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) 
#   scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99", "#AA4499")) +  # "#999933"
#   xlab("tillage") +
#   ylab("total mm 2025") +
#   theme(
#     panel.grid.minor=element_blank(), 
#     panel.grid.major=element_blank(),
#     panel.background = element_rect(fill = 'gray95'))
