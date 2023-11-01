# this script explores ways to quantify resilience in the CLM-19 dataset.
# following ideas in the CLM literature / resilience folder. https://americanfarmlandtrust.sharepoint.com/:f:/s/ClimatePrograms/EuOX-vSqLoZHmB8X1WTkIO4B4ZsmvAwlH2U85dBLcLcoRQ?e=jrJY4o
# started 10/31/2023 BMM

# esp. Scheffer et al. 2015
# we'll start with the idea of ranking resilience. I'm thinking different SHMS
# will have different resiliencies to climate extremes, i.e. with CC and NT, more resilient
# biomass C in SOC accumulation in the face of drought / extreme wet, etc.

# load packages
library(tidyverse)
library(MASS)  # for boxcox
install.packages("ggh4x") 
library(ggh4x) # for strip_themed() and facet_grid2() in ggplot

# pull in data
# biomass 
bmil <- read.csv("data/biomass/biomass_IL.csv")
colnames(bmil)[9] <- "grainc"

# climate - SPEI
spei <- read.csv("data/climate data not large/speiIL.csv")

# let's focus on rcp 6.0 for now
bmil <- bmil[bmil$climate_scenario=="rcp60",]

spei <- spei[spei$rcp=="rcp60",]

# biomass is once per year, spei is monthly, could get annual (or water year) average
# spei or look at July SPEI (or other month or average of months). Let's start
# with july Spei

spei <- spei %>%
  separate_wider_delim(dat, delim=" ", 
                       names=c("month", "year"),  # split the ID into two columns
                       cols_remove=F)

bmilgr <- bmil[,c(2,4, 6:9),]

# make dummy factor for CT, RT, NT
bmilgr$till <- ifelse(grepl("ct-", bmilgr$management_name), "CT", 
                    ifelse(grepl("rt-", bmilgr$management_name), "RT", "NT"))
# # check
# unique(bmilgr$till)

# dummy for CC or NC
bmilgr$cc <- ifelse(grepl("-cc-", bmilgr$management_name), "CC", "NC")
# # check
# unique(bmilgr$cc)

# dummy for N treatment
bmilgr$nfert <- ifelse(grepl("-cn", bmilgr$management_name), "High N", 
                     ifelse(grepl("-fn", bmilgr$management_name), "Fall N","Recommended N"))
# # check
# unique(bmilgr$nfert)


# merge biomass and spei data
spei$year <- as.numeric(spei$year)

speibm <- inner_join(spei[,-2], bmilgr, join_by(site==site_name, year==Year), 
                    relationship="many-to-many")

# grainc z-score
speibm$grainz <- (speibm$grainc - mean(speibm$grainc))/sd(speibm$grainc)

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=speibm[speibm$crop_name %in% c("corn, grain") & speibm$month =="Jul",], 
       aes(x=fit.12mo, y=grainc)) +  #  x=abs(fit.1mo), x=fit.12mo, x=grainz
  geom_point(size=0.1, alpha=0.5) +
  scale_x_continuous(breaks=c(-2.0, -1.6, -1.3, -0.8, 1.30, 1.60, 2.0, 3.0),
                     labels=c(-2.0, -1.6, -1.3, -0.8, 1.30, 1.60, 2.0, 3.0)) +
  facet_grid(cols=vars(ns, till), rows=vars(cc, nfert))

# looks like corn grain c less resilient to extreme weather when: fall N, in the South. nothing obviously apparent between tillage or cover treatments

ggplot(data=speibm[speibm$crop_name %in% c("soybean") & speibm$month =="Jul",], 
       aes(x=fit.12mo, y=grainc)) +  #  x=abs(fit.1mo), x=fit.12mo
  geom_point(size=0.1, alpha=0.5) +
  facet_grid(cols=vars(ns, till), rows=vars(cc))



# following fig. 5c in Scheffer et al 2015 - inferring probabilistic resilience from massive data.
# Not sure we have "massive" data but probably enough to try this approach.
# I'm thinking histograms of crop yields at different drought and wetness categories
# across our "treatments".

# need to add ID variable for SPEI category for each row of data
# again classifications from: https://droughtmonitor.unl.edu/About/AbouttheData/DroughtClassification.aspx


speibm$spcat1mo <- ifelse(speibm$fit.1mo >= -0.4999999 & speibm$fit.1mo <=0.49999999, "Normal",
                          # drought conditions
                          ifelse(speibm$fit.1mo >= -0.7999999 & speibm$fit.1mo <= -0.50, "Abnormally Dry",  
                                 ifelse(speibm$fit.1mo >= -1.2999999 & speibm$fit.1mo <= -0.80, "Moderate Drought",
                                        ifelse(speibm$fit.1mo >= -1.59999999 & speibm$fit.1mo <= -1.30, "Severe Drought",
                                               ifelse(speibm$fit.1mo >= -1.9999999 & speibm$fit.1mo <= -1.60, "Extreme Drought",
                                                      ifelse(speibm$fit.1mo <= -2.0, "Exceptional Drought",
                           # wet conditions                           
                                            ifelse(speibm$fit.1mo <= 0.7999999 & speibm$fit.1mo >= 0.50, "Abnormally Wet",    
                                                  ifelse(speibm$fit.1mo <= 1.2999999 & speibm$fit.1mo >= 0.80, "Moderately Wet",
                                                        ifelse(speibm$fit.1mo <= 1.5999999 & speibm$fit.1mo >= 1.30, "Severely Wet",
                                                              ifelse(speibm$fit.1mo <= 1.999999 & speibm$fit.1mo >= 1.60, "Extremely Wet",
                                                                    ifelse(speibm$fit.1mo >= 2.0, "Exceptionally Wet", "X")))))))))))
                                                             
# Test
unique(speibm$spcat1mo)

speibm$spcat12mo <- ifelse(speibm$fit.12mo >= -0.4999999 & speibm$fit.12mo <=0.49999999, "Normal",
                          # drought conditions
                          ifelse(speibm$fit.12mo >= -0.7999999 & speibm$fit.12mo <= -0.50, "Abnormally Dry",  
                                 ifelse(speibm$fit.12mo >= -1.2999999 & speibm$fit.12mo <= -0.80, "Moderate Drought",
                                        ifelse(speibm$fit.12mo >= -1.59999999 & speibm$fit.12mo <= -1.30, "Severe Drought",
                                               ifelse(speibm$fit.12mo >= -1.9999999 & speibm$fit.12mo <= -1.60, "Extreme Drought",
                                                      ifelse(speibm$fit.12mo <= -2.0, "Exceptional Drought",
                                                             # wet conditions                           
                                                             ifelse(speibm$fit.12mo <= 0.7999999 & speibm$fit.12mo >= 0.50, "Abnormally Wet",    
                                                                    ifelse(speibm$fit.12mo <= 1.2999999 & speibm$fit.12mo >= 0.80, "Moderately Wet",
                                                                           ifelse(speibm$fit.12mo <= 1.5999999 & speibm$fit.12mo >= 1.30, "Severely Wet",
                                                                                  ifelse(speibm$fit.12mo <= 1.999999 & speibm$fit.12mo >= 1.60, "Extremely Wet",
                                                                                         ifelse(speibm$fit.12mo >= 2.0, "Exceptionally Wet", "X")))))))))))

# Test
unique(speibm$spcat12mo)
# NA values expected because first 11 months fit.12mo = NA. 

speibm$spcat12mo <- factor(speibm$spcat12mo,
           levels=c("Exceptional Drought","Extreme Drought","Severe Drought","Moderate Drought", "Abnormally Dry",
                    "Normal", "Abnormally Wet", "Moderately Wet", "Severely Wet", "Extremely Wet", "Exceptionally Wet"),
           ordered=T)



# Find cutoff values for plots
# Fig. 5C caption says probability of finding tree cover > 60% is low/high, etc. Is 60% meaningful? What is meaningful here?
# Perhaps median grainc in spei=normal in 2020s? 75 %ile, i.e., "better than average".

speibm$graincMg <- speibm$grainc/1000  # doing this makes the % frequency more like 10% rather than 0.01%

norm30corn <- speibm[speibm$year>2022 & speibm$year<2030 & 
                       speibm$spcat12mo =="Normal" &
                       speibm$crop_name == "corn, grain",]

norm30grainc_corn75p <- quantile(norm30corn$graincMg, prob=0.75)
norm30grainc_corn90p <- quantile(norm30corn$graincMg, prob=0.90)
norm30grainc_cornmed <- median(norm30corn$graincMg)

norm30soy<- speibm[speibm$year>2022 & speibm$year<2030 & 
                       speibm$spcat12mo =="Normal" &
                       speibm$crop_name == "soybean",]

norm30grainc_soy75p <- quantile(norm30soy$graincMg, prob=0.75)
norm30grainc_soy90p <- quantile(norm30soy$graincMg, prob=0.90)
norm30grainc_soymed <- median(norm30soy$graincMg)





# make plots like Fig. 5c

# A histogram is frequency. but a relative frequency AKA density gives % probability rather than just count.
# if N is large enough, then relative freq histogram starts to resemble the populations distribution,
# the area under the histogram =1, and related to probability.
# probability is the fraction of the area under the frequency distribution curve / density curve.

windows(xpinch=200, ypinch=200, width=5, height=5)


# set facet background color per spcat
pal11 <- c("#a50026", "#dd3d2d","#f67e4b","#FDb366","#FEDa8B", "#eaeccc", "#C2e4ef", "#98cae1","#6ea6cd", "#4a7bb7", "#364b9a")
strip <- strip_themed(background_y = elem_list_rect(fill = pal11),
                      text_y = elem_list_text(color=c("white", rep("black", 9), "white")))

ggplot(data=speibm[speibm$crop_name=="corn, grain" & 
                     !is.na(speibm$spcat12mo) &  # for now just compare extremes: CT+NC and NT+CC
                     speibm$management_name %in% c("ct-nc-cn", "ct-nc-sn","nt-cc-cn", "nt-cc-sn"),],
       aes(x=graincMg)) +
  # geom_histogram() + # histogram (counts)
  geom_density(aes(y=after_stat(density), # sets the y-axis units
                   group=interaction(spcat12mo, cc, till)),
               trim=T) +   # make sure calculating the density per facet, not whole dataset at once
  # labs(x="Corn grain biomass C (kg/ha)", y="Frequency (count) 2022 to 2072") +
  labs(x="Corn grain biomass C (Mg/ha)", y="Frequency (%) 2022 to 2072") +
  scale_y_continuous(labels=scales::percent_format()) +
  geom_vline(xintercept=norm30grainc_cornmed, color="#BBcc33") +  # 4000 chosen arbitrarily - is there something more meaningful?
  geom_vline(xintercept=norm30grainc_corn90p, color="#44bb99") + 
  facet_grid2(rows=vars(spcat12mo), cols=vars(cc, till), scales="free_y", 
             labeller = as_labeller(
               # c("Exceptional Drought"= "D4","Extreme Drought" = "D3","Severe Drought" = "D2","Moderate Drought" = "D1", "Abnormally Dry" = "D0",
               #   "Normal" = "Normal", "Abnormally Wet" = "W0", "Moderately Wet" = "W1", "Severely Wet" = "W2", "Extremely Wet" = "W3", "Exceptionally Wet" = "W4")))
              c("Exceptional Drought"= "Exceptional\nDrought","Extreme Drought" = "Extreme\nDrought",
                "Severe Drought" = "Severe\nDrought","Moderate Drought" = "Moderate\nDrought", 
                "Abnormally Dry" = "Abnormally\nDry", "Normal" = "Normal", 
                "Abnormally Wet" = "Abnormally\nWet", "Moderately Wet" = "Moderately\nWet", 
                "Severely Wet" = "Severely\nWet", "Extremely Wet" = "Extremely\nWet", 
                "Exceptionally Wet" = "Exceptionally\nWet",
                "CC" = "With cover crops", "NC"= "Without cover crops",
                "NT" = "No-till", "CT" = "Conventional till")),
              strip = strip_themed(background_y = elem_list_rect(fill = pal11),  # cool stuff from ggh4x package!
                                   text_y = elem_list_text(color=c("white", rep("black", 9), "white")))) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),  #,
    panel.background = element_rect(fill = 'gray96')
  )	 

# ggsave("plots/resilience/IL_corngrainc_bydrought_hist.png", width=4, height=10, dpi=300)
ggsave("plots/resilience/IL_corngrainc_bydrought_density.png", width=4, height=10, dpi=300)

# why so many NA spcat12mo values?
# check that they are all in 2022

# ch <- speibm[is.na(speibm$spcat12mo),]
# unique(ch$year) # just 2022
# unique(ch$month) # Jan. - Nov. as expected
# sum(is.na(ch[ch$year==2022,"fit.12mo"]))  # 19008
# sum(is.na(ch[!ch$year==2022,"fit.12mo"]))  # 0
