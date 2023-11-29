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
# install.packages("ggh4x") 
library(ggh4x) # for strip_themed() and facet_grid2() in ggplot
# install.packages("AICcmodavg")
library(AICcmodavg) # for aictab()
library(multcompView) # for tukey HSD letters





############################### get spei data ready to merge with other data

# climate - SPEI
spei <- read.csv("data/climate data not large/speiIL.csv")

# let's focus on rcp 6.0 for now
spei <- spei[spei$rcp=="rcp60",]

# biomass is once per year, spei is monthly, could get annual (or water year) average
# spei or look at July SPEI (or other month or average of months). Let's start
# with july Spei

spei <- spei %>%
  separate_wider_delim(dat, delim=" ", 
                       names=c("month", "year"),  # split the ID into two columns
                       cols_remove=F)

spei$season <- ifelse(spei$month %in% c("Dec", "Jan", "Feb"), "Winter",
                      ifelse(spei$month %in% c("Mar", "Apr", "May"), "Spring",
                             ifelse(spei$month %in% c("Jun", "Jul", "Aug"), "Summer", "Fall")))

spei2 <- group_by(spei, season, site, year) %>%
  summarize(fit.12mo.ssnavg = mean(fit.12mo))

spei <- left_join(spei, spei2)
rm(spei2)

spei$year <- as.numeric(spei$year)

# need to add ID variable for SPEI category for each row of data
# again classifications from: https://droughtmonitor.unl.edu/About/AbouttheData/DroughtClassification.aspx


# spei$spcat1mo <- ifelse(spei$fit.1mo >= -0.4999999 & spei$fit.1mo <=0.49999999, "Normal",
#                           # drought conditions
#                           ifelse(spei$fit.1mo >= -0.7999999 & spei$fit.1mo <= -0.50, "Abnormally Dry",  
#                                  ifelse(spei$fit.1mo >= -1.2999999 & spei$fit.1mo <= -0.80, "Moderate Drought",
#                                         ifelse(spei$fit.1mo >= -1.59999999 & spei$fit.1mo <= -1.30, "Severe Drought",
#                                                ifelse(spei$fit.1mo >= -1.9999999 & spei$fit.1mo <= -1.60, "Extreme Drought",
#                                                       ifelse(spei$fit.1mo <= -2.0, "Exceptional Drought",
#                                                              # wet conditions                           
#                                                              ifelse(spei$fit.1mo <= 0.7999999 & spei$fit.1mo >= 0.50, "Abnormally Wet",    
#                                                                     ifelse(spei$fit.1mo <= 1.2999999 & spei$fit.1mo >= 0.80, "Moderately Wet",
#                                                                            ifelse(spei$fit.1mo <= 1.5999999 & spei$fit.1mo >= 1.30, "Severely Wet",
#                                                                                   ifelse(spei$fit.1mo <= 1.999999 & spei$fit.1mo >= 1.60, "Extremely Wet",
#                                                                                          ifelse(spei$fit.1mo >= 2.0, "Exceptionally Wet", "X")))))))))))
# 
# # Test
# unique(spei$spcat1mo)

spei$spcat12mo <- ifelse(spei$fit.12mo >= -0.4999999 & spei$fit.12mo <=0.49999999, "Normal",
                           # drought conditions
                           ifelse(spei$fit.12mo >= -0.7999999 & spei$fit.12mo <= -0.50, "Abnormally Dry",  
                                  ifelse(spei$fit.12mo >= -1.2999999 & spei$fit.12mo <= -0.80, "Moderate Drought",
                                         ifelse(spei$fit.12mo >= -1.59999999 & spei$fit.12mo <= -1.30, "Severe Drought",
                                                ifelse(spei$fit.12mo >= -1.9999999 & spei$fit.12mo <= -1.60, "Extreme Drought",
                                                       ifelse(spei$fit.12mo <= -2.0, "Exceptional Drought",
                                                              # wet conditions                           
                                                              ifelse(spei$fit.12mo <= 0.7999999 & spei$fit.12mo >= 0.50, "Abnormally Wet",    
                                                                     ifelse(spei$fit.12mo <= 1.2999999 & spei$fit.12mo >= 0.80, "Moderately Wet",
                                                                            ifelse(spei$fit.12mo <= 1.5999999 & spei$fit.12mo >= 1.30, "Severely Wet",
                                                                                   ifelse(spei$fit.12mo <= 1.999999 & spei$fit.12mo >= 1.60, "Extremely Wet",
                                                                                          ifelse(spei$fit.12mo >= 2.0, "Exceptionally Wet", "X")))))))))))

# Test
unique(spei$spcat12mo)
# NA values expected because first 11 months fit.12mo = NA. 

spei$spcat12mo <- factor(spei$spcat12mo,
                           levels=c("Exceptional Drought","Extreme Drought","Severe Drought","Moderate Drought", "Abnormally Dry",
                                    "Normal", "Abnormally Wet", "Moderately Wet", "Severely Wet", "Extremely Wet", "Exceptionally Wet"),
                           ordered=T)



spei$spcat12mo2 <- ifelse(spei$fit.12mo >= -0.4999999 & spei$fit.12mo <=0.49999999, "Normal",
                            # drought conditions
                            ifelse(spei$fit.12mo >= -1.2999999 & spei$fit.12mo <= -0.50, "Abnorm Dry/Mod Drt",  
                                   # ifelse(spei$fit.12mo >= -1.2999999 & spei$fit.12mo <= -0.80, "Moderate Drought",
                                   ifelse(spei$fit.12mo >= -1.9999999 & spei$fit.12mo <= -1.30, "Severe/Extreme Drought",
                                          # ifelse(spei$fit.12mo >= -1.9999999 & spei$fit.12mo <= -1.60, "Extreme Drought",
                                          ifelse(spei$fit.12mo <= -2.0, "Exceptional Drought",
                                                 # wet conditions                           
                                                 ifelse(spei$fit.12mo <= 1.2999999 & spei$fit.12mo >= 0.50, "Abnorm/Mod Wet",    
                                                        # ifelse(spei$fit.12mo <= 1.2999999 & spei$fit.12mo >= 0.80, "Moderately Wet",
                                                        ifelse(spei$fit.12mo <= 1.999999 & spei$fit.12mo >= 1.30, "Severe/Extreme Wet",
                                                               # ifelse(spei$fit.12mo <= 1.999999 & spei$fit.12mo >= 1.60, "Extremely Wet",
                                                               ifelse(spei$fit.12mo >= 2.0, "Exceptionally Wet", "X")))))))

# Test
unique(spei$spcat12mo2)
# NA values expected because first 11 months fit.12mo = NA. 

spei$spcat12mo2 <- factor(spei$spcat12mo2,
                            levels=c("Exceptional Drought","Severe/Extreme Drought","Abnorm Dry/Mod Drt", 
                                     "Normal", "Abnorm/Mod Wet", "Severe/Extreme Wet", "Exceptionally Wet"),
                            ordered=T)

spei$spcat12ssn2 <- ifelse(spei$fit.12mo.ssnavg >= -0.4999999 & spei$fit.12mo.ssnavg <=0.49999999, "Normal",
                          # drought conditions
                          ifelse(spei$fit.12mo.ssnavg >= -1.2999999 & spei$fit.12mo.ssnavg <= -0.50, "Abnorm Dry/Mod Drt",  
                                 # ifelse(spei$fit.12mo.ssnavg >= -1.2999999 & spei$fit.12mo.ssnavg <= -0.80, "Moderate Drought",
                                 ifelse(spei$fit.12mo.ssnavg >= -1.9999999 & spei$fit.12mo.ssnavg <= -1.30, "Severe/Extreme Drought",
                                        # ifelse(spei$fit.12mo.ssnavg >= -1.9999999 & spei$fit.12mo.ssnavg <= -1.60, "Extreme Drought",
                                        ifelse(spei$fit.12mo.ssnavg <= -2.0, "Exceptional Drought",
                                               # wet conditions                           
                                               ifelse(spei$fit.12mo.ssnavg <= 1.2999999 & spei$fit.12mo.ssnavg >= 0.50, "Abnorm/Mod Wet",    
                                                      # ifelse(spei$fit.12mo.ssnavg <= 1.2999999 & spei$fit.12mo.ssnavg >= 0.80, "Moderately Wet",
                                                      ifelse(spei$fit.12mo.ssnavg <= 1.999999 & spei$fit.12mo.ssnavg >= 1.30, "Severe/Extreme Wet",
                                                             # ifelse(spei$fit.12mo.ssnavg <= 1.999999 & spei$fit.12mo.ssnavg >= 1.60, "Extremely Wet",
                                                             ifelse(spei$fit.12mo.ssnavg >= 2.0, "Exceptionally Wet", "X")))))))

# Test
unique(spei$spcat12ssn2)
# NA values expected because first 11 months fit.12mo = NA. 

spei$spcat12ssn2 <- factor(spei$spcat12ssn2,
                          levels=c("Exceptional Drought","Severe/Extreme Drought","Abnorm Dry/Mod Drt", 
                                   "Normal", "Abnorm/Mod Wet", "Severe/Extreme Wet", "Exceptionally Wet"),
                          ordered=T)



######################################   pull in biomass data
bmil <- read.csv("data/biomass/biomass_IL.csv")
colnames(bmil)[9] <- "grainc"
bmil <- bmil[bmil$climate_scenario=="rcp60",]
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



##################################### merge biomass and spei data
speibm <- inner_join(spei[,-2], bmilgr, join_by(site==site_name, year==Year), 
                    relationship="many-to-many")

# # grainc z-score
# speibm$grainz <- (speibm$grainc - mean(speibm$grainc))/sd(speibm$grainc)
# 
windows(xpinch=200, ypinch=200, width=5, height=5)
# 
# ggplot(data=speibm[speibm$crop_name %in% c("corn, grain") & speibm$month =="Jul",], 
#        aes(x=fit.12mo, y=grainc)) +  #  x=abs(fit.1mo), x=fit.12mo, x=grainz
#   geom_point(size=0.1, alpha=0.5) +
#   scale_x_continuous(breaks=c(-2.0, -1.6, -1.3, -0.8, 1.30, 1.60, 2.0, 3.0),
#                      labels=c(-2.0, -1.6, -1.3, -0.8, 1.30, 1.60, 2.0, 3.0)) +
#   facet_grid(cols=vars(ns, till), rows=vars(cc, nfert))

# looks like corn grain c less resilient to extreme weather when: fall N, in the South. nothing obviously apparent between tillage or cover treatments

ggplot(data=speibm[speibm$crop_name %in% c("corn, grain"),], 
       aes(x=fit.12mo, y=grainc)) +  #  x=abs(fit.1mo), x=fit.12mo
  geom_point(size=0.1, alpha=0.5) +
  facet_grid(rows=vars(cc, nfert), cols=vars(month))



# why so many NA spcat12mo values?
# check that they are all in 2022

# ch <- speibm[is.na(speibm$spcat12mo),]
# unique(ch$year) # just 2022
# unique(ch$month) # Jan. - Nov. as expected
# sum(is.na(ch[ch$year==2022,"fit.12mo"]))  # 19008
# sum(is.na(ch[!ch$year==2022,"fit.12mo"]))  # 0

# following fig. 5c in Scheffer et al 2015 - inferring probabilistic resilience from massive data.
# Not sure we have "massive" data but probably enough to try this approach.
# I'm thinking histograms of crop yields at different drought and wetness categories
# across our "treatments".

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
pal7 <- c("#a50026", "#f67e4b","#FEDa8B", "#eaeccc", "#C2e4ef","#6ea6cd",  "#364b9a")


ggplot(data=speibm[speibm$crop_name=="corn, grain" & speibm$season == "Summer" &
                     !is.na(speibm$spcat12ssn2) &  # for now just compare extremes: CT+NC and NT+CC
                     speibm$management_name %in% c("ct-nc-cn", "ct-nc-sn","nt-cc-cn", "nt-cc-sn"),],
       aes(x=graincMg)) +
  # geom_histogram() + # histogram (counts)
  geom_density(aes(y=after_stat(density), # sets the y-axis units
                   group=interaction(spcat12ssn2, cc, till)),
               trim=T) +   # make sure calculating the density per facet, not whole dataset at once
  # labs(x="Corn grain biomass C (kg/ha)", y="Frequency (count) 2022 to 2072") +
  labs(x="Corn grain biomass C (Mg/ha)", y="Frequency (%) 2022 to 2072") +
  scale_y_continuous(labels=scales::percent_format()) +
  geom_vline(xintercept=norm30grainc_cornmed, color="#BBcc33") +  # 4000 chosen arbitrarily - is there something more meaningful?
  # geom_vline(xintercept=norm30grainc_corn90p, color="#44bb99") + 
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
ggsave("plots/resilience/IL_corngrainc_bydrought_density Summer.png", width=4, height=10, dpi=300)




# Quantify areas under curves by site, SPEI, and treatments

lsd <- speibm[speibm$crop_name %in% c("corn, grain", "soybean") & speibm$season =="Summer",] %>%
  group_split(site, crop_name, spcat12ssn2, till, cc, nfert) # by site, so that we can compare means and variability of treatments across sites

# ecdf = empirical cumulative distribution function
lecdf <- lapply(lsd, function(i){
  d_fun <- ecdf(i$graincMg)
  cropmed <- ifelse(i$crop_name[1] == "corn, grain", norm30grainc_cornmed, norm30grainc_soymed)
  crop90p <- ifelse(i$crop_name[1] == "corn, grain", norm30grainc_corn90p, norm30grainc_soy90p)
  grmed <- 1 - d_fun(cropmed) # 1- gives the area under the curve to the RIGHT of the value
  gr90p <- 1 - d_fun(crop90p)
  out <- i[1, c("site", "crop_name", "till", "cc", "nfert", "spcat12ssn2")]
  out$grmed <- grmed
  out$gr90p <- gr90p
  out
})

lecdf <- lecdf %>%
  bind_rows(lecdf)

rm(lsd)

# are the areas under the curves different?


################## models: are the areas under the curves different?

se <- function(x) sd(x) / sqrt(length(x))
cv <- function(x) sd(x) / mean(x)


# lecdf$spcat12mo <- factor(lecdf$spcat12mo,
#                            levels=c("Exceptional Drought","Extreme Drought","Severe Drought","Moderate Drought", "Abnormally Dry",
#                                     "Normal", "Abnormally Wet", "Moderately Wet", "Severely Wet", "Extremely Wet", "Exceptionally Wet"),
#                            ordered=F)

# lecdf$spcat12mo2 <- factor(lecdf$spcat12mo2,
#                             levels=c("Exceptional Drought","Severe/Extreme Drought","Abnorm Dry/Mod Drt", 
#                                      "Normal", "Abnorm/Mod Wet", "Severe/Extreme Wet", "Exceptionally Wet"),
#                             ordered=T)

lecdf$spcat12ssn2 <- factor(lecdf$spcat12ssn2,
                           levels=c("Exceptional Drought","Severe/Extreme Drought","Abnorm Dry/Mod Drt", 
                                    "Normal", "Abnorm/Mod Wet", "Severe/Extreme Wet", "Exceptionally Wet"),
                           ordered=T)

##### letters for nt-cc, ct-nc x spei plot

lecdf_simp <-  filter(lecdf, crop_name == "corn, grain") %>%
  filter((cc=="CC" & till=="NT") | (cc == "NC" & till =="CT") )

cornaov_simp <- aov(grmed~cc*nfert*spcat12ssn2,   # since we only have CC+NT vs NC+CT we've made till and cc redundant
                    data=lecdf_simp)

summary(cornaov_simp)
#                         Df Sum Sq Mean Sq F value   Pr(>F)    
# cc                      1   0.24    0.24   5.267   0.0218 *  
# nfert                   2  28.88   14.44 320.501  < 2e-16 ***
# spcat12ssn2             6 216.87   36.15 802.311  < 2e-16 ***
# cc:nfert                2   9.31    4.66 103.346  < 2e-16 ***
# cc:spcat12ssn2          6   3.73    0.62  13.781 1.81e-15 ***
# nfert:spcat12ssn2      12  17.33    1.44  32.049  < 2e-16 ***
# cc:nfert:spcat12ssn2   12   2.58    0.22   4.779 8.54e-08 ***
# Residuals            2526 113.80    0.05                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 384 observations deleted due to missingness

Tukout_simp <- TukeyHSD(cornaov_simp)
cld_simp <- multcompLetters4(cornaov_simp, Tukout_simp)
# table with letters and 3rd quantile
lecdf_sum<- group_by(lecdf, cc, nfert, spcat12ssn2) %>%  # filter(lecdf, till %in% c("CT", "NT")) %>%
  drop_na(spcat12ssn2) %>%
  summarize(mean=mean(grmed), 
            se=se(grmed)) %>%
  arrange(desc(mean))

cld_lecdf <- as.data.frame.list(cld_simp$`cc:nfert:spcat12ssn2`)
lecdf_sum$cld <- cld_lecdf$Letters
# Add till back in, remember redundant with CC
lecdf_sum$till <- ifelse(lecdf_sum$cc == "CC", "NT", "CT")

# 
# cornsoyaov_add <- aov(grmed~crop_name+ till + cc + nfert + spcat12mo, data=lecdf)
# summary(cornsoyaov_add)
# #                   Df Sum Sq Mean Sq F value   Pr(>F)    
# #   crop_name       1   88.0   88.00 2828.32  < 2e-16 ***
# #   till            2    4.1    2.04   65.43  < 2e-16 ***
# #   cc              1    1.5    1.50   48.23 3.88e-12 ***
# #   nfert           2   49.6   24.81  797.26  < 2e-16 ***
# #   spcat12mo      10  629.4   62.94 2022.76  < 2e-16 ***
# #   Residuals   25327  788.0    0.03                     
# # ---
# #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# # 2304 observations deleted due to missingness
# Tukout <- TukeyHSD(cornsoyaov_add)
# cornsoyaov <- aov(grmed~crop_name+ till*cc*nfert*spcat12mo, data=lecdf)
# summary(cornsoyaov)
# cornsoyaov2 <- aov(grmed~crop_name*till*cc*nfert*spcat12mo, data=lecdf)
# summary(cornsoyaov2)
# 
# 
# 
# # Same models as linear models so we can get R2 and do model comparison with AIC etc.
# 
# cornsoylm_add <- lm(grmed~crop_name + till + cc + nfert + spcat12mo, data=lecdf)
# summary(cornsoylm_add) # Adjusted R-squared:  0.4947 
# # broom::glance(cornsoylm_add) # AIC-16004, BIC-15857
# cornsoylm_add1 <- lm(grmed~crop_name + till*cc*nfert*spcat12mo, data=lecdf)
# summary(cornsoylm_add1)  # adjusted R2 = 0.5119
# # broom::glance(cornsoylm_add1) # AIC-16702, BIC-15074
# cornsoylm_add2 <- lm(grmed~crop_name + nfert + till*cc*spcat12mo, data=lecdf)
# summary(cornsoylm_add2) # Adjusted R-squared:  0.4962
# # broom::glance(cornsoylm_add2) # AIC-16028, BIC-15458
# cornsoylm_add3 <- lm(grmed~crop_name + till + nfert*cc*spcat12mo, data=lecdf)  ######## best
# summary(cornsoylm_add3) # Adjusted R-squared:  0.5099 
# # broom::glance(cornsoylm_add3) # AIC-16723, BIC-16153
# cornsoylm_add4 <- lm(grmed~crop_name + cc + nfert*till*spcat12mo, data=lecdf)
# summary(cornsoylm_add4) # Adjusted R-squared:  0.5058
# # broom::glance(cornsoylm_add4) # AIC-16481, BIC-15651
# cornsoylm_add5 <- lm(grmed~crop_name + till + nfert + cc*spcat12mo, data=lecdf)
# summary(cornsoylm_add5) # Adjusted R-squared:  0.4949
# # broom::glance(cornsoylm_add5) # AIC-16002, BIC-15775
# cornsoylm_add6 <- lm(grmed~crop_name + till + cc + nfert*spcat12mo, data=lecdf)
# summary(cornsoylm_add6) # Adjusted R-squared:  0.5034
# # broom::glance(cornsoylm_add6) # AIC-16424, BIC-16115
# cornsoylm_add7 <- lm(grmed~till*cc*spcat12mo, data=lecdf)
# summary(cornsoylm_add7) # Adjusted R-squared:  0.4079 
# # broom::glance(cornsoylm_add7) # AIC-11936, BIC-11390
# cornsoylm_add8 <- lm(grmed~crop_name + till + cc*spcat12mo, data=lecdf)
# summary(cornsoylm_add8) # Adjusted R-squared:  0.4631  
# # broom::glance(cornsoylm_add8) # AIC-14458, BIC-14246
# 
# 
# 
# models <- list(cornsoylm_add, cornsoylm_add1, cornsoylm_add2, cornsoylm_add3, 
#                cornsoylm_add4, cornsoylm_add5, cornsoylm_add6, cornsoylm_add7,
#                cornsoylm_add8)
# model.names <- c("cornsoylm_add", "cornsoylm_add1", "cornsoylm_add2", "cornsoylm_add3", 
#                  "cornsoylm_add4", "cornsoylm_add5", "cornsoylm_add6", "cornsoylm_add7",
#                  "cornsoylm_add8")
# aictab(cand.set = models, modnames = model.names)
# # Model selection based on AICc:
# #   
# # K      AICc Delta_AICc AICcWt Cum.Wt      LL
# # cornsoylm_add3  70 -16722.48       0.00      1      1 8431.44
# # cornsoylm_add1 200 -16698.40      24.08      0      1 8550.80
# # cornsoylm_add4 102 -16480.17     242.31      0      1 8342.50
# # cornsoylm_add6  38 -16423.77     298.71      0      1 8249.95
# # cornsoylm_add2  70 -16027.84     694.64      0      1 8084.12
# # cornsoylm_add   18 -16003.73     718.75      0      1 8019.88
# # cornsoylm_add5  28 -16002.37     720.11      0      1 8029.22
# # cornsoylm_add8  26 -14457.89    2264.59      0      1 7254.97
# # cornsoylm_add7  67 -11935.53    4786.95      0      1 6034.95
# 
# # equivalent AOV
# cornsoyaov_add3 <- aov(grmed~crop_name + till + nfert*cc*spcat12mo, data=lecdf)
# Tukout_add3 <- TukeyHSD(cornsoyaov_add3)
# 
# # compact letter display
# cld_add3 <- multcompLetters4(cornsoyaov_add3, Tukout_add3)
# cld_aov <- multcompLetters4(cornsoyaov_add, Tukout)
# cld_simp <- multcompLetters4(cornaov_simp, Tukout_simp)
# # table with letters and 3rd quantile
# lecdf_sum<- group_by(lecdf, cc, nfert, spcat12mo2) %>%
#  drop_na(spcat12mo2) %>%
#   summarize(mean=mean(grmed), 
#             se=se(grmed)) %>%
#   arrange(desc(mean))
# 
# cld_lecdf <- as.data.frame.list(cld_simp$`cc:nfert:spcat12mo`)
# lecdf_sum$cld <- cld_lecdf$Letters

####################################  cc x nfert x spei plot
ggplot(data=lecdf_sum, aes(x=nfert, y=mean)) +
  geom_bar(stat="identity", fill="gray60") +
  geom_errorbar(aes(x=nfert, ymin=mean-se, ymax=mean+se), width=0.2, size=0.8, color="gray20") +
  facet_grid2(rows=vars(factor(cc, levels=c("NC", "CC")), factor(till, levels=c("CT", "NT"))),
             cols=vars(spcat12ssn2),
             labeller = as_labeller(
               # c("Exceptional Drought"= "D4","Extreme Drought" = "D3","Severe Drought" = "D2","Moderate Drought" = "D1", "Abnormally Dry" = "D0",
               #   "Normal" = "Normal", "Abnormally Wet" = "W0", "Moderately Wet" = "W1", "Severely Wet" = "W2", "Extremely Wet" = "W3", "Exceptionally Wet" = "W4")))
               c("Exceptional Drought" = "Exceptional\nDrought",
                 "Severe/Extreme Drought" = "Severe/Extreme\nDrought",
                 "Abnorm Dry/Mod Drt" = "Abnormally Dry/\nModerate Drought", 
                 "Normal" = "Normal", 
                 "Abnorm/Mod Wet" = "Abnormally Wet/\nModerately Wet", 
                 "Severe/Extreme Wet" = "Severely/\nExtremely Wet", 
                 "Exceptionally Wet" = "Exceptionally\nWet",
                 "NC"= "Without cover crops","CC" = "With cover crops",
                 "CT" = "Conventional Till", "NT" = "No Till")), 
             strip = strip_themed(background_x = elem_list_rect(fill = pal7), #,  # cool stuff from ggh4x package!
                                  text_x = elem_list_text(color= c("white", rep("black", 5), "white")))) +
  ylab("Mean probability corn grain biomass-C\n> median in Normal year in 2020s") +
  xlab("N management") +
  scale_x_discrete(breaks=c("Fall N", "High N", "Recommended N"),
                   labels = c("Fall\nN", "High\nN", "Recomm.\nN")) +
  geom_text(aes(x=nfert, y=mean +0.15, label=cld)) + 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-10, hjust=0.5, vjust=0.5, size=11),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))

ggsave("plots/resilience/IL_corngr biomass prob summer SPEI nc-ct v cc-nt withletters.png", width=13, height=5, dpi=300) 
















################################## resilience: sediment yield
rm(bmil, bmilgr, speibm)
sed <- read.csv("data/water, nitrate, sediments/wdatyr.csv")


sed$crop <- ifelse(sed$crop_system_name=="corn-soy" & sed$year%%2 ==0, "soy",   # %%2 returns the remainder when divided by 2. if no remainder, then its an even number.
                      ifelse(sed$crop_system_name=="corn-soy" & !sed$year%%2 ==0, "corn",
                             ifelse(sed$crop_system_name=="soy-corn" & sed$year%%2 ==0, "corn",
                                    ifelse(sed$crop_system_name=="soy-corn" & !sed$year%%2 ==0, "soy", "X"))))
unique(sed$climate_scenario)


speised <- inner_join(spei[,-2], sed[-4], join_by(site==site_name, year==year), 
                      relationship="many-to-many")

windows(xpinch=200, ypinch=200, width=5, height=5)
ggplot(data=speised, 
       aes(x=fit.12mo, y=sed.yr)) +  #  x=abs(fit.1mo), x=fit.12mo
  geom_point(size=0.1, alpha=0.5) +
  facet_grid(cols=vars(factor(month)), rows=vars(factor(cc), factor(till)))



norm20 <- speised[speised$year>2022 & speised$year<2030 & 
                       speised$spcat12ssn2 =="Normal",]

norm20med <- median(norm20$sed.yr)

windows(xpinch=200, ypinch=200, width=5, height=5)
ggplot(data=speised[speised$season == "Summer" &
                     !is.na(speised$spcat12ssn2) &  # for now just compare extremes: CT+NC and NT+CC
                     speised$management %in% c("ct-nc-cn", "ct-nc-sn","nt-cc-cn", "nt-cc-sn"),],
       aes(x=sed.yr)) +
  # geom_histogram() + # histogram (counts)
  geom_density(aes(y=after_stat(density), # sets the y-axis units
                   group=interaction(spcat12ssn2, cc, till)),
               trim=T) +   # make sure calculating the density per facet, not whole dataset at once
  # labs(x="Corn grain biomass C (kg/ha)", y="Frequency (count) 2022 to 2072") +
  labs(x="sediment yield kg/ha", y="Frequency (%) 2022 to 2072") +
  scale_y_continuous(labels=scales::percent_format()) +
  geom_vline(xintercept=norm20med, color="#BBcc33") +  # 4000 chosen arbitrarily - is there something more meaningful?
  # geom_vline(xintercept=norm30grainc_corn90p, color="#44bb99") + 
  facet_grid2(rows=vars(spcat12ssn2), cols=vars(cc, till), scales="free_y", 
              labeller = as_labeller(
                # c("Exceptional Drought"= "D4","Extreme Drought" = "D3","Severe Drought" = "D2","Moderate Drought" = "D1", "Abnormally Dry" = "D0",
                #   "Normal" = "Normal", "Abnormally Wet" = "W0", "Moderately Wet" = "W1", "Severely Wet" = "W2", "Extremely Wet" = "W3", "Exceptionally Wet" = "W4")))
                c("Exceptional Drought" = "Exceptional\nDrought",
                  "Severe/Extreme Drought" = "Severe/Extreme\nDrought",
                  "Abnorm Dry/Mod Drt" = "Abnormally Dry/\nModerate Drought", 
                  "Normal" = "Normal", 
                  "Abnorm/Mod Wet" = "Abnormally Wet/\nModerately Wet", 
                  "Severe/Extreme Wet" = "Severely/\nExtremely Wet", 
                  "Exceptionally Wet" = "Exceptionally\nWet",
                  "NC"= "Without cover crops","CC" = "With cover crops",
                  "CT" = "Conventional Till", "NT" = "No Till")), 
              strip = strip_themed(background_y = elem_list_rect(fill = pal11),  # cool stuff from ggh4x package!
                                   text_y = elem_list_text(color=c("white", rep("black", 9), "white")))) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),  #,
    panel.background = element_rect(fill = 'gray96')
  )	 

# ggsave("plots/resilience/IL_corngrainc_bydrought_hist.png", width=4, height=10, dpi=300)
ggsave("plots/resilience/IL_sed_bydrought_density Summer.png", width=4, height=10, dpi=300)




# Quantify areas under curves by site, SPEI, and treatments

lsd <- filter(speised, season== "Summer") %>% 
  group_split(site, spcat12ssn2, till, cc, nfert) # by site, so that we can compare means and variability of treatments across sites

# ecdf = empirical cumulative distribution function
lecdf <- lapply(lsd, function(i){
  d_fun <- ecdf(i$sed.yr)
  sedmed <- 1 - d_fun(norm20med) # 1- gives the area under the curve to the RIGHT of the value
  out <- i[1, c("site", "till", "cc", "nfert", "spcat12ssn2")]
  out$sedmed <- sedmed
  out
})

lecdf <- lecdf %>%
  bind_rows(lecdf)

rm(lsd)

# are the areas under the curves different?


################## models: are the areas under the curves different?


# lecdf$spcat12mo <- factor(lecdf$spcat12mo,
#                            levels=c("Exceptional Drought","Extreme Drought","Severe Drought","Moderate Drought", "Abnormally Dry",
#                                     "Normal", "Abnormally Wet", "Moderately Wet", "Severely Wet", "Extremely Wet", "Exceptionally Wet"),
#                            ordered=F)

# lecdf$spcat12mo2 <- factor(lecdf$spcat12mo2,
#                             levels=c("Exceptional Drought","Severe/Extreme Drought","Abnorm Dry/Mod Drt", 
#                                      "Normal", "Abnorm/Mod Wet", "Severe/Extreme Wet", "Exceptionally Wet"),
#                             ordered=T)

lecdf$spcat12ssn2 <- factor(lecdf$spcat12ssn2,
                            levels=c("Exceptional Drought","Severe/Extreme Drought","Abnorm Dry/Mod Drt", 
                                     "Normal", "Abnorm/Mod Wet", "Severe/Extreme Wet", "Exceptionally Wet"),
                            ordered=T)

##### letters for nt-cc, ct-nc x spei plot

lecdf_simp <-  filter(lecdf, (till %in% c("CT", "NT"))) 

sedaov_simp <- aov(sedmed~cc*till*spcat12ssn2,   # since we only have CC+NT vs NC+CT we've made till and cc redundant
                    data=lecdf_simp)

summary(sedaov_simp)
#                       Df Sum Sq Mean Sq  F value   Pr(>F)    
# cc                     1   34.9   34.91  435.935  < 2e-16 ***
# till                   1  208.6  208.63 2604.887  < 2e-16 ***
# spcat12ssn2            6  145.4   24.23  302.582  < 2e-16 ***
# cc:till                1    1.6    1.57   19.641 9.54e-06 ***
# cc:spcat12ssn2         6    1.1    0.18    2.300   0.0321 *  
# till:spcat12ssn2       6    4.2    0.70    8.707 1.87e-09 ***
# cc:till:spcat12ssn2    6    7.5    1.26   15.681  < 2e-16 ***
# Residuals           5108  409.1    0.08                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 768 observations deleted due to missingness

Tukout_simp <- TukeyHSD(sedaov_simp)
cld_simp <- multcompLetters4(sedaov_simp, Tukout_simp)
# table with letters and 3rd quantile
lecdf_sum<- filter(lecdf, till %in% c("CT", "NT")) %>%
  group_by(cc, till, spcat12ssn2) %>%  # 
  drop_na(spcat12ssn2) %>%
  summarize(mean=mean(sedmed), 
            se=se(sedmed)) %>%
  arrange(desc(mean))

cld_lecdf <- as.data.frame.list(cld_simp$`cc:till:spcat12ssn2`)
lecdf_sum$cld <- cld_lecdf$Letters


# 
####################################  sed ~ cc*till*spei plot
windows(xpinch=200, ypinch=200, width=5, height=5)

lecdf_sum$yr <- rep("X", nrow(lecdf_sum))

ggplot(data=lecdf_sum, aes(x=yr, y=mean)) +  # yr is a dummy var
  geom_bar(stat="identity", fill="burlywood3", width=0.7) +
  geom_errorbar(aes(x=yr, ymin=mean-se, ymax=mean+se), width=0.2, size=0.8, color="sienna4") +
  facet_grid2(rows=vars(factor(till, levels=c("CT", "NT")), factor(cc, levels=c("NC", "CC"))),
              cols=vars(spcat12ssn2),
              labeller = as_labeller(
                # c("Exceptional Drought"= "D4","Extreme Drought" = "D3","Severe Drought" = "D2","Moderate Drought" = "D1", "Abnormally Dry" = "D0",
                #   "Normal" = "Normal", "Abnormally Wet" = "W0", "Moderately Wet" = "W1", "Severely Wet" = "W2", "Extremely Wet" = "W3", "Exceptionally Wet" = "W4")))
                c("Exceptional Drought" = "Except.\nDrought",
                  "Severe/Extreme Drought" = "Severe/\nExtreme\nDrought",
                  "Abnorm Dry/Mod Drt" = "Abnormally \nDry/Mod.\nDrought", 
                  "Normal" = "Normal", 
                  "Abnorm/Mod Wet" = "Abnormally/\nMod. Wet", 
                  "Severe/Extreme Wet" = "Severe/\nExtreme\nWet", 
                  "Exceptionally Wet" = "Except.\nWet",
                  "NC"= "Without cover crops","CC" = "With cover crops",
                  "CT" = "Conventional Till", "NT" = "No Till")), 
              strip = strip_themed(background_x = elem_list_rect(fill = pal7), #,  # cool stuff from ggh4x package!
                                   text_x = elem_list_text(color= c("white", rep("black", 5), "white")))) +
  ylab("Mean probability sediment yield\n> median in Normal year in 2020s") +
  xlab("Summer mean SPEI") +
  geom_text(aes(x=yr, y=mean +0.15, label=cld)) + 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))

ggsave("plots/resilience/IL_sed prob summer SPEI ccxtill withletters.png", width=9, height=9, dpi=300) 






















# assumptions - following https://statsandr.com/blog/anova-in-r

# Independence - the observations (grain.c.kgc.ha. observations) are independent, one does
# not depend on the other, they're not dependent on some other variable like, 
# from one individual came multiple observations.

# Normality - not required with large enough sample size >30.  But we can test anyway. 
# the residuals (observed - mean for that group) should be normally distributed.
qqnorm(corndf$grmed)
qqline(corndf$grmed)
hist(corndf$grmed-mean(corndf$grmed))
hist(corndf$grmed)

# Equality of variances 
ggplot(data=corndf, aes(x=interaction(till, cc, spcat12mo), y=grmed)) + 
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))
# an interesting and not so pretty graph
car::leveneTest(grmed ~ cc*till*spcat12mo, data=corndf)
# Levene's Test for Homogeneity of Variance (center = median)
#          Df F value    Pr(>F)    
# group    65    15.2 < 2.2e-16 ***
#       12606                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# HOWEVER: it seems that our large sample size (>35k data points) might be affecting this test
# See this from https://www.theanalysisfactor.com/the-problem-with-tests-for-statistical-assumptions/
# It relies too much on p-values, and therefore, sample sizes. If the sample size is large, 
# Levene’s will have a smaller p-value than if the sample size is small, given the same variances.
# So it’s very likely that you’re ***overstating a problem*** with the assumption in large samples and understating 
# it in small samples. You can’t ignore the actual size difference in the variances when making this decision. 
# So sure, look at the p-value, but also look at the actual variances and how much bigger some are than others. 
# (In other words, actually look at the effect size, not just the p-value).
# The ANOVA is generally considered robust to violations of this assumption when sample sizes 
# across groups are equal. So even if Levene’s is significant, moderately different variances may not be a 
# problem in balanced data sets. Keppel (1992) suggests that a good rule of thumb is that if sample sizes are equal, 
# robustness should hold until the largest variance is more than 9 times the smallest variance.
# This robustness goes away the more unbalanced the samples are. So you need to use judgment here,
# taking into account both the imbalance and the actual difference in variances.
# *** emphasis added

# outliers
summary(corndf$grmed)
# > summary(corndat$Grain.C.kgC.ha.)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.3636  0.5625  0.5469  0.7692  1.0000 

# outliers via histograms
hist(corndf$grmed, breaks=sqrt(nrow(corndf)))

ggplot(data=corndf, aes(x=grmed)) +
  geom_histogram() +
  facet_grid(rows=vars(spcat12mo), cols=vars(till, cc))

#outliers via boxplots
boxplot(grmed~till*spcat12mo, data=corndf) # 

# outliers via z-scores (not sure if this works for data that are already between 0-1)
corndf$grmedz <- scale(corndf$grmed)
hist(corndf$grmedz)
summary(corndf$grmedz)
# V1          
# Min.   :-1.99416  
# 1st Qu.:-0.66826  
# Median : 0.05684  
# Mean   : 0.00000  
# 3rd Qu.: 0.81063  
# Max.   : 1.65206
# < -2 or >2 is considered rare
# < -3 or >3 is extremely rare
# < -3.29 or > -3.29 is used to detect outliers, where one out of 1000 observations will be outside
# we're good here

## outlier summary: boxplots and z-score suggest no extreme outliers. No justification for removing.

# re-run ANOVA - are we accounting for the 0-1 distribution properly?

########### left off here


cornaov_full <- aov(grmed~till*cc*nfert*spcat12mo, data=corndf)
summary(cornaov_full)

# put interaction output into a dataframe we can sort
Tukout_full <- as.data.frame(Tukout_full[5]) %>%  # 5 is the list item for the interaction that looks significant from the summary above
  rownames_to_column(., "term") %>%
  arrange(term)