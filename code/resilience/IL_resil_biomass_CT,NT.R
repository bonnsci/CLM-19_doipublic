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
# install.packages("ggpattern")
library(ggpattern)


spei <- read.csv("data/climate data not large/speiIL_formerge.csv")
# from script spei_IL.R


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

# ggplot(data=speibm[speibm$crop_name %in% c("corn, grain"),], 
#        aes(x=fit.12mo, y=grainc)) +  #  x=abs(fit.1mo), x=fit.12mo
#   geom_point(size=0.1, alpha=0.5) +
#   facet_grid(rows=vars(cc, nfert), cols=vars(month))



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

norm30cornssn <- speibm[speibm$year>2022 & speibm$year<2030 & 
                       speibm$spcat12mossn =="Normal" &
                       speibm$crop_name == "corn, grain",]

# norm30grainc_corn75p <- quantile(norm30corn$graincMg, prob=0.75)
# norm30grainc_corn90p <- quantile(norm30corn$graincMg, prob=0.90)
norm30grainc_cornmedssn <- median(norm30cornssn$graincMg)

# norm30soy<- speibm[speibm$year>2022 & speibm$year<2030 & 
#                        speibm$spcat12mo =="Normal" &
#                        speibm$crop_name == "soybean",]
# 
# norm30grainc_soy75p <- quantile(norm30soy$graincMg, prob=0.75)
# norm30grainc_soy90p <- quantile(norm30soy$graincMg, prob=0.90)
# norm30grainc_soymed <- median(norm30soy$graincMg)





# make plots like Fig. 5c

# A histogram is frequency. but a relative frequency AKA density gives % probability rather than just count.
# if N is large enough, then relative freq histogram starts to resemble the populations distribution,
# the area under the histogram =1, and related to probability.
# probability is the fraction of the area under the frequency distribution curve / density curve.

windows(xpinch=200, ypinch=200, width=5, height=5)


# set facet background color per spcat
pal11 <- c("#a50026", "#dd3d2d","#f67e4b","#FDb366","#FEDa8B", "#eaeccc", "#C2e4ef", "#98cae1","#6ea6cd", "#4a7bb7", "#364b9a")
pal7 <- c("#a50026", "#f67e4b","#FEDa8B", "#eaeccc", "#C2e4ef","#6ea6cd",  "#364b9a")
pal5 <- c( "#f67e4b","#FEDa8B", "#eaeccc", "#C2e4ef","#6ea6cd")


# how many seasons in each SPEI category do we have?
ntest <- speibm[speibm$crop_name=="corn, grain" & speibm$season == "Summer" &
                       !is.na(speibm$spcat12mossn) &  # for now just compare extremes: CT+NC and NT+CC
                       speibm$management_name %in% c("ct-nc-cn") & speibm$site=="IL-n_1",]  # need to look at just one rep otherwise counts artificially inflated
ntestn <- aggregate(season~spcat12mossn, dat=ntest, FUN="length")
# ntestn
# 1 Exceptional Drought      3  # ignore
# 2     Extreme Drought      3  # ignore
# SEVERE DROUGHT   0            # NOT SHOWN IN OUTPUT, ignore
# 3    Moderate Drought     24
# 4      Abnormally Dry     21
# 5              Normal     45
# 6      Abnormally Wet     21
# 7      Moderately Wet     18
# 8        Severely Wet      9  # ignore
# 9       Extremely Wet      6  # ignore
# EXCEPTIONAL WET   0           # NOT SHOWN IN OUTPUT, ignore



# cut out SPEI categories with <10 observations

ggplot(data=speibm[speibm$crop_name=="corn, grain" & speibm$season == "Summer" &
                     !is.na(speibm$spcat12mossn) &  # for now just compare extremes: CT+NC and NT+CC :
                     speibm$management_name %in% c("ct-nc-cn", "ct-nc-sn","nt-cc-cn", "nt-cc-sn") &
                     !speibm$spcat12mossn %in% c("Exceptional Drought", "Extreme Drought", "Severe Drought",
                                              "Exceptionally Wet", "Extremely Wet", "Severely Wet"),],
       aes(x=graincMg)) +
  # geom_histogram() + # histogram (counts)
  geom_density(aes(y=after_stat(density)), # sets the y-axis units
                   #group=interaction(spcat12ssn2, cc, till)),
               trim=T) +   # make sure calculating the density per facet, not whole dataset at once

  #### FUTURE try to FILL AREA UNDER POLYGON > median - but NOT NECESSARY
  
  # labs(x="Corn grain biomass C (kg/ha)", y="Frequency (count) 2022 to 2072") +
  labs(x="Corn grain biomass C (Mg/ha)", y="Frequency (%) 2022 to 2072") +
  scale_y_continuous(labels=scales::percent_format()) +
  geom_vline(xintercept=norm30grainc_cornmedssn, color="#BBcc33", linewidth=1.2) +  # 4000 chosen arbitrarily - is there something more meaningful?
  # geom_vline(xintercept=norm30grainc_corn90p, color="#44bb99") + 
  facet_grid2(cols=vars(spcat12mossn), rows=vars(factor(cc, levels=c("NC", "CC")), 
                                              factor(till, levels=c("CT", "NT"))), 
              # scales="free_y", 
             labeller = as_labeller(
               # c("Exceptional Drought"= "D4","Extreme Drought" = "D3","Severe Drought" = "D2","Moderate Drought" = "D1", "Abnormally Dry" = "D0",
               #   "Normal" = "Normal", "Abnormally Wet" = "W0", "Moderately Wet" = "W1", "Severely Wet" = "W2", "Extremely Wet" = "W3", "Exceptionally Wet" = "W4")))
              # c("Exceptional Drought"= "Exceptional\nDrought","Extreme Drought" = "Extreme\nDrought",
              #   "Severe Drought" = "Severe\nDrought","Moderate Drought" = "Moderate\nDrought",
              #   "Abnormally Dry" = "Abnormally\nDry", "Normal" = "Normal",
              #   "Abnormally Wet" = "Abnormally\nWet", "Moderately Wet" = "Moderately\nWet",
              #   "Severely Wet" = "Severely\nWet", "Extremely Wet" = "Extremely\nWet",
              #   "Exceptionally Wet" = "Exceptionally\nWet",
                c("Moderate Drought" = "Moderate\nDrought",
                  "Abnormally Dry" = "Abnormally\nDry", "Normal" = "Normal",
                  "Abnormally Wet" = "Abnormally\nWet", "Moderately Wet" = "Moderately\nWet",
                "NC"= "No cover crops","CT" = "Conventional till","CC" = "Has cover crops", 
                 "NT" = "No-till")),
              strip = strip_themed(background_x = elem_list_rect(fill = pal5))) +  #,  # cool stuff from ggh4x package!
                                   # text_x = elem_list_text(color=c("white", rep("black", 9), "white")))) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray97'),
    axis.text=element_text(size=12, face="bold"),
    # plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=13))

# ggsave("plots/resilience/biomass/IL_corngrainc_bydrought_hist.png", width=4, height=10, dpi=300)
ggsave("plots/resilience/biomass/IL_corngrainc_bydrought_density Summer.png", width=10, height=4, dpi=300)




# Quantify areas under curves by site, SPEI, and treatments

lsd <- speibm[speibm$crop_name %in% c("corn, grain") & speibm$season =="Summer" &   # , "soybean"
                !speibm$spcat12mossn %in% c("Exceptional Drought", "Extreme Drought", "Severe Drought",
                                         "Exceptionally Wet", "Extremely Wet", "Severely Wet"),] %>%
  group_split(site, spcat12mossn, till, cc, nfert) # crop_name,    by site, so that we can compare means and variability of treatments across sites




# ecdf = empirical cumulative distribution function
lecdf <- lapply(lsd, function(i){
  d_fun <- ecdf(i$graincMg)
  cropmed <- norm30grainc_cornmedssn
#  cropmed <- ifelse(i$crop_name[1] == "corn, grain", norm30grainc_cornmedssn)
#  crop90p <- ifelse(i$crop_name[1] == "corn, grain", norm30grainc_corn90p, norm30grainc_soy90p)
  grmed <- 1 - d_fun(cropmed) # 1- gives the area under the curve to the RIGHT of the value
#  gr90p <- 1 - d_fun(crop90p)
  out <- i[1, c("site", "till", "cc", "nfert", "spcat12mossn")]
  out$grmed <- grmed
  # out$gr90p <- gr90p
  out
})

lecdf <- lecdf %>%
  bind_rows(lecdf)

# unique(lecdf$spcat12mossn)

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

lecdf$spcat12mossn <- factor(lecdf$spcat12mossn,
                           levels=c("Moderate Drought", 
                                    "Abnormally Dry", "Normal" ,
                                    "Abnormally Wet" , "Moderately Wet"),
                           ordered=T)



##### letters for nt-cc, ct-nc x spei plot

lecdf_simp <-  #filter(lecdf, crop_name == "corn, grain") %>%
  filter(lecdf, (cc=="CC" & till=="NT") | (cc == "NC" & till =="CT") )

cornaov_simp <- aov(grmed~cc*nfert*spcat12mossn,   # since we only have CC+NT vs NC+CT we've made till and cc redundant
                    data=lecdf_simp)

summary(cornaov_simp)
#                           Df Sum Sq Mean Sq F value   Pr(>F)    
#   cc                       1   0.07   0.072   1.444 0.229670    
#   nfert                    2  18.79   9.395 188.012  < 2e-16 ***
#   spcat12mossn             4  38.05   9.512 190.347  < 2e-16 ***
#   cc:nfert                 2   9.82   4.911  98.282  < 2e-16 ***
#   cc:spcat12mossn          4   1.31   0.327   6.541 3.16e-05 ***
#   nfert:spcat12mossn       8   1.38   0.173   3.464 0.000567 ***
#   cc:nfert:spcat12mossn    8   0.39   0.049   0.974 0.454632    
# Residuals             1890  94.45   0.050                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 384 observations deleted due to missingness

Tukout_simp <- TukeyHSD(cornaov_simp)
cld_simp <- multcompLetters4(cornaov_simp, Tukout_simp)
# table with letters and 3rd quantile
lecdf_sum<- group_by(lecdf, cc, nfert, spcat12mossn) %>%  # filter(lecdf, till %in% c("CT", "NT")) %>%
  drop_na(spcat12mossn) %>%
  summarize(mean=mean(grmed), 
            se=se(grmed)) %>%
  arrange(desc(mean))

cld_lecdf <- as.data.frame.list(cld_simp$`cc:nfert:spcat12mossn`)
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

windows(xpinch=200, ypinch=200, width=5, height=5)


ggplot(data=lecdf_sum, aes(x=nfert, y=mean)) +
  geom_bar(stat="identity", fill="gray60") +
  geom_errorbar(aes(x=nfert, ymin=mean-se, ymax=mean+se), width=0.2, linewidth=0.8, color="gray20") +
  facet_grid2(rows=vars(factor(cc, levels=c("NC", "CC")), factor(till, levels=c("CT", "NT"))),
             cols=vars(spcat12mossn),
             labeller = as_labeller(
               # c("Exceptional Drought"= "D4","Extreme Drought" = "D3","Severe Drought" = "D2","Moderate Drought" = "D1", "Abnormally Dry" = "D0",
               #   "Normal" = "Normal", "Abnormally Wet" = "W0", "Moderately Wet" = "W1", "Severely Wet" = "W2", "Extremely Wet" = "W3", "Exceptionally Wet" = "W4")))
               c("Moderate Drought" = "Moderate\nDrought", 
                 "Abnormally Dry" = "Abnormally\nDry", 
                 "Normal" = "Normal" ,
                 "Abnormally Wet" = "Abnormally\nWet" , 
                 "Moderately Wet" = "Moderately\nWet",
                 # 
                 # "Exceptional Drought" = "Exceptional\nDrought",
                 # "Severe/Extreme Drought" = "Severe/Extreme\nDrought",
                 # "Abnorm Dry/Mod Drt" = "Abnormally Dry/\nModerate Drought", 
                 # "Normal" = "Normal", 
                 # "Abnorm/Mod Wet" = "Abnormally Wet/\nModerately Wet", 
                 # "Severe/Extreme Wet" = "Severely/\nExtremely Wet", 
                 # "Exceptionally Wet" = "Exceptionally\nWet",
                 "NC"= "Without cover crops","CC" = "With cover crops",
                 "CT" = "Conventional Till", "NT" = "No Till")), 
             strip = strip_themed(background_x = elem_list_rect(fill = pal5), #,  # cool stuff from ggh4x package!
                                  text_x=elem_list_text(color="black"))) +
                                  #text_x = elem_list_text(color= c("white", rep("black", 5), "white")))) +
  ylab("Mean probability corn grain biomass-C\n> median in Normal year in 2020s") +
  xlab("N management") +
  scale_x_discrete(breaks=c("Fall N", "High N", "Recommended N"),
                   labels = c("Fall\nN", "High\nN", "Recomm.\nN")) +
  geom_text(aes(x=nfert, y=mean +0.15, label=cld), size=5) + 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-10, hjust=0.5, vjust=0.5, size=11),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))

ggsave("plots/resilience/biomass/IL_corngr biomass prob summer SPEI nc-ct v cc-nt withletters.png", width=10, height=3.5, dpi=300) 


####### simpler version of above graph for IL farmer report

pal2 <- c("#20243d", "#669947")

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=lecdf_sum[lecdf_sum$nfert %in% c("Fall N", "Recommended N") &
                        lecdf_sum$spcat12mossn %in% c("Moderate Drought", "Normal", "Moderately Wet"),],
       aes(x=nfert, y=mean)) +
  geom_col_pattern(aes(fill=nfert, pattern_density=till), pattern="stripe", alpha=0.7, 
                   show.legend=F, pattern_fill="white", pattern_colour="white", colour="white") + 
  scale_pattern_density_manual(values=c(0.05, 0)) +
  scale_fill_manual(values=pal2) +
  geom_errorbar(aes(x=nfert, ymin=mean-se, ymax=mean+se), width=0.2, linewidth=0.8, color="gray20") +
  facet_grid2(rows=vars(factor(cc, levels=c("NC", "CC")), factor(till, levels=c("CT", "NT"))),
              cols=vars(spcat12mossn),
              labeller = as_labeller(
                # c("Exceptional Drought"= "D4","Extreme Drought" = "D3","Severe Drought" = "D2","Moderate Drought" = "D1", "Abnormally Dry" = "D0",
                #   "Normal" = "Normal", "Abnormally Wet" = "W0", "Moderately Wet" = "W1", "Severely Wet" = "W2", "Extremely Wet" = "W3", "Exceptionally Wet" = "W4")))
                c("Moderate Drought" = "Moderate\nDrought", 
                  #"Abnormally Dry" = "Abnormally\nDry", 
                  "Normal" = "Normal" ,
                  #"Abnormally Wet" = "Abnormally\nWet" , 
                  "Moderately Wet" = "Moderately\nWet",
                  # 
                  # "Exceptional Drought" = "Exceptional\nDrought",
                  # "Severe/Extreme Drought" = "Severe/Extreme\nDrought",
                  # "Abnorm Dry/Mod Drt" = "Abnormally Dry/\nModerate Drought", 
                  # "Normal" = "Normal", 
                  # "Abnorm/Mod Wet" = "Abnormally Wet/\nModerately Wet", 
                  # "Severe/Extreme Wet" = "Severely/\nExtremely Wet", 
                  # "Exceptionally Wet" = "Exceptionally\nWet",
                  "NC"= "Without cover crops","CC" = "With cover crops",
                  "CT" = "Conventional Till", "NT" = "No Till")), 
              strip = strip_themed(background_x = elem_list_rect(fill = pal5[2:4]), #,  # cool stuff from ggh4x package!
                                   text_x=elem_list_text(color="black"))) +
  #text_x = elem_list_text(color= c("white", rep("black", 5), "white")))) +
  ylab("Mean probability corn grain biomass-C\n> median in Normal year in 2020s") +
  xlab("N management") +
  scale_x_discrete(breaks=c("Fall N","Recommended N"),
                   labels = c("Fall\nN", "Spring\nN")) +
  #geom_text(aes(x=nfert, y=mean +0.15, label=cld), size=5) + 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(hjust=0.5, vjust=0.5, size=11),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))

ggsave("plots/resilience/biomass/IL_corngr biomass prob summer SPEI nc-ct v cc-nt simpler.png", width=7, height=6, dpi=300) 






















