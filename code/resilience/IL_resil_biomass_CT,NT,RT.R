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


windows(xpinch=200, ypinch=200, width=5, height=5)
# 


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


norm30grainc_cornmedssn <- median(norm30cornssn$graincMg)





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
                     speibm$management_name %in% c("ct-nc-cn", "ct-nc-sn", "ct-cc-cn", "ct-cc-sn", "rt-cc-cn", "rt-cc-sn", "nt-cc-cn", "nt-cc-sn") &
                     !speibm$spcat12mossn %in% c("Exceptional Drought", "Extreme Drought", "Severe Drought",  # excluding because we have <10 observations in the data, see ntestn above
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
                                                 factor(till, levels=c("CT", "RT", "NT"))), 
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
                  "RT" = "Reduced till", "NT" = "No-till")),
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

# ggsave("plots/resilience/biomass/IL_corngrainc_bydrought_density Summer_CT,RT,NT.png", width=10, height=5, dpi=300)




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
  bind_rows(lecdf) %>%
  mutate(cctill = paste0(cc, ".", till))   # was using this to run ANOVA with nfert, cc, till, and speicat, 
                                           # condensed 2 variables into one. ended up not needing it. --helpful for plotting tho

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
                             ordered=F)  # if this is true, a lm() will try to fit the data across the levels, which isn't quite correct



##### letters for nt-cc, ct-nc x spei plot

# helpful article about compact letter displays: https://schmidtpaul.github.io/dsfair_quarto/ch/summaryarticles/compactletterdisplay.html

lecdf_simp <-  #filter(lecdf, crop_name == "corn, grain") %>%
  # filter(lecdf, (cc=="CC" & till=="NT") | 
  #          (cc == "NC" & till =="CT") | 
  #      #  (cc=="CC" & till=="RT") |  
  #          (cc=="CC" & till=="CT") |
  #        (cc=="NC" & till=="NT")) %>%
  filter(lecdf, cctill %in% c("CC.NT", "NC.CT", "CC.CT", "NC.NT")) %>%
  drop_na(spcat12mossn) %>%
  filter(nfert %in% c("Recommended N"))

# when filter data for REcommended N (spring application) and Fall N (same annual 
# amount but most applied in the fall), The main finding is that with cover crops, 
# corn biomass goes down (because cover crop took up fertilizer N and,
# it was still locked up in cover crop biomass, not available for corn.)

# When filter data for High N vs Recommended N (both spring split applications) 
# - No differences between N treatments.
# so for simplicity, will make this graph and analysis with just Recommended N.

# there is no 3-way interaction between till, cc, and drought category
# it still does not like RT:Moderately Wet


cornaov_simp <- aov(grmed~cc:spcat12mossn:till,  data=lecdf_simp)
cornaov_lm <- lm(grmed~cc:spcat12mossn:till, data=lecdf_simp)

cornaov_simp <- aov(grmed~cctill:spcat12mossn,  data=lecdf_simp)
cornaov_lm <- lm(grmed~cctill:spcat12mossn, data=lecdf_simp)
# note cc:till leads to singularities
# alias(cornaov_lm)
# summary(cornaov_lm)
# Call:
#   lm(formula = grmed ~ cctill:spcat12mossn, data = lecdf_simp)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.8744 -0.1146  0.0278  0.1362  0.4253 
# 
# Coefficients: (1 not defined because of singularities)
#                                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                               0.8741815  0.0236263  37.000  < 2e-16 ***
#   cctillCC.CT:spcat12mossnModerate Drought -0.4145585  0.0334127 -12.407  < 2e-16 ***
#   cctillCC.NT:spcat12mossnModerate Drought -0.2769841  0.0334127  -8.290 2.88e-16 ***
#   cctillNC.CT:spcat12mossnModerate Drought -0.4070064  0.0334127 -12.181  < 2e-16 ***
#   cctillNC.NT:spcat12mossnModerate Drought -0.3424231  0.0334127 -10.248  < 2e-16 ***
#   cctillCC.CT:spcat12mossnAbnormally Dry   -0.2994792  0.0334127  -8.963  < 2e-16 ***
#   cctillCC.NT:spcat12mossnAbnormally Dry   -0.1892485  0.0334127  -5.664 1.83e-08 ***
#   cctillNC.CT:spcat12mossnAbnormally Dry   -0.2798363  0.0334127  -8.375  < 2e-16 ***
#   cctillNC.NT:spcat12mossnAbnormally Dry   -0.2604911  0.0334127  -7.796 1.33e-14 ***
#   cctillCC.CT:spcat12mossnNormal           -0.3734120  0.0334127 -11.176  < 2e-16 ***
#   cctillCC.NT:spcat12mossnNormal           -0.2443920  0.0334127  -7.314 4.59e-13 ***
#   cctillNC.CT:spcat12mossnNormal           -0.4363577  0.0334127 -13.060  < 2e-16 ***
#   cctillNC.NT:spcat12mossnNormal           -0.3623988  0.0334127 -10.846  < 2e-16 ***
#   cctillCC.CT:spcat12mossnAbnormally Wet   -0.0177083  0.0334127  -0.530   0.5962    
# cctillCC.NT:spcat12mossnAbnormally Wet    0.0002604  0.0334127   0.008   0.9938    
# cctillNC.CT:spcat12mossnAbnormally Wet   -0.0606771  0.0334127  -1.816   0.0696 .  
# cctillNC.NT:spcat12mossnAbnormally Wet   -0.0127604  0.0334127  -0.382   0.7026    
# cctillCC.CT:spcat12mossnModerately Wet   -0.0104167  0.0334127  -0.312   0.7553    
# cctillCC.NT:spcat12mossnModerately Wet    0.0404018  0.0334127   1.209   0.2268    
# cctillNC.CT:spcat12mossnModerately Wet   -0.0312500  0.0334127  -0.935   0.3498    
# cctillNC.NT:spcat12mossnModerately Wet           NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.189 on 1260 degrees of freedom
# Multiple R-squared:  0.434,	Adjusted R-squared:  0.4255 
# F-statistic: 50.86 on 19 and 1260 DF,  p-value: < 2.2e-16


summary(cornaov_simp)
#                       Df Sum Sq Mean Sq F value Pr(>F)    
# cctill:spcat12mossn   19  34.52  1.8170   50.86 <2e-16 ***
#   Residuals           1260  45.01  0.0357                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


Tukout_simp <- TukeyHSD(cornaov_simp)
cld_simp <- multcompLetters4(cornaov_simp, Tukout_simp)
# table with letters and 3rd quantile
lecdf_sum<- group_by(lecdf, cc, cctill, till, nfert, spcat12mossn) %>%  # filter(lecdf, till %in% c("CT", "NT")) %>%
  filter(cctill %in% c("CC.NT", "NC.CT", "CC.CT", "NC.NT")) %>%
  filter(nfert %in% c("Recommended N")) %>%
  drop_na(spcat12mossn) %>%
  summarize(mean=mean(grmed), 
            se=se(grmed)) %>%
  arrange(desc(mean))

cld_lecdf <- as.data.frame.list(cld_simp$`cctill:spcat12mossn`)
lecdf_sum$cld <- cld_lecdf$Letters


####################################  cc x till x nfert x spei plot

windows(xpinch=200, ypinch=200, width=5, height=5)

pal2 <- c("#004a23", "#669947")


  

ggplot(data=lecdf_sum[lecdf_sum$spcat12mossn %in% c("Moderate Drought", "Normal", "Moderately Wet"),], 
       aes(x=factor(cctill, levels=c("NC.CT", "NC.NT", "CC.CT", "CC.NT")), 
                                    y=mean)) +
  geom_col_pattern(aes(pattern_density=till, fill=cc), pattern="stripe", alpha=0.7, 
                   show.legend=F, pattern_fill="white", pattern_colour="white", colour="white") + 
  scale_pattern_density_manual(values=c(0.05, 0)) +
  scale_fill_manual(values=pal2) +
  geom_errorbar(aes(x=cctill, ymin=mean-se, ymax=mean+se), width=0.2, linewidth=0.8, color="gray20") +
  facet_grid2(rows=vars(spcat12mossn),  # cols=vars(factor(cc, levels=c("NC", "CC")), factor(till, levels=c("CT", "NT"))),
              labeller = as_labeller(
                # c("Exceptional Drought"= "D4","Extreme Drought" = "D3","Severe Drought" = "D2","Moderate Drought" = "D1", "Abnormally Dry" = "D0",
                #   "Normal" = "Normal", "Abnormally Wet" = "W0", "Moderately Wet" = "W1", "Severely Wet" = "W2", "Extremely Wet" = "W3", "Exceptionally Wet" = "W4")))
                c("Moderate Drought" = "Moderate\nDrought", 
                  # "Abnormally Dry" = "Abnormally\nDry", 
                  "Normal" = "Normal" ,
                  # "Abnormally Wet" = "Abnormally\nWet" , 
                  "Moderately Wet" = "Moderately\nWet")),
                  # 
                  # "Exceptional Drought" = "Exceptional\nDrought",
                  # "Severe/Extreme Drought" = "Severe/Extreme\nDrought",
                  # "Abnorm Dry/Mod Drt" = "Abnormally Dry/\nModerate Drought", 
                  # "Normal" = "Normal", 
                  # "Abnorm/Mod Wet" = "Abnormally Wet/\nModerately Wet", 
                  # "Severe/Extreme Wet" = "Severely/\nExtremely Wet", 
                  # "Exceptionally Wet" = "Exceptionally\nWet",
                  # "NC"= "Without cover crops","CC" = "With cover crops",
                  # "CT" = "Conventional Till", "RT" = "Reduced Till", "NT" = "No Till")), 
              strip = strip_themed(background_y = elem_list_rect(fill = pal5[c(1,3,5)]), #,  # cool stuff from ggh4x package!
                                   text_y=elem_list_text(color="black"))) +
  #text_x = elem_list_text(color= c("white", rep("black", 5), "white")))) +
  ylab("Mean probability corn grain biomass-C\n> median in Normal year in 2020s") +
  xlab("N management") +
  # scale_x_discrete(breaks=c("NC.CT", "NC.NT", "CC.CT", "CC.NT"),
  #                   labels = c("NC.CT", "NC.NT", "CC.CT", "CC.NT")) +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  geom_text(aes(x=cctill, y=mean +0.15, label=cld), size=5) + 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-10, hjust=0.5, vjust=0.5, size=11),
    axis.text.y=element_text(size=11),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))

ggsave("plots/resilience/biomass/IL_corngr biomass prob summer SPEI CC, NC, NT, CT withletters.png", width=4.5, height=6, dpi=300) 


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



