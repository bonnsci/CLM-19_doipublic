# Here we summarize future N2O emissions by DNDC simulations
# i.e., look for effects of till, cc, and Nfert on N2O emissions 2022-2072.

# packages
library(tidyverse) # has ggplot, dplyr, reshape2, etc.
library(reshape2) # for dcast / melt
library(MASS) # for boxcox

se <- function(x) sd(x) / sqrt(length(x))

# load data
ghgdat <- read.csv("data/simulations/un-weighted_resultsCA_20240220.csv")
# as.data.frame(names(ghgdat))
# names(ghgdat) - all are calendar year sums
# 1         site_name
# 2       region_name
# 3         crop_name # we have almonds and grapes together here. note these results have
# Been pre-processed by Regrow weighting the average GHG results for the alley and crop row systems
# weights for almonds: alley=0.55, crop row = 0.45
# 4        management # types:  [1] "cn_ct-bc" "cn_ct-lc" "cn_ct-nc" "cn_nt-bc" "cn_nt-lc" "cn_nt-nc" "cn_rt-bc" "cn_rt-lc" "cn_rt-nc"
#                               [10] "rn_ct-bc" "rn_ct-lc" "rn_ct-nc" "rn_nt-bc" "rn_nt-lc" "rn_nt-nc" "rn_rt-bc" "rn_rt-lc" "rn_rt-nc"
# 5  climate_scenario
# 6              year
# 7              dsoc in tonne C/ha
# 8               n2o DIRECT n2o em in tonne N2o/ha
# 9               ch4 in tonne ch4/ha
# 10              ghg sum of ghg_dsoc, ghg_ch4, ghg_total_n2o in tonne co2e /ha
# 11     indirect_n2o in tonne n2o /ha
# 12        total_n2o sum of n2o + indirect_n2o in tonne n2o/ha
# 13         ghg_dsoc changes in soc stocks in tonne co2e/ha
# 14          ghg_ch4 sum of ch4 em in co2e/ha
# 15          ghg_n2o sum of DIRECT n2o em in tonne co2e/ha
# 16 ghg_indirect_n2o sum of indirect n2o em in tonne co2e/ha
# 17    ghg_total_n2o total n2o em in tonne co2e/ha


ghgdat <- ghgdat[ghgdat$year>2021 & ghgdat$climate_scenario=="rcp60",c(2,4:7, 11, 14, 16, 18)]

ghgdat$till <- ifelse(grepl("_ct-", ghgdat$management), "CT", 
                    ifelse(grepl("_rt-", ghgdat$management), "RT", "NT"))
# # check
# unique(ghgdat$till)

# dummy for CC or NC
ghgdat$cc <- ifelse(grepl("-nc", ghgdat$management),"NC",
                    ifelse(grepl("-bc", ghgdat$management), "TC", "LC"))
# # check
# unique(ghgdat$cc)

# dummy for N treatment
ghgdat$nfert <- ifelse(grepl("cn_", ghgdat$management), "50 N", "40 N")
# # check
# unique(ghgdat$nfert)

# dummy for decade
ghgdat$decade <- ifelse(ghgdat$year <2031, "2020s",
                             ifelse(ghgdat$year>=2031 & ghgdat$year <2041, "2030s",
                                    ifelse(ghgdat$year>=2041 & ghgdat$year <2051, "2040s",
                                           ifelse(ghgdat$year>=2051 & ghgdat$year <2061, "2050s",
                                                  ifelse(ghgdat$year>=2061 & ghgdat$year <2071, "2060s", "2070s")))))





# mean annual n2o emissions by decade

ghgdec <- ghgdat %>%
  group_by(crop_name, till, cc, nfert, decade) %>%  # calculate annual means and se across sites and years per crop per decade
  summarize(net.mean=mean(ghg),
            net.se=se(ghg),
            ghgdsoc.mean = mean(ghg_dsoc),
            ghgdsoc.se=se(ghg_dsoc),
            ghgn2o.mean = mean(ghg_total_n2o),
            ghgn2o.se = se(ghg_total_n2o)
            ) %>%
  melt(id=c("crop_name", "till", "cc", "nfert", "decade")) %>%  # , "crop"  #melt so that net, dsoc, and n2o are in one column for making stacked bar chart
  rename(var1=variable) %>%
  mutate(mean.se = ifelse(grepl(pattern="mean", x=var1), "mean", "se"),
         var2 = ifelse(mean.se=="mean", gsub(pattern = ".mean", replacement="", x=var1), 
                       gsub(pattern = ".se", replacement="", x=var1))) %>%
  dplyr::select(-var1) %>%
  dcast(crop_name+ till + cc + nfert + decade +var2 ~mean.se) # + crop 


# $value is in units of cumulative tco2e/ha


# make stacked bar chart of n2o, soc, net by decade 
# with letters comparing the 2022-72 mean of each treatment group (cover*till*Nfert combo) for soc, n2o, net

# first, find the diffs among n2o bars, soc bars, net bars

ghgdat.a <- ghgdat[ghgdat$crop_name=="almond",]
ghgdat.g <- ghgdat[ghgdat$crop_name=="grape",]

# ALMOND RESULTS
lmn2o <- lm(ghg_total_n2o ~till:cc:nfert, data=ghgdat.a)  
lmsoc <- lm(ghg_dsoc ~till:cc:nfert, data=ghgdat.a)  
lmnet <- lm(ghg ~till:cc:nfert, data=ghgdat.a)
summary(lmn2o)
summary(lmsoc)
summary(lmnet)

# Call:
#   lm(formula = ghg_total_n2o ~ till:cc:nfert, data = ghgdat.a)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.62121 -0.11333 -0.03436  0.07401  2.20544 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            0.426000   0.009673  44.041   <2e-16 ***
#   tillCT:ccLC:nfert40 N  0.156655   0.013679  11.452   <2e-16 ***
#   tillNT:ccLC:nfert40 N  0.306664   0.013679  22.418   <2e-16 ***
#   tillRT:ccLC:nfert40 N  0.176915   0.013679  12.933   <2e-16 ***
#   tillCT:ccNC:nfert40 N -0.142223   0.013679 -10.397   <2e-16 ***
#   tillNT:ccNC:nfert40 N -0.141995   0.013679 -10.380   <2e-16 ***
#   tillRT:ccNC:nfert40 N -0.143195   0.013679 -10.468   <2e-16 ***
#   tillCT:ccTC:nfert40 N -0.157018   0.013679 -11.479   <2e-16 ***
#   tillNT:ccTC:nfert40 N -0.146370   0.013679 -10.700   <2e-16 ***
#   tillRT:ccTC:nfert40 N -0.153672   0.013679 -11.234   <2e-16 ***
#   tillCT:ccLC:nfert50 N  0.310327   0.013679  22.686   <2e-16 ***
#   tillNT:ccLC:nfert50 N  0.460335   0.013679  33.652   <2e-16 ***
#   tillRT:ccLC:nfert50 N  0.330586   0.013679  24.167   <2e-16 ***
#   tillCT:ccNC:nfert50 N  0.011449   0.013679   0.837    0.403    
# tillNT:ccNC:nfert50 N  0.011676   0.013679   0.854    0.393    
# tillRT:ccNC:nfert50 N  0.010477   0.013679   0.766    0.444    
# tillCT:ccTC:nfert50 N -0.003346   0.013679  -0.245    0.807    
# tillNT:ccTC:nfert50 N  0.007302   0.013679   0.534    0.593    
# tillRT:ccTC:nfert50 N        NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.279 on 14958 degrees of freedom
# Multiple R-squared:  0.3185,	Adjusted R-squared:  0.3177 
# F-statistic: 411.2 on 17 and 14958 DF,  p-value: < 2.2e-16
# 
# > summary(lmsoc)
# 
# Call:
#   lm(formula = ghg_dsoc ~ till:cc:nfert, data = ghgdat.a)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.1216 -0.2054 -0.0052  0.2684  2.2932 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           -0.28552    0.02069 -13.800  < 2e-16 ***
#   tillCT:ccLC:nfert40 N -0.64265    0.02926 -21.963  < 2e-16 ***
#   tillNT:ccLC:nfert40 N -0.73298    0.02926 -25.051  < 2e-16 ***
#   tillRT:ccLC:nfert40 N -0.73514    0.02926 -25.125  < 2e-16 ***
#   tillCT:ccNC:nfert40 N  0.23688    0.02926   8.096 6.12e-16 ***
#   tillNT:ccNC:nfert40 N  0.21521    0.02926   7.355 2.01e-13 ***
#   tillRT:ccNC:nfert40 N  0.22033    0.02926   7.530 5.37e-14 ***
#   tillCT:ccTC:nfert40 N  0.02999    0.02926   1.025  0.30547    
# tillNT:ccTC:nfert40 N  0.08104    0.02926   2.770  0.00562 ** 
#   tillRT:ccTC:nfert40 N  0.04851    0.02926   1.658  0.09738 .  
# tillCT:ccLC:nfert50 N -0.69116    0.02926 -23.621  < 2e-16 ***
#   tillNT:ccLC:nfert50 N -0.78149    0.02926 -26.708  < 2e-16 ***
#   tillRT:ccLC:nfert50 N -0.78365    0.02926 -26.782  < 2e-16 ***
#   tillCT:ccNC:nfert50 N  0.18838    0.02926   6.438 1.25e-10 ***
#   tillNT:ccNC:nfert50 N  0.16670    0.02926   5.697 1.24e-08 ***
#   tillRT:ccNC:nfert50 N  0.17182    0.02926   5.872 4.39e-09 ***
#   tillCT:ccTC:nfert50 N -0.01852    0.02926  -0.633  0.52676    
# tillNT:ccTC:nfert50 N  0.03254    0.02926   1.112  0.26617    
# tillRT:ccTC:nfert50 N       NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5968 on 14958 degrees of freedom
# Multiple R-squared:  0.3154,	Adjusted R-squared:  0.3146 
# F-statistic: 405.4 on 17 and 14958 DF,  p-value: < 2.2e-16
# 
# > summary(lmnet)
# 
# Call:
#   lm(formula = ghg ~ till:cc:nfert, data = ghgdat.a)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.8681 -0.2599 -0.0120  0.2883  3.8514 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            0.13559    0.02142   6.331 2.51e-10 ***
#   tillCT:ccLC:nfert40 N -0.49253    0.03029 -16.260  < 2e-16 ***
#   tillNT:ccLC:nfert40 N -0.43239    0.03029 -14.275  < 2e-16 ***
#   tillRT:ccLC:nfert40 N -0.56590    0.03029 -18.683  < 2e-16 ***
#   tillCT:ccNC:nfert40 N  0.09584    0.03029   3.164 0.001558 ** 
#   tillNT:ccNC:nfert40 N  0.07417    0.03029   2.449 0.014353 *  
#   tillRT:ccNC:nfert40 N  0.07819    0.03029   2.582 0.009846 ** 
#   tillCT:ccTC:nfert40 N -0.12687    0.03029  -4.189 2.82e-05 ***
#   tillNT:ccTC:nfert40 N -0.06501    0.03029  -2.146 0.031871 *  
#   tillRT:ccTC:nfert40 N -0.10494    0.03029  -3.465 0.000532 ***
#   tillCT:ccLC:nfert50 N -0.38758    0.03029 -12.796  < 2e-16 ***
#   tillNT:ccLC:nfert50 N -0.32745    0.03029 -10.810  < 2e-16 ***
#   tillRT:ccLC:nfert50 N -0.46096    0.03029 -15.218  < 2e-16 ***
#   tillCT:ccNC:nfert50 N  0.20079    0.03029   6.629 3.50e-11 ***
#   tillNT:ccNC:nfert50 N  0.17911    0.03029   5.913 3.43e-09 ***
#   tillRT:ccNC:nfert50 N  0.18314    0.03029   6.046 1.52e-09 ***
#   tillCT:ccTC:nfert50 N -0.02192    0.03029  -0.724 0.469189    
# tillNT:ccTC:nfert50 N  0.03994    0.03029   1.318 0.187367    
# tillRT:ccTC:nfert50 N       NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6178 on 14958 degrees of freedom
# Multiple R-squared:  0.141,	Adjusted R-squared:  0.1401 
# F-statistic: 144.5 on 17 and 14958 DF,  p-value: < 2.2e-16


aovn2o <- aov(ghg_total_n2o ~till:cc:nfert, data=ghgdat.a)  
aovsoc <- aov(ghg_dsoc ~till:cc:nfert, data=ghgdat.a)  
aovnet <- aov(ghg ~till:cc:nfert, data=ghgdat.a)  

summary(aovn2o)
#                 Df Sum Sq Mean Sq F value Pr(>F)    
# till:cc:nfert    17  544.1   32.01   411.2 <2e-16 ***
#   Residuals     14958 1164.4    0.08                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(aovsoc)
#                 Df Sum Sq Mean Sq F value Pr(>F)    
# till:cc:nfert    17   2455  144.38   405.4 <2e-16 ***
#   Residuals     14958   5327    0.36                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(aovnet)
                  # Df Sum Sq Mean Sq F value Pr(>F)    
# till:cc:nfert    17    937   55.15   144.5 <2e-16 ***
#   Residuals     14958   5709    0.38                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukoutn2o <- TukeyHSD(aovn2o)
# # put interaction output into a dataframe we can sort
# Tukout <- as.data.frame(Tukout[7]) %>%  # [7] is the 3-way interaction term
#   rownames_to_column(., "term") %>%
#   arrange(term)

Tukoutsoc <- TukeyHSD(aovsoc)
Tukoutnet <- TukeyHSD(aovnet)


# compact letter display (cld)  ~ letters for bars
cldn2o<- multcompView::multcompLetters4(aovn2o, Tukoutn2o)
cldsoc<- multcompView::multcompLetters4(aovsoc, Tukoutsoc)
cldnet<- multcompView::multcompLetters4(aovnet, Tukoutnet)

# table with letters n2o
ghgn2osum <- group_by(ghgdat.a, cc, till, nfert) %>%
  summarize(mean.n2o=mean(ghg_total_n2o), 
            se.n2o=se(ghg_total_n2o)) %>%
  arrange(desc(mean.n2o))

cldn2o<- as.data.frame.list(cldn2o$`till:cc:nfert`)
ghgn2osum$cldn2o <- cldn2o$Letters


# table with letters dsoc
ghgsocsum <-group_by(ghgdat.a, cc, till, nfert) %>%
  summarize(mean.soc=mean(ghg_dsoc), 
            se.soc=se(ghg_dsoc)) %>%
  arrange(desc(mean.soc))

cldsoc<- as.data.frame.list(cldsoc$`till:cc:nfert`)
ghgsocsum$cldsoc <- cldsoc$Letters

# table with letters net
ghgnetsum <- group_by(ghgdat.a, cc, till, nfert) %>%
  summarize(mean.net=mean(ghg), 
            se.net=se(ghg)) %>%
  arrange(desc(mean.net))

cldnet<- as.data.frame.list(cldnet$`till:cc:nfert`)
ghgnetsum$cldnet <- cldnet$Letters

# put the summaries together
ghgsummary <- left_join(ghgsocsum, ghgn2osum) %>%
  left_join(.,ghgnetsum)
# clean up
rm(ghgsocsum, ghgn2osum, ghgnetsum, cldn2o, cldnet, cldsoc)



# make plot with letters
windows(xpinch=200, ypinch=200, width=5, height=5)


       
ggplot() +
  # n2o bars, error bars, letters
  geom_bar(data=ghgdec[ghgdec$var == "ghgn2o" & ghgdec$crop_name == "almond",], # need separate call for bars for n2o, for soc, and points for net
           aes(x=nfert, y=mean, group=decade), 
           stat="identity", position=position_dodge(), fill="#ee8866", alpha=0.7, color="#bb5566") + #, color="gray20") +
  geom_errorbar(data=ghgdec[ghgdec$var=="ghgn2o" & ghgdec$crop_name == "almond",], # ghgdec$crop == "corn" &
                aes(x=nfert, ymin= mean-se, ymax=mean+se, group=decade),  
                width=0.3, position=position_dodge(0.9),color="#882255") +
  #geom_bar(data=ghgsummary, aes(x=nfert, y=mean.n2o), stat="identity", color=NA, fill=NA) +
  geom_text(data=ghgsummary, aes(x=nfert, label=cldn2o, 
                                 y=ifelse(mean.n2o<1.5, mean.n2o + 0.2*mean.n2o, mean.n2o+0.35*mean.n2o)), 
            vjust=-0.5, color="#882255", size=4, fontface="bold") +
 
  
  # dsoc bars, error bars, letters
  geom_bar(data=ghgdec[ghgdec$var == "ghgdsoc" & ghgdec$crop_name == "almond",],  # need separate call for bars for n2ghgdec$till %in% c("CT", "NT"),], # for AGU only showing CT and NT not RT to simplify a bit
    aes(x=nfert, y=mean, group=decade),  
    stat="identity", position=position_dodge(), fill="#99ddff", alpha=0.7, color="#33bbee") + #, color="gray20") +
  geom_errorbar(data=ghgdec[ghgdec$var=="ghgdsoc" & ghgdec$crop_name == "almond",],
               aes(x=nfert, ymin= mean-se, ymax=mean+se, group=decade),  
               width=0.3, position=position_dodge(0.9), color="#33bbee") +
  #geom_bar(data=ghgsummary, aes(x=nfert, y=mean.soc), stat="identity", color=NA, fill=NA) +
  geom_text(data=ghgsummary, aes(x=nfert, label=cldsoc, 
                                 y=ifelse(mean.soc>-1, mean.soc + 0.5*mean.soc, mean.soc+0.35*mean.soc)), 
            vjust=-0.5, color="#0077BB", size=4, fontface="bold") +
  
  # zero-lin
  geom_hline(yintercept=0, color="#009988", linewidth=0.5) +
  
  # net points letters
  # geom_line(data=ghgdec[ghgdec$var == "net" &
  #                             ghgdec$till %in% c("CT", "NT"),],
  #            aes(x=nfert, y=mean),
  #            alpha=0.6, linewidth=1, position=position_dodge(0.9)) +  # color="gray25", 
  geom_point(data=ghgdec[ghgdec$var == "net" & ghgdec$crop_name == "almond",],
             aes(x=nfert, y=mean, group=decade),
             color="gray25", size=0.5, position=position_dodge(0.9)) +
  geom_errorbar(data=ghgdec[ghgdec$var=="net" & ghgdec$crop_name == "almond",], 
              aes(x=nfert, ymin= mean-se, ymax=mean+se, group=decade),  
              width=0.3, position=position_dodge(0.9), color="gray35") +
  geom_text(data=ghgsummary, aes(x=nfert, label=cldnet, y=mean.net-0.3),
                                 color="black", size=4, fontface="bold.italic")+

  # make it pretty
  facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))),  #, "RT"
             cols=vars(factor(cc, levels=c("NC", "TC", "LC"))), 
                      # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             labeller = as_labeller(
               c("NC"="No Cover Crop", "TC"="Triticale Cover", "LC" = "Legume Cover",
                 "CT" = "Conventional Till", "RT" = "Reduced Till", "NT" = "No Till"))) +  #, "RT"="Reduced Till"))) +
                 #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recomm. N"))) +
  # scale_fill_manual(values=c("#eaeccc", "#FEDA8B", "#FDB366", "#F67E4B", "#DD3D2D", "#A50026"),
  #                   name="Decade")+ #, name="N management") +
  xlab("Decade") +
  ylab(expression('Mean annual emissions (CO'[2]*'e ha'^-1*')')) +
  # scale_y_continuous(breaks=seq(-3,2,1), limits=c(-3,2.8)) +
 
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))
  
  

ggsave("plots/ghgs/CA_alm_decadal mean net em RCP60_with letters.png", width=8, height=6, dpi=300)



####################### make a simpler version that is the 2022-2072 means


# convert em in tonnes / ha to tonnes /ac

ghgsummary <- mutate(ghgsummary, 
                        mean.n2o.ac=mean.n2o*2.47105,
                        se.n2o.ac=se.n2o*2.47105,
                        mean.soc.ac=mean.soc*2.47105,
                        se.soc.ac=se.soc*2.47105,
                        mean.net.ac=mean.net*2.47105,
                        se.net.ac=se.net*2.47105)


ggplot(data=ghgsummary, aes(x=nfert)) +
  # n2o bars, error bars, letters
  geom_bar(aes(y=mean.n2o.ac), 
    stat="identity", position=position_dodge(), fill="#c44f2d", alpha=0.4) + #, color="gray20") +
  # geom_errorbar(aes(ymin= mean.n2o-se.n2o, ymax=mean.n2o+se.n2o),  
  #               width=0.3, position=position_dodge(0.9),color="#882255") +
  geom_text(data=ghgsummary, aes(x=nfert, label=cldn2o, 
                                 y=mean.n2o.ac + 0.3),
                                 # y=ifelse(mean.n2o<1.5, mean.n2o + 0.2*mean.n2o, mean.n2o+0.35*mean.n2o)), 
              color="#882255", size=4, fontface="bold") +
  # 
  
  # dsoc bars, error bars, letters
  geom_bar(aes(y=mean.soc.ac),  
           stat="identity", position=position_dodge(), fill="#20243d", alpha=0.4) + #, color="gray20") +
  # geom_errorbar(aes(ymin= mean.soc-se.soc, ymax=mean.soc+se.soc),  
  #               width=0.3, position=position_dodge(0.9), color="#33bbee") +
  geom_text(data=ghgsummary, aes(x=nfert, label=cldsoc, 
                              y=mean.soc.ac -0.5),
                                 #y=ifelse(mean.soc>-1, mean.soc + 0.5*mean.soc, mean.soc+0.35*mean.soc)), 
         color="#0077BB", size=4, fontface="bold") +
  
  # zero-line
  geom_hline(yintercept=0, color="#20243d", linewidth=0.5) +
  
  # net points
  geom_point(aes(y=mean.net.ac),
             color="#20243d", size=0.8, position=position_dodge(0.9)) +
  # geom_errorbar(aes(ymin= mean.net-se.net, ymax=mean.net+se.net),  
  #               width=0.3, position=position_dodge(0.9), color="gray35") +
  geom_text(data=ghgsummary, aes(label=cldnet, y=mean.net.ac-0.3),
            color="#20243d", size=4, fontface="bold.italic")+
  
  # make it pretty
  facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))),  #, "RT"
             cols=vars(factor(cc, levels=c("NC", "TC", "LC"))), 
             # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             labeller = as_labeller(
               c("NC"="No Cover Crop", "TC" = "Triticale Cover", "LC"="Legume Cover",
                 "CT" = "Conventional Till", "RT" = "Reduced Till", "NT" = "No Till"))) +  #, "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recomm. N"))) +
  # scale_fill_manual(values=c("#eaeccc", "#FEDA8B", "#FDB366", "#F67E4B", "#DD3D2D", "#A50026"),
  #                   name="Decade")+ #, name="N management") +

  ylab(expression('Mean annual emissions (tonnes CO'[2]*'e per acre)')) +
  # scale_y_continuous(breaks=seq(-6,5,1), limits=c(-6,5)) +
  
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))



ggsave("plots/ghgs/CA_alm_mean 2022-2072 em RCP60_with letters.png", width=6, height=7, dpi=300)












################ make a simpler version averaged across N treatments



# mean annual n2o emissions by decade

ghganndec2 <- ghgdat %>%
  group_by(till, cc, decade) %>%  # , crop # calculate means and se across all sites and 2 crops and 3 Nferts
  # showing corn-soy mean rather than just corn because 50 years of just corn data is like continuous corn rotation on a ha
  # which most ha are not
  summarize(net.mean=mean(ghg),
            net.se=se(ghg),
            ghgdsoc.mean = mean(ghg_dsoc),
            ghgdsoc.se=se(ghg_dsoc),
            ghgn2o.mean = mean(ghg_total_n2o),
            ghgn2o.se = se(ghg_total_n2o)) %>%
  melt(id=c("till", "cc", "decade")) %>%  # , "crop", "nfert"
  rename(var1=variable) %>%
  mutate(mean.se = ifelse(grepl(pattern="mean", x=var1), "mean", "se"),
         var2 = ifelse(mean.se=="mean", gsub(pattern = ".mean", replacement="", x=var1), 
                       gsub(pattern = ".se", replacement="", x=var1))) %>%
  dplyr::select(-var1) %>%
  dcast(till + cc  + decade +var2 ~mean.se) # + crop 


# $value is in units of cumulative tco2e/ha


# make stacked bar chart of n2o, soc, net by decade 
# with letters comparing the 2022-72 mean of each treatment group (cover*till*Nfert combo) for soc, n2o, net

# first, find the diffs among n2o bars, soc bars, net bars

effectn2o <- aov(ghg_total_n2o ~till*cc, data=ghgdat[ghgdat$till %in% c("NT", "CT"),])  # ghgdat$crop=="corn" &
effectsoc <- aov(ghg_dsoc ~till*cc, data=ghgdat[ghgdat$till %in% c("NT", "CT"),])  # ghgdat$crop=="corn" &
effectnet <- aov(ghg ~till*cc, data=ghgdat[ghgdat$till %in% c("NT", "CT"),])  # ghgdat$crop=="corn" &

summary(effectn2o)
# Df Sum Sq Mean Sq F value Pr(>F)    
# till            1   3323    3323  9776.8 <2e-16 ***
# cc              1     37      37   108.1 <2e-16 ***
# till:cc         1    167     167   490.5 <2e-16 ***
# Residuals   39548  13443       0                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(effectsoc)
# Df Sum Sq Mean Sq   F value Pr(>F)    
# till            1     82      82   100.470 <2e-16 ***
# cc              1  15606   15606 19102.264 <2e-16 ***
# till:cc         1      2       2     2.194  0.139    
# Residuals   39548  32309       1                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(effectnet)
# Df Sum Sq Mean Sq F value Pr(>F)    
# till            1   2511    2511    1551 <2e-16 ***
# cc              1  14166   14166    8752 <2e-16 ***
# till:cc         1    209     209     129 <2e-16 ***
# Residuals   39548  64017       2                   

Tukoutn2o <- TukeyHSD(effectn2o)
# # put interaction output into a dataframe we can sort
# Tukout <- as.data.frame(Tukout[7]) %>%  # [7] is the 3-way interaction term
#   rownames_to_column(., "term") %>%
#   arrange(term)

Tukoutsoc <- TukeyHSD(effectsoc)
Tukoutnet <- TukeyHSD(effectnet)


# compact letter display (cld)  ~ letters for bars
cldn2o<- multcompView::multcompLetters4(effectn2o, Tukoutn2o)
cldsoc<- multcompView::multcompLetters4(effectsoc, Tukoutsoc)
cldnet<- multcompView::multcompLetters4(effectnet, Tukoutnet)

# table with letters n2o
ghgn2osum <- filter(ghgdat,till %in% c("NT", "CT")) %>%  # crop=="corn",
  group_by(cc, till) %>%
  summarize(mean.n2o=mean(ghg_total_n2o), 
            se.n2o=se(ghg_total_n2o)) %>%
  arrange(desc(mean.n2o))

cldn2o<- as.data.frame.list(cldn2o$`till:cc`)
ghgn2osum$cldn2o <- cldn2o$Letters


# table with letters dsoc
ghgsocsum <- filter(ghgdat,till %in% c("NT", "CT")) %>%  # crop=="corn",
  group_by(cc, till) %>%
  summarize(mean.soc=mean(ghg_dsoc), 
            se.soc=se(ghg_dsoc)) %>%
  arrange(desc(mean.soc))

cldsoc<- as.data.frame.list(cldsoc$`till:cc`)
ghgsocsum$cldsoc <- cldsoc$Letters

# table with letters net
ghgnetsum <- filter(ghgdat,till %in% c("NT", "CT")) %>%  # crop=="corn",
  group_by(cc, till) %>%
  summarize(mean.net=mean(ghg), 
            se.net=se(ghg)) %>%
  arrange(desc(mean.net))

cldnet<- as.data.frame.list(cldnet$`till:cc`)
ghgnetsum$cldnet <- cldnet$Letters

# put the summaries together
ghgsummary <- left_join(ghgsocsum, ghgn2osum) %>%
  left_join(.,ghgnetsum)
# clean up
rm(ghgsocsum, ghgn2osum, ghgnetsum, cldn2o, cldnet, cldsoc)

# dummy data for letter labels
cc <- c("NC","NC","CC","CC")
till <- c("CT", "NT", "CT", "NT")
xs <- rep(3.5, 4)
n2o.ys <- c(1.2, 1.8, 1.3, 2)
soc.ys <- c(-1, -1.1, -2.5, -2.5)
net.ys <- c(0.6, 1, -0.9, -0.1)
ghgsummary <- left_join(ghgsummary,data.frame(cc=cc, till=till, xs=xs, n2o.ys = n2o.ys, soc.ys=soc.ys, net.ys=net.ys) )
rm(cc, till, xs, n2o.ys, soc.ys, net.ys)

# make plot with letters
windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot() +
  # n2o bars, error bars, letters
  geom_bar(# data=ghganndec2[ghganndec2$crop == "corn" & ghganndec2$var=="ghgn2o" & ghganndec2$till %in% c("CT", "NT"),], 
    data=ghganndec2[ghganndec2$var == "ghgn2o" &  # need separate call for bars for n2o, for soc, and points for net
                     ghganndec2$till %in% c("CT", "NT"),], # for AGU only showing CT and NT not RT to simplify a bit
    aes(x=decade, y=mean), 
    stat="identity", position=position_dodge(), fill="#ee8866", alpha=0.4) + #, color="gray20") + color="#bb5566"
  geom_errorbar(data=ghganndec2[ghganndec2$var=="ghgn2o" & ghganndec2$till %in% c("CT", "NT"),], # ghganndec2$crop == "corn" &
                aes(x=decade, ymin= mean-se, ymax=mean+se),  
                width=0.3, position=position_dodge(0.9),color="#882255") +
  #geom_bar(data=ghgsummary, aes(x=decade, y=mean.n2o), stat="identity", color=NA, fill=NA) +
  geom_text(data=ghgsummary, aes(x=xs, label=cldn2o, y=n2o.ys),
            color="#882255", size=7, fontface="bold") +  #  vjust=-0.5,

  
  # dsoc bars, error bars, letters
  geom_bar(data=ghganndec2[ghganndec2$var == "ghgdsoc" &  # need separate call for bars for n2o, for soc, and points for net
                            ghganndec2$till %in% c("CT", "NT"),], # for AGU only showing CT and NT not RT to simplify a bit
           aes(x=decade, y=mean),  
           stat="identity", position=position_dodge(), fill="#99ddff", alpha=0.4) + #, color="gray20") + color="#33bbee"
  geom_errorbar(data=ghganndec2[ghganndec2$var=="ghgdsoc" & ghganndec2$till %in% c("CT", "NT"),], 
                aes(x=decade, ymin= mean-se, ymax=mean+se),  
                width=0.3, position=position_dodge(0.9), color="#0077BB") +
  #geom_bar(data=ghgsummary, aes(x=decade, y=mean.soc), stat="identity", color=NA, fill=NA) +
  geom_text(data=ghgsummary, aes(x=xs, label=cldsoc, y=soc.ys),
            vjust=-0.5, color="#0077BB", size=7, fontface="bold") +
  
  # zero-line
  geom_hline(yintercept=0, color="#009988", linewidth=0.5) +
  
  # net points letters
  geom_line(data=ghganndec2[ghganndec2$var == "net" &
                              ghganndec2$till %in% c("CT", "NT"),],
             aes(x=decade, y=mean, group=1),
             alpha=0.7, linewidth=1, position=position_dodge(0.9)) +  # color="gray25",
  geom_point(data=ghganndec2[ghganndec2$var == "net" &
                              ghganndec2$till %in% c("CT", "NT"),],
             aes(x=decade, y=mean),
             color="black", size=1, position=position_dodge(0.9)) +
  geom_errorbar(data=ghganndec2[ghganndec2$var=="net" & 
                                 ghganndec2$till %in% c("CT", "NT"),], 
                aes(x=decade, ymin= mean-se, ymax=mean+se),  
                width=0.3, position=position_dodge(0.9), color="gray35") +
  geom_text(data=ghgsummary, aes(x=xs, label=cldnet, y=net.ys),
            color="black", size=7, fontface="bold.italic")+
  
  # make it pretty
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT"))),  #, "RT"
             cols=vars(factor(cc, levels=c("NC", "TC", "LC"))), 
             # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             labeller = as_labeller(
               c("NC"="No Cover Crop", "TC" = "Triticale Cover", "LC"="Legume Cover",
                 "CT" = "Conventional Till",  "NT" = "No Till"))) +  #, "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recomm. N"))) +
  # scale_fill_manual(values=c("#eaeccc", "#FEDA8B", "#FDB366", "#F67E4B", "#DD3D2D", "#A50026"),
  #                   name="Decade")+ #, name="N management") +
  xlab("Decade") +
  ylab(expression(bold('Mean annual emissions (CO'[2]*'e ha'^-1*')'))) +
  #scale_y_continuous(breaks=seq(-3,2,1), limits=c(-3,2.8)) +
  
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray97'),
    axis.text.x=element_text(angle=-30, hjust=0.5, vjust=0, size=12, face="bold"),
    axis.text.y=element_text(size=12, face="bold"),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))



ggsave("plots/ghgs/CA_alm_simulation mean (crop, site, decade, no nfert) annual em by decadeRCP60_with letters.png", width=7, height=8, dpi=300)



### simpler version - average across all years, SOC only

ghgann <- ghgdat %>%
  filter(nfert=="50 N", till %in% c("CT", "NT")) %>%  # not big diffs between the 2 so just use the recommended
  group_by(till, cc) %>%  # , crop # calculate means and se across all sites and all years
  summarize( #net.mean=mean(ghg),
            # net.se=se(ghg),
            ghgdsoc.mean = mean(ghg_dsoc),
            ghgdsoc.se=se(ghg_dsoc)) %>%
            #ghgn2o.mean = mean(ghg_total_n2o),
            # ghgn2o.se = se(ghg_total_n2o)) %>%
  melt(id=c("till", "cc")) %>%  # , "crop", "nfert"
  rename(var1=variable) %>%
  mutate(mean.se = ifelse(grepl(pattern="mean", x=var1), "mean", "se"),
         var2 = ifelse(mean.se=="mean", gsub(pattern = ".mean", replacement="", x=var1), 
                       gsub(pattern = ".se", replacement="", x=var1))) %>%
  dplyr::select(-var1) %>%
  dcast(till + cc  +var2 ~mean.se) # + crop 

ghgann$mean.ac <- ghgann$mean/2.471
ghgann$se.ac <- ghgann$se/2.471

# till cc    var2       mean         se     mean.ac       se.ac
# 1   CT LC ghgdsoc -1.4508175 0.04014978 -0.58713782 0.016248392
# 2   CT NC ghgdsoc  0.1107192 0.01079150  0.04480745 0.004367261
# 3   CT TC ghgdsoc -0.6159672 0.01449961 -0.24927851 0.005867913
# 4   NT LC ghgdsoc -1.6631133 0.04428232 -0.67305274 0.017920808
# 5   NT NC ghgdsoc  0.0712666 0.01105457  0.02884120 0.004473725
# 6   NT TC ghgdsoc -0.6649282 0.01626748 -0.26909276 0.006583358
mean(c(0.11, 0.07))  # -0.09
mean(c(0.66, 0.62)) #0.64
mean(c(1.45, 1.66)) # 1.56

effectsoc <- aov(ghg_dsoc ~till*cc, data=ghgdat[ghgdat$till %in% c("NT", "CT") & ghgdat$nfert=="50 N",])  # 
summary(effectsoc)
#               Df Sum Sq Mean Sq  F value   Pr(>F)    
# till           1     25    25.1   21.096 4.42e-06 ***
#   cc             2   4538  2269.0 1908.698  < 2e-16 ***
#   till:cc        2     16     7.9    6.607  0.00136 ** 
#   Residuals   9978  11862     1.2                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukoutsoc <- TukeyHSD(effectsoc)
cldsoc<- multcompView::multcompLetters4(effectsoc, Tukoutsoc)
cldsoc<- as.data.frame.list(cldsoc$`till:cc`)
ghgann <- arrange(ghgann, desc(mean))
ghgann$cld <- cldsoc$Letters

windows(xpinch=200, ypinch=200, width=5, height=5)

pal2 <- c("#20243d", "#669947")  #  "#C2e4ef",

ggplot(ghgann, aes(x=till, y=-1*mean.ac)) +
  geom_bar(stat="identity", position=position_dodge(), aes(fill=till), width=0.6) + #, color="gray20") + color="#33bbee"
  geom_errorbar(aes(ymin= -1*mean.ac-se.ac, ymax=-1*mean.ac+se.ac),  
                width=0.3, position=position_dodge(0.9), color="#20243d") +
  scale_fill_manual(values=pal2) + 
  # letters
  # geom_text(aes(label=cld),
  #           nudge_y =ifelse(ghgann$mean.ac>0, +0.08, -0.08),
  #           color="#20243d", size=7, fontface="bold") +
  # zero-line
  geom_hline(yintercept=0, color="#20243d", linewidth=0.5) +
  # make it pretty
  facet_grid(cols=vars(factor(cc, levels=c("NC", "TC", "LC"))),  # factor(till, levels=c("CT", "NT"))), 
             # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             labeller = as_labeller(
               c("NC"="No Cover Crop", "TC" = "Triticale Cover", "LC"="Legume Cover"))) +
                 #"CT" = "Conventional Till",  "NT" = "No Till"))) +  #, "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recomm. N"))) +
  # scale_fill_manual(values=c("#eaeccc", "#FEDA8B", "#FDB366", "#F67E4B", "#DD3D2D", "#A50026"),
  #                   name="Decade")+ #, name="N management") +
  xlab("Decade") +
  ylab(expression(bold('Mean change in SOC (CO'[2]*'e per acre)'))) +
  scale_y_continuous(breaks=seq(-0.1, 0.7, 0.1), limits=c(-0.15, 0.75)) +
  #scale_y_continuous(breaks=seq(-3,2,1), limits=c(-3,2.8)) +
  
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray97'),
    axis.text.x=element_text(angle=-30, hjust=0.5, vjust=0, size=12, face="bold"),
    axis.text.y=element_text(size=12, face="bold"),
    plot.margin = unit(c(0.1,1,0.1,0.1), "cm"),
    axis.title=element_text(size=13, face="bold"),
    strip.text=element_text(face="bold", size=11))



ggsave("plots/ghgs/CA_alm_simulation mean dSOC 2022-72 RCP60_no letters.png", width=7, height=4, dpi=300)













######################### plot of 2022 "current" by practice - mean across crops
#########################
# plot of 2022 "current" by practice - mean across crops
# if you want crop specific it looks like this:
# ghgdatl <- ghgdat[,c(1:2,5:12)] %>%
#   group_by(crop_name, year, till, cc, nfert, decade) %>%
#   summarize(net.mean=mean(ghg),  # mean and se across sites
#             net.se=se(ghg),
#             ghgdsoc.mean = mean(ghg_dsoc),
#             ghgdsoc.se=se(ghg_dsoc),
#             ghgn2o.mean = mean(ghg_total_n2o),
#             ghgn2o.se = se(ghg_total_n2o)
#   ) %>%
#   melt(id=c("year",  "crop_name", "till", "cc", "nfert", "decade")) %>%
#   rename(var1=variable) %>%
#   mutate(mean.se = ifelse(grepl(pattern="mean", x=var1), "mean", "se"),
#          var2 = ifelse(mean.se=="mean", gsub(pattern = ".mean", replacement="", x=var1), 
#                        gsub(pattern = ".se", replacement="", x=var1))) %>%
#   dplyr::select(-var1) %>%
#   dcast(year + crop_name + till + cc + nfert + decade +var2 ~mean.se)

# mean across crops WITH NFERT LEVELS
ghgdatl_cropmean <- ghgdat[,c(1:2,5:12)] %>%
  group_by(year, till, cc, nfert, decade) %>%
  summarize(net.mean=mean(ghg),  # mean and se across sites
            net.se=se(ghg),
            ghgdsoc.mean = mean(ghg_dsoc),
            ghgdsoc.se=se(ghg_dsoc),
            ghgn2o.mean = mean(ghg_total_n2o),
            ghgn2o.se = se(ghg_total_n2o)
  ) %>%
  melt(id=c("year",  "till", "cc", "nfert", "decade")) %>%
  rename(var1=variable) %>%
  mutate(mean.se = ifelse(grepl(pattern="mean", x=var1), "mean", "se"),
         var2 = ifelse(mean.se=="mean", gsub(pattern = ".mean", replacement="", x=var1), 
                       gsub(pattern = ".se", replacement="", x=var1))) %>%
  dplyr::select(-var1) %>%  # select is used by other packages too and doesn't work here without dplyr::
  dcast(year + till + cc + nfert + decade +var2 ~mean.se)

# make the plot "current" / 2022 by practice WITH NFERT levels
ggplot(data=ghgdatl_cropmean[ghgdatl_cropmean$till %in% c("CT", "NT") & 
                               ghgdatl_cropmean$year == 2022 & 
                               ghgdatl_cropmean$var2 %in% c("ghgdsoc", "ghgn2o"),],
       aes(x=nfert, y=mean)) +
  geom_bar(aes(fill= var2), stat="identity", position= "stack", alpha=0.7, show.legend=F) +
  scale_fill_manual(values=c("#ee8866","#99ddff"), 
                    breaks=c("ghgn2o", "ghgdsoc"), 
                    name = "Source/Sink", 
                    labels=c(expression('Total N'[2]*'O emissions', "Change in SOC"))) + #, labels=c("ghg_dsoc", "ghg_tn2o")) +
  geom_hline(yintercept=0, color="#009988", linewidth=0.5) +
  geom_point(data=ghgdatl_cropmean[ghgdatl_cropmean$var2 == "net" &
                                     ghgdatl_cropmean$till %in% c("CT", "NT") & 
                                     ghgdatl_cropmean$year == 2022 ,], # 
             aes(x=nfert, y=mean),
             size = 0.5, color="gray25") +
  geom_errorbar(data=ghgdatl_cropmean[ghgdatl_cropmean$till %in% c("CT", "NT") & 
                                        ghgdatl_cropmean$year == 2022,], 
                aes(x=nfert, y=mean, ymin=mean-se, ymax=mean+se, color=var2),
                width = 0.2, show.legend=F) +
  labs(x="N management", 
       y=expression('2022 mean annual emissions (tonnes CO'[2]*'e ha'^'-1'*')')) +
  scale_color_manual(values=c("#0077BB", "#882255", "gray30"), breaks=c("ghgdsoc", "ghgn2o", "net")) +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT"))),   # set the order of facets here. setting the factor to ordered doesn't affect it here
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c("NC"="No Cover Crop","CC"="Has Cover Crop", 
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    legend.text.align=0,
    axis.text.x=element_text(angle=-20, hjust=0))

ggsave("plots/ghgs/CA_alm_simulations net annual em 2022.png", width=6, height=4, dpi=300)


# are the net effects in the bars above significantly different?

# need mean of data across corn-soy
ghgdat_cropmean <- ghgdat[,c(1:2,5:12)] %>%
  group_by(site_name, year, till, cc, nfert, decade) %>%
  summarize(net.mean=mean(ghg),  # mean and se across sites
            ghgdsoc.mean = mean(ghg_dsoc),
            ghgn2o.mean = mean(ghg_total_n2o))

netlm <- lm(net.mean~till*cc*nfert, data=ghgdat_cropmean[ghgdat_cropmean$year==2022,])
summary(netlm)




# mean across crops WITHOUT NFERT LEVELS
ghgdatl_cropmean <- ghgdat[ghgdat$year==2022 & ghgdat$till %in% c("NT", "CT"),c(1:2,5:12)] %>%
  group_by(year, till, cc, decade) %>%
  summarize(net.mean=mean(ghg),  # mean and se across sites
            net.se=se(ghg),
            ghgdsoc.mean = mean(ghg_dsoc),
            ghgdsoc.se=se(ghg_dsoc),
            ghgn2o.mean = mean(ghg_total_n2o),
            ghgn2o.se = se(ghg_total_n2o)
  ) %>%
  melt(id=c("year",  "till", "cc", "decade")) %>%
  rename(var1=variable) %>%
  mutate(mean.se = ifelse(grepl(pattern="mean", x=var1), "mean", "se"),
         var2 = ifelse(mean.se=="mean", gsub(pattern = ".mean", replacement="", x=var1), 
                       gsub(pattern = ".se", replacement="", x=var1))) %>%
  dplyr::select(-var1) %>%  # select is used by other packages too and doesn't work here without dplyr::
  dcast(year + till + cc +decade +var2 ~mean.se)


# first, find the diffs among n2o bars, soc bars, net bars

effectn2o <- aov(ghg_total_n2o ~till*cc, data=ghgdat[ghgdat$till %in% c("NT", "CT") & ghgdat$year==2022,])  # ghgdat$crop=="corn" &
effectsoc <- aov(ghg_dsoc ~till*cc, data=ghgdat[ghgdat$till %in% c("NT", "CT") & ghgdat$year==2022,])  # ghgdat$crop=="corn" &
effectnet <- aov(ghg ~till*cc, data=ghgdat[ghgdat$till %in% c("NT", "CT") & ghgdat$year==2022,])  # ghgdat$crop=="corn" &

summary(effectn2o)
#               Df Sum Sq Mean Sq F value   Pr(>F)    
# till          1   0.44   0.442   5.173   0.0232 *  
# cc            1   3.34   3.338  39.105 6.68e-10 ***
# till:cc       1   0.26   0.259   3.040   0.0817 .  
# Residuals   764  65.21   0.085                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(effectsoc)
#               Df Sum Sq Mean Sq F value  Pr(>F)    
# till          1    5.6     5.6   9.971 0.00165 ** 
# cc            1  324.3   324.3 573.112 < 2e-16 ***
# till:cc       1    2.1     2.1   3.799 0.05166 .  
# Residuals   764  432.4     0.6                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(effectnet)
#               Df Sum Sq Mean Sq F value  Pr(>F)    
# till          1    8.7     8.7   9.497 0.00213 ** 
# cc            1  393.5   393.5 428.791 < 2e-16 ***
# till:cc       1    4.0     4.0   4.317 0.03807 *  
# Residuals   764  701.2     0.9                    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukoutn2o <- TukeyHSD(effectn2o)
# # put interaction output into a dataframe we can sort
# Tukout <- as.data.frame(Tukout[7]) %>%  # [7] is the 3-way interaction term
#   rownames_to_column(., "term") %>%
#   arrange(term)

Tukoutsoc <- TukeyHSD(effectsoc)
Tukoutnet <- TukeyHSD(effectnet)


# compact letter display (cld)  ~ letters for bars
cldn2o<- multcompView::multcompLetters4(effectn2o, Tukoutn2o)
cldsoc<- multcompView::multcompLetters4(effectsoc, Tukoutsoc)
cldnet<- multcompView::multcompLetters4(effectnet, Tukoutnet)

# table with letters n2o
ghgn2osum <- filter(ghgdat,till %in% c("NT", "CT"), year==2022) %>%  # crop=="corn",
  group_by(cc, till) %>%
  summarize(mean.n2o=mean(ghg_total_n2o), 
            se.n2o=se(ghg_total_n2o)) %>%
  arrange(desc(mean.n2o))

cldn2o<- as.data.frame.list(cldn2o$`till:cc`)
ghgn2osum$cldn2o <- cldn2o$Letters


# table with letters dsoc
ghgsocsum <- filter(ghgdat,till %in% c("NT", "CT"), year==2022) %>%  # crop=="corn",
  group_by(cc, till) %>%
  summarize(mean.soc=mean(ghg_dsoc), 
            se.soc=se(ghg_dsoc)) %>%
  arrange(desc(mean.soc))

cldsoc<- as.data.frame.list(cldsoc$`till:cc`)
ghgsocsum$cldsoc <- cldsoc$Letters

# table with letters net
ghgnetsum <- filter(ghgdat,till %in% c("NT", "CT"), year==2022) %>%  # crop=="corn",
  group_by(cc, till) %>%
  summarize(mean.net=mean(ghg), 
            se.net=se(ghg)) %>%
  arrange(desc(mean.net))

cldnet<- as.data.frame.list(cldnet$`till:cc`)
ghgnetsum$cldnet <- cldnet$Letters

# put the summaries together
ghgsummary <- left_join(ghgsocsum, ghgn2osum) %>%
  left_join(.,ghgnetsum)
# clean up
rm(ghgsocsum, ghgn2osum, ghgnetsum, cldn2o, cldnet, cldsoc)
ghgsummary$year <- rep(2022, nrow(ghgsummary))

# dummy data for letter labels
# cc <- c("NC","NC","CC","CC")
# till <- c("CT", "NT", "CT", "NT")
# xs <- rep(3.5, 4)
# n2o.ys <- c(1.2, 1.8, 1.3, 2)
# soc.ys <- c(-1, -1.1, -2.5, -2.5)
# net.ys <- c(0.6, 1, -0.9, -0.1)
# ghgsummary <- left_join(ghgsummary,data.frame(cc=cc, till=till, xs=xs, n2o.ys = n2o.ys, soc.ys=soc.ys, net.ys=net.ys) )
# rm(cc, till, xs, n2o.ys, soc.ys, net.ys)



# make the plot "current" / 2022 by practice WITHOUT NFERT levels
ggplot() +
  geom_bar(data=ghgdatl_cropmean[ghgdatl_cropmean$var2 %in% c("ghgdsoc", "ghgn2o"),],
           aes(x=factor(year), y=mean, fill=var2),
           stat="identity", position= "stack", alpha=0.7, show.legend=F, width=0.6) +
  scale_fill_manual(values=c("#ee8866","#99ddff"), 
                    breaks=c("ghgn2o", "ghgdsoc"), 
                    name = "Source/Sink", 
                    labels=c(expression('Total N'[2]*'O emissions', "Change in SOC"))) + #, labels=c("ghg_dsoc", "ghg_tn2o")) +
  geom_hline(yintercept=0, color="#009988", linewidth=0.5) +
  geom_point(data=ghgdatl_cropmean[ghgdatl_cropmean$var2 == "net" ,], # 
             aes(x=factor(year), y=mean),
             size = 0.5, color="gray25") +
  geom_errorbar(data=ghgdatl_cropmean, 
                aes(x=factor(year), y=mean, ymin=mean-se, ymax=mean+se, color=var2),
                width = 0.2, show.legend=F) +
  geom_text(data=ghgsummary, aes(x=factor(year), label=cldnet, y=mean.net+0.4),
            color="black", size=6, fontface="bold.italic")+
  geom_text(data=ghgsummary, aes(x=factor(year), label=cldn2o, y=mean.n2o+0.4),
            color="#882255", size=6, fontface="bold", alpha=0.7)+
  geom_text(data=ghgsummary, aes(x=factor(year), label=cldsoc, y=mean.soc-0.25),
            color="#0077BB", size=6, fontface="bold", alpha=0.7)+
  labs(x="Year", 
       y=expression(bold('Mean annual emissions (tonnes CO'[2]*'e ha'^'-1'*')'))) +
  scale_color_manual(values=c("#0077BB", "#882255", "gray30"), breaks=c("ghgdsoc", "ghgn2o", "net")) +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT"))),   # set the order of facets here. setting the factor to ordered doesn't affect it here
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             #factor(nfert, levels=c("Fall N", "High N", "Recommended N"))), 
             labeller = as_labeller(
               c("NC"="No Cover Crop","CC"="Has Cover Crop", 
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recommended N"))) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    legend.text.align=0,
    axis.text=element_text(size=12, face="bold"),
    axis.title=element_text(size=12, face="bold"))

ggsave("plots/ghgs/CA_alm_simulations net annual em 2022 withletters.png", width=4, height=5, dpi=300)


















# are the group means significantly different?

n2odat <- ghganndecl[ghganndecl$variable=="n2o_tot",c(1:4,6)]

Neffect <- aov(value~till*cc*nfert, data=n2odat)
summary(Neffect)
Tukout <- TukeyHSD(Neffect)
# put interaction output into a dataframe we can sort
Tukout <- as.data.frame(Tukout[7]) %>%
  rownames_to_column(., "term") %>%
  arrange(term)


# assumptions - following https://statsandr.com/blog/anova-in-r

# Independence - the observations (grain.c.kgc.ha. observations) are independent, one does
# not depend on the other, they're not dependent on some other variable like, 
# from one individual came multiple observations.

# Normality - not required with large enough sample size >30.  But we can test anyway. 
# the residuals (observed - mean for that group) should be normally distributed.
qqnorm(n2odat$value)
qqline(n2odat$value) # not the best
hist(n2odat$value-mean(n2odat$value))
# looks a bit right skewed
# use boxcox function to estimate transformation parameter using MLE.
boxcox(lm(n2odat$value~1))
# resulting lambda is about -0.25. If -0.5 do 1/sqrt(x), if 0, do log(x).
# let's try 1/sqrt(x) first.
# but negative values cannot do sqrt
# 
n2odat$value.tr <- sign(n2odat$value) * (abs(n2odat$value))^(1/3)
hist(n2odat$value.tr) #  better
qqnorm(n2odat$value.tr)
qqline(n2odat$value.tr) # much better
hist(n2odat$value.tr-mean(n2odat$value.tr))

# Equality of variances - OK
ggplot(data=n2odat, aes(x=nfert, y=value.tr)) + 
  geom_boxplot(outlier.size=0.5, outlier.alpha=0.5) +
  facet_grid(rows=vars(till), cols=vars(cc))
# looks ok, some possible outliers in all NT except nt-nc-fn
# maybe one (but close to IQR) in rt-cc-fn
# perhaps more variability in CT and less in NT

leveneTest(value.tr ~ cc*till*nfert, data=n2odat)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group  17  1.1495 0.3025
#       558  

# outliers
summary(n2odat$value.tr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.603   4.272   4.592   4.668   5.034   6.366 

# outliers via histograms
hist(n2odat$value.tr, breaks=sqrt(nrow(n2odat)))

ggplot(data=n2odat, aes(x=value.tr)) +
  geom_histogram() +
  facet_grid(rows=vars(nfert), cols=vars(till, cc))

#outliers via boxplots
b <- boxplot(value.tr~till+cc+nfert, data=n2odat) 
 ################# left off here trying to figure out what treatments have outliers.

# outliers via z-scores
n2odat$z_grainC <- scale(n2odat$value)
hist(n2odat$z_grainC)
summary(n2odat$z_grainC)
# Min.   :-2.31494  
# 1st Qu.:-0.80908  
# Median :-0.03325  
# Mean   : 0.00000  
# 3rd Qu.: 0.84105  
# Max.   : 2.40069  
# < -2 or >2 is considered rare
# < -3 or >3 is extremely rare
# < -3.29 or > -3.29 is used to detect outliers, where one out of 1000 observations will be outside
# this range if normal distribution.
# interquartile range is > -2 and <2. 
# min. and max do not reach 3 or 3.29.

## outlier summary: boxplots and z-score suggest no extreme outliers. No justification for removing.

# re-run ANOVA with the above knowledge (assumptions met)

n2odat$till <- factor(n2odat$till)
n2odat$cc <- factor(n2odat$cc)
n2odat$nfert <- factor(n2odat$nfert)

Neffect <- aov(value~till*cc*nfert, data=n2odat)
summary(Neffect)
# Df    Sum Sq   Mean Sq F value   Pr(>F)    
# till              2 1.637e+08 8.184e+07  61.284  < 2e-16 ***
#   cc                1 5.557e+07 5.557e+07  41.617 1.13e-10 ***
#   nfert             2 2.557e+09 1.279e+09 957.461  < 2e-16 ***
#   till:cc           2 1.742e+07 8.710e+06   6.522  0.00147 ** 
#   till:nfert        4 1.113e+08 2.781e+07  20.829  < 2e-16 ***
#   cc:nfert          2 6.444e+08 3.222e+08 241.278  < 2e-16 ***
#   till:cc:nfert     4 1.750e+07 4.375e+06   3.276  0.01079 *  
#   Residuals     34542 4.613e+10 1.335e+06                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukout <- TukeyHSD(Neffect)

# compact letter display
cld <- multcompLetters4(Neffect, Tukout)

# table with letters and 3rd quantile
cornsum <- group_by(n2odat, cc, till, nfert) %>%
  summarize(mean=mean(value), 
            se=se(value)) %>%
  arrange(desc(mean))

cld <- as.data.frame.list(cld$`till:cc:nfert`)
cornsum$cld <- cld$Letters

cornsum


















ggplot(data=sum_ghganndecl[sum_ghganndecl$variable %in% c("n2o_tot"),],
       aes(x=nfert, y=mean, fill=nfert)) +
  geom_bar(stat="identity", position=position_dodge(), color="#332288", show.legend=F) +
  geom_errorbar(width=0.3, aes(ymin= mean-se, ymax=mean+se),  
                position=position_dodge(0.9),
                color="#332288") +
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT", "RT"))), 
             cols=vars(factor(cc, levels=c("CC", "NC"))),
             labeller = as_labeller(
               c(CC="Has Cover Crop", NC="No Cover Crop",
                 "CT" = "Conventional Till", "NT" = "No Till", "RT"="Reduced Till"))) +
  scale_x_discrete(breaks=c("n2o_20", "n2o_30", "n2o_40", "n2o_50", "n2o_60"),
                   labels=c("2020s", "2030s", "2040s", "2050s", "2060s")) +
  xlab("Decade") +
  ylab("Sum of N2O emissions (CO2e/ha) per decade") +
  scale_fill_manual(values=c("#CC6677","#99DDFF", "#44AA99" ), name="N management") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))
