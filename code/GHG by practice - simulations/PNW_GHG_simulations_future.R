# Here we summarize future N2O emissions by DNDC simulations
# i.e., look for effects of till, cc, and Nfert on N2O emissions 2022-2072.

# packages
library(tidyverse) # has ggplot, dplyr, reshape2, etc.
library(reshape2) # for dcast / melt
library(MASS) # for boxcox

se <- function(x) sd(x) / sqrt(length(x))

# load data
ghgdat <- read.csv("data/simulations/un-weighted_resultsPNW_20240220.csv")
# as.data.frame(names(ghgdat))
# names(ghgdat) - all are calendar year sums
# 1         site_name
# 2       region_name
# 3         crop_name # hops  -- note these data were pre-processed by Regrow to weight the average GHG results 
# across the alley and crop row, for hops it was alley=0.8 and crop row=0.2
# 4        management # types:   [1] "cn_ct-bc" "cn_ct-nc" "cn_nt-bc" "cn_nt-nc" "cn_rt-bc" "cn_rt-nc" "on_ct-bc" "on_ct-nc" "on_nt-bc" "on_nt-nc"
#                                [11] "on_rt-bc" "on_rt-nc"
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


ghgdat <- ghgdat[ghgdat$year>2021 & ghgdat$climate_scenario=="rcp60",c(2,4:7, 11, 14, 18)]

ghgdat$till <- ifelse(grepl("_ct-", ghgdat$management), "CT", 
                    ifelse(grepl("_rt-", ghgdat$management), "RT", 
                           ifelse(grepl("_nt-", ghgdat$management), "NT", NA)))
# # check
# unique(ghgdat$till)

# factor for CC or NC
ghgdat$cc <- ifelse(grepl("-nc", ghgdat$management),"NC", "CC")
# # check
# unique(ghgdat$cc)

# factor for N treatment
ghgdat$nfert <- ifelse(grepl("cn_", ghgdat$management), "Conventional N", "Organic N")
# # check
# unique(ghgdat$nfert)

# factor for decade
ghgdat$decade <- ifelse(ghgdat$year <2031, "2020s",
                             ifelse(ghgdat$year>=2031 & ghgdat$year <2041, "2030s",
                                    ifelse(ghgdat$year>=2041 & ghgdat$year <2051, "2040s",
                                           ifelse(ghgdat$year>=2051 & ghgdat$year <2061, "2050s",
                                                  ifelse(ghgdat$year>=2061 & ghgdat$year <2071, "2060s", "2070s")))))





# $value is in units of cumulative tco2e/ha

# add factor for state
WA <- c("h_1", "h_2", "h_3", "h_4", "h_5", "h_6", "h_7", "h_11")
OR <- c("h_8", "h_9", "h_10", "h_12", "h_13", "h_14", "h_15", "h_16")

ghgdat$state <- ifelse(ghgdat$site_name %in% WA, "WA", 
                       ifelse(ghgdat$site_name %in% OR, "OR", "X"))
# check no X's
unique(ghgdat$state)

# make stacked bar chart of n2o, soc, net by decade 
# with letters comparing the 2022-72 mean of each treatment group (cover*till*Nfert combo) for soc, n2o, net

# first, find the diffs among n2o bars, soc bars, net bars


# hop RESULTS

ghgdat2 <- ghgdat[!ghgdat$till == "RT",]

lmsoc <- lm(ghg_dsoc ~ till*cc*nfert*state, data=ghgdat2)
summary(lmsoc)
# Call:
#   lm(formula = ghg_dsoc ~ till * cc * nfert * state, data = ghgdat2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.91294 -0.13155 -0.03452  0.13743  1.59632 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         9.399e-01  1.574e-02  59.719  < 2e-16 ***
#   tillNT                             -1.548e-01  2.226e-02  -6.957 3.82e-12 ***
#   ccNC                                5.000e-01  2.226e-02  22.464  < 2e-16 ***
#   nfertOrganic N                     -2.610e-01  2.226e-02 -11.728  < 2e-16 ***
#   stateWA                            -8.492e-01  2.226e-02 -38.154  < 2e-16 ***
#   tillNT:ccNC                        -1.053e-03  3.148e-02  -0.033    0.973    
# tillNT:nfertOrganic N              -1.308e-15  3.148e-02   0.000    1.000    
# ccNC:nfertOrganic N                -1.298e-15  3.148e-02   0.000    1.000    
# tillNT:stateWA                      1.388e-01  3.148e-02   4.411 1.05e-05 ***
#   ccNC:stateWA                       -2.022e-01  3.148e-02  -6.423 1.42e-10 ***
#   nfertOrganic N:stateWA             -1.137e-02  3.148e-02  -0.361    0.718    
# tillNT:ccNC:nfertOrganic N          2.627e-15  4.452e-02   0.000    1.000    
# tillNT:ccNC:stateWA                -5.752e-02  4.452e-02  -1.292    0.196    
# tillNT:nfertOrganic N:stateWA      -2.094e-15  4.452e-02   0.000    1.000    
# ccNC:nfertOrganic N:stateWA        -3.090e-15  4.452e-02   0.000    1.000    
# tillNT:ccNC:nfertOrganic N:stateWA  1.457e-15  6.295e-02   0.000    1.000    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.321 on 6640 degrees of freedom
# Multiple R-squared:   0.72,	Adjusted R-squared:  0.7193 
# F-statistic:  1138 on 15 and 6640 DF,  p-value: < 2.2e-16

aovsoc <- aov(ghg_dsoc ~till*cc*nfert*state, data=ghgdat2)  
aovsoc
# Call:
#   aov(formula = ghg_dsoc ~ till * cc * nfert * state, data = ghgdat2)
# 
# Terms:
#   till        cc     nfert     state   till:cc till:nfert  cc:nfert till:state
# Sum of Squares    16.7472  245.3654  118.3735 1350.6867    0.3697     0.0000    0.0000     5.0416
# Deg. of Freedom         1         1         1         1         1          1         1          1
# cc:state nfert:state till:cc:nfert till:cc:state till:nfert:state cc:nfert:state
# Sum of Squares    22.1879      0.0538        0.0000        0.3440           0.0000         0.0000
# Deg. of Freedom         1           1             1             1                1              1
# till:cc:nfert:state Residuals
# Sum of Squares               0.0000  684.2032
# Deg. of Freedom                   1      6640
# 
# Residual standard error: 0.3210026
# Estimated effects may be unbalanced

Tukoutsoc <- TukeyHSD(aovsoc)
Tukoutsoc


cldsoc<- multcompView::multcompLetters4(aovsoc, Tukoutsoc)

ghgsocsum <- group_by(ghgdat2, cc, till, nfert, state) %>%
  summarize(mean.soc=mean(ghg_dsoc), 
            se.soc=se(ghg_dsoc)) %>%
  arrange(desc(mean.soc))

cldsoc<- as.data.frame.list(cldsoc$`till:cc:nfert:state`)
ghgsocsum$cldsoc <- cldsoc$Letters

ghgsocsum$mean.ac <- ghgsocsum$mean.soc/2.471
ghgsocsum$se.ac <- ghgsocsum$se.soc/2.471

ghgsocsum
