# Here we summarize future N2O emissions by DNDC simulations
# i.e., look for effects of till, cc, and Nfert on N2O emissions 2022-2072.

# packages
library(tidyverse) # has ggplot, dplyr, reshape2, etc.
library(reshape2) # for dcast / melt
library(MASS) # for boxcox

se <- function(x) sd(x) / sqrt(length(x))

# load data
ghgdat <- read.csv("data/simulations/un-weighted_resultsNY.csv") # keep the "corn-grain"  "corn-silage" "corn-soy"    "soy-corn"   from this one
ghgdat2 <- read.csv("data/simulations/un-weighted_resultsNYalf_20240220.csv") # new alfalfa rotation data -- removed irrigation and now have 5 rotations rather than 3 so all crops growing every year
# as.data.frame(names(ghgdat))
# names(ghgdat) - all are calendar year sums
# 1         site_name
# 2       region_name
# 3         crop_name  #### this is actually the ROTATION And not the crop grown that year necessarily, see ghgdat$crop made below
unique(ghgdat$crop_name) # "alfalfa-act" "alfalfa-cta" "alfalfa-tac" "corn-grain"  "corn-silage" "corn-soy"    "soy-corn"  
unique(ghgdat2$crop_name) # "alfalfa-act"  "alfalfa-act2" "alfalfa-act3" "alfalfa-cta"  "alfalfa-cta2"
# 4        management 
unique(ghgdat$management) # "ct-cc" "ct-nc" "nt-cc" "nt-nc" "rt-cc" "rt-nc"
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



# combine the two datasets
ghgdat <- ghgdat[ghgdat$crop_name %in% c("corn-grain" , "corn-silage" ,"corn-soy"  ,  "soy-corn"),]
ghgdat2 <- ghgdat2[,-1]
ghgdat <- rbind(ghgdat, ghgdat2)
rm(ghgdat2)


ghgdat <- ghgdat[ghgdat$year>2021 & ghgdat$year<2073& ghgdat$climate_scenario=="rcp60",c(1,3:6, 10, 13, 17)]

ghgdat$till <- ifelse(grepl("ct-", ghgdat$management), "CT", 
                    ifelse(grepl("rt-", ghgdat$management), "RT", "NT"))
# # check
# unique(ghgdat$till)

# factor for CC or NC
ghgdat$cc <- ifelse(grepl("-cc", ghgdat$management), "CC", "NC")
# # check
# unique(ghgdat$cc)


# for now, not making a factor for N treatment, because that is redundant with $crop (made below)
# crop total N budgets:
# corn grain monoculture 210 kg N/ha as UAN 
# corn silage mono       210 kg N/ha (157.5 kgN as manure injected in June, 52.5 kg as UAN in July)
# corn in corn-soy       170 kg N/ha as UAN (soybean credit)
# soybean                  0 kg
# alfalfa                  0 kg
# corn in alf             25 kg N/ha as UAN
# triticale in alf        56 kg N/ha (36 kg as dairy manure injected in Fall at planting; 20 kg as UAN in May)

# ghgdat$nfert <- ifelse(grepl("-cn", ghgdat$management), "High N", 
#                      ifelse(grepl("-fn", ghgdat$management), "Fall N","Recommended N"))
# # check
# unique(ghgdat$nfert)

# factor for decade
ghgdat$decade <- ifelse(ghgdat$year <2031, "2020s",
                             ifelse(ghgdat$year>=2031 & ghgdat$year <2041, "2030s",
                                    ifelse(ghgdat$year>=2041 & ghgdat$year <2051, "2040s",
                                           ifelse(ghgdat$year>=2051 & ghgdat$year <2061, "2050s",
                                                  ifelse(ghgdat$year>=2061 & ghgdat$year <2071, "2060s", "2070s")))))


actcorn <- seq(2017,2072, 5)
ctacorn <- seq(2013, 2072, 5)
cta2corn <- seq(2014, 2072, 5)
act2corn <- seq(2015, 2072, 5)
act3corn <- seq(2016, 2072, 5)

actalf <- sort(c(seq(2014,2072, 5), seq(2015,2072,5), seq(2016,2072,5)))
ctaalf <- sort(c(seq(2015,2072, 5), seq(2016,2072,5), seq(2017,2072,5)))
cta2alf <- sort(c(seq(2016, 2072, 5), seq(2017, 2072, 5), seq(2018, 2072, 5)))
act2alf <- sort(c(2013, 2014, seq(2017, 2072, 5), seq(2018, 2072, 5), seq(2019, 2072, 5)))
act3alf <- sort(c(2013, 2014, 2015, seq(2018, 2072, 5), seq(2019, 2072, 5), seq(2020, 2072, 5)))


acttri <- seq(2018,2072, 5)
ctatri <- seq(2019, 2072, 5)
cta2tri <- seq(2015, 2072, 5)
act2tri <- seq(2016, 2072, 5)
act3tri <- seq(2017, 2072, 5)



# label data for the crop they are (depends on the rotation and the year)
ghgdat$crop <- ifelse(ghgdat$crop_name=="corn-soy" & ghgdat$year%%2 ==0, "soy cs",   # %%2 returns the remainder when divided by 2. if no remainder, then its an even number.
                      ifelse(ghgdat$crop_name=="corn-soy" & !ghgdat$year%%2 ==0, "corn grain cs",  # cs= to indicate corn-soy rotation
                             ifelse(ghgdat$crop_name=="soy-corn" & ghgdat$year%%2 ==0, "corn grain cs",
                                    ifelse(ghgdat$crop_name=="soy-corn" & !ghgdat$year%%2 ==0, "soy cs", 
                                           ifelse(ghgdat$crop_name=="corn-grain", "corn grain mono",
                                                  ifelse(ghgdat$crop_name=="corn-silage", "corn silage mono",
                                                        # corn in alfalfa rotation
                                                          ifelse(ghgdat$crop_name=="alfalfa-act" & ghgdat$year %in% actcorn, "corn alf",
                                                                ifelse(ghgdat$crop_name=="alfalfa-cta" & ghgdat$year %in% ctacorn, "corn alf",
                                                                       ifelse(ghgdat$crop_name=="alfalfa-cta2" & ghgdat$year %in% cta2corn, "corn alf",
                                                                              ifelse(ghgdat$crop_name=="alfalfa-act2" & ghgdat$year %in% act2corn, "corn alf",
                                                                                     ifelse(ghgdat$crop_name=="alfalfa-act3" & ghgdat$year %in% act3corn, "corn alf",
                                                                             # alf in alf rotation
                                                                               ifelse(ghgdat$crop_name=="alfalfa-act" & ghgdat$year %in% actalf, "alf",
                                                                                     ifelse(ghgdat$crop_name=="alfalfa-cta" & ghgdat$year %in% ctaalf, "alf",
                                                                                            ifelse(ghgdat$crop_name=="alfalfa-cta2" & ghgdat$year %in% cta2alf, "alf",
                                                                                                   ifelse(ghgdat$crop_name=="alfalfa-act2" & ghgdat$year %in% act2alf, "alf",
                                                                                                          ifelse(ghgdat$crop_name=="alfalfa-act3" & ghgdat$year %in% act3alf, "alf",
                                                                                            # triticale in alf rotation
                                                                                            ifelse(ghgdat$crop_name=="alfalfa-act" & ghgdat$year %in% acttri, "tri alf",
                                                                                                   ifelse(ghgdat$crop_name=="alfalfa-cta" & ghgdat$year %in% ctatri, "tri alf",
                                                                                                          ifelse(ghgdat$crop_name=="alfalfa-cta2" & ghgdat$year %in% cta2tri, "tri alf", 
                                                                                                                 ifelse(ghgdat$crop_name=="alfalfa-act2" & ghgdat$year %in% act2tri, "tri alf", 
                                                                                                                        ifelse(ghgdat$crop_name=="alfalfa-act3" & ghgdat$year %in% act3tri, "tri alf",   "X")))))))))))))))))))))
                                             

 
# check no Xs
# unique(ghgdat$crop)

rm(act2alf, act2corn, act2tri, act3alf, act3corn, act3tri, actalf, actcorn, acttri, cta2alf, cta2corn, cta2tri, ctaalf, ctacorn, ctatri)




# make stacked bar chart of n2o, soc, net by decade 
# with letters comparing the 2022-72 mean of each treatment group (cover*till*Nfert combo) for soc, n2o, net

# first, find the diffs among n2o bars, soc bars, net bars

lmn2o <- lm(ghg_total_n2o ~crop:till:cc, data=ghgdat) 
lmsoc <- lm(ghg_dsoc ~crop:till:cc, data=ghgdat)  
lmnet <- lm(ghg ~crop:till:cc, data=ghgdat)  

summary(lmn2o)
# Call:
#   lm(formula = ghg_total_n2o ~ crop:till:cc, data = ghgdat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.0460 -0.0899 -0.0157  0.0816  3.7781 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       1.04456    0.01019 102.488  < 2e-16 ***
#   cropalf:tillCT:ccCC              -0.72170    0.01177 -61.324  < 2e-16 ***
#   cropcorn alf:tillCT:ccCC         -0.73967    0.01441 -51.317  < 2e-16 ***
#   cropcorn grain cs:tillCT:ccCC    -0.47966    0.01441 -33.278  < 2e-16 ***
#   cropcorn grain mono:tillCT:ccCC  -0.39961    0.01441 -27.724  < 2e-16 ***
#   cropcorn silage mono:tillCT:ccCC -0.56861    0.01441 -39.449  < 2e-16 ***
#   cropsoy cs:tillCT:ccCC           -0.88217    0.01441 -61.204  < 2e-16 ***
#   croptri alf:tillCT:ccCC           0.07886    0.01441   5.471 4.50e-08 ***
#   cropalf:tillNT:ccCC              -0.62908    0.01177 -53.453  < 2e-16 ***
#   cropcorn alf:tillNT:ccCC         -0.81274    0.01441 -56.387  < 2e-16 ***
#   cropcorn grain cs:tillNT:ccCC    -0.15815    0.01441 -10.973  < 2e-16 ***
#   cropcorn grain mono:tillNT:ccCC   0.17387    0.01441  12.063  < 2e-16 ***
#   cropcorn silage mono:tillNT:ccCC -0.25532    0.01441 -17.714  < 2e-16 ***
#   cropsoy cs:tillNT:ccCC           -0.73875    0.01441 -51.254  < 2e-16 ***
#   croptri alf:tillNT:ccCC          -0.01349    0.01441  -0.936 0.349281    
# cropalf:tillRT:ccCC              -0.69113    0.01177 -58.726  < 2e-16 ***
#   cropcorn alf:tillRT:ccCC         -0.75750    0.01441 -52.554  < 2e-16 ***
#   cropcorn grain cs:tillRT:ccCC    -0.44629    0.01441 -30.963  < 2e-16 ***
#   cropcorn grain mono:tillRT:ccCC  -0.33064    0.01441 -22.939  < 2e-16 ***
#   cropcorn silage mono:tillRT:ccCC -0.52116    0.01441 -36.157  < 2e-16 ***
#   cropsoy cs:tillRT:ccCC           -0.86133    0.01441 -59.758  < 2e-16 ***
#   croptri alf:tillRT:ccCC           0.04985    0.01441   3.458 0.000544 ***
#   cropalf:tillCT:ccNC              -0.70722    0.01177 -60.093  < 2e-16 ***
#   cropcorn alf:tillCT:ccNC         -0.60622    0.01441 -42.059  < 2e-16 ***
#   cropcorn grain cs:tillCT:ccNC    -0.42471    0.01441 -29.465  < 2e-16 ***
#   cropcorn grain mono:tillCT:ccNC  -0.36595    0.01441 -25.389  < 2e-16 ***
#   cropcorn silage mono:tillCT:ccNC -0.48296    0.01441 -33.507  < 2e-16 ***
#   cropsoy cs:tillCT:ccNC           -0.83780    0.01441 -58.125  < 2e-16 ***
#   croptri alf:tillCT:ccNC           0.02799    0.01441   1.942 0.052134 .  
# cropalf:tillNT:ccNC              -0.62922    0.01177 -53.465  < 2e-16 ***
#   cropcorn alf:tillNT:ccNC         -0.66845    0.01441 -46.376  < 2e-16 ***
#   cropcorn grain cs:tillNT:ccNC    -0.23530    0.01441 -16.325  < 2e-16 ***
#   cropcorn grain mono:tillNT:ccNC   0.10807    0.01441   7.498 6.62e-14 ***
#   cropcorn silage mono:tillNT:ccNC -0.09403    0.01441  -6.524 6.94e-11 ***
#   cropsoy cs:tillNT:ccNC           -0.71604    0.01441 -49.678  < 2e-16 ***
#   croptri alf:tillNT:ccNC          -0.05422    0.01441  -3.762 0.000169 ***
#   cropalf:tillRT:ccNC              -0.67673    0.01177 -57.502  < 2e-16 ***
#   cropcorn alf:tillRT:ccNC         -0.62073    0.01441 -43.065  < 2e-16 ***
#   cropcorn grain cs:tillRT:ccNC    -0.40564    0.01441 -28.143  < 2e-16 ***
#   cropcorn grain mono:tillRT:ccNC  -0.30965    0.01441 -21.483  < 2e-16 ***
#   cropcorn silage mono:tillRT:ccNC -0.47041    0.01441 -32.636  < 2e-16 ***
#   cropsoy cs:tillRT:ccNC           -0.81562    0.01441 -56.587  < 2e-16 ***
#   croptri alf:tillRT:ccNC                NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2911 on 44022 degrees of freedom
# Multiple R-squared:  0.4905,	Adjusted R-squared:  0.4901 
# F-statistic:  1034 on 41 and 44022 DF,  p-value: < 2.2e-16

summary(lmsoc)
# Call:
#   lm(formula = ghg_dsoc ~ crop:till:cc, data = ghgdat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.4892 -0.4421  0.1057  0.6837  3.2751 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       1.76795    0.04579  38.610   <2e-16 ***
#   cropalf:tillCT:ccCC              -3.16375    0.05287 -59.836   <2e-16 ***
#   cropcorn alf:tillCT:ccCC         -4.73286    0.06476 -73.087   <2e-16 ***
#   cropcorn grain cs:tillCT:ccCC    -2.10510    0.06476 -32.508   <2e-16 ***
#   cropcorn grain mono:tillCT:ccCC  -3.34133    0.06476 -51.598   <2e-16 ***
#   cropcorn silage mono:tillCT:ccCC -3.45276    0.06476 -53.319   <2e-16 ***
#   cropsoy cs:tillCT:ccCC           -3.68778    0.06476 -56.948   <2e-16 ***
#   croptri alf:tillCT:ccCC           1.34969    0.06476  20.842   <2e-16 ***
#   cropalf:tillNT:ccCC              -2.25093    0.05287 -42.572   <2e-16 ***
#   cropcorn alf:tillNT:ccCC         -5.28277    0.06476 -81.578   <2e-16 ***
#   cropcorn grain cs:tillNT:ccCC    -2.64396    0.06476 -40.829   <2e-16 ***
#   cropcorn grain mono:tillNT:ccCC  -2.97225    0.06476 -45.899   <2e-16 ***
#   cropcorn silage mono:tillNT:ccCC -4.33916    0.06476 -67.007   <2e-16 ***
#   cropsoy cs:tillNT:ccCC           -2.87563    0.06476 -44.407   <2e-16 ***
#   croptri alf:tillNT:ccCC          -0.63961    0.06476  -9.877   <2e-16 ***
#   cropalf:tillRT:ccCC              -2.70860    0.05287 -51.228   <2e-16 ***
#   cropcorn alf:tillRT:ccCC         -4.98284    0.06476 -76.947   <2e-16 ***
#   cropcorn grain cs:tillRT:ccCC    -2.27092    0.06476 -35.068   <2e-16 ***
#   cropcorn grain mono:tillRT:ccCC  -3.09030    0.06476 -47.722   <2e-16 ***
#   cropcorn silage mono:tillRT:ccCC -3.59122    0.06476 -55.457   <2e-16 ***
#   cropsoy cs:tillRT:ccCC           -3.27836    0.06476 -50.626   <2e-16 ***
#   croptri alf:tillRT:ccCC           0.59583    0.06476   9.201   <2e-16 ***
#   cropalf:tillCT:ccNC              -3.31656    0.05287 -62.726   <2e-16 ***
#   cropcorn alf:tillCT:ccNC         -2.58267    0.06476 -39.883   <2e-16 ***
#   cropcorn grain cs:tillCT:ccNC    -1.68241    0.06476 -25.980   <2e-16 ***
#   cropcorn grain mono:tillCT:ccNC  -2.96096    0.06476 -45.724   <2e-16 ***
#   cropcorn silage mono:tillCT:ccNC -2.45467    0.06476 -37.906   <2e-16 ***
#   cropsoy cs:tillCT:ccNC           -3.19657    0.06476 -49.363   <2e-16 ***
#   croptri alf:tillCT:ccNC           0.80407    0.06476  12.417   <2e-16 ***
#   cropalf:tillNT:ccNC              -2.51361    0.05287 -47.540   <2e-16 ***
#   cropcorn alf:tillNT:ccNC         -2.87252    0.06476 -44.359   <2e-16 ***
#   cropcorn grain cs:tillNT:ccNC    -2.04542    0.06476 -31.586   <2e-16 ***
#   cropcorn grain mono:tillNT:ccNC  -2.55031    0.06476 -39.383   <2e-16 ***
#   cropcorn silage mono:tillNT:ccNC -3.46401    0.06476 -53.493   <2e-16 ***
#   cropsoy cs:tillNT:ccNC           -2.45149    0.06476 -37.857   <2e-16 ***
#   croptri alf:tillNT:ccNC          -1.12003    0.06476 -17.296   <2e-16 ***
#   cropalf:tillRT:ccNC              -2.89638    0.05287 -54.779   <2e-16 ***
#   cropcorn alf:tillRT:ccNC         -2.74499    0.06476 -42.389   <2e-16 ***
#   cropcorn grain cs:tillRT:ccNC    -1.81759    0.06476 -28.068   <2e-16 ***
#   cropcorn grain mono:tillRT:ccNC  -2.70729    0.06476 -41.807   <2e-16 ***
#   cropcorn silage mono:tillRT:ccNC -2.56675    0.06476 -39.637   <2e-16 ***
#   cropsoy cs:tillRT:ccNC           -2.83053    0.06476 -43.710   <2e-16 ***
#   croptri alf:tillRT:ccNC                NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.308 on 44022 degrees of freedom
# Multiple R-squared:  0.4696,	Adjusted R-squared:  0.4691 
# F-statistic: 950.7 on 41 and 44022 DF,  p-value: < 2.2e-16


summary(lmnet)
# Call:
#   lm(formula = ghg ~ crop:till:cc, data = ghgdat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.4836 -0.4957  0.1144  0.7563  3.9693 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       2.78409    0.04673  59.572   <2e-16 ***
#   cropalf:tillCT:ccCC              -3.88573    0.05396 -72.005   <2e-16 ***
#   cropcorn alf:tillCT:ccCC         -5.47141    0.06609 -82.784   <2e-16 ***
#   cropcorn grain cs:tillCT:ccCC    -2.58532    0.06609 -39.116   <2e-16 ***
#   cropcorn grain mono:tillCT:ccCC  -3.74313    0.06609 -56.634   <2e-16 ***
#   cropcorn silage mono:tillCT:ccCC -4.02342    0.06609 -60.875   <2e-16 ***
#   cropsoy cs:tillCT:ccCC           -4.57189    0.06609 -69.174   <2e-16 ***
#   croptri alf:tillCT:ccCC           1.42666    0.06609  21.586   <2e-16 ***
#   cropalf:tillNT:ccCC              -2.87574    0.05396 -53.289   <2e-16 ***
#   cropcorn alf:tillNT:ccCC         -6.08759    0.06609 -92.107   <2e-16 ***
#   cropcorn grain cs:tillNT:ccCC    -2.78866    0.06609 -42.193   <2e-16 ***
#   cropcorn grain mono:tillNT:ccCC  -2.78276    0.06609 -42.104   <2e-16 ***
#   cropcorn silage mono:tillNT:ccCC -4.59624    0.06609 -69.542   <2e-16 ***
#   cropsoy cs:tillNT:ccCC           -3.60250    0.06609 -54.507   <2e-16 ***
#   croptri alf:tillNT:ccCC          -0.64887    0.06609  -9.818   <2e-16 ***
#   cropalf:tillRT:ccCC              -3.39879    0.05396 -62.982   <2e-16 ***
#   cropcorn alf:tillRT:ccCC         -5.73771    0.06609 -86.813   <2e-16 ***
#   cropcorn grain cs:tillRT:ccCC    -2.71584    0.06609 -41.091   <2e-16 ***
#   cropcorn grain mono:tillRT:ccCC  -3.42030    0.06609 -51.750   <2e-16 ***
#   cropcorn silage mono:tillRT:ccCC -4.11224    0.06609 -62.219   <2e-16 ***
#   cropsoy cs:tillRT:ccCC           -4.13965    0.06609 -62.634   <2e-16 ***
#   croptri alf:tillRT:ccCC           0.64522    0.06609   9.762   <2e-16 ***
#   cropalf:tillCT:ccNC              -4.02352    0.05396 -74.558   <2e-16 ***
#   cropcorn alf:tillCT:ccNC         -3.18747    0.06609 -48.227   <2e-16 ***
#   cropcorn grain cs:tillCT:ccNC    -2.10689    0.06609 -31.878   <2e-16 ***
#   cropcorn grain mono:tillCT:ccNC  -3.32857    0.06609 -50.362   <2e-16 ***
#   cropcorn silage mono:tillCT:ccNC -2.93784    0.06609 -44.450   <2e-16 ***
#   cropsoy cs:tillCT:ccNC           -4.03534    0.06609 -61.056   <2e-16 ***
#   croptri alf:tillCT:ccNC           0.83079    0.06609  12.570   <2e-16 ***
#   cropalf:tillNT:ccNC              -3.13889    0.05396 -58.166   <2e-16 ***
#   cropcorn alf:tillNT:ccNC         -3.53484    0.06609 -53.483   <2e-16 ***
#   cropcorn grain cs:tillNT:ccNC    -2.26950    0.06609 -34.338   <2e-16 ***
#   cropcorn grain mono:tillNT:ccNC  -2.42804    0.06609 -36.737   <2e-16 ***
#   cropcorn silage mono:tillNT:ccNC -3.56479    0.06609 -53.936   <2e-16 ***
#   cropsoy cs:tillNT:ccNC           -3.15737    0.06609 -47.772   <2e-16 ***
#   croptri alf:tillNT:ccNC          -1.17029    0.06609 -17.707   <2e-16 ***
#   cropalf:tillRT:ccNC              -3.57175    0.05396 -66.187   <2e-16 ***
#   cropcorn alf:tillRT:ccNC         -3.36305    0.06609 -50.884   <2e-16 ***
#   cropcorn grain cs:tillRT:ccNC    -2.22140    0.06609 -33.610   <2e-16 ***
#   cropcorn grain mono:tillRT:ccNC  -3.01604    0.06609 -45.633   <2e-16 ***
#   cropcorn silage mono:tillRT:ccNC -3.03631    0.06609 -45.940   <2e-16 ***
#   cropsoy cs:tillRT:ccNC           -3.64548    0.06609 -55.157   <2e-16 ***
#   croptri alf:tillRT:ccNC                NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.335 on 44022 degrees of freedom
# Multiple R-squared:  0.5329,	Adjusted R-squared:  0.5325 
# F-statistic:  1225 on 41 and 44022 DF,  p-value: < 2.2e-16


aovn2o <- aov(ghg_total_n2o ~crop:till:cc, data=ghgdat)  
aovsoc <- aov(ghg_dsoc ~crop:till:cc, data=ghgdat)  
aovnet <- aov(ghg ~crop:till:cc, data=ghgdat)  

summary(aovn2o)
# Df Sum Sq Mean Sq F value Pr(>F)    
# crop:till:cc    41   3593   87.63    1034 <2e-16 ***
#   Residuals    44022   3731    0.08                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(aovsoc)
# Df Sum Sq Mean Sq F value Pr(>F)    
# crop:till:cc    41  66690  1626.6   950.7 <2e-16 ***
#   Residuals    44022  75319     1.7                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(aovnet)
# Df Sum Sq Mean Sq F value Pr(>F)    
# crop:till:cc    41  89519  2183.4    1225 <2e-16 ***
#   Residuals    44022  78458     1.8                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Tukoutn2o <- TukeyHSD(aovn2o)
Tukoutsoc <- TukeyHSD(aovsoc)
Tukoutnet <- TukeyHSD(aovnet)


# compact letter display (cld)  ~ letters for bars
cldn2o<- multcompView::multcompLetters4(aovn2o, Tukoutn2o)
cldsoc<- multcompView::multcompLetters4(aovsoc, Tukoutsoc)
cldnet<- multcompView::multcompLetters4(aovnet, Tukoutnet)

# table with letters n2o
ghgn2osum <- group_by(ghgdat, crop, cc, till) %>%
  summarize(mean.n2o=mean(ghg_total_n2o), 
            se.n2o=se(ghg_total_n2o)) %>%
  arrange(desc(mean.n2o))

cldn2o<- as.data.frame.list(cldn2o$`crop:till:cc`)
ghgn2osum$cldn2o <- cldn2o$Letters


# table with letters dsoc
ghgsocsum <- group_by(ghgdat, crop, cc, till) %>%
  summarize(mean.soc=mean(ghg_dsoc), 
            se.soc=se(ghg_dsoc)) %>%
  arrange(desc(mean.soc))

cldsoc<- as.data.frame.list(cldsoc$`crop:till:cc`)
ghgsocsum$cldsoc <- cldsoc$Letters

# table with letters net
ghgnetsum <- group_by(ghgdat, crop, cc, till) %>%
  summarize(mean.net=mean(ghg), 
            se.net=se(ghg)) %>%
  arrange(desc(mean.net))

cldnet<- as.data.frame.list(cldnet$`crop:till:cc`)
ghgnetsum$cldnet <- cldnet$Letters

# put the summaries together
ghgsummary <- left_join(ghgsocsum, ghgn2osum) %>%
  left_join(.,ghgnetsum)
# clean up
rm(ghgsocsum, ghgn2osum, ghgnetsum, cldn2o, cldnet, cldsoc, lmn2o, lmnet, lmsoc,
   Tukoutn2o, Tukoutnet, Tukoutsoc, aovn2o, aovnet, aovsoc)



# make plot with letters
windows(xpinch=200, ypinch=200, width=5, height=5)



####################### make a simpler version that is the 2022-2072 means


# convert em in tonnes / ha to tonnes /ac

ghgsummary <- mutate(ghgsummary, 
                     mean.n2o.ac=mean.n2o*2.47105,
                     se.n2o.ac=se.n2o*2.47105,
                     mean.soc.ac=mean.soc*2.47105,
                     se.soc.ac=se.soc*2.47105,
                     mean.net.ac=mean.net*2.47105,
                     se.net.ac=se.net*2.47105)


ggplot(data=ghgsummary, aes(x=crop)) +
  # n2o bars, error bars, letters
  geom_bar(aes(y=mean.n2o.ac), 
           stat="identity", position=position_dodge(), fill="#c44f2d", alpha=0.4) + #, color="gray20") +
  # geom_errorbar(aes(ymin= mean.n2o-se.n2o, ymax=mean.n2o+se.n2o),  
  #               width=0.3, position=position_dodge(0.9),color="#882255") +
  # geom_text(data=ghgsummary, aes(x=nfert, label=cldn2o, 
  #                                y=ifelse(mean.n2o<1.5, mean.n2o + 0.2*mean.n2o, mean.n2o+0.35*mean.n2o)), 
  #           vjust=-0.5, color="#882255", size=4, fontface="bold") +
  # 
  
  # dsoc bars, error bars, letters
  geom_bar(aes(y=mean.soc.ac),  
           stat="identity", position=position_dodge(), fill="#20243d", alpha=0.4) + #, color="gray20") +
  # geom_errorbar(aes(ymin= mean.soc-se.soc, ymax=mean.soc+se.soc),  
  #               width=0.3, position=position_dodge(0.9), color="#33bbee") +
  # geom_text(data=ghgsummary, aes(x=nfert, label=cldsoc, 
  #                                y=ifelse(mean.soc>-1, mean.soc + 0.5*mean.soc, mean.soc+0.35*mean.soc)), 
  #           vjust=-0.5, color="#0077BB", size=4, fontface="bold") +
  
  # zero-line
  geom_hline(yintercept=0, color="#20243d", linewidth=0.5) +
  
  # net points
  geom_point(aes(y=mean.net.ac),
             color="#20243d", size=1, position=position_dodge(0.9)) +
  # geom_errorbar(aes(ymin= mean.net-se.net, ymax=mean.net+se.net),  
  #               width=0.3, position=position_dodge(0.9), color="gray35") +
  # geom_text(data=ghgsummary, aes(label=cldnet, y=mean.net.ac-1),
  #           color="#20243d", size=4, fontface="bold.italic")+
  
  # make it pretty
  facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))),  #, "RT"
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             labeller = as_labeller(
               c("NC"="No Cover Crop", "CC"="Rye Cover Crop",
                 "CT" = "Conventional Till", "RT"= "Reduced Till", "NT" = "No Till"))) +  
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recomm. N"))) +
  # scale_fill_manual(values=c("#eaeccc", "#FEDA8B", "#FDB366", "#F67E4B", "#DD3D2D", "#A50026"),
  #                   name="Decade")+ #, name="N management") +
  
  ylab(expression('Mean annual emissions (tonnes CO'[2]*'e per acre)')) +
  # scale_y_continuous(breaks=seq(-6,5,1), limits=c(-6,5)) +
  
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-30, hjust=0, size=11))



ggsave("plots/ghgs/NY_mean em 2022-72 RCP60_no letters_20240312.png", width=10, height=8, dpi=300)






# Same graph as above but SOC only and average across N treatments for farmer report:

effectsoc <- aov(ghg_dsoc ~till*cc, data=ghgdat[ghgdat$till %in% c("NT", "CT"),])  # ghgdat$crop=="corn" &
summary(effectsoc)
# Df Sum Sq Mean Sq F value Pr(>F)    
# till            1      0     0.1   0.031  0.860    
# cc              1   1079  1079.2 333.187 <2e-16 ***
#   till:cc         1      0     0.0   0.004  0.951    
# Residuals   29372  95135     3.2                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukoutsoc <- TukeyHSD(effectsoc)
cldsoc<- multcompView::multcompLetters4(effectsoc, Tukoutsoc)
ghgsocsum <- filter(ghgdat,till %in% c("NT", "CT")) %>%  # crop=="corn",
  group_by(cc, till) %>%
  summarize(mean.soc=mean(ghg_dsoc), 
            se.soc=se(ghg_dsoc)) %>%
  arrange(desc(mean.soc)) %>%
  ungroup() %>%
  mutate(mean.soc.ac = mean.soc/2.471,
         se.soc.ac = se.soc/2.471,
         dummy = rep("x", 4))

cldsoc<- as.data.frame.list(cldsoc$`till:cc`)
ghgsocsum$cldsoc <- cldsoc$Letters
ghgsocsum
# A tibble: 4 × 8
# cc    till  mean.soc se.soc mean.soc.ac se.soc.ac dummy cldsoc
# <chr> <chr>    <dbl>  <dbl>       <dbl>     <dbl> <chr> <chr> 
#   1 NC    CT      -0.679 0.0209      -0.275   0.00846 x     a     
# 2 NC    NT      -0.681 0.0168      -0.276   0.00681 x     a     
# 3 CC    CT      -1.06  0.0242      -0.429   0.00977 x     b     
# 4 CC    NT      -1.07  0.0215      -0.431   0.00868 x     b     

mean(c(0.429, 0.431)) # 0.43
mean(c(0.276, 0.275)) # 0.2755
(0.43-0.2755)/0.2755



windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=ghgsocsum, aes(x=dummy)) +
  # dsoc bars, error bars, letters
  geom_bar(aes(y=mean.soc.ac*-1),   # Data are in terms of emissions, so -dSOC is negative emissions, 
           # multiply here by -1 to show buildup of SOC as positive.
           stat="identity", position=position_dodge(), fill="#1982be", width=0.6) + #, color="gray20") +
  geom_errorbar(aes(ymin= (-1*mean.soc.ac)-se.soc.ac, ymax=(-1*mean.soc.ac)+se.soc.ac),  
                width=0.2, position=position_dodge(0.9), color="#20243d") +
  geom_text(aes(x=dummy, label=cldsoc, y= (mean.soc.ac*-1) + 0.05),
            vjust=-0.5, color="#0077BB", size=4, fontface="bold") +
  # 
  # zero-line
  # geom_hline(yintercept=0, color="#20243d", linewidth=0.5) +
  
  
  # make it pretty
  facet_grid(cols=vars(factor(cc, levels=c("NC", "CC")), factor(till, levels=c("CT", "NT"))), 
             # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             labeller = as_labeller(
               c("NC"="No Cover Crop", "CC"="Has Cover Crop",
                 "CT" = "Conventional Till", "NT" = "No Till"))) +  #, "RT"="Reduced Till"))) +
  #"Fall N" = "Fall N", "High N" = "High N", "Recommended N"="Recomm. N"))) +
  # scale_fill_manual(values=c("#eaeccc", "#FEDA8B", "#FDB366", "#F67E4B", "#DD3D2D", "#A50026"),
  #                   name="Decade")+ #, name="N management") +
  
  ylab(expression('Mean annual change in SOC (tonnes CO'[2]*'e per acre)')) +
  
  # scale_y_continuous(breaks=seq(-6,5,1), limits=c(-6,5)) +
  
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'))

ggsave("plots/ghgs/NY_simulation mean annual SOC buildup_with letters.png", width=5, height=3, dpi=300)






# mean annual emissions by decade by rotation

ghgdec <- ghgdat %>%
  group_by(crop, till, cc, decade) %>%  #  calculate means and se by decade
  summarize(net_mean=mean(ghg),
            net_se=se(ghg),
            ghgdsoc_mean = mean(ghg_dsoc),
            ghgdsoc_se=se(ghg_dsoc),
            ghgn2o_mean = mean(ghg_total_n2o),
            ghgn2o_se = se(ghg_total_n2o)
  ) %>%
  pivot_longer(cols=-c("crop", "till", "cc", "decade"),
               names_to=c("component", ".value"),
               names_sep="_")


       
ggplot() +
  # n2o bars, error bars, letters
  geom_bar(# data=ghgdec[ghgdec$crop == "corn" & ghgdec$var=="ghgn2o" & ghgdec$till %in% c("CT", "NT"),], 
           data=ghgdec[ghgdec$component == "ghgn2o",],# need separate call for bars for n2o, for soc, and points for net
           aes(x=crop, y=mean, group=decade), 
           stat="identity", position=position_dodge(), fill="#ee8866", alpha=0.7, color="#bb5566") + #, color="gray20") +
  geom_errorbar(data=ghgdec[ghgdec$component=="ghgn2o",], # ghgdec$crop == "corn" &
                aes(x=crop, ymin= mean-se, ymax=mean+se, group=decade),  
                width=0.3, position=position_dodge(0.9),color="#882255") +
  #geom_bar(data=ghgsummary, aes(x=nfert, y=mean.n2o), stat="identity", color=NA, fill=NA) +
  # geom_text(data=ghgsummary, aes(x=nfert, label=cldn2o, 
                                 # y=ifelse(mean.n2o<1.5, mean.n2o + 0.2*mean.n2o, mean.n2o+0.35*mean.n2o)), 
            # vjust=-0.5, color="#882255", size=4, fontface="bold") +
 
  
  # dsoc bars, error bars, letters
  geom_bar(data=ghgdec[ghgdec$component == "ghgdsoc",],  # need separate call for bars for n2o, for soc, and points for net

    aes(x=crop, y=mean, group=decade),  
    stat="identity", position=position_dodge(), fill="#99ddff", alpha=0.7, color="#33bbee") + #, color="gray20") +
  geom_errorbar(data=ghgdec[ghgdec$component=="ghgdsoc" ,], 
               aes(x=crop, ymin= mean-se, ymax=mean+se, group=decade),  
               width=0.3, position=position_dodge(0.9), color="#33bbee") +
  #geom_bar(data=ghgsummary, aes(x=nfert, y=mean.soc), stat="identity", color=NA, fill=NA) +
  # geom_text(data=ghgsummary, aes(x=nfert, label=cldsoc, 
                                 # y=ifelse(mean.soc>-1, mean.soc + 0.5*mean.soc, mean.soc+0.35*mean.soc)), 
            # vjust=-0.5, color="#0077BB", size=4, fontface="bold") +
  
  # zero-lin
  geom_hline(yintercept=0, color="#009988", linewidth=0.5) +
  
  # net points letters
  # geom_line(data=ghgdec[ghgdec$var == "net" &
  #                             ghgdec$till %in% c("CT", "NT"),],
  #            aes(x=nfert, y=mean),
  #            alpha=0.6, linewidth=1, position=position_dodge(0.9)) +  # color="gray25", 
  geom_point(data=ghgdec[ghgdec$component == "net",],
             aes(x=crop, y=mean, group=decade),
             color="gray25", size=0.5, position=position_dodge(0.9)) +
  geom_errorbar(data=ghgdec[ghgdec$component=="net" ,], 
              aes(x=crop, ymin= mean-se, ymax=mean+se, group=decade),  
              width=0.3, position=position_dodge(0.9), color="gray35") +
  # geom_text(data=ghgsummary, aes(x=nfert, label=cldnet, y=mean.net-0.3),
  #                                color="black", size=4, fontface="bold.italic")+

  # make it pretty
  facet_grid(rows=vars(factor(till, levels=c("CT", "RT", "NT"))),  #, "RT"
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
                      # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             labeller = as_labeller(
               c("NC"="No Cover Crop", "CC"="Rye Cover Crop",
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
    panel.background = element_rect(fill = 'gray95'),
    axis.text.x=element_text(angle=-30, hjust=0, size=11))
  
  

ggsave("plots/ghgs/NY_simulation mean em by decade RCP60_no letters.png", width=6, height=5, dpi=300)






################ make a simpler version averaged across N treatments



# mean annual n2o emissions by decade

ghgdec2 <- ghgdat %>%
  group_by(till, cc, decade) %>%  # , crop # calculate means and se across all sites and 2 crops and 3 Nferts
  # showing corn-soy mean rather than just corn because 50 years of just corn data is like continuous corn rotation on a ha
  # which most ha are not
  summarize(net.mean=mean(ghg),
            net.se=se(ghg),
            ghgdsoc.mean = mean(ghg_dsoc),
            ghgdsoc.se=se(ghg_dsoc),
            ghgn2o.mean = mean(ghg_total_n2o),
            ghgn2o.se = se(ghg_total_n2o)
  ) %>%
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
  geom_bar(# data=ghgdec2[ghgdec2$crop == "corn" & ghgdec2$var=="ghgn2o" & ghgdec2$till %in% c("CT", "NT"),], 
    data=ghgdec2[ghgdec2$var == "ghgn2o" &  # need separate call for bars for n2o, for soc, and points for net
                     ghgdec2$till %in% c("CT", "NT"),], # for AGU only showing CT and NT not RT to simplify a bit
    aes(x=decade, y=mean), 
    stat="identity", position=position_dodge(), fill="#ee8866", alpha=0.4) + #, color="gray20") + color="#bb5566"
  geom_errorbar(data=ghgdec2[ghgdec2$var=="ghgn2o" & ghgdec2$till %in% c("CT", "NT"),], # ghgdec2$crop == "corn" &
                aes(x=decade, ymin= mean-se, ymax=mean+se),  
                width=0.3, position=position_dodge(0.9),color="#882255") +
  #geom_bar(data=ghgsummary, aes(x=decade, y=mean.n2o), stat="identity", color=NA, fill=NA) +
  geom_text(data=ghgsummary, aes(x=xs, label=cldn2o, y=n2o.ys),
            color="#882255", size=7, fontface="bold") +  #  vjust=-0.5,

  
  # dsoc bars, error bars, letters
  geom_bar(data=ghgdec2[ghgdec2$var == "ghgdsoc" &  # need separate call for bars for n2o, for soc, and points for net
                            ghgdec2$till %in% c("CT", "NT"),], # for AGU only showing CT and NT not RT to simplify a bit
           aes(x=decade, y=mean),  
           stat="identity", position=position_dodge(), fill="#99ddff", alpha=0.4) + #, color="gray20") + color="#33bbee"
  geom_errorbar(data=ghgdec2[ghgdec2$var=="ghgdsoc" & ghgdec2$till %in% c("CT", "NT"),], 
                aes(x=decade, ymin= mean-se, ymax=mean+se),  
                width=0.3, position=position_dodge(0.9), color="#0077BB") +
  #geom_bar(data=ghgsummary, aes(x=decade, y=mean.soc), stat="identity", color=NA, fill=NA) +
  geom_text(data=ghgsummary, aes(x=xs, label=cldsoc, y=soc.ys),
            vjust=-0.5, color="#0077BB", size=7, fontface="bold") +
  
  # zero-line
  geom_hline(yintercept=0, color="#009988", linewidth=0.5) +
  
  # net points letters
  geom_line(data=ghgdec2[ghgdec2$var == "net" &
                              ghgdec2$till %in% c("CT", "NT"),],
             aes(x=decade, y=mean, group=1),
             alpha=0.7, linewidth=1, position=position_dodge(0.9)) +  # color="gray25",
  geom_point(data=ghgdec2[ghgdec2$var == "net" &
                              ghgdec2$till %in% c("CT", "NT"),],
             aes(x=decade, y=mean),
             color="black", size=1, position=position_dodge(0.9)) +
  geom_errorbar(data=ghgdec2[ghgdec2$var=="net" & 
                                 ghgdec2$till %in% c("CT", "NT"),], 
                aes(x=decade, ymin= mean-se, ymax=mean+se),  
                width=0.3, position=position_dodge(0.9), color="gray35") +
  geom_text(data=ghgsummary, aes(x=xs, label=cldnet, y=net.ys),
            color="black", size=7, fontface="bold.italic")+
  
  # make it pretty
  facet_grid(rows=vars(factor(till, levels=c("CT", "NT"))),  #, "RT"
             cols=vars(factor(cc, levels=c("NC", "CC"))), 
             # factor(nfert, levels=c("Fall N", "High N", "Recommended N"))),  
             labeller = as_labeller(
               c("NC"="No Cover Crop", "CC"="Has Cover Crop",
                 "CT" = "Conventional Till", "NT" = "No Till"))) +  #, "RT"="Reduced Till"))) +
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



ggsave("plots/ghgs/IL_simulation mean (crop, site, decade, no nfert) annual em by decadeRCP60_with letters.png", width=7, height=8, dpi=300)


















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

ggsave("plots/ghgs/IL_simulations net annual em 2022.png", width=6, height=4, dpi=300)


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

ggsave("plots/ghgs/IL_simulations net annual em 2022 withletters.png", width=4, height=5, dpi=300)


















# are the group means significantly different?

n2odat <- ghgdecl[ghgdecl$variable=="n2o_tot",c(1:4,6)]

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


















ggplot(data=sum_ghgdecl[sum_ghgdecl$variable %in% c("n2o_tot"),],
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
