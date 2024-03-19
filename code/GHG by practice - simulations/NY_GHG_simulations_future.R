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
#   Min       1Q   Median       3Q      Max 
# -0.78419 -0.06848 -0.01012  0.05543  1.35415 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       0.181747   0.006879  26.420  < 2e-16 ***
#   cropalf:tillCT:ccCC               0.087006   0.007922  10.983  < 2e-16 ***
#   cropcorn-grain cs:tillCT:ccCC     0.383154   0.008670  44.195  < 2e-16 ***
#   cropcorn-grain mono:tillCT:ccCC   0.463204   0.008670  53.429  < 2e-16 ***
#   cropcorn-silage mono:tillCT:ccCC  0.294202   0.008670  33.935  < 2e-16 ***
#   cropcorn alf:tillCT:ccCC          0.119791   0.009650  12.414  < 2e-16 ***
#   cropsoy cs:tillCT:ccCC           -0.019360   0.008670  -2.233 0.025547 *  
#   croptri alf:tillCT:ccCC           0.067727   0.009729   6.962 3.42e-12 ***
#   cropalf:tillNT:ccCC               0.184898   0.007922  23.340  < 2e-16 ***
#   cropcorn-grain cs:tillNT:ccCC     0.704655   0.008670  81.280  < 2e-16 ***
#   cropcorn-grain mono:tillNT:ccCC   1.036684   0.008670 119.578  < 2e-16 ***
#   cropcorn-silage mono:tillNT:ccCC  0.607492   0.008670  70.072  < 2e-16 ***
#   cropcorn alf:tillNT:ccCC          0.044065   0.009650   4.566 4.98e-06 ***
#   cropsoy cs:tillNT:ccCC            0.124056   0.008670  14.309  < 2e-16 ***
#   croptri alf:tillNT:ccCC          -0.037716   0.009729  -3.877 0.000106 ***
#   cropalf:tillRT:ccCC               0.120432   0.007922  15.203  < 2e-16 ***
#   cropcorn-grain cs:tillRT:ccCC     0.416521   0.008670  48.044  < 2e-16 ***
#   cropcorn-grain mono:tillRT:ccCC   0.532169   0.008670  61.384  < 2e-16 ***
#   cropcorn-silage mono:tillRT:ccCC  0.341648   0.008670  39.408  < 2e-16 ***
#   cropcorn alf:tillRT:ccCC          0.100312   0.009650  10.395  < 2e-16 ***
#   cropsoy cs:tillRT:ccCC            0.001479   0.008670   0.171 0.864581    
# croptri alf:tillRT:ccCC           0.034143   0.009729   3.509 0.000450 ***
#   cropalf:tillCT:ccNC               0.107928   0.007922  13.624  < 2e-16 ***
#   cropcorn-grain cs:tillCT:ccNC     0.438103   0.008670  50.534  < 2e-16 ***
#   cropcorn-grain mono:tillCT:ccNC   0.496855   0.008670  57.311  < 2e-16 ***
#   cropcorn-silage mono:tillCT:ccNC  0.379849   0.008670  43.814  < 2e-16 ***
#   cropcorn alf:tillCT:ccNC          0.248129   0.009650  25.713  < 2e-16 ***
#   cropsoy cs:tillCT:ccNC            0.025007   0.008670   2.884 0.003924 ** 
#   croptri alf:tillCT:ccNC           0.027791   0.009729   2.857 0.004284 ** 
#   cropalf:tillNT:ccNC               0.192678   0.007922  24.322  < 2e-16 ***
#   cropcorn-grain cs:tillNT:ccNC     0.627512   0.008670  72.381  < 2e-16 ***
#   cropcorn-grain mono:tillNT:ccNC   0.970879   0.008670 111.988  < 2e-16 ***
#   cropcorn-silage mono:tillNT:ccNC  0.768781   0.008670  88.676  < 2e-16 ***
#   cropcorn alf:tillNT:ccNC          0.183255   0.009650  18.990  < 2e-16 ***
#   cropsoy cs:tillNT:ccNC            0.146768   0.008670  16.929  < 2e-16 ***
#   croptri alf:tillNT:ccNC          -0.058367   0.009729  -5.999 2.00e-09 ***
#   cropalf:tillRT:ccNC               0.142649   0.007922  18.007  < 2e-16 ***
#   cropcorn-grain cs:tillRT:ccNC     0.457171   0.008670  52.733  < 2e-16 ***
#   cropcorn-grain mono:tillRT:ccNC   0.553160   0.008670  63.805  < 2e-16 ***
#   cropcorn-silage mono:tillRT:ccNC  0.392400   0.008670  45.262  < 2e-16 ***
#   cropcorn alf:tillRT:ccNC          0.232699   0.009650  24.114  < 2e-16 ***
#   cropsoy cs:tillRT:ccNC            0.047185   0.008670   5.443 5.29e-08 ***
#   croptri alf:tillRT:ccNC                 NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1507 on 34230 degrees of freedom
# Multiple R-squared:  0.7546,	Adjusted R-squared:  0.7543 
# F-statistic:  2567 on 41 and 34230 DF,  p-value: < 2.2e-16

summary(lmsoc)
# Call:
#   lm(formula = ghg_dsoc ~ crop:till:cc, data = ghgdat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.3759 -0.4115  0.0662  0.5782  3.3633 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       2.09698    0.05487  38.221  < 2e-16 ***
#   cropalf:tillCT:ccCC              -3.63995    0.06318 -57.612  < 2e-16 ***
#   cropcorn-grain cs:tillCT:ccCC    -2.43413    0.06914 -35.204  < 2e-16 ***
#   cropcorn-grain mono:tillCT:ccCC  -3.67037    0.06914 -53.083  < 2e-16 ***
#   cropcorn-silage mono:tillCT:ccCC -3.78180    0.06914 -54.694  < 2e-16 ***
#   cropcorn alf:tillCT:ccCC         -5.01759    0.07696 -65.195  < 2e-16 ***
#   cropsoy cs:tillCT:ccCC           -4.01682    0.06914 -58.093  < 2e-16 ***
#   croptri alf:tillCT:ccCC           1.40804    0.07759  18.147  < 2e-16 ***
#   cropalf:tillNT:ccCC              -2.66810    0.06318 -42.230  < 2e-16 ***
#   cropcorn-grain cs:tillNT:ccCC    -2.97300    0.06914 -42.997  < 2e-16 ***
#   cropcorn-grain mono:tillNT:ccCC  -3.30129    0.06914 -47.745  < 2e-16 ***
#   cropcorn-silage mono:tillNT:ccCC -4.66819    0.06914 -67.514  < 2e-16 ***
#   cropcorn alf:tillNT:ccCC         -5.54264    0.07696 -72.017  < 2e-16 ***
#   cropsoy cs:tillNT:ccCC           -3.20467    0.06914 -46.348  < 2e-16 ***
#   croptri alf:tillNT:ccCC          -0.64762    0.07759  -8.347  < 2e-16 ***
#   cropalf:tillRT:ccCC              -3.15139    0.06318 -49.879  < 2e-16 ***
#   cropcorn-grain cs:tillRT:ccCC    -2.59996    0.06914 -37.602  < 2e-16 ***
#   cropcorn-grain mono:tillRT:ccCC  -3.41934    0.06914 -49.452  < 2e-16 ***
#   cropcorn-silage mono:tillRT:ccCC -3.92026    0.06914 -56.697  < 2e-16 ***
#   cropcorn alf:tillRT:ccCC         -5.26296    0.07696 -68.383  < 2e-16 ***
#   cropsoy cs:tillRT:ccCC           -3.60740    0.06914 -52.172  < 2e-16 ***
#   croptri alf:tillRT:ccCC           0.55681    0.07759   7.176 7.31e-13 ***
#   cropalf:tillCT:ccNC              -3.78289    0.06318 -59.874  < 2e-16 ***
#   cropcorn-grain cs:tillCT:ccNC    -2.01145    0.06914 -29.091  < 2e-16 ***
#   cropcorn-grain mono:tillCT:ccNC  -3.29000    0.06914 -47.582  < 2e-16 ***
#   cropcorn-silage mono:tillCT:ccNC -2.78371    0.06914 -40.259  < 2e-16 ***
#   cropcorn alf:tillCT:ccNC         -2.93568    0.07696 -38.144  < 2e-16 ***
#   cropsoy cs:tillCT:ccNC           -3.52561    0.06914 -50.989  < 2e-16 ***
#   croptri alf:tillCT:ccNC           0.88274    0.07759  11.377  < 2e-16 ***
#   cropalf:tillNT:ccNC              -2.91335    0.06318 -46.111  < 2e-16 ***
#   cropcorn-grain cs:tillNT:ccNC    -2.37446    0.06914 -34.341  < 2e-16 ***
#   cropcorn-grain mono:tillNT:ccNC  -2.87935    0.06914 -41.643  < 2e-16 ***
#   cropcorn-silage mono:tillNT:ccNC -3.79305    0.06914 -54.857  < 2e-16 ***
#   cropcorn alf:tillNT:ccNC         -3.22227    0.07696 -41.868  < 2e-16 ***
#   cropsoy cs:tillNT:ccNC           -2.78053    0.06914 -40.213  < 2e-16 ***
#   croptri alf:tillNT:ccNC          -1.08607    0.07759 -13.997  < 2e-16 ***
#   cropalf:tillRT:ccNC              -3.32396    0.06318 -52.610  < 2e-16 ***
#   cropcorn-grain cs:tillRT:ccNC    -2.14663    0.06914 -31.046  < 2e-16 ***
#   cropcorn-grain mono:tillRT:ccNC  -3.03633    0.06914 -43.913  < 2e-16 ***
#   cropcorn-silage mono:tillRT:ccNC -2.89579    0.06914 -41.880  < 2e-16 ***
#   cropcorn alf:tillRT:ccNC         -3.09118    0.07696 -40.164  < 2e-16 ***
#   cropsoy cs:tillRT:ccNC           -3.15957    0.06914 -45.695  < 2e-16 ***
#   croptri alf:tillRT:ccNC                NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.202 on 34230 degrees of freedom
# Multiple R-squared:  0.4991,	Adjusted R-squared:  0.4985 
# F-statistic:   832 on 41 and 34230 DF,  p-value: < 2.2e-16


summary(lmnet)
# Call:
#   lm(formula = ghg ~ crop:till:cc, data = ghgdat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.2865 -0.4438  0.0758  0.6126  3.4840 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       2.25154    0.05519  40.799  < 2e-16 ***
#   cropalf:tillCT:ccCC              -3.55455    0.06355 -55.932  < 2e-16 ***
#   cropcorn-grain cs:tillCT:ccCC    -2.05277    0.06955 -29.515  < 2e-16 ***
#   cropcorn-grain mono:tillCT:ccCC  -3.21058    0.06955 -46.163  < 2e-16 ***
#   cropcorn-silage mono:tillCT:ccCC -3.49087    0.06955 -50.193  < 2e-16 ***
#   cropcorn alf:tillCT:ccCC         -4.89727    0.07741 -63.261  < 2e-16 ***
#   cropsoy cs:tillCT:ccCC           -4.03933    0.06955 -58.079  < 2e-16 ***
#   croptri alf:tillCT:ccCC           1.47386    0.07805  18.885  < 2e-16 ***
#   cropalf:tillNT:ccCC              -2.48036    0.06355 -39.030  < 2e-16 ***
#   cropcorn-grain cs:tillNT:ccCC    -2.25611    0.06955 -32.439  < 2e-16 ***
#   cropcorn-grain mono:tillNT:ccCC  -2.25021    0.06955 -32.354  < 2e-16 ***
#   cropcorn-silage mono:tillNT:ccCC -4.06369    0.06955 -58.429  < 2e-16 ***
#   cropcorn alf:tillNT:ccCC         -5.49174    0.07741 -70.940  < 2e-16 ***
#   cropsoy cs:tillNT:ccCC           -3.06994    0.06955 -44.141  < 2e-16 ***
#   croptri alf:tillNT:ccCC          -0.68079    0.07805  -8.723  < 2e-16 ***
#   cropalf:tillRT:ccCC              -3.03133    0.06355 -47.699  < 2e-16 ***
#   cropcorn-grain cs:tillRT:ccCC    -2.18329    0.06955 -31.392  < 2e-16 ***
#   cropcorn-grain mono:tillRT:ccCC  -2.88775    0.06955 -41.521  < 2e-16 ***
#   cropcorn-silage mono:tillRT:ccCC -3.57969    0.06955 -51.470  < 2e-16 ***
#   cropcorn alf:tillRT:ccCC         -5.16066    0.07741 -66.663  < 2e-16 ***
#   cropsoy cs:tillRT:ccCC           -3.60710    0.06955 -51.864  < 2e-16 ***
#   croptri alf:tillRT:ccCC           0.59052    0.07805   7.566 3.94e-14 ***
#   cropalf:tillCT:ccNC              -3.67602    0.06355 -57.844  < 2e-16 ***
#   cropcorn-grain cs:tillCT:ccNC    -1.57434    0.06955 -22.636  < 2e-16 ***
#   cropcorn-grain mono:tillCT:ccNC  -2.79602    0.06955 -40.202  < 2e-16 ***
#   cropcorn-silage mono:tillCT:ccNC -2.40529    0.06955 -34.584  < 2e-16 ***
#   cropcorn alf:tillCT:ccNC         -2.68674    0.07741 -34.706  < 2e-16 ***
#   cropsoy cs:tillCT:ccNC           -3.50279    0.06955 -50.364  < 2e-16 ***
#   croptri alf:tillCT:ccNC           0.90920    0.07805  11.650  < 2e-16 ***
#   cropalf:tillNT:ccNC              -2.71817    0.06355 -42.772  < 2e-16 ***
#   cropcorn-grain cs:tillNT:ccNC    -1.73695    0.06955 -24.974  < 2e-16 ***
#   cropcorn-grain mono:tillNT:ccNC  -1.89549    0.06955 -27.254  < 2e-16 ***
#   cropcorn-silage mono:tillNT:ccNC -3.03224    0.06955 -43.598  < 2e-16 ***
#   cropcorn alf:tillNT:ccNC         -3.03383    0.07741 -39.190  < 2e-16 ***
#   cropsoy cs:tillNT:ccNC           -2.62482    0.06955 -37.740  < 2e-16 ***
#   croptri alf:tillNT:ccNC          -1.14027    0.07805 -14.610  < 2e-16 ***
#   cropalf:tillRT:ccNC              -3.18126    0.06355 -50.058  < 2e-16 ***
#   cropcorn-grain cs:tillRT:ccNC    -1.68885    0.06955 -24.283  < 2e-16 ***
#   cropcorn-grain mono:tillRT:ccNC  -2.48349    0.06955 -35.708  < 2e-16 ***
#   cropcorn-silage mono:tillRT:ccNC -2.50376    0.06955 -36.000  < 2e-16 ***
#   cropcorn alf:tillRT:ccNC         -2.85646    0.07741 -36.899  < 2e-16 ***
#   cropsoy cs:tillRT:ccNC           -3.11292    0.06955 -44.759  < 2e-16 ***
#   croptri alf:tillRT:ccNC                NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.209 on 34230 degrees of freedom
# Multiple R-squared:  0.4943,	Adjusted R-squared:  0.4937 
# F-statistic:   816 on 41 and 34230 DF,  p-value: < 2.2e-16


aovn2o <- aov(ghg_total_n2o ~crop:till:cc, data=ghgdat)  
aovsoc <- aov(ghg_dsoc ~crop:till:cc, data=ghgdat)  
aovnet <- aov(ghg ~crop:till:cc, data=ghgdat)  

summary(aovn2o)
# Df Sum Sq Mean Sq F value Pr(>F)    
# crop:till:cc    41 2390.8   58.31    2567 <2e-16 ***
#   Residuals    34230  777.5    0.02                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(aovsoc)
# Df Sum Sq Mean Sq F value Pr(>F)    
# crop:till:cc    41  49287  1202.1     832 <2e-16 ***
#   Residuals    34230  49459     1.4                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(aovnet)
# Df Sum Sq Mean Sq F value Pr(>F)    
# crop:till:cc    41  48911  1193.0     816 <2e-16 ***
#   Residuals    34230  50040     1.5                   
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
