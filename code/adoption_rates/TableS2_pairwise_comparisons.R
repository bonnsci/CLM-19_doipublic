
# script started 6/13/2024 by Bonnie McGill


################################################
# FIRST RUN "0_Load_adopt_data.R" to load packages and data  
################################################
rm(cens, opt, till, clm, clm2)


# Sig. differences for Fig. 1 and Table S2.

ccwny <- aov(value~source:crop_name, data=dat[dat$year_1==2017& dat$variable=="perc_cc" & dat$region=="Western NY",])
summary(ccwny)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# source:crop_name  2    561  280.52   7.288 0.00273 **
#   Residuals        29   1116   38.49                        
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Tukoutccwny <- TukeyHSD(ccwny)
Tukoutccwny  ########################### yes there are ways to specify which contrasts you want, but I didn't figure that out yet so just seeing them all

#                                           diff         lwr       upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA        NA        NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA        NA        NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA        NA        NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA        NA        NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA        NA        NA        NA
# AgCensus:Cropland-OpTIS:Corn          6.784225  -1.01243 14.580880 0.1165773  ## ND
# OpTIS:Cropland-OpTIS:Corn                   NA        NA        NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA        NA        NA        NA
# OpTIS:Soybeans-OpTIS:Corn            -3.279694 -13.24788  6.688491 0.9131721  XX
# OpTIS:Cropland-AgCensus:Cropland            NA        NA        NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA        NA        NA        NA
# OpTIS:Soybeans-AgCensus:Cropland    -10.063919 -19.04505 -1.082785 0.0211630  ## DIFF
# AgCensus:Soybeans-OpTIS:Cropland            NA        NA        NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA        NA        NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA        NA        NA        NA


ntwny <- aov(value~source:crop_name, data=dat[dat$year_1==2017& dat$variable=="perc_nt" & dat$region=="Western NY",])
summary(ntwny)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# source:crop_name  2   6472    3236   61.34 2.51e-11 ***
#   Residuals        30   1583      53                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1         
Tukoutntwny <- TukeyHSD(ntwny)
Tukoutntwny
# Fit: aov(formula = value ~ source:crop_name, data = dat[dat$year_1 == 2017 & dat$variable == "perc_nt" & dat$region == "Western NY", ])
# 
# $`source:crop_name`
# diff        lwr       upr     p adj
# OpTIS:Corn-AgCensus:Corn                   NA         NA        NA        NA
# AgCensus:Cropland-AgCensus:Corn            NA         NA        NA        NA
# OpTIS:Cropland-AgCensus:Corn               NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Corn            NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Corn               NA         NA        NA        NA
# AgCensus:Cropland-OpTIS:Corn        -19.53821 -28.645355 -10.43106 0.0000046   ***
# OpTIS:Cropland-OpTIS:Corn                  NA         NA        NA        NA
# AgCensus:Soybeans-OpTIS:Corn               NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Corn            14.90545   3.771966  26.03894 0.0038668   XX
# OpTIS:Cropland-AgCensus:Cropland           NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Cropland        NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Cropland     34.44366  24.522246  44.36507 0.0000000   ***
# AgCensus:Soybeans-OpTIS:Cropland           NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Cropland              NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans           NA         NA        NA        NA

rtwny <- aov(value~source:crop_name, data=dat[dat$year_1==2017& dat$variable=="perc_rt" & dat$region=="Western NY",])
summary(rtwny)
# Df Sum Sq Mean Sq F value Pr(>F)  
# source:crop_name  2  630.6  315.29   3.911 0.0309 *
#   Residuals        30 2418.2   80.61                    
Tukoutrtwny <- TukeyHSD(rtwny)
Tukoutrtwny
# $`source:crop_name`
# diff       lwr      upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA        NA       NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA        NA       NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA        NA       NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA        NA       NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA        NA       NA        NA
# AgCensus:Cropland-OpTIS:Corn         -7.936711 -19.19389 3.320470 0.2928783   ND
# OpTIS:Cropland-OpTIS:Corn                   NA        NA       NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA        NA       NA        NA
# OpTIS:Soybeans-OpTIS:Corn           -12.054176 -25.81608 1.707724 0.1128805   XX
# OpTIS:Cropland-AgCensus:Cropland            NA        NA       NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA        NA       NA        NA
# OpTIS:Soybeans-AgCensus:Cropland     -4.117465 -16.38114 8.146215 0.9071230   ND 
# AgCensus:Soybeans-OpTIS:Cropland            NA        NA       NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA        NA       NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA        NA       NA        NA

ctwny <- aov(value~source:crop_name, data=dat[dat$year_1==2017& dat$variable=="perc_ct" & dat$region=="Western NY",])
summary(ctwny)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# source:crop_name  2   8478    4239   67.84 1.16e-11 ***
#   Residuals        29   1812      62 
Tukoutctwny <- TukeyHSD(ctwny)
Tukoutctwny
# diff       lwr        upr    p adj
# OpTIS:Corn-AgCensus:Corn                   NA        NA         NA       NA
# AgCensus:Cropland-AgCensus:Corn            NA        NA         NA       NA
# OpTIS:Cropland-AgCensus:Corn               NA        NA         NA       NA
# AgCensus:Soybeans-AgCensus:Corn            NA        NA         NA       NA
# OpTIS:Soybeans-AgCensus:Corn               NA        NA         NA       NA
# AgCensus:Cropland-OpTIS:Corn         31.34756  21.41400  41.281112 0.000000   ****
# OpTIS:Cropland-OpTIS:Corn                  NA        NA         NA       NA
# AgCensus:Soybeans-OpTIS:Corn               NA        NA         NA       NA
# OpTIS:Soybeans-OpTIS:Corn            -3.01575 -15.71600   9.684505 0.977368   XX
# OpTIS:Cropland-AgCensus:Cropland           NA        NA         NA       NA
# AgCensus:Soybeans-AgCensus:Cropland        NA        NA         NA       NA
# OpTIS:Soybeans-AgCensus:Cropland    -34.36331 -45.80598 -22.920634 0.000000   ****
# AgCensus:Soybeans-OpTIS:Cropland           NA        NA         NA       NA
# OpTIS:Soybeans-OpTIS:Cropland              NA        NA         NA       NA
# OpTIS:Soybeans-AgCensus:Soybeans           NA        NA         NA       NA



cccil <- aov(value~source:crop_name, data=dat[dat$year_1==2017& dat$variable=="perc_cc" & dat$region=="Central IL",])
summary(cccil)
# Df Sum Sq Mean Sq F value  Pr(>F)   
# source:crop_name   2   85.1   42.54   5.159 0.00699 **
#   Residuals        129 1063.7    8.25  
Tukoutcccil <- TukeyHSD(cccil)
Tukoutcccil
# $`source:crop_name`
# diff       lwr        upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA        NA         NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA        NA         NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA        NA         NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA        NA         NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA        NA         NA        NA
# AgCensus:Cropland-OpTIS:Corn        -1.9279243 -3.699088 -0.1567607 0.0243713  ## DIFF
# OpTIS:Cropland-OpTIS:Corn                   NA        NA         NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA        NA         NA        NA
# OpTIS:Soybeans-OpTIS:Corn           -0.6276887 -2.398852  1.1434748 0.9086813  XX
# OpTIS:Cropland-AgCensus:Cropland            NA        NA         NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA        NA         NA        NA
# OpTIS:Soybeans-AgCensus:Cropland     1.3002356 -0.470928  3.0713991 0.2817221  ## ND
# AgCensus:Soybeans-OpTIS:Cropland            NA        NA         NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA        NA         NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA        NA         NA        NA

ntcil <- aov(value~source:crop_name, data=dat[dat$year_1==2017& dat$variable=="perc_nt" & dat$region=="Central IL",])
summary(ntcil)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name   4  65168   16292   56.23 <2e-16 ***
#   Residuals        215  62288     290           

Tukoutntcil <- TukeyHSD(ntcil)
Tukoutntcil
# diff        lwr      upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA         NA       NA        NA
# Transect:Corn-AgCensus:Corn                 NA         NA       NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA         NA       NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA         NA       NA        NA
# Transect:Cropland-AgCensus:Corn             NA         NA       NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA         NA       NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA         NA       NA        NA
# Transect:Soybeans-AgCensus:Corn             NA         NA       NA        NA
# Transect:Corn-OpTIS:Corn             0.5579781 -10.813749 11.92971 1.0000000   ND
# AgCensus:Cropland-OpTIS:Corn         2.0808884  -9.290839 13.45262 0.9997108   ND
# OpTIS:Cropland-OpTIS:Corn                   NA         NA       NA        NA
# Transect:Cropland-OpTIS:Corn                NA         NA       NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA         NA       NA        NA
# OpTIS:Soybeans-OpTIS:Corn            1.4508708  -9.920856 12.82260 0.9999814   XX
# Transect:Soybeans-OpTIS:Corn        44.0125235  32.640796 55.38425 0.0000000   XX
# AgCensus:Cropland-Transect:Corn      1.5229103  -9.848817 12.89464 0.9999730   ND
# OpTIS:Cropland-Transect:Corn                NA         NA       NA        NA
# Transect:Cropland-Transect:Corn             NA         NA       NA        NA
# AgCensus:Soybeans-Transect:Corn             NA         NA       NA        NA
# OpTIS:Soybeans-Transect:Corn         0.8928927 -10.478834 12.26462 0.9999996   XX
# Transect:Soybeans-Transect:Corn     43.4545455  32.082818 54.82627 0.0000000   XX
# OpTIS:Cropland-AgCensus:Cropland            NA         NA       NA        NA
# Transect:Cropland-AgCensus:Cropland         NA         NA       NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA         NA       NA        NA
# OpTIS:Soybeans-AgCensus:Cropland    -0.6300176 -12.001745 10.74171 1.0000000   ND
# Transect:Soybeans-AgCensus:Cropland 41.9316351  30.559908 53.30336 0.0000000   DIFF
# Transect:Cropland-OpTIS:Cropland            NA         NA       NA        NA
# AgCensus:Soybeans-OpTIS:Cropland            NA         NA       NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA         NA       NA        NA
# Transect:Soybeans-OpTIS:Cropland            NA         NA       NA        NA
# AgCensus:Soybeans-Transect:Cropland         NA         NA       NA        NA
# OpTIS:Soybeans-Transect:Cropland            NA         NA       NA        NA
# Transect:Soybeans-Transect:Cropland         NA         NA       NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA         NA       NA        NA
# Transect:Soybeans-AgCensus:Soybeans         NA         NA       NA        NA
# Transect:Soybeans-OpTIS:Soybeans    42.5616527  31.189926 53.93338 0.0000000   DIFF

rtcil <- aov(value~source:crop_name, data=dat[dat$year_1==2017& dat$variable=="perc_rt" & dat$region=="Central IL",])
summary(rtcil)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name   4  26870    6717   36.94 <2e-16 ***
#   Residuals        215  39099     182            
Tukoutrtcil <- TukeyHSD(rtcil)
Tukoutrtcil
#                                             diff          lwr        upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA           NA         NA        NA
# Transect:Corn-AgCensus:Corn                 NA           NA         NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA           NA         NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA           NA         NA        NA
# Transect:Cropland-AgCensus:Corn             NA           NA         NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA           NA         NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA           NA         NA        NA
# Transect:Soybeans-AgCensus:Corn             NA           NA         NA        NA
# Transect:Corn-OpTIS:Corn             -4.357237 -13.36692971   4.652455 0.8472790  ND
# AgCensus:Cropland-OpTIS:Corn         12.924074   3.91438199  21.933767 0.0003829  ***
# OpTIS:Cropland-OpTIS:Corn                   NA           NA         NA        NA
# Transect:Cropland-OpTIS:Corn                NA           NA         NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA           NA         NA        NA
# OpTIS:Soybeans-OpTIS:Corn            21.911167  12.90147504  30.920860 0.0000000  XX
# Transect:Soybeans-OpTIS:Corn         -7.245874 -16.25556607   1.763819 0.2282386 XX  
# AgCensus:Cropland-Transect:Corn      17.281312   8.27161929  26.291004 0.0000003  ***
# OpTIS:Cropland-Transect:Corn                NA           NA         NA        NA
# Transect:Cropland-Transect:Corn             NA           NA         NA        NA
# AgCensus:Soybeans-Transect:Corn             NA           NA         NA        NA
# OpTIS:Soybeans-Transect:Corn         26.268405  17.25871234  35.278097 0.0000000  XX
# Transect:Soybeans-Transect:Corn      -2.888636 -11.89832877   6.121056 0.9851351  XX
# OpTIS:Cropland-AgCensus:Cropland            NA           NA         NA        NA
# Transect:Cropland-AgCensus:Cropland         NA           NA         NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA           NA         NA        NA
# OpTIS:Soybeans-AgCensus:Cropland      8.987093  -0.02259936  17.996785 0.0511369  ND*
# Transect:Soybeans-AgCensus:Cropland -20.169948 -29.17964047 -11.160256 0.0000000  ****
# Transect:Cropland-OpTIS:Cropland            NA           NA         NA        NA
# AgCensus:Soybeans-OpTIS:Cropland            NA           NA         NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA           NA         NA        NA
# Transect:Soybeans-OpTIS:Cropland            NA           NA         NA        NA
# AgCensus:Soybeans-Transect:Cropland         NA           NA         NA        NA
# OpTIS:Soybeans-Transect:Cropland            NA           NA         NA        NA
# Transect:Soybeans-Transect:Cropland         NA           NA         NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA           NA         NA        NA
# Transect:Soybeans-AgCensus:Soybeans         NA           NA         NA        NA
# Transect:Soybeans-OpTIS:Soybeans    -29.157041 -38.16673351 -20.147349 0.0000000  ****

# t1 <- dat[dat$year==2017& dat$variable=="perc_ct" & dat$region=="Central IL",]

ctcil <- aov(value~source:crop_name, data=dat[dat$year_1==2017& dat$variable=="perc_ct" & dat$region=="Central IL",])
summary(ctcil)
#                      Df Sum Sq Mean Sq F value Pr(>F)    
#   source:crop_name   4  49319   12330   33.21 <2e-16 ***
#   Residuals        215  79815     371                   
Tukoutctcil <- TukeyHSD(ctcil)
Tukoutctcil
#                                             diff        lwr         upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA         NA          NA        NA
# Transect:Corn-AgCensus:Corn                 NA         NA          NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA         NA          NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA         NA          NA        NA
# Transect:Cropland-AgCensus:Corn             NA         NA          NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA         NA          NA        NA
# Transect:Soybeans-AgCensus:Corn             NA         NA          NA        NA
# Transect:Corn-OpTIS:Corn              3.871231  -9.001414  16.7438747 0.9902013 ND
# AgCensus:Cropland-OpTIS:Corn        -14.892082 -27.764727  -2.0194382 0.0106582 *
# OpTIS:Cropland-OpTIS:Corn                   NA         NA          NA        NA
# Transect:Cropland-OpTIS:Corn                NA         NA          NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA         NA          NA        NA
# OpTIS:Soybeans-OpTIS:Corn           -23.549919 -36.422563 -10.6772747 0.0000012  XX
# Transect:Soybeans-OpTIS:Corn        -36.667406 -49.540050 -23.7947616 0.0000000  XX
# AgCensus:Cropland-Transect:Corn     -18.763313 -31.635957  -5.8906687 0.0002816  ***
# OpTIS:Cropland-Transect:Corn                NA         NA          NA        NA
# Transect:Cropland-Transect:Corn             NA         NA          NA        NA
# AgCensus:Soybeans-Transect:Corn             NA         NA          NA        NA
# OpTIS:Soybeans-Transect:Corn        -27.421149 -40.293794 -14.5485052 0.0000000 XX
# Transect:Soybeans-Transect:Corn     -40.538636 -53.411281 -27.6659922 0.0000000 XX
# OpTIS:Cropland-AgCensus:Cropland            NA         NA          NA        NA
# Transect:Cropland-AgCensus:Cropland         NA         NA          NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Cropland     -8.657836 -21.530481   4.2148078 0.4708901 ND
# Transect:Soybeans-AgCensus:Cropland -21.775323 -34.647968  -8.9026792 0.0000100 ***
# Transect:Cropland-OpTIS:Cropland            NA         NA          NA        NA
# AgCensus:Soybeans-OpTIS:Cropland            NA         NA          NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA         NA          NA        NA
# Transect:Soybeans-OpTIS:Cropland            NA         NA          NA        NA
# AgCensus:Soybeans-Transect:Cropland         NA         NA          NA        NA
# OpTIS:Soybeans-Transect:Cropland            NA         NA          NA        NA
# Transect:Soybeans-Transect:Cropland         NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA         NA          NA        NA
# Transect:Soybeans-AgCensus:Soybeans         NA         NA          NA        NA
# Transect:Soybeans-OpTIS:Soybeans    -13.117487 -25.990131  -0.2448428 0.0420621  *



ccsil <- aov(value~source:crop_name, data=dat[dat$year_1==2017& dat$variable=="perc_cc" & dat$region=="Southern IL",])
summary(ccsil)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# source:crop_name  2  837.2   418.6   13.97 4.53e-06 ***
#   Residuals        99 2966.9    30.0  
Tukoutccsil <- TukeyHSD(ccsil)
Tukoutccsil
# $`source:crop_name`
# diff       lwr       upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA         NA        NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA         NA        NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA         NA        NA        NA
# AgCensus:Cropland-OpTIS:Corn        -6.2097939 -10.068542 -2.351046 0.0001320  DIFF
# OpTIS:Cropland-OpTIS:Corn                   NA         NA        NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Corn           -0.2735929  -4.132341  3.585155 0.9999473  XX
# OpTIS:Cropland-AgCensus:Cropland            NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Cropland     5.9362010   2.077453  9.794949 0.0002939  DIFF
# AgCensus:Soybeans-OpTIS:Cropland            NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA         NA        NA        NA

ntsil <- aov(value~source:crop_name, data=dat[dat$year_1==2017& dat$variable=="perc_nt" & dat$region=="Southern IL",])
summary(ntsil)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# source:crop_name   4  23723    5931    15.5 8.75e-11 ***
#   Residuals        165  63135     383           
Tukoutntsil <- TukeyHSD(ntsil)
Tukoutntsil
#                                             diff        lwr       upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA         NA        NA        NA
# Transect:Corn-AgCensus:Corn                 NA         NA        NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA         NA        NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA         NA        NA        NA
# Transect:Cropland-AgCensus:Corn             NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA         NA        NA        NA
# Transect:Soybeans-AgCensus:Corn             NA         NA        NA        NA
# Transect:Corn-OpTIS:Corn            -23.875045 -38.788323 -8.961767 0.0000436  ***
# AgCensus:Cropland-OpTIS:Corn        -10.060466 -24.973744  4.852812 0.4630618  ND
# OpTIS:Cropland-OpTIS:Corn                   NA         NA        NA        NA
# Transect:Cropland-OpTIS:Corn                NA         NA        NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Corn            -4.243477 -19.156755 10.669801 0.9930107  XX
# Transect:Soybeans-OpTIS:Corn         12.045543  -2.867734 26.958821 0.2213919  XX
# AgCensus:Cropland-Transect:Corn      13.814579  -1.098699 28.727856 0.0933599  ND*
# OpTIS:Cropland-Transect:Corn                NA         NA        NA        NA
# Transect:Cropland-Transect:Corn             NA         NA        NA        NA
# AgCensus:Soybeans-Transect:Corn             NA         NA        NA        NA
# OpTIS:Soybeans-Transect:Corn         19.631568   4.718290 34.544845 0.0017926  XX
# Transect:Soybeans-Transect:Corn      35.920588  21.007310 50.833866 0.0000000  XX
# OpTIS:Cropland-AgCensus:Cropland            NA         NA        NA        NA
# Transect:Cropland-AgCensus:Cropland         NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Cropland      5.816989  -9.096289 20.730267 0.9496489  ND
# Transect:Soybeans-AgCensus:Cropland  22.106010   7.192732 37.019287 0.0002205  ***
# Transect:Cropland-OpTIS:Cropland            NA         NA        NA        NA
# AgCensus:Soybeans-OpTIS:Cropland            NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA         NA        NA        NA
# Transect:Soybeans-OpTIS:Cropland            NA         NA        NA        NA
# AgCensus:Soybeans-Transect:Cropland         NA         NA        NA        NA
# OpTIS:Soybeans-Transect:Cropland            NA         NA        NA        NA
# Transect:Soybeans-Transect:Cropland         NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA         NA        NA        NA
# Transect:Soybeans-AgCensus:Soybeans         NA         NA        NA        NA
# Transect:Soybeans-OpTIS:Soybeans     16.289021   1.375743 31.202298 0.0209851  *

rtsil <- aov(value~source:crop_name, data=dat[dat$year_1==2017& dat$variable=="perc_rt" & dat$region=="Southern IL",])
summary(rtsil)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name   4  22234    5559   35.47 <2e-16 ***
#   Residuals        165  25861     157          
Tukoutrtsil <- TukeyHSD(rtsil)
Tukoutrtsil
#                                             diff        lwr         upr     p adj
# OpTIS:Corn-AgCensus:Corn                     NA         NA          NA        NA
# Transect:Corn-AgCensus:Corn                  NA         NA          NA        NA
# AgCensus:Cropland-AgCensus:Corn              NA         NA          NA        NA
# OpTIS:Cropland-AgCensus:Corn                 NA         NA          NA        NA
# Transect:Cropland-AgCensus:Corn              NA         NA          NA        NA
# AgCensus:Soybeans-AgCensus:Corn              NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Corn                 NA         NA          NA        NA
# Transect:Soybeans-AgCensus:Corn              NA         NA          NA        NA
# Transect:Corn-OpTIS:Corn            -10.7209347 -20.265524  -1.1763452 0.0153629  *
# AgCensus:Cropland-OpTIS:Corn         11.0853457   1.540756  20.6299351 0.0103262 *
# OpTIS:Cropland-OpTIS:Corn                    NA         NA          NA        NA
# Transect:Cropland-OpTIS:Corn                 NA         NA          NA        NA
# AgCensus:Soybeans-OpTIS:Corn                 NA         NA          NA        NA
# OpTIS:Soybeans-OpTIS:Corn            18.1842236   8.639634  27.7288130 0.0000005 XX
# Transect:Soybeans-OpTIS:Corn        -10.0032876 -19.547877  -0.4586982 0.0321751 XX
# AgCensus:Cropland-Transect:Corn      21.8062803  12.261691  31.3508698 0.0000000 ****
# OpTIS:Cropland-Transect:Corn                 NA         NA          NA        NA
# Transect:Cropland-Transect:Corn              NA         NA          NA        NA
# AgCensus:Soybeans-Transect:Corn              NA         NA          NA        NA
# OpTIS:Soybeans-Transect:Corn         28.9051582  19.360569  38.4497477 0.0000000  XX
# Transect:Soybeans-Transect:Corn       0.7176471  -8.826942  10.2622365 0.9999997  XX
# OpTIS:Cropland-AgCensus:Cropland             NA         NA          NA        NA
# Transect:Cropland-AgCensus:Cropland          NA         NA          NA        NA
# AgCensus:Soybeans-AgCensus:Cropland          NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Cropland      7.0988779  -2.445712  16.6434673 0.3258579 ND
# Transect:Soybeans-AgCensus:Cropland -21.0886333 -30.633223 -11.5440439 0.0000000 ****
# Transect:Cropland-OpTIS:Cropland             NA         NA          NA        NA
# AgCensus:Soybeans-OpTIS:Cropland             NA         NA          NA        NA
# OpTIS:Soybeans-OpTIS:Cropland                NA         NA          NA        NA
# Transect:Soybeans-OpTIS:Cropland             NA         NA          NA        NA
# AgCensus:Soybeans-Transect:Cropland          NA         NA          NA        NA
# OpTIS:Soybeans-Transect:Cropland             NA         NA          NA        NA
# Transect:Soybeans-Transect:Cropland          NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans             NA         NA          NA        NA
# Transect:Soybeans-AgCensus:Soybeans          NA         NA          NA        NA
# Transect:Soybeans-OpTIS:Soybeans    -28.1875112 -37.732101 -18.6429217 0.0000000 ****

ctsil <- aov(value~source:crop_name, data=dat[dat$year_1==2017& dat$variable=="perc_ct" & dat$region=="Southern IL",])
summary(ctsil)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name   4  46423   11606   40.21 <2e-16 ***
#   Residuals        165  47619     289           
Tukoutctsil <- TukeyHSD(ctsil)
Tukoutctsil
# diff          lwr        upr     p adj
# OpTIS:Corn-AgCensus:Corn                     NA           NA         NA        NA
# Transect:Corn-AgCensus:Corn                  NA           NA         NA        NA
# AgCensus:Cropland-AgCensus:Corn              NA           NA         NA        NA
# OpTIS:Cropland-AgCensus:Corn                 NA           NA         NA        NA
# Transect:Cropland-AgCensus:Corn              NA           NA         NA        NA
# AgCensus:Soybeans-AgCensus:Corn              NA           NA         NA        NA
# OpTIS:Soybeans-AgCensus:Corn                 NA           NA         NA        NA
# Transect:Soybeans-AgCensus:Corn              NA           NA         NA        NA
# Transect:Corn-OpTIS:Corn             35.1825441  22.23086813  48.134220 0.0000000  ****
# AgCensus:Cropland-OpTIS:Corn         -0.2265502 -13.17822615  12.725126 1.0000000  ND
# OpTIS:Cropland-OpTIS:Corn                    NA           NA         NA        NA
# Transect:Cropland-OpTIS:Corn                 NA           NA         NA        NA
# AgCensus:Soybeans-OpTIS:Corn                 NA           NA         NA        NA
# OpTIS:Soybeans-OpTIS:Corn           -14.2196244 -27.17130040  -1.267948 0.0198402  XX
# Transect:Soybeans-OpTIS:Corn         -1.2203971 -14.17207305  11.731279 0.9999982  XX
# AgCensus:Cropland-Transect:Corn     -35.4090943 -48.36077024 -22.457418 0.0000000  ****
# OpTIS:Cropland-Transect:Corn                 NA           NA         NA        NA
# Transect:Cropland-Transect:Corn              NA           NA         NA        NA
# AgCensus:Soybeans-Transect:Corn              NA           NA         NA        NA
# OpTIS:Soybeans-Transect:Corn        -49.4021685 -62.35384449 -36.450493 0.0000000 XX
# Transect:Soybeans-Transect:Corn     -36.4029412 -49.35461714 -23.451265 0.0000000 XX
# OpTIS:Cropland-AgCensus:Cropland             NA           NA         NA        NA
# Transect:Cropland-AgCensus:Cropland          NA           NA         NA        NA
# AgCensus:Soybeans-AgCensus:Cropland          NA           NA         NA        NA
# OpTIS:Soybeans-AgCensus:Cropland    -13.9930742 -26.94475021  -1.041398 0.0235791 *
# Transect:Soybeans-AgCensus:Cropland  -0.9938469 -13.94552286  11.957829 0.9999996 ND
# Transect:Cropland-OpTIS:Cropland             NA           NA         NA        NA
# AgCensus:Soybeans-OpTIS:Cropland             NA           NA         NA        NA
# OpTIS:Soybeans-OpTIS:Cropland                NA           NA         NA        NA
# Transect:Soybeans-OpTIS:Cropland             NA           NA         NA        NA
# AgCensus:Soybeans-Transect:Cropland          NA           NA         NA        NA
# OpTIS:Soybeans-Transect:Cropland             NA           NA         NA        NA
# Transect:Soybeans-Transect:Cropland          NA           NA         NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans             NA           NA         NA        NA
# Transect:Soybeans-AgCensus:Soybeans          NA           NA         NA        NA
# Transect:Soybeans-OpTIS:Soybeans     12.9992273   0.04755139  25.950903 0.0483881  *


############## SAME TESTS AS ABOVE BUT FOR 2015, 2018 OPTIS VS TRANSECT, IL ONLY

ntcil2 <- aov(value~source:crop_name:factor(year_1), data=dat[dat$year_1 %in% c(2014, 2016) & 
                                                                dat$variable=="perc_nt" & 
                                                                dat$region=="Central IL" & 
                                                                !dat$source=="AgCensus" ,])
summary(ntcil2)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name:factor(year_1)   7  82554   11793   31.27 <2e-16 ***
#   Residuals                       344 129758     377                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
Tukoutntcil2 <- TukeyHSD(ntcil2)
Tukoutntcil2
#                                                     diff        lwr          upr     p adj
# Transect:Corn:2014-OpTIS:Corn:2014            -13.739909 -26.367825  -1.11199356 0.0221806  *
# OpTIS:Soybeans:2014-OpTIS:Corn:2014             7.917567  -4.710349  20.54548244 0.5434983
# Transect:Soybeans:2014-OpTIS:Corn:2014         29.791909  17.163993  42.41982463 0.0000000
# OpTIS:Corn:2016-OpTIS:Corn:2014                -4.628641 -17.256556   7.99927499 0.9526615
# Transect:Corn:2016-OpTIS:Corn:2014            -19.401273 -32.029188  -6.77335719 0.0001086
# OpTIS:Soybeans:2016-OpTIS:Corn:2014            -3.878019 -16.505934   8.74989701 0.9822725
# Transect:Soybeans:2016-OpTIS:Corn:2014         18.755545   6.127630  31.38346099 0.0002179
# OpTIS:Soybeans:2014-Transect:Corn:2014         21.657476   9.029560  34.28539161 0.0000081
# Transect:Soybeans:2014-Transect:Corn:2014      43.531818  30.903903  56.15973380 0.0000000
# OpTIS:Corn:2016-Transect:Corn:2014              9.111269  -3.516647  21.73918417 0.3540547
# Transect:Corn:2016-Transect:Corn:2014          -5.661364 -18.289279   6.96655198 0.8714830
# OpTIS:Soybeans:2016-Transect:Corn:2014          9.861891  -2.766025  22.48980618 0.2536881
# Transect:Soybeans:2016-Transect:Corn:2014      32.495455  19.867539  45.12337016 0.0000000
# Transect:Soybeans:2014-OpTIS:Soybeans:2014     21.874342   9.246427  34.50225781 0.0000062 ***  
# OpTIS:Corn:2016-OpTIS:Soybeans:2014           -12.546207 -25.174123   0.08170817 0.0528962
# Transect:Corn:2016-OpTIS:Soybeans:2014        -27.318840 -39.946755 -14.69092401 0.0000000
# OpTIS:Soybeans:2016-OpTIS:Soybeans:2014       -11.795585 -24.423501   0.83233019 0.0867336
# Transect:Soybeans:2016-OpTIS:Soybeans:2014     10.837979  -1.789937  23.46589417 0.1532012
# OpTIS:Corn:2016-Transect:Soybeans:2014        -34.420550 -47.048465 -21.79263402 0.0000000
# Transect:Corn:2016-Transect:Soybeans:2014     -49.193182 -61.821097 -36.56526620 0.0000000
# OpTIS:Soybeans:2016-Transect:Soybeans:2014    -33.669928 -46.297843 -21.04201200 0.0000000
# Transect:Soybeans:2016-Transect:Soybeans:2014 -11.036364 -23.664279   1.59155198 0.1369723
# Transect:Corn:2016-OpTIS:Corn:2016            -14.772632 -27.400548  -2.14471657 0.0096819 **
# OpTIS:Soybeans:2016-OpTIS:Corn:2016             0.750622 -11.877294  13.37853763 0.9999997
# Transect:Soybeans:2016-OpTIS:Corn:2016         23.384186  10.756270  36.01210162 0.0000009
# OpTIS:Soybeans:2016-Transect:Corn:2016         15.523254   2.895339  28.15116982 0.0050807
# Transect:Soybeans:2016-Transect:Corn:2016      38.156818  25.528903  50.78473380 0.0000000
# Transect:Soybeans:2016-OpTIS:Soybeans:2016     22.633564  10.005648  35.26147960 0.0000025 ***


rtcil2 <- aov(value~source:crop_name:factor(year_1),  data=dat[dat$year_1 %in% c(2014, 2016) & 
                                                                 dat$variable=="perc_rt" & 
                                                                 dat$region=="Central IL" & 
                                                                 !dat$source=="AgCensus" ,])
summary(rtcil2)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name:factor(year_1)   7  22653    3236   15.84 <2e-16 ***
#   Residuals                       344  70277     204          
Tukoutrtcil2 <- TukeyHSD(rtcil2)
Tukoutrtcil2
#                                                   diff         lwr          upr     p adj
# Transect:Corn:2014-OpTIS:Corn:2014             -9.276224 -18.5695696   0.01712155 0.0508106 ND*
# OpTIS:Soybeans:2014-OpTIS:Corn:2014             1.974749  -7.3185968  11.26809434 0.9981378
# Transect:Soybeans:2014-OpTIS:Corn:2014        -16.826224 -26.1195696  -7.53287845 0.0000018
# OpTIS:Corn:2016-OpTIS:Corn:2014                -2.088100 -11.3814455   7.20524566 0.9973443
# Transect:Corn:2016-OpTIS:Corn:2014             -8.012588 -17.3059333   1.28075791 0.1490132
# OpTIS:Soybeans:2016-OpTIS:Corn:2014             8.128722  -1.1646238  17.42206741 0.1362646
# Transect:Soybeans:2016-OpTIS:Corn:2014        -14.653497 -23.9468424  -5.36015118 0.0000617
# OpTIS:Soybeans:2014-Transect:Corn:2014         11.250973   1.9576272  20.54431838 0.0062419
# Transect:Soybeans:2014-Transect:Corn:2014      -7.550000 -16.8433456   1.74334559 0.2085912
# OpTIS:Corn:2016-Transect:Corn:2014              7.188124  -2.1052215  16.48146970 0.2652738
# Transect:Corn:2016-Transect:Corn:2014           1.263636  -8.0297092  10.55698195 0.9999020
# OpTIS:Soybeans:2016-Transect:Corn:2014         17.404946   8.1116003  26.69829145 0.0000007
# Transect:Soybeans:2016-Transect:Corn:2014      -5.377273 -14.6706183   3.91607286 0.6444950
# Transect:Soybeans:2014-OpTIS:Soybeans:2014    -18.800973 -28.0943184  -9.50762721 0.0000001 ***
# OpTIS:Corn:2016-OpTIS:Soybeans:2014            -4.062849 -13.3561943   5.23049690 0.8856195
# Transect:Corn:2016-OpTIS:Soybeans:2014         -9.987336 -19.2806820  -0.69399084 0.0252343
# OpTIS:Soybeans:2016-OpTIS:Soybeans:2014         6.153973  -3.1393725  15.44731866 0.4702122
# Transect:Soybeans:2016-OpTIS:Soybeans:2014    -16.628246 -25.9215911  -7.33489993 0.0000026
# OpTIS:Corn:2016-Transect:Soybeans:2014         14.738124   5.4447785  24.03146970 0.0000543
# Transect:Corn:2016-Transect:Soybeans:2014       8.813636  -0.4797092  18.10698195 0.0773033
# OpTIS:Soybeans:2016-Transect:Soybeans:2014     24.954946  15.6616003  34.24829145 0.0000000
# Transect:Soybeans:2016-Transect:Soybeans:2014   2.172727  -7.1206183  11.46607286 0.9965893
# Transect:Corn:2016-OpTIS:Corn:2016             -5.924488 -15.2178333   3.36885784 0.5214577  ND
# OpTIS:Soybeans:2016-OpTIS:Corn:2016            10.216822   0.9234762  19.51016734 0.0198601
# Transect:Soybeans:2016-OpTIS:Corn:2016        -12.565397 -21.8587424  -3.27205125 0.0012058
# OpTIS:Soybeans:2016-Transect:Corn:2016         16.141309   6.8479639  25.43465509 0.0000058
# Transect:Soybeans:2016-Transect:Corn:2016      -6.640909 -15.9342547   2.65243650 0.3669202
# Transect:Soybeans:2016-OpTIS:Soybeans:2016    -22.782219 -32.0755642 -13.48887300 0.0000000 ****


ctcil2 <- aov(value~source:crop_name:factor(year_1),  data=dat[dat$year_1 %in% c(2014, 2016) & 
                                                                 dat$variable=="perc_ct" & 
                                                                 dat$region=="Central IL" & 
                                                                 !dat$source=="AgCensus" ,])
summary(ctcil2)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name:factor(year_1)   7  68087    9727   23.45 <2e-16 ***
#   Residuals                       344 142707     415                           
Tukoutctcil2 <- TukeyHSD(ctcil2)
Tukoutctcil2
#                                                   diff        lwr        upr     p adj
# Transect:Corn:2014-OpTIS:Corn:2014             24.531648  11.288585  37.774711 0.0000009 ***
# OpTIS:Soybeans:2014-OpTIS:Corn:2014           -10.131510 -23.374573   3.111553 0.2786906
# Transect:Soybeans:2014-OpTIS:Corn:2014         -8.956989 -22.200051   4.286074 0.4414036
# OpTIS:Corn:2016-OpTIS:Corn:2014                 7.119039  -6.124023  20.362102 0.7259820
# Transect:Corn:2016-OpTIS:Corn:2014             28.922557  15.679494  42.165620 0.0000000
# OpTIS:Soybeans:2016-OpTIS:Corn:2014            -4.310097 -17.553160   8.932966 0.9753069
# Transect:Soybeans:2016-OpTIS:Corn:2014         -2.575170 -15.818233  10.667893 0.9989485
# OpTIS:Soybeans:2014-Transect:Corn:2014        -34.663158 -47.906221 -21.420095 0.0000000
# Transect:Soybeans:2014-Transect:Corn:2014     -33.488636 -46.731699 -20.245573 0.0000000
# OpTIS:Corn:2016-Transect:Corn:2014            -17.412608 -30.655671  -4.169545 0.0018926
# Transect:Corn:2016-Transect:Corn:2014           4.390909  -8.852154  17.633972 0.9725937
# OpTIS:Soybeans:2016-Transect:Corn:2014        -28.841745 -42.084808 -15.598682 0.0000000
# Transect:Soybeans:2016-Transect:Corn:2014     -27.106818 -40.349881 -13.863755 0.0000000
# Transect:Soybeans:2014-OpTIS:Soybeans:2014      1.174522 -12.068541  14.417585 0.9999947 ND
# OpTIS:Corn:2016-OpTIS:Soybeans:2014            17.250550   4.007487  30.493613 0.0021887
# Transect:Corn:2016-OpTIS:Soybeans:2014         39.054067  25.811004  52.297130 0.0000000
# OpTIS:Soybeans:2016-OpTIS:Soybeans:2014         5.821413  -7.421650  19.064476 0.8826527
# Transect:Soybeans:2016-OpTIS:Soybeans:2014      7.556340  -5.686723  20.799403 0.6608812
# OpTIS:Corn:2016-Transect:Soybeans:2014         16.076028   2.832965  29.319091 0.0060218
# Transect:Corn:2016-Transect:Soybeans:2014      37.879545  24.636483  51.122608 0.0000000
# OpTIS:Soybeans:2016-Transect:Soybeans:2014      4.646891  -8.596172  17.889954 0.9625472
# Transect:Soybeans:2016-Transect:Soybeans:2014   6.381818  -6.861245  19.624881 0.8231075
# Transect:Corn:2016-OpTIS:Corn:2016             21.803517   8.560455  35.046580 0.0000226 ***
# OpTIS:Soybeans:2016-OpTIS:Corn:2016           -11.429137 -24.672200   1.813926 0.1481264
# Transect:Soybeans:2016-OpTIS:Corn:2016         -9.694210 -22.937273   3.548853 0.3349839
# OpTIS:Soybeans:2016-Transect:Corn:2016        -33.232654 -46.475717 -19.989591 0.0000000
# Transect:Soybeans:2016-Transect:Corn:2016     -31.497727 -44.740790 -18.254664 0.0000000
# Transect:Soybeans:2016-OpTIS:Soybeans:2016      1.734927 -11.508136  14.977990 0.9999238 ND



ntsil2 <- aov(value~source:crop_name:factor(year_1),  data=dat[dat$year_1 %in% c(2014, 2016) & 
                                                                 dat$variable=="perc_nt" & 
                                                                 dat$region=="Southern IL" & 
                                                                 !dat$source=="AgCensus" ,])
summary(ntsil2)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name:factor(year_1)   7  59829    8547   23.27 <2e-16 ***
#   Residuals                       263  96594     367        
Tukoutntsil2 <- TukeyHSD(ntsil2)
Tukoutntsil2
#                                                  diff        lwr        upr     p adj
# Transect:Corn:2014-OpTIS:Corn:2014            -24.9828181 -39.185030 -10.780606 0.0000047  ***
# OpTIS:Soybeans:2014-OpTIS:Corn:2014             8.8037464  -5.398466  23.005959 0.5562338
# Transect:Soybeans:2014-OpTIS:Corn:2014          7.4260055  -6.776207  21.628218 0.7514392
# OpTIS:Corn:2016-OpTIS:Corn:2014                12.8754024  -1.326810  27.077615 0.1071603
# Transect:Corn:2016-OpTIS:Corn:2014            -24.3055453 -38.614946  -9.996145 0.0000116
# OpTIS:Soybeans:2016-OpTIS:Corn:2014            15.9398483   1.737636  30.142061 0.0159125
# Transect:Soybeans:2016-OpTIS:Corn:2014          3.1083584 -11.093854  17.310571 0.9977109
# OpTIS:Soybeans:2014-Transect:Corn:2014         33.7865644  19.584352  47.988777 0.0000000
# Transect:Soybeans:2014-Transect:Corn:2014      32.4088235  18.206611  46.611036 0.0000000
# OpTIS:Corn:2016-Transect:Corn:2014             37.8582205  23.656008  52.060433 0.0000000
# Transect:Corn:2016-Transect:Corn:2014           0.6772727 -13.632128  14.986673 0.9999999
# OpTIS:Soybeans:2016-Transect:Corn:2014         40.9226664  26.720454  55.124879 0.0000000
# Transect:Soybeans:2016-Transect:Corn:2014      28.0911765  13.888964  42.293389 0.0000001
# Transect:Soybeans:2014-OpTIS:Soybeans:2014     -1.3777409 -15.579953  12.824472 0.9999900  ND
# OpTIS:Corn:2016-OpTIS:Soybeans:2014             4.0716560 -10.130556  18.273868 0.9879539
# Transect:Corn:2016-OpTIS:Soybeans:2014        -33.1092917 -47.418692 -18.799891 0.0000000
# OpTIS:Soybeans:2016-OpTIS:Soybeans:2014         7.1361019  -7.066110  21.338314 0.7876537
# Transect:Soybeans:2016-OpTIS:Soybeans:2014     -5.6953880 -19.897600   8.506824 0.9238140
# OpTIS:Corn:2016-Transect:Soybeans:2014          5.4493969  -8.752816  19.651609 0.9390915
# Transect:Corn:2016-Transect:Soybeans:2014     -31.7315508 -46.040951 -17.422150 0.0000000
# OpTIS:Soybeans:2016-Transect:Soybeans:2014      8.5138429  -5.688370  22.716055 0.5990481
# Transect:Soybeans:2016-Transect:Soybeans:2014  -4.3176471 -18.519859   9.884565 0.9830193
# Transect:Corn:2016-OpTIS:Corn:2016            -37.1809477 -51.490348 -22.871547 0.0000000 ****
# OpTIS:Soybeans:2016-OpTIS:Corn:2016             3.0644459 -11.137766  17.266658 0.9979090
# Transect:Soybeans:2016-OpTIS:Corn:2016         -9.7670440 -23.969256   4.435168 0.4167941
# OpTIS:Soybeans:2016-Transect:Corn:2016         40.2453937  25.935993  54.554794 0.0000000
# Transect:Soybeans:2016-Transect:Corn:2016      27.4139037  13.104503  41.723304 0.0000004
# Transect:Soybeans:2016-OpTIS:Soybeans:2016    -12.8314899 -27.033702   1.370723 0.1097049 ND*

rtsil2 <- aov(value~source:crop_name:factor(year_1), data=dat[dat$year_1 %in% c(2014, 2016) & 
                                                                dat$variable=="perc_rt" & 
                                                                dat$region=="Southern IL" & 
                                                                !dat$source=="AgCensus" ,])
summary(rtsil2)
# Df Sum Sq Mean Sq F value  Pr(>F)    
# source:crop_name:factor(year_1)   7  10270    1467    9.59 1.3e-10 ***
#   Residuals                       262  40082     153          
Tukoutrtsil2 <- TukeyHSD(rtsil2)
Tukoutrtsil2
#                                                     diff         lwr        upr     p adj
# Transect:Corn:2014-OpTIS:Corn:2014            -13.4156076 -22.6511472 -4.1800681 0.0003528  ***
# OpTIS:Soybeans:2014-OpTIS:Corn:2014             2.4958428  -6.7396968 11.7313823 0.9915338
# Transect:Soybeans:2014-OpTIS:Corn:2014        -11.5744311 -20.8099707 -2.3388916 0.0039553
# OpTIS:Corn:2016-OpTIS:Corn:2014                -2.7405331 -11.9760727  6.4950064 0.9852466
# Transect:Corn:2016-OpTIS:Corn:2014            -11.4274614 -20.7316677 -2.1232552 0.0052280
# OpTIS:Soybeans:2016-OpTIS:Corn:2014            -2.5089999 -11.7445394  6.7265397 0.9912614
# Transect:Soybeans:2016-OpTIS:Corn:2014        -13.7832547 -23.0187942 -4.5477151 0.0002093
# OpTIS:Soybeans:2014-Transect:Corn:2014         15.9114504   6.7450919 25.0778089 0.0000066
# Transect:Soybeans:2014-Transect:Corn:2014       1.8411765  -7.3251820 11.0075350 0.9986784
# OpTIS:Corn:2016-Transect:Corn:2014             10.6750745   1.5087160 19.8414330 0.0103402
# Transect:Corn:2016-Transect:Corn:2014           1.9881462  -7.2473934 11.2236857 0.9979392
# OpTIS:Soybeans:2016-Transect:Corn:2014         10.9066077   1.7402492 20.0729662 0.0079236
# Transect:Soybeans:2016-Transect:Corn:2014      -0.3676471  -9.5340056  8.7987114 1.0000000
# Transect:Soybeans:2014-OpTIS:Soybeans:2014    -14.0702739 -23.2366324 -4.9039154 0.0001181 ***
# OpTIS:Corn:2016-OpTIS:Soybeans:2014            -5.2363759 -14.4027344  3.9299826 0.6573147
# Transect:Corn:2016-OpTIS:Soybeans:2014        -13.9233042 -23.1588437 -4.6877646 0.0001710
# OpTIS:Soybeans:2016-OpTIS:Soybeans:2014        -5.0048426 -14.1712011  4.1615159 0.7076637
# Transect:Soybeans:2016-OpTIS:Soybeans:2014    -16.2790974 -25.4454559 -7.1127389 0.0000036
# OpTIS:Corn:2016-Transect:Soybeans:2014          8.8338980  -0.3324605 18.0002565 0.0680319
# Transect:Corn:2016-Transect:Soybeans:2014       0.1469697  -9.0885699  9.3825092 1.0000000
# OpTIS:Soybeans:2016-Transect:Soybeans:2014      9.0654313  -0.1009273 18.2317898 0.0549864
# Transect:Soybeans:2016-Transect:Soybeans:2014  -2.2088235 -11.3751820  6.9575350 0.9958067
# Transect:Corn:2016-OpTIS:Corn:2016             -8.6869283 -17.9224678  0.5486113 0.0821457 ND*
# OpTIS:Soybeans:2016-OpTIS:Corn:2016             0.2315333  -8.9348252  9.3978918 1.0000000
# Transect:Soybeans:2016-OpTIS:Corn:2016        -11.0427215 -20.2090800 -1.8763630 0.0067564
# OpTIS:Soybeans:2016-Transect:Corn:2016          8.9184616  -0.3170780 18.1540011 0.0669474
# Transect:Soybeans:2016-Transect:Corn:2016      -2.3557932 -11.5913328  6.8797463 0.9940409
# Transect:Soybeans:2016-OpTIS:Soybeans:2016    -11.2742548 -20.4406133 -2.1078963 0.0051277 **



ctsil2 <- aov(value~source:crop_name:factor(year_1), data=dat[dat$year_1 %in% c(2014, 2016) & 
                                                                dat$variable=="perc_ct" & 
                                                                dat$region=="Southern IL" & 
                                                                !dat$source=="AgCensus" ,])
summary(ctsil2)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name:factor(year_1)   7 107748   15393   47.93 <2e-16 ***
#   Residuals                       258  82855     321         
Tukoutctsil2 <- TukeyHSD(ctsil2)
Tukoutctsil2
#                                                   diff        lwr         upr     p adj 
# Transect:Corn:2014-OpTIS:Corn:2014             40.240760  26.858089  53.6234321 0.0000000 ****
# OpTIS:Soybeans:2014-OpTIS:Corn:2014           -13.136573 -26.519245   0.2460990 0.0585557
# Transect:Soybeans:2014-OpTIS:Corn:2014          5.976054  -7.406617  19.3587262 0.8723546
# OpTIS:Corn:2016-OpTIS:Corn:2014                -9.692473 -23.279566   3.8946212 0.3673282
# Transect:Corn:2016-OpTIS:Corn:2014             37.584254  24.102081  51.0664266 0.0000000
# OpTIS:Soybeans:2016-OpTIS:Corn:2014           -14.464090 -28.051184  -0.8769962 0.0278852
# Transect:Soybeans:2016-OpTIS:Corn:2014          9.573113  -3.809559  22.9557850 0.3636395
# OpTIS:Soybeans:2014-Transect:Corn:2014        -53.377333 -66.659759 -40.0949074 0.0000000
# Transect:Soybeans:2014-Transect:Corn:2014     -34.264706 -47.547132 -20.9822802 0.0000000
# OpTIS:Corn:2016-Transect:Corn:2014            -49.933233 -63.421600 -36.4448659 0.0000000
# Transect:Corn:2016-Transect:Corn:2014          -2.656506 -16.039178  10.7261655 0.9987740
# OpTIS:Soybeans:2016-Transect:Corn:2014        -54.704850 -68.193217 -41.2164833 0.0000000
# Transect:Soybeans:2016-Transect:Corn:2014     -30.667647 -43.950073 -17.3852214 0.0000000
# Transect:Soybeans:2014-OpTIS:Soybeans:2014     19.112627   5.830202  32.3950528 0.0004220 ***
# OpTIS:Corn:2016-OpTIS:Soybeans:2014             3.444100 -10.044267  16.9324671 0.9939970
# Transect:Corn:2016-OpTIS:Soybeans:2014         50.720827  37.338155  64.1034986 0.0000000
# OpTIS:Soybeans:2016-OpTIS:Soybeans:2014        -1.327517 -14.815884  12.1608498 0.9999889
# Transect:Soybeans:2016-OpTIS:Soybeans:2014     22.709686   9.427260  35.9921116 0.0000099
# OpTIS:Corn:2016-Transect:Soybeans:2014        -15.668527 -29.156894  -2.1801601 0.0106706
# Transect:Corn:2016-Transect:Soybeans:2014      31.608200  18.225528  44.9908714 0.0000000
# OpTIS:Soybeans:2016-Transect:Soybeans:2014    -20.440144 -33.928511  -6.9517774 0.0001546
# Transect:Soybeans:2016-Transect:Soybeans:2014   3.597059  -9.685367  16.8794845 0.9914163
# Transect:Corn:2016-OpTIS:Corn:2016             47.276727  33.689633  60.8638205 0.0000000 ****
# OpTIS:Soybeans:2016-OpTIS:Corn:2016            -4.771617 -18.462828   8.9195937 0.9633588
# Transect:Soybeans:2016-OpTIS:Corn:2016         19.265586   5.777219  32.7539529 0.0004835
# OpTIS:Soybeans:2016-Transect:Corn:2016        -52.048344 -65.635438 -38.4612502 0.0000000
# Transect:Soybeans:2016-Transect:Corn:2016     -28.011141 -41.393813 -14.6284690 0.0000000
# Transect:Soybeans:2016-OpTIS:Soybeans:2016     24.037203  10.548836  37.5255702 0.0000033 ***

