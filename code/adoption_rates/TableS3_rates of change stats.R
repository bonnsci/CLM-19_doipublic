
# script started 6/13/2024 by Bonnie McGill


################################################
# FIRST RUN "0_Load_adopt_data.R" to load packages and data  
################################################
rm(cens, opt, till, clm, clm2)


# Sig. differences for Fig. 1 and Table S3.

ccwny <- aov(value~source:crop_name, data=dat[dat$year==2017& dat$variable=="perc_cc" & dat$region=="Western NY",])
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


ntwny <- aov(value~source:crop_name, data=dat[dat$year==2017& dat$variable=="perc_nt" & dat$region=="Western NY",])
summary(ntwny)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# Df Sum Sq Mean Sq F value   Pr(>F)    
# source:crop_name  2   2466  1233.2   9.308 0.000623 ***
#   Residuals        33   4372   132.5                         
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1               
Tukoutntwny <- TukeyHSD(ntwny)
Tukoutntwny
# $`source:crop_name`
# diff        lwr       upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA          NA        NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA          NA        NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA          NA        NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA          NA        NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA          NA        NA        NA
# AgCensus:Cropland-OpTIS:Corn        -18.886681 -33.2328539 -4.540508 0.0044087 # dIF
# OpTIS:Cropland-OpTIS:Corn                   NA          NA        NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA          NA        NA        NA
# OpTIS:Soybeans-OpTIS:Corn            -5.217583 -21.2076632 10.772498 0.9189727 XX
# OpTIS:Cropland-AgCensus:Cropland            NA          NA        NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA          NA        NA        NA
# OpTIS:Soybeans-AgCensus:Cropland     13.669098  -0.2001372 27.538334 0.0552772  # ND
# AgCensus:Soybeans-OpTIS:Cropland            NA          NA        NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA          NA        NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA          NA        NA        NA

rtwny <- aov(value~source:crop_name, data=dat[dat$year==2017& dat$variable=="perc_rt" & dat$region=="Western NY",])
summary(rtwny)
# Df Sum Sq Mean Sq F value Pr(>F)  
# source:crop_name  2    675   337.4   3.375 0.0464 *
#   Residuals        33   3299   100.0     
Tukoutrtwny <- TukeyHSD(rtwny)
Tukoutrtwny
# diff       lwr      upr     p adj
# OpTIS:Corn-AgCensus:Corn                   NA         NA        NA        NA
# AgCensus:Cropland-AgCensus:Corn            NA         NA        NA        NA
# OpTIS:Cropland-AgCensus:Corn               NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Corn            NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Corn               NA         NA        NA        NA
# AgCensus:Cropland-OpTIS:Corn        -4.663744 -17.126469  7.798981 0.8647140  #nd
# OpTIS:Cropland-OpTIS:Corn                  NA         NA        NA        NA
# AgCensus:Soybeans-OpTIS:Corn               NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Corn            5.652655  -8.238156 19.543466 0.8188908  XX
# OpTIS:Cropland-AgCensus:Cropland           NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Cropland        NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Cropland    10.316399  -1.732004 22.364802 0.1285667  # ND
# AgCensus:Soybeans-OpTIS:Cropland           NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Cropland              NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans           NA         NA        NA        NA

ctwny <- aov(value~source:crop_name, data=dat[dat$year==2017& dat$variable=="perc_ct" & dat$region=="Western NY",])
summary(ctwny)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# source:crop_name  2   9000    4500   55.11 3.03e-11 ***
#   Residuals        33   2695      82      
Tukoutctwny <- TukeyHSD(ctwny)
Tukoutctwny
# $`source:crop_name`
# diff        lwr       upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA         NA        NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA         NA        NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA         NA        NA        NA
# AgCensus:Cropland-OpTIS:Corn         33.634464  22.371657  44.89727 0.0000000  # DIFF
# OpTIS:Cropland-OpTIS:Corn                   NA         NA        NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Corn             3.982243  -8.571152  16.53564 0.9274444  XX
# OpTIS:Cropland-AgCensus:Cropland            NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Cropland    -29.652221 -40.540596 -18.76385 0.0000000  ## DIFF
# AgCensus:Soybeans-OpTIS:Cropland            NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA         NA        NA        NA



cccil <- aov(value~source:crop_name, data=dat[dat$year==2017& dat$variable=="perc_cc" & dat$region=="Central IL",])
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

ntcil <- aov(value~source:crop_name, data=dat[dat$year==2017& dat$variable=="perc_nt" & dat$region=="Central IL",])
summary(ntcil)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name   4  39955    9989   33.03 <2e-16 ***
#   Residuals        215  65021     302    

Tukoutntcil <- TukeyHSD(ntcil)
Tukoutntcil
# diff        lwr      upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA         NA        NA        NA
# Transect:Corn-AgCensus:Corn                 NA         NA        NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA         NA        NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA         NA        NA        NA
# Transect:Cropland-AgCensus:Corn             NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA         NA        NA        NA
# Transect:Soybeans-AgCensus:Corn             NA         NA        NA        NA
# Transect:Corn-OpTIS:Corn            -14.772632 -26.391153 -3.154112 0.0029379  # DIFF
# AgCensus:Cropland-OpTIS:Corn        -11.992904 -23.611424 -0.374383 0.0372144  # DIFF
# OpTIS:Cropland-OpTIS:Corn                   NA         NA        NA        NA
# Transect:Cropland-OpTIS:Corn                NA         NA        NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Corn             0.750622 -10.867899 12.369143 0.9999999  XX
# Transect:Soybeans-OpTIS:Corn         23.384186  11.765665 35.002707 0.0000001 XX
# AgCensus:Cropland-Transect:Corn       2.779729  -8.838792 14.398249 0.9979593  ## ND
# OpTIS:Cropland-Transect:Corn                NA         NA        NA        NA
# Transect:Cropland-Transect:Corn             NA         NA        NA        NA
# AgCensus:Soybeans-Transect:Corn             NA         NA        NA        NA
# OpTIS:Soybeans-Transect:Corn         15.523254   3.904734 27.141775 0.0013464  XX
# Transect:Soybeans-Transect:Corn      38.156818  26.538298 49.775339 0.0000000  XX
# OpTIS:Cropland-AgCensus:Cropland            NA         NA        NA        NA
# Transect:Cropland-AgCensus:Cropland         NA         NA        NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Cropland     12.743526   1.125005 24.362046 0.0198741  ## DIFF
# Transect:Soybeans-AgCensus:Cropland  35.377090  23.758569 46.995610 0.0000000  ## DIFF
# Transect:Cropland-OpTIS:Cropland            NA         NA        NA        NA
# AgCensus:Soybeans-OpTIS:Cropland            NA         NA        NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA         NA        NA        NA
# Transect:Soybeans-OpTIS:Cropland            NA         NA        NA        NA
# AgCensus:Soybeans-Transect:Cropland         NA         NA        NA        NA
# OpTIS:Soybeans-Transect:Cropland            NA         NA        NA        NA
# Transect:Soybeans-Transect:Cropland         NA         NA        NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA         NA        NA        NA
# Transect:Soybeans-AgCensus:Soybeans         NA         NA        NA        NA
# Transect:Soybeans-OpTIS:Soybeans     22.633564  11.015043 34.252085 0.0000002  # DIFF

rtcil <- aov(value~source:crop_name, data=dat[dat$year==2017& dat$variable=="perc_rt" & dat$region=="Central IL",])
summary(rtcil)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name   4  16740    4185   25.39 <2e-16 ***
#   Residuals        215  35439     165        
Tukoutrtcil <- TukeyHSD(rtcil)
Tukoutrtcil
# diff         lwr         upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA          NA         NA        NA
# Transect:Corn-AgCensus:Corn                 NA          NA         NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA          NA         NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA          NA         NA        NA
# Transect:Cropland-AgCensus:Corn             NA          NA         NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA          NA         NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA          NA         NA        NA
# Transect:Soybeans-AgCensus:Corn             NA          NA         NA        NA
# Transect:Corn-OpTIS:Corn             -5.924488 -14.5020941   2.653119 0.4327840  ## ND
# AgCensus:Cropland-OpTIS:Corn          9.122733   0.5451267  17.700339 0.0276120  # DIFF
# OpTIS:Cropland-OpTIS:Corn                   NA          NA         NA        NA
# Transect:Cropland-OpTIS:Corn                NA          NA         NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA          NA         NA        NA
# OpTIS:Soybeans-OpTIS:Corn            10.216822   1.6392154  18.794428 0.0073504  X
# Transect:Soybeans-OpTIS:Corn        -12.565397 -21.1430031  -3.987791 0.0002554  X
# AgCensus:Cropland-Transect:Corn      15.047221   6.4696145  23.624827 0.0000039  ## DIFF
# OpTIS:Cropland-Transect:Corn                NA          NA         NA        NA
# Transect:Cropland-Transect:Corn             NA          NA         NA        NA
# AgCensus:Soybeans-Transect:Corn             NA          NA         NA        NA
# OpTIS:Soybeans-Transect:Corn         16.141309   7.5637032  24.718916 0.0000005  X
# Transect:Soybeans-Transect:Corn      -6.640909 -15.2185154   1.936697 0.2752830  X
# OpTIS:Cropland-AgCensus:Cropland            NA          NA         NA        NA
# Transect:Cropland-AgCensus:Cropland         NA          NA         NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA          NA         NA        NA
# OpTIS:Soybeans-AgCensus:Cropland      1.094089  -7.4835176   9.671695 0.9999815  # ND
# Transect:Soybeans-AgCensus:Cropland -21.688130 -30.2657362 -13.110524 0.0000000  # diff
# Transect:Cropland-OpTIS:Cropland            NA          NA         NA        NA
# AgCensus:Soybeans-OpTIS:Cropland            NA          NA         NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA          NA         NA        NA
# Transect:Soybeans-OpTIS:Cropland            NA          NA         NA        NA
# AgCensus:Soybeans-Transect:Cropland         NA          NA         NA        NA
# OpTIS:Soybeans-Transect:Cropland            NA          NA         NA        NA
# Transect:Soybeans-Transect:Cropland         NA          NA         NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA          NA         NA        NA
# Transect:Soybeans-AgCensus:Soybeans         NA          NA         NA        NA
# Transect:Soybeans-OpTIS:Soybeans    -22.782219 -31.3598249 -14.204612 0.0000000  # DIFF

# t1 <- dat[dat$year==2017& dat$variable=="perc_ct" & dat$region=="Central IL",]

ctcil <- aov(value~source:crop_name, data=dat[dat$year==2017& dat$variable=="perc_ct" & dat$region=="Central IL",])
summary(ctcil)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# source:crop_name   4  31308    7827   20.87 1.5e-14 ***
#   Residuals        215  80631     375
Tukoutctcil <- TukeyHSD(ctcil)
Tukoutctcil
# $`source:crop_name`
# OpTIS:Corn-AgCensus:Corn                    NA         NA          NA        NA   
# Transect:Corn-AgCensus:Corn                 NA         NA          NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA         NA          NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA         NA          NA        NA
# Transect:Cropland-AgCensus:Corn             NA         NA          NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA         NA          NA        NA
# Transect:Soybeans-AgCensus:Corn             NA         NA          NA        NA
# Transect:Corn-OpTIS:Corn             21.803517   8.865243  34.7417915 0.0000111  ## DIFF
# AgCensus:Cropland-OpTIS:Corn          3.990204  -8.948070  16.9284785 0.9884400  ## ND
# OpTIS:Cropland-OpTIS:Corn                   NA         NA          NA        NA
# Transect:Cropland-OpTIS:Corn                NA         NA          NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA         NA          NA        NA 
# OpTIS:Soybeans-OpTIS:Corn           -11.429137 -24.367411   1.5091372 0.1311678  X
# Transect:Soybeans-OpTIS:Corn         -9.694210 -22.632484   3.2440642 0.3185291 X
# AgCensus:Cropland-Transect:Corn     -17.813313 -30.751587  -4.8750389 0.0008081  # DIFF
# OpTIS:Cropland-Transect:Corn                NA         NA          NA        NA
# Transect:Cropland-Transect:Corn             NA         NA          NA        NA
# AgCensus:Soybeans-Transect:Corn             NA         NA          NA        NA
# OpTIS:Soybeans-Transect:Corn        -33.232654 -46.170928 -20.2943802 0.0000000  X
# Transect:Soybeans-Transect:Corn     -31.497727 -44.436001 -18.5594532 0.0000000 X
# OpTIS:Cropland-AgCensus:Cropland            NA         NA          NA        NA
# Transect:Cropland-AgCensus:Cropland         NA         NA          NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Cropland    -15.419341 -28.357615  -2.4810673 0.0072973  # DIFF
# Transect:Soybeans-AgCensus:Cropland -13.684414 -26.622688  -0.7461402 0.0292280  # DIFF
# Transect:Cropland-OpTIS:Cropland            NA         NA          NA        NA
# AgCensus:Soybeans-OpTIS:Cropland            NA         NA          NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA         NA          NA        NA
# Transect:Soybeans-OpTIS:Cropland            NA         NA          NA        NA
# AgCensus:Soybeans-Transect:Cropland         NA         NA          NA        NA
# OpTIS:Soybeans-Transect:Cropland            NA         NA          NA        NA
# Transect:Soybeans-Transect:Cropland         NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA         NA          NA        NA
# Transect:Soybeans-AgCensus:Soybeans         NA         NA          NA        NA
# Transect:Soybeans-OpTIS:Soybeans      1.734927 -11.203347  14.6732011 0.9999727  # ND



ccsil <- aov(value~source:crop_name, data=dat[dat$year==2017& dat$variable=="perc_cc" & dat$region=="Southern IL",])
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

ntsil <- aov(value~source:crop_name, data=dat[dat$year==2017& dat$variable=="perc_nt" & dat$region=="Southern IL",])
summary(ntsil)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name   4  38749    9687      31 <2e-16 ***
#   Residuals        164  51247     312   
Tukoutntsil <- TukeyHSD(ntsil)
Tukoutntsil
# $`source:crop_name`
# diff        lwr         upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA         NA          NA        NA
# Transect:Corn-AgCensus:Corn                 NA         NA          NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA         NA          NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA         NA          NA        NA
# Transect:Cropland-AgCensus:Corn             NA         NA          NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA         NA          NA        NA
# Transect:Soybeans-AgCensus:Corn             NA         NA          NA        NA
# Transect:Corn-OpTIS:Corn            -37.180948 -50.760682 -23.6012133 0.0000000  ## DIFF
# AgCensus:Cropland-OpTIS:Corn        -24.673054 -38.151066 -11.1950415 0.0000015  ## DIFF
# OpTIS:Cropland-OpTIS:Corn                   NA         NA          NA        NA
# Transect:Cropland-OpTIS:Corn                NA         NA          NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA         NA          NA        NA
# OpTIS:Soybeans-OpTIS:Corn             3.064446 -10.413566  16.5424580 0.9985299  XX
# Transect:Soybeans-OpTIS:Corn         -9.767044 -23.245056   3.7109681 0.3615176 XX
# AgCensus:Cropland-Transect:Corn      12.507894  -1.071840  26.0876285 0.0973598  ## ND
# OpTIS:Cropland-Transect:Corn                NA         NA          NA        NA
# Transect:Cropland-Transect:Corn             NA         NA          NA        NA
# AgCensus:Soybeans-Transect:Corn             NA         NA          NA        NA
# OpTIS:Soybeans-Transect:Corn         40.245394  26.665659  53.8251280 0.0000000  XX
# Transect:Soybeans-Transect:Corn      27.413904  13.834169  40.9936381 0.0000001 XX
# OpTIS:Cropland-AgCensus:Cropland            NA         NA          NA        NA
# Transect:Cropland-AgCensus:Cropland         NA         NA          NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Cropland     27.737500  14.259487  41.2155116 0.0000000  ## DIFF
# Transect:Soybeans-AgCensus:Cropland  14.906010   1.427998  28.3840217 0.0183098  ## DIFF
# Transect:Cropland-OpTIS:Cropland            NA         NA          NA        NA
# AgCensus:Soybeans-OpTIS:Cropland            NA         NA          NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA         NA          NA        NA
# Transect:Soybeans-OpTIS:Cropland            NA         NA          NA        NA
# AgCensus:Soybeans-Transect:Cropland         NA         NA          NA        NA
# OpTIS:Soybeans-Transect:Cropland            NA         NA          NA        NA
# Transect:Soybeans-Transect:Cropland         NA         NA          NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA         NA          NA        NA
# Transect:Soybeans-AgCensus:Soybeans         NA         NA          NA        NA
# Transect:Soybeans-OpTIS:Soybeans    -12.831490 -26.309502   0.6465222 0.0756272  ## ND

rtsil <- aov(value~source:crop_name, data=dat[dat$year==2017& dat$variable=="perc_rt" & dat$region=="Southern IL",])
summary(rtsil)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# source:crop_name   4   9644  2411.0   14.93 1.99e-10 ***
#   Residuals        164  26480   161.5 
Tukoutrtsil <- TukeyHSD(rtsil)
Tukoutrtsil
# $`source:crop_name`
# diff         lwr         upr     p adj
# OpTIS:Corn-AgCensus:Corn                     NA          NA          NA        NA
# Transect:Corn-AgCensus:Corn                  NA          NA          NA        NA
# AgCensus:Cropland-AgCensus:Corn              NA          NA          NA        NA
# OpTIS:Cropland-AgCensus:Corn                 NA          NA          NA        NA
# Transect:Cropland-AgCensus:Corn              NA          NA          NA        NA
# AgCensus:Soybeans-AgCensus:Corn              NA          NA          NA        NA
# OpTIS:Soybeans-AgCensus:Corn                 NA          NA          NA        NA
# Transect:Soybeans-AgCensus:Corn              NA          NA          NA        NA
# Transect:Corn-OpTIS:Corn             -8.6869283 -18.4483533   1.0744967 0.1240418  ## ND
# AgCensus:Cropland-OpTIS:Corn         10.2341471   0.5458424  19.9224517 0.0297490  # DIFF
# OpTIS:Cropland-OpTIS:Corn                    NA          NA          NA        NA
# Transect:Cropland-OpTIS:Corn                 NA          NA          NA        NA
# AgCensus:Soybeans-OpTIS:Corn                 NA          NA          NA        NA
# OpTIS:Soybeans-OpTIS:Corn             0.2315333  -9.4567714   9.9198379 1.0000000  XX
# Transect:Soybeans-OpTIS:Corn        -11.0427215 -20.7310262  -1.3544168 0.0129598  XX
# AgCensus:Cropland-Transect:Corn      18.9210754   9.1596504  28.6825004 0.0000003  ## DIFF
# OpTIS:Cropland-Transect:Corn                 NA          NA          NA        NA
# Transect:Cropland-Transect:Corn              NA          NA          NA        NA
# AgCensus:Soybeans-Transect:Corn              NA          NA          NA        NA
# OpTIS:Soybeans-Transect:Corn          8.9184616  -0.8429634  18.6798866 0.1032619  XX
# Transect:Soybeans-Transect:Corn      -2.3557932 -12.1172182   7.4056318 0.9977535 XX
# OpTIS:Cropland-AgCensus:Cropland             NA          NA          NA        NA
# Transect:Cropland-AgCensus:Cropland          NA          NA          NA        NA
# AgCensus:Soybeans-AgCensus:Cropland          NA          NA          NA        NA
# OpTIS:Soybeans-AgCensus:Cropland    -10.0026138 -19.6909185  -0.3143091 0.0372369  ## DIFF
# Transect:Soybeans-AgCensus:Cropland -21.2768686 -30.9651733 -11.5885639 0.0000000  ## DIFF
# Transect:Cropland-OpTIS:Cropland             NA          NA          NA        NA
# AgCensus:Soybeans-OpTIS:Cropland             NA          NA          NA        NA
# OpTIS:Soybeans-OpTIS:Cropland                NA          NA          NA        NA
# Transect:Soybeans-OpTIS:Cropland             NA          NA          NA        NA
# AgCensus:Soybeans-Transect:Cropland          NA          NA          NA        NA
# OpTIS:Soybeans-Transect:Cropland             NA          NA          NA        NA
# Transect:Soybeans-Transect:Cropland          NA          NA          NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans             NA          NA          NA        NA
# Transect:Soybeans-AgCensus:Soybeans          NA          NA          NA        NA
# Transect:Soybeans-OpTIS:Soybeans    -11.2742548 -20.9625595  -1.5859501 0.0100852  ## DIFF

ctsil <- aov(value~source:crop_name, data=dat[dat$year==2017& dat$variable=="perc_ct" & dat$region=="Southern IL",])
summary(ctsil)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name   4  54635   13659   49.76 <2e-16 ***
#   Residuals        160  43918     274          
Tukoutctsil <- TukeyHSD(ctsil)
Tukoutctsil
# $`source:crop_name`
# diff        lwr         upr     p adj
# OpTIS:Corn-AgCensus:Corn                    NA         NA         NA        NA
# Transect:Corn-AgCensus:Corn                 NA         NA         NA        NA
# AgCensus:Cropland-AgCensus:Corn             NA         NA         NA        NA
# OpTIS:Cropland-AgCensus:Corn                NA         NA         NA        NA
# Transect:Cropland-AgCensus:Corn             NA         NA         NA        NA
# AgCensus:Soybeans-AgCensus:Corn             NA         NA         NA        NA
# OpTIS:Soybeans-AgCensus:Corn                NA         NA         NA        NA
# Transect:Soybeans-AgCensus:Corn             NA         NA         NA        NA
# Transect:Corn-OpTIS:Corn             47.276727  34.350522  60.202931 0.0000000  ## DIFF
# AgCensus:Cropland-OpTIS:Corn         15.844727   3.012447  28.677007 0.0046488  ## DIFF
# OpTIS:Cropland-OpTIS:Corn                   NA         NA         NA        NA
# Transect:Cropland-OpTIS:Corn                NA         NA         NA        NA
# AgCensus:Soybeans-OpTIS:Corn                NA         NA         NA        NA
# OpTIS:Soybeans-OpTIS:Corn            -4.771617 -17.796874   8.253640 0.9649795  XX
# Transect:Soybeans-OpTIS:Corn         19.265586   6.433306  32.097866 0.0001732 XX
# AgCensus:Cropland-Transect:Corn     -31.432000 -44.163725 -18.700274 0.0000000  ## DIFF
# OpTIS:Cropland-Transect:Corn                NA         NA         NA        NA
# Transect:Cropland-Transect:Corn             NA         NA         NA        NA
# AgCensus:Soybeans-Transect:Corn             NA         NA         NA        NA
# OpTIS:Soybeans-Transect:Corn        -52.048344 -64.974548 -39.122140 0.0000000  XX
# Transect:Soybeans-Transect:Corn     -28.011141 -40.742866 -15.279415 0.0000000 X
# OpTIS:Cropland-AgCensus:Cropland            NA         NA         NA        NA
# Transect:Cropland-AgCensus:Cropland         NA         NA         NA        NA
# AgCensus:Soybeans-AgCensus:Cropland         NA         NA         NA        NA
# OpTIS:Soybeans-AgCensus:Cropland    -20.616344 -33.448624  -7.784065 0.0000409  # DIFF
# Transect:Soybeans-AgCensus:Cropland   3.420859  -9.215496  16.057214 0.9949914 # ND
# Transect:Cropland-OpTIS:Cropland            NA         NA         NA        NA 
# AgCensus:Soybeans-OpTIS:Cropland            NA         NA         NA        NA
# OpTIS:Soybeans-OpTIS:Cropland               NA         NA         NA        NA
# Transect:Soybeans-OpTIS:Cropland            NA         NA         NA        NA
# AgCensus:Soybeans-Transect:Cropland         NA         NA         NA        NA
# OpTIS:Soybeans-Transect:Cropland            NA         NA         NA        NA
# Transect:Soybeans-Transect:Cropland         NA         NA         NA        NA
# OpTIS:Soybeans-AgCensus:Soybeans            NA         NA         NA        NA
# Transect:Soybeans-AgCensus:Soybeans         NA         NA         NA        NA
# Transect:Soybeans-OpTIS:Soybeans     24.037203  11.204924  36.869483 0.0000008  # DIFF


############## SAME TESTS AS ABOVE BUT FOR 2015, 2018 OPTIS VS TRANSECT, IL ONLY

ntcil2 <- aov(value~source:crop_name:factor(year), data=dat[dat$year %in% c(2015, 2018) & dat$variable=="perc_nt" & dat$region=="Central IL",])
summary(ntcil2)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name:factor(year)   7 121147   17307   46.87 <2e-16 ***
#   Residuals                     344 127025     369   
Tukoutntcil2 <- TukeyHSD(ntcil2)
Tukoutntcil2
# $`source:crop_name:factor(year)`
#                                                 diff        lwr         upr     p adj
# Transect:Corn:2015-OpTIS:Corn:2015            -13.7399092 -26.234134  -1.245684 0.0197955 ## DIFF
# OpTIS:Soybeans:2015-OpTIS:Corn:2015             7.9175668  -4.576658  20.411792 0.5294177
# Transect:Soybeans:2015-OpTIS:Corn:2015         29.7919090  17.297684  42.286134 0.0000000
# OpTIS:Corn:2018-OpTIS:Corn:2015               -18.7024327 -31.196658  -6.208208 0.0001863
# Transect:Corn:2018-OpTIS:Corn:2015            -18.1444546 -30.638679  -5.650230 0.0003378
# OpTIS:Soybeans:2018-OpTIS:Corn:2015           -17.2515619 -29.745787  -4.757337 0.0008450
# Transect:Soybeans:2018-OpTIS:Corn:2015         25.3100908  12.815866  37.804316 0.0000001
# OpTIS:Soybeans:2015-Transect:Corn:2015         21.6574760   9.163251  34.151701 0.0000061
# Transect:Soybeans:2015-Transect:Corn:2015      43.5318182  31.037593  56.026043 0.0000000
# OpTIS:Corn:2018-Transect:Corn:2015             -4.9625235 -17.456748   7.531701 0.9282768
# Transect:Corn:2018-Transect:Corn:2015          -4.4045455 -16.898770   8.089679 0.9615910
# OpTIS:Soybeans:2018-Transect:Corn:2015         -3.5116527 -16.005878   8.982572 0.9894631
# Transect:Soybeans:2018-Transect:Corn:2015      39.0500000  26.555775  51.544225 0.0000000
# Transect:Soybeans:2015-OpTIS:Soybeans:2015     21.8743422   9.380117  34.368567 0.0000047  ## DIFF
# OpTIS:Corn:2018-OpTIS:Soybeans:2015           -26.6199995 -39.114224 -14.125775 0.0000000
# Transect:Corn:2018-OpTIS:Soybeans:2015        -26.0620214 -38.556246 -13.567797 0.0000000
# OpTIS:Soybeans:2018-OpTIS:Soybeans:2015       -25.1691287 -37.663354 -12.674904 0.0000001
# Transect:Soybeans:2018-OpTIS:Soybeans:2015     17.3925240   4.898299  29.886749 0.0007333
# OpTIS:Corn:2018-Transect:Soybeans:2015        -48.4943417 -60.988567 -36.000117 0.0000000
# Transect:Corn:2018-Transect:Soybeans:2015     -47.9363636 -60.430588 -35.442139 0.0000000
# OpTIS:Soybeans:2018-Transect:Soybeans:2015    -47.0434709 -59.537696 -34.549246 0.0000000
# Transect:Soybeans:2018-Transect:Soybeans:2015  -4.4818182 -16.976043   8.012407 0.9578132
# Transect:Corn:2018-OpTIS:Corn:2018              0.5579781 -11.936247  13.052203 1.0000000  ## ND
# OpTIS:Soybeans:2018-OpTIS:Corn:2018             1.4508708 -11.043354  13.945096 0.9999664
# Transect:Soybeans:2018-OpTIS:Corn:2018         44.0125235  31.518299  56.506748 0.0000000
# OpTIS:Soybeans:2018-Transect:Corn:2018          0.8928927 -11.601332  13.387118 0.9999988
# Transect:Soybeans:2018-Transect:Corn:2018      43.4545455  30.960321  55.948770 0.0000000
# Transect:Soybeans:2018-OpTIS:Soybeans:2018     42.5616527  30.067428  55.055878 0.0000000  ## DIFF


rtcil2 <- aov(value~source:crop_name:factor(year), data=dat[dat$year %in% c(2015, 2018) & dat$variable=="perc_rt" & dat$region=="Central IL",])
summary(rtcil2)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name:factor(year)   7  33763    4823   22.44 <2e-16 ***
#   Residuals                     344  73937     215  
Tukoutrtcil2 <- TukeyHSD(rtcil2)
Tukoutrtcil2
# $`source:crop_name:factor(year)`
#                                                 diff        lwr         upr     p adj
# Transect:Corn:2015-OpTIS:Corn:2015             -9.2762240 -18.808516   0.2560681 0.0629694  ## ND
# OpTIS:Soybeans:2015-OpTIS:Corn:2015             1.9747488  -7.557543  11.5070409 0.9984174
# Transect:Soybeans:2015-OpTIS:Corn:2015        -16.8262240 -26.358516  -7.2939319 0.0000038
# OpTIS:Corn:2018-OpTIS:Corn:2015                -5.8894413 -15.421733   3.6428509 0.5627251
# Transect:Corn:2018-OpTIS:Corn:2015            -10.2466786 -19.778971  -0.7143864 0.0251697
# OpTIS:Soybeans:2018-OpTIS:Corn:2015            16.0217262   6.489434  25.5540183 0.0000136
# Transect:Soybeans:2018-OpTIS:Corn:2015        -13.1353149 -22.667607  -3.6030228 0.0008750
# OpTIS:Soybeans:2015-Transect:Corn:2015         11.2509728   1.718681  20.7832649 0.0086651
# Transect:Soybeans:2015-Transect:Corn:2015      -7.5500000 -17.082292   1.9822921 0.2371337
# OpTIS:Corn:2018-Transect:Corn:2015              3.3867828  -6.145509  12.9190749 0.9599307
# Transect:Corn:2018-Transect:Corn:2015          -0.9704545 -10.502747   8.5618376 0.9999863
# OpTIS:Soybeans:2018-Transect:Corn:2015         25.2979502  15.765658  34.8302423 0.0000000
# Transect:Soybeans:2018-Transect:Corn:2015      -3.8590909 -13.391383   5.6732012 0.9210801
# Transect:Soybeans:2015-OpTIS:Soybeans:2015    -18.8009728 -28.333265  -9.2686807 0.0000001  ## DIFF
# OpTIS:Corn:2018-OpTIS:Soybeans:2015            -7.8641900 -17.396482   1.6681021 0.1921564
# Transect:Corn:2018-OpTIS:Soybeans:2015        -12.2214273 -21.753719  -2.6891352 0.0027838
# OpTIS:Soybeans:2018-OpTIS:Soybeans:2015        14.0469774   4.514685  23.5792695 0.0002545
# Transect:Soybeans:2018-OpTIS:Soybeans:2015    -15.1100637 -24.642356  -5.5777716 0.0000548
# OpTIS:Corn:2018-Transect:Soybeans:2015         10.9367828   1.404491  20.4690749 0.0122457
# Transect:Corn:2018-Transect:Soybeans:2015       6.5795455  -2.952747  16.1118376 0.4137341
# OpTIS:Soybeans:2018-Transect:Soybeans:2015     32.8479502  23.315658  42.3802423 0.0000000
# Transect:Soybeans:2018-Transect:Soybeans:2015   3.6909091  -5.841383  13.2232012 0.9369850
# Transect:Corn:2018-OpTIS:Corn:2018             -4.3572373 -13.889529   5.1750548 0.8596579 ## ND
# OpTIS:Soybeans:2018-OpTIS:Corn:2018            21.9111674  12.378875  31.4434596 0.0000000
# Transect:Soybeans:2018-OpTIS:Corn:2018         -7.2458737 -16.778166   2.2864185 0.2866795
# OpTIS:Soybeans:2018-Transect:Corn:2018         26.2684047  16.736113  35.8006969 0.0000000
# Transect:Soybeans:2018-Transect:Corn:2018      -2.8886364 -12.420929   6.6436558 0.9835880
# Transect:Soybeans:2018-OpTIS:Soybeans:2018    -29.1570411 -38.689333 -19.6247490 0.0000000  ## DIFF


ctcil2 <- aov(value~source:crop_name:factor(year), data=dat[dat$year %in% c(2015, 2018) & dat$variable=="perc_ct" & dat$region=="Central IL",])
summary(ctcil2)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name:factor(year)   7  93298   13328   32.31 <2e-16 ***
#   Residuals                     344 141891     412                     
Tukoutctcil2 <- TukeyHSD(ctcil2)
Tukoutctcil2
# $`source:crop_name:factor(year)`
# diff        lwr         upr     p adj
# Transect:Corn:2015-OpTIS:Corn:2015             24.5316478  11.326498  37.73679756 0.0000009   ## DIFF
# OpTIS:Soybeans:2015-OpTIS:Corn:2015           -10.1315102 -23.336660   3.07363950 0.2751551
# Transect:Soybeans:2015-OpTIS:Corn:2015         -8.9569886 -22.162138   4.24816119 0.4374967
# OpTIS:Corn:2018-OpTIS:Corn:2015                26.0013264  12.796177  39.20647613 0.0000001
# Transect:Corn:2018-OpTIS:Corn:2015             29.8725569  16.667407  43.07770665 0.0000000
# OpTIS:Soybeans:2018-OpTIS:Corn:2015             2.4514075 -10.753742  15.65655726 0.9992226
# Transect:Soybeans:2018-OpTIS:Corn:2015        -10.6660795 -23.871229   2.53907029 0.2149461
# OpTIS:Soybeans:2015-Transect:Corn:2015        -34.6631581 -47.868308 -21.45800831 0.0000000
# Transect:Soybeans:2015-Transect:Corn:2015     -33.4886364 -46.693786 -20.28348662 0.0000000
# OpTIS:Corn:2018-Transect:Corn:2015              1.4696786 -11.735471  14.67482832 0.9999748
# Transect:Corn:2018-Transect:Corn:2015           5.3409091  -7.864241  18.54605884 0.9214551
# OpTIS:Soybeans:2018-Transect:Corn:2015        -22.0802403 -35.285390  -8.87509055 0.0000155
# Transect:Soybeans:2018-Transect:Corn:2015     -35.1977273 -48.402877 -21.99257753 0.0000000
# Transect:Soybeans:2015-OpTIS:Soybeans:2015      1.1745217 -12.030628  14.37967144 0.9999946  ## ND
# OpTIS:Corn:2018-OpTIS:Soybeans:2015            36.1328366  22.927687  49.33798638 0.0000000
# Transect:Corn:2018-OpTIS:Soybeans:2015         40.0040671  26.798917  53.20921689 0.0000000
# OpTIS:Soybeans:2018-OpTIS:Soybeans:2015        12.5829178  -0.622232  25.78806751 0.0745156
# Transect:Soybeans:2018-OpTIS:Soybeans:2015     -0.5345692 -13.739719  12.67058053 1.0000000
# OpTIS:Corn:2018-Transect:Soybeans:2015         34.9583149  21.753165  48.16346468 0.0000000
# Transect:Corn:2018-Transect:Soybeans:2015      38.8295455  25.624396  52.03469520 0.0000000
# OpTIS:Soybeans:2018-Transect:Soybeans:2015     11.4083961  -1.796754  24.61354581 0.1471762
# Transect:Soybeans:2018-Transect:Soybeans:2015  -1.7090909 -14.914241  11.49605884 0.9999298
# Transect:Corn:2018-OpTIS:Corn:2018              3.8712305  -9.333919  17.07638026 0.9864764 # ND
# OpTIS:Soybeans:2018-OpTIS:Corn:2018           -23.5499189 -36.755069 -10.34476912 0.0000028
# Transect:Soybeans:2018-OpTIS:Corn:2018        -36.6674058 -49.872556 -23.46225610 0.0000000
# OpTIS:Soybeans:2018-Transect:Corn:2018        -27.4211494 -40.626299 -14.21599964 0.0000000
# Transect:Soybeans:2018-Transect:Corn:2018     -40.5386364 -53.743786 -27.33348662 0.0000000
# Transect:Soybeans:2018-OpTIS:Soybeans:2018    -13.1174870 -26.322637   0.08766277 0.0529733 # ND



ntsil2 <- aov(value~source:crop_name:factor(year), data=dat[dat$year %in% c(2015, 2018) & dat$variable=="perc_nt" & dat$region=="Southern IL",])
summary(ntsil2)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name:factor(year)   7  48675    6954   16.92 <2e-16 ***
#   Residuals                     264 108482     411  
Tukoutntsil2 <- TukeyHSD(ntsil2)
Tukoutntsil2
# $`source:crop_name:factor(year)`
#                                                 diff        lwr         upr     p adj
# Transect:Corn:2015-OpTIS:Corn:2015            -24.9828181 -40.004665  -9.9609707 0.0000194  ## DIFF
# OpTIS:Soybeans:2015-OpTIS:Corn:2015             8.8037464  -6.218101  23.8255938 0.6269888
# Transect:Soybeans:2015-OpTIS:Corn:2015          7.4260055  -7.595842  22.4478529 0.8013665
# OpTIS:Corn:2018-OpTIS:Corn:2015                -1.7371851 -16.759032  13.2846623 0.9999667
# Transect:Corn:2018-OpTIS:Corn:2015            -25.6122298 -40.634077 -10.5903824 0.0000105
# OpTIS:Soybeans:2018-OpTIS:Corn:2015            -5.9806621 -21.002510   9.0411853 0.9265372
# Transect:Soybeans:2018-OpTIS:Corn:2015         10.3083584  -4.713489  25.3302058 0.4197646
# OpTIS:Soybeans:2015-Transect:Corn:2015         33.7865644  18.764717  48.8084118 0.0000000
# Transect:Soybeans:2015-Transect:Corn:2015      32.4088235  17.386976  47.4306709 0.0000000
# OpTIS:Corn:2018-Transect:Corn:2015             23.2456330   8.223786  38.2674804 0.0000994
# Transect:Corn:2018-Transect:Corn:2015          -0.6294118 -15.651259  14.3924356 1.0000000
# OpTIS:Soybeans:2018-Transect:Corn:2015         19.0021559   3.980309  34.0240033 0.0034622
# Transect:Soybeans:2018-Transect:Corn:2015      35.2911765  20.269329  50.3130239 0.0000000
# Transect:Soybeans:2015-OpTIS:Soybeans:2015     -1.3777409 -16.399588  13.6441065 0.9999932  ## ND
# OpTIS:Corn:2018-OpTIS:Soybeans:2015           -10.5409314 -25.562779   4.4809160 0.3895844
# Transect:Corn:2018-OpTIS:Soybeans:2015        -34.4159762 -49.437824 -19.3941288 0.0000000
# OpTIS:Soybeans:2018-OpTIS:Soybeans:2015       -14.7844085 -29.806256   0.2374389 0.0572877
# Transect:Soybeans:2018-OpTIS:Soybeans:2015      1.5046120 -13.517235  16.5264594 0.9999875
# OpTIS:Corn:2018-Transect:Soybeans:2015         -9.1631905 -24.185038   5.8586569 0.5770489
# Transect:Corn:2018-Transect:Soybeans:2015     -33.0382353 -48.060083 -18.0163879 0.0000000
# OpTIS:Soybeans:2018-Transect:Soybeans:2015    -13.4066676 -28.428515   1.6151798 0.1191582
# Transect:Soybeans:2018-Transect:Soybeans:2015   2.8823529 -12.139494  17.9042003 0.9990179
# Transect:Corn:2018-OpTIS:Corn:2018            -23.8750448 -38.896892  -8.8531973 0.0000556  # DIFF
# OpTIS:Soybeans:2018-OpTIS:Corn:2018            -4.2434771 -19.265324  10.7783703 0.9889672
# Transect:Soybeans:2018-OpTIS:Corn:2018         12.0455435  -2.976304  27.0673909 0.2220334
# OpTIS:Soybeans:2018-Transect:Corn:2018         19.6315677   4.609720  34.6534151 0.0021337
# Transect:Soybeans:2018-Transect:Corn:2018      35.9205882  20.898741  50.9424356 0.0000000
# Transect:Soybeans:2018-OpTIS:Soybeans:2018     16.2890205   1.267173  31.3108680 0.0230924 # DIFF

rtsil2 <- aov(value~source:crop_name:factor(year), data=dat[dat$year %in% c(2015, 2018) & dat$variable=="perc_rt" & dat$region=="Southern IL",])
summary(rtsil2)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name:factor(year)   7  25191    3599   23.98 <2e-16 ***
#   Residuals                     263  39463     150       
Tukoutrtsil2 <- TukeyHSD(rtsil2)
Tukoutrtsil2
# $`source:crop_name:factor(year)`
#                                                 diff        lwr         upr     p adj
# Transect:Corn:2015-OpTIS:Corn:2015            -13.4156076 -22.5618525  -4.2693627 0.0002931  ## DIFF
# OpTIS:Soybeans:2015-OpTIS:Corn:2015             2.4958428  -6.6504021  11.6420876 0.9910262
# Transect:Soybeans:2015-OpTIS:Corn:2015        -11.5744311 -20.7206760  -2.4281863 0.0034430
# OpTIS:Corn:2018-OpTIS:Corn:2015                -3.5917318 -12.7379766   5.5545131 0.9314388
# Transect:Corn:2018-OpTIS:Corn:2015            -14.3126664 -23.4589113  -5.1664216 0.0000783
# OpTIS:Soybeans:2018-OpTIS:Corn:2015            14.5924918   5.4462469  23.7387367 0.0000511
# Transect:Soybeans:2018-OpTIS:Corn:2015        -13.5950194 -22.7412642  -4.4487745 0.0002265
# OpTIS:Soybeans:2015-Transect:Corn:2015         15.9114504   6.8337177  24.9891831 0.0000051
# Transect:Soybeans:2015-Transect:Corn:2015       1.8411765  -7.2365562  10.9189092 0.9985934
# OpTIS:Corn:2018-Transect:Corn:2015              9.8238758   0.7461431  18.9016086 0.0235778
# Transect:Corn:2018-Transect:Corn:2015          -0.8970588  -9.9747915   8.1806739 0.9999886
# OpTIS:Soybeans:2018-Transect:Corn:2015         28.0080994  18.9303667  37.0858321 0.0000000
# Transect:Soybeans:2018-Transect:Corn:2015      -0.1794118  -9.2571445   8.8983209 1.0000000
# Transect:Soybeans:2015-OpTIS:Soybeans:2015    -14.0702739 -23.1480066  -4.9925412 0.0000962  ## DIFF
# OpTIS:Corn:2018-OpTIS:Soybeans:2015            -6.0875745 -15.1653072   2.9901582 0.4510120
# Transect:Corn:2018-OpTIS:Soybeans:2015        -16.8085092 -25.8862419  -7.7307765 0.0000011
# OpTIS:Soybeans:2018-OpTIS:Soybeans:2015        12.0966490   3.0189163  21.1743818 0.0015743
# Transect:Soybeans:2018-OpTIS:Soybeans:2015    -16.0908621 -25.1685948  -7.0131294 0.0000038
# OpTIS:Corn:2018-Transect:Soybeans:2015          7.9826994  -1.0950333  17.0604321 0.1312122
# Transect:Corn:2018-Transect:Soybeans:2015      -2.7382353 -11.8159680   6.3394974 0.9837723
# OpTIS:Soybeans:2018-Transect:Soybeans:2015     26.1669229  17.0891902  35.2446556 0.0000000
# Transect:Soybeans:2018-Transect:Soybeans:2015  -2.0205882 -11.0983209   7.0571445 0.9974525
# Transect:Corn:2018-OpTIS:Corn:2018            -10.7209347 -19.7986674  -1.6432020 0.0087014  # DIFF
# OpTIS:Soybeans:2018-OpTIS:Corn:2018            18.1842236   9.1064909  27.2619563 0.0000001
# Transect:Soybeans:2018-OpTIS:Corn:2018        -10.0032876 -19.0810203  -0.9255549 0.0194694
# OpTIS:Soybeans:2018-Transect:Corn:2018         28.9051582  19.8274255  37.9828909 0.0000000
# Transect:Soybeans:2018-Transect:Corn:2018       0.7176471  -8.3600857   9.7953798 0.9999976 
# Transect:Soybeans:2018-OpTIS:Soybeans:2018    -28.1875112 -37.2652439 -19.1097785 0.0000000 # DIFF



ctsil2 <- aov(value~source:crop_name:factor(year), data=dat[dat$year %in% c(2015, 2018) & dat$variable=="perc_ct" & dat$region=="Southern IL",])
summary(ctsil2)
# Df Sum Sq Mean Sq F value Pr(>F)    
# source:crop_name:factor(year)   7  99069   14153      43 <2e-16 ***
#   Residuals                     263  86556     329 
Tukoutctsil2 <- TukeyHSD(ctsil2)
Tukoutctsil2
# $`source:crop_name:factor(year)`
# diff        lwr         upr     p adj
# Transect:Corn:2015-OpTIS:Corn:2015             40.2407603  26.695261  53.7862595 0.0000000  # DIFF
# OpTIS:Soybeans:2015-OpTIS:Corn:2015           -13.1365727 -26.682072   0.4089264 0.0646776
# Transect:Soybeans:2015-OpTIS:Corn:2015          5.9760544  -7.569445  19.5215536 0.8793711
# OpTIS:Corn:2018-OpTIS:Corn:2015                 6.3788045  -7.166695  19.9243036 0.8383109
# Transect:Corn:2018-OpTIS:Corn:2015             41.5613485  28.015849  55.1068477 0.0000000
# OpTIS:Soybeans:2018-OpTIS:Corn:2015            -7.8408200 -21.386319   5.7046792 0.6418280
# Transect:Soybeans:2018-OpTIS:Corn:2015          5.1584074  -8.387092  18.7039065 0.9414122
# OpTIS:Soybeans:2015-Transect:Corn:2015        -53.3773331 -66.821366 -39.9332998 0.0000000
# Transect:Soybeans:2015-Transect:Corn:2015     -34.2647059 -47.708739 -20.8206726 0.0000000
# OpTIS:Corn:2018-Transect:Corn:2015            -33.8619559 -47.305989 -20.4179226 0.0000000
# Transect:Corn:2018-Transect:Corn:2015           1.3205882 -12.123445  14.7646215 0.9999891
# OpTIS:Soybeans:2018-Transect:Corn:2015        -48.0815803 -61.525614 -34.6375470 0.0000000
# Transect:Soybeans:2018-Transect:Corn:2015     -35.0823529 -48.526386 -21.6383196 0.0000000
# Transect:Soybeans:2015-OpTIS:Soybeans:2015     19.1126272   5.668594  32.5566605 0.0005245  # DIFF
# OpTIS:Corn:2018-OpTIS:Soybeans:2015            19.5153772   6.071344  32.9594105 0.0003571
# Transect:Corn:2018-OpTIS:Soybeans:2015         54.6979213  41.253888  68.1419546 0.0000000
# OpTIS:Soybeans:2018-OpTIS:Soybeans:2015         5.2957528  -8.148281  18.7397861 0.9303623
# Transect:Soybeans:2018-OpTIS:Soybeans:2015     18.2949801   4.850947  31.7390134 0.0011189
# OpTIS:Corn:2018-Transect:Soybeans:2015          0.4027500 -13.041283  13.8467833 1.0000000
# Transect:Corn:2018-Transect:Soybeans:2015      35.5852941  22.141261  49.0293274 0.0000000
# OpTIS:Soybeans:2018-Transect:Soybeans:2015    -13.8168744 -27.260908  -0.3728411 0.0391180
# Transect:Soybeans:2018-Transect:Soybeans:2015  -0.8176471 -14.261680  12.6263862 0.9999996
# Transect:Corn:2018-OpTIS:Corn:2018             35.1825441  21.738511  48.6265774 0.0000000  # DIFF
# OpTIS:Soybeans:2018-OpTIS:Corn:2018           -14.2196244 -27.663658  -0.7755911 0.0297258
# Transect:Soybeans:2018-OpTIS:Corn:2018         -1.2203971 -14.664430  12.2236362 0.9999937
# OpTIS:Soybeans:2018-Transect:Corn:2018        -49.4021685 -62.846202 -35.9581352 0.0000000
# Transect:Soybeans:2018-Transect:Corn:2018     -36.4029412 -49.846974 -22.9589079 0.0000000
# Transect:Soybeans:2018-OpTIS:Soybeans:2018     12.9992273  -0.444806  26.4432606 0.0662586  # ND

