# OpTIS adoption rate information for Illinois and New York aggregated by counties 2015-2021.
# Data shared with AFT from Regrow analysis.
# Column "kpi_name" gives different types of land use areas, with "area" meaning the 
# analyzed acres of the county, tillage type/ cover crop / etc.
# units are ACRES

# load packages
library(reshape2) # for dcast()
library(ggplot2)
library(dplyr)


# to update data, run "download data.R"


# standard error
se <- function(x){sd(x)/(sqrt(length(x)))}

# load the data
datil <- read.csv("data/optis/datil_cleanlong.csv")




# what are the state average adoption rates by year and crop
means_cropyear <- datil %>% 
                group_by(state, year, crop_name, variable) %>%
                summarize(Mean = mean(value), se=se(value))
                          

# # what are the state average adoption rates by year (across all crops)
means_year <- datil%>%
              group_by(state, year, variable) %>%
              summarize(Mean = mean(value))

# what are the state average adoption rates across all available years (2015-2021) per crop?
means_crop <- datil%>%
                    group_by(state, crop_name, variable) %>%
                    summarize(Mean = mean(value), se=se(value))

# what are the state average adoption rates across MOST RECENT 4 YEARS 2018-2021?  
# note results not all that different between means_crop and means_crop_1821
# for now, will report most recent data:
means_crop_1821 <- datil%>%
              filter(year>2017) %>%
              group_by(crop_name, variable) %>%
              summarize(Mean = mean(value), se=se(value))

means_1821 <- datil%>%
  filter(year>2017) %>%
  group_by(variable) %>%
  summarize(Mean = mean(value), se=se(value))

# what are the state average adoption rates across all years and crops
means_global <- datil%>%
                group_by(variable) %>%
                summarize(Mean = mean(value), se = se(value))


# plots for mean adoption rates

windows(xpinch=200, ypinch=200, width=5, height=5)


# State mean change over time
# facet by crop

pal4 <- c("#999933", "#332288", "#88ccee",  "#cc6677")


ggplot(data=means_cropyear, aes(x=year, y=Mean, group=variable)) +
  geom_line(aes(color=variable), linewidth=0.4) +
  geom_point(aes(color=variable), size=0.5) +
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se, color=variable), width=0.1, linewidth=0.6) +
  scale_color_manual(values=pal4, 
                     name="Practice",
                     breaks=c("perc_cc", "perc_nt", "perc_rt", "perc_ct"),
                     labels=c("Cover Crops", "No Till", "Reduced Till", "Conventional Till")) +
  facet_grid(rows=vars(crop_name)) + 
  scale_x_continuous(breaks=seq(2015,2021, 1), name="Year") +
  ylab("State Mean Percent Adoption") +
  theme(
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank() ,
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=14, face="bold"),
        axis.title.y=element_text(size=14, face="bold"),
        panel.background = element_rect(fill = 'white') ,
        panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
        strip.text=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        legend.title=element_text(size=12, face="bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        legend.key=element_rect(fill="white"),
        legend.key.size = unit(0.4, "cm")
  )	 

ggsave("plots/state_rates_15-21_IL.png")


library(lme4)
# rate of change - all practices together
summary(lm(value~year*variable + county + crop_name, data=datil))
mem <- lmer(value~year*variable + crop_name + (1|county), data=datil)
isSingular(mem)
summary(lm(value~year, data=datil))
summary(lm(value~year*variable, data=datil))
summary(lm(value~year + crop_name, data=datil))
summary(lm(value~year + county, data=datil))
summary(lm(value~year*variable + crop_name, data=datil)) # no improvement with crop_name
summary(lm(value~year*variable + county, data=datil)) # no improvement with county

# rate of change - cover crops
# summary(lm(value~year, data=datil[datil$variable %in% "perc_cc",])) # R2 only 0.05, p is sig., slope=0.926
summary(lm(value~year + county, data=datil[datil$variable %in% "perc_cc",])) # improves R2 to 0.5, slope=0.99 (+/-0.07)
# Call:
#   lm(formula = value ~ year + county, data = datil[datil$variable %in% 
#                                                      "perc_cc", ])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -14.267  -2.491  -0.324   1.764  38.313 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -1.999e+03  1.371e+02 -14.585  < 2e-16 ***
#   year               9.935e-01  6.793e-02  14.626  < 2e-16 ***
#   countyAlexander    1.896e+00  1.912e+00   0.991 0.321642    
# countyBond         1.698e+00  1.912e+00   0.888 0.374561    
# countyBoone       -2.741e+00  1.912e+00  -1.433 0.151975    
# countyBrown       -3.217e-01  1.912e+00  -0.168 0.866411    
# countyBureau      -3.058e+00  1.912e+00  -1.600 0.109936    
# countyCalhoun      4.204e+00  1.912e+00   2.199 0.028068 *  
#   countyCarroll     -3.324e+00  1.912e+00  -1.739 0.082350 .  
# countyCass        -8.630e-02  1.912e+00  -0.045 0.964008    
# countyChampaign   -3.935e+00  1.912e+00  -2.058 0.039803 *  
#   countyChristian   -3.923e+00  1.912e+00  -2.052 0.040386 *  
#   countyClark       -1.388e+00  1.912e+00  -0.726 0.467889    
# countyClay         4.996e+00  1.912e+00   2.613 0.009084 ** 
#   countyClinton      6.573e+00  1.912e+00   3.437 0.000606 ***
#   countyColes       -2.940e+00  1.912e+00  -1.537 0.124428    
# countyCook        -2.629e+00  2.038e+00  -1.290 0.197300    
# countyCrawford     4.076e+00  1.912e+00   2.132 0.033205 *  
#   countyCumberland  -1.616e+00  1.912e+00  -0.845 0.398132    
# countyDeKalb      -4.573e+00  1.912e+00  -2.392 0.016906 *  
#   countyDeWitt      -4.538e+00  1.912e+00  -2.373 0.017782 *  
#   countyDouglas     -3.246e+00  1.912e+00  -1.698 0.089842 .  
# countyDuPage      -4.128e+00  2.636e+00  -1.566 0.117664    
# countyEdgar       -1.970e+00  1.912e+00  -1.030 0.303069    
# countyEdwards      1.137e+01  1.912e+00   5.948 3.49e-09 ***
#   countyEffingham    4.211e-01  1.912e+00   0.220 0.825711    
# countyFayette      8.500e-02  1.912e+00   0.044 0.964550    
# countyFord        -4.937e+00  1.912e+00  -2.582 0.009933 ** 
#   countyFranklin     6.956e+00  1.912e+00   3.638 0.000286 ***
#   countyFulton      -2.022e+00  1.912e+00  -1.057 0.290482    
# countyGallatin     8.374e+00  1.912e+00   4.380 1.29e-05 ***
#   countyGreene       3.934e+00  1.912e+00   2.058 0.039822 *  
#   countyGrundy      -5.009e+00  1.949e+00  -2.571 0.010266 *  
#   countyHamilton     7.630e+00  1.912e+00   3.990 6.97e-05 ***
#   countyHancock     -2.669e+00  1.912e+00  -1.396 0.163055    
# countyHardin       1.078e+01  1.948e+00   5.533 3.82e-08 ***
#   countyHenderson   -2.838e+00  1.912e+00  -1.484 0.138000    
# countyHenry       -3.516e+00  1.912e+00  -1.839 0.066156 .  
# countyIroquois    -3.949e+00  1.912e+00  -2.065 0.039099 *  
#   countyJackson     -2.862e-01  1.990e+00  -0.144 0.885682    
# countyJasper      -3.640e-01  1.912e+00  -0.190 0.849056    
# countyJefferson    5.178e+00  1.912e+00   2.708 0.006861 ** 
#   countyJersey       2.684e+00  1.912e+00   1.404 0.160668    
# countyJo Daviess  -2.657e-01  1.912e+00  -0.139 0.889505    
# countyJohnson      6.557e+00  1.912e+00   3.429 0.000625 ***
#   countyKane        -4.658e+00  1.912e+00  -2.436 0.014973 *  
#   countyKankakee    -2.404e+00  1.912e+00  -1.257 0.208853    
# countyKendall     -5.117e+00  2.039e+00  -2.510 0.012202 *  
#   countyKnox        -3.231e+00  1.912e+00  -1.690 0.091329 .  
# countyLaSalle     -4.122e+00  1.912e+00  -2.156 0.031292 *  
#   countyLawrence     8.533e+00  1.912e+00   4.463 8.79e-06 ***
#   countyLee         -4.006e+00  1.912e+00  -2.095 0.036340 *  
#   countyLivingston  -4.795e+00  1.912e+00  -2.508 0.012265 *  
#   countyLogan       -2.458e+00  1.912e+00  -1.285 0.198927    
# countyMacon       -3.723e+00  1.912e+00  -1.947 0.051730 .  
# countyMacoupin     2.489e-01  1.912e+00   0.130 0.896438    
# countyMadison      6.051e+00  1.912e+00   3.165 0.001589 ** 
#   countyMarion       7.706e-01  1.912e+00   0.403 0.687002    
# countyMarshall    -1.774e+00  1.912e+00  -0.928 0.353804    
# countyMason        2.978e+00  1.912e+00   1.558 0.119542    
# countyMassac       8.915e+00  1.912e+00   4.663 3.45e-06 ***
#   countyMcDonough   -3.982e+00  1.912e+00  -2.083 0.037466 *  
#   countyMcHenry     -2.193e+00  1.912e+00  -1.147 0.251663    
# countyMcLean      -4.325e+00  1.912e+00  -2.262 0.023855 *  
#   countyMenard       6.064e-02  1.912e+00   0.032 0.974705    
# countyMercer      -2.681e+00  1.912e+00  -1.402 0.161071    
# countyMonroe       1.477e+01  1.912e+00   7.726 2.22e-14 ***
#   countyMontgomery  -1.730e+00  1.912e+00  -0.905 0.365626    
# countyMorgan       9.661e-01  1.912e+00   0.505 0.613450    
# countyMoultrie    -3.359e+00  1.912e+00  -1.757 0.079236 .  
# countyOgle        -3.835e+00  1.912e+00  -2.006 0.045087 *  
#   countyPeoria      -1.950e+00  1.912e+00  -1.020 0.308008    
# countyPerry        2.572e+00  1.949e+00   1.320 0.187050    
# countyPiatt       -4.213e+00  1.949e+00  -2.162 0.030797 *  
#   countyPike        -7.272e-02  1.912e+00  -0.038 0.969667    
# countyPope         4.115e+00  1.912e+00   2.152 0.031557 *  
#   countyPulaski      9.439e+00  1.912e+00   4.937 8.98e-07 ***
#   countyPutnam      -1.189e+00  1.912e+00  -0.622 0.534181    
# countyRandolph     8.947e+00  1.912e+00   4.679 3.19e-06 ***
#   countyRichland     6.168e+00  1.912e+00   3.226 0.001288 ** 
#   countyRock Island -1.140e+00  1.912e+00  -0.596 0.551247    
# countySaline       4.892e+00  1.912e+00   2.559 0.010625 *  
#   countySangamon    -1.152e+00  1.912e+00  -0.602 0.547039    
# countySchuyler    -7.136e-01  1.912e+00  -0.373 0.709067    
# countyScott        2.727e+00  1.912e+00   1.426 0.154118    
# countyShelby      -2.193e+00  1.912e+00  -1.147 0.251662    
# countySt. Clair    1.580e+01  1.912e+00   8.262 3.52e-16 ***
#   countyStark       -4.130e+00  1.912e+00  -2.160 0.030957 *  
#   countyStephenson  -6.315e-01  1.912e+00  -0.330 0.741259    
# countyTazewell     1.476e-01  1.912e+00   0.077 0.938464    
# countyUnion        3.340e+00  1.912e+00   1.747 0.080945 .  
# countyVermilion   -3.295e+00  1.912e+00  -1.723 0.085108 .  
# countyWabash       3.338e+00  1.912e+00   1.746 0.081054 .  
# countyWarren      -3.298e+00  1.912e+00  -1.725 0.084779 .  
# countyWashington   1.510e+00  1.912e+00   0.790 0.429734    
# countyWayne        9.004e+00  1.912e+00   4.709 2.76e-06 ***
#   countyWhite        1.484e+01  1.912e+00   7.762 1.69e-14 ***
#   countyWhiteside   -2.197e+00  1.912e+00  -1.149 0.250703    
# countyWill        -2.576e+00  1.912e+00  -1.347 0.178160    
# countyWilliamson   5.116e+00  1.912e+00   2.676 0.007548 ** 
#   countyWinnebago    2.186e+00  1.912e+00   1.143 0.253208    
# countyWoodford    -2.457e+00  1.912e+00  -1.285 0.199064    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.059 on 1291 degrees of freedom
# Multiple R-squared:  0.5398,	Adjusted R-squared:  0.5038 
# F-statistic: 14.99 on 101 and 1291 DF,  p-value: < 2.2e-16


# slope means that cc adoption is increasing by 1% per year. trend is significant (i.e. not zero)
# which looks about right given the plot above.
# at this rate we will reach 50% adoption in year ___?
# according to means_1821 cover crop adoption is at 7.9 (+/- 0.30)
# increasing by 1.0% per year
(50-7.9)/1  # 43 years to get to 50%.
2023+43 # 2066

# rate of change - no till
# summary(lm(value~year, data=datil[datil$variable %in% "perc_nt",])) # R2 only 0.02, p is sig., slope=-1.53
summary(lm(value~year + county, data=datil[datil$variable %in% "perc_nt",])) # improves R2 to 0.5, slope=-1.45 (+/0.2)
# Call:
#   lm(formula = value ~ year + county, data = datil[datil$variable %in% 
#                                                      "perc_nt", ])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -47.793  -7.967   0.570   8.859  45.678 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       2964.5416   365.3666   8.114 1.12e-15 ***
#   year                -1.4494     0.1810  -8.006 2.59e-15 ***
#   countyAlexander     22.7698     5.1463   4.425 1.05e-05 ***
#   countyBond          23.9945     5.1463   4.663 3.44e-06 ***
#   countyBoone         -6.2040     5.1463  -1.206 0.228216    
# countyBrown         15.9678     5.1463   3.103 0.001958 ** 
#   countyBureau       -12.0224     5.1463  -2.336 0.019634 *  
#   countyCalhoun       14.2181     5.1463   2.763 0.005811 ** 
#   countyCarroll        5.9702     5.1463   1.160 0.246214    
# countyCass           0.6519     5.1463   0.127 0.899219    
# countyChampaign    -16.3785     5.1463  -3.183 0.001494 ** 
#   countyChristian    -23.3070     5.1463  -4.529 6.47e-06 ***
#   countyClark          6.3738     5.1463   1.239 0.215742    
# countyClay          10.1940     5.1463   1.981 0.047815 *  
#   countyClinton       10.1540     5.1463   1.973 0.048695 *  
#   countyColes        -20.2754     5.1463  -3.940 8.58e-05 ***
#   countyCook          29.1096     5.1463   5.656 1.89e-08 ***
#   countyCrawford      14.3107     5.1463   2.781 0.005500 ** 
#   countyCumberland     8.9264     5.1463   1.735 0.083059 .  
# countyDeKalb       -14.3494     5.1463  -2.788 0.005375 ** 
#   countyDeWitt        -2.1956     5.1463  -0.427 0.669710    
# countyDouglas      -22.7506     5.1463  -4.421 1.06e-05 ***
#   countyDuPage        -3.3129     5.1463  -0.644 0.519850    
# countyEdgar         -6.9115     5.1463  -1.343 0.179502    
# countyEdwards       23.5126     5.1463   4.569 5.37e-06 ***
#   countyEffingham     10.8127     5.1463   2.101 0.035825 *  
#   countyFayette       20.1866     5.1463   3.923 9.21e-05 ***
#   countyFord          -6.7757     5.1463  -1.317 0.188191    
# countyFranklin      24.8324     5.1463   4.825 1.56e-06 ***
#   countyFulton        18.3592     5.1463   3.567 0.000373 ***
#   countyGallatin      -5.9395     5.1463  -1.154 0.248652    
# countyGreene         8.7123     5.1463   1.693 0.090705 .  
# countyGrundy        -5.4404     5.1463  -1.057 0.290633    
# countyHamilton      11.3478     5.1463   2.205 0.027623 *  
#   countyHancock       -1.8791     5.1463  -0.365 0.715072    
# countyHardin        38.9906     5.1463   7.576 6.68e-14 ***
#   countyHenderson     -8.7048     5.1463  -1.691 0.090982 .  
# countyHenry         10.6191     5.1463   2.063 0.039265 *  
#   countyIroquois      -3.0976     5.1463  -0.602 0.547338    
# countyJackson       18.3450     5.1463   3.565 0.000377 ***
#   countyJasper        12.8278     5.1463   2.493 0.012802 *  
#   countyJefferson     22.9676     5.1463   4.463 8.78e-06 ***
#   countyJersey        12.7080     5.1463   2.469 0.013662 *  
#   countyJo Daviess     8.5825     5.1463   1.668 0.095612 .  
# countyJohnson       47.8497     5.1463   9.298  < 2e-16 ***
#   countyKane          -4.9055     5.1463  -0.953 0.340661    
# countyKankakee      -2.2385     5.1463  -0.435 0.663647    
# countyKendall       -1.4330     5.1463  -0.278 0.780708    
# countyKnox          15.3801     5.1463   2.989 0.002855 ** 
#   countyLaSalle       -8.4003     5.1463  -1.632 0.102855    
# countyLawrence       6.0957     5.1463   1.184 0.236435    
# countyLee          -16.4313     5.1463  -3.193 0.001442 ** 
#   countyLivingston    -6.5642     5.1463  -1.276 0.202347    
# countyLogan         -4.9220     5.1463  -0.956 0.339038    
# countyMacon        -12.8886     5.1463  -2.504 0.012384 *  
#   countyMacoupin      -9.6262     5.1463  -1.871 0.061634 .  
# countyMadison        4.7305     5.1463   0.919 0.358157    
# countyMarion        14.7020     5.1463   2.857 0.004346 ** 
#   countyMarshall      -9.0389     5.1463  -1.756 0.079252 .  
# countyMason        -10.1023     5.1463  -1.963 0.049854 *  
#   countyMassac        35.8206     5.1463   6.961 5.34e-12 ***
#   countyMcDonough     -7.9933     5.1463  -1.553 0.120612    
# countyMcHenry       -9.3239     5.1463  -1.812 0.070249 .  
# countyMcLean        -3.7133     5.1463  -0.722 0.470701    
# countyMenard        -1.6965     5.1463  -0.330 0.741709    
# countyMercer         9.0603     5.1463   1.761 0.078546 .  
# countyMonroe         1.2510     5.1463   0.243 0.807974    
# countyMontgomery    -1.4259     5.1463  -0.277 0.781765    
# countyMorgan        -8.1442     5.1463  -1.583 0.113766    
# countyMoultrie     -22.0329     5.1463  -4.281 1.99e-05 ***
#   countyOgle          -4.4175     5.1463  -0.858 0.390831    
# countyPeoria         3.0369     5.1463   0.590 0.555211    
# countyPerry         25.4155     5.1463   4.939 8.88e-07 ***
#   countyPiatt        -12.9911     5.1463  -2.524 0.011707 *  
#   countyPike          -5.9879     5.1463  -1.164 0.244820    
# countyPope          34.7311     5.1463   6.749 2.23e-11 ***
#   countyPulaski       30.4770     5.1463   5.922 4.05e-09 ***
#   countyPutnam         1.2733     5.1463   0.247 0.804622    
# countyRandolph      21.6641     5.1463   4.210 2.73e-05 ***
#   countyRichland       9.5998     5.1463   1.865 0.062350 .  
# countyRock Island    7.6715     5.1463   1.491 0.136283    
# countySaline         8.4425     5.1463   1.641 0.101139    
# countySangamon     -18.9021     5.1463  -3.673 0.000249 ***
#   countySchuyler      13.7677     5.1463   2.675 0.007559 ** 
#   countyScott          4.4609     5.1463   0.867 0.386194    
# countyShelby        -5.3531     5.1463  -1.040 0.298438    
# countySt. Clair      7.2640     5.1463   1.412 0.158331    
# countyStark         -4.9967     5.1463  -0.971 0.331755    
# countyStephenson     3.6950     5.1463   0.718 0.472890    
# countyTazewell     -13.4315     5.1463  -2.610 0.009159 ** 
#   countyUnion         28.5026     5.1463   5.539 3.68e-08 ***
#   countyVermilion     -1.4834     5.1463  -0.288 0.773206    
# countyWabash         3.8457     5.1463   0.747 0.455029    
# countyWarren        -6.8013     5.1463  -1.322 0.186534    
# countyWashington    16.0017     5.1463   3.109 0.001915 ** 
#   countyWayne         14.9115     5.1463   2.898 0.003823 ** 
#   countyWhite         13.8173     5.1463   2.685 0.007346 ** 
#   countyWhiteside     -6.6493     5.1463  -1.292 0.196564    
# countyWill          20.9042     5.1463   4.062 5.15e-05 ***
#   countyWilliamson    34.4139     5.1463   6.687 3.36e-11 ***
#   countyWinnebago      7.9516     5.1463   1.545 0.122559    
# countyWoodford      -9.9353     5.1463  -1.931 0.053749 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 13.62 on 1312 degrees of freedom
# Multiple R-squared:  0.5692,	Adjusted R-squared:  0.5361 
# F-statistic: 17.16 on 101 and 1312 DF,  p-value: < 2.2e-16
# slope means that nt adoption is DECREASING on average by 1.5% per year.  trend is significant (i.e. not zero)
# which looks about right given the plot above.
# at this rate we will reach 50% adoption in year ___?
# according to means_1821 no till adoption is at 41.3% (+/- 0.70)
# decreasing by 1.45% per year
41.3/1.45  # in 28 years no no-till 


# rate of change - reduced till
# summary(lm(value~year, data=datil[datil$variable %in% "perc_rt",])) # R2 only 0.02, p is sig., slope=1.21
summary(lm(value~year + county, data=datil[datil$variable %in% "perc_rt",])) # improves R2 to 0.29, slope=1.14 (+/-0.2)
# Call:
#   lm(formula = value ~ year + county, data = datil[datil$variable %in% 
#                                                      "perc_rt", ])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -40.751  -8.911  -1.352   7.481  42.664 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -2.261e+03  3.479e+02  -6.500 1.14e-10 ***
#   year               1.142e+00  1.724e-01   6.624 5.09e-11 ***
#   countyAlexander   -1.481e+01  4.886e+00  -3.030 0.002490 ** 
#   countyBond        -1.368e+01  4.886e+00  -2.799 0.005195 ** 
#   countyBoone        5.246e+00  4.886e+00   1.074 0.283184    
# countyBrown       -8.456e+00  4.886e+00  -1.730 0.083780 .  
# countyBureau       4.690e+00  4.886e+00   0.960 0.337312    
# countyCalhoun     -1.294e+01  4.886e+00  -2.649 0.008167 ** 
#   countyCarroll      5.830e+00  4.886e+00   1.193 0.233024    
# countyCass        -1.764e+00  4.886e+00  -0.361 0.718212    
# countyChampaign    6.478e+00  4.886e+00   1.326 0.185151    
# countyChristian    3.979e+00  4.886e+00   0.814 0.415603    
# countyClark       -6.029e+00  4.886e+00  -1.234 0.217486    
# countyClay        -2.246e+00  4.886e+00  -0.460 0.645832    
# countyClinton     -4.578e+00  4.886e+00  -0.937 0.349034    
# countyColes       -1.862e-01  4.886e+00  -0.038 0.969601    
# countyCook        -2.055e+01  4.886e+00  -4.206 2.78e-05 ***
#   countyCrawford    -1.039e+01  4.886e+00  -2.126 0.033705 *  
#   countyCumberland  -3.437e+00  4.886e+00  -0.703 0.481976    
# countyDeKalb       1.165e+01  4.886e+00   2.385 0.017237 *  
#   countyDeWitt       2.383e+00  4.886e+00   0.488 0.625928    
# countyDouglas      2.008e+00  4.886e+00   0.411 0.681180    
# countyDuPage       1.901e+00  4.886e+00   0.389 0.697274    
# countyEdgar       -1.158e+00  4.886e+00  -0.237 0.812740    
# countyEdwards     -1.405e+01  4.886e+00  -2.875 0.004103 ** 
#   countyEffingham   -5.157e+00  4.886e+00  -1.055 0.291482    
# countyFayette     -1.300e+01  4.886e+00  -2.660 0.007916 ** 
#   countyFord         6.211e+00  4.886e+00   1.271 0.203902    
# countyFranklin    -1.281e+01  4.886e+00  -2.622 0.008855 ** 
#   countyFulton      -1.112e+01  4.886e+00  -2.276 0.023023 *  
#   countyGallatin     5.030e-02  4.886e+00   0.010 0.991788    
# countyGreene      -1.718e+00  4.886e+00  -0.352 0.725140    
# countyGrundy      -2.561e+00  5.087e+00  -0.503 0.614765    
# countyHamilton    -4.874e+00  4.886e+00  -0.997 0.318755    
# countyHancock     -8.292e-01  4.886e+00  -0.170 0.865282    
# countyHardin      -2.483e+01  4.980e+00  -4.987 6.97e-07 ***
#   countyHenderson    4.239e+00  4.886e+00   0.868 0.385823    
# countyHenry       -3.409e+00  4.886e+00  -0.698 0.485555    
# countyIroquois     2.256e+00  4.886e+00   0.462 0.644459    
# countyJackson     -1.120e+01  4.886e+00  -2.292 0.022087 *  
#   countyJasper      -3.985e+00  4.886e+00  -0.815 0.414984    
# countyJefferson   -1.239e+01  4.886e+00  -2.536 0.011344 *  
#   countyJersey      -7.416e+00  4.886e+00  -1.518 0.129332    
# countyJo Daviess  -2.261e+00  4.886e+00  -0.463 0.643710    
# countyJohnson     -3.392e+01  4.886e+00  -6.941 6.11e-12 ***
#   countyKane         9.031e+00  4.886e+00   1.848 0.064811 .  
# countyKankakee     1.078e+00  4.886e+00   0.221 0.825380    
# countyKendall     -7.198e+00  4.886e+00  -1.473 0.141005    
# countyKnox        -8.784e+00  4.886e+00  -1.798 0.072469 .  
# countyLaSalle     -6.121e+00  4.886e+00  -1.253 0.210528    
# countyLawrence    -5.709e+00  4.886e+00  -1.168 0.242919    
# countyLee          5.584e+00  4.886e+00   1.143 0.253353    
# countyLivingston  -1.735e+00  4.886e+00  -0.355 0.722626    
# countyLogan        5.942e+00  4.886e+00   1.216 0.224230    
# countyMacon        7.740e+00  4.886e+00   1.584 0.113462    
# countyMacoupin     5.085e+00  4.886e+00   1.041 0.298268    
# countyMadison     -1.202e+00  4.886e+00  -0.246 0.805661    
# countyMarion      -6.473e+00  4.886e+00  -1.325 0.185501    
# countyMarshall     6.130e+00  4.886e+00   1.255 0.209864    
# countyMason        7.594e+00  4.886e+00   1.554 0.120393    
# countyMassac      -2.453e+01  4.886e+00  -5.019 5.90e-07 ***
#   countyMcDonough    9.103e-01  4.886e+00   0.186 0.852239    
# countyMcHenry      4.029e+00  4.886e+00   0.825 0.409741    
# countyMcLean       8.109e-01  4.886e+00   0.166 0.868216    
# countyMenard       2.375e+00  4.886e+00   0.486 0.627030    
# countyMercer      -2.428e+00  4.886e+00  -0.497 0.619422    
# countyMonroe      -3.949e+00  4.886e+00  -0.808 0.419170    
# countyMontgomery  -9.739e-01  4.886e+00  -0.199 0.842052    
# countyMorgan      -3.340e+00  4.886e+00  -0.683 0.494412    
# countyMoultrie     5.116e+00  4.886e+00   1.047 0.295302    
# countyOgle         9.634e+00  4.886e+00   1.972 0.048876 *  
#   countyPeoria       3.337e-01  4.886e+00   0.068 0.945560    
# countyPerry       -1.599e+01  4.886e+00  -3.273 0.001092 ** 
#   countyPiatt        5.191e+00  4.886e+00   1.062 0.288319    
# countyPike        -1.105e-02  4.886e+00  -0.002 0.998196    
# countyPope        -2.588e+01  4.886e+00  -5.297 1.38e-07 ***
#   countyPulaski     -1.846e+01  4.886e+00  -3.778 0.000165 ***
#   countyPutnam      -4.814e-01  4.886e+00  -0.099 0.921540    
# countyRandolph    -1.439e+01  4.886e+00  -2.944 0.003292 ** 
#   countyRichland    -2.417e+00  4.886e+00  -0.495 0.620923    
# countyRock Island -6.766e-01  4.886e+00  -0.138 0.889900    
# countySaline      -5.453e+00  4.886e+00  -1.116 0.264687    
# countySangamon     1.314e+00  4.886e+00   0.269 0.788105    
# countySchuyler    -5.272e+00  4.886e+00  -1.079 0.280798    
# countyScott       -2.517e+00  4.886e+00  -0.515 0.606572    
# countyShelby       1.148e+00  4.886e+00   0.235 0.814236    
# countySt. Clair   -2.051e+00  4.886e+00  -0.420 0.674766    
# countyStark        3.793e+00  4.886e+00   0.776 0.437754    
# countyStephenson   4.042e+00  4.886e+00   0.827 0.408266    
# countyTazewell     3.105e+00  4.886e+00   0.635 0.525220    
# countyUnion       -2.047e+01  4.886e+00  -4.189 2.99e-05 ***
#   countyVermilion   -7.605e-01  4.886e+00  -0.156 0.876340    
# countyWabash      -1.627e+00  4.886e+00  -0.333 0.739168    
# countyWarren       1.187e+00  4.886e+00   0.243 0.808102    
# countyWashington  -7.131e+00  4.886e+00  -1.459 0.144708    
# countyWayne       -5.927e+00  4.886e+00  -1.213 0.225346    
# countyWhite       -7.295e+00  4.886e+00  -1.493 0.135684    
# countyWhiteside    5.492e+00  4.886e+00   1.124 0.261222    
# countyWill        -1.340e+01  4.886e+00  -2.743 0.006171 ** 
#   countyWilliamson  -2.141e+01  4.886e+00  -4.381 1.27e-05 ***
#   countyWinnebago    8.765e-01  4.886e+00   0.179 0.857676    
# countyWoodford     3.964e+00  4.886e+00   0.811 0.417427    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 12.93 on 1309 degrees of freedom
# Multiple R-squared:  0.3367,	Adjusted R-squared:  0.2855 
# F-statistic: 6.579 on 101 and 1309 DF,  p-value: < 2.2e-16

# slope means that rt adoption is increasing on average by 1.1% per year.  trend is significant (i.e. not zero)
# which looks about right given the plot above.
# at this rate we will reach 50% adoption in year ___?
# according to means_1821 rt adoption is at 41.4 (+/- 0.55)
# increasing by 1.14% per year
(50-41.4)/1.14  # 8 years to get to 50% adoption 
2023+8 # 2031

# rate of change - conventional till
# summary(lm(value~year, data=datil[datil$variable %in% "perc_ct",])) # R2 only 0.01, p is sig., slope=0.80
summary(lm(value~year + county, data=datil[datil$variable %in% "perc_ct",])) # improves R2 to 0.36, slope=0.80 (+/-0.1)
# Call:
#   lm(formula = value ~ year + county, data = datil[datil$variable %in% 
#                                                      "perc_ct", ])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -25.247  -6.208  -1.859   3.726  47.757 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -1566.2974   280.8973  -5.576 2.99e-08 ***
#   year                  0.7845     0.1392   5.636 2.14e-08 ***
#   countyAlexander      -8.0506     4.0068  -2.009 0.044722 *  
#   countyBond          -10.1269     3.9319  -2.576 0.010118 *  
#   countyBoone           1.0270     3.9319   0.261 0.793983    
# countyBrown          -9.1048     3.9319  -2.316 0.020735 *  
#   countyBureau          7.7794     3.9319   1.979 0.048080 *  
#   countyCalhoun        -4.0223     3.9319  -1.023 0.306508    
# countyCarroll       -11.6402     3.9319  -2.960 0.003128 ** 
#   countyCass            1.2114     3.9319   0.308 0.758056    
# countyChampaign      10.4037     3.9319   2.646 0.008245 ** 
#   countyChristian      19.9424     3.9319   5.072 4.51e-07 ***
#   countyClark          -1.5472     3.9319  -0.394 0.694013    
# countyClay           -8.1705     3.9319  -2.078 0.037907 *  
#   countyClinton        -6.2049     3.9319  -1.578 0.114789    
# countyColes          20.7860     3.9319   5.286 1.46e-07 ***
#   countyCook          -10.6254     4.0068  -2.652 0.008104 ** 
#   countyCrawford       -6.4214     3.9319  -1.633 0.102683    
# countyCumberland     -5.2969     3.9319  -1.347 0.178168    
# countyDeKalb          3.3057     3.9319   0.841 0.400655    
# countyDeWitt          0.3787     3.9319   0.096 0.923280    
# countyDouglas        21.2994     3.9319   5.417 7.22e-08 ***
#   countyDuPage         -1.7258     4.3086  -0.401 0.688823    
# countyEdgar           7.8157     3.9319   1.988 0.047048 *  
#   countyEdwards       -11.5478     3.9319  -2.937 0.003373 ** 
#   countyEffingham      -5.9030     3.9319  -1.501 0.133524    
# countyFayette        -7.2711     3.9319  -1.849 0.064649 .  
# countyFord            1.0005     3.9319   0.254 0.799183    
# countyFranklin      -13.2604     3.9319  -3.372 0.000767 ***
#   countyFulton         -7.7190     3.9319  -1.963 0.049840 *  
#   countyGallatin        6.1278     3.9319   1.558 0.119365    
# countyGreene         -6.5358     3.9319  -1.662 0.096708 .  
# countyGrundy         -1.8933     3.9319  -0.482 0.630219    
# countyHamilton       -7.2448     3.9319  -1.843 0.065622 .  
# countyHancock         3.1513     3.9319   0.801 0.423011    
# countyHardin        -10.1848     5.0761  -2.006 0.045019 *  
#   countyHenderson       4.6411     3.9319   1.180 0.238073    
# countyHenry          -8.5525     3.9319  -2.175 0.029800 *  
#   countyIroquois        1.0236     3.9319   0.260 0.794648    
# countyJackson        -7.8699     3.9319  -2.002 0.045544 *  
#   countyJasper         -9.3942     3.9319  -2.389 0.017027 *  
#   countyJefferson     -12.7719     3.9319  -3.248 0.001191 ** 
#   countyJersey         -5.0834     3.9319  -1.293 0.196290    
# countyJo Daviess     -6.9107     3.9319  -1.758 0.079052 .  
# countyJohnson       -15.0029     4.4447  -3.375 0.000759 ***
#   countyKane           -4.1964     3.9319  -1.067 0.286055    
# countyKankakee        0.6170     3.9319   0.157 0.875337    
# countyKendall        -3.0264     3.9319  -0.770 0.441617    
# countyKnox          -10.2056     3.9319  -2.596 0.009550 ** 
#   countyLaSalle        11.4996     3.9319   2.925 0.003508 ** 
#   countyLawrence       -0.9908     3.9319  -0.252 0.801082    
# countyLee            11.4162     3.9319   2.903 0.003753 ** 
#   countyLivingston      1.3927     3.9319   0.354 0.723240    
# countyLogan          -0.4451     3.9319  -0.113 0.909878    
# countyMacon           5.5645     3.9319   1.415 0.157246    
# countyMacoupin        5.0857     3.9319   1.293 0.196089    
# countyMadison        -3.3821     3.9319  -0.860 0.389858    
# countyMarion         -8.5882     3.9319  -2.184 0.029125 *  
#   countyMarshall        3.5057     3.9319   0.892 0.372766    
# countyMason           2.6886     3.9319   0.684 0.494228    
# countyMassac        -12.6730     3.9319  -3.223 0.001300 ** 
#   countyMcDonough       7.4854     3.9319   1.904 0.057165 .  
# countyMcHenry         5.4693     3.9319   1.391 0.164460    
# countyMcLean          3.3914     3.9319   0.863 0.388549    
# countyMenard         -0.1801     3.9319  -0.046 0.963471    
# countyMercer         -6.5632     3.9319  -1.669 0.095317 .  
# countyMonroe          3.1509     3.9319   0.801 0.423063    
# countyMontgomery      2.7785     3.9319   0.707 0.479911    
# countyMorgan         11.8076     3.9319   3.003 0.002725 ** 
#   countyMoultrie       17.4437     3.9319   4.436 9.92e-06 ***
#   countyOgle           -5.2208     3.9319  -1.328 0.184477    
# countyPeoria         -2.8465     3.9319  -0.724 0.469230    
# countyPerry         -13.4542     3.9319  -3.422 0.000642 ***
#   countyPiatt           8.3619     3.9319   2.127 0.033636 *  
#   countyPike            5.9559     3.9319   1.515 0.130081    
# countyPope          -10.9980     3.9319  -2.797 0.005232 ** 
#   countyPulaski       -12.4862     3.9319  -3.176 0.001531 ** 
#   countyPutnam         -0.3318     3.9319  -0.084 0.932757    
# countyRandolph       -8.6270     3.9319  -2.194 0.028406 *  
#   countyRichland       -7.3946     3.9319  -1.881 0.060243 .  
# countyRock Island    -7.1147     3.9319  -1.809 0.070610 .  
# countySaline         -3.5318     3.9319  -0.898 0.369228    
# countySangamon       18.0904     3.9319   4.601 4.62e-06 ***
#   countySchuyler       -8.6988     3.9319  -2.212 0.027116 *  
#   countyScott          -1.8768     3.9319  -0.477 0.633214    
# countyShelby          4.6249     3.9319   1.176 0.239708    
# countySt. Clair      -4.7090     3.9319  -1.198 0.231279    
# countyStark           1.4001     3.9319   0.356 0.721842    
# countyStephenson     -8.3031     3.9319  -2.112 0.034903 *  
#   countyTazewell       10.7528     3.9319   2.735 0.006328 ** 
#   countyUnion          -9.4937     4.0068  -2.369 0.017964 *  
#   countyVermilion       2.3291     3.9319   0.592 0.553715    
# countyWabash         -2.6135     3.9319  -0.665 0.506372    
# countyWarren          4.6256     3.9319   1.176 0.239646    
# countyWashington     -9.8650     3.9319  -2.509 0.012230 *  
#   countyWayne          -9.2636     3.9319  -2.356 0.018621 *  
#   countyWhite          -7.1835     3.9319  -1.827 0.067936 .  
# countyWhiteside       1.1798     3.9319   0.300 0.764172    
# countyWill          -10.4335     3.9319  -2.654 0.008063 ** 
#   countyWilliamson    -14.6437     3.9319  -3.724 0.000204 ***
#   countyWinnebago      -9.1806     3.9319  -2.335 0.019701 *  
#   countyWoodford        6.3137     3.9319   1.606 0.108572    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10.4 on 1292 degrees of freedom
# Multiple R-squared:  0.413,	Adjusted R-squared:  0.3671 
# F-statistic: 8.998 on 101 and 1292 DF,  p-value: < 2.2e-16


# slope means that ct adoption is increasing on average by 0.8% per year.  trend is significant (i.e. not zero)
# which looks about right given the plot above.
# at this rate we will reach 50% adoption in year ___?
# According to means_1821 conventional till adoption is at 17.0% (+/- 0.50)
# increasing by 0.78% per year
(50-17)/0.78  # 42 years to get to 50% adoption
2023+42 # 2065














# which counties have at least 5 years of consecutive measurements?
# work with a subset of data
subs <- datw[,c(1,2,5,4)]
# 
subs <- subs %>%
  group_by(state, county, crop_name) %>%
  mutate(con_yrs = cumsum(c(1, diff(year)>1))) %>%  # make col that is "1" for consecutive years, 2 if non-consecutive years
  group_by(state, county, crop_name, con_yrs) %>% # group by new column
  mutate(start = min(year), end = max(year), diff=end - start) # list first and last year of consecutive groups

# join back together with rest of data
datw2 <- left_join(datw, subs) %>%
  filter(diff>4) # only keep county-crop-years with at least 5 years consecutive data 2015-2021
# removes 102 county-crop-years
ggplot(data=datw2, aes(y=perc_cc, x=year, group=county)) +
  geom_line(aes(color=county), show.legend=F) + #, position="jitter", size=0.5) +
  facet_grid(rows=vars(crop_name), cols=vars(state))
unique(datw2$diff)

sum(is.na(datw$perc_cc))
sum(is.na(datw$acres))
sum(is.na(datw$cc.acres))
test <- datw[rowSums(is.na(datw))>0,]
# turns out that we create NAs for counties where certain kpi_names are not measured
# for that county that year that crop
# so need to do the subs code above for each kpi_name