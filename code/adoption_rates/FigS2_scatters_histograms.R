# script started 6/13/2024 by Bonnie McGill
# Modified by Dan Manter
# Modified by McGill 3/17/25

# This script creates Fig. S2  plots 

################################################
# FIRST RUN "0_Load_adopt_data.R" to load packages and data  
################################################

# Further prep data

rm(dat, clm2, clm)
cens$crop_name <- rep("Soybeans", nrow(cens))
censb <- cens
censb$crop_name <- rep("Corn", nrow(censb))
cens <- bind_rows(cens, censb)
rm(censb)


# make optis and transect year -1 dataset
cens1 <- cens %>%   select(-source, -year) # AgCensus
opt1 <- opt %>%   select(-source, -year)  # OpTIS
till1 <- till %>%   select(-source, -year) # Survey
# Combine 3 datasets (opt not available for external users)
dat1 <- cens1 %>%
  full_join(opt1, by=c('year_1', 'state', 'county', 'variable', 'crop_name')) 
names(dat1) <- c('state', 'county', 'variable', 'AgCensus', 'crop_name', "year_1", 'OpTIS')

dat1 <- dat1 %>%
  full_join(till1, by=c('state', 'county', 'year_1', 'crop_name', 'variable'))
names(dat1) <- c('state', 'county', 'variable', 'AgCensus', 'crop_name', "year", 'OpTIS', 'Transect')
dat1$set <- rep("year-1", times=nrow(dat1))

# make optis and transect year-0 dataset
cens <- cens %>%   select(-source, -year_1) # AgCensus
opt <- opt %>%   select(-source, -year_1)  # OpTIS
till <- till %>%   select(-source, -year_1) # Survey
# Combine 3 datasets (opt not available for external users)
dat0 <- cens %>%
  full_join(opt, by=c('year', 'state', 'county', 'variable', 'crop_name')) 
names(dat0) <- c('year', 'state', 'county', 'variable', 'AgCensus', 'crop_name', 'OpTIS')
dat0 <- dat0 %>%
  full_join(till, by=c('state', 'county', 'year', 'crop_name', 'variable'))
names(dat0) <- c('year', 'state', 'county', 'variable', 'AgCensus', 'crop_name', 'OpTIS', 'Transect')
dat0$set <- rep("year-0", times=nrow(dat0))
dat0<- dat0[,c("state", "county", "variable", "AgCensus", "crop_name", "year", "OpTIS", "Transect", "set")]

rm(cens, cens1, clm, clm2, opt, opt1, till, till1, gddmax, scale_till)

dat <- full_join(dat0,dat1)

# pull out 2017 data (year that all 3 data sources overlap)
dat.2017 <- dat %>%
  filter(year == 2017)

dat.2017 <- dat.2017[dat.2017$variable != 'perc_cc',]
dat.2017$variable <- factor(dat.2017$variable, levels=c('perc_ct', 'perc_rt', 'perc_nt'))
levels(dat.2017$variable) <- list('No-till'="perc_nt", 'Reduced till'="perc_rt", 'Conventional till'="perc_ct")




### MAKE PLOTS


### plots comparing tillage estimates for  OpTIS and Transect - Illinois only

################### Fig. S2a-d ###################

windows(xpinch=200, ypinch=200, width=5, height=5)

till3col <- c("#88CCEE", "#999933", "#AA4499")


ggplot(dat.2017, aes(x=Transect, y=OpTIS)) +                              
  geom_abline(slope=1, intercept=0) +
  geom_point(aes(fill=variable), shape=21, size=2) + 
  scale_fill_manual(values=till3col, name='Tillage Type') +
  ggpubr::stat_cor(method='spearman',
                   aes(label=after_stat(r.label)),  # not showing p-value because this is not a subsample of a whole population, this is all the counties
                   r.accuracy=0.01,
                   #aes(label = paste(after_stat(rr.label), after_stat(p.label), sep="~`,`~")), makes it say R2 which technically is incorrect
                   label.y=95, label.x=5, size=6) +
  facet_grid(set ~ crop_name, 
             labeller=as_labeller(c("year-0" = '2017 Crop Year', 
                                    "year-1"="2018 Crop Year",
                                    "Corn" = "Corn",
                                    "Soybeans"= "Soybean"))) + 
  scale_x_continuous(name="Adoption (%) from Transect", limits=c(0,100), expand=expansion(mult=c(0.05,0.05))) +
  scale_y_continuous(name="Adoption (%) from OpTIS", limits=c(0,100), expand=expansion(mult=c(0,0.05))) +
  theme_bw() + 
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
        legend.key.spacing.x=unit(1, "cm"),
        legend.position = "top",
        legend.box.spacing = unit(0.05, "cm"),
        legend.text=element_text(size=14, margin=margin(l=0.05)),
        strip.text=element_text(size=14, face="bold"),
        legend.title=element_text(size=16),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12))
ggsave("plots/adoption/FigS2a-d.png", height=7, width=7)


### plot residuals by crop


################### Fig. S2e-h ###################

resid.2017 <- dat.2017 %>%
  mutate(resid = OpTIS - Transect)


resid.2017 %>%
  group_by(set, crop_name, variable) %>%
  summarize(mean = mean(resid, na.rm=T),
            sd = sd(resid, na.rm=T)) 

# # A tibble: 12 × 5
# # Groups:   set, crop_name [4]
# set    crop_name variable            mean    sd
# <chr>  <chr>     <fct>              <dbl> <dbl>
#  1 year-0 Corn      No-till            20.7   24.4
#  2 year-0 Corn      Reduced till        7.70  20.8
#  3 year-0 Corn      Conventional till -30.2   33.7
#  4 year-0 Soybeans  No-till            -9.40  25.9
#  5 year-0 Soybeans  Reduced till       17.6   13.4
#  6 year-0 Soybeans  Conventional till  -9.42  19.8
#  7 year-1 Corn      No-till             4.49  27.2
#  8 year-1 Corn      Reduced till       10.2   18.9
#  9 year-1 Corn      Conventional till -14.9   31.3
# 10 year-1 Soybeans  No-till           -34.5   26.3
# 11 year-1 Soybeans  Reduced till       31.9   18.8
# 12 year-1 Soybeans  Conventional till   3.06  18.6

ggplot(resid.2017, aes(x=resid, fill=variable)) +                       
  geom_density(aes(color=variable), alpha=0.4) +
  scale_color_manual(values=till3col, name="Tillage type") +
  scale_fill_manual(values=till3col, name="Tillage type")+
  facet_grid(set ~ crop_name,  
             labeller=as_labeller(c("year-0" = '2017 Crop Year', 
                                    "year-1"="2018 Crop Year",
                                    "Corn" = "Corn",
                                    "Soybeans"= "Soybean"))) + 
  scale_x_continuous(name="OpTIS (%) - Transect (%)", limits=c(-100, 100), expand=c(0.05,0.05))+ 
  scale_y_continuous(name="Density", expand=c(0,0)) + 
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
        legend.key.spacing.x=unit(1, "cm"),
        legend.position = "top",
        legend.box.spacing = unit(0.2, "cm"),
        legend.text=element_text(size=14, margin=margin(l=5)),
        strip.text=element_text(size=14, face="bold"),
        legend.title=element_text(size=16),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12))
ggsave("plots/adoption/FigS2e-h.png", height=7, width=7)


# are the residuals significantly different by tillage type?
till.tr <- aov(resid~variable*crop_name, data=resid.2017[resid.2017$set=="year-1",])
summary(till.tr)
#                      Df Sum Sq Mean Sq F value Pr(>F)    
# variable             2 140919   70460 122.087 <2e-16 ***
# crop_name            1      7       7   0.012  0.913    
# variable:crop_name   2 115156   57578  99.766 <2e-16 ***
#   Residuals          590 340505     577                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 370 observations deleted due to missingness

TukeyHSD(till.tr)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = resid ~ variable * crop_name, data = resid.2017)
# 
# $variable
# diff       lwr        upr     p adj
# Reduced-Conventional  27.02954  21.36358  32.695498 0.0000000
# No-till-Conventional  -9.16837 -14.83433  -3.502411 0.0004646
# No-till-Reduced      -36.19791 -41.85673 -30.539090 0.0000000
# 
# $crop_name
# diff      lwr      upr     p adj
# Soybeans-Corn 0.2140375 -3.65128 4.079355 0.9134347
# 
# $`variable:crop_name`
# diff        lwr        upr     p adj
# Reduced:Corn-Conventional:Corn           25.106503  15.343933  34.869073 0.0000000
# No-till:Corn-Conventional:Corn           19.423339   9.660769  29.185909 0.0000003
# Conventional:Soybeans-Conventional:Corn  17.993871   8.231301  27.756441 0.0000029
# Reduced:Soybeans-Conventional:Corn       46.837247  37.099114  56.575380 0.0000000
# No-till:Soybeans-Conventional:Corn      -19.570260 -29.308393  -9.832128 0.0000002
# No-till:Corn-Reduced:Corn                -5.683164 -15.445733   4.079406 0.5560028
# Conventional:Soybeans-Reduced:Corn       -7.112632 -16.875202   2.649938 0.2975668
# Reduced:Soybeans-Reduced:Corn            21.730744  11.992611  31.468877 0.0000000
# No-till:Soybeans-Reduced:Corn           -44.676763 -54.414896 -34.938630 0.0000000
# Conventional:Soybeans-No-till:Corn       -1.429468 -11.192038   8.333102 0.9983564
# Reduced:Soybeans-No-till:Corn            27.413907  17.675774  37.152040 0.0000000
# No-till:Soybeans-No-till:Corn           -38.993600 -48.731733 -29.255467 0.0000000
# Reduced:Soybeans-Conventional:Soybeans   28.843375  19.105243  38.581508 0.0000000
# No-till:Soybeans-Conventional:Soybeans  -37.564132 -47.302264 -27.825999 0.0000000
# No-till:Soybeans-Reduced:Soybeans       -66.407507 -76.121141 -56.693873 0.0000000







### plots comparing tillage estimates for OpTIS and AgCensus


################### Fig. S2i-l ###################

dat.2017.mean <- dat.2017 %>%
  group_by(state,county, variable, set) %>%  #, county, variable
  summarize(AgCensus = mean(AgCensus, na.rm=TRUE),
            OpTIS = mean(OpTIS, na.rm=TRUE),
            Transect = mean(Transect, na.rm=TRUE))

levels(dat.2017.mean$state) <- c("Illinois", "New York")

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(dat.2017.mean, aes(x=AgCensus, y=OpTIS)) +                  
  geom_abline(slope=1, intercept=0) +
  geom_point(aes(fill=variable), shape=21, size=2) + 
  scale_fill_manual(values=till3col, name='Tillage Type') +
  ggpubr::stat_cor(method='spearman',
                   # aes(label = paste(after_stat(rr.label), after_stat(p.label), sep="~`,`~")),
                   aes(label=after_stat(r.label)),  # not showing p-value because this is not a subsample of a whole population, this is all the counties
                   r.accuracy=0.01,
                   label.y=95, label.x=5, size=6) +
  facet_grid(set ~ state, 
             labeller=as_labeller(c("year-0" = '2017 Crop Year', 
                                    "year-1"="2018 Crop Year",
                                    "Illinois" = "Illinois",
                                    "New York"= "New York"))) + 
  scale_x_continuous(name="Adoption (%) from AgCensus", limits=c(0,100), expand=expansion(mult=c(0.05,0.05))) +
  scale_y_continuous(name="Adoption (%) from OpTIS", limits=c(0,100), expand=expansion(mult=c(0,0.05))) +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
        legend.key.spacing.x=unit(1, "cm"),
        legend.position = "top",
        legend.box.spacing = unit(0.05, "cm"),
        legend.text=element_text(size=14, margin=margin(l=0.05)),
        strip.text=element_text(size=14, face="bold"),
        legend.title=element_text(size=16),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12))
ggsave("plots/adoption/FigS2i-l.png", height=7, width=7)






### plot residuals by crop

################# Fig. S2m-p ##############################

resid.2017 <- dat.2017.mean %>%
  mutate(resid = OpTIS - AgCensus) %>%
  select(-Transect)


resid.2017 %>%
  group_by(set, state, variable) %>%
  summarize(mean = mean(resid, na.rm=T),
            sd = sd(resid, na.rm=T))

# # A tibble: 12 × 5
# # Groups:   set, variable [6]
# set    variable          state       mean    sd
# <chr>  <fct>             <chr>      <dbl> <dbl>
#  1 year-0 No-till           Illinois  18.1   13.3 
#  2 year-0 No-till           New York   8.07  19.9 
#  3 year-0 Reduced till      Illinois  -6.74  11.2 
#  4 year-0 Reduced till      New York   8.14  18.4 
#  5 year-0 Conventional till Illinois -13.6   11.5 
#  6 year-0 Conventional till New York -36.2    9.77
#  7 year-1 No-till           Illinois   1.92  11.5 
#  8 year-1 No-till           New York  12.1   19.1 
#  9 year-1 Reduced till      Illinois   0.243 10.9 
# 10 year-1 Reduced till      New York  14.2   20.8 
# 11 year-1 Conventional till Illinois  -2.62  12.7 
# 12 year-1 Conventional till New York -34.5   10.3 

mean(c(12.1, 14.2, 34.5))


ggplot(resid.2017, aes(x=resid, fill=variable)) +
  geom_density(aes(color=variable), alpha=0.4) +
  scale_color_manual(values=till3col, name="Tillage type") +
  scale_fill_manual(values=till3col, name="Tillage type")+
  facet_grid(set ~ state,  
             labeller=as_labeller(c("year-0" = '2017 Crop Year', 
                                    "year-1"="2018 Crop Year",
                                    "Illinois" = "Illinois",
                                    "New York"= "New York"))) + 
  scale_x_continuous(name="OpTIS (%) - AgCensus (%)", limits=c(-60, 60), expand=c(0.05,0.05))+ 
  scale_y_continuous(name="Density", expand=expansion(mult=c(0,0.05))) + 
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
        legend.key.spacing.x=unit(1, "cm"),
        legend.position = "top",
        legend.box.spacing = unit(0.2, "cm"),
        legend.text=element_text(size=14, margin=margin(l=5)),
        strip.text=element_text(size=14, face="bold"),
        legend.title=element_text(size=16),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12))
ggsave("plots/adoption/FigS2m-p.png", height=7, width=7)



# are the residuals significantly different by tillage type?

till.ce <- aov(resid~variable*state, data=resid.2017[resid.2017$set=="year-1",])
summary(till.ce)
#                  Df Sum Sq Mean Sq F value Pr(>F)    
# variable         2  14060    7030  40.799 <2e-16 ***
# state            1    266     266   1.546  0.214    
# variable:state   2  27181   13591  78.875 <2e-16 ***
#   Residuals      379  65304     172                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 98 observations deleted due to missingness

TukeyHSD(till.ce)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = resid ~ variable * state, data = resid.2017)
# 
# $variable
# diff       lwr       upr     p adj
# Reduced-Conventional 12.4018362  8.540934 16.262739 0.0000000
# No-till-Conventional 13.2607859  9.399883 17.121689 0.0000000
# No-till-Reduced       0.8589497 -2.986842  4.704741 0.8588886
# 
# $state
# diff       lwr      upr     p adj
# New York-Illinois -2.031593 -5.244445 1.181259 0.2145182
# 
# $`variable:state`
# diff         lwr        upr     p adj
# Reduced:Illinois-Conventional:Illinois        2.865868  -2.4250800   8.156816 0.6310444
# No-till:Illinois-Conventional:Illinois        4.543178  -0.7477699   9.834126 0.1390009
# Conventional:New York-Conventional:Illinois -31.834154 -40.1027859 -23.565521 0.0000000
# Reduced:New York-Conventional:Illinois       16.773634   8.7432866  24.803982 0.0000001
# No-till:New York-Conventional:Illinois       14.680641   6.6502934  22.710988 0.0000040
# No-till:Illinois-Reduced:Illinois             1.677310  -3.6136379   6.968258 0.9443404
# Conventional:New York-Reduced:Illinois      -34.700022 -42.9686539 -26.431389 0.0000000
# Reduced:New York-Reduced:Illinois            13.907766   5.8774186  21.938114 0.0000157
# No-till:New York-Reduced:Illinois            11.814773   3.7844254  19.845120 0.0004473
# Conventional:New York-No-till:Illinois      -36.377332 -44.6459641 -28.108699 0.0000000
# Reduced:New York-No-till:Illinois            12.230456   4.2001084  20.260803 0.0002390
# No-till:New York-No-till:Illinois            10.137463   2.1071152  18.167810 0.0045530
# Reduced:New York-Conventional:New York       48.607788  38.3675419  58.848033 0.0000000
# No-till:New York-Conventional:New York       46.514794  36.2745487  56.755040 0.0000000
# No-till:New York-Reduced:New York            -2.092993 -12.1418155   7.955829 0.9912297



### plots comparing cover crops estimates for OpTIS and AgCensus

############## Fig. S2q-r #####################

dat.2017 <- dat %>%
  filter(year == 2017)

dat.2017 <- dat.2017[dat.2017$variable == 'perc_cc',]

dat.2017.mean <- dat.2017 %>%
  group_by(state, county, variable, set) %>%
  summarize(AgCensus = mean(AgCensus, na.rm=TRUE),
            OpTIS = mean(OpTIS, na.rm=TRUE))

states2col <- c("#44AA99", "#332288")


ggplot(dat.2017.mean, aes(x=AgCensus, y=OpTIS, fill=state)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(shape=21, size=3) + 
  scale_fill_manual(values=states2col, labels=c('Illinois', 'New York'), name='State') +
  scale_color_manual(values=states2col, labels=c('Illinois', 'New York'), name='State') +
  ggpubr::stat_cor(method='spearman', 
                   aes(color=state,
                       label=after_stat(r.label)),  # not showing p-value because this is not a subsample of a whole population, this is all the counties
                   r.accuracy=0.01,
                   label.y=c(40,37), size=6) +
  facet_grid(variable~set, labeller=as_labeller(c("perc_cc" = 'Winter Cover Crops',
                                                  "year-0" = '2017 Crop Year',
                                                  "year-1"= "2018 Crop Year"))) + 
  scale_x_continuous(name="Adoption (%) from AgCensus", limits=c(0,40), expand=expansion(mult=c(0.05,0.05))) +
  scale_y_continuous(name="Adoption (%) from OpTIS", limits=c(0,40), expand=expansion(mult=c(0,0.05))) +
  theme_bw() +
  guides(color="none") +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
    legend.key.spacing.x=unit(1, "cm"),
    legend.position= c(0.91, 0.5),
    legend.box.spacing = unit(0.05, "cm"),
    legend.box.background = element_blank(),
    legend.text=element_text(size=12, margin=margin(l=0.05)),
    strip.text=element_text(size=12, face="bold"),
    legend.title=element_text(size=13),
    axis.title=element_text(size=12),
    axis.text=element_text(size=11))


ggsave("plots/adoption/FigS2q-r.png", height=3.5, width=6.4)






########### Fig. S2s-t ################

resid.2017 <- dat.2017.mean %>%
  mutate(resid = OpTIS - AgCensus)

resid.2017 %>%
  group_by(state, variable, set) %>%
  summarize(mean = mean(resid, na.rm=T),
            sd = sd(resid, na.rm=T))

# # A tibble: 4 × 5
# # Groups:   state, variable [2]
# state    variable set     mean    sd
# <chr>    <chr>    <chr>  <dbl> <dbl>
#   1 Illinois perc_cc  year-0  1.82  3.66
# 2 Illinois perc_cc  year-1  2.65  5.21
# 3 New York perc_cc  year-0 -6.09  6.30
# 4 New York perc_cc  year-1 -4.29  8.88

ggplot(resid.2017, aes(x=resid, fill=state)) +
  geom_density(aes(color=state), alpha=0.4) +
  scale_color_manual(values = states2col, labels=c('Illinois', 'New York'),  name='State') +
  scale_fill_manual(values=states2col, labels=c('Illinois', 'New York'), name='State') +
  facet_grid(variable ~ set, 
             labeller=as_labeller(c("perc_cc" = 'Winter Cover Crop',
                                    "year-0" = '2017 Crop Year',
                                    "year-1"= "2018 Crop Year"))) +
  scale_x_continuous(name="OpTIS (%) - AgCensus (%)", expand=c(0.05,0.05), limits=c(-30, 30))+ 
  scale_y_continuous(name="Density", limits=c(0, 0.12), breaks=c(0,0.03, 0.06, 0.09, 0.12), expand=c(0,0)) + 
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
        legend.key.spacing.x=unit(1, "cm"),
        legend.position = c(0.9, 0.7),
        legend.box.spacing = unit(0.2, "cm"),
        legend.text=element_text(size=11, margin=margin(l=5)),
        strip.text=element_text(size=12, face="bold"),
        legend.title=element_text(size=12),
        axis.title=element_text(size=12),
        axis.text=element_text(size=11))

ggsave("plots/adoption/FigS2s-t.png", height=3.5, width=6.55)



# are the residuals significantly different by tillage type?

cc.ce <- aov(resid~state, data=resid.2017)
summary(cc.ce)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# state         1   1196  1195.7    30.1 2.04e-07 ***
# Residuals   131   5204    39.7                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 28 observations deleted due to missingness

TukeyHSD(cc.ce)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = resid ~ state, data = resid.2017)
# 
# $state
# diff       lwr       upr p adj
# New York-Illinois -6.941947 -9.445089 -4.438805 2e-07

