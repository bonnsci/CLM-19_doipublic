# script started 6/13/2024 by Bonnie McGill
# Modified by Dan Manter
# Modified by McGill 3/12/25

# This script creates Fig. S3 
# "Comparison of adoption data sources for 
# tillage (a-d) and cover crops (e-f) across Illinois (102 counties) and New York 
# (42 counties) in 2017."

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

cens <- cens %>%   select(-source) # AgCensus
opt <- opt %>%   select(-source)  # OpTIS
till <- till %>%   select(-source) # Survey

### Combine 3 datasets (opt not available for external users)
dat <- cens %>%
  full_join(opt, by=c('year', 'state', 'county', 'variable', 'crop_name')) 
names(dat) <- c('year', 'state', 'county', 'variable', 'AgCensus', 'crop_name', 'OpTIS')

dat <- dat %>%
  full_join(till, by=c('state', 'county', 'year', 'crop_name', 'variable'))
names(dat) <- c('year', 'state', 'county', 'variable', 'AgCensus', 'crop_name', 'OpTIS', 'Transect')


# pull out 2017 data (year that all 3 data sources overlap)
dat.2017 <- dat %>%
  filter(year == 2017)

dat.2017 <- dat.2017[dat.2017$variable != 'perc_cc',]
dat.2017$variable <- factor(dat.2017$variable, levels=c('perc_ct', 'perc_rt', 'perc_nt'))
levels(dat.2017$variable) <- c('Conventional', 'Reduced', 'No-till')




### MAKE PLOTS


### plots comparing tillage estimates for  OpTIS and Transect - Illinois only

################### Fig. S3a ###################

windows(xpinch=200, ypinch=200, width=5, height=5)

till3col <- c("#88CCEE", "#999933", "#AA4499")


ggplot(dat.2017, aes(x=Transect, y=OpTIS)) +                              
  geom_abline(slope=1, intercept=0) +
  geom_point(aes(fill=variable), shape=21, size=3) + 
  #scale_fill_brewer(palette='Set1', labels=c('Corn', 'Soybeans'), name='Crop') +
  scale_fill_manual(values=till3col, name='Tillage Type') +
  # scale_fill_brewer(palette='Set1', name='Tillage Type') +
  #stat_smooth(method='lm', se=FALSE) +
  ggpubr::stat_cor(method='spearman',
                   aes(label=after_stat(r.label)),  # not showing p-value because this is not a subsample of a whole population, this is all the counties
                       r.accuracy=0.01,
                   #aes(label = paste(after_stat(rr.label), after_stat(p.label), sep="~`,`~")), makes it say R2 which technically is incorrect

                   label.y=100) +
  facet_grid(. ~ crop_name) + 
  xlim(0,100) + ylim(0,100) +
  labs(y="Adoption (%) from OpTIS", x="Adoption (%) from Transect") + 
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
ggsave("plots/adoption/OpTIS_vs_Transect.scatter.png", height=4, width=7)


### plot residuals by crop


################### Fig. S3b ###################

resid.2017 <- dat.2017 %>%
  mutate(resid = OpTIS - Transect)


resid.2017 %>%
  group_by(crop_name, variable) %>%
  summarize(mean = mean(resid, na.rm=T),
            sd = sd(resid, na.rm=T))

ggplot(resid.2017, aes(x=resid, fill=variable)) +                       
  #geom_histogram(aes(y = ..density..), position="identity", alpha=0.5, color='black') +
  geom_density(aes(color=variable), alpha=0.4) +
  scale_color_manual(values=till3col, name="Tillage type") +
  scale_fill_manual(values=till3col, name="Tillage type")+
  # scale_color_brewer(palette='Set1', name='Tillage Type') +
  # scale_fill_brewer(palette='Set1', name='Tillage Type') +
  facet_grid(. ~ crop_name) + 
  labs(y="Density", x="OpTIS (%) - Transect (%)") + 
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
        legend.key.spacing.x=unit(1, "cm"),
        legend.position = "bottom",
        legend.box.spacing = unit(0.2, "cm"),
        legend.text=element_text(size=14, margin=margin(l=5)),
        strip.text=element_text(size=14, face="bold"),
        legend.title=element_text(size=16),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12))
ggsave("plots/adoption/OpTIS_vs_Transect.histogram.png", height=4, width=7)


# are the residuals significantly different by tillage type?
till.tr <- aov(resid~variable*crop_name, data=resid.2017)
summary(till.tr)
#                      Df Sum Sq Mean Sq F value Pr(>F)    
# variable             2 112941   56471   99.79 <2e-16 ***
# crop_name            1      0       0    0.00  0.989    
# variable:crop_name   2  70374   35187   62.18 <2e-16 ***
# Residuals          581 328797     566                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 379 observations deleted due to missingness

TukeyHSD(till.tr)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = resid ~ variable * crop_name, data = resid.2017)
# 
# $variable
# diff       lwr       upr     p adj
# Reduced-Conventional 32.412465  26.75118 38.073747 0.0000000
# No-till-Conventional 25.338301  19.67702 30.999584 0.0000000
# No-till-Reduced      -7.074164 -12.70634 -1.441988 0.0092173
# 
# $crop_name
# diff       lwr      upr     p adj
# Soybeans-Corn 0.02760406 -3.829366 3.884574 0.9887896
# 
# $`variable:crop_name`
# diff         lwr        upr     p adj
# Reduced:Corn-Conventional:Corn           37.88199100  28.1145438  47.649438 0.0000000   x
# No-till:Corn-Conventional:Corn           50.92455382  41.1571067  60.692001 0.0000000   x
# Conventional:Soybeans-Conventional:Corn  20.76581011  10.9734803  30.558140 0.0000000   x
# Reduced:Soybeans-Conventional:Corn       47.76617000  38.0231644  57.509176 0.0000000
# No-till:Soybeans-Conventional:Corn       20.77847913  11.0354735  30.521485 0.0000000
# No-till:Corn-Reduced:Corn                13.04256282   3.3255938  22.759532 0.0019061   x
# Conventional:Soybeans-Reduced:Corn      -17.11618088 -26.8581615  -7.374200 0.0000100
# Reduced:Soybeans-Reduced:Corn             9.88417900   0.1917788  19.576579 0.0426254   x
# No-till:Soybeans-Reduced:Corn           -17.10351186 -26.7959121  -7.411112 0.0000090
# Conventional:Soybeans-No-till:Corn      -30.15874370 -39.9007243 -20.416763 0.0000000
# Reduced:Soybeans-No-till:Corn            -3.15838382 -12.8507840   6.534016 0.9382317
# No-till:Soybeans-No-till:Corn           -30.14607468 -39.8384749 -20.453674 0.0000000   x
# Reduced:Soybeans-Conventional:Soybeans   27.00035988  17.2828849  36.717835 0.0000000   x
# No-till:Soybeans-Conventional:Soybeans    0.01266902  -9.7048060   9.730144 1.0000000   x ND
# No-till:Soybeans-Reduced:Soybeans       -26.98769086 -36.6554598 -17.319922 0.0000000   x 








### plots comparing tillage estimates for OpTIS and AgCensus


################### Fig. S3c ###################





dat.2017.mean <- dat.2017 %>%
  group_by(state,county, variable) %>%  #, county, variable
  summarize(AgCensus = mean(AgCensus, na.rm=TRUE),
            OpTIS = mean(OpTIS, na.rm=TRUE),
            Transect = mean(Transect, na.rm=TRUE))

levels(dat.2017.mean$state) <- c("Illinois", "New York")

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(dat.2017.mean, aes(x=AgCensus, y=OpTIS)) +                  
  geom_abline(slope=1, intercept=0) +
  geom_point(aes(fill=variable), shape=21, size=3) + 
  #stat_smooth(method='lm', se=FALSE) +
  scale_fill_manual(values=till3col, name='Tillage Type') +
  # scale_fill_brewer(palette='Set1', name='Tillage Type') +
  ggpubr::stat_cor(method='spearman',
                   # aes(label = paste(after_stat(rr.label), after_stat(p.label), sep="~`,`~")),
                   aes(label=after_stat(r.label)),  # not showing p-value because this is not a subsample of a whole population, this is all the counties
                   r.accuracy=0.01,

                   label.y=100) +
  facet_grid(. ~ state) +  
  xlim(0,100) + ylim(0,100) +
  labs(y="Adoption (%) from OpTIS", x="Adoption (%) from AgCensus") + 
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
ggsave("plots/adoption/OpTIS_vs_AgCensus.scatter.png", height=4, width=7)






### plot residuals by crop

################# Fig. S3d ##############################

resid.2017 <- dat.2017.mean %>%
  mutate(resid = OpTIS - AgCensus) %>%
  select(-Transect)


resid.2017 %>%
  group_by(state, variable) %>%
  summarize(mean = mean(resid, na.rm=T),
            sd = sd(resid, na.rm=T))

resid.2017$variable <- factor(resid.2017$variable, 
                              levels=c('Conventional', 'Reduced', 'No-till'))

ggplot(resid.2017, aes(x=resid, fill=variable)) +
  #geom_histogram(aes(y = ..density..), position="identity", alpha=0.5, color='black') +
  geom_density(aes(color=variable), alpha=0.4) +
  scale_color_manual(values=till3col, name="Tillage type") +
  scale_fill_manual(values=till3col, name="Tillage type")+
  # scale_color_brewer(palette='Set1', name='Tillage Type') +
  # scale_fill_brewer(palette='Set1', name='Tillage Type') +
  facet_grid(. ~ state) + #, labeller = as_labeller(c("illinois" = "Illinois", "new york" = "New York"))) + 
  #geom_vline(xintercept=0, color='black', lty=2) +
  labs(y="Density", x="OpTIS (%) - AgCensus (%)") + 
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
        legend.key.spacing.x=unit(1, "cm"),
        legend.position = "bottom",
        legend.box.spacing = unit(0.2, "cm"),
        legend.text=element_text(size=14, margin=margin(l=5)),
        strip.text=element_text(size=14, face="bold"),
        legend.title=element_text(size=16),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12))
ggsave("plots/adoption/OpTIS_vs_AgCensus.histogram.png", height=4, width=7)



# are the residuals significantly different by tillage type?

till.ce <- aov(resid~variable*state, data=resid.2017)
summary(till.ce)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# variable         2  75712   37856  218.18  < 2e-16 ***
#   state            1   2143    2143   12.35 0.000494 ***
#   variable:state   2  15747    7873   45.38  < 2e-16 ***
#   Residuals      378  65585     174                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 99 observations deleted due to missingness
TukeyHSD(till.ce)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = resid ~ variable * state, data = resid.2017)
# 
# $variable
# diff      lwr      upr p adj
# Reduced-Conventional 14.90012 11.01802 18.78221     0
# No-till-Conventional 34.34499 30.46290 38.22709     0
# No-till-Reduced      19.44488 15.58568 23.30407     0
# 
# $state
# diff       lwr       upr     p adj
# New York-Illinois -5.739134 -8.950146 -2.528122 0.0004943
# 
# $`variable:state`
# diff        lwr        upr     p adj
# Reduced:Illinois-Conventional:Illinois        6.83068623   1.494535  12.166838 0.0037923    x
# No-till:Illinois-Conventional:Illinois       31.68493536  26.348784  37.021087 0.0000000    x
# Conventional:New York-Conventional:Illinois -22.59016400 -30.781924 -14.398404 0.0000000    x
# Reduced:New York-Conventional:Illinois       21.70567526  13.629669  29.781682 0.0000000
# No-till:New York-Conventional:Illinois       21.63817510  13.562168  29.714182 0.0000000
# No-till:Illinois-Reduced:Illinois            24.85424913  19.544846  30.163653 0.0000000    x
# Conventional:New York-Reduced:Illinois      -29.42085023 -37.595212 -21.246489 0.0000000
# Reduced:New York-Reduced:Illinois            14.87498903   6.816631  22.933347 0.0000031    x
# No-till:New York-Reduced:Illinois            14.80748887   6.749131  22.865847 0.0000035
# Conventional:New York-No-till:Illinois      -54.27509936 -62.449461 -46.100738 0.0000000
# Reduced:New York-No-till:Illinois            -9.97926010 -18.037618  -1.920902 0.0058138
# No-till:New York-No-till:Illinois           -10.04676026 -18.105119  -1.988402 0.0053419    x
# Reduced:New York-Conventional:New York       44.29583926  34.119024  54.472654 0.0000000    x
# No-till:New York-Conventional:New York       44.22833910  34.051524  54.405154 0.0000000    x
# No-till:New York-Reduced:New York            -0.06750016 -10.151374  10.016374 1.0000000    x ND



### plots comparing cover crops estimates for OpTIS and AgCensus

############## Fig. S3e #####################

dat.2017 <- dat %>%
  filter(year == 2017)

dat.2017 <- dat.2017[dat.2017$variable == 'perc_cc',]

dat.2017.mean <- dat.2017 %>%
  group_by(state, county, variable) %>%
  summarize(AgCensus = mean(AgCensus, na.rm=TRUE),
            OpTIS = mean(OpTIS, na.rm=TRUE))

states2col <- c("#44AA99", "#332288")


ggplot(dat.2017.mean, aes(x=AgCensus, y=OpTIS, fill=state)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(shape=21, size=3) + 
  #stat_smooth(method='lm', se=FALSE) +
  scale_fill_manual(values=states2col, labels=c('Illinois', 'New York'), name='State') +
  scale_color_manual(values=states2col, labels=c('Illinois', 'New York'), name='State') +
  # scale_fill_brewer(palette='Set2', labels=c('Illinois', 'New York'), name='State') +
  ggpubr::stat_cor(method='spearman', 
                   aes(color=state,

                       label=after_stat(r.label)),  # not showing p-value because this is not a subsample of a whole population, this is all the counties
                   r.accuracy=0.01,

                   label.y=c(40,37)) +
  facet_grid(. ~ variable, labeller=as_labeller(c("perc_cc" = 'Cover Crop'))) + 
  xlim(0,40) + ylim(0,40) +
  labs(y="Adoption (%) from OpTIS", x="Adoption (%) from AgCensus") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
        legend.key.spacing.x=unit(1, "cm"),
        legend.position = c(0.8, 0.4),
        legend.box.spacing = unit(0.05, "cm"),
        legend.text=element_text(size=12, margin=margin(l=0.05)),
        strip.text=element_text(size=12, face="bold"),
        legend.title=element_text(size=13),
        axis.title=element_text(size=12),
        axis.text=element_text(size=11))

ggsave("plots/adoption/OpTIS_vs_AgCensus.scatter.cc.png", height=3, width=3)





########### Fig. S3f ################

resid.2017 <- dat.2017.mean %>%
  mutate(resid = OpTIS - AgCensus,
         county.lower = tolower(county),
         state.lower = tolower(state))

resid.2017 %>%
  group_by(state, variable) %>%
  summarize(mean = mean(resid, na.rm=T),
            sd = sd(resid, na.rm=T))

ggplot(resid.2017, aes(x=resid, fill=state)) +
  #geom_histogram(aes(y = ..density..), position="identity", alpha=0.5, color='black') +
  geom_density(aes(color=state), alpha=0.4) +
  scale_color_manual(values = states2col, labels=c('Illinois', 'New York'),  name='State') +
  scale_fill_manual(values=states2col, labels=c('Illinois', 'New York'), name='State') +
  # scale_color_brewer(palette='Set2', labels=c('Illinois', 'New York'),  name='State') +
  # scale_fill_brewer(palette='Set2', labels=c('Illinois', 'New York'), name='State') +
  facet_grid(. ~ variable, labeller = as_labeller(c("perc_cc" = "Cover Crop"))) + 
  #geom_vline(xintercept=0, color='black', lty=2) +
  labs(y="Density", x="OpTIS (%) - AgCensus (%)") + 
  theme_bw() +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(),
        legend.margin=margin(c(0.5, 0.5, 0.5, 0.5)),
        legend.key.spacing.x=unit(1, "cm"),

        legend.position = c(0.78, 0.82),
        legend.box.spacing = unit(0.2, "cm"),
        legend.text=element_text(size=11, margin=margin(l=5)),
        strip.text=element_text(size=12, face="bold"),
        legend.title=element_text(size=12),
        axis.title=element_text(size=12),
        axis.text=element_text(size=11))
ggsave("plots/adoption/OpTIS_vs_AgCensus.histogram.cc.png", height=3, width=3.3)


# are the residuals significantly different by tillage type?

cc.ce <- aov(resid~state, data=resid.2017)
summary(cc.ce)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# state         1   1196  1195.7    30.1 2.04e-07 ***
#   Residuals   131   5204    39.7                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 28 observations deleted due to missingness
TukeyHSD(cc.ce)

# 95% family-wise confidence level
# 
# Fit: aov(formula = resid ~ state, data = resid.2017)
# 
# $state
#                     diff       lwr       upr p adj
# New York-Illinois -6.941947 -9.445089 -4.438805 2e-07

