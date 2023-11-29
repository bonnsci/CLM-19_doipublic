# What are the current emissions and C sequestration in IL?
# Data shared with AFT from Regrow DNDC analysis.
# 

# load packages
library(reshape2) # for dcast()
library(ggplot2)
library (tidyverse)
#library(dplyr) #is having conflict with 'filter' lap

# to update data, run "download data.R"

# load IL DNDC scenario output 
# (corresponds to "il_adoption_scenarios_final_outputs.csv")
scenil <- read.csv("data/scenarios/scenil.csv")

########### METADATA IN 3 ROWS per data column (column name -- unit -- description)
# column name       unit          description
# site_name         NA            String combination of one of the locations we simulated on. In the format of {x}_{y} where x refers to the region and y refers to the numeric id of that site. These will be of the format of h_{}, v_{}, a_{}, f_{}, IL-n_{}, IL-s_{} which represents the points for hops, vineyards, almonds, and forage, Illinois North, Illinois South respectively
# crop [see note below]     NA            Name of the cropping system. Full list of unique names are: almonds, hops, grape, alfalfa, corn, corn-grain, corn-silage and soybean
# scenario code     integer       Scenario number that can be found in section 2.
# climate_scenario  NA            Either ‘rcp26’ or ‘rcp60’ that were defined in section 1.2
# year              integer       Calendar year in which results are for.
# dsoc              tonne C/ha    Calendar year change in soil organic carbon stocks (Dec. 31 - Jan. 1).
# n2o               tonne N/ha    Calendar year daily sum (Jan. 1 - Dec. 31) of nitrous oxide direct emissions in co2 equivalents.
# ch4               tonne C/ha    Calendar year daily sum (Jan. 1 - Dec. 31) of methane emissions in co2 equivalents.
# indirect_n2o      tonne N/ha    Calendar year daily sum (Jan. 1 - Dec. 31) of nitrous oxide indirect emissions in co2 equivalents.
# total_n2o         tonne N/ha    Sum of n2o and indirect_n2o
# ghg               tonne co2e/ha Sum of ghg_dsoc, ghg_ch4, ghg_total_n2o in co2 equivalents.
# ghg_dsoc          tonne co2e/ha Calendar year change in soil organic carbon stocks (Dec. 31 - Jan. 1) in co2 equivalents.
# ghg_ch4           tonne co2e/ha Calendar year daily sum (Jan. 1 - Dec. 31) of methane emissions in co2 equivalents.
# ghg_n2o           tonne co2e/ha Calendar year daily sum (Jan. 1 - Dec. 31) of nitrous oxide direct emissions in co2 equivalents.
# ghg_indirect_n2o  tonne co2e/ha Calendar year daily sum (Jan. 1 - Dec. 31) of nitrous oxide indirect emissions in co2 equivalents.
# ghg_total_n2o     tonne co2e/ha Sum of ghg_n2o and ghg_indirect_n2o in co2 equivalents 

# note on crop: For systems with multiple crops (not hops, almonds, or grapes), 
# scenario outputs are the average of the crop types within a rotation. In other words, 
# each crop was run each year and results were averaged across the crop types for that year. 
# For example, IL corn (corn-soybeans) and NY soy (corn-soybeans) there were two scenarios 
# (one growing corn, one growing soy) averaged in a year. The NY alfalfa rotation was 
# averaged across the three separate crops (alfalfa, corn, triticale) each year in scenario output.
# So even though unique(scenil$crop) gives "corn, grain" it actually is
# the average of corn-soy.

# 
# 
# library(tibble)
# library(ggpmisc)

se <- function(x) sd(x) / sqrt(length(x))


# ID for decade
scenil$decade <- ifelse(scenil$year <2031, "2020s",
                        ifelse(scenil$year>=2031 & scenil$year <2041, "2030s",
                               ifelse(scenil$year>=2041 & scenil$year <2051, "2040s",
                                      ifelse(scenil$year>=2051 & scenil$year <2061, "2050s",
                                             ifelse(scenil$year>=2061 & scenil$year <2071, "2060s", "2070s")))))




scenil <- scenil[scenil$climate_scenario=="rcp60",]

#stacked bar with Net balance for 2022
scenil_2022 <- scenil %>% filter (scenario.code %in% c(1,7,8) & year ==2022 & climate_scenario=="rcp60") %>% #taking only scenario 1 and 2022 for analysis
  dplyr::select(ghg, ghg_dsoc, ghg_total_n2o, scenario.code) %>%
  gather(var, value, ghg:ghg_total_n2o) %>%
  group_by (var, scenario.code) %>% 
  summarise_all(list(mean, se)) %>%
  rename(mean = fn1, se = fn2) %>%
  mutate (year= "2022")  #fn1, fn2


scenil_2022$scenario.code <- factor(scenil_2022$scenario.code, levels=c("8", "7", "1"), ordered=T)


ggplot(data=scenil_2022[scenil_2022$var %in% c("ghg_dsoc", "ghg_total_n2o"),],
       aes(x=scenario.code, y=mean)) + 
  geom_bar(aes(fill=var), stat="identity", position= "stack") +
  ylim(-2,1) +
  scale_fill_manual(values=c("#ee8866","#99ddff"), 
                    breaks=c("ghg_total_n2o", "ghg_dsoc"), 
                    name = "Source/Sink", 
                    labels=c(expression('Total N'[2]*'O emissions', "Change in SOC"))) +
  geom_point(data=scenil_2022[scenil_2022$var == "ghg",],
             aes(x=scenario.code, y=mean), size=1, color= "#004488", show.legend=F) +
  geom_errorbar(data=scenil_2022, aes(x=scenario.code, y=mean, ymin=mean-se, ymax=mean+se), 
                linewidth=0.8,
                width=0.2,
                color = "#004488") + 
  geom_hline(yintercept=0, color="#009988", linewidth=0.5) +
  #annotate("text", x=Net$year, y=Net$mean +0.1, label= "Net Balance", aes(group=scenario.code)) +
  #scale_y_continuous(breaks=seq(0,70,10)) +
  labs(x="Scenario", 
      y=expression('2022 mean annual emissions (tonnes CO'[2]*'e ha'^'-1'*')')) +
  scale_color_manual(values=c("#004488", "#882255", "gray30"), breaks=c("ghgdsoc", "ghgn2o", "net")) +
  scale_x_discrete(labels=c("8" = "Fall N", "7" = "High N", "1" = "Recommended N"))+
  theme_bw()+
  # facet_grid(cols=vars(scenario.code)) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text=element_text(size=12),
    axis.title.x=element_text(size=12, face="bold"),
    axis.title.y=element_text(size=12, face="bold", lineheight=0.2),
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
    strip.text=element_text(size=12, face="bold"),
    legend.text=element_text(size=11),
    legend.title=element_text(size=12, face="bold"),
    plot.margin = unit(c(1,0.5,0.5,1), "cm"),
    axis.text.x=element_text(angle=-20, hjust=0),
    legend.text.align=0,) #-> p
    #legend.position = "none")
  

ggsave("plots/ghgs/IL_Net_2022_scenarios 1,7,8_RCP60.png", width=6, height=4.5)  


# stacked dsoc, total n2O with net

# make decadal data set for stacked bars by decade and scenario (with variability across sites)
decdat <- scenil %>%
  mutate(net = ghg_dsoc + ghg_total_n2o) %>%
  group_by(scenario.code, decade) %>%
  summarize(ghg_dsoc.mean = mean(ghg_dsoc),
            ghg_dsoc.se = se(ghg_dsoc),
            ghg_tn2o.mean = mean(ghg_total_n2o),
            ghg_tn2o.se = se(ghg_total_n2o),
            ghg_net.mean = mean(net),
            ghg_net.se = se(net)) %>%
  pivot_longer(cols=starts_with("ghg"), names_to = "variable", values_to = "value") %>%
  mutate(mean.se = ifelse(grepl(".mean", variable), "mean", "se")  ,
         variable = gsub(".mean", "", 
                         gsub(".se", "", variable))) %>%
  pivot_wider(names_from = mean.se, values_from = value)

# pop out window for plots
windows(xpinch=200, ypinch=200, width=5, height=5)




ggplot() +
  geom_bar(data=decdat[decdat$variable %in% c("ghg_dsoc", "ghg_tn2o") & decdat$scenario.code <5 ,], # &            aes(x=decade, y=mean, fill= variable), 
           aes(x=decade, y=mean, fill= variable),
           stat="identity", position= "stack", alpha=0.5) +
  scale_fill_manual(values=c("#ee8866","#99ddff"), 
                    breaks=c("ghg_tn2o", "ghg_dsoc"), 
                    name = "Source/Sink", 
                    labels=c(expression('Total N'[2]*'O emissions', "Change in SOC"))) + #, labels=c("ghg_dsoc", "ghg_tn2o")) +
  geom_hline(yintercept=0, color="#009988", linewidth=0.5) +
  geom_line(data=decdat[decdat$variable %in% c("ghg_net") & decdat$scenario.code <5 ,], # decdat$scenario.code %in% c(1,4,6, 7,8) & 
            aes(x=decade, y=mean, group=1),
            linewidth = 1, color="#004488", alpha=0.3) +
  geom_errorbar(data=decdat[decdat$scenario.code <5,], #[decdat$scenario.code %in% c(1,4,6,7,8),], 
                aes(x=decade, y=mean, ymin=mean-se, ymax=mean+se), color="#004488",# variable),
                width = 0.3, show.legend=F) +
  geom_point(data=decdat[decdat$variable %in% c("ghg_net") & decdat$scenario.code <5 ,], # decdat$scenario.code %in% c(1,4,6, 7,8) & 
             aes(x=decade, y=mean),
             size = 0.8, color="#004488") +
  scale_y_continuous(breaks=seq(-1.5, 1.5, 0.5)) +
  labs(x="Decade", 
       y= expression('Mean annual emissions (tonnes CO'[2]*'e ha'^'-1'*')')) +
  #scale_color_manual(values=c("#004488", "#882255", "gray30"), breaks=c("ghg_dsoc", "ghg_tn2o", "ghg_net")) +
  facet_grid(cols=vars(scenario.code), 
             labeller = as_labeller(
               c("1" = "(1) BAU adopt +\nrecommended N rate", 
                 "2" = "(2) BAU CC, 3x CT adopt +\nrecommended N rate",
                 "3" = "(3) 3x CC, BAU CT adopt +\nrecommended N rate",
                 "4" = "(4) 3xBAU adoption +\nrecommended N rate"))) + #, 
                 # "5" = "(5) CC 25% by 2030, BAU CT +\nadopt recommended N rate",
                 # "6" = "(6) CC 25%, CT 90% by 2030 +\nrecommended N rate", 
                 # "7" = "(7) 3xBAU adoption +\nhigh N rate",
                 # "8" = "(8) 3xBAU adoption +\nrecommended N rate, fall applied"))) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    legend.text.align=0,
    axis.text.x=element_text(angle=-30, hjust=0))

ggsave("plots/ghgs/IL_net balance by decade scenarios 1-4 RCP60.png", width=10, height=5, dpi=300)
# ggsave("plots/ghgs/IL_net balance by decade scenarios 1-8 RCP60.png", width=16, height=5, dpi=300)


# why does Fall N have lower net emissions? --need to investigate simulation data...

# are the net ghg points in the above plot significantly different?
# specifically are the 2020s different from each other? 
# Are the 2070s significantly different?

# use corn -soy means
scenil_cropmeans <- group_by(scenil, site_name, scenario.code, year) %>%
  summarize(mean.net = mean(ghg),
            mean.dsoc = mean(ghg_dsoc),
            mean.n2o = mean(ghg_total_n2o))

# the 2020s are not sig. diff.
lm20 <- lm(mean.net~factor(scenario.code), data=scenil_cropmeans[scenil_cropmeans$year<2031 & scenil_cropmeans$scenario.code <5,])
summary(lm20)

# the 2070s
lm70 <- lm(mean.net~factor(scenario.code), data=scenil_cropmeans[scenil_cropmeans$year>2070 & scenil_cropmeans$scenario.code <5,])
summary(lm70)

aov70 <- aov(mean.net~factor(scenario.code), data=scenil_cropmeans[scenil_cropmeans$year>2070 & scenil_cropmeans$scenario.code <5,])
summary(aov70)

Tukout <- TukeyHSD(aov70)

cld <- multcompView::multcompLetters4(aov70,Tukout)

# table with letters 
decdat70s <- filter(decdat, decade=="2070s", scenario.code <5, variable=="ghg_net") %>%
  arrange(desc(mean))

cld <- as.data.frame.list(cld$`factor(scenario.code`)
decdat70s$cld <- cld$Letters


# what is the difference between 2070s and 2020s for each scenario?

decdat2070 <- filter(decdat, decade %in% c("2020s", "2070s"), variable=="ghg_net", scenario.code <5) %>%
  dplyr::select(scenario.code, mean, decade) %>%
  dcast(., scenario.code~ decade, value.var="mean") %>%
  rename(t20s = "2020s", t70s = "2070s") %>%
  mutate(diff70m20 = t70s - t20s,
         factor20 = diff70m20/t20s)








##by sub-scenario
##year by year trends for all managements
unw %>% filter (grepl('IL-', site_name)) %>%
  filter (year %in% (2010:2022) & climate_scenario == "rcp26") %>%  #taking only scenario 1, rcp26 and years 2010-2022 for analysis
  select(crop_name, management, year, ghg_dsoc, ghg_total_n2o) %>% 
  mutate(Net = ghg_dsoc + ghg_total_n2o) %>%
  gather(var, value, ghg_dsoc:Net) %>%
  ggplot(aes(as.numeric(year), value, color= management)) +
  geom_point () +
  facet_grid(crop_name ~var, scales = "free_y") +
  geom_line() +
  scale_x_continuous(breaks=seq(2010,2022,1))
  
ggsave("plots/trend_s1_shms.png", width=12, height=6)     

  
  
###summary figure by management practice across years 
  
unw %>% filter (grepl('IL-', site_name)) %>%
  filter (year == 2022 & climate_scenario == "rcp26") %>%  #taking only scenario 1, rcp26 and years 2010-2022 for analysis
  select(crop_name, management,ghg_dsoc, ghg_total_n2o) %>%
  mutate(Net = ghg_dsoc + ghg_total_n2o) %>%
  gather(var, value, ghg_dsoc:Net) %>%
  group_by(crop_name, management, var) %>%
  summarise_all(list(mean, sd)) %>% #fn1, fn2
  ggplot(aes(x=management, y=fn1, group=management)) + 
  geom_bar(stat="identity", position=position_dodge(), aes(fill=management)) +
  geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2), linewidth=0.8, color="#000000",
                position=position_dodge(0.9), width=0.2) +
  facet_grid(crop_name ~var, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave("plots/mean_s1_shms.png", width=12, height=6)     

#dropping the fertilizer from management code
unw %>% filter (grepl('IL-', site_name)) %>%
  filter (year == 2022 & climate_scenario == "rcp26") %>%  #taking only scenario 1, rcp26 and years 2010-2022 for analysis
  mutate( management2= str_sub(management, 1, 5)) %>%
  select(crop_name, management2,ghg_dsoc, ghg_total_n2o) %>%
  mutate(Net = ghg_dsoc + ghg_total_n2o) %>%
  rename(SOC = ghg_dsoc, N2O = ghg_total_n2o) %>%
  #mutate (management2 = sub("*-*-*-.", "", management)) #sub("(_[^_]*)_", "\\1", vec)
  gather(var, value, SOC:Net) %>%
  group_by(crop_name, management2, var) %>%
  summarise_all(list(mean, sd)) %>% #fn1, fn2
  ggplot(aes(x=  reorder(management2, fn1), y=fn1, group=management2)) + 
  #ggplot(aes(x=management2, y=fn1, group=management2)) + 
  geom_bar(stat="identity", position=position_dodge(), aes(fill=management2)) +
  geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2), linewidth=0.8, color="#000000",
                position=position_dodge(0.9), width=0.2) +
  facet_grid(var ~ crop_name, scales = "free_y") + #ncol=2, strip.position = "right"
  #facet_wrap(var ~., ncol=1) +
  ylim(-4,2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip() +
  xlab("Soil Health Management Systems") +
  #scale_x_discrete(labels=c("2022" = ""))+
  ylab("CO2 equivalents (Tn/ha/Yr)") +
  scale_fill_discrete(name=NULL) +
  theme_bw()+
  theme(
    legend.position= "none",
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text=element_text(size=12),
    axis.title.x=element_text(size=12, face="bold"),
    axis.title.y=element_text(size=12, face="bold"),
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
    strip.text=element_text(size=12, face="bold"),
    legend.text=element_text(size=11),
    #legend.title=element_text(size=12, face="bold"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave("plots/mean_s1_shms-noN.png", width=6, height=8)  


#summarizing some numbers to write the results 
unw %>% filter (grepl('IL-', site_name)) %>%
  filter (year == 2022 & climate_scenario == "rcp26") %>%  #taking only scenario 1, rcp26 and years 2010-2022 for analysis
  mutate( management2= str_sub(management, 1, 5)) %>%
  select(management2,ghg_dsoc, ghg_total_n2o) %>%
  mutate(Net = ghg_dsoc + ghg_total_n2o) %>%
  gather(var, value, ghg_dsoc:Net) %>%
  group_by(management2, var) %>%
  summarise_all(list(mean, sd)) %>%
  #filter (var == "Net") %>%
  filter (var == "ghg_total_n2o") %>%
  #filter (var == "ghg_dsoc") %>%
  
  arrange(fn1) %>%  ## pick the system that has the max C storage capacity
  filter(fn1 %in% range(fn1))
  #arrange(fn2) %>%  ## pick the system that has more stable C storage capacity across years? 
  #filter(fn2 %in% range(fn2))

## diff in the effect of CC in CT vs NT or RT
dif = -1.388 - (-1.16)
p_dif = dif/1.16


##Anova analysis for management effect 

dta<-unw %>% filter (grepl('IL-', site_name)) %>%
             filter (year == 2022 & climate_scenario == "rcp26") %>%
             mutate(Net = ghg_dsoc + ghg_total_n2o)


n2o_eff<- aov(ghg_total_n2o ~ management +  site_name + crop_name, 
                  data= dta) 

summary(n2o_eff)
TukeyHSD(n2o_eff)
TukeyHSD(n2o_eff, conf.level=.90)$management %>% as.data.frame() %>%
  filter (`p adj` < 0.01)


net_eff<- aov(Net ~ management +  site_name, 
              data= dta) 

summary(net_eff)
TukeyHSD(net_eff)
TukeyHSD(net_eff, conf.level=.90)$management %>% as.data.frame() %>%
  filter (`p adj` < 0.01)


library(emmeans)
library(multcomp)
library(multcompView)


model <- lm(Net ~ management, data = dta)

# get (adjusted) means
model_means <- emmeans(object = model,
                       specs = ~ management) 

# add letters to each mean
model_means_cld <- cld(object = model_means,
                       adjust = "Tukey",
                       Letters = letters,
                       alpha = 0.05)

model_means_cld <- model_means_cld %>% 
  as.data.frame() 

#next add letters into plot once we have final version 
