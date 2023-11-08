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
# croP              NA            Name of the cropping system. Full list of unique names are: almonds, hops, grape, alfalfa, corn, corn-grain, corn-silage and soybean
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


# load IL unweighted simulation DNDC output 
# (corresponds to "yearly_outputs_post-weighting.csv")

unw <- read.csv("data/simulations/un-weighted_resultsIL.csv")


###current GHG emissions and C sequestration from corn-soybean agriculture in Illinois
##state level
# n2o tonne N/ha Calendar year daily sum (Jan. 1 - Dec. 31) of nitrous oxide direct emissions in co2 equivalents.
# ch4 tonne C/ha Calendar year daily sum (Jan. 1 - Dec. 31) of methane emissions in co2 equivalents.
# cseq == dsoc tonne C/ha Calendar year change in soil organic carbon stocks (Dec. 31 - Jan. 1).

#insert the Net inside the bars

library(tibble)
library(ggpmisc)

se <- function(x) sd(x) / sqrt(length(x))

#data.tb <- tibble(x = "n" , y = -2.7, 
 #                 plot = list(p +
  #                              theme_bw(4)))



# ID for decade
scenil$decade <- ifelse(scenil$year <2031, "2020s",
                        ifelse(scenil$year>=2031 & scenil$year <2041, "2030s",
                               ifelse(scenil$year>=2041 & scenil$year <2051, "2040s",
                                      ifelse(scenil$year>=2051 & scenil$year <2061, "2050s",
                                             ifelse(scenil$year>=2061 & scenil$year <2071, "2060s", "2070s")))))




# not needed? 
# scenil %>% filter (scenario.code == 1 & year ==2022 & climate_scenario=="rcp60") %>% #taking only scenario 1 and 2022 for analysis
#   select(ghg_dsoc,  ghg_total_n2o) %>%  # ghg_n2o, 
#   gather(var, value, ghg_dsoc:ghg_total_n2o) %>%
#   group_by (var) %>% #we decided to do average between the two climates
#   summarise_all(list(mean, sd)) %>% #fn1, fn2
#   ggplot(aes(x=var, y=fn1, fill= var)) + 
#   geom_bar(stat="identity", position=position_dodge()) +
#   geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2), linewidth=0.8, color="#000000",
#                 position=position_dodge(0.9), width=0.2) + ylim(-2.7,1.5) +
#  scale_fill_manual(values=c("#DDAA33", "#228833")) +   # , "#66CCEE"
#                     #labels=c("Corn", "Soybeans", "Winter Wheat"),
#                    #name="Climate Scenario") +
#   #scale_y_continuous(breaks=seq(0,70,10)) +
#   ylab("Co2 equivalents (Tn/ha/Yr)") +
#   scale_x_discrete(labels=c("ghg_dsoc" = "SOC", "ghg_n2o" = "N2O","ghg_total_n2o" = "Total_N2O"))+
#   xlab("Greenhouse gas") +
#   #geom_plot(data = data.tb, aes(x, y, label = plot)) +
#   theme_bw()+
#   theme(
#     panel.grid.minor=element_blank(), 
#     panel.grid.major=element_blank() ,
#     axis.text=element_text(size=12),
#     axis.title.x=element_text(size=12, face="bold"),
#     axis.title.y=element_text(size=12, face="bold"),
#     panel.background = element_rect(fill = 'white') ,
#     panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
#     strip.text=element_text(size=12, face="bold"),
#     legend.text=element_text(size=11),
#     legend.title=element_text(size=12, face="bold"),
#     plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"), 
#     legend.position = "none")
#   
# 
# ggsave("plots/n20_dsoc_2022sn1.png", width=8, height=6)     

#stacked with net balance start==mean

#Calculate the balance
scenil %>% filter (scenario.code == 1 & year ==2022 & climate_scenario=="rcp60") %>% #taking only scenario 1 and 2022 and rcp 6.0 for analysis
  select(ghg_dsoc, ghg_total_n2o) %>%
  mutate (Net = ghg_dsoc + ghg_total_n2o) %>%
  select (Net) %>%
  summarise_all(list(mean, sd)) %>% 
  mutate (year = "2022") -> Net

#stacked bar with Net balance
scenil %>% filter (scenario.code == 1 & year ==2022 & climate_scenario=="rcp60") %>% #taking only scenario 1 and 2022 for analysis
  select(ghg_dsoc, ghg_total_n2o) %>%
  gather(var, value, ghg_dsoc:ghg_total_n2o) %>%
  group_by (var) %>% 
  summarise_all(list(mean, sd)) %>%
  mutate (year= "2022") %>% #fn1, fn2
  ggplot(aes(x=year, y=fn1, fill= var)) + 
  geom_bar(aes(x=year, y=fn1, fill= var), stat="identity", position= "stack") +
  geom_errorbar(aes(ymin=fn1-fn2, ymax=fn1+fn2), linewidth=0.8, color="#000000",
                width=0.2) + ylim(-2.7,1) +
  scale_fill_manual(values=c("#DDAA33", "#228833"), 
  labels=c("SOC","N2O"),
  name="Greenhouse gas") +
  geom_hline(yintercept = Net$fn1, linetype = "dashed", linewidth = 1) +
  annotate("text", x=Net$year, y=Net$fn1 +0.1, label= "Net Balance") +
  #scale_y_continuous(breaks=seq(0,70,10)) +
  ylab("Co2 equivalents (Tn/ha/Yr)") +
  scale_x_discrete(labels=c("2022" = ""))+
  xlab("Year:2022") +
  theme_bw()+
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    axis.text=element_text(size=12),
    axis.title.x=element_text(size=12, face="bold"),
    axis.title.y=element_text(size=12, face="bold"),
    panel.background = element_rect(fill = 'white') ,
    panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
    strip.text=element_text(size=12, face="bold"),
    legend.text=element_text(size=11),
    legend.title=element_text(size=12, face="bold"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) -> p
    #legend.position = "none")
  

ggsave("plots/Net_2022sn1.png", width=6, height=8)  


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
  geom_bar(data=decdat[decdat$scenario.code %in% c(1,4,6) & 
                         decdat$variable %in% c("ghg_dsoc", "ghg_tn2o"),], 
           aes(x=decade, y=mean, fill= variable), 
           stat="identity", position= "stack") +
  scale_fill_manual(values=c("#99ddff", "#ee8866"), name = "Source/Sink", 
                    labels=c("Change in SOC", expression('Total N'[2]*'O emissions'))) + #, labels=c("ghg_dsoc", "ghg_tn2o")) +
  geom_hline(yintercept=0, color="#009988", linewidth=0.5) +
  geom_point(data=decdat[decdat$scenario.code %in% c(1,4,6) & 
                           decdat$variable %in% c("ghg_net"),], 
             aes(x=decade, y=mean),
             size = 0.5, color="gray30") +
  geom_errorbar(data=decdat[decdat$scenario.code %in% c(1,4,6),], 
                aes(x=decade, y=mean, ymin=mean-se, ymax=mean+se, color=variable),
                width = 0.3, show.legend=F) +
  labs(x="Decade", 
       y= expression('Mean annual emissions (tonnes CO'[2]*'e ha'^'-1'*')')) +
  scale_color_manual(values=c("#004488", "#882255", "gray30"), breaks=c("ghg_dsoc", "ghg_tn2o", "ghg_net")) +
  facet_grid(rows=vars(scenario.code)) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank(),
    panel.background = element_rect(fill = 'gray95'),
    legend.text.align=0)


ggsave("plots/ghgs/IL_net balance by decade scenarios 1,4,6.png", width=6, height=7, dpi=300)







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
