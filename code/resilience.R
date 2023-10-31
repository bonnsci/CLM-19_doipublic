# this script explores ways to quantify resilience in the CLM-19 dataset.
# following ideas in the CLM literature / resilience folder. https://americanfarmlandtrust.sharepoint.com/:f:/s/ClimatePrograms/EuOX-vSqLoZHmB8X1WTkIO4B4ZsmvAwlH2U85dBLcLcoRQ?e=jrJY4o
# started 10/31/2023 BMM

# esp. Scheffer et al. 2015
# we'll start with the idea of ranking resilience. I'm thinking different SHMS
# will have different resiliencies to climate extremes, i.e. with CC and NT, more resilient
# biomass C in SOC accumulation in the face of drought / extreme wet, etc.

# load packages
library(tidyverse)
library(MASS)  # for boxcox

# pull in data
# biomass 
bmil <- read.csv("data/biomass/biomass_IL.csv")
colnames(bmil)[9] <- "grainc"

# climate - SPEI
spei <- read.csv("data/climate data not large/speiIL.csv")

# let's focus on rcp 6.0 for now
bmil <- bmil[bmil$climate_scenario=="rcp60",]

spei <- spei[spei$rcp=="rcp60",]

# biomass is once per year, spei is monthly, could get annual (or water year) average
# spei or look at July SPEI (or other month or average of months). Let's start
# with july Spei

spei <- spei %>%
  separate_wider_delim(dat, delim=" ", 
                       names=c("month", "year"),  # split the ID into two columns
                       cols_remove=F)

bmilgr <- bmil[,c(2,4, 6:9),]

# make dummy factor for CT, RT, NT
bmilgr$till <- ifelse(grepl("ct-", bmilgr$management_name), "CT", 
                    ifelse(grepl("rt-", bmilgr$management_name), "RT", "NT"))
# # check
# unique(bmilgr$till)

# dummy for CC or NC
bmilgr$cc <- ifelse(grepl("-cc-", bmilgr$management_name), "CC", "NC")
# # check
# unique(bmilgr$cc)

# dummy for N treatment
bmilgr$nfert <- ifelse(grepl("-cn", bmilgr$management_name), "High N", 
                     ifelse(grepl("-fn", bmilgr$management_name), "Fall N","Recommended N"))
# # check
# unique(bmilgr$nfert)


# merge biomass and spei data
spei$year <- as.numeric(spei$year)

speibm <- inner_join(spei[,-2], bmilgr, join_by(site==site_name, year==Year), 
                    relationship="many-to-many")

# grainc z-score
speibm$grainz <- (speibm$grainc - mean(speibm$grainc))/sd(speibm$grainc)

windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data=speibm[speibm$crop_name %in% c("corn, grain") & speibm$month =="Jul",], 
       aes(x=fit.12mo, y=grainc)) +  #  x=abs(fit.1mo), x=fit.12mo, x=grainz
  geom_point(size=0.1, alpha=0.5) +
  scale_x_continuous(breaks=c(-2.0, -1.6, -1.3, -0.8, 1.30, 1.60, 2.0, 3.0),
                     labels=c(-2.0, -1.6, -1.3, -0.8, 1.30, 1.60, 2.0, 3.0)) +
  facet_grid(cols=vars(ns, till), rows=vars(cc, nfert))

# looks like corn grain c less resilient to extreme weather when: fall N, in the South. nothing obviously apparent between tillage or cover treatments

ggplot(data=speibm[speibm$crop_name %in% c("soybean") & speibm$month =="Jul",], 
       aes(x=fit.12mo, y=grainc)) +  #  x=abs(fit.1mo), x=fit.12mo
  geom_point(size=0.1, alpha=0.5) +
  facet_grid(cols=vars(ns, till), rows=vars(cc))




speijuly <- spei[spei$month=="Jul",]

hist(speijuly$fit.12mo)
hist(bmil$Grain.C.kgC.ha.) # bm il data contain a lot of zeros for rye grain since it is not harvested as grain
unique(bmil$crop_name)
bmilcs <- bmil[bmil$crop_name %in% c("corn, grain", "soybean"),]
hist(bmilcs$Grain.C.kgC.ha.) # that's ugly
boxcox(lm(bmilcs$Grain.C.kgC.ha.~1))
# close to zero. Let's try the log
hist(log(bmilcs$Grain.C.kgC.ha.)) # not much better
hist(sqrt(bmilcs$Grain.C.kgC.ha.)) # maybe better
boxcox(lm(sqrt(bmilcs$Grain.C.kgC.ha.)~1))


hist(bmil$grainz)
