

# load packages
library(reshape2) # for dcast()
library(tidyverse)







# load the NASS survey data for acres harvested
# corn and soy available at county level
# alfalfa and wheat only available at state level
corn <- read.csv("data/NASS harvested acres/NASS survey NY all counties corn grain and silage acres harvested 2015-21.csv.csv")
soy <- read.csv("data/NASS harvested acres/NASS survey NY all counties soybean acres harvested 2015-21.csv.csv")
alf <- read.csv("data/NASS harvested acres/NASS survey NY state alfalfa acres harvested 2015-21.csv.csv")
wheat <- read.csv("data/NASS harvested acres/NASS survey NY state wheat acres harvested 2015-21.csv.csv")



# simplify the datasets
corn <- corn %>%
  select(Year, County, Data.Item, Value) %>%
  mutate(Value=as.numeric(gsub(",", "", Value)))
soy <- soy %>%
  select(Year, County, Commodity, Value) %>%
  mutate(Value=as.numeric(gsub(",", "", Value)))
alf <- alf %>%
  select(Year, Commodity, Value) %>% # data.item = "HAYLAGE, ALFALFA - ACRES HARVESTED" even though Commodity - "HAYLAGE"
  mutate(Value=as.numeric(gsub(",", "", Value))) 
wheat <- wheat %>%
  select(Year, Commodity, Value, Period)  %>%
  filter(Period %in% "YEAR") %>%
  select(Year, Commodity, Value) %>%
  mutate(Value=as.numeric(gsub(",", "", Value)))

# get corn and soy state numbers
cornagg <- aggregate(Value ~ Year + Data.Item, dat=corn, FUN=sum)
soyagg <- aggregate(Value ~ Year + Commodity, dat=soy, FUN=sum)

colnames(cornagg)[2] <- "Commodity"
dat <- rbind(cornagg, soyagg, wheat, alf)

dat <- dat %>%
  group_by(Year) %>%
  mutate(total_ac=sum(Value)) %>%
  ungroup() %>%
  mutate(percent_ac=(100*Value)/total_ac)

datagg <- aggregate(percent_ac~Commodity, dat, mean)
  
