# script started 9/25/2024 by Bonnie McGill
# Figure S1. 

library(ggplot2)
library(tidyverse)
library(maps)
library(sf)

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)


# get map data
all <- map_data("county", c("New York", "Illinois"))
states <- map_data("state", regions=c("New York", "Illinois"))
# all.county <- unique(all[c("subregion", "region")])  # to check how counties are spelled

# assign area groups
all$area <- ifelse(all$region == "new york" & all$subregion %in% c("niagara", "orleans", "monroe", "wayne",
                                                                 "erie", "genesee", "wyoming", "livingston", "ontario", "yates", "seneca",
                                                                 "chautauqua", "cattaraugus", "allegany", "steuben", "schuyler", "chemung"),
                     "WNY", 
                     ifelse(all$region=="illinois" & all$subregion %in% c("henderson", "warren", "knox", "stark", "marshall", "woodford", "livingston", "ford", "iroquois",
                                                                      "hancock", "mcdonough", "fulton", "peoria", "tazewell", "mclean",
                                                                      "adams", "schuyler", "brown", "cass", "mason", "menard", "logan", "de witt", "piatt", "champaign", "vermilion",
                                                                      "pike", "scott", "morgan", "sangamon", "macon", 
                                                                      "calhoun", "greene", "jersey", "macoupin", "montgomery", "christian", "shelby", "moultrie", "douglas", "coles", "cumberland", "edgar", "clark"),
                            "CIL",
                            ifelse(all$region=="illinois" & all$subregion %in% c("madison", "bond", "fayette", "effingham", "jasper", "crawford", 
                                                                             "st clair", "clinton", "marion", "clay", "richland", "lawrence",
                                                                             "monroe", "washington", "jefferson", "wayne", "edwards", "wabash",
                                                                             "randolph", "perry", "franklin", "hamilton", "white", 
                                                                             "jackson", "williamson", "saline", "gallatin",
                                                                             "union", "johnson", "pope", "hardin",
                                                                             "alexander", "pulaski", "massac"),
                                   "SIL", NA)))



# il <- all[all$region == 'illinois',]
windows(xpinch=200, ypinch=200, width=5, height=5)

ann_text <- data.frame(x= c(-89.2, -89, -77.8), 
                       y = c(40, 38.4, 42.6), 
                       region = c("illinois", "illinois", "new york") ,
                       lab= c("Central Illinois\n(CIL)", "Southern\nIllinois\n(SIL)", "Western\nNew York\n(WNY)"))

mapa <- ggplot() +
  geom_polygon(data=all,
               mapping=aes(x=long, y=lat, group=subregion, fill=area),   #group=interaction(subregion,region)
               color="gray65", linewidth=0.2, alpha=0.4, show.legend=F) +
  scale_fill_manual(name = "Study Area",
                    breaks = c("WNY", "CIL", "SIL"),
                    labels=c("Western NY (WNY)", "Central Illinois (CIL)", "Southern Illinois (SIL)"),
                    values = c("#88CCEE", "#999933", "#FFAABB"),
                    na.value="white") +
  geom_polygon(data=states, mapping=aes(x=long, y=lat, group=group),
               color="gray30", linewidth=1, fill=NA) +
  facet_grid(.~ region, scales='free', space='free_x', switch='y') +
  ditch_the_axes +
  guides(color=guide_legend(override.aes=list(size=0))) +
  geom_text(data=ann_text, mapping=aes(x=x, y=y, label=lab), 
            size=6, fontface="bold", hjust="center", col="black", lineheight=0.9) +
  theme(
  legend.title=element_text(size=10, face="bold"),
        legend.text=element_text(size=10),
        legend.key.size=unit(0.4, "cm"),
        panel.background = element_rect(fill="white"),
        # legend.key=element_rect(colour="white"),
        strip.text=element_blank())

mapa


ggsave("plots/maps/FigS1_countymap_3areas.png", height=7.25, width=8)



