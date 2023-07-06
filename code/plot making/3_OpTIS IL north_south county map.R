library(tidyverse)
library(maps)
library(reshape2)
# install.packages("sf")
library(sf)

# load data created in "OpTIS adoption rates IL.R"
means_countyNS <- read.csv("data/optis/means_county_ilNS.csv")
# get map data to match county names
means_countyNS$county <- str_remove(means_countyNS$county, " County")
# simplify dataset - just need counties and whether they are north or south
means_countyNS <- means_countyNS[,1:2]
# remove duplicated rows
means_countyNS <- means_countyNS %>%
  distinct() %>%
  arrange(county)

# missing Lake (N), Hardin (S), Jo Daviess (N), Union (S) Counties
means_countyNS[nrow(means_countyNS)+1,] <- c("N", "Lake")
means_countyNS[nrow(means_countyNS)+1,] <- c("S", "Hardin")
means_countyNS[nrow(means_countyNS)+1,] <- c("N", "Jo Daviess")
means_countyNS[nrow(means_countyNS)+1,] <- c("S", "Union")


# get county spatial data
ill <- map_data("county", "Illinois", proj="mercator")

# get ready to join with means_county data
ill$subregion <- str_to_title(ill$subregion)
ill$subregion <- ifelse(ill$subregion %in% "De Kalb", "DeKalb", 
                        ifelse(ill$subregion %in% "De Witt", "DeWitt", 
                               ifelse(ill$subregion %in% "Du Page", "DuPage", 
                                      ifelse(ill$subregion %in% "La Salle", "LaSalle",
                                             ifelse(ill$subregion %in% "Mcdonough", "McDonough",
                                                    ifelse(ill$subregion %in% "Mchenry", "McHenry",
                                                           ifelse(ill$subregion %in% "Mclean", "McLean",
                                                                  ifelse(ill$subregion %in% "St Clair", "St. Clair", 
                                                                         ill$subregion))))))))

# join rates with spatial data:
ill <- left_join(ill, means_countyNS, join_by(subregion==county))

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)



windows(xpinch=200, ypinch=200, width=5, height=5)

# plot for cover crops
ggplot() +
  geom_polygon(data=ill,
               mapping=aes(x=long, y=lat, fill=NS, group=subregion),
               color="black", linewidth=0.2, show.legend=F) +
  coord_sf(default_crs = sf::st_crs(4326)) +  # , # read x and y as latitude and longitude
  #          xlim = c(-92, -87), ylim=c(36.5, 43)) +
  scale_fill_manual(values=c("#EECC66","#6699CC"),
                    name= "Regions of Illinois") +
  annotate("text", x=0.01, y=0.78, label="North", size=14, fontface="bold") +
  annotate("text", x=0.01, y=0.73, label="South", size=14, fontface="bold") +  #, color="white"
  theme_bw() +
  ditch_the_axes # +
  # theme(legend.title=element_text(size=10, face="bold"),
  #       legend.text=element_text(size=10),
  #       legend.key.size=unit(0.4, "cm"))



ggsave("plots/county_ILnorthsouth.png")
