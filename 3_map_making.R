####
#Map-Making: make af ew maps to chart how different school aspects of interest
#(e.g. number of intl schools, specific words used in school mission statements).
###

#Load libraries
require(rgdal)
library(rgeos)
require(ggplot2)
library(geosphere)
library(sf)
library(mapview)
library(ggmap) 

#Bring in schools visited. File was compiled by Ruth and includes lat/lon of schools, 
#the category of site (core vs. secondary interviews) and if the school has website or not

schools_visited <- read_csv("./data/schools_visited.csv") %>%
  separate(lat_lon, c("lat", "lon"), sep = ', ', convert = TRUE) %>%
  filter(!is.na(lat)) %>%
  filter(!cat == "Schools not visited")

lagos <- get_stamenmap(bbox = c(left = 3.1, bottom = 6.38, 
                                right = 3.7, top = 6.70), 
                       zoom = 12 ,
                       color = "bw",
                       maptype = "toner-background")

#basic streetmap
lagos_st <- get_stamenmap(bbox = c(left = 3.1, bottom = 6.32, 
                                   right = 3.6, top = 6.70), 
                          maptype = "terrain",
                          zoom = 12)

ggmap(lagos_st)

#Bring in LGA shapefile
shp2 <- st_read('./lagos_shapefiles/nga_admbnda_adm2_osgof_20190417.shp') %>%
  #st_transform(32617)
  filter(ADM1_EN == "Lagos")

#names
nga_adminboundaries_tabulardata <- read_csv("./lagos_shapefiles/nga_adminboundaries_tabulardata.csv")

lga_names <- nga_adminboundaries_tabulardata %>%
  select(id = "OBJECTID *", admin1Name_en, admin2Name_en) %>%
  filter(admin1Name_en == "Lagos" | admin1Name_en == "sokoto")

# using sf
sf_cent <- st_centroid(shp2) %>%
  filter(ADM1_EN == "Lagos")

#working base map!
ggmap(lagos) +
  geom_sf(data = shp2, inherit.aes = FALSE, alpha = 0.3) +
  geom_sf(data = sf_cent, inherit.aes = FALSE) 

#working base map!

#join shaofiles with count data
shp2_counts <- left_join(shp2, 
                         final_scraped_dataset_w_counts, 
                         by = c("ADM2_REF" = "loc"))

sf_cent_counts <- left_join(sf_cent, 
                            final_scraped_dataset_w_counts, 
                            by = c("ADM2_REF" = "loc")) 

#coloured area for schools that have International in Mission statement or school name
ggmap(lagos) +
  geom_sf(data = shp2_counts, aes(fill = Private_intl_name_mission), 
          inherit.aes = FALSE, alpha = 0.6) +
  geom_sf_text(data = sf_cent, check_overlap = TRUE, inherit.aes = FALSE, colour = "white", 
               aes(label = ADM2_REF), show.legend = TRUE) +
  scale_fill_viridis_c(name = "Has 'International' name or Mission") + 
  theme_void() +
  theme(legend.position="bottom")

ggsave("school_name_mission.png", height = 5, width = 8, units = 'in', dpi = 600, path = "./plots/")

#coloured area for schools offering foreing tests
ggmap(lagos) +
  geom_sf(data = shp2_counts, aes(fill = Private_us_uk_exams), 
          inherit.aes = FALSE, alpha = 0.6) +
  geom_sf_text(data = sf_cent, check_overlap = TRUE, inherit.aes = FALSE, colour = "white", 
               aes(label = ADM2_REF), show.legend = TRUE) +
  scale_fill_viridis_c(name = "Offers international exams") + 
  theme_void() +
  theme(legend.position="bottom")

ggsave("school_foreing tests.png", height = 5, width = 8, units = 'in', dpi = 600, path = "./plots/")

#coloured area
ggmap(lagos) +
  geom_sf(data =  sf_cent_counts, aes(size = toefl), 
          fill = "blue",  alpha = 0.3, show.legend = "point", inherit.aes = FALSE)

##working base map of schools
ggmap(lagos) +
  geom_sf(data = shp2, inherit.aes = FALSE, alpha = 0.3) +
  geom_jitter(data =  schools_visited, aes(x = lon, y = lat, fill = cat), 
              size = 5.1,  shape = 21, alpha = 0.7, width = 0.007) + 
  scale_fill_manual(values = c("blue", "darkgreen", "lightyellow"), 
                    name = "Schools in study") +  
  theme_void() +
  theme(legend.position="bottom")

ggsave("schools_visited.png", height = 5, width = 8, units = 'in', dpi = 600, path = "./plots/")

##working base map of schools v2
ggmap(lagos) +
  geom_sf(data = shp2_counts, aes(fill = ADM2_REF), 
          inherit.aes = FALSE, alpha = 0.6) +
  #geom_sf(data = shp2, inherit.aes = FALSE, alpha = 0.3) +
  theme_void() +
  scale_fill_discrete(name = "Lagos State Government Areas") +
  theme(legend.position="bottom")

ggsave("schools_visited_v2.png", height = 5, width = 8, units = 'in', dpi = 600, path = "~/Google Drive/Nigeria_school_scraping/")


