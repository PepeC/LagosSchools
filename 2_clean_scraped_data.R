###
# Clean scraped data: Here we go and clean up the scraped data to make some 
# analyses related to words that come up frequently
###

# Words to search: British, Global, International, GCSC, IGCSC, SAT, A-Levels, American, Canadian, TOEFL, IELTS
# 1st question: How many schools are marketing themselves as "international" or talking about students to prepare to be global citizens?
# 2nd question: How many are actually doing specific activies for students to take these foreign classes. 
# So, basically, out of the ones that say they are international, how many are actually offering the exam itself.

#Bring in data from csv



final_scraped_dataset_w <- get_all_schools_v2_final %>%
  left_join(all_links, by = c("name" = "link_name")) %>%
  separate(name, c("school_name", "area"), sep = ",")

#Save R file
#save(final_scraped_dataset_w,file="~/Google Drive/Nigeria_school_scraping/final_scraped_dataset_w.Rda")

#write_csv(final_scraped_dataset_w, path = "~/Google Drive/Nigeria_school_scraping/final_scraped_dataset_w.csv")



# Now extract useful info
final_scraped_dataset_w_clean <- final_scraped_dataset_w %>%
  mutate(loc = str_trim(loc, c("both")), #trim whitespace
         loc = ifelse(loc == "Ajeromi LGA", "Ajeromi-Ifelodun",
                      ifelse(loc == "Amuwo Odofin", "Amuwo-Odofin",
                             ifelse(loc == "Lagos-Island", "Lagos Island",
                                    ifelse(loc == "Ibeju-Lekki", "Ibeju/Lekki",
                                           ifelse(loc == "Ifako/Ijaye", "Ifako-Ijaye",loc)))))) %>%
  filter(!loc == "") %>%
  mutate(type = str_trim(type, c("both")),
         type = ifelse(str_detect(`type`, "Privately") == "TRUE", "Private", type),
         type = ifelse(str_detect(`type`, "State") == "TRUE", "State", type))



#example of coutns
final_scraped_dataset_w_counts <- final_scraped_dataset_w_clean %>%
  distinct(school_name, area, loc, .keep_all = TRUE) %>%
  mutate(
    intl_name = str_detect( `school_name` , "INTERNATIONAL|British|American|Canadian|French"),
    intl_mission = str_detect( `  Our Mission Statement` , "INTERNATIONAL|British|American|Canadian|French|Global"),
    intl_subject = str_detect( `  Subject Offered` , "INTERNATIONAL|British|American|Canadian|Global"),
    us_edu = str_detect( `  Examinations Taken` , "TOEFL|T.O.E.F.L|SAT|S.A.T"),
    uk_edu = str_detect( `  Examinations Taken` , "IELTS|I.E.L.T.S|IGCSE| I.G.C.S.E|A Level|Checkpoint|Cambridge"),
    year_founded = str_extract(`  School History`, "\\d+"),
    intl_name_mission = ifelse(intl_name == "TRUE" | intl_mission == "TRUE", "TRUE", "FALSE"),
    us_uk_exams = ifelse(us_edu == "TRUE" | uk_edu == "TRUE", "TRUE", "FALSE"),
    intl_name_mission_and_exams = ifelse(us_uk_exams == "TRUE" & intl_name_mission == "TRUE", "TRUE", "FALSE")) %>%
  gather(var, vals, us_uk_exams, intl_name, intl_mission, intl_name_mission, intl_subject, intl_name_mission_and_exams) %>%
  group_by(type, loc, var) %>% # 
  summarise(loc_counts = sum(ifelse(vals == "TRUE", 1, 0), na.rm = T)) %>%
  unite(type_var, type, var) %>%
  spread(type_var, loc_counts)

write_csv(final_scraped_dataset_w_counts, path = "~/Google Drive/Nigeria_school_scraping/final_scraped_dataset_w.csv")

#Map-Making ---- 
#get centroids
require(rgdal)
library(rgeos)
require(ggplot2)
library(geosphere)
library(sf)
library(mapview)
library(ggmap)

#devtools::install_github("dkahle/ggmap", ref = "tidyup")

#Bring in schools visited
schools_visited <- read_csv("~/Google Drive/Nigeria_school_scraping/schools_visited.csv") %>%
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
shp2 <- st_read('~/Google Drive/Nigeria_school_scraping/nga_adm_osgof_20190417_SHP/nga_admbnda_adm2_osgof_20190417.shp') %>%
  #st_transform(32617)
  filter(ADM1_EN == "Lagos")

#names
nga_adminboundaries_tabulardata <- read_csv("~/Google Drive/Nigeria_school_scraping/nga_adm_osgof_20190417_SHP/nga_adminboundaries_tabulardata.csv")

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
shp2_counts <- left_join(shp2, final_scraped_dataset_w_counts, by = c("ADM2_REF" = "loc"))

sf_cent_counts <- left_join(sf_cent, final_scraped_dataset_w_counts, by = c("ADM2_REF" = "loc")) 

#coloured area
ggmap(lagos) +
  geom_sf(data = shp2_counts, aes(fill = Private_intl_name_mission), 
          inherit.aes = FALSE, alpha = 0.6) +
  geom_sf_text(data = sf_cent, check_overlap = TRUE, inherit.aes = FALSE, colour = "white", 
               aes(label = ADM2_REF), show.legend = TRUE) +
  scale_fill_viridis_c(name = "Has 'International' name or Mission") + 
  theme_void() +
  theme(legend.position="bottom")

ggsave("school_name_mission.png", height = 5, width = 8, units = 'in', dpi = 600, path = "~/Google Drive/Nigeria_school_scraping/")

#coloured area
ggmap(lagos) +
  geom_sf(data = shp2_counts, aes(fill = Private_us_uk_exams), 
          inherit.aes = FALSE, alpha = 0.6) +
  geom_sf_text(data = sf_cent, check_overlap = TRUE, inherit.aes = FALSE, colour = "white", 
               aes(label = ADM2_REF), show.legend = TRUE) +
  scale_fill_viridis_c(name = "Offers international exams") + 
  theme_void() +
  theme(legend.position="bottom")

ggsave("school_foreing tests.png", height = 5, width = 8, units = 'in', dpi = 600, path = "~/Google Drive/Nigeria_school_scraping/")


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

ggsave("schools_visited.png", height = 5, width = 8, units = 'in', dpi = 600, path = "~/Google Drive/Nigeria_school_scraping/")

##working base map of schools v2
ggmap(lagos) +
  geom_sf(data = shp2_counts, aes(fill = ADM2_REF), 
          inherit.aes = FALSE, alpha = 0.6) +
  #geom_sf(data = shp2, inherit.aes = FALSE, alpha = 0.3) +
  theme_void() +
  scale_fill_discrete(name = "Lagos State Government Areas") +
  theme(legend.position="bottom")

ggsave("schools_visited_v2.png", height = 5, width = 8, units = 'in', dpi = 600, path = "~/Google Drive/Nigeria_school_scraping/")



#  1. How many schools brand / market themselves as 'international'?



A) In name? 
  
  International

#Or country names - 'British' - 'American' - 'Canadian' - 'French' etc. etc. 



#B) In broader terms e.g. mission and vision statements?

Global 

International



#subquestions: What is geographical distribution? Of these, how many are private / public? what is overlap with answer to question below? 





# using knoema
library(Knoema)

ed.dat.lagos <- Knoema('iynrgrf', type = "DataFrame", list('timerange' = '2004M1-2014M1', 'frequency' = 'A',
                                                           'Indicators' = '6433619;6435419;6435519;6435619;6753319;6753419;6753519;6753619;6754919', 
                                                           'States' = '25'), host='nigeria.opendataforafrica.org')  


ed.dat.lagos2 <- Knoema('iynrgrf',  type = "DataFrame", 
                        list('timerange' = '1981-2014', 'frequency' = 'A', 
                             'Indicators' = '6436319;6436219;6436119;6436019;6435919;6433719;6433819;6433919;6434019;6434119;6434219;6434319;6434419;6434519;6434619;6434719;6434819;6434919;6435019;6435119;6435219;6435319;6435419;6435519;6438419;6438519;6438619;6438719;6438819;6438919;6439019;6439119;6439219;6435719;6435819;6435619', 
                             'States' = '25'), host='nigeria.opendataforafrica.org')

#NECo exams
ed.dat.lagos3 <- Knoema('sttzqvd',  type = "DataFrame", 
                        list('frequency' = 'H', 'Indicator' = 'A2;A3;A4;A5;A6;A7', 'Sex' = 'M;F;T', 'State' = 'NG-LA'), 
                        host='nigeria.opendataforafrica.org')

ed.dat.lagos2 %>% select("Lagos - Private secondary schools, Total - A",                        
                         "Lagos - Public Primary School, Total - A",                            
                         "Lagos - Public Secondary schools, Total - A",                         
                         "Lagos - Pupils in Primary School, Total - A") %>% rownames_to_column("year")

