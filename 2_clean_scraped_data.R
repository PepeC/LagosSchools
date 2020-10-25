###
# Clean scraped data: Here we go and clean up the scraped data to make some 
# analyses related to words that come up frequently
###

# Words to search: British, Global, International, GCSC, IGCSC, SAT, A-Levels, American, Canadian, TOEFL, IELTS
# 1st question: How many schools are marketing themselves as "international" or talking about students to prepare to be global citizens?
# 2nd question: How many are actually doing specific activies for students to take these foreign classes. 
# So, basically, out of the ones that say they are international, how many are actually offering the exam itself.

#Bring in data from csv
final_scraped_dataset <- read_csv("final_scraped_dataset_w.csv")

# Now clean location data
final_scraped_dataset_w_clean <- final_scraped_dataset %>%
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

#save as csv
write_csv(final_scraped_dataset_w_counts, path = "~/final_scraped_dataset_w_counts.csv")
