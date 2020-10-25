###
# Nigeria International School Profile Analysis: This code scrapes pages from
# The official Lagos State Schools internet portal
### 

library("tidyverse")
library("rvest")
library("robotstxt")


#First write function to get profile number/url for each page
get_all_links <- function(q, n){
  data <- data.frame()
  n <- n
  q <- q
  i = 1
  for(i in q:n){
    school_page <- read_html(paste0("http://lagosschoolsonline.com/schools/", i))
    urls <- school_page %>% # feed `main.page` to the next step
      html_nodes(".col-sm-8 a") %>% # get the CSS nodes
      html_attr("href") # extract the URLs
    # Get link text
    links <- school_page %>% # feed `main.page` to the next step
      html_nodes(".col-sm-8 a") %>% # get the CSS nodes
      html_text() # extract the link text
    loc <- school_page %>% 
      html_nodes(".text-xs-right .fg-orange") %>% # get the CSS nodes
      html_text("href")
    type <- school_page %>%
      html_nodes(".text-xs-center+ .col-sm-2 .fg-orange") %>% # get the CSS nodes
      html_text("href") # extract the link text
    # Combine `links` and `urls` into a data.frame
    data_pag <- data.frame(links = links, 
                           urls = urls, 
                           loc = loc, 
                           type = type, 
                           stringsAsFactors = FALSE)
    data <- bind_rows(data, data_pag)
    print(i)
  }
  Sys.sleep(10) #only run every 10 seconds to not overwhelm servers.
  data
}

#Now, run for all the pages
get_all_links <- get_all_links(1, 3) #as of 24/10/20, there were 533 pages


#now with the column of ids, use the older code to extract the page info going only to specf profile pages
all_links <- get_all_links %>%
  separate(urls, c("school", "porf", "profile"), sep = "/") %>%
  mutate(link_name = links, profile = as.numeric(profile)) %>%
  select(link_name, profile, loc, type)

#take a look
all_links

#Write to a csv for safekeeping
#write_csv(all_links, path = "~/Google Drive/Nigeria_school_scraping/all_links.csv")

#Now, in the seecond part, go to each page and extract all needed info

#This function gets all the data from a page
getschool <- function(school_url){
  #Using CSS selectors to scrape the panel text
  school_info_1col <- html_nodes(school_url, '.panel-profile p') %>%
    html_text() %>%
    as.data.frame() %>%
    rename(text = ".")
  
  #this pulls the titles of the firsst col
  school_info_titles_1col <- html_nodes(school_url,'h4') %>% 
    html_text() %>%
    as.data.frame() %>%
    filter(! . == "Past 3 Year Performance in %") %>%
    filter(! . == " Reviews") %>%
    mutate(name = tail(., 1)) %>%
    filter (! . == name) %>%
    rename(subtitle = ".")
  
  school_info_main <- bind_cols(school_info_titles_1col, school_info_1col) %>%
    spread(subtitle, text)
  school_info_main
}

#This version gets all the pages within a limit
get_all_schools_v2 <- function(all_links){
  all_links <- all_links
  data <- data.frame()
  i = 1
  for(i in all_links$profile){ 
    school <- read_html(paste0("http://lagosschoolsonline.com/schools/profile/", i))
    tryCatch({
      record_school <- getschool(school)
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", i))
      message(cond)
      # Choose a return value in case of error
      return(NA)
    }) 
    data <- bind_rows(data, record_school)
    print(i)
  }
  Sys.sleep(10)
  data
}

#Query codes
get_all_schools_v2_final <- get_all_schools_v2 (all_links)


#Now join with links/titles
final_scraped_dataset_w <- get_all_schools_v2_final %>%
  left_join(all_links, by = c("name" = "link_name")) %>%
  separate(name, c("school_name", "area"), sep = ",")

#Save R file
save(final_scraped_dataset_w,file="./data/final_scraped_dataset_w.Rda")

write_csv(final_scraped_dataset_w, path = "./data/final_scraped_dataset_w.csv")
