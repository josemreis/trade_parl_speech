#### Setting things up-----------------------------------------------------------------------------
### relevant packages
require(tidyverse)
require(rvest)
require(jsonlite)
require(httr)

### Directories
## set current directory at file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## output folders
# txt
if(!dir.exists("speech_raw")){
  dir.create("speech_raw")
}

if(!dir.exists("speech_data")){
  dir.create("speech_data")
}

#### The scraper--------------------------------------------------------------------------------


#### Setting things up
### page template
search_template <- "http://api.openparliament.ca/search/?q=trade%2A%20Type%3A%20%22debate%22&sort=date+asc&page="

### first page
## get all pages
page_n <- paste0(search_template, "1") %>%
  read_html() %>%
  html_node(".long-paginator > li:nth-child(12) > a:nth-child(1)") %>%
  html_text() %>%
  as.numeric()

### csv to keep track fo the pages scraped
scraper_log <- read_csv("scraper_log.csv")

#### Start the mapping function
map(seq(1, page_n, by = 1), function(n){

  ## create the page url
  page <- paste0(search_template, n)
  
  if (!page %in% scraper_log$page){
    ## get all urls of speeches and turn them to json format
    speeches_json <- page %>%
      read_html() %>%
      html_nodes(xpath = "//a[@class = 'statement_topic']") %>%
      html_attr("href") %>%
      str_remove(., "#.*$") %>%
      paste0("http://api.openparliament.ca", ., "?format=json")
    
    ### Make the api queries and turn to csv
    map(speeches_json, function(current_url){
      
      ## make the filename 
      filename <- str_remove_all(current_url,"http://api.openparliament.ca/|/\\?format\\=json") %>%
        str_replace_all("/", "_") %>%
        paste0("speech_raw/", .)
      
      if(!file.exists(filename)){
        
        ## make the request
        req <- httr::GET(current_url, add_headers("email" = "jose.reis@ile-hamburg.de",
                                                  "purpose" = "academic research"), content_type_json())
        # parse the response
        resp <- content(req)
        
        if(req$status_code == "200"){
          
          ## turn to dataframe
          dta <- tibble(speaker = ifelse(is_empty(resp$attribution$en),
                                         NA_character_,
                                         resp$attribution$en),
                        speaker_url = ifelse(is_empty(resp$politician_url),
                                             NA_character_,
                                             resp$politician_url),
                        text = ifelse(is_empty(resp$content$en),
                                      NA_character_,
                                      resp$content$en),
                        speech_url = str_remove_all(current_url, "api\\.|\\?format\\=json"),
                        speech_url_json = current_url,
                        source_id = ifelse(is_empty(resp$source_id),
                                           NA_character_,
                                           resp$source_id),
                        politician_membership_url = ifelse(is_empty(resp$politician_membership_url),
                                                           NA_character_,
                                                           resp$politician_membership_url))
          
          ## export it
          write_csv(dta,
                    path = filename)
        
          
        } else {
          
          stop_for_status(req)
          
        }
        
        
      }
      
    })
    
    ## mark this page as done
    done <- tibble(page = page,
                   done = "1")
    
    # append
    write_csv(done,
              append = TRUE,
              path = "scraper_log.csv")
    
    
  } else {
    
    print("page scraped!")
  }
    
 }
)

#### Put them together
listed <- list.files("speech_raw", full.names = TRUE)

dta <- map(listed, ~(data.table::fread(., sep = ",", fill = TRUE))) %>%
  data.table::rbindlist(fill = TRUE, use.names = TRUE)


## export
write_csv(dta,
          path = "speech_data/1_CAN_speech_raw.csv")
