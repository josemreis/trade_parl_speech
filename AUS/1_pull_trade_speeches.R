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

if(!dir.exists("meta")){
  dir.create("meta")
}

if(!dir.exists("speech_data")){
  dir.create("speech_data")
}

#### Queries to openaustralia's API-----------------------------------------------------------------------

### Setting things up
## prepare the queries
## read in the api key
key <- read_lines('/home/jmr/openaustralia_keys.txt')

## prepare the url
query_url_template <- paste0("http://www.openaustralia.org/api/getDebates?key=", key, "&type=representatives&search=trade&order=d&output=js&page=")

### pagination works would multiples of 20, so we will use a while loop. If offset + 20 > num_results, stop
page <- 1
# baseline, any number larger or equal than 0
result_diff <- 0
### start the while loop
while (result_diff >= 0){
  
  ## create the filename
  filename <- paste0("meta/call_", page, ".csv")
  ## proceed if call hasn't been made yet
  if (file.exists(filename) == FALSE) {
    ## prepare the query url
    my_url <- paste0(query_url_template, page)
    ## make the request
    req <- httr::GET(my_url)
    
    ## check the status
    if(req$status_code == 200){
      
      ## parse the response
      # parse
      parsed <- fromJSON(content(req), flatten = TRUE)
      
      # get the data
      dta <- as_tibble(parsed$rows) %>%
        select(-contains("office"))
      
      # ## export it
      write_csv(dta,
                path = filename)
      
    } else {
      
      stop_for_status(req)
    }
    
    
  }
  # offset
  offset <- parsed$info$first_result + 19
  # get the difference between number of results and next offset
  result_diff <- parsed$info$total_results - (offset + 20)
  
  ## next page
  page <- page + 1
}

### Put them all in one
listed <- list.files("meta", full.names = TRUE)

dta <- map(listed, ~(data.table::fread(.))) %>%
  data.table::rbindlist(use.names = TRUE)


## export
write_csv(dta,
          path = "speech_data/1_AUS_speech_raw.csv")
