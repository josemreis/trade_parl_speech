#### Setting things up-----------------------------------------------------------------------------
### relevant packages
require(tidyverse)
require(jsonlite)

### Directories
## set current directory at file location
setwd("/home/jmr/Dropbox/Current projects/thesis_papers/media and trade preferences/data/parliamentary_speech/US/congressional-record")

## output folders
# txt
if(!dir.exists("speech_raw")){
  dir.create("speech_raw")
}

## get the current cr file
current_cr <- read_lines('/home/jmr/Dropbox/Current projects/thesis_papers/media and trade preferences/data/parliamentary_speech/US/congressional-record/current_cr.txt')

### Parse all the json files
listed_files <- list.files(paste(current_cr, "json", sep = "/"), full.names = TRUE)

#### Parse, filter, and export relevant documents
map(listed_files, function(cur_file){
  print(cur_file)
  parsed <- fromJSON(cur_file, flatten = TRUE)
  
  df_raw <- cbind(parsed$content, as_tibble(parsed$header)) %>%
    mutate(doc_title = ifelse(is_empty(parsed$title) || is.na(parsed$title),
                               NA_character_,
                              parsed$title),
           doc_id = parsed$id,
           congress = parsed$congress,
           mentions_trade_txt = if_else(str_detect(text, regex("trade", ignore_case = TRUE)),
                                        "1",
                                        "0")) %>%
    as_tibble() %>%
    select(doc_title, everything())
  
  ## filter the ones which match the word trade
  filtered <- filter(df_raw, mentions_trade_txt == "1" & kind == "speech")
  
  if (nrow(filtered) > 0){
    
    write_csv(filtered,
              path = paste0("speech_raw/", unique(filtered$doc_id), ".csv"))
    
  }})


