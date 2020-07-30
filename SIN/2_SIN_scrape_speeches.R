  #### Setting things up-----------------------------------------------------------------------------
  ### relevant packages
  require(tidyverse)
  require(rvest)
  require(httr)
  
  ### Directories
  ## set current directory at file location
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  
  #### Helper functions--------------------------------------------------------------------------
  ### randomize user agent  
  ua_df <- read_delim("https://raw.githubusercontent.com/51Degrees/Device-Detection/master/data/20000%20User%20Agents.csv", delim = "\n", col_names = FALSE)
  
  random_ua <- function(ua_list = ua_df$X1){
    
    return(sample(ua_list, 1))
    
  }

### prep page
prep_page <- function(page){
  ### turn the url to post request format
  page_id <- str_extract(page, "(?<=reportid\\=).*$")
  page2 <- paste0("https://sprs.parl.gov.sg/search/getHansardTopic/?id=", page_id)
  cat("\n>> POST page done")
  return(page2)
}

### parse post request
parse_post <- function(page2){
  ### empty handle, for avoiding overlapping cookies
  h1 <- handle('')
  ### Make a post request to get the JSON page
  post_request <- RETRY(
    verb = "POST",
    url = page2,
    handle = h1,
    encode = "json",
    add_headers(
      `referrer` = page,
      `User-Agent` = random_ua(), 
      `Origin` = "https://sprs.parl.gov.sg", 
      `Content-Type` = "application/json",
      `Accept-Language` = "q=0.9,en-US;q=0.8,en",
      `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8"
    ),
    content_type_json(),
    times = 4,
    pause_base = 20
  )
  
  ### Post responses come in two type, check which and parse accordingly
  ## extract the page and parse
  resp_raw <- content(post_request, as = "parsed")
  
  if (post_request$status_code == 200 & "resultHTML" %in% names(resp_raw)){
    
    # content
    rel_resp <- resp_raw$resultHTML
    
    
  } else if (post_request$status_code == 200 & "htmlContent" %in% names(resp_raw)) {
    
    # as is
    rel_resp <- resp_raw$htmlContent
    
  } else {
    
    rel_resp <- NA_character_
    
  }
  cat("\n>> POST request parsed")
  Sys.sleep(runif(1, min = 12, max = 25))
  return(rel_resp) 
}

### Get the metadata
get_meta <- function(parsed_post){
  
  ### Again, 2 ways of getting it for 2 html source-types
  ## check if a node called meta exists, if yes it is the old way, if not it is the new one
  meta_nodes <- try(parsed_post %>% 
    read_html() %>% 
    xml_find_all("//meta") %>% 
    length(), silent = TRUE)
  
  has_meta <- if_else(class(meta_nodes) != "try-error" && meta_nodes > 2,
                      "1",
                      "0")
  
  if (has_meta == "0"){
    
    ## parse it
    meta <- try(parsed_post %>%
      keep( ~ !is.null(.) & !is.list(.)) %>%
      as_tibble(), silent = TRUE)
        
      if (class(meta) == "try-error"){
        
        meta <- NA
        
      }
    
    
  } else {
    
    ## parse it
    meta <- try(parsed_post %>%
      read_html() %>%
      xml_nodes(xpath = "//meta[@name]") %>%
      map(., function(node){
        
        ## get att
        col <- node %>%
          xml_attr("name")
        
        ## get content
        cont <- node %>%
          xml_attr("content")
        
        ## df
        ret <- as_tibble(cont) %>%
          set_names(.,
                    col)
        
        return(ret)
      }) %>%
      bind_cols(), silent = TRUE)
    
  }
  
  
        if (class(meta) == "try-error"){
          
          meta <- NA
          
        }
  
  cat("\n>> Metadata collected")
  return(list("meta" = meta, "has_meta" = has_meta))
  
}

#### Tidy speech
tidy_speech <- function(has_meta, parsed_post, meta_df){
  
  ### Again, 2 ways of getting it for 2 html source-types
  ## check if a node called meta exists, if yes it is the old way, if not it is the new one
  if (has_meta == "0"){
    
    ## get the root node
    root <- try(meta_df$content %>%
      unlist() %>%
      read_html() %>%
      xml_root(), silent = TRUE)
    
    if(class(root) == "try-error"){
      
      root <- try(parsed_post %>%
                    unlist() %>%
                    read_html() %>%
                    xml_root(), silent = TRUE)
      
    }
    
    
    ## create the speech_df
    speech_df <- try(root %>%
                       xml_children() %>%
                       str_split(., '\\<strong((\\s+?)\\w+\\=\\"[a-zA-Z0-9_ ]+\\")?\\>(?!Column)|\\<b\\>(?!Column)') %>%
                       unlist() %>%
                       subset(., str_detect(., "\\</strong\\>|\\</b\\>")) %>%
                       map_df(.,
                              ~list(speaker = str_remove_all(str_split(.x, "</span\\>\\:|</b\\>\\:|\\</strong\\>\\:|\\:</b\\>|\\:\\</strong\\>|\\:\\</span\\>")[[1]][1], '\\<(/)?.*?\\>'),
                                    speech_raw = str_remove_all(str_split(.x, "</span\\>\\:|</b\\>\\:|\\</strong\\>\\:|\\:</b\\>|\\:\\</strong\\>|\\:\\</span\\>")[[1]][2], '\\<(/)?.*?\\>'))) %>%
                       filter(nchar(speech_raw) > 5 & str_detect(speaker, regex("session", ignore_case = TRUE)) == FALSE) %>%
                       mutate(speech_raw = str_remove(speech_raw, "^\\:"),
                              speaker = str_remove(speaker, "\\:|[[:cntrl:]]") %>%
                                str_trim()),
                     silent = TRUE)
    
    } else {
    
      ## get the root node
      root <- parsed_post %>%
        read_html() %>%
        xml_root()
      
      ## create the speech_df
      speech_df <- try(root %>%
                         xml_children() %>%
                         str_split(., '\\<strong((\\s+?)\\w+\\=\\"[a-zA-Z0-9_ ]+\\")?\\>(?!Column)|\\<b\\>(?!Column)') %>%
                         unlist() %>%
                         subset(., str_detect(., "\\</strong\\>|\\</b\\>")) %>%
                         map_df(.,
                                ~list(speaker = str_remove_all(str_split(.x, "</span\\>\\:|</b\\>\\:|\\</strong\\>\\:|\\:</b\\>|\\:\\</strong\\>|\\:\\</span\\>")[[1]][1], '\\<(/)?.*?\\>'),
                                      speech_raw = str_remove_all(str_split(.x, "</span\\>\\:|</b\\>\\:|\\</strong\\>\\:|\\:</b\\>|\\:\\</strong\\>|\\:\\</span\\>")[[1]][2], '\\<(/)?.*?\\>'))) %>%
                         filter(nchar(speech_raw) > 5 & str_detect(speaker, regex("session", ignore_case = TRUE)) == FALSE) %>%
                         mutate(speech_raw = str_remove(speech_raw, "^\\:"),
                                speaker = str_remove(speaker, "\\:|[[:cntrl:]]") %>%
                                  str_trim()),
                       silent = TRUE) 
    }
      
            if (class(speech_df) == "try-error" & is_empty(meta_df) == FALSE){
              
              speech_raw_final <- cbind(tibble(speech = "failed"), meta_df)
              
            } else if(is_empty(meta_df) == FALSE & nrow(speech_df) == 0) {
              
              speech_raw_final <- cbind(tibble(speech = "failed"), meta_df)
              
            } else if(is_empty(meta_df) == FALSE) {
              
              ## final dataframe
              speech_raw_final <- cbind(speech_df, meta_df) %>%
                select(-one_of("txt", "content")) %>%
                as_tibble()
              
            } else {
              
              speech_raw_final <- tibble(speech = "failed", metadata = "failed")
              
            }
      
      cat("\n>> speech data collected\n")
      return(speech_raw_final)
    
}
 
#### Scrape the speeches----------------------------------------------------------------------
listed_files <- list.files("meta/query_raw", full.names = TRUE)

listed_df <- map_df(listed_files, read_csv)

map(listed_df$speech_page, function(page){
      ## check if it was already done
      filename <- paste0("speech_raw/", str_extract(page, "(?<=reportid\\=).*"), ".csv")
      print(page)
      if (!file.exists(filename)){
        
        ### prep post page
        page2 <- prep_page(page = page)
        
        ### parse post request
        parsed_post <- parse_post(page2 = page2)
        
            if (!is.na(parsed_post)){
              
              ### get the metadata
              parsed_meta <- get_meta(parsed_post = parsed_post)
              
              ## tidy the speeches
              tidy_speech_raw <- tidy_speech(has_meta = parsed_meta$has_meta,
                                             parsed_post = parsed_post,
                                             meta_df = parsed_meta$meta) %>%
                mutate(speech_url = page,
                       post_page = page2,
                       doc_id = str_extract(page, "(?<=reportid\\=).*")) %>%
                as_tibble()
              
              
              print(tidy_speech_raw)
              
              ## export it
              write_csv(tidy_speech_raw,
                        path = filename)
              
              
            } else {
              
              tidy_speech_raw <- tibble(speech = "failed", metadata = "failed") %>%
                mutate(speech_url = page,
                       post_page = page2,
                       doc_id = str_extract(page, "(?<=reportid\\=).*")) %>%
                as_tibble()
              
              ## export it
              write_csv(tidy_speech_raw,
                        path = filename)
              
            }
        
      } else {
        
        print("done!")
      }
      
})
