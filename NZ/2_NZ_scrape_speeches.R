#### Setting things up-----------------------------------------------------------------------------
### relevant packages
require(tidyverse)
require(rvest)
require(httr)
require(wayback)

### Directories
## set current directory at file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### get the metadata
listed_files <- list.files("meta/meta_raw", full.names = TRUE)

if (!file.exists("meta/query_metadata.csv")){
      ## read them
      metadata_raw <- map_df(listed_files, ~(read_csv(.)))
      
      ## export
      write_csv(metadata_raw,
                path = "meta/query_metadata.csv")
  
}

## load the query metadata
query_meta <- read_csv("meta/query_metadata.csv") %>%
  select(-X1)
#### Helper functions--------------------------------------------------------------------------
### randomize user agent
ua_df <- read_delim("https://raw.githubusercontent.com/51Degrees/Device-Detection/master/data/20000%20User%20Agents.csv", delim = "\n", col_names = FALSE)

random_ua <- function(ua_list = ua_df$X1){
  
  return(sample(ua_list, 1))
  
}

### internet archive url fetcher
ia_fetch <- function(page){
  
  ## check if it is available
  ia_page <- try(archive_available(page) %>%
    arrange(desc(timestamp)) %>%
    slice(1) %>%
    pull(closet_url), silent = TRUE)
  
  if (class(ia_page) != "try-error" & nchar(ia_page) > 3){
    
    ret <- ia_page
    
    cat("\n>> Has archived page\n")
    
  } else {
    
    ret <- page
    
    cat("\n>> No archived page\n")
  }
  
  return(ret)
  
}

### parse get request
parse_GET <- function(page){
  ## if ia page, no wait time
  if (str_detect(page, fixed("web.archive")) == FALSE){
    
        ### empty handle, for avoiding overlapping cookies
        h1 <- handle('')
        ### Make a post request to get the JSON page
        get_request <- try(
          RETRY(
            verb = "GET",
            url = page,
            handle = h1,
            encode = "json",
            add_headers(
              `referrer` = page,
              `User-Agent` = random_ua(),  
              `Content-Type` = "application/json",
              `Accept-Language` = "q=0.9,en-US;q=0.8,en",
              `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8"
            ),
            content_type_xml(),
            times = 5,
            pause_base = 30
          ), silent = TRUE)
        
        if(class(get_request) == "try-error"){
          
          return(NA)
          
          
        }
        
        ### if status ok
        if(get_request$status_code == 200){
          
              ## extract the page and parse
              resp_raw <- read_html(get_request)
              
              cat("\n>> GET request parsed\n")
              
          
        } else {
          
              Sys.sleep(300)
              h1 <- handle('')
              ### Make a post request to get the JSON page
              get_request <- RETRY(
                verb = "GET",
                url = page,
                handle = h1,
                encode = "json",
                add_headers(
                  `referrer` = page,
                  `User-Agent` = random_ua(),  
                  `Content-Type` = "application/json",
                  `Accept-Language` = "q=0.9,en-US;q=0.8,en",
                  `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8"
                ),
                content_type_xml(),
                times = 5,
                pause_base = 30
              )
              
              if(get_request$status_code == 200){
                
                ## extract the page and parse
                resp_raw <- read_html(get_request)
                
                cat("\n>> GET request parsed")
            
          } else if(get_request$status_code == 404) {
                ## page not found error
                resp_raw <- NA
            
          } else {
            
                stop_for_status(get_request)
            
          }
          
          
        }
        
        Sys.sleep(runif(1, min = 2, max = 15))
    
    
  } else {
    
    ### empty handle, for avoiding overlapping cookies
    h1 <- handle('')
    ### Make a post request to get the JSON page
    get_request <- RETRY(
      verb = "GET",
      url = page,
      handle = h1,
      encode = "json",
      add_headers(
        `referrer` = page,
        `User-Agent` = random_ua(),  
        `Content-Type` = "application/json",
        `Accept-Language` = "q=0.9,en-US;q=0.8,en",
        `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8"
      ),
      content_type_xml(),
      times = 5,
      pause_base = 30
    )
    
    ### if status ok
    if(get_request$status_code == 200){
      
      ## extract the page and parse
      resp_raw <- read_html(get_request)
      
      cat("\n>> GET request parsed")
      
      
    } else {
      
      Sys.sleep(300)
      h1 <- handle('')
      ### Make a post request to get the JSON page
      get_request <- RETRY(
        verb = "GET",
        url = page,
        handle = h1,
        encode = "json",
        add_headers(
          `referrer` = page,
          `User-Agent` = random_ua(),  
          `Content-Type` = "application/json",
          `Accept-Language` = "q=0.9,en-US;q=0.8,en",
          `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8"
        ),
        content_type_xml(),
        times = 5,
        pause_base = 30
      )
      
      if(get_request$status_code == 200){
        
        ## extract the page and parse
        resp_raw <- read_html(get_request)
        
        cat("\n>> GET request parsed\n")
        
      } else if(get_request$status_code == 404) {
        ## page not found error
        resp_raw <- NA
        
      } else {
        
        stop_for_status(get_request)
        
      }
      
      
    }
    
    
    
  }
  
  return(resp_raw) 
}

### Scrape speech_data
get_speech <- function(page_parsed, page){
  
  if (!is.na(page_parsed)){
    ## get the root node
    root <- xml_root(page_parsed)
    
    ## get all the paragraphs
    pars_raw <- root %>% 
      xml_find_all('//p') %>%
      map_df(.,
             ~ list(txt = xml_text(.x),
                    attr = xml_attr(.x, "class"),
                    child_nodes = xml_children(.x) %>% 
                      paste(., collapse = "\n")),
             .id = "paragraph") %>%
      filter(str_detect(attr, regex("\\ba\\b|speech|interject|sub|question|answer|statement|intervent", ignore_case = TRUE)))
    
    ## extract speakers, remove them from the text
    speech_raw <- pars_raw %>%
      filter(nchar(txt) > 3) %>%
      mutate(speaker = map_chr(child_nodes, function(x) paste0(unlist(str_extract_all(str_remove_all(x, "[\r\n\t]+"), "(?<=\\<strong\\>).*?(?=\\</strong\\>)")), collapse = " ")),
             speaker = replace(speaker, which(nchar(speaker) < 3), NA_character_),
             speech_page = page) %>%
      mutate(speaker = zoo::na.locf0(speaker)) %>% ## assign speaker to the last available speaker
      select(paragraph, speaker, speech_raw = txt, speech_node_attr = attr, speech_page) 
    
  } else {
    
      speech_raw <- NA
    
  }
  
  ## return
  return(speech_raw)
}

#### Scrape it
map(query_meta$speech_page, function(page){
  ## check if it was already done
  filename <- paste0("speech_raw/", str_replace(str_extract(page, "(?<=document/).*$"), "/", "_"), ".csv")
  print(page)
  if (!file.exists(filename)){
    
    ## get IA page, if available
    page2 <- ia_fetch(page)
    
    ## parse
    page_parsed <- parse_GET(page2)
    
    if (!is.na(page_parsed)){
      
      speech_df_raw <- get_speech(page_parsed = page_parsed, page = page2) %>%
        inner_join(., query_meta)
      
      
      print(speech_df_raw)
      
      ## export it
      write_csv(speech_df_raw,
                path = filename)
      
      
    } else {
      
      tidy_speech_raw <- tibble(speech_page = page) %>%
        inner_join(., query_meta)
      
      ## export it
      write_csv(tidy_speech_raw,
                path = filename)
      
    }
    
  } else {
    
    ### check if no speakers were parsed, if yes it is probably due to the previous bug. Re-run it
    dta <- read_csv(filename) %>%
      filter(!is.na(speaker))
    
    if (nrow(dta) == "0"){
      
      print("rescraping..")
      ## re run it
      speech_df_raw <- try(get_speech(page_parsed = page_parsed, page = page) %>%
        inner_join(., query_meta), silent = TRUE)
      
      
      print(speech_df_raw)
      
      if (class(speech_df_raw) != "try-error"){
        
        ## export it
        write_csv(speech_df_raw,
                  path = filename)
        
      }
      
    } else {
      
      print("done!")
      
    }
    
  }
  
})
