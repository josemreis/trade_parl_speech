#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb 25 23:51:13 2020

@author: jmr
"""

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 24 17:45:50 2020

@author: jmr
"""
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import os
import time
from random import randrange
import random
from fake_useragent import UserAgent
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
import re
import pandas as pd
import numpy as np
import math
import glob
from datetime import datetime

### Setting things up
meta_dir = '/home/jmr/Dropbox/Current projects/thesis_papers/media and trade preferences/data/parliamentary_speech/SIN/meta/' 
query_dir = '/home/jmr/Dropbox/Current projects/thesis_papers/media and trade preferences/data/parliamentary_speech/SIN/meta/query_raw/' 

# query url
query_url = 'https://sprs.parl.gov.sg/search/home'
# headless
headless = True
#### Metadata scraper
## chrome options
## randomize the user agent
ua = UserAgent()
userAgent = ua.random
chromeOptions = webdriver.ChromeOptions()
chromeOptions.add_argument(f'user-agent={userAgent}')
chromeOptions.add_experimental_option("excludeSwitches", ["ignore-certificate-errors", "safebrowsing-disable-download-protection", "safebrowsing-disable-auto-update", "disable-client-side-phishing-detection"])
chromeOptions.add_argument('--disable-extensions')
chromeOptions.add_argument('--profile-directory=Default')
chromeOptions.add_argument("--incognito")
chromeOptions.add_argument("--disable-plugins-discovery");
chromeOptions.add_argument("--start-maximized")
chromeOptions.add_argument('--headless')
chromeOptions.add_argument('--no-sandbox') 
chromeOptions.add_argument('--disable-gpu')

## fire up the selenium driver
driver = webdriver.Chrome(options = chromeOptions)
# go to the query results page
driver.get(query_url)
# wait for it to load (takes a while)
WebDriverWait(driver, 140).until(EC.presence_of_element_located((By.CSS_SELECTOR, "#divmpscreen2 > div.row > div.col-sm-12.text-right.pull-right > div > button:nth-child(2)")))

### Prepare the query
driver.find_element_by_xpath('//*[@id="divmpscreen2"]/div[2]/div[1]/div/div[1]/input').send_keys("trade")

## make the query
driver.find_element_by_css_selector("#divmpscreen2 > div.row > div.col-sm-12.text-right.pull-right > div > button:nth-child(2)").click()
driver.switch_to.window(driver.window_handles[1])

#### Metadata scraper
## while loop (unknown number of pages)
result_diff = 1

while result_diff > 0:
    ## scrape results number
    WebDriverWait(driver, 40).until(EC.presence_of_element_located((By.XPATH,'//*[@class = "showingResults"]')))
    time.sleep(2)
    res = driver.find_element_by_xpath('//*[@class = "showingResults"]').text
    last_result = int(res.split(" to ")[1].split(" of ")[0])  
    all_results = int(res.split(" to ")[1].split(" of ")[1])  
    result_diff = all_results - last_result
    print("scraping -> " + res)
    ## file name
    current_results = re.sub("\s+", "_", re.sub("Showing ", "", res))
    filename = query_dir + current_results + ".csv"
    ## chekc if done
    cont = []
    if os.path.isfile(filename) == False:
        ## Get all the speeches url
        speeches_elem = driver.find_elements_by_xpath('//*[@id="searchResults"]/table//td/a')
        for page in speeches_elem:
            # get the page
            page.click()
            WebDriverWait(driver, 40).until(EC.presence_of_element_located((By.CSS_SELECTOR,'#header-full-top')))
            driver.switch_to.window(driver.window_handles[2])
            time.sleep(randrange(1, 2))
            ## get url
            speech_page = driver.current_url 
            ## export
            dta = pd.DataFrame([{"speech_page":speech_page}])
            cont.append(dta)
            ## close the window
            driver.close()
            driver.switch_to.window(driver.window_handles[1]) 
        ## concat and export
        export = pd.concat(cont)
        export.to_csv(filename)
    ## click next page
    driver.find_element_by_xpath('//a[em[@class = "fa fa-angle-right"]]').click()
    time.sleep(randrange(4, 8))        
