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
meta_dir = '/home/jmr/Dropbox/Current projects/thesis_papers/media and trade preferences/data/parliamentary_speech/NZ/meta/' 
meta_raw = '/home/jmr/Dropbox/Current projects/thesis_papers/media and trade preferences/data/parliamentary_speech/NZ/meta/meta_raw/' 

# query url
query_url = 'https://www.parliament.nz/en/pb/hansard-debates/rhr/search?Criteria.Keyword=trade&Criteria.ViewAll=1'

#### Metadata scraper
## fire up the selenium driver
driver = webdriver.Chrome()
# go to the query results page
driver.get(query_url)
# wait for it to load (takes a while)
WebDriverWait(driver, 140).until(EC.presence_of_element_located((By.CSS_SELECTOR, "div.wrapper--main:nth-child(1) > div:nth-child(1) > h1:nth-child(1)")))

### Scrape the speech page, title, speech_date, speech_type, and hansard record data
speech_rows = len(driver.find_elements_by_xpath('//*[@id="filterformsearchtarget"]/div[1]/div/div[4]/ol/li[@class="accordion-details-list__item"]'))

## loop for extracting the data
for row in range(1, speech_rows + 1):
    if os.path.isfile(meta_raw + str(row) + ".csv") == False:
        ## get the speech page
        speech_page = driver.find_element_by_xpath('//*[@id="filterformsearchtarget"]/div[1]/div/div[4]/ol/li[@class="accordion-details-list__item"][' + str(row) + ']/div/a').get_attribute("href")
        ## get the title
        speech_title = driver.find_element_by_xpath('//*[@id="filterformsearchtarget"]/div[1]/div/div[4]/ol/li[@class="accordion-details-list__item"][' + str(row) + ']/div/a').get_attribute("title")
        ## get the date
        speech_date = driver.find_element_by_xpath('/html/body/div[3]/div[1]/div[2]/div[1]/div/div[4]/div[4]/table/tbody/tr[' + str(row) + ']/td[3]').text
        ## hansard record
        # click on the view more
        #driver.find_element_by_xpath('//*[@id="filterformsearchtarget"]/div[1]/div/div[4]/ol/li[@class="accordion-details-list__item"][' + str(row) + ']/div//span[@class = "accordion-details-list__view-icon iconf-plus"]').click()
        # details
        speech_type = driver.find_element_by_xpath('/html/body/div[3]/div[1]/div[2]/div[1]/div/div[4]/div[4]/table/tbody/tr[1]/td[2]').text
        hansard_details = driver.find_element_by_xpath('/html/body/div[3]/div[1]/div[2]/div[1]/div/div[4]/div[4]/table/tbody/tr[' + str(row) + ']/td[4]').text
        ## turn to data frame
        dta = pd.DataFrame([{"speech_title":speech_title,
                       "speech_date": speech_date,
                       "speech_type": speech_type,
                       "hansard_details": hansard_details,
                       "speech_page": speech_page}])
        # export
        dta.to_csv(meta_raw + str(row) + ".csv")
        time.sleep(1)

    