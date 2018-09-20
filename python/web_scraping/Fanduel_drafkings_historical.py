# -*- coding: utf-8 -*-
"""

"""


import os, sys,random,pandas,time,re,subprocess 
from subprocess import Popen, PIPE
import numpy as np
from pandas import DataFrame as df
import requests
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver import DesiredCapabilities
from selenium.webdriver.firefox.firefox_binary import FirefoxBinary
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC



def open_browser():
    chromedriver_path = "C:/Users/MonticT/Documents/AnalysisProjects/Fantasy/chromedriver.exe"
    chromeOptions = webdriver.ChromeOptions()
    #chromeOptions.add_experimental_option("prefs",prefs)
    chromeOptions.add_argument("--disable-extensions")
    chromeOptions.add_argument("--start-maximized")
    ''' capabilities = webdriver.DesiredCapabilities().CHROME '''
    port_nums = ['6001',	'6002','6003','6004',	'6005',	'6006',	'6007',	'6008',	'6009',	'6010',	'6012',	'6013',	'6014',	'6015',	'6016',	'6017',	'6018',	'6019',	'6020',	'6021',	'6022',	'6023',	'6024',	'6025',	'6027',	'6028',	'6029',	'6030',	'6031',	'6032',	'6034',	'6035',	'6036',	'6038',	'6039',	'6041',	'6042',	'6043',	'6044',	'6045',	'6046',	'6047',	'6049',	'6051',	'6052',	'6053',	'6054',	'6055',	'6056',	'6057',	'6058',	'6059',	'6060',	'6061',	'6062',	'6063',	'6064',	'6065',	'6066',	'6067',	'6068',	'6069',	'6070',	'6071',	'6072',	'6073',	'6074',	'6076',	'6077',	'6078',	'6079',	'6080',	'6081',	'6082',	'6083',	'6084',	'6086',	'6087',	'6088',	'6089',	'6090',	'6091',	'6094',	'6095',	'6096',	'6097',	'6098',	'6099',	'6100']
    port_num = int(random.choice(port_nums))

    driver = webdriver.Chrome(executable_path = chromedriver_path
                                   , chrome_options = chromeOptions
                                   , port = port_num)
    """
    self.driver = webdriver.Firefox(executable_path=self.chromedriver_path
                                    ,capabilities=DesiredCapabilities.FIREFOX,firefox_profile=None)
                                    #,proxy=None,options=None
                                    #,firefox_options=None)
    #self.driver.manage().window().maximize()
    """
    driver.get("http://www.dli.pa.gov/Businesses/Compensation/WC/insurance/Pages/Workers-Compensation-Insurance-Search-Form.aspx")
    return(driver)


import requests,pandas
from bs4 import BeautifulSoup
import urllib.request

sesh = requests.session()

headers = {
        'ACCEPT_ENCODING':'gzip, deflate, br',
        'ACCEPT_LANGUAGE':'en-US,en;q=0.9',
        'USER_AGENT':'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/66.0.3359.117 Safari/537.36'
        }

#Login Url
dk_url = 'http://rotoguru1.com/cgi-bin/fyday.pl?week=1&game=dk&scsv=1'

resp = sesh.get(dk_url,headers=headers)

html_soup = BeautifulSoup(resp.text, 'html.parser')

print(html_soup)
html_soup.find()
records = html_soup.find('pre').text
print(records)

#Get DK Salaries and Points
for i in range(1,18):
    dk_url = 'http://rotoguru1.com/cgi-bin/fyday.pl?week='+str(i)+'&game=dk&scsv=1'
    resp = sesh.get(dk_url,headers=headers)
    html_soup = BeautifulSoup(resp.text, 'html.parser')
    records = html_soup.find('pre').text
    with open('C:/Users/MonticT/Documents/AnalysisProjects/Fantasy/Dk-Salaries/dk_2017_wk_'+str(i)+'.txt', 'w') as the_file:
        the_file.write(records)
    print(i)
    
#2017 DK Salaries
for i in range(1,18):
    dk_url = 'http://rotoguru1.com/cgi-bin/fyday.pl?week='+str(i)+'&year=2017&game=dk&scsv=1'
    print(dk_url)
    resp = sesh.get(dk_url,headers=headers)
    html_soup = BeautifulSoup(resp.text, 'html.parser')
    records = html_soup.find('pre').text
    print(i)
    with open('C:/Users/MonticT/Documents/AnalysisProjects/Fantasy/Dk-Salaries/dk_2017_wk_'+str(i)+'.txt', 'w') as the_file:
        the_file.write(records)
    
#2014 - 2016 Get DK Salaries and Points
for x in range(2014,2017):
    for i in range(1,18):
        fanduel_url = 'http://rotoguru1.com/cgi-bin/fyday.pl?week='+str(i)+'&year='+str(x)+'&game=dk&scsv=1'
        print(fanduel_url)
        resp = sesh.get(fanduel_url,headers=headers)
        html_soup = BeautifulSoup(resp.text, 'html.parser')
        records = html_soup.find('pre').text
        print(i)
        with open('C:/Users/MonticT/Documents/AnalysisProjects/Fantasy/Dk-Salaries/dk_'+str(x)+'_wk_'+str(i)+'.txt', 'w') as the_file:
            the_file.write(records)



#2017 Get Fanduel Salaries and Points
for i in range(1,18):
    fanduel_url = 'http://rotoguru1.com/cgi-bin/fyday.pl?week='+str(i)+'&game=fd&scsv=1'
    resp = sesh.get(fanduel_url,headers=headers)
    html_soup = BeautifulSoup(resp.text, 'html.parser')
    records = html_soup.find('pre').text
    print(i)
    with open('C:/Users/MonticT/Documents/AnalysisProjects/Fantasy/Fanduel-Salaries/fd_2017_wk_'+str(i)+'.txt', 'w') as the_file:
        the_file.write(records)
    
#2011 - 2016 Get Fanduel Salaries and Points
for x in range(2011,2017):
    for i in range(1,18):
        fanduel_url = 'http://rotoguru1.com/cgi-bin/fyday.pl?week='+str(i)+'&year='+str(x)+'&game=fd&scsv=1'
        print(fanduel_url)
        resp = sesh.get(fanduel_url,headers=headers)
        html_soup = BeautifulSoup(resp.text, 'html.parser')
        records = html_soup.find('pre').text
        print(i)
        with open('C:/Users/MonticT/Documents/AnalysisProjects/Fantasy/Fanduel-Salaries/fd_'+str(x)+'_wk_'+str(i)+'.txt', 'w') as the_file:
            the_file.write(records)











