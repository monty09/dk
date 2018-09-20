#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed May 30 19:20:12 2018

@author: storm
"""

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
login2 = 'https://www.nectron.com/LoginCheck.asp'
data = {
        'email':'taylormonticelli@upwork.com',
        'password':'upwork',
        'submit':'Log In'
        }
login_resp = sesh.post(login2,data = data)

df = pandas.read_csv('/Users/storm/Desktop/Python/Manasota/nectron/nectron.csv',index_col =None)

df['link1'] = ""
df['link2'] = ""
df['link3'] = ""
df['link4'] = ""
df['link5'] = ""
df['link6'] = ""
    
rows = df.shape[0]
for i in range(0,rows):
    search = df['SearchLink'][i]
    link_resp = sesh.get(search)
    html_soup = BeautifulSoup(link_resp.text, 'html.parser')
    records = html_soup.find_all('div',{'class':'recordname'})
    try:
        link1 = records[0].a['href']
    except:
        link1 = "None"
    try:
        link2 = records[1].a['href']
    except:
        link2 = "None"
    try:
        link3 = records[2].a['href']
    except:
        link3 = "None"
    try:
        link4 = records[3].a['href']
    except:
        link4 = "None"
    try:
        link5 = records[4].a['href']
    except:
        link5 = "None"
    try:
        link6 = records[5].a['href']
    except:
        link6 = "None"
        
    df['link1'][i] = link1
    df['link2'][i] = link2
    df['link3'][i] = link3
    df['link4'][i] = link4
    df['link5'][i] = link5
    df['link6'][i] = link6

df.to_csv('/Users/storm/Desktop/Python/Manasota/nectron/nectron2.csv',index = None)



df2 = pandas.read_csv('/Users/storm/Desktop/Python/Manasota/nectron/nectron2.csv',index_col =0)

rows = df2.shape[0]
for j, row in df2.iterrows():
    #i = 0
    print(j)
    search = df2.loc[j,'link1']
    
    link_resp = sesh.get(search)
    html_soup = BeautifulSoup(link_resp.text, 'html.parser')

    header_el = html_soup.find_all('div',{'class':'Desc1Header'})
    title = header_el[0].h1.text.replace('\t','').replace('\r', '').replace('\n','').replace('/','')
    df2.loc[j,'Title'] = title
    
    image_link = html_soup.find('img',{'name':'mainImage'})['src']
    print(image_link)
    if 'http' not in image_link:
        image_link = 'https://www.nectron.com/'+image_link
    picture = sesh.get(image_link)

    file_name = '/Users/storm/Desktop/Python/Manasota/nectron/images/'+title+'.jpg'
    with open(file_name, 'wb') as file:
        file.write(picture.content)
        
    df2.loc[j,'ImageTitle'] = file_name.replace('/Users/storm/Desktop/Python/Manasota/nectron','')

    product_infos = html_soup.find_all('span',{'class':'productboxlist'})
    manupart = ""
    ourpart = ""
    manufacturer = ""
    PageYield = ""
    CostPerPage = ""
    condition = ""
    
    for i in range(0,len(product_infos)-1):
        #print(product_infos[i].text)
        product_text = product_infos[i].text.replace('\xa0','')
        #print(product_text)
        prod1 = product_infos[i+1].text.replace('\xa0','')
        
        if 'Manufacturer Part' in product_text:
            manupart = product_infos[i+1].text
            
        if 'OurPart' in product_text:
            ourpart = product_infos[i+1].text
            
        if 'Manufacturer:' in product_text:
            manufacturer = product_infos[i+1].text
            
        if 'Yield' in product_text:
            PageYield = product_infos[i+1].text
        
        if 'Cost Per Page' in product_text:
            CostPerPage = product_infos[i+1].text
            
        if 'Condition' in product_text:
            condition = product_infos[i+1].text

    print(manupart,ourpart,manufacturer,PageYield,CostPerPage,condition)
    #In Stock
    in_stock = html_soup.find('span',{'class':'productboxStock'}).text.replace('\t','').replace('\r', '').replace('\n','')
    
    df2.loc[j,'Stock'] = in_stock
    df2.loc[j,'ManufacturerPart'] = manupart
    df2.loc[j,'OurPart'] = ourpart
    df2.loc[j,'PageYield'] = PageYield
    df2.loc[j,'CostPerPage'] = CostPerPage
    df2.loc[j,'Condition'] = condition
    df2.loc[j,'OverviewPage'] = html_soup.find('div',{'id':'tabbed-nav2'}).text.replace('\t','').replace('\n','').replace('\r','')
    
df2.to_csv('/Users/storm/Desktop/Python/Manasota/nectron/nectronRET2.csv')


with open('/Users/storm/Desktop/Python/Manasota/nectron/nectron2.html', 'w') as the_file:
    the_file.write(link_resp.text)









