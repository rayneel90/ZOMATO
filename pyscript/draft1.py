##############################################################################
#                             Import Packages                                #
##############################################################################

import requests
import json
from pymongo import MongoClient


##############################################################################
#                                 Constants                                  #
##############################################################################

header = {"Accept": "application/json",
          "user-key": "eeb494140f638584b11b6717ba08a777"}
proxydic={'http':'https://118.97.18.65:8080',
          'https':'http://118.97.18.65:8080'}

conn=MongoClient()['zomato']
##############################################################################
#                                 Get Proxy                                  #
##############################################################################

with open('data/proxy_list.txt') as fil:
    proxies=fil.readlines()
proxies = proxies[0].split('\\n')[1:]
##############################################################################
#                              Get Categories                                #
##############################################################################

url = "https://developers.zomato.com/api/v2.1/categories"
resp = requests.get(url,headers=header)
dat = json.loads(resp.text)
categories = [i['categories'] for i in dat['categories']]


##############################################################################
#                              Get Collections                               #
##############################################################################

url = 'https://developers.zomato.com/api/v2.1/collections?city_id=3'
resp = requests.get(url,headers=header)
dat = json.loads(resp.text)
collections = [i['collection'] for i in dat['collections']]


##############################################################################
#                              Get Cuisines                                  #
##############################################################################

url='https://developers.zomato.com/api/v2.1/cuisines?city_id=3'
resp = requests.get(url,headers=header)
dat = json.loads(resp.text)
cuisines = [i['cuisine'] for i in dat['cuisines']]


##############################################################################
#                              Get Cuisines                                  #
##############################################################################

url='https://developers.zomato.com/api/v2.1/establishments?city_id=3'
resp = requests.get(url,headers=header)
dat = json.loads(resp.text)
establishments = [i['establishment'] for i in dat['establishments']]

##############################################################################
#                              Get Cuisines                                  #
##############################################################################

url='https://developers.zomato.com/api/v2.1/search?entity_id=3&entity_type=\
city&cuisines={0}&establishment_type={1}&category={2}&start={3}&sort=rating\
&order=asc'

for cuisine in cuisines:
    for establishment in establishments:
        for category in categories:
            start = 0
            while(1):
            resp = requests.get(url.format(cuisine['cuisine_id'],
                                           establishment['id'],
                                           category['id'],
                                           start,
                                           ),headers=header)
            dat = json.loads(resp.text)
            data_to_insert=[res['restaurant'] for res in dat['restaurant']]
            conn['restaurants'].insert_many(data_to_insert)
            if dat['results_shown'] + dat['results_start'] \
                    >= min(100,dat['results_found']):
                break
            start = start + 20

