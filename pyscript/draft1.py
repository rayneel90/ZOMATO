##############################################################################
#                             Import Packages                                #
##############################################################################

import requests
import json
from pymongo import MongoClient
from time import sleep

##############################################################################
#                                 Constants                                  #
##############################################################################

header = {"Accept": "application/json",
          "user-key": "eeb494140f638584b11b6717ba08a777"}
proxydic = {'http': 'https://118.97.18.65:8080',
            'https': 'http://118.97.18.65:8080'}

conn = MongoClient()['zomato']
##############################################################################
#                                 Get Proxy                                  #
##############################################################################

with open('data/proxy_list.txt') as fil:
    proxies = fil.readlines()
proxies = proxies[0].split('\\n')[1:]
##############################################################################
#                              Get Categories                                #
##############################################################################

url = "https://developers.zomato.com/api/v2.1/categories"
resp = requests.get(url, headers=header)
dat = json.loads(resp.text)
categories = [i['categories'] for i in dat['categories']]


##############################################################################
#                              Get Collections                               #
##############################################################################

url = 'https://developers.zomato.com/api/v2.1/collections?city_id=3'
resp = requests.get(url, headers=header)
dat = json.loads(resp.text)
collections = [i['collection'] for i in dat['collections']]


##############################################################################
#                              Get Cuisines                                  #
##############################################################################

url = 'https://developers.zomato.com/api/v2.1/cuisines?city_id=3'
resp = requests.get(url, headers=header)
dat = json.loads(resp.text)
cuisines = [i['cuisine'] for i in dat['cuisines']]


##############################################################################
#                              Get Cuisines                                  #
##############################################################################

url = 'https://developers.zomato.com/api/v2.1/establishments?city_id=3'
resp = requests.get(url, headers=header)
dat = json.loads(resp.text)
establishments = [i['establishment'] for i in dat['establishments']]
