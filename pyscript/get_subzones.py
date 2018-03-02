##############################################################################
#                             Import Packages                                #
##############################################################################

import requests
import json
from bs4 import BeautifulSoup as bs
import pandas as pd


##############################################################################
#                                 Constants                                  #
##############################################################################

header = {"Accept": "application/json",
          "user-key": "eeb494140f638584b11b6717ba08a777"}
proxydic = {'http': 'https://118.97.18.65:8080',
          'https': 'http://118.97.18.65:8080'}


##############################################################################
#                                 Get Proxy                                  #
##############################################################################

with open('data/proxy_list.txt') as fil:
    proxies = fil.readlines()
proxies = proxies[0].split('\\n')[1:]
##############################################################################
#                              Get Subzones                                  #
##############################################################################
subzone_list = ["Airoli",
                "Ambarnath",
                "Ambivli",
                "Andheri",
                "Asangaon",
                "Atgaon",
                "Badlapur",
                "Baman Dongari",
                "Bandra",
                "Bhandup",
                "Bhayandar",
                "Bhivpuri Road",
                "Bhiwandi",
                "Boisar",
                "Borivali",
                "Byculla",
                "CBD Belapur",
                "Charni Road",
                "Chembur",
                "Chhatrapati Shivaji Maharaj Terminus",
                "Chinchpokli",
                "Chunabhatti",
                "Churchgate",
                "Cotton Green",
                "Currey Road",
                "Dadar Central & Dadar Western",
                "Dahanu Road",
                "Dahisar",
                "Dativali",
                "Diva",
                "Dockyard Road",
                "Dolavli",
                "Dombivli",
                "Prabhadevi",
                "Gavan",
                "Ghansoli",
                "Ghatkopar",
                "Goregaon",
                "Govandi",
                "Grant Road",
                "Guru Tegh Bahadur Nagar",
                "Jogeshwari",
                "Juchandra",
                "Juinagar",
                "Kalamboli",
                "Kalwa",
                "Kalyan",
                "Kaman Road",
                "Kandivali",
                "Kanjurmarg",
                "Karjat",
                "Kasara",
                "Kelavli",
                "Kelve Road",
                "Khadavli",
                "Khandeshwar",
                "Khar Road",
                "Kharbao",
                "Khardi",
                "Kharghar",
                "Kharkopar",
                "Khopoli",
                "King's Circle",
                "Kopar",
                "Kopar Khairane",
                "Kurla",
                "Lower Parel",
                "Lowjee",
                "Mahalaxmi",
                "Mahim",
                "Malad",
                "Mankhurd",
                "Mansarovar",
                "Marine Lines",
                "Masjid",
                "Matunga",
                "Matunga Road",
                "Mumbai Central",
                "Mira Road",
                "Mumbra",
                "Nahur",
                "Naigaon",
                "Nallasopara",
                "Neral",
                "Nerul",
                "Navade Road",
                "Nhava Sheva",
                "Nilaje",
                "Palghar",
                "Palasdari",
                "Panvel",
                "Parel",
                "Rabale",
                "Ram Mandir",
                "Ranjanpada",
                "Roha",
                "Sagar Sangam",
                "Sandhurst Road",
                "Sanpada",
                "Santacruz",
                "Seawoods–Darave",
                "Saphale",
                "Sion",
                "Shahad",
                "Shelu",
                "Thansit",
                "Targhar",
                "Thane",
                "Thakurli",
                "Titwala",
                "Turbhe",
                "Ulhasnagar",
                "Umbermali",
                "Umroli",
                "Uran City",
                "Wadala Road",
                "Vaitarna",
                "Vangaon",
                "Vangani",
                "Vasai Road",
                "Vashi",
                "Vasind",
                "Vidyavihar",
                "Vikhroli",
                "Vile Parle",
                "Virar",
                "Vithalwadi",
                "Mulund",
                "Tilak Nagar",
                "Sewri",
                "Taloja",
                "Pen",
                "Rasayani",
                "Nagothane",
                ]

for sz in subzone_list:
    url="https://developers.zomato.com/api/v2.1/locations?query="+sz
    resp = requests.get(url, headers=header)
    dat = json.loads(resp.text)
    for loc in dat['location_suggestions']:
        if loc['entity_type'] in ['zone','subzone']:
            print(loc['entity_id'],loc['entity_type'],loc['title'],loc['city_name'])
