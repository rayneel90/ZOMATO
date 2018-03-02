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
proxydic={'http':'https://118.97.18.65:8080',
          'https':'http://118.97.18.65:8080'}


##############################################################################
#                                 Get Proxy                                  #
##############################################################################

txt=requests.get('https://free-proxy-list.net/').text
df=pd.read_html(txt)[0]
df=df.dropna()
df.Port=df.Port.astype('int')
iplist=df['IP Address'].map(str)+':'+df['Port'].map(str)
iplist=iplist.tolist()
valid_iplist=[]
for ip in iplist:
    proxydic={'http':'https://'+ip,
          'https':'http://'+ip}
    url = "https://developers.zomato.com/api/v2.1/categories"
    try:
        resp = requests.get(url, headers=header,proxies=proxydic)
        if resp.status_code==200:
            valid_iplist.append('http://'+ip)
            print (ip)
    except:
        pass
        print ("No Luck")
    print(ip)
with open("data/proxy_list.txt",'w') as fil:
    fil.writelines(valid_iplist)
