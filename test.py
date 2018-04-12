import requests
import pandas as pd
from pymongo import MongoClient
from bs4 import BeautifulSoup

conn = MongoClient()['mf']
from sqlalchemy import create_engine
engine = create_engine("mysql://neel:pass@123@localhost/mf")

dt_range = pd.date_range(start='04-01-2006', end=pd.datetime.today()).tolist()
while dt_range:
    dt = dt_range.pop(0).strftime('%d-%b-%Y')
    url_tmplt = 'http://portal.amfiindia.com/DownloadNAVHistoryReport_Po.aspx' \
                '?frmdt={0}&todt={1}'
    txt = requests.get(url_tmplt.format(dt, dt)).text
    dat = [i.strip() for i in txt.split('\n') if ';' in i]
    dat = [i.split(';') for i in dat]
    df = pd.DataFrame(dat)
    df.columns = [i.replace(' ', '_') for i in df.iloc[0]]
    df = df.drop(0)
    df.Date = pd.to_datetime(df.Date, format='%d-%b-%Y')
    conn['daily_data'].insert_many(df.to_dict(orient='record'))
    df.to_sql('daily_data_'+dt.split('-')[-1], engine, if_exists='append')
    print(dt)






txt = requests.get('http://fundpicker.thefundoo.com/FundCard/1916/Tata-Equity-PE--G-').text
soup = BeautifulSoup(txt,'lxml')
soup.find_all('tbody',{'id':'tbody_consist'})
tbody_consist