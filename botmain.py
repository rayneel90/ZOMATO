import win32com.client
from time import sleep
import pandas as pd
import pickle

df = pd.read_csv('data/master.csv',encoding='ANSI')
df = df[df['COURIER NAME']=='Shree Maruti']

shell = win32com.client.Dispatch("WScript.Shell")
shell.Run('chrome.exe')
shell.AppActivate('chrome')
sleep(10)

shell.SendKeys("^(L)")
shell.SendKeys("http://www.shreemaruticourier.com/track-your-shipment/")
sleep(1)
shell.SendKeys("{ENTER}")
sleep(10)
lst = []
for rec in df.to_dict(orient='record'):
    shell.SendKeys(rec['AWB NO'])
    shell.SendKeys('{ENTER}')
    sleep(5)
    shell.SendKeys("^(s)")
    sleep(1)
    shell.SendKeys('C:\\Users\\nilabja21607\\Desktop\\temp\\htmls\\temp.html')
    sleep(1)
    shell.SendKeys("{ENTER}")
    sleep(3)
    shell.SendKeys("{LEFT}")
    shell.SendKeys("{ENTER}")
    sleep(7)
    with open('htmls/temp.html','r') as fil:
        txt = fil.read()
    dat = pd.read_html(txt)[0]
    dat.columns = dat.iloc[0]
    dat = dat.to_dict(orient='record')[-1]
    dat.update(rec)
    lst.append(dat)
    with open('data/out.pkl','wb') as fil:
        pickle.dump(lst,fil)
    sleep(10)

    with open('data/out.pkl','rb') as fil:
        lst = pickle.load(fil)
