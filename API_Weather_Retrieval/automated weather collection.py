import urllib.request
import re
import time
import csv
import os
os.chdir("C:/Users/treneau/Documents")

str1 = 'http://api.wunderground.com/api/88e2f56333477b74/history_'
str2 = '/q/'
str3 = '.json'
count = 0

with (open("weather_fixed.csv",'rU')) as f:
    mycsv = csv.reader(f)
    mycsv = list(mycsv)
for rown in range(1, len(mycsv)):
    if(count > 999):
        print('limit reached')
        break
    if(mycsv[rown][3]=='[]'):
        coord = mycsv[rown][2]
        date = mycsv[rown][1]     
        strf = str1 + date[1:5] + date[6:8] + date[9:11] + str2 + coord + str3
        req = urllib.request.Request(strf)
        count = count + 1
        time.sleep(6)
        response = urllib.request.urlopen(req)
        testing = response.read()
        testing = str(testing)
        meantempi = re.findall('meantempi":"(\d+)',testing)
        maxtempi = re.findall('maxtempi":"(\d+)',testing)
        mintempi = re.findall('mintempi":"(\d+)',testing)
        precipi = re.findall('precipi":"(\d+.\d+)',testing)
        if(len(meantempi) >= 1):
           meantempi = re.findall('meantempi":"(\d+)',testing)[-1]
        if(len(maxtempi) >= 1):
            maxtempi = re.findall('maxtempi":"(\d+)',testing)[-1]
        if(len(mintempi) >= 1):
             mintempi = re.findall('mintempi":"(\d+)',testing)[-1]
        if(len(precipi) >= 1):
            precipi = re.findall('precipi":"(\d+.\d+)',testing)[-1]
        mycsv[rown][3] = meantempi
        mycsv[rown][4] = maxtempi
        mycsv[rown][5] = mintempi
        mycsv[rown][6] = precipi
        with open("weather_fixed.csv", 'w', newline = '') as f:
            writer = csv.writer(f, delimiter=',')
            writer.writerows(mycsv)
print('finished')
