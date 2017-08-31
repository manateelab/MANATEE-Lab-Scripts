import urllib.request
import re
import time
import csv
import os
os.chdir("C:/Users/treneau/Dropbox/WCMCseed/Analysis/GPX_AnalysisReneau/workspace")
    key = 'insert_wunderground_api_key_here'
    dateList = []
    coordList = []       
    str1 = 'http://api.wunderground.com/api/'
    str2 = '/history_'
    str3 = '/q/'
    str4 = '.json'
    meantempiList = []
    maxtempiList = []
    mintempiList = [] 
    precipiList = []
    for a in range(0, len(dateList)):
        strf = str1 + key + str2 + dateList[a][3:7] + dateList[a][8:10] + dateList[a][11:13] + str3 + coordList[a][3:-2] + str4
        req = urllib.request.Request(strf)
        #time.sleep(6)
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
        meantempiList.append(meantempi)
        maxtempiList.append(maxtempi)
        mintempiList.append(mintempi)
        precipiList.append(precipi)
    outputstr = 'outputfile.csv'
        with open(outputstr, 'w') as csvoutput:
            writer = csv.writer(csvoutput, lineterminator='\n')
            for b in range(0, len(dateList)):
                    writer.writerow(row+[dateList[b]]+[coordList[b]]+[meantempiList[b]]+[maxtempiList[b]]+[mintempiList[b]]+[precipiList[b]])     
