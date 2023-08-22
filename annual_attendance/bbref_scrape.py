# import selenium webdriver for website control and pandas for data manipulation
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options

import pandas as pd

# use Chrome to access the web
options = Options()
options.add_argument("start-maximized")
options.add_argument("--ignore-certificate-errors")
options.add_argument("--allow-running-insecure-content")
options.add_experimental_option('excludeSwitches', ['enable-logging'])
driver = webdriver.Chrome(options = options)

# scrape data from 1998 to 2023 since number of teams (30) has been constant over that period
for i in range(1998, 2024):
    # build the url containing attendance data based on current year of for loop
    url_yr = 'https://www.baseball-reference.com/leagues/MLB/' + str(i) + '-misc.shtml'
    # open the website
    driver.get(url_yr)

    # get the HTML code of the table containing attendance data
    pvtable = driver.find_element(By.XPATH, '//*[@id="div_teams_miscellaneous"]').get_attribute('outerHTML')
    
    # use pandas to returns a list of data frame objects from the table
    df = pd.read_html(pvtable)
    # set df to the first object returned (the desired attendance data table)
    df = df[0]

    # delete unwanted columns; if column doesn't exist then use 'except' to pass to avoid error
    cols_del = ['Chall', 'Succ', 'Succ%', 'Managers']
    for col in cols_del:
        try:
            del df[col]
        except:
            pass

    # build the file name based on current year of for loop
    file_yr = 'attend_' + str(i) + '.csv'
    # output the table to a csv file for each year
    df.to_csv(file_yr)

# close Chrome
driver.quit()