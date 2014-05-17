__author__ = 'Ricky'
from bs4 import BeautifulSoup
from urllib import request
from collections import OrderedDict
from time import sleep
import csv

BASE_URL = "http://www.toucanet.com/"
year_columns = ("Date", "Tournament", "Division", "TotalDivs", "Placement", "Players", "Wins", "Margin",
               "GamesFirst", "GamesLast", "Rating", "Norm")
player_columns = ("Name",) # comma important to ensure single-element tuple is saved as tuple, not string
columns = player_columns + year_columns

def make_soup(urlname):
    html = request.urlopen(urlname).read()
    return BeautifulSoup(html, "lxml")

def get_years_links(player_url):
    soup = make_soup(player_url) # where url is the url we're passing into the original function
    p_links = soup.find("p", id="links") #the id of the section with all the years URL
    years_links = [player_url + ay["href"] for ay in p_links.findAll("a")]
    years_links.pop(0)   #remove first link, which was an Up button
    return years_links

def get_year_ratings(year_url):
    soup = make_soup(year_url) # where url is the location we're passing into the original function
    table_contents = soup.find_all("table")[2]  #third table tag is the one containing post-tourney ratings
    tourney_header = table_contents.tr.extract()  #this removes header from the main table_contents
    tourneys = []
    for tr in table_contents.findAll("tr"):
        td = tr.find("span", "ratingcarry")#iterate through td and check for span with class "ratingcarry"
        if not td: #if not ratingcarry
            all_td = tr.find_all("td")
            line_read = [str(td.a.string).strip() if td.a
                         else str(td.span.string).strip() if td.span
                         else str(td.string).strip() for td in all_td] # if there is <a> or <span> tag in td, get its string instead
            #tourneys.append(OrderedDict(zip(year_columns,line_read)))
            tourneys.append(line_read)
    print(year_url + " done")
    return tourneys

if __name__ == '__main__':
    player_url = ("http://www.toucanet.com/archives/p/rpur/") #To-Do: function to populate multi URL from index page
    player_name = ["Ricky Purnomo",] #To-Do: extract names from index page (not individual URL)

    years = get_years_links(player_url)

    ratings = [] # a list to store the tournament results
    for year in years:
        year_ratings = get_year_ratings(year)
        ratings.extend(year_ratings)
        sleep(1) # be nice

    output_rows = [] #to merge name with tournament results as list of dictionaries
    for entries in ratings:
        output_rows.append(OrderedDict(zip(columns, player_name + entries)))

    test_file = open('rating.csv','w')
    csvwriter = csv.DictWriter(test_file, delimiter=',', lineterminator='\n',fieldnames=columns)
    csvwriter.writeheader()
    #writeheader requires writing as w instead of wb; to compensate, lineterminator is forced to \n for Windows
    for row in output_rows:
        csvwriter.writerow(row)
    test_file.close()