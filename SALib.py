__author__ = 'Ricky'

#library of functions to scrape from Scrabble Associations at toucanet.com

from urllib import request
from time import sleep
import csv


from collections import OrderedDict
from bs4 import BeautifulSoup

BASE_URL = "http://www.toucanet.com/"
year_cols = ("Date", "Tournament", "Division", "TotalDivs", "Placement", "Players", "Wins", "Margin",
            "GamesFirst", "GamesLast", "Rating", "Norm")
player_cols = ("Name",)  # comma important to ensure single-element tuple is saved as tuple, not string
rating_cols = player_cols + year_cols
h2h_cols = ("Opp", "Tournament", "Date", "Round", "ScoreFor", "ScoreAgainst", "Difference", "Win", "Loss", "Margin")

def make_soup(urlname):  #create BeautifulSoup object
    html = request.urlopen(urlname).read()
    return BeautifulSoup(html, "lxml")

def get_years_URL(player_url):  #get URL links for all the years where player in player_url is active
    soup = make_soup(player_url)  # where url is the url we're passing into the original function
    p_links = soup.find("p", id="links")  #the id of the section with all the years URL
    years_URL = [player_url + ay["href"] for ay in p_links.findAll("a")]
    years_URL.pop(0)   #remove first link, which was an Up button
    return years_URL

def get_opp_URL(player_url, opps_list):  #get URL links for all the opponents named in opp_list
    ### TO-DO: empty opp_list returns all available URL as opponent, instead of error ###
    soup = make_soup(player_url)
    opps_URL =[]
    for opp_name in opps_list:
        a_URL = soup.find("a", text=opp_name)
        if a_URL:  # add only if opponent name is found
            opps_URL.append(player_url + a_URL["href"])

    return opps_URL

def get_player_name(player_url):  #get player name of player in player_url is active
    soup = make_soup(player_url)  # where url is the url we're passing into the original function
    player_name = soup.b.string  #first bold element in page
    return [player_name, ]   #return as a single-element list

def get_year_ratings(year_url):  #returns every tournament records of a player in a year, as listed in year_url
    soup = make_soup(year_url)  # where url is the location we're passing into the original function
    table_contents = soup.find_all("table")[2]  #third table tag is the one containing post-tourney ratings
    tourney_header = table_contents.tr.extract()  #this removes header from the main table_contents
    tourneys = []
    for tr in table_contents.findAll("tr"):
        td = tr.find("span", "ratingcarry")  # iterate through td and check for span with class "ratingcarry"
        if not td:  # if not ratingcarry
            all_td = tr.find_all("td")
            line_read = [str(td.a.string).strip() if td.a
                         else str(td.span.string).strip() if td.span
                         else str(td.string).strip() for td in all_td]  # if there is <a> or <span> tag in td, get its string instead
            tourneys.append(line_read)
    print(year_url + " done")
    return tourneys

def get_player_records(player_url):  # returns all records of a player in player_url across all years
    player_name = get_player_name(player_url)
    years = get_years_URL(player_url)
    ratings = []  # a list to store the tournament results
    for year in years:
        ratings.extend(get_year_ratings(year))
        sleep(1)  # be nice

    output_rows = []  # to merge name with tournament results as list of dictionaries
    for entries in ratings:
        output_rows.append(OrderedDict(zip(rating_cols, player_name + entries)))

    return output_rows

def get_players_links(index_url):  # get URL links in index_url
    soup = make_soup(index_url)  # where url is the url we're passing into the original function
    return soup.find_all("tr", valign="CENTER")  #players are all in <tr> tags with center valign

def get_players_links_by_title(index_url, player_title): #get URL links for all players where player in index_url
    tr = get_players_links(index_url)
    #iterate through list to keep only those where the player's title is in player_title
    for tr_row in tr[:]:  # iterate over a COPY of tr to allow removal without side effects
        title_check = str(tr_row.td.next_sibling.string).strip()  # extract title from second column, strip off the \n
        #print(tr_row.a)  #just for debugging
        if title_check not in player_title:
            tr.remove(tr_row)

    BASE_ARCHIVE_URL = "http://www.toucanet.com/archives/p/"
    players_links = [BASE_ARCHIVE_URL + tr_row.a["href"] for tr_row in tr]  # build all players URL from href in <a> tag

    return players_links

def get_players_links_by_name(index_url, player_names):  # get URL links for all players where player in index_url
    tr = get_players_links(index_url)
    # iterate through list to keep only those where the player's name is in player_names
    for tr_row in tr[:]:  # iterate over a COPY of tr to allow removal without side effects
        name_check = str(tr_row.a.string).strip()  # extract name from <a> tag, strip off the \n
        #print(tr_row.a)  # just for debugging
        if name_check not in player_names :
            tr.remove(tr_row)

    BASE_ARCHIVE_URL = "http://www.toucanet.com/archives/p/"
    players_links = [BASE_ARCHIVE_URL + tr_row.a["href"] for tr_row in tr]  # build URL for all players from href in <a> tag

    return players_links

def get_opp_records(opp_URL):  # returns all match records in the head-to-head page in given URL
    soup = make_soup(opp_URL)
    opp_name = soup.find_all("b")[1].string  # second bold element in page
    table_contents = soup.find_all("table")[1]  # second table tag is the one containing post-tourney ratings
    header = table_contents.tr.extract()  # this removes header from the main table_contents
    opp_records = []
    for tr in table_contents.findAll("tr"):
        all_td = tr.find_all("td")
        line_read = [str(td.a.string).strip() if td.a
                     else str(td.span.string).strip() if td.span
                     else str(td.string).strip() for td in all_td]  # if there is <a> or <span> tag in td, get its string instead
        opp_records.append([opp_name, ] + line_read)

    print(opp_URL + " done")

    return opp_records

def get_player_h2h(player_url, opps_list):
    player_name = get_player_name(player_url)
    opps_URL = get_opp_URL(player_url, opps_list)

    h2h = []
    for opp_URL in opps_URL:
        opp_records = get_opp_records(opp_URL)
        h2h.extend(opp_records)

    output_rows = []   # to merge name with tournament results as list of dictionaries
    for records in h2h:
        output_rows.append(OrderedDict(zip(player_cols + h2h_cols + ("Source", ),
                                           player_name + records + ["SA", ])))

    return output_rows

def calc_win_prob(rating1, rating2):
    return .5 + 1/12*(rating1-rating2)

if __name__ == '__main__':
    player_url = ("http://www.toucanet.com/archives/p/rpur/")  #To-Do: function to populate multi URL from index page

    output_rows = get_player_records(player_url)

    test_file = open('rating.csv','w')
    csvwriter = csv.DictWriter(test_file, delimiter=',', lineterminator='\n',fieldnames=rating_cols)
    csvwriter.writeheader()
    # writeheader requires writing as w instead of wb; to compensate, lineterminator is forced to \n for Windows
    for row in output_rows:
        csvwriter.writerow(row)
    test_file.close()