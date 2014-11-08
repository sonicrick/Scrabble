__author__ = 'Ricky'

#library of functions to scrape from Scrabble Associations at toucanet.com


from collections import OrderedDict
from urllib import request
import urllib.parse

from bs4 import BeautifulSoup


BASE_url = "http://www.wespa.org/"
year_cols = ("Date", "Tournament", "Division", "TotalDivs", "Placement", "Players", "Wins", "Margin",
             "GamesFirst", "GamesLast", "Rating", "Norm")  # TO-DO: this is SA value, change to WESPA
player_cols = ("Name",)  # comma important to ensure single-element tuple is saved as tuple, not string
rating_cols = player_cols + year_cols
h2h_cols = ("Opp", "Tournament", "ScoreFor", "ScoreAgainst")

def make_soup(urlname):  # create BeautifulSoup object
    html = request.urlopen(urlname).read()
    return BeautifulSoup(html, "lxml")

"""
# TODO: adapt from SA
def get_years_url(player_url): #get url links for all the years where player in player_url is active
    soup = make_soup(player_url) # where url is the url we're passing into the original function
    p_links = soup.find("p", id="links") #the id of the section with all the years url
    years_url = [player_url + ay["href"] for ay in p_links.findAll("a")]
    years_url.pop(0)   #remove first link, which was an Up button
    return years_url
"""

"""
# TODO: adapt from SA
def get_opp_url(player_url, opps_list): #get url links for all the opponents named in opp_list
    ### TO-DO: empty opp_list returns all available url as opponent, instead of error ###
    soup = make_soup(player_url)
    opps_url =[]
    for opp_name in opps_list:
        a_url = soup.find("a", text=opp_name)
        if a_url:  # add only if opponent name is found
            opps_url.append(player_url + a_url["href"])

    return opps_url
"""

"""
# TODO: adapt from SA
def get_player_name(player_url): #get player name of player in player_url is active
    soup = make_soup(player_url) # where url is the url we're passing into the original function
    player_name = soup.b.string #first bold element in page
    return [player_name, ]  #return as a single-element list
"""

"""
# TODO: adapt from SA
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
"""

"""
# TODO: adapt from SA
def get_player_records(player_url): #returns all records of a player in player_url across all years
    player_name = get_player_name(player_url)
    years = get_years_url(player_url)
    ratings = []  # a list to store the tournament results
    for year in years:
        ratings.extend(get_year_ratings(year))
        sleep(1)  # be nice

    output_rows = []  # to merge name with tournament results as list of dictionaries
    for entries in ratings:
        output_rows.append(OrderedDict(zip(rating_cols, player_name + entries)))

    return output_rows
"""

"""
# TODO: adapt from SA
def get_players_links(index_url): #get url links in index_url
    soup = make_soup(index_url) # where url is the url we're passing into the original function
    return soup.find_all("tr", valign="CENTER") #players are all in <tr> tags with center valign
"""

"""
# TODO: adapt from SA
def get_players_links_by_title(index_url, player_title): #get url links for all players where player in index_url
    tr = get_players_links(index_url)
    #iterate through list to keep only those where the player's title is in player_title
    for tr_row in tr[:]:  # iterate over a COPY of tr to allow removal without side effects
        title_check = str(tr_row.td.next_sibling.string).strip()  # extract title from second column, strip off the \n
        #print(tr_row.a) #just for debugging
        if title_check not in player_title :
            tr.remove(tr_row)

    BASE_ARCHIVE_url = "http://www.toucanet.com/archives/p/"
    players_links = [BASE_ARCHIVE_url + tr_row.a["href"] for tr_row in tr]  # build url for all players from href in <a> tag

    return players_links
"""

"""
# TODO: adapt from SA
def get_players_links_by_name(index_url, player_names):  # get url links for all players where player in index_url
    tr = get_players_links(index_url)
    # iterate through list to keep only those where the player's name is in player_names
    for tr_row in tr[:]:  # iterate over a COPY of tr to allow removal without side effects
        name_check = str(tr_row.a.string).strip()  # extract name from <a> tag, strip off the \n
        #print(tr_row.a)  # just for debugging
        if name_check not in player_names :
            tr.remove(tr_row)

    BASE_ARCHIVE_url = "http://www.toucanet.com/archives/p/"
    players_links = [BASE_ARCHIVE_url + tr_row.a["href"] for tr_row in tr]  # build url for all players from href in <a> tag

    return players_links
"""


def get_opp_records(search_url, opp):  # returns all match records in the head-to-head page in given url
    opp_url = search_url + "&name2=" + opp
    opp_url = urllib.parse.quote(opp_url, safe=':/=?&')
    soup = make_soup(opp_url)
    table_contents = soup.find("table", class_="playerresults")
    #header = table_contents.tr.extract()  # this removes header from the main table_contents
    opp_records = []

    def tag_filter(tag):
        check = tag['class'][0] in ("win", "loss", "draw")
        return check

    for tr in table_contents.findAll("tr"):
        if tag_filter(tr):
            all_td = tr.find_all("td")
            line_read = [str(td.a.string).strip() for td in all_td]
            opp_records.append([opp, ] + line_read)

    print(opp_url + " done")

    return opp_records


def get_player_h2h(player, opps_list):
    # TO-DO: check if going through search will give right url for wrongly spelled name?
    search_url = "http://www.wespa.org/aardvark/cgi-bin/headmini.cgi?name1=" + player
    h2h = []
    for opp in opps_list:
        opp_records = get_opp_records(search_url, opp)
        h2h.extend(opp_records)

    output_rows = []   # to merge name with tournament results as list of dictionaries
    for records in h2h:
        output_rows.append(OrderedDict(zip(player_cols + h2h_cols + ("Source", ),
                                           [player, ] + records + ["WESPA", ])))

    return output_rows


"""
# TODO: adapt from SA
def calc_win_prob(rating1, rating2):
    return .5 + 1/12*(rating1-rating2)
"""

"""
# TODO: adapt from SA
if __name__ == '__main__':
    player_url = ("http://www.toucanet.com/archives/p/rpur/") #To-Do: function to populate multi url from index page

    output_rows = get_player_records(player_url)

    test_file = open('rating.csv','w')
    csvwriter = csv.DictWriter(test_file, delimiter=',', lineterminator='\n',fieldnames=rating_cols)
    csvwriter.writeheader()
    # writeheader requires writing as w instead of wb; to compensate, lineterminator is forced to \n for Windows
    for row in output_rows:
        csvwriter.writerow(row)
    test_file.close()
"""