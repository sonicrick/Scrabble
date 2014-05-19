__author__ = 'Ricky'
import parseRatings
import csv

def get_players_links(index_url, player_title=["GM",]): #get URL links for all players where player in index_url
    soup = parseRatings.make_soup(index_url) # where url is the url we're passing into the original function
    tr = soup.find_all("tr", valign="CENTER") #players are all in <tr> tags with center valign
    #iterate through list to keep only those where the player's title is in player_title
    for tr_row in tr[:]: #iterate over a COPY of tr to allow removal without side effects
        title_check = str(tr_row.td.next_sibling.string).strip() #extract title from second column, strip off the \n
        print(tr_row.a)
        if title_check not in player_title :
            tr.remove(tr_row)

    BASE_ARCHIVE_URL = "http://www.toucanet.com/archives/p/"
    players_links = [BASE_ARCHIVE_URL + tr_row.a["href"] for tr_row in tr] #build URL for all players from href in <a> tag

    return players_links

index_url = ("http://www.toucanet.com/archives/p/bytitle.html")
player_title = ["GM",] #warning: norm stars e.g. "*M" needs to be keyed in as a separate entry

player_url_list = get_players_links(index_url, player_title)

player_records = []
for player_url in player_url_list:
    player_records.extend(parseRatings.get_player_records(player_url))

output_file = open('Player records by title.csv','w')
csvwriter = csv.DictWriter(output_file, delimiter=',', lineterminator='\n',fieldnames=parseRatings.columns)
csvwriter.writeheader()
#writeheader requires writing as w instead of wb; to compensate, lineterminator is forced to \n for Windows
for row in player_records:
    csvwriter.writerow(row)
output_file.close()