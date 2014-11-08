__author__ = 'Ricky'

import urllib.request

import SALib

'''
def get_photo_links(index_url, player_title=["GM",]): #get URL links for all players where player in index_url
    soup = parseRatings.make_soup(index_url) # where url is the url we're passing into the original function
    tr = soup.find_all("tr", valign="CENTER") #players are all in <tr> tags with center valign
    #iterate through list to keep only those where the player's title is in player_title
    for tr_row in tr[:]: #iterate over a COPY of tr to allow removal without side effects
        title_check = str(tr_row.td.next_sibling.string).strip() #extract title from second column, strip off the \n
        print(tr_row.a)
        if title_check not in player_title :
            tr.remove(tr_row)

    players_links = [BASE_ARCHIVE_URL + tr_row.a["href"] for tr_row in tr] #build URL for all players from href in <a> tag

    return players_links
'''

def get_photo_links(index_url): #get filename for all players in index_url
    soup = SALib.make_soup(index_url) # where url is the url we're passing into the original function
    images = soup.find_all("img")
    photo_list = [str(photo["src"])[11:] for photo in images]
    return photo_list

index_url = ("http://www.toucanet.com/archives/p/bytitle.html")
#player_title = ["GM",] #warning: norm stars e.g. "*M" needs to be keyed in as a separate entry

BASE_PHOTO_URL = "http://www.toucanet.com/mugshot/m/"

photo_list = get_photo_links(index_url)

for photo_name in photo_list:
    urllib.request.urlretrieve(BASE_PHOTO_URL + photo_name , photo_name)