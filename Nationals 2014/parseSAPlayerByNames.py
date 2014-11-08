__author__ = 'Ricky'

import csv

import SALib


### WORK IN PROGRESS: Adapt player list from getSAHeadToHead.py

index_url = ("http://www.toucanet.com/archives/p/bytitle.html")
player_title = ["GM",]  # warning: norm stars e.g. "*M" needs to be keyed in as a separate entry

player_url_list = SALib.get_players_links_by_title(index_url, player_title)

player_records = []
for player_url in player_url_list:
    player_records.extend(SALib.get_player_records(player_url))

output_file = open('Player records by title.csv','w')
csvwriter = csv.DictWriter(output_file, delimiter=',', lineterminator='\n',fieldnames=SALib.rating_cols)
csvwriter.writeheader()
#writeheader requires writing as w instead of wb; to compensate, lineterminator is forced to \n for Windows
for row in player_records:
    csvwriter.writerow(row)
output_file.close()