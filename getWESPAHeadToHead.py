__author__ = 'Ricky'

import csv

import WESPALib

index_url = "http://www.wespa.org/aardvark/index.shtml"

player_list = ("Hubert Wee", "Marlon Prudencio", "Tony Sim", "Wong Zhi Yuan", "Victor Gwee",
               "Victor Chua", "Ricky Purnomo",  "Michael Tang", "Yong Jian Rong", "Shim Yen Nee")

opps_list = ("Nigel Richards", "Aaron Chong", "Vannitha Balasingam", "Yeo Kian Hung", "William Kang",
             "Martin Teo", "Vinnith Ramamurti", "Jocelyn Lor", "Tan Jin Chor", "Alex Tan")

#player_url_list = WESPALib.get_players_links_by_name(index_url, player_names=player_list)

player_h2h = []
for player in player_list:
    player_h2h.extend(WESPALib.get_player_h2h(player, opps_list))

output_file = open('WESPA Head-to-Head.csv', 'w')
csvwriter = csv.DictWriter(output_file, delimiter=',', lineterminator='\n',
                           fieldnames=WESPALib.player_cols+WESPALib.h2h_cols+("Source", ))
csvwriter.writeheader()
#writeheader requires writing as w instead of wb; to compensate, lineterminator is forced to \n for Windows
for row in player_h2h:
    csvwriter.writerow(row)
output_file.close()