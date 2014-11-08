__author__ = 'Ricky'

import csv

import SALib

index_url = "http://www.toucanet.com/archives/p/"

#player_list = ("Gwee, Victor")
player_list = ("Wee Ming Hui, Hubert", "Prudencio, Marlon", "Sim, Tony", "Wong Zhi Yuan", "Gwee, Victor",
               "Chua Kim Loong, Victor", "Purnomo, Ricky", "Tang, Michael", "Yong Jian Rong", "Shim Yen Nee")

opps_list = ("Nigel Richards", "Aaron Chong", "Vannitha Bala", "Yeo Kian Hung Henry", "William Kang",
             "Martin Teo", "Jocelyn Lor", "Tan Jin Chor", "Alex Tan")

player_url_list = SALib.get_players_links_by_name(index_url, player_names=player_list)

player_h2h = []
for player_url in player_url_list:
    player_h2h.extend(SALib.get_player_h2h(player_url, opps_list))

output_file = open('Head-to-Head.csv','w')
csvwriter = csv.DictWriter(output_file, delimiter=',', lineterminator='\n',
                           fieldnames=SALib.player_cols+SALib.h2h_cols+("Source", ))
csvwriter.writeheader()
#writeheader requires writing as w instead of wb; to compensate, lineterminator is forced to \n for Windows
for row in player_h2h:
    csvwriter.writerow(row)
output_file.close()