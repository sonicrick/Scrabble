__author__ = 'Ricky'
'''
WORK IN PRORESS
'''

import csv

year_columns = ("Date", "Tournament", "Division", "TotalDivs", "Placement", "Players", "Wins", "Margin",
               "GamesFirst", "GamesLast", "Rating", "Norm")
player_columns = ("Name",) # comma important to ensure single-element tuple is saved as tuple, not string
columns = player_columns + year_columns

if __name__ == '__main__':
    input_file = open('rating.csv','r')
    output_file = open('cleaned.csv', 'w')
    csvreader = csv.DictReader(input_file, delimiter=',')
    ratings_data = []
    for row in csvreader:
        ratings_data.append(row)
    input_file.close()

    csvwriter = csv.DictWriter(output_file, delimiter=',', lineterminator='\n',fieldnames=columns)
    csvwriter.writeheader()
    #writeheader requires writing as w instead of wb; to compensate, lineterminator is forced to \n for Windows
#    for row in output_rows:
#        csvwriter.writerow(row)
#    output_file.close()