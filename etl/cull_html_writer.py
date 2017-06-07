# Use Python 3
# Example args:
# 'First Cull Images 2017-06-07.csv' 'First_Cull_Images.hmlt'
from sys import argv

script, source_data, html_file = argv

html_out = open(html_file, 'w')

html_out.write('<!DOCTYPE html><html>')

import csv
with open(source_data) as c1:
	header, *data = csv.reader(c1)
	for row in data:
		attachment_width = int(row[1])
		attachment_height = int(row[2])
		html_out.write('<img src="{0}" height={1} width={2} </img>'.format(
			row[3], 300, 300 * (attachment_width / attachment_height)
			)
		)

html_out.write('<html>')
