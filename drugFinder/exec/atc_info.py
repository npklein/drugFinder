import requests
import re
import html2text
import argparse
'''
Author: Niek de Klein
'''

# Parsing commandline
parser = argparse.ArgumentParser(description='Takes a file of ATC codes and returns the whocc info.', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument('-a','--atc', help='atc text file, enter separated', required=True)                 
parser.add_argument('-o','--outfile_full_name', help='Outfile path for file with full name', required=True)
parser.add_argument('-d','--outfile_description', help='Outfile path for file with description', required=True)
args = vars(parser.parse_args())

atc_file_path = args['atc']
atc_file_path = args['atc']
outfile_full_name = args['outfile_full_name']
outfile_description = args['outfile_description']

result = {}
for atc_code in open(atc_file_path).read().strip().split():
    submit_atc_code = 'http://www.whocc.no/atc_ddd_index/?code='+atc_code
    req = requests.get(submit_atc_code)
    plain_text = html2text.html2text(req.text)
    description = re.findall('([A-Z0-9]+) \*\*\[(.+?)\]',plain_text,re.DOTALL)#+ )\*\*\[(.*?)\]
    full_name = re.findall('\[(.+?)\]\(\./\?code='+atc_code+'.+?\&showdescription=yes\)', plain_text)
    result[atc_code] = [description, full_name]

out_full_name = open(outfile_full_name,'w')
out_full_name.write('atc_code\tfull_name\n')
out_description = open(outfile_description,'w')
out_description.write('atc_code\tpartial_code\tdescription\n')
for atc in result:
    for description in result[atc][0]:
        out_description.write(atc+'\t'+description[0].replace('\n','')+'\t'+description[1].replace('\n','')+'\n')
    for name in result[atc][1]:
        out_full_name.write(atc+'\t'+name.replace('\n','')+'\n')
out_description.close()
out_full_name.close()
