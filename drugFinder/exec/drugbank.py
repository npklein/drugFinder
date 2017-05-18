import requests
import re
import argparse
from bs4 import BeautifulSoup, SoupStrainer


'''
Author: Niek de Klein
'''

# Parsing commandline
parser = argparse.ArgumentParser(description='Takes a file of drug names and returns the drugbank accession code.', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument('-i','--input', help='drugnames text file, enter separated', required=True)                 
parser.add_argument('-o','--outfile', help='Outfile path', required=True)
args = vars(parser.parse_args())

input_file_path = args['input']
output_file_path = args['outfile']



result = {}
for drug_name in open(input_file_path).read().strip().split():
    submit_drug_name = 'http://www.drugbank.ca/search?query='+drug_name+'&search_type=drugs'
    req = requests.get(submit_drug_name)

    for link in BeautifulSoup(req.text, parse_only=SoupStrainer('a')):
        if link.has_attr('href'):
            if re.search('>'+drug_name.upper().replace(' ','.').replace('-','.')+'<', str(link).upper().replace(' ','.').replace('-','.')):
                result[drug_name] = link['href'].split('/')[-1]
            
outfile = open(output_file_path,'w')
outfile.write('drug_name\tdrugbank_code\n')
for drug_name in result:
    outfile.write(drug_name+'\t'+result[drug_name]+'\n')
    
outfile.close()
