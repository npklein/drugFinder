import http.cookiejar
import urllib
import requests
import re
import shutil

import argparse
'''
Author: Niek de Klein
'''

# Parsing commandline
parser = argparse.ArgumentParser(description='Gets the results from the connectivity map for a given up.grp and down.grp file.', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument('-u','--up', help='up.grp file', required=True)                 # all_proteins_path
parser.add_argument('-d','--down', help='down.grp file', required=True)    # interest_protein_path
parser.add_argument('-n','--username', help='Connectivity map username', required=True)    # interest_protein_path
parser.add_argument('-p','--password', help='Connectivity map password', required=True)    # interest_protein_path
parser.add_argument('-o','--outfile_detailed', help='Outfile name', required=True)            # pfam_domains_file_path
parser.add_argument('-t','--outfile_permuted', help='Outfile name', required=True)            # pfam_domains_file_path
args = vars(parser.parse_args())

up_grp = args['up']
down_grp = args['down']
user = args['username']
pwd = args['password']
outfile_detailed = args['outfile_detailed']
outfile_permuted = args['outfile_permuted']

submit_signature_url = 'http://www.broadinstitute.org/cmap/newQuery?servletAction=querySetup&queryType=quick'
login_url = 'http://www.broadinstitute.org/cmap/j_security_check'

login_values = urllib.parse.urlencode({'j_username': user,
                                       'j_password': pwd, 
                                       'submit': 'sign in'})

payload_submit_signature = bytes(login_values, 'ascii')

cj = http.cookiejar.CookieJar()
opener = urllib.request.build_opener(
    urllib.request.HTTPRedirectHandler(),
    urllib.request.HTTPHandler(debuglevel=0),
    urllib.request.HTTPSHandler(debuglevel=0),
    urllib.request.HTTPCookieProcessor(cj))
print(submit_signature_url)
resp_1 = opener.open(submit_signature_url) #First call to capture the JSESSIONID
print(login_url)
resp = opener.open(login_url, payload_submit_signature)

values = {'ups':open(up_grp,'rb'),
          'dns':open(down_grp,'rb')}
submit_signature_url = 'http://www.broadinstitute.org/cmap/newQuery'
req = requests.post(submit_signature_url, files=values, cookies=cj, data={"servletAction": "quickQuery"})
try:
    result_ID = re.findall('permuted_result.jsp\?resultDescId=(\d+)',req.text)[0]
except:
    if re.match('no tags found in up tag file.', req.text):
        print('No tags found in up tag file, so no outfile written')
    if re.match('no tags found in up tag file.', req.text):
        print('No tags found in down tag file, so no outfile written')
result_url = 'http://www.broadinstitute.org/cmap/download_detailed.jsp?resultDescId='
# Download the file from `url` and save it locally under `file_name`:
with opener.open(result_url+result_ID) as response, open(outfile_detailed, 'wb') as out_file:
    shutil.copyfileobj(response, out_file)


result_url = 'http://www.broadinstitute.org/cmap/download_permuted.jsp?resultDescId='
# Download the file from `url` and save it locally under `file_name`:
with opener.open(result_url+result_ID) as response, open(outfile_permuted, 'wb') as out_file:
    shutil.copyfileobj(response, out_file)

print('worked')
