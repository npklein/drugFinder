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
username = args['username']
password = args['password']
outfile_detailed = args['outfile_detailed']
outfile_permuted = args['outfile_permuted']

login = 'https://portals.broadinstitute.org/cmap/j_security_check'
index = 'https://portals.broadinstitute.org/cmap/index.jsp'
data = { 'j_username':username, 'j_password':password, 'submit':'sign+in' }

with requests.Session() as session:
    session.get(index)    # you need this to get cookies #
    post = session.post(login, data=data)

    submit_signature_url = 'http://www.broadinstitute.org/cmap/newQuery?servletAction=querySetup&queryType=quick'

    # input data to submit
    values = {'ups':open(up_grp,'rb'),
              'dns':open(down_grp,'rb')}

    submit_signature_url = 'https://portals.broadinstitute.org/cmap/newQuery?servletAction=querySetup&queryType=quick'

    req = session.post(submit_signature_url, files=values, data={"servletAction": "quickQuery"})
    try:
        result_ID = re.findall('permuted_result.jsp\?resultDescId=(\d+)',req.text)[0]
    except:
        if re.match('no tags found in up tag file.', req.text):
            raise RuntimeError('No tags found in up tag file, so no outfile written')
        if re.match('no tags found in up tag file.', req.text):
            raise RuntimeError('No tags found in down tag file, so no outfile written')
        raise

    result_url = 'http://www.broadinstitute.org/cmap/download_detailed.jsp?resultDescId='+result_ID
    # Download the file from `url` and save it locally under `file_name`:
    response = session.get(result_url)
    if response.status_code != requests.codes.ok:
        print(response.content)
        raise RuntimeError('Response not OK')
    with open(outfile_detailed, 'wb') as out_file:
        out_file.write(response.content)
        print('file saved at',outfile_detailed)

    result_url = 'http://www.broadinstitute.org/cmap/download_permuted.jsp?resultDescId='+result_ID
    # Do same for permuted results
    response = session.get(result_url)
    if response.status_code != requests.codes.ok:
        print(response.content)
        raise RuntimeError('Response not OK')
    with open(outfile_permuted, 'wb') as out_file:
        out_file.write(response.content)
        print('file saved at',outfile_permuted)

print('worked')
