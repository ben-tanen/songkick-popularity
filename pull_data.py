#################
# LOAD PACKAGES #
#################

import requests, re, json
from datetime import datetime
from bs4 import BeautifulSoup

###############
# GET API KEY #
###############

api_keys = json.load(open("data/api-keys.json"))
api_key = api_keys['songkick-api-key']

###########################
# DEFINE HELPER FUNCTIONS #
###########################

def status_message(message, loud):
    if loud:
        print(message)

def valid_key(key, obj):
    return key in obj.keys()

def get_api_data(url):
    response = requests.get(url)
    if response.status_code == 200:
        return response.json()

def search_artists(query):
    url = "https://api.songkick.com/api/3.0/search/artists.json?apikey=%s&query=%s" % (api_key, query)
    return get_api_data(url)

def get_artist_events(artist_id):
    # get first page of artist events
    page = 1
    url = "https://api.songkick.com/api/3.0/artists/%s/gigography.json?apikey=%s" % (artist_id, api_key)
    results = get_api_data(url)['resultsPage']
    events = results['results']['event']

    # loop until reaching final page of results
    while results['perPage'] * results['page'] < results['totalEntries']:
        page += 1
        results = get_api_data(url + "&page=%d" % page)['resultsPage']
        events += results['results']['event']

    return events

def get_venue(venue_id):
    url = "https://api.songkick.com/api/3.0/venues/%d.json?apikey=%s" % (venue_id, api_key)
    return get_api_data(url)
    
############################
# DEFINE PARSING FUNCTIONS #
############################

