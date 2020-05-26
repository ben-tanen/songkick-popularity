#################
# LOAD PACKAGES #
#################

import os, requests, json, math
import pandas as pd

###############
# GET API KEY #
###############

os.chdir("/Users/ben-tanen/Desktop/Projects/songkick-popularity/")

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
    
############################
# DEFINE PARSING FUNCTIONS #
############################

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

#######################
# LOOK FOR ARTIST IDS #
#######################

# pull in artist names without IDs
artists = pd.read_csv('data/artists.csv', encoding = 'latin1')

id_results = [ ]

for ix, row in artists.iterrows():
    artist = row['artist']
    artist_id = row['id']

    # skip artists with ids already
    if not math.isnan(artist_id):
        continue
    
    print(">>> pulling search results for '%s'" % artist)

    # query search results
    artist_results1 = search_artists(artist)
    artist_results2 = [{'name': r['displayName'], 'id': r['id']} for \
                       r in artist_results1['resultsPage']['results']['artist']]
    
    id_results.extend(artist_results2)

# stop until ids have been filled in
input('Press any key once artist IDs filled in...')

##############################
# PULL IN EVENTS FOR ARTISTS #
##############################

# pull in artists after filling in IDs
artists = pd.read_csv('data/artists.csv', encoding = 'latin1')

# loop through artists and pull in full list of events
all_events = [ ]

for ix, row in artists.iterrows():
    [artist, artist_id] = [row['artist'], row['id']]

    print(">>> pulling event results for %s (%d)" % (artist, ix))

    event_results = get_artist_events(artist_id)

    # parse out content for each event
    for e in event_results:
        all_events.append({
            'name': e['displayName'],
            'id': e['id'],
            'artist': artist,
            'billing': [a['billing'] for a in e['performance'] if a['artist']['id'] == artist_id][0],
            'popularity': e['popularity'],
            'date': e['start']['date'],
            'venue-id': e['venue']['id'],
            'venue-name': e['venue']['displayName']
        })

# convert to df and export
all_events_df = pd.DataFrame(all_events)
all_events_df.to_csv('data/concerts.csv', index = False)

#############################
# PULL IN VENUES FOR EVENTS #
#############################

# get unique list of venue ids
venue_ids = all_events_df['venue-id'].unique().tolist()

# loop through venue ids and pull in relevant information
all_venues = [ ]

for venue_id in venue_ids:
    try:
        venue_results = get_venue(venue_id)
        venue = venue_results['resultsPage']['results']['venue']
    except:
        print(">>> %s - invalid venue id" % venue_id)
        continue

    print(">>> pulling venue results for %s, %d/%d" % (venue['displayName'], venue_ids.index(venue_id), len(venue_ids)))

    all_venues.append({
        'name': venue['displayName'],
        'id': venue['id'],
        'metroArea': venue['metroArea']['displayName'],
        'lat': venue['lat'],
        'lng': venue['lng'],
        'capacity': venue['capacity']
    })

# convert to df and export
all_venues_df = pd.DataFrame(all_venues)
all_venues_df.to_csv('data/venues.csv', index = False)
