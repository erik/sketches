#!/usr/bin/env python3

from urllib.parse import parse_qsl
from urllib.request import urlretrieve
import json
import os
import os.path
import re
import sys

from bs4 import BeautifulSoup
import requests
import shapely.geometry


ROUTE_BASE_URL = 'http://www.thepassageride.com/Routes/'
OUTPUT_DIR = 'data/'


def fetch_text(url):
    r = requests.get(url)
    if r.status_code != 200:
        r.raise_for_status()
    return r.text


def scrape_route_list(html):
    print('Fetching route list...', end='')

    routes = []
    soup = BeautifulSoup(html, 'html.parser')

    for link in soup.select('#wikitext a[href*="/Routes/"]'):
        href = link.get('href')
        routes.append(
            {'name': link.text, 'number': int(href.strip(ROUTE_BASE_URL)), 'url': href,}
        )
    print('done (%d routes)' % len(routes))

    return routes


def fetch_route_description(route_url):
    print('\t%s' % route_url)

    html = fetch_text(route_url)
    soup = BeautifulSoup(html, 'html.parser')

    description = [p.prettify() for p in soup.select('#wikitext p')]
    map_url = soup.select_one('#wikitext a[href*="gmap-pedometer"]')
    if map_url is not None:
        map_url = map_url.get('href')

    # Media embedded in description
    media_urls = [
        i.get('src')
        for i in soup.select('img[src*="/wiki/uploads/"]')
        if 'thumbs' not in i.get('class')
    ]

    thumbs = [
        i.get('href')
        for i in soup.select('a.thumblink[href*="/wiki/uploads/"]')
    ]

    return {
        'map_url': map_url,
        'description': '\n'.join(description),
        'media': media_urls + thumbs,
        'thumbs': thumbs,
    }


def download_media(desc):
    out = {
        url: 'media/%s' % normalize_file_name(url)
        for url in desc['media']
    }

    # Download all the requested media
    for url, path in out.items():
        print('\t' + url)
        urlretrieve(url, filename=os.path.join(OUTPUT_DIR, path))
        desc['description'] = desc['description'].replace(url, path)

    desc['thumbs'] = [out[t] for t in desc['thumbs']]
    return desc


def fetch_route_map(map_url):
    print('\t%s' % map_url, end='')
    _, map_id = map_url.split('?r=')

    path = '/getRoute.php' if int(map_id) <= 5_000_000 else '/gp/ajaxRoute/get'
    r = requests.post('https://www.gmap-pedometer.com' + path, {'rId': map_id})
    if r.status_code != 200:
        r.raise_for_status()

    data = parse_qsl(r.text)
    polyline = [x[1] for x in data if x[0] == 'polyline'][0]

    coords = []
    points = [float(pt) for pt in polyline.split('a')]
    for i in range(0, len(points) - 1, 2):
        # lat, lon
        coords.append([points[i + 1], points[i]])

    return coords


def route_to_geojson(route_meta, coords):
    return {
        'type': 'Feature',
        'geometry': {'type': 'LineString', 'coordinates': coords},
        'properties': route_meta,
    }


def simplify_route(coords):
    # This seems to be a good trade off between accurately
    # representing the full route and limiting the data.
    tolerance = 0.001
    line = shapely.geometry.LineString(coords)
    return list(line.simplify(tolerance).coords)


def normalize_file_name(name):
    name = name.lower()
    name = re.sub('[^a-z0-9\.]', '_', name)
    return re.sub('_+', '_', name)


def to_file_name(route):
    name = '%03d_%s.geojson' % (route['number'], route['name'])
    return normalize_file_name(name)


def main():
    for d in ['routes', 'media']:
        os.makedirs(os.path.join(OUTPUT_DIR, d), exist_ok=True)

    html = fetch_text(ROUTE_BASE_URL)

    routes = []
    for r in scrape_route_list(html):
        print('#%d "%s"' % (r['number'], r['name']))

        desc = fetch_route_description(r['url'])
        desc = download_media(desc)

        coords = []
        if desc['map_url'] is not None:
            coords = fetch_route_map(desc['map_url'])

        path = os.path.join('routes', to_file_name(r))
        desc['geojson'] = path

        full_geo = route_to_geojson({**r, **desc}, coords)

        # Full resolution for the individual route file, low
        # resolution for the overview file.
        f = os.path.join(OUTPUT_DIR, path)
        with open(f, 'w') as fp:
            json.dump(full_geo, fp, indent=4)

        simple_coords = simplify_route(coords) if coords else []
        simple_geo = route_to_geojson({**r, **desc}, simple_coords)
        routes.append(simple_geo)

        print(
            ' ... done (%d coords, simplified to %d)'
            % (len(coords), len(simple_coords))
        )

    collection = {'type': 'FeatureCollection', 'features': routes}

    print('Dumping full resolution to file...')
    with open(os.path.join(OUTPUT_DIR, 'index.geojson'), 'w') as fp:
        json.dump(collection, fp, indent=4)
    print('All done!')


if __name__ == '__main__':
    main()
