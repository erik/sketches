import json
import os.path
import sys
import time

import requests


OFFICES_PATH = './data/offices.json'
OUTPUT_PATH = './data/wait_time.json'

WAIT_DATA_URL = 'https://www.dmv.ca.gov/wasapp/webdata/output3.txt'


def load_offices():
    with open(OFFICES_PATH, 'r') as f:
        return json.load(f)


def load_wait_time(offices):
    r = requests.get(WAIT_DATA_URL)

    if r.status_code != 200:
        print(r)
        sys.exit("Failed to get wait data")

    wait_times = {}

    # First line is the header row
    lines = r.text.splitlines()[1:]

    for line in lines:
        office, appt_time, non_appt_time, _ = line.split(',')
        if office not in offices:
            print(f'Unrecognized office number: {office}')
            continue

        wait_times[office] = {
            'appt': appt_time,
            'no_appt': non_appt_time,
        }
        print(f'{offices[office]}: \n'
              f'  appointment    {appt_time}\n'
              f'  no appointment {non_appt_time}\n')

    return wait_times


def load_previous_output():
    if os.path.exists(OUTPUT_PATH):
        with open(OUTPUT_PATH, 'r') as f:
            return json.load(f)

    return []


def write_output(wait_by_office):
    data = load_previous_output()

    data.append({
        'timestamp': int(time.time()),
        'wait_times': wait_by_office
    })

    with open(OUTPUT_PATH, 'w') as f:
        json.dump(data, f, indent=4)


def main():
    offices = load_offices()
    wait_times = load_wait_time(offices)
    write_output(wait_times)


if __name__ == '__main__':
    main()
