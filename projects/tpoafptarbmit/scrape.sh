#!/bin/env bash

set -eux
# set -o pipefail

# TODO: Ensure these are installed before starting.
REQUIRED_EXECUTABLES='curl pup jq'

ROUTE_BASE_URL='http://www.thepassageride.com/Routes'
PUP=~/go/bin/pup

TMP_DIR="./tmp"

# So that we can use `fetch_route` etc in `xargs` call.
set -o allexport

save_result () {
    tee "$TMP_DIR/$1"
}

use_saved () {
    cat "$TMP_DIR/$1"
}

fetch_cached () {
    file="$TMP_DIR/$2"

    if [ -f "$file" ]; then
        cat "$file"
    else
        echo "fetching $1"
        curl -s "$1" | tee "$file"
    fi
}

fetch_route_list_html () {
    fetch_cached "$ROUTE_BASE_URL/" "routes.html"
}

extract_route_links () {
    < /dev/stdin $PUP -p '#wikitext a[href*="/Routes/"] json{}' \
        | jq -c ".[]|{
  number: .href | ltrimstr(\"$ROUTE_BASE_URL/\") | tonumber
, href
, text
}" \
        | save_result "routes.json"
}

scrape () {
    fetch_route_lsit_html \
        | extract_route_links \
        | cat
}


fetch_route () {
    json="$1"
    num=$(echo "$json" | jq .number -r)
    route_page_html="$(fetch_cached "$ROUTE_BASE_URL/$num" "route_$num.html")"

    map_link=$(
        echo "$route_page_html" \
            | $PUP -p '#wikitext a[href*="gmap-pedometer"] json{}' \
            | jq -c '.[].href'
    )

    echo $map_link | save_result "route_${num}_map"
}

# fetch_route_list_html \
#     | extract_route_links \
#     | xargs -n8 -P8 fetch_route


mkdir -p "$TMP_DIR"

fetch_route_list_html \
    | extract_route_links \
    | while read -r json; do
    echo "json: $json"
    fetch_route "$json"
done
