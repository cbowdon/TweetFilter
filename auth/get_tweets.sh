#!/bin/bash

search_url="https://api.twitter.com/1.1/search/tweets.json?q=code&lang=en"

token_json=$(cat /home/chris/Tweet/auth/bear_token.json)

function extract_token {
    echo -n $1 | grep -o '"access_token":".*",' | cut -d '"' -f 4
}

token=$(extract_token $token_json)

auth="Authorization: Bearer "$token

curl --url $search_url \
    --request "GET" \
    --header "$auth"
