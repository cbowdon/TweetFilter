#!/bin/bash

token_url="https://api.twitter.com/oauth2/token"

key_file="/home/chris/Tweet/auth/key"
secret_file="/home/chris/Tweet/auth/secret"

# Examples from Twitter docs useful to confirm encoding is working
#key=xvz1evFS4wEEPTGEFPHBog
#secret=L8qq9PZyRg6ieKGEKhZolGC0vJWLw8iEJ88DRdyOg
#encoded=eHZ6MWV2RlM0d0VFUFRHRUZQSEJvZzpMOHFxOVBaeVJnNmllS0dFS2hab2xHQzB2SldMdzhpRUo4OERSZHlPZw==

# TODO url-encode consumer key and secret
key=$(cat $key_file)
secret=$(cat $secret_file)
cred=$key":"$secret
enc_cred=$(printf $key":"$secret | base64 --wrap=0)

content_type="Content-Type: application/x-www-form-urlencoded;charset=UTF-8"
host="Host: api.twitter.com"
auth="Authorization: Basic "$enc_cred
body="grant_type=client_credentials"

curl --url $token_url \
    --header "$auth" \
    --header "$content_type" \
    --request "POST" \
    --data "$body"
