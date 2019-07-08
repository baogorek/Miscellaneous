# https://realpython.com/python-requests/

import requests
from requests.exceptions import HTTPError

try:
    response = requests.get(
        'https://api.github.com',
        params={'q': 'requests+language:python'},

    )
    response.raise_for_status()

except HTTPError as http_err:
    print(f'HTTP error occurred: {http_err}')

except Exception as err:
    print(f'Other error occurred: {err}')

else:
    print("successful connection")

response.content # content in bytes
response.text # content in text
resp_json = response.json()
resp_json['user_search_url']

# httpbin.org is a great resource created by the author of requests, Kenneth Reitz.
# It’s a service that accepts test requests and responds with data about the requests.

response = requests.post('https://httpbin.org/post', json={'key':'value'})
json_response = response.json()
json_response['data'] #You can see from the response that the server received your request data


# These examples don't cover auth


