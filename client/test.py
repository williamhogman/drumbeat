import pprint
import requests
import json


def place(data):
    req = requests.get('http://localhost:4000/sync', data=json.dumps(data))
    try:
        pprint.pprint(req.json())
    except:
        pprint.pprint(req)
        pprint.pprint(req.content)

http_get = {
    'url': 'http://httpbin.org/get',
}

http_bin = {
    'url': 'http://httpbin.org/post',
    'method': 'post',
}


#place([http_get])
place([http_get, http_bin, dict(type='quote')])
