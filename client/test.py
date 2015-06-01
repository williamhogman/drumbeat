import pprint
import requests
import json


def place(data):
    req = requests.get('http://localhost:4000/', data=json.dumps(data))
    try:
        pprint.pprint(req.json())
    except:
        pprint.pprint(req)


http_bin = {
    'url': 'http://httpbin.org/post',
    'method': 'post',
    'body': {'foo': 1}
}

place(http_bin)

place({
    'url': {'type': 'quote'},
    'respond_to': http_bin
})
