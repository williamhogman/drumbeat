import pprint
import requests
import json

foo = json.dumps(dict(url='http://httpbin.org/post', method='post', body=dict(foo=1)))


req = requests.get('http://localhost:4000/json', data=foo)
try:
   pprint.pprint(req.json())
except:
   pprint.pprint(req)
