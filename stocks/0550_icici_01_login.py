
from breeze_connect import BreezeConnect
import http.client
import json
import pandas as pd

conn = http.client.HTTPSConnection("api.icicidirect.com")
payload = "{\r\n    \"password\": \"L786psh*\",\r\n    \"dOB\": \"30081978\",\r\n    \"iP_ID\": \"120.0.0.1\",\r\n    \"appKey\": \"816jz797)907Hc9T#832a5761l34871Y\",\r\n    \"idirect_Userid\": \"W0350754\",\r\n    \"user_Data\": \"ALL\"\r\n}"
headers = {"content-Type":"application/json"}

conn.request("GET", "/breezeapi/api/v1/customerlogin", payload, headers)
res = conn.getresponse()

data = json.loads((res.read()).decode("utf-8"))
print(data)

# Initialize SDK
breeze = BreezeConnect(api_key="816jz797)907Hc9T#832a5761l34871Y")

# Generate Session
breeze.generate_session(api_secret="632264130p03oZ*c^04I6z470q71Q4sX",
                      session_token=data.get("Success").get("API_Session"))

# Connect to websocket
breeze.ws_connect()

