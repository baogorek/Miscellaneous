# https://github.com/runtimeverification/verified-smart-contracts/blob/uniswap/uniswap/x-y-k.pdf

import requests
import json

import pandas as pd

pd.set_option('display.float_format', lambda x: '%.5f' % x)

# How to get the first block after 2020 starts:
eth_blocks_url = "https://api.thegraph.com/subgraphs/name/blocklytics/ethereum-blocks"
uniswap_url = "https://api.thegraph.com/subgraphs/name/uniswap/uniswap-v2"
headers = {"Content-Type": "application/json"}

data = {
  "query": 
  "
  blocks(first: 1, orderBy: timestamp, orderDirection: asc, where: {timestamp_gt: "1577836800"}) {
    id
    number
    timestamp
  }
  "
}

x = requests.post(eth_blocks_url, json=data)

payload = json.dumps({'query': '{ bundle(id: "1") { ethPrice } }' })

x = requests.post(uniswap_url, data=payload, headers={"Content-Type": "application/json"})
x.content

 # Challenge 1: Try reading in pairs with skip

{
   pair(id: "0xa478c2975ab1ea89e8196811f51a7b7ade33eb11",
        block: {number: 11811966}) {
     id
     token0 {
       name
     }
     token1 {
       name
     }
    reserve0
    reserve1
    token0Price
    token1Price
    createdAtTimestamp
    createdAtBlockNumber
    txCount
    liquidityProviderCount
   }
}

payload = json.dumps({'query':
"""
{
 pairDayDatas(first: 1000, skip: 0, orderBy: date, orderDirection: asc,
   where: {
     pairAddress: "0xa478c2975ab1ea89e8196811f51a7b7ade33eb11",
     date_gt: 1588291200
   }
 ) {
     date
     token0 {
      name
    }
    token1 {
      name
    }
     reserve0
     reserve1
 }
}
"""
})

x = requests.post(uniswap_url, data=payload, headers=headers)

obj = json.loads(x.content)

df = pd.DataFrame(obj['data']['pairDayDatas'])
df['date'] = pd.to_datetime(df.date, unit='s')
df['reserve0'] = df['reserve0'].astype(float)
df['reserve1'] = df['reserve1'].astype(float)
df = df.rename({'reserve0': 'dai_reserves', 'reserve1': 'weth_reserves'}, axis=1)
df = df[['date', 'dai_reserves', 'weth_reserves']]

df['k'] = df['dai_reserves'] * df['weth_reserves']
df['price_eth_in_dai'] = df['k'] / (df['weth_reserves'] - 1) - df['dai_reserves'] 

df.to_csv('dai_eth.csv', index=False)

# Price calc from the reserves
1594.3278965204954

res_dai = 73132926.324154939775607691
res_eth = 45871.693527667125123858

k = res_dai * res_eth

k / (res_eth - 1) - res_dai
1594.3278965204954

fee = .003

price_66 = 1594.293
