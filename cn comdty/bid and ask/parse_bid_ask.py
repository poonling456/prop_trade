import sys, os
import pandas as pd
import glob
import datetime

def read_bid_ask(df):
    df = df[df.type != 'TRADE']
    df['bid'] = [p if t == "BID" else None for t, p in zip(df.type, df.price)]
    df['bid'] = df['bid'].shift()
    df['bid_size'] = [s if t == "BID" else None for t, s in zip(df.type, df["size"])]
    df['bid_size'] = df['bid_size'].shift()
    df['ask'] = [p if t == "ASK" else None for t, p in zip(df.type, df.price)]
    df['ask_size'] = [s if t == "ASK" else None for t, s in zip(df.type, df["size"])]
    df = df[df.type == 'ASK']
    df = df[['dt', 'bid', 'bid_size', 'ask', 'ask_size']]
    df.dropna(inplace=True)
    df["ba_ratio"] = df.bid_size / df.ask_size

    return df

files = glob.glob('parsed_data/*.pkl')

for file in files:
    basename = os.path.basename(file)
    base = os.path.splitext(basename)[0]

    print basename

    file_create = 'data/%s.pkl' % base
    file_create2 = 'data/%s.csv' % base
    # if os.path.isfile(file_create):
    #     continue

    df = pd.read_pickle(file)
    df = read_bid_ask(df)

    print df
    # df.to_csv('parsed_data/' + file[9:])
    df.to_pickle(file_create)
    df.to_csv(file_create2)
