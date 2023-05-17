import sys, os
import pandas as pd
import glob
import datetime

def futures_parse(df):
    df.columns = ['index', 'dt', 'type', 'price', 'size', 'concode']
    df = df[['dt', 'type', 'price', 'size']]
    df['dt'] = pd.to_datetime(df.dt, format='%Y-%m-%d %H:%M:%S')
    df['price'] = df.price.apply(pd.to_numeric, errors='coerce')
    df = df[df.price != 0]

    return df

files = glob.glob('raw_data/*.csv')

for file in files:
    basename = os.path.basename(file)
    base = os.path.splitext(basename)[0]

    print basename

    file_create = 'parsed_data/%s.pkl' % base
    # if os.path.isfile(file_create):
    #     continue

    df = pd.read_csv(file)
    df = futures_parse(df)

    print df.head(5)
    # df.to_csv('parsed_data/' + file[9:])
    df.to_pickle(file_create)
