import pandas as pd
import numpy as np
from datetime import datetime, timedelta

def load_cmc_data(start_date, end_date, first=150): 
    """
    load snapshots between start_date and end_date
    returns defaultdict {date: data,}
    """

    start_dt = datetime.strptime(start_date, '%Y%m%d')
    end_dt = datetime.strptime(end_date, '%Y%m%d')
    delta = end_dt - start_dt
    
    cmc = {}
    for dt in range(delta.days+1):
        snapshot = datetime.strftime(start_dt + timedelta(days=dt),'%Y%m%d')
        cmc[snapshot] = pd.read_csv(f'./cmc_snapshots/cmc_snapshot_{snapshot}.csv').iloc[:first]
        if len(cmc[snapshot]) != first:
            print(f'Problem in loaded {snapshot} - of length {len(cmc[snapshot])}') # check for data integrity; make sure there are N rows
    
    return cmc


data = load_cmc_data('20150101', '20201129')
print('loaded cmc data')


for sym in ['btc','eth']:
    df = pd.DataFrame(columns = ['date','price'])
    for dt, data in data.items():
        try:
            price = data[data['symbol']==sym.upper()]['price'].to_list()[0]
        except:
            price = None
        df = df.append({'date':dt, 'price':price}, ignore_index=True)

    df = df.sort_values('date')
    df = df[~df['price'].isna()]
    df['date'] = df['date'].apply(lambda x: datetime.strptime(x, '%Y%m%d'))
    df = df.set_index('date')
    dfg = df.resample('1W').apply(lambda x : x.iloc[-1])
    dfg = dfg.sort_index()
    dfg['ret'] = dfg['price']/dfg['price'].shift(1)
    dfg['logret'] = dfg['ret'].apply(lambda x: np.log(x))
    for t in range(1,4):
        dfg[f'logret_lag{t}'] = dfg['logret'].shift(t)
    dfg.to_csv(f'./data/{sym}.csv', index=True)
    print('saved data')
