import pandas as pd

LOCAL_DROPBOX = '/Users/Peter 1/Dropbox (Team Fame)/data/board/inputs/'

def pull_reconciled_demand_returns():
    demand_returns = pd.read_csv(LOCAL_DROPBOX + 'reconciled_demand_returns.csv')
    inventory_returns = demand_returns[['year_month','inventory_returns']]\
                                      .rename(columns={'inventory_returns':'value'})\
                                      .groupby(['year_month'])\
                                      .sum()\
                                      .reset_index()
    inventory_returns['A'] = 'Cost of Sales'
    inventory_returns['B'] = 'Cost of Goods Sold'
    inventory_returns['C'] = 'Direct'
    inventory_returns['D'] = 'Inventory Returns'

    demand_returns = demand_returns[['year_month','Cohort','returns']]
    demand_returns['A'] = 'Net Revenue'
    demand_returns['B'] = 'Returns'
    demand_returns['C'] = 'Direct'
    demand_returns = demand_returns.rename(columns={'Cohort':'D','returns':'value'})
    return pd.concat([inventory_returns, demand_returns])
