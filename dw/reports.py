import pandas as pd
import sales, returns
import google_apps.analytics as ga
import nps

def monthly_kpis():
    """Merges the above reports utilizing their standard formatting.
    """
    traffic = ga.pull_traffic()
    net_revenue_direct = sales.pull_direct_net_revenue()
    reconciled_returns = returns.pull_reconciled_demand_returns()
    direct_costs = sales.pull_cost_of_sales()
    factory_performance = sales.pull_factory_performance()
    nps_responses = nps.pull()
    monthly_kpis = pd.concat([traffic, net_revenue_direct, direct_costs,
                              factory_performance,nps_responses,reconciled_returns,])
    monthly_kpis = monthly_kpis.pivot_table(index=['A','B','C','D'],
                                            columns='year_month',
                                            values='value',
                                            fill_value='')
    return monthly_kpis

def daily_kpis():
    daily_df = sales.pull_daily_direct_csv()
    nps_df = nps.pull_daily()
    daily_df['date_str'] = daily_df.Date.apply(lambda date: str(date))
    nps_df['date_str'] = nps_df.Date.apply(lambda date: str(date))
    del nps_df['Date']
    daily_kpis = daily_df.merge(nps_df, how='left', on='date_str')
    del daily_kpis['date_str']
    daily_kpis['Date'] = pd.to_datetime(daily_kpis.Date)
    return daily_kpis.fillna(0)

def revenue_attribution():
    c = ga.pull_all_channels()
    return c[['channelGrouping','transactions']]

if __name__ == '__main__':
    from datetime import date
    monthly_kpis().reset_index().to_csv('~/data/monthly_kpis_2016_Present_' + str(date.today()) + '.csv', index=False)
