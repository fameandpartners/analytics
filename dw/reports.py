import pandas as pd
from spree_db import sales, returns
import google_apps.analytics as ga
import nps

# Hacky way to make sure I don't put the password in git
from secrets import PASS, DKEY

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

def revenue_attribution():
    c = ga.pull_all_channels()
    return c[['channelGrouping','transactions']]

if __name__ == '__main__':
    print(monthly_kpis().to_csv())
