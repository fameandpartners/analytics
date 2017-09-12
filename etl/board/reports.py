import pandas as pd

import spree_db.sales as sales
import google_apps.analytics as ga
import nps

# Hacky way to make sure I don't put the password in git 
from secrets import PASS, DKEY

def pull_monthly_kpis():
    """Merges the above reports utilizing their standard formatting.
    """
    traffic = ga.pull_traffic()
    net_revenue_direct = sales.pull_direct_net_revenue()
    direct_costs = sales.pull_cost_of_sales()
    factory_performance = sales.pull_factory_performance()
    monthly_kpis = pd.concat([traffic, net_revenue_direct, direct_costs,
                              factory_performance,])
    monthly_kpis = monthly_kpis.pivot_table(index=['A','B','C','D'], 
                                            columns='year_month', 
                                            values='value', 
                                            fill_value='')
    return monthly_kpis


