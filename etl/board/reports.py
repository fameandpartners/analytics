import pandas as pd

import spree_db.sales as sales
import google_apps.analytics as ga
import delighted_export as nps

# Hacky way to make sure I don't put the password in git 
from secrets import PASS, DKEY

def traffic():
    ga_conn = ga.initialize_analyticsreporting()

    device_traffic = ga.get_report(
        ga_conn=ga_conn, 
        dimensions=[{'name': 'ga:deviceCategory'},{'name': 'ga:yearMonth'},])
    device_traffic = ga.melt_ga(device_traffic, a='Traffic', c='Device', d='deviceCategory')

    geo_traffic = ga.get_report(
        ga_conn=ga_conn,
        dimensions=[{'name': 'ga:country'},{'name': 'ga:yearMonth'},])
    bucketed_countries = geo_traffic.apply(lambda row: ga.bucket_countries(row), axis=1)
    geo_traffic['country'] = bucketed_countries
    geo_traffic = geo_traffic.groupby(['yearMonth','country'])\
                             .sum()\
                             .reset_index()
    geo_traffic = ga.melt_ga(geo_traffic, a='Traffic', c='Geography', d='country')

    channel_traffic = ga.get_report(
        ga_conn=ga_conn, 
        dimensions=[{'name': 'ga:channelGrouping'},{'name': 'ga:yearMonth'},])
    bucketed_channels = channel_traffic.apply(lambda row: ga.bucket_channels(row), axis=1)
    channel_traffic['channelGrouping'] = bucketed_channels
    channel_traffic = channel_traffic.groupby(['yearMonth','channelGrouping'])\
                                     .sum()\
                                     .reset_index()
    channel_traffic = ga.melt_ga(channel_traffic, a='Traffic', c='Channel', d='channelGrouping')
    return pd.concat([device_traffic, geo_traffic, channel_traffic])

# Net Revenue
# Direct
products_sold = sales.pull_csv()
