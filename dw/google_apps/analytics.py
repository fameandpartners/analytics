"""Google Analytics Reporting API V4."""
import os

from apiclient.discovery import build
from oauth2client.service_account import ServiceAccountCredentials
import pandas as pd

def pull_traffic():
    """Queries Google Analytics to create a few reports and then merges the
    reports into one report in a standard format.
    """
    ga_conn = initialize_analyticsreporting()

    device_traffic = get_report(
        ga_conn=ga_conn,
        dimensions=[{'name': 'ga:deviceCategory'},{'name': 'ga:yearMonth'},])
    device_traffic = melt_ga(device_traffic,
                                a='Traffic',
                                c='Device',
                                d='deviceCategory')

    geo_traffic = get_report(
        ga_conn=ga_conn,
        dimensions=[{'name': 'ga:country'},{'name': 'ga:yearMonth'},])
    bucketed_countries = geo_traffic.apply(lambda row: bucket_countries(row), axis=1)
    geo_traffic['country'] = bucketed_countries
    geo_traffic = geo_traffic.groupby(['yearMonth','country'])\
                             .sum()\
                             .reset_index()
    geo_traffic = melt_ga(geo_traffic,
                             a='Traffic',
                             c='Geography',
                             d='country')

    channel_traffic = get_report(
        ga_conn=ga_conn,
        dimensions=[{'name': 'ga:channelGrouping'},{'name': 'ga:yearMonth'},])
    bucketed_channels = channel_traffic.apply(lambda row: bucket_channels(row), axis=1)
    channel_traffic['channelGrouping'] = bucketed_channels
    channel_traffic = channel_traffic.groupby(['yearMonth','channelGrouping'])\
                                     .sum()\
                                     .reset_index()
    channel_traffic = melt_ga(channel_traffic,
                                 a='Traffic',
                                 c='Channel',
                                 d='channelGrouping')
    df_out = pd.concat([device_traffic, geo_traffic, channel_traffic])
    return df_out

def pull_all_channels():
    ga_conn = initialize_analyticsreporting()
    channel_traffic = get_report(
        ga_conn=ga_conn,
        date_ranges=[{'startDate': '2017-01-01', 'endDate': 'today'}],
        dimensions=[{'name': 'ga:channelGrouping'},])
    return channel_traffic


__location__ = os.path.realpath(
    os.path.join(os.getcwd(), os.path.dirname(__file__)))

SCOPES = ['https://www.googleapis.com/auth/analytics.readonly']
KEY_FILE_LOCATION = os.path.join(__location__, 'ga_secrets.json')
VIEW_ID = '72841736'


def initialize_analyticsreporting():
    """Initializes an Analytics Reporting API V4 service object.

    Returns:
        An authorized Analytics Reporting API V4 service object.
    """
    credentials = ServiceAccountCredentials.from_json_keyfile_name(
        KEY_FILE_LOCATION, SCOPES)

    # Build the service object.
    ga_conn = build('analyticsreporting', 'v4', credentials=credentials)

    return ga_conn

def get_report(
        ga_conn,
        date_ranges=[{'startDate': '2016-01-01', 'endDate': 'today'}],
        metrics=[{'expression': 'ga:sessions'},{'expression': 'ga:transactions'}],
        dimensions=[{'name': 'ga:yearMonth'}]):
    """Queries the Analytics Reporting API V4 and returns the results in a
    pandas DataFrame.
    """
    results = ga_conn.reports().batchGet(
            body={
                'reportRequests': [
                {
                    'viewId': VIEW_ID,
                    'pageSize': '5000',
                    'dateRanges': date_ranges,
                    'metrics': metrics,
                    'dimensions': dimensions
                }]
            }).execute()

    ga_report = results.get('reports')[0]
    rows = []
    for row in ga_report.get('data').get('rows'):
        dimensions = row.get('dimensions')
        metrics = row.get('metrics')[0].get('values')
        rows.append(dimensions + list(map(int, metrics)))
    df = pd.DataFrame(rows)
    d_cols = [d.split(':')[1] for d in ga_report.get('columnHeader').get('dimensions')]
    m_headers = ga_report.get('columnHeader').get('metricHeader').get('metricHeaderEntries')
    m_cols = [m.get('name').split(':')[1] for m in m_headers]
    df.columns = d_cols + m_cols
    return df

def bucket_countries(country_row):
    country = country_row['country']
    if country in ['United States','Australia',]:
        return country
    else:
        return 'Other'

def bucket_channels(channel_row):
    channel = channel_row['channelGrouping']
    if channel in ['Facebook','Paid Search','Referral','Display',]:
        return 'Paid'
    else:
        return 'Organic & Direct'

def melt_ga(df, a, c, d):
    new_df = df.melt(id_vars=['yearMonth', d],
                     value_vars=['sessions','transactions'])\
               .rename(columns={'yearMonth': 'year_month',
                                'variable': 'B',
                                d: 'D'})
    new_df['A'] = a
    new_df['B'] = new_df['B'].apply(lambda v: v.title())
    new_df['C'] = c
    new_df['D'] = new_df['D'].apply(lambda d: d.title())
    new_df['year_month'] = (new_df['year_month'].apply(lambda d: d[:4])
                            + '-'
                            + new_df['year_month'].apply(lambda d: d[4:]))
    return new_df[['A','B','C','D','year_month','value']]
