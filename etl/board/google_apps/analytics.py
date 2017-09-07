"""Hello Analytics Reporting API V4."""
import os

from apiclient.discovery import build
from oauth2client.service_account import ServiceAccountCredentials
import pandas as pd

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
        date_ranges=[{'startDate': '2017-01-01', 'endDate': 'today'}],
        metrics=[{'expression': 'ga:sessions'},{'expression': 'ga:transactions'}],
        dimensions=[{'name': 'ga:country'}, {'name': 'ga:yearMonth'}]
        ):
    """Queries the Analytics Reporting API V4.
    """
    results = ga_conn.reports().batchGet(
            body={
                'reportRequests': [
                {
                    'viewId': VIEW_ID,
                    'dateRanges': date_ranges,
                    'metrics': metrics,
                    'dimensions': dimensions
                }]
            }
    ).execute()

    print('GA Report with %s rows.' % results.get('reports')[0].get('data').get('rowCount'))

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




