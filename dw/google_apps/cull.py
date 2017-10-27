from datetime import date, timedelta
import pandas as pd
from analytics import initialize_analyticsreporting, VIEW_ID

def get_traffic(ga_conn):
    date_ranges = [{'startDate': str(date.today() - timedelta(days=120)),
                    'endDate': str(date.today() - timedelta(days=30))}]
    metrics = [{'expression': 'ga:sessions'},{'expression': 'ga:bounces'}]
    dimensions = [{'name': 'ga:pagePathLevel2'}]
    order_bys = [{'fieldName': 'ga:sessions', 'sortOrder': 'DESCENDING'}]
    dimension_filters = [{"filters": [{"dimensionName": "ga:pagePathLevel2",
                                       "operator": "REGEXP",
                                       "expressions": ["dress-"]}]}]
    request_body = {
        'reportRequests': [
        {
            'viewId': VIEW_ID,
            'pageSize': '5000',
            'dateRanges': date_ranges,
            'metrics': metrics,
            'dimensions': dimensions,
            'orderBys': order_bys,
            "dimensionFilterClauses": dimension_filters
        }]
    }
    results = ga_conn.reports().batchGet(body=request_body).execute()
    ga_report = results.get('reports')[0]
    next_page_token = ga_report.get('nextPageToken')
    reports_data = [ga_report.get('data')]
    pages_requested = 0
    while next_page_token and pages_requested < 10:
        new_request_body = request_body
        new_request_body['reportRequests'][0]['pageToken'] = next_page_token
        new_results = ga_conn.reports().batchGet(body=new_request_body).execute()
        new_report = new_results.get('reports')[0]
        next_page_token = new_report.get('nextPageToken')
        pages_requested += 1
        reports_data.append(new_report.get('data'))
    rows = []
    for data in reports_data:
        for row in data.get('rows'):
            dimensions = row.get('dimensions')
            metrics = row.get('metrics')[0].get('values')
            rows.append(dimensions + list(map(int, metrics)))
    report = pd.DataFrame(rows)
    d_cols = [d.split(':')[1] for d in ga_report.get('columnHeader').get('dimensions')]
    m_headers = ga_report.get('columnHeader').get('metricHeader').get('metricHeaderEntries')
    m_cols = [m.get('name').split(':')[1] for m in m_headers]
    report.columns = d_cols + m_cols
    return report

if __name__ == '__main__':
    ga_conn = initialize_analyticsreporting()
    traffic = get_traffic(ga_conn)
    traffic = traffic.rename(columns={'pagePathLevel2': 'Page', 'sessions': 'Sessions', 'bounces': 'Bounces'})
    print(traffic.to_csv(index=False))
