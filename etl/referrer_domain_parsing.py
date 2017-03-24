import psycopg2 as psql

conn = psql.connect(
	dbname="fandp_web_production",
	user="peterm",
	host="postgres-read-replica.production.fameandpartners.com",
	password="8fktC9MeVxiZ5Z+oAsUw/jQr")

cur = conn.cursor()

cur.execute("""
SELECT referrer, COUNT(*)
FROM marketing_user_visits
WHERE LOWER(utm_source) = 'linkshare'
	AND referrer IS NOT NULL
GROUP BY referrer
""")

results = cur.fetchall()

from urlparse import urlparse

domains = []
for record in results:
	parsed_uri = urlparse(record[0])
	domain = '{uri.scheme}://{uri.netloc}/'.format(uri=parsed_uri)
	domains.append((domain, record[0], record[1]))

import pandas as pd

domain_df = pd.DataFrame(domains)
domain_df.columns = ['domain','full_url','count']
domain_df.to_csv('~/Documents/Data/referrer_domain_parsed.csv')

conn.close()
cur.close()
