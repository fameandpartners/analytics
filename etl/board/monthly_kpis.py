import spree_db.sales as sales
import google_apps.analytics as ga
import delighted_export as nps

# Hacky way to make sure I don't put the password in git 
from secrets import PASS, DKEY

# Traffic
ga_conn = ga.initialize_analyticsreporting()
device_traffic = ga.get_report(
    ga_conn=ga_conn, 
    dimensions=[{'name': 'ga:yearMonth'},{'name': 'ga:deviceCategory'}])
geo_traffic = ga.get_report(
    ga_conn=ga_conn, 
    dimensions=[{'name': 'ga:yearMonth'},{'name': 'ga:country'}])
channel_traffic = ga.get_report(
    ga_conn=ga_conn, 
    dimensions=[{'name': 'ga:yearMonth'},{'name': 'ga:channelGrouping'}])

# Net Revenue
# Direct
products_sold = sales.pull_csv()
