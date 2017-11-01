import os

from dateutil.parser import parse
import psycopg2 as psql
import pandas as pd

from secrets import PASS

__location__ = os.path.realpath(
    os.path.join(os.getcwd(), os.path.dirname(__file__)))
QUERIES = os.path.join(__location__, 'queries/')

def connect_to_fame():
    return psql.connect(
        user='peterm',
        password=PASS,
        dbname='fandp_web_production',
        host='postgres-read-replica.production.fameandpartners.com'
    )

def sql_to_dataframe(sql_file, connection):
    """Execute SQL query in file and then save the results to a DataFrame.
    """
    f = open(sql_file, 'r')
    sql = f.read()
    f.close()

    curs = connection.cursor()
    curs.execute(sql)
    results = curs.fetchall()
    df = pd.DataFrame(results)
    df.columns = [column[0] for column in curs.description]
    curs.close()
    return df

STATIC_DATA = 'Rscripts/static-data/'

def pull_direct_csv():
    return pd.read_csv(STATIC_DATA + 'monthly_direct.csv')

def pull_daily_direct_csv():
    return pd.read_csv(STATIC_DATA + 'daily_direct.csv')

def pull_direct_factory_csv():
    return pd.read_csv(STATIC_DATA + 'monthly_factory_direct.csv')

def pull_cohort_assignments_csv():
    return pd.read_csv(STATIC_DATA + 'cohort_assignments.csv')

def pull_acquisitions():
    conn = connect_to_fame()
    return sql_to_dataframe(QUERIES + 'first_order_dates.sql', conn)

def pull_direct_net_revenue():
    monthly_direct = pull_direct_csv().rename(columns={'Cohort':'D'})
    monthly_direct['A'] = 'Net Revenue'

    base_cols = ['year_month','A','D']

    customers = monthly_direct.melt(id_vars=base_cols,
                                    value_vars=['Repeat Customers',
                                                'New Customers'])\
                               .rename(columns={'variable':'C'})
    customers['B'] = 'Customers'

    gross_revenue = monthly_direct[base_cols + ['Gross Revenue']]\
                                  .rename(columns={'Gross Revenue':'value'})
    gross_revenue['B'] = 'Gross Revenue'
    gross_revenue['C'] = 'Direct'

    transactions = monthly_direct.melt(id_vars=base_cols,
                                       value_vars=['Orders','Units',
                                                   'Customized Units',
                                                   'Re-fulfilled Units'])\
                                 .rename(columns={'variable':'B'})
    transactions['C'] = 'Direct'

    contra_revenue = monthly_direct.groupby(['year_month'])\
                                   .sum()\
                                   .reset_index()[['year_month','Discounts']]\
                                   .rename(columns={'Discounts':'value'})

    contra_revenue['A'] = 'Net Revenue'
    contra_revenue['B'] = 'Contra Revenue'
    contra_revenue['C'] = 'Direct'
    contra_revenue['D'] = 'Discounts'
    return pd.concat([customers,gross_revenue,contra_revenue,transactions,])

def pull_cost_of_sales():
    monthly_direct = pull_direct_csv()
    direct = monthly_direct.groupby(['year_month']).sum().reset_index()
    direct['A'] = 'Cost of Sales'

    direct_costs = direct.melt(id_vars=['year_month','A'],
                               value_vars=['Product Cost',
                                           'Packaging Materials'])\
                         .rename(columns={'variable':'D'})
    direct_costs['B'] = 'Cost of Goods Sold'
    direct_costs['C'] = 'Direct'

    direct_expenses = direct[['year_month','A','Shipping']]\
                            .rename(columns={'Shipping':'value'})
    direct_expenses['B'] = 'Sales Expenses'
    direct_expenses['C'] = 'Direct'
    direct_expenses['D'] = 'Shipping'

    cohorts = monthly_direct[['year_month','Cohort','COGS']]\
                            .rename(columns={'Cohort':'D','COGS':'value'})
    cohorts['A'] = 'Cost of Sales'
    cohorts['B'] = 'Cost of Goods Sold'
    cohorts['C'] = 'Direct'
    return pd.concat([direct_costs,direct_expenses,cohorts])

def pull_factory_performance():
    factory_direct = pull_direct_factory_csv()
    factory_direct = factory_direct.melt(id_vars=['year_month','Factory',],
                                         value_vars=['Units','Avg. Make Time',])\
                                   .rename(columns={'Factory':'D',
                                                    'variable':'C'})
    factory_direct['A'] = 'Factory Performance'
    factory_direct['B'] = 'Direct'
    return factory_direct
