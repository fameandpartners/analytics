import psycopg2 as psql
import pandas as pd

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

def sizeof_fmt(num, suffix='B'):
    for unit in ['','Ki','Mi','Gi','Ti','Pi','Ei','Zi']:
        if abs(num) < 1024.0:
            return '%3.1f %s%s' % (num, unit, suffix)
        num /= 1024.0
    return '%.1f %s%s' % (num, 'Yi', suffix)

def pull(PASS):
    """Queries the Spree database to get the sales DataFrame
    """
    conn = psql.connect(
        user='peterm', 
        password=PASS, 
        dbname='fandp_web_production', 
        host='postgres-read-replica.production.fameandpartners.com'
    )
    ordered_units = sql_to_dataframe('queries/ordered_units.sql', conn)
    customizations = sql_to_dataframe('queries/customizations.sql', conn)
    products = sql_to_dataframe('queries/products.sql', conn)
    shipments = sql_to_dataframe('queries/shipments.sql', conn)
    addresses = sql_to_dataframe('queries/addresses.sql', conn)
    returns = sql_to_dataframe('queries/returns.sql', conn)
    return_events = sql_to_dataframe('queries/return_events.sql', conn)
    payments = sql_to_dataframe('queries/payments.sql', conn)
    adjustments = sql_to_dataframe('queries/adjustments.sql', conn)
    promotions = sql_to_dataframe('queries/promotions.sql', conn)
    product_taxons = sql_to_dataframe('queries/product_taxons.sql', conn)
    dress_images = sql_to_dataframe('queries/dress_images.sql', conn)
    slow_fast_items = sql_to_dataframe('queries/slow_fast_items.sql', conn)
    conn.close()

    results = [
        ordered_units,customizations,products,shipments,addresses,returns,
        return_events,payments,adjustments,promotions,product_taxons,
        dress_images,slow_fast_items,
    ]
    total_memory = sum([r.memory_usage().sum() for r in results])
    print('Size: ' + sizeof_fmt(total_memory))

def pull_csv():
    return pd.read_csv(
        '/Users/Peter 1/Dropbox (Team Fame)/data/board/inputs/products_sold.csv',
        index_col='line_item_id'
    )


