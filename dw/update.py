from sys import argv
from sqlalchemy import create_engine
from jobs import (
    load_sales, load_products, load_product_taxons, load_facebook_images,
    load_customization_values, load_line_item_customizations, load_daily_kpis,
    load_cohort_assignments
)
import os

def create_dw_engine(production = False):
    if production:
        password = os.environ['RDS_PASSWORD']
        host = os.environ['RDS_HOSTNAME']
        engine = create_engine(f'postgresql+psycopg2://peter:{password}@{host}:5432/dw')
    else:
        host = 'localhost'
        engine = create_engine(f'postgresql+psycopg2://peterh:@{host}:5432/dw_dev')
    return engine

def which_db(sys_argv):
    try:
        which_eng = sys_argv[1]
        production = True if which_eng == 'production' else False
    except:
        production = False
    return production

if __name__ == '__main__':
    production = which_db(argv)
    print('Connecting to our Data Warehouse')
    dw_engine = create_dw_engine(production=production)
    print('Loading Sales Data')
    load_sales(engine=dw_engine)
    print('Loading Product Data')
    load_products(engine=dw_engine)
    print('Loading Product Taxons')
    load_product_taxons(engine=dw_engine)
    print('Loading Facebook Images')
    load_facebook_images(engine=dw_engine)
    print('Loading Customization Values')
    load_customization_values(engine=dw_engine)
    print('Loading Line Item Customizations')
    load_line_item_customizations(engine=dw_engine)
    print('Loading Daily KPIs')
    load_daily_kpis(engine=dw_engine)
    print('Loading Cohort Assignments')
    load_cohort_assignments(engine=dw_engine)
    print('All Data Loaded into Warehouse')
