from sys import argv
from sqlalchemy import create_engine
from jobs import (
    load_sales, load_products, load_daily_kpis, load_cohort_assignments
)
from secrets import DWPASS

def create_dw_engine(production = False):
    if production:
        password = DWPASS
        host = 'fp-dw.cs60zg0gg0au.us-east-1.rds.amazonaws.com'
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
    print('Loading Daily KPIs')
    load_daily_kpis(engine=dw_engine)
    print('Loading Cohort Assignments')
    load_cohort_assignments(engine=dw_engine)
    print('All Data Loaded into Warehouse')
