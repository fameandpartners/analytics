from sys import argv
from sqlalchemy import create_engine
from jobs import load_test
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

def query_test(engine):
    query = engine.execute('select * from test')
    return query.fetchall()

def which_db(sys_argv):
    try:
        which_eng = sys_argv[1]
        production = True if which_eng == 'production' else False
    except:
        production = False
    return production

if __name__ == '__main__':
    # Connect to Data Warehouse
    production = which_db(argv)
    print('Connecting to our Data Warehouse')
    dw_engine = create_dw_engine(production=production)
    print('Loading Test Data')
    load_test(engine=dw_engine)
    print('Done :)\nHere are your datas')
    print(query_test(engine=dw_engine))