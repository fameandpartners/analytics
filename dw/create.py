from sys import argv

from sqlalchemy import create_engine
from sqlalchemy import MetaData
from sqlalchemy import Table, Column
from sqlalchemy import Integer, String

from secrets import DWPASS


def create_dw_engine(production = False):
    if production:
        password = DWPASS
        host = 'fp-dw.cs60zg0gg0au.us-east-1.rds.amazonaws.com'
        engine = create_engine(f'postgresql+psycopg2://peter:{password}@{host}:5432/dw')
    else:
        host = 'localhost'
        password = ''
        engine = create_engine(f'postgresql+psycopg2://peterh:{password}@{host}:5432/dw_dev')
    return engine


def create_dw_tables(engine):
    metadata = MetaData()
    test_table = Table('test', metadata,
                    Column('id', Integer, primary_key=True),
                    Column('name', String)
                )
    test_table.create(engine)

if __name__ == '__main__':
    which_eng = argv[1]
    production = True if which_eng == 'production' else False
    print('Connecting to our Data Warehouse')
    engine = create_dw_engine(production=production)
    if len(engine.table_names()) == 0:
        print('Creating tables in our Data Warehouse')
        create_dw_tables(engine=engine)
        print('Done :)')
    else:
        print(f'Hey! There are already tables in {which_eng} bud.\nMaybe you meant to run update.py?')
