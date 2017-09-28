from sys import argv
from sqlalchemy.orm import sessionmaker
from create import create_dw_engine, which_db
from create import Test

def load_all_data(engine, session):
    load_test(Session())

def load_test(session):
    t1 = Test(first_name='Jim', last_name='Bob')
    t2 = Test(first_name='Bob', last_name='Jim')
    session.add(t1)
    session.add(t2)
    session.commit()


def query_test(engine):
    query = engine.execute('select * from test')
    return query.fetchall()

if __name__ == '__main__':
    # Connect to Data Warehouse
    production = which_db(argv)
    print('Connecting to our Data Warehouse')
    dw_engine = create_dw_engine(production=production)
    if len(dw_engine.table_names()) > 0:
        print('Loading Test Data')
        Session = sessionmaker(bind=dw_engine)
        load_test(engine=dw_engine, session=Session)
        print('Done :)\nHere are your datas')
        print(query_test(engine=dw_engine))
    else:
        print('Whoah there cowboy!\nThat DW is missing some tables.\nHow bout you try running create.py?')