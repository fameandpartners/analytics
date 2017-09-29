from feather import read_dataframe
from sqlalchemy.orm import sessionmaker
from sqlalchemy.schema import CreateTable, DropTable
from models import Test

def load_test(engine):
    Session = sessionmaker(bind=engine)
    t1 = Test(first_name='Jim', last_name='Bob')
    t2 = Test(first_name='Bob', last_name='Jim')
    session = Session()
    if Test.__tablename__ in engine.table_names():
        session.execute(DropTable(Test.__table__))
    session.execute(CreateTable(Test.__table__))
    session.add(t1)
    session.add(t2)
    session.commit()

def load_sales():
    sales = read_dataframe('feathers/sales.feather')
    return sales