from models import Test
from sqlalchemy.orm import sessionmaker

def load_test(engine):
    Session = sessionmaker(bind=engine)
    session = Session()
    t1 = Test(first_name='Jim', last_name='Bob')
    t2 = Test(first_name='Bob', last_name='Jim')
    session = Session()
    if 'test' in engine.table_names():
        Test.__table__.drop(engine)
    Test.metadata.create_all(engine)
    session.add(t1)
    session.add(t2)
    session.commit()