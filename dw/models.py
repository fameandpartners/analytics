from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import Column, Integer, String

Base = declarative_base()

class Test(Base):
     __tablename__ = 'test'

     id = Column(Integer, primary_key=True)
     first_name = Column(String)
     last_name = Column(String)

     def __repr__(self):
        return "<User(first_name='%s', last_name='%s')>" % (self.name, self.fullname)
