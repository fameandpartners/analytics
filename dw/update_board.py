from sys import argv
from update import which_db, create_dw_engine
from jobs import load_daily_kpis, load_monthly_cohort_kpis

if __name__ == '__main__':
    production = which_db(argv)
    print('Re-Connecting to our Data Warehouse')
    dw_engine = create_dw_engine(production=production)
    print('Loading Daily KPIs')
    load_daily_kpis(engine=dw_engine)
    print('Loading Monthly KPIs')
    load_monthly_cohort_kpis(engine=dw_engine)
    print('Data Loaded into Warehouse')
