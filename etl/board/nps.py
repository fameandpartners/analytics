import delighted
import pandas as pd

from secrets import DKEY
import spree_db.sales as sales


def pull():
    delighted.api_key = DKEY
    responses = []
    for page in range(1, 1000):
        delighted_page = delighted.SurveyResponse.all(
            per_page=100,page=page,expand=['person'])
        if len(delighted_page) > 0:
            for response in delighted_page:
                response['email'] = response.person.email
                responses.append(dict(response))
        else:
            break

    scores = pd.DataFrame(responses)[['email','score']]
    acquisitions = sales.pull_acquisitions()
    cohorts = sales.pull_cohort_assignments_csv()

    df = pd.merge(scores, acquisitions, on='email')\
           .merge(cohorts, on='email', how='left')\
           .fillna('Not Assigned')

    df['year_month'] = df['first_order_date'].apply(lambda d: d.isoformat()[:7])
    df = df.rename(columns={'assigned_cohort':'Cohort'})\
           .groupby(['year_month','Cohort'])\
           .agg([nps_score, promoter_count, detractor_count, len])\
           .reset_index()
    df.columns = pd.Index(['year_month','Cohort','NPS Score','Promoters','Detractors','Responses',], dtype='object')
    return df[df.year_month.str.contains('2017')]

def promoter_count(scores):
    return sum(score >= 9 for score in scores)

def detractor_count(scores):
    return sum(score <= 6 for score in scores)

def nps_score(scores):
    """Takes a list of NPS responses [0-10] and returns the NPS score for that
    list of responses.
    """
    responses = len(scores)
    promoters = promoter_count(scores)
    detractors = detractor_count(scores)
    return 100 * ((promoters / responses) - (detractors / responses))
