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

    nps_df = pd.merge(scores, acquisitions, on='email')\
           .merge(cohorts, on='email', how='left')\
           .fillna('Not Assigned')
    nps_df['year_month'] = nps_df['first_order_date'].apply(lambda d: d.isoformat()[:7])
    nps_df = nps_df.groupby(['year_month','assigned_cohort'])\
           .agg([nps_score, promoter_count, detractor_count, len])\
           .reset_index()
    nps_df.columns = pd.Index(['year_month','Cohort','Score','Promoters','Detractors','Responses',], dtype='object')

    nps_df = nps_df[nps_df.year_month.str.contains('2017')]
    nps_df = nps_df.melt(id_vars=['year_month','Cohort',],
                 value_vars=['Score','Promoters','Detractors','Responses',])\
           .rename(columns={'variable':'C','Cohort':'D'})
    nps_df['A'] = 'NPS Score'
    nps_df['B'] = 'Direct'
    return nps_df

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
