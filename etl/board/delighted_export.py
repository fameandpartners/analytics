import delighted
import pandas as pd

def pull(DKEY):
    print('Querying Delighted NPS Responses')
    delighted.api_key = DKEY
    responses = []
    for page in range(1, 100):
        print('Querying Page: ' + str(page))
        delighted_page = delighted.SurveyResponse.all(per_page=100,page=page)
        if len(delighted_page) > 0:
            for response in delighted_page:
                responses.append(dict(response))
        else:
            break
    return pd.DataFrame(responses)