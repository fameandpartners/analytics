import pandas as pd

def parse_fabrics():
    f = pd.ExcelFile('~/Downloads/Fabric_styles_with_factory_name.xlsx')
    fz = f.parse('Zhesi\'s')
    fm = f.parse('Milly\'s')

    def clean_fabrics(x):
        return str(x).replace('#','').upper()

    fz['main'] = fz['main fabric '].apply(lambda x: clean_fabrics(x))
    fm['main'] = fm['main fabric '].apply(lambda x: clean_fabrics(x))

    fz['lining'] = fz['Lining fabric'].apply(lambda x: clean_fabrics(x))
    fm['lining'] = fm['Lining fabric'].apply(lambda x: clean_fabrics(x))

    fz['interlining'] = fz['interlining fabric'].apply(lambda x: clean_fabrics(x))
    fm['interlining'] = None
    fm['interlining_usage'] = None

    fz['contrast_1'] = fz['contrast fabric'].apply(lambda x: clean_fabrics(x))
    fm['contrast_1'] = fm['contrast 1'].apply(lambda x: clean_fabrics(x))

    fz['contrast_2'] = fz['contrast fabric (2)'].apply(lambda x: clean_fabrics(x))
    fm['contrast_2'] = fm['contrast 2'].apply(lambda x: clean_fabrics(x))

    fz['contrast_3'] = fz['contrast fabric (3)'].apply(lambda x: clean_fabrics(x))
    fm['contrast_3'] = None
    fm['contrast_3_usage'] = None

    all_columns = ['product_id','main','main_usage','lining','lining_usage',
                   'interlining','interlining_usage','contrast_1',
                   'contrast_1_usage','contrast_2','contrast_2_usage',
                   'contrast_3','contrast_3_usage']

    df = pd.concat(
        [
            fz.rename(columns={
                'main fabric usage(M/PC)':'main_usage',
                'lining fabric usage(M/PC)':'lining_usage',
                'contrast fabric usage(M/PC)':'contrast_1_usage',
                'contrast fabric usage(M/PC).1':'contrast_2_usage',
                'contrast fabric usage(M/PC).2':'contrast_3_usage',
                'interlining fabric usage(M/PC)':'interlining_usage'
            })[all_columns],
            fm.rename(columns={
                'main fabric usage(M/PC)':'main_usage',
                'lining fabric usage(M/PC)':'lining_usage',
                'usage(m/pc)':'contrast_1_usage',
                'usage(m/pc).1':'contrast_2_usage'
            })[all_columns],
        ]
    )
    def melt_fabrics(df, fabric, usage):
        return df.melt(id_vars=['product_id'],value_vars=[fabric])\
                 .rename(columns={'variable':'fabric_type','value':'fabric'})\
                 .merge(df.melt(id_vars=['product_id'],
                                 value_vars=[usage])\
                          .rename(columns={'variable':'usage_type','value':'usage'}),
                         on='product_id')
    df = pd.concat(
            [
                melt_fabrics(df, fabric='main', usage='main_usage'),
                melt_fabrics(df, fabric='lining', usage='lining_usage'),
                melt_fabrics(df, fabric='interlining', usage='interlining_usage'),
                melt_fabrics(df, fabric='contrast_1', usage='contrast_1_usage'),
                melt_fabrics(df, fabric='contrast_2', usage='contrast_2_usage'),
                melt_fabrics(df, fabric='contrast_3', usage='contrast_3_usage'),
            ]
        )

    df = df[(df.usage > 0) & (df.product_id > 0)]
    return df[['product_id','fabric_type','fabric','usage']]

if __name__ == '__main__':
    parse_fabrics().to_csv('~/data/fabric_usage.csv', index_label=False)
