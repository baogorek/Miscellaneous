%run functions.py # lazy module loading
%run pandas_crossjoin_example.py

import numpy as np
import pandas as pd
from sklearn import ensemble, neighbors, linear_model, metrics, preprocessing
from sklearn.feature_extraction.text import CountVectorizer
from datetime import datetime
from scipy.stats.mstats import gmean, hmean
import glob, re
from matplotlib.backends.backend_pdf import PdfPages
import matplotlib

#matplotlib.interactive(True)
pd.set_option('display.max_columns', None)

# Storing all data as a dictionary
data = {
    'tra': pd.read_csv('input/air_visit_data.csv'),
    'as': pd.read_csv('input/air_store_info.csv'),
    'hs': pd.read_csv('input/hpg_store_info.csv'),
    'ar': pd.read_csv('input/air_reserve.csv'),
    'hr': pd.read_csv('input/hpg_reserve.csv'),
    'id': pd.read_csv('input/store_id_relation.csv'),
    'sub': pd.read_csv('input/sample_submission.csv'),
    'cal': (pd.read_csv('input/date_info.csv')
            .rename(columns = {'calendar_date' : 'visit_date'}))
}

cal_data = augment_calendar_data(data['cal'])

# Create a grid
cal_store_grid = (df_crossjoin(pd.DataFrame(data['tra'].air_store_id.unique()),
                              pd.DataFrame(cal_data.visit_date.unique()))
                  .reset_index(drop = True)
                  .rename(columns = {'0_x': 'air_store_id',
                                     '0_y': 'visit_date'}))
cal_store_grid.shape # 428,593 = 829 stores * 517 days
cal_data_grid = pd.merge(cal_store_grid, cal_data)

# Prepare training and submission data together
data['tra']['visit_date'] = pd.to_datetime(data['tra']['visit_date'])
data['sub']['visit_date'] = pd.to_datetime(data['sub']['id']
                                           .map(lambda x: str(x).split('_')[2]))
data['sub']['air_store_id'] = (data['sub']['id']
                               .map(lambda x: '_'.join(x.split('_')[:2])))
data['sub'].visitors = np.nan # So 0s are not used in computations

all_data = pd.concat([data['tra'], data['sub']]).reset_index(drop = True)
all_data.shape # 284,127 rows, not all stores have data for each combination

# all_data_grid has a record for every store date combination (428,593 rows)
all_data_grid = pd.merge(cal_data_grid, all_data, how = 'left',
                    on = ['air_store_id', 'visit_date'])
all_data_grid = all_data_grid.sort_values(['air_store_id', 'visit_date'])
all_data_grid.reset_index(drop = True, inplace = True)

first_valid_df = (all_data_grid
                    .groupby('air_store_id')
                    .apply(lambda x: x.visitors.first_valid_index())
                    .reset_index()
                    .rename(columns = {0: 'first_nonnan_idx'}))

assert(first_valid_df.loc[first_valid_df.air_store_id == "air_00a91d42b08b08d9",
                          "first_nonnan_idx"].iloc[0] == 182)

all_data_grid = pd.merge(all_data_grid, first_valid_df, how = "left")
all_data_grid = (all_data_grid.loc[all_data_grid.index.to_series() >=
                                   all_data_grid.first_nonnan_idx]
                              .drop('first_nonnan_idx', axis = 1)
                              .reset_index(drop = True))
# 329219
# all_data has a record for every store day combo with at least one customer
all_data = pd.merge(all_data, cal_data, how = 'inner', on = ['visit_date'])
all_data = all_data.sort_values(['air_store_id', 'visit_date'])

# Average based features ---------------------------------------------
# 1. Median of medians!

test_mm = score_median_of_medians(all_data)
sub_mofm = test_mm[['id', 'visitors']].copy()
sub_mofm.to_csv("submissions/sub_mofm.csv", index = False) # .56

# 2. Time-weighted average of visitors by store, dow, and weekday_holiday

all_data = add_weighted_mean_features(all_data)
all_data_grid = add_weighted_mean_features(all_data_grid)

test_wm = (all_data[all_data.id.isnull() == False].copy()
                                                  .reset_index(drop = True))
test_wm.visitors = np.expm1(test_wm.wmean_visitors)
sub_wm = test_wm[['id', 'visitors']].copy()
sub_wm.to_csv("submissions/sub_wm.csv", index = False) # .497

# Reservation Features -------------
# Throwing away data! HPG reservations without an air_store_id.
hpg_res = pd.merge(data['hr'], data['id'], how = 'inner',
                   on = ['hpg_store_id']).drop(['hpg_store_id'], axis = 1)

res_df = pd.concat([data['ar'].assign(source = "air"),
                    hpg_res.assign(source = "hpg")]).reset_index(drop = True)


res_df2 = process_reservations(res_df, cal_store_grid, cal_data_grid, 85)

pre_all_data_grid = all_data_grid.copy()
all_data = pd.merge(all_data, res_df2, on = ['air_store_id', 'visit_date'],
                    how = 'left')
all_data_grid = pd.merge(pre_all_data_grid, res_df2,
                    on = ['air_store_id', 'visit_date'], how = 'left')

# Kalman Filtering by store

unique_stores = data['sub']['air_store_id'].unique() # 821 unique stores

## Cross validation (didn't help me much)
B = 821 
cv_last_dt = '2017-03-01'

configs = [
 (0, 0, 0, .1, 0, 0) #.4915
]

configs = [
 (0, 0, 0, .1, 1., 1.) # .4760 with pctile 65, .4753 with 75, .4741 with 85, .4737 w/100
]

configs = [
 (0, 0, 0, .1, .5, 1.) # .4758
]

configs = [
 (0, 0, 0, .1, 1., .5),  #.4750
 (0, 0, 0, .1, .5, .5), # .4750 
 (0.00001, 0, 0, .5, 1., 1.), # .4746 
 (0.0001, 0, 0, .5, 1., 1.), # .4746 - improved it just a little!
 (0, 0, .00001, .5, 1., 1.), # .4745
 (0, 0, 0, .8, 1., 1.) , #.4744
 (0, 0, 0, 1.6, 1., 1.) #.4752
]

configs = [
 (0, 0, 0, 1.0, 1., 1.), # .4746
 (0, 0, 0, 1.3, 1., 1.), # .4749
 (0, 0, .0001, .5, 1., 1.), # .4742 just right fot mean
 (0, 0, .001, .5, 1., 1.), # .4745 a little too far
 (0, 0, .01, .5, 1., 1.), # .48 way too far!
 (.001, 0, 0, .5, 1., 1.), # .4746
 (.01, 0, 0, .5, 1., 1.)] #.4746

configs = [
 (.0001, 0, 0.0001, .5, 1., 1.), # .4742
 (.0001, 0.0001, 0.0001, .5, 1., 1.) # .4744
]

results = []

for config in configs:
  print config
  pdf = PdfPages('c:/devl/plots/kalman_cv.pdf')
  rmse_list = []
  for i in np.random.choice(range(len(unique_stores)), B, False):
  
    print(i)
    air_store_id = unique_stores[i]
    a_store = (all_data_grid.loc[all_data_grid.air_store_id == air_store_id]
                            .copy())
    val_visitors = a_store.loc[a_store.visit_date > cv_last_dt, 'visitors']
     
    a_store.loc[a_store.visit_date > cv_last_dt, 'visitors'] = np.nan
    if np.sum(~a_store.visitors.isnull()) > 30:
      #w, b, mu, s = config
      output = run_kalman(a_store, pdf, w = config[0], b = config[1],
                          mu = config[2], s = config[3],
                          tvp_b = config[4], tvp_mu = config[5])
      a_store = a_store.loc[a_store.visit_date >= output[3]] 
      cv_pred = output[0][(a_store.visit_date > cv_last_dt).values]
      cv_rmse = get_rmse(cv_pred, np.log1p(val_visitors))
      rmse_list.append(cv_rmse)
  
  pdf.close()
  results.append(np.mean(rmse_list))


# The real thing!

all_data_grid['visitors_k'] = np.nan
all_data_grid['rmse_k'] = np.nan
all_data_grid['aic_k'] = np.nan
pdf = PdfPages('c:/devl/plots/kalman8.pdf')

for i in range(len(unique_stores)):
    print i
    air_store_id = unique_stores[i]
    a_store = (all_data_grid.loc[all_data_grid.air_store_id == air_store_id]
                            .copy())

    output = run_kalman(a_store, pdf, w = 0.0, b = 0.0,
                        mu = 0.0, s = 0.02,
                        tvp_b = 1.0, tvp_mu = 1.0)
    all_data_grid.loc[(all_data_grid.air_store_id == air_store_id) & 
                      (all_data_grid.visit_date >= output[3]),
                      'visitors_k'] = output[0]
    all_data_grid.loc[all_data_grid.air_store_id == air_store_id,
                      'aic_k'] = output[1]
    all_data_grid.loc[all_data_grid.air_store_id == air_store_id,
                     'rmse_k'] = output[2]
   

pdf.close()

# Create submission for kalman
test_k = (all_data_grid[all_data_grid.id.isnull() == False]
          .copy().reset_index(drop = True))
test_k.visitors = np.expm1(test_k.visitors_k.clip(0, 14))
assert(sum(test_k.visitors.isnull()) == 0)
sub_k = test_k[['id', 'visitors']].copy()
sub_k.to_csv("submissions/sub_k17.csv", index = False)

test_k = test_k[["air_store_id", "visit_date", "visitors", "rmse_k", "aic_k"]]

## Geometric Mean

filenames = ["SubmissonK.csv", "nitin_surya2.csv", "sub_k17.csv"]
test_multi = get_multisub(filenames)

test_multi['visitors'] = gmean(test_multi.iloc[:, 1:], 1)

test_multi_out = test_multi[["id", "visitors"]]

test_multi_out.to_csv("submissions/test_multi_out9.csv", index = False) #.485

