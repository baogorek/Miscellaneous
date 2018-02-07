import pandas as pd
import numpy as np
from sklearn import ensemble, neighbors, linear_model, metrics, preprocessing
from datetime import datetime
import glob, re
import statsmodels.api as sm
from matplotlib import pyplot as plt
import pdb
import datetime
import warnings
from os.path import splitext
from scipy.stats.mstats import gmean

warnings.filterwarnings("ignore", category=DeprecationWarning)


def get_multisub(filenames):
    subs = [pd.read_csv("submissions/" + f) for f in filenames]

    test_multi = reduce(lambda left, right: pd.merge(left, right, on = 'id'), subs)
    test_multi.columns = ['id'] + [splitext(f)[0] for f in filenames]
    return test_multi

def get_rmse(pred, actual):
  return np.sqrt(np.nanmean((pred - actual) ** 2))

def augment_calendar_data(cal_df):
  cal_df['visit_date'] = pd.to_datetime(cal_df['visit_date'])
  cal_df['weekend_flg'] = np.logical_or(
    cal_df['day_of_week'] == 'Saturday',
    cal_df['day_of_week'] == 'Sunday')

  cal_df['weekday_holiday_flg'] = np.logical_and(
    cal_df['holiday_flg'],
    np.logical_not(cal_df['weekend_flg']))

  cal_df['weekend_holiday_flg'] = np.logical_and(
       cal_df['holiday_flg'], cal_df['weekend_flg'])
 
  cal_df['date_int'] = (cal_df['visit_date']
                          .apply(lambda x: x.strftime('%Y%m%d')).astype(int))

  # Exponentially decaying weights with dates closer to present near 1.0
  cal_df['weight'] = ((cal_df.index + 1.0) / len(cal_df)) ** 5
  cal_df['year'] = cal_df['visit_date'].dt.year
  cal_df['month'] = cal_df['visit_date'].dt.month
  return cal_df


def process_reservations(res_df, cal_store_grid, cal_data_grid, pct_thresh = 65):

  # Good example of one with consistant reservations throughout the year
  #res_grid = res_grid.loc[res_grid.air_store_id == "air_a24bf50c3e90d583"]

  # Good example of one with big 0 patch: air_03963426c9312048, loc 144 to 300
  #res_grid = res_grid.loc[res_grid.air_store_id == "air_03963426c9312048"]

  # Good example of one with no reservations: air_049f6d5b402a31b2
  #res_grid = res_grid.loc[res_grid.air_store_id == "air_049f6d5b402a31b2"]

  # Aggregating reservations
  res_df[['reserve_datetime', 'visit_datetime']] = (
    res_df[['reserve_datetime', 'visit_datetime']].apply(pd.to_datetime))
  
  res_df['visit_date'] = pd.to_datetime(res_df['visit_datetime'].dt.date)
  res_df['reserve_date'] = pd.to_datetime(res_df['reserve_datetime'].dt.date)
  
  diff_dates = res_df['visit_date'] - res_df['reserve_date']
  
  res_df['reserve_diff_days'] = diff_dates.apply(lambda x: x.days)

  f = (lambda x, y: np.where((res_df['reserve_diff_days'] >= x) &
                             (res_df['reserve_diff_days'] <= y),
                             res_df['reserve_visitors'], 0))
  
  new_cols = []
  for i in range(0, 39):
    new_name = 'res_' + str(i) + 'd'
    new_cols.append(new_name)
    res_df[new_name] = f(0, i)

  res_df['res_39+d'] = f(40, 99999)

  res_pivoted = res_df.pivot_table(
    index = ['air_store_id', 'visit_date'],
    values = ['reserve_visitors'] + new_cols + ['res_39+d'],
    aggfunc = 'sum')

  res_pivoted.reset_index(inplace = True) 
 
  # Res grid is expanded to all dates and all stores, regardless of the data present 
  res_grid = pd.merge(cal_store_grid, res_pivoted,
                      on = ['air_store_id', 'visit_date'], how = 'left')
  
  res_grid.fillna(0, inplace = True) # But not all should be zero!
  res_grid = pd.merge(res_grid, cal_data_grid, how = "inner")

  # Setting up two variables for storage
  res_grid['reserves_est'] = res_grid.reserve_visitors
  res_grid['keep'] = 1.0

  #res_grid = res_grid.loc[(res_grid.air_store_id == "air_a24bf50c3e90d583") | 
  #                        (res_grid.air_store_id == 'air_082908692355165e')]

  res_grid_train = res_grid.loc[res_grid.visit_date < '2017-04-23']
  
  before_total_reservations = np.sum(res_grid_train.reserve_visitors)

  # Zero out weights associated with a reservation gap
  unique_stores = res_grid_train['air_store_id'].unique()
  cuml_deletes = 0
  delete_instances = 0
  for i in range(len(unique_stores)):
    air_store_id = unique_stores[i]
    one_store = res_grid_train.loc[res_grid_train.air_store_id == air_store_id]
    first_good_index = find_zero_streak(one_store, 'reserve_visitors')

    # Zero out weights (not reservations!) before a "first good index"
    if (first_good_index > 0) and (first_good_index < one_store.shape[0]):
      first_good_dt = one_store.iloc[first_good_index].visit_date
      res_grid_train.loc[(res_grid_train.air_store_id == air_store_id) &
                         (res_grid_train.visit_date < first_good_dt),
                         "weight"] = 0.0
      print("zeroing out weights before %s for %s" % (str(first_good_dt),
                                                      air_store_id))

    recent_reserve_cts = (one_store.loc[(one_store.visit_date > '2017-01-01') &
                                        (one_store.visit_date < '2017-04-23'),
                                        "reserve_visitors"])
    recent_sum = np.sum(recent_reserve_cts)
    recent_pctile = np.percentile(recent_reserve_cts, pct_thresh) 
    zero_if = (first_good_index > 408) or (recent_sum < 30) or (recent_pctile == 0)
    # For inconsistant or very few reservations, delete them all!
    if ((np.sum(one_store.reserve_visitors) > 0) and zero_if):
      delete_instances += 1
      print delete_instances
      print("Completely zeroing out reservations associated with")
      print("%s with first good index %d, sum_recent %d recent pctile %.2f" %
            (air_store_id, first_good_index, recent_sum, recent_pctile))
      cuml_deletes += np.sum(res_grid_train.loc[
                             (res_grid_train.air_store_id == air_store_id),
                             "reserve_visitors"])
      print(cuml_deletes)
      res_grid.loc[(res_grid.air_store_id == air_store_id), "keep"] = 0.0
    
  # Done with store loop. Now onto extra days loop
  for i in range(39): 
    future_visit_dt = pd.to_datetime('2017-04-23') + datetime.timedelta(days=i)
    print("processing reservation means for " + str(future_visit_dt))

    cuml_var = 'res_' + str(i) + 'd'
    wmean_df = weighted_mean_pred(res_grid_train, cuml_var)
   
    res_grid = pd.merge(res_grid, wmean_df, how = 'left',
                        on = ['air_store_id', 'day_of_week', 'weekday_holiday_flg'])
    
    res_grid.loc[res_grid.visit_date == future_visit_dt, "reserves_est"] = (
      res_grid.loc[res_grid.visit_date == future_visit_dt, 'wmean_' + cuml_var] +
      res_grid.loc[res_grid.visit_date == future_visit_dt, "reserve_visitors"]
    )
   
  print("Checking before & after log1p mean reservation counts") 
  for i in range(len(unique_stores)):
    
    air_store_id = unique_stores[i]
    mean_train = np.mean(np.log1p(
                         res_grid.loc[(res_grid.air_store_id == air_store_id) &
                                      (res_grid.visit_date > '2017-01-01') &
                                      (res_grid.visit_date < '2017-04-23'),
                                      "reserves_est"]))
    mean_test = np.mean(np.log1p(res_grid.loc[(res_grid.air_store_id == air_store_id) &
                                      (res_grid.visit_date >= '2017-04-23'),
                                      "reserves_est"]))
    print("air_store_id %s: mean train reserves: %.2f, mean test reserves: %.2f" % 
          (air_store_id, mean_train, mean_test))
    assert(abs(mean_train - mean_test) < .75)

  assert(np.sum(res_grid.loc[res_grid.visit_date < '2017-04-23', 'reserves_est']) ==
         before_total_reservations)

  # Where the 0 resetting happens
  res_grid['reserve_visitors'] = res_grid['reserves_est'] * res_grid['keep']

  after_total_reservations = np.sum(res_grid.loc[res_grid.visit_date < '2017-04-23',
                                    "reserve_visitors"])
  assert(before_total_reservations - after_total_reservations == cuml_deletes)

  res_out = res_grid[['air_store_id', 'visit_date', 'reserve_visitors']]
  return res_out

def weighted_mean_pred(dat, var):

  w_mean_l1p = lambda df: (0 if df['weight'].sum() == 0 else
                           ((df['weight'] * np.log1p(df[var])).sum() /
                            df['weight'].sum()))
  df = dat[['air_store_id', 'day_of_week','weekday_holiday_flg', 'weight', var]]

  
  wmean_df = (df.groupby(['air_store_id', 'day_of_week', 'weekday_holiday_flg'])
                .apply(w_mean_l1p)
                .reset_index())
  new_name = 'wmean_' + var
  wmean_df.rename(columns = {0:new_name}, inplace = True)

  wmean_df[new_name] = np.expm1(wmean_df[new_name])
  return wmean_df
 
def find_zero_streak(dat, var, max_streak_tol = 50):
  
  streak = (dat.groupby( (dat[var] != 0).cumsum()).cumcount() +
            ( (dat[var] != 0).cumsum() == 0).astype(int))
  past_tol = streak > max_streak_tol
  last_index_past_tol = -1
  if np.sum(past_tol) > 0:
    last_index_past_tol = np.max(np.argwhere(past_tol))
  return last_index_past_tol + 1 # first non_zero_value

class CustomStateSpaceExog(sm.tsa.statespace.MLEModel):
    def __init__(self, endog, design,
                 initial_state = [18, 5, -12, -8, 0, 0, 0, 0, 0],
                 initial_state_cov = np.eye(11),
                 initial_params = [1, 1, 1],
                 estimate_params = True):
        # The Super class is MLEModel, not Representation
        super(CustomStateSpaceExog, self).__init__(endog,
                                               k_states = 11,
                                               k_posdef = 3,
                                               initialization = 'known',
                             initial_state = initial_state,
                             initial_state_cov = initial_state_cov)
        self.initial_params = initial_params
        self.estimate_params = estimate_params
        self['design'] = design
        seasonal_design = np.array([[1,  0,  0,  0,  0,  0,  0],
                                    [0, -1, -1, -1, -1, -1, -1],
                                    [0,  1,  0,  0,  0,  0,  0],
                                    [0,  0,  1,  0,  0,  0,  0],
                                    [0,  0,  0,  1,  0,  0,  0],
                                    [0,  0,  0,  0,  1,  0,  0],
                                    [0,  0,  0,  0,  0,  1,  0]])
        # Parameters that are static over time 
        my_design = np.pad(seasonal_design, ((0, 2), (0, 2)),
                           mode = "constant")
        np.fill_diagonal(my_design[-2:, -2:], 1)
        
        # Parameters that change over time
        my_design = np.pad(my_design, ((2, 0), (2, 0)),
                                    mode = "constant")
        np.fill_diagonal(my_design[0:2, 0:2], 1)
        self['transition'] = my_design
        
        # R matrix that's multiplied by the state equation's error in state eq
        self['selection', 0, 0] = 1
        self['selection', 1, 1] = 1
        self['selection', 2, 2] = 1

        if not estimate_params:
            self['state_cov', 0, 0] = initial_params[0]
            self['state_cov', 1, 1] = initial_params[1]
            self['state_cov', 2, 2] = initial_params[2]
            self['obs_cov', 0, 0] = initial_params[3]

    def update(self, params, transformed = True, **kwargs):
        params = super(CustomStateSpaceExog, self).update(params, transformed,
                                                          **kwargs)
        if self.estimate_params:
            self['state_cov', 0, 0] = params[0]
            self['state_cov', 1, 1] = params[1]
            self['state_cov', 2, 2] = params[2]
            self['obs_cov', 0, 0] = params[3]
        else:
            pass

    @property
    def start_params(self):
        return self.initial_params

    def transform_params(self, unconstrained):
        return unconstrained**2

    def untransform_params(self, constrained):
        return constrained**0.5


class CustomStateSpaceExog2(sm.tsa.statespace.MLEModel):
    def __init__(self, endog, design,
                 initial_state,
                 initial_state_cov,
                 initial_params,
                 estimate_params = True
                 ):
        # The Super class is MLEModel, not Representation
        super(CustomStateSpaceExog2, self).__init__(endog,
                                               k_states = 11,
                                               k_posdef = 11,
                                               initialization = 'known',
                             initial_state = initial_state,
                             initial_state_cov = initial_state_cov)
        self.estimate_params = estimate_params
        self.initial_params = initial_params
        self['design'] = design
        seasonal_design = np.array([[1,  0,  0,  0,  0,  0,  0],
                                    [0, -1, -1, -1, -1, -1, -1],
                                    [0,  1,  0,  0,  0,  0,  0],
                                    [0,  0,  1,  0,  0,  0,  0],
                                    [0,  0,  0,  1,  0,  0,  0],
                                    [0,  0,  0,  0,  1,  0,  0],
                                    [0,  0,  0,  0,  0,  1,  0]])
        # Parameters that are static over time 
        my_design = np.pad(seasonal_design, ((0, 2), (0, 2)),
                           mode = "constant")
        np.fill_diagonal(my_design[-2:, -2:], 1)
        
        # Parameters that change over time
        my_design = np.pad(my_design, ((2, 0), (2, 0)),
                                    mode = "constant")
        np.fill_diagonal(my_design[0:2, 0:2], 1)
        self['transition'] = my_design
        
        # R matrix that's multiplied by the state equation's error in state eq
        self['selection', 0, 0] = 1
        self['selection', 1, 1] = 1
        self['selection', 2, 2] = 1

        self['state_cov', 0, 0] = initial_params[0]
        self['state_cov', 1, 1] = initial_params[1]
        self['state_cov', 2, 2] = initial_params[2]
        self['state_cov', 3, 3] = initial_params[3]
        self['state_cov', 4, 4] = initial_params[4]
        self['state_cov', 5, 5] = initial_params[5]
        self['state_cov', 6, 6] = initial_params[6]
        self['state_cov', 7, 7] = initial_params[7]
        self['state_cov', 8, 8] = initial_params[8]
        self['state_cov', 9, 9] = initial_params[9]
        self['state_cov', 10, 10] = initial_params[10]

        self['obs_cov', 0, 0] = initial_params[11]

    def update(self, params, transformed = True, **kwargs):
        params = super(CustomStateSpaceExog2, self).update(params, transformed,
                                                          **kwargs)
        if self.estimate_params:
            self['obs_cov', 0, 0] = params[11]
        else:
            pass

    @property
    def start_params(self):
        return self.initial_params

    def transform_params(self, unconstrained):
        return unconstrained**2

    def untransform_params(self, constrained):
        return constrained**0.5


# Next case to explore: air_1653a6c513865af3, where predictions drop off
#  to near nothing, but not exactly nothing! and so future predictions drop too much

def starting_linreg(one_store):
    
    mask = ~np.isnan(one_store.visitors)
    x = one_store.total_log1p_reserves[mask].values
    intercept_1 = np.float64(x > .69)
    X = np.stack((intercept_1, x, one_store.weekday_holiday_flg[mask],
                 one_store.weekend_holiday_flg[mask]), axis = 1)
    
    regr = linear_model.LinearRegression(fit_intercept = True)
    out = regr.fit(X,
             np.log1p(one_store.visitors)[mask])
    beta = modify_beta_if_needed(out.coef_)
    intr = out.intercept_
    intr = intr if intr < 7 else np.log1p(one_store.visitors)
    pred = out.predict(X)
    return (pred, intr, beta)
   

def run_kalman(one_store, pdf, w = 0, b = 0, mu = 0, s = 0,
               tvp_b = 1, tvp_mu = 1):
    use_red = False

    air_store_id = one_store.air_store_id.values[0]
    one_store.loc[:, 'total_log1p_reserves'] = np.log1p(one_store.reserve_visitors) 

    pred, intr, beta = starting_linreg(one_store) 
    if (one_store.shape[0] > 240) and (np.max(one_store.reserve_visitors) > 0): 
      pred_red, intr_red, beta_red = starting_linreg(one_store.iloc[120:])
      use_red = (beta_red[1] > beta[1] * 2) & (beta_red[1] > .1)


    if use_red:
        print "beta doubles in last 120 days, taking subset..."
        one_store = one_store.iloc[120:]
        pred = pred_red
        intr = intr_red
        beta = beta_red


    mask = ~np.isnan(one_store.visitors)
    plt.scatter(one_store.total_log1p_reserves[mask],
                np.log1p(one_store.visitors)[mask])

    plt.scatter(one_store.total_log1p_reserves[mask],  pred,
                color = 'red')
    plt.title(air_store_id + ": " + str(np.round(intr, 2)) +
              ", " + str(np.round(beta, 2)))
    pdf.savefig()
    plt.close()
    
    # Complex kalman 
    model_type = "complex"
    x = np.array([[0., 0, 1., 1, 0, 0, 0, 0, 0, 0, 0]])
    reg_design = np.transpose(np.repeat(x, one_store.shape[0], axis = 0))
   
    reg_design[0, :] = one_store.total_log1p_reserves.values > .69
    reg_design[1, :] = one_store.total_log1p_reserves.values
    reg_design[9, :] = one_store.weekend_holiday_flg
    reg_design[10, :] = one_store.weekday_holiday_flg

    reg_design = np.reshape(reg_design, (1, 11, one_store.shape[0]))
    
    my_css = CustomStateSpaceExog(np.log1p(one_store.visitors.values),
                                  reg_design,
                              np.array([beta[0], beta[1], intr,
                                        0, 0, 0, 0, 0, 0, beta[3], beta[2]]),
                              np.diag(np.repeat(.5, 11)),
                              [0.01, 0.01, .05, .25],
                              estimate_params = True)
    # 'newton', 'nm', 'bfgs', 'lbfgs', 'powell', 'cg', 'ncg', 'basinhopping' 
    res = my_css.fit(method = 'nm', maxiter = 3000)
    pred = res.predict()
    
    rmse = get_rmse(pred, np.log1p(one_store.visitors))
    print("RMSE of full model is %.3f" % rmse)
    
    #w = .0001
    #b = .0005 
    #mu = .001
    #s = 0
    my_css2 = CustomStateSpaceExog2(np.log1p(one_store.visitors.values),
                                  reg_design,
                              np.array([-.5, .3, intr,
                                        0, 0, 0, 0, 0, 0, beta[3], beta[2]]),
                              np.diag(np.repeat(.5, 11)),
                              [(res.params[0] + b) * tvp_b, (res.params[1] + b) * tvp_b,
                               (res.params[2] + mu) * tvp_mu,
                               w, w, w, w, w, w,                             
                               tvp_b * b, tvp_b * b, res.params[3] + s],
                              False)

    res = my_css2.fit(method = 'nm', maxiter = 3000)
    pred = res.predict()
    rmse = np.round(np.sqrt(np.nanmean((pred - np.log1p(one_store.visitors)) ** 2)), 3)
    print("RMSE of regularized model is %.3f" % rmse)

    go_simple = (np.max(one_store.reserve_visitors) == 0) or (beta[1] < -.05)

    if go_simple:
      model_type = "simple"
      
      print "Switching to simple"

      # Simple kalman 
      x_s = np.array([[1., 1, 0, 0, 0, 0, 0, 0, 0]])
      reg_design_s = np.transpose(np.repeat(x_s, one_store.shape[0], axis = 0))
   
      reg_design_s[7, :] = one_store.weekend_holiday_flg
      reg_design_s[8, :] = one_store.weekday_holiday_flg

      reg_design_s = np.reshape(reg_design_s, (1, 9, one_store.shape[0]))
      
      my_css = SimpleStateSpaceExog(np.log1p(one_store.visitors.values),
                                    reg_design_s,
                                np.array([intr,
                                          0, 0, 0, 0, 0, 0, beta[3], beta[2]]),
                                np.diag(np.repeat(.5, 9)),
                                [.01, .25],
                                estimate_params = True)
      # 'newton', 'nm', 'bfgs', 'lbfgs', 'powell', 'cg', 'ncg', 'basinhopping' 
      res = my_css.fit(method = 'nm', maxiter = 3000)
      pred = res.predict()
      rmse = np.round(np.sqrt(np.nanmean((pred - np.log1p(one_store.visitors)) ** 2)), 3)
      print("RMSE of reduced model is %.3f" % rmse)
      my_css2 = CustomStateSpaceExog2(np.log1p(one_store.visitors.values),
                                    reg_design,
                                np.array([0, 0, intr,
                                          0, 0, 0, 0, 0, 0, beta[3], beta[2]]),
                                np.diag(np.repeat(.5, 11)),
                                [0, 0,
                                 tvp_mu * (res.params[0] + mu),
                                 w, w, w, w, w, w,                             
                                 tvp_b * b, tvp_b * b, res.params[1] + s],
                                False)

      res = my_css2.fit(method = 'nm', maxiter = 3000)
      pred = res.predict()
      rmse = np.round(np.sqrt(np.nanmean((pred - np.log1p(one_store.visitors)) ** 2)), 3)
      print("RMSE of simple regularized model is %.3f" % rmse)


    # switch to weighted mean - hasn't worked yet
    #pred = one_store.wmean_visitors.values
    #rmse = np.round(np.sqrt(np.nanmean((pred - np.log1p(one_store.visitors)) ** 2)), 3)

    plt.plot(np.log1p(one_store.visitors.values))
    plt.plot(pred, linewidth = .5)
    plt.plot(one_store.total_log1p_reserves.values / 5.0)      
    final_params = str(np.round(res.params, 3))
    plt.title(air_store_id + ", RMSE:" + str(rmse) + "\nparams:" + final_params +
            ", model_type: " + model_type + ", use_red: " + str(use_red))
    
    pdf.savefig()
    plt.close()

    out = (pred, res.aic, rmse, np.min(one_store.visit_date))
    return out
    #new_min_index = get_last_changepoint(res.resid, 7)
    #if ((one_store.shape[0] - new_min_index < 150) | (new_min_index == 0)):
    #    print "No new changepoints found. Returning predictions"
    #    return out
    #else:
    #    print (air_store_id + ": Going around again! new min index is " +
    #           str(new_min_index))
    #    return run_kalman(one_store.iloc[new_min_index:, :], pdf)

def modify_beta_if_needed(beta):
    return np.array([b if abs(b) < 5 else 0 for b in beta])


def get_last_changepoint(resid, max_days_over = 5, plot = False):
    # Poor man's changepoint detection
    m_rngs = np.abs(np.diff(resid))
    ucl = 3.267 * np.nanmean(m_rngs)

    if plot:
      plt.plot(m_rngs)
      plt.axhline(y = ucl)

    days_over = np.nan_to_num(pd.rolling_sum(m_rngs > ucl, 30))
    max_arg_over = 0
    args_where_over = np.argwhere(days_over > max_days_over)
    if len(args_where_over) > 0:
      max_arg_over = np.max(args_where_over) + 1
      
    return max_arg_over


def score_median_of_medians(all_data):
  test_mm = (all_data[all_data.id.isnull() == False].copy()
                                                    .reset_index(drop = True))
  test_mm.shape # 821 stores * 39 days (fewer stores than training set!
  
  r = 1.61803398875
  windows = np.round(r**np.arange(0, 9) * 7).astype(int)
  
  train = all_data[all_data.id.isnull()].copy().reset_index(drop = True)
  train_dates = np.unique(train.visit_date)
  
  col_list = []
  for win_len in windows:
    val = 'MW' + str(win_len)
    col_list.append(val)
    min_date = np.datetime64(train_dates[-win_len])
    tmp1 = (train.loc[train.visit_date >= min_date]
                  [['air_store_id', 'weekend_flg', 'visitors']]
                 .groupby(['air_store_id', 'weekend_flg'])
                 .median()
                 .rename(columns = {'visitors': val})
                 .reset_index())
    test_mm = test_mm.merge(tmp1, how = 'left')
  
  complete = (test_mm[['air_store_id'] + col_list]
               .groupby(['air_store_id'])
               .fillna(method = 'bfill')
               .copy())
  
  test_mm['visitors'] = complete[col_list].median(axis = 1)
  return test_mm  

def add_weighted_mean_features(all_data):
  w_mean_l1p = lambda x: (x.weight * np.log1p(x.visitors)).sum() / x.weight.sum()
  
  wmean_visitors_df = (all_data.loc[all_data.visitors.notnull(),
                                    ['air_store_id', 'day_of_week',
                                     'weekday_holiday_flg', 'weight',
                                     'visitors']]
                     .groupby(['air_store_id', 'day_of_week',
                               'weekday_holiday_flg'])
                     .apply(w_mean_l1p)
                     .reset_index())
  wmean_visitors_df.rename(columns = {0:'wmean_visitors'}, inplace = True)
  
  all_data = pd.merge(all_data, wmean_visitors_df, how = 'left',
                   on = ['air_store_id', 'day_of_week', 'weekday_holiday_flg'])
  
  # Imputation of the weighted mean feature by two levels of backoff
  # Backoff level one - backoff to a (store_id, day_of_week) slice
  wmean_visitors_bk1 = (
    wmean_visitors_df[wmean_visitors_df.weekday_holiday_flg == 0]
    .rename(columns = {'wmean_visitors': 'wmean_visitors_bk1'})
    .drop(['weekday_holiday_flg'], axis = 1))
  
  # Backoff level two - backoff to (store_id) level mean of weighted mean
  wmean_visitors_bk2 = (wmean_visitors_df[['air_store_id', 'wmean_visitors']]
                        .groupby('air_store_id')
                        .mean()
                        .reset_index()
                        .rename(columns = {'wmean_visitors':
                                           'wmean_visitors_bk2'}))
  all_data = pd.merge(all_data, wmean_visitors_bk1, how = 'left',
                      on = ['air_store_id', 'day_of_week'])
  
  all_data = pd.merge(all_data, wmean_visitors_bk2, how = 'left',
                      on = ['air_store_id'])
  
  # Imputation by backoff
  missings = all_data.wmean_visitors.isnull()
  sum(missings)
  
  all_data.loc[missings, 'wmean_visitors'] = all_data.loc[missings,
                                                          'wmean_visitors_bk1']
  
  missings = all_data.wmean_visitors.isnull()
  all_data.loc[missings, 'wmean_visitors'] = all_data.loc[missings,
                                                          'wmean_visitors_bk2']
  
  missings = all_data.wmean_visitors.isnull()
  assert(sum(missings) == 0)
  return all_data  


class SimpleStateSpaceExog(sm.tsa.statespace.MLEModel):
    def __init__(self, endog, design,
                 initial_state = [18, 5, -12, -8, 0, 0, 0, 0, 0],
                 initial_state_cov = np.eye(11),
                 initial_params = [1, 1, 1],
                 estimate_params = True):
        # The Super class is MLEModel, not Representation
        super(SimpleStateSpaceExog, self).__init__(endog,
                                               k_states = 9,
                                               k_posdef = 1,
                                               initialization = 'known',
                             initial_state = initial_state,
                             initial_state_cov = initial_state_cov)
        self.initial_params = initial_params
        self.estimate_params = estimate_params
        self['design'] = design
        seasonal_design = np.array([[1,  0,  0,  0,  0,  0,  0],
                                    [0, -1, -1, -1, -1, -1, -1],
                                    [0,  1,  0,  0,  0,  0,  0],
                                    [0,  0,  1,  0,  0,  0,  0],
                                    [0,  0,  0,  1,  0,  0,  0],
                                    [0,  0,  0,  0,  1,  0,  0],
                                    [0,  0,  0,  0,  0,  1,  0]])
        # Parameters that are static over time 
        my_design = np.pad(seasonal_design, ((0, 2), (0, 2)),
                           mode = "constant")
        np.fill_diagonal(my_design[-2:, -2:], 1)
        self['transition'] = my_design
        
        # R matrix that's multiplied by the state equation's error in state eq
        self['selection', 0, 0] = 1

        if not estimate_params:
            self['state_cov', 0, 0] = initial_params[0]
            self['obs_cov', 0, 0] = initial_params[1]

    def update(self, params, transformed = True, **kwargs):
        params = super(SimpleStateSpaceExog, self).update(params, transformed,
                                                          **kwargs)
        if self.estimate_params:
            self['state_cov', 0, 0] = params[0]
            self['obs_cov', 0, 0] = params[1]
        else:
            pass

    @property
    def start_params(self):
        return self.initial_params

    def transform_params(self, unconstrained):
        return unconstrained**2

    def untransform_params(self, constrained):
        return constrained**0.5


class SimplestStateSpaceExog(sm.tsa.statespace.MLEModel):
    def __init__(self, endog, design,
                 initial_state = [18, 5, -12, -8, 0, 0, 0, 0, 0],
                 initial_state_cov = np.eye(11),
                 initial_params = [1, 1, 1],
                 estimate_params = True):
        # The Super class is MLEModel, not Representation
        super(SimplestStateSpaceExog, self).__init__(endog,
                                               k_states = 9,
                                               k_posdef = 0,
                                               initialization = 'known',
                             initial_state = initial_state,
                             initial_state_cov = initial_state_cov)
        self.initial_params = initial_params
        self.estimate_params = estimate_params
        self['design'] = design
        seasonal_design = np.array([[1,  0,  0,  0,  0,  0,  0],
                                    [0, -1, -1, -1, -1, -1, -1],
                                    [0,  1,  0,  0,  0,  0,  0],
                                    [0,  0,  1,  0,  0,  0,  0],
                                    [0,  0,  0,  1,  0,  0,  0],
                                    [0,  0,  0,  0,  1,  0,  0],
                                    [0,  0,  0,  0,  0,  1,  0]])
        # Parameters that are static over time 
        my_design = np.pad(seasonal_design, ((0, 2), (0, 2)),
                           mode = "constant")
        np.fill_diagonal(my_design[-2:, -2:], 1)
        self['transition'] = my_design
        
        # R matrix that's multiplied by the state equation's error in state eq
        # pass - nothing here!

        if not estimate_params:
            self['obs_cov', 0, 0] = initial_params[0]

    def update(self, params, transformed = True, **kwargs):
        params = super(SimplestStateSpaceExog, self).update(params, transformed,
                                                          **kwargs)
        if self.estimate_params:
            self['obs_cov', 0, 0] = params[0]
        else:
            pass

    @property
    def start_params(self):
        return self.initial_params

    def transform_params(self, unconstrained):
        return unconstrained**2

    def untransform_params(self, constrained):
        return constrained**0.5

