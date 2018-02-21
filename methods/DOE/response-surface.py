# Idea: Replecate the response surface study shown here:
#http://www.itl.nist.gov/div898/handbook/pri/section4/pri473.htm
import pandas as pd
import numpy as np
from pyDOE import ccdesign
import statsmodels.formula.api as smf
from statsmodels.stats.anova import anova_lm

def factor_to_value(x, min_val, max_val):
    d = max_val - min_val
    x = x * (d / 2.0)
    x = x + min_val + (d / 2.0)
    return x

uniformity_df = pd.read_csv("uniformity.csv")

min_pressure = 4 # torr
max_pressure = 80 # torr
min_h2_wf6 = 2 # ratio
max_h2_wf6 = 10 # ratio

# Experimenters used central composite inscribed (CCI) w/ 3 centerpoint runs

design = ccdesign(2, center = (2, 1), alpha = 'r', face='cci')

design_df = pd.DataFrame(design, columns = ['coded_pressure', 'coded_h2_wf6'])
design_df = np.round(design_df, 2) # to match example data

hand_rolled = (design_df.groupby(['coded_pressure', 'coded_h2_wf6'])
                        .size()
                        .reset_index()
                        .rename(columns={0:'count'}))

from_article = (uniformity_df.groupby(['coded_pressure', 'coded_h2_wf6'])
                             .size()
                             .reset_index()
                             .rename(columns={0:'count'}))

assert(hand_rolled.equals(from_article))

## Do the analysis of variance

# uniformity
my_formula = ("uniformity ~ coded_pressure + np.power(coded_pressure, 2) + " +
              "coded_h2_wf6 + np.power(coded_h2_wf6, 2) + " +
              "coded_pressure * coded_h2_wf6")

mod = smf.ols(formula = my_formula, data = uniformity_df) 
res = mod.fit()
res.summary()

# stress

# Getting back the non-rounded .71 for regression
#design = ccdesign(2, center = (2, 1), alpha = 'r', face='cci')
#design_df = pd.DataFrame(design, columns = ['coded_pressure', 'coded_h2_wf6'])
#df2 = pd.concat([uniformity_df, design_df], axis=1)
#df2 = df2.drop([5, 6], axis = 1)

my_formula = ("stress ~ pressure + np.power(pressure, 2) + " +
              "h2_wf6 + np.power(h2_wf6, 2) + " +
              "pressure * h2_wf6")

mod = smf.ols(formula = my_formula, data = uniformity_df) 
res = mod.fit()
res.summary()

anova_lm(res, typ = "I")
anova_lm(res, typ = "III") # Slightest difference

%run orthogonal_poly.py
# Using orthogonal polynomials
pressure_ortho = ortho_poly_fit(uniformity_df['pressure'], 2)
uniformity_df['pressure_ortho'] = pressure_ortho[0][:, 1]
uniformity_df['pressure_ortho_sq'] = pressure_ortho[0][:, 2]

h2_wf6_ortho = ortho_poly_fit(uniformity_df['h2_wf6'], 2)
uniformity_df['h2_wf6_ortho'] = h2_wf6_ortho[0][:, 1]
uniformity_df['h2_wf6_ortho_sq'] = h2_wf6_ortho[0][:, 2]

my_formula = ("stress ~ pressure_ortho + pressure_ortho_sq" +
              " + h2_wf6_ortho + h2_wf6_ortho * pressure_ortho")

mod = smf.ols(formula = my_formula, data = uniformity_df) 
res = mod.fit()
res.summary()

anova_lm(res, typ = "I")
anova_lm(res, typ = "II") # Slightest difference

# These two are correlated
np.corrcoef(uniformity_df['h2_wf6_ortho_sq'],
            uniformity_df['pressure_ortho_sq'])

# TODO: so I may have misunderstood the level of orthogonality



# Note, after reading the rest of the article, I'm not as excited about
# replicating the rest of it. The methods are mostly visual in nature and
# Thus limited to two factors at a time. Going to branch out and see if
# I can arrive at an analytical solution to getting to the maximum

# let f(x, y) = a + b x^2 + c y^2 + d x + e y + f x y, then:
b = res.params[2]
c = res.params[4]
d = res.params[1]
e = res.params[3]
f = res.params[5]

A = np.array([[2 * b, f], [2 * c, f]])
b = np.array([-d, -e])

x_argmax, y_argmax = np.dot(np.linalg.inv(A), b)


