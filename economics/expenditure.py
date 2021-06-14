import pandas as pd
import numpy as np

import matplotlib.pyplot as plt
plt.interactive(True)

# www.bls.gov/cex/pumd_data.htm#csv
# https://www.bls.gov/cex/pumd-getting-started-guide.htm

# Let's start with the FMLI file with CU level Summary Expenditures
# This is the "Interview Survey" as opposed to the "diary survey"
def read_fmli(fname, year, quarter):
    fmli = pd.read_csv(fname)
    fmli.insert(0, 'year', year)
    fmli.insert(1, 'quarter', quarter)
    return fmli

def months_in_scope(interview_mo, quarter):
    months_in_scope = np.nan
    if quarter in [1, 2, 3, 4]:
        if interview_mo in [1, 2, 3]:
            months_in_scope = interview_mo - 1
        elif interview_mo in [4, 5, 6, 7, 8, 9, 10, 11, 12]:
            months_in_scope = 3
        else:
            raise ValueError(f"interview_mo {interview_mo} outside of range")
    elif quarter == 5:
        if interview_mo in [1, 2, 3]:
            months_in_scope = 4 - interview_mo
        else:
            raise ValueError(f"interview_mo {interview_mo} outside of range")

    else:
        raise ValueError(f"quarter {quarter} outside of range")
    return months_in_scope

# The x means that this file appeared in last year's also, and updates were made
# On the same information
fmli_q1 = read_fmli("/mnt/c/devl/data/carbon/CE/intrvw19/intrvw19/fmli191x.csv",
                    2019, 1)
fmli_q2 = read_fmli("/mnt/c/devl/data/carbon/CE/intrvw19/intrvw19/fmli192.csv",
                    2019, 2)
fmli_q3 = read_fmli("/mnt/c/devl/data/carbon/CE/intrvw19/intrvw19/fmli193.csv",
                    2019, 3)
fmli_q4 = read_fmli("/mnt/c/devl/data/carbon/CE/intrvw19/intrvw19/fmli194.csv",
                    2019, 4)
fmli_q5 = read_fmli("/mnt/c/devl/data/carbon/CE/intrvw19/intrvw19/fmli201.csv",
                    2019, 5)

# 5 quarters!
fmli_2019 = pd.concat([fmli_q1, fmli_q2, fmli_q3, fmli_q4, fmli_q5])

#NEWID is a unique sequential number concatenated with the number of the interview.
# The last digit of NEWID indicates the interview number in a series of 4,
# or the week of diary collection in a series of 2.
# All values prior to the last digit, identify a CU.

def get_unit_id(newid):
    return int(str(newid)[:-1])

def get_interview_id(newid):
    return int(str(newid)[-1])

# 91 PSUs coded as "S" (Urban > 2.5 Mil), "N" (Urban < 2.5 Mil), "R" (rural)
# Hm I only see the S PSUs

fmli_2019.insert(0, 'psu', fmli_2019['PSU']) # TODO: remove this. Not what I thought it was
fmli_2019.insert(1, 'cu_id', fmli_2019.NEWID.apply(lambda x: get_unit_id(x)))
fmli_2019.insert(2, 'interview_id', fmli_2019.NEWID.apply(lambda x: get_interview_id(x)))
fmli_2019.insert(3, 'interview_mo', fmli_2019['QINTRVMO'])
fmli_2019.insert(4, 'interview_yr', fmli_2019['QINTRVYR'])
fmli_2019.insert(5, 'months_in_scope', fmli_2019.apply(
    lambda row: months_in_scope(row['interview_mo'], row['quarter']), axis=1
))
fmli_2019.insert(6, 'weight', fmli_2019['FINLWT21'])
fmli_2019.sort_values(['psu', 'cu_id', 'interview_id'], inplace=True)

# Check that we pulled out the interview id correctly
assert(all(fmli_2019['INTERI'] == fmli_2019['interview_id']))

# Manually check that interview month and year meet expectations
fmli_2019.interview_mo.value_counts()  # Twice as many for 1, 2, 3
fmli_2019.interview_yr.value_counts()

# Manually check months in scope
fmli_2019[['quarter', 'interview_mo', 'months_in_scope']].drop_duplicates()
fmli_2019.months_in_scope.value_counts()


# Let's learn about weights
# Each quarter's interview is separately weighted to be representative
# of the population.

# There are 44 half sample replicate weights (WTREP01 - WTREP44),
# which are used for variance computations.

weight_vars = [f'WTREP{i:02d}' for i in np.arange(1, 45)]
weight_df = fmli_2019[['cu_id', 'interview_id', 'quarter'] + weight_vars]

# Why are some of the WTREP vars nan?
#  A: because their half-sample wasn't chosen in the vvariance

# https://www.bls.gov/opub/hom/cex/calculation.htm
# Standard errors calculated via "balanced repeated replication"
# CUs divided into 43 strata
#   CUs within each stratum randomly divided into 2 "half samples"
#   44 estimates of y_bar are created using one "half sample" per stratum
# There 43 Choose 2 half samples and it's not clear why 44 was the number choosen,
# but each of the y_bar estimates are called "replicate estimates"
#
# From Wikipedia page, this is a stratified sampling procedure
# https://en.wikipedia.org/wiki/Balanced_repeated_replication

weight_df.shape 
sum(weight_df['WTREP01'].isna()) # very close to half the rows
sum(weight_df['WTREP02'].isna()) # very close to half the rows

# So we can get the strata used in BRB, but the weights are different
# (approximately, but not exactly, twice what the original weights are)

# So now we're moving past the variance
# weights into the full weights

# Calibration final weight for the full sample
# I believe the only one of these numbered variables left in present survey
fmli_2019[['FINLWT21']]
plt.hist(fmli_2019[['FINLWT21']])

# Three different kinds of wieghts (that we can't see!)
# 1. weighting control sampling - cluster sampling type of adjustment
#    a cluster is discovered where only one unit was discovered - they get them all
# 2. Non-interview adjustment - refusal to answer
# 3. Calibration factor - Sounds like a raking adj to 24 known pop counts


# Let's estimate Annual income
# FINATXEM - Total amount of family income after estimated taxes
# in the last 12 months (Imputed or collected data)
fmli_2019[['FINATXEM']] 

replicate_quarters = 4  # Each quarter is weighted for annual results
proportion_in_scope = fmli_2019['months_in_scope'] / 3 # Peanut butter spread

denom = np.sum(fmli_2019['weight'] / replicate_quarters * proportion_in_scope)
numer = np.sum(fmli_2019['weight'] / replicate_quarters * proportion_in_scope * fmli_2019['FINATXEM'])

income_est = numer / denom
print(f'Est Total Family Income After Taxes 2019 is {income_est:.2f}')

# From Section 6.1 of the Getting Started Guide (https://www.bls.gov/cex/pumd-getting-started-guide.htm)
# Why do users need data from two years to estimate one calendar year?
# Users report expenditures for the three months prior to the interview.
# That is, an interview conducted in Feb 2017 would have expenditure information
# for Nov 2016, Dec 2016, and Jan 2017. 

# ZAPPAREL
# Ummel has it as "Clothing and footware" with category "CLOFTW"
# CE has it at "Expenditures for apparel and services, including
# clothing for men, women, boys, girls, and children, footwear,
# and other apparel products and services"
# Is it the same?




# Alcoholic beverages this quarter
plt.hist(fmli_q1['ALCBEVCQ'])

# Fuel, oil, and other fuels this quarter

plt.hist(fmli_q1['ALLFULCQ'])


# NIPA - National Income and Product Accounts
# Had to delete a few rows - still needs work
nipa_pce = pd.read_csv("/mnt/c/devl/data/carbon/NIPA/dollars_per_type.csv")
