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

fmli_q1 = read_fmli("/mnt/c/devl/data/carbon/CE/intrvw19/intrvw19/fmli191x.csv",
                    2019, 1)
fmli_q2 = read_fmli("/mnt/c/devl/data/carbon/CE/intrvw19/intrvw19/fmli192.csv",
                    2019, 2)
fmli_q3 = read_fmli("/mnt/c/devl/data/carbon/CE/intrvw19/intrvw19/fmli193.csv",
                    2019, 3)
fmli_q4 = read_fmli("/mnt/c/devl/data/carbon/CE/intrvw19/intrvw19/fmli194.csv",
                    2019, 4)

fmli_2019 = pd.concat([fmli_q1, fmli_q2, fmli_q3, fmli_q4])

#NEWID is a unique sequential number concatenated with the number of the interview.
# The last digit of NEWID indicates the interview number in a series of 4,
# or the week of diary collection in a series of 2.
# All values prior to the last digit, identify a CU.

def get_unit_id(newid):
    return int(str(newid)[:-1])

def get_interview_id(newid):
    return int(str(newid)[-1])

fmli_2019.insert(0, 'cu_id', fmli_2019.NEWID.apply(lambda x: get_unit_id(x)))
fmli_2019.insert(1, 'interview_id', fmli_2019.NEWID.apply(lambda x: get_interview_id(x)))

fmli_2019.sort_values(['cu_id', 'interview_id'], inplace=True)


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




# So now we're moving past the variance
# weights into the full weights

# Calibration final weight for the full sample
# I believe the only one of these numbered variables left in present survey
fmli_2019[['FINLWT21']]

plt.hist(fmli_2019[['FINLWT21']])

fmli_2019[['FINLWT21']].value_counts()

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
