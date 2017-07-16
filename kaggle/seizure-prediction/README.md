
Seizure Prediction Scoring Code from Ben Ogorek
=================================================================
*Reproducing the submit_6.csv submission and scoring new matrices*

## How to use this code

### Setting up the Environment

With the understanding that this code will be run on a Barcoo system, the
following dependencies were installed and tested on the
*RHEL-6.0\_GA-x86\_64-6-Hourly2 - ami-09680160* Amazon community EC2 instance:

 - Python 2.7.12 
     * numpy
     * pandas
     * sklearn
     * scipy
     * pywt
 - R 3.3.2
     * xgboost
     * glmnet
     * randomforest

Here are the steps used to prepare the clean EC2 instance to run the code
(yum install was not working, but conda was able to serve the same purpose):

```
wget http://repo.continuum.io/miniconda/Miniconda2-4.2.12-Linux-x86_64.sh -O ~/miniconda.sh
bash ~/miniconda.sh -b -p $HOME/miniconda
export PATH="$HOME/miniconda/bin:$PATH"

conda install numpy
conda install pandas
conda install scikit-learn

pip install PyWavelets

# Preparing to install R:

conda install gcc
conda install bzip2
conda install xz
conda install pcre

# Downloading and installing R from source
wget https://cran.r-project.org/src/base/R-3/R-3.3.2.tar.gz
tar -xf R-3.3.2.tar.gz
cd R-3.3.2
./configure LDFLAGS="-L/root/miniconda/lib64 -L/root/miniconda/lib" --with-readline=no --with-x=no

# Add R and Rscript to the path
export PATH=$PATH:/root/R-3.3.2/bin

# Start R and install packages
R 
install.packages("glmnet")
install.packages("xgboost")
install.packages("randomForest")
```

### Running the code

Change directories to seizure-prediction directory (from the zip file sent over
email). Save .mat files in a single directory (which may contain subdirectories)
and run the bash program scoring.sh with a chosen output csv filename:

```
  ./scoring.sh -i <input directory> -o <output csv file>
```

The .mat files must be named in a way that matches the regular
expression `.*_\\d+_\\d+.mat` where the first digit sequence represents the
user and the second represents the index of the 10-minute EEG sequence.

Sample data has been included with the code. To test while in the
seizure-prediction directory, simply run

```
./scoring.sh -i data/sample_input -o sample_output.csv
```

## Description of the Analytical Approach
This solution uses a wavelet-based representaiton of EEG data in conjunction
with an ensemble of standard machine
learning techniques. [Gandhia, Panigrahib, and Ananda (2011)][1] found that
Coiflets 1 was "the most suitable candidate among the wavelet families"
for classification of EEG signals within their study, and thus
the coef1 wavelet in python's PyWavelets library was the basis for the data
representation used here. For each matrix, the discrete wavelet transform (DWT)
returned a list of coefficients for
the different "scales," where there were many more coefficients for the higher
resolutions than lower resolutions, one of the desirable properties
of the DWT relative to the short-time Fourier transform (STFT).
This allowed for a much
finer resolution at the higher frequencies. However, averaging the
wavelet "energy" over longer periods of time seemed to work better, potentially
reducing this benefit of a wavelet-based decomposition.

To reduce dimensionality of the wavelet-based representation, a maximum
across time of the different energy averages was taken by channel and scale;
the max average energies were features for the future models. To deal with
data dropout
(and the possibility of a high max energy due to low sample size variability),
if a time bin had less than 30% percent of its total possible
sample size, it was dropped from the analysis. (Earlier trials used 60%, and
the last minute change to 30% may have contributed to an observed increase
in AUC.)

The predictive portion of approach used an ensemble of regularized generalized
linear models, a gradient-boosted tree, a random forest, and an ensemble of
mulitivel perceptrons. Additionally, there were separate regularized linear
models built for each subject. All the models were blended together in
a linear combination with weights determined by cross validation (with the help
of simulated annealing to attain a high AUC). 

## Description of the contest training and test scoring code

The contest data processing is reproducible using the file 
*contest\_processing.py*. File
paths are hard-coded and the word "image" is used to refer to the 10-minute
time segments (a misnomer owing to a past contest). The data set produced during
the contest is *data/explore6.csv* (also a misnomer).

The contest scoring is reproducible using the file
*contest\_cv\_train\_test.R*. One adhoc
change was made after the contest to rename a variable from "img" to "segment."
After running crossvalidation and training the models, the image was saved in
*data/backup\_6.RData* (and later whittled down to
*data/trained\_models.RData*).

## Description of the scoring code

The bash program *scoring.sh* sequentially calls the python program
*step1\_processing.py* and
the R program *step2\_scoring.R* in sequence. Aside from changes to allow
running
on the command line and changing the name "image" to "sequence", the processing
file works identically on both contest and scoring files. It settles upon
one choice of wavelet (Coiflets 1) and a set of resolutions numbers for each
scale, though these can be changed easily.

There are also tunable parameters in *util.py* that do not have a proper
interface in *step1\_processing.py*: 

 - The EEG voltage truncation limits in `normalize_mat` are arbitrary and were
   set at +/-0.025 during the final contest submissions.
 - `create_wavelet_decomp` uses a hardcoded level of 14 in the in the wavelet
    decomposition. This was an arbitrarily chosen depth limit based on the
    (untested) assumption that low-frequency information would not add much
    value to classification.
 - In `create_wide_df`, time-buckets of wavelet energies were removed from the
   analysis if they did not have 30% of the possible values populated. The 30%
   was an arbitrary number.
 - In `generate_train`, `max_prop_zeros` is arbitrarily set to .6, meaning that
   any .mat files containing more than 60% zeros is not allowed in the training
   set.

The scoring program *step2\_scoring.R* is also similar to the program used in
the contest (*contest_cv_train_test.R*), with the following exceptions:

 - No cross validation or training is done. Rather the ensemble weights and
   trained model objects are loaded in from *data/trained\_models.RData*.
 - The contest processed test data set passed in as an optional argument to
   `predict_nnet`. This is to match the contest submission scores when only a
   subset of records from the test data set are included, since the function
   otherwise scales the data set in a way that depends on which subset is used
   in the scoring event.
 - After filling in missing predictions with zeros, *observations are sorted
   by filename*. This was a step that was accidentally left out of the contest
   submissions (see the last lines of *data/submit\_6.csv*).

## References
[1]: http://www.sciencedirect.com/science/article/pii/S0925231211003158
