Dagster with R and Python
==========================================

In bash, in an environment with Python3 available and R installed

TODO: I'd like to figure out how to do this with a graph-backed op.

Any way to "register" the files being created with R?


## 1. Dropping and picking up files
```{bash}
dagster dev -f pipeline-files-only.p
```

Leaves some csvs in the root directory. Not sure how to clean them up.


## 2. Using the Arrow Flight Server

R needs "arrow" and "reticulate" packages installed. Python needs pyarrow

1. Start the Arrow Flight server. Ideally, dagster would do this, but this is just a proof of concept.
```{bash}
Rscript ./arrow-flight-server.R
```

In another session, run the dagster UI
```{bash}
dagster dev -f full-arrow-io-pipeline.py
```
