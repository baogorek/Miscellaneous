Dagster with R and Python
==========================================

In bash, in an environment with Python3 available and R installed
(R needs "arrow" and "reticulate" packages installed)


1. Start the Arrow Flight server. Ideally, dagster would do this, but this is just a proof of concept.
```{bash}
Rscript ./arrow-flight-server.R
```

In another session, run the dagster UI
```{bash}
dagster dev -f hello-dagster.py
```

Materialize the data assets. If successful, an R process helped create a dagster data asset.
