Example of Imports vs Depends in the R DESCRIPTION file
=========================================================

The package in this directory, `testPkg`, doesn't have a DESCRIPTION file, but
the script in this directory will create it using the functionality provided by
[devtools](https://github.com/hadley/devtools). First, the glmnet package will
be added to the Imports section of the DESCRIPTION file, and later it will be
added to the Depends section. The package `testPkg` itself imports the `glmnet`
function from the `glmnet` package, but does not export it, and thereby this
example shows the key difference between Imports and Depends in the DESCRIPTION
file.  

To follow this example:
  * In addition to R, install packages devtools and glmnet
  * Run the following code in the same directory as this markdown documnet

```r
# Please install the devtools and glmnet packages for this example
devtools_installed <- requireNamespace("devtools", quietly = TRUE)
if (!devtools_installed) {
  stop("Please install devtools to use this example")
}

glmnet_installed <- requireNamespace("glmnet", quietly = TRUE)
if (!glmnet_installed) {
  stop("Please install glmnet to use this example")
}

# Our package testPkg needs a DESCRIPTION file
devtools::create_description("./testPkg")

# Put glmnet under Imports in the DESCRIPTION file 
devtools::use_package("glmnet", type = "Imports", pkg = "./testPkg")

# Now install and load (i.e., attach) testPkg
devtools::install("./testPkg")
library(testPkg)

# cv.glmnet was exported, so this function should be attached
cv.glmnet

# The glmnet function was imported but not exported, so this should fail!
glmnet

# however, the glmnet package is loaded:
glmnet::glmnet

# Now let's add glmnet to Depends. Note the "Are you sure?" message 
devtools::use_package("glmnet", type = "Depends", pkg = "./testPkg")

devtools::install("./testPkg")
library(testPkg)

# glmnet is attached now, and the glmnet fn is in the search path
glmnet
```
