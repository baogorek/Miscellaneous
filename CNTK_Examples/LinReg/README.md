
# Small Data Linear Regression in Microsoft's CNTK
As of May 2016, documentation is still emerging for Microsoft's neural network
toolkit CNTK. This example is part of an effort to understand the system
through the simplest possible examples.

## Prerequisites
Install CNTK, perhaps from a [binary download](https://github.com/Microsoft/CNTK/wiki/CNTK-Binary-Download-and-Configuration).
For Windows users, add the cntk folder (with CNTK.exe) to your path.

## The Regression Task
The file `Data/fiveRows.txt` has five rows of (x, z, y) triples for the
regression of y on x and z. The goal is to match the estimates from `lm`
within R:

```R
read.delim("Data/fiveRows.txt", header = F)
five <- read.delim("Data/fiveRows.txt", header = F)
names(five) <- c("x", "z", "y")
five <- read.delim("Data/fiveRows.txt", header = F, delim = " ")
five <- read.delim("Data/fiveRows.txt", header = F, sep = " ")
names(five) <- c("x", "z", "y")
lm(y ~ x + z, data = five)
```

## Running CNTK
While in the LinReg folder, run the command `cntk configFile=Config/linreg.cntk`.
Check to see that the file `LinReg.dnn.__AllNodes__.txt` has been created in 
the Models directory which will contain the linear regression parameter values.


