# shinyPCA
R package holding a R Shiny application for Principipal Component Analysis

## Install and load package

```
library(devtools)
install_github("sarahbonnin/shinyPCA")
library("shinyPCA")
```

## Run app

```
runShinyPCA()
```

## How to run app

**User input:**
* Upload expression matrix (compulsory):
  * Each column of the matrix will be represented as a point in the PCA.
* Upload sample metadata file (optional): 
  * Text file that contains columns with samples metadata: the first column must correspond to the first row of the input file!

**Customize plot:**
* Plot title
* Title font size
* x-axis and y-axis font sizes
* Point label size

