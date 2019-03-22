# Cytotidyr
![Logo](https://raw.githubusercontent.com/bjreisman/cytotidyr/master/vignettes/figures/cytotidyrlogo2.png)

Cytotidyr is a wrapper for the CytobankAPI package which allows for all steps of a cytobank preprocessing workflow to be reproduced in R, such that you can go from an experiment on cytobank to a flowFrame or data.frame with all your preprocessed events as quickly as possible. It also includes a simple shiny application for converting data on cytobank into csv files, which can easily be adapted to other applications. 
Cytotidyr provides a set of function for applying the following preprocessing steps from cytobank:
  - Data scaling
- Panel assignment
- Compensation
- Gating on populations
- Sample tagging

All of these steps are designed to be applied to flowFrame or flowSet objects as defined in the flowCore package allowing it to integrate with existing analysis pipelines. New methods for the as.data.frame() function in base R also allow these flowCore objects to be converted to data.frames or tibbles consisting of _*tidy data*_ for integration with tidyverse based workflows.


## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

Cytotidyr was developed for R 3.6+. In order to fully utilize the features of cytotidyr, you'll need to install and load the flowCore and CytobankAPI pacakges. These are also dependencies, so if you install cytotidyr flowCore and CytobankAPI should be automatically installed. 
```{r}
if (!requireNamespace("BiocManager", quietly = TRUE))
install.packages("BiocManager")
BiocManager::install("flowCore", version = "3.8")

install("CytobankAPI")
```

### Installation

A step by step series of examples that tell you how to get a development env running

Say what the step will be

```{r}
#install.packages("devtools")
devtools::install_github("bjreisman/cytotidyr")
```
### Getting Started

First we'll load the dependencies neccessary to run this example:
  ```{r}
library(cytotidyr)
library(flowCore)
library(CytobankAPI)
library(scico)
library(tidyverse)
library(flowWorkspace)
library(CytoML)
```

Next I'll import a cytobank experiment from cytobank using an API token (under "account settings") and the experiment ID ("from the experiment URL"). 

The token below is no longer valid, but I've saved the output and we can load it two chunks below:
  ```{r}
token <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJqdGkiOiI3YjYyMzIyYmMwZGZiZmFmYzI0ZWQ5NTg2ZDdlOGMzMyIsImV4cCI6MTU1MzE5OTk2MiwidXNlcl9pZCI6MTQ3LCJhdWQiOiJjeXRvYmFua19hcGlfdjFfdXNlcnMiLCJpYXQiOjE1NTMxNzExNjIsImlzcyI6Imh0dHBzOi8vdmFuZGVyYmlsdC5jeXRvYmFuay5vcmcvIiwibmJmIjoxNTUzMTcxMTYyLCJzdWIiOiJjeXRvYmFua19hcGlfdjEifQ.T1Wn-aHTUxppSPw_NODalWgZ2heZL9ALM2g5EhJbSew"
cyto_session <- authenticate("vanderbilt", auth_token = token)
experiment.id <- 29564

exp_info <- fetchCytobankExperiment(cyto_session = cyto_session, experiment.id)
saveRDS(exp_info, "exp_info_sample.rds")
```

In order to minimize the size of the API calls, `fetchCytobankExperimet` doesn't import the actual FCS files. In order to load those, we'll need to manually download them using `CytobankAPI` and read them in as FCS files. Again this requires access to a specific experiment, so we'll load a previously loaded version in the next chunk. 
```{r}
fcspath <- fcs_files.download_zip(cyto_session, experiment.id, exp_info$fcs_files$originalId)
fcspath_unzipped <- unzip(fcspath)
myflowset <- read.flowSet(fcspath_unzipped)
```
This chunk will load the same data as above, but will actually execute. 
```{r}
exp_info <- readRDS(system.file("extdata", "exp_info_sample.rds", package = "cytotidyr"))
myflowset <- read.flowSet(system.file("extdata",
c("Donor 2 mem post-sort.fcs", "Donor 2 pre-sort.fcs" ),
package = "cytotidyr"))
```

Here's an example of a typical workflow for going from an experiment + FCS files to a data.frame
```{r}
mygatingset <- GatingSet(myflowset) # convert to a gatingset to use flowWorkspace
#> ..done!
mygatingset <- flowWorkspace::transform(mygatingset, exp_info$transforms) #transform the data
markernames(mygatingset) <- exp_info$panels$`Panel 1` #apply the panels
CytoML::gating(exp_info$gates, mygatingset) #run gating 
#> intact
#> purity LR
#> purity LL
#> purity UL
#> purity UR
#> singles
#> ..done!
myflowset_preprocessed <- flowWorkspace::getData(mygatingset, "singles") #back to a flowSet
myflowset_tagged <- tagFlowSet(myflowset_preprocessed, exp_info$sampletags) #apply sampletags

mytidydata <- as.data.frame(myflowset_tagged)

str(mytidydata)
#> 'data.frame':    28735 obs. of  15 variables:
#>  $ FSC-A            : num  156339 134925 105803 148191 151899 ...
#>  $ FSC-W            : num  84410 80593 76462 83050 82579 ...
#>  $ FSC-H            : num  121381 109717 90684 116939 120550 ...
#>  $ SSC-A            : num  3.81 3.6 3.42 3.42 3.45 ...
#>  $ SSC-W            : num  88618 89930 81403 86246 83903 ...
#>  $ SSC-H            : num  3.5 3.28 3.2 3.15 3.21 ...
#>  $ FL-A             : num  1.374 2.379 1.961 0.629 3.073 ...
#>  $ PE-A             : num  5.25 2.23 4.8 5.28 2.19 ...
#>  $ APC-A            : num  1.1506 3.1148 0.0578 0.1319 3.9569 ...
#>  $ Alexa Fluor 700-A: num  0.221 0.728 1.987 2.32 1.391 ...
#>  $ Time             : num  0 0 0 0 0.1 ...
#>  $ FCS Filename     : chr  "Donor 2 pre-sort.fcs" "Donor 2 pre-sort.fcs" "Donor 2 pre-sort.fcs" "Donor 2 pre-sort.fcs" ...
#>  $ Sample.Type      : Factor w/ 2 levels "Post-Sort","Pre-Sort": 2 2 2 2 2 2 2 2 2 2 ...
#>  $ Plate            : Factor w/ 1 level "Samples": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ FCS.File.Category: Factor w/ 1 level "Experiment Files": 1 1 1 1 1 1 1 1 1 1 ...
```
## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments
* Brian Bachmann
* Jonathan Irish
* P. Brent Ferrell
* Sierra Barone 
* [RGLab's set of packages](https://github.com/RGLab) including flowCore, flowWorkspace, and CytoML, on which Cytotidyr is built.
