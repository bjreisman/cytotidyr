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
```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("flowCore", version = "3.8")

install("CytobankAPI")
```

### Installation

A step by step series of examples that tell you how to get a development env running

Say what the step will be

```
#install.packages("devtools")
devtools::install_github("bjreisman/cytotidyr")
```

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments
* Brian Bachmann
* Jonathan Irish
* P. Brent Ferrell
* Sierra Barone 
