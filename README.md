_readme is a work in progress as of 2/11/2019_
# Cytotidyr

Cytotidyr is a wrapper for the CytobankAPI package which allows for all steps of a cytobank preprocessing workflow to be reproduced in R, such that you can go from data to data on cytobank to a flowFrame or data.frame with all your preprocessed events as quickly as possible. It also includes a simple shiny application for converting data on cytobank into csv files, which can easily be adapted to other applications. 

Cytotidyr provides a set of function for applying the following preprocessing steps from cytobank:
- Data scaling
- Panel assignment
- Compensation
- Gating on populations
- Sample tagging

All of these steps are designed to be applied to flowFrame or flowSet objects as defined in the flowCore pacakge allowing it to integrate with existing analysis pipelines. New methods for the as.data.frame() function in baes R also allow these flowCore objects to be converted to data.frames or tibbles consisting of _*tidy data*_ for integration with tidyverse based analysis pipelines. 

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

### Installing

A step by step series of examples that tell you how to get a development env running

Say what the step will be

```
#install.packages("devtools")
devtools::install_github("bjreisman/debarcoder")
```


# The part below here will need some work:
```
until finished
```

End with an example of getting some data out of the system or using it for a little demo

## Running the tests

Explain how to run the automated tests for this system

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Deployment

Add additional notes about how to deploy this on a live system

## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Billie Thompson** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone whose code was used
* Inspiration
* etc
