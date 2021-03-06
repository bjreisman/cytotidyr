% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseCytobankExperiment.R
\name{parseCytobankExperiment}
\alias{parseCytobankExperiment}
\title{Parses ACS files exported from Cytobank for use in flowWorkspace}
\usage{
parseCytobankExperiment(acspath)
}
\arguments{
\item{acspath}{A path to the exported ACS file}
}
\value{
A list of experiment information including

1. A tibble of FCS files contained in the experiment and their panels

2. A tibble of sample tags from the experiment

3. A table of scales exported directly from cytobank

4. A transformerList version of the scales for use in flowWorkspace

5. A list of compensation matricies

6. A gating + population heirarchy imported from Cytobank by CytoML

7. A list of panels contained in the experiment

8. The experiment ID (character vector)

9. The experiment name (character vector)

10. A list of files unpacked from the ACS file, including FCS files
}
\description{
Parses ACS files exported from Cytobank for use in flowWorkspace
}
