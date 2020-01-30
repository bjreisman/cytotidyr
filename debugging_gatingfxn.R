library(debarcoder)
library(flowCore)
library(cytotidyr)
library(tidyverse)
library(CytobankAPI)
library(ggridges)
library(flowWorkspace)
library(CytoML)


?fcs_files.download_zip
###################
token <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJqdGkiOiJkZTA1YTI5ZDZlNWYyYzhmM2Q0OTM2OTQ0Yjk4MjJmNSIsImV4cCI6MTU3OTgyODg3MywidXNlcl9pZCI6MTQ3LCJhdWQiOiJjeXRvYmFua19hcGlfdjFfdXNlcnMiLCJpYXQiOjE1Nzk4MDAwNzMsImlzcyI6Imh0dHBzOi8vdmFuZGVyYmlsdC5jeXRvYmFuay5vcmcvIiwibmJmIjoxNTc5ODAwMDczLCJzdWIiOiJjeXRvYmFua19hcGlfdjEifQ.56spt3P5TUTUtDzgs-4otUgxh7D9d_vqojY-SCSSWe0"
experiment.id <- 33997
cyto_session <- authenticate("vanderbilt", auth_token = token)
exp_info <- fetchCytobankExperiment(cyto_session, experiment.id)


exp_info$fcs_files
sampletags.i <- exp_info$sampletags %>%
  left_join(exp_info$fcs_files[,c(1,3)], by = c("FCS.Filename" = "filename"))
dir.create("data")
fcszip.path <- fcs_files.download_zip(cyto_session,
                                      experiment_id = experiment.id,
                                      fcs_files =  sampletags.i$originalId,
                                      directory = "data",
                                      timeout = 1000)


#debug
fcszip.path <- file.path("data", list.files("data", pattern = ".zip"))
#fcszip.path
fcsunzip.paths <- unzip(fcszip.path,
                        exdir = "data")


mygatingset <- cytobank_to_gatingset(exp_info$gates.path, fcsunzip.paths)
#myflowset
#mygatingset_compesated <- compensate(mygatingset, exp_info$compensations$JB)
mygatingset_scaled <- transform(mygatingset, exp_info$transforms)
markernames(mygatingset_scaled) <- exp_info$panels$`Panel 1`


singles <-getData(mygatingset_scaled, "singles")
singles <- gs_pop_get_data(mygatingset_scaled, "singles")
