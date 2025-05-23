% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_documentation.R
\docType{data}
\name{sample.data.fixations}
\alias{sample.data.fixations}
\title{Fixations from 7 individuals}
\format{
A data frame
\describe{
  \item{x}{fixation position x}
  \item{y}{fixation position y}
  \item{duration}{duration of fixation in milliseconds}
  \item{onset}{onset of fixation in milliseconds}
  \item{offset}{offset of fixation in milliseconds}
  \item{rmsd}{Sample-to-sample root mean square deviation of all samples}
  \item{rms.from.center}{Root means square deviation of all included samples from the centroid of the fixation}
  \item{missing.samples}{proportion of missing samples}
  \item{fixation.algorithm}{Name of the fixation filter algorithm }
  \item{threshold}{Threshold setting for the fixation classification algorithm}
  \item{id}{Participant id}}
}
\source{
The dataset was stored in the package at 'data/example_data.RData'
}
\usage{
sample.data.fixations
}
\description{
This dataset contains fixation data from 7 individuals during a free viewing tasks. Data were
recorded at 1200 Hz using a Tobii Pro Spectrum eye tracker
}
\keyword{datasets}
