% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VisualizationFunctions.R
\name{plot_algorithm_results}
\alias{plot_algorithm_results}
\title{Plot vdescriptives one or more fixation detection algorithms}
\usage{
plot_algorithm_results(data_in, plot.variable = "rmsd")
}
\arguments{
\item{data_in}{Data frame with fixations to plot}

\item{plot.variable}{Variable to plot. If left empty, RMSD of fixations are plotted. Alternatives are "rmsd", "duration", "missing.samples", "rms.from.center"}
}
\value{
A ggplot with visualizations of the selected validity measure
}
\description{
This function visualizes validity measures of fixations detected with one or more fixation detection algorithms.
The function is tested for fixation data frames generated with kollaR event detection algorithms. By default, the function can plot
Root Mean Square Deviations of subsequent samples within the detected fixations (precision), the RMSD from the fixation centroid, fixation duration and the proportion of missing raw samples.
The output data is a ggplot which can be modified further outside the function.
If you want to use this function to compare more than one fixation detection algorithms, combine them using the function
rbind in base R. For example, \code{rbind(my_data1[["fixations"]], my_data2[["fixations"]])} would generate a combined data frame with the
fixations detected by two event classification procedures.
}
\examples{
plot_algorithm_results(data_in = sample.data.fixations, plot.variable = "rmsd")
}
