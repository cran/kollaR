% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VisualizationFunctions.R
\name{filt_plot_2d}
\alias{filt_plot_2d}
\title{Plot fixations vs. individual sample coordinates in 2D space. In the current release, filt_plot_2d is a
wrapper around fixation_plot_2d which accepts the same arguments.}
\usage{
filt_plot_2d(
  raw.data,
  fixation.data,
  plot.window = c(NA, NA),
  raw.columns = c("x.raw", "y.raw"),
  fixation.columns = c("x", "y"),
  fixation.radius = 40,
  xres = 1920,
  yres = 1080,
  order.vertical = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{raw.data}{gaze matrix which must include columns for x and y coordinates in the and raw data (single samples) as specified in the raw.columns parameter}

\item{fixation.data}{Data frame with fixation data which must include columns for fixation x and y coordinates as specified in the fixation.columns parameter as well as the
variable onset which indicates the onset of the fixation. Make sure the onset varables match the timing in the raw.data df}

\item{plot.window}{vector defining the time window to plot. If left empty, the 50-65% interval of the data are plotted. If the submitted values are
<0, they are assumed to be proportions, e.g., \code{plot.window = c(0.3,0.35)} plots the 30-35 percent of max.length interval of the data. Numbers >1 are assumed to refer to sample order
in the data}

\item{raw.columns}{Names of variable containing raw data. Default x.raw and y.raw}

\item{fixation.columns}{Names of variable containing filtered data. Default x and y}

\item{fixation.radius}{Radius of circles showing fixations.}

\item{xres}{horizontal resolution of the screen or area to plot on. Default 1920}

\item{yres}{vertical resolution of the screen or area to plot on. Default 1080}

\item{order.vertical}{If TRUE, stack subplots on top of each other in a single column}

\item{verbose}{if TRUE, print the resulting plot}
}
\value{
a ggplot of raw and fixated values plotted on the y axis and sample number on the x axis
}
\description{
This function plots and returns a ggplot2 figure showing fixations and individual gaze coordinates plotted against time.
The interval to plot can be defined as a proportion of the data frame or by sample numbers. This function uses one data.frame with fixations and one with sample-by-sample raw data
}
