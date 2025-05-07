\name{kollaR-package}
\alias{kollaR}
\docType{package}
\title{Fixation and Saccade Detection, Visualization, and Analysis of Eye Tracking Data}
\description{
  Functions for analyzing eye-tracking data, including event detection (sometimes referred to as fixation filtering), visualizations, and area of interest (AOI) based analyses. See separate documentation for each function. Make sure it works with your data. Currently included algoritms are I-VT, I-DT, adaptive velocity threshold  and two-means clustering). The principles underlying I-VT and I-DT filters are described in Salvucci & Goldberg (2000,\doi{10.1145/355017.355028}). Two-means clustering is described in Hessels et al. (2017, \doi{10.3758/s13428-016-0822-1}). The adaptive velocity algorithm is described in Nyström and Holmqvist (2010, \doi{10.3758/BRM.42.1.188})
}
\details{
  Overview of functions:
    Pre-processing (smoothing, interpolation, downsampling):'preprocess_gaze', 'downsample_gaze'

    Fixation and Saccade Classification (Fixation Filters)**: `algorithm_ivt`, `algorithm_idt`, `algorithm_i2mc`, `algorithm_adaptive`

    Fixation and Post-Processing)**: `merge_adjacent_fixations`, `trim_fixations`

    Visualization of Output from Fixation and Saccade Detection and Preprocessing Algorithms: `fixation_plot_ts`, `fixation_plot_temporal`, `fixation_plot_2d`,
                  `plot_velocity_profiles`, `plot_sample_velocity`, `plot_algorithm_results`

    Visualization of Gaze Data: `static_plot`, `animated_fixation_plot`

    AOI Based Analyses: `draw_aoi`, `aoi_test`
}
\author{
  Johan Lundin Kleberg \email{johan.lundin.kleberg@su.se}
}


\keyword{package}
