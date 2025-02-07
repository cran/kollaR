\name{kollaR-package}
\alias{kollaR}
\docType{package}
\title{Filtering, Visualization, and Analysis of Eye Tracking Data}
\description{
  Functions for analyzing eye-tracking data, including fixation filtering/event detection (I-VT, I-DT, and two-means clustering),
  visualizations, and area of interest (AOI) based analyses. See separate documentation for each function. Make sure it works with your data. The principles underlying I-VT and I-DT filters are described in
  Salvucci & Goldberg (2000,\doi{10.1145/355017.355028}). Two-means clustering is described in Hessels et al. (2017, \doi{10.3758/s13428-016-0822-1}).
}
\details{
  Overview of functions:
    Pre-processing (smoothing, interpolation, downsampling):'process_gaze', 'downsample_gaze'

    Fixation and Saccade Detection (Fixation Filters)**: `ivt_filter`, `idt_filter`, `cluster2m`

    Visualization of Output from Fixation Filter and Preprocessing Algorithms: `filt_plot_temporal`, `filt_plot_2d`, `plot_velocity_profiles`,
                `plot_sample_velocity`, `plot_filter_results`

    Visualization of Gaze Data: `static_plot`, `animated_fixation_plot`

    AOI Based Analyses: `draw_aoi`, `aoi_test`
}
\author{
  Johan Lundin Kleberg \email{johan.lundin.kleberg@su.se}
}


\keyword{package}
