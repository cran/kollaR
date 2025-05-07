
#'Interpolation and smoothing of gaze-vector
#'@description Pre-processing gaze
#'Interpolate over gaps in data and smooth the x and y vectors using a moving average filter.
#'The gaze vector must contain the variables timestamp, and variables containing unfiltered x
#'and y coordinates. Default names: x.raw and y.raw. Timestamps are assumed to be in
#'milliseconds. The unprocessed x and y variables are kept under the names x.unprocessed and y.unprocessed for comparison.
#'The function will add the variable timestamp.t to the data frame before returning. This is a theoretical timestamp based on the detected median sample-to-sample
#'timestamp difference as compared to the actual registered time stamps in the data. This can be useful in some validation analyses.
#'@param gaze_raw Data frame containing unfiltered timestamp, x.raw and y.raw vectors.
#'@param max_gap_ms The maximum gaps defined as subsequent NAs in the data to interpolate over in milliseconds. Default 75 ms
#'@param marg_ms The margin in milliseconds before and after the gap to use as basis for interpolation.
#'@param filter_ms The size of the moving average window to use in smoothing. Default 15 ms
#'@param xcol Name of column containing unprocessed x coordinates
#'@param ycol Name of column containing unprocessed y coordinates
#'@return data frame with gaze data after interpolation and filtering
#'@examples
#'processed_gaze <- preprocess_gaze(sample.data.unprocessed)

preprocess_gaze <- function(gaze_raw, max_gap_ms = 75, marg_ms = 5, filter_ms = 15, xcol = "x.raw", ycol = "y.raw") {
  #Recalculate these parameters to number of samples depending on the sampling rate in the gaze vector. Make sure
  #there is a variable called 'timestamp' in the unit ms

  if (nrow(gaze_raw) > 500) {
    one.sample <- mean(diff(gaze_raw$timestamp[1:500]),na.rm = T)
  } else {one.sample <- mean(diff(gaze_raw$timestamp),na.rm=T)}


  if (sum(gaze_raw$timestamp<0)>0) {
    warning("The timestamp column contains negative values. Check the data file")
  }

  #Save unprocessed variables in the matrix
  gaze_raw$x.unprocessed <- gaze_raw[[xcol]]
  gaze_raw$y.unprocessed <- gaze_raw[[ycol]]

  if (one.sample < 0.02 | one.sample >100) {warning("Unlikely sample to sample difference in timestamps. Are timestamps in milliseconds?")}
  gaze_raw$timestamp.t <- seq(0,dim(gaze_raw)[1]-1)*one.sample #Theoretical timestamp based on smapling rate. Tobii Spectrum 1200 Hz data have ~20% samples with identical timestamps.
  max_gap <- round(max_gap_ms/one.sample)
  marg <- round(marg_ms/one.sample)
  filter_window <- round(filter_ms/one.sample)

  #Interpolate over gaps in the data
  gaze_raw[[xcol]]<- interpolate_with_margin(gaze_raw[[xcol]], marg = marg, max_gap = max_gap) #Interpolate. Use the median of the samples directly before and after the gap
  gaze_raw[[ycol]]<- interpolate_with_margin(gaze_raw[[ycol]], marg = marg, max_gap = max_gap)

  #Smooth the x and y vectors
  gaze_raw[[xcol]]<- rollmean(gaze_raw[[xcol]], k = filter_window, align = "left", na.pad =T)
  gaze_raw[[ycol]] <- rollmean(gaze_raw[[ycol]], k = filter_window, align = "left", na.pad =T)
  gaze_raw$sample <- seq(1, dim(gaze_raw)[1])
  return(gaze_raw)


}



#'Interpolation and smoothing of gaze-vector. This function will be replaced by preprocess_gaze in future versions.
#'process_gaze is a wrapper around preprocess gaze (the two functions produce the same result)
#'@description Pre-processing of gaze.
#'Interpolate over gaps in data and smooth the x and y vectors using a moving average filter.
#'The gaze vector must contain the variables timestamp, and variables containing unfiltered x
#'and y coordinates. Default names: x.raw and y.raw. Timestamps are assumed to be in
#'milliseconds. The unprocessed x and y variables are kept under the names x.unprocessed and y.unprocessed for comparison.
#'The function will add the variable timestamp.t to the data frame before returning. This is a theoretical timestamp based on the detected median sample-to-sample
#'timestamp difference as compared to the actual registered time stamps in the data. This can be useful in some validation analyses.
#'@param gaze_raw Data frame containing unfiltered timestamp, x.raw and y.raw vectors.
#'@param max_gap_ms The maximum gaps defined as subsequent NAs in the data to interpolate over in milliseconds. Default 75 ms
#'@param marg_ms The margin in milliseconds before and after the gap to use as basis for interpolation.
#'@param filter_ms The size of the moving average window to use in smoothing. Default 15 ms
#'@param xcol Name of column containing unprocessed x coordinates
#'@param ycol Name of column containing unprocessed y coordinates
#'@return data frame with gaze data after interpolation and filtering
#'@examples
#'processed_gaze <- process_gaze(sample.data.unprocessed)

process_gaze <- function(gaze_raw, max_gap_ms = 75, marg_ms = 5, filter_ms = 15, xcol = "x.raw", ycol = "y.raw") {
  warning('This function will be replaced with preprocess_gaze in future versions')
  output <- preprocess_gaze(gaze_raw = gaze_raw, max_gap_ms = max_gap_ms, marg_ms = 30, filter_ms = 15, xcol = xcol, ycol = ycol)
  return(output)
}



#'Interpolate over gaps (subsequent NAs) in vector.
#' @param data_in Vector to interpolate in
#' @param marg Margin in samples before and after gap to use for interpolation
#' @param max_gap Maximum length of gaps in sample
#' @return vector with interpolated gaps

interpolate_with_margin <- function(data_in, marg, max_gap){

  #Funktion för att interpolera över gaps i en array. Interpolering med marginal (median av flera samples) i båda ändarna.
  na_index <- is.na(data_in)
  data_rle <- rle(na_index)

  #Kod för att hitta längd på perioder av NAs för interpolering
  na_starts = numeric()
  na_ends = numeric()
  na_lengths = numeric()

  # Current index tracker
  current_index = 1

  # Loop through the rle lengths
  for (i in seq_along(data_rle$lengths)) {
    if (data_rle$values[i]) { # If this is a run of NAs
      na_starts = c(na_starts, current_index)
      na_ends = c(na_ends, current_index + data_rle$lengths[i] - 1)
      na_lengths = c(na_lengths, data_rle$lengths[i])
    }
    current_index = current_index + data_rle$lengths[i]
  }

  index_gap <- data.frame(start = na_starts, stop = na_ends, length = na_lengths)
  #Interpolera bara över tillräckligt korta gaps
  index_gap <- dplyr::filter(index_gap, length <= max_gap)
  if (dim(index_gap)[1] >0){
    for (i in 1:dim(index_gap)[1]){
      if (index_gap$start[i]-marg >0 & index_gap$stop[i]+marg <= length(data_in)){

        interpol.before <- median(data_in[(index_gap$start[i]-marg) :(index_gap$start[i]-1)], na.rm = T)
        interpol.after <-  median(data_in[(index_gap$stop[i]+1) :(index_gap$stop[i]+marg)], na.rm = T)
        data_in[index_gap$start[i]:index_gap$stop[i]] <- mean(c(interpol.before,interpol.after))
      }
    }
  }
  return(data_in)
}



#' Find subsequent periods in a vector with values below a threshold. Used internally by the function suggest_threshold
#' @description This function is used internally by suggest_threshold.
#' @param data_in Data to process
#' @param min_samples Minimum length of consecutive run in samples
#' @param margin Shrink the period of consecutive runs at both ends with this margin
#' @param threshold Search for values under this threshold


find.valid.periods <- function(data_in, threshold, min_samples, margin = 0) {
  below <- data_in <= threshold
  runs <- rle(below)  # TRUE = below-threshold run

  out <- rep(FALSE, length(data_in))
  idx <- 1
  for (i in seq_along(runs$lengths)) {
    run_length <- runs$lengths[i]
    run_value <- runs$values[i]
    if (run_value && run_length >= min_samples) {
      # Compute margins within this run
      start_idx <- idx + margin
      end_idx <- idx + run_length - margin - 1
      if (start_idx <= end_idx) {
        out[start_idx:end_idx] <- TRUE
      }
    }
    idx <- idx + run_length
  }
  return(out)
}





#' Data-driven identification of threshold parameters for adaptive veloctity-based saccade detection.
#' @description
#' The function is based on a procedure suggested by Nyström and Holmqvist 2010. Behavior Research Methods, 42, 188-204. The function can be used to identify
#' specific thresholds for saccade onset for individuals and/or segments of the data, as an alternative to using the same thresholds for each participants. It is
#' used in kollaR by the function 'algorithm_adaptive'
#'
#' Peak velocity and saccade amplitude are typically highly postively correlated. It is therefore important to consider that differences in gaze behavior
#' between individuals and/or data segment may lead to differences in proposed saccade onset velocity threshold.
#'#'
#' The input data should be pre-processed (e.g., noise removal and interpolation over gaps)
#' The output is a list with three cells: "peak.threshold" and "onset.threshold" are parameters used by the
#' function algorithm_adaptive (see Nyström and Holmqvist 2010 for details). "velocity" is a data frame with sample-to-sample velocity in the unit specified by the
#' parameter one_degree
#'
#'
#' @param gaze Data frame with gaze data before saccade and fixation data identification. The data frame must include the variable timestamp with
#' timing in milliseconds and columns for x and y coordinates specified by the columns 'xcol' and 'ycol' respectively.
#' @param xcol column in the gaze data frame where x coordinates are found. Default: x.raw
#' @param ycol column in the gaze data frame where y coordinates are found. Default: y.raw
#' @param peak.threshold.start initial peak threshold value in degrees of the visual field. Default: 200
#' @param one_degree one degree of the visual field in the unit of the x and y coordinates in the data. Typically pixels or degrees.
#' @param velocity.filter.ms If velocity.filter.ms is not NA, the velocity vector is smoothed using a moving median filter corresponding to this value in ms
#' before the propose threshold is identified. Default: 10.
#' @param onset.threshold.sd sd of sample-by-sample velocities used to select the proposed velocity threshold (proposed.velocity.threshold)
#' @param min.period.ms Update the peak velocity thresholds iteratively based on data within consecuitive runs of samples below the previous thresholds. Should be approximately
#' minimum fixation duration.
#' @param margin.ms A margin around min.period.ms. This reduces the risk that samples included in the threshold estimation belong o a saccade
#' @return list including separate data frames for proposed saccade onset threshold, peak threshold, and sample-to-sample velocity

suggest_threshold <- function(gaze, velocity.filter.ms =10, one_degree =40,
                              ycol = "y.raw", xcol = "x.raw", peak.threshold.start = 130, onset.threshold.sd = 3, min.period.ms =40,
                              margin.ms = 3)
{



  #Return an error message if one degree is specified to extend beyond the maximum x coordinate.
  if(max(gaze[[xcol]],na.rm = TRUE)< one_degree) {
    warning("Make sure that gaze coordinates are in the same scale as the parameter one_degree!")
    warning("The current settings assume that the horizontal point of gaze does not move further than one degree. This may be an error which may lead the function to crash")

  }

  #STEP 1: CALCULATE SAMPLE-TO-SAMPLE VELOCITY

  one.sample <- mean(diff(gaze$timestamp),na.rm = T)
  if (one.sample < 0.02) {warning("Unlikely small sample to sample difference in timestamps. Are timestamps in milliseconds?")}


  message("Calculating proposed velocity threshold")



  gaze$xdiff<- c(NA, diff(gaze[[xcol]]))
  gaze$ydiff <- c(NA, diff(gaze[[ycol]]))


  gaze$distance <- sqrt(gaze$xdiff^2+gaze$ydiff^2)
  #Recalculate to degrees of the visual field
  gaze$distance <- gaze$distance/one_degree


  gaze$velocity <- gaze$distance/one.sample #Velocity per second (assuming that timestamps are in ms!)


  if (!is.na(velocity.filter.ms)) {
      #Calculate samples to smooth velocity vector
      velocity.smooth.window <- round(velocity.filter.ms/one.sample)
      #Smoooth the velocity vector
      gaze$velocity <-rollmedian(gaze$velocity, k = velocity.smooth.window, na.pad = T, align = "center")
  }


  #Recalculate to degreees per second
  gaze$velocity <- gaze$velocity*1000





  #STEP 2 - FIND A DATA DRIVEN VELOCITY THRESHOLD
  min.period.samples <- round(min.period.ms/one.sample)
  margin.samples <- round(margin.ms/one.sample)


  threshold.proposals <- data.frame()
  pt <- peak.threshold.start
  i <- 1
  finished <- FALSE
  while (!finished){



    keep <- find.valid.periods(gaze$velocity, threshold = pt,min_samples = min.period.samples, margin = margin.samples)

    m = mean(gaze$velocity[keep],na.rm=T)
    s = sd(gaze$velocity[keep],na.rm=T)
    proposed.peak.threshold <- m + (6*s)
    proposed.onset.threshold <- m + (onset.threshold.sd*s)

    if  (abs(pt - proposed.peak.threshold) <=1) {
      final.peak.threshold <- proposed.peak.threshold
      finished = TRUE
    } else {
      pt <- proposed.peak.threshold
      i <- i +1
    }
    threshold.proposals <- rbind(threshold.proposals,
                                 data.frame(iteration = i, peak.threshold = pt, onset.threshold = proposed.onset.threshold)
    )

  }


  if (i < 3) {
    warning(
      paste0("The peak threshold algorithm stopped after only ", i, " iterations. The identified threshold may be unreliable. Check for noise and/or artifacts in the data. It may help to adjust the parameter peak.threshold.start downwards.")

    )

  }



  m = mean(gaze$velocity[gaze$velocity<final.peak.threshold],na.rm=T)
  s = sd(gaze$velocity[gaze$velocity<final.peak.threshold],na.rm=T)
  proposed.velocity.threshold <- m + (3*s)

  out <- list()
  out[["onset.threshold"]] <- proposed.velocity.threshold
  out[["peak.threshold"]] <- final.peak.threshold
  out[["velocity"]] <- gaze$velocity
  #out[["iterations"]] <- threshold.proposals

  return(out)


}
