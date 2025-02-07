
#'Interpolation and smoothing of gaze-vector
#'@description Preprocessing of gaze vector
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

process_gaze <- function(gaze_raw, max_gap_ms = 75, marg_ms = 30, filter_ms = 15, xcol = "x.raw", ycol = "y.raw") {
  #Recalculate these parameters to number of samples depending on the sampling rate in the gaze vector. Make sure
  #there is a variable called 'timestamp' in the unit ms
  one.sample <- median(diff(gaze_raw$timestamp[1:500]),na.rm = T)

  #Save unprocessed variables in the matrix
  gaze_raw$x.unprocessed <- gaze_raw[["x.raw"]]
  gaze_raw$y.unprocessed <- gaze_raw[["y.raw"]]

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
