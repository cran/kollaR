
#' Merge adjacent fixations
#' @description Merge fixations which appear close in space and time. This function is called by other functions and typically
#' not used outside them
#' @param fixations Data frame with fixations
#' @param gaze_raw Data matrix with raw data. See description of the algorithm_ivt function
#' @param one_degree One degree of the visual field in the scale of the x and y coordinates. Typically pixels or proportion of the screen. Make sure the setting matches your data.
#' @param xcol X coordinates in the raw gaze data matrix (gaze_raw)
#' @param ycol Y coordinates in the raw gaze data matrix (gaze_raw)
#' @param ms.threshold Maximum time elapsed between fixations to be merged.
#' @param distance.threshold Subsequent fixations occurring withing this distance are merged. Set to 0 if you don't want to merge fixations.
#' @return A new data frame with fixations

merge_adjacent_fixations <- function (fixations, gaze_raw, distance.threshold = 0.5, ms.threshold = 75, one_degree = 40,
                                      xcol = "x.raw", ycol = "y.raw") {
  #Loop through fixation candidates and merge adjacent fixations
  message("Merging adjacent fixations")
  i <- 1
  while (i < dim(fixations)[1]){
    d <- (sqrt((fixations$x[i] - fixations$x[i+1])^2 + (fixations$y[i] - fixations$y[i+1])^2))/one_degree #distance in degrees
    t <- fixations$onset[i+1] - fixations$offset[i] #Time elapsed between fixations in ms
    if(d < distance.threshold & t < ms.threshold) { #FIXATIONS ARE CLOSE. MERGE THIS AND THE NEXT FIXATIONS

      fixation.candidate.starts <- fixations$firstline[i]
      fixation.candidate.stops <- fixations$lastline[i+1] #Fixations are merged. The merged fixation ends with the last sample of the subsequent fixation in the current data


      merged_fixation <- summarize_fixation_metrics(fixation.candidate.starts, fixation.candidate.stops, gaze_raw[[xcol]], gaze_raw[[ycol]],
                                                    gaze_raw$timestamp, one_degree = one_degree)

      fixations$x[i] <- merged_fixation$x
      fixations$y[i] <- merged_fixation$y
      fixations$duration[i] <- merged_fixation$duration
      fixations$offset[i] <-  merged_fixation$offset
      fixations$firstline[i] <- fixation.candidate.starts
      fixations$lastline[i] = fixation.candidate.stops
      fixations$missing.samples[i] = merged_fixation$missing.samples
      fixations$rmsd[i] <- merged_fixation$rmsd
      fixations$rms.from.center[i] <- merged_fixation$rms.from.center




      fixations <- fixations[-(i+1),] #Remove the next fixation after merging its data with this one

      # i <- i+1

    } else {i <- i+1}
  }
  return(fixations)
}



#' Dispersion-based fixation detection algorithm \code{(I-DT)}
#' @description This function will be replaced by the function algorithm_idt in subsequent versions. The two functions take the
#' same input arguments.idt_filter is a wrapper around idt_algorithm.
#'
#' Apply a dispersion-based fixation \code{(I-DT)} detection algorithm to the eye tracking data.
#' The algorithm identifies fixations as samples clustering within a spatial area.
#' The procedure is described in Blignaut 2009
#' Input data must be a data frame with the variables timestamp, x.raw and y.raw as variables. Other variables can
#' be included but will be ignored. This function does not perform pre-processing in the form of interpolation or smoothing. Use the function process.gaze for this.
#' Timestamps are assumed to be in milliseconds.
#' The output data is a list with two data frames: fixations includes all detected fixations with coordinates, duration
#' and a number of other metrics, filt.gaze is a sample-by-sample data frame with time stamps, raw gaze coordinated and fixation coordinates.
#' The function can be slow for long recordings and/or data recorded at high sampling rates.
#' @param gaze_raw Data frame with gaze data before event detection. Include the variable timestamp with timing in ms and columns with raw
#' x and y data as specified by the parameters xcol and ycol or their default values
#' @param one_degree One degree of the visual field in the unit of the raw x and y coordinates. The unit is typically pixels or proportion
#' of the screen. Make sure that the setting matches your data
#' @param dispersion.threshold Maximum radius of fixation candidates. Samples clustering within a circle of this limit will be
#' classified as a fixation if the duration is long enough.
#' @param min.duration Minimum duration of fixations in milliseconds
#' @param xcol Name of the column where raw x values are stored. Default: x.raw
#' @param ycol Name of the column where raw y values are stored. Default: y.raw
#' @param distance.threshold Subsequent fixations occurring withing this distance are merged. Set to 0 if you don't want to merge fixations.
#' @param merge.ms.threshold Only subsequent fixations occurring within this time window are merged
#' @param missing.samples.threshold Remove fixations with a higher proportion of missing samples. Range 0-1
#' @examples
#' idt_data <- idt_filter(sample.data.processed)
#' @return list including separate data frames for fixations and sample-by-sample data including gaze coordinates with and without fixation detection.
#' The fixations data frame includes onset, offset, x, y, sample-to-sample root-mean-square deviations (RMSD, precision), RMSD from fixation centroid, and missing samples of each fixation.
idt_filter <- function(gaze_raw, one_degree = 40, dispersion.threshold =1, min.duration = 50, xcol = "x.raw", ycol = "y.raw",
                          distance.threshold = 0.7, merge.ms.threshold = 75, missing.samples.threshold = 0.5) {

  warning('This function will be replaced by algorithm_idt in future versions of kollaR')
  output <- algorithm_idt(gaze_raw, one_degree, dispersion.threshold, min.duration, xcol, ycol,
  distance.threshold, merge.ms.threshold, missing.samples.threshold)

  return(output)

}




#' Dispersion-based fixation detection algorithm \code{(I-DT)}
#' @description Apply a dispersion-based fixation \code{(I-DT)} filter to the eye tracking data.
#' The algorithm identifies fixations as samples clustering within a spatial area.
#' The procedure is described in Blignaut 2009
#' Input data must be a data frame with the variables timestamp, x.raw and y.raw as variables. Other variables can
#' be included but will be ignored. This function does not perform pre-processing in the form of interpolation or smoothing. Use the function preprocess_gaze for this.
#' Timestamps are assumed to be in milliseconds.
#' The output data is a list with two data frames: fixations includes all detected fixations with coordinates, duration
#' and a number of other metrics, filt.gaze is a sample-by-sample data frame with time stamps, raw gaze coordinates (e.g., before event detection) and
#' fixation coordinates.
#' The function can be very slow for long recordings and/or data recorded at high sampling rates.
#' @param gaze_raw Data frame with gaze data before fixation detection. Include the variable timestamp with timing in ms and columns with raw
#' x and y data as specified by the parameters xcol and ycol or their default values
#' @param one_degree One degree of the visual field in the unit of the raw x and y coordinates. The unit is typically pixels or proportion
#' of the screen. Make sure that the setting matches your data
#' @param dispersion.threshold Maximum radius of fixation candidates. Samples clustering within a circle of this limit will be
#' classified as a fixation if the duration is long enough.
#' @param min.duration Minimum duration of fixations in milliseconds
#' @param xcol Name of the column where raw x values are stored. Default: x.raw
#' @param ycol Name of the column where raw y values are stored. Default: y.raw
#' @param distance.threshold Subsequent fixations occurring withing this distance are merged. Set to 0 if you don't want to merge fixations.
#' @param merge.ms.threshold Only subsequent fixations occurring within this time window are merged
#' @param missing.samples.threshold Remove fixations with a higher proportion of missing samples. Range 0-1
#' @return list including separate data frames for fixations and sample-by-sample data including gaze coordinates before and after fixation detection.
#' The fixations data frame includes onset, offset, x, y, sample-to-sample root-mean-square deviations (RMSD, precision), RMSD from fixation centroid,  and missing samples of each fixation.


algorithm_idt <- function(gaze_raw, one_degree = 40, dispersion.threshold =1, min.duration = 50, xcol = "x.raw", ycol = "y.raw",
                       distance.threshold = 0.7, merge.ms.threshold = 75, missing.samples.threshold = 0.5) {
  #Apply a dispersion-based fixation filter.
  #Following Blignaut 2009

  #Create new variables in the raw gaze matrix with the expected variable names if
  #the data are stored under different names (specified in the xcol and ycol parameters)
  if (!"x.raw" %in% names(gaze_raw)){
    gaze_raw$x.raw <- gaze_raw[[xcol]]
    gaze_raw$y.raw <- gaze_raw[[ycol]]
  }
#Return an error message if one degree is specified to extend beyond the maximum x coordinate.
  if(max(gaze_raw[[xcol]],na.rm = TRUE)< one_degree) {
    warning("Make sure that gaze coordinates are in the same scale as the parameter one_degree!")
    warning("The current settings assume that the horizontal point of gaze does not move further than one degree. This may be an error which may lead the function to crash")

  }

  fixations <- data.frame() #Save fixations here
  #Create a data frame to store filtered x and y coordinates. Retain raw x and y for comparison
  filt.gaze <- data.frame(timestamp = gaze_raw$timestamp,
                          x.raw = gaze_raw$x.raw, y.raw = gaze_raw$y.raw,x = rep(NA,dim(gaze_raw)[1]), y = rep(NA,dim(gaze_raw)[1])) #Save filtered gaze coordinates here

  filt.gaze$fixation.candidate <- NA
  filt.gaze$d <- NA
  filt.gaze$cand.x <- NA
  filt.gaze$cand.y <- NA

  sample_index <- 2
  fixation.candidate <- FALSE
  while (sample_index < dim(gaze_raw)[1]-1){


    if (fixation.candidate == FALSE){
      #Calculate the Euclidean distance between this and the previous point
      d <- sqrt((gaze_raw$x.raw[sample_index-1] - gaze_raw$x.raw[sample_index])^2 + (gaze_raw$y.raw[sample_index-1] - gaze_raw$y.raw[sample_index])^2)

      if (d <= dispersion.threshold*one_degree & !is.na(d)) { #The two samples are within the spatial limit. May be a valid fixation
        fixation.candidate <- TRUE
        fixation.candidate.start <- sample_index-1
        #Calculate the tentative center of the fixation
        fixation.candidate.x <- mean(gaze_raw$x.raw[fixation.candidate.start:sample_index], na.rm = T)
        fixation.candidate.y <- mean(gaze_raw$y.raw[fixation.candidate.start:sample_index], na.rm = T)

      }
      #    sample_index <- sample_index +1 #Continue to next sample
    } else if (fixation.candidate == TRUE) { #A tentative fixation has already been identified
      #  d <- sqrt((gaze_raw$x.raw[sample_index-1] - gaze_raw$x.raw[sample_index])^2 + (gaze_raw$y.raw[sample_index-1] - gaze_raw$y.raw[sample_index])^2)
      #Calculate the distance from this sample and the mean of the proposed fixation
      dist.from.center <- sqrt((fixation.candidate.x - gaze_raw$x.raw[sample_index])^2 + (fixation.candidate.y - gaze_raw$y.raw[sample_index])^2)

      if (dist.from.center <= dispersion.threshold*one_degree & !is.na(dist.from.center)){
        #Update the tentative center of the fixation
        fixation.candidate.x <- mean(gaze_raw$x.raw[fixation.candidate.start:sample_index], na.rm = T)
        fixation.candidate.y <- mean(gaze_raw$y.raw[fixation.candidate.start:sample_index], na.rm = T)

        filt.gaze$cand.x[sample_index] <- fixation.candidate.x
        filt.gaze$cand.y[sample_index] <- fixation.candidate.y


      } else if (dist.from.center > dispersion.threshold*one_degree | is.na(dist.from.center)) {
        #A candidate fixation has been detected but the next sample is outside the dispersion threshold. Fixation has ended
        #Summarize the fixation metrics and save them.Fixation ends if a NA value appears

        this.fixation <- summarize_fixation_metrics(fixation.candidate.start, sample_index,
                                                    x = gaze_raw$x.raw, y = gaze_raw$y.raw, timestamp = gaze_raw$timestamp,
                                                    one_degree = one_degree)

        fixations <- rbind(fixations, this.fixation)


        #Set these values to default to start looking for the next fixation
        fixation.candidate <- FALSE
        fixation.candidate.start <- NA
        fixation.candidate.x <- NA
        fixation.candidate.y <- NA
      } else if (is.na(d)) {  #Stop counting if there is a NA
        fixation.candidate <- FALSE
        fixation.candidate.start <- NA
        fixation.candidate.x <- NA
        fixation.candidate.y <- NA

      }



    }

    sample_index <- sample_index +1 #Continue to next sample

  }

  #Merge adjacent fixations
  if (distance.threshold >0){
      fixations <- merge_adjacent_fixations(fixations, filt.gaze, distance.threshold = distance.threshold, ms.threshold = merge.ms.threshold, one_degree = one_degree)
  }
  fixations <- dplyr::filter(fixations, .data$duration > min.duration) #Remove short fixations
  fixations <- dplyr::filter(fixations, .data$missing.samples < missing.samples.threshold)


  #Save filtered x and y coordinates for each sample
  for (i in 1: dim(fixations)[1]){
    filt.gaze$x[fixations$firstline[i]: fixations$lastline[i]] <- fixations$x[i]
    filt.gaze$y[fixations$firstline[i]: fixations$lastline[i]] <- fixations$y[i]


  }


  fixations$fixation.algorithm <- "idt"
  fixations$threshold <- paste0(round(dispersion.threshold), " deg.")
  out <- list()
  out[['fixations']] <- fixations
  out[['filt.gaze']] <- filt.gaze
  return(out)
}

#'I-VT algorithm for fixation and saccade detection
#'@description Apply an \code{I-VT} event detection algorithm to the eye tracking data.
#' The algorithm identifies saccades as periods with sample-to-sample velocity above a threshold and fixations as periods between saccades.
#' See Salvucci and Goldberg 2000. Identifying fixations and saccades in eye tracking protocols. Proc. 2000 symposium on Eye tracking
#' research and applications for a description.
#'
#' Input data must be a data frame with the variables timestamp, x.raw and y.raw as variables. Other variables can
#' be included but will be ignored. This function does not perform pre-processing in the form of interpolation or smoothing. Use the function preprocess_gaze for this.
#' Timestamps are assumed to be in milliseconds. X and y coordinates can be in pixels or proportion of the screen
#' depending on the format of your data. Make sure that the parameter one_degree matches the unit of your data
#' The output data is a list with three data frames: fixations includes all detected fixations with coordinates, duration
#' and a number of other metrics, saccades includes data for saccades, filt.gaze is a sample-by-sample data frame with time stamps, and gaze coordinates before ("raw") and after fixation detection.
#' The function has a number of parameters for removing potentially invalid fixations and saccades. The parameter min.fixation.duration can be used to remove unlikely
#' short fixations. If the parameter missing.samples threshold is set to a value lower than 1, fixations with a higher proportion of missing raw samples are removed.
#'
#'
#'
#' @param gaze_raw Data frame with gaze data before fixation and saccade detection. Include the variable timestamp with timing in ms and columns with raw
#' x and y data as specified by the parameters xcol and ycol or their default values
#' @param one_degree One degree of the visual field in the unit of the raw x and y coordinates, typically pixels or proportion of the screen. Make sure that
#' it is consistent with the format of your data
#' @param velocity.threshold Velocity threshold for saccade detection in degrees/second
#' @param velocity.filter.ms Window in milliseconds for moving average window used for smoothing the sample to sample velocity vector.
#' @param min.saccade.duration Minimum duration of saccades in milliseconds
#' @param missing.samples.threshold Remove fixations with a higher proportion of missing samples. Range 0 to 1.
#' @param min.fixation.duration Minimum duration of fixations in milliseconds
#' @param xcol Name of the column where raw x values are stored. Default: x.raw
#' @param ycol Name of the column where raw y values are stored. Default: y.raw
#' @param distance.threshold Subsequent fixations occurring withing this distance are merged. Set to 0 if you don't want to merge fixations.
#' @param merge.ms.threshold Subsequent fixations occuring within this time window and distance specified by distance.threshold are merged.
#' Set to 0 if you don't want to merge fixations.
#' @param trim.fixations If TRUE, the onset of each fixation will be shifted forwards to the first non-missing (non-NA) sample during the period.
#' The offset will be shifted backwards to the last non-missing
#' sample. If TRUE, and the parameter trim.dispersion.threshold is a positive number, samples at the margins with large distances from the centroid
#' will also be excluded
#' @param trim.dispersion.threshold If not NA and trim.fixations is TRUE, fixation onsets and offsets will also be shrinked to exclude any samples
#' at the margins with a larger distance from the centroid than trim.dispersion.threshold*MAD.
#' @param save.velocity.profiles If TRUE, return velocity profiles of each detected saccade as a variable in the saccades data frame
#' @return list including separate data frames for fixations and sample-by-sample data including x and y coordinates before and after fixation detection.
#' The fixations data frame gives onset, offset, x, y, sample-to-sample RMSD (precision),root-mean-square deviations from fixation centroid,  and missing samples of each fixation.
#' @examples
#' ivt_data <- algorithm_ivt(sample.data.processed, velocity.threshold = 30,
#' min.fixation.duration = 40)

algorithm_ivt <- function(gaze_raw, velocity.filter.ms =20, velocity.threshold = 35, min.saccade.duration = 10, min.fixation.duration = 40,
                        one_degree =40, save.velocity.profiles = FALSE,xcol = "x.raw", ycol = "y.raw", distance.threshold = 0.7,
                        merge.ms.threshold = 75, missing.samples.threshold = 0.5, trim.fixations = FALSE, trim.dispersion.threshold =NA){


  #Create new variables in the raw gaze matrix with the expected variable names if
  #the data are stored under different names (specified in the xcol and ycol parameters)
  if (!"x.raw" %in% names(gaze_raw)){
    gaze_raw$x.raw <- gaze_raw[[xcol]]
    gaze_raw$y.raw <- gaze_raw[[ycol]]
  }

  #Return an error message if one degree is specified to extend beyond the maximum x coordinate.
  if(max(gaze_raw[[xcol]],na.rm = TRUE)< one_degree) {
    warning("Make sure that gaze coordinates are in the same scale as the parameter one_degree!")
    warning("The current settings assume that the horizontal point of gaze does not move further than one degree. This may be an error which may lead the function to crash")

  }


  fixations <- data.frame() #Save fixations here
  saccades <- data.frame() #Save saccades here
  #velocity.profiles <- list() #Save velocity profiles here if requested in the function input. Otherwise leave empty
  #Create a data frame to store filtered x and y coordinates. Retain raw x and y for comparison
  filt.gaze <- data.frame(timestamp = gaze_raw$timestamp,
                          x.raw = gaze_raw$x.raw, y.raw = gaze_raw$y.raw,x = rep(NA,dim(gaze_raw)[1]), y = rep(NA,dim(gaze_raw)[1])) #Save filtered gaze coordinates here

  velocity.profile <- list() #Leave empty if the parameter is not specified as TRUE


  one.sample <- mean(diff(gaze_raw$timestamp),na.rm = T)
  if (one.sample < 0.02) {warning("Unlikely small sample to sample difference in timestamps. Are timestamps in milliseconds?")}

  #Calculate samples to smooth velocity vector
  velocity.smooth.window <- round(velocity.filter.ms/one.sample)

  #Detect saccades first
  message("Calculating saccades")
  #Calculate sample to sample distance in raw unit (pixels or proportion of screen)
  gaze_raw$distance <- c(NA,sqrt(diff(gaze_raw$x.raw)^2 +diff(gaze_raw$y.raw)^2))
  #Recalculate to degrees of the visual field
  gaze_raw$distance <- gaze_raw$distance/one_degree

  #
  gaze_raw$velocity <- gaze_raw$distance/one.sample #Velocity per second (assuming that timestamps are in ms!)

  gaze_raw$velocity <- gaze_raw$velocity*1000

  #Smoooth the velocity vector
  gaze_raw$velocity <-rollmedian(gaze_raw$velocity, k = velocity.smooth.window, na.pad = T, align = "center")


  v <- gaze_raw$velocity>velocity.threshold

  #Detect the samples where gaze velocity first exceeds the specified threshold for saccade detection
  saccade.starts <- numeric()
  saccade.ends <- numeric()
  data_rle <- rle(v)
  current_index <- 1
  # Loop through the rle lengths
  for (i in seq_along(data_rle$lengths)) {
    if (!is.na(data_rle$values[i])) {

      if (data_rle$values[i] == TRUE) {
        saccade.starts = c(saccade.starts, current_index)
        saccade.ends = c(saccade.ends, current_index + data_rle$lengths[i] - 1)
        #  na_lengths = c(na_lengths, data_rle$lengths[i])
      }
    }
    current_index = current_index + data_rle$lengths[i]
  }

  #Loop through candidate saccades and save the metrics
  for (i in seq_along(saccade.starts)) {

    #Where did the saccade start and land?
    x.onset = gaze_raw$x.raw[saccade.starts[i]]
    y.onset = gaze_raw$y.raw[saccade.starts[i]]
    x.offset = gaze_raw$x.raw[saccade.ends[i]]
    y.offset = gaze_raw$y.raw[saccade.ends[i]]


    #Calculate the Euclidean distance between onset and offset (amplitude)
    amplitude <- sqrt((x.onset - x.offset)^2 + (y.onset-y.offset)^2)
    #Normalize to degrees
    amplitude <- amplitude/one_degree


    #Save velocity profiles if this is specified in the input
    if (save.velocity.profiles == TRUE) {
      velocity.profile[[i]] <- gaze_raw$velocity[saccade.starts[i]:saccade.ends[i]]

    }


    saccades <- rbind(saccades,
                      data.frame(
                        onset = gaze_raw$timestamp[saccade.starts[i]],
                        x.onset = x.onset,
                        y.onset = y.onset,
                        offset = gaze_raw$timestamp[saccade.ends[i]],
                        x.offset = gaze_raw$x.raw[saccade.ends[i]],
                        y.offset = gaze_raw$y.raw[saccade.ends[i]],
                        duration = gaze_raw$timestamp[saccade.ends[i]] - gaze_raw$timestamp[saccade.starts[i]],
                        amplitude = amplitude,
                        peak.velocity = max(gaze_raw$velocity[saccade.starts[i]:saccade.ends[i]], na.rm = T),
                        missing.samples = mean(is.na(gaze_raw$x.raw[saccade.starts[i]:saccade.ends[i]]))
                      )

    )



  }

  #Save velocity profiles of each saccade if requested
  if (save.velocity.profiles == TRUE) {
    saccades$velocity.profile <- velocity.profile

  }
  saccades <- dplyr::filter(saccades, .data$duration >= min.saccade.duration)


  #Loop through all saccades. Define fixations as intervals between saccades.
  #Summarize the fixation metrics and save them
  message("Calculating fixations")
  fixation.starts <- saccade.ends

  #If the recording ends with a fixation and not a saccade, keep it. Add the end of the recording to the array saccade.starts which is used to detect ends of fixations
  if (length(fixation.starts) >= length(saccade.starts)) {

    saccade.starts <- c(saccade.starts, nrow(gaze_raw))}



  for (i in seq_along(fixation.starts)) {

    fixation.candidate.starts <- fixation.starts[i]
    fixation.candidate.stops <- saccade.starts[i+1]-1

    this.fixation <- summarize_fixation_metrics(fixation.candidate.starts, fixation.candidate.stops,
                                                x = gaze_raw$x.raw, y = gaze_raw$y.raw, timestamp = gaze_raw$timestamp, one_degree = one_degree)

    fixations <- rbind(fixations, this.fixation)



  }





  #Step 3: Loop through fixation candidates and merge adjacent fixations
 #Exclude NAs and/or samples with large deciations from the centroid
  if (trim.fixations == TRUE){

    fixations <- trim_fixations(fixations, filt.gaze, xcol = xcol, ycol = ycol,threshold = trim.dispersion.threshold)
  }



  #Merge adjacent fixations
  if (distance.threshold >0){
    fixations <- merge_adjacent_fixations(fixations, filt.gaze, distance.threshold = distance.threshold, ms.threshold = merge.ms.threshold, one_degree = one_degree)
  }


  #Remove too short fixations
  #fixations <- dplyr::filter(fixations, .data$duration >=min.fixation.duration)


  fixations <- dplyr::filter(fixations, .data$duration >= min.fixation.duration)
  fixations <- dplyr::filter(fixations, .data$missing.samples < missing.samples.threshold)

  #Save filtered x and y coordinates for each sample
  for (i in 1: dim(fixations)[1]){
    filt.gaze$x[fixations$firstline[i]: fixations$lastline[i]] <- fixations$x[i]
    filt.gaze$y[fixations$firstline[i]: fixations$lastline[i]] <- fixations$y[i]


  }



  fixations$fixation.algorithm <- "ivt"
  fixations$threshold <- paste0(round(velocity.threshold), " deg.")

  out <- list()
  out[["saccades"]] <- saccades
  out[["fixations"]] <-fixations
  out[["filt.gaze"]] <- filt.gaze

  return(out)

}


#'I-VT algorithm for fixation and saccade detection
#'@description Apply an \code{I-VT} filter to the eye tracking data.
#' This function is a wrapper around the function ivt_algorithm. It will be replaced by algorithm_ivt in future versions.
#' The algorithm identifies saccades as periods with sample-to-sample velocity above a threshold and fixations as periods between saccades.
#' See Salvucci and Goldberg 2000. Identifying fixations and saccades in eye tracking protocols. Proc. 2000 symposium on Eye tracking
#' research and applications for a description.
#'
#' Input data must be a data frame with the variables timestamp, x.raw and y.raw as variables. Other variables can
#' be included but will be ignored. This function does not perform pre-processing in the form of interpolation or smoothing. Use the function process.gaze for this.
#' Timestamps are assumed to be in milliseconds.
#' The output data is a list with three data frames: fixations includes all detected fixations with coordinates, duration
#' and a number of other metrics, saccades includes data for saccades, filt.gaze is a sample-by-sample data frame with time stamps, and gaze coordinates before ("raw") and
#' after fixation detection.
#' The function has a number of parameters for removing potentially invalid fixations and saccades. The parameter min.fixation.duration can be used to remove unlikely
#' short fixations. If the parameter missing.samples threshold is set to a value lower than 1, fixations with a higher proportion of missing raw samples are removed.
#'
#'
#'
#' @param gaze_raw Data frame with gaze data before fixation and saccade detection. Include the variable timestamp with timing in ms and columns with raw
#' x and y data as specified by the parameters xcol and ycol or their default values
#' @param one_degree One degree of the visual field in the unit of the raw x and y coordinates, typically pixels or proportion of the screen. Make sure that
#' it is consistent with the format of your data
#' @param velocity.threshold Velocity threshold for saccade detection in degrees/second
#' @param velocity.filter.ms Window in milliseconds for moving average window used for smoothing the sample to sample velocity vector.
#' @param min.saccade.duration Minimum duration of saccades in milliseconds
#' @param missing.samples.threshold Remove fixations with a higher proportion of missing samples. Range 0 to 1.
#' @param min.fixation.duration Minimum duration of fixations in milliseconds
#' @param xcol Name of the column where raw x values are stored. Default: x.raw
#' @param ycol Name of the column where raw y values are stored. Default: y.raw
#' @param distance.threshold Subsequent fixations occurring withing this distance are merged. Set to 0 if you don't want to merge fixations.
#' @param merge.ms.threshold Subsequent fixations occuring within this time window and distance specified by distance.threshold are merged. Set to 0 if you don't want to merge fixations.
#' @param trim.fixations If TRUE, the onset of each fixation will be shifted forwards to the first non-missing (non-NA) sample during the period. The offset will be shifted backwards to the last non-missing
#' sample. If TRUE, and the parameter trim.dispersion.threshold is a positive number, samples at the margins with large distances from the centroid will also be excluded
#' @param trim.dispersion.threshold If not NA and trim.fixations is TRUE, fixation onsets and offests will also be shrinked to exclude any samples at the margins with a larger
#' @param save.velocity.profiles If TRUE, return velocity profiles of each detected saccade as a variable in the saccades data frame
#' @return list including separate data frames for fixations and sample-by-sample data including gaze coordinates before and after fixation detection.
#' The fixations data frame gives onset, offset, x, y, sample-to-sample root-mean-square deviations (RMSD, precision), RMSD from fixation centroid,  and missing samples of each fixation.
#' @examples
#' ivt_data <- ivt_filter(sample.data.processed, velocity.threshold = 30, min.fixation.duration = 40)

ivt_filter <- function(gaze_raw, velocity.filter.ms =20, velocity.threshold = 35, min.saccade.duration = 10, min.fixation.duration = 40, one_degree =40, save.velocity.profiles = FALSE,
                          xcol = "x.raw", ycol = "y.raw", distance.threshold = 0.7, merge.ms.threshold = 75, missing.samples.threshold = 0.5, trim.fixations = FALSE, trim.dispersion.threshold =NA){

  output <- algorithm_ivt(gaze_raw = gaze_raw, velocity.filter.ms = velocity.filter.ms, velocity.threshold = velocity.threshold,
                          min.saccade.duration = min.saccade.duration, min.fixation.duration = min.fixation.duration,
                          one_degree = one_degree, save.velocity.profiles = save.velocity.profiles,
                          xcol = xcol, ycol = ycol, distance.threshold = distance.threshold, merge.ms.threshold = merge.ms.threshold,
                          missing.samples.threshold = missing.samples.threshold,
                          trim.fixations = trim.fixations, trim.dispersion.threshold = trim.dispersion.threshold)

  return(output)
  message('This function will be replaced by ivt_algorithm in future versions of kollaR')
}


#' Find transition weights for each sample in a gaze matrix.
#' @description This function is used internally by the function algorithm_i2mc
#' @param data_in Input data
#' @param window.step.size Step size
#' @param windowsize Window size
#' @return transition weights.


find.transition.weights <- function(data_in, window.step.size = 6, windowsize) {
  transition.weights <- as.list(rep(NA,dim(data_in)[1])) #Store fixation weights for each sample here

  quit.loop = FALSE
  this.onset <- 1
  onsets <- this.onset

  while (quit.loop == FALSE){

    this.offset <- this.onset+windowsize

    if (!this.offset>max(data_in$sample)){
      this.window <- select(data_in, .data$x.raw,.data$y.raw)[this.onset:this.offset,]
      find.nas <-which(is.na(this.window$x.raw) | is.na(this.window$y.raw))
      if (length(find.nas)>0){ #The selected window contains NAs. Move window to after the last NA
        this.onset <-this.onset+max(find.nas)+1
      } else { #NO NAs in this window. Continue with analysis


        k2 <- kmeans(this.window, centers = 2)
        transitions <- diff(k2$cluster)
        transitions <- abs(transitions)
        n.transitions <- sum(transitions != 0, na.rm =T)
        w <- transitions/n.transitions #Transition weights for this interval

        for (i in 1:length(w)){
          transition.weights[[this.onset+i]] <- c(transition.weights[[this.onset+i]], w[i])
        }
      }

    } else (quit.loop = TRUE) #The offset of the window is after the last sample. Quit loop

    this.onset <- this.onset + window.step.size
    onsets <- c(onsets, this.onset)
  }
  return(transition.weights)
}


#' Downsample gaze
#' @description This function downs-samples gaze by a specified factor. Data are down-sampled by splitting the data in bins and calculating the mean of each bin.
#' @param ds.factor The factor to down-sample by. For example, setting ds.factor to 10 down-samples data recorded at 1000 HZ to 100 HZ.
#' @param data_in Data frame which must contain the variables specified by the parameters xcol and ycol.
#' @param xcol Name of the column where raw x values are stored. Default: x
#' @param ycol Name of the column where raw y values are stored. Default: y
#' @return Data frame with downsampled gaze data. The output variables are x, y, and the numbers of the first and last samples of the original data frame included in the bin.
downsample_gaze <- function (data_in, ds.factor, xcol = "x", ycol = "y"){
  n.bins <- floor(dim(data_in)[1]/ds.factor)
  bin <-rep(1:n.bins, each = ds.factor)
  data_in$bin <- NA
  data_in$bin[1:length(bin)] <- bin
  downsampled_df <- data_in %>%
    group_by(bin) %>%
    summarize(x.raw = mean(.data[[xcol]]), y.raw = mean(.data[[ycol]]), first.sample = min(sample), last.sample = max(sample))
return(downsampled_df)
}




#' Fixation detection by two-means clustering
#' @description Identify fixations in a gaze matrix using identification by two-means clustering. The algorithm is based on Hessels et al 2017. Behavior research methods, 49, 1802-1823.
#' Data from the left and right eye are not processed separately. Adjust your analysis scripts to include this steps if you want the algorithm to include this step, as in Hessels et al 2017.
#' Input data must be a data frame with the variables timestamp, x.raw and y.raw as variables. Other variables can
#' be included but will be ignored. This function does not perform pre-processing in the form of interpolation or smoothing. Use the function process.gaze for this.
#' Timestamps are assumed to be in milliseconds. X and y coordinates can be in pixels or proportion of the screen. Make sure that the parameter one_degree is consistent with the format of your data.
#' The output data is a list with two data frames: fixations includes all detected fixations with coordinates, duration
#' and a number of other metrics, filt.gaze is a sample-by-sample data frame with time stamps, raw gaze coordinates (e.g., before fixation detection)
#' and fixation coordinates.
#' If the input downsampling.factors is not empty, transition weights will be calculated based on the data in the original sampling rate and data at all sampling rate specified in this variable.
#' According to Hessels et al 2017, this step makes the analysis less vulnerable to noise in the data.
#' If the parameter threshold.on.off is not NA, the onsets and offsets of fixations will be shifted to exclude samples at the margins with a larger absolute distance from the
#' fixation centroid than a threshold value. The threshold value is defined as the median of all absolute distances from the centroid plus
#' threshold.on.off * MAD of the absolute distances. Default threshold is 3. Samples with large distances from the fixation centroid right at the onset and offset of a fixation may
#' belong to a saccade.
#'
#' @param gaze_raw Data frame with gaze data prior to fixation detection. Include the variable timestamp with timing in ms and columns with
#' x and y data as specified by the parameters xcol and ycol or their default values
#' @param one_degree One degree of the visual field in the unit of the raw x and y coordinates, typically pixels or proportion of the screen. Make
#' sure that the setting matches the format of your data
#' @param distance.threshold Subsequent fixations occurring withing this distance are merged. Set to 0 if you do not want to merge fixations.
#' @param merge.ms.threshold Only fixations occurring within this time window in milliseconds are merged
#' @param window.step.size Distance between starting points of subsequent analysis windows in samples
#' @param windowlength.ms Length of the moving analysis windows
#' @param windowlength.ms Length of the moving analysis windows
#' @param weight.threshold Samples with a transition weight exceeding it are candidates for fixation detection.
#' @param min.fixation.duration Minimum duration of accepted fixations. Shorter fixations are discarded
#' @param downsampling.factors Factors to downsample the data by in calculating fixation weights. If downsampling.factors has the values \code{c(10, 2)}, transition weights will be calculated base on data in
#' the original sampling rate as well as the two downsampled data sets.
#' @param xcol Name of the column where raw x values are stored. Default: x.raw
#' @param ycol Name of the column where raw y values are stored. Default: y.raw
#' @param missing.samples.threshold Remove fixations with a higher proportion of missing samples. Range 0 to 1.
#' @param threshold.on.off if not NA, shift fixation onset and offset to exclude samples at the margin with absolute distances from the fixation center > threshold.on.off * MAD (distance) + median (distance)
#' @return list including separate data frames for fixations and sample-by-sample data including gaze coordinates before ("raw") and after fixation detection.
#'The "fixations" data frame gives onset, offset, x, y, sample-to-sample root-mean-square deviations (RMSD, precision), RMSD from fixation centroid, and missing samples of each fixation.
#' @examples
#' gaze <- algorithm_i2mc(sample.data.processed)


algorithm_i2mc <- function (gaze_raw, windowlength.ms = 200, distance.threshold = 0.7, one_degree = 40, window.step.size = 6,
                       min.fixation.duration = 40, weight.threshold = 2, xcol = "x.raw", ycol = "y.raw", merge.ms.threshold = 40,
                       downsampling.factors =NA, missing.samples.threshold = 0.5, threshold.on.off = 3) {

  if (!"timestamp" %in% names(gaze_raw)) {warning("Variable timestamp missing in gaze matrix")}

  one.sample <- mean(diff(gaze_raw$timestamp))
  windowsize <- round(windowlength.ms/one.sample) #Windowsize for two means clustering. Size adjusted for sampling rate

  #Make sure that the gaze matrix has a variable called sample starting with 1
  gaze_raw$sample <- seq(1,dim(gaze_raw)[1])

  #Create new variables in the raw gaze matrix with the expected variable names if
  #the data are stored under different names (specified in the xcol and ycol parameters)
  if (!"x.raw" %in% names(gaze_raw)){
    gaze_raw$x.raw <- gaze_raw[[xcol]]
    gaze_raw$y.raw <- gaze_raw[[ycol]]
  }

  #Return an error message if one degree is specified to extend beyond the maximum x coordinate.
  if(max(gaze_raw[[xcol]],na.rm = TRUE)< one_degree) {
    warning("Make sure that gaze coordinates are in the same scale as the parameter one_degree!")
    warning("The current settings assume that the horizontal point of gaze does not move further than one degree. This may be an error which may lead the function to crash")

  }


  fixations <- data.frame() #Save fixations here
  #Save a matrix with filtered and unfiltered gaze coordinates by sample
  filt.gaze <- data.frame(timestamp = gaze_raw$timestamp,
                          x.raw = gaze_raw$x.raw, y.raw = gaze_raw$y.raw,x = rep(NA,dim(gaze_raw)[1]), y = rep(NA,dim(gaze_raw)[1])) #Save filtered gaze coordinates here


  message("Searching for fixations")

  #Step 1 - calculate transition weights for the data at original sampling rate
  message("Calculating transition weights at original sampling rate")
  transition.weights <- find.transition.weights(gaze_raw, window.step.size = window.step.size, windowsize = windowsize)
  transition.weights <- lapply(transition.weights, mean, na.rm = TRUE) #Calculate a mean for the original sampling rate


  #Step 2 - calculate transition weights for downsampled data
  if (sum(!is.na(downsampling.factors)) >0) {
    message("Calculating fixation weights in downsampled data")
    for (ds.factor in downsampling.factors){
      message(paste0("Downsampling by factor: ", ds.factor))
      ds <- downsample_gaze(gaze_raw, ds.factor = ds.factor, xcol ="x.raw", ycol ="y.raw")
      ds$sample <- seq(1, nrow(ds))
      windowsize.ds <- floor(windowsize/ds.factor)
      ds.weights <- find.transition.weights(ds, window.step.size = window.step.size, windowsize = windowsize.ds)
      ds.weights <- lapply(ds.weights, mean, na.rm = TRUE) #Calculate a transition weight for each bin in the downsampled data

      #Add the transition weight for each sample calculated in downsampled data
      for (this.bin in 1:nrow(ds)){
        for(this.sample in ds$first.sample[this.bin]: ds$last.sample[this.bin]){
          transition.weights[[this.sample]] <- c(transition.weights[[this.sample]],
                                                 ds.weights[[this.bin]]
          )

        }

      }

    }
  }

  #Step 3 calculate fixations based on information about when cluster weights exceed threshold
  m <- lapply(transition.weights, mean, na.rm = TRUE)
  m <- unlist(m)

  fixation.threshold <- mean(m,na.rm =T) + weight.threshold*sd(m, na.rm = T)
  threshold.exceeded <- which(m>fixation.threshold) #Store all instances of weights > threshold indicating a saccade
  under.threshold <- which(m<fixation.threshold) #Store all instances of weights < threshold indicating a fixation



  fixation.candidate.starts <- min(which(m<fixation.threshold)) #Start at first sample under threshold
  quit.loop = FALSE
  fixations <- data.frame()
  while (quit.loop == FALSE){
    #If instances of cluster weight over threshold after the onset of the fixation candidate is found, take the closest as the end of the fixation.
    #If no instance is found in the recording, let the last sample be the end of the fixation
    if (sum(threshold.exceeded>fixation.candidate.starts)==0) {fixation.candidate.stops <- length(m)
    } else {fixation.candidate.stops <- min(threshold.exceeded[threshold.exceeded>fixation.candidate.starts])}

    #Calculate the center of the fixation
    fixation.candidate.x = mean(gaze_raw$x.raw[fixation.candidate.starts:fixation.candidate.stops], na.rm = TRUE)
    fixation.candidate.y = mean(gaze_raw$y.raw[fixation.candidate.starts:fixation.candidate.stops], na.rm = TRUE)



        this.fixation <- summarize_fixation_metrics(fixation.candidate.starts, fixation.candidate.stops,
                                                x = gaze_raw$x.raw, y = gaze_raw$y.raw, timestamp = gaze_raw$timestamp, one_degree = one_degree)



    fixations <- rbind(fixations,
                       this.fixation)


    #Move to the next fixation candidate (next sample with weights < threshold if there is one. Otherwise quit)
    if(sum(under.threshold>fixation.candidate.stops)>0){
      fixation.candidate.starts <- min(under.threshold[under.threshold>fixation.candidate.stops])
    } else {quit.loop = TRUE}

  }

#Step 3. Trim fixations. Shrink the period included in fixations to exclude overlap with sacccades.
  if (!is.na(threshold.on.off)){
    fixations <- trim_fixations(fixations = fixations, gaze = gaze_raw, xcol = "x.raw", ycol = "y.raw", threshold = threshold.on.off)
  }


  #Step 4: Loop through fixation candidates and merge adjacent fixations
  #Merge adjacent fixations
  if (distance.threshold >0){
    fixations <- merge_adjacent_fixations(fixations, filt.gaze, distance.threshold = distance.threshold, ms.threshold = merge.ms.threshold, one_degree = one_degree)
  }


  #Remove too short fixations
  fixations <- dplyr::filter(fixations, .data$duration >=min.fixation.duration)
  fixations <- dplyr::filter(fixations, .data$missing.samples < missing.samples.threshold)



  #Save filtered x and y coordinates for each sample
  for (i in 1: dim(fixations)[1]){
    filt.gaze$x[fixations$firstline[i]: fixations$lastline[i]] <- fixations$x[i]
    filt.gaze$y[fixations$firstline[i]: fixations$lastline[i]] <- fixations$y[i]


  }


  #Remove variables to make the output consistent with other functions
  #  fixations <- select(fixations, -firstline, -lastline)

  fixations$fixation.algorithm <- "i2mc"
  fixations$threshold <- paste0(round(weight.threshold), " SD")
  out <- list()
  out[['fixations']] <- fixations
  out[['filt.gaze']] <- filt.gaze


  return(out)

}


#' Fixation detection by two-means clustering
#' @description Identify fixations in a gaze matrix using identification by two-means clustering. The algorithm is based on Hessels et al 2017. Behavior research methods, 49, 1802-1823.
#' Data from the left and right eye are not processed separately. Adjust your analysis scripts to include this steps if you want the algorithm to include this step, as in Hessels et al 2017.
#' Input data must be a data frame with the variables timestamp, x.raw and y.raw as variables. Other variables can
#' be included but will be ignored. This function does not perform pre-processing in the form of interpolation or smoothing. Use the function process.gaze for this.
#' Timestamps are assumed to be in milliseconds. X and y coordinates can be in pixels or proportion of the screen. Make sure that the parameter one_degree is consistent with the format of your data!
#' The output data is a list with two data frames: fixations includes all detected fixations with coordinates, duration
#' and a number of other metrics, filt.gaze is a sample-by-sample data frame with time stamps, gaze coordinates before fixation detection ("raw") for and fixation coordinates.
#' If the input downsampling.factors is not empty, transition weights will be calculated based on the data in the original sampling rate and data at all sampling rate specified in this variable.
#' According to Hessels et al 2017, this step makes the analysis less vulnerable to noise in the data.
#'
#' @param gaze_raw Data frame with gaze data before fixation detection. Include the variable timestamp with timing in ms and columns with
#' x and y data as specified by the parameters xcol and ycol or their default values
#' @param one_degree One degree of the visual field in the unit of the raw x and y coordinates, typically pixels or proportion of the screen. Make
#' sure that the setting matches the format of your data
#' @param distance.threshold Subsequent fixations occurring withing this distance are merged. Set to 0 if you do not want to merge fixations.
#' @param merge.ms.threshold Only fixations occurring within this time window in milliseconds are merged
#' @param window.step.size Distance between starting points of subsequent analysis windows in samples
#' @param windowlength.ms Length of the moving analysis windows
#' @param windowlength.ms Length of the moving analysis windows
#' @param weight.threshold Samples with a transition weight exceeding it are candidates for fixation detection.
#' @param min.fixation.duration Minimum duration of accepted fixations. Shorter fixations are discarded
#' @param downsampling.factors Factors to downsample the data by in calculating fixation weights. If downsampling.factors has the values \code{c(10, 2)}, transition weights will be calculated base on data in
#' the original sampling rate as well as the two downsampled data sets.
#' @param xcol Name of the column where raw x values are stored. Default: x.raw
#' @param ycol Name of the column where raw y values are stored. Default: y.raw
#' @param missing.samples.threshold Remove fixations with a higher proportion of missing samples. Range 0 to 1.
#' @return list including separate data frames for fixations and sample-by-sample data including gaze coordinates before fixation classification ("raw")
#' and fixation coordinates.
#'The "fixations" data frame gives onset, offset, x, y, sample-to-sample root-mean-square deviations (RMSD, precision), RMSD from fixation centroid,  and missing samples of each fixation.
#' @examples
#' gaze <- cluster2m(sample.data.processed)


cluster2m <- function (gaze_raw, windowlength.ms = 200, distance.threshold = 0.7, one_degree = 40, window.step.size = 6,
                            min.fixation.duration = 40, weight.threshold = 2, xcol = "x.raw", ycol = "y.raw", merge.ms.threshold = 40,
                            downsampling.factors =NA, missing.samples.threshold = 0.5) {

  output <- algorithm_i2mc(gaze_raw = gaze_raw, windowlength.ms = windowlength.ms, distance.threshold = distance.threshold, one_degree = one_degree,
                           window.step.size = window.step.size, min.fixation.duration = min.fixation.duration, weight.threshold = weight.threshold,
                           xcol = xcol, ycol = ycol, merge.ms.threshold = merge.ms.threshold, downsampling.factors = downsampling.factors,
                           missing.samples.threshold = missing.samples.threshold)

  warning('cluster2m will be removed in future versions and replaced by algorithm_i2mc')
  return(output)
}






#' Adaptive velocity-based algorithm for saccade and fixation detection
#' @description
#' The function is based on a procedure suggested by Nyström and Holmqvist 2010. Behavior Research Methods, 42, 188-204.
#' Velocity thresholds for saccade onset and offset are adapted to the specific data at hand.
#' Please see Nyström and Holmqvist (2010) for a description of this procedure.
#' STEP 1: The function starts by identifying peak velocities larger than an initial threshold (peak threshold, specified by the parameter peak.threshold.start),
#' and then iteratively adjusts this threshold through the following steps: A) The mean (M) and standard deviation (SD) of the velocities of all samples
#' with velocties below the peak threshold are calculated. B) the updated peak threshold is defined as M +6SD. C) Steps A-B are repeated until
#' the difference between the old and new peak threshold is < 1 degree. D) The threshold for identification of saccade onsets (saccade onset threshold) is defined as M + onset.threshold *SD.
#' For each segment in the data with velocities above peak threshold, go through steps 2-3:
#'
#' STEP 2: Saccade onset is defined by searching backwards from the leftmost sample with velocity above peak threshold to the first sample with a velocity above saccade onset threshold and a higher velocity than the previous sample.
#' STEP 3: Define saccade offset threshold as a weighted sum of a) the saccade onset threshold and b) the 'local noise factor' defined as mean + 3SD of sample-to-sample velocity during a period just before saccade onset. This period has the same length as
#' the minimum fixation duration (specified by the parameter min.fixation.duration)
#' Saccade onset is defined by searching forward from the rightmost sample with a velocity above peak threshold to the first sample with a velocity below saccade offset threshold and a lower velocity than the previous sample.
#' STEP 4: Fixations are defined as periods between saccades (as in the \code{I-VT} algorithm)
#'
#' If the function can not detect a sample which fulfills both the threshold criterion and the acceleration/deceleration criterion during step 2-3, it will try to find a sample that fulfills the threshold criterion. If this fails, the saccade is discarded.
#'
#' The input data should be pre-processed (e.g., noise removal and interpolation over gaps)
#'
#' The output data is a list with three data frames: fixations includes all detected fixations with coordinates, duration
#' and a number of other metrics, saccades includes data for saccades, filt.gaze is a sample-by-sample data frame with time stamps, and gaze coordinates before ("raw") and after fixation detection.
#' The function has a number of parameters for removing potentially invalid fixations and saccades. The parameter min.fixation.duration can be used to remove unlikely
#' short fixations. If the parameter missing.samples threshold is set to a value lower than 1, fixations with a higher proportion of missing raw samples are removed.
#'
#'
#' @param gaze Data frame with gaze data before saccade and fixation data identification. The data frame must include the variable timestamp with
#' timing in milliseconds and columns for x and y coordinates specified by the columns 'xcol' and 'ycol' respectively.
#' @param xcol column in the gaze data frame where x coordinates are found. Default: x.raw
#' @param ycol column in the gaze data frame where y coordinates are found. Default: y.raw
#' @param one_degree one degree of the visual field in the unit of the x and y coordinates in the data. Typically pixels or degrees.
#' @param velocity.filter.ms If velocity.filter.ms is not NA, the velocity vector is smoothed using a moving median filter corresponding to this value in ms
#' before the propose threshold is identified. Default: 10.
#' @param min.fixation.duration Minimum duration of accepted fixations. This parameter is also used to calculate 'local noise' prior to the onset of a saccade. Default: 40
#' @param min.saccade.duration Minimum duration of accepted saccades in ms. Default: 10
#' @param save.velocity.profiles If TRUE, save velocity profiles of each saccade. Default: FALSE.
#' @param missing.samples.threshold Remove fixations with a higher proportion of missing samples. Range 0 to 1.
#' @param distance.threshold Subsequent fixations occurring withing this distance are merged. Set to 0 if you don't want to merge fixations.
#' @param merge.ms.threshold Subsequent fixations occuring within this time window and distance specified by distance.threshold are merged. Set to 0 if you don't want to merge fixations.
#' @param peak.threshold.start Initial peak threshold value in degrees of the visual field. Default: 200
#' @param onset.threshold.sd Number of standard deviations used to define saccade onset threshold
#' @param alpha Weight of the saccade onset threshold when defining threshold for saccade offset
#' @param beta Weight of local noise factor when defining threshold for saccade offset
#' @param min.period.ms Peak velocity threshold will be iteratively updated based on periods in the data in which consecutive values of at least this time period are below the previous threshold estimate.
#' @param trim.fixations If TRUE, the onset of each fixation will be shifted forwards to the first non-missing (non-NA) sample during the period. The offset will be shifted backwards to the last non-missing
#' sample. If TRUE, and the parameter trim.dispersion.threshold is a positive number, samples at the margins with large distances from the centroid will also be excluded
#' @param trim.dispersion.threshold If not NA and trim.fixations is TRUE, fixation onsets and offests will also be shrinked to exclude any samples at the margins with a larger
#' distance from the fixation centroid than trim.dispersion.threshold * MAD.
#' @param margin.ms A margin in ms around periods of identified consecutive values below the previous threshold estimate which will be excluded from the estimates. This makes
#' the algorithm more robust to noise.
#' @return list including separate data frames for fixations, saccades, and sample-by-sample data



algorithm_adaptive <- function(gaze, min.fixation.duration = 40, min.saccade.duration = 10, xcol = "x.raw", ycol = "y.raw",
                               save.velocity.profiles = FALSE, missing.samples.threshold = 0.5, merge.ms.threshold = 75,
                               distance.threshold = 0.7, alpha = 0.7, beta = 0.3, one_degree = 40, velocity.filter.ms = 10,
                               peak.threshold.start = 200, onset.threshold.sd = 3, min.period.ms = 40, margin.ms = 3,
                               trim.fixations = TRUE, trim.dispersion.threshold =NA) {


  t <- suggest_threshold(gaze, velocity.filter.ms =velocity.filter.ms, one_degree =one_degree, xcol = xcol, ycol = ycol,
                         peak.threshold.start = peak.threshold.start, onset.threshold.sd = onset.threshold.sd,
                         min.period.ms = min.period.ms, margin.ms = margin.ms)



  onset.threshold <- t[["onset.threshold"]]
  peak.threshold <- t[["peak.threshold"]]
  velocity.vect <- t[["velocity"]]

  one.sample <- median(diff(gaze$timestamp),na.rm =T)


  #Create variables
  saccades <- data.frame() # Store saccades here
  velocity.profile <- list() #Leave empty if the parameter is not specified as TRUE
  fixations <- data.frame() #Store fixations here

  #Create a data frame to store filtered x and y coordinates. Retain raw x and y for comparison
  filt.gaze <- data.frame(timestamp = gaze$timestamp,
                          x.raw = gaze[[xcol]], y.raw = gaze[[ycol]],x = rep(NA,nrow(gaze)), y = rep(NA,nrow(gaze))
  )


  #Step 2.
  #Find transitions between periods with velocities above peak threshold. Transitions are stored in the variable
  #d where 1 is a transition to a period with velocities above threshold and -1 to a period with velocitites below.
  velocity.vect$above.pt <- velocity.vect$velocity>peak.threshold
  velocity.vect$d <- c(NA, diff(velocity.vect$above.pt))
  velocity.vect$acceleration <- c(NA, diff(velocity.vect$velocity))
  velocity.vect$deceleration <- c(NA, diff(velocity.vect$velocity)<0)

  peak.onsets <- which(velocity.vect$d ==1)

  message("Searching for saccades")

  nonvalid.saccade.onset <- 1 #Invalid line of onset for the next saccade. Start with 1. Update this variable with each new saccade so that it matches the last one.
  #Otherwise, periods of NAs in the data can cause the same saccade to be indentified multiple times

  for (i in 1: length(peak.onsets)) {

    #FIND SACCADE ONSET

    # For this proposed saccade, find the corresponding onset
    w <- which(velocity.vect$velocity[1:peak.onsets[i]] < onset.threshold &
                 velocity.vect$deceleration[1:peak.onsets[i]] == FALSE)

    if (length(w) >0) {
      saccade.starts <- max(w)

    } else {saccade.starts <- peak.onsets[i]} #Define saccade onset as the first sample > peak.threshold if the procedure fails

    #CALCULATE LOCAL NOISE FACTOR (BEFORE THE ONSET OF THIS SACCADE)
    #Calculate the local noise factor (in a period corresponding to the minimum fixation durationbefore the current saccade)

    #Define the period before the onset of the saccade
    period.length <- round(min.fixation.duration/one.sample)
    p2 <- saccade.starts-1
    p1 <- p2 - period.length

    #If the period covers 1, start with sample 1
    if (p1 <1) {p1 <- 1}
    velocity.vect$velocity[p1:p2]

    #Calculate local noise factor
    m <- mean(velocity.vect$velocity[p1:p2], na.rm =T)
    s <- sd(velocity.vect$velocity[p1:p2], na.rm =T)
    local.noise.factor = m + 3*s



    # FIND SACCADE OFFSET

    #Find the last sample with velocity above peak threshold in this segment
    w <- which(velocity.vect$d[peak.onsets[i]:nrow(velocity.vect)] == -1)

    if (length(w) >0) {
      peak.period.offset <- min(w)+peak.onsets[i] # An offset of a data segment with velocities > peak.threshold is found
      if (i < length(peak.onsets)) { #Check if the offset comes after a new onset (in this case, the actual offset is missing)
        if (peak.period.offset > peak.onsets[i+1]) {
          peak.period.offset <- NA
        }
      }
    } else {peak.period.offset <- NA}


    #Define the threshold for saccade offset as a weighted sum of local noise factor and global noise factor (onset threshold)
    offset.threshold <- (alpha*onset.threshold) + (beta*local.noise.factor)

    if (!is.na(peak.period.offset)) {

      w <- which(velocity.vect$velocity[peak.period.offset:nrow(velocity.vect)] < offset.threshold &
                   velocity.vect$deceleration[peak.period.offset:nrow(velocity.vect)] == TRUE)

      if(length(w) >0) { #Found a saccade offset defined by velocity < threshold and deceleration

        saccade.ends <- min(w, na.rm =TRUE) + peak.period.offset}

      else { #Unable to define saccade offset as a combination of velocity < threshold and deceleration. Try without requiring a deceleration.
        w <- which(velocity.vect$velocity[saccade.starts:nrow(velocity.vect)] < offset.threshold)
        if(length(w) >0) {saccade.ends <- min(w,na.rm =TRUE) + saccade.starts}


      }


      x.onset <- gaze[saccade.starts, xcol]
      y.onset <- gaze[saccade.starts, ycol]
      x.offset <- gaze[saccade.ends, xcol]
      y.offset <- gaze[saccade.ends, ycol]
      duration <- gaze$timestamp[saccade.ends] - gaze$timestamp[saccade.starts]
      onset <- gaze$timestamp[saccade.starts]

      #Calculate the Euclidean distance between onset and offset (amplitude)
      amplitude <- sqrt((x.onset - x.offset)^2 + (y.onset-y.offset)^2)
      #Normalize to degrees
      amplitude <- amplitude/one_degree


      if (nrow(saccades)>0) {nonvalid.saccade.onset <- saccades$onset[nrow(saccades)]} #Discard this saccade if it has already been stored

      if (duration > min.saccade.duration & onset != nonvalid.saccade.onset) {
        #Summarize saccades
        saccades <- rbind(saccades,
                          data.frame(
                            onset = onset,
                            x.onset = x.onset,
                            y.onset = y.onset,
                            offset = gaze$timestamp[saccade.ends],
                            x.offset = x.offset,
                            y.offset = y.offset,
                            duration = duration,
                            amplitude = amplitude,
                            peak.velocity = max(velocity.vect$velocity[saccade.starts:saccade.ends], na.rm = T),
                            missing.samples = mean(is.na(gaze[saccade.starts:saccade.ends, xcol])),
                            firstline = saccade.starts,
                            lastline = saccade.ends
                          ))



        #Save velocity profiles if this is specified in the input
        if (save.velocity.profiles == TRUE) {
          saccade.nr <- nrow(saccades)
          velocity.profile[[saccade.nr]] <- velocity.vect$velocity[saccade.starts:saccade.ends]

        }


      }



    }
  }

  #IDENTIFY FIXATIONS
  message("Searching for fixations")

  if (nrow(saccades) == 0) {warning("No saccades detected. Check the format of your data! The function will end")}


  for (i in 1: (nrow(saccades)-1)) {

    fixation.candidate.starts <- saccades$lastline[i]+1
    fixation.candidate.stops <- saccades$firstline[i+1]-1

    this.fixation <- summarize_fixation_metrics(fixation.candidate.starts, fixation.candidate.stops,
                                                             x = gaze[[xcol]], y = gaze[[ycol]], timestamp = gaze$timestamp, one_degree = one_degree)

    fixations <- rbind(fixations, this.fixation)


  }


  #Step 3: Loop through fixation candidates and merge adjacent fixations
  #Merge adjacent fixations
  if (trim.fixations == TRUE){

    fixations <- trim_fixations(fixations, filt.gaze, xcol = xcol, ycol = ycol,threshold = trim.dispersion.threshold)
  }



  if (distance.threshold >0){
    fixations <- merge_adjacent_fixations(fixations, filt.gaze, distance.threshold = distance.threshold, ms.threshold = merge.ms.threshold, one_degree = one_degree)
  }

  fixations <- dplyr::filter(fixations, .data$duration >= min.fixation.duration)
  fixations <- dplyr::filter(fixations, .data$missing.samples < missing.samples.threshold)

  #Save filtered x and y coordinates for each sample
  for (i in 1: dim(fixations)[1]){
    filt.gaze$x[fixations$firstline[i]: fixations$lastline[i]] <- fixations$x[i]
    filt.gaze$y[fixations$firstline[i]: fixations$lastline[i]] <- fixations$y[i]


  }

  fixations$fixation.algorithm <- "adaptive"
  fixations$threshold <- paste0(round(peak.threshold), " deg. (peak); ", round(onset.threshold), " deg. (onset)")


  if (save.velocity.profiles == TRUE) {saccades$velocity.profile <- velocity.profile}

  out <- list()
  out[["saccades"]] <- saccades
  out[["fixations"]] <-fixations
  out[["filt.gaze"]] <- filt.gaze


  return(out)
}


#' Summarize fixation statistics
#' @description
#' Summarize descriptives for a fixation defined by onset and offset rows in the data. Used internally by event classification functions.
#' @param x X coordinates
#' @param y Y coordinates
#' @param one_degree one degree of the visual field in the unit of the x and y coordinates in the data. Typically pixels or degrees.
#' @param timestamp Timestamps in milliseconds
#' @param fixation.candidate.starts First row in the data included in the fixation
#' @param fixation.candidate.stops Last row in the data included in the fixation
#' @return data frame with fixation descriptives

summarize_fixation_metrics <- function (fixation.candidate.starts, fixation.candidate.stops, x,y, timestamp, one_degree = 40) {
  #Calculate the center of the detected fixation
  fixation.candidate.x <- mean(x[fixation.candidate.starts: fixation.candidate.stops],na.rm = TRUE)
  fixation.candidate.y <- mean(y[fixation.candidate.starts: fixation.candidate.stops],na.rm = TRUE)


  #Calculate the RMS of the detected fixation from fixation center
  xdiff.center <- (x[fixation.candidate.starts:fixation.candidate.stops] - fixation.candidate.x)
  ydiff.center <- (y[fixation.candidate.starts:fixation.candidate.stops] - fixation.candidate.y)
  dist.from.center <- sqrt(ydiff.center^2+xdiff.center^2)

  rms.from.center <- sqrt(mean(dist.from.center^2))/one_degree


  #Calculate the precision RMS
  xdiff <- diff(x[fixation.candidate.starts:fixation.candidate.stops])
  ydiff <- diff(y[fixation.candidate.starts:fixation.candidate.stops])
  s2s.distance <- sqrt(ydiff^2 + xdiff^2)
  rms_deviation <- sqrt(mean(s2s.distance^2))
  rms <- round(mean(rms_deviation,na.rm =TRUE)/one_degree,3)



  #av.diff <- rowMeans(dplyr::select(dist.data, xdiff, ydiff))



  fixation <-  data.frame(
    x = fixation.candidate.x,
    y = fixation.candidate.y,
    duration = timestamp[fixation.candidate.stops] - timestamp[fixation.candidate.starts],
    onset = timestamp[fixation.candidate.starts],
    offset=  timestamp[fixation.candidate.stops],
    missing.samples = mean(is.na(x[fixation.candidate.starts:fixation.candidate.stops])),
    rmsd = rms,
    rms.from.center = rms.from.center,
    firstline = fixation.candidate.starts,
    lastline = fixation.candidate.stops
  )

  return(fixation)
}





#' Adjust the onset and offset of fixations to avoid misclassification of saccade samples as belonging to fixations
#' @description
#' Shrink the period classified as a fixation by removing samples close to the onset and offset with excessive differences from the fixation center.
#' This reduces the risk that samples belonging to saccades are misclassified as belonging to a fixation. The function is used internally by other functions
#' and should typically not be called outside of them.
#'
#' adjust_fixation_timing starts by calculating the median (MD) and MAD of the absolute distances from the fixation center of all included samples. The fixation onset
#' is shifted forwards to the first sample with a distance to the fixation center under t* MAD + MD where t is specified by the input parameter threshold.
#' Analogously, fixation offset is shifted backwards to the last included sample with distance to the fixation center under t* MAD + MD
#'
#' @param x X coordinates
#' @param y Y coordinates
#' @param fixation.candidate.starts First row in the data included in the fixation
#' @param fixation.candidate.stops Last row in the data included in the fixation
#' @param threshold Threshold for highest accepted distance from fixation center in MADs from the median. Default 3. If NA, just remove NAs
#' at the onset and offest of fixation but ignore deviations from fixation center
#' @return data frame with adjusted first and last row of the fixation

adjust_fixation_timing <- function (fixation.candidate.starts, fixation.candidate.stops, x,y, threshold =3) {

  fixation.candidate.x <- mean(x[fixation.candidate.starts: fixation.candidate.stops],na.rm = TRUE)
  fixation.candidate.y <- mean(y[fixation.candidate.starts: fixation.candidate.stops],na.rm = TRUE)


  #Calculate the RMS of the detected fixation
  xdiff <- (x[fixation.candidate.starts:fixation.candidate.stops] - fixation.candidate.x)^2
  ydiff <- (y[fixation.candidate.starts:fixation.candidate.stops] - fixation.candidate.y)^2


  #Calculate dthe distance from center of the proposed fixation. Absoulute values
  dist.from.center <- rowMeans(data.frame(xdiff = sqrt(xdiff),
                                          ydiff = sqrt(ydiff)))


  #Calculate median and MAD of absolute distance from center of fixation for each sample
  md<- median(dist.from.center,na.rm =T)
  dist.mad <- mad(dist.from.center, na.rm =T)

  #First and last sample of fixation must have a lower distance from center of fixation than this value

  if (!is.na(threshold)) {
    lim.value <- md + threshold*dist.mad
    exit <- FALSE
    #Start at the first sample and calculate a new adjusted starting

    i <- 1
    while (!exit) {

      if (is.na(dist.from.center[i])) {i <- i +1
      } else if (!is.na(dist.from.center[i]) & dist.from.center[i] > lim.value) {i <- i +1
      } else if (i >= length(dist.from.center)){exit <- TRUE
      } else if (!is.na(dist.from.center[i]) & dist.from.center[i] <= lim.value) {exit <- TRUE}

    }
    new.startpoint <- i



    i <- length(dist.from.center) #Start at last sample and walk backwards
    exit <- FALSE
    while (!exit) {
      if (is.na(dist.from.center[i])) {i <- i-1
      } else if (!is.na(dist.from.center[i]) & dist.from.center[i] > lim.value) {i <- i-1
      } else if (i <= new.startpoint){exit <- TRUE
      } else if (!is.na(dist.from.center[i]) & dist.from.center[i] <= lim.value) {exit <- TRUE}

    }
    new.stop <- i

  } else if (is.na(threshold)){ #No threshold value specified. Just shift onset and offset to include first/last non-NA
    if (sum(!is.na(xdiff)) >0) {new.startpoint <- min(which(!is.na(xdiff)))
    new.stop <- max(which(!is.na(xdiff)))
    }


  }


  new.index <- data.frame(firstline = fixation.candidate.starts + new.startpoint,
                          lastline = fixation.candidate.starts + new.stop)

  return(new.index)
}




#' Adjust the onset and offset of fixations to avoid misclassification of saccade samples as belonging to fixations
#' @description
#'
#' Adjust the onset and offset of all fixations in a data frame (The function adjust_fixation_timing does this for a single fixation).
#'
#' Shrink the period classified as a fixation by removing samples at the onset and offset with excessive differences from the fixation center or which are missing (X or Y are NA).
#' This reduces the risk that samples belonging to saccades are misclassified as belonging to a fixation. Please note that this procedure is included by default in the
#' event classification algorithm 'alogorithm_i2mc' (see documentation for this function for details)
#'
#' The procedure starts by calculating the median (MD) and MAD of the absolute distances from the fixation center of all included samples. The fixation onset
#' is shifted forwards to the first sample with a distance to the fixation center under t* MAD + MD where t is specified by the input parameter threshold.
#' Analogously, fixation offset is shifted backwards to the last included sample with distance to the fixation center under t* MAD + MD
#'
#' trim_fixations will look for variables called 'fixation.algorithm' and 'threshold' in the data frame 'fixations'. These columns are produced by kollaR event classification
#' algorithms. If they are found, they will be transfered to the output data frame.
#'
#'
#' @param fixations Data frame with fixations to trim. The data frame must include the variables 'firstline' (index of first row in the sample-by-sample data belonging to
#' each fixation), 'lastline' (index of last row in the sample-by-sample data belonging to each fixation). The function works with the fixation output from kollaR event classification algorithms
#' the fixation)
#' @param gaze Data frame with sample-to-sample data. Must include timestamps in milliseconds specified by the variable timestamp, and X and Y coordinates specified by the
#' parameters 'xcol' and 'ycol'.
#' @param xcol Variable in the sample-to-sample data frame where X coordinates (before event classification) are found
#' @param ycol Variable in the sample-to-sample data frame where X coordinates (before event classification) are found
#' @param threshold Threshold for highest accepted distance from fixation center in MADs from the median. Default 3. If NA, just remove NAs
#' at the onset and offest of fixation but ignore deviations from fixation center
#' @param one_degree One degree of the visual field in the units of the X and Y coordinates (which is typically pixels or degrees of the visual field)
#' @return data frame with fixations after adjustment of onset and offset



trim_fixations <- function(fixations, gaze, xcol ="x.raw", ycol = "y.raw", threshold = 3, one_degree = 40){

  trimmed.fixations <- data.frame()
  if (length(gaze[[xcol]]) == 0 | length(gaze[[ycol]]) == 0) {
    message("Warning! No X and/or Y coordinates found in the sample level data. Did you misspecify the variable names
            in the parameters xcol and/or ycol?")
  }

  trimmed.fixations <- data.frame()
  for (i in 1: nrow(fixations)) {

    #Find trimmed on- and offsets
    new.on.off <- adjust_fixation_timing(fixations$firstline[i], fixations$lastline[i], x = gaze[[xcol]], y = gaze[[ycol]],
                                          threshold = threshold)

    #Add this fixation to the data frame
    trimmed.fixations <- rbind(trimmed.fixations,
                               summarize_fixation_metrics(fixation.candidate.starts = new.on.off$firstline, fixation.candidate.stops = new.on.off$lastline, x = gaze[[xcol]],
                                                          y = gaze[[ycol]], timestamp = gaze$timestamp, one_degree = one_degree)

    )

  }

  if ("fixation.algorithm" %in% names(fixations)) {
    trimmed.fixations$fixation.algorithm <- fixations$fixation.algorithm
  } else {trimmed.fixations$fixation.algorithm <- "unknown"}

  if ("threshold" %in% names(fixations)) {
    trimmed.fixations$threshold <- fixations$threshold
  } else {trimmed.fixations$threshold <- "unknown"}

  return(trimmed.fixations)

}
