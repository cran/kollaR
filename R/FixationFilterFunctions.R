
#' Merge adjacent fixations
#' @description Merge fixations which appear close in space and time. This function is called by other functions and typically
#' not used outside them
#' @param fixations Data frame with fixations
#' @param gaze_raw Data matrix with raw data. See description of the ivt_filter function
#' @param one_degree One degree of the visual field in the scale of the x and y coordinates. Typically pixels
#' @param ms.threshold Maximum time elapsed between fixations to be merged.
#' @param distance.threshold Subsequent fixations occurring withing this distance are merged. Set to 0 if you don't want to merge fixations.
#' @return A new data frame with fixations

merge_adjacent_fixations <- function (fixations, gaze_raw, distance.threshold = 0.5, ms.threshold = 75, one_degree = 40) {
  #Loop through fixation candidates and merge adjacent fixations
  message("Merging adjacent fixations")
  i <- 1
  while (i < dim(fixations)[1]){
    d <- (sqrt((fixations$x[i] - fixations$x[i+1])^2 + (fixations$y[i] - fixations$y[i+1])^2))/one_degree #distance in degrees
    t <- fixations$onset[i+1] - fixations$offset[i] #Time elapsed between fixations in ms
    if(d < distance.threshold & t < ms.threshold) { #FIXATIONS ARE CLOSE. MERGE THIS AND THE NEXT FIXATIONS

      fixation.candidate.starts <- fixations$firstline[i]
      fixation.candidate.stops <- fixations$lastline[i+1]

      #Calculate the center of the fixation
      fixation.candidate.x = mean(gaze_raw$x.raw[fixation.candidate.starts:fixation.candidate.stops], na.rm = TRUE)
      fixation.candidate.y = mean(gaze_raw$y.raw[fixation.candidate.starts:fixation.candidate.stops], na.rm = TRUE)


      #Calculate the RMS of the detected fixation
      xdiff <- (gaze_raw$x.raw[fixation.candidate.starts:fixation.candidate.stops] - fixation.candidate.x)^2
      ydiff <- (gaze_raw$y.raw[fixation.candidate.starts:fixation.candidate.stops] - fixation.candidate.y)^2

      ydist <- sqrt(mean(ydiff, na.rm = TRUE))
      xdist <- sqrt(mean(xdiff, na.rm = TRUE))

      rms <- mean(c(xdist,ydist))/one_degree

      #Update the merged fixation
      fixations$x[i] <- fixation.candidate.x
      fixations$y[i] <- fixation.candidate.y
      fixations$duration[i] <- gaze_raw$timestamp[fixation.candidate.stops] - gaze_raw$timestamp[fixation.candidate.starts]
      fixations$offset[i] <-  gaze_raw$timestamp[fixation.candidate.stops]
      fixations$firstline[i] <- fixation.candidate.starts
      fixations$lastline[i] = fixation.candidate.stops
      fixations$missing.samples[i] = mean(is.na(gaze_raw$x.raw[fixation.candidate.starts:fixation.candidate.stops]))
      fixations$rmsd[i] <- rms



      fixations <- fixations[-(i+1),] #Remove the next fixation after merging its data with this one

      # i <- i+1

    } else {i <- i+1}
  }
  return(fixations)
}








#' Dispersion-based fixation detection algorithm \code{(I-DT)}
#' @description Apply a dispersion-based fixation \code{(I-DT)} filter to the eye tracking data.
#' The algorithm identifies fixations as samples clustering within a spatial area.
#' The procedure is described in Blignaut 2009
#' Input data must be a data frame with the variables timestamp, x.raw and y.raw as variables. Other variables can
#' be included but will be ignored. This function does not perform pre-processing in the form of interpolation or smoothing. Use the function process.gaze for this.
#' Timestamps are assumed to be in milliseconds. Default settings assume that x and y coordinates are in pixels.
#' The output data is a list with two data frames: fixations includes all detected fixations with coordinates, duration
#' and a number of other metrics, filt.gaze is a sample-by-sample data frame with time stamps, raw and filtered gaze coordinates.
#' The function can be slow for long recordings and/or data recorded at high sampling rates.
#' @param gaze_raw Data frame with unfiltered gaze data. Include the variable timestamp with timing in ms and columns with raw
#' x and y data as specified by the paramerers xcol and ycol or their default values
#' @param one_degree One degree of the visual field in the unit of the raw x and y coordinates, typically pixels
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
#' @return list including separate data frames for fixations and sample-by-sample data including filtered and unfiltered data.
#' The fixations data frame includes onset, offset, x, y, RMSD and missing samples of each fixation.


idt_filter <- function(gaze_raw, one_degree = 40, dispersion.threshold =1, min.duration = 50, xcol = "x.raw", ycol = "y.raw",
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

        #Calculate the RMS of the detected fixation
        xdiff <- (gaze_raw$x.raw[fixation.candidate.start:sample_index-1] - fixation.candidate.x)^2
        ydiff <- (gaze_raw$y.raw[fixation.candidate.start:sample_index-1] - fixation.candidate.y)^2

        ydist <- sqrt(mean(ydiff,na.rm = TRUE))
        xdist <- sqrt(mean(xdiff,na.rm = TRUE))

        rms <- mean(c(xdist,ydist))
        rms <- rms/one_degree

        fixations <- rbind(fixations,
                           data.frame(
                             x = fixation.candidate.x,
                             y = fixation.candidate.y,
                             duration = gaze_raw$timestamp[sample_index-1] - gaze_raw$timestamp[fixation.candidate.start],
                             onset = gaze_raw$timestamp[fixation.candidate.start],
                             offset=  gaze_raw$timestamp[sample_index-1],
                             missing.samples = mean(is.na(gaze_raw$x.raw[fixation.candidate.start:sample_index-1])),
                             rmsd = rms,
                             firstline = fixation.candidate.start,
                             lastline = sample_index-1

                           ))


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


  fixations$fixation.filter <- "dispersion"
  fixations$threshold <- paste0(round(dispersion.threshold), " deg.")
  out <- list()
  out[['fixations']] <- fixations
  out[['filt.gaze']] <- filt.gaze
  return(out)
}

#'I-VT algorithm for fixation and saccade detection
#'@description Apply an \code{I-VT} filter to the eye tracking data.
#' The algorithm identifies saccades as periods with sample-to-sample velocity above a threshold and fixations as periods between saccades.
#' See Salvucci and Goldberg 2000. Identifying fixations and saccades in eye tracking protocols. Proc. 2000 symposium on Eye tracking
#' research and applications for a description.
#'
#' Input data must be a data frame with the variables timestamp, x.raw and y.raw as variables. Other variables can
#' be included but will be ignored. This function does not perform pre-processing in the form of interpolation or smoothing. Use the function process.gaze for this.
#' Timestamps are assumed to be in milliseconds. Default settings assume that x and y coordinates are in pixels.
#' The output data is a list with three data frames: fixations includes all detected fixations with coordinates, duration
#' and a number of other metrics, saccades includes data for saccades, filt.gaze is a sample-by-sample data frame with time stamps, raw and filtered gaze coordinates for fixations.
#' The function has a number of parameters for removing potentially invalid fixations and saccades. The parameter min.fixation.duration can be used to remove unlikely
#' short fixations. If the parameter missing.samples threshold is set to a value lower than 1, fixations with a higher proportion of missing raw samples are removed.
#'
#'
#'
#' @param gaze_raw Data frame with unfiltered gaze data. Include the variable timestamp with timing in ms and columns with raw
#' x and y data as specified by the parameters xcol and ycol or their default values
#' @param one_degree One degree of the visual field in the unit of the raw x and y coordinates, typically pixels
#' @param velocity.threshold Velocity threshold for saccade detection in degrees/second
#' @param velocity.filter.ms Window in milliseconds for moving average window used for smoothing the sample to sample velocity vector.
#' @param min.saccade.duration Minimum duration of saccades in milliseconds
#' @param missing.samples.threshold Remove fixations with a higher proportion of missing samples. Range 0 to 1.
#' @param min.fixation.duration Minimum duration of fixations in milliseconds
#' @param xcol Name of the column where raw x values are stored. Default: x.raw
#' @param ycol Name of the column where raw y values are stored. Default: y.raw
#' @param distance.threshold Subsequent fixations occurring withing this distance are merged. Set to 0 if you don't want to merge fixations.
#' @param merge.ms.threshold Subsequent fixations occuring within this time window and distance specified by distance.threshold are merged. Set to 0 if you don't want to merge fixations.
#' @param save.velocity.profiles If TRUE, return velocity profiles of each detected saccade as a variable in the saccades data frame
#' @return list including separate data frames for fixations and sample-by-sample data including filtered and unfiltered data.
#' The fixations data frame gives onset, offset, x, y, RMSD and missing samples of each fixation.
#' @examples
#' ivt_data <- ivt_filter(sample.data.processed, velocity.threshold = 30, min.fixation.duration = 40)

ivt_filter <- function(gaze_raw, velocity.filter.ms =20, velocity.threshold = 35, min.saccade.duration = 10, min.fixation.duration = 40, one_degree =40, save.velocity.profiles = FALSE,
                       xcol = "x.raw", ycol = "y.raw", distance.threshold = 0.7, merge.ms.threshold = 75, missing.samples.threshold = 0.5){


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
                        peak.velocity = max(gaze_raw$velocity[saccade.starts[i]:saccade.ends[i]], na.rm = T)

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

    #Calculate the center of the detected fixation
    fixation.candidate.x <- mean(gaze_raw$x.raw[fixation.candidate.starts: fixation.candidate.stops],na.rm = TRUE)
    fixation.candidate.y <- mean(gaze_raw$y.raw[fixation.candidate.starts: fixation.candidate.stops],na.rm = TRUE)


    #Calculate the RMS of the detected fixation
    xdiff <- (gaze_raw$x.raw[fixation.candidate.starts:fixation.candidate.stops] - fixation.candidate.x)^2
    ydiff <- (gaze_raw$y.raw[fixation.candidate.starts:fixation.candidate.stops] - fixation.candidate.y)^2

    ydist <- sqrt(mean(ydiff, na.rm = TRUE))
    xdist <- sqrt(mean(xdiff, na.rm = TRUE))

    rms <- mean(c(xdist,ydist))/one_degree

    fixations <- rbind(fixations,
                       data.frame(
                         x = fixation.candidate.x,
                         y = fixation.candidate.y,
                         duration = gaze_raw$timestamp[fixation.candidate.stops] - gaze_raw$timestamp[fixation.candidate.starts],
                         onset = gaze_raw$timestamp[fixation.candidate.starts],
                         offset=  gaze_raw$timestamp[fixation.candidate.stops],
                         missing.samples = mean(is.na(gaze_raw$x.raw[fixation.candidate.starts:fixation.candidate.stops])),
                         rmsd = rms,
                         firstline = fixation.candidate.starts,
                         lastline = fixation.candidate.stops
                       ))


  }





  #Step 3: Loop through fixation candidates and merge adjacent fixations
  #Merge adjacent fixations
  if (distance.threshold >0){
    fixations <- merge_adjacent_fixations(fixations, filt.gaze, distance.threshold = distance.threshold, ms.threshold = merge.ms.threshold, one_degree = one_degree)
  }


  #Remove too short fixations
  fixations <- dplyr::filter(fixations, .data$duration >=min.fixation.duration)


  #Save filtered x and y coordinates for each sample
  for (i in 1: dim(fixations)[1]){
    filt.gaze$x[fixations$firstline[i]: fixations$lastline[i]] <- fixations$x[i]
    filt.gaze$y[fixations$firstline[i]: fixations$lastline[i]] <- fixations$y[i]


  }



  fixations <- dplyr::filter(fixations, .data$duration >= min.fixation.duration)
  fixations <- dplyr::filter(fixations, .data$missing.samples < missing.samples.threshold)
  fixations$fixation.filter <- "ivt"
  fixations$threshold <- paste0(round(velocity.threshold), " deg.")

  out <- list()
  out[["saccades"]] <- saccades
  out[["fixations"]] <-fixations
  out[["filt.gaze"]] <- filt.gaze

  return(out)

}


#' Find transition weights for each sample in a gaze matrix.
#' @description This function is used internally by the function cluster2m
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
#' Timestamps are assumed to be in milliseconds. Default settings assume that x and y coordinates are in pixels.
#' The output data is a list with two data frames: fixations includes all detected fixations with coordinates, duration
#' and a number of other metrics, filt.gaze is a sample-by-sample data frame with time stamps, raw and filtered gaze coordinates for fixations.
#' If the input downsampling.factors is not empty, transition weights will be calculated based on the data in the original sampling rate and data at all sampling rate specified in this variable.
#' According to Hessels et al 2017, this step makes the analysis less vulnerable to noise in the data.
#'
#' @param gaze_raw Data frame with unfiltered gaze data. Include the variable timestamp with timing in ms and columns with raw
#' x and y data as specified by the parameters xcol and ycol or their default values
#' @param one_degree One degree of the visual field in the unit of the raw x and y coordinates, typically pixels
#' @param distance.threshold Subsequent fixations occurring withing this distance are merged. Set to 0 if you do not want to merge fixations.
#' @param merge.ms.threshold Only fixations occurring within this time window in milliseconds are merged
#' @param window.step.size Distance between starting points of subsequent analysis windows in samples
#' @param windowlength.ms Length of the moving analysis windows
#' @param windowlength.ms Length of the moving analysis windows
#' @param weight.threshold Samples with a transition weight exceeding it are candidates for fixation detection.
#' @param min.fixation.duration Minimum duration of accepted fixations. Shorter fixations are discarded
#' @param downsampling.factors Factors to downsample the data by in calculating fixation weights. If downsampling.factors has the values \code{c(10, 2)}, transition weights will be calculated base on data in
#' the original sampling rate as well as the two donwsampled data sets.
#' @param xcol Name of the column where raw x values are stored. Default: x.raw
#' @param ycol Name of the column where raw y values are stored. Default: y.raw
#' @param missing.samples.threshold Remove fixations with a higher proportion of missing samples. Range 0 to 1.
#' @return list including separate data frames for fixations and sample-by-sample data including filtered and unfiltered data.
#'The "fixations" data frame gives onset, offset, x, y, RMSD and missing samples of each fixation.
#' @examples
#' gaze <- cluster2m(sample.data.processed)

cluster2m <- function (gaze_raw, windowlength.ms = 200, distance.threshold = 0.7, one_degree = 40, window.step.size = 6,
                       min.fixation.duration = 40, weight.threshold = 2, xcol = "x.raw", ycol = "y.raw", merge.ms.threshold = 40,
                       downsampling.factors =NA, missing.samples.threshold = 0.5) {

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


    #Calculate the RMS of the detected fixation
    xdiff <- (gaze_raw$x.raw[fixation.candidate.starts:fixation.candidate.stops] - fixation.candidate.x)^2
    ydiff <- (gaze_raw$y.raw[fixation.candidate.starts:fixation.candidate.stops] - fixation.candidate.y)^2

    ydist <- sqrt(mean(ydiff, na.rm = TRUE))
    xdist <- sqrt(mean(xdiff, na.rm = TRUE))

    rms <- mean(c(xdist,ydist))/one_degree

    fixations <- rbind(fixations,
                       data.frame(
                         x = fixation.candidate.x,
                         y = fixation.candidate.y,
                         duration = gaze_raw$timestamp[fixation.candidate.stops] - gaze_raw$timestamp[fixation.candidate.starts],
                         onset = gaze_raw$timestamp[fixation.candidate.starts],
                         offset=  gaze_raw$timestamp[fixation.candidate.stops],
                         firstline = fixation.candidate.starts,
                         lastline = fixation.candidate.stops,
                         missing.samples = mean(is.na(gaze_raw$x.raw[fixation.candidate.starts:fixation.candidate.stops])),
                         rmsd = rms


                       ))

    #Move to the next fixation candidate (next sample with weights < threshold if there is one. Otherwise quit)
    if(sum(under.threshold>fixation.candidate.stops)>0){
      fixation.candidate.starts <- min(under.threshold[under.threshold>fixation.candidate.stops])
    } else {quit.loop = TRUE}

  }

  #Step 3: Loop through fixation candidates and merge adjacent fixations
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

  fixations$fixation.filter <- "i2mc"
  fixations$threshold <- paste0(round(weight.threshold), " SD")
  out <- list()
  out[['fixations']] <- fixations
  out[['filt.gaze']] <- filt.gaze


  return(out)

}

