#' Plot fixation filtered vs. raw gaze coordinates
#' @description This function plots and returns a ggplot2 figure showing two time series of gaze coordinates plotted against time.
#' The interval to plot can be defined as a proportion of the data frame or by sample numbers. Use this function to plot data before and after processing or filtering
#' to examine their effects. For example, unprocessed x or y coordinates can be plotted against x and y coordinates following pre-processing and/or a fixation filter.
#' Either the x or the y vector is plotted
#' @param data_in gaze matrix which must include columns for filtered and unfiltered data as specified in the var1 and var2 paramters
#' @param plot.window vector defining the time window to plot. If left empty, the 50-65% interval of the data are plotted. If the submitted values are
#' <0, they are assumed to be proportions, e.g., \code{plot.window = c(0.3,0.35)} plots the 30-35% interval of the data. Numbers >1 are assumed to refer to sample order
#' in the data
#' @param var1 Name of the first variable to plot. Default "x.raw"
#' @param var2 Name of the second variable to plot (overlayed on var1) Default: "x"
#' @param verbose If TRUE, print the resulting plot
#' @examples new.plot <- filt_plot_temporal(sample.data.filtered, plot.window = c(1000, 2000))
#' @return a ggplot with gaze coordinates plotted on the y axis and sample number on the x axis

filt_plot_temporal <- function(data_in, plot.window = c(NA,NA), var1= "x.raw", var2 ="x", verbose = TRUE){

  #No samples selected. Plot a default interval in the data
  if (sum(is.na(plot.window))>0) {
    default.interval = c(0.5, 0.65)
    message("No plot window selected. Plotting default interval of the data (50-65%)")
    #Select the interval to plot.
    p1 <- round(default.interval[1]*dim(data_in)[1])
    p2 <- round(default.interval[2]*dim(data_in)[1])
  } else if (plot.window[1] > 1) {        #Plot a user specified interval in samples
    p1 <- plot.window[1]
    p2 <- plot.window[2]

  } else if (plot.window[1] <=1) { #Plot a user specified interval defined as proportion of the recording
    p1 <- round(plot.window[1]*dim(data_in)[1])
    p2 <- round(plot.window[2]*dim(data_in)[1])

  }


  data_in <- data_in[p1:p2,]

  # Add sample numbers to the data frame
  data_in$index <- seq(1, dim(data_in)[1])


  #Create a color map that matches the input variables
  color_map <- setNames(c("black", "red"), c(var1, var2))


  g <- ggplot(data = data_in) +
    geom_line(aes(x = index, y = .data[[var1]], color = var1), linewidth = 1.7) +
    geom_line(aes(x = index, y = .data[[var2]], color = var2), linewidth = 1) +
    scale_color_manual(values = color_map, name = "Data") +
    ylab("Gaze position") +
    xlab("Sample") +
    theme(text = element_text(size = 15))

 if (verbose){
  suppressWarnings(
    print(g))}

  return(g)

}




#' Plot fixation filtered vs. raw or unfiltered gaze coordinates in 2D space.
#' @description This function plots and returns a ggplot2 figure showing fixation filtered and raw gaze coordinates plotted against time.
#' The interval to plot can be defined as a proportion of the data frame or by sample numbers. This function uses one data.frame with fixations and one with sample-by-sample raw data
#' @param raw.data gaze matrix which must include columns for filtered and unfiltered data as specified in the raw.columns parameter
#' @param filtered.data Data frame with fixation data which must include columns for filtered x and y data as specified in the raw.columns parameter as well as the
#' variable onset which indicates the onset of the fixation. Make sure the onset varables match the timing in the raw.data df
#' @param plot.window vector defining the time window to plot. If left empty, the 50-65% interval of the data are plotted. If the submitted values are
#' <0, they are assumed to be proportions, e.g., \code{plot.window = c(0.3,0.35)} plots the 30-35 percent of max.length interval of the data. Numbers >1 are assumed to refer to sample order
#' in the data
#' @param raw.columns Names of variable containing raw data. Default x.raw and y.raw
#' @param filt.columns Names of variable containing filtered data. Default x and y
#' @param xres horizontal resolution of the screen or area to plot on. Default 1920
#' @param yres vertical resolution of the screen or area to plot on. Default 1080
#' @param fixation.radius Radius of circles showing fixations.
#' @param verbose if TRUE, print the resulting plot
#' @return a ggplot of raw and fixated values plotted on the y axis and sample number on the x axis

filt_plot_2d <- function(raw.data,filtered.data, plot.window = c(NA, NA), raw.columns = c("x.raw", "y.raw"), filt.columns =c("x", "y"), fixation.radius = 40, xres =
                           1920, yres = 1080, verbose = TRUE){


  #No samples selected. Plot a default interval in the data
  if (sum(is.na(plot.window))>0) {
    default.interval = c(0.5, 0.65)
    message("No plot window selected. Plotting default interval of the data (50-65%)")
    #Select the interval corresponding to a default interval
    plot.window[1] <- raw.data$timestamp[round(default.interval[1]*dim(raw.data)[1])]
    plot.window[2] <- raw.data$timestamp[round(default.interval[2]*dim(raw.data)[1])]

  } else if (sum(is.na(plot.window))== 0 & plot.window[2]<1){ #Plot a user specified interval in samples
    plot.window[1] <- raw.data$timestamp[round(plot.window[1]*dim(raw.data)[1])]
    plot.window[2] <- raw.data$timestamp[round(plot.window[2]*dim(raw.data)[1])]}



  w <- which(raw.data$timestamp> plot.window[1] & raw.data$timestamp<= plot.window[2])
  if (length(w) == 0) {
    warning("No gaze data found within the specified time range.")
  }
  raw.data <- raw.data[w,]

  # Add sample numbers to the data frame
  raw.data$sample <- seq(1, dim(raw.data)[1])


  #Select the raw and filtered gaze variables and give them new names before plotting
  #data_in$filt.x <- data_in[[filt.columns[1]]]
  #data_in$filt.y <- data_in[[filt.columns[2]]]

  raw.data$raw.x <- raw.data[[raw.columns[1]]]
  raw.data$raw.y <- raw.data[[raw.columns[2]]]
  raw.data$radius <- fixation.radius
  # raw.data$fixation.filter <- "raw"

  #Select the filtered data
  w <- which(filtered.data$onset >= plot.window[1] & filtered.data$onset <= plot.window[2])
  filtered.data <- filtered.data[w,]
  #To plot multiple fixation filters, create a label with both threshold and name of filter
  filtered.data$fixation.filter <- paste0(filtered.data$fixation.filter, "_", filtered.data$threshold)
  filtered.data$radius <- fixation.radius


  g <- ggplot(data = raw.data) + geom_circle(data = filtered.data, aes(x0 = .data$x, y0 = .data$y, r = .data$radius, fill = .data$fixation.filter, group = .data$fixation.filter))+
    xlim(c(1,xres))+ylim(c(1, yres))+
    geom_point(aes(x = .data$raw.x, y = .data$raw.y, fill = "Unfilt", group = "Unfilt"), size = 0.5) +
    scale_fill_discrete(name = "Filter")+ylab("Gaze postition X")+xlab("Gaze position Y")+facet_wrap(vars(.data$fixation.filter))+
    theme(text = element_text(size = 15))

  if (verbose){
    suppressWarnings(
      print(g)
    )}
  return(g)
}


#' Plot fixations in 2D space overlaied on a stimulus image
#' @description This function plots and returns a ggplot2 figure showing fixations on a background with one or multiple images, typically the stimuli. Data can
#' represent one or multiple participants
#' The interval to plot is defined by sample numbers. Fixations must have the variables x, y, and onset. The function is tested with .jpg-images. If paths to multiple images
#' are given, all will be displayed. Fixations are shown on a white background if no background images are defined
#' @param gazedata Data frame with fixation data which must include columns for x and y coordinates as well as the
#' variable onset which indicates the onset of the fixation. If the categorical or factor variable id is included, separate colors will represent each participant. Make sure the onset variables match the timing  the plot.onset and plot.offset input.
#' @param background.images data frame with background images to use as background. The data frame must include the variables min.x, min.y, max.x, and max.y variables representing
#' where the images should be placed on the background and the variable path specifying a full file path.
#' #Example:
#' \code{background.images <- data.frame(
#'        path = "my_image.jpg",
#'        min.x = 1,
#'        min.y = 1,
#'        max.x = 200.
#'        max.y = 200)}
#'
#' @param xres horizontal resolution of the screen or area to plot on. Default 1920
#' @param yres vertical resolution of the screen or area to plot on. Default 1080
#' @param plot.onset Onset of the interval in the gaze_data$onset variable to plot in the same unit, typically milliseconds
#' @param plot.offset Offset of the interval in the gaze_data$onset variable to plot in the same unit, typically milliseconds
#' @param show.legend If TRUE, show values in "id" in legend
#' @param group.by If not NA, plot each level in the variable in a separate panel. For example group.by \code{=} "group" returns a separate panel for each group and group.by \code{=}"id"
#' returns a separate panel for each id.
#' @param connect.lines If TRUE, gaze coordinates are connected with lines
#' @param verbose If TRUE, the resulting figure is displayed automatically
#' @param gazepoint.size Size of the circle illustrating the point of gaze
#' @param id_color_map A ggplot color map specifying a color to plot for each id. ids should match the variable id'in the gazedata matrix. Set to NA to assign values automatically.
#' @return a ggplot of raw and fixated values plotted on the y axis and sample number on the x axis


static_plot <- function(gazedata, xres = 1920, yres = 1080, plot.onset, plot.offset, background.images = NA, show.legend = TRUE, group.by = NA,
                        gazepoint.size = 4, id_color_map =NA, connect.lines = TRUE, verbose = TRUE) {

  if (!"id" %in% names(gazedata)){gazedata$id <-"1"}

  gazedata <- filter(gazedata, .data$onset >= plot.onset, .data$offset <= plot.offset)
  gazedata$id <- factor(gazedata$id)
  gazedata <- gazedata %>% group_by(id)

  if (!is.na(group.by)) {
    message(paste0("Splitting plot by variable ", group.by))
    gazedata$group <- gazedata[[group.by]]
  }

  if (is.data.frame(background.images)){ #Rescale from pixels to proportion of the screen
    background.images$min.x <- background.images$min.x/xres
    background.images$min.y <- background.images$min.y/yres
    background.images$max.x <- background.images$max.x/xres
    background.images$max.y <- background.images$max.y/yres

    #Use this aesthetic in all plots
    plot.aes <- aes(x = .data$x, y = .data$y, group = .data$id, colour = .data$id)

    #Create an empty white image
    background <- ggplot()+xlim(1,xres) +ylim(yres,1)+ coord_fixed()+
      theme_void()

    # Add  smulus images to background
    for (i in dim(background.images[1])){
      im <- jpeg::readJPEG(background.images$path[i], native = TRUE)
      background <- background+  inset_element(p = im, left = background.images$min.x[i], right = background.images$max.x[i],
                                               bottom = background.images$min.y[i], top = background.images$max.y[i])

    }
    #Save the background as a temporary file with pixel measures corresponding to screen size
    t <- tempdir()
    temp.path <- paste0(t, "\\background_temp.jpg")
    ggplot2::ggsave(temp.path, width = xres, height = yres, units = "px")
    im <- jpeg::readJPEG(temp.path)

    #Plot on the stimulus background
    suppressMessages(
    g <- ggplot(data = gazedata, aes(x = .data$x, y = .data$y, group = .data$id, colour = .data$id))+xlim(1,xres) +ylim(yres,1)+ coord_fixed() + # Use custom colors
      ggpubr::background_image(im)+theme(panel.background = element_rect(fill = "white", color = "white"))+geom_point(size =gazepoint.size, show.legend = show.legend)
    )

  } else {
    #Plot without background if no images are passed
    suppressMessages(
    g <- ggplot(data = gazedata, aes(x = .data$x, y = .data$y, group = .data$id, colour = .data$id))+xlim(1,xres) +ylim(yres,1)+ coord_fixed() + # Use custom colors
      theme(panel.background = element_rect(fill = "white", color = "white"))+geom_point(size =gazepoint.size, show.legend = show.legend)
    )

  }

  if(connect.lines == TRUE){
    g <- g+ geom_line(show.legend = show.legend)

  }

  #Match the colors to specific IDs
  if (sum(!is.na(id_color_map))>0) {
   g <- g+scale_color_manual(values = id_color_map)}
  #Split the plot in different panels if the group.by argument is not NA
  if (!is.na(group.by)) {
    g <- g + facet_wrap(vars(.data$group))


  }
  if (verbose == TRUE){
  suppressMessages(
    print(g)
  )
  }
  return(g)
}




#' Create ggplot of saccade velocity profiles
#' @description This function plots and returns a ggplot showing velocity profiles of saccades plotted against time. Saccades should be generated with the ivt.filter functions with
#' the save.velosity.profiles parameter set to TRUE. The interval to plot is defined by saccade number as they appear in the sacccades data frame.
#' @param saccades data frame including saccades. Each saccade must have a list with a vector of the veloctity profiles
#' @param onset first saccade to plot. The value must correspond to a number in the variable "number" in the saccades data frame. If left empty, all saccades are plotted
#' @param offset last saccade to plot. The value must correspond to a number in the variable "number" in the saccades data frame.
#' @param verbose If TRUE, print the resulting plot
#' @return ggplot with velocity profiles
#' @examples
#' new.plot <- plot_velocity_profiles(sample.data.saccades, onset = 10, offset = 20)



plot_velocity_profiles <- function(saccades, onset =NA, offset= NA, verbose = TRUE){

  saccades$number <- seq(1,dim(saccades)[1])
  if (!is.na(onset)){
    saccades <- saccades[saccades$number >= onset & saccades$number <= offset,]
  }

  velocity.profiles <- data.frame()
  for (n in seq_along(saccades$number)){
    vp <- data.frame(
      velocity =unlist(saccades$velocity.profile[n]))
    vp$number <- n
    vp$amplitude <- saccades$amplitude[n]
    vp$sample <- seq(1, length(vp$velocity))
    vp$amplitude_label <- paste0("amplitude: ", round(vp$amplitude,1))
    velocity.profiles <- rbind(velocity.profiles, vp)
  }
  p <- ggplot(data = velocity.profiles, aes(x = .data$sample, y = .data$velocity, group = .data$amplitude_label))+geom_line(color = "red", linewidth =1) +
    geom_hline(yintercept = 30)+ geom_text(aes(label = .data$amplitude_label), x = Inf, y = Inf, hjust = 1, vjust = 1, size = 4) +
    facet_wrap(vars(number))+theme(text = element_text(size = 15))

  if (verbose) {
  print(p)}

  return(p)
}


#' Create GIF animation of fixations on a stimulus images
#' @description This function plots and returns a .gif showing fixations on a background with one or multiple images, typically the stimuli.
#' The interval to plot is defined by sample numbers. Fixations must have the variables x, y, and onset. The function works with .jpg images. If paths to multiple images
#' are given, all will be displayed. Fixations are shown on a white background if no background images are defined. .gif images can be saved to a file.
#' Gaze data are plotted on a reversed y-axis where x and y are 0 is the upper left corner, corresponding to the structure of data from Tobii eye trackers. If there are multiple participants specified in the
#' variable id, each participant will get a unique color. You may get an error message if some participants lack data during single frames. This is usually no cause for concern.
#'
#' @param gazedata Data frame with fixation data which must include columns for x and y coordinates as well as the
#' variable onset which indicates the onset of the fixation. Make sure the onset variables match the timing  the plot.onset and plot.offset input.
#' If the categorical or factor variable id is included, separate colors will represent each participant. Make sure the onset variables match the timing  the plot.onset and plot.offset input.
#' @param background.images data frame with information about background images to use as background. The data frame must include the variables min.x, min.y, max.x, and max.y variables representing
  #' where the images should be placed on the background, the variable path specifying a full file path, and the onset and offset of each image in units corresponding to the time stamps of the gazedata
#' matrix. Background images should be in JPEG format. This is an example:
#'  \code{
#' background.images <- data.frame(
#'                       min.x = c(100, 800),
#'                       min.y = c(100, 100),
#'                       max.x = c(300, 100),
#'                       max.y = c(600, 600),
#'                       path = c("~/path_to_image1/image1.jpg", "~/path_to_image1/image2.jpg"),
#'                       onset = c(1, 4000),
#'                       offset = c(4000, 6000)}
#'
#' @param xres horizontal resolution of the screen or area to plot on. Default 1920
#' @param yres vertical resolution of the screen or area to plot on. Default 1080
#' @param plot.onset Onset of the interval in the gaze_data$onset variable to plot in the same unit, typically milliseconds
#' @param plot.offset Offset of the interval in the corresponding to the variable onset in the input data frame gazedata to plot in the same unit, typically milliseconds
#' @param save.gif If TRUE, save the created .gif file under the name specified in the filename parameter
#' @param filename Name of path where the .gif is saved
#' @param gif.dpi Resolution in dpi if .gif is saved. Lower values give smaller files.
#' @param n.loops Specify the number of times to play the plotted sequence. Default is 1. If n.loops is 0, the .gif will play in an eternal loop
#' @param gazepoint.size Size of marker representing fixation coordinates.
#' @param show.legend If TRUE, show values of the variable id in legend
#' @param id_color_map A character vector with HEX color codes for each id. If \code{NA}, a color map with unique colors for each id is created by the function.
#' You can create a specific color map for your data using the following code:
#' new_color_map <-  \code{c("#FB61D7", "#00C094")}
#' names(new_color_map) <-  \code{c("Id1", "Id2")}
#' @param framerate Frames per seconds of the returned animation. Default 10
#' @param resolution.scaling Scaling of the original images and gaze data. Default is 0.5. Decreasing the size of the images can make the function quicker.
#' This can be useful if you want to assign specific colors for different groups
#' @param show.progress If TRUE, show progression of the function in the prompt
#' @return a magick animation of raw and fixated values plotted on the y axis and sample number on the x axis



animated_fixation_plot <- function(gazedata, xres = 1920, yres = 1080, plot.onset, plot.offset, background.images = NA, filename ="scanpath.gif",
                                   save.gif = FALSE, gif.dpi = 300, gazepoint.size = 2, n.loops = 1, show.legend = TRUE, id_color_map =NA,
                                   resolution.scaling = 0.5, framerate = 10, show.progress = TRUE) {

  #Rescale the plot data
  xres <- xres* resolution.scaling
  yres <- yres*resolution.scaling
  gazedata$x <- gazedata$x *resolution.scaling
  gazedata$y <- gazedata$y *resolution.scaling

  #Rescale the images
  if (is.data.frame(background.images)){
    if (nrow(background.images)>0){
    background.images$min.x <- background.images$min.x*resolution.scaling
    background.images$min.y <- background.images$min.y*resolution.scaling
    background.images$max.x <- background.images$max.x*resolution.scaling
    background.images$max.y <- background.images$max.y*resolution.scaling
  }
  }


  if (!"id" %in% names(gazedata)){gazedata$id <-"1"}

  if (sum(!is.na(id_color_map)) == 0) {   # Generate unique colors for each id if no color map was passed
    unique_ids <- unique(gazedata$id)
    color_palette <- scales::hue_pal()(length(unique_ids))
    id_color_map <- setNames(color_palette, unique_ids)
    gazedata$id <- factor(gazedata$id, levels = names(id_color_map))
    }

  ms.per.frame <- 1000/framerate
  duration <- plot.offset - plot.onset
  #To get correct timing, the frames per second passed to gganimate must scale to 100
  if (floor(duration/framerate) != duration/framerate) {
    exclude.from.plot <- duration - floor(duration/ms.per.frame)*ms.per.frame
    plot.offset <- plot.onset + floor(duration/ms.per.frame)*ms.per.frame
    message(
      paste0("duration/framerate is not even. Excluding the last ",round(exclude.from.plot), "ms"))
  }

  ms.per.frame <- 1000/framerate

  framestarts <- seq(plot.onset, plot.offset, ms.per.frame)

  framestops <- framestarts + ms.per.frame + 0.1 #Add a margin to avoid overlapping labels

  still.plots <- vector(mode='list', length=length(framestarts))
  still.images <- vector(mode='list', length=length(framestarts))

  for (i in 1: length(framestarts)){

    #Select the relevant gaze data. Set the onsets and offsets to match frame timing. Important for static.plot
    w <- which(gazedata$offset >= framestarts[i] & gazedata$onset<framestops[i])
    fixations.by.frame <- gazedata[w,]

    #Add ids without data to the frame to make them appear in the figure legend in the figure produced by static_plot
    missing <- unique_ids[!unique_ids %in% fixations.by.frame$id]
    if (length(missing)>0){
      m <-as.data.frame(matrix(NA, nrow = length(missing), ncol = length(names(fixations.by.frame))))
      names(m) <- names(fixations.by.frame)
      m$id <- missing
      m$x <- -1000 #Set x and y values of the empty entries to be outside the plot limits. This ensures that frames without data will be printed with
      #just the stimulus background
      m$y <- -1000
      fixations.by.frame <- rbind(fixations.by.frame, m)
    }


    if(nrow(fixations.by.frame)>0){
      fixations.by.frame$offset <- framestops[i] - 0.01
      fixations.by.frame$onset <- framestarts[i] + 0.01}



    if (is.data.frame(background.images)) {
      image.nrs <- which(background.images$offset >= framestops[i] & background.images$onset < framestops[i])
      images.by.frame <- background.images[image.nrs,]
      if (nrow(images.by.frame) == 0) {images.by.frame <- NA}

    } else {images.by.frame<- NA}

    suppressWarnings(
      still.plots[[i]]<- static_plot(gazedata = fixations.by.frame, background.images = images.by.frame, xres = xres, yres = yres, plot.onset = framestarts[i], plot.offset = framestops[i],
                                     show.legend = show.legend, gazepoint.size = gazepoint.size, id_color_map = id_color_map, connect.lines = FALSE, verbose = FALSE)
    )
    suppressWarnings(
      still.plots[[i]]<- still.plots[[i]] +  theme(axis.text.x = element_blank(), axis.text.y = element_blank())+xlab("")+ylab("")   # Remove x and y-axis tick labels
    )

    tmp_file <- tempfile(fileext = ".png")
    ggplot2::ggsave(tmp_file, still.plots[[i]], width = xres, height = yres, dpi = 300, units = "px")

    # Read the image file as a magick object
    still.images[[i]] <- magick::image_read(tmp_file)
    if (show.progress == TRUE) { #Print the currently generated frame
        cat("\r", paste0("Creating frame ", i))}
  }
  animation <- magick::image_animate(image_join(still.images), fps = framerate, loop = n.loops)
  if (save.gif == TRUE){
    message("Saving .gif to file")
    magick::image_write(animation, path = filename)

  }
  return(animation)
}





#'Plot the sample-to-sample velocity of eye tracking data.
#'@description
#'
#' This function visualizes the sample-to-sample velocity in a period of eye tracking data. This can be helpful when determining a suitable velocity threshold for saccade detection
#' Input data must be a data frame with the variables timestamp, x.raw and y.raw as variables. Other variables can
#' be included but will be ignored. This function does not perform pre-processing in the form of interpolation or smoothing. Use the function process.gaze for this.
#' Timestamps are assumed to be in milliseconds. Default settings assume that x and y coordinates are in pixels.
#' The output data is a plot of sample-to-sample velocity in the selected interval.
#' @param data_in Data frame with gaze data to plot. Include the variable timestamp with timing in ms and columns with raw
#' x and y data as specified by the paramerers xcol and ycol or their default values
#' @param one_degree One degree of the visual field in the unit of the raw x and y coordinates, typically pixels
#' @param velocity.filter.ms Window in milliseconds for moving average window used for smoothing the sample-to-sample velocity vector.
#' @param threshold.line Can be specified to add a line showing a potential velocity threshold for saccade detection. No threshold is shown if this parameter is NA
#' @param plot.window vector defining the time window to plot. If left empty, the 50-65% interval of the data are plotted. If the submitted values are
#' <0, they are assumed to be proportions, e.g., plot.window = c(0.3,0.35) plots the 30-35 percent of max.length interval of the data. Numbers >1 are assumed to refer to sample order
#' in the data
#' @param xcol Name of the column where raw x values are stored. Default: "x.raw"
#' @param ycol Name of the column where raw y values are stored. Default: "y.raw"
#' @param verbose If TRUE, print the resulting plot
#' @return A ggplot showing the sample-to-sample velocity of the selected data interval
#' @examples
#' plot_sample_velocity(data_in = sample.data.processed, threshold.line = 35)
#'

plot_sample_velocity <- function(data_in, velocity.filter.ms =20, plot.window = c(NA, NA), xcol = "x.raw", ycol = "y.raw", threshold.line = NA,
                                 one_degree = 40, verbose = TRUE){



  #No samples selected. Plot a default interval in the data
  if (sum(is.na(plot.window))>0) {
    default.interval = c(0.5, 0.65)
    message("No plot window selected. Plotting default interval of the data (50-65%)")

    #Select the interval to plot.
    p1 <- round(default.interval[1]*dim(data_in)[1])
    p2 <- round(default.interval[2]*dim(data_in)[1])
  } else if (sum(is.na(plot.window))== 0 & plot.window[2]<1){ #Plot a user specified interval in samples
    p1 <- round(plot.window[1]*dim(data_in)[1])
    p2 <- round(plot.window[2]*dim(data_in)[1])
  } else { #Look for rows matching ms intervals
    p1 <- which(data_in$timestamp>plot.window[1])[1]
    p2 <- tail(which(data_in$timestamp<plot.window[2]),1)

  }

  data_in <- data_in[p1:p2,]

  #Create new variables in the raw gaze matrix with the expected variable names if
  #the data are stored under different names (specified in the xcol and ycol parameters)
  if (!"x.raw" %in% names(data_in)){
    data_in$x.raw <- data_in[[xcol]]
    data_in$y.raw <- data_in[[ycol]]
  }


  one.sample <- mean(diff(data_in$timestamp),na.rm = T)
  if (one.sample < 0.02) {warning("Unlikely small sample to sample difference in timestamps. Are timestamps in milliseconds?")}

  #Calculate samples to smooth velocity vector
  velocity.smooth.window <- round(velocity.filter.ms/one.sample)

  #Calculate sample to sample distance in raw unit (pixels or proportion of screen)
  data_in$distance <- c(NA,sqrt(diff(data_in$x.raw)^2 +diff(data_in$y.raw)^2))
  #Recalculate to degrees of the visual field
  data_in$distance <- data_in$distance/one_degree

  #
  data_in$velocity <- data_in$distance/one.sample #Velocity per second (assuming that timestamps are in ms!)

  data_in$velocity <- data_in$velocity*1000

  #Smoooth the velocity vector
  data_in$velocity <-rollmedian(data_in$velocity, k = velocity.smooth.window, na.pad = T, align = "center")


  #Create a ggplot of sample to sample velocity
  data_in$sample <- seq(1, dim(data_in)[1])
  p <- ggplot(data = data_in, aes(x = .data$sample, y = .data$velocity)) +geom_line(color = "darkblue", linewidth = 1)+theme_minimal()+theme(text = element_text(size =15))+
    ylab("velocity (degrees/second)")

  #Add a line showing a potential threshold for saccade identification
  if (!is.na(threshold.line)){
    line.label <- paste0(threshold.line, " deg.")
    p <- p+geom_hline(yintercept = threshold.line, linetype ="dashed", linewidth = 1)+
      geom_label(data = data.frame(x = 100, y = threshold.line), aes(x = .data$x, y = .data$y), label = line.label)
  }

  if (verbose){
  print(p)}

  return(p)

}


#'Plot validity measures from one or more fixation detection algorithms
#'@description
#'
#' This function visualizes validity measures of fixations detected with one or more fixation detection algorithms.
#' The function is tested for fixation data frames generated with kollaR event detection algorithms. By default, the function can plot
#' Root Mean Square Deviations of detected fixations, fixation duration and the proportion of missing raw samples.
#' The output data is a ggplot which can be modified further outside the function.
#' If you want to use this function to compare more than one fixation detection algorithms, combine them using the function
#' rbind in base R. For example, \code{rbind(my_data1[["fixations"]], my_data2[["fixations"]])} would generate a combined data frame with the
#' fixations detected by two event classification procedures.
#'
#' @param data_in Data frame with fixations to plot
#' @param plot.variable Variable to plot. If left empty, RMSD of fixations are plotted. Alternatives are "rmsd", "duration", "missing.samples"
#' @return A ggplot with visualizations of the selected validity measure
#' @examples
#' plot_filter_results(data_in = sample.data.fixations, plot.variable = "rmsd")

plot_filter_results <- function(data_in, plot.variable = "rmsd") {
  data_in$algorithm <- paste0(data_in$fixation.filter, "_", data_in$threshold)
  if (sum(grepl(plot.variable, names(data_in))) != 1){
    warning("The variable to plot must be present in the fixation data frame!")
  }

  names(data_in) <- gsub(plot.variable, "ms", names(data_in))
  g <- ggplot(data = data_in, aes(x = .data$algorithm, y = .data$ms, fill = .data$algorithm, group = .data$algorithm))+
    geom_jitter(width = 0.4, color = "black", fill = "black", alpha = 0.6, size = 0.6)+
    geom_boxplot(width = 0.15, outliers = FALSE, alpha = 0.7) + theme_minimal()+theme(text = element_text(size = 15))+geom_violin(width = 0.6, fill = NA)+
    ylab(plot.variable)

  return(g)
}


