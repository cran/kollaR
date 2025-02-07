#'Test whether a gaze coordinates are within or outside a rectangular or elliptical AOI.
#'The aois df must contain the variables x0, x1, y0 and y1. x0 is the minimum x value, y0 the minimum y value. x1 the maximum x value. y1 the maximum y value and
#'type where rect means that the AOI is a rectangle and circle that the AOI is a circle or ellipse
#'If a column called name is present, the output for each
#'AOI will be labelled accordingly. Otherwise, the output will be labelled according to the order of the AOI in the data frame.
#'The df 'gaze' must contain the variables onset, duration, x, and y.
#'Latency will be defined as the value in onset of the first detected gaze coordinate in the AOI Make sure
#'that the timestamps are correct!
#'The function can be used with gaze data either fixations, saccades, or single samples. Note that the output variables are not equally relevant for all types of
#'gaze data. For example, both total duration and latency are relevant in many analyses focusing on fixations, but total duration may be less relevant in analyses of
#'saccades.
#'@param gaze data frame with each row representing a gaze coordinate from a fixation, saccade, or sample. Must include the variables x, y, duration, and onset. Onset zero should
#'typically be trial onset
#'@param aois data frame with AOIs.
#'@param outside If FALSE, summarize data within AOIs. If TRUE, summarize data outside AOIs.
#'@return data frame with total duration, number of occurrences and latency to first detected gaze coordinates for each AOI. Data are in long format.
aoi_test <- function(gaze, aois, outside = FALSE){

  #If the data frame aois has a variable with AOI names, name the output accordingly. Otherwise, name the output by aoi number
  if (is.null(aois$name)) {
    aois$name <- seq(1, nrow(aois))

  }

  n.aois <- dim(aois)[1]
  data.out <- data.frame()

  for (i in 1: n.aois){
    #Find the relevant fixations

    if(aois$type[i] == "circle") { #Test whether the gaze coordinates are within a circle/ellipse
      h <- (aois$x0[i]+aois$x1[i])/2
      k <- (aois$y0[i] + aois$y1[i])/2
      a <- abs(aois$x1[i] - aois$x0[i])/2
      b <- abs(aois$y1[i] - aois$y0[i])/2

      inaoi <- ((gaze$x -h)^2 /a^2 + (gaze$y-k)^2/b^2) <= 1

    } else if (aois$type[i] =="rect") { #Test whether gaze coordinates are within a rectangle
      inaoi <- gaze$x>= aois$x0[i] & gaze$x <= aois$x1[i] &
        gaze$y >= aois$y0[i] & gaze$y <= aois$y1[i]


    }


    if (outside == FALSE){
      suppressWarnings(
        firsthit <- min(which(inaoi ==TRUE)) #First registered gaze point inside AOI
      )
      data.out <- rbind(
        data.out, data.frame(
          total.duration = sum(gaze$duration[inaoi]),
          n.fix = sum(inaoi),
          latency = gaze$onset[firsthit],
          aoi = aois$name[i]))

    } else if (outside == TRUE) {
      suppressWarnings(
        firsthit <- min(which(inaoi ==FALSE))) #First registered gaze point OUTSIDE AOI
      data.out <- rbind(
        data.out, data.frame(
          total.duration = sum(gaze$duration[!inaoi]),
          n.fix = sum(!inaoi),
          latency = gaze$onset[firsthit],
          aoi = aois$name[i]

        ))
    }

  }
  return(data.out)
}



#'Draw one or more areas of interest, AOIs, on a stimulus image and save to the R prompt.
#'The input is the path to a 2D image. Supported file formats: JPEG, BMP, PNG.
#'The function returns a data frame with all saved AOIs. By default, AOIs are drawn in a
#'coordinate system where y is 0 in the lower extreme of the image, e.g., an ascending y axis. Tobii eye trackers use a coordinate system with a descending
#'y-axis, e.g., x and y are 0 in the upper left corner of the image. Make sure that your AOIS match the coordinate system of your eye tracker output.
#'By setting the parameter reverse.y.axis to TRUE, the saved AOIs will be reformatted to fit a coordinate system with a descending y-axis.
#'All AOIS have the variables x0, x1, y0 and y1. x0 is the minimum x value, y0 the minimum y value. x1 the maximum x value. y1 the maximum y value
#'@param image.path path to a valid image file with the suffix .jpeg, .jpg, .png or .bmp
#'@param reverse.y.axis If TRUE, save AOIs positioned on a reverse Y-axis where y is 0 in the upper end of the image. Note that AOIs will be converted to fit a reversed Y axis
#'before printed in the R prompt and saved, but will be shown in the original coordinate system when plotted inside the function.
#'@return data frame with type and coordinates of drawn AOIs


draw_aois <- function(image.path, reverse.y.axis = FALSE) {
  saved_aois <- shiny::reactiveVal(data.frame())
  ui <- shiny::fluidPage(
    actionButton("print_shapes", "Print AOIs to R prompt"),
    actionButton("save_exit", "Save AOIs and Exit"),
    br(),
    plotlyOutput("plot"),

  )


  if (grepl(".jpg" ,image.path) |grepl(".jpeg", image.path)) {
    mime <- "image/jpeg"
  } else if (grepl(".png", image.path)) {
    mime <- "image/png"
  } else if (grepl(".bmp", image.path)) {
    mime <- "image/bmp"} else {
      print("Supported image formats are .jpg, .png, .bmp")
      mime <- NA
    }


  server <- function(input, output, session) {

    # Reactive value to store active shapes
    active_shapes <- shiny::reactiveValues(data = NULL)

    # Encode the provided image path to a base64 string
    img_base64 <- base64enc::dataURI(file = image.path, mime = mime)

    #  img_base64 <- base64enc::dataURI(file = image_path, mime = "image/jpeg")

    image.dimensions <- magick::image_read(image.path)
    image.dimensions <- magick::image_info(image.dimensions)
    img_width <- image.dimensions$width
    img_height <- image.dimensions$height
    scale_factor <- 1


    # Create the Plotly plot
    output$plot <- renderPlotly({
      fig <- plotly::plot_ly(width = img_width * scale_factor, height = img_height * scale_factor) %>%
        add_trace(x = c(0, img_width * scale_factor),
                  y = c(0, img_height * scale_factor),
                  type = 'scatter',
                  mode = 'markers',
                  alpha = 0) %>%
        layout(
          images = list(
            list(
              source = img_base64,
              x = 0,
              sizex = img_width * scale_factor,
              y = img_height * scale_factor,
              sizey = img_height * scale_factor,
              xref = "x",
              yref = "y",
              opacity = 1.0,
              layer = "below",
              sizing = "stretch"
            )
          )
        ) %>%
        config(modeBarButtonsToAdd = list("drawcircle", "drawrect", "eraseshape"))

      xconfig <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = TRUE,
        showgrid = FALSE,
        range = c(0, img_width * scale_factor),
        fixedrange = TRUE
      )

      yconfig <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = TRUE,
        showgrid = FALSE,
        range = c(0,img_height * scale_factor),
        scaleanchor="x",
        fixedrange = TRUE
      )

      fig <- fig %>% layout(xaxis = xconfig, yaxis = yconfig)


      fig
    })


    observe({
      relayout_data <- plotly::event_data("plotly_relayout")
      updated.shapes <- sum(grepl("shapes\\[", names(relayout_data))) #If this part of the string occurs, only position/size was updated

      if (length(relayout_data$shapes)>0) { #Some new AOIs are saved. Save a complete list
        aois <- as.data.frame(relayout_data$shapes)
        aois <- aois[,9:13]
        #Reverse y-axis to fit common eye tracker coordinate systems if requested
        if (reverse.y.axis == TRUE) {
          aois$y0 <- img_height - aois$y0
          aois$y1 <- img_height - aois$y1
        }
        #Update AOIs
        saved_aois(aois) #Save the updated values in the reactive value saved_aois

      }
      if (updated.shapes >0) {
        updates <- relayout_data
        shape.nr <- as.numeric(substr(names(updates[1]),8,8))+1  #Find the number of the AOI that was updated
        aois <- saved_aois()

        aois$x0[shape.nr] <- updates[[1]]
        aois$y0[shape.nr] <- updates[[2]]
        aois$x1[shape.nr] <- updates[[3]]
        aois$x0[shape.nr] <- updates[[4]]

        if (reverse.y.axis == TRUE) { #Reverse the y axis after update
          aois$y0[shape.nr]  <- img_height - aois$y0[shape.nr]
          aois$y1[shape.nr]  <- img_height - aois$y1[shape.nr]
        }

        saved_aois(aois) #Save the updated values in the reactive value saved_aois
      }
      if (length(relayout_data$shapes) == 0 & !is.null(relayout_data$shapes)) { #The last active AOI has been removed. Save an empty df
        saved_aois(data.frame())
      }


    })


    # Save all currently drawn AOIs in a global variable and return
    shiny::observeEvent(input$print_shapes, {
      message("All drawn AOIs:\n")
      message(saved_aois())
      message("Make sure that the Y axis direction is consistent with your eye tracker output!")
      if (reverse.y.axis == TRUE) {
        message("The Y coordinates of the saved AOIs are reversed (assuming that y = 0 is in the upper end of the image")
      }

    })

    shiny::observeEvent(input$save_exit, {
      shiny::stopApp(saved_aois())
    })

  }

  shiny::runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)

}
