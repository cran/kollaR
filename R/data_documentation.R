#' Unprocessed sample-by-sample example data
#'
#' This dataset contains data from 1 individual during a free viewing tasks before pre-processing. Data were
#' recorded at 1200 Hz using a Tobii Pro Spectrum eye tracker
#'
#' @format A data frame
#' \describe{
#'   \item{id}{participant number}
#'   \item{timestamp}{timestamp in ms recorded by the eye tracker}
#'   \item{x.raw}{gaze position x}
#'   \item{y.raw}{gaze position y}
#' }
#'
#' @source The dataset was stored in the package at 'data/example_data.RData'
"sample.data.unprocessed"



#' Pre-processed sample-by-sample example data
#'
#' This dataset contains data from 1 individuals during a free viewing tasks after pre-processing. Data were
#' recorded at 1200 Hz using a Tobii Pro Spectrum eye tracker
#'
#' @format A data frame
#' \describe{
#'   \item{id}{participant number}
#'   \item{timestamp}{timestamp in ms recorded by the eye tracker}
#'   \item{x.raw}{gaze position x}
#'   \item{y.raw}{gaze position y}
#'   \item{x.unprocessed}{copy of gaze position x before preprocessing}
#'   \item{y.unprocessed}{copy of gaze position y before preprocessing}
#'   \item{timestamp.t}{"'Theoretical timestamp' for comparison."}
#'   \item{sample}{sample nr in recording}


#' }
#'
#' @source The dataset was stored in the package at 'data/example_data.RData'
"sample.data.processed"








#' Saccades from 3 individuals
#'
#' This dataset contains saccade data from 3 individuals during a free viewing tasks. Data were
#' recorded at 1200 Hz using a Tobii Pro Spectrum eye tracker
#'
#' @format A data frame
#' \describe{
#'   \item{onset}{onset of the saccade in ms}
#'   \item{x.onset}{gaze position x at onset}
#'   \item{y.onset}{gaze position y at onset }
#'   \item{offset}{offset of the saccade in ms}
#'   \item{x.offset}{gaze position x at offset}
#'   \item{y.offset}{gaze position y at offset }
#'   \item{duration}{duration of saccade in ms}
#'   \item{amplitude}{amplitude of saccade in degrees}
#'   \item{peak.velocity}{peak velocity of saccade}
#'   \item{velocity.profile}{velocity profile}
#'   \item{missing.samples}{proportion of missing samples}
#'   \item{id}{participant number}
#'   }
#' @source The dataset was stored in the package at 'data/example_data.RData'
"sample.data.saccades"


#' Fixations from 7 individuals
#'
#' This dataset contains fixation data from 7 individuals during a free viewing tasks. Data were
#' recorded at 1200 Hz using a Tobii Pro Spectrum eye tracker
#'
#' @format A data frame
#' \describe{
#'   \item{x}{fixation position x}
#'   \item{y}{fixation position y}
#'   \item{duration}{duration of fixation in milliseconds}
#'   \item{onset}{onset of fixation in milliseconds}
#'   \item{offset}{offset of fixation in milliseconds}
#'   \item{rmsd}{Sample-to-sample root mean square deviation of all samples}
#'   \item{rms.from.center}{Root means square deviation of all included samples from the centroid of the fixation}
#'   \item{missing.samples}{proportion of missing samples}
#'   \item{fixation.algorithm}{Name of the fixation filter algorithm }
#'   \item{threshold}{Threshold setting for the fixation classification algorithm}
#'   \item{id}{Participant id}}
#' @source The dataset was stored in the package at 'data/example_data.RData'
"sample.data.fixations"


#' Fixations from 1 individual
#'
#' This dataset contains fixation data from 1 individuals during a free viewing tasks. Data were
#' recorded at 1200 Hz using a Tobii Pro Spectrum eye tracker
#'
#' @format A data frame
#' \describe{
#'   \item{x}{fixation position x}
#'   \item{y}{fixation position y}
#'   \item{duration}{duration of fixation in milliseconds}
#'   \item{onset}{onset of fixation in milliseconds}
#'   \item{offset}{offset of fixation in milliseconds}
#'   \item{rmsd}{Sample-to-sample root mean square deviation of all samples}
#'   \item{rms.from.center}{Root means square deviation of all included samples from the centroid of the fixation}
#'   \item{missing.samples}{proportion of missing samples}
#'   \item{fixation.algorithm}{Name of the fixation filter algorithm }
#'   \item{threshold}{Threshold setting for the fixation classification algorithm}
#'   \item{id}{Participant id}
#'   }
#' @source The dataset was stored in the package at 'data/example_data.RData'
"sample.data.fixation1"


#' Sample-to-sample raw and fixation classified data from 1 individual
#'
#' This dataset contains sample-to-sample data from 1 individuals during a free viewing tasks. Data were
#' recorded at 1200 Hz using a Tobii Pro Spectrum eye tracker. Fixations were classified with the I-VT algorithm with a velocity threshold set to 30 degrees/seconds
#' and default settings in the function algorithm_ivt
#'
#' @format A data frame
#' \describe{
#'   \item{x}{fixation position x}
#'   \item{y}{fixation position y}
#'   \item{x.raw}{fixation position x}
#'   \item{y.raw}{fixation position y}
#'   \item{timestamp}{timestamp in milliseconds}
#'   }
#' @source The dataset was stored in the package at 'data/example_data.RData'
"sample.data.classified"

