#' Get data frame names
#'
#' @param dt A list of data.table or data.frames
#' @return A vector of characters
dt_names <- function(dt = NULL){

  # get the names of the files
  if (!is.null(dt)) {
    sapply(dt, names) %>%
      do.call(c, .) %>%
      unique(.)
  }

}

dt_method_recieved <- function(dt = NULL){

  if (!is.null(dt)){
    sapply(dt, function(x) x[,"Method Received"]) %>%
      do.call(c, .) %>%
      unique(.)
  }
}

dt_program_entries <- function(dt = NULL){

  if (!is.null(dt)){
    sapply(dt, function(x) x[,"Program Entry Point"]) %>%
      do.call(c, .) %>%
      unique(.)
  }
}

dt_current_method <- function(dt = NULL){

  if (!is.null(dt)){
    sapply(dt, function(x) x[,"Current Method"]) %>%
      do.call(c, .) %>%
      unique(.)
  }

}

condom_at_last_sex <- function(dt = NULL){

  if (!is.null(dt)){
    sapply(dt, function(x) x[,"Used EC/Condoms last sex"]) %>%
      do.call(c, .) %>%
      unique(.)
  }

}

pregancy_results <- function(dt = NULL){

  if (!is.null(dt)){
    sapply(dt, function(x) x[,"Pregnant?"]) %>%
      do.call(c, .) %>%
      unique(.)
  }

}

condom_as_dual <- function(dt = NULL){

  if (!is.null(dt)){
    sapply(dt, function(x) x[, "Received Condoms as a dual Method"]) %>%
      do.call(c, .) %>%
      unique(.)
  }

}

program_activities <- function(dt = NULL){

  if (!is.null(dt)){
    sapply(dt, function(x) x[, "Program Activity"]) %>%
      do.call(c, .) %>%
      unique(.)
  }

}

attendance_frequencies <- function(dt = NULL){

  if (!is.null(dt)){
    sapply(dt, function(x) x[, "Attendance Frequency"]) %>%
      do.call(c, .) %>%
      unique(.)
  }

}
