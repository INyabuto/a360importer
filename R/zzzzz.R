#' Get data frame names
#'
#' @param dt A list of data.table or data.frames
#' @return A vector of characters
dt_names <- function(dt){

  # get the names of the files
  if (length(dt) > 0) {

    dt_names <- sapply(dt, names)

    dt_names_c <- do.call(c, dt_names)

    dt_names <- unique(dt_names_c)

    names(dt_names) <- dt_names

    dt_names
  }

}

dt_method_recieved <- function(dt){

  # get all the methods in the file
  dt_methods <- sapply(dt, function(x) x[,`Method Received`])

  dt_methods_c <- do.call(c, dt_methods)

  dt_methods <- unique(dt_methods_c)

  dt_methods

}

dt_program_entries <- function(dt){

  # get all the entry points
  dt_programs <- sapply(dt, function(x) x[,`Program Entry Point`])

  dt_programs_c <- unique(do.call(c, dt_programs))

  dt_programs_c
}

dt_current_method <- function(dt){
  dt_methods <- sapply(dt, function(x) x[,`Current Method`])

  unique(do.call(c,dt_methods))
}

condom_at_last_sex <- function(dt){
  dt_condom_last_sex <- sapply(dt, function(x) x[,`Used EC/Condoms last sex`])

  unique(do.call(c,dt_condom_last_sex))
}

pregancy_results <- function(dt){
  dt_results <- sapply(dt, function(x) x[,`Pregnant?`])
  unique(do.call(c, dt_results))
}

condom_as_dual <- function(dt){
  dt_condom <- sapply(dt, function(x) x[,`Received Condoms as a dual Method`])
  unique(do.call(c, dt_condom))
}
