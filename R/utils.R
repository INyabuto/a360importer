#' Remove empty rows
#'
#' \code{remove_empty_rows} finds rows with entire NAs and and wipes off.
#'
#' @param df A data frame
#' @return a packed df
remove_empty_rows <- function(dt){

  # replace "" with NAs
  for(col in names(dt)) {
    set(dt, i= which(dt[[col]] == ""), j = col, value=NA)
  }

  dt[rowSums(is.na(dt)) != ncol(dt),]


}

#' Remove NAs
#'
#' @param dt a data.table
#' @return a table without NAs
remove_nas <- function(dt){

  dt[is.na(dt)] <- ""

}



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


