#' Select Colums to Transform
#'
#' \code{select_att} selects columns names and types to transform from the attendance sheets.
#'
#' It selects the following columns: State, Region, LGA, Ward, Facility, Type of Facility, Name, ID #,
#' Age, Date Calculation and Program Activity.
#'
#' @param dt A data.table or data,.frame
#' @param cols A vector of characters
#' @return A chopped data.table or data.frame
select_att <- function(dt = NULL, cols = NULL){

  if (is.null(dt)){
    stop("An attendance sheet must be specified", call. = FALSE)
  }

  if (!is.null(dt) && is.null(cols)){
    cols <- c("State","Region","LGA","Ward","Facility","Type of Facility","Name","ID #", "Age","Date Calculation","Program Activity")

    dplyr::select(dt,cols)
  }

  if (!is.null(dt) && !is.null(cols)){
    cols <- cols

    dplyr::select(dt, cols)
  }

}

#' Select Columns to Transform - MMA Attendance
#'
#' \code{select_att_mma} selects columns names and types to transform from the MMA attendance sheets
#'
#' It selects the following columns: State, Region, LGA, Ward, Facility, Girl ID, Age, Session 1 Date
#' and Attendance Frequency.
#'
#' @param dt A data.table or data.frame
#' @param cols A vector of characters
#' @return A chopped data.table or data.frame
select_att_mma <- function(dt = NULL, cols = NULL){

  if (is.null(dt)){
    stop("MMA attendance sheet must be specified", call. = FALSE)
  }

  if (!is.null(dt) && is.null(cols)){
    cols <- c("State","Region","LGA","Ward","Facility","Girl ID","Age","Session 1 Date", "Attendance Frequency")

    dplyr::select(dt, cols)
  }

  if (!is.null(dt) && !is.null(cols)){
    cols <- cols

    dplyr::select(dt, cols)

  }


}



#' Select Columns to Transform
#'
#' \code{select_prov} selects column names and types to transform from the service provison sheets.
#'
#' It selects the following columns: State, Region, LGA , Ward, Facility, Type of Facility, Date Calculation,
#' ID #, Age (Formula), Program Entry Point, Current Method,Used EC/Condoms last sex, Pregnant?, Method recieved
#' and Received Condoms as a dual Method`
#'
#' @param dt A data.table or data.frame
#' @param cols A vector of characters
#' @return a chopped data.table or data.frame
select_prov <- function(dt = NULL, cols = NULL){

  if (is.null(dt)){
    stop("Service Provision sheet must be specified", call. = FALSE)
  }

  if (!is.null(dt) && is.null(cols)){
    cols <- c("State","Region","LGA","Ward","Facility","Type of Facility","ID #", "Age (Formula)","Date Calculation","Program Entry Point",
              "Current Method","Used EC/Condoms last sex","Pregnant?", "Method Received","Received Condoms as a dual Method")
    dplyr::select(dt,cols)
  }

  if (!is.null(dt) && !is.null(cols)){
    cols <- cols

    dplyr::select(dt,cols)

  }



}


