#' Select Colums to Transform
#'
#' \code{select_att} selects columns names and types to transform from the attendance sheets.
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
#' @param dt A data.table or data.frame
#' @param cols A vector of characters
#' @return A chopped data.table or data.frame
select_att_mma <- function(dt = NULL, cols = NULL){

  if (is.null(dt)){
    stop("MMA attendance sheet must be specified", call. = FALSE)
  }

  if (!is.null(dt) && is.null(cols)){
    cols <- c("State","Region","LGA","Ward","Facility","Girl ID","Session 1 Date", "Attendance Frequency")

    dplyr::select(dt, cols)
  }

  if (!is.null(dt) && !is.null(cols)){
    cols <- cols

    dplyr::select(dt, cols)

  }


}



# Program Activity
# Name
# Date Calculation
# ID #
# Age (Formula)
# Type of Facility
# Attendance Frequency
# Facility


#' Select Columns to Transform
#'
#' \code{select_prov} select column names and types to transform from the serv ice provison sheets.
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
              "Visit Type","Rec. Counseling","Current Method","Used EC/Condoms last sex","Signed assent/Consent (All)","15-17 Given Consent Form",
              "15-17 Returned Consent Form", "Pregnant?", "Method Received","Received Condoms as a dual Method", "Currently Using","Took up Method", "Provider Outcome Status", "STI Treatment","Date of treatment",
              "Case Treated","Drugs Given", "QTY of Drugs Given")
    dplyr::select(dt,cols)
  }

  if (!is.null(dt) && !is.null(cols)){
    cols <- cols

    dplyr::select(dt,cols)

  }



}
