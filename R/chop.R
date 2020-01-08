#' Select columns to import
#'
#' @description This function selects columns to import from a data frame
#'   object. Its a general puporse function, so you can specify any columns to
#'   import. It returns a data frame with the specified columns.
#'
#' @details \code{select_cols} is used to chop the attendance, mma attendance
#' and service log sheets in data frames:
#'
#' \itemize{
#' \item{\code{select_att}} - chops attendance data frame
#' \item{\code{select_att_mma}} - chops MMA attendance data frame
#' \item{\code{select_prov}} - chops service provision
#' }
#'
#' @importFrom dplyr select
#'
#' @param df a data frame
#' @param cols character string with column names to import.
#' @param ... Additional arguments passed to \code{\link[dplyr]{select}}.
#' @return A data frame with selected columns to import.
#' @examples
#' \dontrun{
#'  # select mpg form mtcars
#'  select_cols(mtcars, "mpg")
#'
#'  # load sheets df
#'  attendance_df <- read_sheet("./data/attendance.csv")
#'  attendance2_df <- read_sheet("./data/attendance2.csv")
#'  service_provision_df <- read_sheet("./data/service provision.csv")
#'
#'  # chop attendance
#'  select_att(attendance_df)
#'
#'  # chop attendance 2
#'  select_att_mma(attendance2_df)
#'
#'  # chop service provision df
#'  select_prov(service_provision_df)
#' }
#' @export
select_cols <- function(df = NULL, cols = NULL, ...){

  if (is.null(df)){
    stop("A data frame to chop is missing, please specify", call. = FALSE)
  }

  if (is.null(cols)){
    warning("no column names are specified, defaulting to NULL")
  }

  succeeded <- cols_exists(names(df), cols)

  if (any(succeeded)){

     cols <- names(df)[succeeded]

     dplyr::select(df, cols, ...) -> slashed
   }

  if (!any(succeeded)){
    stop("unknown columns specified: ", sQuote(cols))
  }

  slashed

}

#' verify all names exists
cols_exists <- function(x,y){
  x %in% y
}


#' Select columns to import (Attendance)
#' @rdname select_cols
select_att <- function(df = NULL, ...){

  cols <- c("State","Region","LGA","Ward","Facility","Type of Facility","Name","ID #", "Age","Date Calculation","Program Activity")

  select_cols(df, cols, ...)
}

#' Select columns to import (MMA Attendance)
#' @rdname select_cols
select_att_mma <- function(df = NULL, ...){

  cols <- c("State","Region","LGA","Ward","Facility","Girl ID","Age","Session 1 Date", "Program Activity","Attendance Frequency")

  select_cols(df, cols, ...)
}

#' Select columns to import (Service Provision)
#' @rdname select_cols
select_prov <- function(df = NULL, ...){
  cols <- c("State","Region","LGA","Ward","Facility","Type of Facility","ID #", "Age (Formula)","Date Calculation","Program Entry Point",
            "Current Method","Used EC/Condoms last sex","Pregnant?", "Method Received","Received Condoms as a dual Method")

  select_cols(df, cols, ...)
}










#'
#' #' Chop Attendance sheets
#' #'
#' #' \code{chop_att} transforms attendance sheets in preparation for the next stage.
#' #'
#' #' @param dt A list of data.tables or data.frames
#' #' @return transformed list
#' #' @export
#' chop_sp <- function(dt = NULL){
#'   if (is.null(dt)){
#'     stop("A list of data.tables or data.frames must be specified", call. = F)
#'   }
#'
#'   if (!is.null(dt)  & length(dt) > 1){
#'
#'     dt2 <- vector("list", length = length(dt))
#'
#'     for (i in seq_along(dt)){
#'
#'       dt2[[i]] <- select_prov(dt[[i]])
#'
#'       print(names(dt[i]))
#'     }
#'
#'   }
#' }
#'
#'
#'
#'
#' #' Select Colums to Transform
#' #'
#' #' \code{select_att} selects columns names and types to transform from the attendance sheets.
#' #'
#' #' It selects the following columns: State, Region, LGA, Ward, Facility, Type of Facility, Name, ID #,
#' #' Age, Date Calculation and Program Activity.
#' #'
#' #' @param dt A data.table or data,.frame
#' #' @param cols A vector of characters
#' #' @return A chopped data.table or data.frame
#' #' @export
#' select_att <- function(dt = NULL, cols = NULL){
#'
#'   if (is.null(dt)){
#'     stop("An attendance sheet must be specified", call. = FALSE)
#'   }
#'
#'   if (!is.null(dt) && is.null(cols)){
#'     cols <- c("State","Region","LGA","Ward","Facility","Type of Facility","Name","ID #", "Age","Date Calculation","Program Activity")
#'
#'     if (all(!cols %in% names(dt))){
#'       stop(sprintf("Columns are missing in %s sheet", dt))
#'     }
#'
#'     dplyr::select(dt,cols)
#'   }
#'
#'   if (!is.null(dt) && !is.null(cols)){
#'     cols <- cols
#'
#'     dplyr::select(dt, cols)
#'   }
#'
#' }
#'
#' #' Select Columns to Transform - MMA Attendance
#' #'
#' #' \code{select_att_mma} selects columns names and types to transform from the MMA attendance sheets
#' #'
#' #' It selects the following columns: State, Region, LGA, Ward, Facility, Girl ID, Age, Session 1 Date
#' #' and Attendance Frequency.
#' #'
#' #' @param dt A data.table or data.frame
#' #' @param cols A vector of characters
#' #' @return A chopped data.table or data.frame
#' #' @export
#' select_att_mma <- function(dt = NULL, cols = NULL){
#'
#'   if (is.null(dt)){
#'     stop("MMA attendance sheet must be specified", call. = FALSE)
#'   }
#'
#'   if (!is.null(dt) && is.null(cols)){
#'     cols <- c("State","Region","LGA","Ward","Facility","Girl ID","Age","Session 1 Date", "Program Activity","Attendance Frequency")
#'
#'     dplyr::select(dt, cols)
#'   }
#'
#'   if (!is.null(dt) && !is.null(cols)){
#'     cols <- cols
#'
#'     dplyr::select(dt, cols)
#'
#'   }
#'
#'
#' }
#'
#'
#'
#' #' Select Columns to Transform
#' #'
#' #' \code{select_prov} selects column names and types to transform from the service provison sheets.
#' #'
#' #' It selects the following columns: State, Region, LGA , Ward, Facility, Type of Facility, Date Calculation,
#' #' ID #, Age (Formula), Program Entry Point, Current Method,Used EC/Condoms last sex, Pregnant?, Method recieved
#' #' and Received Condoms as a dual Method`
#' #'
#' #' @param dt A data.table or data.frame
#' #' @param cols A vector of characters
#' #' @return a chopped data.table or data.frame
#' #' @export
#' select_prov <- function(dt = NULL, cols = NULL){
#'
#'   if (is.null(dt)){
#'     stop("Service Provision sheet must be specified", call. = FALSE)
#'   }
#'
#'   if (!is.null(dt) && is.null(cols)){
#'     cols <- c("State","Region","LGA","Ward","Facility","Type of Facility","ID #", "Age (Formula)","Date Calculation","Program Entry Point",
#'               "Current Method","Used EC/Condoms last sex","Pregnant?", "Method Received","Received Condoms as a dual Method")
#'     dplyr::select(dt,cols)
#'   }
#'
#'   if (!is.null(dt) && !is.null(cols)){
#'     cols <- cols
#'
#'     dplyr::select(dt,cols)
#'
#'   }
#'
#' }


