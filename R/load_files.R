#'
#'
#'Load A360 NG legacy files into R
#'
#'\code{load_files} loads A360 legacy files in the specified directory into R
#'
#'@param path System path to the directory with data
#'@return a list of data frames
#'@examples
#'\dontrun{
#'load_files(file.path(Sys.getenv("HOME"),"data")
#'}
load_files <- function(path = NULL){

  if (!is.null(path)){

    files <- vector("list", length(list.files(path, pattern = ".csv")))

    # stop if path is empty
    if (length(files) == 0){
      stop("Ensure you are in the correct path or the directory has a csv file")
    }

    file_names <- list.files(path, pattern = ".csv")


    for (i in seq_along(files)) {

      files[[i]] <- data.table::fread(file.path(path, file_names[i]))

    }

  }else{
    stop("System path to the directory with files must be specified", call. = FALSE)
  }


  names(files) <- file_names

  files

}


#'Load A360 NG legacy files into R
#'
#'\code{load_files} loads A360 legacy files in the specified directory into R
#'
#'@param path System path to the directory with data
#'@return a list of data frames
#'@examples
#'\dontrun{
#'load_files(file.path(Sys.getenv("HOME"),"data")
#'}
load_files_sp <- function(path = NULL){

  if (!is.null(path)){

    files <- vector("list", length(list.files(path, pattern = "Provision.csv")))

    # stop if path is empty
    if (length(files) == 0){
      stop("Ensure you are in the correct path or the directory has a csv file")
    }

    file_names <- list.files(path, pattern = "Provision.csv")


    for (i in seq_along(files)) {

      files[[i]] <- data.table::fread(file.path(path, file_names[i]))

    }

  }else{
    stop("System path to the directory with files must be specified", call. = FALSE)
  }


  names(files) <- file_names

  files

}


#'Load A360 NG legacy files into R
#'
#'\code{load_files} loads A360 legacy files in the specified directory into R
#'
#'@param path System path to the directory with data
#'@return a list of data frames
#'@examples
#'\dontrun{
#'load_files(file.path(Sys.getenv("HOME"),"data")
#'}
load_files_att <- function(path = NULL){

  if (!is.null(path)){

    files <- vector("list", length(list.files(path, pattern = "Attendance.csv")))

    # stop if path is empty
    if (length(files) == 0){
      stop("Ensure you are in the correct path or the directory has a csv file")
    }

    file_names <- list.files(path, pattern = "Attendance.csv")


    for (i in seq_along(files)) {

      files[[i]] <- data.table::fread(file.path(path, file_names[i]))

    }

  }else{
    stop("System path to the directory with files must be specified", call. = FALSE)
  }


  names(files) <- file_names

  files

}


#'Load A360 NG legacy files into R
#'
#'\code{load_files} loads A360 legacy files in the specified directory into R
#'
#'@param path System path to the directory with data
#'@return a list of data frames
#'@examples
#'\dontrun{
#'load_files(file.path(Sys.getenv("HOME"),"data")
#'}
load_files_att2 <- function(path = NULL){

  if (!is.null(path)){

    files <- vector("list", length(list.files(path, pattern = "Attendance2.csv")))

    # stop if path is empty
    if (length(files) == 0){
      stop("Ensure you are in the correct path or the directory has a csv file")
    }

    file_names <- list.files(path, pattern = "Attendance2.csv")


    for (i in seq_along(files)) {

      files[[i]] <- data.table::fread(file.path(path, file_names[i]))

    }

  }else{
    stop("System path to the directory with files must be specified", call. = FALSE)
  }


  names(files) <- file_names

  files

}



#'Covert excel dates to R dates
#'
#'\code{to_r_date} converts xlsx date number to R date
#'
#'@param x Vector with xlsx dates numbers
#'@param origin Origin date 1900-01-01
#'@return R date type object
to_r_date <- function(x, origin = "1900-01-01", ...){

  if (!is.na(x) && nchar(x) < 7){
    as.Date(as.numeric(x), origin = origin, ...)
  }

  if (!is.na(x) &&  grepl("/", x)){
      as.Date(x, format = "%d/%m/%Y")
  }

  if (!is.na(x) && grepl("-", x)){
      as.Date(x, format = "%d-%b-%y")

  }
}



#/Users/isaiahnyabuto/Documents/Workspace/A360/NG data upload/legacy files/NG A360 Master Data- Final Cleaned April & May 19
#/Users/isaiahnyabuto/Downloads/NG A360 Master Data- Final Cleaned April & May 19
#/Users/isaiahnyabuto/Downloads/NG A360 Master Data- Final Cleaned June 19
#/Users/isaiahnyabuto/Documents/Workspace/A360/NG data upload/docs/historical files/NG A360 Master Data- Final Cleaned April & May 19
