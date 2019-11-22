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
load_files <- function(path = NULL, sheet, guess_max = 30000, skip = 1){

  if(!is.null(path)){

    files <- vector("list", length(list.files(path, pattern = ".xlsx")))

    file_names <- list.files(path, pattern = ".xlsx")

    for (i in seq_along(files)) {

      files[[i]] <- readxl::read_excel(file.path(path,file_names[i]), sheet = sheet, guess_max = guess_max, skip = skip)
    }

  }else{
    stop("System path to the directory with files must be specified", call. = FALSE)
  }

}


#'Covert excel dates to R dates
#'
#'\code{to_r_date} converts xlsx date number to R date
#'
#'@param x Vector with xlsx dates numbers
#'@param origin Origin date 1900-01-01
#'@return R date type object
to_r_date <- function(x, origin = "1900-01-01", ...){

  if (!is.na(x)){
    if(nchar(x) < 7){
      as.Date(as.numeric(x), origin = origin, ...)
    }else{
      as.Date(x, format = "%d/%m/%Y")
    }
  }
}

#/Users/isaiahnyabuto/Documents/Workspace/A360/NG data upload/legacy files/NG A360 Master Data- Final Cleaned April & May 19
