#' Run mappings
#'
#' \code{run_mappings_sp} run mapping scripts on the service provision sheets.
#'
#' @param dt A list of data.tables or data.frames
#' @return transformed list
run_mappings_sp <- function(dt = NULL){

  if (is.null(dt)){
    stop("A list of data.tables or data.frames must be specified", call. = FALSE)
  }

  if (!is.null(dt)){
    dt2 <- lapply(dt, function(x) map_sp(x))

    names(dt2) <- names(dt)

    dt
  }
}




#' Map service provision sheet
#'
#' \code{map_sp} maps the service provision sheets with the existing options codes
#' provided in NG RH A360 - Provider Client Records program in DHIS2.
#'
#' @param df A data.table or data.frame
#' @return A mapped data.table or data.frame
map_sp <- function(df){

  #cols <- c("Method Received", "Program Entry Point", "Current Method", "Used EC/Condoms last sex","Pregnant?")

  df %>% dplyr::mutate(`Method Received` = map_method_recieved(.$`Method Received`),
                         `Program Entry Point` = map_program_entry_point(.$`Program Entry Point`),
                         `Current Method` = map_current_method(.$`Current Method`),
                         `Used EC/Condoms last sex` = map_used_ec_last_sex(.$`Used EC/Condoms last sex`),
                         `Pregnant?` =  map_preg_results(.$`Pregnant?`))

}


#' Map Attendance sheets
#'
#' \code{map_att} maps attendance sheets with the existing options codes in
#'NG RH A360 -Provider Client Records program in DHIS2.
#'
#'@param df A data.table or data.frame
#'@return A mapped data.table or data.frame
map_att <- function(dt = NULL){
  if (!is.null(dt)){

    options <- de_uid("Program type (9ja/MMA/Walk-in)") %>%
      de_optionset(.) %>%
      .$options

    dt %>% mutate(`Program Activity` = map_program_activity(.$`Program Activity`))

  }

}

map_att2 <- function(dt){

  if (!is.null(dt)){

    options <- de_uid("Program type (9ja/MMA/Walk-in)") %>%
      de_optionset(.) %>%
      .$options

    options2 <- de_uid("Program type (9ja/MMA/Walk-in)") %>%
      de_optionset(.) %>%
      .$options

    dt %>% mutate(`Program Activity` = map_program_activity(.$`Program Activity`),
                  `Attendance Frequency` = map_attendance_freq(.$`Attendance Frequency`))

  }
}

map_attendance_freq <- function(x){
  if (is.null(x)){
    stop("A vector of character or string must be specified in Program Activity", call. = F)
  }

  if (!is.null(x)){
    options <- de_uid("MMA Session") %>%
      de_optionset(.) %>%
      .$options

    plyr::mapvalues(x,
                    from = options$name,
                    to = options$code, warn_missing = F)
  }

}


#'Map Program Activity
#'
#'\code{map_program_entry_point} maps program activities with the option codes provided in
#'NG RH A360 - Provider Client Records program in DHIS2.
#'
#'@param x A vector of character or string
#'@return Existing option codes in DHIS2
map_program_activity <- function(x = NULL){
  if (is.null(x)){
    stop("A vector of character or string must be specified in Program Activity", call. = F)
  }

  if (!is.null(x)){
    options <- de_uid("Program type (9ja/MMA/Walk-in)") %>%
      de_optionset(.) %>%
      .$options

    plyr::mapvalues(x,
                    from = options$name,
                    to = options$code, warn_missing = F)
  }
}



#'Map Method Recieved
#'
#'\code{map_method_recieved} maps method recieved option with the option codes provided in
#'NG RH A360 - Provider Client Records program in DHIS2.
#'
#'@param x A vector of character or string
#'@return Existing option codes in DHIS2
map_method_recieved <- function(x = NULL){
  if (is.null(x)){
    stop("A vector of character or string must be specified in Method Recieved")
  }

  if (!is.null(x)){
    options <- de_optionset(de_uid("Method taken up (old program)"))$options
    plyr::mapvalues(x,
                    from = options$name,
                    to = options$code, warn_missing = F)
  }
}

#'Map Program Entry Point
#'
#'\code{map_program_entry_point} maps program entry points with the option codes provided in
#'NG RH A360 - Provider Client Records program in DHIS2.
#'
#'@param x A vector of character or string
#'@return Existing option codes in DHIS2
map_program_entry_point <- function(x = NULL){
  if (is.null(x)){
    stop("A vector of character or string must be specified in Program Entry Point")
  }

  if (!is.null(x)){
    options <- de_optionset(de_uid("Program attended by the girl"))$options
    plyr::mapvalues(x,
                    from = options$name,
                    to = options$code, warn_missing = F)
  }
}


#'Map Current Method
#'
#'\code{map_current_method} maps current methods with the options codes provided in
#'NG RH A360 - Provider Client Records program in DHIS2.
#'
#'@param x A vector of character or string
#'@return Existing option codes in DHIS2
map_current_method <- function(x = NULL){
  if (is.null(x)){
    stop("A vector of character or string must be specified in Current Method")
  }

  if (!is.null(x)){
    options <- de_optionset(de_uid("Current method"))$options
    plyr::mapvalues(x,
                    from = options$name,
                    to = options$code, warn_missing = F)
  }
}

#' Map used EC/Condom last sex
#'
#' \code{map_used_ec_last_sex} maps used EC/Condom last sex with the options codes provided in
#' NG RH A360 - Provider Client Records program in DHIS2.
#'
#' @param x A vector of character or string
#' @return Existing option codes in DHIS2
map_used_ec_last_sex <- function(x = NULL){
  if (is.null(x)){
    stop("A vector of character or string must be specified in Used EC/Condom last sex")
  }

  if (!is.null(x)){
    options <- de_optionset(de_uid("EC or condoms used at last sex - Old Program"))$options
    plyr::mapvalues(x,
                    from = options$name,
                    to = options$code, warn_missing = F)
  }
}

#' Map pregenancy results
#'
#' \code{map_preg_results} maps pregnancy results with the option codes provided in
#' NG RH A360 - Provider Client Records program in DHIS2.
#'
#' @param x A vector of character or string
#' @return Existing option codes in DHIS2
map_preg_results <- function(x = NULL){

  if (is.null(x)){
    stop("A vector of character or string must be specified in Pregnant?")
  }

  if (!is.null(x)){
    options <- de_optionset(de_uid("Pregnancy status"))$options
    plyr::mapvalues(x,
                    from = options$name,
                    to = options$code, warn_missing = F)
  }

}



