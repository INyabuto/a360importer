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
#' \code{map_sp} maps the service provision sheets with the existing options
#' provided in NG RH A360 - Provider Client Records program in DHIS2.
#'
#' @param df A data.table or data.frame
#' @return A mapped data table or data.frame
map_sp <- function(df = NULL){

  cols <- c("Method Received", "Program Entry Point", "Current Method", "Used EC/Condoms last sex","Pregnant?")

  if (!is.null(df) & cols %in% names(df)){
    df$`Method Received` <- sapply(df$`Method Received`, function(x) map_method_recieved(x))
    df$`Program Entry Point` <- sapply(df$`Program Entry Point`, function(x) map_program_entry_point(x))
    df$`Current Method` <- sapply(df$`Current Method`, function(x) map_current_method(x))
    df$`Used EC/Condoms last sex` <- sapply(df$`Used EC/Condoms last sex`, function(x) map_used_ec_last_sex(x))
    df$`Pregnant?` <- sapply(df$`Pregnant?`, function(x) map_preg_results(x))
  }

  if (!is.null(df) & !cols %in% names(df)){
    stop(sprintf("Missing columns dectected!\n columns [%s]", cat(!cols %in% names(df))),
         call. = FALSE)
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
    stop("A vector of character or string must be specified", call. = FALSE)
  }

  if (!is.null(x) & x != ""){
    options <- de_optionset(de_uid("Method taken up (old program)"))$options
    plyr::mapvalues(x,
                    from = option$name,
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
    stop("A vector of character or string must be specified", call. = FALSE)
  }

  if (!is.null(x) & x != ""){
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
    stop("A vector of character or string must be specified", call. = FALSE)
  }

  if (!is.null(x) & x != ""){
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
    stop("A vector of character or string must be specified", call. = FALSE)
  }

  if (!is.null(x) & x != ""){
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
    stop("A vector of character or string must be specified", call. = FALSE)
  }

  if (!is.null(x) & x != ""){
    options <- de_optionset(de_uid("Pregnancy status"))$options
    plyr::mapvalues(x,
                    from = options$name,
                    to = options$code, warn_missing = F)
  }

}

#' #' Map condom as dual method
#' #'
#' #' \code{map_condom_as_dual} maps condom as a dual option with the DHIS2
#' #' NG RH A360 - Providr Client Record program.
#' #'
#' #' @param x A vector of characteer or string
#' #' @return Existing codes in DHIS2
#' map_condom_as_dual <- function(x = NULL){
#'   if (is.null(x)){
#'     stop("A vector of character or string must be specified", call. = FALSE)
#'   }
#'
#'   if (!is.null(x) & x != ""){
#'
#'   }
#' }
