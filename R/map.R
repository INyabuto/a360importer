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

  if (!is.null(x)){
    options <- de_optionset(de_uid("Method taken up (old program)"))$options
    plyr::mapvalues(x,
                    from = option$name,
                    to = options$code, warn_missing = F)
  }
}

#'Map Program Entry Point
#'
#'\code{map_program_entry_point} maps program entry points with the option codes provided in
#'NG RH A360 - Provider Client Record program in DHIS2.
#'
#'@param x A vector of character or string
#'@return Existing option codes in DHIS2
map_program_entry_point <- function(x = NULL){
  if (is.null(x)){
    stop("A vector of character or string must be specified", call. = FALSE)
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
#'NG RH A360 - Provider Client Record in DHIS2.
#'
#'@param x A vector of character or string
#'@return Existing option codes in DHIS2
map_current_method <- function(x = NULL){
  if (is.null(x)){
    stop("A vector of character or string must be specified", call. = FALSE)
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
#' NG RH A360 - Provider Client Record in DHIS2.
#'
#' @param x A vector of character or string
#' @return Existing option codes in DHIS2
map_used_ec_last_sex <- function(x = NULL){
  if (is.null(x)){
    stop("A vector of character or string must be specified", call. = FALSE)
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
#' NG RH A360 - Provider Clinet Record in DHIS2.
#'
#' @param x A vector of character or string
#' @retun Existing option codes in DHIS2
map_preg_results <- function(x = NULL){

}
