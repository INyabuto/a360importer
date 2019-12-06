#' Run validation
#'
#' \code{run_validation_sp} runs validation scripts in the service provision sheets.
#'
#' @param dt A list of data.tables or data.frames
#' @return A validated list
run_validation_sp <- function(dt = NULL){

  if (is.null(dt)){
    stop("A list of data.tables or data.frames must be specified", call. = F)
  }

  if (!is.null(dt)){
    dt2 <- lapply(dt, function(x) validate_sp(x))

    names(d2) <- names(dt)

    dt2
  }

}


validate_sp <- function(df){

  df$`Method Received` <- sapply(df$`Method Received`, function(y) validate_method_recieved(y))
  df$`Program Entry Point` <- sapply(df$`Program Entry Point`, function(y) validate_program_entry_point(y))
  df$`Current Method` <- sapply(df$`Current Method`, function(y) validate_current_method(y))
  df$`Used EC/Condoms last sex` <- sapply(df$`Used EC/Condoms last sex`, function(y) validate_ec_last_sex(y))
  df$`Pregnant?` <- sapply(df$`Pregnant?`, function(y) validate_preg_results(y))
  df$`Received Condoms as a dual Method` <- sapply(df$`Received Condoms as a dual Method`, function(y) validate_condom_as_dual(y))

  df
}



#' Validate method recieved
#'
#' \code{validate_method_recieved} validates the different method options listed in the service provision sheets
#' against those provided in NG RH A360 - Provider Client Record program.
#'
#' @param x A vector of character or string
#' @return An existing method in DHIS2
validate_method_recieved <- function(x = NULL){

  # get id of method recieved and options
  # uid <- de_uid("Method taken up (old program)")
  # options <- de_optionset(uid)$options

  if (!is.null(x) & x != ""){

    switch(str_trim(x),
           "0: No Method" = "None (Counselling only)",
           "1: IUCD" = "IUCD - Copper-T",
           "2a: Implant - Jadelle" = "Implant: Jadelle",
           "2b: Implant - Implanon" = "Implant: Implanon",
           "3a: Injection - Norigynon" = "Injectable: Norigynon",
           "3b: Injection - Noristerat" = "Injectable: Noristerat",
           "3c: Injection - Depo Provera" = "Injectable: Depo-Provera",
           "3C: Injection - Depo Provera" = "Injectable: Depo-Provera",
           "3d: Injection - Sayana Press" = "Injectable: Sayana Press",
           "3d: Injection - Sayana press" = "Injectable: Sayana Press",
           "4a: Pills - Microgynon" = "Pills - Microgynon",
           "4b: Pills - Combination 3" = "Pills - Combination3",
           "4c: Pills - Exluton" = "Pills - Excluton",
           "6a: Condom - Male" = "Condoms - Male",
           "6b: Condom - Female" = "Condoms - Female",
           "7: Emergency pill" = "Emergency Pill",
           stop(sprintf("Unkown input [%s] in column: Method Recieved", x), call. = FALSE)
    )

  }

}


#' Validate MMA attendance freq.
#'
#' \code{validate_attendance_freq} validates the different frequencies provided in MMA attendance sheets
#' against the current set up in NG RH A360 - 9ja and MMA events program.
#'
#' @param x A vector of character or string
#' @return An existing frequency in DHIS2
validate_attendance_freq <- function(x = NULL){

  if (!is.null(x) & x != ""){
    switch(str_trim(x),
           `1` = "Session 1",
           `2` = "Session 2",
           `3` = "Session 3",
           `4` = "Session 4",
           stop(sprintf("Unknown input in [%s] sheet in column: Attendance_freq",x), call. = FALSE)
    )
  }

}

#' Validate Program Entry Point
#'
#' \code{validate_program_entry_point} validates the different program entry points provided in the service provision sheets
#' against those listed in NG RH A360 - Provider Client Record program.
#'
#' @param x A vector of character or string
#' @return An existing program entry point in DHIS2
validate_program_entry_point <- function(x = NULL){

  # get the de_uid and options
  # uid <- de_uid("Program attended by the girl")
  # options <- de_optionset(uid)$options

  if (!is.null(x) & x != ""){

    switch(str_trim(x),
           "Walk-in 9JA Girls Hub" = "Walk-in 9JA Girls Hub",
           "LLH Class (9JA Girls)" = "LLH Class (9JA Girls)",
           "LLH Reach Out (9JA Girls)" = "LLH Reach Out (9JA Girls)",
           "Walk-in 9JA Girls Spoke" = "Walk-in 9JA Girls Spoke",
           "Walk-in MMA Hub" = "Walk-in MMA Hub",
           "LFH Class (MMA)" = "LFH Class (MMA)",
           "LFH Reach Out (MMA)" = "LFH Reach Out (MMA)",
           "Walk-in MMA Spoke" = "Walk-in MMA Spoke",
           stop(sprintf("Unknown input [%s] in column: Program Entry Point", x), call. = FALSE))
  }


}


#' Validate current method
#'
#' \code{validate_current_method} validates different current methods listed in the service provision sheets
#' against those provided in NG RH A360 - Provider Client Records program in DHIS2.
#'
#' @param x A vector of character string
#' @return An existing method in DHIS2
validate_current_method <- function(x = NULL){

  # get de uid and options
  # uid <- de_uid("Current method")
  # options <- de_optionset(uid)$options

  if (!is.null(x) & x != ""){
    switch(str_trim(x),
           "0: No Method" = "None",
           "1: IUCD" = "IUCD",
           "2: Implant" = "Implant",
           "3: Injection" = "Injectable",
           "4: Pills" = "Pills",
           "5: Condoms" = "Condom",
           "1: IUD" = "IUCD",
           "3d: Injection - Sayana Press" = "Injectable",
           "3c: Injection - Depo Provera" = "Injectable",
           stop(sprintf("Unknown input [%s] in column: Current Method", x), call. = FALSE)
    )
  }
}


#' Validate used EC/Condom last sex
#'
#'\code{validate_ec_last_sex} validates the different options listed on service provision sheets against those provided
#'in NG RH A360 - Provider Client Records program in DHIS2.
#'
#'@param x A vector of character string
#'@return An existing option in DHIS2
validate_ec_last_sex <- function(x = NULL){
 # get de_uid and options
  #uid <- de_uid("EC or condoms used at last sex - Old Program")
  #options <- de_optionset(uid)$options

  if (!is.null(x) & x != ""){
    switch(str_trim(x),
           "Neither" = "Neither",
           "Condom" = "Condoms",
           "Condoms" = "Condoms",
           "Both" = "Both",
           "EC" = "EC",
           stop(sprintf("Unknown input [%s] in column: Used Used EC/Condoms last sex", x), call. = FALSE))
  }
}

#' Validate pregancy results
#'
#' \code{validate_preg_results} validates the pregnacy results in the service provision sheets against those provided
#' in the NG RH A360 - Provider Client Records program in DHIS2.
#'
#' @param x A vector of character string
#' @return An existing option in DHIS2
validate_preg_results <- function(x = NULL){

  # uid <- de_uid("Pregnancy status")
  # options <- de_optionset(uid)$options

  if (!is.null(x) & x != ""){
    switch(str_trim(x),
           "Not Pregnant" = "Not pregnant",
           "Pregnant" = "Pregnant",
           "Unknown" = "Unknown",
           stop(sprintf("Unknown input [%s] in column: Pregnant?", x), call. = FALSE))
  }
}

#' Validate condom as dual method
#'
#' \code{validate_condom_as_dual}  Validates the different options listed in the service provision sheets
#' against the options in NG RH A360 - Provider Client Records program in DHIS2.
#'
#' @param x A vector of character string
#' @return An existing option in DHIS2
validate_condom_as_dual <- function(x = NULL){

  if (!is.null(x) & x != ""){
    switch(str_trim(x),
           "No" = "false",
           "Yes" = "true",
           stop(sprintf("Unknown input [%s] in column: Received Condoms as a dual Method", x), call. = FALSE))
  }
}
