## code to prepare `settings` dataset goes here

usethis::use_data("settings")

meta <- read.csv("./inst/extdata/meta.csv", stringsAsFactors = F)
usethis::use_data(meta, internal = TRUE)

usethis::use_data_raw(name = "settings", open = interactive())

