## TODO: implement helper class ----
# object to carry around code_in, code_out, and split_in as attributes
to_pm <- function(x, code_in, code_out, split_in){
  pm <- list()
  pm$x <- x
  pm$code_in <- code_in
  pm$code_out <- code_out
  pm$split_in <- split_in
}

## predicates ----
check_pm_split_sum <- function(x, code_in, split_in){
  x %>%
    dplyr::group_by(code_in) %>%
    dplyr::summarise(split_total = sum(split_in))
}

check_pm_NA <- function(pm){

}

check_pm_duplicates <- function(pm){

}

## errors ----
error_incomplete_split <- function(pm){

}
