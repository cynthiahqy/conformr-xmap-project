# Generated from create-xmap.Rmd: do not edit by hand

#' Verify crossmap properties of named vectors
#'
#' @param x a named vector
#' @param ref_names,ref_values a vector of character strings 
#'
#' @return `x` or throw error
#' @name verify_named
#' @examples
#' ## check each fruit has a unique color
#' fruit_color <- c(apple = "green", strawberry = "red", banana = "yellow")
#' verify_named_all_1to1(fruit_color)
#' 
#' ## check no student is assigned to multiple groups
#' student_groups <- list(GRP1 = c("kate", "jane", "peter"),
#'                       GRP2 = c("terry", "ben", "grace"),
#'                       GRP3 = c("cindy", "lucy", "alex" ))
#' verify_named_no_dup_names(student_groups)
#' 
#' ## check 
NULL

#' @describeIn verify_named Verify named vector or list has only one-to-one relations
#' @export
#' @examples
#' 
verify_named_all_pairs_1to1 <- function(x){
  stopifnot(is.vector(x))
  unique_names <- unique(names(x))
  unique_values <- unique(unlist(unname(x)))
  stopifnot(length(unique_names) == length(unique_values))
  invisible(x)
}

#' @describeIn verify_named Verify name-value pairs of named vector or list are not duplicated
#' @export
verify_named_all_pairs_unique <- function(x){
  stopifnot(is.vector(x))
  pairs <- as_pairs_from_named(x)
  stopifnot(!as.logical(anyDuplicated(pairs)))
  invisible(x)
}

#' @describeIn verify_named Verify names of named vector or list are not duplicated
#' @export
verify_named_all_names_unique <- function(x){
  stopifnot(is.vector(x))
  dup_idx <- anyDuplicated(names(x))
  stopifnot(!as.logical(dup_idx))
  invisible(x)
}

#' @describeIn verify_named Verify values in named vector or list are not duplicated (after unnesting)
#' @export
verify_named_all_values_unique <- function(x){
  stopifnot(is.vector(x))
  dup_idx <- anyDuplicated(unlist(unname(x)))
  stopifnot(!as.logical(dup_idx))
  #stopifnot(unlist(unname(x)) == unique(unlist(unname(student_groups))))
  invisible(x)
}

#' @describeIn verify_named Verify unique names of named vector or list **exactly** match an expected set of name values
#' @export
verify_named_matchset_names_exact <- function(x, ref_set){
  stopifnot(is.vector(x))
  unique_names <- unique(names(x))
  stopifnot(setequal(ref_set, unique_names))
  invisible(x)
}

#' @describeIn verify_named Verify unique values of named vector or list **exactly** match an expected set of name values
#' @export
verify_named_matchset_values_exact <- function(x, ref_set){
  stopifnot(is.vector(x))
  unique_values <- unique(unlist(unname(x)))
  stopifnot(setequal(ref_set, unique_values))
  invisible(x)
}

#' @describeIn verify_named Verify unique names of named vector or list **contain** an expected set of name values
#' @export
verify_named_matchset_names_contain <- function(x, ref_set){
  stopifnot(is.vector(x))
  unique_names <- unique(names(x))
  stopifnot(ref_set %in% unique_names)
  invisible(x)
}

#' @describeIn verify_named Verify unique names of named vector or list **contain** an expected set of name values
#' @export
verify_named_matchset_values_contain <- function(x, ref_set){
  stopifnot(is.vector(x))
  unique_values <- unique(unlist(unname(x)))
  stopifnot(ref_set %in% unique_values)
  invisible(x)
}

#' @describeIn verify_named Verify unique names of named vector or list are **within** an expected set of name values
#' @export
verify_named_matchset_names_within <- function(x, ref_set){
  stopifnot(is.vector(x))
  unique_x <- unique(names(x))
  stopifnot(unique_x %in% ref_set)
  invisible(x)
}

#' @describeIn verify_named Verify unique names of named vector or list are **within** an expected set of name values
#' @export
verify_named_matchset_values_within <- function(x, ref_set){
  stopifnot(is.vector(x))
  unique_x <- unique(unlist(unname(x)))
  stopifnot(unique_x %in% ref_set)
  invisible(x)
}

#' @describeIn verify_named (alias) verify named vector or list has only one-to-one relations
#' @export
verify_named_as_recode_unique <- verify_named_all_pairs_1to1

#' @describeIn verify_named (alias)
#' @export
verify_named_no_dup_values <- verify_named_all_values_unique

#' @describeIn verify_named (alias)
#' @export
verify_named_no_dup_names <- verify_named_all_names_unique
