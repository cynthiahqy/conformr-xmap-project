# Generated from create-xmap.Rmd: do not edit by hand

#' Verify crossmap properties of named vectors or lists
#'
#' @param x a named vector
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
verify_named_all_1to1 <- function(x){
  stopifnot(is.vector(x))
  unique_names <- unique(names(x))
  unique_values <- unique(unlist(unname(x)))
  stop <- !(length(unique_names) == length(unique_values))
  if (stop){
    cli::cli_abort("Not all relations in `x` are 1-to-1.")
  }
  invisible(x)
}

#' @describeIn verify_named Verify name-value pairs of named vector or list are not duplicated
#' @export
verify_named_all_unique <- function(x){
  stopifnot(is.vector(x))
  pairs <- as_pairs_from_named(x)
  stop <- as.logical(dup_idx)
  if (stop){
    cli::cli_abort("Duplicated pairs found in `x`.
                   Use `as_pairs_from_named(x)` with `base::duplicated()` to identify duplicates.")
  }  
  invisible(x)
}

#' @describeIn verify_named Verify names of named vector or list are not duplicated
#' @export
verify_named_all_names_unique <- function(x){
  stopifnot(is.vector(x))
  dup_idx <- anyDuplicated(names(x))
  stop <- as.logical(dup_idx)
  if (stop){
    cli::cli_abort("Duplicated names found in `x`.
                   Use `base::duplicated(names(x))` to identify duplicates.")
  }
  invisible(x)
}

#' @describeIn verify_named Verify values in named vector or list are not duplicated (after unnesting)
#' @export
verify_named_all_values_unique <- function(x){
  stopifnot(is.vector(x))
  dup_idx <- anyDuplicated(unlist(unname(x)))
  stop <- as.logical(dup_idx)
  if (stop){
    cli::cli_abort("Duplicated values found in `x`.
                   Use `base::duplicated(unlist(unname(x)))` to identify duplicates.")
  }
  #stopifnot(unlist(unname(x)) == unique(unlist(unname(student_groups))))
  invisible(x)
}

#' @describleIn abort Abort message for verify_named_matchset_* functions
#' @export
msg_abort_named_matchset <- function(set_type = c("names", "values"),
                                match_type = c("exact", "within", "contain")){
  match_text <- switch(match_type,
         exact = "do not exactly match",
         within = "are not all within",
         contain = "do not contain all elements of")
  
  cli::format_error("The {set_type} of {.var x} {match_text} {.var ref_set}")
}

#' Verify unique names or values of named vector or list match expected set
#' 
#' @inheritParams verify_named
#' @param ref_set a vector of character strings 
#' @rdname verify_named_matchset
#' 
#' @return `x` or throw an Error
#' @examples
#' fruit_color <- c(apple = "green", strawberry = "red", banana = "yellow")
#' fruit_set <- c("apple", "strawberry", "banana", "pear")
#' fruit_color |> 
#'   verify_named_matchset_names_within(ref_set = fruit_set)
NULL

#' @describeIn verify_named_matchset Verify unique names of named vector or list **exactly** match an expected set of name values
#' @export
verify_named_matchset_names_exact <- function(x, ref_set){
  stopifnot(is.vector(x))
  unique_names <- unique(names(x))
  stop <- !setequal(ref_set, unique_names)
  if (stop) {
    cli::cli_abort(msg_abort_named_matchset("names", "exact"),
                   class = "abort_matchset")
  }
  invisible(x)
}

#' @describeIn verify_named_matchset Verify unique values of named vector or list **exactly** match an expected set of name values
#' @export
verify_named_matchset_values_exact <- function(x, ref_set){
  stopifnot(is.vector(x))
  unique_values <- unique(unlist(unname(x)))
  stop <- !setequal(ref_set, unique_values)
  if (stop) {
    cli::cli_abort(msg_abort_named_matchset("values", "exact"),
                   class = "abort_matchset")
  }
  invisible(x)
}

#' @describeIn verify_named_matchset Verify unique names of named vector or list **contain** an expected set of name values
#' @export
verify_named_matchset_names_contain <- function(x, ref_set){
  stopifnot(is.vector(x))
  unique_names <- unique(names(x))
  stop <- !all(ref_set %in% unique_names)
  if (stop){
    cli::cli_abort(msg_abort_named_matchset("names", "contain"),
                   class = "abort_matchset")
  }
  invisible(x)
}

#' @describeIn verify_named_matchset Verify unique names of named vector or list **contain** an expected set of name values
#' @export
verify_named_matchset_values_contain <- function(x, ref_set){
  stopifnot(is.vector(x))
  unique_values <- unique(unlist(unname(x)))
  stop <- !all(ref_set %in% unique_values)
  if (stop){
    cli::cli_abort(msg_abort_named_matchset("values", "contain"),
                   class = "abort_matchset")
  }
  invisible(x)
}

#' @describeIn verify_named_matchset Verify unique names of named vector or list are **within** an expected set of name values
#' @export
verify_named_matchset_names_within <- function(x, ref_set){
  stopifnot(is.vector(x))
  unique_x <- unique(names(x))
  stop <- !all(unique_x %in% ref_set)
  if (stop){
    cli::cli_abort(msg_abort_named_matchset("names", "within"),
                   class = "abort_matchset")
  }
  invisible(x)
}

#' @describeIn verify_named_matchset Verify unique names of named vector or list are **within** an expected set of name values
#' @export
verify_named_matchset_values_within <- function(x, ref_set){
  stopifnot(is.vector(x))
  unique_x <- unique(unlist(unname(x)))
  stop <- !all(unique_x %in% ref_set)
  if (stop){
    cli::cli_abort(msg_abort_named_matchset("values", "within"),
                   class = "abort_matchset")
  }
  invisible(x)
}

#' @describeIn verify_named (alias) verify named vector or list has only one-to-one relations
#' @export
verify_named_as_recode_unique <- verify_named_all_1to1

#' @describeIn verify_named (alias)
#' @export
verify_named_no_dup_values <- verify_named_all_values_unique

#' @describeIn verify_named (alias)
#' @export
verify_named_no_dup_names <- verify_named_all_names_unique
