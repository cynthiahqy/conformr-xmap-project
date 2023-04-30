# Generated from create-xmap.Rmd: do not edit by hand

#' Verify crossmap properties of named vectors or lists
#'
#' @param x a Named vector or list. Lists values are flattened via `unlist()`.
#'
#' @return `x` or throws an error
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
#' 
verify_named_all_1to1 <- function(x){
  stopifnot(is.vector(x))
  unique_names <- unique(names(x))
  unique_values <- unique(unlist(unname(x)))
  stop <- !(length(unique_names) == length(unique_values))
  if (stop){
    cli::cli_abort("Not all relations in `x` are 1-to-1.",
                   class = "abort_not_1to1")
  }
  invisible(x)
}

#' @describeIn verify_named Verify name-value pairs of named vector or list are not duplicated
#' @export
verify_named_all_unique <- function(x){
  stopifnot(is.vector(x))
  pairs <- as_pairs_from_named(x)
  dup_idx <- anyDuplicated(pairs)
  stop <- as.logical(dup_idx)
  if (stop){
    cli::cli_abort(c(
          "Duplicated pairs found in `x`.",
    "i" = "Use `as_pairs_from_named(x) |> base::duplicated()` to identify duplicates."),
    class = "abort_not_unique"
    )
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
    cli::cli_abort(c(
    "Duplicated names found in `x`.",
    "i" = "Use `base::duplicated(names(x))` to identify duplicates."),
    class = "abort_not_unique"
    )
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
    cli::cli_abort(c(
    "Duplicated values found in `x`.",
    "i" = "Use `base::duplicated(unlist(unname(x)))` to identify duplicates."),
    class = "abort_not_unique"
    )
  }
  #stopifnot(unlist(unname(x)) == unique(unlist(unname(student_groups))))
  invisible(x)
}

#' @describeIn abort Abort message for verify_named_matchset_* functions
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
#' @name verify_named_matchset
#' @inheritParams verify_named
#' @param ref_set a vector of character strings 
#' 
#' @return `x` or throw an error
#' @examples
#' fruit_color <- c(apple = "green", strawberry = "red", banana = "yellow")
#' fruit_set <- c("apple", "strawberry", "banana", "pear")
#' fruit_color |> 
#'   verify_named_matchset_names_within(ref_set = fruit_set)
NULL

#' @describeIn verify_named_matchset Names of `x` **exactly** match `ref_set`
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

#' @describeIn verify_named_matchset Values of `x` **exactly** match `ref_set`
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

#' @describeIn verify_named_matchset Names of `x` **contain** all of `ref_set`
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

#' @describeIn verify_named_matchset Values of `x` **contain** all of `ref_set`
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

#' @describeIn verify_named_matchset Names of `x` are all **within** `ref_set`
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

#' @describeIn verify_named_matchset Values of `x` are all **within** `ref_set`
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

#' @describeIn verify_named Alias of `verify_named_all_1to1()`
#' @export
verify_named_as_recode_unique <- verify_named_all_1to1

#' @describeIn verify_named Alias of `verify_named_all_values_unique()`
#' @export
verify_named_no_dup_values <- verify_named_all_values_unique

#' @describeIn verify_named Alias of `verify_named_all_names_unique()`
#' @export
verify_named_no_dup_names <- verify_named_all_names_unique
