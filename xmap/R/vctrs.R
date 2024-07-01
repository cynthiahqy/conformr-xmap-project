# Generated from create-xmap.Rmd: do not edit by hand

# --- xmap_percent -----------------------------------------------------------

## constructor
new_percent <- function(x = double()) {
  if (!is_double(x)) {
    abort("`x` must be a double vector.")
  }
  vctrs::new_vctr(x, class = "xmap_percent")
}

## helpers
percent <- function(x = double()) {
  x <- vctrs::vec_cast(x, double())
  new_percent(x)
}

is_percent <- function(x) {
  inherits(x, "xmap_percent")
}

## printing
format.xmap_percent <- function(x, ...) {
  out <- formatC(signif(vctrs::vec_data(x) * 100, 3))
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], "%")
  out
}

vec_ptype_abbr.xmap_percent <- function(x, ...) {
  "pct"
}

## casting and coercion
vec_ptype2.xmap_percent.double <- function(x, y, ...) double()
vec_ptype2.double.xmap_percent <- function(x, y, ...) double()

vec_cast.xmap_percent.xmap_percent <- function(x, to, ...) x
vec_cast.xmap_percent.double <- function(x, to, ...) percent(x)
vec_cast.double.xmap_percent <- function(x, to, ...) vec_data(x)
