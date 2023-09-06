# Generated from create-xmap.Rmd: do not edit by hand

new_percent <- function(x = double()) {
  if (!is_double(x)) {
    abort("`x` must be a double vector.")
  }
  vctrs::new_vctr(x, class = "xmap_percent")
}

percent <- function(x = double()) {
  x <- vctrs::vec_cast(x, double())
  new_percent(x)
}

is_percent <- function(x) {
  inherits(x, "xmap_percent")
}

format.xmap_percent <- function(x, ...) {
  out <- formatC(signif(vctrs::vec_data(x) * 100, 3))
  out[is.na(x)] <- NA
  out[!is.na(x)] <- paste0(out[!is.na(x)], "%")
  out
}

vec_ptype_abbr.xmap_percent <- function(x, ...) {
  "pct"
}
