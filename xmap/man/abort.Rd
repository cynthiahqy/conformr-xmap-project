% Generated by roxygen2: do not edit by hand
% Please edit documentation in create-xmap.Rmd.
\name{abort_missing_cols}
\alias{abort_missing_cols}
\alias{abort_any_na}
\alias{abort}
\alias{abort_weights_col_type}
\alias{abort_dup_pairs}
\alias{abort_bad_weights}
\alias{abort_col_order}
\alias{abort_from_set}
\alias{msg_abort_frac_weights}
\alias{abort_not_reversible}
\alias{msg_abort_named_matchset}
\title{Validation functions and messages for xmap or candidate links (Internal)}
\usage{
abort_missing_cols(df, cols)

abort_any_na(df)

abort_weights_col_type(df, col_weights)

abort_dup_pairs(df, col_from, col_to)

abort_bad_weights(col_weights, call = rlang::caller_env())

abort_col_order(df, col_from, col_to, col_weights)

abort_from_set(df, col_from, from_set)

msg_abort_frac_weights(impact)

abort_not_reversible(df, col_to)

msg_abort_named_matchset(
  set_type = c("names", "values"),
  match_type = c("exact", "within", "contain")
)
}
\arguments{
\item{df}{a data.frame-like object containing links}

\item{col_from, col_to, col_weights}{character vector or values naming columns from \code{df}}
}
\value{
An error if the validation condition fails,
and invisibly returns \code{df} otherwise.
}
\description{
Checks issues with data.frame like objects containing validated \code{xmap} or candidate links.
}
\section{Functions}{
\itemize{
\item \code{abort_missing_cols()}: Abort if named columns can't be found in df

\item \code{abort_any_na()}: Abort if xmap_df has missing values

\item \code{abort_weights_col_type()}: Abort if xmap_df has wrong column types

\item \code{abort_dup_pairs()}: Abort if duplicate source-target pairs are found

\item \code{abort_bad_weights()}: Abort for invalid mapping weights

\item \code{abort_col_order()}: Abort if xmap_df columns are not in order

\item \code{abort_from_set()}: Abort if from_set attribute doesn't match xmap_df values

\item \code{msg_abort_frac_weights()}: Abort message for fractional weights

\item \code{abort_not_reversible()}: Abort if xmap_df is not reversible without new weights

\item \code{msg_abort_named_matchset()}: Abort message for verify_named_matchset_* functions

}}