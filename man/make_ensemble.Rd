% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiple_filter_fns.R
\name{make_ensemble}
\alias{make_ensemble}
\title{Generate predictions based on an ensemble of classification algorithms;}
\usage{
make_ensemble(x, y, library, multiple = FALSE)
}
\arguments{
\item{x}{training data; matrix of features/covariates used for prediction.}

\item{y}{training labels; must be factor of non-integer values i.e. "one"/"two" instead of 1/2}

\item{library}{A vector of strings indicating the algorithms to be included in the ensemble. Available algorithms can be queried here: https://topepo.github.io/caret/available-models.html}

\item{multiple}{Boolean value indicating whether to run multiple iterations of cross-validation to generate ensemble predictions; default = FALSE; If TRUE, will perform 5 iterations}
}
\value{
A list containing: "MF", samples to be kept by majority filter; "CF", samples to be kept by consensus filter, "full_res", the full list of discordant predictions
}
\description{
Generate predictions based on an ensemble of classification algorithms;
}
