% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ResIN_utils.R
\name{ResIN_utils}
\alias{ResIN_utils}
\title{ResIN_utils}
\usage{
ResIN_utils(
  df,
  node_vars = NULL,
  cor_method = "auto",
  weights = NULL,
  method_wCorr = "Polychoric",
  remove_negative = TRUE,
  EBICglasso = FALSE,
  EBICglasso_arglist = NULL
)
}
\arguments{
\item{df}{A data-frame object containing the raw data.}

\item{node_vars}{An optional character string detailing the attitude item columns to be selected for ResIN analysis (i.e. the subset of attitude variables in df).}

\item{cor_method}{Which correlation method should be used? Defaults to "auto" which applies the \code{cor_auto} function from the \code{qgraph} package. Possible arguments are \code{"auto"}, \code{"pearson"}, \code{"kendall"}, and \code{"spearman"}.}

\item{weights}{An optional continuous vector of survey weights. Should have the same length as number of observations in df. If weights are provided, weighted correlation matrix will be estimated with the \code{weightedCorr} function from the \code{wCorr} package.}

\item{method_wCorr}{If weights are supplied, which method for weighted correlations should be used? Defaults to \code{"Polychoric"}. See \code{wCorr::weightedCorr} for all correlation options.}

\item{remove_negative}{Should all negative correlations be removed? Defaults to TRUE (highly recommended). Setting to FALSE makes it impossible to estimate a force-directed network layout. Function will use igraph::layout_nicely instead.}

\item{EBICglasso}{Should a sparse, Gaussian-LASSO ResIN network be estimated? Defaults to FALSE. If set to TRUE, \code{EBICglasso} function from the \code{qgraph} packages performs regularization on (nearest positive-semi-definite) ResIN correlation matrix.}

\item{EBICglasso_arglist}{An argument list feeding additional instructions to the \code{EBICglasso} function if \code{EBICglasso} is set to TRUE.}
}
\value{
A list object containing the original dataframe, (\code{resin_df}), the dummy-coded dataframe (\code{resin_dummies}), the ResIN correlation and covariance matrices (\code{resin_cor} & \code{resin_vcov}), and a numeric vector detailing which item responses belong to which item (\code{same_items}).
}
\description{
Generates auxiliary utensils useful for Response-Item Networks analysis.
}
\examples{

## Load the 12-item simulated Likert-type ResIN toy dataset
data(lik_data)

## Extract the utilities
output <- ResIN_utils(lik_data)

}
\references{
Epskamp S, Cramer AOJ, Waldorp LJ, Schmittmann VD, Borsboom D (2012). “qgraph: Network Visualizations of Relationships in Psychometric Data.” Journal of Statistical Software, 48(4), 1–18.
}
