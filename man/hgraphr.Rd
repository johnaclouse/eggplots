% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hgraphr.R
\name{hgraphr}
\alias{hgraphr}
\title{hgraphr}
\usage{
hgraphr(
  df,
  index,
  color_palette = c("#feedde", "#fdd0a2", "#fdae6b", "#fd8d3c", "#f16913")
)
}
\arguments{
\item{df}{data frame containing values to be plotted. Data frame must
contain: dimension, measure, individual, and neighborhood. Dimension is a
factor, measure is numeric, and individual and neighborhood are logical.}

\item{index}{numeric value representing the individual's relative risk index based on the population.}

\item{color_palette}{vector of RGB background colors to be used for
percentile ranges {(0, 50), (50, 80), (80, 90), (90, 95), (95,
100)}}
}
\description{
A plot for showing high dimensional attributes with indication of percentile
ranges.
}
\examples{
df = data.frame(
dimension = factor(state.name[1:22]),
measure = runif(22) * 100,
individual = sample(c(TRUE, FALSE, FALSE), 22, replace = TRUE),
neighborhood = sample(c(TRUE, TRUE, FALSE), 22, replace = TRUE)
) \%>\%
  dplyr::mutate(measure = ifelse(individual == FALSE &
                                   neighborhood == FALSE, NA, measure))
hgraphr(df, index = 67)
}
