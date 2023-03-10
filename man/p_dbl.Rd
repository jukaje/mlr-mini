% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spacemaker.R
\name{p_dbl}
\alias{p_dbl}
\title{make hyperparameter space}
\usage{
p_dbl(range1, range2)
}
\arguments{
\item{range1}{start (left) value}

\item{range2}{end (right) value}

\item{factors}{vector of factors}
}
\description{
This function returns the type, and start and end of a range for dbl
}
\examples{
x <- p_dbl(1,3)

x

#> $type
#> [1] "dbl"

#> $start
#> [1] 1

#> $end
#> [1] 3
}
