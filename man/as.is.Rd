% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.r
\name{as.is}
\alias{as.is}
\title{Cast an object to match class of another object}
\usage{
as.is(x, vec)
}
\arguments{
\item{x}{object to transform}

\item{vec}{object to extract class}
}
\description{
Cast an object to match class of another object
}
\examples{
as.is(c("1", "2", "3"), 1:3)

}
