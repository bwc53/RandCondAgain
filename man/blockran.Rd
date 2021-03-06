% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BlockRandomization.R
\name{blockran}
\alias{blockran}
\title{Block Randomization Function}
\usage{
blockran(conditions, subjects.per.condition, groups = NULL)
}
\arguments{
\item{conditions}{Number of experimental conditions}

\item{subjects.per.condition}{Number of participants per group}

\item{groups}{List in which each element corresponds to one stratifying variable
and each element is a character vector consisting of names for the levels of
the stratifying variable}
}
\value{
List in which each element is the randomization sequence for a single group
}
\description{
The purpose of this function is to randomize participants to experimental conditions
}
\examples{
mydesign <- blockran(5, 10)
mydesign <- blockran(4, 5, list(Gender = c("Male", "Female"), History = c("None", "Low", "High")))
}

