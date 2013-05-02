\name{coalesce}
\alias{coalesce}
\title{A more versatile form of the T-SQL \code{coalesce()} function.}
\usage{
  coalesce(...,
    fun = (function(x, y) if (!is.na(x)) x else y))
}
\arguments{
  \item{...}{an arbitrary number of data objects, including
  vectors, lists, data objects}

  \item{fun}{a two argument function that returns an atomic
  value}
}
\description{
  Little more than a wrapper for \code{\link{vectorize}},
  allows for duplication of SQL coalesce functionality,
  certain types of if-else statements, and
  \code{\link{apply}}/\code{\link{Reduce}} combinations.
}
\examples{
coalesce(c(NA,1,2))
coalesce(c(NA,1,2),c(3,4,NA))
}

