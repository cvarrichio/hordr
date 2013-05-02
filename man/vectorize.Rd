\name{vectorize}
\alias{vectorize}
\title{Robust alternative to Vectorize function that accepts function with two or more arguments.}
\usage{
  vectorize(fun, type = 0)
}
\arguments{
  \item{fun}{a two or more argument function}

  \item{type}{1 forces a row-wise evaluation, even on
  atomic vectors}
}
\description{
  Returns a function that will work an arbitrary number of
  vectors, lists or data frames, though output may be
  unpredicatable in unusual applications The results are
  also intended to be more intuitive than
  \code{\link{Vectorize}}.
}
\examples{
vectorize(`+`)(c(1,2,3))
vectorize(sum)(c(1,2,3),c(1,2,3))
# Compare these results to Vectorize, which does not vectorize sum at all.
Vectorize(sum)(c(1,2,3),c(1,2,3))
# Any combination of vectors, lists, matrices, or data frames an be used (although cbind has some strange behavior in certain cases)
vectorize(`+`)(c(1,2,3),list(1,2,3),cbind(c(1,2,3)))
}

