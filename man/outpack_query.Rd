% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{outpack_query}
\alias{outpack_query}
\title{Query outpack's database}
\usage{
outpack_query(
  expr,
  pars = NULL,
  scope = NULL,
  require_unpacked = FALSE,
  root = NULL
)
}
\arguments{
\item{expr}{The query expression}

\item{pars}{Optionally, a named list of parameters to substitute
into the query (using the \verb{this:} prefix)}

\item{scope}{Optionally, a scope query to limit the packets
searched by \code{pars}}

\item{require_unpacked}{Logical, indicating if we should require
that the packets are unpacked. If \code{FALSE} (the default) we
search through all packets known to this outpack root,
regardless of if they are locally available, but if \code{TRUE}, only
unpacked packets will be considered.}

\item{root}{The outpack root. Will be searched for from the
current directory if not given.}
}
\value{
A character vector of matching ids
}
\description{
Evaluate a query against the outpack database, returning a vector
of matching packet ids.
}
