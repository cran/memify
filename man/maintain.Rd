\name{memify support functions}
\alias{memify support}
\alias{arglist}
\alias{update.memified}
\alias{arglist<-}

\title{
   Extract, Update, and Replace Argument Lists of Memified Functions
}
\description{
   These functions support the use of memified functions.
}
\usage{
arglist(f)

arglist(x) <- value

\method{update}{memified}(object, \dots)
}

\arguments{
  \item{x, f, object}{
      A memified function.
   }
   \item{value}{
   A \strong{named list} of argument values to replace the existing argument list.
   An attempt will be made to coerce a non-list to a list. If successful, the coerced arglist will be used and a warning reporting the coercion will be issued. Otherwise an error will be thrown.
   }
   \item{\dots}{
   Tagged argument-value pairs to add to or replace existing arglist arguments.
   }
}

\value{
   \code{arglist()} returns the existing argument list.

   \code{arglist <- value} replaces the existing argument list with the (possibly coerced) new list.

   \code{update()} (silently) adds additional named arguments to a function's arglist and/or changes the values of any that already exist. \code{NULL} is invisibly returned.
   }

\note{
      \dQuote{arglists}, the remembered argument lists of memified functions, consist only of values that have been \emph{explicitly} specified in prior calls of the memified function, or via \code{update} or \code{arglist <- value} functionality. Hence default arguments that have not been so changed or specified will not be in the arglists. See \code{\link{formals}} or \code{\link{args}} for ways to extract/change such defaults.
}

\section{Warning}{
   The update and replacement functions do not check that valid argument names
   are used. Invalid arguments can of course cause errors when the memified
   functions are subsequently invoked.
}
\author{
Bert Gunter
}

\seealso{
   \code{\link{memify}}
}

\examples{

f.m <- memify(function(a = 0, b) sin(a+b))
f.m( b = pi/4) # uses default for a
arglist(f.m)
update(f.m, a= pi/4) ## new default for a
f.m(b = -pi/4)
arglist(f.m) <- list() ## resets arglist
f.m( b = pi/4) ## The original default of a = 0 is used

## cleanup
rm(f.m)
}

\keyword{ data }
\keyword{ utilities }
