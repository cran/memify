\name{memify}
\alias{memify}
\title{
Enable Functions To Keep State
}
\description{
Constructs new \sQuote{memified} versions of functions that keep state -- remember the values of their arguments -- between calls.
}
\usage{
memify(f, envir = parent.frame())
}

\arguments{
  \item{f}{
   A function (a closure) to convert. If name is a character string then the function with that name is found and used.
   }
   \item{envir}{
   The \code{\link{environment}} in which the function is defined or found via \code{get()} when f is a character string.
   }
}
\details{
   One should (almost) \emph{never} assign the memified function back to its original function name, as this would replace the original function by its memified version, causing all manner of problems. \strong{You have been warned!}
}

\value{
   A function of class \code{"memified"}, extending the class of f. It is the same as f, except it "remembers" the values of all arguments from its previous calls and uses them as defaults if they are not respecified in the current call. See the examples.
}
\author{
Bert Gunter
}
\note{
\code{\link{primitive}} functions cannot be memified. They are not closures and some do not make use of named arguments, as they match by position rather than name. However, see the examples below for a workaround of this limitation.

Note also that \emph{unnamed} \dots arguments in calls are \emph{not} remembered. They can be included and accessed as usual, but are forgotten when the function returns.
}

\seealso{
   \code{\link{arglist}}
   \code{\link{update.memified}}
   \code{\link{arglist<-}}
}

\examples{

add2 <- function(a,b) a+b
add2.m <- memify(add2)
add2.m(2,3)  ## a = 2, b= 3, as usual
add2.m(5) ## a =5; b = 3 from previous call
add2.m(b = 10) ## a = 5 from previous call
add2.m() ## both a and b from previous call
z <- 100
add2.m(1,z) ## if not missing, arguments are evaluated as usual
rm(z)

## Also as usual, unexpected arguments produce an error:
\dontrun{add2.m(unused = 10)}

## Memifying functions with unnamed ... arguments:
sum.m <- memify(function(a,b, ...) sum(a, b, ...))
sum.m(2, 3, 10, 5) ## a =2, b = 3, ... = c(10,5)
sum.m()  ## unnamed arguments are forgotten and not reused!
sum.m( b = 7, 5) ## Is 5 the value for a or ... ?
sum.m()  ## It's for a, following R's standard argument matching rules
arglist(sum.m) ## Is a better way to check argument lists

## memify may be useful in plot functions with many arguments:
plot.m <- memify(plot)
x <- 1:9; y <- runif(9)
plot.m(x,y, col = "blue")
## Change the default type argument and col to "red"
plot.m(col = "red", type = "b")
## make lwd = 2
plot.m(lwd = 2)

## memifying a primitive function:
## exponentiation via '^' is a primitive function that uses positional matching
`^`
## memify a wrapper to convert a primitive to a closure
exp.m <- memify(function(y = 1, x = 0) y^x)
exp.m() ## uses default values
exp.m(2,3) ## y = 2, x = 3
exp.m(x = 5) ## y = 2
exp.m() ## same as previous

## cleanup
rm(add2, add2.m, sum.m, plot.m, exp.m)
}

\keyword{ data }
\keyword{ utilities }
