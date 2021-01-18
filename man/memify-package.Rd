\name{memify-package}
\alias{memify-package}
\docType{package}
\title{
   A Simple Framework to Construct and Maintain Functions That Keep State
}
\description{
   This package provides a simple way to construct and maintain versions of R functions that keep their state -- i.e. "remember" their explicitly specifed arguments from previous calls. If not overridden, the remembered values of these arguments are automatically reused as the defaults for subsequent calls.

   This may be convenient when a function with a large argument list -- plotting functions are typical examples -- needs to be repeatedly invoked with only a few changes in the arguments at each invocation; or when interacting with a function to determine what parameter values give the most informative results. While there are certainly other ways to do this, the simplicity of this approach may make it a useful alternative.
}

\details{

The package has only one key function: \code{memify()}. It takes one argument, \code{f}, a function or a function name as a name/symbol or character string. The result, almost always assigned to a different name, is a new version of \code{f} that will keep state. It has S3 class \code{"memified"} that extends the function's original class vector . The original version of \code{f} of course remains.

Some minor additional convenience functions, \code{arglist()}, \code{arglist(x) <- value}, and an update method, \code{update.memified()}, are also provided. See the examples here and in their repective Help pages for usage.

}

\note{
The \emph{arglist} of a memified function consists only of arguments that were explicitly specified in a prior call. Hence, any formal default arguments that were not changed or specified will not be included in the memified function's arglist, which can be extracted via the \code{arglist()} function.
}

\author{
\packageAuthor{memify}

Maintainer: \packageMaintainer{memify}
}

\keyword{ package }

\examples{
mod <- function(x, b = 5) x \%\% b
mod.m <- memify(mod) ## or memify("mod")
mod.m(7) ## using default b = 5
mod.m(b = 3) ## using remembered x = 7
mod.m()  ## the same as previous

arglist(mod.m) ## list of all memified arguments
update (mod.m, b = 9) ## silently updates arglist
arglist(mod.m)

arglist(mod.m) <- list(x=11) ## replaces the arglist with a new list
arglist(mod.m)
## b is no longer memified, so mod's default = 5 is used:
mod.m()

## cleanup
rm(mod.m)
}





