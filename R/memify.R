
memify <- function(f, envir = parent.frame())
{
   if (is.character(f))
      f <- get(f, mode = "function", envir = envir)
   if (is.primitive(f))
      stop("Cannot memify primitive functions.\n")
   if (!inherits(f, "function"))
      stop("Argument must inherit from class 'function'")
   if(inherits(f,"memified")){
      warning("This function is already memified")
      return(NULL)
   }
   arglist <- list()
   ### the memified function:
   structure(function(...) {
      m <- tryCatch(
         as.list(match.call(f)[-1]),
         error = function(e)
            stop("\nBad function call:\n", conditionMessage(e))
      )
      nm <- names(m)
      hasname <- nm != "" #logical index of named values
      if (any(hasname)) {
         if (anyDuplicated(nm, incomparables = ""))
            warning("Duplicated names in call; only the first will be used.")
         arglist <<- modifyList(arglist, m[hasname])
      }
      do.call(f, modifyList(m, arglist))
   },
   class = c("memified", class(f))
   )
}
##########################
##########################
arglist <- function(f){
   if(!inherits(f,"memified"))
      stop("This function is not memified")
   else environment(f)$arglist
}
###########################
###########################
update.memified <- function(object, ...)
{
   m <- tryCatch(
      list(...),
      error = function(e) {
         stop("\nUnable to update: Cannot coerce arrguments to a list.\n",
              e)
      }
   )
   if (!length(m)) {
      stop("Unable to update: no arguments.\n")
   }
   nm <- names(m)
   if (is.null(nm) || any(nm == "")) {
      stop("There are unnamed arguments. All arguments must be named.")
   }
   else {
      if (anyDuplicated(nm)) {
         warning("There are duplicated argument names.\n",
                 "Only the first will be used.\n")
      }
      environment(object)$arglist <- modifyList(arglist(object), m)
   }
   invisible(NULL)
}
######################################
######################################
"arglist<-" <- function(x, value) {
   if(!inherits(x,"memified"))
      stop( "x is not a 'memified' function'\n",
            "Use memify() to memify a function.")
   if (is.null(value))
      stop(
         "Replacement must be a list.\n",
         "Use '... <- list()' to remove all memified argument values."
      )
   if (!inherits(value, "list")) {
      value <- tryCatch(
         as.list(value),
         error = function(e)
            stop(
               "Cannot coerce replacement value to list\n",
               conditionMessage(e),
               "\n"
            )
      )
      warning("Non-list replacement coerced to list\n")
   }
   if (!identical(value, list())) {
      nm <- names(value)
      if (is.null(nm) || any(nm == ""))
         stop("All list components must be named.")
   }
   environment(x)$arglist <- value
   x
}
