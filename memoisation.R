## Performance Optimisation Strategies in R: Memoisation
##
## This example is based on:
## http://dirk.eddelbuettel.com/code/rcpp/html/Misc_2fibonacci_8r-example.html
##
## It was originally written as a response to:
## http://stackoverflow.com/questions/6807068/why-is-my-recursive-function-so-slow-in-r
##
## It will show the Fibonacci series of numbers computed
## a) in R (slow)
## b) in C++ (faster)
## c) in optimized R (lightning fast)

library(Rcpp)
library(rbenchmark)

## The c++ version
fibRcpp <- cppFunction('
int fibonacci(const int x) {
   if (x == 0) return(0);
   if (x == 1) return(1);
   return (fibonacci(x - 1)) + fibonacci(x - 2);
}
')

## Non-optimised R
fibR <- function(x) {
  if (x == 0) return(0)
  if (x == 1) return(1)
  Recall(x - 1) + Recall(x - 2)
}

## Implements a 'memory' for the fibR function
memoiseFibR <- local({
  memory <- list()
  function(x) {
    valueName <- as.character(x)
    if (!is.null(memory[[valueName]])) return(memory[[valueName]])
    if (x == 0) return(0)
    if (x == 1) return(1)
    res <- Recall(x - 1) + Recall(x - 2)
    memory[[valueName]] <<- res # store results
    res
  }
})

## Derived from the above we factored out the original function 'fibR' and
## replaced it with an argument 'fun'. Problems:
##   - only accepts functions with one argument (easy to fix)
##   - does not work with recursion
memoise <- function(fun) {
  memory <- list()
  function(x) {
    valueName <- as.character(x)
    if (!is.null(memory[[valueName]])) return(memory[[valueName]])
    res <- fun(x)
    memory[[valueName]] <<- res
    res
  }
}

## A differnet implementation using meta programming: templates.
##   - Allows for recursion.
##   - Also for more than one argument by using a check sum
##   - Higher complexity, hard to debug.
memoise2 <- function(fun) {

  functionTemplate <- templates::tmpl(
    "local({
     memory <- list()
     function({{ args }}) {
       valueName <- as.character(openssl::md5(serialize(list({{ args }}), NULL)))
       if (!is.null(memory[[valueName]])) return(memory[[valueName]])
       res <- {{ body }}
       memory[[valueName]] <<- res
       res
     }
   })"
  )

  templates::tmplEval(
    functionTemplate,
    args = paste(names(formals(fun)), collapse = ", "),
    body = paste(deparse(body(fun)), collapse = "\n"),
    .envir = environment(fun)
  )

}

## campare the different results
N <- 35
benchmark(
  baseR = fibR(N),
  Rcpp = fibRcpp(N),
  Rmem1 = memoiseFibR(N),
  Rmem2 = memoise(fibR)(N),
  Rmem3 = memoise2(fibR)(N),
  columns = c("test", "replications", "elapsed", "user.self"),
  order = "elapsed",
  replications = 1
)

