# This example is based off of Problem 4, chapter 8, Lange 2013
# the function should have a global minimum at (x1,y1) = (1,1) with function value 0
library(pracma)

#' The original Rosenbrock objective function which we wish to majorize
#'
#' @param x1 scalar
#' @param x2 scalar
#'
#' @return scalar output
#' @export
#'
#' @examples
#' f(1,1)
f <- function(x1,x2){
  100*(x1^2-x2)^2 + (x1-1)^2
}

#' Iterative procedure for the x2 variable, derived by minimizing the surrogate for x2
#' see Lange for details of the inequality used to derive the surrogate
#'
#' @param x1 scalar
#' @param x2 scalar
#'
#' @return next iterate of the x2 variable
#' @export
#'
#' @examples
#' x2_iter(1,1)
x2_iter <- function(x1,x2){
  0.5*(x1^2+x2)
}

#' The iterative procedure for the x1 variable, derived by minimizing the surrogate for x1
#' see Lange for details of the inequality used to derive the surrogate
#'
#' @param x1 scalar
#' @param x2 scalar
#'
#' @return next iterate of the x1 variable
#' @export
#'
#' @examples
#' x1_iter(1,1)
x1_iter <- function(x1,x2){
  require(pracma)
  roots <- pracma::Real(polyroot(c(-1,-(200*(x1^2 + x2)-1),0,400)))
  f_eval <- f(roots, x2)
  # print(c(roots, f_eval))
  best_root <- roots[f_eval == min(f_eval)]
  best_root
}

#' A function for performing the iterative procedure and printing the results
#'
#' @param num_iter integer, must be greater than 0
#' @param x1_init scalar
#' @param x2_init scalar
#' @param tol positive real
#'
#' @return numeric vector containing current x1 iterate, current x2 iterate,
#' value of objective at current iterate, number of iterations performed, and
#' a list of all values of the objective function, up to and including the
#' current value.
#' @export
#'
#' @examples
#' do_iterations(1,1,1,0.5)
do_iterations <- function(num_iter, x1_init, x2_init, tol) {
  if (num_iter < 1) {
    stop("Number of iterations must be greater than one")
  }
  if (tol < 0) {
    stop("Tolerance cannot be less than zero")
  }
  iter = 0
  f_values = rep(0,num_iter)
  x1_current <- x1_init
  x2_current <- x2_init
  for (i in c(1:num_iter)) {
    iter = iter +1
    x1_prev <- x1_current
    x1_current <- x1_iter(x1_current, x2_current)
    x2_current <- x2_iter(x1_prev, x2_current)
    # print(c(x1_current, x2_current, f(x1_current, x2_current), iter))
    # break if error is within tolerance
    er <- sum(c(x1_current-1, x2_current-1))^2
    f_values[iter] = (f(x1_current, x2_current))
    if (er < tol) {
      return(c(x1_current, x2_current, f(x1_current, x2_current), iter, f_values[1:iter]))
      break
    }
  }
  return(c(x1_current, x2_current, f(x1_current, x2_current), iter, f_values[1:iter]))
}

# At optimal point, choosing the first root works
do_iterations(100,1,1, tol = 0.1)

# However, even in a neighbourhood of the optimal point the solution blows up
do_iterations(num_iter = 5,2,2, tol = 0.1)
