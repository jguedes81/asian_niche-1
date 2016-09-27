tune_svm_parallel <- function (x, y = NULL, data = NULL, degree = NULL, gamma = NULL, 
                               coef0 = NULL, cost = NULL, nu = NULL, class.weights = NULL, 
                               epsilon = NULL, ...) 
{
  call <- match.call()
  call[[1]] <- as.symbol("best.svm")
  ranges <- list(degree = degree, gamma = gamma, coef0 = coef0, 
                 cost = cost, nu = nu, class.weights = class.weights, 
                 epsilon = epsilon)
  ranges[sapply(ranges, is.null)] <- NULL
  if (length(ranges) < 1) 
    ranges = NULL
  modeltmp <- if (inherits(x, "formula")) 
    tune_parallel("svm", train.x = x, data = data, ranges = ranges, 
         ...)
  else tune_parallel("svm", train.x = x, train.y = y, ranges = ranges, 
            ...)
  if (!is.null(modeltmp$best.model)) 
    modeltmp$best.model$call <- call
  modeltmp
}