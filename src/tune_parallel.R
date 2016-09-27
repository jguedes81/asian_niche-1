tune_parallel <- function (method, train.x, train.y = NULL, data = list(), validation.x = NULL, 
                               validation.y = NULL, ranges = NULL, predict.func = predict, 
                               tunecontrol = tune.control(), ...) 
{
  call <- match.call()
  resp <- function(formula, data) {
    model.response(model.frame(formula, data))
  }
  classAgreement <- function(tab) {
    n <- sum(tab)
    if (!is.null(dimnames(tab))) {
      lev <- intersect(colnames(tab), rownames(tab))
      p0 <- sum(diag(tab[lev, lev]))/n
    }
    else {
      m <- min(dim(tab))
      p0 <- sum(diag(tab[1:m, 1:m]))/n
    }
    p0
  }
  if (tunecontrol$sampling == "cross") 
    validation.x <- validation.y <- NULL
  useFormula <- is.null(train.y)
  if (useFormula && (is.null(data) || length(data) == 0)) 
    data <- model.frame(train.x)
  if (is.vector(train.x)) 
    train.x <- t(t(train.x))
  if (is.data.frame(train.y)) 
    train.y <- as.matrix(train.y)
  if (!is.null(validation.x)) 
    tunecontrol$fix <- 1
  n <- nrow(if (useFormula) 
    data
    else train.x)
  perm.ind <- sample(n)
  if (tunecontrol$sampling == "cross") {
    if (tunecontrol$cross > n) 
      stop(sQuote("cross"), " must not exceed sampling size!")
    if (tunecontrol$cross == 1) 
      stop(sQuote("cross"), " must be greater than 1!")
  }
  train.ind <- if (tunecontrol$sampling == "cross") 
    tapply(1:n, cut(1:n, breaks = tunecontrol$cross), function(x) perm.ind[-x])
  else if (tunecontrol$sampling == "fix") 
    list(perm.ind[1:trunc(n * tunecontrol$fix)])
  else lapply(1:tunecontrol$nboot, function(x) sample(n, n * 
                                                        tunecontrol$boot.size, replace = TRUE))
  parameters <- if (is.null(ranges)) 
    data.frame(dummyparameter = 0)
  else expand.grid(ranges)
  p <- nrow(parameters)
  if (!is.logical(tunecontrol$random)) {
    if (tunecontrol$random < 1) 
      stop("random must be a strictly positive integer")
    if (tunecontrol$random > p) 
      tunecontrol$random <- p
    parameters <- parameters[sample(1:p, tunecontrol$random), 
                             ]
  }
  model.variances <- model.errors <- c()
  for (para.set in 1:p) {
    
    
    
    
    sampling.errors <- c()
    for (sample in 1:length(train.ind)) {
      repeat.errors <- c()
      for (reps in 1:tunecontrol$nrepeat) {
        pars <- if (is.null(ranges)) 
          NULL
        else lapply(parameters[para.set, , drop = FALSE], 
                    unlist)
        model <- if (useFormula) 
          do.call(method, c(list(train.x, data = data, 
                                 subset = train.ind[[sample]]), pars, list(...)))
        else do.call(method, c(list(train.x[train.ind[[sample]], 
                                            ], y = train.y[train.ind[[sample]]]), pars, 
                               list(...)))
        pred <- predict.func(model, if (!is.null(validation.x)) 
          validation.x
          else if (useFormula) 
            data[-train.ind[[sample]], , drop = FALSE]
          else if (inherits(train.x, "matrix.csr")) 
            train.x[-train.ind[[sample]], ]
          else train.x[-train.ind[[sample]], , drop = FALSE])
        true.y <- if (!is.null(validation.y)) 
          validation.y
        else if (useFormula) {
          if (!is.null(validation.x)) 
            resp(train.x, validation.x)
          else resp(train.x, data[-train.ind[[sample]], 
                                  ])
        }
        else train.y[-train.ind[[sample]]]
        if (is.null(true.y)) 
          true.y <- rep(TRUE, length(pred))
        repeat.errors[reps] <- if (!is.null(tunecontrol$error.fun)) 
          tunecontrol$error.fun(true.y, pred)
        else if ((is.logical(true.y) || is.factor(true.y)) && 
                 (is.logical(pred) || is.factor(pred) || is.character(pred))) 
          1 - classAgreement(table(pred, true.y))
        else if (is.numeric(true.y) && is.numeric(pred)) 
          crossprod(pred - true.y)/length(pred)
        else stop("Dependent variable has wrong type!")
      }
      sampling.errors[sample] <- tunecontrol$repeat.aggregate(repeat.errors)
    }
    model.errors[para.set] <- tunecontrol$sampling.aggregate(sampling.errors)
    model.variances[para.set] <- tunecontrol$sampling.dispersion(sampling.errors)
  }
  best <- which.min(model.errors)
  pars <- if (is.null(ranges)) 
    NULL
  else lapply(parameters[best, , drop = FALSE], unlist)
  structure(list(best.parameters = parameters[best, , drop = FALSE], 
                 best.performance = model.errors[best], method = if (!is.character(method)) deparse(substitute(method)) else method, 
                 nparcomb = nrow(parameters), train.ind = train.ind, sampling = switch(tunecontrol$sampling, 
                                                                                       fix = "fixed training/validation set", bootstrap = "bootstrapping", 
                                                                                       cross = if (tunecontrol$cross == n) "leave-one-out" else paste(tunecontrol$cross, 
                                                                                                                                                      "-fold cross validation", sep = "")), performances = if (tunecontrol$performances) cbind(parameters, 
                                                                                                                                                                                                                                               error = model.errors, dispersion = model.variances), 
                 best.model = if (tunecontrol$best.model) {
                   modeltmp <- if (useFormula) do.call(method, c(list(train.x, 
                                                                      data = data), pars, list(...))) else do.call(method, 
                                                                                                                   c(list(x = train.x, y = train.y), pars, list(...)))
                   call[[1]] <- as.symbol("best.tune")
                   modeltmp$call <- call
                   modeltmp
                 }), class = "tune")
}




