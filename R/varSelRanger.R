#' Variable selection using recursive feature elimination.
#'
#' Models are evaluated based on the OOB error rate.
#'
#' @param data Training data of class \code{data.frame}, \code{matrix} or \code{dgCMatrix}.
#' @param dependent.variable.name Name of the dependent variable. For survival forests this is the time variable.
#' @param formula Object of class formula or character describing the model to fit. Interaction terms supported only for numerical variables. (Formula interface not yet supported in varSelRanger).
#' @param frac Fraction of top variables to drop after each iteration.
#' @param importance Variable importance mode. One of 'none', 'impurity', 'impurity_corrected', 'permutation'. The 'impurity' measure is the Gini index for classification, the variance of the responses for regression and the sum of test statistics (see splitrule) for survival.
#' @param min.vars Minimum number of variables allowed in a model.
#' @param ... Further arguments to be passed to \link{\code{ranger::ranger}}.
#' @export
#' @importFrom ranger ranger
varSelRanger <- function(data, dependent.variable.name, formula=NULL, frac=0.2, min.vars=1, importance="impurity", ...) {
  if(!is.null(formula)) stop("The formula interface is not supported yet.")
  if(!any(c("data.frame","matrix","Matrix") %in% class(data))) stop("'data' must be a data frame or matrix.")
  if(!is.numeric(frac)) stop("'frac' must be numeric.")
  if(frac <= 0 || frac >= 1) stop("'frac' must be greater than 0 and less than 1.")

  vars <- setdiff(colnames(data), dependent.variable.name)
  selected.vars <- NULL
  errors <- NULL

  while(length(vars) >= min.vars) {
    data <- data[,c(vars, dependent.variable.name),drop=FALSE]
    fit <- ranger(
      data=data,
      dependent.variable.name=dependent.variable.name,
      importance=importance,
      ...
    )

    selected.vars <- c(selected.vars, list(vars))
    errors <- c(errors, fit$prediction.error)

    imp <- sort(fit$variable.importance, TRUE)
    nsel <- floor(length(imp) * (1 - frac))

    vars <- names(head(imp, nsel))
  }

  list(
    selected.vars = selected.vars,
    errors = errors
  )
}
