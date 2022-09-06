#' Fit a `bespoke_class`
#'
#' `bespoke_class()` fits a model.
#'
#' @param x Depending on the context:
#'
#'   * A __data frame__ of predictors.
#'   * A __matrix__ of predictors.
#'   * A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' @param y When `x` is a __data frame__ or __matrix__, `y` is the outcome
#' specified as:
#'
#'   * A __data frame__ with 1 numeric column.
#'   * A __matrix__ with 1 numeric column.
#'   * A numeric __vector__.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   * A __data frame__ containing both the predictors and the outcome.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @return
#'
#' A `bespoke_class` object.
#'
#' @examples
#' predictors <- mtcars[, -1]
#' outcome <- mtcars[, 1]
#'
#' # XY interface
#' mod <- bespoke_class(predictors, outcome)
#'
#' # Formula interface
#' mod2 <- bespoke_class(mpg ~ ., mtcars)
#'
#' # Recipes interface
#' library(recipes)
#' rec <- recipe(mpg ~ ., mtcars)
#' rec <- step_log(rec, disp)
#' mod3 <- bespoke_class(rec, mtcars)
#'
#' @export
bespoke_class <- function(x, ...) {
  UseMethod("bespoke_class")
}

#' @export
#' @rdname bespoke_class
bespoke_class.default <- function(x, ...) {
  stop("`bespoke_class()` is not defined for a '", class(x)[1], "'.", call. = FALSE)
}

# XY method - data frame

#' @export
#' @rdname bespoke_class
bespoke_class.data.frame <- function(x, y, ...) {
  processed <- hardhat::mold(x, y)
  bespoke_class_bridge(processed, ...)
}

# XY method - matrix

#' @export
#' @rdname bespoke_class
bespoke_class.matrix <- function(x, y, ...) {
  processed <- hardhat::mold(x, y)
  bespoke_class_bridge(processed, ...)
}

# Formula method

#' @export
#' @rdname bespoke_class
bespoke_class.formula <- function(formula, data, ...) {
  processed <- hardhat::mold(formula, data)
  bespoke_class_bridge(processed, ...)
}

# Recipe method

#' @export
#' @rdname bespoke_class
bespoke_class.recipe <- function(x, data, ...) {
  processed <- hardhat::mold(x, data)
  bespoke_class_bridge(processed, ...)
}

# ------------------------------------------------------------------------------
# Bridge

bespoke_class_bridge <- function(processed, ...) {
  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  fit <- bespoke_class_impl(predictors, outcome)

  .new_bespoke_class(
    coefs = fit$coefs,
    blueprint = processed$blueprint
  )
}


# ------------------------------------------------------------------------------
# Implementation

bespoke_class_impl <- function(predictors, outcome) {
  list(coefs = 1)
}
