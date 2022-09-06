#' Predict from a `bespoke_class`
#'
#' @param object A `bespoke_class` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"numeric"` for numeric predictions.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @examples
#' train <- mtcars[1:20,]
#' test <- mtcars[21:32, -1]
#'
#' # Fit
#' mod <- bespoke_class(mpg ~ cyl + log(drat), train)
#'
#' # Predict, with preprocessing
#' predict(mod, test)
#'
#' @export
predict.bespoke_class <- function(object, new_data, type = "numeric", ...) {
  forged <- hardhat::forge(new_data, object$blueprint)
  rlang::arg_match(type, valid_bespoke_class_predict_types())
  predict_bespoke_class_bridge(type, object, forged$predictors)
}

valid_bespoke_class_predict_types <- function() {
  c("numeric")
}

# ------------------------------------------------------------------------------
# Bridge

predict_bespoke_class_bridge <- function(type, model, predictors) {
  predictors <- as.matrix(predictors)

  predict_function <- get_bespoke_class_predict_function(type)
  predictions <- predict_function(model, predictors)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

get_bespoke_class_predict_function <- function(type) {
  switch(
    type,
    numeric = predict_bespoke_class_numeric
  )
}

# ------------------------------------------------------------------------------
# Implementation

predict_bespoke_class_numeric <- function(model, predictors) {
  predictions <- rep(1L, times = nrow(predictors))
  hardhat::spruce_numeric(predictions)
}
