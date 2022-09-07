# Copyright 2022 Bedford Freeman & Worth Pub Grp LLC DBA Macmillan Learning.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Predict from a `bespoke_classification`
#'
#' @param object A `bespoke_classification` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"class"` for "hard" class predictions.
#' - `"prob"` for class probabilities.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @export
predict.bespoke_classification <- function(object,
                                  new_data,
                                  type = c("class", "prob"),
                                  ...) {
  forged <- hardhat::forge(new_data, object$blueprint)
  type <- rlang::arg_match(type)
  .predict_bespoke_classification_bridge(type, object, forged$predictors)
}


# ------------------------------------------------------------------------------
# Bridge

#' Prepare bespoke_classification Data for Prediction
#'
#' @inheritParams predict.bespoke_classification
#' @param model A `bespoke_classification` model object. hardhat switches to model from
#'   here forward, I think because the object has to actually be a model at this
#'   point for anything to make sense, and they don't have to fight the predict
#'   generic for the name.
#' @param predictors Forged predictor data.
#'
#' @return A tibble with output dependent on type.
#' @keywords internal
.predict_bespoke_classification_bridge <- function(type, model, predictors) {
  predict_function <- .get_bespoke_classification_predict_function(type)
  predictions <- predict_function(model, predictors)

  hardhat::validate_prediction_size(predictions, predictors)

  return(predictions)
}

#' Choose the Predict Function for a Bespoke Classification
#'
#' This is pointless right now but I'm keeping it in place to make it relatively
#' easy to implement probabilities.
#'
#' @inheritParams predict.bespoke_classification
#'
#' @return The predict function.
#' @keywords internal
.get_bespoke_classification_predict_function <- function(type) {
  switch(
    type,
    class = .predict_bespoke_classification_class,
    prob = .predict_bespoke_classification_prob
  )
}

# ------------------------------------------------------------------------------
# Implementation

#' Predict Classes for Bespoke Classification Models
#'
#' @inheritParams .predict_bespoke_classification_bridge
#'
#' @return A tibble with a factor column identifying the outcome.
#' @keywords internal
.predict_bespoke_classification_class <- function(model, predictors) {
  predictions <- rlang::exec(
    .fn = model$fn,
    predictors,
    !!!model$dots
  )
  predictions <- .factorize_predictions(
    predictions = predictions,
    outcome_levels = model$outcome_levels
  )

  return(hardhat::spruce_class(predictions))
}

#' Predict Probabilities for Bespoke Classification Models
#'
#' @inheritParams .predict_bespoke_classification_bridge
#'
#' @return A tibble with class probabilities (1 or 0).
#' @keywords internal
.predict_bespoke_classification_prob <- function(model, predictors) {
  # Start with the hard classes.
  predictions <- .predict_bespoke_classification_class(model, predictors)

  # One-hot encode the predictions, as doubles.
  predictions_wide <- vapply(
    model$outcome_levels,
    function(this_outcome) {
      as.numeric(this_outcome == predictions$.pred_class)
    },
    rep(1, nrow(predictions))
  )

  return(
    hardhat::spruce_prob(
      pred_levels = model$outcome_levels,
      prob_matrix = predictions_wide
    )
  )
}

#' Coerce Predictions to Factor
#'
#' @param predictions Predictions produced by a bespoke model function.
#' @param outcome_levels A character vector indicating the expected outcome
#'   levels, in the same order that they were defined in the training data.
#'
#' @return Predictions as a factor with the expected levels.
#' @keywords internal
#' @rdname dot-factorize_predictions
.factorize_predictions <- function(predictions, outcome_levels) {
  UseMethod(".factorize_predictions")
}

#' @export
#' @rdname dot-factorize_predictions
.factorize_predictions.default <- function(predictions, outcome_levels) {
  rlang::abort(
    "Predictions are not coercible to a factor."
  )
}

#' @export
#' @rdname dot-factorize_predictions
.factorize_predictions.integer <- function(predictions, outcome_levels) {
  expected_levels <- seq_along(outcome_levels)
  if (any(!(predictions %in% expected_levels))) {
    rlang::abort(
      "Predicted classes are outside the range of the training training data."
    )
  }

  return(
    factor(
      predictions,
      levels = seq_along(outcome_levels),
      labels = outcome_levels
    )
  )
}

# TODO: In theory we could deal with it if the output is a set of probabilities.


#' @export
#' @rdname dot-factorize_predictions
.factorize_predictions.factor <- function(predictions, outcome_levels) {
  return(
    .factorize_predictions(
      as.character(predictions),
      outcome_levels
    )
  )
}

#' @export
#' @rdname dot-factorize_predictions
.factorize_predictions.character <- function(predictions, outcome_levels) {
  if (any(!(predictions %in% outcome_levels))) {
    rlang::abort(
      "Predicted classes are outside the range of the training training data."
    )
  }

  return(
    factor(
      predictions,
      levels = outcome_levels
    )
  )
}
