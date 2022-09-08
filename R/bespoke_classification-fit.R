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

#' Fit a `bespoke_classification`
#'
#' `bespoke_classification()` produces a fitted "model," where the model is
#' simply a user-supplied function. Note that, despite appearances, the "model"
#' is *not* actually trained to fit the data; it is only put into the context of
#' a "fitted" model in order to play nice with other tidymodels functions. It
#' will, however, return a member of the training outcomes for each input during
#' prediction.
#'
#' @param x Depending on the context:
#'
#'   * A __data frame__ of predictors.
#'   * A __matrix__ of predictors.
#'   * A __recipe__ specifying a set of preprocessing steps created from
#'   [recipes::recipe()].
#'
#' @param y When `x` is a __data frame__ or __matrix__, `y` is the outcome
#'   specified as:
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
#'   and the predictor terms on the right-hand side.
#'
#' @param fn A function that takes a data.frame as input and returns *a vector*
#'   (integer, character, or factor) indicating the outcomes as output (with one
#'   value per input row). We may someday extend this for probabilities.
#'
#' @param ... Additional parameters passed on to the model "function."
#'
#' @return
#'
#' A `bespoke_classification` model object.
#'
#' @export
bespoke_classification <- function(x, ...) {
  UseMethod("bespoke_classification")

  # TODO: Ideally all methods should leave character vectors in the predictors
  # as-is, but right now that would break, so we aren't implementing that yet.
}

#' @export
#' @rdname bespoke_classification
bespoke_classification.default <- function(x, ...) {
  stop(
    "`bespoke_classification()` is not defined for a '",
    class(x)[1],
    "'.",
    call. = FALSE
  )
}

# XY method - data frame

#' @export
#' @rdname bespoke_classification
bespoke_classification.data.frame <- function(x, y, fn, ...) {
  processed <- hardhat::mold(x, y)
  return(
    .bespoke_classification_bridge(processed, fn = fn, ...)
  )
}

# XY method - matrix

#' @export
#' @rdname bespoke_classification
bespoke_classification.matrix <- function(x, y, fn, ...) {
  processed <- hardhat::mold(x, y)
  .bespoke_classification_bridge(processed, fn = fn, ...)
}

# Formula method

#' @export
#' @rdname bespoke_classification
bespoke_classification.formula <- function(formula, data, fn, ...) {
  processed <- hardhat::mold(formula, data)
  .bespoke_classification_bridge(processed, fn = fn, ...)
}

# Recipe method

#' @export
#' @rdname bespoke_classification
bespoke_classification.recipe <- function(x, data, fn, ...) {
  processed <- hardhat::mold(x, data)
  .bespoke_classification_bridge(processed, fn = fn, ...)
}

# ------------------------------------------------------------------------------
# Bridge

#' Create the "Model"
#'
#' This bridge is the same for each path into bespoke_classification. It takes
#' the processed predictors (which are now a tibble) and outcome (which is now a
#' factor) and, in this case, makes sure they make sense with the supplied
#' function.
#'
#' @inheritParams .bespoke_classification_impl
#' @param processed Processed inputs molded by hardhat.
#'
#' @return A `bespoke_classification` model object.
#' @keywords internal
.bespoke_classification_bridge <- function(processed, fn, ...) {
  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  # We don't have to actually "fit" this, but I left in the "fit" step to
  # demonstrate the hardhat format. Really all this "fit" function does is
  # validate that the data makes sense with the function.
  fit <- .bespoke_classification_impl(
    predictors, outcome, fn = fn, ...
  )

  .new_bespoke_classification(
    fn = fit$fn,
    dots = fit$dots,
    outcome_levels = fit$outcome_levels,
    blueprint = processed$blueprint
  )
}


# ------------------------------------------------------------------------------
# Implementation

#' Validate the Model Function
#'
#' Normally this would be the place where the actual fit happens. Here, we
#' simply validate that the function makes sense with the input data.
#'
#' @param predictors A tibble of predictors.
#' @param outcome A factor of output classes.
#' @param fn A function that will generate "predictions" from predictors.
#' @param ... Additional parameters passed on to `fn`.
#'
#' @return A list with elements `fn`, `dots`, and `outcome_levels`.
#' @keywords internal
.bespoke_classification_impl <- function(predictors, outcome, fn, ...) {
  # Log the extra parameters that we've been passing along.
  dots <- rlang::list2(...)

  # I'm going to need these levels. I'm rather surprised hardhat doesn't do this
  # by default.
  outcome_levels <- levels(outcome)

  # Run a "prediction" to make sure things make sense. These will throw errors
  # if something is wrong.
  predictions <- fn(predictors, ...)

  # Confirm that the number of predictions matches expectations.
  hardhat::validate_prediction_size(predictions, predictors)

  # Confirm that the predictions can be coerced to match the known outcomes.
  predictions <- .factorize_predictions(predictions, outcome_levels)

  return(
    list(
      fn = fn,
      dots = dots,
      outcome_levels = outcome_levels
    )
  )
}
