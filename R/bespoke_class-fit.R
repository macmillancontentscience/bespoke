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

#' Fit a `bespoke_class`
#'
#' `bespoke_class()` produces a fitted "model," where the model is simply a
#' user-supplied function. Note that, despite appearances, the "model" is *not*
#' actually trained to fit the data; it is only put into the context of a
#' "fitted" model in order to play nice with other tidymodels functions. It
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
#' @param fn A function that takes a data.frame as input and returns
#'   *an integer vector* indicating the outcomes as output. We may someday
#'   extend this for probabilities, especially if later steps in tidymodels turn
#'   out to really want that.
#'
#' @param ... Additional parameters passed on to the model "function."
#'
#' @return
#'
#' A `bespoke_class` model object.
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
bespoke_class.data.frame <- function(x, y, fn, ...) {
  processed <- hardhat::mold(x, y)
  return(
    .bespoke_class_bridge(processed, fn = fn, ...)
  )
}

# XY method - matrix

#' @export
#' @rdname bespoke_class
bespoke_class.matrix <- function(x, y, fn, ...) {
  processed <- hardhat::mold(x, y)
  .bespoke_class_bridge(processed, fn = fn, ...)
}

# Formula method

#' @export
#' @rdname bespoke_class
bespoke_class.formula <- function(formula, data, fn, ...) {
  processed <- hardhat::mold(formula, data)
  .bespoke_class_bridge(processed, fn = fn, ...)
}

# Recipe method

#' @export
#' @rdname bespoke_class
bespoke_class.recipe <- function(x, data, fn, ...) {
  processed <- hardhat::mold(x, data)
  .bespoke_class_bridge(processed, fn = fn, ...)
}

# ------------------------------------------------------------------------------
# Bridge

#' Create the "Model"
#'
#' This bridge is the same for each path into bespoke_class. It takes the
#' processed predictors (which are now a tibble) and outcome (which is now a
#' factor) and, in this case, makes sure they make sense with the supplied
#' function.
#'
#' @inheritParams .bespoke_class_impl
#' @param processed Processed inputs molded by hardhat.
#'
#' @return A `bespoke_class` model object.
#' @keywords internal
.bespoke_class_bridge <- function(processed, fn, ...) {
  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  # We don't have to actually "fit" this, but I left in the "fit" step to
  # demonstrate the hardhat format. Really all this "fit" function does is
  # validate that the data makes sense with the function.
  fit <- .bespoke_class_impl(
    predictors, outcome, fn = fn, ...
  )

  .new_bespoke_class(
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
.bespoke_class_impl <- function(predictors, outcome, fn, ...) {
  # Log the extra parameters that we've been passing along.
  dots <- rlang::list2(...)

  # TODO: Validate that the function can take a row of the data and produce an
  # outcome that makes sense with the training data. It doesn't have to produce
  # the SAME outcome, but make sure it's valid. It should also know what to do
  # with the dots. Probably note the style of output and return that eventually
  # (if it already returns a class, no need to do the cleanup later; and if it
  # knows about probabilities, it'd be nice to use them).

  return(
    list(
      fn = fn,
      dots = dots,
      outcome_levels = levels(outcome)
    )
  )
}
