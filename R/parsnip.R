#' Set Up Bespoke for Parsnip
#'
#' @return Nothing that's intentional.
#' @keywords internal
.make_bespoke <- function() {
  parsnip::set_new_model("bespoke")
  parsnip::set_model_mode(model = "bespoke", mode = "classification")
  # parsnip::set_model_mode(model = "bespoke", mode = "regression")

  parsnip::set_model_engine(
    model = "bespoke",
    mode = "classification",
    eng = "bespoke"
  )

  # Consider getting fancy with parsnip::set_dependency when users require other
  # packages.

  # set_model_arg is for tunable parameters. Right now it doesn't make sense for
  # us to have any.

  parsnip::set_model_arg(
    model = "bespoke",
    eng = "bespoke",
    parsnip = "fn",
    original = "fn",
    func = list(pkg = "no", fun = "no"), # Can't be tuned.
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "bespoke",
    mode = "classification",
    eng = "bespoke",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(fun = "bespoke_class"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "bespoke",
    mode = "classification",
    eng = "bespoke",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = TRUE
    )
  )

  parsnip::set_pred(
    model = "bespoke",
    mode = "classification",
    eng = "bespoke",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "class"
      )
    )
  )

  parsnip::set_pred(
    model = "bespoke",
    mode = "classification",
    eng = "bespoke",
    type = "prob",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "prob"
      )
    )
  )
}

bespoke_env <- new.env()
bespoke_env$parsnip_added <- FALSE

#' Bespoke Models
#'
#' `bespoke()` defines a model that makes predictions using a set function.
#' "Training" such a model only consists of verifying that the output is
#' consistent with the types of outputs seen in the training data; the function
#' is not tailored at all to better predict the training data. Such models can
#' be useful for establishing a basline which any true models should outperform.
#'
#' @inheritParams bespoke_class
#' @param mode A single character string for the prediction outcome mode. Online
#'   "classification" is valid.
#' @param engine A single character string specifying what computational engine
#'   to use for "fitting." Only "bespoke" is valid.
#'
#' @return A specification for a model.
#' @export
bespoke <- function(mode = "classification",
                    engine = "bespoke",
                    fn = NULL) {
  # This only makes sense if they have parsnip installed.
  rlang::check_installed("parsnip")

  # Register the model exactly once.
  if (!bespoke_env$parsnip_added) {
    .make_bespoke()
    bespoke_env$parsnip_added <- TRUE
  }

  # Check for correct mode
  if (mode  != "classification") {
    rlang::abort("`mode` should be 'classification'")
  }

  # Capture the arguments in quosures
  args <- list(
    fn = rlang::enquo(fn)
  )

  # Save some empty slots for future parts of the specification
  return(
    parsnip::new_model_spec(
      "bespoke",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  )
}
