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

# It seems easiest to test the whole enchilada, since everything relies on one
# another.

test_that("Can fit and predict bespoke_classifications", {
  oil_fit_formula <- bespoke_classification(
    class ~ .,
    oils,
    fn = random_baseline,
    n_classes = 7
  )

  oil_fit_df <- bespoke_classification(
    oils[, 1:7],
    oils$class,
    fn = random_baseline,
    n_classes = 7
  )

  oil_fit_matrix <- bespoke_classification(
    as.matrix(oils[, 1:7]),
    oils$class,
    fn = random_baseline,
    n_classes = 7
  )

  oil_fit_recipe <- bespoke_classification(
    recipes::recipe(
      class ~ .,
      oils
    ),
    oils,
    fn = random_baseline,
    n_classes = 7
  )

  # All of them should give the same output given the same random seed, in
  # theory.
  set.seed(4242L)
  pred_formula <- predict(oil_fit_formula, to_predict)
  set.seed(4242L)
  pred_df <- predict(oil_fit_df, to_predict)
  set.seed(4242L)
  pred_matrix <- predict(oil_fit_matrix, to_predict)
  set.seed(4242L)
  pred_recipe <- predict(oil_fit_recipe, to_predict)

  expect_identical(pred_df, pred_formula)
  expect_identical(pred_matrix, pred_formula)
  expect_identical(pred_recipe, pred_formula)

  expect_snapshot(
    {
      set.seed(4242L)
      predict(oil_fit_formula, to_predict, type = "prob")
    }
  )
})

test_that("Expected failure modes fail as expected.", {
  expect_error(
    bespoke_classification(
      1L
    ),
    "not defined for a 'integer'"
  )
})

test_that("Function outputs are validated.", {
  expect_error(
    {
      oil_fit_corn <- bespoke_classification(
        class ~ .,
        oils,
        fn = always_corn
      )
    },
    NA
  )

  expect_error(
    {
      oil_fit_corn_factor <- bespoke_classification(
        class ~ .,
        oils,
        fn = always_corn_factor
      )
    },
    NA
  )

  expect_error(
    bespoke_classification(
      class ~ .,
      oils,
      fn = always_bad
    ),
    "outside the range of the training"
  )

  expect_error(
    bespoke_classification(
      class ~ .,
      oils,
      fn = bad_length
    ),
    "must match the size of"
  )

  expect_error(
    bespoke_classification(
      class ~ .,
      oils,
      fn = always_12
    ),
    "outside the range of the training"
  )

  expect_error(
    bespoke_classification(
      class ~ .,
      oils,
      fn = always_weird
    ),
    "not coercible"
  )

  expect_snapshot(
    {
      set.seed(4242L)
      predict(oil_fit_corn, to_predict)
    }
  )

  expect_snapshot(
    {
      set.seed(4242L)
      predict(oil_fit_corn_factor, to_predict)
    }
  )
})

test_that("It also works in a parsnip context.", {
  bespoke_spec <- bespoke(fn = random_baseline) %>%
    parsnip::set_engine("bespoke", n_classes = 7)

  bespoke_fit <- bespoke_spec %>%
    parsnip::fit(class ~ ., oils)

  expect_snapshot(
    {
      set.seed(4242L)
      predict(bespoke_fit, new_data = to_predict, type = "class")
    }
  )
  expect_snapshot(
    {
      set.seed(4242L)
      predict(bespoke_fit, new_data = to_predict, type = "prob")
    }
  )
})

test_that("The parsnip context also fails gracefully.", {
  expect_error(
    bespoke(mode = "fubar"),
    "`mode` should be 'classification'"
  )
})
