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

#' Construct a Bespoke Classification Model Object
#'
#' @param fn A user-supplied function.
#' @param dots Additional parameters to pass onto the function.
#' @param outcome_levels The levels that were present in the training data.
#' @param blueprint The `{hardhat}` blueprint to prepare the predictors.
#'
#' @return A [hardhat::new_model()].
#' @keywords internal
.new_bespoke_class <- function(fn, dots, outcome_levels, blueprint) {
  hardhat::new_model(
    fn = fn,
    dots = dots,
    outcome_levels = outcome_levels,
    blueprint = blueprint,
    class = "bespoke_class"
  )
}
