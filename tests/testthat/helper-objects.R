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

random_baseline <- function(new_data, n_classes) {
  # Return a number between 1 and n_classes for each row of input.
  return(
    sample(seq_len(n_classes), nrow(new_data), replace = TRUE)
  )
}

oils <- modeldata::oils

to_predict <- oils[1:7]

always_corn <- function(new_data) {
  return(
    rep(
      "corn",
      nrow(new_data)
    )
  )
}

always_corn_factor <- function(new_data) {
  return(
    factor(always_corn(new_data))
  )
}

always_bad <- function(new_data) {
  return(
    rep(
      "bad",
      nrow(new_data)
    )
  )
}

bad_length <- function(new_data) {
  return(
    rep(
      "bad",
      nrow(new_data) + 1L
    )
  )
}

always_12 <- function(new_data) {
  return(
    rep(
      12L,
      nrow(new_data)
    )
  )
}

always_weird <- function(new_data) {
  return(
    rep(
      12.1,
      nrow(new_data)
    )
  )
}
