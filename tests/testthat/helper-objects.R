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
