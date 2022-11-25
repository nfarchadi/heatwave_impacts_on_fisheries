# function to calculate eddy kinetic energy (eke)

calculate_eke <- function(x) {
  (x[[1]] ^ 2 + x[[2]] ^ 2) / 2
}