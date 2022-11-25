#function to calcualte isothermal layer depth (ILD)
fun <- function(x) {
  if (all(is.na(x))){
    NA
  } else{
    dep[which.min(abs(x[1] - x[2:40] - 0.5))]
  }
} 