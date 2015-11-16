insert_minor <- function(major_labs, n_minor) {
  labs <- c(sapply( major_labs, function(x) c(x, rep("", 4) ) ) )
  labs[1:(length(labs)-n_minor)]
  }