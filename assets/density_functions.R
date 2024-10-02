
rgbeta <- function(n, 
                   mean, 
                   var, 
                   min = 0, 
                   max = 1, 
                   player = "") {
  dmin <- mean - min
  dmax <- max - mean
  
  if (nchar(player) > 0) {
    print(glue("modeling {player}"))
    print(var)
    print(dmin*dmax)
  }
  
  if (var > (dmin * dmax)) {
    print("fixing var")
    # print(var)
    # print((dmin * dmax))
    var <- (dmin * dmax) -.001
    print(var)
  }
  
  if (min == max){
    # in cases where the min and max rank are the same
    max <- max+1
    mean <- mean+(max-min)/2
    
    dmin <- mean - min
    dmax <- max - mean
  }
  
  if (dmin <= 0 || dmax <= 0){
    stop(paste("mean must be between min =", min, "and max =", max)) 
  }
  
  if (var >= dmin * dmax){
    stop(paste("var must be less than (mean - min) * (max - mean) =", dmin * dmax))
  }
  
  # mean and variance of the standard beta distributed variable
  mx <- (mean - min) / (max - min)
  vx <- var / (max - min)^2
  
  # find the corresponding alpha-beta parameterization
  a <- ((1 - mx) / vx - 1 / mx) * mx^2
  b <- a * (1 / mx - 1)
  
  # generate standard beta observations and transform
  x <- rbeta(n, a, b)
  y <- (max - min) * x + min
  
  return(y)
}
