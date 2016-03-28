AR.model <- function(
  previous = 1,  
  coefficients = c(0.5, 0.8),
  initVals = c(1), 
  times = 200 
  # default parameters stand for:
  # X(t) = 0.5 + 0.8*X(t-1) + error(t)
  # <=> (in matrix form) 
  #                               |-   -|
  #                               | 0.5 |
  #       [1, X(t-1), error(t)] * | 0.8 |  
  #                               |  1  |
  #                               |-   -|
  #
  # with X(1) = 1
){ 
  coefficients <- c(coefficients, 1) # constant 1 is for the noise item
  errors <- rnorm(times) # for white noise series
  
  series <- initVals
  for (start in 1:times) {
    end <- start+previous-1
    previousVals <- series[end:start]
    result <- as.vector(t(coefficients)%*%c(1, previousVals, errors[start]))
    series <- c(series, result)
  }
  
  plot(series, type = "l")
  abline(h = mean(series), col = "red")
}

AR.customized <- function(){
  input <- readline(prompt="Enter the most previous Subscript: ")
  previous <- as.integer(input)
  
  coefficients <- vector()
  input <- readline(prompt="Enter the value of constant: ")
  coefficients <- c(coefficients, as.integer(input))
  for (itr in 1:previous) {
    prompt <-  paste("Enter the coefficient for the X(t-", toString(itr,width = NULL), "): ", sep = "")
    input <- readline(prompt= prompt)
    coefficients <- c(coefficients, as.integer(input))
  }
  
  initVals <- vector()
  for (itr in 1:previous) {
    prompt <- paste("Enter the initial value for the X(", toString(itr,width = NULL), "): ", sep = "")
    input <- readline(prompt=prompt)
    initVals <- c(initVals, as.integer(input))
  }
  
  input <- readline(prompt="Times to recur: ")
  times <- as.integer(input)
  AR.model(previous, coefficients, initVals, times)
}
main <- function(){
    cat("+++++++++++++++\n")
    input <- readline(prompt="Use the default parameters, Y/N? :")
    choose <- tolower(toString(input))
    if(choose == "y"||choose == "yes" ){
      AR.model()
    }else{
      AR.customized()
    }
    cat("+++++++++++++++")
}
main()



