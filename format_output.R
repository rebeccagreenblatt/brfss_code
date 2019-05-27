format_output <- function(model){ 
  results <- as.data.frame(cbind(exp(coefficients(model)), exp(confint(model))))
  
  if (!is.na(results["AGE",][1])) {
    results["AGE",] <- exp(10*log(results["AGE",]))
    
  }
  
  one_star <- exp(confint(model, level = 0.95))
  two_stars <- exp(confint(model, level = 0.999))
  
  combin <- paste0(format(round(results[,1], 2), nsmall=2), " (", 
                   format(round(results[,2], 2), nsmall=2), ", ", 
                   format(round(results[,3], 2), nsmall=2), ")",
                   ifelse((two_stars[,1] > 1 & two_stars[,2] > 1) | 
                            (two_stars[,1] < 1 & two_stars[,2] < 1), "**", 
                          ifelse(one_star[,1] > 1 & one_star[,2] > 1 | 
                                   one_star[,1] < 1 & one_star[,2] < 1, "*", "")))
  
  return(combin)
}