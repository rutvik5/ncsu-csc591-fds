require(data.table)

#Q1a
# return: string(“cylinders”, “displacement”, “horsepower”, “weight”, “acceleration”, “year”, “origin”), 
# represents the covariate providing the best prediction
SLR <- function(path='../data/hw23R-linear.txt'){

}

#Q1b 
# return: list of following variables, Intercept, CylindersCoeff, DispCoeff, HPCoeff, WeightCoeff, AccCoeff, YearCoeff, OriginCoeff
MLR <- function(path='../data/hw23R-linear.csv'){

  # fill in the list with the coeffes you compute
  result <- list("Intercept"=, "CylindersCoeff"=, "DispCoeff"=, "HPCoeff"=, "WeightCoeff"=, "AccCoeff"=, "YearCoeff"=, "OriginCoeff"=)
  return(result)
}
  
#Q2
# return: list of following variables, Intercept， Lag1oeff，Lag2Coeff，Lag3Coeff, Lag4Coeff, Lag5Coeff, VolumeCoeff
LogisticRegression <- function(path='../data/hw23R-logistic.txt'){

  # fill in the list with the coeffes you compute
  result <- list("Intercept" = ,"Lag1Coeff" =, "Lag2Coeff" =, "Lag3Coeff" =,"Lag4Coeff" =, "Lag5Coeff" =,"VolumeCoeff"=  )
  return(result)
}

#Q3
# return: float of training accuracy 
LogisticRegressionImproved <- function(path='../data/hw23R-logistic.txt'){

}

#Q4
# return: list of two variables, Intercept， xCoeff
BigSLR <- function(path='../data/slr-90m-data.csv'){
  
  # fill in the list with the coeffes you compute
  # from the next line, you should infer that model should be the variable name of your model
  result <- list("Intercept"=model$coefficients[1], "xCoeff"=)
  return(result)
}

#Q5
# return: string ("reject" to reject null hypothesis, "not-reject" to not reject null hypothesis)
ZTest <- function(x, test_type, alpha, pop_mean, pop_sd){
  
}

#Q6
# populationDistribution: string('uniform','normal')
# sampleSize: integer (~30)
# numberOfSamples: integer (>100)
# return: list of two variables, mean and se
CLT <- function(populationDistribution, sampleSize, numberOfSamples){
  
  # fill in the list with the mean and std you compute
  result <- list("mean"=, "se"=)
  return(result)
}

