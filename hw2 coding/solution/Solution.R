require(data.table)
library(caret)
library(biglm)

#Q1a
# return: string(“cylinders”, “displacement”, “horsepower”, “weight”, “acceleration”, “year”, “origin”), 
# represents the covariate providing the best prediction
slr <- function(path='../data/hw23R-linear.txt'){
  
  
  linear.df<<- read.csv(path, header = TRUE, stringsAsFactors = FALSE)
  
  plot(linear.df$cylinders, linear.df$mpg, main = "mpg~cylinders", xlab = "Number of cylinders", ylab = "Miles per gallon", pch= 20)
  cylinder.lm<- lm(linear.df$mpg~linear.df$cylinders)
  abline(cylinder.lm, col='blue')
  
  
  plot(linear.df$displacement, linear.df$mpg, main = "mpg~displacement", xlab = "Engine displacement (cu. inches)", ylab = "Miles per gallon", pch= 20)
  displacement.lm<- lm(linear.df$mpg~linear.df$displacement)
  abline(displacement.lm, col='blue')
  
  
  plot(linear.df$horsepower, linear.df$mpg, main = "mpg~horsepower", xlab = "Engine horsepower", ylab = "Miles per gallon", pch= 20)
  horsepower.lm<- lm(linear.df$mpg~linear.df$horsepower)
  abline(horsepower.lm, col='blue')
  
  
  plot(linear.df$weight, linear.df$mpg, main = "mpg~weight", xlab = "Vehicle weight (lbs.)", ylab = "Miles per gallon", pch= 20)
  weight.lm<- lm(linear.df$mpg~linear.df$weight)
  abline(weight.lm, col='blue')
  
  
  plot(linear.df$acceleration, linear.df$mpg, main = "mpg~acceleration", xlab = "Time to accelerate from 0 to 60 mph (sec.)", ylab = "Miles per gallon", pch= 20)
  acceleration.lm<- lm(linear.df$mpg~linear.df$acceleration)
  abline(acceleration.lm, col='blue')
  
  
  plot(linear.df$year, linear.df$mpg, main = "mpg~year", xlab = "Model year (modulo 100)", ylab = "Miles per gallon", pch= 20)
  year.lm<- lm(linear.df$mpg~linear.df$year)
  abline(year.lm, col='blue')
  

  plot(linear.df$origin, linear.df$mpg, main = "mpg~origin", xlab = "Origin of car (1. American, 2. European, 3. Japanese)", ylab = "Miles per gallon", pch= 20)
  origin.lm<- lm(linear.df$mpg~linear.df$origin)
  abline(origin.lm, col='blue')
  
  
  rsq_list<- list("cylinder" = summary(cylinder.lm)$r.squared,
                    "displacement" = summary(displacement.lm)$r.squared,
                    "horsepower" = summary(horsepower.lm)$r.squared,
                    "weight" = summary(weight.lm)$r.squared,
                    "acceleration"=summary(acceleration.lm)$r.squared,
                    "year" = summary(year.lm)$r.squared,
                    "origin" = summary(origin.lm)$r.squared)
  
  return(names(which.max(rsq_list)))
  
}

#Q1b 
# return: list of following variables, Intercept, CylindersCoeff, DispCoeff, HPCoeff, WeightCoeff, AccCoeff, YearCoeff, OriginCoeff
MLR <- function(path='../data/hw23R-linear.csv'){
  
  #fit multiple independent variables to a new linear model 
  mlr.lm<- lm(linear.df$mpg~ linear.df$cylinders+ linear.df$displacement + linear.df$horsepower + linear.df$weight + linear.df$acceleration + linear.df$year + linear.df$origin)
  summary(mlr.lm)
  #summarize the fitted multiple regression model to find out the regresssion coefficients 
  coef_mlr<- coef(mlr.lm)
  summary(mlr.lm)$coefficients[2,1]

  View(coef_mlr)
  # fill in the list with the coeffes you compute
  result <- list("Intercept"=summary(mlr.lm)$coefficients[1,1], "CylindersCoeff"=summary(mlr.lm)$coefficients[2,1], "DispCoeff"=summary(mlr.lm)$coefficients[3,1], "HPCoeff"=summary(mlr.lm)$coefficients[4,1], "WeightCoeff"=summary(mlr.lm)$coefficients[5,1], "AccCoeff"=summary(mlr.lm)$coefficients[6,1], "YearCoeff"=summary(mlr.lm)$coefficients[7,1], "OriginCoeff"=summary(mlr.lm)$coefficients[8,1])
  return(result)
}

#Q2
# return: list of following variables, Intercept， Lag1oeff，Lag2Coeff，Lag3Coeff, Lag4Coeff, Lag5Coeff, VolumeCoeff
LogisticRegression <- function(path='../data/hw23R-logistic.txt'){
  
  logistic.df<<- read.csv(path, header = TRUE)
  
  logistic.df$Direction<- factor(logistic.df$Direction, levels=c("Down", "Up"), ordered = TRUE)
  #calculate the number of rows for the train data in order to split the complete data by 75%
  sample_size<- floor(0.75* nrow(logistic.df))
  
  #set the value of seed to repeatedly produce the same samples
  set.seed(135)
  
  #split the data into train.data and test.data based on the sample size calculated
  training_index <- sample(seq_len(nrow(logistic.df)), size = sample_size)
  train.data<-logistic.df[training_index, ]
  test.data<- logistic.df[-training_index, ]
  
  #remove the Year column from the test data since it wont be required for the analysis
  test.data[, test.data$Year<- NULL]
  
  #fit the training data into a logit model for logisitic regression
  logit.model<- glm(Direction~ Lag1 + Lag2 +Lag3 + Lag4 + Lag5 +Volume +Today, family = binomial(link= "logit"), data= train.data)
  
  anova(logit.model)
  #summarize the training data to identify the regression coefficients
  summary(logit.model)
  
  #predict the values of Direction for the test data based on our logistic regression model
  predicted_val <- predict(logit.model, test.data, type = "response")
  
  #convert the values to binary for comparison with the observed values
  predicted_val<- ifelse(predicted_val> 0.5,1,0)
  predicted_val<- factor(predicted_val, levels=c("0", "1"), ordered = TRUE )
  
  #fill the values of Direction in a new variable so that the levels for comparison of test.data and predicted_val remains the same
  comp_direction<- ifelse(test.data$Direction == "Up" ,1,0)
  comp_direction<- factor(comp_direction, levels=c("0", "1"), ordered = TRUE )
  
  confusionMatrix(comp_direction, predicted_val)
  
  
  # fill in the list with the coeffes you compute
  result <- list("Intercept" = summary(logit.model)$coefficients[1,1] ,"Lag1Coeff" = summary(logit.model)$coefficients[2,1], "Lag2Coeff" = summary(logit.model)$coefficients[3,1], "Lag3Coeff" = summary(logit.model)$coefficients[4,1] ,"Lag4Coeff" = summary(logit.model)$coefficients[5,1], "Lag5Coeff" = summary(logit.model)$coefficients[6,1],"VolumeCoeff"= summary(logit.model)$coefficients[7,1], "TodayCoeff" = summary(logit.model)$coefficients[8,1])
  return(result)
}

#Q3
# return: float of training accuracy 
LogisticRegressionImproved <- function(path='../data/hw23R-logistic.txt'){
  logistic.df<<- read.csv(path, header = TRUE)
  
  logistic.df$Direction<- factor(logistic.df$Direction, levels=c("Down", "Up"), ordered = TRUE)
  #calculate the number of rows for the train data in order to split the complete data by 75%
  sample_size<- floor(0.75* nrow(logistic.df))
  
  #set the value of seed to repeatedly produce the same samples
  set.seed(135)
  
  #split the data into train.data and test.data based on the sample size calculated
  training_index <- sample(seq_len(nrow(logistic.df)), size = sample_size)
  train.data<-logistic.df[training_index, ]
  test.data<- logistic.df[-training_index, ]
  
  #remove the Year column from the test data since it wont be required for the analysis
  test.data[, test.data$Year<- NULL]
  
  #fit the training data into a logit model for logisitic regression
  logit.model<- glm(Direction~ Lag1 + Today, family = binomial(link= "logit"), data= train.data)
  
  anova(logit.model)
  #summarize the training data to identify the regression coefficients
  summary(logit.model)
  
  #predict the values of Direction for the test data based on our logistic regression model
  predicted_val <- predict(logit.model, test.data, type = "response")
  
  #convert the values to binary for comparison with the observed values
  predicted_val<- ifelse(predicted_val> 0.5,1,0)
  predicted_val<- factor(predicted_val, levels=c("0", "1"), ordered = TRUE )
  
  #fill the values of Direction in a new variable so that the levels for comparison of test.data and predicted_val remains the same
  comp_direction<- ifelse(test.data$Direction == "Up" ,1,0)
  comp_direction<- factor(comp_direction, levels=c("0", "1"), ordered = TRUE )
  
  cm<- confusionMatrix(comp_direction, predicted_val)
  
  return(cm$overall['Accuracy'])
  
}

#Q4
# return: list of two variables, Intercept， xCoeff
Bigslr <- function(path='../data/slr-90m-data.csv'){
  
  big_data<- fread(path, header = TRUE)
  
  #fit the big data into a linear model
  bigd_model<- bigglm(y~x, data = big_data)
  
  #extract all the coefficients into a list
  coef_list<- coef(bigd_model)
  
  set.seed(123)
  #divide the dataset into samples of 1%, 2%, 3%, 4%, 5% and store them in seprate tables
  sample1<-big_data[sample(nrow(big_data), size= floor(0.01* nrow(big_data)), replace = F), ]
  sample2<-big_data[sample(nrow(big_data), size= floor(0.02* nrow(big_data)), replace = F), ]
  sample3<-big_data[sample(nrow(big_data), size= floor(0.03* nrow(big_data)), replace = F), ]
  sample4<-big_data[sample(nrow(big_data), size= floor(0.04* nrow(big_data)), replace = F), ]
  sample5<-big_data[sample(nrow(big_data), size= floor(0.05* nrow(big_data)), replace = F), ]
  
  #fit the values of these 5 samples in 5 different linear regression models
  lm1<- lm(y~x, data = sample1)
  lm2<- lm(y~x, data = sample2)
  lm3<- lm(y~x, data = sample3)
  lm4<- lm(y~x, data = sample4)
  lm5<- lm(y~x, data = sample5)
  
  #plot the lines of respective graphs with the legend
  plot(big_data$x,big_data$y,xlab="x", ylab="y", type = "n")
  abline(bigd_model, col="grey", lty=1)
  abline(lm1, col= "blue", lty=2)
  abline(lm2, col= "green", lty=3)
  abline(lm3, col= "red", lty=4)
  abline(lm4, col= "yellow", lty=5)
  abline(lm5, col= "pink", lty=6)
  
  legend("topleft", c("Entire Dataset", "Sample 1", "Sample 2", "Sample 3", "Sample 4", "Sample 5"), col= c("grey", "blue", "green", "red", "yellow", "pink"), lty= c(1,1,1,1,1,1))

  # fill in the list with the coeffes you compute
  # from the next line, you should infer that model should be the variable name of your model
  result <- list("Intercept"= summary(bigd_model)$mat[1,1], "xCoeff"= summary(bigd_model)$mat[2,1])
  return(result)
}

#Q5
# return: string ("reject" to reject null hypothesis, "not-reject" to not reject null hypothesis)
ZTest <- function(x, test_type, alpha, pop_mean, pop_sd){

  #calculate the z value using the given data
  z_val<- (mean(x)-pop_mean)/ (pop_sd/sqrt(length(x)))
  
  #based on the hypothesis test type, compute the critical value and compare it with the z value
  if(test_type== "left-tailed"){
    critical_val<- qnorm(alpha)
    if(z_val< critical_val){
      return("reject")
    }
    else return("not-reject")
  }
  
  if(test_type== "right-tailed"){
    critical_val<- qnorm(1-alpha)
    
    if(z_val > critical_val){
      return("reject")
    }
    else return("not-reject")
  }
  
  if(test_type == "two-tailed"){
    critical_val_left<- qnorm(alpha/2)
    critical_val_right<- qnorm(1- (alpha/2))
    
    if(z_val< critical_val_left || z_val> critical_val_right){
      return("reject")
    }
    else return("not-reject")
  }
  
}

#Q6
# populationDistribution: string('uniform','normal')
# sampleSize: integer (~30)
# numberOfSamples: integer (>100)
# return: list of two variables, mean and se
CLT <- function(populationDistribution, sampleSize, numberOfSamples){
  
  if(populationDistribution == "uniform"){
    #generate random variables for uniform population distribution
    unif_data<- runif(sampleSize*numberOfSamples)
    
    #model the data into a table
    unif_data_table<- matrix(unif_data, numberOfSamples)
    
    #calculate the mean of each row of the table and store it in a vector
    mean_sample<- rowMeans(unif_data_table)
    
    #draw a histogram to show the distribution of the mean of all the samples
    hist(mean_sample, main = "Sample Mean Distribution- Uniform Population")
    
    #theoretical mean and standard deviation of population
    mean_pop<- mean(unif_data)
    sd_pop<- sd(unif_data)
    
    #calculate mean of the sample means
    mean_mean_sample<- mean(mean_sample)
    
    #calculate the standard deviation of the standard means
    sd_mean_sample<- sd(mean_sample)
    
    #calculate the standard error of means
    standard_error<- sd_pop/sqrt(numberOfSamples)
    
  }
  
  if(populationDistribution == "normal"){
    #generate random variables for normal population distribution
    norm_data<- rnorm(sampleSize*numberOfSamples)
    
    #model the data into a table
    norm_data_table<- matrix(norm_data, numberOfSamples)
    
    #calculate the mean of each row of the table and store it in a vector
    mean_sample<- rowMeans(norm_data_table)
    
    #draw a histogram to show the distribution of the mean of all the samples
    hist(mean_sample, main = "Sample Mean Distribution- Normal Population")
    
    #theoretical mean and standard deviation of population
    mean_pop<- mean(norm_data)
    sd_pop<- sd(norm_data)
    
    #calculate mean of the sample means
    mean_mean_sample<- mean(mean_sample)
    
    #calculate the standard deviation of the standard means
    sd_mean_sample<- sd(mean_sample)
    
    #calculate the standard error of means
    standard_error<- sd_pop/sqrt(numberOfSamples)
    
  }
  # fill in the list with the mean and std you compute
  result <- list("mean"=mean_mean_sample, "se"=standard_error)
  return(result)
}

