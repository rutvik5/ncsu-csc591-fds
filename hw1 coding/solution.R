
#Question 1
#create a new data frame named 'data.df'
data.df<- data.frame("PassengerId", "Survived", "Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Ticket", "Fare", "Cabin", "Embarked")

#add the csv file into the data frame
data.df<- read.csv("train.csv", header=TRUE, stringsAsFactors = FALSE)


#Question 2
#store the number of rows in data.df.n_rows variable
data.df.n_rows<- nrow(data.df)

#store the number of columns in data.df.n_cols variable
data.df.n_cols<- ncol(data.df)


#Question 3
#subset the required columns and store them into 'data.df.subset' data frame
data.df.subset<- data.df[, c("PassengerId", "Age", "Fare", "Embarked")]

#Question 4
# a)
#calculate the meadian for the age column
age.meadian<- median(data.df.subset[,c("Age")], na.rm= TRUE)

#replace the NA values of Age column by its median
data.df.subset$Age[is.na(data.df.subset$Age)]<- age.meadian

# b)
#calulate the mode value of embarked attribute by calculating the maximum number of occurences of the values of Embarked attribute
embarked.mode<- max(data.df.subset$Embarked)

#replace the null values in embarked attribute with its mode
data.df.subset$Embarked[which(data.df.subset$Embarked=="")]<- embarked.mode

# c)
#There are some variables in the Fare attribute which contain the value=0
#Therefore, we must replace such values with the mean of the Fare attribute.
#We should use the mean as it will give us a balanced value of fare, without changing the characteristic of the fare attribute to a large extent

#calculate mean of fare
fare.mean<- mean(data.df.subset$Fare)

#replace the missing values from the fare attribute
data.df.subset$Fare[which(data.df.subset$Fare== 0 | is.na(data.df.subset$Fare) | data.df.subset$Fare== "" )]<- fare.mean


#Question 5

# a)
#create a histogram for Age
hist(data.df.subset$Age, xlab= "Age", main= "Histogram of Age")

# b)
#create a scatter plot for Age vs Fare
plot(data.df.subset$Age, data.df.subset$Fare, xlab= "Age", ylab= "Fare", main= "Age vs Fare")


#Question 6

#Calculate the standard deviation and mean for age
age.sd<- sd(data.df.subset$Age)
age.mean<- mean(data.df.subset$Age)

#Storing the values of PassengerId in the anomalous_indices vector by checking if the value of age is = mean+- 2* standard deviation
anomalous_indices<- data.df.subset$PassengerId[data.df.subset$Age > age.mean + 2* age.sd | data.df.subset$Age < age.mean - 2* age.sd ]


#Question 7
# a)
#Storing all the rows with age between 25 and 80(both included) in  a new data frame named data.df.subset.v2
data.df.subset.v2<- data.df.subset[data.df.subset$Age >= 25 & data.df.subset$Age <= 80, ]

# b)
#Subset the new data frame to only include the attributes- Age, Fare and Embarked
data.df.subset.v2<- data.df.subset.v2[, c("Age", "Fare", "Embarked")]


#Question 8
#We include the scales package to use its rescale function
library(scales)

#Rescale the column Fare using the rescale function to which we pass the values of Fare and set the limits from 0 to 100
data.df.subset.v2$Fare_Rescaled<- rescale(data.df.subset.v2$Fare, to = c(0,100))

