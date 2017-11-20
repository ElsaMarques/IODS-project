#Elsa MArques R Studio exercice 3 

#Load libraries that will be used further on
library(dplyr)
library(ggplot2)
library(GGally)
library(tidyr)

#Load the data stored locally on the project folder 
read("student-mat.csv", sep = ";")
read("student-por.csv")

#Make the vectors student-mat and student-por
math <- read.table("student-mat.csv", sep = ";", header = TRUE)
por <- read.table("student-por.csv", sep = ";" , header = TRUE)

#View student-mat structure and dimensions 
str(math)
dim(math)


#View student-por structure and dimensions 
str(por)
dim(por)

#Use the arguments "school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet"
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

#Join the two data matrices by leaving behind NA data
math_por <- inner_join(math, por, by = c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

#Check the names of the assembled data columns
colnames(math_por)

#Glimpse the joined data
glimpse(math_por)

#Check the structure and dimensions of the joined data
str(math_por)
dim(math_por)

#EDIT FURTHER FROM HERE ONWARDS 

#View the columns from the datasets which were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

#Print columns not used for joining
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data
glimpse(alc)


# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# initialize a plot of alcohol use
g1 <- ggplot(data = alc, aes(x = alc_use, fill = sex))

# define the plot as a bar plot and draw it
g1 + geom_bar()

# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

# initialize a plot of 'high_use'
g2 <- ggplot(data = alc, aes(x = high_use, fill = sex))

# draw a bar plot of high_use by sex
g2 + facet_wrap("sex")


# use gather() to gather columns into key-value pairs and then glimpse() at the resulting data
gather(alc) %>% glimpse

# draw a bar plot of each variable
gather(alc) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar()

# produce summary statistics by group
alc %>% group_by(sex, high_use) %>% summarise(count = n(), mean_grade = mean(G3))



# initialize a plot of high_use and G3
g1 <- ggplot(alc, aes(x = high_use, y = G3, col = sex))

# define the plot as a boxplot and draw it
g1 + geom_boxplot() + ylab("grade")

# initialise a plot of high_use and absences
g2 <- ggplot(alc, aes(x = high_use, y = absences, col = sex))

# define the plot as a boxplot and draw it
g2 + geom_boxplot() + ggtitle("Student absences by alcohol consumption and sex")



# find the model with glm()
m <- glm(high_use ~ failures + absences + sex, data = alc, family = "binomial")

# print out a summary of the model
summary(m)

# print out the coefficients of the model
coef(m)


# find the model with glm()
m <- glm(high_use ~ failures + absences + sex, data = alc, family = "binomial")

# compute odds ratios (OR)
OR <- coef(m) %>% exp

# compute confidence intervals (CI)
CI <- confint(m) %>% exp

# print out the odds ratios with their confidence intervals
cbind(OR, CI)


# fit the model
m <- glm(high_use ~ failures + absences + sex, data = alc, family = "binomial")

# predict() the probability of high_use
probabilities <- predict(m, type = "response")

# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)

# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = probability > 0.5)

# see the last ten original classes, predicted probabilities, and class predictions
select(alc, failures, absences, sex, high_use, probability, prediction) %>% head(10)

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$probability > 0.5)



# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability, y = high_use, col = prediction))

# define the geom as points and draw the plot
g + geom_point()

# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction) %>%
  prop.table() %>% addmargins () 


# the logistic regression model m and dataset alc with predictions are available

# define a loss function (mean prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# call loss_func to compute the average number of wrong predictions in the (training) data
loss_func(class = alc$high_use, prob = alc$probability)


# the logistic regression model m and dataset alc (with predictions) are available

# define a loss function (average prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# compute the average number of wrong predictions in the (training) data
loss_func(class = alc$high_use, prob = alc$probability)

# K-fold cross-validation
library(boot)
cv <- cv.glm(data = alc, cost = loss_func, glmfit = m, K = 10)

# average number of wrong predictions in the cross validation
cv$delta[1]





