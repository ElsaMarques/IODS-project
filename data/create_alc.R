#Elsa Marques - R Studio exercice 3 

#Load libraries that will be used further on
library(dplyr)
library(ggplot2)
library(GGally)
library(tidyr)

#Set the working directory
setwd("~/Documents/GitHub/IODS-project")

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

#Use the joined data to create a new data frame
alc <- select(math_por, one_of(join_by))

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

# average weekday and weekend alcohol consumption to create a new column 'alc_use'
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# 'alc_use' used to create a new logical column 'high_use' (students with 'alc_use' > 2)
alc <- mutate(alc, high_use = alc_use > 2)

# gather columns into key-value pairs and then glimpse the resulting data
gather(alc) %>% glimpse()

# produce summary statistics by group
alc %>% group_by(sex, high_use) %>% summarise(count = n(), mean_grade = mean(G3))

#save the combined data as .CSV
write.csv(alc, file = "student-combined.csv" )

#check you can read the data back
read.csv("student-combined.csv")





