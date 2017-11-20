#Elsa MArques R Studio exercice 3 

#Load libraries that will be used further on
library(dplyr)
library(ggplot2)
library(GGally)


#Load the data stored locally on the project folder 
read.csv("student-mat.csv")
read.csv("student-por.csv")

#Make the vectors student-mat and student-por
student-mat <- read.table("student-mat.csv", sep = ";", header = TRUE)
student-por <- read.table("student-por.csv", sep = ";" , header = TRUE)


#View student-mat structure and dimensions 
str(student-mat)
dim(student-mat)


#View student-por structure and dimensions 
str(student-por)
head(student-por)
dim(student-por)

#Join the two data matrices by leaving behind NA data
#Use the arguments "school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet"
inner_joint(student-por, student-mat, c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet"))
