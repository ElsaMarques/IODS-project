#Elsa Marques - RStudio exercice 4 = Data wrangling for Week5
#Dimmensionality reduction techniques

#Set the working directory for my GitHub folder 
setwd("~/Documents/GitHub/IODS-project")

#Load human development data 
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

#informations about this data set can be found here
#http://hdr.undp.org/en/content/human-development-index-hdi
#The HDI was created to emphasize that people and their capabilities should be the ultimate criteria for assessing the development of a country, not economic growth alone.

#Load Gender inequality data
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

#informations about this data set can be found here
#http://hdr.undp.org/sites/default/files/hdr2015_technical_notes.pdf
#Gender Development Index (GDI) measures gender ine- qualities in achievement in three basic dimensions of human development: health, measured by female and male life expec- tancy at birth; education, measured by female and male expect- ed years of schooling for children and female and male mean years of schooling for adults ages 25 and older; and command over economic resources, measured by female and male estimat- ed earned income.

#See the structure and dimensions of both datasets.
str(hd)
dim(hd)

str(gii)
dim(gii)

#Create summaries of the variables.
summary(hd)
summary(gii)

#rename the variables with (shorter) descriptive names.
library(dplyr)
colnames(gii) <- c("GII.Rank", "Country", "GII", "Mat.Mor" , "Ado.Birth" , "Parli.F", "Edu2.F" , "Edu2.M" , "Labo.F" , "Labo.M" )
colnames(hd) <- c("HDI.Rank", "Country", "HDI", "Life.Exp", "Edu.Exp", "Edu.Mean", "GNI", "GNI.Minus.Rank")                  

#Mutate the “Gender inequality” data and create two new variables.
#ratio of Female and Male populations with secondary education in each country. 
gii <- mutate(gii, Edu2.FM = Edu2.F / Edu2.M)

#ratio of labour force participation of females and males in each country.
gii <- mutate(gii, Labo.FM  = Labo.F / Labo.M)

#Call the new joined data "human"
#Join the two datasets using the variable Country in both datasets as the identifier. 
human <- inner_join(hd, gii, "Country")

#The joined data should have 195 observations and 19 variables. 

#Save the new dataset in my data folder.
write.csv(human, file = "human.csv")

#Test if you can read back the saved new joined combined dataset
read.csv("human.csv")




#RStudio exercise 5 from here onwards

#Transform the Gross National Income (GNI) variable to numeric 
library(tidyverse)
library(dplyr)
library(stringr)

#Check that human data is properly loaded from previous wrangling
read.csv("human.csv")

#Check the structure of "GNI"
str(human$GNI)

#Remove the comas in the GNI data and use the pipe to save it as numeric
human$GNI <- str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric()
human$GNI    

#Exclude unneeded variables:
keep <- c("Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")
human <- select(human, one_of(keep))
human

#Remove all rows with missing values
#Print out a completeness indicator of the 'human' data
complete.cases(human)

#Print out the data along with a completeness indicator as the last column
data.frame(human[-1], comp = complete.cases(human))

#Filter out all rows with NA values
human_ <- filter(human, complete.cases(human))
human_

#Remove the observations which relate to regions instead of countries. 
# look at the last 10 observations of human
tail(human_, n = 10)

# define the last indice we want to keep
last <- nrow(human_) - 7

# choose everything until the last 7 observations
human_ <- human[1:last, ]

#Define the row names of the data by the country names
row.names(human_) <- human_$Country

#Remove the country name column from the data. 
human_ <- select(human_, -Country)

#View the new data frame "human_"
View(human_)

#The dataset "human_" has now 155 observations of 8 variables. 

#Save the human data in your data folder including the row names. 
write.csv(human_, file = "human_.csv", row.names = TRUE)

#Check if you can read it back 
read.csv("human_.csv")
