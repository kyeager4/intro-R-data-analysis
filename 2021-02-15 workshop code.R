#' Install the ggplot2 package -- you only need to do this once
install.packages("ggplot2")

#' Load the ggplot2 package -- do this at the start of your R session if you
#' will be using the package
library("ggplot2")


## Step 1: Get data into R ####################################################

#' Load the CSV data file into R.
mydata <- read.csv("data/Sample_Dataset_2019.csv")

  

## Step 2: Look at the data we imported into R ################################

#' View the data as a table in a pop-up window
View(mydata)

#' Look at first 5 rows (head) and last 5 rows (tail) of dataframe
head(mydata)
tail(mydata)

#' Print the names of the variables in the dataframe
names(mydata)

#' Print the variables' types
str(mydata)

#' Print min/max/median for all variables
summary(mydata)


## Step 3: Selecting columns/Access the variables within the dataset ##########

#' Use the $ operator to access a variable within the dataframe
mean(mydata$SleepTime, na.rm=TRUE)

## Step 4: Filtering/Access rows within the dataset ###########################

#' Use the subset() function to filter the rows in the dataset to just those
#' meeting a logical condition
freshmen_only <- subset(mydata, Rank==1)

## Data Analysis ##############################################################

#' Compute Pearson correlation between two variables: SleepTime and StudyTime
cor(mydata$SleepTime, mydata$StudyTime, use="pairwise.complete.obs")

#' What happens when we pass the entire dataframe object to the cor() function?
cor(mydata, use="pairwise.complete.obs")

#' We get an error message if we try to pass the entire dataframe to cor()
#' because the dataframe contains non-numeric variables.
#' Let's create a dataset with just the 4 test score variables (English, 
#' Reading, Math, Science), which can then safely be passed to cor(). 
#' The subset() function can select a range of columns using the select argument.
mydata_4cols <- subset(mydata, select=English:Science)
cor(mydata_4cols, use="pairwise.complete.obs")



#' ggplot code: create x-y scatterplot (geom_point()) with a linear regression
#' trend line added (geom_smooth(method="lm")).
ggplot(mydata, aes(x=SleepTime, y=StudyTime)) +
  geom_point() +
  geom_smooth(method="lm")


#' ggplot code: create x-y scatterplot (geom_point) with a linear regression
#' trend line added (geom_smooth(method="lm")) for each class rank group.
ggplot(mydata, aes(x=SleepTime, y=StudyTime, color=factor(Rank))) +
  geom_point() + 
  geom_smooth(method="lm", se=FALSE)


#' Fit linear regression model. The first argument to lm() is the model
#' formula, which follows the syntax:
#' y ~ x1 + x2 + ...
#' where y is the dependent variable; x1, x2, etc are independent variables
mod1 <- lm(StudyTime ~ SleepTime, data=mydata)
summary(mod1)



