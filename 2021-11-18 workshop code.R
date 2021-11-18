

## Step 0: Load R packages (if necessary). ####################################

#' If you've never installed these packages, install them; otherwise,
#' skip to loading the packages
install.packages(c("haven", "readxl", "ggplot2"))


#' Load the haven package if you want to import an SPSS, SAS, or Stata data file
library(haven)

#' Load the readxl package if you want to import an Excel (xls or xlsx) file
library(readxl)

#' Load the ggplot2 package if you want to create ggplot graphics
library(ggplot2)

## Step 1: Get data into R ####################################################

#' Import the CSV data
#'   Note: the addition of na.strings="" makes it so that blanks are
#'   correctly identified as missing values. If omitted, you'll see blanks
#'   appearing as valid/nonmissing categories in your frequency tables
mydata <- read.csv(file="Sample_Dataset_2019.csv", na.strings="")

#' Import the SPSS data
mydata_spss <- haven::read_spss(file="Sample_Dataset_2019.sav")

#' Import the Excel (xlsx) data
#'   Note that readxl::read_excel uses a different argument name for the file
#'   than read.csv and haven::read_spss.
mydata_excel <- readxl::read_excel(path="Sample Dataset 2019.xlsx")




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

#' Print the summary statistics (min/max/median) for all variables in the dataframe.
summary(mydata)



## Step 3: Access the variables within the dataframe ##########################


#' Use the $ operator to access a variable within the dataframe
print(mydata$Mile)
mean(mydata$Mile, na.rm=TRUE)

#' You can use the $ operator to add new variables to the dataframe, or edit
#' existing ones
mydata$MileMinutes <- mydata$Mile/60
mean(mydata$MileMinutes, na.rm=TRUE)


## Step 4: Subsetting/Access rows and columns within the dataset ##############

#' The subset() function can filter the rows of the dataframe or
#' select specific columns of the dataframe (or both!)


# Use the subset argument to select rows based on a logical condition
freshmen_only <- subset(mydata, subset = Rank==1)
jrsr_only <- subset(mydata, subset = Rank >= 3)

#' Use the select argument keep or drop specific columns
fitness_data <- subset(mydata, select = c(Athlete, Smoking, Mile)) #retain specific variables (note the use of the vector function c())
testscore_data <- subset(mydata, select = English:Science) #retain a range of variables
data_dropbday <- subset(mydata, select = -bday) #drop a specific variable using the - operator

#' You can use both the subset and the select arguments in the same call
#'   Here, we retain just the juniors/seniors and only the variables Rank, Mile, and English
jrsr_only <- subset(mydata, 
                    subset = Rank >= 3,
                    select = c(Rank, Mile, English))


## Data Analysis ##############################################################

#' Summary statistics for numeric variables: mean, sd, min, max, median, sum
#'   Note the inclusion of na.rm=TRUE -- if omitted, and if any missing values
#'   are present, the function will evaluate as NA
mean(mydata$StudyTime, na.rm=TRUE)
sd(mydata$StudyTime, na.rm=TRUE)
min(mydata$StudyTime, na.rm=TRUE)
max(mydata$StudyTime, na.rm=TRUE)
median(mydata$StudyTime, na.rm=TRUE)
sum(mydata$StudyTime, na.rm=TRUE)

#' Categorical variables: Frequency tables
#'   Note: Missing values are not reported unless requested
table(mydata$State)
table(mydata$State, useNA="always")

#' Compute Pearson correlation between two variables: SleepTime and StudyTime
cor(mydata$SleepTime, mydata$StudyTime, use="pairwise.complete.obs")

#' What happens when we pass the entire dataframe object to the cor() function?
cor(mydata, use="pairwise.complete.obs")

#' We get an error message if we try to pass the entire dataframe to cor()
#' because the dataframe contains non-numeric variables.
#' Let's create a dataset with just the 4 test score variables (English, 
#' Reading, Math, Science), which can then safely be passed to cor(). 
mydata_4cols <- subset(mydata, select=English:Science)
cor(mydata_4cols, use="pairwise.complete.obs")


#' Fit a simple linear regression model using SleepTime as the DV and StudyTime
#' as the IV. 
#'   The first argument to lm() is the model formula, which follows the syntax:
#'       y ~ x1 + x2 + ...
#'   where 
#'       y is the dependent variable; 
#'       x1, x2, etc are independent variables
model_1 <- lm(SleepTime ~ StudyTime, data=mydata)
summary(model_1)

#' Fit a multiple linear regression model with two numeric IVs:
model_2 <- lm(SleepTime ~ StudyTime + CommuteTime, data=mydata)
summary(model_2)


#' Fit a multiple linear regression model with a numeric IV (StudyTime)
#' and a categorical IV (Rank):
model_2 <- lm(SleepTime ~ StudyTime + factor(Rank), data=mydata)
summary(model_2)


#' ggplot code: create x-y scatterplot (geom_point()) with a linear regression
#' trend line added (geom_smooth(method="lm")).
ggplot(mydata, aes(x=SleepTime, y=StudyTime)) +
  geom_point() +
  geom_smooth(method="lm")


