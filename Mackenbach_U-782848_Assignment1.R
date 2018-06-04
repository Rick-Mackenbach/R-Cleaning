## Rick Mackenbach
## U782848

##---------Dependencies-----------##
library(dplyr)
library(ggplot2)
##--------------------------------##


##---------Load data Set----------##
data_set <- read.csv('ratings.csv')
##--------------------------------##


##    Question 1    ##
##
sorted_gender <- data_set

## Rename the column to Gender.
colnames(sorted_gender)[7] <- "Gender"

## Substr all elements in Gender column to only first letter.
sorted_gender$Gender <- substr(sorted_gender$Gender, 1, 1)

## UpperCase all elements
sorted_gender$Gender <- toupper(sorted_gender$Gender)

## Filter out all elements where Gender == F
answer1 <- filter(sorted_gender, Gender == "F")
rm(sorted_gender)
answer1$Gender <- 'Female'
answer1$Duration..in.seconds. <- answer1$Duration..in.seconds. / 60
names(answer1)[3] <- 'Duration in Minutes'


##-------------------------------##

##    Question 2    ##
sorted_jobs <- data_set

## Rename the column to occupation
colnames(sorted_jobs)[10] <- "Occupation"
factor(sorted_jobs$Occupation)

## Paste all values of student and unemployed into new vector
occupation_needed <- c("Student", "Student ", "student", "student ")

## Filter the jobs we want
answer2 <- filter(sorted_jobs, Occupation %in% occupation_needed)

## Remove unnecessary variables
rm(occupation_needed)
rm(sorted_jobs)


##-------------------------------##

##    Question 3    ##
sorted_gender <- data_set

## Rename the column to Gender.
colnames(sorted_gender)[7] <- "Gender"

## Substr all elements in Gender column to only first letter.
sorted_gender$Gender <- substr(sorted_gender$Gender, 1, 1)

## UpperCase all elements
sorted_gender$Gender <- toupper(sorted_gender$Gender)

## Pipe function to fit all in one function
answer3 <- sorted_gender %>%
  group_by(Gender) %>%
  summarise(Attr_Black1 = mean(Black1_2, na.rm=TRUE),
            Attr_White2 = mean(White1_2))

## Remove unnecessary variables
rm(sorted_gender)

## Rename to Male and Female
answer3$Gender[1] <- "Female"
answer3$Gender[2] <- "Male"


##-------------------------------##

##    Question 4    ##
## Use mutate function, where we substring the startDate and endDate
## column variable to the info we want
dataset_2 <- mutate(data_set, startTime = substr(data_set$StartDate,12,16),
                              endTime = substr(data_set$EndDate,12,16))
dataset_2$LocationLatitude <- NULL
answer4 <- dataset_2

## Remove unnecessary variables
rm(dataset_2)

##-------------------------------##

##    Question 5    ##
## Use GGplot to, set parameters bins = 20, rename scales accordingly
ggplot(data = data_set, aes(x = Q33)) +
  geom_histogram(bins = 20) +
  scale_x_continuous(name = 'Age') +
  scale_y_discrete(name = 'Number of Observations')


##-------------------------------##

##    Question 6    ##
## Re-use part of question 1
sorted_gender <- data_set
colnames(sorted_gender)[7] <- "Gender"
sorted_gender$Gender <- substr(sorted_gender$Gender, 1, 1)
sorted_gender$Gender <- toupper(sorted_gender$Gender)

## Subset $gender, where we subset instances where Gender = M or F,
## and rename them.
sorted_gender$Gender[sorted_gender$Gender == "M"] <- 'Male'
sorted_gender$Gender[sorted_gender$Gender == "F"] <- 'Female'

## Create barplot
ggplot(data = sorted_gender, aes(x = Gender, y = Black1_1)) +
  geom_boxplot() +
  scale_x_discrete(name = 'Gender participant') +
  scale_y_discrete(name = 'Competence rating first black model')


##-------------------------------##

##    Question 7    ##
## Use GGplot geom_jitter() to avoid overlapping datapoints and rename
## scales accordingly.
ggplot(data = data_set, aes(x = Black1_1, y = White1_1)) +
  geom_jitter() +
  scale_x_continuous(name = 'Competence rating first black model') +
  scale_y_continuous(name = 'Competence rating first white model')


##-------------------------------##

##    Question 8    ##
##

## First we make 'A' with only instances of models we want averages of
a <- select(data_set, contains('_1'))

## Create a new matrix 'B' with as column the row 0 of A
b <- data.frame(colnames(a))

## Now we add all the necessary information to our new matrix B
colnames(b)[1] <- 'Model'
b[2] <- colMeans(a, na.rm = TRUE)
colnames(b)[2] <- 'Rating'
b[3] <- substr(b$Model, 1,5)
colnames(b)[3] <- 'Race'

## Plot our barPlot with Race filling the bars.
ggplot(data = b, aes(x = reorder(Model, +Rating), y = Rating, fill = Race)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual('Race', values = c('Black' = 'black', 'White' = 'white')) +
  scale_x_discrete(name = 'Model') +
  scale_y_continuous(name = 'Average Competence')

## Remove unnecessary variables
rm(a)
rm(b)


##-------------------------------##

##    Question 9    ##
##

## First we make 'A' with only instances of models we want averages of
a <- select(data_set, contains('_2'))

## Create a new matrix 'B' with as column the row 0 of A
b <- data.frame(colnames(a))

## Now we append all the necessary information to our new matrix B
## And rename the columns for better interpretation
colnames(b)[1] <- 'Model'
b[2] <- colMeans(a, na.rm = TRUE)
colnames(b)[2] <- 'Rating'
b[3] <- substr(b$Model, 1,5)
colnames(b)[3] <- 'Race'
b[4] <- substr(b$Model, 6,7)
colnames(b)[4] <- 'Attire'

## Change even numbers to Formal, and afterward everything not 'Formal' becomes
## 'Informal'
b[4] <- as.numeric(gsub('_', '', b$Attire))
b$Attire[b$Attire %% 2 != 0] <- "Formal"
b$Attire[b$Attire != 'Formal'] <- "Informal"

answer9 <- b

## Remove unnecessary variables
rm(a)
rm(b)



##-------------------------------##

##    Question 10   ##
## GGplot with adjusted theme elements
## The data sorted_gender is being reused from exercise 6 (The code to make
## sorted_gender can be found there)

ggplot(data = sorted_gender, aes(x = Black10_1, y = Black11_1, color=Gender)) +
  geom_jitter(na.rm = TRUE) +
  ## Setting the scale setting
  scale_x_discrete(name = 'Competence black model 10', limits = c(1,2,3,4,5,6))+
  scale_y_continuous(name = 'Competence black model 11', limits = c(1,6)) +
  ## Background
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = 'white')) +
  ## Adjusting the axes lines
  theme(axis.line = element_line(size = 0.5, colour = 'grey'))

## Remove unnecessary variables (sorted_gender is not necessary anymore)
rm(sorted_gender)

##-------------------------------##

##    Question 11   ##
## We can specify binwidth to 'force' the plot to put the instances in 3 bins
ggplot(data = data_set, aes(x = Black1_2)) +
  ## Fill as takes by colorschemecolector from the document
  geom_bar(binwidth = 3.2, fill="#474747", col="white")



##-------------------------------##

##    Question 12   ##
##

## Select the columns we want
a <- select(data_set, one_of(c('Black5_3', 'Black5_4', 'Q35')))

## Remove the na values
a <- na.omit(a)

## Change all non-student jobs to 'Non-Student'
a$Q35[a$Q35 %in% c("Student", "Student ", "student", "student ")] <- 'Student'
a$Q35 <- as.character(a$Q35)
a$Q35[a$Q35 != 'Student' ] <- 'Non-Student'

## Create our final plot
ggplot(data = a, aes(x = Black5_3, y = Black5_4, color = Q35)) +
  geom_jitter() +
  scale_color_manual('Group', values = c('Student' = 'darkblue',
                                         'Non-Student' = 'red')) +
  scale_y_discrete(limits = c(1,2,3,4,5,6))

## Remove unnecessary variables
rm(a)


## Only the answer vars are kept, other variables that were created to make
## the answers can be found in each exercise.


## End of script ##
