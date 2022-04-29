'Employee Database
-------------------'
# Read/Load the data: '.csv file' (comma-separated values file)
employeedata <- read.csv(file.choose(),header=TRUE)
View(employeedata)

# view the first 6 rows
head(employeedata)

# view the last 6 rows
tail(employeedata)

# what is this employeedata object?
class(employeedata)

# Information about data frame
str(employeedata)

'Subsetting data
-----------------'
# Accessing the particular column from the data set
'--------------------------------------------------'
# Method-1
employeedata$gender
# Method-2
employeedata['gender']
# Method-3
employeedata[,2]

# Access the first six data entries of the 'gender' variable
head(employeedata['gender'])

# Accessing Multiple columns 
employeedata[,2:4]
employeedata[,c(1:3,5)]

# Accessing the columns names in the data sets (Variable names)
colnames(employeedata)

# Accessing a particular row
employeedata[1,]
employeedata[100,]
employeedata[c(1,4,7),]

# Command to Extract an Element
employeedata[4,5] # Element at 4th row, 5th column

# Selecting all the Observations based on condition(s)
employeedata[which(employeedata$gender=='Male'
                   & employeedata$educ > 19),]

employeedata[which(employeedata$gender=='Female'
                   & employeedata$jobcat == 'Manager'),]

# Coverting the int type variable to Factor type
class(employeedata$educ) 

# Set up factors.
employeedata$educ <- as.factor(employeedata$educ)
class(employeedata$educ) # Data type now 'factor'

'GGPLOT2 Package
----------------'
library(ggplot2)
library(tidyverse)

# 1. How many male and female working in the company?
'---------------------------------------------------'
# Simple Bar Chart
table(employeedata$gender)
ggplot(employeedata, aes(x = gender)) + 
  geom_bar()

# Add some customization for labels and theme.
ggplot(employeedata, aes(x = gender)) + 
  # theme_bw() +
  # theme_classic()+
   theme_dark() +
  # theme_get() +
  #theme_gray() +
  geom_bar() +
  labs(y = "Employee Count",
       title = "Gender Wise Employee")

# 2. With respect to Job Category, How many male and female working in the company?
'----------------------------------------------------------------------------------'
# Multiple Bar Chart (Grouped)
ggplot(employeedata, aes(x = gender, fill = jobcat)) + 
  theme_gray() +
  geom_bar(position="dodge") +
  labs(y = "Employee Count", title = "Gender wise Job Categories")

# Multiple Bar Chart (Stacked)
ggplot(employeedata, aes(x = gender, fill = jobcat)) + 
  theme_gray() +
  geom_bar() +
  labs(y = "Employee Count", title = "Gender wise Job Categories")

# 3. What is the average salary of managers, clerical staff etc. (i.e. Job Category) 
# working in the company? 
'-----------------------------------------------------------------------------------'
# Simple Bar Chart 
ggplot(employeedata, aes(x = jobcat, y=salary)) + 
  theme_gray() +
  stat_summary(fun = mean, geom = "bar") +
  labs(y = "Salary", title = "Job Categories wise Salary")

# Simple Bar Chart with error_bars
bar <- ggplot(employeedata, aes(x = jobcat, y=salary)) 
bar + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar") +
  labs(y = "Salary", title = "Job Categories wise Salary")


# 3. What is the average salary of managers, clerical staff etc. (i.e. Job Category) 
# working in the company by gender? 
'-----------------------------------------------------------------------------------'
# Grouped
bar <- ggplot(employeedata, aes(x = gender, y=salary, fill = jobcat))
bar + stat_summary(fun = mean, geom = "bar", position ="dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.9), width = 0.2) +
  theme_gray() +
  labs(y = "Salary", title = "Gender wise Job Categories")

# Stacked
bar <- ggplot(employeedata, aes(x = gender, y=salary, fill = jobcat))
bar + stat_summary(fun = mean, geom = "bar", position ="stack") +
  theme_gray() +
  labs(y = "Salary", title = "Gender wise Job Categories")

# 4. Classify the employees with respect to their education? 
'-----------------------------------------------------------'
# Grouped
ggplot(employeedata, aes(x = educ, fill = gender)) + 
  theme_bw() +
  geom_bar(position="dodge") +
  labs(y = "Employee Count", title = "Education Wise Employees data")

# 5. Number of Job category wise employees with respect to education and gender
'------------------------------------------------------------------------------'
'We can leverage facets to further segment the data and enable
 "visual drill-down" into the data.'
ggplot(employeedata, aes(x = educ, fill = jobcat)) + 
  theme_bw() +
  facet_wrap(~ gender) +
  geom_bar(position="dodge") +
  labs(y = "Job Category", title = "Employees Data by Education and Job Catagory")

# 6. What is the distribution of Employee Salary?
'------------------------------------------------'
# Histogram
ggplot(employeedata, aes(x = salary)) +
  theme_bw() +
  geom_histogram(binwidth = 20000) + 
  labs(y = "Employee Count",
       x = "Salary (binwidth = 20000)",
       title = "Employee Salary Distribtion")

# Frequency Polygon
ggplot(employeedata, aes(x = salary)) +
  theme_bw() +
  geom_freqpoly(binwidth = 20000) + 
  labs(y = "Employee Count",
       x = "Salary (binwidth = 20000)",
       title = "Employee Salary Distribtion")

# Box Plot
options(scipen=999)
ggplot(employeedata, aes(y = salary)) +
  theme_bw() +
  geom_boxplot() 

# 7. What is the distribution of male and female (i.e. employee) Salary?
'-----------------------------------------------------------------------'
ggplot(employeedata, aes(x = salary, fill = gender)) +
  theme_bw() +
  geom_histogram(binwidth = 20000) +
  labs(y = "Employee Count",
       x = "Salary (binwidth = 20000)",
       title = "Employee Salary Distribtion")

# Box-and-whisker plot: Another great visualization for this question
ggplot(employeedata, aes(x = gender, y = salary)) +
  theme_bw() +
  geom_boxplot() +
  stat_summary(fun=median, geom="point", size=2, color="red") + 
  labs(y = "Salary",
       x = "Gender",
       title = "Gender Wise Employees Salary")

# Vilion Plot
ggplot(employeedata, aes(x = gender, y = salary)) +
  theme_bw() +
  geom_violin() +
  labs(y = "Salary",
       x = "Gender",
       title = "Gender Wise Employees Salary")

# Rotate the violin plot
ggplot(employeedata, aes(x = gender, y = salary)) +
  theme_bw() +
  geom_violin() +
  coord_flip() + # Rotate the plot 
  labs(y = "Salary",
       x = "Gender",
       title = "Gender Wise Employees Salary")

# Add summary statistics on a violin plot
# 1. Add mean Points
ggplot(employeedata, aes(x = gender, y = salary)) +
  theme_bw() +
  geom_violin() +
  stat_summary(fun=mean, geom="point", shape=23, size=2) + # Add mean points 
  labs(y = "Salary",
       x = "Gender",
       title = "Gender Wise Employees Salary")

# 2. Add median Points
ggplot(employeedata, aes(x = gender, y = salary)) +
  theme_bw() +
  geom_violin() +
  stat_summary(fun=median, geom="point", size=2, color="red") + # Add median points 
  labs(y = "Salary",
       x = "Gender",
       title = "Gender Wise Employees Salary")

# 3. Add median and quartile
ggplot(employeedata, aes(x = gender, y = salary)) +
  theme_bw() +
  geom_violin() +
  geom_boxplot(width=0.1) + # Add median and quartile
  stat_summary(fun=median, geom="point", size=2, color="red")+ # Add median points 
  labs(y = "Salary",
       x = "Gender",
       title = "Gender Wise Employees Salary")

' LINE PLOT
-----------'
'Orange Data'
# -----------
View(Orange)
head(Orange)

# Line Plot
ggplot(Orange, aes(age, circumference, colour = Tree)) +
  geom_point(size = 5, alpha = 0.9) +
  geom_line(linetype = "dotted") +
  labs(title = "Trees age & Circumference")

Orange %>% 
  filter(Tree != "1" &
           Tree != 5) %>%
  ggplot(aes(age, circumference, colour = Tree)) +
  geom_point(size = 5, alpha = 0.9) +
  geom_line(linetype = "dotted") +
  labs(title = "Trees age & Circumference")


'Data Visualization to Show Deviations'
#--------------------------------------

'Diverging Charts based on Z-Score
----------------------------------'
library(ggplot2)
# Load the data
data("mtcars") 
# Create new column for car names
mtcars$carname <- rownames(mtcars)
mtcars
# Compute the Z-Score of the mpg
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)
mtcars
# Above / Below based on the Z-Score
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")
mtcars
# Sort the data based on the Z-Score
mtcars <- mtcars[order(mtcars$mpg_z), ]
mtcars
# Convert to factor to retain sorted order in plot.
mtcars$carname <- factor(mtcars$carname, levels = mtcars$carname)
mtcars

# 1) Diverging Barcharts
'------------------------'
ggplot(mtcars, aes(x=carname, y=mpg_z, label=mpg_z)) +
  geom_bar(stat='identity', aes(fill=mpg_type), width=.5) +
  scale_fill_manual(name="Mileage", labels = c("Above Average", "Below Average"),
                    values = c("above"="#00ba38", "below"="#f8766d")) +
  labs(subtitle="Normalised mileage from 'mtcars'", title= "Diverging Bars") +
  coord_flip()

# 2) Diverging Dot Plot
'----------------------'
ggplot(mtcars, aes(x=carname, y=mpg_z, label=mpg_z)) + 
  geom_point(stat='identity', aes(col=mpg_type), size=6)  +
  scale_color_manual(name="Mileage", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="Diverging Dot Plot", 
       subtitle="Normalized mileage from 'mtcars': Dotplot") + 
  ylim(-2.5, 2.5) +
  coord_flip()

# Diverging Lollipop Chart
'-------------------------'
ggplot(mtcars, aes(x=carname, y=mpg_z, label=mpg_z)) + 
  geom_point(stat='identity', fill="black", size=6)  +
  geom_segment(aes(y = 0, x = carname,  yend = mpg_z, 
                   xend = carname), color = "black") +
  geom_text(color="white", size=2) +
  labs(title="Diverging Lollipop Chart", 
       subtitle="Normalized mileage from 'mtcars': Lollipop") + 
  ylim(-2.5, 2.5) +
  coord_flip()

'Data Visualization to Show Correlations'
#----------------------------------------
' SCATTER PLOT 
--------------'
# Exam Anxiety data
'-------------------'
# Read/Load the data 
examData <- read.delim("Exam Anxiety.dat",  header = TRUE)
View(examData)

# Simple scatter
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + labs(x = "Exam Anxiety", y = "Exam Performance %") 

# Simple scatter with regression line
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se=F) + 
  labs(x = "Exam Anxiety", y = "Exam Performance %") 

#Simple scatter with regression line + CI
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + 
  geom_smooth(method = "lm", colour = "Red") + 
  labs(x = "Exam Anxiety", y = "Exam Performance %") 

# Simple scatter with regression line + coloured CI
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + 
  geom_smooth(method = "lm", colour = "Red", alpha = 0.1, fill = "Red") + 
  labs(x = "Exam Anxiety", y = "Exam Performance %") 

# Grouped scatter with regression line + CI
scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))
scatter + geom_point() + 
  geom_smooth(method = "lm", aes(fill = Gender), alpha = 0.1) + 
  labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender") 


'Bubble Plot
------------'
# mtcars data set
data(mtcars)
mtcars

# Basic Bubble Plot
ggplot(mtcars, aes(x = wt, y = mpg, size = hp)) +
  geom_point()

# Improved bubble plot
ggplot(mtcars, aes(x = wt, y = mpg, size = hp)) +
  geom_point(alpha = .5, fill = "red", color = "black", shape = 21) +
  scale_size_continuous(range = c(1, 14)) + # specifies the minimum and maximum size of the plotting symbol
   labs(title = "Auto mileage by weight and horsepower",
       subtitle = "Motor Trend US Magazine (1973-74 models)",
       x = "Weight (1000 lbs)",
       y = "Miles per (US) gallon",
       size = "Gross\n horsepower") #\n - Next line

# Bubble plot for Exam Anxiety data
ggplot(examData, 
       aes(x = Anxiety, y = Exam, size = Revise, fill=Gender)) +
  geom_point(alpha = .5, color = "black", shape = 21) +
  scale_size_continuous(range = c(1, 14)) +
  labs(title = "Examination Performance by Exam Anxiety & Revision time",
       x = "Exam Anxiety",
       y = "Exam Performance %",
       size = "Revision \n Time",
       fill = "Gender") +
  theme_minimal() 

'Correlogram
------------'
# Visualization of a correlation matrix using ggplot2
library(ggcorrplot)

data(mtcars)

# Correlation matrix
corr <- round(cor(mtcars), 1)
corr

# Compute a correlation matrix p-values
p.mat <- cor_pmat(mtcars)
head(p.mat)

# Plot the correlation matrix
'----------------------------'
# with default method = "square" 
ggcorrplot(corr) 

# With method = "circle"
ggcorrplot(corr, method = "circle")

# using hierarchical clustering
ggcorrplot(corr, method = "circle", hc.order = TRUE, 
           outline.color = "white")

# Add correlation coefficients
ggcorrplot(corr, method = "circle", hc.order = TRUE, 
           outline.color = "white", lab = TRUE)

# Add correlation significance level
ggcorrplot(corr, method = "circle", hc.order = TRUE, 
           outline.color = "white", lab = TRUE, p.mat = p.mat)

# Get the lower triangle of the correlogram
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, 
           lab_size = 3, method="square",
           colors = c("red", "white", "green"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)

'Data Visualization to Show Rankings
------------------------------------'
# Read/Load the data 
df <- read.csv("Pareto Chart.csv",  header = TRUE)
View(df)

'Ordered Bar Chart
------------------'
defects <- df
defects$Defects <- factor(df$Defects,                                    
                  levels = df$Defects[order(df$Counts, decreasing = TRUE)]) # Factor levels in decreasing order

ggplot(defects, aes(Defects, Counts)) + 
  geom_bar(stat = "identity")

'Pareto Chart
-------------'
library(ggQC)
ggplot(df, aes(x=Defects, y=Counts)) +
  stat_pareto(point.color = "red", point.size = 3, 
              line.color = "black", bars.fill = c("red", "orange"))



