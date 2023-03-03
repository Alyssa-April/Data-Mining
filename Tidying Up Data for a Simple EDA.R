## TIDYING UP DATA FOR A SIMPLE EXPLORATORY DATA ANALYSIS

## Two files, "Set1_assignment1_HRdata.csv" and "Set1_assignment1_FINdata.csv"
## are used as data.

### READ FILE INTO R

# Use strip.white to delete any spaces before and after characters if there are any
hr_data <- read.csv("Set1_assignment1_HRdata.csv", strip.white = TRUE)

#__________________________________________________________________________________________

### QUESTION 1
library(tidyverse) # to use dplyr and tidyr package

# Separate deptid_gender into two columns named deptid and gender
hr_data <- hr_data %>% separate(deptid_gender, c("deptid", "gender"), ";") 

# Remove anything other than M or F in the gender column as there are rows that have "," or "-",
# for example row 12 has ",M".
hr_data[,3] <- gsub("[^MF]", "", hr_data[,3])

#__________________________________________________________________________________________

### QUESTION 2

# Unite three columns into one
hr_data <- unite(hr_data, col = "birthdate", c('birthday', 'birthmonth', 'birthyear'), sep = "/")

# Check if there are missing values in the birthdate column
# Since output is integer(0), none of the elements in column birthdate have missing values
which(is.na(hr_data[,4]))

#__________________________________________________________________________________________

### QUESTION 3

# Strptime converts character vectors of birthdate to date format
proper_birthdate <- strptime(hr_data[,4], format="%d/%m/%Y")

# Convert birthdate column to a proper date format              
hr_data <- hr_data %>% mutate(birthdate = as.POSIXct(proper_birthdate))

# Check that now the birthdate column is of type POSIXct
str(hr_data)

# There are two missing values in the birthdate column at row 288 and 3927.
# These appear as missing values because the year 1966 and 1982 were not leap years
# but the date entered is 29 February. There is error in birthdate entry.
which(is.na(hr_data[,4]))

#__________________________________________________________________________________________

### QUESTION 4

# Remove rows with missing value in birthdate column from the whole data set, 
# that is remove row 288 and 3927.
hr_data <- hr_data[!is.na(hr_data[,4]), ]

# Observe that there are no more missing values in the birthdate column
which(is.na(hr_data[,4]))

#___________________________________________________________________________________________

### QUESTION 5

# Observe that the enterdate and exitdate columns are characters.
# Convert character vectors of enterdate and exitdate columns to date format.
proper_enterdate <- strptime(hr_data$enterdate, format="%d/%m/%Y")
proper_exitdate <- strptime(hr_data$exitdate, format="%d/%m/%Y")

# Convert enterdate and exitdate columns to a proper date format              
hr_data <- hr_data %>% mutate(enterdate = as.POSIXct(proper_enterdate),
                              exitdate = as.POSIXct(proper_exitdate))

# Check that now the enterdate and exitdate columns are of type POSIXct
str(hr_data)

#__________________________________________________________________________________________

### QUESTION 6

# difftime is a function that finds the time interval between two dates. 
# In this case, 31 Oct 2022 minus the birthdate of each employee.
# Once the number of days between the dates are obtained, divide it by 365.25 to 
# obtain the age in years. Then, make a new column called age to store it.
# as.numeric is used so that the age is num. 
# I used as.POSIXct, not as.Date, where there is a very slight difference in decimal 
# values for both date formats.
hr_data <- hr_data %>% 
  mutate(age = as.numeric((difftime(as.POSIXct("2022-10-31"), hr_data[,4], units = "days"))/365.25))

#__________________________________________________________________________________________

### QUESTION 7

# Create a new column called status. If there is NA value in exitdate column, the entry for
# status will be "active". Other than NA, the entries for exitdate column are only dates. 
# So if there is a date entry, the entry for status will be "inactive" as the employee
# already left the company.
hr_data <- hr_data %>% 
  mutate(status = case_when(is.na(hr_data$exitdate) == TRUE ~ "active", TRUE ~ "inactive"))

#__________________________________________________________________________________________

### QUESTION 8

# Create a new column called lengthofservice. If the status column's value is "inactive",
# values from enterdate column are substracted from exitdate column. If status column's 
# value is "active", values from enterdate column are substracted from 31 Oct 2022.
hr_data <- hr_data %>% 
  mutate(lengthofservice = case_when(
    hr_data$status == "inactive" ~ as.numeric((difftime(hr_data$exitdate, hr_data$enterdate, 
                                                        units = "days"))/365.25),
    hr_data$status == "active" ~ as.numeric((difftime(as.POSIXct("2022-10-31"), hr_data$enterdate, 
                                                        units = "days"))/365.25)))

#___________________________________________________________________________________________

### QUESTION 9 

library(ggplot2) # used for cut_number function and boxplots in Question 13

# Get 3 intervals of equal frequency for the lengthofservice column and store it in a new
# column called "losgroup".
# The function cut_number here makes 3 groups with 1715 observations each.
hr_data <- hr_data %>% mutate(losgroup = cut_number(hr_data$lengthofservice, 3))

# Ensures that the losgroup column contains ordered intervals. 
hr_data$losgroup <- factor(hr_data$losgroup, ordered = TRUE)

# This confirms that each interval has 1715 observations.
table(hr_data$losgroup)

# Check that the losgroup column has ordered intervals.
str(hr_data)

#____________________________________________________________________________________________

### QUESTION 10

# read data into the variable fin_data
fin_data <- read.csv("Set1_assignment1_FINdata.csv", strip.white = TRUE)

# Make a new column called "latest_salary" to store the latest salary for each employee.
# which.max function will take the index of the first maximum yearid it finds, thus saving
# the salary of the latest year in latest_salary.
# Whichever row is taken for each group does not matter now since the latest salary is already stored 
# in "latest_salary". So, use the first function to extract the first value from each group. 
# The summarise function creates a new data frame with salary based on the grouping
# of staffid. 
fin_data <- fin_data %>% 
  group_by(staffid) %>%  # group the data set by staffid
  mutate(latest_salary = salary[which.max(yearid)]) %>%
  summarise(salary = first(latest_salary)) 

# Match the staffid of those entries which intersect in both data sets. Only those that 
# intersect are joined since earlier in Question 3, two rows were removed. 
full_data <- inner_join(hr_data, fin_data)

#_____________________________________________________________________________________________

### QUESTION 11

# Extract rows that where the employees have an "active" status.
full_data <- full_data %>% filter(full_data$status == "active")

# Subset the columns needed using the select function.
final_data<- full_data %>% 
  select(staffid, deptid, status, gender, namefirst, namelast, losgroup, salary)

final_data # view the final data set

#_____________________________________________________________________________________________

### QUESTION 12

# If we plot this, we get a representation of probability densities, not a relative frequency histogram.
# It plots the density component of first_hist.
first_hist <- hist(final_data$salary, breaks = seq(0, 70000, by = 10000), freq = FALSE, col = "purple", plot = FALSE)

# So, we need to adjust the density component of first_histogram to be relative frequency components by
# dividing the counts for each interval by the sum of counts. This represents the relative frequency formula.
first_hist$density <- (first_hist$counts)/(sum(first_hist$counts))

# ANSWER FOR 12(a) PLOT
# Now, plot the histogram and set freq = FALSE where it will plot the density component. Since it was 
# already updated to be relative frequency values, we can obtain the relative frequency histogram.
plot(first_hist, freq = FALSE, main = "Histogram of Employee Salaries (12(a))", xlab = "Salary", 
     ylab = "Relative Frequency", col = "purple")

# The same steps and logic are repeated for the second_hist
second_hist <- hist(final_data$salary, breaks = seq(0, 70000, by = 5000), freq = FALSE, col = "purple", plot = FALSE)

second_hist$density <- (second_hist$counts)/(sum(second_hist$counts))

# ANSWER FOR 12(b) PLOT
plot(second_hist, freq = FALSE, main = "Histogram of Employee Salaries (12(b))", xlab = "Salary", 
     ylab = "Relative Frequency", col = "purple")

# The second histogram describes the variable best as it is a better indication of how the 
# salaries are spread out. As can be seen, the first histogram lacks details since we see that as 
# the salary increases, the relative frequency of employees with that salary decreases. However, if we 
# look at the second histogram, we will notice that the relative frequency increases and decreases often
# as the salary increases. It is easier to notice any spikes or drops in the second histogram.
# Therefore, too few bins (as in the first histogram) will mask underlying patterns that could be important.

# In the first histogram, we see that many employees, with a relative frequency around 0.47 or
# 47% of them, have a salary between 0 to 10000. While in the second histogram, roughly 0.43 or 
# 43% of the employees, which is the majority, have a salary between 5000 to 10000. 
# The second histogram narrows down this range for better interpretation and reporting.

# The second histogram gives a clearer picture for certain outliers too. 
# From the second histogram, it is seen that there is a very small proportion of employees with a 
# salary between 60000 to 65000. In the first histogram this relative frequency appears in the 
# 60000 to 70000 salary range, rendering it less informational as the range is much bigger and thus
# less specific. 

# Conclusion: Second histogram describes the variable best.

#_____________________________________________________________________________________________

### QUESTION 13

# plot 3 boxplots side by side in one graph
ggplot(final_data, aes(x = losgroup, y = salary)) +           
  geom_boxplot() +
  labs(title = "Boxplots of the Salary for Each Interval in the losgroup", 
       x = "losgroup", y = "Salary")

# The three boxplots plotted in this graph show the salaries for employees who have worked in
# three different lengths of service intervals. Employees who have worked longer, have bigger salaries.
# This is observed through the fact that employees with a length of service of 25.5 to 46.6 years have 
# the biggest median salary at around 26500. Employees with a length of service of 13.5 to 25.5 years
# on the other hand, have a median salary of roughly 9000, which is a big gap from the previously mentioned
# interval. Lastly, employees who have worked for 0 to 13.5 years have the smallest median of roughly 7600. 

# However, the [0,13.5] interval has the smallest salary range, as its median, first quartile and
# third quartile are all nearly the same value. The biggest salary range is the (25.5, 46.6] interval. 
# The interquartile range, that is the middle 50% of the data, of the (25.5, 46.6] interval is also 
# the largest since its third quartile and first quartile are the furthest apart. For this interval, 
# most employees have a salary roughly between 21800 and 34700, but some employees have salaries as 
# low as roughly 2300 and as high as roughly 53500 (not including outliers). Thus, 25% of employees in 
# the (25.5, 46.6] interval have a salary less than 21800 and 25% of employees have a salary more 
# than 34700.This interval's middle half of the data has more variability than the other two intervals. 

# For the (13.5, 25.5] interval, 25% of employees have a salary less than roughly 7000 and 25% of employees
# have a salary more than roughly 12900. This shows that this interval's central portion is less spread out
# than the (25.5, 46.6] interval. 

# Noticeably, the length of service between 0 and 13.5 years has the most number of outliers,
# especially outside the smallest value within the inner fences. This can heavily distort the data
# distribution. Besides that, there is only one outlier each for the (13.5, 25.5] and (25.5, 46.6] 
# intervals with roughly 26500 salary and 64000 salary respectively. This means that the employees 
# receive a salary way out of the range of his/her colleagues from the same length of service groups.

# The skewness of the boxplots are not very clear as the medians go to one side of the box but
# the shorter whisker is in the opposite direction. 

#____________________________________________________________________________________________
