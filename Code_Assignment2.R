# Load libraries
library(stats) # to use quantile, IQR and mad function

# Read the data into the program
data <- read.csv("Set1_assignment2.csv", header = TRUE)
data

# See if there are any NA values. It is found that there are 25 NA values.
# However, one of the rows has two NA, values. Thus, 24 rows must be removed.
sum(is.na(data))
# Check that there are 312 rows. 
nrow(data)

# Remove all the rows with NA values.
data <- na.omit(data)
# Check that 24 rows have been removed since we only have 288 rows now.
nrow(data)

#_______________________________________________________________________________

# Boxplot Method (Informal method)

# Get the boxplot for all the variables
boxplot(data, main = "Dataset of Patient Characteristics in Different Stages 
of Kidney Disease", xlab = "Variables")

# Look at the $out component to get the values of the outliers for each variable
boxplot.stats(data$class)
boxplot.stats(data$age)
boxplot.stats(data$glu)
boxplot.stats(data$sod)
boxplot.stats(data$pot)

#_______________________________________________________________________________

# Quartile Test (Formal method)

# Find Q1, Q3, IQR, lower and upper inner fences for the variable class.
# subset the data to show the values of the outliers and how many outliers there are.
q_class <- quantile(data$class, probs=c(.25, .75))
iqr_class <- IQR(data$class)
up_class <- q_class[2] + 1.5*iqr_class
down_class <- q_class[1] - 1.5*iqr_class
class_out <- subset(data$class, data$class < down_class | data$class > up_class)
class_out
length(class_out)

# Variable age
q_age <- quantile(data$age, probs=c(.25, .75))
iqr_age <- IQR(data$age)
up_age <- q_age[2] + 1.5*iqr_age
down_age <- q_age[1] - 1.5*iqr_age
age_out <- subset(data$age, data$age < down_age | data$age > up_age)
age_out
length(age_out)

# Variable glu
q_glu <- quantile(data$glu, probs=c(.25, .75))
iqr_glu <- IQR(data$glu)
up_glu <- q_glu[2] + 1.5*iqr_glu
down_glu <- q_glu[1] - 1.5*iqr_glu
glu_out <- subset(data$glu, data$glu < down_glu | data$glu > up_glu)
glu_out
length(glu_out)

# Variable sod
q_sod <- quantile(data$sod, probs=c(.25, .75))
iqr_sod <- IQR(data$sod)
up_sod <- q_sod[2] + 1.5*iqr_sod
down_sod <- q_sod[1] - 1.5*iqr_sod
sod_out <- subset(data$sod, data$sod < down_sod | data$sod > up_sod)
sod_out
length(sod_out)

# Variable pot
q_pot <- quantile(data$pot, probs=c(.25, .75))
iqr_pot <- IQR(data$pot)
up_pot <- q_pot[2] + 1.5*iqr_pot
down_pot <- q_pot[1] - 1.5*iqr_pot
pot_out <- subset(data$pot, data$pot < down_pot | data$pot > up_pot)
pot_out
length(pot_out)

# Quartile Test to Find Extreme Outliers (use constant 3 instead of 1.5)

# Variable age
qex_age <- quantile(data$age, probs=c(.25, .75))
iqrex_age <- IQR(data$age)
upex_age <- q_age[2] + 3*iqr_age
downex_age <- q_age[1] - 3*iqr_age
age_out_ex <- subset(data$age, data$age < downex_age | data$age > upex_age)
age_out_ex
length(age_out_ex)

# variable glu
qex_glu <- quantile(data$glu, probs=c(.25, .75))
iqrex_glu <- IQR(data$glu)
upex_glu <- q_glu[2] + 3*iqr_glu
downex_glu <- q_glu[1] - 3*iqr_glu
glu_out_ex <- subset(data$glu, data$glu < downex_glu | data$glu > upex_glu)
glu_out_ex
length(glu_out_ex)

# variable sod
qex_sod <- quantile(data$sod, probs=c(.25, .75))
iqrex_sod <- IQR(data$sod)
upex_sod <- q_sod[2] + 3*iqr_sod
downex_sod <- q_sod[1] - 3*iqr_sod
sod_out_ex <- subset(data$sod, data$sod < downex_sod | data$sod > upex_sod)
sod_out_ex
length(sod_out_ex)

# variable pot
qex_pot <- quantile(data$pot, probs=c(.25, .75))
iqrex_pot <- IQR(data$pot)
upex_pot <- q_pot[2] + 3*iqr_pot
downex_pot <- q_pot[1] - 3*iqr_pot
pot_out_ex <- subset(data$pot, data$pot < downex_pot | data$pot > upex_pot)
pot_out_ex
length(pot_out_ex)


#_______________________________________________________________________________

# Hampel Test (Formal method)

# Get the lower and upper bounds by subtraction or addition of 3*(median absolute deviation)
# from the median.
# Subset the data to show the values of the outliers and how many outliers there are.
# Since the lower and upper bound for the variable class is 1, the Hampel Test,
# does not work for this variable. This is because all class 0 will be treated as outliers.
low_class <- median(data$class) - 3 * mad(data$class)
high_class <- median(data$class) + 3 * mad(data$class)
out_class <- subset(data$class, data$class < low_class | data$class > high_class)
out_class

# Variable age
low_age <- median(data$age) - 3 * mad(data$age)
high_age <- median(data$age) + 3 * mad(data$age)
out_age <- subset(data$age, data$age < low_age | data$age > high_age)
out_age
length(out_age)

# Variable glu
low_glu <- median(data$glu) - 3 * mad(data$glu)
high_glu <- median(data$glu) + 3 * mad(data$glu)
out_glu <- subset(data$glu, data$glu < low_glu | data$glu > high_glu)
out_glu
length(out_glu)

# Variable sod
low_sod <- median(data$sod) - 3 * mad(data$sod)
high_sod <- median(data$sod) + 3 * mad(data$sod)
out_sod <- subset(data$sod, data$sod < low_sod | data$sod > high_sod)
out_sod
length(out_sod)

# Variable pot
low_pot <- median(data$pot) - 3 * mad(data$pot)
high_pot <- median(data$pot) + 3 * mad(data$pot)
out_pot <- subset(data$pot, data$pot < low_pot | data$pot > high_pot)
out_pot
length(out_pot)

#_______________________________________________________________________________

# sort the outliers in increasing order to be put in pdf document
sort(glu_out)

sort(sod_out)

sort(glu_out_ex)

