print("This file was created withing RStudio")

print("...and now it lives on GitHub!")

# Quiz 1 (questions 11-20):

# In the dataset provided for this Quiz, what are the column names of the dataset?
names(data)

# Extract the first 2 rows of the data frame and print them to the console. 
# What does the output look like?
data[1:2,]

# How many observations (i.e. rows) are in this data frame?
nrow(data)

# Extract the last 2 rows of the data frame and print them to the console. 
# What does the output look like?
data[152:153,]

# What is the value of Ozone in the 47th row?
data[47,]

# How many missing values are in the Ozone column of this data frame?
sum(is.na(ozone))

# What is the mean of the Ozone column in this dataset? Exclude missing values 
# (coded as NA) from this calculation.
mean(ozone[!is.na(ozone)])

# Extract the subset of rows of the data frame where Ozone values are above 31 
# and Temp values are above 90. What is the mean of Solar.R in this subset?
mean(clean_data[clean_data[["Ozone"]] > 31 & clean_data[["Temp"]] > 90, "Solar.R"])

# What is the mean of "Temp" when "Month" is equal to 6?
mean(data[data[["Month"]] == 6, "Temp"])

# What was the maximum ozone value in the month of May (i.e. Month is equal to 5)?
max(data[rem_na & data[["Month"]] == 5, "Ozone"])
