# =================================== Libraries ===================================
# Package for reading .arff files
library(foreign)

# Packages for plots (tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)

# Package for Spider / Radar plots
library(fmsb)

# Library for Decision Trees
library(rpart)
library(rpart.plot)

# Library for PCA
library(factoextra)

# Creating dataframe object & reading the dataset file
path <- './Autism-Adult-Data.arff'
dataframe_autism <- data.frame()
dataframe_autism <- read.arff(path)

#Renaming 'others' to 'Others' in Ethnicity line 658
dataframe_autism[658, 13] <- 'Others'

# Removing zero frequencies on ethnicities
dataframe_autism$ethnicity <- droplevels(dataframe_autism$ethnicity)

# Removing line 53 from the dataframe - Outlier / Noise data found (383 years old woman)
dataframe_autism <- dataframe_autism[-c(53), ]

# Cheking & eliminating NA data (95 rows will be gone)
any(is.na(dataframe_autism))
dataframe_autism <- na.omit(dataframe_autism)
any(is.na(dataframe_autism))

# Renaming column names (FIX: jundice' to 'jaundice', 'austim' to 'immediate_family_with_asd', 'contry_of_res' to 'country_of_residence', 'result' to 'A10_test_result', 'who_is_completing_the_test', 'Class/ASD' to 'class_asd')
colnames(dataframe_autism) <- c("A1_Score", "A2_Score", "A3_Score", "A4_Score", "A5_Score",
                                     "A6_Score", "A7_Score", "A8_Score", "A9_Score", "A10_Score",
                                     "age", "gender", "ethnicity", "jaundice",
                                     "immediate_family_with_asd", "country_of_residence",
                                     "used_app_before", "A10_test_result", "age_desc",
                                     "who_is_completing_the_test", "class_asd")

# ==================================== CASE 00 ====================================
# General Data Visualizations

plot(dataframe_autism)

# ==================================== CASE 01 ====================================
# Relation between occurrences of jaundice in patients diagnosed with ASD

# Filter cases of ASD positive on occurrences of patients born with jaundice
filter_jaundice_yes <- filter(dataframe_autism, jaundice %in% c('yes'))
    
# Get total of 'yes' & 'no' on class_asd
count(filter_jaundice_yes, class_asd)
jaundice_with_asd <- 28 # Yes
jaundice_without_asd <- 31 # No

# Set values for slices of pie chart
slices <- c(jaundice_with_asd, jaundice_without_asd)

# Set labels for slices of pie chart
lbls <- c('ASD Diganosed', 'Non-ASD')
# Rounds values to 100% format
pct <- round(slices / sum(slices) * 100)
# Add percents to labels
lbls <- paste(lbls, pct)
# Add % to labels
lbls <- paste(lbls,'%', sep = '')

# Pie chart plot
pie(slices,
    labels = lbls,
    col = c('purple', 'green'),
    main = 'Occurrences of Autism on Jaundice',
    sub = 'Percentage times that Autism Spectrum Disorder (ASD) happenned on positive cases of Jaundice'
    )

# ==================================== CASE 02 ====================================
# Occurrences of ASD on idividuals with immediate family members also with ASD

# Filter cases of ASD positive on occurrences of patients with immediate parents with ASD
immediate_family_with_asd <- filter(dataframe_autism, immediate_family_with_asd %in% c('yes'))

# Get total of 'yes' & 'no' on class_asd
count(immediate_family_with_asd, class_asd)
immediate_family_with_asd <- 41 # Yes
immediate_family_without_asd <- 44 # No

# Set values for slices of pie chart
slices <- c(immediate_family_with_asd, immediate_family_without_asd)

# Set labels for slices of pie chart
lbls <- c('ASD Diganosed', 'Non-ASD')
# Rounds values to 100% format
pct <- round(slices / sum(slices) * 100)
# Add percents to labels
lbls <- paste(lbls, pct)
# Add % to labels
lbls <- paste(lbls,'%', sep = '')

# Pie chart plot
pie(slices,
    labels = lbls,
    col = c('purple', 'green'),
    main = 'Autism cases on persons with Immediate family ASD',
    sub = 'Percentage times that Autism Spectrum Disorder (ASD) happenned on individuals with immediate family members with ASD'
)

# ==================================== CASE 03 ====================================
# Occurrences of ASD cases by different ethnicities

# Fitting Labels horizontally
par(las = 2) # Make label text perpendicular to axis
par(mar = c(5, 8, 4, 2)) # Increase y-axis margin.

# Make table with the desired values
ethnicity_asd <- table(dataframe_autism$class_asd, dataframe_autism$ethnicity)

# Plot + sort
barplot(ethnicity_asd[,order(apply(ethnicity_asd, 2, max))],
        main = 'Ethnicity x ASD Cases',
        sub = 'ASD occurrences by different Ethnicities',
        xlab = 'ASD Cases',
        col = c('green', 'purple'),
        space = 0.2,
        horiz = TRUE,
        cex.names = 0.9,
        xlim = c(0, 250),
        legend = rownames(ethnicity_asd),
        args.legend = list(
            x = 230,
            y = 10,
            bty = 'a'
        )
)

# Make table this time only with ASD positive cases
ethnicity_asd_yes <- table(filter_asd_yes$class_asd, filter_asd_yes$ethnicity)

# Plot + sort
barplot(ethnicity_asd_yes[,order(apply(ethnicity_asd, 2, max))],
        main = 'Ethnicity x Positive ASD Cases',
        sub = 'ASD Positive occurrences by different Ethnicities',
        xlab = 'ASD Cases',
        col = c('green', 'purple'),
        space = 0.2,
        horiz = TRUE,
        cex.names = 0.9,
        xlim = c(0, 120)
)

# ==================================== CASE 04 ====================================
# Country of Residence x ASD cases

# Make table with the desired values
country_of_residence <- table(dataframe_autism$class_asd, dataframe_autism$country_of_residence)

# Dividing into sections first/second/third world

# First world
new_zeland  <- filter(dataframe_autism, country_of_residence %in% c('New Zealand'))
united_states <- filter(dataframe_autism, country_of_residence %in% c('United States'))
united_kingdom <- filter(dataframe_autism, country_of_residence %in% c('United Kingdom'))

count(new_zeland, class_asd)
count(united_states, class_asd)
count(united_kingdom, class_asd)

# New Zealand + United States + United Kingdom
yes_first_world_asd <- 13 + 53 + 29
no_first_world_asd <- 62 + 59 + 47

# Second world
romania <- filter(dataframe_autism, country_of_residence %in% c('Romania'))
armenia <- filter(dataframe_autism, country_of_residence %in% c('Armenia'))
serbia <- filter(dataframe_autism, country_of_residence %in% c('Serbia'))

count(romania, class_asd)
count(armenia, class_asd)
count(serbia, class_asd)

# Romania + Armenia + Serbia
yes_second_world_asd <- 1 + 1 + 0
no_second_world_asd <- 2 + 2 + 1

# Third world
sri_lanka <- filter(dataframe_autism, country_of_residence %in% c('Sri Lanka'))
india <- filter(dataframe_autism, country_of_residence %in% c('India'))
united_arab_emirates <- filter(dataframe_autism, country_of_residence %in% c('United Arab Emirates'))

count(sri_lanka, class_asd)
count(india, class_asd)
count(united_arab_emirates, class_asd)

# Sri Lanka + India + United Arab Emirates
yes_third_world_asd <- 0 + 6 + 3
no_third_world_asd <- 14 + 75 + 64

# Comparing the sum by world (diagnosed and not)

# 3rd World
slices <- c(yes_third_world_asd, no_third_world_asd)
lbls <- c('ASD Diganosed', 'Non-ASD')
pct <- round(slices / sum(slices) * 100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,'%', sep = '')

pie(slices,
    labels = lbls,
    col = c('purple', 'green'),
    main = 'ASD cases on 3rd World Countries',
    sub = 'ASD percentage occurrences of ASD cases on 3rd World counties'
)

# 2nd World
slices <- c(yes_second_world_asd, no_second_world_asd)
lbls <- c('ASD Diganosed', 'Non-ASD')
pct <- round(slices / sum(slices) * 100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,'%', sep = '')

pie(slices,
    labels = lbls,
    col = c('purple', 'green'),
    main = 'ASD cases on 2nd World Countries',
    sub = 'ASD percentage occurrences of ASD cases on 2nd World counties'
)

# 1st World
slices <- c(yes_first_world_asd, no_first_world_asd)
lbls <- c('ASD Diganosed', 'Non-ASD')
pct <- round(slices / sum(slices) * 100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,'%', sep = '')

pie(slices,
    labels = lbls,
    col = c('purple', 'green'),
    main = 'ASD cases on 1st World Countries',
    sub = 'ASD percentage occurrences of ASD cases on 1st World counties'
)

# Compairing the positive asd cases in first and third 
slices <- c(yes_first_world_asd, yes_second_world_asd, yes_third_world_asd)
lbls <- c('ASD First World', 'ASD Second World', 'ASD Third World')
pct <- round(slices / sum(slices) * 100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,'%', sep = '')

pie(slices,
    labels = lbls,
    col = c('purple', 'green', 'orange'),
    main = 'ASD Positive cases on [1st / 2nd / 3rd] World',
    sub = '1st, 2nd and 3rd World ASD positive cases comparsions'
)

# ==================================== CASE 05 ====================================
# ASD ocurrences based on age range - generation 

# Add new column for generation
for (i in 1:nrow(dataframe_autism)) {
    if (dataframe_autism$age[i] <= 24)
        dataframe_autism$generation[i] <- 'Gen Z'
    if (dataframe_autism$age[i] >= 25 && dataframe_autism$age[i] <= 39)
        dataframe_autism$generation[i] <- 'Millennials'
    if (dataframe_autism$age[i] >= 34 && dataframe_autism$age[i] <= 44)
        dataframe_autism$generation[i] <- 'Xennials'
    if (dataframe_autism$age[i] >= 40 && dataframe_autism$age[i] <= 54)
        dataframe_autism$generation[i] <- 'Gen X'
    if (dataframe_autism$age[i] >= 55 && dataframe_autism$age[i] <= 73)
        dataframe_autism$generation[i] <- 'Boomer'
}

generations_asd <- table(dataframe_autism$class_asd, dataframe_autism$generation)

barplot(generations_asd[,order(apply(generations_asd, 2, max))],
        main = 'Generations x ASD Cases',
        sub = 'ASD Positive occurrences by different generations',
        xlab = 'ASD Cases',
        col = c('green', 'purple'),
        space = 0.2,
        horiz = TRUE,
        cex.names = 0.9,
        xlim = c(0, 250),
        legend = rownames(ethnicity_asd),
        args.legend = list(
            x = 250,
            y = 2.2,
            bty = 'a'
        )
)

# ==================================== CASE 06 ====================================
# Occurrences of ASD on individuals for who is completing their A10 test

filter_asd_yes <- filter(dataframe_autism, class_asd %in% c('YES'))

par(las = 2) # Make label text perpendicular to axis
par(mar = c(5, 8, 4, 2)) # Increase y-axis margin.

who_test_asd <- table(dataframe_autism$class_asd, dataframe_autism$who_is_completing_the_test)

barplot(who_test_asd[,order(apply(who_test_asd, 2, max))],
     main = 'Who completed the A10 Test x ASD Cases',
     sub = 'ASD Positive occurrences for who is completing the A10 Test by patient',
     col = c('green', 'purple'),
     space = 0.2,
     horiz = TRUE,
     cex.names = 0.8,
     legend = rownames(who_test_asd),
     xlim = c(0, 600),
     args.legend = list(
         x = 600,
         y = 3,
         bty = 'a'
     )
)

# ==================================== CASE 07 ====================================
# Mean values for A10 test results based on who completed the test

# Fitting Labels horizontally
par(las = 2) # Make label text perpendicular to axis
par(mar = c(5, 8, 4, 2)) # Increase y-axis margin.

self_occurences <- filter(dataframe_autism, who_is_completing_the_test %in% c('Self'))
parent_occurences <- filter(dataframe_autism, who_is_completing_the_test %in% c('Parent'))
relative_occurences <- filter(dataframe_autism, who_is_completing_the_test %in% c('Relative'))
others_occurences <- filter(dataframe_autism, who_is_completing_the_test %in% c('Others'))
professional_occurences <- filter(dataframe_autism, who_is_completing_the_test %in% c('Health care professional'))

means <- vector()
means[1] <- round(nrow(filter(self_occurences, class_asd %in% c('YES'))) / nrow(self_occurences) * 100)
means[2] <- round(nrow(filter(parent_occurences, class_asd %in% c('YES'))) / nrow(parent_occurences) * 100)
means[3] <- round(nrow(filter(relative_occurences, class_asd %in% c('YES'))) / nrow(relative_occurences) * 100)
means[4] <- round(nrow(filter(others_occurences, class_asd %in% c('YES'))) / nrow(others_occurences) * 100)
means[5] <- round(nrow(filter(professional_occurences, class_asd %in% c('YES'))) / nrow(professional_occurences) * 100)

occurrences_who_test_asd <- as.data.frame(means)

barplot(means,
        main = 'Who completed the A10 Test Occurences',
        sub = 'ASD Positive occurrences for whom is completing the A10 Test by patient',
        names.arg = c('Self', 'Parent', 'Relative', 'Others', 'H.Care Professional'),
        col = 'purple',
        horiz = TRUE,
        xlim = c(0, 40),
        cex.names = 0.8,
)

# ==================================== CASE 08 ====================================
# A10 Decision Tree

# Expected results, the actual classes of the numbers
expectedResult <- as.vector(dataframe_autism[, ncol(dataframe_autism)])

# New dataframe with only the test boolean answers
dataframe_a10 <- dataframe_autism[,1:10]
dataframe_a10$class_asd <- dataframe_autism[,21]

# Setting seed for random number generator (for each time we need a new train set)
set.seed(777)

# Getting round value for 80% of total row lines from the dataframe
sample_80percent <- floor(0.8 * nrow(dataframe_a10))

# Setting up training data 80%
train_index <- sample(seq_len(nrow(dataframe_a10)), size = sample_80percent)

# Collect the dataframe rows up to the 80% index
train <- dataframe_a10[train_index, ]
# Collect the dataframe rows up to the remaining 20%
test <- dataframe_a10[-train_index, ]

# Setting up the decision tree
decisionTree <- rpart(class_asd ~ ., train, method = "class", control = rpart.control(minsplit = 1))

# Plot the tree
plot <- rpart.plot(decisionTree, type = 3)

# Amount of samples that reach the node, amount of samples that doesn't belong to the majority class
classif <- test[,ncol(dataframe_a10)]
test <- test[,-ncol(dataframe_a10)]
pred <- predict(decisionTree, test, type = "class") # Prob or class

# Accuracy measures for Decision Tree
treeAccuracy <- length(which(pred == expectedResult))/length(expectedResult)

barplot(treeAccuracy,
        main = 'Decision Tree Accuracy',
        xlab = 'Decision Tree',
        col = 'green',
        ylim = c(0, 1),
        xlim = c(0, 1),
        width = c(0.1, 0.1)
)

treeAccuracy