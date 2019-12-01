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

# Creating dataframe object & reading the dataset file
path <- './Autism-Adult-Data.arff'
dataframe_autism <- data.frame()
dataframe_autism <- read.arff(path)

#Renaming 'others' to 'Others' in Ethnicity line 658
dataframe_autism[658,13]= 'Others'

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
lbls <- c('ASD', 'Non-ASD')
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
# Occurrences of ASD cases by different ethnicities

# Fitting Labels horizontally
par(las = 2) # Make label text perpendicular to axis
par(mar = c(5, 8, 4, 2)) # Increase y-axis margin.

# Make table with the desired values
ehtnicity_asd <- table(dataframe_autism$class_asd, dataframe_autism$ethnicity)
ehtnicity_asd <- ehtnicity_asd[-c(12),,drop=F]

# Plot + sort
barplot(ehtnicity_asd[,order(apply(ehtnicity_asd, 2, max))],
        main = 'Ethnicity x ASD Cases',
        sub = 'ASD occurrences by different Ethnicities',
        xlab = 'ASD Cases',
        col = c('green', 'purple'),
        space = 0.2,
        horiz = TRUE,
        cex.names = 0.9,
        xlim = c(0, 250),
        legend = rownames(ehtnicity_asd),
        args.legend = list(
            x = 230,
            y = 10,
            bty = 'a'
            )
        )

# ==================================== CASE 03 ====================================
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
lbls <- c('ASD', 'Non-ASD')
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

# ==================================== CASE 04 ====================================
# Occurrences of ASD on individuals for who is completing their A10 test

filter_asd_yes <- filter(dataframe_autism, class_asd %in% c('YES'))

# Fitting Labels horizontally
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

# ==================================== CASE 05 ====================================
# Mean values for A10 test results based on who completed the test
    



# ==================================== CASE 06 ====================================
# Country of Residence x ASD cases

#O que ta ruim de ler
# Fitting Labels horizontally
par(las = 2) # Make label text perpendicular to axis
par(mar = c(5, 8, 4, 2)) # Increase y-axis margin.


# Make table with the desired values
country_of_residence <- table(dataframe_autism$class_asd, dataframe_autism$country_of_residence)

# Plot + sort
barplot(country_of_residence[,order(apply(country_of_residence, 2, max))],
        main = 'Country of residence x ASD Cases',
        sub = 'ASD occurrences by country that the individual lives',
        xlab = 'ASD Cases',
        col = c('green', 'purple'),
        space = 0.5,
        horiz = TRUE,
        cex.names = 0.9,
        xlim = c(0,120),
        legend = rownames(country_of_residence),
        args.legend = list(
            x = 100,
            y = 10,
            bty = 'a'
        )
)

#------Dividing into sections first world/second/third

#~~~First world
#Giovaniiiiiiiiiiiii ^???^ -
par(las = 2) # Make label text perpendicular to axis
par(mar = c(5, 8, 4, 2)) # Increase y-axis margin.



#first <- filter(dataframe_autism, country_of_residence %in% c('New Zealand','France','Ireland'))
nz <- filter(dataframe_autism, country_of_residence %in% c('New Zealand'))
fr<- filter(dataframe_autism, country_of_residence %in% c('France'))
irl<- filter(dataframe_autism, country_of_residence %in% c('Ireland'))
usa<- filter(dataframe_autism, country_of_residence %in% c('United States'))
ne<- filter(dataframe_autism, country_of_residence %in% c('Netherlands'))
ne<- filter(dataframe_autism, country_of_residence %in% c('Netherlands'))
arb<- filter(dataframe_autism, country_of_residence %in% c('United Arab Emirates'))
uni<-filter(dataframe_autism, country_of_residence %in% c('United Kingdom'))
count(arb, class_asd)


count(nz, class_asd)
count(fr,class_asd)
count(irl,class_asd)
count (usa,class_asd)
count(ne,class_asd)
count(uni,class_asd)

#New Zealand
nz_with_asd <- 13 # Yes
nz_without_asd <- 62 # No

#United States
usa_with_asd <- 53 # Yes
usa_without_asd <- 59 # No

#United Kingdom
usa_with_asd <- 29 # Yes
usa_without_asd <- 47 # No

yes_first_world_asd<-13+53+29
no_first_world_asd<-62+59+47

#Second world
romania <- filter(dataframe_autism, country_of_residence %in% c('Romania'))
count(romania, class_asd)
armania <- filter(dataframe_autism, country_of_residence %in% c('Armania'))
count(armenia, class_asd)
serbia <- filter(dataframe_autism, country_of_residence %in% c('Serbia'))
count(serbia, class_asd)

#Romania
romania_with_asd <- 1 # Yes
romania_without_asd <- 2 # No
#Armania
armania_with_asd <- 1 # Yes
armenia_without_asd <- 2 # No
#Serbia
Serbia_with_asd <- 0 # Yes
Serbia_without_asd <- 1 # No

#Third world
sri_lanka <- filter(dataframe_autism, country_of_residence %in% c('Sri Lanka'))
count(sri_lanka, class_asd)
india <- filter(dataframe_autism, country_of_residence %in% c('India'))
count(india, class_asd)
arb<- filter(dataframe_autism, country_of_residence %in% c('United Arab Emirates'))
count(arb, class_asd)

#Sri Lanka
sri_lanka_with_asd <- 0 # Yes
sri_lanka_without_asd <- 14 # No
#India
india_with_asd <- 6 # Yes
india_without_asd <- 75 # No
#United Arab Emirates
united_arab_em_with_asd <- 3 # Yes
united_arab_em_without_asd <- 64 # No

yes_third_world_asd<- 0 + 6 + 3
no_third_world_asd<-14+75+64


#Comparing the sum by world (diagnosed and not)

#====Compairing the third world



# Set values for the first world pie chart
slices <- c(yes_third_world_asd, no_third_world_asd)

# Set labels for slices of pie chart
lbls <- c('ASD Diagnosed', 'Non-ASD')
# Set the partitions
lbls <- paste(lbls, slices)

# Pie chart plot
pie(slices,
    labels = lbls,
    col = c('purple', 'green'),
    main = 'Autism cases on 3rd world countries',
    sub = '<pensar em algo melhor>'
)

#== Compairing to the first world

# Set values for the first world pie chart
slices <- c(yes_first_world_asd, no_first_world_asd)

# Set labels for slices of pie chart
lbls <- c('ASD Diagnosed', 'Non-ASD')
# Set the partitions
lbls <- paste(lbls, slices)

# Pie chart plot
pie(slices,
    labels = lbls,
    col = c('purple', 'green'),
    main = 'Autism cases on 3rd world countries',
    sub = '<pensar em algo melhor>'
)


#Compairing the positive asd cases in first and third 


# Set values for the first world pie chart
slices <- c(yes_first_world_asd, yes_third_world_asd)

# Set labels for slices of pie chart
lbls <- c('ASD First world', 'ASD Third world')



# Pie chart plot
pie(slices,
    labels = lbls,
    col = c('purple', 'green'),
    main = 'Autism cases on persons with Immediate family ASD',
    sub = 'Percentage times that Autism Spectrum Disorder (ASD) happenned on individuals with immediate family members with ASD'
)


# Giovani >:3 ->Aqui estao outras ideias, algumas ja estao com codigom so nao coloquei pq tava ficando muito "poluido" 
#Going to compare the India United States because of health care??
#Compare The asd positive 1rst world x 2rd and 3rd countries



# ==================================== CASE 07 ====================================
# A10 Decision Tree

# Expected results, the actual classes of the numbers
expectedResult <- as.vector(dataframe_autism[, ncol(dataframe_autism)])

# New dataframe with only the test boolean answers
dataframe_a10 <- dataframe_autism[,1:10]

# Setting seed for random number generator (for each time we need a new train set)
set.seed(777)

# Getting round value for 80% of total row lines from the dataframe
sample_80percent <- floor(0.8 * nrow(dataframe_a10))

# Setting up training data 80%
train_index <- sample(seq_len(nrow(dataframe_a10)), size = sample_80percent)

# Collect the dataframe rows up to the 80% index
train <- dataframe_autism[train_index, ]
# Collect the dataframe rows up to the remaining 20%
test <- dataframe_autism[-train_index, ]

# Setting up the decision tree
decisionTree <- rpart(class_asd ~ ., train, method = "class", control = rpart.control(minsplit = 1))

# Plot the tree
plot <- rpart.plot(decisionTree, type = 3)

# Amount of samples that reach the node, amount of samples that doesn't belong to the majority class
classif <- test[,ncol(dataframe_autism)]
test <- test[,-ncol(dataframe_autism)]
pred <- predict(decisionTree, test, type = "class") # Prob or class

# Accuracy measures for Decision Tree
treeAccuracy <- length(which(pred == expectedResult))/length(expectedResult)

# ==================================== CASE 08 ====================================
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
        legend = rownames(ehtnicity_asd),
        args.legend = list(
            x = 250,
            y = 2.2,
            bty = 'a'
        )
)
# ==================================== EXTRA 01 ====================================
# Years x ASD -> Years with ASD x each etnicity - to check if something whent in that country



#Giovani :3 -> Entao os de terceiro mundo nos top 3 tem a maior quantidade, sera que tiramos 1 pra ficar mais "even"?

# TODO MAIN
# FIX other ETHNICITY (Cloudy check)
# ELON MUSK (eiiitaaa, seria bem legal tho XD) - https://canaltech.com.br/saude/elon-musk-fala-em-curar-o-autismo-com-as-inovacoes-da-neuralink-155367/
# Completar o paper no docs

# TODO SIDE
# Barplots terem grid (usar ggplot é uma opção)
# Piechart -- para --> Doughnut




# Entender o dataset, e apresentar gráficos descritivos, com análises, que
# representem os dados. Para isso, será necessário utilizar técnicas de visualização
# de dados.

# Se for pertinente, aplicar técnicas de redução de dimensionalidade

# Aplicar um dos algoritmos estudados em Inteligência Artificial. Discutir a tarefa de
# aprendizado de máquinas sendo tratada, e a escolha do algoritmo.

# Apresentar e discutir os resultados obtidos.

# A data de entrega e apresentação do trabalho será dia 02/12, durante a aula.
# A dupla deverá, além de apresentar, entregar um relatório impresso, contendo também o
# código R para execução.

