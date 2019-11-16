# Package for reading .arff files
library(foreign)

# Packages for plots (tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)

# Creating dataframe object & reading the dataset file
path <- './Autism-Adult-Data.arff'
dataframe_autism <- data.frame()
dataframe_autism <- read.arff(path)

# Removing line 53 from the dataframe - Outlier / Noise data found (383 years old woman)
dataframe_autism <- dataframe_autism[-c(53), ]

# Cheking & eliminating NA data (95 rows will be gone)
any(is.na(dataframe_autism))
dataframe_autism <- na.omit(dataframe_autism)

# Rename column 'Class/ASD' to 'class_asd'
names(dataframe_autism)[21] <- 'class_asd'

# CASE 01 - Relation between occurrences of jundice in patients diagnosed with ASD
# Get total of 'yes' & 'no' on class_asd
count(dataframe_autism, class_asd)
jundice_with_asd <- 180
jundice_without_asd <- 428

# Set values for slices of pie chart
slices <- c(jundice_with_asd, jundice_without_asd)

# Set labels for slices of pie chart with rounded for 100%
lbls <- c('ASD', 'Non-ASD')
pct <- round(slices / sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,'%', sep = '') # ad % to labels

# Pie chart plot
pie(slices,
    labels = lbls,
    col = rainbow(length(lbls)),
    main = 'Occurrences of Autism on Jundice',
    sub = 'Percentage times that Autism Spectrum Disorder (ASD) happenned on positive cases of Jundice'
    )


# Entender o dataset, e apresentar gráficos descritivos, com análises, que
# representem os dados. Para isso, será necessário utilizar técnicas de visualização
# de dados.


# Se for pertinente, aplicar técnicas de redução de dimensionalidade


# Aplicar um dos algoritmos estudados em Inteligência Artificial. Discutir a tarefa de
# aprendizado de máquinas sendo tratada, e a escolha do algoritmo.


# Apresentar e discutir os resultados obtidos.



# A data de entrega e apresentação do trabalho será dia 02/12, durante a aula.
# A dupla deverá, além de ap
