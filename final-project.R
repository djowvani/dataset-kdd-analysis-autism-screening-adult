# Package for reading .arff files
library(foreign)

# Packages for plots (tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)

# Package for 3D plots
library(scatterplot3d)

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
# Filter cases of ASD positive on occurrences of patients born with jundice
filter_jundice_asd <- filter(dataframe_autism, jundice %in% c("yes"))
    
# Get total of 'yes' & 'no' on class_asd
count(filter_jundice_asd, class_asd)
jundice_with_asd <- 28
jundice_without_asd <- 31

# Set values for slices of pie chart
slices <- c(jundice_with_asd, jundice_without_asd)

# Set labels for slices of pie chart
lbls <- c('ASD', 'Non-ASD')
# Rounds values to 100% format
pct <- round(slices / sum(slices)*100)
# Add percents to labels
lbls <- paste(lbls, pct)
# Add % to labels
lbls <- paste(lbls,'%', sep = '')

# Pie chart plot
pie(slices,
    labels = lbls,
    col = rainbow(length(lbls)),
    main = 'Occurrences of Autism on Jundice',
    sub = 'Percentage times that Autism Spectrum Disorder (ASD) happenned on positive cases of Jundice'
    )

# CASE 02 - Ethnicity x ASD positive
barplot(
    
        )

# CASE 03 - Ethnicity x Jundice x ASD posittive
colors <- c("#999999", "#E69F00", "#56B4E9")
scatterplot3d(filter_jundice_asd,
              x = filter_jundice_asd$ethnicity,
              y = filter_jundice_asd$jundice,
              z = filter_jundice_asd$class_asd,
              main = 'Ethnicity x Jundice x ASD posittive',
              sub = '',
              xlab = '',
              ylab = '',
              zlab = '',
              color = colors
              )

# CASE 04 - 
# CASE 05 - 
# CASE 06 - 
# CASE 07 - 
# CASE 08 - 


# analisar % de quando preenchido pela propria pessoa / outros com ASD





# Entender o dataset, e apresentar gráficos descritivos, com análises, que
# representem os dados. Para isso, será necessário utilizar técnicas de visualização
# de dados.


# Se for pertinente, aplicar técnicas de redução de dimensionalidade


# Aplicar um dos algoritmos estudados em Inteligência Artificial. Discutir a tarefa de
# aprendizado de máquinas sendo tratada, e a escolha do algoritmo.


# Apresentar e discutir os resultados obtidos.



# A data de entrega e apresentação do trabalho será dia 02/12, durante a aula.
# A dupla deverá, além de ap
