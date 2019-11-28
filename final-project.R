# Package for reading .arff files
library(foreign)

# Packages for plots (tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)

# Package for 3D plots
library(scatterplot3d)

# Package for Spider / Radar plots
library(fmsb)

# Creating dataframe object & reading the dataset file
path <- './Autism-Adult-Data.arff'
dataframe_autism <- data.frame()
dataframe_autism <- read.arff(path)

# Removing line 53 from the dataframe - Outlier / Noise data found (383 years old woman)
dataframe_autism <- dataframe_autism[-c(53), ]

# Cheking & eliminating NA data (95 rows will be gone)
any(is.na(dataframe_autism))
dataframe_autism <- na.omit(dataframe_autism)

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

ehtnicity_asd <- table(dataframe_autism$class_asd, dataframe_autism$ethnicity)

barplot(ehtnicity_asd,
        main = 'Ethnicity x ASD Cases',
        sub = 'ASD Positive occurrences by different Ethnicities',
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

barplot(who_test_asd,
        main = 'Who completed A10 Test x ASD Cases',
        sub = 'ASD Positive occurrences for who is completing the A10 Test by patient',
        col = c('green', 'purple'),
        space = 0.2,
        horiz = TRUE,
        cex.names = 0.8,
        legend = rownames(who_test_asd),
        args.legend = list(
            x = 500,
            y = 3,
            bty = 'a'
            )
        )

# ==================================== CASE 05 ====================================
# Mean values for A10 test results based on who completed the test
    
# CLOUDY nesse eu tava pensando em usar um spider / radar plot, roda pra tu ver como é bonitooo, a gente cosegue até ver no shape q gera, como q vai (talvez) vá ficar mó elevação nas partes q não são a self (caso nossa teoria esteja certa, pra +casos de asd na galera q n respondeu por sí o teste)

# Create data: note in High school for Jonathan:
data <- as.data.frame(matrix( sample( 2:20 , 5 , replace=T) , ncol=5))
colnames(data) <- c("Health Care Professional", "Parent" , "Relative" , "Others" , "Self")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(20,10) , rep(0,10) , data)

# Check your data, it has to look like this!
# head(data)

# The default radar chart 
radarchart(data,
           pcol = 'green',
           pfcol = rgb(0.647, 0.486, 0.941, 0.7),
           plwd = 1,
           cglcol = 'black',
           cglwd = 0.8
           )

radarchart(
    
          )
# ==================================== CASE 06 ====================================
# Country of Residence x ASD cases

# ==================================== CASE 07 ====================================
# ???

# ==================================== CASE 08 ====================================
# Ethnicity x Jaundice x ASD positive

scatterplot3d(filter_jaundice_asd,
              x = filter_jaundice_asd$ethnicity,
              y = filter_jaundice_asd$jaundice,
              z = filter_jaundice_asd$class_asd,
              main = 'Ethnicity x Jaundice x ASD Positive',
              sub = '',
              xlab = '',
              ylab = '',
              zlab = '',
              color = colors
)



# DOING
# Plot stacked bars de análise de % de quando preenchido pela propria pessoa / outros com ASD
# Plot de radar para médias e pontuações em relação à quem/oq do paciente fez o teste

# TODO MAIN
# FIX other ETHNICITY
# Árvore de decisão para relevância de cada questão do teste A10
# Definir como plotar fodamente essa questão do A10 e a relevância dele (NOSSO CORE!!)
# Definir acurácia pra essa árvore de decisão
# Completar o paper no docs

# TODO SIDE
# Fix sort pra barplots serem em ordem
# Fix ggplot para barplots terem grid
# Plots bonitos (um melhor piechart)
# Fazer pie values via table de count (Vitor)




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

