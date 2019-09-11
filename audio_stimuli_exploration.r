########## AUDIO STIMULI ANALYSIS #############
install.packages('ggplot2')
install.packages('dplyr')
install.packages('sp')
install.packages('tidyverse')
install.packages('fields')
install.packages('ggpubr')
library('ggplot2')
library('dplyr')
library('sp')
library('tidyverse')
library('fields')
library('ggpubr')

tracks <- read.csv('20-stimuli.csv')
View(tracks)
attributes <- tracks[,5:11]
View(attributes)
write.csv(summary(attributes), 'attributes-summary.csv')

sds <- c(1:7)
vars <- c(1:7)
cvs <- c(1:7)
names(sds) <- c('valence', 'energy', 'danceability', 'acousticness', 'liveness', 'tempo', 'speechiness')
names(vars) <- c('valence', 'energy', 'danceability', 'acousticness', 'liveness', 'tempo', 'speechiness')
names(cvs) <- c('valence', 'energy', 'danceability', 'acousticness', 'liveness', 'tempo', 'speechiness')

for(i in 1:7){
  
  sds[i] <- sd(attributes[, i])
  vars[i] <- var(attributes[, i])
  cvs[i] <- sds[i]/mean(attributes[, i])

}

