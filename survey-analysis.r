### SURVEY ANALYSIS ###
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

survey <- read.csv('survey.csv')
tracks <- read.csv('20-stimuli.csv')

duplicated(survey$userid)

# Functions for user profile analysis 
men <- (subset(survey, survey$sex == 'Male'))
women <- (subset(survey, survey$sex == 'Female'))

nrow(subset(men, men$musicedu == 'yes'))

nrow(subset(survey, education == 'phd'))

profile.vars <- select(survey, age, musichrs)
summary(profile.vars)

musicedu.yes <- subset(survey, survey$musicedu == 'yes')
musicedu.no <- subset(survey, survey$musicedu == 'no')
########################################################

images <- read.csv('visual-stimuli.csv')
art.data <- read.csv('art_data.txt')

# visual stimuli
for(row in 1:nrow(images)) {
  images[row, 2] <- art.data[which(art.data$IMAGE == images[row, 1]), 2]
  images[row, 3] <- art.data[which(art.data$IMAGE == images[row, 1]), 3]
}

image.sets <- read.csv('ImageFilenames20.csv')

# find X, Y values of each set
for(i in 1:20) {
  set <- c(image.sets[i, 'image1'], image.sets[i, 'image2'], image.sets[i, 'image3'], image.sets[i, 'image4'])
  set.x <- c(1:4)
  set.y <- c(1:4)
  for(k in 1:4) {
    set.x[k] <- images[which(images$IMAGE == set[k]), 'X']
    set.y[k] <- images[which(images$IMAGE == set[k]), 'Y']
  }
  image.sets[i, 'Xmedian'] <- median(set.x)
  image.sets[i, 'Ymedian'] <- median(set.y)
  image.sets[i, 'Xmean'] <- mean(set.x)
  image.sets[i, 'Ymean'] <- mean(set.y)
}

write.csv(image.sets, 'ImageFilenames20.csv', row.names = FALSE)

# Create temp set with values (song 13)
temp.set <- images[1:4,]
temp.set$X <- NA
temp.set$Y <- NA

temp.set[1, 'IMAGE'] <- image.sets[13, 'image1']
temp.set[2, 'IMAGE'] <- image.sets[13, 'image2']
temp.set[3, 'IMAGE'] <- image.sets[13, 'image3']
temp.set[4, 'IMAGE'] <- image.sets[13, 'image4']

for(i in 1:4) {
  temp.set[i, 'X'] <- images[which(images$IMAGE == temp.set[i, 'IMAGE']), 'X']
  temp.set[i, 'Y'] <- images[which(images$IMAGE == temp.set[i, 'IMAGE']), 'Y']
}


# create list of images used in order, with values (DOESN'T WORK)
all.sets <- images
for(i in 1:20) {
  all.sets[ (i-1)*4+1 , 'IMAGE'] <- image.sets[i, 'image1']
  all.sets[ (i-1)*4+1 , 'X'] <- images[which(images$IMAGE == all.sets[(i-1)*4+1, 'IMAGE']), 'X']
  all.sets[ (i-1)*4+1 , 'Y'] <- images[which(images$IMAGE == all.sets[(i-1)*4+1, 'IMAGE']), 'Y']
  
  all.sets[ (i-1)*4+2 , 'IMAGE'] <- image.sets[i, 'image2']
  all.sets[ (i-1)*4+2 , 'X'] <- images[which(images$IMAGE == all.sets[(i-1)*4+2, 'IMAGE']), 'X']
  all.sets[ (i-1)*4+2 , 'Y'] <- images[which(images$IMAGE == all.sets[(i-1)*4+2, 'IMAGE']), 'Y']
  
  all.sets[ (i-1)*4+3 , 'IMAGE'] <- image.sets[i, 'image3']
  all.sets[ (i-1)*4+3 , 'X'] <- images[which(images$IMAGE == all.sets[(i-1)*4+3, 'IMAGE']), 'X']
  all.sets[ (i-1)*4+3 , 'Y'] <- images[which(images$IMAGE == all.sets[(i-1)*4+3, 'IMAGE']), 'Y']
  
  all.sets[ (i-1)*4+4 , 'IMAGE'] <- image.sets[i, 'image4']
  all.sets[ (i-1)*4+4 , 'X'] <- images[which(images$IMAGE == all.sets[(i-1)*4+4, 'IMAGE']), 'X']
  all.sets[ (i-1)*4+4 , 'Y'] <- images[which(images$IMAGE == all.sets[(i-1)*4+4, 'IMAGE']), 'Y']
  print(i)
}

# plot one set with median, average and shifted average (X)
ggplot(temp.set) +
  geom_point(aes(x=X, y=Y), size=3) +
  geom_vline(aes(xintercept=median(X)), color='red', linetype='dashed', size=1) +
  geom_text(aes(x=median(X), label="median X of the image set\n", y=0), color="red", angle=90) +
  geom_vline(aes(xintercept=mean(measures[,38])), color='green', linetype='dashed', size=1) +
  geom_text(aes(x=mean(measures[,38]), label="average X for song from survey\n", y=0), color="green", angle=90) +
  geom_vline(aes(xintercept=mean(measures[,38])-median(X)), color='purple', linetype='dashed', size=1) +
  geom_text(aes(x=mean(measures[,38])-median(X), label="adjusted average\n", y=0), color="purple", angle=90)

# plot one set with median, average and shifted average (Y)
ggplot(temp.set) +
  geom_point(aes(x=X, y=Y), size=3) +
  geom_hline(aes(yintercept=median(Y)), color='red', linetype='dashed', size=1) +
  geom_text(aes(y=median(Y), label="median Y of the image set\n", x=0), color="red", angle=0) +
  geom_hline(aes(yintercept=mean(measures[,39])), color='green', linetype='dashed', size=1) +
  geom_text(aes(y=mean(measures[,39]), label="average Y for song from survey\n", x=0), color="green", angle=0) +
  geom_hline(aes(yintercept=mean(measures[,39])-median(Y)), color='purple', linetype='dashed', size=1) +
  geom_text(aes(y=mean(measures[,39])-median(Y), label="adjusted average\n", x=0), color="purple", angle=0)


### Plot visual stimuli semantic space
ggplot(images, aes(x=X, y=Y)) +
  geom_point(color='blue') +
  ggtitle("Visual Stimuli") +
  xlab("Warm - Cool") +
  ylab("Hard - Soft") +
  geom_vline(aes(xintercept=median(X)), color='red', linetype='dashed', size=1) +
  geom_hline(aes(yintercept=median(Y)), color='red', linetype='dashed', size=1)

# keep only columns with song-image associations
survey.simple <- survey[, c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41)]
write.csv(survey.simple, 'survey-simple.csv', row.names = FALSE)

survey.simple <- read.csv('survey-simple.csv')
survey.clean <- read.csv('survey-clean.csv')

names(survey.clean) <- names(survey.simple)
song.ids <- names(survey.simple)

(song.ids == names(survey.clean))


measures <- read.csv('measures.csv')
# create measures table
for(col in seq(from=1, to=58, by=3)){
  for(row in 1:nrow(measures)) {
    image <- measures[row, col]
    measures[row, col+1] <- art.data[which(art.data$IMAGE == image), 2]
    measures[row, col+2] <- art.data[which(art.data$IMAGE == image), 3]
  }
}

for(col in seq(from=1, to=58, by=3)){
  id <- names(measures)[col]
  names(measures)[col+1] <- (paste(id, '___X', sep=''))
  names(measures)[col+2] <- (paste(id, '___Y', sep=''))
}
  

write.csv(measures, 'measures.csv', row.names = FALSE)
averages <- data.frame(songid = names(survey.simple), X = c(1:20), Y = c(1:20))

hist(measures[,2])

# to find column number in measures table

songcols <- c(1:20)
for(i in 1:20) {
  songcols[i] <- (i-1)*3+1
}
print(songcols)

xcols <- c(1:20)
for(i in 1:20) {
  xcols[i] <- (i-1)*3+2
}
print(xcols)

ycols <- c(1:20)
for(i in 1:20) {
  ycols[i] <- (i-1)*3+3
}
print(ycols)

###
# Consolidate song ids, mean values and adjusted mean values
song.values <- data.frame( 'song' = song.ids, 'avgX' = c(1:20), 'avgY' = c(1:20), 'adjX' = c(1:20), 'adjY' = c(1:20))
for(i in 1:20) {
  song.values[i, 'avgX'] <- mean(measures[, (i-1)*3+2])
  song.values[i, 'avgY'] <- mean(measures[, (i-1)*3+3])
  
  song.values[i, 'adjX'] <- mean(measures[, (i-1)*3+2]) - image.sets[i, 'Xmedian']
  song.values[i, 'adjY'] <- mean(measures[, (i-1)*3+3]) - image.sets[i, 'Ymedian']
}
write.csv(song.values, 'song-values.csv', row.names = FALSE)
song.values <- read.csv('song-values.csv')
ggplot(song.values) +
  geom_point(aes(x=adjX, y=adjY), size=3) +
  geom_point(aes(x=avgX, y=avgY), size=3, color='red', alpha=0.1)

# Fix order based on 20-stimuli file
audio.stimuli <- read.csv('20-stimuli.csv')
order <- c(1:20)
song.values$order <- NA
for(i in 1:20){
  song.values[i, 'order'] <- which(audio.stimuli$track.id == song.values[i, 'song'])
  order[i] <- which(audio.stimuli$track.id == song.values[i, 'song'])
}

song.values <- song.values[order(order),]
song.values <- subset(song.values, select=-order)
write.csv(song.values, 'song-values.csv', row.names = FALSE)
song.values <- read.csv('song-values.csv')
####

### Enrich 20-stimuli file with song values
audio.stimuli$avgX <- song.values$avgX
audio.stimuli$avgY <- song.values$avgY
audio.stimuli$adjX <- song.values$adjX
audio.stimuli$adjY <- song.values$adjY

write.csv(audio.stimuli, '20-stimuli.csv', row.names = FALSE)
