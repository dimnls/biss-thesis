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

art <- read.csv('art_data.txt')
beach <- read.csv('beach_data.txt')
birthday <- read.csv('birthday_data.txt')
california <- read.csv('california_data.txt')
family <- read.csv('family_data.txt')
friends <- read.csv('friends_data.txt')
nyc <- read.csv('nyc_data.txt')
sky <- read.csv('sky_data.txt')
snow <- read.csv('snow_data.txt')
summer <- read.csv('summer_data.txt')
travel <- read.csv('travel_data.txt')
wedding <- read.csv('wedding_data.txt')

data <- list(art, beach, birthday, california, family, friends, nyc, sky, snow, summer, travel, wedding)
names(data) <- c('art', 'beach', 'birthday', 'california', 'family', 'friends', 'nyc', 'sky', 'snow', 'summer', 'travel', 'wedding')

medians.X <- c(1:12)
medians.Y <- c(1:12)

sds.X <- c(1:12)
sds.Y <- c(1:12)
vars.X <- c(1:12)
vars.Y <- c(1:12)

names(sds.X) <- c('art', 'beach', 'birthday', 'california', 'family', 'friends', 'nyc', 'sky', 'snow', 'summer', 'travel', 'wedding')
names(sds.Y) <- c('art', 'beach', 'birthday', 'california', 'family', 'friends', 'nyc', 'sky', 'snow', 'summer', 'travel', 'wedding')

# find largest SD in X and Y
sds.X[match(max(sds.X), sds.X)]
sds.Y[match(max(sds.Y), sds.Y)]

# find medians, sd and var
for(i in 1:12){
  medians.X[i] <- median(data[[i]]$X)
  medians.Y[i] <- median(data[[i]]$Y)
  
  sds.X[i] <- sd(data[[i]]$X)
  sds.Y[i] <- sd(data[[i]]$Y)
  
  vars.X[i] <- var(data[[i]]$X)
  vars.Y[i] <- var(data[[i]]$Y)
  
}

# create graphs
art_gg <- ggplot(art, aes(x=X, y=Y)) +
  geom_point(color='blue', shape=1) +
  ggtitle("ART") +
  xlab("Warm - Cool") +
  ylab("Hard - Soft") +
  geom_vline(xintercept=medians.X[1], color='red') +
  geom_hline(yintercept=medians.Y[1], color='red')

beach_gg <- ggplot(beach, aes(x=X, y=Y)) +
  geom_point(color='blue', shape=1) +
  ggtitle("BEACH") +
  xlab("Warm - Cool") +
  ylab("Hard - Soft") +
  geom_vline(xintercept=medians.X[2], color='red') +
  geom_hline(yintercept=medians.Y[2], color='red')

birthday_gg <- ggplot(birthday, aes(x=X, y=Y)) +
  geom_point(color='blue', shape=1) +
  ggtitle("BIRTHDAY") +
  xlab("Warm - Cool") +
  ylab("Hard - Soft") +
  geom_vline(xintercept=medians.X[3], color='red') +
  geom_hline(yintercept=medians.Y[3], color='red')

california_gg <- ggplot(california, aes(x=X, y=Y)) +
  geom_point(color='blue', shape=1) +
  ggtitle("CALIFORNIA") +
  xlab("Warm - Cool") +
  ylab("Hard - Soft") +
  geom_vline(xintercept=medians.X[4], color='red') +
  geom_hline(yintercept=medians.Y[4], color='red')

family_gg <- ggplot(family, aes(x=X, y=Y)) +
  geom_point(color='blue', shape=1) +
  ggtitle("FAMILY") +
  xlab("Warm - Cool") +
  ylab("Hard - Soft") +
  geom_vline(xintercept=medians.X[5], color='red') +
  geom_hline(yintercept=medians.Y[5], color='red')

friends_gg <- ggplot(friends, aes(x=X, y=Y)) +
  geom_point(color='blue', shape=1) +
  ggtitle("FRIENDS") +
  xlab("Warm - Cool") +
  ylab("Hard - Soft") +
  geom_vline(xintercept=medians.X[6], color='red') +
  geom_hline(yintercept=medians.Y[6], color='red')

nyc_gg <- ggplot(nyc, aes(x=X, y=Y)) +
  geom_point(color='blue', shape=1) +
  ggtitle("NYC") +
  xlab("Warm - Cool") +
  ylab("Hard - Soft") +
  geom_vline(xintercept=medians.X[7], color='red') +
  geom_hline(yintercept=medians.Y[7], color='red')

sky_gg <- ggplot(sky, aes(x=X, y=Y)) +
  geom_point(color='blue', shape=1) +
  ggtitle("SKY") +
  xlab("Warm - Cool") +
  ylab("Hard - Soft") +
  geom_vline(xintercept=medians.X[8], color='red') +
  geom_hline(yintercept=medians.Y[8], color='red')

snow_gg <- ggplot(snow, aes(x=X, y=Y)) +
  geom_point(color='blue', shape=1) +
  ggtitle("SNOW") +
  xlab("Warm - Cool") +
  ylab("Hard - Soft") +
  geom_vline(xintercept=medians.X[9], color='red') +
  geom_hline(yintercept=medians.Y[9], color='red')

summer_gg <- ggplot(summer, aes(x=X, y=Y)) +
  geom_point(color='blue', shape=1) +
  ggtitle("SUMMER") +
  xlab("Warm - Cool") +
  ylab("Hard - Soft") +
  geom_vline(xintercept=medians.X[10], color='red') +
  geom_hline(yintercept=medians.Y[10], color='red')

travel_gg <- ggplot(travel, aes(x=X, y=Y)) +
  geom_point(color='blue', shape=1) +
  ggtitle("TRAVEL") +
  xlab("Warm - Cool") +
  ylab("Hard - Soft") +
  geom_vline(xintercept=medians.X[11], color='red') +
  geom_hline(yintercept=medians.Y[11], color='red')

wedding_gg <- ggplot(wedding, aes(x=X, y=Y)) +
  geom_point(color='blue', shape=1) +
  ggtitle("WEDDING") +
  xlab("Warm - Cool") +
  ylab("Hard - Soft") +
  geom_vline(xintercept=medians.X[12], color='red') +
  geom_hline(yintercept=medians.Y[12], color='red')

# group graphs into 2 grids
ggarrange(art_gg, beach_gg, birthday_gg, california_gg, family_gg, friends_gg, 
          ncol = 3, nrow = 2)

ggarrange(nyc_gg, sky_gg, snow_gg, summer_gg, travel_gg, wedding_gg, 
          ncol = 3, nrow = 2)

# all graphs in one grid
ggarrange(art_gg, beach_gg, birthday_gg, california_gg, family_gg, friends_gg, nyc_gg, sky_gg, snow_gg, summer_gg, travel_gg, wedding_gg, 
          ncol = 3, nrow = 4)


