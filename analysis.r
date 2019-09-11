install.packages('ggplot2')
install.packages('dplyr')
install.packages('sp')
install.packages('tidyverse')
install.packages('fields')
library('ggplot2')
library('dplyr')
library('sp')
library('tidyverse')
library('fields')


data <- read.csv('art_data.txt')
head(data)
typeof(data)

scaledData <- data
scaledData$X <- scale(data$X)
scaledData$Y <- scale(data$Y)

head(scaledData)

sortedX <- data[order(data$X),]
sortedY <- data[order(data$Y),]

median.X <- median(data$X)
median.Y <- median(data$Y)

# take sample of images, weighting more those furthest from the median origin
sampled.weighted <- sample_n(data, 2000, weight = sqrt( (data$X-median.X)^2+(data$Y-median.Y)^2 ) )
sampled <- sample_n(data, 2000 )

# characterize in which median quadrant each sample lies (sample.weighted)
for(row in 1:nrow(sampled.weighted)) {
  point.X <- sampled.weighted[row, 'X']
  point.Y <- sampled.weighted[row, 'Y']
  if(point.X > median.X) {
    if(point.Y > median.Y) {
      sampled.weighted[row, 'Quadrant'] = 1
    }
    else {
      sampled.weighted[row, 'Quadrant'] = 4
    }
  }
  else {
    if(point.Y > median.Y) {
      sampled.weighted[row, 'Quadrant'] = 2
    }
    else {
      sampled.weighted[row, 'Quadrant'] = 3
    }
  }
}

# characterize in which median quadrant each sample lies (all data)
for(row in 1:nrow(data)) {
  point.X <- data[row, 'X']
  point.Y <- data[row, 'Y']
  if(point.X > median.X) {
    if(point.Y > median.Y) {
      data[row, 'Quadrant'] = 1
    }
    else {
      data[row, 'Quadrant'] = 4
    }
  }
  else {
    if(point.Y > median.Y) {
      data[row, 'Quadrant'] = 2
    }
    else {
      data[row, 'Quadrant'] = 3
    }
  }
}

# check how many points in each quadrant
a <- table(sampled.weighted$Quadrant)
a

images.Q1 <- sampled.weighted[sampled.weighted$Quadrant==1,]
images.Q2 <- sampled.weighted[sampled.weighted$Quadrant==2,]
images.Q3 <- sampled.weighted[sampled.weighted$Quadrant==3,]
images.Q4 <- sampled.weighted[sampled.weighted$Quadrant==4,]


summary(data)
summary(sampled)
summary(sampled.weighted)

boxplot(data %>% select(X,Y))
boxplot(sampled %>% select(X,Y))
boxplot(sampled.weighted %>% select(X,Y))

# plot orignal data and sample
ggplot(data, aes(x=X, y=Y)) +
  geom_point(color='blue', shape=1) +
  stat_ellipse(color = 'green', level=0.7) +
  geom_point(aes(x=X, y=Y), sampled.weighted, color='red', shape=1) +
  stat_ellipse(aes(x=X, y=Y), sampled, size=2, color='yellow', level=0.7)


# plot sample & weighted sample, with mean and median lines separating in quarters
ggplot(sampled, aes(x=X, y=Y)) +
  geom_point(shape=1, color='blue') +
  geom_point(aes(x=X, y=Y), sampled.weighted, shape=1, color='red') +
  stat_ellipse(aes(x=X, y=Y, color='random'), sampled, size=2, level=0.90) +
  stat_ellipse(aes(x=X, y=Y, color='weighted'), sampled.weighted, size=2, level=0.90) +
  # geom_vline(xintercept=mean(data$X), color='blue') +
  # geom_hline(yintercept=mean(data$Y), color='blue') +
  geom_vline(xintercept=median(data$X), color='red') +
  geom_hline(yintercept=median(data$Y), color='red') +
  # scale_color_manual(name='Sample', values=c(random='blue', weighted='red')) +
  scale_color_manual(name='90% Conf. Ellipse', values=c(random='orange', weighted='green'))


# calculate points inside vs outside ellipse
p <- ggplot(data, aes(x=X, y=Y)) +
  geom_point() +
  stat_ellipse()

build <- ggplot_build(p)$data
points <- build[[1]]
ell <- build[[2]]

dat <- data.frame(
  points[1:2], 
  in.ell = as.logical(point.in.polygon(points$x, points$y, ell$x, ell$y))
)

points.inside <- data[dat$in.ell == 'TRUE',]
points.outside <- data[dat$in.ell == 'FALSE',]

ggplot(points.inside, aes(x=X, y=Y)) +
  geom_point(color='red') +
  geom_point(aes(x=X, y=Y), points.outside, color= 'blue')

ratio <- as.integer(length(points.inside$X) / length(points.outside$X))
# sample.inside.size = 1000/(1+ratio)
# sample.outside.size = sample.inside.size*ratio
sample.inside.size <- 350
sample.outside.size <- 650

sample.inside <- sample_n(points.inside, sample.inside.size)
sample.outside <- sample_n(points.outside, sample.outside.size)
sample.total <- rbind(sample.inside, sample.outside)

ggplot(sample.total, aes(x=X, y=Y)) +
  geom_point()

ggplot(data, aes(x=X, y=Y)) +
  geom_point(color='blue') +
  geom_point(aes(x=X, y=Y), sample.total, color='red')



# plot original and scaled data
ggplot(data, aes(x=X, y=Y)) +
  geom_point(size=2, shape=16, color='blue') +
  stat_ellipse(size = 2, color = 'green', level=0.7) +
  geom_point(aes(x=X, y=Y), scaledData, size=2, shape=16, color='red') +
  stat_ellipse(aes(x=X, y=Y), scaledData, size=2, color='yellow', level=0.7) +
  geom_point(aes(x=X, y=Y), sampledScaled, size=2, shape=16, color='green') +
  stat_ellipse(aes(x=X, y=Y), sampledScaled, size=2, color='purple', level=0.7)


hist(data$X)
plot(density(data$X))

##########################################

# set.seed(123)
image1 <- sample_n(sampled.weighted, 1)
image1row <- which(sampled.weighted$IMAGE == image1$IMAGE)
start.X <- image1$X[1]
start.Y <- image1$Y[1]

distances <- rdist(sampled.weighted[, 2:3])
# distances <- round(distances, 4)
distances[image1row, ]
top3distances <- tail(sort(distances[image1row, ]), 3)

images.rest <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("IMAGE", "X", "Y", "Quadrant"))

for(value in top3distances) {
  temp.id <- which(distances[image1row, ] == value)
  print(temp.id)
  images.rest <- rbind(images.rest, sampled.weighted[temp.id, ])
  print(sampled.weighted[temp.id, ])
}

# plot weighted sample with median axes
ggplot(sampled.weighted, aes(x=X, y=Y)) +
  geom_point(color=sampled.weighted$Quadrant) +
  # geom_point(aes(x=X, y=Y), image1, color='purple', shape=15, size=2) +
  # geom_point(aes(x=X, y=Y), images.rest, color='orange', shape=15, size=2) +
  # geom_point(aes(x=X, y=Y), image2, color='orange', shape=15, size=4) +
  # geom_point(aes(x=X, y=Y), image1, color='red', size=4, shape=15) +
  # geom_point(aes(x=X, y=Y), image3, color='green', size=4, shape=15) +
  geom_vline(xintercept=median.X, color='red') +
  geom_hline(yintercept=median.Y, color='red')

############################################################

# sampling on quadrants separately and heavily favoring distance from median center
image1 <- sample_n(images.Q1, 1, weight = 100*sqrt( (images.Q1$X-median.X)^2+(images.Q1$Y-median.Y)^2 ) )
image2 <- sample_n(images.Q2, 1, weight = 100*sqrt( (images.Q2$X-median.X)^2+(images.Q2$Y-median.Y)^2 ) )
image3 <- sample_n(images.Q3, 1, weight = 100*sqrt( (images.Q3$X-median.X)^2+(images.Q3$Y-median.Y)^2 ) )
image4 <- sample_n(images.Q4, 1, weight = 100*sqrt( (images.Q4$X-median.X)^2+(images.Q4$Y-median.Y)^2 ) )

# plot weighted sample with median axes and sampled images
ggplot(sampled.weighted, aes(x=X, y=Y)) +
  geom_point(color=sampled.weighted$Quadrant) +
  geom_point(aes(x=X, y=Y), image1, color='purple', shape=15, size=4) +
  geom_point(aes(x=X, y=Y), image2, color='purple', shape=15, size=4) +
  geom_point(aes(x=X, y=Y), image3, color='purple', shape=15, size=4) +
  geom_point(aes(x=X, y=Y), image4, color='purple', shape=15, size=4) +
  geom_vline(xintercept=median.X, color='red') +
  geom_hline(yintercept=median.Y, color='red')

#####

file1 <- paste(image1$IMAGE, '.jpg', sep='')
file2 <- paste(image2$IMAGE, '.jpg', sep='')
file3 <- paste(image3$IMAGE, '.jpg', sep='')
file4 <- paste(image4$IMAGE, '.jpg', sep='')

files <- data.frame(image1 = file1, image2 = file2, image3 = file3, image4 = file4)

# image_db <- files
image_db <- rbind(image_db, files)

write.csv(sampled.weighted, file="Sampled-Weighted (2000).csv")
write.csv(image_db, file="Image Filenames (20).csv")

image1row <- which(sampled.weighted$IMAGE == 15903956939)
sampled.weighted[image1row,]

newimage <- which.min(abs(sampled.weighted$Y-(-1.4)))
sampled.weighted[newimage,]

newimage <- which.min(sqrt( (sampled.weighted$X-1.7)^2 + (sampled.weighted$Y-1.4)^2 ))
sampled.weighted[newimage,]
