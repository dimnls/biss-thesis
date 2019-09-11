library('ggplot2')
library('dplyr')
library('sp')
library('tidyverse')
library('fields')
library('ggpubr')

measures <- read.csv('measures.csv')

# Song names are in different order due to adjustments needed
# because of different order in survey results.
# The graphs are displayed in the correct order of the songs in the survey. 

# Histograms of x values for songs
s1x <- ggplot(measures, aes(x=measures[,2])) + 
  ggtitle("Song 16") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,2])), color='red', linetype='dashed', size=1)

s2x <- ggplot(measures, aes(x=measures[,5])) + 
  ggtitle("Song 2") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,5])), color='red', linetype='dashed', size=1)

s3x <- ggplot(measures, aes(x=measures[,8])) + 
  ggtitle("Song 1") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,8])), color='red', linetype='dashed', size=1)

s4x <- ggplot(measures, aes(x=measures[,11])) + 
  ggtitle("Song 7") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,11])), color='red', linetype='dashed', size=1)

s5x <- ggplot(measures, aes(x=measures[,14])) + 
  ggtitle("Song 15") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,14])), color='red', linetype='dashed', size=1)

s6x <- ggplot(measures, aes(x=measures[,17])) + 
  ggtitle("Song 20") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,17])), color='red', linetype='dashed', size=1)

s7x <- ggplot(measures, aes(x=measures[,20])) + 
  ggtitle("Song 10") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,20])), color='red', linetype='dashed', size=1)

s8x <- ggplot(measures, aes(x=measures[,23])) + 
  ggtitle("Song 14") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,23])), color='red', linetype='dashed', size=1)

s9x <- ggplot(measures, aes(x=measures[,26])) + 
  ggtitle("Song 9") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,26])), color='red', linetype='dashed', size=1)

s10x <- ggplot(measures, aes(x=measures[,29])) + 
  ggtitle("Song 8") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,29])), color='red', linetype='dashed', size=1)

s11x <- ggplot(measures, aes(x=measures[,32])) + 
  ggtitle("Song 19") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,32])), color='red', linetype='dashed', size=1)

s12x <- ggplot(measures, aes(x=measures[,35])) + 
  ggtitle("Song 13") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,35])), color='red', linetype='dashed', size=1)

s13x <- ggplot(measures, aes(x=measures[,38])) + 
  ggtitle("Song 3") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,38])), color='red', linetype='dashed', size=1)

s14x <- ggplot(measures, aes(x=measures[,41])) + 
  ggtitle("Song 18") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,41])), color='red', linetype='dashed', size=1)

s15x <- ggplot(measures, aes(x=measures[,44])) + 
  ggtitle("Song 11") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,44])), color='red', linetype='dashed', size=1)

s16x <- ggplot(measures, aes(x=measures[,47])) + 
  ggtitle("Song 17") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,47])), color='red', linetype='dashed', size=1)

s17x <- ggplot(measures, aes(x=measures[,50])) + 
  ggtitle("Song 4") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,50])), color='red', linetype='dashed', size=1)

s18x <- ggplot(measures, aes(x=measures[,53])) + 
  ggtitle("Song 5") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,53])), color='red', linetype='dashed', size=1)

s19x <- ggplot(measures, aes(x=measures[,56])) + 
  ggtitle("Song 6") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,56])), color='red', linetype='dashed', size=1)

s20x <- ggplot(measures, aes(x=measures[,59])) + 
  ggtitle("Song 12") +
  xlab("Warm - Cool Values (X)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,59])), color='red', linetype='dashed', size=1)

######
# Group histograms for X values
ggarrange(s3x, s2x, s13x, s17x, s18x, s19x, s4x, s10x, s9x, s7x, s15x, s20x, s12x, s8x, s5x, s1x, s16x, s14x, s11x, s6x,
          ncol = 4, nrow = 5)

# Histograms of y values for songs
s1y <- ggplot(measures, aes(x=measures[,3])) + 
  ggtitle("Song 16") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,3])), color='red', linetype='dashed', size=1)

s2y <- ggplot(measures, aes(x=measures[,6])) + 
  ggtitle("Song 2") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,6])), color='red', linetype='dashed', size=1)

s3y <- ggplot(measures, aes(x=measures[,9])) + 
  ggtitle("Song 1") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,9])), color='red', linetype='dashed', size=1)

s4y <- ggplot(measures, aes(x=measures[,12])) + 
  ggtitle("Song 7") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,12])), color='red', linetype='dashed', size=1)

s5y <- ggplot(measures, aes(x=measures[,15])) + 
  ggtitle("Song 15") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,15])), color='red', linetype='dashed', size=1)

s6y <- ggplot(measures, aes(x=measures[,18])) + 
  ggtitle("Song 20") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,18])), color='red', linetype='dashed', size=1)

s7y <- ggplot(measures, aes(x=measures[,21])) + 
  ggtitle("Song 10") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,21])), color='red', linetype='dashed', size=1)

s8y <- ggplot(measures, aes(x=measures[,24])) + 
  ggtitle("Song 14") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,24])), color='red', linetype='dashed', size=1)

s9y <- ggplot(measures, aes(x=measures[,27])) + 
  ggtitle("Song 9") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,27])), color='red', linetype='dashed', size=1)

s10y <- ggplot(measures, aes(x=measures[,30])) + 
  ggtitle("Song 8") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,30])), color='red', linetype='dashed', size=1)

s11y <- ggplot(measures, aes(x=measures[,33])) + 
  ggtitle("Song 19") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,33])), color='red', linetype='dashed', size=1)

s12y <- ggplot(measures, aes(x=measures[,36])) + 
  ggtitle("Song 13") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,36])), color='red', linetype='dashed', size=1)

s13y <- ggplot(measures, aes(x=measures[,39])) + 
  ggtitle("Song 3") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,39])), color='red', linetype='dashed', size=1)

s14y <- ggplot(measures, aes(x=measures[,42])) + 
  ggtitle("Song 18") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,42])), color='red', linetype='dashed', size=1)

s15y <- ggplot(measures, aes(x=measures[,45])) + 
  ggtitle("Song 11") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,45])), color='red', linetype='dashed', size=1)

s16y <- ggplot(measures, aes(x=measures[,48])) + 
  ggtitle("Song 17") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,48])), color='red', linetype='dashed', size=1)

s17y <- ggplot(measures, aes(x=measures[,51])) + 
  ggtitle("Song 4") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,51])), color='red', linetype='dashed', size=1)

s18y <- ggplot(measures, aes(x=measures[,54])) + 
  ggtitle("Song 5") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,54])), color='red', linetype='dashed', size=1)

s19y <- ggplot(measures, aes(x=measures[,57])) + 
  ggtitle("Song 6") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,57])), color='red', linetype='dashed', size=1)

s20y <- ggplot(measures, aes(x=measures[,60])) + 
  ggtitle("Song 12") +
  xlab("Hard - Soft Values (Y)") +
  ylab("Responses") +
  geom_histogram(binwidth=0.5, fill='darkblue') +
  geom_vline(aes(xintercept=mean(measures[,60])), color='red', linetype='dashed', size=1)

######

ggarrange(s3y, s2y, s13y, s17y, s18y, s19y, s4y, s10y, s9y, s7y, s15y, s20y, s12y, s8y, s5y, s1y, s16y, s14y, s11y, s6y,
          ncol = 4, nrow = 5)

####