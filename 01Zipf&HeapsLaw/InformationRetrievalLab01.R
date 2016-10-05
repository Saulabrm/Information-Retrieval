library(dplyr)
library(stringi)
setwd("/opt/lucene-3.6.2")
words <- read.csv("someFiles.txt", col.names = c("Word","Frequency"), encoding = "UTF-8",stringsAsFactors = FALSE, sep =" ")

## Fit the graph with the function.. the exponential functuin f=c*(rank + b) ^ a ) .. or use the logarithm to fit to a linear.
## Python scipy -- curve_fit

# Remove numbers
words <- filter(words, !grepl('[[:digit:]]+', Word ))
dim(words)
#Remove roman numbers
words <- filter(words, !grepl("^m{0,4}(cm|cd|d?c{0,3})(xc|xl|l?X{0,3})(ix|iv|v?i{0,3})$", Word ))
dim(words)

# Remove anything with punctuation or webpages.
words <- filter(words, !grepl('[:punct:]+', Word ))
dim(words)

# Remove non ASCII
words <-  filter(words, !grepl("[^0-9A-Za-z///' ]",Word))
words <-  filter(words, !grepl("'",Word))
words <-  filter(words, !grepl("_",Word))

test <-  filter(test, !grepl('[:punct:]+', Word ))
head(test)



# Arrange
words = words %>% arrange(desc(Frequency))

# Add Rank
words$Rank = 1:nrow(words)

plot(words$Frequency,words$Rank)


# Remove the frequencies lower than 2
words <- words %>% filter(Frequency > 1)
plot(words$Frequency,words$Rank)

#Logarithm
plot(log(words$Frequency),log(words$Rank))



## =====
setwd("/opt/lucene-3.6.2/Data")
files = list.files()[1:5]

novels_collection = matrix(0,nrow= length(files),ncol =2)
for(i in 1:length(files)){
  x <- read.csv(files[i], col.names = c("DistinctWords","WordOccurrences"), stringsAsFactors = FALSE, sep =" ")
  b = tail(x,3)[2,1]
  novels_collection[i,1] <- as.numeric(substr(b, 1, nchar(b)-1))
  novels_collection[i,2] <- as.numeric(tail(x,3)[3,2])
}
novels_collection <-  as.data.frame(novels_collection)
colnames(novels_collection) =  c("DistinctWords","WordOccurrences")
Novels <- c(1:length(n))
novels_collection = cbind("Number of novels" = Novels, novels_collection)

print(novels_collection)

# Function for Heaps law
heaps <- function(K, n, B){
  K*n^B
}
heaps(2,117795,.7)
# y = K * n ^ B ==> log(y) = log(K) + B * log(n)

fitHeaps <- lm(log(DistinctWords) ~ log(WordOccurrences), data = novels_collection[,2:3])

logK <- summary(fitHeaps)$coef[1]
B <-  summary(fitHeaps)$coef[2]
K = exp(logK)

EstDistWords = heaps(K,novels_collection$WordOccurrences,B)
EstDistWords


#Plot of Data
plot(novels_collection$WordOccurrences, novels_collection$DistinctWords,
     xlab= "Word Occurrences (N)", ylab="Distinct Words (d)", main="Heap's Law", pch=19)
lines(novels_collection$WordOccurrences,EstDistWords, lwd = 3, col="lightblue")


logy <- log(novels_collection$DistinctWords)
logn <- log(novels_collection$WordOccurrences)
fit <- lm(logy ~ logn)
fit
para <- coef(fit)  ## log(K) and B
para[1] <- exp(para[1])    ## K and B
