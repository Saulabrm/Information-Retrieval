library(dplyr)
library(stringi)
library(gtools)

##### Load Data
setwd("/opt/lucene-3.6.2")
words <- read.csv("someFiles.txt", col.names = c("Word","Frequency"), encoding = "UTF-8",stringsAsFactors = FALSE, sep =" ")
## Fit the graph with the function.. the exponential functuin f=c*(rank + b) ^ a ) .. or use the logarithm to fit to a linear.

##### Cleaning #####
# Remove numbers
words <- filter(words, !grepl('[[:digit:]]+', Word ))

# Remove anything with punctuation or webpages.
words <- filter(words, !grepl('[[:punct:]]+', Word ))

# Remove roman numbers
# WORKS REALLY SLOW may be we should just keep them
isroman <- roman2int(words$Word)
words <- words[is.na(isroman),]

# Remove words with Accents
unwanted_array = list(    'S', 's', 'Z', 'z', 'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É',
                          'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', 'Ø', 'Ù',
                          'Ú', 'Û', 'Ü', 'Ý', 'Þ', 'ß', 'à', 'á', 'â', 'ã', 'ä', 'å', 'æ', 'ç',
                          'è', 'é', 'ê', 'ë', 'ì', 'í', 'î', 'ï', 'ð', 'ñ', 'ò', 'ó', 'ô', 'õ',
                          'ö', 'ø', 'ù', 'ú', 'û', 'ý', 'ý', 'þ', 'ÿ' )

words <-  filter(words, !grepl(paste(unwanted_array, collapse='|'), Word ))

#Remove the last ones
words <- words[1:(length(words$Word)-9),]
# write.table(words, "processed.txt")


####### Create Frequency-Rank Table #######
# Arrange
words = words %>% arrange(desc(Frequency))
head(words)

# Add Rank
words$Rank = 1:nrow(words)

#Plot Frequency vs. Rank
plot(words$Rank, words$Frequency, xlab= "Rank", ylab="Frequency", main="Word Distribution")

# Remove the frequencies lower than 1
words <- words %>% filter(Frequency > 1)
plot(words$Rank, words$Frequency, xlab= "Rank", ylab="Frequency", main="Word Distribution")


#Logarithm
x <- log(words$Rank)
y <- log(words$Frequency)
df <- data.frame(x,y)

x_rank <- words$Rank
 
plot(x, y, xlab = "log(Rank)", ylab = "log(Frequency)", main = "Zipf's Law")


zipf <- function(rank, a,b,c) {
  return(log(c)+a*log(rank+b))
}

# Choosing the best parameters visually
lines(x, zipf(x_rank,-1.3,18,1000000),col = "green", lwd=3)

# Estimating the parameters
fitted <- nls(y ~ zipf(x_rank,a,b,c), data = df, start = list(a=-1.3, b=18, c=1000000), trace = T)

# Plotting the Zipf's law with the parameters estimated
lines(x, predict(fitted),lty=2,col="lightblue",lwd=4)
Zipfs_coef = coef(fitted)

##### HEAPS LAW ######

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

