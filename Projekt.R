library(ggplot2)

fpath<-"unigram_freq.csv"
word_data<-read.csv(fpath,header=TRUE,stringsAsFactors = FALSE)

dim(word_data)
colnames(word_data)
str(word_data)
head(word_data)

word_data <-word_data[order(decreasing = TRUE, word_data$count),]
word_data$popularity <- 1:nrow(word_data)
word_data$length <- nchar(word_data$word)

View(word_data)

pl1 <- ggplot(word_data, aes(x=popularity, y=count))+geom_point()
pl1





max_len <- max(word_data$length)
max_len

word_length <- c(1:max_len)
word_count <- rep(0,max_len)
word_popularity <- rep(0,max_len)
for(row in 1:nrow(word_data)){
  word_count[word_data[row, "length"]] <- word_count[word_data[row, "length"]] + 1
  word_popularity[word_data[row, "length"]] <- word_popularity[word_data[row, "length"]] + word_data[row, "count"]
}

var_with_mean <- function(mean,vector){
  w <- vector - mean
  w <- w^2
  return(sum(w)/(length(vector)-1))
}

length_data <- data.frame(word_length, word_count, word_popularity)
View(length_data)



c1<-qchisq(0.025,max_len-1)
c2<-qchisq(0.975,max_len-1)
c1
c2
t<-qt(0.975,max_len-1)
t



pl2 <- ggplot(length_data, aes(x=word_length, y=word_popularity))+geom_point()
pl2
X1 <- weighted.mean(word_length,word_popularity)
X1
V1 = var_with_mean(X1,word_length)
V1
S1 = sqrt(V1)
S1
deviation_conf1a = S1*sqrt((max_len-1)/c2)
deviation_conf1b = S1*sqrt((max_len-1)/c1)
deviation_conf1a
deviation_conf1b
deviation_conf1b-deviation_conf1a
expected_value_conf1a = X1 - S1*t/sqrt(max_len)
expected_value_conf1b = X1 + S1*t/sqrt(max_len)
expected_value_conf1a
expected_value_conf1b
expected_value_conf1b - expected_value_conf1a



pl3 <- ggplot(length_data, aes(x=word_length, y=word_count))+geom_point()
pl3
X2<- weighted.mean(word_length,word_count) 
X2
V2 = var_with_mean(X2,word_length)
V2
S2 = sqrt(V2)
S2
deviation_confidence2a = S2*sqrt((max_len-1)/c2)
deviation_confidence2b = S2*sqrt((max_len-1)/c1)
deviation_confidence2a
deviation_confidence2b
deviation_confidence2b-deviation_confidence2a
expected_value_conf2a = X2 - S2*t/sqrt(max_len)
expected_value_conf2b = X2 + S2*t/sqrt(max_len)
expected_value_conf2a
expected_value_conf2b
expected_value_conf2b - expected_value_conf2a

ggplot(length_data, aes(x=word_count, y=word_popularity))+geom_point()
