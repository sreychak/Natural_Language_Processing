#importing the dataset
dataset_original = read.delim("Restaurant_Reviews.tsv",
                     header = TRUE, sep = "\t",
                     quote='', 
                     stringsAsFactors = FALSE)
#Cleaning the text
#install.packages('tm')
#install.packages('SnowballC')
#install.packages('caTools')
library(SnowballC)
library(tm)
library(caTools)
Corpus = VCorpus(VectorSource(dataset$Review))
#lower letter
Corpus = tm_map(Corpus,content_transformer(tolower))
#remove numbers
Corpus = tm_map(Corpus,removeNumbers)
#Remove punctuations
Corpus = tm_map(Corpus,removePunctuation)
#Remove stop words
Corpus = tm_map(Corpus,removeWords, stopwords())
#stemming
Corpus = tm_map(Corpus,stemDocument)
#remove spaces
Corpus = tm_map(Corpus,stripWhitespace)

# Creating the bag of word model
#We will count 1000 columns 1 column for each review
dtm = DocumentTermMatrix(Corpus)
dtm = removeSparseTerms(dtm, 0.99)

#Creating classification model using random forest
dataset = as.data.frame(as.matrix(dtm))
dataset$Liked = dataset_original$Liked
set.seed(123)

dataset$Liked = factor(dataset$Liked, levels = c(0,1))
split = sample.split(dataset$Liked, SplitRatio = 0.80)
training_set = subset(dataset, split==TRUE)
test_set = subset(dataset, split==FALSE)

#Fitting random forest classififcation
#install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-97],
                          y = training_set$Liked,
                          ntree = 10)
#Predicting the test set results
y_pred = predict(classifier,newdata = test_set[-97])

#Making the confusion Matrix
cm = table(test_set[, 97], y_pred)
