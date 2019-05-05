setwd("C:/Users/rohit/Desktop/Big data project/Final Report/ml-latest")
{
  .libPaths("C:/R")
}
install.packages("recommenderlab")
library(recommenderlab)
install.packages("ggplot2")
library(ggplot2)
install.packages("data.table")
library(data.table)
install.packages("reshape2")
library(reshape2)
movies <- read.csv(file = "movies.csv")
str(movies)
ratings <- read.csv(file = "ratings.csv")
str(ratings)
summary(movies)
head(ratings)
install.packages("onehot")
library(onehot)
genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
View(genres)
table(genres)
levels(movies$genres)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
head(genres2)
colnames(genres2) <- c(1:7)
View(genres2)
genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")
genre_matrix <- matrix(0,8571,18) #empty matrix
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list
genre_list
#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
   genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}
#convert into dataframe
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
} #convert from characters to integers

library(Matrix)
real = as(ratings, "realRatingMatrix")
real
dimnames(real)
rowCounts(real)
colCounts(real)
rowMeans(real)
as(real, "matrix")
as(real, "data.frame")
b <- binarize(real, minRating=4)
b

#View(genre_matrix2)
binaryratings <- ratings
for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] <- 1
  }
  else{
    binaryratings[i,3] <- -1
  }
}
View(binaryratings)

binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
binaryratings2 = binaryratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds
#View(binaryratings2)

#Remove rows that are not rated from movies dataset
movieIds <- length(unique(movies$movieId)) #8570
ratingmovieIds <- length(unique(ratings$movieId)) #8552
movies2 <- movies[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(movies2) <- NULL

#Remove rows that are not rated from genre_matrix2
genre_matrix3 <- genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(genre_matrix3) <- NULL

#Calculate dot product for User Profiles
result = matrix(0,18,706)
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c]))
  }
}

#Convert to Binary scale
for (i in 1:nrow(result)){
  if (result[i] < 0){
    result[i] <- 0
  }
  else {
    result[i] <- 1
  }
}
View(result)

resul-t2 <- result[1,] #First user's profile
sim_mat <- rbind.data.frame(result2, genre_matrix3)
sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) #convert data to type integer
View(result2)

#Calculate Jaccard distance between user profile and all movies
install.packages("proxy")
library(proxy)
sim_results <- dist(sim_mat, method = "Jaccard")
sim_results <- as.data.frame(as.matrix(sim_results[1:8552]))
rows <- which(sim_results == min(sim_results))
#Recommended movies
movies[rows,2]
head(result2)

#user based Filtering

library(reshape2)
#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds
head(ratingmat)


#Creation of the Recommender Model
#install.packages("recommenderlab")
library(recommenderlab)
#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

#Normalize the data
ratingmat_norm <- normalize(ratingmat)

#Create Recommender Model. "UBCF" stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat_norm, method = "UBCF", param=list(method="Cosine",nn=30))
recom <- predict(recommender_model, ratingmat[1], n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list <- as(recom, "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result <- matrix(0,10)



for (i in c(1:10)){
  recom_result[i] <- as.integer(recom_list[[1]][i])
}
recom_result<-as.data.frame(movies[recom_result,2])
colnames(recom_result)<-list("Top-10 Movies")
recom_result


evaluation_scheme <- evaluationScheme(ratingmat, method="cross-validation", k=5, given=3, 
                                      goodRating=5) 
algorithms<-list(
  "random items"=list(name="RANDOM",param=NULL),
  "popular items"=list(name="POPULAR",param=NULL),
  "user-based CF"=list(name="UBCF",param=list(method="Cosine",nn=30))
)

evaluation_results<-evaluate(evaluation_scheme,algorithms,n=c(1,3,5,10,15,20)) #n=c denote top-N
table(evaluation_results)
plot(evaluation_results,legend="bottomright") #plot the avged ROC
plot(evaluation_results,"prec/rec") #plot the avged prec/rec

#get results for all runs of 'random items'
eval_results <- getConfusionMatrix(evaluation_results[[1]]) 
#alternatively, get avged result for 'random items'
avg(evaluation_results[[1]])
install.packages("e1071")
require(e1071) #Holds the Naive Bayes Classifier
levels(movies$genres)
rating <- naiveBayes(rating~., data = ratings)
rating
