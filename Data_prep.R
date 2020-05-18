##Please set your working directory before run the code
getwd()
setwd("...")

##Let's read data from CSV file and assign it to variable
raw_data<-read.csv("BBB_raw_data.csv")

##It's good to know if we are successful - we should check dimensions and print variable to visually check if data are 
##loaded correctly
dim(raw_data)

raw_data

raw_data[1,1]
raw_data[209,135]

raw_data[1,]

##Options to print/show just values from specific column - in our case its column nr 1
raw_data[,1]
raw_data$tpsa
raw_data[,"tpsa"]

raw_data[1]

raw_data[raw_data$logBBB<(-1.5),]
raw_data[raw_data[,135]<(-1.5),]

sapply(raw_data, class)

sapply(raw_data, class)

##Let's search for missing values in our dataset
is.na(raw_data)

##As searching visually is not easy and efficient let's try mathematic: FALSE is 0 whereas TRUE is 1 so...
sum(is.na(raw_data))

raw_data[is.na(raw_data[,"logBBB"]),]

colSums(is.na(raw_data))

colsNA<-colnames(raw_data)[colSums(is.na(raw_data)) > 0]

colsNA

rowsNA<-rownames(raw_data)[rowSums(is.na(raw_data)) > 0]

##It's not numeric value so it must be converted - as.numeric() is an option here
rowsNA

rowsNA<-which(rowSums(is.na(raw_data)) > 0)
rowsNA

data<-raw_data[-rowsNA,]
##Check if any row contains missing value
rownames(data)[rowSums(is.na(data)) > 0]

##Check if any column contains missing value
colnames(data)[colSums(is.na(data)) > 0]

##Check dimensions of newly created data
dim(data)

#Lets make it wrong 

data_bad<-read.csv("BBB_raw_data_tabSep.txt")

##Let's print our data - something is definitle not right
data_bad

##What are dimensions of our variable
dim(data_bad)

##What?? How to correct the error
data_correct<-read.csv("BBB_raw_data_tabSep.txt", sep="")
data_correct

##What else is important? Check if your dataset has variable/columns name included
data_bad2<-read.csv("BBB_raw_data_tabSep.txt", sep="", header = FALSE)
data_bad2

sapply(data_bad2, class)

##And now we see how such simple settings affect our work - it just not work...
sum(data_bad2$V1)

##Of course we expect to have one more row
dim(data_bad2)


##Load library
library(caret)

set.seed(1234) ##Please check what will happen if we will not set seed

##Let's get "random" row numbers for trainig data
trainIndex <- createDataPartition(data$logBBB, p = .8, list=TRUE)

trainIndex
##Having rows numbers we are capable to split dataset into training part and testing part
train_data<-data[trainIndex$Resample1,]
test_data<-data[-trainIndex$Resample1,]

dim(train_data)
dim(test_data)

##Why not to save out datasets into files for future use?
write.csv(train_data, file="BBB_data_no1.csv", row.names=FALSE)
write.csv(test_data, file="t-BBB_data_no1.csv", row.names=FALSE)

set.seed(1234) ##Again...

foldIndex <- createFolds(data$logBBB, k = 10, returnTrain = FALSE)

foldIndex

##Did we get what was expected? - check it.
sort(unlist(foldIndex))
##Seems to be right
##Let's split dataset into 
loop<-1
for(i in foldIndex){
  print(i)
  
  ##Create training subset, name for file and file itself
  train_data<-data[-i,]
  name<-paste("cv_BBB_data_no", loop, ".csv", sep="")
  write.csv(train_data, name, row.names=FALSE)
  
  ##Create test subset, name for file and file itself
  t_name<-paste("t-cv_BBB_data_no", loop, ".csv", sep="")
  test_data<-data[i,]
  write.csv(test_data, t_name, row.names=FALSE)
  
  ##Increase loop variable by 1 to not overwrite just created files.
  loop<-loop+1    
}

##It's also good to know what is inside files so save row numbers for every fold
track_record<-as.matrix(foldIndex, row.names=names(foldIndex))
write.table(track_record, file="track_record.txt", row.names=TRUE)    

##That's all... in case of data splittig
track_record

##Standardization
##Let's take 2 variables with distribution different from normal as an example
summary(data[,c("wpsa1","wpsa2")])

##We use function preProcess implemented in caret package. 
pp_settings <- preProcess(data[,c("wpsa1","wpsa2")], method=c("center", "scale"))

##Use specified settings to transform given data
standardized_data <- predict(pp_settings, data[,c("wpsa1","wpsa2")])
summary(standardized_data)

hist(standardized_data$wpsa2)

##Distribution did not change!!!
hist(data$wpsa2)

##Again prepare object with scaling settings
pp_settings2 <- preProcess(data[,c("wpsa1","wpsa2")], method=c("range"))

scaled_data <- predict(pp_settings2, data[,c("wpsa1","wpsa2")])

summary(scaled_data)

##Again distribution did not change
hist(scaled_data$wpsa2)

##Another transformation used quite frequently is logarithm
log_data = log(data[,c("wpsa1","wpsa2")])

summary(log_data)

##This time, variable distribution changed!!!
hist(log_data$wpsa2)

##Do we have a normal distribution?
shapiro.test(log_data$wpsa2)

##Did we have a normal distribution before?
shapiro.test(data$wpsa2)


data[1]
poly_x <- poly(data[[1]], degree = 3)
poly_x
plot(y=poly_x[,1], x=data[[1]])
plot(y=poly_x[,2], x=data[[1]])
plot(y=poly_x[,3], x=data[[1]])


##Add noise to data
noised_data<-data
for(l in 1:5){
  for(i in 1:dim(data)[1]){
    r=rnorm(dim(data)[2], mean = 1, sd = 0.01)
    new_row<-data[i,]*r
    noised_data<-rbind(noised_data, new_row)
  }
}
