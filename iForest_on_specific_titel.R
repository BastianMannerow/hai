##### packages needed
#install.packages("readxl")
#install.packages("caret")
#install.packages("h2o")
library(caret)
library(h2o)
library(dplyr)

## import data
data <- read.csv("./Data/data_zusatz_hh_sh_ep14.csv", header = TRUE)
data$Gesamttitel <- as.character(data$Gesamttitel)

# filter data for the 3 specific titel
#d1 <- subset(data, Gesamttitel == "140281246")
#d1 <- subset(data, Gesamttitel == "140263102")
d1 <- subset(data, Gesamttitel == "140335601")

## iForest for first title
csv_file_data <- "d1.csv"
write.csv(d1[-(1)], file = csv_file_data, row.names = FALSE) # remove first row, no information

h2o.init()
# upload data
train <- h2o.uploadFile(path = csv_file_data)

# building IForest model
model <- h2o.isolationForest(training_frame = train, 
                                sample_size = 7,
                                ntrees = 100,
                                max_depth = 4,
                                categorical_encoding = "Enum")

# calculate scores
scores <- as.data.frame(h2o.predict(model, train))
# show distribution of anomaly scores
scores_ordered <- arrange(scores, desc(scores$predict))
par(mfrow = c(1,2))
hist(scores$predict, 
     main = "Histogram of anomaly scores", 
     xlab = "anomaly score", 
     ylab = "Frequency", 
     col = "blue", 
     border = "blue", 
     breaks = 5)
plot(1:dim(scores_ordered)[1], scores_ordered$predict, pch = 1, col = "black", xlab = "Samples", ylab = "anomaly score")
abline(h = 0.6, col = "orange")
par(mfrow = c(1,1))

# add score and anomaly indicator to dataframe
d1 <- cbind(d1, s = scores$predict)
d1 <- d1 %>% mutate(anomaly = ifelse(s >=0.6,"1","0"))
# write to csv
csv_file_result <- "iForest_titel_140335601_results.csv"
write.csv(d1, file = csv_file_result, row.names = FALSE)
