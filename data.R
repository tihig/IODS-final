#Data wrangling here
library(dplyr); library(ggplot2)

# Hypothesis: people who on average get high points in exam and attitude towards statistics score high in interest
learn <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)
str(learn)


#making variable Interest into the data set
learn <- mutate(learn, Interest = De + Dc)
learn$Interest

#Creating the varaibles deep, stra and surf like in exercises
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D07","D14","D22","D30")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

deep_columns <- select(learn, one_of(deep_questions))
learn$deep <- rowMeans(deep_columns)

surface_columns <- select(learn, one_of(surface_questions))
learn$surf <- rowMeans(surface_columns)

strategic_columns <- select(learn, one_of(strategic_questions))
learn$stra <- rowMeans(strategic_columns)



#Creating new variable att_points= average of attitude and points
learn <- mutate(learn, att_points = (Attitude + Points) / 2)

# Calculate mean 25.9071
mean(learn$att_points)

#Creating new variable high_points that is true if att_points is greater than 26 (above mean)
learn <- mutate(learn, high_points = att_points > 26)
learn$high_points

#Choosing the necessary columns to keep
keep_columns <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points", "Interest", "high_points")
learn <- select(learn, one_of(keep_columns))

#Renaming the columns
new_names <- c("gender","age","attitude", "deep", "stra", "surf", "points", "interest", "high_points")
colnames(learn)<-new_names

summary(learn)
glimpse(learn)

#Load the data frame into .csv- file
write.csv(learn, "learn.csv", row.names = FALSE)

install.packages("prettydoc")
