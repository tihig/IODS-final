#Data wrangling here
library(dplyr); library(ggplot2)

# Hypothesis: people who on average get high points in exam and attitude towards statistics score high in interest
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)
str(lrn14)


#making variable Interest into the data set
lrn14 <- mutate(lrn14, Interest = De + Dc)
lrn14$Interest

#Creating the varaibles deep, stra and surf like in exercises
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D07","D14","D22","D30")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)

surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)

strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)

keep_columns <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points", "Interest")
learn <- select(lrn14, one_of(keep_columns))
summary(learn)
dim(learn)

#Creating new variable att_points= average of attitude and points
learn <- mutate(learn, att_points = (Attitude + Points) / 2)
plot(learn$att_points)
#Creating new variable high_points that is true if att_points is greater than 25
learn <- mutate(learn, high_points = att_points > 25)
learn$high_points

m <- glm(high_points ~ Interest, data = learn, family = "binomial")
summary(m)
coef(m)

plot(m)
