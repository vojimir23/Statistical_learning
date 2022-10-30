library("arules")
library(plyr)
library(dplyr)
library(scales) 
library(ggplot2)
library(readxl)
library(RColorBrewer)
library("arulesViz")
library(gridExtra)

# Imports of ratings and movie names
ratings <- read.csv("Netflix_Dataset_Rating.csv")
movies <- read.csv("Netflix_Dataset_Movie.csv")
all_data <- merge(ratings,movies,by="Movie_ID")
n_distinct(all_data$Name)
# Deleting badly rated movies
average <- all_data %>% group_by(Name,Movie_ID)  %>%  summarize(mean=mean(Rating)) 
data1 <- average %>% filter(mean>=3) 
data<-merge(data1,all_data,by='Movie_ID')
names(data)[names(data) == "Name.x"] <- "Name"
# Plot of number of each rating
ratings <- all_data %>%
  count(Rating, sort = FALSE) 
n_distinct(data$Name)
n_distinct(data$User_ID)

p<-ggplot(data=ratings, aes(x=Rating,y=n )) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() + scale_y_continuous(labels = comma_format(big.mark = ".",decimal.mark = ",")) + labs(x = "Ratings") + labs(y='Absolute values of rated movies')
p


# Converting rating-per-row dataframe into sparse User-Item matrix suitable for Apriori algorithm
user_item_matrix <- as(split(data[,"Name"], data[,"User_ID"]), "transactions")
user_item_matrix
summary(user_item_matrix)

library(rmarkdown)

# Presenting the most frequent movies in dataset ( in user/item matrix)
itemFrequencyPlot(user_item_matrix,topN=20,type="relative",col=brewer.pal(3,'Blues'), main="Relative Item Frequency Plot - Top 20",ylab="")



# APRIORI
association.rules <- apriori(user_item_matrix, parameter = list(supp=0.2, conf=0.8,target = "rules",maxlen=10))
inspect(sort(association.rules[1:10]))
inspect(head(sort(association.rules, by = "confidence",decreasing = TRUE), n = 10))
#inspect(association.rules) 
sorted <- sort(association.rules, by = 'confidence', decreasing = TRUE)
inspect(head(sorted))

RD_right.association.rules <- apriori(user_item_matrix, parameter = list(supp=0.2, conf=0.8),appearance = list(default="lhs",rhs="Lord of the Rings: The Fellowship of the Ring"))
inspect(head(sort(RD_right.association.rules, by = "lift",decreasing = TRUE), n = 10))
#RD_left.association.rules  <- apriori(user_item_matrix, parameter = list(supp=0.01, conf=0.5),appearance = list(default="rhs",lhs="Lord of the Rings: The Fellowship of the Ring")) # ovde zeza malo, smanjio conf
#inspect(head(sort(RD_left.association.rules, by = "lift"), n = 10))

#write(RD_right.association.rules, file="RD_right.association.rules.csv", sep=",", quote=TRUE,row.names=FALSE)
#inspectDT(association.rules[1:10])
# Filtering the rules according to confidence or lift and plotting

subRules<-association.rules[quality(association.rules)$lift>1]
top10subRules <- head(subRules, n = 10, by = "lift")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

# Filter top 10 rules with highest lift
#subRules2<-head(subRules, n=20, by="lift")
plot(top10subRules, method="paracoord")



# ------------------------------------------------------------------------------------------------

# Plotting relationship between number of rules and support Levels/confidence Levels

# Support and confidence values
supportLevels <- c(0.4,0.3,0.2,0.1)
confidenceLevels <- c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)


rules_sup10 <- integer(length=9)
rules_sup5 <- integer(length=9)
rules_sup1 <- integer(length=9)
rules_sup0.5 <- integer(length=9)

# Apriori algorithm with a support level of 40%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup10[i] <- length(apriori(user_item_matrix, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules",maxlen=10)))
  
}

# Apriori algorithm with a support level of 30%
for (i in 1:length(confidenceLevels)){
  
  rules_sup5[i] <- length(apriori(user_item_matrix, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules",maxlen=10)))
  
}

# Apriori algorithm with a support level of 20%
for (i in 1:length(confidenceLevels)){
  
  rules_sup1[i] <- length(apriori(user_item_matrix, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules",maxlen=10)))
  
}

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)){
  
  rules_sup0.5[i] <- length(apriori(user_item_matrix, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules",maxlen=10)))
  
}
#--------------------------------.

# Plotting 
# Number of rules found with a support level of 40%
plot1 <- qplot(confidenceLevels, rules_sup10, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules", 
               main="Apriori with a support level of 40%") + scale_y_continuous(labels = comma_format(big.mark = ".",decimal.mark = ","))+theme_bw()+scale_x_continuous(breaks=confidenceLevels)

# Number of rules found with a support level of 30%
plot2 <- qplot(confidenceLevels, rules_sup5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules", 
               main="Apriori with a support level of 30%") + 
  scale_y_continuous(labels = comma_format(big.mark = ".",decimal.mark = ","))+theme_bw()+scale_x_continuous(breaks=confidenceLevels)

# Number of rules found with a support level of 20%
plot3 <- qplot(confidenceLevels, rules_sup1, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules", 
               main="Apriori with a support level of 20%") + 
  scale_y_continuous(labels = comma_format(big.mark = ".",decimal.mark = ","))+theme_bw()+scale_x_continuous(breaks=confidenceLevels)

# Number of rules found with a support level of 10%
plot4 <- qplot(confidenceLevels, rules_sup0.5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules", 
               main="Apriori with a support level of 10%") + 
  scale_y_continuous(labels = comma_format(big.mark = ".",decimal.mark = ",")) +theme_bw()+scale_x_continuous(breaks=confidenceLevels)
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5, confidenceLevels)

# Number of rules found with a support level of 40%, 30%, 20% and 10%
ggplot(data=num_rules, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 40%)
  geom_line(aes(y=rules_sup10, colour="Support level of 40%")) + 
  geom_point(aes(y=rules_sup10, colour="Support level of 40%")) +
  
  # Plot line and points (support level of 30%)
  geom_line(aes(y=rules_sup5, colour="Support level of 30%")) +
  geom_point(aes(y=rules_sup5, colour="Support level of 30%")) +
  
  # Plot line and points (support level of 20%)
  geom_line(aes(y=rules_sup1, colour="Support level of 20%")) + 
  geom_point(aes(y=rules_sup1, colour="Support level of 20%")) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y=rules_sup0.5, colour="Support level of 10%")) +
  geom_point(aes(y=rules_sup0.5, colour="Support level of 10%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank()) + scale_y_continuous(labels = comma_format(big.mark = ".",decimal.mark = ","))

######------------------------------------------------------------------------------------
# Dendrogram and Jaccard distance
itemFrequency(user_item_matrix)
s <- user_item_matrix[, itemFrequency(user_item_matrix) > 0.5]
s
d_jaccard <- dissimilarity(s, which = "items")
d_jaccard
plot(hclust(d_jaccard, method = "ward.D2"), main = "Dendrogram for movies")





