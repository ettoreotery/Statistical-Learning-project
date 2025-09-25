library(readxl)
library(ggplot2)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(rpart)
library(rpart.plot)
library(tree)

# Read the data
Houses_in_Milan <- read_excel("~/Desktop/Milano.xlsx")


# Data cleaning 

# Function to clear a column of non-numeric characters
clean_column <- function(col) {
# Removes everything that is not numeric (including symbols like "-", "+")
  cleaned_col <- gsub("[^0-9.]", "", col)
# Converts to numeric; if fails returns NA
  as.numeric(cleaned_col)
}

# Apply the cleanup function to each column, handling NA after cleanup
Houses_in_Milan2 <- Houses_in_Milan %>%
  mutate(across(everything(), clean_column)) %>%  # Clear all columns
  na.omit()  # Removes rows with NA resulting from conversion

# Convert to data frame and visualize data
Houses_in_Milan2 <- as.data.frame(Houses_in_Milan2)


# PCA 

# mean and std
medie <- colMeans(Houses_in_Milan2)
scarto <- apply(Houses_in_Milan2, 2, sd)

n <- nrow(Houses_in_Milan2)
p <- ncol(Houses_in_Milan2)
M <- colMeans(Houses_in_Milan2)
sigma <- apply(Houses_in_Milan2, 2, sd)
descriptive<-round(cbind(M, sigma),2)
descriptive

rho <- cor(Houses_in_Milan2)
round(rho,3)

eigen(rho)

autoval <- eigen(rho)$values
autovec <- eigen(rho)$vectors

pvarsp = autoval/p
pvarspcum = cumsum(pvarsp)
tab<-round(cbind(autoval,pvarsp*100,pvarspcum*100),3)
colnames(tab)<-c("eigenvelues", "% variance","% cum variance")
tab

plot(autoval, type="b", main="Scree Diagram", xlab="Number of Component", ylab="Eigenvalues")
abline(h=1, lwd=3, col="red")

eigen(rho)$vectors[,1:2]

comp<-round(cbind(-eigen(rho)$vectors[,1]*sqrt(autoval[1]),-eigen(rho)$vectors[,2]*sqrt(autoval[2])),3)
rownames(comp)<-row.names(descriptive)
colnames(comp)<-c("Comp1","Comp2")
comp

communality<-comp[,1]^2+comp[,2]^2
comp<-cbind(comp,communality)
comp

Houses_in_Milan2.scale <- scale(Houses_in_Milan2, T, T)
score <- Houses_in_Milan2.scale%*%autovec[,1:2]

scorez <- round(cbind(-score[,1]/sqrt(autoval[1]), -score[,2]/sqrt(autoval[2])), 2)
plot(scorez, main="Scores plot", xlab="comp1", ylab="comp2")
points(scorez, pch=19, col="blue") # Add points to the graph
abline(v=0, h=0, col="red")

plot(comp[,1:2], type="n", main="Loadings plot", xlab="comp1", ylab="comp2", xlim=range(-1,1))
text(comp, labels=rownames(comp)) # Adds only variable names
abline(v=0, h=0, col="red")

# PCA using pricomp function

acp<-princomp(Houses_in_Milan2, cor=T)
summary(princomp(Houses_in_Milan2, cor=T))

screeplot(princomp(Houses_in_Milan2, cor=T))

plot(princomp(Houses_in_Milan2, cor=T)$scores, col = "grey", pch = 16)
abline(h=0, v=0)

fviz_pca_biplot(acp, label = "var",
                col.ind = "black",) 


# decision tree using PCA results

# Select numeric columns for PCA (except target variable)
numeric_cols <- Houses_in_Milan2[, c("rooms", "m2", "bathrooms", "condominium_expenses")]

# Run PCA on standardized numerical data
pca_result <- PCA(scale(numeric_cols), graph = FALSE)

# View a summary of the PCA
summary(pca_result)

# Select the main components to include
num_components <- 2  # Adjust the number of main components
pca_data <- data.frame(pca_result$ind$coord[, 1:num_components])

# Rename the main component columns
colnames(pca_data) <- paste0("PC", 1:num_components)

# Add the target variable ('price')
pca_data$price <- Houses_in_Milan2$price

# Convert 'price' to a binary variable 'price_category' based on the threshold of 700000
pca_data$price_category <- ifelse(pca_data$price <= 700000, "No", "Yes")

# Split the data into training and test sets
set.seed(101)
train_index <- sample(1:nrow(pca_data), 1200)  # Use 1200 lines for training
train_data <- pca_data[train_index, ]
test_data <- pca_data[-train_index, ]

# Build the classification decision tree
classification_tree <- rpart(price_category ~ PC1 + PC2, data = train_data, method = "class")

# Decision Tree Plot
rpart.plot(classification_tree)

# Test set predictions
tree_pred <- predict(classification_tree, test_data, type = "class")

# Comparison between forecasts and actual values
confusion_matrix <- table(tree_pred, test_data$price_category)

# View the confusion matrix
print(confusion_matrix)

# I compute the success rate
success_rate <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(success_rate)


# Decision trees

# Creating the High variable based on the Price condition <= 700000
Houses_in_Milan2$price <- ifelse(Houses_in_Milan2$price <= 700000, "No", "Yes")

# Transforming the variable High into a factor
Houses_in_Milan2$price = as.factor(Houses_in_Milan2$price)

# Creating the decision tree
tree_Houses_in_Milan2 = tree(price ~ . - price, data = Houses_in_Milan2)

# Decision Tree Summary
summary(tree_Houses_in_Milan2)

# Adding labels to decision tree leaves
text(tree_Houses_in_Milan2, pretty = 0)

tree_Houses_in_Milan2

# training
  
set.seed(101)
train=sample(1:nrow(Houses_in_Milan2),1200)
tree_Houses_in_Milan2=tree(price~.-price,Houses_in_Milan2,subset=train)
plot(tree_Houses_in_Milan2);text(tree_Houses_in_Milan2,pretty=0)

# confusion matrix

tree.pred=predict(tree_Houses_in_Milan2,Houses_in_Milan2[-train,],type="class")
with(Houses_in_Milan2[-train,],table(tree.pred,price))

# I compute the success rate

confusion_matrix <- table(tree.pred, Houses_in_Milan2[-train,]$price)
success_rate <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(success_rate)
###############


# pruning

# This tree was grown to full depth, and might be too variable. We now use CV to prune it.

set.seed(101)
cv.Houses_in_Milan2=cv.tree(tree_Houses_in_Milan2,FUN=prune.misclass)
cv.Houses_in_Milan2

plot(cv.Houses_in_Milan2)

# 3 final node give me the best accuracy with the smallest number of leafs
prune.Houses_in_Milan2=prune.misclass(tree_Houses_in_Milan2,best=3)
plot(prune.Houses_in_Milan2);text(prune.Houses_in_Milan2,pretty=0)

# confusion matrix

tree.pred=predict(prune.Houses_in_Milan2,Houses_in_Milan2[-train,],type="class")
with(Houses_in_Milan2[-train,],table(tree.pred,price))

# I compute the success rate

confusion_matrix <- table(tree.pred, Houses_in_Milan2[-train,]$price)
success_rate <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(success_rate)

################

