######################################################################
# PROJECT: HOUSING PRICE PROJECT - MARIA LUCIA CORNEJO QUENAYA       #
######################################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gplots)) install.packages("gplots", repos = "http://cran.us.r-project.org")
if(!require(repr)) install.packages("repr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")


library(forecast)
library(tidyverse)
library(caret)
library(plyr)
library(data.table)
library(ggplot2)
library(recosystem)
library(lubridate)
library(readr)
library(gplots)
library(reshape2)
library(repr)
library(gridExtra)
library(rpart)
library(rpart.plot)
library(randomForest)



#Loading data
getwd()
train_data <- read.csv("train.csv")

#Checking missing data
missing_rows <- train_data[!complete.cases(train_data),]
head(missing_rows)
nrow(missing_rows)

#####################################################################################
#CREATING TRAIN AND TEST SETS                                                       # 
#####################################################################################
#Building subset of train dataset for prediction.
#Showing all variable names
variable_names <- names(train_data)
variable_names

#Selecting important variables by creating a vector that contains variable names
selected_variables <- c('Id','MSZoning','Utilities', 'Neighborhood','BldgType','HouseStyle',
                'OverallQual','OverallCond','YearBuilt', 'ExterQual','ExterCond',
                'BsmtQual','BsmtCond','TotalBsmtSF','Heating','HeatingQC', 
                'CentralAir','Electrical','GrLivArea','BedroomAbvGr','KitchenAbvGr',
                'KitchenQual','TotRmsAbvGrd','Functional','Fireplaces','FireplaceQu',
                'GarageArea','GarageQual','GarageCond','OpenPorchSF','PoolArea',
                'Fence','MoSold','YrSold','SaleType','SaleCondition','SalePrice')

#Building subset of train dataset that is used for prediction
selected_train <- train_data[,selected_variables]
head(selected_train)



#####################################################################################
#DATA ANALYSIS AND VISUALIZATIONS                                                   # 
#####################################################################################
summary(selected_train)

#Checking the target variable: SalePrice
summary(selected_train$SalePrice)

#Histogram of SalePrice's distribution
options(scipen=10000)
ggplot(selected_train, aes(x = SalePrice, fill = ..count..)) +
  geom_histogram(binwidth = 5000,fill="#69b3a2", color="#e9ecef") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "HISTOGRAM OF SALEPRICE", y = "COUNT OF HOUSES", x = "HOUSING PRICE")

#Log term of SalePrice
selected_train$lSalePrice <- log(selected_train$SalePrice)

#Histogram of log SalePrice distribution
ggplot(selected_train, aes(x = lSalePrice, fill = ..count..)) +
  geom_histogram(binwidth = 0.05,fill="#69b3a2", color="#e9ecef") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "HISTOGRAM OF log SALEPRICE", y = "COUNT OF HOUSES", x = "HOUSING PRICE")

#Counting house by MSZoning
options(repr.plot.width=5, repr.plot.height=5)
ggplot(selected_train, aes(x = MSZoning, fill = MSZoning )) + 
  geom_bar()+scale_fill_hue(c = 80)+
  theme(plot.title = element_text(hjust = 0.5),legend.position="right", legend.background = element_rect
  (size=0.5))+geom_text(stat='count',aes(label=..count..),vjust=-0.15)+
  labs(title = "DISTRIBUTION OF MSZONING",y="Count",x="MSZONING")

#Checking distribution of MSZoning
table(selected_train$MSZoning)

#Boxplot of SalePrice by MSZoning, adding average value of SalePrice as red point
ggplot(selected_train, aes(x=MSZoning, y=SalePrice, fill=MSZoning)) + 
  geom_boxplot(alpha=0.3) +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "BOXPLOT OF SALEPRICE BY MSZONING",y="SALEPRICE",x="MSZONING")

#Displaying the average house size per area
ddply(train_data, .(MSZoning), summarize,  size=mean(GrLivArea))
#Couningt houses in each catetory and also show maximum and minimum SalePrice
ddply(train_data, .(BldgType), summarize,Total = length(BldgType),Max_price=max(SalePrice),Min_price=min(SalePrice))

#Distribution of SalePrice by BldfType
ggplot(selected_train, aes(SalePrice)) +
  geom_histogram(aes(fill = BldgType), position = position_stack(reverse = TRUE), binwidth = 20000) +
  coord_flip() +
  theme(legend.position=c(0.9,0.8), 
  legend.background = element_rect(fill="grey", size=0.5))+
  labs(title = "HISTOGRAM OF SALEPRICE",y="COUNT",x="HOUSING PRICE")

#Distribution of SalePrice by OverallQual
ggplot(selected_train, aes(x = SalePrice,fill = as.factor(OverallQual))) +
  geom_histogram(position = "stack", binwidth = 10000) +
  scale_fill_discrete(name="OverallQual")+
  theme(legend.position=c(0.9,0.5), 
  legend.background = element_rect(fill="grey",size=0.5))+
  labs(title = "HISTOGRAM OF SALEPRICE",y="COUNT",x="HOUSING PRICE")

#Cconverting factor to numeric
selected_train$ExterCond2 <- as.numeric(factor(selected_train$ExterCond, 
                                             levels = c("Ex", "Fa","Gd", "TA","Po"),
                                             labels = c(5,2,4,3,1) ,ordered = TRUE))
selected_train$HeatingQC2 <- as.numeric(factor(selected_train$HeatingQC, 
                                             levels = c("Ex", "Fa","Gd", "TA","Po"),
                                             labels = c(5,2,4,3,1) ,ordered = TRUE))
selected_train$CentralAir2 <- as.numeric(factor(selected_train$CentralAir, 
                                              levels = c("N", "Y"),
                                              labels = c(0,1) ,ordered = TRUE))

#Selecting variables will be used for model buidling and heat map
model_variables <- c('SalePrice', 'OverallQual','OverallCond','YearBuilt','ExterCond2',
               'TotalBsmtSF','HeatingQC2', 'CentralAir2','GrLivArea','BedroomAbvGr','KitchenAbvGr',
               'TotRmsAbvGrd','Fireplaces','GarageArea','OpenPorchSF','PoolArea',
               'YrSold')
heat_map <- selected_train[,model_variables]


#Showing correlation heatmap for SalePrice variable
qplot(x=Var1, y=Var2, data=melt(cor(heat_map, use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(low = "green", high = "red", mid = "white", 
  midpoint = 0, limit = c(-1,1), space = "Lab", 
  name="CORRELATION") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))+
  coord_fixed()+
  theme(plot.title = element_text(hjust = 0.4))+
  labs(title = "CORRELATION HEATMAP",y="VAR2",x="VAR1")

#correlation between SalePrice and some numeric variables
plot1 <- ggplot(selected_train, aes(x=GrLivArea, y=SalePrice)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)+
  labs(title = "SCATTER PLOT OF SALEPRICE AND GRLIVAREA",y="SALEPRICE",x="GRLIVAREA")


# scatter plot of TotalBsmtSF
plot2 <- ggplot(selected_train, aes(x=TotalBsmtSF, y=SalePrice)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)+
  labs(title = "SCATTER PLOT OF SALEPRICE AND TOTALBSMTSF",y="SALEPRICE",x="TOTALBSMTSF")


#scatter plot of TotRmsAbvGrd
plot3 <- ggplot(selected_train, aes(x=TotRmsAbvGrd, y=SalePrice)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)+
  labs(title = "SCATTER PLOT OF SALEPRICE AND TOTRMSABVGRD",y="SALEPRICE",x="TOTRMSABVGRD")


#scatter plot of GarageArea
plot4 <- ggplot(selected_train, aes(x=GarageArea, y=SalePrice)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=FALSE)+
  labs(title = "SCATTER PLOT OF SALEPRICE AND GARAGEAREA",y="SALEPRICE",x="GARAGEAREA")
grid.arrange(plot1, plot2,plot3,plot4)

#####################################################################################
#PREDICTIVE MODELING                                                                # 
#####################################################################################

#####################
#MODEL 1            #
#####################

#Building model dataset for linear regression 
model_linear <- selected_train[, model_variables]
model_linear$lSalePrice <- log(model_linear$SalePrice)

#Partitioning  data
set.seed(10000)
train_data.index <- sample(c(1:dim(model_linear)[1]), dim(model_linear)[1]*0.8)
model_linear_train = model_linear[train_data.index,]
model_linear_valid <- model_linear[-train_data.index,]


#Using lm() to run linear regression of SalePrice on all variables in model dataset
linearegression <- lm(lSalePrice~.-SalePrice, data = model_linear_train)
summary(linearegression)

#Using predict() to make prediction on a new set
prediction1 <- predict(linearegression,model_linear_valid,type = "response")
res <- model_linear_valid$lSalePrice - prediction1
linreg_prediction <- data.frame("Predicted" = prediction1, "Actual" = model_linear_valid$lSalePrice, "Residual" = res)
accuracy(prediction1, model_linear_valid$lSalePrice)

#####################
#MODEL 2            #
#####################
#Using  classification tree
class.tree <- rpart(lSalePrice~.-SalePrice,
                    data = model_linear_train,control = rpart.control(cp = 0.01))
plotcp(class.tree)
printcp(class.tree)

rpart.plot(class.tree, 
           box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)


#####################
#MODEL 3            #
#####################
#Using Random Forest
rf_model <- randomForest(lSalePrice ~.-SalePrice, data = model_linear_train, 
                   importance =TRUE,ntree=500,nodesize=7, na.action=na.roughfix)
#Ploting Variable Importance
varImpPlot(rf_model, type=1)

#Computing rediction
rf.pred <- predict(rf_model, newdata=model_linear_valid )
accuracy(rf.pred, model_linear_valid$lSalePrice)

plot(rf.pred, model_linear_valid$lSalePrice, main = "PREDICTED vs. ACTUAL LOG SALEPRICE") 
abline(0,1)


