#reading in the divorce data
divorce <- read.csv("C:/Users/icptadmin/Documents/Datathon/Divorce Project/divorce (extract.me)/divorce.csv")

#getting training and testing datasets
train_ind <- sample(seq_len(nrow(divorce)), size = 136)
train <- divorce[train_ind, ]
test <- divorce[-train_ind, ]
#creating a neural network
nn=neuralnet(Class~Atr1+Atr2+Atr3+Atr4+Atr5+Atr6+Atr7+Atr8+Atr9+Atr10+Atr11+Atr12+Atr13+Atr14+Atr15+Atr16+Atr17+Atr18+Atr19+Atr20+Atr21+Atr22+Atr23+Atr24+Atr25+Atr26+Atr27+Atr28+Atr29+Atr30+Atr31+Atr32+Atr33+Atr34+Atr35+Atr36+Atr37+Atr38+Atr39+Atr40+Atr41+Atr42+Atr43+Atr44+Atr45+Atr46+Atr47+Atr48+Atr49+Atr50+Atr51+Atr52+Atr53+Atr54,data=train, hidden = c(20, 10),act.fct = "logistic", linear.output = FALSE)
#testing neural network
temp_test <- subset(test, select = names(divorce)[-55])
#gathering results
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = test$Class, prediction = nn.results$net.result)
roundedresults <- sapply(results, round, digits = 0)
results$actual <- as.numeric(results$actual)
roundedresultsdf <- data.frame(roundedresults)
attach(roundedresultsdf)
table(actual, prediction.2)

#reading in categorized question data
category <- read.csv("C:/Users/icptadmin/Documents/GitHub/rice-datathon/output.csv")
#creating the multiple regression model
reg_model <- glm(Divorce.Status ~ Communications + Experiences + Values + Empathy + Respect + Acquiesce + Self.righteous, data = category, family = "binomial")
#viewing the details of the model
summary(reg_model)