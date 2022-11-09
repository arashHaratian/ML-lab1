library(ggplot2)
library(dplyr)

#=====Import Data=====
diabetes_data <- read.csv("D:/下載/pima-indians-diabetes.csv")
colnames(diabetes_data) <- c("number_of_times_pregnant",
                             "plasma_glucose_concentration", 
                             "blood_pressure",
                             "triceps_skinfold_thickness",
                             "serum_insulin",
                             "bmi",
                             "diabetes_pedigree_function",
                             "age",
                             "diabetes")
#=====EX.1=====
plot_data <- diabetes_data
plot_data$diabetes <- as.factor(plot_data$diabetes)
ggplot(plot_data, aes(x=age, y=plasma_glucose_concentration,color=diabetes)) +
    geom_point() + scale_color_manual(values=c("#000000", "#ff0000")) 

#=====EX.2=====
n=dim(diabetes_data)[1]
set.seed(12345) 
id <- sample(1:n, floor(n*0.8)) 
train_data <-  diabetes_data[id,] 
test_data <- diabetes_data[-id,] 

model <- glm( diabetes ~plasma_glucose_concentration + age , data = train_data, family = binomial)

summary(model)$coef
#p = exp(-5.77295229 +  0.03539974*plasma_glucose_concentration +0.02112258*age)/ [1 + exp(-5.77295229 +0.03539974*plasma_glucose_concentration +0.02112258*age)]
probabilities <- model %>% predict(test_data, type = "response")#The type="response" option tells R to output probabilities of the form P(Y = 1|X), as opposed to other information such as the logit.
predicted_classes <- ifelse(probabilities > 0.5, "1", "0")
predicted_classes

missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}
missclass(as.factor(as.vector(test_data$diabetes)),as.numeric(predicted_classes))
1-mean(predicted_classes == test_data$diabetes)
