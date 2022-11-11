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
diabetes_data <- diabetes_data
diabetes_data$diabetes <- as.factor(diabetes_data$diabetes)
ggplot(diabetes_data, aes(x=age, y=plasma_glucose_concentration,color=diabetes)) +
    geom_point() + scale_color_manual(values=c("#000000", "#ff0000")) 

#=====EX.2=====


model <- glm( diabetes ~plasma_glucose_concentration + age , data = diabetes_data, family = binomial)

summary(model)$coef
#p = exp(-5.89785793 +  0.03558250*plasma_glucose_concentration +0.02450157*age)/ [1 + exp(-5.89785793 +0.03558250*plasma_glucose_concentration +0.02450157*age)]
diabetes_data$probabilities <- model %>% predict(diabetes_data, type = "response")#The type="response" option tells R to output probabilities of the form P(Y = 1|X), as opposed to other information such as the logit.
diabetes_data$predicted_classes_0.5 <- as.factor(ifelse(diabetes_data$probabilities > 0.5, "1", "0"))

ggplot(diabetes_data) +
  geom_point(aes(x=age, y=plasma_glucose_concentration,color=predicted_classes_0.5)) +
  scale_color_manual(values=c("#000000", "#ff0000"))+
  labs(title="Assigment 3 Question 2, r=0.5")

missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}
misscalssification <- missclass(diabetes_data$diabetes,diabetes_data$predicted_classes_0.5)
1-mean(diabetes_data$predicted_classes_0.5 == diabetes_data$diabetes)
cat("The misscalssification is",misscalssification)


#====EX.3=====
diabetes_data <- diabetes_data
slope <- coef(model)[3]/(-coef(model)[2])
intercept <- coef(model)[1]/(-coef(model)[2])
ggplot(diabetes_data) +
  geom_point(aes(x=age, y=plasma_glucose_concentration,color=predicted_classes_0.5)) +
  geom_abline(aes(x=intercept, y=slope))+
  scale_color_manual(values=c("#000000", "#ff0000"))+
  labs(title="Assigment 3 Question 3, r=0.5, with decision boundary")

# slope <- coef(model)[3]/(-coef(model)[2])
# intercept <- coef(model)[1]/(-coef(model)[2])
# # plot(1, xlim = c(20, 80),ylim = c(0,200));abline(slope, coef(model)[1])
# plot(diabetes_data$age, diabetes_data$plasma_glucose_concentration, col = diabetes_data$predicted_classes_0.5)
# # abline(coef = coef(model))
# abline(intercept, slope)


#====EX.4=====
  #===== r=0.2 =====
  diabetes_data$predicted_classes_0.2 <- as.factor(ifelse(diabetes_data$probabilities > 0.2, "1", "0"))
 
 ggplot(diabetes_data) +
    geom_point(aes(x=age, y=plasma_glucose_concentration,color=predicted_classes_0.2)) +
    scale_color_manual(values=c("#000000", "#ff0000"))+
    geom_smooth(aes(x=age, y=plasma_glucose_concentration))+
    labs(title="Assigment 3 Question 4, r=0.2, with decision boundary")
  
  #===== r=0.8 =====
  diabetes_data$predicted_classes_0.8 <- as.factor(ifelse(diabetes_data$probabilities > 0.8, "1", "0"))
  
 ggplot(diabetes_data) +
    geom_point(aes(x=age, y=plasma_glucose_concentration,color=predicted_classes_0.8)) +
    scale_color_manual(values=c("#000000", "#ff0000"))+
    geom_smooth(aes(x=age, y=plasma_glucose_concentration))+
    labs(title="Assigment 3 Question 4, r=0.8, with decision boundary")

 
#=====EX.5=====
 diabetes_data_ex5<- diabetes_data
 diabetes_data_ex5$z1 <- (diabetes_data$plasma_glucose_concentration)^4
 diabetes_data_ex5$z2 <- (diabetes_data$plasma_glucose_concentration)^3 * diabetes_data$age
 diabetes_data_ex5$z3 <- (diabetes_data$plasma_glucose_concentration)^2 * (diabetes_data$age)^2
 diabetes_data_ex5$z4 <- (diabetes_data$plasma_glucose_concentration)^1 * (diabetes_data$age)^3
 diabetes_data_ex5$z5 <- (diabetes_data$age)^4
 model_2 <- glm( diabetes ~plasma_glucose_concentration + age + z1 + z2 + z3 + z4
                +z5 , data = diabetes_data_ex5, family = binomial)
 diabetes_data_ex5$probabilities <- model_2 %>% predict(diabetes_data_ex5, type = "response")#The type="response" option tells R to output probabilities of the form P(Y = 1|X), as opposed to other information such as the logit.
 diabetes_data_ex5$predicted_classes_0.5 <- as.factor(ifelse(diabetes_data_ex5$probabilities > 0.5, "1", "0"))
 
 ggplot(diabetes_data_ex5) +
   geom_point(aes(x=age, y=plasma_glucose_concentration,color=predicted_classes_0.5)) +
   scale_color_manual(values=c("#000000", "#ff0000"))+
   labs(title="Assigment 5, r=0.5")
 
 
 
#==== Fixing decision boundary=====
  ggplot(diabetes_data) +
    geom_point(aes(x=age, y=plasma_glucose_concentration,color=predicted_classes_0.5)) +
    scale_color_manual(values=c("#000000", "#ff0000"))+
    geom_smooth(aes(x=age, y=plasma_glucose_concentration))+
    labs(title="Assigment 3 Question 3, r=0.5, with decision boundary")

