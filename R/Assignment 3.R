library(ggplot2)

#=====Import Data=====
diabetes_data <- read.csv("D:/下載/pima-indians-diabetes.csv", header = FALSE)
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
diabetes_data_1 <- diabetes_data
diabetes_data_1$diabetes <- as.factor(ifelse(diabetes_data_1$diabetes == 1, "Yes", "No"))

plot_assigment3_q1 <- ggplot(diabetes_data_1, aes(x=age, y=plasma_glucose_concentration,color=diabetes)) +
    geom_point() + 
    scale_color_manual(values=c("#000000", "#ff0000")) +
    labs(title="Assigment 3 Question 1, the original data",colour = "Diabetes")
plot_assigment3_q
#=====EX.2=====


model_1 <- glm( diabetes ~plasma_glucose_concentration + age , data = diabetes_data_1, family = binomial)

summary(model_1)$coef
#p = exp(-5.89785793 +  0.03558250*plasma_glucose_concentration +0.02450157*age)/ [1 + exp(-5.89785793 +0.03558250*plasma_glucose_concentration +0.02450157*age)]
diabetes_data_1$probabilities <- predict(model_1,diabetes_data_1, type = "response")#The type="response" option tells R to output probabilities of the form P(Y = 1|X), as opposed to other information such as the logit.
diabetes_data_1$predicted_classes_0.5 <- as.factor(ifelse(diabetes_data_1$probabilities > 0.5, "Yes", "No"))

plot_assigment3_q2<- ggplot(diabetes_data_1) +
  geom_point(aes(x=age, y=plasma_glucose_concentration,color=predicted_classes_0.5)) +
  scale_color_manual(values=c("#000000", "#ff0000"))+ 
  labs(title="Assigment 3 Question 2, r=0.5",colour = "Diabetes")
plot_assigment3_q2


missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}
misscalssification <- missclass(diabetes_data_1$diabetes,diabetes_data_1$predicted_classes_0.5)
misscalssification


#====EX.3=====
inverse_logit <- function(threshold){ #To correct the intercept on the plot
  return(-log((1-threshold)/threshold))
  
}

decision_boundary <- function(a, b, c, ...){ #function to plot decision boundary
  slope <- -a / b
  intercept <- -c / b
  geom_abline(slope = slope,
              intercept = intercept, ...)
  
}

plot_assigment3_q3 <- ggplot(diabetes_data_1) +
  geom_point(aes(x=age, y=plasma_glucose_concentration,color=predicted_classes_0.5)) +
  scale_color_manual(values=c("#000000", "#ff0000"))+
  labs(title="Assigment 3 Question 3, r=0.5, with decision boundary",colour = "Diabetes")+
  decision_boundary(model_1$coefficients[3],model_1$coefficients[2], 
                    model_1$coefficients[1]-inverse_logit(0.5)) 
  
plot_assigment3_q3


#====EX.4=====
  #===== r=0.2 =====
  diabetes_data_1$predicted_classes_0.2 <- as.factor(ifelse(diabetes_data_1$probabilities > 0.2, "Yes", "No"))
 
plot_assigment3_q4_r0.2<- ggplot(diabetes_data_1) +
    geom_point(aes(x=age, y=plasma_glucose_concentration,color=predicted_classes_0.2)) +
    scale_color_manual(values=c("#000000", "#ff0000"))+
    labs(title="Assigment 3 Question 4, r=0.2, with decision boundary",colour = "Diabetes")+
  decision_boundary(model_1$coefficients[3],model_1$coefficients[2],
                    model_1$coefficients[1]-inverse_logit(0.2)) 
  
plot_assigment3_q4_r0.2

  #===== r=0.8 =====
  diabetes_data_1$predicted_classes_0.8 <- as.factor(ifelse(diabetes_data_1$probabilities > 0.8, "Yes", "No"))
  
plot_assigment3_q4_r0.8 <- ggplot(diabetes_data_1) +
    geom_point(aes(x=age, y=plasma_glucose_concentration,color=predicted_classes_0.8)) +
    scale_color_manual(values=c("#000000", "#ff0000"))+
    labs(title="Assigment 3 Question 4, r=0.8, with decision boundary",colour = "Diabetes")+
    decision_boundary(model_1$coefficients[3],model_1$coefficients[2],
                       model_1$coefficients[1]-inverse_logit(0.8))
plot_assigment3_q4_r0.8
 
#=====EX.5=====
 diabetes_data_ex5<- diabetes_data
 diabetes_data_ex5$z1 <- (diabetes_data_ex5$plasma_glucose_concentration)^4
 diabetes_data_ex5$z2 <- (diabetes_data_ex5$plasma_glucose_concentration)^3 * diabetes_data_ex5$age
 diabetes_data_ex5$z3 <- (diabetes_data_ex5$plasma_glucose_concentration)^2 * (diabetes_data_ex5$age)^2
 diabetes_data_ex5$z4 <- (diabetes_data_ex5$plasma_glucose_concentration)^1 * (diabetes_data_ex5$age)^3
 diabetes_data_ex5$z5 <- (diabetes_data_ex5$age)^4
 model_2 <- glm( diabetes ~plasma_glucose_concentration + age + z1 + z2 + z3 + z4
                +z5 , data = diabetes_data_ex5, family = binomial)
 diabetes_data_ex5$probabilities <- predict(model_2,diabetes_data_ex5, type = "response")#The type="response" option tells R to output probabilities of the form P(Y = 1|X), as opposed to other information such as the logit.
 diabetes_data_ex5$predicted_classes_0.5 <- as.factor(ifelse(diabetes_data_ex5$probabilities > 0.5 , "Yes", "No"))

 plot_assigment3_q5 <- ggplot(diabetes_data_ex5,aes(x=age, y=plasma_glucose_concentration)) +
   geom_point(aes(x=age, y=plasma_glucose_concentration,color=predicted_classes_0.5)) +
   scale_color_manual(values=c("#000000", "#ff0000"))+
   labs(title="Assigment 5, r=0.5, r=0.8, with decision boundary",colour = "Diabetes")+
  
   decision_boundary(model_2$coefficients[3],
           model_2$coefficients[2],
           model_2$coefficients[1]-inverse_logit(0.5)) 
 plot_assigment3_q5

