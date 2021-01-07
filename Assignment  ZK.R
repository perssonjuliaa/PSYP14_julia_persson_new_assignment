# ----- seminar function for coef_table ----

coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

# Assignment 1

# ----- loading packages ----

library(tidyverse)
library(lm.beta)
library(lmtest)
library(psych)
library(dplyr)
library(sandwich)
library(car)

# ----- section 1 ----
  
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
View(data_sample_1)
print(data_sample_1)

data_sample_1 %>% 
  summary()

describe(data_sample_1)

data_sample_1 %>% 
ggplot()+
  aes(x = age, fill = sex)+
  geom_histogram()

data_sample_1 %>% 
  ggplot()+
  aes(x = STAI_trait, fill = sex)+
  geom_histogram()




data_sample_1 <- data_sample_1[-c(93), ] # excluding participant

data_sample_1 %>% 
  summary(data_sample_1)
describe(data_sample_1)


data_sample_1 %>% 
  ggplot()+
  aes(y = pain, x = age)+
  geom_histogram(aes(color = sex), size = 4)+
  geom_smooth()

data_sample_1 <- data_sample_1 %>% 
  mutate(sex = factor(sex))

# ----- section 2 ----

model1=lm(pain ~ age + sex, data = data_sample_1)
model1 %>% 
  summary(model1)

# ----- section 3 ----

model2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = data_sample_1)
model2 %>% 
  summary(model2)

# ----- section 4 ----

model2 %>% plot(which = 5)
model2 %>% plot(which = 4)

data_sample_1 %>% slice(c(68, 99, 112))

# ----- section 5 ----

model2 %>% residualPlots()

# ----- section 6 ----

model2 %>% plot(which = 2)
residuals_model2 = enframe(residuals(model2))
residuals_model2 %>% 
  ggplot() + aes (x = value) +
  geom_histogram

describe(residuals(model2))

# ----- section 7 ----

model2 %>% vif()
select(pain, cortisol_serum, cortisol_saliva) %>% 
  pairs.panels(col = "red", lm = T)

# ----- section 8 ----

plot(which = 3)
ncvTest(model2)
bptest(model2)

# ----- section 9 ----

mod2_final = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)
mod2_final %>% 
  summary()

# ----- section 10 ----

mod2_final %>% plot(which = 2)

residuals_mod2_final = enframe(residuals(mod_final))
residuals_mod2_final %>% 
  ggplot() + aes(x = value) +
  geom_histogram()

describe(residuals(mod2_final))

mod2_final %>% residualPlots()

mod2_final %>% plot(which = 3)
mod2_final %>% ncvTest()
mod2_final %>% bptest()

mod2_final %>% vif()

# ----- section 11 ----

AIC(model1)
coef_table(model1)

AIC(mod2_final)
coef_table(mod2_final)

anova(model1, mod2_final)



# Assignment 2 

# ----- loading packages ----

library(tidyverse)
library(lm.beta)
library(lmtest)
library(Psych)
library(sandwich)
library(boot)
library(lmboot)
library(car)
library(dplyr)

# ----- section 1 ----

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
View(data_sample_1)
data_sample_1 <- data_sample_1[-c(93), ] # excluding participant

describe(data_sample_1)
summary(data_sample_1)

data_sample_1 %>% 
  ggplot() +
  aes(x = wieght, y = pain, color = sex) + 
  geom_point()

data_sample_1 %>% 
  ggplot() + 
  aes(x = IQ) + 
  geom_histogram()

data_sample_1 %>% 
  ggplot() + 
  aes(x = household_income, y = pain) + 
  geom_point()

data_sample_1 %>% 
  ggplot() + 
  aes(y = pain, x = IQ) + 
  geom_point(aes(color = sex), size = 3) + 
  geom_smooth()

data_sample_1 %>% 
  ggplot() + 
  aes(y = pain, x= weight) + 
  geom_point(aes(color = sex), size = 3) +
  geom_smooth()

data_sample_1 %>% 
  ggplot() + 
  aes(y = pain, x = household_income) +
  geom_point(aes(color = sex), size = 3) +
  geom_smooth()

# ----- section 2 ----
# the other researchers model

model3 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_1)
summary(model3)
model3 %>% 
  summary()

AIC(model3)

model3 %>% plot(which = 5)
model3 %>% plot(which = 4)
data_sample_1 %>% slice(c(3, 102, 112))

model3 %>% plot(which = 2)

residuals_model3 = enframe(residuals(model3))
residuals_model3 %>% 
  ggplot() + 
  aes(x = value) +
  geom_histogram()

describe(residuals_model3)

model3 %>% residualsPlots()

model3 %>% plot(which = 3)
model3 %>% ncvTest()
model3 %>% bptest()

# ----- section 3 ----
# backward regression

model4 = step(model3, direction = "backward")
summary(model4)
model4 %>%
  summary()

backward_model = lm(pain ~ sex + age + pain_cat + mindfulness + cortisol_serum + household_income, data_sample_1)
summary(backward_model)
backward_model %>% 
  summary()

AIC(backward_model)
coef_table(backward_model, data_sample_1)

theory_based_model = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)
theory_based_model %>% 
  summary()

AIC(theory_based_model)

anova(backward_model, theory_based_model)

# ----- section 3 ----

data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")
View(data_sample_2)
summary(data_sample_2)

data_sample_2 <- data_sample_2 %>% 
  mutate(sex = factor(sex))

backward_model = lm(pain ~ sex + age + pain_cat + mindfulness + cortisol_serum + household_income, data_sample_2)
summary(backward_model)

theory_based_model = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_2)
# ----- section 4 ----

predict_backward_model <- predict(backward_model, data_sample_2)

predict_theory_based_model <- predict(theory_based_model, data_sample_2)

RSS_backward_model = sum((data_sample_2[,"pain"]-predict_backward_model)^2)

RSS_theory_based_model = sum((data_sample_2[,"pain"]-predict_theory_based_model)^2)

# Assignment 3 

# ----- loading packages ----

library(psych)
library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn)

# ----- section 1 ----

data_sample_3 = read.csv("https://tinyurl.com/ha-dataset3")
View(data_sample_3)
data_sample_3 <- data_sample_3[-c(77, 182),]

data_sample_3 <- data_sample_3 %>% 
  mutate(sex = factor(sex))

describe(data_sample_3)

summary(data_sample_3)

data_sample_3 %>% 
  ggplot()+ 
  aes(y = pain, x = age) +
  geom_point(aes(color = hospital), size = 4) +
  geom_smooth(method = "lm", se = F)
  

data_sample_3 %>% 
  ggplot() +
  aes(y = pain, x = cortisol_serum, color = hospital) +
  geom_point(size = 4) + 
  geom_smooth(method = "lm", se = F, fulltange = TRUE)

# ----- section 2 ----


model_5 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness  + cortisol_serum + (1 | hospital), data = data_sample_3)
summary(model_5)

AIC(model_5)

confint(model_5)

r.squaredGLMM(model_5)

coef_table(model_5)

theory_based_model = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)
summary(theory_based_model)

confint(model_5)
confint(theory_based_model)

r.squaredGLMM(pain_model_5)


# ----- section 3 ----

data_sample_4 = read.csv("https://tinyurl.com/ha-dataset4")
View(data_sample_4)
summary(data_sample_4)

data_sample_4 <- data_sample_4[-c(5, 80, 87),]
mutate(sex = factor(sex),
       hospital = factor(hospital))

# ----- section 4 ----

model_6 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = data_sample_4)
summary(model_6)

RSS_model_6 = sum((data_sample_4$pain - predict(model_6))^2)

model_6_mean <- lm(pain ~ 1, data = data_sample_4)

TSS_model_6 = sum((data_sample_4$pain - predict(model_6_mean))^2)

R2 = 1-(RSS_model_6/TSS_model_6)
R2


# ----- section 5 ----

model_7 = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_sample_3)
summary(model_7)

AIC(model_7)

pred_slope = predict(model_7)

plot_model_7 = data_sample_3 %>% 
  ggplot() +
  aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) +
  geom_line(color='red', aes(y=pred_slope, x=cortisol_serum)) + 
  labs(x= "rep_post_pain", y = "cortisol_serum") +
  facet_wrap( ~ hospital, ncol = 3)
plot_model_7


  


