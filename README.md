# Variable-Selection-Regression-R
Practice selecting variables for use in regression analysis in R studio

---
title: "HW01 - Statistical Modeling"
author: "Kyle Barisone"
output:
  pdf_document: default
  word_document: default
  html_document:
    df_print: paged
---

```{r, warning=FALSE, message=FALSE}
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
# load appropriate libraries and import data here

# For this assignment, to get you started, I have included code to directly import 
# data sets from my website. 
# After you create your own data management files, this approach should not be used. 

EQ <- read.delim("https://norcalbiostat.netlify.com/data/Earthq.txt", 
                  sep="", header=TRUE)
View(EQ)
hiv <- read.delim("https://norcalbiostat.netlify.com/data/PARHIV_081217.txt", 
                  sep="\t", stringsAsFactors = FALSE, header=TRUE)
depress <- read.delim("https://norcalbiostat.netlify.com/data/depress_081217.txt", 
                  sep="\t", header=TRUE)
View(hiv)

library(ggplot2)  
library(dplyr)
library(leaps)
library(pander)

```

# Part I: Statistical Modeling

## 1. Fit a linear regression model

```{r}

mv_model <- lm(cesd ~ income + age, data=depress)
summary(mv_model)
```


### a. Analyze the residuals

```{r}
mv_model <- lm(cesd ~ income + age, data=depress)
summary(mv_model)

model.resid <- resid(mv_model)
plot(depress$cesd, model.resid, ylab="Residuals", xlab="CESD", 
         main="CESD residual plot") 

qqnorm(mv_model$residuals)
qqline(mv_model$residuals, col='red')
```
The qqplot indicates that the data is fairly normal since slight deviations in the tails are expected. However since it seems that it curves slightly more as the x-axis increases it could indicate data that is slightly right skewed. 
### b. Interpret each coefficient
```{r}
confint(mv_model)
```
B_0: someone who makes no money per year, and is 0 years old is expected to have a cesd score of 15.59. 95% CI: (12.36, 18.71) with p-value < 0.001.

B_1: After controlling for age, someone who earns $1000 per year greater is expected have 0.11 lower cesd. 95% CI: (-0.18, -0.05) p-value < 0.0001.

B_2: After controlling for income, for every year older someone gets, they are expected to have a cesd score that is 0.10 lower. 95% CI: (-0.15, -0.04) p-value < 0.0001. 
## 2. Test gender as a moderator using a) using a stratified model

```{r}
depress$SEX1 <- ifelse(depress$sex=="0", "Male", "Female")

mv_model <- depress %>% select(cesd, age, income, SEX1)
male <- mv_model %>% select(cesd, age, income, SEX1) %>% filter(SEX1 == "Male")
female <- mv_model %>% select(cesd, age, income, SEX1) %>% filter(SEX1 == "Female")
model_male <- lm(cesd ~ age + income, data = male)
model_female <- lm(cesd ~ age + income, data = female)

summary(model_male)
summary(model_female)

ggplot(depress, aes(x=age, y=cesd, col=as.factor(SEX1))) + 
  geom_point() + theme_bw() + theme(legend.position="top") + 
  scale_color_manual(name="SEX", values=c("red", "darkgreen")) + 
  geom_smooth(se=FALSE, method="lm") + 
  geom_smooth(aes(x=age, y=cesd), col="blue", se=FALSE, method='lm')
```


### b) using an interaction model.

```{r}
summary(lm(cesd ~ age + sex + income + age*sex, data=depress))
```

## 3. Which of the two models in question 2 assumes that the affect of income on depression is constant (does not change) between males and females?

The interaction model assumes the affects are constant while the stratified model fits them separately based on gender.


## 4. Determine whether the regression plane can be improved by also including `weight`. Use two measures of model fit to justify your answer to this question

```{r}
summary(lm(FFEV1 ~ FAGE + FHEIGHT, data=FEV))
summary(lm(FFEV1 ~ FAGE + FHEIGHT + FWEIGHT, data=FEV))
```   

Weight improves the model but the change is not very significant. Multiple R-squared changes from .334 to .356 and adjusted R-squared changes from .325 to .343 when the variable weight is added, Standard error also decreases slightly (by less than .01)
    
## 5. Does weight _confound_ the relationship between age or height and FEV1?

```{r}
summary(lm(FFEV1 ~ FHEIGHT, data=FEV))
summary(lm(FFEV1 ~ FHEIGHT + FWEIGHT, data=FEV))

summary(lm(FFEV1 ~ FAGE, data=FEV))
summary(lm(FFEV1 ~ FAGE + FWEIGHT, data=FEV))
```

Since both models are significant p<.0001 before and after weight is added, it seems that weight is not a confounder of age or height when measuring FEV1.
## 6. Fit a model of income using age, sex, educational level and religion as predictors.

```{r}
new.dep <- depress %>% select(income, age, sex, educat, relig)
View(new.dep)

new.dep$relig <- factor(new.dep$relig, labels = c("Protestant", "Catholic", "Jewish", "No Religion"))
new.dep$sex <- factor(new.dep$sex, labels = c("Male", "Female"))
new.dep$educat <- factor(new.dep$educat, labels = c("less than highschool", "some highschool", "finished highschool", "some college", "bachelors", "masters", "doctorate"))

dep.model2 <- lm(income ~ age + sex + educat + relig, data = new.dep)

summary(dep.model2)
```

### a. Use a general F test to determine whether religion has an effect on income.
```{r}
new.dep2 <- depress %>% select(income, age, sex, educat, relig)
new.dep2$relig <- factor(new.dep2$relig, labels = c("Protestant", "Catholic", "Jewish", "No Religion"))
new.dep2$sex <- factor(new.dep2$sex, labels = c("Male", "Female"))
new.dep2$educat <- factor(new.dep2$educat, labels = c("less than highschool", "some highschool", "finished highschool", "some college", "bachelors", "masters", "doctorate"))
full_model <- lm(income ~ age + sex + educat +relig, data=new.dep2)
pander(summary(full_model))
survey::regTermTest(full_model, "relig")
```

The P - Value is .246 so we can conclude that religion is not a good predictor of income level for an individual.

### b. State the reference categories for both religion and educational level.

The reference category for religion is protestant and the reference category for education level is less than highschool.


### c. Interpret the coefficient for each level of educational level
```{r}
confint(full_model)
```
B_3: After controlling for sex, age, and religion, individuals who have completed some high school are expected to have an income of 18.28 thousand dollars higher than people who did not complete any high school. 95% CI: (3.89, 32.67) (p-value = 0.012)

B_4: After controlling for sex, age, and religion, individuals who have finished high school are expected to have an income of 11.98 thousand dollars higher than people who did not complete any high school. 95% CI: (-1.93, 25.90)  (p-value = .0913)

B_5: After controlling for sex, age, and religion, individuals who have completed some college are expected to have an income of 29.14 thousand dollars higher than people who did not complete any high school. 95% CI: (13.78, 44.50)  (p-value = .0002)

B_6: After controlling for sex, age, and religion, individuals who have completed their bachelors are expected to have an income 29.68 thousand dollars higher than people who did not complete any high school. 95% CI: (13.44, 45.92)  (p-value = .0004)

B_7: After controlling for sex, age, and religion, individuals who have completed their masters are expected to have an income 20.77 thousand dollars higher than people who did not complete any high school. 95% CI: (6.48, 35.05)  (p-value = .0045)

B_8: After controlling for sex, age, and religion, individuals who have completed their doctorate are expected to have an income 7.644 thousand dollars higher than people who did not complete any high school. 95% CI: (-6.40, 21.68)  (p-value = .2848)

\newpage



# Part II: Variable Selection

## 1. PMA6 9.9
```{r}
OC.data <- FEV %>% select(OCFEV1, OCAGE, OCHEIGHT, OCWEIGHT, OCFVC)

subset_result <- regsubsets(OCFEV1~.,data=OC.data, nvmax = 5)
plot(subset_result, scale="bic")
summary(subset_result)


nullmodel=lm(OCFEV1~1, data=OC.data)
fullmodel=lm(OCFEV1~., data=OC.data)

model_step_b <- step(fullmodel,direction='backward')
model_step_f <- step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='forward')

summary(model_step_b)
summary(model_step_f)

models <- summary(subset_result)
data.frame(AdjustedR2 = models$adjr2)
```
Using the models above, and looking at the r-squared values for each of predictors, the variables that best predict fev1 in the oldest child in order from strongest to least strong are FVC, Height, AGE, then weight.

## 2. PMA6 9.11
```{r}
subset_result <- regsubsets(OCHEIGHT ~ OCAGE + OCWEIGHT + MHEIGHT + MWEIGHT + FHEIGHT + FWEIGHT,data=FEV, nbest=2, nvmax = 14)
plot(subset_result, scale="bic")
summary(subset_result)
```
Using subset regression, the variables that best predict height in the oldest child are their age, their weight, and the height of the father.

## 3. PMA6 9.12
```{r}
model_12 <- FEV %>% select(OCHEIGHT, OCSEX, OCAGE, OCWEIGHT, MHEIGHT, MWEIGHT, FHEIGHT, FWEIGHT)
model_12$OCSEX <- ifelse(model_12$OCSEX == 1, "Male", "Female")

model_12female <- model_12 %>% select(OCHEIGHT, OCSEX, OCAGE, OCWEIGHT, MHEIGHT, MWEIGHT, FHEIGHT, FWEIGHT) %>% 
  filter(OCSEX == "Female")
model_12male <- model_12 %>% select(OCHEIGHT, OCSEX, OCAGE, OCWEIGHT, MHEIGHT, MWEIGHT, FHEIGHT, FWEIGHT) %>%
  filter(OCSEX == "Male")

model_12female <- model_12female %>% select(OCHEIGHT, OCAGE, OCWEIGHT, MHEIGHT, MWEIGHT, FHEIGHT, FWEIGHT)
model_12male <- model_12male %>% select(OCHEIGHT, OCAGE, OCWEIGHT, MHEIGHT, MWEIGHT, FHEIGHT, FWEIGHT)
View(model_12)
summary(regsubsets(OCHEIGHT ~., data = model_12female, nvmax = 3))
summary(regsubsets(OCHEIGHT ~., data = model_12male, nvmax = 3))
```
For girls who are the oldest child the best variables to predict height are their age, weight and height of the middle child. For boys who are the oldest child, the best predictors are age, weight, and fathers height.

## 4. PMA6 9.13
Some potential confounding variables could be race or ethnicity since different cultures drink at different ages. A precision variable could be the amount of parents they live with or their friends/siblings.
```{r}

hiv.model <- hiv %>% select(AGEALC, ETHN, LIVWITH, SIBLINGS, FRNDS) %>% na.omit()
hiv.model$ETHN <- factor(hiv.model$ETHN,labels = c("Hispanic","Black","Other"))

hiv.model$LIVWITH <- factor(hiv.model$LIVWITH,labels = c("Both_parents","One_parent","Other"))

hiv_lm <- lm(AGEALC ~  ETHN + LIVWITH + LIVWITH*ETHN, data = hiv.model)
summary(hiv_lm)
```
None of the variables had a significant p level. I might need to fit more variables with the model.

## 4. PMA6 9.14
```{r}
drink.model <- regsubsets(AGEALC ~ GENDER + LIVWITH + SIBLINGS + JOBMO +
EDUMO + HOWREL + ATTSERV + NGHB1 + NGHB2 + NGHB3 + NGHB4 + NGHB5 + NGHB6 +
NGHB7 + NGHB8 + NGHB9 + NGHB10 + NGHB11 + FINSIT + ETHN + AGESMOKE + AGEMAR +
LIKESCH + HOOKEY + NHOOKEY, data=filter(hiv, AGEALC!="0"))
summary(drink.model)

drink.lm <- lm(AGEALC ~ NGHB9 + AGESMOKE + NHOOKEY, data=filter(hiv, AGEALC!="0"))
summary(drink.lm)
```
The best variables to predict the age at which adolescents started drinking are NGHB9(homelessness in the community),AGESMOKE(the age that they started smoking), and NHOOKEY(how often they skipped school). When a linear model is fit using these 3 variables as predictors, we get P-values which are all <.01. The age at which they started smoking seems to be the best predictor with a p-value of .0002 when homelessness and hookey are held constant.
   
