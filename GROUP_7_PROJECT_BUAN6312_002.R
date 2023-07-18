#Importing required Libraries
library(haven)
library(readr)
install.packages("corrplot")
library("corrplot") 
library(car)
library(lmtest)
library(sandwich)
library(plm)

#Data import
guns <- read_dta("C:/Users/ankit/Downloads/guns.dta")
summary(guns)
attach(guns)

#Export file as csv to import in Power BI for visualizations
write_csv(guns, file = "/Users/ankit/OneDrive/Desktop/Spring 2023/Applied Econ/Project/guns.csv")

#Correlation Matrix
corrplot(cor(guns), method = "circle",addCoef.col='black')  

#Histograms of all variables
hist(guns$incarc_rate)
hist(guns$pb1064)
hist(guns$pw1064)
hist(guns$pm1029)
hist(guns$vio)
hist(guns$mur)
hist(guns$rob)
hist(guns$pop)
hist(guns$avginc)
hist(guns$density)

#Creation of Total Crime rate variable which is a weighted average of rob, mur and vio
guns$Total_Crime_rate <- (guns$vio+guns$mur+guns$rob)/3
hist(guns$Total_Crime_rate)

#Log transformation of variables which are positively skewed i.e. Total rime rate, incarc_rate, density
guns$lg_incarc <- log(guns$incarc_rate)
guns$lg_density <- log(guns$density)
guns$lg_vio <- log(guns$vio)
guns$lg_TotalCrimeRate <- log(guns$Total_Crime_rate)

#Comparing histograms of log transformed variables
par(mfrow=c(2,2))
hist(guns$Total_Crime_rate)
hist(guns$lg_TotalCrimeRate)
hist(guns$incarc_rate)
hist(guns$lg_incarc)
hist(guns$density)
hist(guns$lg_density)

#Effect of shall on crime rate
model1 <- lm(lg_TotalCrimeRate~shall, data=guns)
summary(model1)

#Effect of shall in presence of other variables
model1_2 <- lm(lg_TotalCrimeRate~lg_incarc+pb1064+pw1064+pm1029+pop+avginc+lg_density+shall, data = guns)
summary(model1_2)

#to check joint significance using F test
linearHypothesis(model1_2, c("pb1064=0","pw1064=0"))

#Dropping pb1064 & pw1064 as they are not significant
model1_3 <- lm(lg_TotalCrimeRate~lg_incarc+pm1029+pop+avginc+lg_density+shall, data = guns)
summary(model1_3)

#To check collinearity in the model
vif(model1_3)

#Heteroskedasticity test for model
bptest(model1_3)

#Considering this data as panel data
#Pooled model with robust SE
pooled_model <- plm(lg_TotalCrimeRate~lg_incarc+pm1029+pop+avginc+lg_density+shall, data = guns,index=c('stateid','year'), model='pooling')
summary(pooled_model)
coeftest(pooled_model, vcov. = vcovHC, type = "HC1")

#Fixed effect model
fe_model <- plm(lg_TotalCrimeRate~lg_incarc+pm1029+pop+avginc+lg_density+shall, data = guns,index=c('stateid','year'), model='within')
summary(fe_model)

#Entity Fixed Time effect model
efte_model <- plm(lg_TotalCrimeRate~lg_incarc+pm1029+pop+avginc+lg_density+shall+as.factor(year), data = guns,index=c('stateid','year'), model='within')
summary(efte_model)

#Random Effect model
re_model <- plm(lg_TotalCrimeRate~lg_incarc+pm1029+pop+avginc+lg_density+shall, data = guns,index=c('stateid','year'), model='random')
summary(re_model)

#Hausman Test 
phtest(fe_model,efte_model)


#using log(vio) as dependent variable
model2 <- lm(lg_vio~lg_incarc+pm1029+pb1064+pw1064+pop+avginc+lg_density+shall, data = guns)
summary(model2)
coeftest(model2, vcov. = vcovHC, type = "HC1")

#Fixed effect Model
fe_model <- plm(lg_vio~lg_incarc+pm1029+pb1064+pw1064+pop+avginc+lg_density+shall, data = guns, index=c('stateid','year'), model='within')
summary(fe_model)

#Entity Fixed time effect model
fte_model <- plm(lg_vio~lg_incarc+pm1029+pop+avginc+lg_density+shall+as.factor(year), data = guns, index=c('stateid','year'), model='within')
summary(fte_model)

#Random effect model
re_model <- plm(lg_vio~lg_incarc+pm1029+pop+avginc+lg_density+shall, data = guns, index=c('stateid','year'), model='random')
summary(re_model) 

#Hausman Test  
phtest(fe_model,re_model)
