# installing and packages
#install.packages("lme4")
#install.packages("dplyr")
#install.packages("nlme")

#loading the libraries
library(lme4)
library(nlme)
library(nlme)
library(ggplot2)
library(dplyr)
library(lattice) 
library(geepack)

#EXAMPLE ONE POTTHOFF-ROY DATA
##loading and reading the csv file
PRlong <- read.csv("PotthoffRoyLong.csv", header = T)

##grouping the gender into male 'M' and female 'F'
PRlong$gender <- ifelse(PRlong$group == 1, "M", "F")

### Printing the first 20 elements
head(PRlong[, c(1,3,4,5,6)], 20)

### Printing the last 20 elements
tail(PRlong[, c(1,3,4,5,6)], 20)

str(PRlong)

###printing the dimensions
dim(PRlong)


###Adjusting the PRlong data set
PRlong         <- PRlong %>% arrange(subject)

PRlong$age     <- 2*PRlong$time + 6
PRlong$age 
PRlong$age.c.8 <- PRlong$age - 8
PRlong$age.c.8 
PRlong$time    <- as.factor(PRlong$time)
PRlong$time  
PRlong$gender  <- as.factor(PRlong$gender)
PRlong$gender
PRlong$subject <- as.factor(PRlong$subject)
PRlong$subject

 ### visualizing the data set
xyplot(dental ~ age|gender, data = PRlong, type = "b", lwd = 2,
       pch = 19, cex = 1.2, groups = subject)

###grouping the data
dental.grouped <- groupedData(dental ~ age | subject, outer = ~ gender,
                              data = PRlong)

###Visualizing the grouped data 
plot(dental.grouped, display="subject",
     outer=TRUE, aspect=1, key=F,
     xlab="Age", ylab="Dental Growth (mm)", pch=19, cex=0.8,
     main="Potthoff & Roy (1964) Orthodontic Measurements on Children")


##Fitting the Random intercept model into the PRlong dataset
Random_intercept_model <- lme(dental ~ 1+ age + gender + age*gender , random = ~ 1 | subject, data = PRlong, method = "ML")
summary(Random_intercept_model)

##Fitting the Random Intercept trend model into the PRlong dataset.

Random_inter_trend <- lme(dental ~  age + gender + age*gender,
             random =  ~  age | subject,
             data=PRlong, method="ML")
summary(Random_inter_trend)

coef(Random_inter_trend)


#creating a dataframe with the fitted values
PRlong$fitted_vals1 <- fitted(Random_intercept_model)
View(PRlong)
  newdata <- data.frame(subject = PRlong$subject, age = PRlong$age, gender = PRlong$gender, fitted_vals1= PRlong$fitted_vals1)
  newdata
  # Plotting the predicted values against the original data
  ggplot(PRlong, aes(x = age, y = dental, color = factor(gender))) +
    geom_point() +
    geom_line(data = newdata, aes(x = age, y = fitted_vals1, group = subject), color = "blue") +
    stat_smooth(method =  "lm", aes(group = NULL), se = FALSE, color = "red", size = 1.1)+
    facet_wrap(~ gender, ncol = 4, scales = "free") +
    labs(title = "Random Intercept Model: dental vs. age and gender",
         x = "age (years)", y = "dental growth in mm ")
  
  
#Visualizing The Random Intercept and trend model.
PRlong$fitted_val_2 = fitted(Random_inter_trend)
Newdata_2 <- data.frame(subject = PRlong$subject, age = PRlong$age, gender = PRlong$gender, fitted_vals1= PRlong$fitted_vals1, fitted_val_2 = PRlong$fitted_val_2)
View(Newdata_2 )

ggplot(PRlong, aes(x = age, y = dental, color = factor(gender))) +
  geom_point() +
  geom_line(data = Newdata_2, aes(x = age, y = fitted_val_2, group = subject), color = "blue") +
  stat_smooth(method =  "lm", aes(group = NULL), se = FALSE, color = "black", size = 1.1)+
  facet_wrap(~ gender, ncol = 4, scales = "free") +
  labs(title = "Random Intercept and trend Model: dental vs. age and gender",
       x = "age (years)", y = "dental growth in mm ")

 
 ###comparing the two models
 anova(Random_inter_trend, Random_intercept_model)

 
 
 
 #EXAMPLE TWO SITKA SPRUCE DATA
 
 #install.packages("MASS") 
 library(MASS) 
 summary(Sitka) 
 head(Sitka, 30) 
 tail(Sitka, 30) 
 tail(Sitka, 145)
 
 ###Visualizing the Sitka data set
 xyplot(size ~ Time|as.factor(treat), data=Sitka, type="l", lwd=2, groups=tree)
 
 ###visualizing the grouped data
 SitkaL.grouped <- groupedData(size ~ Time | tree, outer=~ treat, data=Sitka) 
 plot(SitkaL.grouped, display="tree", outer=TRUE, aspect=1, key=F, xlab="days", ylab="size", pch=19, cex=0.8, main="Sitka's Trees - L portion only") 

 


####fitting the curvilinear model
curvelinear <- lme(size ~ Time + I(Time^2) + treat + Time*treat,
                 random =   ~ Time | tree, data = SitkaL.grouped, method = 'ML')
summarycurvelinear)

###centering the variable 'time' to avoid having negative intercepts.

SitkaL.grouped$Time.c.152 <- SitkaL.grouped$Time - 152
View(SitkaL.grouped)

#Sitka Curvilinear model with time centered at 152
curvelinear <- lme(size ~ Time.c.152 + I((Time.c.152)^2) + treat + Time.c.152*treat,
                 random =   ~ Time.c.152 | tree, data = SitkaL.grouped, method = 'ML')
summary(curvelinear)

#Model trimming  removing the dummy variable term 'treatozone' 
#since it is not statistically significant

curvelinear <- lme(size ~ Time.c.152 + I((Time.c.152)^2)  + Time.c.152*treat,
                 random =   ~ Time.c.152 | tree, data = SitkaL.grouped, method = 'ML')
summary(curvelinear)


##Visualizing the curvilinear model on Sitka spruce data
ggplot(SitkaL.grouped, aes(x = Time, y = size)) + 
  geom_point(aes(fill = as.factor(tree)), pch = 21, size = 1, stroke = 0.5) + 
  stat_smooth(aes(col = as.factor(tree)), se = FALSE, method = "lm", formula = y ~ x + I(x^2), lwd = 0.5) + 
  stat_smooth(aes(group=treat), col="black", se=FALSE, method="lm", 
              formula=y~x+I(x^2), lwd=1) +
  facet_wrap(~treat) +
  scale_x_continuous(name = "Time (Days)", breaks = c(150,260)) + 
  scale_y_continuous(name = "Size Score (0-10)", limits = c(0,10)) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 14, colour = "black"), 
        axis.title = element_text(size = 14, face = "bold"),
        strip.text.x = element_text(size = 14),
        legend.position = "none")


###### Example 3 DNase data
library(datasets)
View(DNase)

#Visualing the data through a scatter plot
ggplot(data = DNase, aes(x = conc, y = density, color = factor(Run))) +
  geom_point() +
  labs(x = "Concentration", y = "Density", color = "Run") +
  theme_minimal()

####visiualizing the data using coplot
coplot(density ~ conc | Run, data = DNase,
       show.given = FALSE, type = "b")


coplot(density ~ log(conc) | Run, data = DNase,
       show.given = FALSE, type = "b")



###FITTING THE LOGISTIC MODEL TO THE DNASE DATA
Log_model <- nlme(density ~ SSlogis(conc, Asym, xmid, scal),
              data = DNase,
              fixed = Asym + xmid + scal ~ 1,
              random = Asym + xmid + scal ~ 1 | Run,
              start = c(Asym = max(DNase$density),
                        xmid = mean(DNase$conc),
                        scal = 1))

summary(Log_model)
predicted_density <- fitted(Log_model)

##visualizing the logistic model using ggplot

ggplot(DNase, aes(x = conc, y = density)) +
  geom_point() +
  geom_line(aes(y = predicted_density), color = "red") +
  facet_wrap(~ Run, ncol = 2) +
  labs(x = "Concentration", y = "Density")


##EXAMPLE 4 TOENAIL DATA
toenail <- read.table("toenail.dat")
toenail[1,1] <- 1
colnames(toenail) <- c("id", "y", "trt", "month", "visit")
head(toenail, 30)
dim(toenail)
tail(toenail)
range(toenail$month)
#install.packages("visdat")
library(visdat)
vis_miss(toenail)
#library(reshape2)
#install.packages("geepack")


###fitting the GEE Model into the Toenail dataset
marginal <- geeglm(y ~ month + trt:month, data=toenail,
                   corstr="exchangeable", id=id,
                   family="binomial")
summary(marginal)


####Fitting the GLMM Model in to the Toenail data
mixed <- glmer(y ~ month + month:trt + (1 | id), nAGQ=100,
               data=toenail, family="binomial" )
summary(mixed)
