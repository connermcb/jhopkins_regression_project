knitr::opts_chunk$set(echo = TRUE)
# load data
library(datasets)
data("mtcars")

# load requisite packages
library(dplyr)
library(GGally)
library(ggplot2)
library(knitr)
library(tibble)
library(xtable)
# initial linear model with single predictor `am`
fit1 <- lm(mpg ~ am, data=mtcars)
# linear model that now includes weight as a controlling factor
fit2 <- lm(mpg ~ factor(am) + wt, data=mtcars)
## create dataframe with means of numeric features grouped by cylinder count
# vector of variables to summarize
num_features <- c("cyl", "disp", "hp", "drat", "wt", "qsec", "gear", "carb")

# group data by cyl and summarize selected features, means
mns <- mtcars[mtcars$cyl != 6,]%>%
  group_by(cyl)%>%
  summarize(disp=mean(disp), hp=mean(hp), 
            drat=mean(drat), wt=mean(wt), 
            qsec=mean(qsec), gear=mean(gear), 
            carb=mean(carb))

# add column with names for each row
mns <- cbind(c("Means 4cyl", "Means 8cyl"), mns)
colnames(mns)[1] <- " "

# vector of outliers from residuals plot, subset on outliers
outlier_names <- c("Chrysler Imperial","Fiat 128", "Toyota Corolla")
outliers <- mtcars[outlier_names, num_features]
outliers <- rownames_to_column(outliers, var=" ")

# bind means dataframe and outliers
outliers_sumry <- rbind(mns, outliers)
kable(outliers_sumry, digits=3, 
      caption="Comparison of Outliers & Grouped Means")

# create linear model with weight and acceleration added to predictors
fit3 <- lm(mpg ~ factor(am) + wt + qsec, data=mtcars)

# analysis of variance of second and third models
anvar <- anova(fit2, fit3)

# test change in hat values for each observation if removed from data
hvs_fit3 <- hatvalues(fit3)

# test influence of each observation
dff_fit3 <- dffits(fit3)
dfbts_fit3 <- dfbetas(fit3)

# recreate model 3 using revised dataset
fit4 <- lm(mpg~factor(am) + wt + qsec, 
           data=mtcars[rownames(mtcars) != "Chrysler Imperial",])

# output pretty table of coefficients
kable(summary(fit4)$coef, digits = 3, caption="Coefficients of Finalized Model")
summary(fit3)
# calculate predictive sum of squares for each model
PRESS <- sapply(list(fit1, fit2, fit3, fit4), 
                function(mdl){sum(rstandard(mdl, type="pred")^2)})

# create data frame with PRESS values
mdls <- c("Model 1", "Model 2", "Model 3", "Rmv Outlier")
predictors <- c("am", "am + wt", "am + wt + qsec", "am + wt + qsec")
rdf <- cbind(mdls, predictors, round(PRESS, 3))
colnames(rdf) <- c("Model", "Predictors", "PRESS")
# pairs plot of mtcars features
ggpairs(mtcars)
## density plot of mpg by transmission type
mtcars$am <- relevel(as.factor(mtcars$am), ref=2)
g <- ggplot()+
  geom_density(data=mtcars,aes(x=mpg, color=factor(am))) +
  labs(title="Fuel Efficiency (mpg) by Transmission Type")+
  scale_color_manual(name="Transmission Type",
                     labels=c("Automatic", "Manual"),
                     values=c("#F8766D", "#00BFC4"))+
  theme(panel.background = element_blank(), 
        legend.position = c(1,1),
        legend.title = element_text(size=8),
        legend.text = element_text(size = 8),
        legend.justification = c(1,1),
        legend.background = element_rect(color="black", fill="grey85"))

aut_dens <- density(mtcars[mtcars$am==0,]$mpg)
q95 <- quantile(mtcars[mtcars$am==0,]$mpg, probs=0.95)
df_aut_dens <- data.frame(aut_dens$x, aut_dens$y)

g + geom_area(data=subset(df_aut_dens, aut_dens.x >= q95),
              aes(x=aut_dens.x, y=aut_dens.y), fill="#00BFC4", alpha=.25)+
  geom_vline(xintercept = mean(mtcars[mtcars$am==1,]$mpg), 
             size=1, color="#F8766D")
# plot of displacement by number of cylinders
par(mfrow=c(1,1), mar=c(4, 4, 3, 2)+0.1)
plot(mtcars$cyl, mtcars$disp, col=factor(mtcars$am), pch=20,
     main="Displacement by Number of Cylinders",
     ylab="Diplacement (cu.in.)", xlab="No. of Cylinders")
legend(4, 450, legend=c("automatic", "manual"), col = c("red", "black"), pch=20)
# plot of weight by displacement colored by number of cylinders
plot(mtcars$disp, mtcars$wt, col=mtcars$cyl, pch=as.integer(mtcars$am),
     main="Displacement by Weight",
     ylab="Weight (tons)", xlab="Displacement (cu.in.)")
legend(72,5.3, legend=c("Automatic", "Manual"), pch=c(2,1))
legend(385,2.8, legend=c("4cyl", "6cyl", "8cyl"), c("blue", "magenta", "gray"))
# residuals plots
par(mfrow=c(2, 1), mar=c(2, 2, 2, 2))
plot(fit2, which=1)
abline(h=0, lty=2, col="blue")
plot(fit2, which=2)
# scatterplot of covariates
plot(mtcars$qsec, mtcars$mpg, col=mtcars$wt, pch=20,
     main="Acceleration by Miles/Gallon")
abline(lm(mpg~qsec, data=mtcars))
text(mtcars$qsec, mtcars$mpg, rownames(mtcars), cex = 0.5, pos=2)
text(mtcars$qsec, mtcars$mpg, mtcars$wt, cex = 0.5, pos=4)
legend(22.1, 20, legend=c("1", "2", "3", "4", "5"),
       fill=palette()[1:5], title="Int of WT", cex = 0.7)
fit <- lm(mpg~factor(am) + wt + qsec, data=mtcars)
plot(fit, which=1)
## 
