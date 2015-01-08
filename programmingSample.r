installChecker <- function(package){
  # install required packages
  if(is.element(package,rownames(installed.packages())) == T){
    return(paste(package, "already installed"))
  }else {
    install.packages(package) 
  }
}

modelPerf <- function(object, ...){
  # displays diagnostics of models based on information theoretic approach
  # logic holds for w/e statistical comparison, actual calcs done by R fun'
  # should I include likelihood stuff/rel. prob?
  modelList <- list(object, ...)
  results   <- data.frame(Model = seq(1,length(modelList)))
  results$AIC <- sapply(FUN = AIC, modelList)
  results$BIC <- sapply(FUN = BIC, modelList)
  results
}

hypTest <- function(model){
  # calculate AICc of the 2 different models  
  # http://warnercnr.colostate.edu/~anderson/PDF_files/ttests.pdf
  null.param <- 1; alt.param  <- 2 #linear regression with a dummy var, also length(fit.aov2$coefficients)
  null  <- AIC(model) + (2*null.param*(null.param + 1))/(length(model$residuals)*null.param)
  alt   <- AIC(model) + (2*alt.param*(alt.param + 1))/(length(model$residuals)*alt.param)
  pData <- c(null,alt); dI <- pData - min(pData)
  lhood <- sapply(dI, function(x)exp((-1/2)*x))
  prob  <- lhood/sum(lhood)
  data.frame(model = c("null", "alternative"), AICc = pData, 
             Likelihood = lhood, Rel.Probability = prob)
}

# An experiment has been performed to test the effects of a fertilizer.
# Four plots were randomly assigned to either control or fertilized treatment.
# Please provide code (not output) to explore the data and describe the results.
dat4 <- data.frame(treatment = factor(rep(c("control", "fertilized"), each = 20)),
                   plot = factor(rep(c("C", "D", "B", "A"), each = 10)),
                   y = c( 9.99, 10.31,  9.64, 10.51,  9.31,  9.55, 10.23,  9.95,  9.96, 10.49,
                          9.17,  9.23,  9.35,  9.71,  8.92, 10.23, 10.15,  9.70, 10.09, 10.46,
                         10.94,  9.75, 10.44, 10.09, 10.53, 10.43,  9.75, 10.58, 11.06,  9.72,
                          9.98, 10.44,  9.78, 10.01, 10.12, 11.84, 10.64, 10.80,  9.75,  9.99))

requiredpkgs <- c("plyr", "ggplot2")
sapply(requiredpkgs, installChecker)
lapply(requiredpkgs, require, character.only=T)

# exploratory analysis followed model selection 
# descriptive model using anova

# variance 
tapply(dat4$y, dat4$treatment, var) #treatment
tapply(dat4$y, dat4$plot, var) #plot

# visualizations
ggplot(dat4, aes(y)) + geom_density()
ggplot(dat4, aes(y)) + geom_density(aes(fill = treatment), alpha = .5)
ggplot(dat4, aes(y)) + geom_density(aes(fill = plot), alpha = .5)

qqnorm(dat4$y[which(dat4$treatment == "control")])
qqline(dat4$y[which(dat4$treatment == "control")])
qqnorm(dat4$y[which(dat4$treatment == "fertilized")])
qqline(dat4$y[which(dat4$treatment == "fertilized")])

# things look normal enough for the small sample size
# even with an outlier... sadly a fairly influential one, 

dat4$y[which(dat4$y == max(dat4$y))]<-NA #remove outlier, still "balanced" but its a judgement call

# more exploratory graphs for post outlier

ggplot(dat4, aes(y)) + geom_density(aes(fill = treatment), alpha = .5)
ggplot(dat4, aes(y)) + geom_density(aes(fill = plot), alpha = .5)



# when analyzed by both factors things appear to be approx. normal, 
# could use a more quantitative normality test... like anderson darling or ks test
# anova robustness to viol of norm, but small sample sizes/power issues could manifest
# thats moot a posteriori 

# model selection according to AIC/BIC
# and likelihood/relative probability
# some of these could be t-tests but they are similar results.. 
# and preserves rigor of AIC/BIC
# testing all possible models because of possibility of interaction
# confounding and etc. 

fit1      <- lm(y ~ treatment + plot, data = dat4)
fit.aov1  <- aov(fit1)

fit2      <- lm(y ~ treatment , data = dat4)
fit.aov2  <- aov(fit2)

fit3      <- lm(y ~ plot , data = dat4)
fit.aov3  <- aov(fit3)

fit4      <- lm(y ~ treatment + plot + treatment:plot, data = dat4)
fit.aov4  <- aov(fit4)

modelPerf(fit.aov1, fit.aov2, fit.aov3, fit.aov4)

# take the best model and look at effect size/metric
summary(fit.aov2)
TukeyHSD(fit.aov2)
hypTest(fit.aov2)
ggplot(dat4, aes(treatment,y)) + geom_boxplot()

