dat4 <- data.frame(treatment = factor(rep(c("control", "fertilized"), each = 20)),
                   plot = factor(rep(c("C", "D", "B", "A"), each = 10)),
                   y = c( 9.99, 10.31,  9.64, 10.51,  9.31,  9.55, 10.23,  9.95,  9.96, 10.49,
                          9.17,  9.23,  9.35,  9.71,  8.92, 10.23, 10.15,  9.70, 10.09, 10.46,
                          10.94,  9.75, 10.44, 10.09, 10.53, 10.43,  9.75, 10.58, 11.06,  9.72,
                          9.98, 10.44,  9.78, 10.01, 10.12, 11.84, 10.64, 10.80,  9.75,  9.99))

installChecker <- function(package) {
  # install required packages
  if(is.element(package,rownames(installed.packages())) == T) {
    return(paste(package, "already installed"))
  } else {
    install.packages(package) 
  }
}

requiredpkgs <- c("plyr", "ggplot2", "rgl", "ks", "KernSmooth")
sapply(requiredpkgs, installChecker)
lapply(requiredpkgs, require, character.only=T)

# bayesian analysis of both params assuming exchangeability
# aka joint inference for the params

#prior treatment

# precision - gamma (v0/2, (v0*var0)/2)
# var0, (v0, k0) <- sample variance, sample size
#mean n(sample mean, var/ k0)
#control 

#priors
#non informative

mean0 <- 0
k0    <- 1
s20   <- 0
nu0   <- 1

#data
treatments.stats <- ddply(dat4, .(treatment), summarise, samplemean = mean(y), samplevar = var(y))
y <- dat4$y[which(dat4$treatment == "control")]
n <- 20

#posterior
kn  <- k0 + n
nun <- nu0 +n
mun <- (k0*mean0 + n*treatments.stats[1,2])/kn
s2n <- (nu0 * s20 + (n-1)*treatments.stats[1,3] + k0 *n *(treatments.stats[1,2]-mean0)^2/(kn))/(nun)

#use mc to approximate joint posterior

s2.postsample.c     <- 1/rgamma(1e5, nun/2, s2n*nun/2)
theta.postsample.c  <- rnorm(1e5, mun, sqrt(s2.postsample.c/kn))

plot(density(s2.postsample.c))
plot(density(theta.postsample.c))

bandwidth.c <- c(dpik(s2.postsample.c), dpik(theta.postsample.c))
x.c   <- cbind(s2.postsample.c, theta.postsample.c)
est.c <- bkde2D(x.c, bandwidth = bandwidth.c)

res <- data.frame(est.c$x1, est.c$x2, est.c$fhat)
params.c <- res[which(res[,3] == max(res[,3])), c(1:2)]

#############################################

#fertilized

#priors
#non informative priors

mean0 <- 0
k0    <- 1
s20   <- 0
nu0   <- 1


#posterior

kn  <- k0 + n
nun <- nu0 +n
mun <- (k0*mean0 + n*treatments.stats[2,2])/kn
s2n <- (nu0 * s20 + (n-1)*treatments.stats[2,3] + k0 *n *(treatments.stats[1,2]-mean0)^2/(kn))/(nun)

#mc sampling from these distributions to determine mean and var conf intervals
#best one chosen via aic/bic

s2.postsample.f     <- 1/rgamma(1e5, nun/2, s2n*nun/2)
theta.postsample.f  <- rnorm(1e5, mun, sqrt(s2.postsample.f/kn))

plot(density(s2.postsample.f))
plot(density(theta.postsample.f))

bandwidth.f <- c(dpik(s2.postsample.f), dpik(theta.postsample.f))
x.f   <- cbind(s2.postsample.f, theta.postsample.f)
est.f <- bkde2D(x.f, bandwidth = bandwidth.f)

res <- data.frame(est.f$x1, est.f$x2, est.f$fhat)
params.f <- res[which(res[,3] == max(res[,3])), c(1:2)]


#contour comparisons of joint posterior distributions
contour(est.c$x1, est.c$x2, est.c$fhat)
contour(est.f$x1, est.f$x2, est.f$fhat, add = T, col = 2)


#compare the results from this to the results from a frequentist t-test/anova analysis

fit2      <- lm(y ~ treatment , data = dat4)
fit.aov2  <- aov(fit2)
TukeyHSD(fit.aov2)


effectsize.mc <- data.frame(effectsize = theta.postsample.f - theta.postsample.c)
effectvar.mc  <- data.frame(effectvar = s2.postsample.f - s2.postsample.c)
vline         <- quantile(effectsize.mc[,1], c(.025,.975))
vlines2       <- quantile(effectvar.mc[,1], c(.025,.975))

ggplot(effectsize.mc, aes(effectsize)) + geom_density() +
  geom_vline(xintercept = vline[1], linetype = "dashed", col = "red") + 
  geom_vline(xintercept = vline[2], linetype = "dashed", col = "red")

ggplot(effectvar.mc, aes(effectvar)) + geom_density() +
  geom_vline(xintercept = vlines2[1], linetype = "dashed", col = "red") + 
  geom_vline(xintercept = vlines2[2], linetype = "dashed", col = "red")

