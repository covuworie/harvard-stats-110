#############
# CHAPTER 1 #
#############

# factorials and binomial coefficients
factorial(n)
choose(n,k)

# sample command
n <- 10; k <- 5
sample(n,k)
sample(n,k,replace=TRUE)
sample(n,n)

# matching problem simulation
n <- 100
r <- replicate(10^4,sum(sample(n)==(1:n)))
sum(r>=1)/10^4

# birthday problem
k <- 23
1-prod((365-k+1):365)/365^k
pbirthday(23)
qbirthday(0.5)
pbirthday(23,coincident=3)


#############
# CHAPTER 2 #
#############

# family with two children
n <- 10^5
child1 <- sample(2,n,replace=TRUE)
child2 <- sample(2,n,replace=TRUE)

# elder is a girl
n.ab <- sum(child1==1 & child2==1)
n.b <- sum(child1==1)
n.ab/n.b

# at least one is a girl
n.ab <- sum(child1==1 & child2==1)
n.b <- sum(child1==1 | child2==1)
n.ab/n.b

# Monty Hall simulation (interactive game version)
n <- 10^5
cardoor <- sample(3,n,replace=TRUE)

monty <- function() {
    doors <- 1:3
    
    # randomly pick where the car is
    cardoor <- sample(doors,1)  
    
    # prompt player
    print("Monty Hall says `Pick a door, any door!'")  
    
    # receive the player's choice of door (should be 1,2, or 3)
    chosen <- scan(what = integer(), nlines = 1, quiet = TRUE)
   
    # pick Monty's door (can't be the player's door or the car door)
    if (chosen != cardoor) montydoor <- doors[-c(chosen, cardoor)]
    else montydoor <- sample(doors[-chosen],1)
    
    # find out whether the player wants to switch doors
    print(paste("Monty opens door ", montydoor, "!", sep=""))
    print("Would you like to switch (y/n)?")
    reply <- scan(what = character(), nlines = 1, quiet = TRUE)
    
    # interpret what player wrote as "yes" if it starts with "y"
    if (substr(reply,1,1) == "y") chosen <- doors[-c(chosen,montydoor)]

    # announce the result of the game!
    if (chosen == cardoor) print("You won!")
    else print("You lost!")
}


#############
# CHAPTER 3 #
#############

# dbinom, pbinom, rbinom
dbinom(3,5,0.2)
pbinom(3,5,0.2)
rbinom(7,5,0.2)

# dhyper, phyper, rhyper
dhyper(2,10,5,4)
phyper(2,10,5,4)
rhyper(1,10,5,4)

# can also do
dbinom(0:5,5,1/2)
dbinom(5,5,c(1/4,1/2,3/4))
dbinom(5,5:8,1/2)

# plotting PMFs
xs <- 0:5
ys <- dbinom(xs,5,0.2)
plot(xs,ys,type='h')
points(xs,ys,pch=16)

# how to sample from any PMF
n <- 100
xs <- c(0,1,5,10)
ps <- c(0.25,0.5,0.1,0.15) # c() creates a vector
sample(xs,n,prob=ps,replace=TRUE)

#############
# CHAPTER 4 #
#############

dgeom(5,0.2)
pgeom(5,0.2)
rgeom(1,0.2)
dnbinom(5,5,0.2)
dpois(1,5)

# number of matches
n <- 100
r <- replicate(10^4,sum(sample(n)==(1:n)))
mean(r) # this is equivalent to sum(r) / n

# number of distinct birthdays
k <- 20
r <- replicate(10^4,{bdays <- sample(365,k,replace=TRUE); length(unique(bdays))})
mean(r)
365*(1-(364/365)^k)

#############
# CHAPTER 5 #
#############

dexp(1,2)		# Expo(1) is the default Exponential
dunif(0.5,0,2)	# Unif(0,1) is the default Uniform
dnorm(1.4,2,3)	# 3 is the SD, not the variance
pnorm(2,0,2)
rnorm(10) 		# explain that N(0,1) is the default Normal
# to generate from N(mu,sigma), can either do
mu <- 1
sigma <- 2
rnorm(1,mu,sigma)
# or
mu + sigma * rnorm(1)

# plotting density functions
curve(dnorm,from=-3,to=3,n=100)

# or can evaluate over a fine grid using the seq command

# options in plotting:
# axis labels and title: xlab, ylab, main
# axis limits: xlim, ylim
# type of plot: 'l', 'h', 'p', 's'
# (optional) if 'l', can decide line width with lwd, line type with lty
# (optional) if 'p', can decide plotting character with pch
# col: 'red', 'green', 'blue', etc.
# also, histograms, as we discuss next.
x <- seq(-3,3,0.01)
y <- dnorm(x)
plot(x,y,type='l')
plot(x,y,type="l",xlab="x",ylab="dnorm(x)",main="Standard Normal PDF",col="orange")

# Universality of the Uniform
u <- runif(10^4)
hist(log(u/(1-u))) 			# Logistic
hist(log(u/(1-u)),breaks=100)
# breaks controls how many bins. 
hist(sqrt(-2*log(1-u))) 		# Rayleigh

# simulating Poisson process
n <- 50
x <- rexp(n,10)
t <- cumsum(x)
plot(t,1:n,type='s')

#############
# CHAPTER 6 #
#############

# defining a function
M <- function(t) {exp(t^2/2)}

# defining the Normal MGF
g <- function(t,mean=0,sd=1) {exp(mean*t + sd^2*t^2/2)}

# moments using numerical integration
g <- function(x) x^6*dnorm(x)
integrate(g, lower = -Inf, upper = Inf)
h <- function(x) x^2*dunif(x,-1,1)
integrate(h, lower = -1, upper = 1)

# moments of a discrete r.v.
g <- function(k) k^2*dpois(k,7)
sum(g(0:100))

# sample moments
x <- rnorm(100)
mean(x^6)

# sample mean and sample variance
z <- rnorm(1000)
mean(z)
var(z)

# sample skewness
skew <- function(x) {
    centralmoment <- mean((x-mean(x))^3)
    centralmoment/(sd(x)^3)
}

# sample kurtosis
kurt <- function(x) {
    centralmoment <- mean((x-mean(x))^4)
    centralmoment/(sd(x)^4) - 3
}

# find a median numerically
# Example: Expo(1) distribution, whose median is log(2)
g <- function(x) pexp(x) - 1/2
uniroot(g,lower=0,upper=1)
# since Expo(1) is a built-in distribution, qexp(1/2) is simpler here

# find a mode numerically
h <- function(x) x^5*exp(-x)
optimize(h,lower=0,upper=20,maximum=TRUE)

# median of Bin(50,0.2)
n <- 50; p <- 0.2
which.max(pbinom(0:n,n,p)>=0.5)

# sample mode
datamode <- function(x) {
    t <- table(x)
    m <- max(t)
    as.numeric(names(t[t==m]))
}

# dice simulation
r <- replicate(10^6,sum(sample(6,6,replace=TRUE)))
sum(r==18)/10^6

#############
# CHAPTER 7 #
#############

# dmultinom
x <- c(2,0,3)
n <- sum(x)
p <- c(1/3,1/3,1/3)
dmultinom(x,n,p)
dmultinom(x,7,p) 	# throws an error
dmultinom(x,prob=p) # also okay
rmultinom(10,n,p)

# no pmultinom because the joint CDF is complicated

# generate Bivariate Normal random vectors
# uses mvtnorm package
meanvector <- c(0,0)
rho <- 0.7
covmatrix <- matrix(c(1,rho,rho,1), nrow = 2, ncol = 2)
r <- rmvnorm(n = 10^3, mean = meanvector, sigma = covmatrix)

# another way to generate Bivariate Normal random vectors
rho <- 0.7
tau <- sqrt(1-rho^2)
x <- rnorm(10^3)
y <- rnorm(10^3)
z <- x
w <- rho*x + tau*y

# dcauchy
dcauchy(0)
pcauchy(3) 			# compare to pnorm(3)
hist(rcauchy(1000))		# hilarity!
hist(rcauchy(1000),breaks=100)


#############
# CHAPTER 8 #
#############

# Beta and Gamma distributions
dbeta(0.5,1,2)
pbeta(0.5,1,2)
rbeta(1,1,2)
dgamma(3,3,2)
pgamma(3,3,2)
rgamma(1,3,2)

# check mean and variance of a Gamma
n <- 10^5
y <- rgamma(n,3,2)
mean(y)
var(y)

# convolution of Unifs
nsim <- 10^5
x <- runif(nsim)
y <- runif(nsim)
hist(x+y)

# Bayes' billiards, Discrete Uniform
nsim <- 10^5
n <- 10 # number of balls
p <- runif(nsim)
x <- rbinom(nsim,n,p) # feed the entire vector of p's

# marginal PMF of X
hist(x,breaks=seq(-1/2,n+1/2,1))

# posterior PDF of p given X=x
hist(p[x==3])
hist(rbeta(10^4,4,8))
hist(p[x==4],xlim=c(0,1))
hist(rbeta(10^4,5,7),xlim=c(0,1))

# order statistics
# generate order statistics of 10 i.i.d. N(0,1) r.v.s
sort(rnorm(10)) 

# generate 10^4 replications of the 9th order statistic of 10 i.i.d. N(0,1) r.v.s
order_stats <- replicate(10^4, sort(rnorm(10)))
x9 <- order_stats[9,]
hist(x9)


#############
# CHAPTER 9 #
#############

# bidding on unknown asset
b <- 0.6 			# choose your bid
nsim <- 10^5
v <- runif(nsim)
mean(v[b > 2/3*v]-b) # no matter what, you lose money

# time until HH versus HT, using stringr package
r <- replicate(10^3,paste(sample(c("H","T"),100,replace=T),collapse=""))
t <- str_locate(r,"HH")
mean(t[,2])

# linear regression
x <- rnorm(100)
y <- 3 + 5*x + rnorm(100)
b <- cov(x,y) / var(x)
a <- mean(y) - b*mean(x)
plot(x,y)
abline(a=a,b=b)

##############
# CHAPTER 10 #
##############

# Jensen's
x <- rexp(1000)
log(mean(x))
mean(log(x))

# running proportion of Heads
n <- 300
p <- 1/2
x <- rbinom(n,1,p)
xbar <- cumsum(x)/(1:n) # element by element division!
plot(xbar,type='l',ylim=c(0,1))

# Monte Carlo estimate of pi
# plot
pdf("montecarlopi.pdf",width=4,height=4,pointsize=10)
x <- y <- seq(-1.5,1.5,length=1000)
z <- outer(x,y,function(x,y) x^2+y^2-1)
par(mar=c(2,2,2,2))
contour(x,y,z,level=0,drawlabels=F,xlim=c(-1,1),ylim=c(-1,1),xaxs='i',yaxs='i',lwd=3)
# actual code
nsim <- 10^6
x <- runif(nsim,-1,1)
y <- runif(nsim,-1,1)
points(x[1:1000],y[1:1000],pch=16,cex=0.5)
4*sum(x^2+y^2<1)/nsim
dev.off()

# visualizing the central limit theorem
nsim <- 10^4
n <- 12
x <- matrix(runif(n*nsim), nrow=nsim, ncol=n)
xbar <- rowMeans(x)
hist(xbar)

# animating the central limit theorem with the animation package
ani.options(interval = 0.03, nmax = 213)
quincunx()

# Chi-Square and Student-t distributions
dchisq(2,5)
pchisq(2,5)
rchisq(10,5)
dt(4,3)
pt(4,3)
rt(1,3)

##############
# CHAPTER 11 #
##############

# matrix calculations
Q <- matrix(c(1/3,1/3,1/3,0,
              0,0,1/2,1/2,
              0,1,0,0,
              1/2,0,0,1/2),nrow=4,ncol=4,byrow=TRUE)
Q2 <- Q %*% Q
Q3 <- Q2 %*% Q
Q4 <- Q2 %*% Q2
Q5 <- Q3 %*% Q2
Q5[3,4]
eigen(t(Q))

# simulate from random walk on the integers
N <- 10				# absorbing state
p <- 1/2
nsim <- 80
x <- rep(0,nsim) 	# preallocate a vector of 0s to fill up
x[1] <- 5			# initialize
for (i in 2:nsim){	# explain what a for-loop does! and an if statement!
	x[i] <- sample(x[i-1]+c(1,-1), 1, prob=c(p,1-p))
}
plot(x,type='l')

# simulate from the gambler's ruin Markov chain
N <- 10				# absorbing state
p <- 1/2
nsim <- 80
x <- rep(0,nsim) 	# preallocate a vector of 0s to fill up
x[1] <- 5			# initialize
for (i in 2:nsim){	# explain what a for-loop does! and an if statement!
	if (x[i-1]==0 | x[i-1]==N){
		x[i] <- x[i-1]
	}
	else{
		x[i] <- sample(x[i-1]+c(1,-1), 1, prob=c(p,1-p))
	}
}
plot(x,type='l')

# simulate from arbitrary Markov chain with finite state space
Q <- matrix(c(1/3,1/3,1/3,0,
			0,0,1/2,1/2,
			0,1,0,0,
			1/2,0,0,1/2),nrow=4,ncol=4,byrow=TRUE)
M <- nrow(Q)
nsim <- 10^4
x <- rep(0,nsim)
x[1] <- sample(1:M,1)
for (i in 2:nsim){
    x[i] <- sample(M, 1, prob=Q[x[i-1],])
}
x <- x[-(1:(nsim/2))]
table(x)/length(x)


##############
# CHAPTER 12 #
##############

# Metropolis-Hastings
y <- 3
sigma <- 1
mu <- 0
tau <- 2
d <- 1
niter <- 10^4
theta <- rep(0,niter)

theta[1] <- y
for (i in 2:niter){
    theta.p <- theta[i-1] + rnorm(1,0,d)
    r <- dnorm(y,theta.p,sigma) * dnorm(theta.p,mu,tau) /
           (dnorm(y,theta[i-1],sigma) * dnorm(theta[i-1],mu,tau))
    flip <- rbinom(1,1,min(r,1))
    theta[i] <- if(flip==1) theta.p else theta[i-1]
}
theta <- theta[-(1:(niter/2))]
hist(theta)
mean(theta)
var(theta)

# Gibbs sampler
x <- 7
lambda <- 10
a <- 1
b <- 1
niter <- 10^4
p <- rep(0,niter)
N <- rep(0,niter)

p[1] <- 0.5
N[1] <- 2*x
for (i in 2:niter){
    p[i] <- rbeta(1,x+a,N[i-1]-x+b)
    N[i] <- x + rpois(1,lambda*(1-p[i-1]))
}
p <- p[-(1:(niter/2))]
N <- N[-1:(niter/2))]
hist(p)
mean(p)
var(p)
median(p)

##############
# CHAPTER 13 #
##############

# arrivals from 1D Poisson process
L <- 5 # generate on (0,L]
lambda <- 10 # rate of the process
n <- rpois(1,lambda*L)
t <- sort(runif(n,0,L))
plot(t,1:n,type="s")  # staircase plot

# thinning of a Poisson process
p <- 0.3
y <- rbinom(n,1,p)
t1 <- t[y==1]
t2 <- t[y==0]

# 2D Poisson process
L <- 5
lambda <- 10
n <- rpois(1,lambda*L^2)
x <- runif(n,0,L)
y <- runif(n,0,L)
plot(x,y,pch=4)