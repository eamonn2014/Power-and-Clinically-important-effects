

# mu1 <- null# sample$mu1
# mu2 <- mu2
# sd1 <- sd2<- sd
# alpha <- .05
# beta <- .1
# 
# muDiff  <-  mu2-mu1                  # true difference in means
# 
# crit1 <- qnorm(1-as.numeric(alpha/2))
# 
# power.t.test(  delta = mu2, sd = sd, sig.level = null.05,
#              power = .9,
#              type = c("two.sample" ),
#              alternative = c("two.sided"),
#              strict = FALSE, tol = .Machine$double.eps^null.25)
# 
# pow <- pwr::pwr.t.test(d=(mu2-mu1)/sd1 ,power=1-beta, sig.level=as.numeric(alpha), type="two.sample",
#                        alternative="two.sided")
# 
# 
# n1 <- n2 <-(2*(crit1 + qnorm(1-beta) )^2 ) / ((muDiff)/sd1)^2 
# 
# #n1 <- n2 <- pow$n
#  
# #n1 <- ceiling(n1)
# 
# 
# sigDiff <- sqrt((sd1^2/n1) + (sd2^2/n2))
# 
# # pow <- pwr::pwr.t.test(d=(mu2-mu1)/sd1 ,power=1-beta, sig.level=as.numeric(alpha), type="two.sample",
# #                        alternative="two.sided")
# 
# se1 <- se2 <- sigDiff   # pooled se
# 
# crit <- mu1 + crit1 * se1
# 
# #U <- null +  sigDiff *  crit1  #
# 
# 
# ##this wont change
# 
# 1-pnorm(qnorm(1-beta) + qnorm(1-alpha/2))
# 
# qnorm(1-alpha/2)/ (qnorm(1-beta) + qnorm(1-alpha/2))
# 
# ##
# 
# ab <- qnorm(1-beta) + qnorm(1-alpha/2)
# 
# mu1 + ab *se1
# mu1 + qnorm(1-alpha/2) *se1


# check result via simulation~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# first tab results
sd=20
# I confirm check changing sd only changes N that is required, which ic intuitive as both trt and ctrl have same SD
mu2=5
sims=50000
beta=.1
alpha=.05
null=0
pow<-pwr::pwr.t.test(d=(mu2-null)/sd ,power=1-beta, sig.level=as.numeric(alpha), type="two.sample", alternative="two.sided")
n <- pow$n
n

# proportion of times get a certain pvalue or less
mean(replicate(sims, t.test(rnorm(n,null,sd),rnorm(n,mu2,sd), conf.level=.95, alternative="two.sided") $p.value)<0.05)    #90% power
mean(replicate(sims, t.test(rnorm(n,null,sd),rnorm(n,mu2,sd), conf.level=.95, alternative="two.sided") $p.value)<0.0012)  #50% power

# median pvalue I get
median(replicate(sims, t.test(rnorm(n,null,sd),rnorm(n,mu2,sd), conf.level=.95, alternative="two.sided") $p.value))     #p=~0.0012
 
#change what the true result is and see median pvalue we get
median(replicate(sims, t.test(rnorm(n,null,sd),rnorm(n,3.023,sd), conf.level=.95, alternative="two.sided") $p.value)) #p=~0.05
median(replicate(sims, t.test(rnorm(n,null,sd),rnorm(n,3.973,sd), conf.level=.95, alternative="two.sided") $p.value)) #p=~0.01
median(replicate(sims, t.test(rnorm(n,null,sd),rnorm(n,5.076,sd), conf.level=.95, alternative="two.sided") $p.value)) #p=~0.001

# first tab results
sd=20
# I confirm check changing sd only changes N that is required, which ic intuitive as both trt and ctrl have same SD
fact=2
mu2=5*fact ##changing mean difference by a factor, changes the effect by a factor, see below
sims=10000
beta=.1
alpha=.05
null=0
pow<-pwr::pwr.t.test(d=(mu2-null)/sd ,power=1-beta, sig.level=as.numeric(alpha), type="two.sample", alternative="two.sided")
n <- pow$n
n

# proportion of times get a certain pvalue or less
mean(replicate(sims, t.test(rnorm(n,null,sd),rnorm(n,mu2,sd), conf.level=.95, alternative="two.sided") $p.value)<0.05)    #90% power
mean(replicate(sims, t.test(rnorm(n,null,sd),rnorm(n,mu2,sd), conf.level=.95, alternative="two.sided") $p.value)<0.0012)  #50% power

# median pvalue I get
median(replicate(sims, t.test(rnorm(n,null,sd),rnorm(n,mu2,sd), conf.level=.95, alternative="two.sided") $p.value))     #p=~0.0012

#change what the true result is and see median pvalue we get
median(replicate(sims, t.test(rnorm(n,null,sd),rnorm(n,3.023*fact,sd), conf.level=.95, alternative="two.sided") $p.value)) #p=~0.05
median(replicate(sims, t.test(rnorm(n,null,sd),rnorm(n,3.973*fact,sd), conf.level=.95, alternative="two.sided") $p.value)) #p=~0.01
median(replicate(sims, t.test(rnorm(n,null,sd),rnorm(n,5.076*fact,sd), conf.level=.95, alternative="two.sided") $p.value)) #p=~0.001

###############################################################
# first tab results
 
x <- replicate(sims, t.test(rnorm(n,null,sd),rnorm(n,mu2,sd), conf.level=.95, alternative="two.sided"))
mean(x["p.value",]<0.05)
median(unlist(x["p.value",]))
