 
 # sort mu1 > mu2x 

###################################################################################################
# take homes tab
rm(list=ls())
require(pwr)
p2 <- function(x) {formatC(x, format="f", digits=2)}

std1=10  # pop sd

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
mu1 <- 0
mu2x <- 1
muDiff  <-  mu2x-mu1                  # true difference in means



  

alpha <- A <-  .05 # alpha
beta  <- B <-  .2 # beta

#scenario 2
alpha2 <- A2 <-  .05 # alpha
beta2  <- B2 <-  .2  # beta

crit1 <- qnorm(1-as.numeric(alpha/2))

pow <-  pwr.t.test(d=(muDiff)/std1 ,power=1-beta, 
                   sig.level=as.numeric(alpha), type="two.sample",
                   alternative="two.sided")

n <- ceiling(pow$n)
se <- sqrt(std1^2/n + std1^2/n)
crit2 <- mu1 + crit1 * se  
# simulate to show~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x1 <- rnorm(n, 10,std1)
y1 <- rnorm(n, 11,std1)
plot(density(x1),type="l",col="red", xlab="", ylab="", main="")
lines(density(y1),col="green")

simulations <- replicate(1000, 
                         
                  t.test(x= rnorm(n, 10,std1), 
                         y= rnorm(n, 11,std1), 
                           paired=FALSE),
                           simplify=FALSE)

 table(sapply(simulations, "[[", "p.value") < .05)/1000


# end of simulation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# back to main code

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
pow2 <-  pwr.t.test(d=(muDiff)/std1 ,power=1-beta2, sig.level=as.numeric(alpha2), type="two.sample",
                   alternative="two.sided")
n2 <- ceiling(pow2$n)
se2 <- sqrt(std1^2/n2 + std1^2/n2)



if (muDiff > 0) {

###################################################################################################
###################################################################################################
###################################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
par(mfrow=c(2,1))
# scenario 1
#sd<- sd2 <-se

a <- 1-A/2
b <- 1-B


mu2 <- qnorm(a)*se+qnorm(b)*se
u975 <-qnorm(a)*(se)
cex1 <- 1
cex2 <-.6
lower = -6*se
upper = 9*se
x <- seq(-6*se, 7*se, 0.01)
num <- dnorm(x, mean =0, sd= 1)
y <-   dnorm(x, mean =0, sd= se)
fact <- 1/(max(num)/max(y))              # helps with plotting
se.for.plot <- ifelse(se > se2, se, se2) # helps with plotting

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
curve(dnorm(x, mean =0, sd= se),  xaxs="i", from = -5*se.for.plot , to = 8*se.for.plot , # xlim=c(-4*sd,7*sd), 
      bty="n",yaxt="n",lwd=2, # xaxt="n", 
      col='red',
      ylab='',xlab='Treatment Effect', 
      #axis(side = 1, at = c(10, 50, 100))
      main=paste("Figure 1: Sampling distribution of the null",mu1,"and alt treatment effects",muDiff,"(mean difference between 
                  the two randomised groups) pop sd=",std1,", se=",p2(se), ", alpha=",A, ", power=",1-B,", N.tot=",n*2,sep=" "))         

curve(dnorm(x, mean =  mu2, sd=se), lwd=2, add=TRUE , xlab='Treatment Effect', col="forestgreen")

gap=0.0001

xx <-      seq(  qnorm(a)*se, upper,   by=gap)
xxx <-     seq(  lower, qnorm(a)*se,   by=gap)
xxxx <-    seq(  lower, -qnorm(a)*se,  by=gap)

# power
polygon(x = c(qnorm(a)*se,                           xx,  upper),
        y = c(0 , dnorm(mean=mu2x, sd=se,     xx),     0),
        col="lightgreen")

# type I error
polygon(x = c(qnorm(a)*se,                       xx,  upper),
        y = c(0 , dnorm(mean=mu1, sd=se,     xx),     0),
        col="red")

# type I error
polygon(x = c(lower,                       xxxx,  -qnorm(a)*se),
        y = c(0 , dnorm(mean=mu1, sd=se,     xxxx),     0),
        col="red")

#type II error area, beta
polygon(x = c(lower,                           xxx,  qnorm(a)*se),
        y = c(0 , dnorm(mean=mu2x, sd=se,      xxx),    0),
        col="forestgreen")


abline(v= u975,              col="blue",  lwd=1,  lty=3) 
abline(v= mu2,               col="blue",  lwd=1,  lty=3)
abline(v= 0,                 col="blue",  lwd=.5,  lty=3)  

# +/-se
text(x=0,y=.26*fact,  labels=paste0("SE =  +/- ",p2(se),""),cex= cex2)
arrows( 0, .24*fact,  se, .24*fact, col = 1:3, code=2)
arrows( 0, .24*fact, -se, .24*fact, col = 1:3)

# typeI
arrows(0, .3*fact, u975, .3*fact, col = "red")
text(x=u975/2, y=.32*fact,  labels=paste0( p2(qnorm(a))   ,"xSE= ", p2(u975), ""),cex= cex2)

# typeII
if (b<0.5) { 
  # typeII
  arrows(qnorm(a)*se, .2*fact,              qnorm(a)*se+qnorm(b)*se, .2*fact, col = "blue", lwd=1.5)
} else {
  arrows(qnorm(a)*se+qnorm(b)*se, .2*fact, qnorm(a)*se,              .2*fact, col = "forestgreen", lwd=1.5)
}
text(x= (se*qnorm(a)+se*qnorm(b)/2), y=.18*fact,  labels=paste0(p2(qnorm(b)),"xSE=", p2(qnorm(b)*se), ""),cex= cex2) #se


# total
arrows( 0, .35*fact, se*qnorm(a)+qnorm(b)*se, .35*fact, col = 1:3)
text(x=   (se*qnorm(a)+se*qnorm(b))/2 , y=.36*fact,  labels=paste0(p2(qnorm(a)+qnorm(b)),"xSE= ", 
                                                                   p2(se*qnorm(a)+qnorm(b)*se), ""),cex= cex2)

legend(x=-5*se.for.plot , .4*fact ,  "Legend:",
       legend=c(
          expression(paste("Power (1-",beta,")")),
         expression(paste("Type II error (",beta,")   ")),
         expression(paste("Type I error (",alpha,")"))),
       fill=c("green","forestgreen","red"),
       cex=.7, bty = "n")
 

###################################################################################################
###################################################################################################
###################################################################################################


A <-  A2
B <-  B2

a <- 1-A/2
b <- 1-B

mu2 <-  qnorm(a)*se2+qnorm(b)*se2
u975 <- qnorm(a)*(se2)
cex1 <- 1
lower = -6*se2
upper =  9*se2
x <- seq(-6*se2, 7*se2, 0.1)

num <- dnorm(x, mean =0, sd= 1)
y <-   dnorm(x, mean =0, sd= se2)
fact <- 1/(max(num)/max(y))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
curve(dnorm(x, mean =0, sd= se2),    xaxs="i",from = -5*se.for.plot , to = 8*se.for.plot ,
      bty="n",yaxt="n",lwd=2, # xaxt="n", 
      col='red',
      ylab='',xlab='Treatment Effect', 
      main=paste("Figure 1a: Sampling distribution of the null",mu1,"and alt treatment effects",muDiff,"(mean difference between 
                  the two randomised groups) pop sd=",std1,", se=",p2(se2), ", alpha=",A2, ", power=",1-B,", N.tot=",n2*2,sep=" "))         



curve(dnorm(x, mean =  mu2, sd=se2), lwd=2, add=TRUE , xlab='Treatment Effect', col="forestgreen")
 

gap=0.001

xx <-      seq(  qnorm(a)*se2, upper,   by=gap)
xxx <-     seq(  lower, qnorm(a)*se2,   by=gap)
xxxx <-    seq(  lower, -qnorm(a)*se2,  by=gap)

polygon(x = c(qnorm(a)*se2,                           xx,  upper),
        y = c(0 , dnorm(mean=mu2x, sd=se2,     xx),     0),
        col="lightgreen")

polygon(x = c(qnorm(a)*se2,                       xx,  upper),
        y = c(0 , dnorm(mean=mu1, sd=se2,     xx),     0),
        col="red")

polygon(x = c(lower,                       xxxx,  -qnorm(a)*se2),
        y = c(0 , dnorm(mean=mu1, sd=se2,     xxxx),     0),
        col="red")

#type II error area, beta
polygon(x = c(lower,                           xxx,  qnorm(a)*se2),
        y = c(0 , dnorm(mean=mu2x, sd=se2,      xxx),    0),
        col="forestgreen")

abline(v= u975,          col="blue",  lwd=1,  lty=3)
abline(v= mu2, col="blue",  lwd=1,  lty=3)
abline(v=0,                  col="blue", lwd=.5, lty=3)  

# +/-se
text(x=0,y=.26*fact,  labels=paste0("SE =  +/- ",p2(se2),""),cex= cex2)
arrows( 0, .24*fact,  se2, .24*fact, col = 1:3, code=2)
arrows( 0, .24*fact, -se2, .24*fact, col = 1:3)

# typeI
arrows(0, .3*fact, u975, .3*fact, col = "red")
text(x=u975/2, y=.32*fact,  labels=paste0( p2(qnorm(a)),"xSE= ", p2(u975), " "),cex= cex2)  #se

if (b<0.5) { 
# typeII
  arrows(qnorm(a)*se2, .2*fact,              qnorm(a)*se2+qnorm(b)*se2, .2*fact, col = "blue", lwd=1.5)
} else {
  arrows(qnorm(a)*se2+qnorm(b)*se2, .2*fact, qnorm(a)*se2,              .2*fact, col = "forestgreen", lwd=1.5)
}
  text(x= (se2*qnorm(a)+se2*qnorm(b)/2), y=.18*fact,  labels=paste0(p2(qnorm(b)),"xSE=", p2(qnorm(b)*se2), ""),cex= cex2) #se

# total
arrows( 0, .35*fact, se2*qnorm(a)+qnorm(b)*se2, .35*fact, col = 1:3)
text(x=   (se2*qnorm(a)+se2*qnorm(b))/2 , y=.36*fact,  labels=paste0(p2(qnorm(a)+qnorm(b)),"xSE= ",
                                                                   p2(se2*qnorm(a)+qnorm(b)*se2), ""),cex= cex2) #se

legend(x=-5*se.for.plot , .4*fact,  "Legend:",
       legend=c(
         expression(paste("Power (1-",beta,")")),
         expression(paste("Type II error (",beta,")   ")),
         expression(paste("Type I error (",alpha,")"))),
       fill=c("green","forestgreen","red"),
       cex=.7, bty = "n")
 
par(mfrow=c(1,1))
###################################################################################################
###################################################################################################

} else {
###################################################################################################
  
  
  crit1 <- qnorm(1-as.numeric(alpha/2))
  
  pow <-  pwr.t.test(d=(muDiff)/std1 ,power=1-beta, 
                     sig.level=as.numeric(alpha), type="two.sample",
                     alternative="two.sided")
  
  n <- ceiling(pow$n)
  se <- sqrt(std1^2/n + std1^2/n)
  crit2 <- mu1 + crit1 * se  
  
  #sceanrio 2
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  pow2 <-  pwr.t.test(d=(muDiff)/std1 ,power=1-beta2, sig.level=as.numeric(alpha2), type="two.sample",
                      alternative="two.sided")
  n2 <- ceiling(pow2$n)
  se2 <- sqrt(std1^2/n2 + std1^2/n2)
  
  
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  par(mfrow=c(2,1))
  # scenario 1
  
  a <- 1-A/2
  b <- 1-B
  
  
  mu2 <- -1*(qnorm(a)*se+qnorm(b)*se)  # new
  u975 <-qnorm(a)*(se)
  cex1 <- 1
  cex2 <-.6
  lower = -9*se  # new
  upper = 6*se   # new
  x <- seq(-7*se, 6*se, 0.01)  #new
  num <- dnorm(x, mean =0, sd= 1)
  y <-   dnorm(x, mean =0, sd= se)
  fact <- 1/(max(num)/max(y))              # helps with plotting
  se.for.plot <- ifelse(se > se2, se, se2) # helps with plotting
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  curve(dnorm(x, mean =0, sd= se),  xaxs="i", from = -8*se.for.plot , to = 5*se.for.plot , 
        # xlim=c(-4*sd,7*sd), 
        bty="n",yaxt="n",lwd=2, # xaxt="n", 
        col='red',
        ylab='',xlab='Treatment Effect', 
        #axis(side = 1, at = c(10, 50, 100))
        main=paste("Figure 1: Sampling distribution of the null",mu2x,"and alt treatment effects",muDiff,"(mean difference between 
                  the two randomised groups) pop sd=",std1,", se=",p2(se), ", alpha=",A, ", power=",1-B,", N.tot=",n*2,sep=" "))         
  
  curve(dnorm(x, mean =  mu2, sd=se), lwd=2, add=TRUE , xlab='Treatment Effect', col="forestgreen")
  
  gap=0.0001
  
  xx <-      seq(  qnorm(a)*se, upper,   by=gap)
  xxx <-     seq(  lower, qnorm(a)*se,   by=gap)
  xxxx <-    seq(  lower, -qnorm(a)*se,  by=gap)
  xxxxx <-   seq(  -qnorm(a)*se, upper,   by=gap)
  
  # type I error
  polygon(x = c(qnorm(a)*se,                           xx,  upper),
          y = c(0 , dnorm(mean=mu2x, sd=se,     xx),     0),
          col="red")
  
  # power
  polygon(x = c(lower,                           xxxx, -qnorm(a)*se ),
          y = c(0 , dnorm(mean=muDiff, sd=se,      xxxx),    0),
          col="lightgreen")
  
  # type I error
  polygon(x = c(lower,                      xxxx,  -qnorm(a)*se),
          y = c(0 , dnorm(mean=mu2x, sd=se,     xxxx),     0),
          col="red")
  
  # typeII 
  polygon(x = c(-qnorm(a)*se            ,                xxxxx,  upper),
          y = c(0 , dnorm(mean=muDiff, sd=se,      xxxxx),    0),
          col="forestgreen")
  
  
  abline(v= u975,              col="blue",  lwd=1,  lty=3) 
  abline(v= mu2,               col="blue",  lwd=1,  lty=3)
  abline(v= 0,                 col="blue",  lwd=.5,  lty=3)  
  
  # +/-se
  text(x=0,y=.26*fact,  labels=paste0("SE =  +/- ",p2(se),""),cex= cex2)
  arrows( 0, .24*fact,  se, .24*fact, col = 1:3, code=2)
  arrows( 0, .24*fact, -se, .24*fact, col = 1:3)
  
  # typeI
  arrows(0, .3*fact, -u975, .3*fact, col = "red")
  text(x=-u975/2, y=.32*fact,  labels=paste0( p2(qnorm(a))   ,"xSE= ", p2(u975), ""),cex= cex2)
  
  # typeII
  if (b>0.5) {  
    arrows(muDiff, .20*fact, -qnorm(a)*se, .20*fact,    col = "blue", lwd=1.5)
  } else {
    arrows(muDiff, .20*fact, -qnorm(a)*se, .20*fact,    col = "blue", lwd=1.5)
  }
  
  text(x= -(se*qnorm(a)+se*qnorm(b)/2), y=.18*fact,  
       labels=paste0(p2(qnorm(b)),"xSE= ", p2(qnorm(b)*se), ""),cex= cex2)
  
  
  # total
  arrows( 0, .35*fact,muDiff, .35*fact, col = 1:3)
  text(x=   muDiff/2 , y=.36*fact,  labels=paste0(p2(qnorm(a)+qnorm(b)),"xSE= ", 
                                                  p2(se*qnorm(a)+qnorm(b)*se), ""),cex= cex2)
  
  legend(x=-6*se.for.plot , .35*fact ,  "Legend:",
         legend=c(
           expression(paste("Power (1-",beta,")")),
           expression(paste("Type II error (",beta,")   ")),
           expression(paste("Type I error (",alpha,")"))),
         fill=c("green","forestgreen","red"),
         cex=.7, bty = "n")
  
  
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  se <- se2
  A <- A2
  B <- B2
  n<-n2
  
  # scenario 1
  #sd<- sd2 <-se
  
  a <- 1-A/2
  b <- 1-B
  
  
  mu2 <- -1*(qnorm(a)*se+qnorm(b)*se)  # new
  u975 <-qnorm(a)*(se)
  cex1 <- 1
  cex2 <-.6
  lower = -9*se  # new
  upper = 6*se   # new
  x <- seq(-7*se, 6*se, 0.01)  #new
  num <- dnorm(x, mean =0, sd= 1)
  y <-   dnorm(x, mean =0, sd= se)
  fact <- 1/(max(num)/max(y))              # helps with plotting
  #se.for.plot <- ifelse(se > se2, se, se2) # helps with plotting
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  curve(dnorm(x, mean =0, sd= se),  xaxs="i", from = -8*se.for.plot , to = 5*se.for.plot , 
        # xlim=c(-4*sd,7*sd), 
        bty="n",yaxt="n",lwd=2, # xaxt="n", 
        col='red',
        ylab='',xlab='Treatment Effect', 
        #axis(side = 1, at = c(10, 50, 100))
        main=paste("Figure 1: Sampling distribution of the null",mu2x,"and alt treatment effects",muDiff,"(mean difference between 
                  the two randomised groups) pop sd=",std1,", se=",p2(se), ", alpha=",A, ", power=",1-B,", N.tot=",n*2,sep=" "))         
  
  curve(dnorm(x, mean =  mu2, sd=se), lwd=2, add=TRUE , xlab='Treatment Effect', col="forestgreen")
  
  gap=0.0001
  
  xx <-      seq(  qnorm(a)*se, upper,   by=gap)
  xxx <-     seq(  lower, qnorm(a)*se,   by=gap)
  xxxx <-    seq(  lower, -qnorm(a)*se,  by=gap)
  xxxxx <-    seq(  -qnorm(a)*se, upper,   by=gap)
  
  # type I error
  polygon(x = c(qnorm(a)*se,                           xx,  upper),
          y = c(0 , dnorm(mean=mu2x, sd=se,     xx),     0),
          col="red")
  
  # power
  polygon(x = c(lower,                           xxxx, -qnorm(a)*se ),
          y = c(0 , dnorm(mean=muDiff, sd=se,      xxxx),    0),
          col="lightgreen")
  
  # type I error
  polygon(x = c(lower,                      xxxx,  -qnorm(a)*se),
          y = c(0 , dnorm(mean=mu2x, sd=se,     xxxx),     0),
          col="red")
  
  # typeII 
  polygon(x = c(-qnorm(a)*se            ,                xxxxx,  upper),
          y = c(0 , dnorm(mean=muDiff, sd=se,      xxxxx),    0),
          col="forestgreen")
  
  
  
  abline(v= u975,              col="blue",  lwd=1,  lty=3) 
  abline(v= mu2,               col="blue",  lwd=1,  lty=3)
  abline(v= 0,                 col="blue",  lwd=.5,  lty=3)  
  
  # +/-se
  text(x=0,y=.26*fact,  labels=paste0("SE =  +/- ",p2(se),""),cex= cex2)
  arrows( 0, .24*fact,  se, .24*fact, col = 1:3, code=2)
  arrows( 0, .24*fact, -se, .24*fact, col = 1:3)
  
  # typeI
  arrows(0, .3*fact, -u975, .3*fact, col = "red")
  text(x=-u975/2, y=.32*fact,  labels=paste0( p2(qnorm(a))   ,"xSE= ", p2(u975), ""),cex= cex2)
  
  # typeII
  
  if (b>0.5) {  
    arrows(muDiff, .20*fact, -qnorm(a)*se, .20*fact,    col = "blue", lwd=1.5)
  } else {
    arrows(muDiff, .20*fact, -qnorm(a)*se, .20*fact,    col = "blue", lwd=1.5)
  }
  
  text(x= -(se*qnorm(a)+se*qnorm(b)/2), y=.18*fact,  
       labels=paste0(p2(qnorm(b)),"xSE= ", p2(qnorm(b)*se), ""),cex= cex2)
  
  
  # total
  arrows( 0, .35*fact,muDiff, .35*fact, col = 1:3)
  text(x=   muDiff/2 , y=.36*fact,  labels=paste0(p2(qnorm(a)+qnorm(b)),"xSE= ", 
                                                  p2(se*qnorm(a)+qnorm(b)*se), ""),cex= cex2)
  
  legend(x=-6*se.for.plot , .35*fact ,  "Legend:",
         legend=c(
           expression(paste("Power (1-",beta,")")),
           expression(paste("Type II error (",beta,")   ")),
           expression(paste("Type I error (",alpha,")"))),
         fill=c("green","forestgreen","red"),
         cex=.7, bty = "n")
  par(mfrow=c(1,1))
  }