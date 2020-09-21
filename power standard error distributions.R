###################################################################################################
# 1. First a simple simulation of power for a test 
###################################################################################################  

std1 <- 10
sims <- 1000
x1 <- rnorm(n, 10,std1)
y1 <- rnorm(n, 11,std1)
plot(density(x1),type="l",col="red", xlab="", ylab="", main="")
lines(density(y1),col="green")

simulations <- replicate(sims,

                         t.test(x= rnorm(n, 10,std1),
                                y= rnorm(n, 11,std1),
                                paired=FALSE),
                         simplify=FALSE)

table(sapply(simulations, "[[", "p.value") < .05)/sims

###################################################################################################
# End of simulation of power for a test 
###################################################################################################  

###################################################################################################
# 2.code to present standard error distributions useful for teaching power calculations
###################################################################################################
rm(list=ls())

#require(pwr)
p2 <- function(x) {formatC(x, format="f", digits=2)}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#inputs

std1=sample(6:20,1)  # pop sd

mu1 <-  rnorm(1,100,100)
mu2 <-  mu1+rnorm(1,0, 10)

muDiff  <-  mu2-mu1                  # true difference in means

alpha <- A <-  .05  # alpha
beta  <- B <-  sample(2:9,1)/10  # beta

gap <- 0.001
cex1 <- 1                       # font size
cex2 <-.6   

a <- 1-A/2
b <- 1-B  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# # power calculation
#  pow <-  pwr.t.test(d=(muDiff)/std1 ,power=1-beta,
#                     sig.level=as.numeric(alpha), type="two.sample",
#                     alternative="two.sided")
 
  crit1 <- qnorm(1-as.numeric(alpha/2))
  
  n <-ceiling((2*(crit1 + qnorm(1-beta) )^2 ) / ((muDiff)/std1)^2 )
   
  se <- sqrt(std1^2/n + std1^2/n)  # se of dif
  
  if (muDiff > 0) {
  
  ###################################################################################################
  ###################################################################################################
  ###################################################################################################
  lower = mu1-6*se                         # for plotting
  upper = mu2+6*se                         # for plotting
  x <- seq(-6*se, 6*se, 0.01)              # for dnorm
  num <- dnorm(x, mean =0, sd= 1)          # helps with plotting
  y <-   dnorm(x, mean =0, sd= se)         # helps with plotting
  fact <- 1/(max(num)/max(y))              # helps with plotting
   
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  curve(dnorm(x, mean =mu1, sd= se),  xaxs="i", from = lower , to =upper ,  
        bty="n",yaxt="n",lwd=2, # xaxt="n", 
        col='red',
        ylab='',xlab='Treatment Effect', 

        main=paste("Figure 1: Sampling distribution of the null in red, mean=",p2(mu1),"
        & alternative hypothesised treatment effect",p2(muDiff),"
                  pop sd=",std1,", se=",p2(se), ", alpha=",A, ", power=",1-B,", N total=",n*2,sep=" ")
        , sub="When power > 0.5, Red arrow = alpha; black arrow = alpha + beta; blue = beta")          
  
  curve(dnorm(x, mean =  mu2, sd=se), lwd=2, add=TRUE , xlab='Treatment Effect', col="forestgreen")
  
  xx <-      seq(  mu1+qnorm(a)*se, upper,   by=gap) # null upper limit  -> 
  xxx <-     seq(  lower, mu1+qnorm(a)*se,   by=gap) # -> null upper limit
  xxxx <-    seq(  lower, mu1-qnorm(a)*se,   by=gap) # -> null lower limit
  
  # power
  polygon(x = c(mu1+qnorm(a)*se,                           xx,  upper),
          y = c(0 , dnorm(mean=mu2, sd=se,     xx),     0),
          col="lightgreen")

  # type I error
  polygon(x = c(mu1+qnorm(a)*se,                       xx,  upper),
          y = c(0 , dnorm(mean=mu1, sd=se,     xx),     0),
          col="red")
  
  # type I error
  polygon(x = c(lower,                         xxxx,   mu1-qnorm(a)*se),
          y = c(0 , dnorm(mean=mu1, sd=se,     xxxx),     0),
          col="red")
  
  #type II error area, beta
  polygon(x = c(lower,                           xxx,  mu1+qnorm(a)*se),
          y = c(0 , dnorm(mean=mu2, sd=se,      xxx),    0),
          col="forestgreen")
 
  abline(v= mu1+qnorm(a)*se,   col="blue",  lwd=1,  lty=3)   # meeting of power and alpha level
  abline(v= mu2,               col="blue",  lwd=1,  lty=3)   # alt hyp
  abline(v= mu1,               col="blue",  lwd=1,  lty=3)  # null
  
  #adding arrows
  # +/-se
  text( x=mu1,y=.26*fact,  labels=paste0("SE =  +/- ",p2(se),""),cex= cex2)
  arrows( mu1,  .24*fact,  mu1+se, .24*fact, col = 1:3, code=2)
  arrows( mu1,  .24*fact,  mu1-se, .24*fact, col = 1:3)
  
  # type I
  arrows(mu1, .3*fact, mu1+qnorm(a)*(se), .3*fact, col = "red")
  text(x=mu1+qnorm(a)*(se)/2, y=.32*fact,  
       labels=paste0( p2(qnorm(a)),"xSE= ", p2(qnorm(a)*(se)), ""),cex= cex2)
  
  # typeII
  if (b<0.5) { 
    arrows(mu1+qnorm(a)*(se), .2*fact,   mu2, .2*fact, col = "blue", lwd=1.5)
  } else {
    arrows(mu2, .2*fact,   mu1+qnorm(a)*(se), .2*fact, col = "blue", lwd=1.5)
  }
  text(x= mu2-( +se*qnorm(b))/2, y=.18*fact,  
       labels=paste0(p2(qnorm(b)),"xSE=", p2(qnorm(b)*se), ""),cex= cex2) #se
   
   #total
   arrows( mu1, .35*fact, mu2, .35*fact, col = 1:3)
   text(x=   mu1+(mu2-mu1)/2 , y=.36*fact,  labels=paste0(p2(qnorm(a)+qnorm(b)),"xSE= ", 
                                                                      p2(mu2-mu1), ""),cex= cex2)
  
  legend(x=mu1-5*se , .4*fact ,  "Legend:",
         legend=c(
           expression(paste("Power (1-",beta,")")),
           expression(paste("Type II error (",beta,")   ")),
           expression(paste("Type I error (",alpha,")"))),
         fill=c("green","forestgreen","red"),
         cex=.7, bty = "n")
  ###################################################################################################
  } else {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  upper = mu1+6*se                         # for plotting
  lower = mu2-6*se    
  x <- seq(-7*se, 6*se, 0.01)  
  num <- dnorm(x, mean =0, sd= 1)
  y <-   dnorm(x, mean =0, sd= se)
  fact <- 1/(max(num)/max(y))              # helps with plotting
  se.for.plot <- se                        # helps with plotting
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  curve(dnorm(x, mean =mu1, sd= se),  xaxs="i", from = lower , to = upper , 
        bty="n",yaxt="n",lwd=2, # xaxt="n", 
        col='red',
        ylab='',xlab='Treatment Effect', 
      
        main=paste("Figure 1: Sampling distribution of the null in red, mean=",p2(mu1),"
        & alternative hypothesised treatment effect",p2(muDiff),"
                  pop sd=",std1,", se=",p2(se), ", alpha=",A, ", power=",1-B,", N total=",n*2,sep=" ")
        , sub="When power > 0.5, Red arrow = alpha; black arrow = alpha + beta; blue = beta")          
  
  curve(dnorm(x, mean =  mu2, sd=se), lwd=2, add=TRUE , xlab='Treatment Effect', col="forestgreen")
  
  xx <-      seq(  mu1+qnorm(a)*se, upper,   by=gap) # null upper limit  -> 
  xxx <-     seq(  mu1+qnorm(a)*se, upper,  by=gap) # -> null upper limit
  xxxx <-    seq(  lower, mu1-qnorm(a)*se,   by=gap) # -> null lower limit
  xxxxx <-     seq(  lower, mu1+qnorm(a)*se,   by=gap) # -> null upper limit
  
  # type I error
  polygon(x = c(mu1+qnorm(a)*se,                           xx,  upper),
          y = c(0 , dnorm(mean=mu1, sd=se,     xx),     0),
          col="red")

  #type II error 
  polygon(x = c(lower,                           xxxxx,  mu2+qnorm(a)*se),
          y = c(0 , dnorm(mean=mu2, sd=se,      xxxxx),    0),
          col="forestgreen")
  
  # power
  polygon(x = c(lower,                         xxxx,   mu1-qnorm(a)*se),
          y = c(0 , dnorm(mean=mu2, sd=se,     xxxx),     0),
          col="lightgreen")
  
  # type I error
  polygon(x = c(lower,                         xxxx,   mu1-qnorm(a)*se),
          y = c(0 , dnorm(mean=mu1, sd=se,     xxxx),     0),
          col="red")
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  abline(v= mu1-qnorm(a)*(se), col="blue",  lwd=1,  lty=3) 
  abline(v= mu2,               col="blue",  lwd=1,  lty=3)
  abline(v= mu1,               col="blue",  lwd=1,  lty=3)  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # +/-se
  text(x=mu1,y=.26*fact,  labels=paste0("SE =  +/- ",p2(se),""),cex= cex2)
  arrows( mu1, .24*fact,  mu1+se, .24*fact, col = 1:3, code=2)
  arrows( mu1, .24*fact,  mu1-se, .24*fact, col = 1:3)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # typeI
  arrows(mu1, .3*fact, mu1-qnorm(a)*(se), .3*fact, col = "red")
  text(x=mu1-qnorm(a)*(se)/2, y=.32*fact,  labels=paste0( p2(qnorm(a))   ,
                                                          "xSE= ", p2(qnorm(a)*(se)), ""),cex= cex2)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # type II
  if (b>0.5) {  
    arrows(mu2 , .20*fact, mu1-qnorm(a)*se, .20*fact,    col = "blue", lwd=1.5)
  } else {
    arrows(mu1-qnorm(a)*se , .20*fact, mu2 , .20*fact,    col = "blue", lwd=1.5)
  }
  
  text(x=   mu2+(qnorm(b)*se)/2, y=.18*fact,  
       labels=paste0(p2(qnorm(b)),"xSE= ", p2(qnorm(b)*se), ""),cex= cex2)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # total
  arrows( mu1, .35*fact, mu2, .35*fact, col = 1:3)
  text(x= mu1-  abs(muDiff/2) , y=.36*fact,  labels=paste0(p2(qnorm(a)+qnorm(b)),"xSE= ", 
                                                  p2(se*qnorm(a)+qnorm(b)*se), ""),cex= cex2)
  
  legend(x=mu1-6*se  , .35*fact ,  "Legend:",
         legend=c(
           expression(paste("Power (1-",beta,")")),
           expression(paste("Type II error (",beta,")   ")),
           expression(paste("Type I error (",alpha,")"))),
         fill=c("green","forestgreen","red"),
         cex=.7, bty = "n")
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 