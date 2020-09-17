 

###################################################################################################
# take homes tab
rm(list=ls())
p2 <- function(x) {formatC(x, format="f", digits=2)}


std1=10

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
mu1 <- 0 
mu2x <- 1
muDiff  <-  mu2x-mu1                  # true difference in means

alpha <- A <-  .05 # alpha
beta  <- B <-  .2  # beta

#scenario 2
alpha2 <- A2 <-  .05 # alpha
beta2  <- B2 <-  .01  # beta

crit1 <- qnorm(1-as.numeric(alpha/2))


pow <-  pwr.t.test(d=(muDiff)/std1 ,power=1-beta, sig.level=as.numeric(alpha), type="two.sample",
                   alternative="two.sided")

n <- ceiling(pow$n)

# simulate to show
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

# end of simulation

 

 par(mfrow=c(2,1))

se <- sqrt(std1^2/n + std1^2/n)
crit2 <- mu1 + crit1 * se ##################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
pow2 <-  pwr.t.test(d=(muDiff)/std1 ,power=1-beta2, sig.level=as.numeric(alpha2), type="two.sample",
                   alternative="two.sided")
n2 <- ceiling(pow2$n)
se2 <- sqrt(std1^2/n2 + std1^2/n2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# scenario 1
sd<- sd2 <-se
#limz <- sd <- se
 

#pvalue2<-  0.05

a <- 1-A/2
b <- 1-B


mu2 <- qnorm(a)*sd+qnorm(b)*sd

u975 <-qnorm(a)*(sd)
cex1 <- 1
cex2 <-.6
lower = -6*sd
upper = 9*sd
x <- seq(-6*sd, 7*sd, 0.1)


num <- dnorm(x, mean =0, sd= 1)
y <- dnorm(x, mean =0, sd= sd)
fact <- 1/(max(num)/max(y))

se.for.plot <- ifelse(se > se2, se, se2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


curve(dnorm(x, mean =0, sd= sd),  xaxs="i", from = -5*se.for.plot , to = 8*se.for.plot , # xlim=c(-4*sd,7*sd), 
      bty="n",yaxt="n",lwd=2, # xaxt="n", 
      col='red',
      ylab='',xlab='Treatment Effect', 
      #axis(side = 1, at = c(10, 50, 100))
      main=paste("Figure 1: Sampling distribution of the null",mu1,"and alt treatment effects",mu2x,"(mean difference between 
                  the two randomised groups) pop sd=",std1,", se=",p2(sd), ", alpha=",A, ", power=",1-B,", N=",n*2,sep=" "))         


curve(dnorm(x, mean =  mu2, sd=sd), lwd=2, add=TRUE , xlab='Treatment Effect', col="forestgreen")

# abline(v= u975,          col="blue",  lwd=1,  lty=3) 
# abline(v= mu2, col="blue",  lwd=1,  lty=3)
# abline(v=0,                  col="blue", lwd=.5, lty=3)  

gap=0.001

xx <-    seq(  qnorm(a)*sd, upper,  by=gap)
xxx <-    seq(  lower, qnorm(a)*sd,  by=gap)
xxxx <-    seq(  lower, -qnorm(a)*sd,  by=gap)

polygon(x = c(qnorm(a)*sd,                           xx,  upper),
        y = c(0 , dnorm(mean=mu2x, sd=sd,     xx),     0),
        col="lightgreen")


polygon(x = c(qnorm(a)*sd,                       xx,  upper),
        y = c(0 , dnorm(mean=mu1, sd=sd,     xx),     0),
        col="red")

polygon(x = c(lower,                       xxxx,  -qnorm(a)*sd),
        y = c(0 , dnorm(mean=mu1, sd=sd,     xxxx),     0),
        col="red")
#type II error area, beta
polygon(x = c(lower,                           xxx,  qnorm(a)*sd),
        y = c(0 , dnorm(mean=mu2x, sd=sd,      xxx),    0),
        col="forestgreen")


abline(v= u975,          col="blue",  lwd=1,  lty=3) 
abline(v= mu2, col="blue",  lwd=1,  lty=3)
abline(v=0,                  col="blue", lwd=.5, lty=3)  

# +/-se
# text(x=0,y=.26*fact,  labels=paste0("se =  +/- ",sd,""),cex= cex1)
# arrows( 0, .24*fact,  sd, .24*fact, col = 1:3, code=2)
# arrows( 0, .24*fact, -sd, .24*fact, col = 1:3)
# 
# # typeI
# arrows(0, .3*fact, u975, .3*fact, col = "red")
# text(x=u975/2, y=.32*fact,  labels=paste0(" ", p2(u975), "se"),cex= cex1)
# 
# # typeII
# arrows(qnorm(a)*sd+qnorm(b)*sd, .26*fact, qnorm(a)*sd, .26*fact, col = "forestgreen", lwd=1.5)
# text(x= (sd*qnorm(a)+sd*qnorm(b)/2), y=.24*fact,  labels=paste0(" ", p2(qnorm(b)*sd), "se"),cex= cex1)
# 
# # total
# arrows( 0, .35*fact, sd*qnorm(a)+qnorm(b)*sd, .35*fact, col = 1:3)
# text(x=   (sd*qnorm(a)+sd*qnorm(b))/2 , y=.36*fact,  labels=paste0(" ", 
#                                                                    p2(sd*qnorm(a)+qnorm(b)*sd), "se"),cex= cex1)

# +/-se
text(x=0,y=.26*fact,  labels=paste0("SE =  +/- ",p2(sd),""),cex= cex2)
arrows( 0, .24*fact,  sd, .24*fact, col = 1:3, code=2)
arrows( 0, .24*fact, -sd, .24*fact, col = 1:3)

# typeI
arrows(0, .3*fact, u975, .3*fact, col = "red")
text(x=u975/2, y=.32*fact,  labels=paste0( p2(qnorm(a))   ,"xSE= ", p2(u975), ""),cex= cex2)

# typeII
arrows(qnorm(a)*sd+qnorm(b)*sd, .26*fact, qnorm(a)*sd, .26*fact, col = "forestgreen", lwd=1.5)
text(x= (sd*qnorm(a)+sd*qnorm(b)/2), y=.24*fact,  labels=paste0(p2(qnorm(b)),"xSE= ", p2(qnorm(b)*sd), ""),cex= cex2)

# total
arrows( 0, .35*fact, sd*qnorm(a)+qnorm(b)*sd, .35*fact, col = 1:3)
text(x=   (sd*qnorm(a)+sd*qnorm(b))/2 , y=.36*fact,  labels=paste0(p2(qnorm(a)+qnorm(b)),"xSE= ", 
                                                                   p2(sd*qnorm(a)+qnorm(b)*sd), ""),cex= cex2)

legend(x=-5*se.for.plot , .4*fact ,  "Legend:",
       legend=c(
          expression(paste("Power (1-",beta,")")),
         expression(paste("Type II error (",beta,")   ")),
         expression(paste("Type I error (",alpha,")"))),
       fill=c("green","forestgreen","red"),
       cex=.7)


#zz <- qnorm(1- pvalue2/2)*sd
# points(zz,      0, col="orange",     pch=4, cex=3, lwd=3) 
# points(-zz,      0, col="orange",     pch=4, cex=3, lwd=3) 

###################################################################################################
###################################################################################################


sd <- se2
A <-  A2
B <-  B2
#PV<-  input$PV
pvalue2<-  0.05

a <- 1-A/2
b <- 1-B

mu2 <- qnorm(a)*sd+qnorm(b)*sd
u975 <- qnorm(a)*(sd)
cex1 <- 1
lower = -6*sd
upper = 9*sd
x <- seq(-6*sd, 7*sd, 0.1)


num <- dnorm(x, mean =0, sd= 1)
y <- dnorm(x, mean =0, sd= sd)
fact <- 1/(max(num)/max(y))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
curve(dnorm(x, mean =0, sd= sd),    xaxs="i",from = -5*se.for.plot , to = 8*se.for.plot ,
      bty="n",yaxt="n",lwd=2, # xaxt="n", 
      col='red',
      ylab='',xlab='Treatment Effect', 
      main=paste("Figure 1a: Sampling distribution of the null",mu1,"and alt treatment effects",mu2x,"(mean difference between 
                  the two randomised groups) pop sd=",std1,", se=",p2(se2), ", alpha=",A2, ", power=",1-B,", N=",n2*2,sep=" "))         



curve(dnorm(x, mean =  mu2, sd=sd), lwd=2, add=TRUE , xlab='Treatment Effect', col="forestgreen")

# abline(v= u975,          col="blue",  lwd=1,  lty=3)
# abline(v= mu2, col="blue",  lwd=1,  lty=3)
# abline(v=0,                  col="blue", lwd=.5, lty=3)  

gap=0.001

xx <-    seq(  qnorm(a)*sd, upper,  by=gap)
xxx <-    seq(  lower, qnorm(a)*sd,  by=gap)
xxxx <-    seq(  lower, -qnorm(a)*sd,  by=gap)

polygon(x = c(qnorm(a)*sd,                           xx,  upper),
        y = c(0 , dnorm(mean=mu2x, sd=sd,     xx),     0),
        col="lightgreen")

polygon(x = c(qnorm(a)*sd,                       xx,  upper),
        y = c(0 , dnorm(mean=mu1, sd=sd,     xx),     0),
        col="red")

polygon(x = c(lower,                       xxxx,  -qnorm(a)*sd),
        y = c(0 , dnorm(mean=mu1, sd=sd,     xxxx),     0),
        col="red")

#type II error area, beta
polygon(x = c(lower,                           xxx,  qnorm(a)*sd),
        y = c(0 , dnorm(mean=mu2x, sd=sd,      xxx),    0),
        col="forestgreen")

abline(v= u975,          col="blue",  lwd=1,  lty=3)
abline(v= mu2, col="blue",  lwd=1,  lty=3)
abline(v=0,                  col="blue", lwd=.5, lty=3)  






# +/-se
text(x=0,y=.26*fact,  labels=paste0("SE =  +/- ",p2(sd),""),cex= cex2)
arrows( 0, .24*fact,  sd, .24*fact, col = 1:3, code=2)
arrows( 0, .24*fact, -sd, .24*fact, col = 1:3)



# typeI
arrows(0, .3*fact, u975, .3*fact, col = "red")
text(x=u975/2, y=.32*fact,  labels=paste0( p2(qnorm(a)),"xSE= ", p2(u975), " "),cex= cex2)  #se

# typeII
arrows(qnorm(a)*sd+qnorm(b)*sd, .26*fact, qnorm(a)*sd, .26*fact, col = "forestgreen", lwd=1.5)
text(x= (sd*qnorm(a)+sd*qnorm(b)/2), y=.24*fact,  labels=paste0(p2(qnorm(b)),"xSE=", p2(qnorm(b)*sd), ""),cex= cex2) #se

# total
arrows( 0, .35*fact, sd*qnorm(a)+qnorm(b)*sd, .35*fact, col = 1:3)
text(x=   (sd*qnorm(a)+sd*qnorm(b))/2 , y=.36*fact,  labels=paste0(p2(qnorm(a)+qnorm(b)),"xSE= ",
                                                                   p2(sd*qnorm(a)+qnorm(b)*sd), ""),cex= cex2) #se

#zz <- qnorm(1- pvalue2/2)*sd

legend(x=-5*se.for.plot , .4*fact,  "Legend:",
       legend=c(
         expression(paste("Power (1-",beta,")")),
         expression(paste("Type II error (",beta,")   ")),
         expression(paste("Type I error (",alpha,")"))),
       fill=c("green","forestgreen","red"),
       cex=.7)

# points(zz,      0, col="orange",     pch=4, cex=3, lwd=3) 
# points(-zz,      0, col="orange",     pch=4, cex=3, lwd=3) 
par(mfrow=c(1,1))
###################################################################################################

