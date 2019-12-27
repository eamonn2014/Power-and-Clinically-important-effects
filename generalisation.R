
 
alpha <- .05
a <- 1-alpha/2
b <- 0.9
sd <- 1#sample(1:10,1)
mu2 <- qnorm(a)*sd+qnorm(b)*sd
u975 <- qnorm(a)(sd)

x <- seq(-6*sd, 7*sd, 0.1)


num <- dnorm(x, mean =0, sd= 1)
y <- dnorm(x, mean =0, sd= sd)
fact <- 1/(max(num)/max(y))


curve(dnorm(x, mean =0, sd= sd), xlim=c(-4*sd,7*sd),
      bty="n",yaxt="n",lwd=2, # xaxt="n", 
      col='red',
      ylab='',xlab='Observed Treatment Effect', 
      main=paste0("           "))

curve(dnorm(x, mean =  mu2, sd=sd), lwd=2, add=TRUE , xlab='Observed Treatment Effect', col="green")

abline(v= u975*sd,          col="blue",  lwd=1,  lty=3)
abline(v= mu2, col="blue",  lwd=1,  lty=3)
abline(v=0,                  col="blue", lwd=.5, lty=3)  
 
# +/-se
text(x=0,y=.26*fact,  labels=paste0("SE= +/- ",sd,""),cex=.7)
arrows( 0, .25*fact,  sd, .25*fact, col = 1:3, code=2)
arrows( 0, .25*fact, -sd, .25*fact, col = 1:3)

# typeI
arrows(0, .1*fact, u975*sd, .1*fact, col = 1:3)
text(x=sd*u975/2,y=.112*fact,  labels=paste0(" ", p2(u975*sd), "se"),cex=.7)

# typeII
arrows(qnorm(a)*sd+qnorm(b)*sd, .09*fact, qnorm(a)*sd, .09*fact, col = 1:3)
text(x= (sd*qnorm(a)+sd*qnorm(b)/2), y=.10*fact,  labels=paste0(" ", p2(qnorm(b)*sd), "se"),cex=.7)

# total
arrows( 0, .15*fact, sd*qnorm(a)+qnorm(b)*sd, .15*fact, col = 1:3)
text(x=   (sd*qnorm(a)+sd*qnorm(b))/2 , y=.16*fact,  labels=paste0(" ", p2(sd*qnorm(a)+qnorm(b)*sd), "se"),cex=.7)
     
     
1- pnorm(qnorm(a)+qnorm(b))
qnorm(a)/ (qnorm(a)+qnorm(b))