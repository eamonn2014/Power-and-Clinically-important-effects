#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#library(DT)
rm(list=ls())
library(shiny)
#library(nlme)
#library(VCA)
library(shinyWidgets)#
library(pwr)
options(max.print=1000000)
fig.width <- 1200
fig.height <- 450
library(shinythemes)        # more funky looking apps
p0 <- function(x) {formatC(x, format="f", digits=0)}
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
p3 <- function(x) {formatC(x, format="f", digits=3)}
p4 <- function(x) {formatC(x, format="f", digits=4)}
p6 <- function(x) {formatC(x, format="f", digits=6)}
options(width=100)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
                       #Select true population parameters and probability of errors:")),
                      mu2=2# Mean treatment effect under alternative hypothesis",
                      sd1=sd2=25
                      alpha = c(0.05 )  # "Alpha, Type I error",
                      beta=.10 # "Beta, Type II error",
              
               pvalue2=0.05 # "Enter a (two-sided) P-Value and see the associated effect size (orange crosses on x axis of Figure 3):",
    
  # --------------------------------------------------------------------------
 
    #  # N <- input$N
    mu1 <- 0 #input$mu1
 
    
 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
  
  # random.samplex <- reactive({
  #   #
  #   #   # Dummy line to trigger off button-press
  #   foo <- input$resamplex
  #   #
  #   #  # N <- input$N
  #   mu1 <- input$nullx #input$mu1
  #   mu2 <- input$mu2x
  #   sd1 <- input$sdx
  #   sd2 <- input$sdx
  #   alpha <- input$alpha
  #   beta <- input$beta
  #   
  #   return(list(
  #     mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2, alpha=alpha, beta=beta     ))
  # })
  # # 
  # --------------------------------------------------------------------------
  # # Set up the dataset based on the inputs 
  # make.regression <- reactive({
  #   
  #   #   https://stats.stackexchange.com/questions/28876/difference-between-anova-power-simulation-and-power-calculation
  #   
  #   #   sample <- random.sample()
  #   
  #   mu1 <- 0# sample$mu1
  #   mu2 <- input$mu2
  #   sd1 <- input$sd1
  #   sd2 <- input$sd1
  #   alpha <- input$alpha
  #   beta <- input$beta
    
    muDiff  <-  mu2-mu1                  # true difference in means
    
    crit1 <- qnorm(1-as.numeric(alpha/2))
    
    pow <-  pwr.t.test(d=(mu2-mu1)/sd1 ,power=1-beta, sig.level=as.numeric(alpha), type="two.sample",
                       alternative="two.sided")
    
    # this equation from armitage and LSHTM notes seems to give stable 60.5% but not R canned equation above?
    n1 <- n2 <-(2*(crit1 + qnorm(1-beta) )^2 ) / ((muDiff)/sd1)^2 
    
    #n1 <- n2 <- pow$n
    
    x <- seq(mu1-6*sd1, mu2+6*sd2, 0.1)
    
    sigDiff <- sqrt((sd1^2/n1) + (sd2^2/n2))
    
    se1 <- se2 <- sigDiff   # pooled se
    
    crit <- mu1 + crit1 * se1
    
 
  # #---------------------------------------------------------------------------
  # #grab the power
 
 
  # allowing mixing in numeric and text 
  # https://stackoverflow.com/questions/39250200/rshiny-textoutput-and-paragraph-on-same-line
  
 
    
    A <- mu1    # this will be zero
    B <- mu2    # this is an input
    C <- crit   # 0 + crit1 * se1, where crit1 is alpha 2 sided input and se1 the pooled SE
    D <- se1    # pooled SD
    E <- sigDiff #pooled Sd again
    FF <-  mu1 +  sigDiff # 0 + pooled SE
    FF2 <- mu1 -  sigDiff # 0 - pooled SE
    
    L <- mu1 -  sigDiff * crit1  # 0 - pooled SE x user input alpha level 
    U <- mu1 +  sigDiff * crit1  #
    V <- mu2 -  sigDiff * crit1  # mu2 - pooled SE x user input alpha level
    W <- mu2 +  sigDiff * crit1
    
    X <- beta  # user input 
    Y <- alpha # user input 
    N <- n1    # sample size  
    
    if (B>0) {
      
      HTML(paste0( "The distribution in pink, shows the distribution of observed treatment effects when the
    therapy doesn't work (true effect is: "
                   , tags$span(style="color:red", p0(A)) ,
                   "). While the true effect is  "
                   , tags$span(style="color:red", p0(A)) ,
                   ", observed effects anywhere from " 
                   , tags$span(style="color:red", p1(L)) , ## only true for 95% so alpha 0.05 <- change this? add crosses??*********
                   " to  "
                   , tags$span(style="color:red", p1(U)) , ## only true for 95% so alpha 0.05?*********
                   " are quite likely to occur. Getting an observed effect of "
                   , tags$span(style="color:red", p1(FF)) , # 1 SE up from null
                   " (purple cross), for example,  is well within this
                               sampling variability and not conclusive evidence the therapy works.
                               The solid red vertical lines are the threshold for
                               rejecting the null hypothesis of no effect.
                               A red vertical line sits at a treatment effect of "
                   , tags$span(style="color:red", p3(U)) , # based in user input alpha mu + pooled SE * user input 
                   ", above "
                   , tags$span(style="color:red", p1(100-Y/2*100)) , # user inputted 2 sided alpha
                   "% of the null (pink) distribution. There is only a "
                   , tags$span(style="color:red", p1(Y/2*100)) , # simply alpha user input divided by 2
                   "% chance that a null (no effect) therapy will produce an observed effect above the vertical line. 
                 This limits our chance of type I error at alpha= "
                   , tags$span(style="color:red", p1(Y*100)) , # alpha user input
                   "% (two sided, we add both of the red areas together).<br><b><br><b> The green distribution shows the range of likely 
                 values when the therapy achieves the desired "
                   , tags$span(style="color:red", p1(B)) , # alternative user input
                   " point effect. Values anywhere from "
                   , tags$span(style="color:red", p1(V)) , #  only true for 95% so alpha 0.05 <- change this? add crosses??*********
                   " to  "
                   , tags$span(style="color:red", p1(W)) , # only true for 95% so alpha 0.05 <- change this? add crosses?*********
                   " are quite likely. Fortunately, if the true effect is "
                   , tags$span(style="color:red", p1(B)) , # simple user input
                   " points, then we have a "
                   , tags$span(style="color:red", p1(100-X*100)) , # power
                   "% chance of getting a value above the vertical line placed at "
                   , tags$span(style="color:red", p3(U)) ,  #upper threshold of null dist.
                   " and rejecting the null hypothesis. This is our "
                   , tags$span(style="color:red", p1(100-X*100)) , # power again
                   "% power. <br><b><br><b>Why did we choose N="
                   , tags$span(style="color:red", p0(N)) , #from ttest power calculation
                   " per arm? The pink and green distributions get narrower as the sample size increases. N ="
                   , tags$span(style="color:red", p0(N)) , #see above
                   " is the smallest sample size where we can find a cutoff that simultaneously gives us "
                   , tags$span(style="color:red", p1(Y*100)) , # alpha user input
                   "% type I error and "
                   , tags$span(style="color:red", p1(100-X*100)) , #power
                   "% power, whilst accounting for the SD. Smaller N results in too much overlap.",
                   
                   "<br><b><br><b> ",
                   "What effect do I need to see in my study to get a two sided P-value of "
                   , tags$span(style="color:red", p3(Y)) , #alpha user input
                   " answer: "
                   , tags$span(style="color:red", p3(C)) , #this is 1 se * pnorm 1-alpha
                   ". That is "
                   , tags$span(style="color:red", p1(C/B*100)) ,
                   "% of the value we were shooting for..."
                   , tags$span(style="color:red", p3(B)  ),  
                   
                   "<br><b><br><b> ",
                   "What effect do I need to see in my study to get p=0.05 two sided, answer: "
                   , tags$span(style="color:red", p3(qnorm(0.05/2,  mean=A, sd=E, lower.tail = FALSE ))) ,
                   "<br><b><br><b> ",
                   "What effect do I need to see in my study to get p=0.01 two sided, answer: "
                   , tags$span(style="color:red", p3(qnorm(0.01/2,  mean=A, sd=E, lower.tail = FALSE ))) ,
                   "<br><b><br><b> ",
                   "What effect do I need to see in my study to get p=0.001 two sided, answer: "
                   , tags$span(style="color:red", p3(qnorm(0.001/2,  mean=A, sd=E, lower.tail = FALSE ))),
                   "")) 
      
    } else if (B<0) {
      
      HTML(paste0( "The distribution in pink, shows the distribution of observed treatment effects when the
    therapy doesn't work (true effect is: "
                   , tags$span(style="color:red", p0(A)) ,
                   "). While the true effect is  "
                   , tags$span(style="color:red", p0(A)) ,
                   ", observed effects anywhere from " 
                   , tags$span(style="color:red", p1(L)) ,
                   " to  "
                   , tags$span(style="color:red", p1(U)) ,
                   " are quite likely to occur. Getting an observed effect of "
                   , tags$span(style="color:red", p1(FF2)) ,
                   " (purple cross), for example,  is well within this
                               sampling variability and not conclusive evidence the therapy works.
                               The solid red vertical lines are the threshold for
                               rejecting the null hypothesis of no effect.
                               A red vertical line sits at a treatment effect of "
                   , tags$span(style="color:red", p3(L)) ,
                   ", below "
                   , tags$span(style="color:red", p1(100-Y/2*100)) ,
                   "% of the null distribution. There is only a "
                   , tags$span(style="color:red", p1(Y/2*100)) ,
                   "% chance that a null (no effect) therapy will produce an observed effect below the vertical line. 
                   This limits our chance of type I error at alpha= "
                   , tags$span(style="color:red", p1(Y*100)) ,
                   "% (two sided, we add both of the red areas together). <br><b><br><b>The green distribution shows the range of
                   likely values when the therapy achieves the desired "
                   , tags$span(style="color:red", p1(B)) ,
                   " point effect. Values anywhere from "
                   , tags$span(style="color:red", p1(V)) ,
                   " to  "
                   , tags$span(style="color:red", p1(W)) ,
                   " are quite likely. Fortunately, if the true effect is "
                   , tags$span(style="color:red", p1(B)) ,
                   " points, then we have a "
                   , tags$span(style="color:red", p1(100-X*100)) ,
                   "% chance of getting a value below the vertical line placed at "
                   , tags$span(style="color:red", p3(L)) ,
                   " and rejecting the null hypothesis. This is our "
                   , tags$span(style="color:red", p1(100-X*100)) ,
                   "% power. <br><b><br><b>Why did we choose N="
                   , tags$span(style="color:red", p0(N)) ,
                   " per arm? The pink and green distributions get narrower as the sample size increases. N ="
                   , tags$span(style="color:red", p0(N)) ,
                   " is the smallest sample size where we can find a cutoff that simultaneously gives us "
                   , tags$span(style="color:red", p1(Y*100)) ,
                   "% type I error and "
                   , tags$span(style="color:red", p1(100-X*100)) ,
                   "% power, whilst accounting for the SD. Smaller N results in too much overlap.",
                   
                   "<br><b><br><b> ",
                   "What effect do I need to see in my study to get a two sided P-value of "
                   , tags$span(style="color:red", p3(Y)) ,
                   " answer: "
                   , tags$span(style="color:red", p3(C)) ,
                   ". That is "
                   , tags$span(style="color:red", p1(C/B*100)) ,
                   "% of the value we were shooting for..."
                   , tags$span(style="color:red", p3(B)  ),  
                   
                   "<br><b><br><b> ",
                   "What effect do I need to see in my study to get p=0.05 two sided, answer: "
                   , tags$span(style="color:red", p3(qnorm(0.05/2,  mean=A, sd=E, lower.tail = FALSE ))) ,
                   "<br><b><br><b> ",
                   "What effect do I need to see in my study to get p=0.01 two sided, answer: "
                   , tags$span(style="color:red", p3(qnorm(0.01/2,  mean=A, sd=E, lower.tail = FALSE ))) ,
                   "<br><b><br><b> ",
                   "What effect do I need to see in my study to get p=0.001 two sided, answer: "
                   , tags$span(style="color:red", p3(qnorm(0.001/2,  mean=A, sd=E, lower.tail = FALSE ))),
                   # "<br><b><br><b> ",
                   # "What effect do I need to see in my study to get p=0.05 two sided, answer: "
                   # , tags$span(style="color:red", p3(qnorm(0.05/2,  mean=A, sd=E, lower.tail = FALSE ))) ,
                   # "<br><b><br><b> ",
                   # "What effect do I need to see in my study to get p=0.01 two sided, answer: "
                   # , tags$span(style="color:red", p3(qnorm(0.01/2,  mean=A, sd=E, lower.tail = FALSE ))) ,
                   # "<br><b><br><b> ",
                   # "What effect do I need to see in my study to get p=0.001 two sided, answer: "
                   # , tags$span(style="color:red", p3(qnorm(0.001/2,  mean=A, sd=E, lower.tail = FALSE ))),
                   "" ))
    }
 
  # --------------------------------------------------------------------------
   
    
    A <-  alpha
    B <-  beta
    C <-  qnorm(1-B) +  qnorm(1-A/2)
    CC <- qnorm(1-.1) + qnorm(1-0.05/2)
    D <-  qnorm(1-A/2)
    E <-  (1-pnorm(C))*2 
    
    HTML(paste0("What is the Z value and P-Value if the mean difference comes in as hypothesised? From the null, with alpha of " 
                , tags$span(style="color:red", p3(A)) ,
                " two sided, we move "
                , tags$span(style="color:red", p3(qnorm(1-A/2))) ,
                " Z score to the right. Now when beta is "
                , tags$span(style="color:red", p3(B)) ,
                " we imagine moving right to left "
                , tags$span(style="color:red", p2(qnorm(1-B))) ,
                " Z score from the alternative hypothesised mean difference. 
               Therefore we have worked out the mean difference to satisfy both alpha and beta.
               Together this results in a Z score from the null of " 
                , tags$span(style="color:red", p3(qnorm(1-B))) ,
                " + "
                , tags$span(style="color:red", p3(qnorm(1-A/2)))  ,
                " = "
                , tags$span(style="color:red", p3(C)  ),
                " and the associated two sided P-Value is "
                , tags$span(style="color:red", p4(2*(1-pnorm(C)) )),
                ". Being "
                , tags$span(style="color:red", p0((1-B)*100)) ,
                "% sure of getting P < "
                , tags$span(style="color:red", A) ,
                " means we are " 
                , tags$span(style="color:red", 50) ,
                "% sure of getting p < "
                , tags$span(style="color:red", p4(2*(1-pnorm(C)))  ),
                ". </b> We can learn some more of the implications. The estimate of the true effect size, 
                  which we 'powered' for (considered the smallest difference of clinical importance) when 
                  what is known as 'statistical significance' is reached, here, and conventionally for many studies, when P < "
                , tags$span(style="color:red", p2(A)) ,
                " will be smaller if the treatment works as expected, due to sampling variation",
                "<br><b><br><b> ",
                "The effect size at which we attain a two-sided P-value of " 
                , tags$span(style="color:red", A) ,
                " is " 
                , tags$span(style="color:red", p3(qnorm(1-A/2))) ,
                ". That is "
                , tags$span(style="color:red", p1(D/C*100)) ,
                "% of the value we were shooting for..."
                , tags$span(style="color:red", p3(C)  ),
                ". For all studies with the same alpha and beta we can quote this percentage relationship. That is, when two-sided P = "
                , tags$span(style="color:red", A) ,
                " the effect size estimate will be "
                , tags$span(style="color:red", p1(D/C*100)) ,
                " % of the true value.",
                "<br><b><br><b> ",
                "The P-value (two-sided) that we attain if the treatment estimate matches the alternative mean difference " 
                , tags$span(style="color:red", p3(C)) ,
                " is " 
                , tags$span(style="color:red", p4(2*(1-pnorm(C)))  ),
                ". For all studies with the same alpha and beta we can quote this P-value relationship. That is, being "
                , tags$span(style="color:red", p0((1-B)*100)) ,
                "% sure of getting P < "
                , tags$span(style="color:red", A) ,
                " means we are " 
                , tags$span(style="color:red", 50) ,
                "% sure of getting p < "
                , tags$span(style="color:red", p4(2*(1-pnorm(C)))  ),
                "<br><b><br><b> ",
                "<br><b><br><b> ",
                
                # "What effect do I need to see in my study to get a two sided P-value of "
                # , tags$span(style="color:red", p3(A)) ,
                # " answer: "
                # , tags$span(style="color:red", p3(qnorm(1-A/2))) ,
                # ". That is "
                # , tags$span(style="color:red", p1(D/C*100)) ,
                # "% of the value we were shooting for..."
                # , tags$span(style="color:red", p3(C)  ),
                # 
                # "<br><b><br><b> ",
                # "What effect do I need to see in my study to get P=0.05 two sided, answer: "
                # , tags$span(style="color:red", p3(qnorm(0.05/2,  mean=0, sd=1, lower.tail = FALSE ))),
                # "<br><b><br><b> ",
                # "What effect do I need to see in my study to get P=0.01 two sided, answer: "
                # , tags$span(style="color:red", p3(qnorm(0.01/2,  mean=0, sd=1, lower.tail = FALSE ))) ,
                # "<br><b><br><b> ",
                # "What effect do I need to see in my study to get P=0.0012 two sided, answer: "
                # , tags$span(style="color:red", p3(qnorm((1-pnorm(CC)),  mean=0, sd=1, lower.tail = FALSE ))),
                # "<br><b><br><b> ",
                #  "What effect do I need to see in my study to get P=0.001 two sided, answer: "
                # , tags$span(style="color:red", p3(qnorm(0.001/2,  mean=0, sd=1, lower.tail = FALSE ))),
                ""
    )) 
 
  # --------------------------------------------------------------------------
  # take homes tab
 
    
    A <-   alpha
    B <-   beta
    C <-  qnorm(1-B) +  qnorm(1-A/2)
    CC <- qnorm(1-.1) + qnorm(1-0.05/2)
    D <-  qnorm(1-A/2)
    E <-  (1-pnorm(C))*2 
    sd <-  sd1  
    
    HTML(paste0("Here we can modify the 'Alpha, Type I error', 'Beta, Type II error' and 'Standard deviation' only. 
      First notice that changing the standard deviation has no 
                  effect on the relationship between the two distributions. Slide the standard deviation to 1 to see some familiar 
                  Z-scores. The ratio "
                , tags$span(style="color:red", p2(D*sd)) , "se/" , tags$span(style="color:red", p2(C*sd)) ,
                "se = " 
                , tags$span(style="color:red", p3(D/C)) ,
                " tells us that the effect size when the two sided P Value = " 
                , tags$span(style="color:red", p3(A)) ,
                " will be "
                , tags$span(style="color:red", p3(D/C)) ,
                " times the value that we initally powered the study to find - but that was the smallest difference considered
                  clinically important to pick up! This proportion is constant for any SD as we have shown and any alternative mean effect, 
                  contingent on alpha and beta not changing. Move the slider above to see the treatment effect for a given P-Value.
                    <br><b><br><b> 
                  If we actually achieve the hoped for effect, the P-Value will be "
                , tags$span(style="color:red", p6(E)) ,
                ", this is the region of the red distribution extending beyond "
                , tags$span(style="color:red", p3(C*sd)) , "se in both directions from the null value. 
                             <br><b><br><b> 
                             Here is some 'R code': ",
                "<br><b><br><b>",
                " 2*(1 - pnorm(qnorm("
                , tags$span(style="color:red", p3(1-A/2)) ,
                ")+qnorm("
                , tags$span(style="color:red", p3(1-B)) ,
                "))) ="
                , tags$span(style="color:red", p6(E)) ,
                "<br><b><br><b>",
                "   ",
                "qnorm("
                , tags$span(style="color:red", p3(1-A/2)) ,
                ")/ (qnorm("
                , tags$span(style="color:red", p3(1-A/2)) ,
                ")+qnorm("
                , tags$span(style="color:red", p3(1-B)) ,
                ")) ="
                , tags$span(style="color:red", p3(D/C)) ,
                
                ""
    )) 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
 
    
    #x <- seq(mu1-6*sigDiff, mu2+6*sigDiff, 0.1)
    
    if (mu1<mu2) {
      # plotting limits
      upper <- mu2+6*se1
      lower <- mu1-6*se1
      gap=0.001
      # z of distibution alpha tresholds
      crit <-  mu1 + crit1 * se1  # how many ses above mean
      crit2 <- mu1 - crit1 * se1  # how many ses below mean
      # ranges for polygons, remember this is not N(0,1)
      xx <-    seq( crit,  upper,  by=gap)
      xxx <-   seq( lower, crit,   by=gap)
      xxxx <-  seq( lower, crit2,  by=gap)
      xxxxx <- seq( crit2, crit,   by=gap)
      # co-ordinates for placement of text
      Y= max(dnorm(x, mean =mu1, sd=se1))
      X1 = mu1-se1*4
      X2 = mu2+se2*4
      X3 = mu2+se2*3
      X11 = mu1-se1*6.5
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #  the distribution of observed effect when the therapy has true effect 0 (null), 
      curve(dnorm(x, mean =mu1, sd= se1), xlim=c(lower, upper),
            bty="n",yaxt="n",lwd=2,  #xaxt="n", 
            col='red',
            ylab='',xlab='Observed Treatment Effect', 
            # main=paste0("Figure 1: Sampling distribution of the null and alternative treatment effects 
            #             (standard error of the mean difference between the two randomised groups).\nUnder the null hypothesis, 
            #             mean difference = ",mu1," & SE = ",p2(se1),", under alternative hypothesis, mean difference = ",mu2," & SE = ",p2(se2),".\n 
            #             This requires N per group = ",p0(n1),", to have power= ",1-beta," and alpha two-sided = ",alpha,""))
            main=paste0("Figure 1: Sampling distribution of the null and alternative treatment effects (mean difference between the two randomised groups).\nUnder the null hypothesis, mean difference = ",mu1," & SE = ",p2(se1),", under alternative hypothesis, mean difference = ",mu2," & SE = ",p2(se2),".\n This requires N per group = ",p0(n1),", to have power= ",1-beta," and alpha two-sided = ",alpha,""))
      
      # and the green shows the distribution of observed effect when the therapy truly has our hoped for effect. 
      curve(dnorm(x, mean =mu2, sd=se2), lwd=2, add=TRUE , xlab='Observed Treatment Effect', col="green")
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # power distribution
      polygon(x = c(crit,                           xx,  upper),
              y = c(0 , dnorm(mean=mu2, sd=se2,     xx),     0),
              col="lightgreen")
      
      # type 1 error upper, plot from crit value to infinity
      polygon(x=c(crit,                                xx, upper),
              y=c(0,   dnorm(mean=mu1, sd=se1 ,        xx),    0),
              col="red")
      
      # null distribution
      polygon(x=c(crit2,                               xxxxx,  crit),
              y=c(0,   dnorm(mean=mu1, sd=se1 ,        xxxxx),    0),
              col=rgb(1,0,0,alpha=0.3))
      
      # type 1 error lower
      polygon(x=c(lower,                               xxxx, crit2),
              y=c(0,   dnorm(mean=mu1, sd=se1 ,        xxxx),    0),
              # col="#0BAD4D")
              col="red")
      
      # type II error area, beta
      polygon(x = c(lower,                           xxx,  crit),
              y = c(0 , dnorm(mean=mu2, sd=se2,      xxx),    0),
              col="forestgreen")
      
      
      par(oma=c(0,0,0,0))
      mtext(paste("Under null hypothesis mean difference=",mu1,"SE=",p2(se1),", under alternative hypothesis mean difference =",mu2,"SE=",p2(se2)," "),
            side=3,line=0,outer=TRUE,cex=1.2)
      text(x=X1,y=Y*1.1,  labels=paste0("This side of red line\nreject H0"),cex=1)
      points(c(6,2), c(2,1), pch = 3, cex = 4, col = "red")
      
      
      # rejecting line and sample point
      abline(v=crit, col="red",  lwd=1,  lty=1)
      abline(v=crit2,col="red",  lwd=1,  lty=1)
      abline(v=mu1,  col="blue", lwd=.5, lty=3)  
      abline(v=mu2,  col="blue", lwd=.5, lty=3)  
      
      shrink <- .65
      text(x=X1,y=Y*shrink,  labels=paste0("This side of red line\nreject H0"),cex=1)
      text(x=X3,y=Y*shrink,  labels=paste0("This side of red line\nreject H0"),cex=1)
      text(x=mu1,y=Y*shrink,  labels=paste0("Between red lines\nfail to reject H0"),cex=1)
      
      
      points(mu1 + sigDiff,      0, col="purple",     pch=4, cex=3, lwd=3) 
      
      legend(x=X11, Y*1 ,  "Legend:",
             legend=c(
               expression(paste("Power (1-",beta,")")),
               expression(paste("Type II error (",beta,")   ")),
               expression(paste("Type I error (",alpha,")"))),
             fill=c("green","forestgreen","red"),
             cex=1)
      
      
      
    } else if (mu1>mu2) {
      
      # plotting limits
      upper <- mu1+6*se1
      lower <- mu2-6*se1
      gap=0.001
      # z of distibution alpha tresholds
      crit <-  mu1 + crit1 * se1  # how many ses above mean
      crit2 <- mu1 - crit1 * se1  # how many ses below mean
      # ranges for polygons, remember this is not N(0,1)
      xx <-    seq( crit,  upper,  by=gap)
      xxx <-   seq( lower, crit,   by=gap)
      xxxx <-  seq( lower, crit2,  by=gap)
      xxxxx <- seq( crit2, crit,   by=gap)
      x0 <- seq( crit2, upper,   by=gap)
      # co-ordinates for placement of text
      Y= max(dnorm(x, mean =mu1, sd=se1))
      X1 = mu2-se1*3
      X2 = mu2+se2*4
      X3 = mu1+se2*4
      X11 = mu2-se1*6.5
      
      
      #  the distribution of observed effect when the therapy has true effect 0 (null),
      curve(dnorm(x, mean =mu1, sd= se1), xlim=c(lower, upper),
            bty="n",yaxt="n",lwd=2,  #xaxt="n",
            col='red',
            ylab='',xlab='Observed Treatment Effect', 
            main=paste0("Figure 1: Sampling distribution of the null and alternative treatment effects (mean difference between the two randomised groups).\nUnder the null hypothesis, mean difference = ",mu1," & SE = ",p2(se1),", under alternative hypothesis, mean difference = ",mu2," & SE = ",p2(se2),".\n This requires N per group = ",p0(n1),", to have power= ",1-beta," and alpha two-sided = ",alpha,""))
      
      # and the green shows the distribution of observed effect when the therapy truly has our hoped for effect.
      curve(dnorm(x, mean =mu2, sd=se2), lwd=2, add=TRUE , xlab='Observed Treatment Effect', col="green")
      
      # type 1 error upper, plot from crit value to infinity
      polygon(x=c(crit,                                xx, upper),
              y=c(0,   dnorm(mean=mu1, sd=se1 ,        xx),    0),
              col="red")
      
      
      # null distribution
      polygon(x=c(crit2,                               xxxxx,  crit),
              y=c(0,   dnorm(mean=mu1, sd=se1 ,        xxxxx),    0),
              col=rgb(1,0,0,alpha=0.3))
      
      # type 1 error lower
      
      polygon(x = c(crit2,                           xxx,  upper),
              y = c(0 , dnorm(mean=mu2, sd=se2,     xxx),     0),
              col="lightgreen")
      
      
      polygon(x=c(lower,                               xxxx, crit2),
              y=c(0,   dnorm(mean=mu1, sd=se1 ,        xxxx),    0),
              # col="#0BAD4D")
              col="red")
      
      
      #type II error area, beta
      polygon(x = c(crit2,                           x0,  upper),
              y = c(0 , dnorm(mean=mu2, sd=se2,      x0),    0),
              col="forestgreen")
      
      
      # rejecting line and sample point
      abline(v=crit, col="red",  lwd=1,  lty=1)
      abline(v=crit2,col="red",  lwd=1,  lty=1)
      abline(v=mu1,  col="blue", lwd=.5, lty=3)  
      abline(v=mu2,  col="blue", lwd=.5, lty=3)  
      
      shrink <- .65
      text(x=X1,y=Y*shrink,  labels=paste0("This side of red line\nreject H0"),cex=1)
      text(x=X3,y=Y*shrink,  labels=paste0("This side of red line\nreject H0"),cex=1)
      text(x=mu1,y=Y*shrink,  labels=paste0("Between red lines\nfail to reject H0"),cex=1)
      
      points(mu1 - sigDiff,      0, col="purple",     pch=4, cex=3, lwd=3) 
      
      legend(x=X11, Y*1 ,  "Legend:",
             legend=c(
               expression(paste("Power (1-",beta,")")),
               expression(paste("Type II error (",beta,")   ")),
               expression(paste("Type I error (",alpha,")"))),
             fill=c("green","forestgreen","red"),
             cex=1)
      
    }  
    
 
  
  #---------------------------------------------------------------------------
  # take homes tab
 
    
    sd <- sd1
    A <-  alpha
    B <-  beta
    #PV<-  input$PV
    pvalue2<-  pvalue2
    
    a <- 1-A/2
    b <- 1-B
    
    mu2 <- qnorm(a)*sd+qnorm(b)*sd
    u975 <- qnorm(a)*(sd)
    cex1 <- 1
    lower = -6*sd
    upper = 7*sd
    x <- seq(-6*sd, 7*sd, 0.1)
    
    
    num <- dnorm(x, mean =0, sd= 1)
    y <- dnorm(x, mean =0, sd= sd)
    fact <- 1/(max(num)/max(y))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    curve(dnorm(x, mean =0, sd= sd), xlim=c(-4*sd,7*sd),
          bty="n",yaxt="n",lwd=2, # xaxt="n", 
          col='red',
          ylab='',xlab='Treatment Effect', 
          main=paste0("Figure 3: Sampling distribution of the null and alternative treatment effects (mean difference between the two randomised groups).\nUnder the null hypothesis and under the alternative hypothesis, the relative relationships."))         
    
    
    curve(dnorm(x, mean =  mu2, sd=sd), lwd=2, add=TRUE , xlab='Treatment Effect', col="forestgreen")
    
    abline(v= u975,          col="blue",  lwd=1,  lty=3)
    abline(v= mu2, col="blue",  lwd=1,  lty=3)
    abline(v=0,                  col="blue", lwd=.5, lty=3)  
    
    gap=0.001
    
    xx <-    seq(  qnorm(a)*sd, upper,  by=gap)
    xxx <-    seq(  lower, qnorm(a)*sd,  by=gap)
    xxxx <-    seq(  lower, -qnorm(a)*sd,  by=gap)
    
    polygon(x = c(qnorm(a)*sd,                       xx,  upper),
            y = c(0 , dnorm(mean=0, sd=sd,     xx),     0),
            col="red")
    
    polygon(x = c(lower,                       xxxx,  -qnorm(a)*sd),
            y = c(0 , dnorm(mean=0, sd=sd,     xxxx),     0),
            col="red")
    #type II error area, beta
    polygon(x = c(lower,                           xxx,  qnorm(a)*sd),
            y = c(0 , dnorm(mean=mu2, sd=sd,      xxx),    0),
            col="forestgreen")
    
    # +/-se
    text(x=0,y=.26*fact,  labels=paste0("se =  +/- ",sd,""),cex= cex1)
    arrows( 0, .24*fact,  sd, .24*fact, col = 1:3, code=2)
    arrows( 0, .24*fact, -sd, .24*fact, col = 1:3)
    
    # typeI
    arrows(0, .3*fact, u975, .3*fact, col = "red")
    text(x=u975/2, y=.32*fact,  labels=paste0(" ", p2(u975), "se"),cex= cex1)
    
    # typeII
    arrows(qnorm(a)*sd+qnorm(b)*sd, .26*fact, qnorm(a)*sd, .26*fact, col = "forestgreen", lwd=1.5)
    text(x= (sd*qnorm(a)+sd*qnorm(b)/2), y=.24*fact,  labels=paste0(" ", p2(qnorm(b)*sd), "se"),cex= cex1)
    
    # total
    arrows( 0, .35*fact, sd*qnorm(a)+qnorm(b)*sd, .35*fact, col = 1:3)
    text(x=   (sd*qnorm(a)+sd*qnorm(b))/2 , y=.36*fact,  labels=paste0(" ", 
                                                                       p2(sd*qnorm(a)+qnorm(b)*sd), "se"),cex= cex1)
    
    zz <- qnorm(1- pvalue2/2)*sd
    points(zz,      0, col="orange",     pch=4, cex=3, lwd=3) 
    points(-zz,      0, col="orange",     pch=4, cex=3, lwd=3) 
    
  
  
  #---------------------------------------------------------------------------
  # standard normal
  ###########################
   
    
    mu1=0
    sd1=se1=1
    se1=se2=1
    
    beta <- beta
    alpha <- alpha
    crit1 <- qnorm(1-as.numeric(alpha/2))
    crit2 <- qnorm(1-as.numeric(beta))   #power
    
    # z of distibution alpha tresholds
    crita <-  mu1 + crit1 * se1          # how many ses above mean
    mu2 <- critb <- crita + crit2        # how many ses above crit
    # ranges for polygons, remember this is not N(0,1)
    
    # plotting limit
    x <- seq(mu1-6*se1, mu2+6*se2, 0.1)
    
    upper <- mu2+6*se1
    lower <- mu1-6*se1
    gap=0.001
    xx <-    seq( crita,  upper,  by=gap)
    xxx <-   seq( lower, -crita,   by=gap)
    #xxxx <-  seq( lower, critb,  by=gap)
    xxxxx <- seq( -crita, crita,   by=gap)
    xy <-  seq( lower, crita,   by=gap)
    # co-ordinates for placement of text
    Y= max(dnorm(x, mean =mu1, sd=se1))
    X1 =  mu1-se1*4
    X2 =  mu2+se2*4
    X3 =  mu2+se2*3
    X11 = mu1-se1*6.5
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #  the distribution of observed effect when the therapy has true effect 0 (null),
    curve(dnorm(x, mean =mu1, sd= se1), xlim=c(lower, upper),
          bty="n",yaxt="n",lwd=2,  #xaxt="n",
          col='red',
          ylab='',xlab='Observed Treatment Effect',
          main=paste0("Figure 2: Standard normal sampling distribution of the null and alternative treatment effects. Power= ",1-beta," and alpha two-sided = ",alpha,"\nUnder the null hypothesis, mean difference = ",p2(mu1)," & SE = ",p2(se1),", under alternative hypothesis, mean difference = ",p2(mu2)," & SE = ",p2(se2),""))
    
    # and the green shows the distribution of observed effect when the therapy truly has our hoped for effect.
    curve(dnorm(x, mean =mu2, sd=se2), lwd=2, add=TRUE , xlab='Observed Treatment Effect', col="green")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #power distribution
    polygon(x = c(crita,                           xx,  upper),
            y = c(0 , dnorm(mean=mu2, sd=se2,     xx),     0),
            col="lightgreen")
    
    # type 1 error upper, plot from crit value to infinity
    polygon(x=c(crita,                                xx, upper),
            y=c(0,   dnorm(mean=mu1, sd=se1 ,        xx),    0),
            col="red")
    
    # null distribution
    polygon(x=c(-crita,                               xxxxx,  crita),
            y=c(0,   dnorm(mean=mu1, sd=se1 ,        xxxxx),    0),
            col=rgb(1,0,0,alpha=0.3))
    
    # type 1 error lower
    polygon(x=c(lower,                               xxx, -crita),
            y=c(0,   dnorm(mean=mu1, sd=se1 ,        xxx),    0),
            # col="#0BAD4D")
            col="red")
    
    # # type II error area, beta
    polygon(x = c(lower,                           xy,  crita),
            y = c(0 , dnorm(mean=mu2, sd=se2,      xy),    0),
            col="forestgreen")
    
    
    par(oma=c(0,0,0,0))
    mtext(paste("Under null hypothesis mean difference=",mu1,"SE=",p2(se1),", under alternative hypothesis mean difference =",mu2,"SE=",p2(se2)," "),
          side=3,line=0,outer=TRUE,cex=1.2)
    text(x=X1,y=Y*1.1,  labels=paste0("This side of red line\nreject H0"),cex=1)
    points(c(6,2), c(2,1), pch = 3, cex = 4, col = "red")
    
    
    # rejecting line and sample point
    abline(v=-crita, col="red",  lwd=1,  lty=1)
    abline(v=crita, col="red",  lwd=1,  lty=1)
    abline(v=mu1,  col="blue", lwd=.5, lty=3)
    abline(v=mu2,  col="blue", lwd=.5, lty=3)
    
    shrink <- .65
    text(x=X1,y=Y*shrink,  labels=paste0("This side of red line\nreject H0"),cex=1)
    text(x=X3,y=Y*shrink,  labels=paste0("This side of red line\nreject H0"),cex=1)
    text(x=mu1,y=Y*shrink,  labels=paste0("Between red lines\nfail to reject H0"),cex=1)
    
    
    legend(x=X11, Y*1 ,  "Legend:",
           legend=c(
             expression(paste("Power (1-",beta,")")),
             expression(paste("Type II error (",beta,")   ")),
             expression(paste("Type I error (",alpha,")"))),
           fill=c("green","#004987","red"),
           cex=1)
    
   
 
    
    sims <- 1000
    null <-   0
    mu2 <-   mu2
    sd <-   sd
    alpha <- alpha
    beta <-  beta
    
    theory <- 2*(1 - pnorm(qnorm(1-alpha/2) + qnorm(1-beta))) # expected pvalue at H1
    
    muDiff <- mu2-null
    d <- (mu2-null)/sd 
    # calc sample size
    n <-(2*(qnorm(1-alpha/2) + qnorm(1-beta) )^2 ) / (d)^2  #power, obtain N
    
    
    
    dfx=n-1
    # x <- (replicate(sims, t.test(rt(n, df=dfx), 
    #                              rt(n, df=dfx)*sqrt(1 * (dfx-2)/dfx)+d,
    #                              conf.level=.95, alternative="two.sided") $p.value))  
    #   
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ok
    #sigDiff <- sqrt((sd^2/n) + (sd^2/n))
    x <- (replicate(sims, t.test(rt(n,df=dfx, sd), 
                                 rt(n, df=dfx, sd)*1+d, 
                                 conf.level=1-alpha, paired = FALSE, alternative="two.sided") $p.value))  
    #   
    (medp <- median(x) )# median pvalue if H1 true
    (meanp <- mean(x <alpha)) # power again
    #   
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # get the mean differnces see jow many are less than d expect 50%!
    # xx <- function() {
    #   
    #   diff((t.test(rnorm(n,0,1),rnorm(n, d,1), conf.level=.95, alternative="two.sided"))$estimate)
    # }
    # 
    # zz<- unlist(replicate(sims, xx()))
    # fiftyperc <-  sum(zz<=d)/length(zz)  
    
    
    # dd <- as.data.frame(cbind( alpha=alpha, beta=beta, meand=muDiff, sd=sd1, 
    #                           d=d,
    #                           n1=n1, theory=theory, meanp = meanp , medp=medp) )
    # 
    # namez <- c( "Alpha", "Beta", "Mean diff.", "Common SD in each group", "Standardised Diff." , "N per group" , "Expected P-value", 
    #             "Simulated Power", "Simulated Median P-Value")
    # 
    # names(dd) <- namez
    # 
    
    dd <- as.data.frame(cbind( alpha=alpha, beta=beta, power=1-beta, meand=muDiff, sd=sd, d=d , n=n) )
    namez <- c( "Alpha", "Beta", "Power","Mean difference", "Common SD in each group", "Standardised Difference" , "N per group")
    names(dd) <- namez
    #    
    # dd1 <- as.data.frame(cbind( medp=medp,    p.simH1=p.simH1 , meanp=meanp ) )
    # tmp <- paste0("Probability P-Value is less than ",p6(theory)," at H1")
    # namez <- c(  "Simulated P-Value at H1",  tmp, "Simulated Power")
    # names(dd1) <- namez
    # 
    # namez <- c( "Expected P-Value at H1",   tmp )
    # dd2 <- as.data.frame(cbind( medp=theory,   meanp=.5  ) )
    # names(dd2) <- as.character(namez)
    # 
    
    
   