#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(shiny)
library(nlme)
library(VCA)
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
ui <- fluidPage(theme = shinytheme("journal"),
                
                shinyUI(pageWithSidebar(
                  
                  headerPanel("The relationship between power, alpha, beta and P-Values"),
                  
                  
                  sidebarPanel( 
                    
                    div(p("When being introduced to frequentist power calcualtions invariably you will be shown two distributions. 
                    If you are not familiar with the properties of distributions the concepts may seem quite strange, 
                    here I try to explain what's going on using RShiny.
                    The sliders below are used to select the
                    true population parameters for a normally distributed continuous response. 
                          On the first tab '1 Sample size', the required number of subjects is calculated based on the inputs. 
                          This information is then used on the '2 Operating characteristics' tab where the 
                          operating characteristics of the study are displayed. On the third tab 
                          '3 The potential for statistically significant but clinically unimportant results', 
                          the relationship between alpha, beta, the Z score and P-Value 
                          are explored using the standard Normal distribution. Finally the fourth tab '4 Take home'
                          shows some relationships that are useful 
                          to remember.")),
                    
                    div(
                      br(),
                  
                      actionButton(inputId='ab1', label="R code here", 
                                   icon = icon("th"), 
                                   onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/Three-level-nested-variance-components-analysis2/master/2levelnested/app.R', '_blank')"),
                      
                       
                     br(), br(),  
                      div(("Select true population parameters")),
                      br(),

                       
                      
                      sliderInput("mu2",
                                  "Mean treatment effect under alternative hypothesis",
                                  min=-30, max=30, step=1, value=5, ticks=FALSE),
                      
                      sliderInput("sd1", "Standard deviation (SD)",
                                  min = 1, max = 50, step=0.1, value = 20, ticks=FALSE), #c( sqrt(2*20^2/337))
                    
                      
                      sliderInput("alpha", "Alpha, Type I error",
                                  min = 0.001, max = .5, step=.001, value = c(0.05 ),ticks=FALSE),
                      
                      sliderInput("beta", "Beta, Type II error",
                                  min = 0.01, max = .99, step=.01, value = c(0.10 ),ticks=FALSE)
                    )
                  ),
                  
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
                  mainPanel(
                    
                  #  htmlOutput("testHTML") ,
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    navbarPage(       
                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
                      tags$style(HTML(" 
                            .navbar-default .navbar-brand {color: cyan;}
                            .navbar-default .navbar-brand:hover {color: blue;}
                            .navbar { background-color: lightgrey;}
                            .navbar-default .navbar-nav > li > a {color:black;}
                            .navbar-default .navbar-nav > .active > a,
                            .navbar-default .navbar-nav > .active > a:focus,
                            .navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}
                            .navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}
                            .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}
                            .navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}
                            .navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}
                   ")), 
                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end of section to add colour     
                      
                        tabPanel("1 Sample size", 
                                 h3("Two sample T-test calculations"),
                                 p(strong("With the following inputs, 'Mean treatment effect under alternative hypothesis', 'Standard deviation', Alpha ('Type I error') and Beta ('Type II error'), we perform a sample size calculation 
                                        and estimate the sample size required for each group. We have equal randomisation 1:1 of subjects to the two groups and a continuous response. 'd' in the output printed below is 
                                        (Mean treatment effect under alternative hypothesis'- 0)/ standard deviation.
                                        ")), 
                                 
                                 div( verbatimTextOutput("ssize2")),
                                 
                     ),
                      
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      tabPanel("2 Operating characteristics", 
                               h3("Operating characteristics of frequentist power calculations"),
                              
                         #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                               
                      div(plotOutput("reg.plot", width=fig.width, height=fig.height)),  
                          
                       #https://stackoverflow.com/questions/39250200/rshiny-textoutput-and-paragraph-on-same-line
                      h4(htmlOutput("textWithNumber1",) ),
                      
                      p("$$\\begin{align*}
                      \\text{Standard error of treatment effect} = \\sqrt{\\left({\\frac{ \\sigma_1^2}{n_1}  + \\frac{ \\sigma_2^2}{n_2} }\\right)}   \\\\
                      \\end{align*}$$"),
                      p(""),
                      
                      h4(htmlOutput("textWithNumber",) ),
                      
                      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
                      # for some reason this is need or abpve will not render!
                      withMathJax(
                        helpText('
                            $$   $$')),  
                      
                        
                        p(strong("")),
                      
                         div( verbatimTextOutput("ssize"))
                      ) ,
                      
                     tabPanel("3 The potential for statistically significant but clinically unimportant results", 
                               h4(htmlOutput("textWithNumber2",) ) ,
                                    
                                  div(plotOutput("norm.plot", width=fig.width, height=fig.height)),
                                  h4(htmlOutput("textWithNumber3",) ) ,
                                  width = 12 ),
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
                     tabPanel("4 Take home", 
                            #  h4(htmlOutput("textWithNumber2",) ) ,
                              
                              div(plotOutput("norm.plot1", width=fig.width, height=fig.height)),
                              h4(htmlOutput("textWithNumber4",) ) ,
                              width = 12 )
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
                     
                     
                                
                    )))
                )
)

server <- shinyServer(function(input, output) {
  
  # --------------------------------------------------------------------------
  # This is where a new sample is instigated only random noise is required to be generated
  # random.sample <- reactive({
  #   
  #   # Dummy line to trigger off button-press
  #   foo <- input$resample
  #   
  #  # N <- input$N
  #   mu1 <- 0 #input$mu1
  #   mu2 <- input$mu2
  #   sd1 <- input$sd1
  #   sd2 <- input$sd1
  #   alpha <- input$alpha
  #   beta <- input$beta
  # 
  #   return(list( 
  #     mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2, alpha=alpha, beta=beta     ))
  #  }) 
  # 
  # --------------------------------------------------------------------------
  # Set up the dataset based on the inputs 
  make.regression <- reactive({
    
    #   https://stats.stackexchange.com/questions/28876/difference-between-anova-power-simulation-and-power-calculation
    
 #   sample <- random.sample()
     
    mu1 <- 0# sample$mu1
    mu2 <- input$mu2
    sd1 <- input$sd1
    sd2 <- input$sd1
    alpha <- input$alpha
    beta <- input$beta
     
    muDiff  <-  mu2-mu1                  # true difference in means
    
    crit1 <- qnorm(1-as.numeric(alpha/2))
     
    pow <- pwr::pwr.t.test(d=(mu2-mu1)/sd1 ,power=1-beta, sig.level=as.numeric(alpha), type="two.sample",
                           alternative="two.sided")
    
    # this equation from LSHTM seems to give stable 60.5% but not R canned equation above?
    n1 <- n2 <-(2*(crit1 + qnorm(1-beta) )^2 ) / ((muDiff)/sd1)^2 
    
    #n1 <- n2 <- pow$n
    
    x <- seq(mu1-6*sd1, mu2+6*sd2, 0.1)
    
    sigDiff <- sqrt((sd1^2/n1) + (sd2^2/n2))
    
    se1 <- se2 <- sigDiff   # pooled se
    
    crit <- mu1 + crit1 * se1
    
    return(list( x=x, se1=se1, se2=se2, 
                sigDiff=sigDiff, n1=n1, n2=n2, 
                crit=crit,
                crit1=crit1,
                muDiff=muDiff, 
                alpha=alpha, beta=beta, 
                mu1=mu1, mu2=mu2 , sd1=sd1, pow=pow
                )) 
    
  })  
  
  # #---------------------------------------------------------------------------
  # #grab the power
  output$ssize2 <- renderPrint({

    return(make.regression()$pow)

  })
  # allowing mixing in numeric and text 
  # https://stackoverflow.com/questions/39250200/rshiny-textoutput-and-paragraph-on-same-line
  
  output$textWithNumber <- renderText({ 
    
    A <- make.regression()$mu1    # this will be zero
    B <- make.regression()$mu2    # this is an input
    C <- make.regression()$crit   # 0 + crit1 * se1, where crit1 is alpha 2 sided input and se1 the pooled SE
    D <- make.regression()$se1    # pooled SD
    E <- make.regression()$sigDiff #pooled Sd again
    FF <-  make.regression()$mu1 +  make.regression()$sigDiff # 0 + pooled SE
    FF2 <- make.regression()$mu1 -  make.regression()$sigDiff # 0 - pooled SE
    
    L <- make.regression()$mu1 -  make.regression()$sigDiff * make.regression()$crit1  # 0 - pooled SE x user input alpha level 
    U <- make.regression()$mu1 +  make.regression()$sigDiff * make.regression()$crit1  #
    V <- make.regression()$mu2 -  make.regression()$sigDiff * make.regression()$crit1  # mu2 - pooled SE x user input alpha level
    W <- make.regression()$mu2 +  make.regression()$sigDiff * make.regression()$crit1
    
    X <- make.regression()$beta  # user input 
    Y <- make.regression()$alpha # user input 
    N <- make.regression()$n1    # sample size from R's ttest power calculation
    
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
                 " are decently likely to occur. Getting an observed effect of "
                 , tags$span(style="color:red", p1(FF)) , # 1 SE up from null
                 ", for example,  is well within this
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
                  "% (two sided, we add both of the red areas together). The green distribution shows the range of likely 
                 values when the therapy achieves the hoped for "
                 , tags$span(style="color:red", p1(B)) , # alternative user input
                 " point effect. Values anywhere from "
                 , tags$span(style="color:red", p1(V)) , #  only true for 95% so alpha 0.05 <- change this? add crosses??*********
                 " to  "
                 , tags$span(style="color:red", p1(W)) , # only true for 95% so alpha 0.05 <- change this? add crosses?*********
                 " are decently likely. Fortunately, if the true effect is "
                 , tags$span(style="color:red", p1(B)) , # simple user input
                 " points, then we have a "
                 , tags$span(style="color:red", p1(100-X*100)) , # power
                 "% chance of getting a value above the vertical line placed at "
                 , tags$span(style="color:red", p3(U)) ,  #upper threshold of null dist.
                 " and rejecting the null hypothesis. This is our "
                 , tags$span(style="color:red", p1(100-X*100)) , # power again
                 "% power. Why did we choose N="
                 , tags$span(style="color:red", p0(N)) , #from ttest power calculation
                 " per arm? The pink and green distributions get narrower as the sample size increases. N ="
                 , tags$span(style="color:red", p0(N)) , #see above
                 " is the smallest sample size where we can find a cutoff that simultaneously gives us "
                 , tags$span(style="color:red", p1(Y*100)) , # alpha user input
                 "% type I error and "
                 , tags$span(style="color:red", p1(100-X*100)) , #power
                 "% power. Smaller N results in too much overlap.",
                 
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
                   , tags$span(style="color:red", p3(U)) ,
                   " are decently likely to occur. Getting an observed effect of "
                   , tags$span(style="color:red", p1(FF2)) ,
                   ", for example,  is well within this
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
                   "% (two sided, we add both of the red areas together). The green distribution shows the range of
                   likely values when the therapy achieves the hoped for "
                   , tags$span(style="color:red", p1(B)) ,
                   " point effect. Values anywhere from "
                   , tags$span(style="color:red", p1(V)) ,
                   " to  "
                   , tags$span(style="color:red", p1(W)) ,
                   " are decently likely. Fortunately, if the true effect is "
                   , tags$span(style="color:red", p1(B)) ,
                   " points, then we have a "
                   , tags$span(style="color:red", p1(100-X*100)) ,
                   "% chance of getting a value below the vertical line placed at "
                   , tags$span(style="color:red", p3(L)) ,
                   " and rejecting the null hypothesis. This is our "
                   , tags$span(style="color:red", p1(100-X*100)) ,
                   "% power. Why did we choose N="
                   , tags$span(style="color:red", p0(N)) ,
                   " per arm? The pink and green distributions get narrower as the sample size increases. N ="
                   , tags$span(style="color:red", p0(N)) ,
                   " is the smallest sample size where we can find a cutoff that simultaneously gives us "
                   , tags$span(style="color:red", p1(Y*100)) ,
                   "% type I error and "
                   , tags$span(style="color:red", p1(100-X*100)) ,
                   "% power. Smaller N results in too much overlap.",
                   
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
   })
    
    output$textWithNumber1 <- renderText({ 

        HTML(paste0( "Figure 1 depicts the sampling distribution of the treatment effect 
                                        (that is, the standard error of the mean difference between the two randomised groups); 
                                 when the treatment works as expected (green) and also when the treatment does not work (pink).
                                        Remember we are presenting two scenarios in Figure 1, both cannot occur. The following equation
                                        shows the standard error (SE) calculation used for each scenario, this is also known as the 
                                        standard deviation of the mean and shows the spread or variation.
                                        Therefore the two sample sizes ",HTML(" <em>n1</em>")," and ",HTML(" <em>n2</em>")," 
                     need to be included in the calculation of each sampling distribution."))
    
   })

    output$textWithNumber2 <- renderText({ 
       
      HTML(paste0("Notice on the previous tab changing the inputs did not effect the relationshiip between the two curves.
                  We can exploit this to help understanding. This means we can refer to one distribution, the standard normal. 
                  This is the Normal distribution with mean 0 and variance 1. Use only the 'alpha' and 'beta' sliders. 
                  The 'Mean treatment effect under alternative hypothesis' and 'Standard deviation' are not needed and have no effect here."))
        })
   
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$textWithNumber3 <- renderText({ 
 
    A <-  input$alpha
    B <-  input$beta
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
                  what is known as 'statistical significance', here when P < "
                           , tags$span(style="color:red", p2(A)) ,
                  " is reached, will be smaller even if the treatment works as expected due to sampling variation",
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
    })
    
    
    ##take homes tab
    output$textWithNumber4 <- renderText({ 
      
      A <-  input$alpha
      B <-  input$beta
      C <-  qnorm(1-B) +  qnorm(1-A/2)
      CC <- qnorm(1-.1) + qnorm(1-0.05/2)
      D <-  qnorm(1-A/2)
      E <-  (1-pnorm(C))*2 
      sd <- input$sd1 
      
 
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
                  " of the value that we initally powered the study to find - but that was the smallest difference considered clinically important to pick up! This proportion is constant for any SD as we have shown and any alternative mean effect contingent on alpha and beta not changing.
                    <br><b><br><b> 
                  If we actually achieve the hoped for effect, the P-Value will be "
                  , tags$span(style="color:red", p6(E)) ,
                  ", this is the region of the red distribution, extending in total beyond "
                  , tags$span(style="color:red", p3(C*sd)) , "se in both directions. Here is some R code: ",
                  "<br><b><br><b>",
                   " 2*(1 - pnorm(qnorm("
                  , tags$span(style="color:red", p3(1-A/2)) ,
                  ")+qnorm("
                  , tags$span(style="color:red", p3(1-B)) ,
                  "))) ="
                  , tags$span(style="color:red", p6(E)) ,
                  "<br><b><br><b>",
                  " and ",
                  "<br><b><br><b>",
                  "qnorm("
                  , tags$span(style="color:red", p3(1-A/2)) ,
                  ")/ (qnorm("
                  , tags$span(style="color:red", p3(1-A/2)) ,
                  ")+qnorm("
                  , tags$span(style="color:red", p3(1-B)) ,
                  ")) ="
                  , tags$span(style="color:red", p3(D/C)) ,
                  
                  
                  
                  # 
                  # 1- pnorm(qnorm(a)+qnorm(b))
                  # qnorm(a)/ (qnorm(a)+qnorm(b))
                  # 
    
                  
                  ""
      )) 
    })
 
   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
  output$reg.plot <- renderPlot({         
    
    data1 <- make.regression()
    
    df <- data1 
    
     d=df$d 
     limit=df$limit
     x=df$x 
     se1=df$se1
     se2=df$se2
     sigDiff=df$sigDiff
     n1=df$n1 
     n2=df$n2 
     crit=df$crit
     crit1=df$crit1
     muDiff=df$muDiff
     alpha=df$alpha
     beta=df$beta
     mu1=df$mu1
     mu2=df$mu2

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
     main=paste0("Figure 1: Sampling distribution of the null and alternative treatment effects (standard error of the mean difference between the two randomised groups).\nUnder the null hypothesis, mean difference = ",mu1," & SE = ",p2(se1),", under alternative hypothesis, mean difference = ",mu2," & SE = ",p2(se2),".\n This requires N per group = ",p0(n1),", to have power= ",1-beta," and alpha two-sided = ",alpha,""))
  
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
     
     points(2, 0,        col="darkred", pch=4, cex=3, lwd=3)
     points(3.019800, 0, col="darkorange", pch=4, cex=3, lwd=3)
     points(3.584303, 0, col="darkgreen", pch=4, cex=3, lwd=3)
     points(mu2,      0, col="purple", pch=4, cex=3, lwd=3)
     
     legend(x=X11, Y*1 ,  "Legend:",
            legend=c(
               expression(paste("Power (1-",beta,")")),
              expression(paste("Type II error (",beta,")   ")),
              expression(paste("Type I error (",alpha,")"))),
            fill=c("green","#004987","red"),
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
             main=paste0("Figure 1: Sampling distribution of the null and alternative treatment effects (standard error of the mean difference between the two randomised groups).\nUnder the null hypothesis, mean difference = ",mu1," & SE = ",p2(se1),", under alternative hypothesis, mean difference = ",mu2," & SE = ",p2(se2),".\n This requires N per group = ",p0(n1),", to have power= ",1-beta," and alpha two-sided = ",alpha,""))
       
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
         
         points(2, 0,        col="darkred", pch=4, cex=3, lwd=3)
         points(3.019800, 0, col="darkorange", pch=4, cex=3, lwd=3)
         points(3.584303, 0, col="darkgreen", pch=4, cex=3, lwd=3)
         points(mu2,      0, col="purple", pch=4, cex=3, lwd=3)
         
         legend(x=X11, Y*1 ,  "Legend:",
                legend=c(
                  expression(paste("Power (1-",beta,")")),
                  expression(paste("Type II error (",beta,")   ")),
                  expression(paste("Type I error (",alpha,")"))),
                fill=c("green","#004987","red"),
                cex=1)
       
       
       
       
       
      }  
      
   
    
  })
  
  #---------------------------------------------------------------------------
  # take homes tab
  output$norm.plot1 <- renderPlot({ 
     
    sd <- input$sd1
    A <-  input$alpha
    B <-  input$beta
    
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
          main=paste0("           "))
    
    curve(dnorm(x, mean =  mu2, sd=sd), lwd=2, add=TRUE , xlab='Treatment Effect', col="forestgreen")
    
    abline(v= u975,          col="blue",  lwd=1,  lty=3)
    abline(v= mu2, col="blue",  lwd=1,  lty=3)
    abline(v=0,                  col="blue", lwd=.5, lty=3)  
    
    gap=0.001
    
    xx <-    seq(  qnorm(a)*sd, upper,  by=gap)
    xxx <-    seq(  lower, qnorm(a)*sd,  by=gap)
    
    polygon(x = c(qnorm(a)*sd,                       xx,  upper),
            y = c(0 , dnorm(mean=0, sd=sd,     xx),     0),
            col="red")
    
    #type II error area, beta
    polygon(x = c(lower,                           xxx,  qnorm(a)*sd),
            y = c(0 , dnorm(mean=mu2, sd=sd,      xxx),    0),
            col="forestgreen")
    
    # +/-se
    text(x=0,y=.26*fact,  labels=paste0("se = +/- ",sd,""),cex= cex1)
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
    
    
    
  
    
    
    
    1- pnorm(qnorm(a)+qnorm(b))
    qnorm(a)/ (qnorm(a)+qnorm(b))
    
    
  }) 
   
  
  #---------------------------------------------------------------------------
  # standard normal
    
    ############################
    
     output$norm.plot <- renderPlot({ 
       
       mu1=0
       sd1=se1=1
       se1=se2=1
       
       #mu2 <- input$mu99
       # beta <- input$beta99
       # alpha <- input$alpha99
       beta <- input$beta
       alpha <- input$alpha
       crit1 <- qnorm(1-as.numeric(alpha/2))
       crit2 <- qnorm(1-as.numeric(beta))   #power
     
       # z of distibution alpha tresholds
       crita <-  mu1 + crit1 * se1  # how many ses above mean
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
             main=paste0("Figure 2: Standard Normal sampling distribution of the null and alternative treatment effects. Power= ",1-beta," and alpha two-sided = ",alpha,"\nUnder the null hypothesis, mean difference = ",p2(mu1)," & SE = ",p2(se1),", under alternative hypothesis, mean difference = ",p2(mu2)," & SE = ",p2(se2),""))
       
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
       
       points(2, 0,        col="darkred", pch=4, cex=3, lwd=3)
       points(3.019800, 0, col="darkorange", pch=4, cex=3, lwd=3)
       points(3.584303, 0, col="darkgreen", pch=4, cex=3, lwd=3)
       points(mu2,      0, col="purple", pch=4, cex=3, lwd=3)
       
       legend(x=X11, Y*1 ,  "Legend:",
              legend=c(
                expression(paste("Power (1-",beta,")")),
                expression(paste("Type II error (",beta,")   ")),
                expression(paste("Type I error (",alpha,")"))),
              fill=c("green","#004987","red"),
              cex=1)
       
   
     
     }) 
    
 
})
# Run the application 
shinyApp(ui = ui, server = server)