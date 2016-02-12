library(shiny)
library(shinyjs)
library(shinythemes)
library(BayesFactor)
library(rjags)
library(MASS)


shinyUI(fluidPage(
  useShinyjs(),
  theme = shinytheme("spacelab"),
  titlePanel("Guess Bayes factor"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("data_points", label = strong("Amount of data points"), min = 10, max = 200, value=50),
      sliderInput("set_cor", strong("Specify correlation"),
                  min = -1, max = 1, value = c(0.4, 0.8), step = 0.1),
      checkboxInput("reg_lines", "Credible regression lines"),
      checkboxInput("reg_mean", "Regression line with mean values of coefficients"),
      selectInput("prior", label = h5("Change the prior scale:"), 
                  choices = list("medium (sqrt(2)/4)" = 1, "wide (1/2)" = 2,
                                 "ultrawide (sqrt(2)/2)" = 3), selected = 1),
      selectInput("category", "Bayes factor is:",
                  choices = list("anecdotal ( < 3)" = 1,
                                 "moderate (3 - 10)" = 2,
                                 "strong (10 - 30)" = 3,
                                 "very strong (30 - 100)" = 4,
                                 "extreme ( > 100)" = 5), selected = 2),
      textInput("answer", "Numeric value:"),
      actionButton("new_plot", label = "New plot"),
      actionButton("submit", "Guess!"),
      br(),
      br(),
      textOutput("correct_answer")      
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Plot",
          br(),
          p("Click on the New plot button to generate new data and scatterplot. You can switch on and off credible regression lines, regression line with mean values. You can also change the prior scale and specify correlation."),     
          p("You should try to guess the Bayes factor. You might choose one of the categorical values or try to guess a numeric value."),
          br(),
          plotOutput("scatter_plot"),
          textOutput("cor"),  
          textOutput("score")  
        ),
      tabPanel(
        "Info",
        br(),
        p("Here is a game of guessing a Bayes factor given some scatter plot with regression lines. 
          Bayes factor is a Bayesian alternative to classical hypothesis testing.",
        a(href="https://en.wikipedia.org/wiki/Bayesian_linear_regression", "Here"), 
        "you can learn more about Bayesian linear regression and",
        a(href="https://en.wikipedia.org/wiki/Bayes_factor", "here"), 
        "about Bayes factor. The prior in this game is Cauchy distribution. More information about the prior you can find",
        a(href="https://cran.r-project.org/web/packages/BayesFactor/BayesFactor.pdf#page=45", "here.")),   
        p("The scale (r) can be set to three different values and therefore affect the result. Figure 1, 2, 3 show density plots of Cauchy distribution with different scales.",
        align="justify"),
        fluidRow(
          column(width=4,
                 plotOutput("fig1"),
                 p("Figure 1. Cauchy prior with scale sqrt(2)/4 (medium)")
          ),
          column(width=4,
                 plotOutput("fig2"),
                 p("Figure 2. Cauchy prior with scale 1/2 (wide)")
          ),
          column(width=4,
                 plotOutput("fig3"),
                 p("Figure 1. Cauchy prior with scale sqrt(2)/2 (ultrawide)")
          )
        ),
        p("Answer can be inputted as a categorical value or as a numeric value. Categorical values were chosen according to the classification in",
          a(href="http://bayesmodels.com/", "Lee & Wagenmakers 2013."),
          "Choosing categorical value is much easier than numeric, therefore, for the right categorical value you will get 1 point, for the right numerical value -- 5 points.")
        
    ))
))))