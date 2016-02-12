library(shiny)
library(shinyjs)
library(shinythemes)
library(ggplot2)
library(BayesFactor)
library(rjags)
library(MASS)

shinyServer(  
  
  function(input, output) {
    observe({
   
      plot1 = rcauchy(100, 0, sqrt(2)/4)
      plot2 = rcauchy(100, 0, 1/2)
      plot3 = rcauchy(100, 0, sqrt(2)/2)
      
      
      # plots for prior
      
      output$fig1 = renderPlot({
        df = data.frame(plot1)
        dfs <- stack(df)
        ggplot(dfs, aes(x=values)) + geom_density() + xlim(-10,10) + ylim(0,1)
      })
      
      output$fig2 = renderPlot({
        df = data.frame(plot2)
        dfs <- stack(df)
        ggplot(dfs, aes(x=values)) + geom_density() + xlim(-10,10) + ylim(0,1)
      })
      
      output$fig3 = renderPlot({
        df = data.frame(plot3)
        dfs <- stack(df)
        ggplot(dfs, aes(x=values)) + geom_density() + xlim(-10,10) + ylim(0,1)
      })      
    })
    
    # generate data, do all calculations
    generate = reactive({
      
      # dependency on the "New plot" click
      input$new_plot
      
      # generate random data
      isolate({
      amount = input$data_points
      
      # get correlation values
      lower = input$set_cor[1]
      upper = input$set_cor[2]
      
      if (input$set_cor[1] == -1 & input$set_cor[2] == -1) {
          lower = -0.99
          upper = -0.99
        }
      
      if (input$set_cor[1] == 1 & input$set_cor[2] == 1) {
        lower = 0.99
        upper = 0.99
        }
      
      r = runif(1, lower, upper)
      raw = mvrnorm(amount, mu = c(0,0), Sigma = matrix(c(1,r,r,1), ncol = 2),
                     empirical = TRUE)
      y = raw[,1]
      x = raw[,2]

      df = data.frame(x, y)  
      
      # set scale
      if (input$prior == 1) {
        scale_jags = sqrt(2)/4
      }
      if (input$prior == 2) {
        scale_jags = 1/2
      }
      if (input$prior == 3) {
        scale_jags = sqrt(2)/2
      }
      
      # using rjags to estimate intercept for visualization
      
      # according to the documentation on regressionBF
      # https://cran.r-project.org/web/packages/BayesFactor/BayesFactor.pdf#page=45
      
      modelString = "
      model{
          g ~ dgamma(1/2, r/2)
          b0 ~ dnorm(0, g)
          b1 ~ dnorm(0, g)
          varE ~ dunif(0, 1000)
          for (i in 1: length(x)){
          y[i] ~ dnorm(b0 + b1 * x[i], 1/varE)
        }
      }
      "      
      dat = list('x' = x, 'y' = y, 'r' = scale_jags)
      
      jagsModel = jags.model(file = textConnection(modelString), data = dat, n.chains = 2)
      samplesJags = coda.samples(jagsModel, variable.names = c('b0', 'b1'), n.iter = 10000)      
                
      # get lower and upper values of b0 and b1 (for each chain)
      HPD = as.data.frame(HPDinterval(samplesJags))
      b0_lower_1 = HPD$lower[1]
      b0_lower_2 = HPD$lower.1[1]
      b1_lower_1 = HPD$lower[2]
      b1_lower_2 = HPD$lower.1[2]
          
      b0_upper_1 = HPD$upper[1]
      b0_upper_2 = HPD$upper.1[1]
      b1_upper_1 = HPD$upper[2]
      b1_upper_2 = HPD$upper.1[2]
          
      # calculate means for lower and upper values of b0 and b1 (two chains together)
      b0_lower_mean = mean(c(b0_lower_1, b0_lower_2))
      b0_upper_mean = mean(c(b0_upper_1, b0_upper_2))
      b1_lower_mean = mean(c(b1_lower_1, b1_lower_2))
      b1_upper_mean = mean(c(b1_upper_1, b1_upper_2))
      
      # get mean b0 and b1
      s = summary(samplesJags)
      b0_mean = s$statistics[,"Mean"][1]
      b1_mean = s$statistics[,"Mean"][2]
      })
      
      # return the list of values needed for plots
      list(x = x, y = y, df = df, b0_lower_mean = b0_lower_mean, 
           b0_upper_mean = b0_upper_mean, b1_lower_mean = b1_lower_mean, 
           b1_upper_mean = b1_upper_mean,
           b0_mean = b0_mean,
           b1_mean = b1_mean
           )
    })
    
    # calculate Bayes factor
    calculate_bf = reactive({
      x = generate()$x
      y = generate()$y 
      df = generate()$df
      
      # set scale
      if (input$prior == 1) {
        scale_lm = "medium"
      }
      if (input$prior == 2) {
        scale_lm = "wide"
      }
      if (input$prior == 3) {
        scale_lm = "ultrawide"
      }
        
      bf = lmBF(y ~ x, data = df, rscaleCont = scale_lm) # default is medium, sqrt(2)/4
      list(bf = bf)
    })
    
      
    # show plot
    output$scatter_plot = renderPlot({    
      
      # if "New plot" button is not pressed -- show nothing
      if (input$new_plot == 0) return () 
            
      # show scatter plot
      x = generate()$x
      y = generate()$y   
      plot(x, y, col="black", bty="l", las="1", pch=19)
      
      # show correlation
      output$cor = renderText({
        isolate({
        if (input$set_cor[1] == -1 & input$set_cor[2] == -1) {
          paste("Correlation: -1")
        }
        
        else if (input$set_cor[1] == 1 & input$set_cor[2] == 1) {
          paste("Correlation: 1")
        }
        
        else {paste("Correlation:", round(cor(x, y),2))}
        })
      })
      
      # show credible regression lines
      if (input$reg_lines) {  
          
        # get values from generate()
        b0_lower_mean = generate()$b0_lower_mean
        b0_upper_mean = generate()$b0_upper_mean
        b1_lower_mean = generate()$b1_lower_mean
        b1_upper_mean = generate()$b1_upper_mean
          
        b0 = b0_lower_mean
        b1 = b1_lower_mean
          
        precision = 15
          
        step_b0 = (b0_upper_mean - b0_lower_mean)/precision
        step_b1 = (b1_upper_mean - b1_lower_mean)/precision
          
        # draw lines in the interval
        while (b1 <= b1_upper_mean) {
          abline(b0, b1, col = "skyblue")  
          b0 = b0 + step_b0
          b1 = b1 + step_b1
        }
          
        # draw boundaries
        abline(b0_upper_mean, b1_upper_mean, col = "pink")
        abline(b0_lower_mean, b1_lower_mean, col = "pink")
      }
      
      # show regression mean line
      if (input$reg_mean) {
      
        # get mean values from generate()
        b0_mean = generate()$b0_mean
        b1_mean = generate()$b1_mean
        abline(b0_mean, b1_mean, col = "darkviolet")
      }    
    })
    
    # set counter for scores
    counter = reactiveValues(
      score = 0
      )
        
    # generate answer
    text_answer = reactive({        
      if (input$submit == 0) return ()
      
      # do not update automatically
      isolate({
        
        # get bf
        bf = calculate_bf()$bf        
        answer = as.data.frame(bf)$bf
        
        # if categorical value is chosen
        if (input$answer == "") {
          
        cat_guess = input$category
        
        if (answer <= 3) {
          cat_bf = 1
        }
        if (answer > 3 & answer <= 10) {
          cat_bf = 2          
        }
        if (answer > 10 & answer <= 30) {
          cat_bf = 3       
        }
        if (answer > 30 & answer <= 100) {
          cat_bf = 4        
        }
        if (answer > 100) {
          cat_bf = 5
        }  
        
        if (cat_bf == cat_guess) {
          response = "Right!"
          counter$score = counter$score + 1
        }
        else {
          response = "Try again."
          }
        }
        
        # if numerical value is given
        else {
          guess = input$answer
          
          # calculate how precise the answer is to show different phrases
          how_good = abs(round(answer - as.double(guess, 2), 2))
          response = ""
          
          if (how_good <= 2) {
            response = "Awesome result!"
            counter$score = counter$score + 5
          }
          if (how_good >= 2 & how_good <= 5) {
            response = "Great!"
          }
          if (how_good >= 5 & how_good <= 20) {
            response = "Almost right."
          }
          if (how_good >= 20 & how_good <= 50) {
            response = "Not really."
          }
          if (how_good >= 50) {
            response = "Try again."
          } 
        }
        
        # return answer and show scores
        result = paste(response, "Bayes factor is", round(answer, digits = 2))           
        output$score = renderText({paste("Score:", counter$score)})
        list(result = result)
      })    
    })
    
    # show answer
    output$correct_answer = renderText({
      
      # clear answer if "New plot" is clicked again
      if (input$new_plot > input$submit) {paste("")}      
      
      # otherwise show answer
      else {text_answer()$result}
    })     
    
    # disable input fields after submitting an answer
    observeEvent(input$submit, {
      disable("answer")
      disable("category")
      disable("submit")
      show("correct_answer")
    })    
    
    # enable again when new plot appears
    observeEvent(input$new_plot, {
      enable("answer")
      enable("category")
      enable("submit")
      reset("answer")
      hide("correct_answer")
    })  
    
    # enable again when prior is changed
    observeEvent(input$prior, {
      enable("answer")
      enable("category")
      enable("submit")
      reset("answer")
      hide("correct_answer")
    })
    
})
  