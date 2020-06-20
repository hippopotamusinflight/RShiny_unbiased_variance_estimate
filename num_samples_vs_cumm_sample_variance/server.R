library(shiny)
library(tidyverse)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$population_size <- renderText({
    paste("size of population = ", input$pop_sz, " (uniform distribution with min=0, max=200)")
  })  
  output$sample_size <- renderText({
    paste("size of each sample = ", input$samp_sz)
  })
  output$number_of_samples <- renderText({
    paste("number of samples taken = ", input$num_samples)
  })
  output$seed_number <- renderText({
    paste("seed number = ", input$seed_num)
  })

  population <- reactive({
    set.seed(input$seed_num)
    runif(input$pop_sz, 0, 200) %>% floor()
  })
  output$population_mean <- renderText({
    pop_mean <- round(mean(population()), 3)
    paste("population mean = ", pop_mean)
  })
  output$population_variance <- renderText({
    pop_var <- round(sum(((population() - mean(population()))^2))/length(population()), 3)
    paste("population variance = ", pop_var)
  })
  
  graph_input_df <- function(n_adjuster){
    pop_var <- sum(((population() - mean(population()))^2))/length(population())
    pop_mean <- mean(population())
    
    samp_var_vector = c()
    cumm_samp_var_vector = c()
    
    samp_mean_diff_vector = c()
    samp_pop_mean_var_vector = c()
    
    for (i in seq(input$num_samples)){
      samp <- sample(population(), input$samp_sz, replace = TRUE)
      samp_var <- sum(((samp - mean(samp))^2))/(length(samp)-n_adjuster)
      samp_var_vector <- c(samp_var_vector, samp_var)
      cumm_samp_var_vector <- c(cumm_samp_var_vector, mean(samp_var_vector))
      
      samp_mean_diff_vector <- c(samp_mean_diff_vector, mean(samp) - pop_mean)
      samp_pop_mean_var <- sum(((samp - pop_mean)^2))/(length(samp)-n_adjust)
      samp_pop_mean_var_vector <- c(samp_pop_mean_var_vector, samp_pop_mean_var)
    }
    return(cbind(seq(input$num_samples),
                 cumm_samp_var_vector,
                 samp_mean_diff_vector,
                 variance_diff = (samp_var_vector - samp_pop_mean_var_vector)))
  }
  
  output$sample_variance_biased <- renderPlot({
    n_adjust <- 0
    par(mfrow=c(1,2))
    plot(x = graph_input_df(n_adjust)[,1],
         y = graph_input_df(n_adjust)[,2],
         xlim=c(0, input$num_samples), 
         ylim=c(pop_var - 2500, pop_var + 2500),
         xlab = "number of samples",
         ylab = "cumulative sample variance (biased)",
         pch = 20,
         cex = 0.3)
    abline(h = pop_var, col="blue")
    
    # cannot get the parabola shape...
    plot(x = graph_input_df(n_adjust)[,3],
         y = graph_input_df(n_adjust)[,4],
         # xlim=c(,), 
         # ylim=c(,),
         xlab = "sample mean - population mean",
         ylab = "sample variance - sample variance calculated from population mean",
         pch = 20,
         cex = 0.3)
    abline(h = 0, col="blue")
    abline(v = 0, col="blue")
  })

  output$sample_variance_unbiased1 <- renderPlot({
    n_adjust <- 1
    par(mfrow=c(1,2))
    plot(x = graph_input_df(n_adjust)[,1],
         y = graph_input_df(n_adjust)[,2],
         xlim=c(0, input$num_samples), 
         ylim=c(pop_var - 2500, pop_var + 2500),
         xlab = "number of samples",
         ylab = "cumulative sample variance (biased)",
         pch = 20,
         cex = 0.3)
    abline(h = pop_var, col="blue")
    
    plot(x = graph_input_df(n_adjust)[,3],
         y = graph_input_df(n_adjust)[,4],
         # xlim=c(,), 
         # ylim=c(,),
         xlab = "sample mean - population mean",
         ylab = "sample variance - sample variance calculated from population mean",
         pch = 20,
         cex = 0.3)
    abline(h = 0, col="blue")
    abline(v = 0, col="blue")
  })
  
  output$sample_variance_unbiased2 <- renderPlot({
    n_adjust <- 2
    par(mfrow=c(1,2))
    plot(x = graph_input_df(n_adjust)[,1],
         y = graph_input_df(n_adjust)[,2],
         xlim=c(0, input$num_samples), 
         ylim=c(pop_var - 2500, pop_var + 2500),
         xlab = "number of samples",
         ylab = "cumulative sample variance (biased)",
         pch = 20,
         cex = 0.3)
    abline(h = pop_var, col="blue")
    
    plot(x = graph_input_df(n_adjust)[,3],
         y = graph_input_df(n_adjust)[,4],
         # xlim=c(,), 
         # ylim=c(,),
         xlab = "sample mean - population mean",
         ylab = "sample variance - sample variance calculated from population mean",
         pch = 20,
         cex = 0.3)
    abline(h = 0, col="blue")
    abline(v = 0, col="blue")
  })
})
