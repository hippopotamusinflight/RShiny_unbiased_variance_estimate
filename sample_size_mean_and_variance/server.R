library(shiny)
library(tidyverse)
library(ggplot2)
library(cowplot)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$population_size <- renderText({
    paste("size of population = ", "<b>",input$pop_sz,"</b>", " (uniform distribution with min=1, max=20)")
  })
  output$number_of_samples <- renderText({
    paste("number of samples taken for each sample size = ", "<b>",input$num_samples,"</b>")
  })
  output$seed_number <- renderText({
    paste("seed number = ", "<b>",input$seed_num,"</b>")
  })
  
  get_population <- reactive({
    set.seed(input$seed_num)
    runif(input$pop_sz, 0, 20) %>% floor()
  })
  get_population_mean <- reactive({
    mean(get_population())
  })
  get_population_varn <- reactive({
    sum(((get_population() - mean(get_population()))^2))/length(get_population())
  })
  
  output$population_distribution <- renderPlot({
    subtitl <- paste("N =", length(get_population()), 
                     ", mean = ", round(get_population_mean(),3), 
                     ", variance = ", round(get_population_varn(), 3))
    ggplot(data.frame(pop=get_population()), aes(pop)) + 
      geom_histogram(binwidth = 1,
                     colour='darkblue',
                     fill='lightblue',
                     size=0.1) +
      labs(x = "value of random variable",
           y = "counts",
           title = "population distribution",
           subtitle = subtitl
      ) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })

  output$sample_sizes_vec <- renderText({
    paste("sequence of sample sizes from ", "<b>",input$sample_sz[1],"</b>", " to ", "<b>",input$sample_sz[2],"</b>")
  })
  output$bias_adjuster <- renderText({
    paste("population mean estimate, bias adjuster = ",
          "<b>",input$bias_adju,"</b>")
  })
    
  get_graph_input_df <- reactive({

    population <- get_population()
    n_adjuster <- input$bias_adju
    pop_var <- sum(((population - mean(population))^2))/length(population)

    sample_size_vec = c()
    sample_varn_vec_all_sample_sizes = c()
    sample_mean_vec_all_sample_sizes = c()

    for (i in input$sample_sz[1]:input$sample_sz[2]){

      sample_varn_vec_per_sample_size = c()
      sample_mean_vec_per_sample_size = c()

      for (j in seq(input$num_samples)){
        sample_size_vec <- c(sample_size_vec, i)
        sampl <- sample(population, i, replace = TRUE)
        sample_varn <- sum(((sampl - mean(sampl))^2))/(length(sampl)-n_adjuster)
        sample_varn_vec_per_sample_size <- c(sample_varn_vec_per_sample_size, sample_varn)
        sample_mean_vec_per_sample_size <- c(sample_mean_vec_per_sample_size, mean(sampl))
      }
      sample_varn_vec_all_sample_sizes <- c(sample_varn_vec_all_sample_sizes, sample_varn_vec_per_sample_size)
      sample_mean_vec_all_sample_sizes <- c(sample_mean_vec_all_sample_sizes, sample_mean_vec_per_sample_size)
    }

    return(data.frame(sample_sizes = sample_size_vec,
                      sample_num = rep(seq(input$num_samples), length(input$sample_sz[1]:input$sample_sz[2])),
                      sample_varns = sample_varn_vec_all_sample_sizes,
                      sample_v_div_pop_v = sample_varn_vec_all_sample_sizes/pop_var,
                      sample_means = sample_mean_vec_all_sample_sizes))
  })
  
  output$graph_input_df_dim <- renderText({
    paste("dimensions of graph input df = ", "<b>",toString(dim(get_graph_input_df())),"</b>")
  })
  get_bar_input_df <- reactive({
    get_graph_input_df() %>% 
      group_by(sample_sizes) %>% 
      summarise(mean_sv_div_pv = mean(sample_v_div_pop_v))
  })
  
  output$sample_mean_variance_size <- renderPlot({

    plt1 <- ggplot(data = get_graph_input_df(), aes(x=sample_means, y=sample_varns, color=sample_sizes)) +
      scale_color_gradient(low="blue", high="red") +
      geom_point(size=0.3, shape=20) +
      geom_hline(yintercept = pop_var, color = "grey29") +
      geom_vline(xintercept = pop_mean, color = "grey29") +
      labs(x = "sample mean",
           y = "sample variance",
           title = "sample mean vs sample variance\ncomparing to population mean and variance") +
      theme(plot.title = element_text(hjust = 0.5))

    plt2 <- ggplot(data = get_bar_input_df(), aes(x=sample_sizes, y=mean_sv_div_pv, fill=sample_sizes)) +
      scale_fill_gradient(low = "blue", high = "red") +
      geom_bar(stat = "identity") +
      scale_x_continuous(breaks = seq(input$sample_sz[1],input$sample_sz[2],by=1)) +
      ylim(0, 2) +
      labs(x = "sample sizes",
           y = "sample variance / population variance\n-> mean groupby sample size",
           title = "sample size vs variance\n") +
      theme(plot.title = element_text(hjust = 0.5))
    
    plot_grid(plt1, plt2)
  })
  
  output$explanation <- renderText({
    if (input$bias_adju == 0) {
      text_out <- paste("graphs shows majority of small samples (< 4) UNDER estimate population variance",
                   "for small samples:",
                   "n = 2, biased sample variance / population variance = ~1/2 = 0.5",
                   "n = 3, biased sample variance / population variance = ~2/3 = 0.66",
                   "n = 4, biased sample variance / population variance = ~3/4 = 0.75",
                   "n = 5, biased sample variance / population variance = ~4/5 = 0.8",
                   "therefore empirically, biased estimate approaches (n-1)/n of population variance",
                   sep = "<br>")
    }
    if (input$bias_adju == 1) {
      text_out <- paste("(n-1) leads to unbiased estimate",
                   "graphs shows even for small samples (<4), does not under or over estimate population variance",
                   paste("no matter the sample size, sample variance / population variance = ~", "<b>","1","</b>"),
                   sep = "<br>")
    }
    if (input$bias_adju == 2) {
      text_out <- paste("graphs shows majority of small samples (even as large as 50 samples) OVER estimate population variance<br>")
    }
    text_out
  })
})
