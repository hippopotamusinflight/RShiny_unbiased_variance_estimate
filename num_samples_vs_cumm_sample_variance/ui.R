library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Number of Samples vs Cumulative Sample Variance"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("pop_sz",
                   label = "Enter size of population",
                   value = 10000),
      numericInput("samp_sz",
                   label = "Enter size of each sample",
                   value = 5),
      numericInput("num_samples",
                   label = "Enter number of samples to take",
                   value = 2500),
      numericInput("seed_num",
                   label = "Enter a seed number for reproducibility",
                   value = 42)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("population_size"),
      textOutput("sample_size"),
      textOutput("number_of_samples"),
      textOutput("seed_number"),
      textOutput("population_mean"),
      textOutput("population_variance"),
      plotOutput("sample_variance_biased"),
      plotOutput("sample_variance_unbiased1"),
      plotOutput("sample_variance_unbiased2")
    )
  )
))
