library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sample Size, Mean, Variance | and effect on population mean estimation"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      style = "position:fixed;width:22%;",
      # width = 3,
      numericInput("pop_sz",
                   label = "Enter size of population",
                   value = 10000),
      numericInput("num_samples",
                   label = "Enter number of samples to take for each sample size",
                   value = 200),
      numericInput("seed_num",
                   label = "Enter a seed number for reproducibility",
                   value = 42),
      sliderInput("sample_sz",
                  label = "select range of sample sizes",
                  min = 2, max = 50, value = c(2,10), step = 1),
      sliderInput("bias_adju",
                  label = "enter adjuster to compensate for bias in estimating population variance",
                  min = 0, max = 2, value = 0, step = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput("population_size"),
      htmlOutput("number_of_samples"),
      htmlOutput("seed_number"),
      plotOutput("population_distribution"),
      HTML("<br><br>"),
      htmlOutput("sample_sizes_vec"),
      htmlOutput("bias_adjuster"),
      htmlOutput("graph_input_df_dim"),
      HTML("<br><br>"),
      plotOutput("sample_mean_variance_size"),
      HTML("<br>"),
      htmlOutput("explanation"),
      HTML("<br><br>")
    )
  )
))
