# Load required packages
library(shiny)
library(ggplot2)

# Define the UI
ui <- fluidPage(
  titlePanel("Central Limit Theorem Illustration"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose Distribution:",
                  choices = c("Uniform", "Exponential", "Binomial")),
      numericInput("n", "Sample Size (n):", value = 30, min = 1, step = 1),
      numericInput("samples", "Number of Samples:", value = 1000, min = 100, step = 100),
      actionButton("draw", "Draw Samples")
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("meanPlot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  observeEvent(input$draw, {
    n <- input$n
    samples <- input$samples
    dist <- input$dist
    
    # Generate the samples based on the selected distribution
    sample_data <- replicate(samples, {
      if (dist == "Uniform") {
        sample(runif(n), n, replace = TRUE)
      } else if (dist == "Exponential") {
        sample(rexp(n, rate = 1), n, replace = TRUE)
      } else if (dist == "Binomial") {
        sample(rbinom(n, size = 10, prob = 0.5), n, replace = TRUE)
      }
    })
    
    # Calculate the sample means
    sample_means <- colMeans(sample_data)
    
    # Plot the original distribution
    output$distPlot <- renderPlot({
      if (dist == "Uniform") {
        data <- data.frame(x = runif(10000))
        ggplot(data, aes(x = x)) + 
          geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
          ggtitle("Original Distribution (Uniform)") +
          theme_minimal()
      } else if (dist == "Exponential") {
        data <- data.frame(x = rexp(10000))
        ggplot(data, aes(x = x)) + 
          geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
          ggtitle("Original Distribution (Exponential)") +
          theme_minimal()
      } else if (dist == "Binomial") {
        data <- data.frame(x = rbinom(10000, size = 10, prob = 0.5))
        ggplot(data, aes(x = x)) + 
          geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black") +
          ggtitle("Original Distribution (Binomial)") +
          theme_minimal()
      }
    })
    
    # Plot the distribution of the sample means
    output$meanPlot <- renderPlot({
      data <- data.frame(x = sample_means)
      ggplot(data, aes(x = x)) + 
        geom_histogram(aes(y = ..density..), bins = 30, fill = "orange", color = "black") +
        stat_function(fun = dnorm, args = list(mean = mean(sample_means), sd = sd(sample_means)), 
                      color = "red", linetype = "dashed") +
        ggtitle("Distribution of Sample Means") +
        theme_minimal()
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
