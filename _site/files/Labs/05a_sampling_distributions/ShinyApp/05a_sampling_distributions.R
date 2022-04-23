set.seed(1234)
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(openintro)
library(infer)

global_monitor <- tibble(
  scientist_work = c(rep("Benefits", 80000), rep("Doesn't benefit", 20000))
)
samp1 <- global_monitor %>%
  sample_n(50)
sample_props50 <- global_monitor %>%
  rep_sample_n(size = 50, reps = 15000, replace = TRUE) %>%
  count(scientist_work) %>%
  mutate(p_hat = n /sum(n)) %>%
  filter(scientist_work == "Doesn't benefit")




  ui <- fluidPage(
    titlePanel("Foundations for statistical inference - Sampling distributions - Exercise 6"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        
        
        selectInput("outcome",
                    "Outcome of interest:",
                    choices = c("Benefits", "Doesn't benefit"),
                    selected = "Doesn't benefit"),
        
        numericInput("n_samp",
                     "Sample size:",
                     min = 1,
                     max = nrow(global_monitor),
                     value = 30),
        
        numericInput("n_rep",
                     "Number of samples:",
                     min = 1,
                     max = 30000,
                     value = 15000),
        
        hr(),
        
        sliderInput("binwidth",
                    "Binwidth:",
                    min = 0, max = 0.25,
                    value = 0.01,
                    step = 0.005)
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("sampling_plot"),
        textOutput("sampling_mean"),
        textOutput("sampling_se")
      )
    )
  )
  
  server <- function(input, output) {
    
    # create sampling distribution
    sampling_dist <- reactive({
      global_monitor %>%
        rep_sample_n(size = input$n_samp, reps = input$n_rep, replace = TRUE) %>%
        count(scientist_work) %>%
        mutate(p_hat = n /sum(n)) %>%
        filter(scientist_work == input$outcome)
    })
    
    # plot sampling distribution
    output$sampling_plot <- renderPlot({
      
      ggplot(sampling_dist(), aes(x = p_hat)) +
        geom_histogram(binwidth = input$binwidth) +
        xlim(0, 1) +
        labs(
          x = paste0("p_hat (", input$outcome, ")"),
          title = "Sampling distribution of p_hat",
          subtitle = paste0("Sample size = ", input$n_samp, " Number of samples = ", input$n_rep)
        ) +
        theme(plot.title = element_text(face = "bold", size = 16))
    })
    
    ggplot(data = sample_props50, aes(x = p_hat)) +
      geom_histogram(binwidth = 0.02) +
      labs(
        x = "p_hat (Doesn't benefit)",
        title = "Sampling distribution of p_hat",
        subtitle = "Sample size = 50, Number of samples = 15000"
      )
    
    # mean of sampling distribution
    output$sampling_mean <- renderText({
      paste0("Mean of sampling distribution = ", round(mean(sampling_dist()$p_hat), 2))
    })
    
    # mean of sampling distribution
    output$sampling_se <- renderText({
      paste0("SE of sampling distribution = ", round(sd(sampling_dist()$p_hat), 2))
    })
  }

shinyApp(ui = ui, server = server)
