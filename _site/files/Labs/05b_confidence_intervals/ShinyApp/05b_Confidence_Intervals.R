library(tidyverse)
library(openintro)
library(infer)
us_adults <- tibble(
  climate_change_affects = c(rep("Yes", 62000), rep("No", 38000))
)
n_s <- 60
samp <- us_adults %>%
  sample_n(size = n_s)
reps = 1000
store_ci <- function(i, n, reps, conf_level, success) {
  us_adults %>%
    sample_n(size = n) %>%
    specify(response = climate_change_affects, success = success) %>%
    generate(reps, type = "bootstrap") %>%
    calculate(stat = "prop") %>%
    get_ci(level = conf_level) %>%
    rename(
      x_lower = names(.)[1],
      x_upper = names(.)[2]
    )
}

shinyApp(
  ui <- fluidPage(
    h4("Confidence intervals for the proportion of US adults who think 
     climate change"),
    
    h4(selectInput("success", "",
                   choices = c(
                     "is affecting their local community" = "Yes",
                     "is not affecting their local community" = "No"
                   ),
                   selected = "Yes", width = "50%"
    )),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        numericInput("n_samp",
                     "Sample size for a single sample from the population:",
                     min = 1,
                     max = 1000,
                     value = 60
        ),
        
        hr(),
        
        numericInput("conf_level",
                     "Confidence level",
                     min = 0.01,
                     max = 0.99,
                     value = 0.95,
                     step = 0.05
        ),
        
        hr(),
        
        radioButtons("n_ci",
                     "Number of confidence intervals:",
                     choices = c(10, 25, 50, 100),
                     selected = 50, inline = TRUE
        ),
        
        actionButton("go", "Go")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("ci_plot")
      )
    )
  ),
  
  server <- function(input, output) {
    
    # set true p
    p <- reactive(ifelse(input$success == "Yes", 0.62, 0.38))
    
    # create df_ci when go button is pushed
    df_ci <- eventReactive(input$go, {
      map_dfr(1:input$n_ci, store_ci,
              n = input$n_samp,
              reps = 1000, conf_level = input$conf_level,
              success = input$success
      ) %>%
        mutate(
          y_lower = 1:input$n_ci,
          y_upper = 1:input$n_ci,
          capture_p = ifelse(x_lower < p() & x_upper > p(), "Yes", "No")
        )
    })
    
    # plot df_ci
    output$ci_plot <- renderPlot({
      ggplot(df_ci()) +
        geom_segment(aes(x = x_lower, y = y_lower, xend = x_upper, yend = y_upper, color = capture_p)) +
        geom_point(aes(x = x_lower, y = y_lower, color = capture_p)) +
        geom_point(aes(x = x_upper, y = y_upper, color = capture_p)) +
        geom_vline(xintercept = p(), color = "darkgray") +
        labs(
          y = "", x = "Bounds of the confidence interval",
          color = "Does the interval capture the true population proportion?"
        ) +
        theme(legend.position = "bottom")
    })
  },
  options = list(height = 700)
)

shinyApp(ui = ui, server = server)