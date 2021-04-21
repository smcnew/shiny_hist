# t-test tutorial

library(shiny)
library(scales)
ui <- fluidPage(
  sliderInput (inputId = "num",
               label = "How many samples from each group?",
               value = "100", min = 3, max = 300),
  sliderInput (inputId = "diff",
               label = "What is the true difference in mean height?",
               value = "6", min = 0, max = 10),
  sliderInput(inputId = "var",
              label = "How much variation is there within groups? (i.e. standard deviation)",
              value = "3", min = 1, max = 7),
  plotOutput("hist"),
  verbatimTextOutput("results")
)
server <- function(input, output) {
  men <- reactive({rnorm(n = input$num, mean = 69, sd = input$var)})
  women <- reactive({rnorm(n = input$num, mean = 69 - input$diff, sd = input$var)})
  output$hist <- renderPlot({
    hist(men(), ylab = "Number of observations", xlab = "Height in inches",
       main = "Difference in male and female height", col = alpha("#CCBB44", 0.4),
       xlim = c(40, 100))
  hist(women(), add = T, col = alpha("#BB5566", 0.4))
  legend ("topright", legend = c("men", "women"), cex = 1.4,
          pt.bg = c(alpha("#CCBB44", 0.4), alpha("#BB5566", 0.4)), pch = 22)
  })
  output$results <- renderPrint(t.test(men(), women()))
}
shinyApp(ui = ui, server = server)

