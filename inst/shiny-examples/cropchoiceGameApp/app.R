#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(econGame)
# Define UI for application
ui <- fluidPage(
  titlePanel("Crop Choice Game"),
  sidebarPanel(
    textInput(
      inputId = "resultsFilename",
      label = "Enter the filename of the workbook with the output.",
      value = NULL
    ),
    textInput(
      inputId = "rolesFilename",
      label = "Enter the filename of the workbook with the Student Names.",
      value = NULL
    ),
    textInput(
      inputId = "user",
      label = "Enter the user ID for the OneDrive account with the output.",
      value = NULL
    ),
    textInput(
      inputId = "team",
      label =
        "Enter the team/group ID for the OneDrive account with the output.",
      value = NULL
    ),
    textInput(
      inputId = "drive",
      label =
        "Enter the letter of the local drive containing the workbook.",
      value = "c"
    ),
    textInput(
      inputId = "subdir",
      label =
        "Enter the subdirectory path of the workbook.",
      value = NULL
    ),
    actionButton("go", "Load New Responses"),
    textInput(
      inputId = "partners",
      label = "Random or Student-Chosen Partners?",
      value = "random"
    ),
    numericInput(
      inputId = "payoff1",
      label = "Enter Anil's payoff for the Rice-Rice outcome.",
      value = 1
    ),
    numericInput(
      inputId = "payoff2",
      label = "Enter Anil's payoff for the Cassava-Rice outcome.",
      value = 4
    ),
    numericInput(
      inputId = "payoff3",
      label = "Enter Anil's payoff for the Rice-Cassava outcome.",
      value = 2
    ),
    numericInput(
      inputId = "payoff4",
      label = "Enter Anil's payoff for the Cassava-Cassava outcome.",
      value = 3
    ),
    numericInput(
      inputId = "seed",
      label = "Random Seed",
      value = 8675309
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Outcomes", tableOutput("payoffMatrix")),
      tabPanel("Plot", plotOutput("outcomePlot")),
      tabPanel("Results", tableOutput("results")),
      tabPanel("Grades", tableOutput("grades"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- eventReactive(input$go, {
    resultsFilename <- input$resultsFilename
    rolesFilename <- input$rolesFilename
    user <- input$user
    team <- input$team
    drive <- input$drive
    subdir <- input$subdir
    partners <- input$partners
    seed <- input$seed
    payoff <- c(input$payoff1, input$payoff2, input$payoff3, input$payoff4)
    g <- cropchoiceGame(
      resultsFilename,
      rolesFilename,
      user,
      drive,
      team,
      subdir,
      partners,
      c("Anil", "Bala"),
      payoff,
      seed
    )
    g
  })
  output$treePlot <- renderPlot({
    g <- data()
    g$tree
  }, width = 600)
  output$outcomePlot <- renderPlot({
    g <- data()
    plot(g)
  }, width = 600)
  output$payoffMatrix <- renderTable({
    g <- data()
    g$payoff
  }, rownames = TRUE, align = 'lcc',
  caption = "Student Payoff Matrix", caption.placement = "top")
  output$results <- renderTable({
    g <- data()
    g$results
  }, align = 'c')
  output$grades <- renderTable({
    g <- data()
    g$grades
  })
}

# Run the application
shinyApp(ui = ui, server = server)
