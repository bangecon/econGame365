#
# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.
#
library(econGame)

# Define UI for application
ui <- fluidPage(
  titlePanel("Ultimatum Game"),
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
    textInput(
      inputId = "partners",
      label = "Random or Student-Chosen Partners?",
      value = "random"
    ),
    numericInput(
      inputId = "endowment",
      label = "Enter the initial endowment that students receive.",
      value = 5
    ),
    actionButton("go", "Load New Responses"),
    numericInput(
      inputId = "seed",
      label = "Random Seed",
      value = 8675309
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot", width = '600px', height = '600px')),
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
    endowment <- input$endowment
    seed <- input$seed
    g <- ultimatumGame(
      resultsFilename,
      rolesFilename,
      user,
      drive,
      team,
      subdir,
      partners,
      endowment,
      c("Proposer", "Responder"),
      seed
    )
    g
  })
   output$grades <- renderTable({
     g <- data()
     g$grades
  })
  output$results <- renderTable({
    g <- data()
    g$results
  })
  output$plot <- renderPlot({
    g <- data()
    plot(g)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
