#
# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.
#
library(econGame)

# Define UI for application
ui <- fluidPage(
  titlePanel("Public Good Game"),
  sidebarPanel(
    textInput(
      inputId = "filename",
      label = "Enter the filename of the workbook with the output.",
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
    numericInput(
      inputId = "endowment",
      label = "Enter the initial endowment that students receive.",
      value = 5
    ),
    numericInput(
      inputId = "benefit",
      label = "Enter the group return on contributions.",
      value = 0.1
    ),
    actionButton("go", "Load New Responses")
  ),
  mainPanel(tabsetPanel(
    tabPanel("Plot",
             plotOutput("outcomePlot")),
    tabPanel("Results", tableOutput("blindedResults")),
    tabPanel("Grades", tableOutput("grades"))
  ))
)

# Define server logic
server <- function(input, output) {
  data <- eventReactive(input$go, {
    filename <- input$filename
    user <- input$user
    team <- input$team
    drive <- input$drive
    subdir <- input$subdir
    endowment <- input$endowment
    benefit <- input$benefit
    g <- publicgoodGame(filename, user, drive, team, subdir, endowment, benefit)
    g
  })
  output$grades <- renderTable({
    g <- data()
    g$grades
  })
  output$blindedResults <- renderTable({
    g <- data()
    g$blindedResults
  })
  output$outcomePlot <- renderPlot({
    g <- data()
    plot(g)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
