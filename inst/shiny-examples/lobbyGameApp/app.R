#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(econGame365)

# Define UI for application
ui <- fluidPage(
  titlePanel("Lobby Lottery Auction Game"),
  sidebarPanel(
    textInput(
      inputId = "filename",
      label = "Enter the filename of the Excel Workbook with the output.",
      value = NULL
    ),
    textInput(
      inputId = "user",
      label = "Enter the user ID for the OneDrive account with the output.",
      value = "jb0616165"
    ),
    textInput(
      inputId = "team",
      label = "Enter the team or group ID for the OneDrive account with the output.",
      value = "sau.edu"
    ),
    textInput(
      inputId = "drive",
      label = "Enter the letter of the local drive for path containing the Excel Workbook with the output.",
      value = "c"
    ),
    textInput(
      inputId = "subdir",
      label = "Enter the subdirectory path of the Excel Workbook with the output.",
      value = NULL
    ),
    numericInput(
      inputId = "endowment",
      label = "Enter the endowment each player receives at the start of the game.",
      value = 5
    ),
    numericInput(
      inputId = "prize",
      label = "Enter the ID of the Google Sheet with the output.",
      value = 4
    ),
    numericInput(
      inputId = "seed",
      label = "Enter the random seed for selecting the lottery winner.",
      value = 8675309
    ),
    actionButton("go", "Load New Responses")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Winner", tableOutput("winner")),
      tabPanel("Results", tableOutput("results")),
      tabPanel("Grades", tableOutput("grades"))
    )
  )
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
    prize <- input$prize
    seed <- input$seed
    g <- lobbyGame(filename, user, drive, team, subdir, endowment, prize, seed)
    g
  })
  output$winner <- renderTable({
    g <- data()
    g$winner
  })
  output$results <- renderTable({
    g <- data()
    g$results
  })
  output$grades <- renderTable({
    g <- data()
    g$grades
  })
}

# Run the application
shinyApp(ui = ui, server = server)
