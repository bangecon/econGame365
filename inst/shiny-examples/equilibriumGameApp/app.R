#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(econGame365)

# Define UI for application
ui <- fluidPage(
  titlePanel("Market Equilibrium Game"),
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
      value = NULL
    ),
    textInput(
      inputId = "subdir",
      label = "Enter the subdirectory path of the Excel Workbook with the output.",
      value = NULL
    ),
    actionButton("go", "Load New Responses"),
  ),
  mainPanel(tabsetPanel(
    tabPanel("Schedule", tableOutput("schedule")),
    tabPanel("Plot", plotOutput(
      "plot", width = '600px', height = '600px'
    )),
    tabPanel("Results", tableOutput("results")),
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
    g <- equilibriumGame(filename, user, drive, team, subdir)
    g
  })
  output$schedule <- renderTable({
    g <- data()
    g$schedule
  })
  output$plot <- renderPlot({
    g <- data()
    plot(g)
  })
  output$grades <- renderTable({
    g <- data()
    g$grades
  })
  output$results <- renderTable({
    g <- data()
    subset(g$results)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
