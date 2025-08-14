#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(econGame)

# Define UI for application
ui <- fluidPage(
  titlePanel("Entry Game"),
  sidebarPanel(
    textInput(
      inputId = "filename",
      label = "Enter the filename of the Excel Workbook with the output.",
      value = NULL
    ),
    textInput(
      inputId = "user",
      label = "Enter the user ID for the OneDrive account with the output.",
      value = NULL
    ),
    textInput(
      inputId = "team",
      label = "Enter the team or group ID for the OneDrive account with the output.",
      value = NULL
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
    numericInput(
      inputId = "round",
      label = "Enter the round you want to display.",
      value = 1
    ),
    actionButton("go", "Load New Responses")
  ),
  mainPanel(tabsetPanel(
    tabPanel("Equilibrium", tableOutput("equilibrium")),
    tabPanel("Plot", plotOutput(
      "plot", width = '800px', height = '800px'
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
    g <- entryGame(filename, user, drive, team, subdir)
    g
  })
  output$equilibrium <- renderTable({
    g <- data()
    e <- matrix(c(
      unlist(g$equilibria$Q_c[[input$round]]),
      unlist(g$equilibria$Q_s[[input$round]]),
      unlist(g$equilibria$P_c[[input$round]]),
      unlist(g$equilibria$P_s[[input$round]])
    ),
    nrow = 2,
    ncol = 2)
    colnames(e) <- c("Quantity", "Price")
    rownames(e) <- c("Corn", "Soybeans")
    e
  }, rownames = TRUE)
  output$plot <- renderPlot({
    g <- data()
    plot(g,
         round = input$round,
         nrow = 1,
         ncol = 2)
  })
  output$grades <- renderTable({
    g <- data()
    g$grades
  })
  output$results <- renderTable({
    g <- data()
    subset(g$results, Round == input$round)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
