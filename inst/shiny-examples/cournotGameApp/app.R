#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(econGame365)

# Define UI for application
ui <- fluidPage(
  titlePanel("Cournot Duopoly Game"),
  sidebarPanel(
    textInput(
    inputId = "resultsFilename",
    label = "Enter the filename of the workbook with the output.",
    value = NULL
  ),
  textInput(
    inputId = "rolesFilename",
    label = "Enter the filename of the workbook with the Student Names.",
    value = "StudentList.xlsx"
  ),
  textInput(
    inputId = "user",
    label = "Enter the user ID for the OneDrive account with the output.",
    value = "jb0616165"
  ),
  textInput(
    inputId = "team",
    label = "Enter the team/group ID for the OneDrive account with the output.",
    value = "sau.edu"
  ),
  textInput(
    inputId = "drive",
    label = "Enter the letter of the local drive containing the workbook.",
    value = "c"
  ),
  textInput(
    inputId = "subdir",
    label = "Enter the subdirectory path of the workbook.",
    value = NULL
  ),
  textInput(
    inputId = "partners",
    label = "Random or Student-Chosen Partners?",
    value = "random"
  ),
  actionButton("go", "Load New Responses"),
  numericInput(
    inputId = "a",
    label = "Enter the intercept of the inverse demand function.",
    value = 10
    ),
  numericInput(
    inputId = "b",
    label = "Enter the slope of the inverse demand function.",
    value = -1
    ),
  numericInput(
    inputId = "c",
    label = "Enter the marginal cost of the output.",
    value = 6
    ),
  numericInput(
    inputId = "f",
    label = "Enter the fixed cost of the output.",
    value = 0
    ),
  numericInput(
    inputId = "seed",
    label = "Random Seed",
    value = 8675309
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Payoffs",
               tableOutput("payoff"),
               tableOutput("output"),
               tableOutput("price")),
      tabPanel("Plot", plotOutput("plot")),
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
    a <- input$a
    b <- input$b
    c <- input$c
    f <- input$f
    g <- cournotGame(resultsFilename, rolesFilename, user, drive, team, subdir, a, b, c, f, partners, seed)
    g
  })
  output$plot <- renderPlot({
    g <- data()
    plot(g)
  }, width = 600)
  output$payoff <- renderTable({
    g <- data()
    g$payoff
  }, rownames = TRUE, align = 'lcc',
  caption = "Student Payoff Matrix", caption.placement = "top")
  output$output <- renderTable({
    g <- data()
    g$output
  }, rownames = TRUE, align = 'lcc',
  caption = "Quantity Outcome Matrix", caption.placement = "top")
  output$price <- renderTable({
    g <- data()
    g$price
  }, rownames = TRUE, align = 'lcc',
  caption = "Price Outcome Matrix", caption.placement = "top")
  output$results <- renderTable({
    g <- data()
    subset(g$results, Round = round)
  }, align = 'c')
  output$grades <- renderTable({
    g <- data()
    g$grades
  })
}

# Run the application
shinyApp(ui = ui, server = server)
