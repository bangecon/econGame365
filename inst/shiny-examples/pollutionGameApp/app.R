#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(econGame)

# Define UI for application
ui <- fluidPage(
  titlePanel("Pollution Policy Game"),
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
    actionButton("go", "Load New Responses"),
    numericInput(
      inputId = "price",
      label = "Enter the price of the output.",
      value = 4
    ),
    numericInput(
      inputId = "externality",
      label = "Enter the size of the externality.",
      value = 3
    ),
    numericInput(
      inputId = "tax",
      label = "Enter the size of the Pigouvian tax on the externality.",
      value = 3
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("No Regulation",
               tabsetPanel(
                 tabPanel("Summary", tableOutput("summary1")),
                 tabPanel("Results", tableOutput("results1")),
                 tabPanel("Grades", tableOutput("grades1"))
               )),
      tabPanel(
        "Command & Control",
        tabsetPanel(
          tabPanel("Summary", tableOutput("summary2")),
          tabPanel("Results", tableOutput("results2")),
          tabPanel("Grades", tableOutput("grades2"))
        )
      ),
      tabPanel("Pollution Tax",
               tabsetPanel(
                 tabPanel("Summary", tableOutput("summary3")),
                 tabPanel("Results", tableOutput("results3")),
                 tabPanel("Grades", tableOutput("grades3"))
               )),
      tabPanel("Cap & Trade",
               tabsetPanel(
                 tabPanel("Summary", tableOutput("summary4")),
                 tabPanel("Schedule", tableOutput("schedule4")),
                 tabPanel("Plot", plotOutput("plot", width = 600, height = 600)),
                 tabPanel("Results", tableOutput("results4")),
                 tabPanel("Grades", tableOutput("grades4"))
               )),
      tabPanel("Overall Summary",
               tabsetPanel(
                 tabPanel("Summary", tableOutput("summary")),
                 tabPanel("Results", tableOutput("results")),
                 tabPanel("Grades", tableOutput("grades"))
               ))
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
    price <- input$price
    externality <- input$externality
    tax <- input$tax
    g <- pollutionGame(
      filename, user, drive, team, subdir, price, externality, tax
    )
    g
  })
  output$summary1 <- renderTable({
    g <- data()
    g$summary$summary1
  }, rownames = TRUE, colnames = FALSE)
  output$results1 <- renderTable({
    g <- data()
    g$results$results1
  })
  output$grades1 <- renderTable({
    g <- data()
    g$grades$grades1
  })
  output$summary2 <- renderTable({
    g <- data()
    g$summary$summary2
  }, rownames = TRUE, colnames = FALSE)
  output$results2 <- renderTable({
    g <- data()
    g$results$results2
  })
  output$grades2 <- renderTable({
    g <- data()
    g$grades$grades2
  })
  output$summary3 <- renderTable({
    g <- data()
    g$summary$summary3
  }, rownames = TRUE, colnames = FALSE)
  output$results3 <- renderTable({
    g <- data()
    g$results$results3
  })
  output$grades3 <- renderTable({
    g <- data()
    g$grades$grades3
  })
  output$summary4 <- renderTable({
    g <- data()
    g$summary$summary4
  }, rownames = TRUE, colnames = FALSE)
  output$results4 <- renderTable({
    g <- data()
    g$results$results4
  })
  output$grades4 <- renderTable({
    g <- data()
    g$grades$grades4
  })
  output$schedule4 <- renderTable({
    g <- data()
    g$marketSchedule
  })
  output$plot <- renderPlot({
    g <- data()
    plot(g)
  })
  output$summary <- renderTable({
    g <- data()
    g$summary$summary
  }, rownames = TRUE, colnames = TRUE)
  output$grades <- renderTable({
    g <- data()
    g$grades$grades
  })
  output$results <- renderTable({
    g <- data()
    subset(g$results$results)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
