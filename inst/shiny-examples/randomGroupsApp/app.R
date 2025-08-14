library(econGame365)

ui <- fluidPage(
  titlePanel("Random Groups"),
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
      label =
        "Enter the team or group ID for the OneDrive account with the output.",
      value = NULL
    ),
    textInput(
      inputId = "drive",
      label =
        "Enter the letter of the local drive containing the Excel Workbook with the output.",
      value = "c"
    ),
    textInput(
      inputId = "subdir",
      label =
        "Enter the subdirectory path of the Excel Workbook with the output.",
      value = NULL
    ),
    actionButton("go", "Load New Responses"),
    numericInput(
      inputId = "seed",
      label = "Random Seed",
      value = 8675309
    ),
    hr(),
    a("Created by Jim Bang", href='https://github.com/bangecon'),
    a("St. Ambrose University", href='https://www.sau.edu/')
  ),
  mainPanel(textOutput("groups"), style = "font-size:20px; ")
)

server <- function(input, output) {
  data <- eventReactive(input$go, {
    filename <- input$filename
    user <- input$user
    team <- input$team
    drive <- input$drive
    subdir <- input$subdir
    partners <- input$partners
    seed <- input$seed
    g <- randomGroups(filename, user, drive, team, subdir, size, seed, roleLabs)
    colnames(g$long) <- c("First Name", "Last Name", "Group", "Member")
    g
  })
  output$groups <- renderTable( {
    g <- data()
    g$long
  }, rownames = FALSE, align = 'c',
  caption = "Student Groups", caption.placement = "top")
}

shinyApp(ui = ui, server = server)
