library(econGame365)

ui <- fluidPage(
  titlePanel("Random Roles"),
  sidebarPanel(
    textInput(
      inputId = "filename",
      label = "Enter the filename of the Excel Workbook with the output.",
      value = "studentList.xlsx"
    ),
    textInput(
      inputId = "user",
      label = "Enter the user ID for the OneDrive account with the output.",
      value = "jb0616165"
    ),
    textInput(
      inputId = "team",
      label =
        "Enter the team or group ID for the OneDrive account with the output.",
      value = "sau.edu"
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
    numericInput(
      inputId = "seed",
      label = "Random Seed",
      value = 8675309
    ),
    numericInput(
      inputId = "size",
      label = "Group Size",
      value = 2
    ),
    textInput(
      inputId = "role1",
      label = "Enter the name of the first role.",
      value = "First Mover"
    ),
    textInput(
      inputId = "role2",
      label = "Enter the name of the second role.",
      value = "Second Mover"
    ),
    actionButton("go", "Load New Responses"),
    hr(),
    a("Created by Jim Bang", href='https://github.com/bangecon'),
    a("St. Ambrose University", href='https://www.sau.edu/')
  ),
  mainPanel(tableOutput("groups"), style = "font-size:20px; ")
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
    roleLabs = c(input$role1, input$role2)
    g <- randomRoles(filename, user, drive, team, subdir, size, seed, roleLabs)
    colnames(g$long) <- c("First Name", "Last Name", "Role")
    g
  })
  output$groups <- renderTable( {
    g <- data()
    g$long
  }, rownames = FALSE, align = 'c',
  caption = "Student Roles", caption.placement = "top")
}

shinyApp(ui = ui, server = server)
