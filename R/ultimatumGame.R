##' Tabulate ultimatum game results.
##'
##' Tabulates and assigns points for the results of a simple in-class ultimatum game.
##'
##' @details \code{ultimatumGame} tabulates the results of a simple ultimatum game based on Thaler (1988). Students form pairs and determine a "proposer" and a "responder" by flipping a coin (or some other fair mechanism). The pair receives 5 "extra credit" points to share. The proposer offers to give a number of points to the responder (between 0 and 5) and the responder decides whether to accept or reject. If the responder accepts, then the pair shares the points according to the aggreement; if the responder rejects, both players get nothing.
##'
##' @param resultsFilename (required) is a file name corresponding to the Excel spreadsheet containing the submissions (e.g. 'Crop Choice Game.xlsx').
##' @param rolesFilename (required if `partners == "random"`) is a file name corresponding to the Excel spreadsheet containing the submissions (e.g. 'Crop Choice Game.xlsx').
##' @param user is the user ID that matches the user account containing the OneDrive folder with the spreadsheet containing the submissions.
##' @param drive is the letter assigned to the local drive containing the directory (e.g. `'c'`). Default is \code{'OneDrive'}.
##' @param team is the name of the OneDrive team or group (e.g. 'sau.edu'). Default is \code{'OneDrive'}.
##' @param subdir is the path between the onedrive location and the workbook files (e.g. `ESPP/Activities`). Default is `NULL`.
##' @param partners is a character string indicating whether students were randomly assigned partners or chose their own partners. Default is `'random'`.
##' @param endowment is the number of gifted points students start the game with (default is 5).
##' @param roleLabs is a character vector specifying the labels for the two roles in the game. Default is `c("Anil", "Bala")`.
##' @param seed is a number that sets the seed for the activity when `partners == "random"`. Default is `8675309`.
##' @return \code{type} returns the type of activity (ultimatumGame).
##' @return \code{results} returns the original submissions.
##' @return \code{grades} returns the aggregated, stacked points "won" by each student for the entire activity.
##'
##' @references Holt, Charles A. (1996). Classroom games: Trading in a pit market. \emph{Journal of Economic Perspectives,} 10(1), pp.193-203.
##'
##' @export

ultimatumGame <- function(resultsFilename,
                          rolesFilename,
                          user,
                          drive = "c",
                          team = NULL,
                          subdir = NULL,
                          partners = 'random',
                          endowment = 5,
                          roleLabs = c("Proposer", "Responder"),
                          seed = 8675309,
                          ...) {
  # Set up the Google Sheets, read responses, and initialize output objects.
  if (partners == "random") {
    roles <- randomRoles(
      filename = rolesFilename,
      user = user,
      drive = drive,
      team = team,
      subdir = subdir,
      roleLabs = roleLabs,
      seed = seed
    )$wide
    drive <- paste0(drive, ":/Users/")
    if (is.null(team)) {
      team <- "/OneDrive/"
    } else {
      team <- paste0("/OneDrive - ", team, "/")
    }
    path <- paste0(drive, user, team, subdir, resultsFilename)
    results <- readxl::read_excel(path)
    colnames(results) <- make.names(colnames(results))
    results$First.Name <- make.names(results$First.Name)
    results$Last.Name <- make.names(results$Last.Name)
    results <- results %>%
      replace_na(list(First.Name = "John", Last.Name = "Doe")) %>%
      mutate(First.Name = str_to_title(First.Name),
             Last.Name = str_to_title(Last.Name))
    results <- as.data.frame(results)
    results <-  results[order(results$First.Name, results$Last.Name), ]
    ProposerResults <- subset(results, Role == "Proposer")[, c(
      "First.Name", "Last.Name", "Role", "Offer")]
    colnames(ProposerResults) <-
      c("First.Name.1", "Last.Name.1", "Role.1", "Offer")
    ResponderResults <- subset(results, Role == "Responder")[, c(
      "First.Name", "Last.Name", "Role", "Response")]
    colnames(ResponderResults) <-
      c("First.Name.2", "Last.Name.2", "Role.2", "Response")
    resultsWide <- merge(
      roles,
      results[, which(names(results) %in% c(
        "First.Name", "Last.Name", "Role", "Offer"))],
      by.x = c("First.Name.1", "Last.Name.1", "Role.1"),
      by.y = c("First.Name", "Last.Name", "Role")
    )
    resultsWide <- merge(
      resultsWide,
      results[, which(names(results) %in% c(
        "First.Name", "Last.Name", "Role", "Response"))],
      by.x = c("First.Name.2", "Last.Name.2", "Role.2"),
      by.y = c("First.Name", "Last.Name", "Role")
    )
  } else {
    drive <- paste0(drive, ":/Users/")
    if (is.null(team)) {
      team <- "/OneDrive/"
    } else {
      team <- paste0("/OneDrive - ", team, "/")
    }
    path <- paste0(drive, user, team, subdir, resultsFilename)
    results <- readxl::read_excel(path)
    colnames(results) <- make.names(colnames(results))
    results$First.Name <- make.names(results$First.Name)
    results$Last.Name <- make.names(results$Last.Name)
    results$Partner.First.Name <- make.names(results$Partner.First.Name)
    results$Partner.Last.Name <- make.names(results$Partner.Last.Name)
    results <- results %>%
      replace_na(list(First.Name = "John", Last.Name = "Doe")) %>%
      mutate(First.Name = str_to_title(First.Name),
             Last.Name = str_to_title(Last.Name),
             Partner.First.Name = str_to_title(Partner.First.Name),
             Partner.Last.Name = str_to_title(Partner.Last.Name))
    results <- as.data.frame(results)
    results <-  results[order(results$First.Name, results$Last.Name), ]
    ProposerResults <- subset(results, Role == "Proposer")[, c(
      "First.Name",
      "Last.Name",
      "Role",
      "Offer"
    )]
    colnames(ProposerResults) <- c(
      "First.Name.1",
      "Last.Name.1",
      "Role.1",
      "Offer"
    )
    ResponderResults <- subset(results, Role == "Responder")[, c(
      "First.Name",
      "Last.Name",
      "Role",
      "Response"
    )]
    colnames(ResponderResults) <- c(
      "First.Name.2",
      "Last.Name.2",
      "Role.2",
      "Response"
    )
    colnames(results)[which(names(results) %in% "First.Name")] <-
      "First.Name.1"
    colnames(results)[which(names(results) %in% "Last.Name")] <-
      "Last.Name.1"
    colnames(results)[which(names(results) %in% "Partner.First.Name")] <-
      "First.Name.2"
    colnames(results)[which(names(results) %in% "Partner.Last.Name")] <-
      "Last.Name.2"
    colnames(results)[which(names(results) %in% "Role")] <- "Role.1"
    roles <- merge(
      results[results$Role =="Proposer", c(
        "First.Name.1",
        "Last.Name.1",
        "First.Name.2",
        "Last.Name.2",
        "Role.1"
      )],
      results[results$Role =="Responder", c(
        "First.Name.1",
        "Last.Name.1",
        "Role.1")],
      by.x = c("First.Name.2", "Last.Name.2"),
      by.y = c("First.Name.1", "Last.Name.1"))
    colnames(roles)[which(names(roles) %in% "Role.1.x")] <- "Role.1"
    colnames(roles)[which(names(roles) %in% "Role.1.y")] <- "Role.2"
    resultsWide <- merge(
      roles,
      results[, which(names(results) %in% c(
        "First.Name.1", "Last.Name.1", "Role.1", "Offer"))],
      by = c("First.Name.1", "Last.Name.1", "Role.1")
    )
    resultsWide <- merge(
      resultsWide,
      results[, which(names(results) %in% c(
        "First.Name.1", "Last.Name.1", "Role.1", "Response"))],
      by.x = c("First.Name.2", "Last.Name.2", "Role.2"),
      by.y = c("First.Name.1", "Last.Name.1", "Role.1")
    )
  }
  # Calculate the proposers' grades
  resultsWide$Outcome <- factor(ifelse(
    resultsWide$Offer >= resultsWide$Response, "Accept", "Reject"),
    levels = c("Reject", "Accept"))
  resultsWide$Score.1 <- ifelse(
    resultsWide$Outcome == "Accept", endowment - resultsWide$Offer, 0)
  resultsWide$Score.2 <- ifelse(
    resultsWide$Outcome == "Accept", resultsWide$Offer, 0)
  results <- data.frame(
    First.Name = c(resultsWide$First.Name.1, resultsWide$First.Name.2),
    Last.Name = c(resultsWide$Last.Name.1, resultsWide$Last.Name.2),
    Partner.First.Name = c(resultsWide$First.Name.2, resultsWide$First.Name.1),
    Partner.Last.Name = c(resultsWide$Last.Name.2, resultsWide$Last.Name.1),
    Role = c(resultsWide$Role.1, resultsWide$Role.2),
    Offer = c(resultsWide$Offer, resultsWide$Offer),
    Response = c(resultsWide$Response, resultsWide$Response),
    Outcome = c(resultsWide$Outcome, resultsWide$Outcome),
    Score = c(resultsWide$Score.1, resultsWide$Score.2)
  )
  results <- results[order(results$Last.Name), ]
  grades <- results[, c("First.Name", "Last.Name", "Score")]
  grades  <- grades[order(grades$Last.Name, grades$First.Name), ]
  out <- list(
    type = "ultimatumGame",
    roles = roles,
    ProposerResults = ProposerResults,
    ResponderResults = ResponderResults,
    results = results[order(results$Last.Name, results$First.Name), ],
    resultsWide = resultsWide,
    grades = grades
  )
  class(out) <- c('econGame', class(out))
  out
}
