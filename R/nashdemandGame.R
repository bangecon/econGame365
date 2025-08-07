##' Tabulate Nash demand game results.
##'
##' Tabulates and assigns points for the results of a simple in-class ultimatum game.
##'
##' @details \code{nashdemandGame} tabulates the results of a simple Nash demand game. Students are assigned to pairs. The pair receives an endowment of points to share. Each player makes a demand and if the sum is less than or equal to the total endowment; if the sum exceeds the endowment both players get nothing.
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
##'
##' @return \code{type} returns the type of activity (ultimatumGame).
##' @return \code{results} returns the original submissions.
##' @return \code{grades} returns the aggregated, stacked points "won" by each student for the entire activity.
##'
##' @references Holt, Charles A. (1996). Classroom games: Trading in a pit market. \emph{Journal of Economic Perspectives,} 10(1), pp.193-203.
##'
##' @export

nashdemandGame <- function(resultsFilename,
                          rolesFilename,
                          user,
                          drive = "c",
                          team = NULL,
                          subdir = NULL,
                          partners = 'random',
                          endowment = 5,
                          seed = 8675309,
                          ...) {
  # Set up the Google Sheets, read responses, and initialize output objects.
  if (partners == "random") {
    roles <- randomGroups(
      filename = rolesFilename,
      user = user,
      drive = drive,
      team = team,
      subdir = subdir,
      seed = seed
    )
    if (is.null(team)) {
      team <- "/OneDrive/"
    } else {
      team <- paste0("/OneDrive - ", team, "/")
    }
    path <- paste0(drive, ":/Users/", user, team, subdir, resultsFilename)
    results <- readxl::read_excel(path)
    colnames(results) <- make.names(colnames(results))
    results$First.Name <- make.names(results$First.Name)
    results$Last.Name <- make.names(results$Last.Name)
    results <- results %>%
      replace_na(list(First.Name = "John", Last.Name = "Doe")) %>%
      mutate(First.Name = str_to_title(First.Name),
             Last.Name = str_to_title(Last.Name))
    results <- merge(roles$long, results, by = c("First.Name", "Last.Name"))
    results <-  results[order(results$Group, results$Member), ]
    results1 <- subset(results, Member == 1)[, c(
      "First.Name", "Last.Name", "Demand")]
    colnames(results1) <-
      c("First.Name.1", "Last.Name.1", "Demand.1")
    results2 <- subset(results, Member == 2)[, c(
      "First.Name", "Last.Name", "Demand")]
    colnames(results2) <-
      c("First.Name.2", "Last.Name.2", "Demand.2")
    resultsWide <- cbind(results1, results2)
  } else {
    if (is.null(team)) {
      team <- "/OneDrive/"
    } else {
      team <- paste0("/OneDrive - ", team, "/")
    }
    path <- paste0(drive, ":/Users/", user, team, subdir, resultsFilename)
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
    results$group1 <- paste0(
      results$First.Name,
      results$Last.Name,
      "-",
      results$Partner.First.Name,
      results$Partner.Last.Name
    )
    results$group2 <- paste0(
      results$Partner.First.Name,
      results$Partner.Last.Name,
      "-",
      results$First.Name,
      results$Last.Name
    )
    results$Group <- round(rank(apply(
      results[, c("group1", "group2")], 1, min)), 0)/2
    results$Member <- with(results, ave(Group, Group, FUN = seq_along))
    results <- as.data.frame(results[order(results$Group, results$Member),])
    results1 <- subset(results, Member == 1)[, c(
      "First.Name", "Last.Name", "Demand")]
    colnames(results1) <-
      c("First.Name.1", "Last.Name.1", "Demand.1")
    results2 <- subset(results, Member == 2)[, c(
      "First.Name", "Last.Name", "Demand")]
    colnames(results2) <-
      c("First.Name.2", "Last.Name.2", "Demand.2")
    resultsWide <- cbind(results1, results2)
  }
  resultsWide$Outcome <- factor(ifelse(
    resultsWide$Demand.1 + resultsWide$Demand.2 <= endowment,
    "Accept",
    "Reject"
    ),
    levels = c("Reject", "Accept"))
  resultsWide$Score.1 <- ifelse(
    resultsWide$Outcome == "Accept", resultsWide$Demand.1, 0)
  resultsWide$Score.2 <- ifelse(
    resultsWide$Outcome == "Accept", resultsWide$Demand.2, 0)
  results <- data.frame(
    First.Name = c(resultsWide$First.Name.1, resultsWide$First.Name.2),
    Last.Name = c(resultsWide$Last.Name.1, resultsWide$Last.Name.2),
    Partner.First.Name = c(resultsWide$First.Name.2, resultsWide$First.Name.1),
    Partner.Last.Name = c(resultsWide$Last.Name.2, resultsWide$Last.Name.1),
    Demand = c(resultsWide$Demand.1, resultsWide$Demand.2),
    PartnerDemand = c(resultsWide$Demand.2, resultsWide$Demand.1),
    Outcome = c(resultsWide$Outcome, resultsWide$Outcome),
    Score = c(resultsWide$Score.1, resultsWide$Score.2)
  )
  results <- results[order(results$Last.Name), ]
  grades <- results[, c("First.Name", "Last.Name", "Score")]
  grades  <- grades[order(grades$Last.Name, grades$First.Name), ]
  out <- list(
    type = "nashdemandGame",
    results = results[order(results$Last.Name, results$First.Name), ],
    resultsWide = resultsWide,
    grades = grades
  )
  class(out) <- c('econGame', class(out))
  out
}
