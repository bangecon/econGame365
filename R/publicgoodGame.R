##' Tabulate public good game results.
##'
##' Tabulates and assigns points for the results of a simple in-class public good game.
##'
##' @details \code{publicgoodGame} tabulates the results of a simple public good game based on Leuthold (1993) and Holt and Laury (1997). Each student starts with an endowment of 5 points that they can either keep or anonymously and voluntarily contribute to a public good. For each point a student keeps, they receive only that point. For each point contributed to the public good, \emph{everyone} receives R/N points, where N is the number of participants and R > 1 is the return the group earns on public good contributions. In other words, students have the opportunity to boost their points if everyone contributes all of them. But, if only a few participants contribute, their net return could diminish.
##'
##'
##' @param filename (required) is a file name corresponding to the Excel spreadsheet containing the submissions (e.g. 'Public Good Game.xlsx'; required).
##' @param user is the user ID that matches the user account containing the OneDrive folder with the spreadsheet containing the submissions.
##' @param drive is the letter assigned to the local drive containing the directory (e.g. 'c'). Default is \code{'OneDrive'}.
##' @param team is the name of the OneDrive team or group (e.g. 'sau.edu'). Default is \code{'OneDrive'}.
##' @param endowment is the number of gifted points students start the game with (default is 5).
##' @param value is the amount that each point contributed to the public good returns to *each member of the class* (default is 0.1).
##' @param names character list of the column names in `sheet`.
##' @param auth is a logical indicating whether to use an authentication token to access the Sheet containing the individual submissions.
##' @param email is an email address that matches the user account containing the Sheet with the individual submissions (if `auth == TRUE`).
##'
##' @return \code{type} returns the type of activity (publicgoodGame).
##' @return \code{results} returns the original submissions.
##' @return \code{blinded esults} returns the submissions without student names.
##' @return \code{grades} returns the aggregated, stacked points "won" by each student for the entire activity.
##'
##' @references Holt, Charles A. and Laury, Susan K. (1997). Classroom Games: Voluntary Provision of a Public Good.
##' \emph{Journal of Economic Perspectives} 11(4), pp. 209-215.
##'
##' @export

publicgoodGame <- function(filename,
                           user,
                           drive = "c",
                           team = NULL,
                           subdir = NULL,
                           endowment = 5,
                           benefit = 0.1,
                           ...) {
  drive <- paste0(drive, ":/Users/")
  if (is.null(team)) {
    team <- "/OneDrive/"
  } else {
    team <- paste0("/OneDrive - ", team, "/")
  }
  path <- paste0(drive, user, team, subdir, filename)
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
  N <- nrow(results)
  totalContributions <- sum(results$Contribution)
  individualReallocations <- benefit*totalContributions
  results$Reallocation <- individualReallocations
  results$Score <-
    endowment + results$Reallocation - results$Contribution
  blindedResults <-
    data.frame(results[,-which(names(results) %in% c("Id", "Start.time", "Completion.time", "Email", "Name", "First.Name", "Last.Name"))])
  grades <-
    with(results,
         as.data.frame(
           cbind(Last.Name, First.Name, Contribution, Reallocation, Score)
         ))
  out <- list(
    type = "publicgoodGame",
    results = results[order(results$Last.Name, results$First.Name), ],
    blindedResults = blindedResults[order(blindedResults$Contribution), ],
    grades = grades[order(grades$Last.Name, grades$First.Name), ]
  )
  class(out) <- c('econGame', class(out))
  out
}
