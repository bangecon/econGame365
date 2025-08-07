##' Tabulate results for a simple in-class prisoner's dilemma game.
##'
##' @details \code{pestcontrolGame} tabulates the results of a simple stag hunt game in which students' points depend on their strategy and the strategy chosen by their randomly-assigned partners.
##'
##' @param resultsFilename (required) is a file name corresponding to the Excel spreadsheet containing the submissions (e.g. 'Pollution Activity,xlsx').
##' @param rolesFilename is a file name corresponding to the Excel spreadsheet containing the list of participants (e.g. 'Pollution Activity,xlsx').
##' @param user is the user ID that matches the user account containing the OneDrive folder with the spreadsheet containing the submissions.
##' @param drive is the letter assigned to the local drive containing the directory. Default is `c`.
##' @param team is the name of the OneDrive team or group (e.g. 'sau.edu'). Default is \code{'OneDrive'}.
##' @param subdir is the path between the onedrive location and the workbook files (e.g. `ESPP/Activities`). Default is `NULL`.
##' @param partners is a character string indicating whether students were randomly assigned partners or chose their own partners. Default is 'random'.
##' @param roleLabs is a character vector specifying the labels for the two roles in the game. Default is c("Anil", "Bala").
##' @param payoff is a vector indicating the interdependent payoffs from the different pairs of {Student, Partner} strategies: \code{c({Compete, Compete}, {Collude, Compete}, {Compete, Collude}, {Collude, Collude})}
##' @param seed is a number that sets the seed for the activity when `partners == "random"`. Default is `8675309`.
##'
##' @return \code{type} returns the type of activity (`pestcontrolGame`).
##' @return \code{payoff} returns the payoff matrix for the game.
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @export

pestcontrolGame <-
  function(resultsFilename,
           rolesFilename,
           user,
           drive = "c",
           team = NULL,
           subdir = NULL,
           partners = 'random',
           roleLabs = c("Anil", "Bala"),
           payoff = c(3, 4, 1, 2),
           seed = 8675309,
           ...) {
    if (length(payoff) != 4)
      stop("Payoff must have length == 4")
    if (partners == 'random') {
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
      AnilResults <- subset(results, Role == "Anil")[, c(
        "First.Name", "Last.Name", "Role", "Strategy")]
      colnames(AnilResults) <-
        c("First.Name.1", "Last.Name.1", "Role.1", "Strategy.1")
      BalaResults <- subset(results, Role == "Bala")[, c(
        "First.Name", "Last.Name", "Role", "Strategy")]
      colnames(BalaResults) <-
        c("First.Name.2", "Last.Name.2", "Role.2", "Strategy.2")
      resultsWide <- merge(
        roles,
        results[, which(names(results) %in% c(
          "First.Name", "Last.Name", "Role", "Strategy"))],
        by.x = c("First.Name.1", "Last.Name.1", "Role.1"),
        by.y = c("First.Name", "Last.Name", "Role")
      )
      colnames(resultsWide)[7] <- "Strategy.1"
      resultsWide <- merge(
        resultsWide,
        results[, which(names(results) %in% c(
          "First.Name", "Last.Name", "Role", "Strategy"))],
        by.x = c("First.Name.2", "Last.Name.2", "Role.2"),
        by.y = c("First.Name", "Last.Name", "Role")
      )
      colnames(resultsWide)[8] <- "Strategy.2"
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
      AnilResults <- subset(results, Role == "Anil")[, c(
        "First.Name",
        "Last.Name",
        "Role",
        "Strategy"
      )]
      colnames(AnilResults) <- c(
        "First.Name.1",
        "Last.Name.1",
        "Role.1",
        "Strategy.1"
      )
      BalaResults <- subset(results, Role == "Bala")[, c(
        "First.Name",
        "Last.Name",
        "Role",
        "Strategy"
      )]
      colnames(BalaResults) <- c(
        "First.Name.2",
        "Last.Name.2",
        "Role.2",
        "Strategy.2"
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
      colnames(results)[which(names(results) %in% "Strategy")] <-
        "Strategy.1"
      roles <- merge(
        results[results$Role =="Anil", c(
          "First.Name.1",
          "Last.Name.1",
          "First.Name.2",
          "Last.Name.2",
          "Role.1"
        )],
        results[results$Role =="Bala", c(
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
          "First.Name.1", "Last.Name.1", "Role.1", "Strategy.1"))],
        by = c("First.Name.1", "Last.Name.1", "Role.1")
      )
      resultsWide <- merge(
        resultsWide,
        results[, which(names(results) %in% c(
          "First.Name.1", "Last.Name.1", "Role.1", "Strategy.1"))],
        by.x = c("First.Name.2", "Last.Name.2", "Role.2"),
        by.y = c("First.Name.1", "Last.Name.1", "Role.1")
      )
      colnames(resultsWide)[7:8] <- c("Strategy.1", "Strategy.2")
    }


    resultsWide$Outcome <-
      paste0(resultsWide$Strategy.1, "-", resultsWide$Strategy.2)
    resultsWide$Score.1 <- ifelse(
      resultsWide$Outcome == "IPC-IPC",
      payoff[1],
      ifelse(
        resultsWide$Outcome == "IPC-Terminator",
        payoff[3],
        ifelse(resultsWide$Outcome == "Terminator-IPC", payoff[2], payoff[4])
      )
    )
    resultsWide$Score.2 <- ifelse(
      resultsWide$Outcome == "IPC-IPC",
      payoff[1],
      ifelse(
        resultsWide$Outcome == "IPC-Terminator",
        payoff[2],
        ifelse(resultsWide$Outcome == "Terminator-IPC", payoff[3], payoff[4])
      )
    )
    results <- data.frame(
      First.Name = c(resultsWide$First.Name.1, resultsWide$First.Name.2),
      Last.Name = c(resultsWide$Last.Name.1, resultsWide$Last.Name.2),
      Partner.First.Name = c(resultsWide$First.Name.2, resultsWide$First.Name.1),
      Partner.Last.Name = c(resultsWide$Last.Name.2, resultsWide$Last.Name.1),
      Role = c(resultsWide$Role.1, resultsWide$Role.2),
      Strategy = c(resultsWide$Strategy.1, resultsWide$Strategy.2),
      Partner.Strategy = c(resultsWide$Strategy.2, resultsWide$Strategy.1),
      Score = c(resultsWide$Score.1, resultsWide$Score.2)
    )
    results <- results[order(results$Last.Name), ]



    payoffMatrix <- matrix(
      c(paste0("(",payoff[1], ",", payoff[1], ")"),
        paste0("(",payoff[2], ",", payoff[3], ")"),
        paste0("(",payoff[3], ",", payoff[2], ")"),
        paste0("(",payoff[4], ",", payoff[4], ")")),
      nrow = 2, ncol = 2)
    colnames(payoffMatrix) <-
      c("Bala = IPC", "Bala = Terminator")
    rownames(payoffMatrix) <-
      c("Anil = IPC", "Anil = Terminator")
    grades <-
      aggregate(Score ~ First.Name + Last.Name,
                data = results,
                FUN = sum)
    grades  <- grades[order(grades$Last.Name, grades$First.Name),]
    colnames(grades) <- c("First Name", "Last Name", "Score")
    out <- list(
      type = "pestcontrolGame",
      roles = roles,
      AnilResults = AnilResults,
      BalaResults = BalaResults,
      payoff = payoffMatrix,
      results = results[order(results$Last.Name, results$First.Name), ],
      resultsWide = resultsWide,
      grades = grades
    )
    class(out) <- c('econGame', class(out))
    out
  }
