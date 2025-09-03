##' Tabulate entry and exit game results.
##'
##' Tabulates and assigns points for the results of a simple in-class entry and exit game using random values.
##'
##' @details \code{equilibriumGame} tabulates the results of a simple entry and exit game based on a simplified version of Garratt (2000) with two crops (corn and soybeans). The instructor informs the students that they will choose to plant corn, soybeans, or nothing. Producing corn incurs a cost of four points, while producing soybeans incurs a cost of 10 points. Selling a unit of corn brings revenue equal to \eqn{P_c = (N/2) + 6 - Q_c}, where N equals the number of students participating and Q_c equals the number of students choosing to produce corn. Selling a unit of soybeans brings revenue equal to \eqn{P_s = (N/2) + 10 - Q_s}. These parameters allow for there to be a "normal profit" of one point per student in each market, and lessens the chances that students might win negative points. Students choosing to produce nothing sell their labor in the labor market and break even (less the "normal profit").
##'
##' @param filename (required) is a file name corresponding to the Excel spreadsheet containing the submissions (e.g. 'Pollution Activity,xlsx'; required).
##' @param user (required) is the user ID that matches the user account containing the OneDrive folder with the spreadsheet containing the submissions.
##' @param drive is the letter assigned to the local drive containing the directory. Default is `"c"`.
##' @param team is the name of the OneDrive team or group (e.g. 'sau.edu'). Default is \code{'OneDrive'}.
##' @param subdir is the path between the onedrive location and the workbook files (e.g. `ESPP/Activities`). Default is `NULL`.
##'
##' @return \code{type} returns the type of activity (entryGame).
##' @return \code{results} returns the original submissions (with market prices and points per round added).
##' @return \code{rounds} returns the number of rounds in "results"
##' @return \code{equilibria} returns a list containing the equilibria for each round.
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @references Garratt (2000). A Free Entry and Exit Experiment. \emph{Journal of Economic Education,} 31(3), pp.237-243.
##'
##' @export

entryGame <-
  function(filename,
           user,
           drive = "c",
           team = NULL,
           subdir = NULL,
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
    results <- results %>%
      replace_na(list(First.Name = "John", Last.Name = "Doe")) %>%
      mutate(First.Name = str_to_title(First.Name),
             Last.Name = str_to_title(Last.Name))
    rounds <- max(results$Round)
    equilibria <-
      list(
        N = list(NULL),
        Dinv_c = list(NULL),
        Q_c = list(NULL),
        P_c = list(NULL),
        Dinv_s = list(NULL),
        Q_s = list(NULL),
        P_s = list(NULL)
      )
    for (i in 1:rounds) {
      # Calculate the equilibrium for each round
      roundresult <- subset(results, Round == i)
      equilibria$N[[i]] <- nrow(roundresult)
      equilibria$Dinv_c[[i]] <-
        function(x) {
          (equilibria$N[[i]] / 2) +  6 - x
        }
      equilibria$Dinv_s[[i]] <-
        function(x) {
          (equilibria$N[[i]] / 2) + 10 - x
        }
      equilibria$Q_c[[i]] <-
        nrow(subset(roundresult, Market == "Corn"))
      equilibria$Q_s[[i]] <-
        nrow(subset(roundresult, Market == "Soybeans"))
      equilibria$P_c[[i]] <-
        equilibria$Dinv_c[[i]](equilibria$Q_c[[i]])
      equilibria$P_s[[i]] <-
        equilibria$Dinv_s[[i]](equilibria$Q_s[[i]])
    }
    # Calculate student points.
    results$Price <- NA
    results$Cost <- NA
    results$Score <- NA
    for (i in 1:nrow(results)) {
      results$Price[i] <- ifelse(
        results$Market[[i]] == "Soybeans",
        equilibria$P_s[[results$Round[[i]]]],
        ifelse(results$Market[[i]] == "Corn",
               equilibria$P_c[[results$Round[[i]]]], 0)
      )
      results$Cost[i] <- ifelse(results$Market[[i]] == "Soybeans",
                                10,
                                ifelse(results$Market[[i]] == "Corn", 4, 0))
      results$Score[i] <- results$Price[i] - results$Cost[i]
    }
    results <- data.frame(
      First.Name = results$First.Name,
      Last.Name = results$Last.Name,
      Round = results$Round,
      Market = results$Market,
      Price = results$Price,
      Cost = results$Cost,
      Score = results$Score
    )
    grades <-
      aggregate(
        Score ~ Last.Name + First.Name,
        data = results,
        FUN = sum,
        na.action = na.pass
      )
    out <- list(
      type = "entryGame",
      equilibria = equilibria,
      results = results[order(results$Round,
                              results$Market,
                              results$Last.Name,
                              results$First.Name),],
      rounds = rounds,
      grades = grades[order(grades$Last.Name, grades$First.Name),]
    )
    class(out) <- c('econGame', class(out))
    out
  }
