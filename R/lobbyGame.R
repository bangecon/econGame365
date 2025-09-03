##' Tabulate results for an all-pay auction game.
##'
##' Tabulates and assigns points for the results of a simple in-class entry and exit game using random values.
##'
##' @details \code{lobbyGame} tabulates the results of a simple lobbying game based on a simplified version of Goeree and Holt (1999). By default students receive an "endowment" of "extra credit" points (default is 5) at the beginning of a round of bidding. In the first iteration, students "bid" on a monopoly license "prize" (default is 4 points) by making an anonymous number of "contributions" to politicians in order to influence the decision by the government. Each contribution costs 1 point, and is tabulated as the number of times a student submits the Google Form. The winner of the auction is determined by a lottery from a random row sampled from the responses. In this iteration, students submit bids orally or electronically in real time according to an all-pay auction.
##'
##' @param filename (required) is a file name corresponding to the Excel spreadsheet containing the submissions (e.g. 'Pollution Activity,xlsx'; required).
##' @param user (required) is the user ID that matches the user account containing the OneDrive folder with the spreadsheet containing the submissions.
##' @param drive is the letter assigned to the local drive containing the directory. Default is `"c"`.
##' @param team is the name of the OneDrive team or group (e.g. 'sau.edu'). Default is \code{'OneDrive'}.
##' @param subdir is the path between the onedrive location and the workbook files (e.g. `ESPP/Activities`). Default is `NULL`.
##' @param endowment is the size of the initial endowment of points the instructor wishes to give each student.
##' @param prize is the value of the license or prize each interest group (student) is bidding on.
##' @param seed is the value for the numeric seed in the randomization for selecting the "winner" (default is 8675309).
##' @param names character list of the column names in `sheet`.
##' @param auth is a logical indicating whether to use an authentication token to access the Sheet containing the individual submissions.
##' @param email is an email address that matches the user account containing the Sheet with the individual submissions (if `auth == TRUE`).
##'
##' @return \code{type} returns the type of activity (allpayGame).
##' @return \code{results} returns the original submissions (with market prices and points per round added).
##' @return \code{winner} returns the name of the winner and the size of the prize.
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @references Goeree, Jacob K. & Charles A. Holt (1999). Classroom Games: Rent-Seeking and the Inefficiency of Non-market Allocations. \emph{Journal of Economic Perspectives,} 13(3), pp.217-226.
##'
##' @export

lobbyGame <-
  function(filename,
           user,
           drive = "c",
           team = NULL,
           subdir = NULL,
           endowment = 5,
           prize = 4,
           seed = 8675309,
           ...)
  {
    drive <- paste0(drive, ":/Users/")
    if (is.null(team)) {
      team <- "/OneDrive/"
    } else {
      team <- paste0("/OneDrive - ", team, "/")
    }
    path <- paste0(drive, user, team, subdir, filename)
    results <- readxl::read_excel(path)[, c("First Name", "Last Name")]
    colnames(results) <- make.names(colnames(results))
    results <- results %>%
      replace_na(list(First.Name = "John", Last.Name = "Doe")) %>%
      mutate(First.Name = str_to_title(First.Name),
             Last.Name = str_to_title(Last.Name))
    set.seed(seed)
    winner <- results[sample(nrow(results), 1), ]
    results$Contributions <- 1
    winner <- cbind(winner, prize)
    colnames(winner)[3] <- "Prize"
    grades <-
      aggregate(
        Contributions ~ Last.Name + First.Name,
        data = results,
        FUN = sum,
        na.action = na.pass
      )
    grades <- cbind(grades, endowment)
    colnames(grades)[4] <- "Endowment"
    grades <-
      merge(grades,
            winner,
            by = c("Last.Name", "First.Name"),
            all = TRUE)
    grades <-
      replace_na(grades, list(Prize = 0))
    grades$Points <-
      grades$Endowment + grades$Prize - grades$Contributions
    revenue <- sum(grades$Contributions)
    out <- list(
      type = "lobbyGame",
      results = results[order(results$Last.Name,
                              results$First.Name), ],
      winner = winner,
      revenue = revenue,
      grades = grades[order(grades$Last.Name, grades$First.Name), ]
    )
    class(out) <- c('econGame', class(out))
    out
  }
