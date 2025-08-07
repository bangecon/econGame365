##' Tabulate results for a simple in-class Cournot duopoly game.
##'
##' @details \code{cournotGame} tabulates the results of a simple Cournot duopoly game in which students' points depend on their strategy and the strategy chosen by their randomly-assigned partners.
##'
##' @param resultsFilename (required) is a file name corresponding to the Excel spreadsheet containing the submissions (e.g. 'Pollution Activity,xlsx').
##' @param rolesFilename is a file name corresponding to the Excel spreadsheet containing the list of participants (e.g. 'Pollution Activity,xlsx').
##' @param user is the user ID that matches the user account containing the OneDrive folder with the spreadsheet containing the submissions.
##' @param drive is the letter assigned to the local drive containing the directory. Default is `c`.
##' @param team is the name of the OneDrive team or group (e.g. 'sau.edu'). Default is \code{'OneDrive'}.
##' @param subdir is the path between the onedrive location and the workbook files (e.g. `ESPP/Activities`). Default is `NULL`.
##' @param a is the value of the intercept of the linear inverse-demand function (default is 10).
##' @param b is the value of the slope of the linear inverse-demand function (default is -1).
##' @param c is the value of the firm's marginal cost (default is 6).
##' @param f is the value of the firm's fixed cost (default is 0).
##' @param partners is a character string equal to \code{'students'} or \code{'random'} indicating whether students choose their own partners (default) or whether the function should generate them randomly after the students have decided their strategies. If \code{partners = 'students'}, then the form must include fields for the student's own first and last names and fields for their partners' first and last names. If partners are random, then the columns in `sheet` should either contain columns named 'First Name' and 'Last Name', or the user needs to specify the columns in `sheet` corresponding to this information to pass to `randomGroups()`.
##' @param seed is a number that sets the seed for the activity when `partners == "random"`. Default is `8675309`.
##'
##' @return \code{type} returns the type of activity (`cournotGame`).
##' @return \code{payoff} returns the payoff matrix.
##' @return \code{output} returns the matrix of quantities corresponding to the strategies.
##' @return \code{price} returns the matrix of prices corresponding to the total merket output for each combination of strategies.
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points 'won' by each student for the entire activity.
##'
##' @export

cournotGame <- function(resultsFilename,
                        rolesFilename,
                        user,
                        drive = "c",
                        team = NULL,
                        subdir = NULL,
                        a = 10,
                        b = -1,
                        c = 6,
                        f = 0,
                        partners = "random",
                        seed = 8675309,
                        ...) {
  if (a  <= 0)
    stop("The intercept of the demand function needs to be positive.")
  if (b  >= 0)
    stop("Demand curves are downward-sloping!")
  if (c <= 0)
    stop("There ain't no such thing as a free lunch (TANSTAAFL)!")
  if (f <  0)
    stop("Fixed costs must be non-negative.")
  Q.M = (a - c) / (-2 * b)
  Q.C = Q.M / 2
  Q.D = (a - c) / (-3 * b)
  Q.C.off = Q.M / 2
  Q.D.off = (a - c + b * Q.C.off) / (-2 * b)
  price <- function(q1, q2)
    a + b * (q1 + q2)
  if (partners == 'random') {
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
    results1 <- subset(results, Member == 1)[, c("First.Name", "Last.Name", "Output")]
    colnames(results1) <-
      c("First.Name.1", "Last.Name.1", "Output.1")
    results2 <- subset(results, Member == 2)[, c("First.Name", "Last.Name", "Output")]
    colnames(results2) <-
      c("First.Name.2", "Last.Name.2", "Output.2")
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
      mutate(
        First.Name = str_to_title(First.Name),
        Last.Name = str_to_title(Last.Name),
        Partner.First.Name = str_to_title(Partner.First.Name),
        Partner.Last.Name = str_to_title(Partner.Last.Name)
      )
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
    results$Group <- round(rank(apply(results[, c("group1", "group2")], 1, min)), 0) /
      2
    results$Member <- with(results, ave(Group, Group, FUN = seq_along))
    results <- as.data.frame(results[order(results$Group, results$Member), ])
    results1 <- subset(results, Member == 1)[, c("First.Name", "Last.Name", "Output")]
    colnames(results1) <-
      c("First.Name.1", "Last.Name.1", "Output.1")
    results2 <- subset(results, Member == 2)[, c("First.Name", "Last.Name", "Output")]
    colnames(results2) <-
      c("First.Name.2", "Last.Name.2", "Output.2")
    resultsWide <- cbind(results1, results2)
  }
  resultsWide$Outcome <-
    factor(paste0(resultsWide$Output.1, "-", resultsWide$Output.2))
  resultsWide$Q.1 <-
    ifelse(
      resultsWide$Output.1 == "Low",
      Q.C,
      ifelse(resultsWide$Output.2 == "Low", Q.D.off, Q.D)
    )
  resultsWide$Q.2 <-
    ifelse(
      resultsWide$Output.2 == "Low",
      Q.C,
      ifelse(resultsWide$Output.1 == "Low", Q.D.off, Q.D)
    )
  resultsWide$Price <- price(resultsWide$Q.1, resultsWide$Q.2)
  resultsWide$Profit.1 <- resultsWide$Q.1 * (resultsWide$Price - c) - f
  resultsWide$Profit.2 <- resultsWide$Q.2 * (resultsWide$Price - c) - f
  results <- data.frame(
    First.Name = c(resultsWide$First.Name.1, resultsWide$First.Name.2),
    Last.Name = c(resultsWide$Last.Name.1, resultsWide$Last.Name.2),
    Partner.First.Name = c(resultsWide$First.Name.2, resultsWide$First.Name.1),
    Partner.Last.Name = c(resultsWide$Last.Name.2, resultsWide$Last.Name.1),
    Output = c(resultsWide$Q.1, resultsWide$Q.2),
    Partner.Output = c(resultsWide$Q.2, resultsWide$Q.1),
    Outcome = c(resultsWide$Outcome, resultsWide$Outcome),
    Price = c(resultsWide$Price, resultsWide$Price),
    Profit = c(resultsWide$Profit.1, resultsWide$Profit.2)
  )
  payoffMatrix <- matrix(c(
    paste0("(", round(Q.D * (price(Q.D, Q.D) - c) - f, 4), ", ",
           round(Q.D * (price(Q.D, Q.D) - c) - f, 4), ")"),
    paste0("(", round(Q.C.off * (price(Q.C.off, Q.D.off) - c) - f, 4), ", ",
           round(Q.D.off * (price(Q.C.off, Q.D.off) - c) - f, 4), ")"),
    paste0("(", round(Q.C.off * (price(Q.C.off, Q.D.off) - c) - f, 4), ", ",
           round(Q.D.off * (price(Q.C.off, Q.D.off) - c) - f, 4), ")"),
    paste0("(", round(Q.C * (price(Q.C, Q.C) - c) - f, 4), ", ",
           round(Q.C * (price(Q.C, Q.C) - c) - f, 4), ")")),
    nrow = 2, ncol = 2)
  colnames(payoffMatrix) <-
    c("Partner = High Output", "Partner = Low Output")
  rownames(payoffMatrix) <-
    c("Strategy = High Output", "Strategy = Low Output")
  outputMatrix <- matrix(c(
    paste0("(", round(Q.D, 4), ", ", round(Q.D, 4), ")"),
    paste0("(", round(Q.C.off, 4), ", ", round(Q.D.off, 4), ")"),
    paste0("(", round(Q.D.off, 4), ", ", round(Q.C.off, 4), ")"),
    paste0("(", Q.C, ", ", Q.C, ")")),
    nrow = 2, ncol = 2)
  colnames(outputMatrix) <-
    c("Partner = High Output", "Partner = Low Output")
  rownames(outputMatrix) <-
    c("Strategy = High Output", "Strategy = Low Output")
  priceMatrix <- matrix(c(
    round(price(Q.D, Q.D), 4),
    round(price(Q.C.off, Q.D.off), 4),
    round(price(Q.C.off, Q.D.off), 4),
    round(price(Q.C, Q.C), 4)),
    nrow = 2, ncol = 2)
  colnames(priceMatrix) <-
    c("Partner = High Output", "Partner = Low Output")
  rownames(priceMatrix) <-
    c("Strategy = High Output", "Strategy = Low Output")
  grades <- results[, c("First.Name", "Last.Name", "Profit")]
  colnames(grades) <- c("First.Name", "Last.Name", "Score")
  grades  <- grades[order(grades$Last.Name, grades$First.Name), ]
  out <- list(
    type = "cournotGame",
    payoff = payoffMatrix,
    output = outputMatrix,
    price = priceMatrix,
    results = results[order(results$Last.Name, results$First.Name), ],
    grades = grades[order(grades$Last.Name, grades$First.Name), ]
  )
  class(out) <- c('econGame', class(out))
  out
}
