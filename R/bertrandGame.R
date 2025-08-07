##' Tabulate results for a simple in-class stag hunt game.
##'
##' @details \code{bertrandGame} tabulates the results of a simple stag hunt game in which students' points depend on their strategy and the strategy chosen by their randomly-assigned partners.
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
##' @return \code{type} returns the type of activity (`bertrandGame`).
##' @return \code{payoff} returns the payoff matrix.
##' @return \code{output} returns the matrix of quantities corresponding to the strategies.
##' @return \code{price} returns the matrix of prices corresponding to the total merket output for each combination of strategies.
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points 'won' by each student for the entire activity.
##'
##' @export

bertrandGame <-
  function(resultsFilename,
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
    P.C <- (a + c)/2
    Profit.C <- (a - c)^2/(-4*b)
    Q.D = (a - c) / (-3 * b)
    Q.D.off = (a - c + b * Q.C) / (-2 * b)
    price <- function(q1, q2)
      a + b * (q1 + q2)
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
      results1 <- subset(results, Member == 1)[, c("First.Name", "Last.Name", "Price")]
      colnames(results1) <-
        c("First.Name.1", "Last.Name.1", "Price.1")
      results2 <- subset(results, Member == 2)[, c("First.Name", "Last.Name", "Price")]
      colnames(results2) <-
        c("First.Name.2", "Last.Name.2", "Price.2")
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
      results1 <- subset(results, Member == 1)[, c("First.Name", "Last.Name", "Price")]
      colnames(results1) <-
        c("First.Name.1", "Last.Name.1", "Price.1")
      results2 <- subset(results, Member == 2)[, c("First.Name", "Last.Name", "Price")]
      colnames(results2) <-
        c("First.Name.2", "Last.Name.2", "Price.2")
      resultsWide <- cbind(results1, results2)
    }
    resultsWide$Price.M  <- apply(
      resultsWide[, c("Price.1", "Price.2")], 1, min)
    resultsWide$Q.M <- (a - resultsWide$Price.M) / (-b)
    resultsWide$Q.1 <- ifelse(
      resultsWide$Price.1 > resultsWide$Price.M,
      0,
      ifelse(
        resultsWide$Price.1 == resultsWide$Price.2,
        resultsWide$Q.M / 2,
        resultsWide$Q.M
      )
    )
    resultsWide$Q.2 <- ifelse(
      resultsWide$Price.2 > resultsWide$Price.M,
      0,
      ifelse(
        resultsWide$Price.1 == resultsWide$Price.2,
        resultsWide$Q.M / 2,
        resultsWide$Q.M
      )
    )
    resultsWide$Profit.1 <- resultsWide$Q.1 * (resultsWide$Price.M - c) - f
    resultsWide$Profit.2 <- resultsWide$Q.2 * (resultsWide$Price.M - c) - f
    results <- data.frame(
      First.Name = c(resultsWide$First.Name.1, resultsWide$First.Name.2),
      Last.Name = c(resultsWide$Last.Name.1, resultsWide$Last.Name.2),
      Partner.First.Name = c(resultsWide$First.Name.2, resultsWide$First.Name.1),
      Partner.Last.Name = c(resultsWide$Last.Name.2, resultsWide$Last.Name.1),
      Price = c(resultsWide$Price.1, resultsWide$Price.2),
      Market.Price = c(resultsWide$Price.M, resultsWide$Price.M),
      Output = c(resultsWide$Q.1, resultsWide$Q.2),
      Partner.Output = c(resultsWide$Q.2, resultsWide$Q.1),
      Profit = c(resultsWide$Profit.1, resultsWide$Profit.2)
    )
    results <- results[order(results$Last.Name, results$First.Name),]
    payoffMatrix <- matrix(c(
      paste0("(", 0, ", ", 0, ")"),
      paste0("(", 0, ", ", round((P.C - 0.01 - c)*(a + b*(P.C - 0.01)) - f, 2), ")"),
      paste0("(", round((P.C - 0.01 - c)*(a + b*(P.C - 0.01)) - f, 2), ", ", 0, ")"),
      paste0("(", round(Profit.C/2, 2), ", ", round(Profit.C/2, 2), ")")),
      nrow = 2, ncol = 2)
    colnames(payoffMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(payoffMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")
    outputMatrix <- matrix(c(
      paste0("(", (a + b*c)/2, ", ", (a + b*c)/2, ")"),
      paste0("(", 0, ", ", a + b*(P.C - 0.01), ")"),
      paste0("(", a + b*(P.C - 0.01), ", ", 0, ")"),
      paste0("(", Q.C, ", ", Q.C, ")")),
      nrow = 2, ncol = 2)
    colnames(outputMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(outputMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")
    priceMatrix <- matrix(c(c, P.C - 0.01, P.C - 0.01, P.C), nrow = 2, ncol = 2)
    colnames(priceMatrix) <- c("Partner = Defect", "Partner = Collude")
    rownames(priceMatrix) <- c("Strategy = Defect", "Strategy = Collude")
    grades <- results[, c("First.Name", "Last.Name", "Profit")]
    colnames(grades) <- c("First.Name", "Last.Name", "Score")
    out <- list(type = "bertrandGame",
                payoff = payoffMatrix,
                output = outputMatrix,
                price = priceMatrix,
                results = results,
                grades = grades)
    class(out) <- c("econGame", class(out))
    out
  }
