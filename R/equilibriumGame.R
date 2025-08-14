##' Tabulate market equilibrium game results.
##'
##' Tabulates and assigns points for the results of a simple in-class market equilibrium game using random values.
##'
##' @details \code{equilibriumGame} tabulates the results of a simple equilibrium game based on Holt (1996). The instructor informs the students that they own a single unit of a eCoin currency that each of them values differently. Students then receive a random value from 1 to 10 using a link to a Google Sheet. This number represents their (constant) value they attach to the unit they presently own and for if they were to acquire one more unit of eCoin. Students submit their name, their value draw, a "bid" corresponding to the highest amount they would pay for a second eCoin, and an "ask" corresponding to the lowest amount they would accept to part with the eCoin they already own. Students keep their consumer and producer surpluses from each round as "extra credit" points. \code{equilibriumGame} tabulates the supply and demand schedules; calculates the equilibrium (with the help of a C++ helper function provided by "David" on Stack Overflow, \url{https://stackoverflow.com/questions/23830906/intersection-of-two-step-functions/}); graphs the equilibrium; and tabulates the scores for each student.
##'
##' @param filename (required) is a file name corresponding to the Excel spreadsheet containing the submissions (e.g. 'Pollution Activity,xlsx'; required).
##' @param user (required) is the user ID that matches the user account containing the OneDrive folder with the spreadsheet containing the submissions.
##' @param drive is the letter assigned to the local drive containing the directory. Default is `"c"`.
##' @param team is the name of the OneDrive team or group (e.g. 'sau.edu'). Default is \code{'OneDrive'}.
##' @param subdir is the path between the onedrive location and the workbook files (e.g. `ESPP/Activities`). Default is `NULL`.
##'
##' @return \code{type} the type of activity (equlibriumGame).
##' @return \code{results} the original submissions (with market price and points added).
##' @return \code{rounds} the number of rounds in `results`.
##' @return \code{schedules} returns a list containing the supply and demand schedules for each round.
##' @return \code{equilibria} returns a list containing the equilibria for each round.
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @references Holt, Charles A. (1996). Classroom games: Trading in a pit market. \emph{Journal of Economic Perspectives,} 10(1), pp.193-203.
  ##'
##' @export

equilibriumGame <-
  function(filename,
           user,
           drive = "c",
           team = NULL,
           subdir = NULL,
           ...) {
    Rcpp::sourceCpp(
      code = '
      #include <Rcpp.h>
      #include <map>
      // [[Rcpp::export]]
      Rcpp::List find_optimum(Rcpp::NumericVector price_supply,
        Rcpp::NumericVector quant_supply,
        Rcpp::NumericVector price_demand,
        Rcpp::NumericVector quant_demand) {
          std::map<double, double> supply;
          std::map<double, double> demand;
          // fill the maps
          for (int i = 0; i < price_supply.size(); ++i) {
            supply[price_supply[i]] += quant_supply[i];
          }
          for (int i = 0; i < price_demand.size(); ++i) {
            demand[price_demand[i]] += quant_demand[i];
          }
          if (supply.empty() || demand.empty())
            return Rcpp::List::create(Rcpp::Named("price") = 0,
              Rcpp::Named("quantity") = 0);
          auto sIt = supply.begin(), nextS = std::next(sIt, 1);
          const auto endS = supply.end();
          auto dIt = demand.rbegin(), nextD = std::next(dIt, 1);
          const auto endD = demand.rend();\
          // quantity and prices at either side
          double pS = sIt->first, pD = dIt->first;
          double qS = 0, qD = 0;
          // next prices
          double nextPS = nextS->first, nextPD = nextD->first;
          if (pD < pS)
            return Rcpp::List::create(Rcpp::Named("price") = 0, Rcpp::Named("quantity") = 0);
          // add the best price from each side!
          qS += sIt->second;
          qD += dIt->second;
          while (pS < pD) {
            if (nextS == endS && nextD == endD) {
              pD = qD < qS ? pS : pD;
              break;
            }
            while (qS <= qD && sIt != endS && nextS->first <= pD) {
              ++sIt;
              ++nextS;
              pS = sIt->first;
              qS += sIt->second;
            }
            if (sIt == endS) break;
            if (nextD->first < pS) {
              pD = qD < qS ? pS : pD;
              break;
            }
            while (qD < qS && dIt != endD && nextD->first >= pS) {
              ++dIt;
              ++nextD;
              pD = dIt->first;
              qD += dIt->second;
            }
            if (dIt == endD) break;
          }
          double price = pD;
          double vol = qS < qD ? qS : qD;
          return Rcpp::List::create(Rcpp::Named("price") = price,
            Rcpp::Named("quantity") = vol);
    }
                    '
    )
    # Set up the Google Sheets, read responses, and initialize output objects.
    drive <- paste0(drive, ":/Users/")
    if (is.null(team)) {
      team <- "/OneDrive/"
    } else {
      team <- paste0("/OneDrive - ", team, "/")
    }
    path <- paste0(drive, user, team, subdir, filename)
    results <- readxl::read_excel(path)
    results[, 8:11] <- lapply(results[8:11], as.numeric)
    colnames(results) <- make.names(colnames(results))
    results <- results %>%
      replace_na(list(First.Name = "John", Last.Name = "Doe")) %>%
      mutate(First.Name = str_to_title(First.Name),
             Last.Name = str_to_title(Last.Name))
    responses <- nrow(results)
    results$qi <- 1
    results$Price <- NULL
    schedule <- as.data.frame(matrix(
      nrow = 10,
      ncol = 3,
      dimnames = list(NULL, c('Price', 'Demand', 'Supply'))
    ))
    schedule$Price <- c(1:10)
    responses <- nrow(results)
    for (i in 1:10) {
      schedule$Demand[i] <- nrow(subset(results, Bid >= i))
      schedule$Supply[i] <- nrow(subset(results, Ask <= i))
    }
    # Calculate the equilibrium for each round
    equilibrium <- find_optimum(
      results$Ask, results$qi, results$Bid, results$qi)
    results$Price <- equilibrium$price
    # Calculate student points.
    results$Score <-
      I(results$Bid >= results$Price) * (results$Benefit - results$Price) +
      I(results$Ask <= results$Price) * (results$Price - results$Cost)
    results <- data.frame(
      First.Name = results$First.Name,
      Last.Name = results$Last.Name,
      Benefit = results$Benefit,
      Cost = results$Cost,
      Bid = results$Bid,
      Ask = results$Ask,
      Price = results$Price,
      Score = results$Score
    )
    grades <- data.frame(
      First.Name = results$First.Name,
      Last.Name = results$Last.Name,
      Score = results$Score
    )
    out <- list(
      type = "equilibriumGame",
      results = as.data.frame(
        results[order(results$Last.Name, results$First.Name), ]),
      schedule = schedule,
      equilibrium = equilibrium,
      grades = as.data.frame(
        grades[order(grades$Last.Name, grades$First.Name), ])
    )
    class(out) <- c('econGame', class(out))
    out
  }
