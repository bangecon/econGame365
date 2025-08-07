##' Randomly assigns partner roles
##'
##' @details \code{randomRoles} uses a random number to assign partner roles (e.g. leader-follower, proposer-responder) to a list of student names a Google Sheet containing a student roster into
##'
##' @param filename (required) is a file name corresponding to the Excel spreadsheet containing the submissions (e.g. 'Student List.xlsx'; required).
##' @param user is the user ID that matches the user account containing the OneDrive folder with the spreadsheet containing the submissions.
##' @param drive is the letter assigned to the local drive containing the directory (e.g. 'c'). Default is \code{'OneDrive'}.
##' @param team is the name of the OneDrive team or group (e.g. 'sau.edu'). Default is \code{'OneDrive'}.
##' @param subdir is the path between the onedrive location and the workbook files (e.g. `ESPP/Activities`). Default is `NULL`.
##' @param size is the size of each group (default is `2`).
##' @param seed is a random seed (default is `8675309`).
##' @param roleLabs contains the labels for the roles (default is `c("Leader", "Follower")`)
##'
##' @return \code{studentList} returns list of names and group numbers.
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @export

randomRoles <- function(filename,
                        user,
                        drive = "c",
                        team = NULL,
                        subdir = NULL,
                        size = 2,
                        seed = 8675309,
                        roleLabs = c("First Mover", "Second Mover"),
                        ...) {
  drive <- paste0(drive, ":/Users/")
  if(is.null(team)) {
    team <- "OneDrive"
  } else {
    team <- paste0("/OneDrive - ", team, "/")
  }
  path <- paste0(drive, user, team, subdir, filename)
  studentList <- readxl::read_excel(path)
  colnames(studentList) <- make.names(colnames(studentList))
  studentList$First.Name <- make.names(studentList$First.Name)
  studentList$Last.Name <- make.names(studentList$Last.Name)
  studentList <- studentList %>%
    replace_na(list(First.Name = "John", Last.Name = "Doe")) %>%
    mutate(First.Name = str_to_title(First.Name),
           Last.Name = str_to_title(Last.Name))
  set.seed(seed)
  studentList$Rand <- runif(nrow(studentList))
  responses <- nrow(studentList)
  studentList <- studentList %>%
    mutate(Role = factor(rank(Rand) / responses <= 0.5,
                         labels = c(roleLabs[1], roleLabs[2])))
  out.long <- studentList[order(studentList$Role),
                          c("First.Name", "Last.Name", "Role")] %>%
    as.data.frame()
  out.first <- subset(out.long, Role == roleLabs[1])
  colnames(out.first) <- c("First.Name.1", "Last.Name.1", "Role.1")
  out.second <- subset(out.long, Role == roleLabs[2])
  colnames(out.second) <- c("First.Name.2", "Last.Name.2", "Role.2")
  out.wide <- cbind(out.first, out.second) |>
    as.data.frame()
  out <- list(
    long = out.long,
    wide = out.wide
  )
  out
}
