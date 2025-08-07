##' Creates a random list of partner groups
##'
##' @details \code{partnerList} uses a random number to divide a \code{studentList} groups of size N.
##'
##' @param filename (required) is a file name corresponding to the Excel spreadsheet containing the submissions (e.g. 'Student List.xlsx'; required).
##' @param user is the user ID that matches the user account containing the OneDrive folder with the spreadsheet containing the submissions.
##' @param drive is the letter assigned to the local drive containing the directory (e.g. 'c'). Default is \code{'OneDrive'}.
##' @param team is the name of the OneDrive team or group (e.g. 'sau.edu'). Default is \code{'OneDrive'}.
##' @param subdir is the path between the onedrive location and the workbook files (e.g. `ESPP/Activities`). Default is `NULL`.
##' @param size is the size of each group (default is `2`).
##' @param seed is a random seed (default is `8675309`).
##'
##' @return \code{studentList} returns list of names and group numbers.
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @export

randomGroups <- function(filename,
                         user,
                         drive = "c",
                         team = NULL,
                         subdir = NULL,
                         size = 2,
                         seed = 8675309,
                         ...) {
  if(is.null(team)) {
    team <- "/OneDrive/"
  } else {
    team <- paste0("/OneDrive - ", team, "/")
  }
  path <- paste0(drive, ":/Users/", user, team, subdir, filename)
  studentList <- readxl::read_excel(path)
  colnames(studentList) <- make.names(colnames(studentList))
  studentList$First.Name <- make.names(studentList$First.Name)
  studentList$Last.Name <- make.names(studentList$Last.Name)
  studentList <- studentList %>%
    replace_na(list(First.Name = "John", Last.Name = "Doe")) %>%
    mutate(First.Name = str_to_title(First.Name),
           Last.Name = str_to_title(Last.Name))
  set.seed(seed)
  studentList$Group <- runif(nrow(studentList))
  studentList <- studentList %>%
    mutate(Group = ceiling(rank(Group) / size))
  studentList$Member <- with(studentList,
                             ave(Group, Group, FUN = seq_along))
  studentList <-
    as.data.frame(studentList[order(studentList$Group, studentList$Member),])
  studentListWide <- reshape(studentList,
                             idvar = c("Group"),
                             timevar = "Member",
                             direction = "wide")
  studentListWide$Member.1 <- 1
  studentListWide$Member.2 <- 2
  out <- list(
    long = studentList,
    wide = studentListWide
  )
  out
}
