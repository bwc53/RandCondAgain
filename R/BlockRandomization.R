#' Block Randomization Function
#'
#' The purpose of this function is to randomize participants to experimental conditions
#'
#' @param conditions Number of experimental conditions
#' @param subjects.per.condition Number of participants per group
#' @param groups List in which each element corresponds to one stratifying variable
#'         and each element is a character vector consisting of names for the levels of
#'         the stratifying variable
#' @return List in which each element is the randomization sequence for a single group
#' @examples
#' mydesign <- blockran(5, 10)
#' mydesign <- blockran(4, 5, list(Gender = c("Male", "Female"), History = c("None", "Low", "High")))
#' @export
#'
blockran <- function (conditions, subjects.per.condition, groups = NULL) {
  num.vars <- length(groups)
  num.groups <- 1
  if (num.vars == 0) {
    group.names <- 'Random Sequence'
  }
  else {
    for (i in 1:num.vars) {
      num.groups <- num.groups * length(groups[[i]])
    }
    group.names <- vector(mode = "character", length = num.groups)
    reps <- 1
    for (i in 1:num.vars) {
      for (igroups in 1:num.groups) {
        group.names[igroups] <- paste(group.names[igroups], names(groups)[[i]], ' = ')
      }
      ct.groups <- 0
      ct.repeat <- num.groups / length(groups[[i]]) / reps
      for (ct.reps in 1:reps) {
        for (j in 1:length(groups[[i]])) {
          for (k in 1:ct.repeat) {
            ct.groups <- ct.groups + 1
            if (!i == num.vars) {
              group.names[ct.groups] <- paste(group.names[ct.groups], groups[[i]][j], ';')
            }
            else {
              group.names[ct.groups] <- paste(group.names[ct.groups], groups[[i]][j])
            }
          }
        }
      }
      reps <- reps * length(groups[[i]])
    }
  }
  sequences <- array(NA, dim = c(subjects.per.condition, conditions, num.groups))
  tsequence <- matrix(nrow = subjects.per.condition, ncol = conditions)
  for (i in 1:num.groups) {
    for (j in 1:subjects.per.condition) {
      tsequence[j, ] <- sample(1:conditions, size = conditions, replace = FALSE)
    sequences[,,i] <- tsequence
    }
  }
  return(list(group.names, sequences))
}
