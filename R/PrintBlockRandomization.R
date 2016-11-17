#' Print Block Randomization Function
#'
#' The purpose of this function is to print the output from \code{blockran}, the block
#' randomization function.
#'
#' @param design.list List in which each element is the randomization sequence for a single group
#' @return None
#' @examples
#' pblockran(blockran(4, 5, list(Gender = c("Male", "Female"), History = c("None", "Low", "High"))))
#' @export
#'
pblockran <- function (design.list) {
  for (i in 1:length(design.list[[1]])) {
    print(design.list[[1]][i], quote = FALSE)
    print(design.list[[2]][,,i])
  }
}
