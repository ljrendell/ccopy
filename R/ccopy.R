#' Strip output, messages and command/continuation prompts from console text
#'
#' Utility function. For a vector of character strings, each string that does
#' not commence either with \code{"> "} (the console command prompt) or \code{"+
#' "} (the console continuation prompt) is removed. These prompt characters are
#' then deleted from each of the remaining strings.
#'
#' @param charVec A character vector (as might be read from the system
#'   clipboard, where each element corresponds a new line of text). Intended to
#'   contain text copied from the R console.
#'
#' @return A character vector obtained from \code{charVec} by deleting those
#'   elements not commencing with \code{"> "} or \code{"+ "}, and from the
#'   remaining strings deleting these first two characters.
#'
#' @keywords internal
strippedConsoleText <- function(charVec) {
  # Retain only those strings beginning with "> " or "+ "
  charVec <- grep("^[>+] ", charVec, value = TRUE)
  # Remove first two characters from each string
  charVec <- substr(charVec, 3, nchar(charVec))
  return(charVec)
}


#' Copy commands only
#'
#' Take the content of the system clipboard (assumed to contain text copied from
#' the R console), delete any lines of text not commencing with \code{"> "} or
#' \code{"+ "}, delete these characters from the beginning of all remaining
#' lines, and then write these remaining lines back to the clipboard.
#'
#' @details This function should be run when the system clipboard contains text
#'   copied from the R console. \code{"> "} is the default console command
#'   prompt in R, and \code{"+ "} is the default console continuation prompt;
#'   deleting all lines of text that do not commence with these strings is
#'   intended to delete all lines that do not correspond to user commands (such
#'   as lines of output, or error/warning messages). From these remaining lines,
#'   the first two characters (either \code{"> "} and \code{"+ "}) are deleted.
#'
#'   The resulting lines comprise executable R code, comprising only the user
#'   commands from the original console text. This is written to the clipboard
#'   in order that it may be pasted by the user.
#'
#' @export
#'
#' @seealso \code{\link{pco}}, \code{\link{rco}}
#'
#' @examples
#' ## Suppose the following (uncommented) lines are copied to the clipboard:
#' # > a <- 10
#' # > b <- 5
#' # > sum(a,
#' # + b)
#' # [1] 15
#' ## The following line does this copying:
#' writeLines(c("> a <- 10", "> b <- 5", "> sum(a,", "+ b)", "[1] 15"),
#'   "clipboard")
#'
#' ## Copy commands only:
#' cco()
#'
#' ## Pasting from the clipboard gives the following (uncommented) lines:
#' # a <- 10
#' # b <- 5
#' # sum(a,
#' # b)
cco <- function() {
  text <- readLines("clipboard", warn = FALSE)
  text <- strippedConsoleText(text)
  text <- paste(text, collapse = "\n")
  writeLines(text, "clipboard", sep = "") # Prevents extra new line at end
}


#' Print commands only
#'
#' Take the content of the system clipboard (assumed to contain text copied from
#' the R console), delete any lines of text not commencing with \code{"> "} or
#' \code{"+ "}, delete these characters from the beginning of all remaining
#' lines, and then print these remaining lines in the console.
#'
#' @details This function should be run when the system clipboard contains text
#'   copied from the R console. \code{"> "} is the default console command
#'   prompt in R, and \code{"+ "} is the default console continuation prompt;
#'   deleting all lines of text that do not commence with these strings is
#'   intended to delete all lines that do not correspond to user commands (such
#'   as lines of output, or error/warning messages). From these remaining lines,
#'   the first two characters (either \code{"> "} and \code{"+ "}) are deleted.
#'
#'   The resulting lines comprise executable R code, comprising only the user
#'   commands from the original console text. This is printed as output in the R
#'   console (though not executed).
#'
#' @export
#'
#' @seealso \code{\link{cco}}, \code{\link{rco}}
#'
#' @examples
#' ## Suppose the following (uncommented) lines are copied to the clipboard:
#' # > a <- 10
#' # > b <- 5
#' # > sum(a,
#' # + b)
#' # [1] 15
#' ## The following line does this copying:
#' writeLines(c("> a <- 10", "> b <- 5", "> sum(a,", "+ b)", "[1] 15"),
#'   "clipboard")
#'
#' ## Print commands only:
#' pco()
#' ## The following output is printed:
#' # a <- 10
#' # b <- 5
#' # sum(a,
#' # b)
pco <- function() {
  text <- readLines("clipboard", warn = FALSE)
  text <- strippedConsoleText(text)
  cat(text, sep = "\n")
}


#' Run commands only
#'
#' Take the content of the system clipboard (assumed to contain text copied from
#' the R console), delete any lines of text not commencing with \code{"> "} or
#' \code{"+ "}, delete these characters from the beginning of all remaining
#' lines, and then execute these remaining lines in the console.
#'
#' @param ... Arguments to be passed to \code{\link{source}} (see below).
#'
#' @details This function should be run when the system clipboard contains text
#'   copied from the R console. \code{"> "} is the default console command
#'   prompt in R, and \code{"+ "} is the default console continuation prompt;
#'   deleting all lines of text that do not commence with these strings is
#'   intended to delete all lines that do not correspond to user commands (such
#'   as lines of output, or error/warning messages). From these remaining lines,
#'   the first two characters (either \code{"> "} and \code{"+ "}) are deleted.
#'
#'   The resulting lines comprise executable R code, comprising only the user
#'   commands from the original console text. This code is executed using the
#'   base function \code{\link{source}}, to which any arguments are passed (e.g.
#'   \code{echo = TRUE} causes each expression to be printed before evaluation).
#'
#' @export
#'
#' @examples
#' #' ## Suppose the following (uncommented) lines are copied to the clipboard:
#' # > a <- 10
#' # > b <- 5
#' # > sum(a,
#' # + b)
#' # [1] 15
#' ## The following line does this copying:
#' writeLines(c("> a <- 10", "> b <- 5", "> sum(a,", "+ b)", "[1] 15"),
#'   "clipboard")
#'
#' ## Run commands only:
#' rco()
#'
#' ## Check assigned value of a (should be 10):
#' a
rco <- function(...) {
  text <- readLines("clipboard", warn = FALSE)
  text <- strippedConsoleText(text)
  source(textConnection(text), ...)
}
