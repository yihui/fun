##' Test Alzheimer's disease by finding out the different character in
##' a character rectangle
##'
##' Please try hard to find the letter "N" in 300 "M"s, one "6" in 300
##' "9"s, etc.
##'
##' Follow the instructions and finish the test.
##' @param char1 the 'background' character
##' @param char2 the character to be found out
##' @param nr number of rows of the character rectangle
##' @param nc number of columns
##' @param seed seed for random number generation
##' @param ... other arguments passed to \code{\link[base]{set.seed}}
##' @return If at least one test item has been passed, a data.frame
##' will be returned telling the result of the test.
##' @note Don't be too serious about this test. I'm no doctor, but I
##' think this will be a good present to your friends.
##' @author Yihui Xie <\url{http://yihui.name}>
##' @export
##' @examples
##' x = alzheimer_test()
alzheimer_test = function(char1 = c("9", "O", "M", "I",
    "F", "D"), char2 = c("6", "C", "N", "T", "E", "O"), nr = 10,
    nc = 30, seed = NULL, ...) {
    if (!interactive()) return()
    cat("This is a REAL neurological test. Sit comfortably and be calm.\n\n")
    mlen = max(length(char1), length(char2), length(nr), length(nc))
    char1 = rep(char1, length = mlen)
    char2 = rep(char2, length = mlen)
    nr = rep(nr, length = mlen)
    nc = rep(nc, length = mlen)
    if (!is.null(seed))
        set.seed(seed, ...)
    tm1 = tm2 = ans = ans.u = ans.t = NULL
    for (j in 1:mlen) {
        x = rep(char1[j], nr[j] * nc[j])
        idx = sample(nr[j] * nc[j], 1)
        x[idx] = char2[j]
        mx = matrix(x, nr[j], nc[j])
        cat("\n\nTEST ", j, "\n")
        writeLines(formatUL(c(paste("Find the \"", char2[j],
            "\" below", sep = ""), "Do not use any cursor help"),
            offset = 2))
        cat("\n")
        m = menu(c("Ready, Go!", "Let me quit the test!"))
        if (m == 0 | m == 2) {
            j = j - 1
            break
        }
        tmp = Sys.time()
        cat("\n")
        cat(apply(mx, 1, paste, collapse = ""), sep = "\n")
        cat("\nFind it now?\n")
        m = menu(c("Yes! (Input the answer later)", "No... (See the answer later)"))
        tm1 = c(tm1, as.numeric(difftime(Sys.time(), tmp, units = "secs")))
        ans.true = c(ifelse(idx%%nr[j] == 0, nr[j], idx%%nr[j]),
            idx%/%nr[j] + 1)
        tmp = Sys.time()
        if (m == 0 | m == 2) {
            cat("\nCharacter \"", char2[j], "\" is at [", ifelse(idx%%nr[j] ==
                0, nr[j], idx%%nr[j]), ", ", idx%/%nr[j] + 1,
                "].\n\n\n", sep = "")
            ans = c(ans, 3)
            ans.user = c(NA, NA)
        } else {
            cat("\nPlease input the Row number and Column number respectively\n  when you find the character:\n")
            ans.user = scan(nmax = 2)
            if (length(ans.user) == 2 & is.numeric(ans.user)) {
                ans = c(ans, as.integer(all(ans.user == ans.true)))
            } else ans = c(ans, 2)
            if (ans[length(ans)] != 1)
                cat("\nWrong answer! :( \nThe correct answer should be: ",
                  ans.true, "\n\n\n")
        }
        ans.u = rbind(ans.u, ans.user)
        ans.t = rbind(ans.t, ans.true)
        tm2 = c(tm2, as.numeric(difftime(Sys.time(), tmp, units = "secs")))
        if (j < mlen) {
            for (i in 1:round(max(nc) * 1.5)) {
                cat("  [Take a rest and continue the next test> ",
                  rep("-", i), ">\r", sep = "")
                flush.console()
                Sys.sleep(0.1)
            }
        } else {
            for (i in 1:round(max(nc) * 1.5)) {
                cat("  [All tests are finished; see the results> ",
                  rep("-", i), ">\r", sep = "")
                flush.console()
                Sys.sleep(0.1)
            }
        }
    }
    if (j >= 1) {
        cat("\nThere are", sum(ans == 1), "correct answers in all", j, "tests.\n")
        res = data.frame(char1[1:j], char2[1:j], tm1[1:j], tm2[1:j],
            ans[1:j], ans.u[1:j, , drop = FALSE], ans.t[1:j, , drop = FALSE])
        return(res)
    }
}
