context("strippedConsoleText must delete strings not beginning with the
         commmand or continuation prompt, and delete these prompts from the
         remaining strings")

test_that("strippedConsoleText works correctly", {

  testVec <- c("> a", ">  b", "> c ",
               "+ d", "+ +e", "+ f> ",
               ">g", " > h",
               "+i", " + j",
               "k", "l> ", "m + ",
               "[1] n", "Warning: o",
               "> \"Error: p\"")

  expect_identical(ccopy:::strippedConsoleText(testVec),
                   c("a", " b", "c ", "d", "+e", "f> ", "\"Error: p\""))

})
