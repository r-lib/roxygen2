context("Rd: markdown escaping")

tag_df <- function(tag, start, end, argend = NULL) {
  df <- data.frame(
    stringsAsFactors = FALSE,
    tag = tag, start = start, end = end
  )
  if (!is.null(argend)) df$argend <- argend
  df
}

test_that("find_all_tag_names", {

  text <- "blah blah \\mytag blah blah"
  expect_equal(
    find_all_tag_names(text),
    tag_df("\\mytag", 11, 16)
  )
})

test_that("find_all_rd_tags", {

  cases <- list(
    ## No tags
    list("", character(), numeric(), numeric(), numeric()),
    list("nothing to see here",
         character(), numeric(), numeric(), numeric()),
    list("\nstill\nnothing\n",
         character(), numeric(), numeric(), numeric()),

    ## One tag
    list("blah blah \\mytag blah blah", "\\mytag", 11, 16, 16),
    list("blah blah \\mytag{arg1} blah blah",
         "\\mytag", 11, 16, 22),
    list("blah blah \\mytag{arg1}{arg2} blah blah",
         "\\mytag", 11, 16, 28),
    list("blah\\mytag", "\\mytag", 5, 10, 10),
    list("blah \\mytag", "\\mytag", 6, 11, 11),
    list("blah\\mytag{arg}", "\\mytag", 5, 10, 15),
    list("\\mytag hoohoo", "\\mytag", 1, 6, 6),
    list("\\mytag", "\\mytag", 1, 6, 6),
    list("\\mytag{arg}", "\\mytag", 1, 6, 11),
    list("blah \\mytag\nblah blah", "\\mytag", 6, 11, 11),

    ## Multiple tags
    list("blah \\tag1 \\tag2{arg} blah", c("\\tag1", "\\tag2"),
         c(6, 12), c(10, 16), c(10, 21)),
    list("blah \\tag1{ \\tag2{arg} } blah", c("\\tag1", "\\tag2"),
         c(6, 13), c(10, 17), c(24, 22)),
    list("blah \\tag1{\n\\tag2{arg}\n} blah", c("\\tag1", "\\tag2"),
         c(6, 13), c(10, 17), c(24, 22))
  )

  for (case in cases) {
    expect_equal(
      find_all_rd_tags(case[[1]]),
      do.call(tag_df, case[-1]),
      info = case[[1]]
    )
  }

})

test_that("find_fragile_rd_tags", {

  fragile <- c("\\frag", "\\frag1", "\\frag2")

  cases <- list(
    list("This is \\frag{here}, \\this{arg} not", "\\frag"),
    list("Embedded \\frag{ into \\frag1{arg} plus }", "\\frag"),
    list(
      "blah \\cmd{ \\frag{arg} \\frag{arg} } \\frag2 blah",
      c("\\frag", "\\frag", "\\frag2")
    )
  )

  for (case in cases) {
    expect_equal(
      find_fragile_rd_tags(case[[1]], fragile)$tag,
      case[[2]],
      info = case[[1]]
    )
  }

})


test_that("str_sub_same", {

  expect_equal(
    str_sub_same(
      "123456789ab",
      data.frame(start = c(1,6), end = c(2,10), argend = c(2,10)),
      "xxx"
    ),
    "xxx-1-345xxx-2-b"
  )

  expect_equal(
    str_sub_same(
      "123456789ab",
      data.frame(start = c(1,8), end = c(7,10), argend = c(7,10)),
      "xxx"
    ),
    "xxx-1-xxx-2-b",
  )

  expect_equal(
    str_sub_same(
      "123456789ab",
      data.frame(start = numeric(), end = numeric(),
                 argend = numeric()),
      "xxx"
    ),
    "123456789ab"
  )

})
