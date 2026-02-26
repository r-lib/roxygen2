tag_df <- function(tag, start, end, argend = NULL) {
  df <- data.frame(
    tag = tag,
    start = start,
    end = end
  )
  if (!is.null(argend)) {
    df$argend <- argend
  }
  df
}

test_that("find_all_tag_names", {
  text <- r"(blah blah \mytag blah blah)"
  expect_equal(
    find_all_tag_names(text),
    tag_df(r"(\mytag)", 11, 16)
  )
})

test_that("find_all_rd_tags", {
  cases <- list(
    ## No tags
    list("", character(), numeric(), numeric(), numeric()),
    list("nothing to see here", character(), numeric(), numeric(), numeric()),
    list("\nstill\nnothing\n", character(), numeric(), numeric(), numeric()),

    ## One tag
    list(r"(blah blah \mytag blah blah)", r"(\mytag)", 11, 16, 16),
    list(r"(blah blah \mytag{arg1} blah blah)", r"(\mytag)", 11, 16, 22),
    list(r"(blah blah \mytag{arg1}{arg2} blah blah)", r"(\mytag)", 11, 16, 28),
    list(r"(blah\mytag)", r"(\mytag)", 5, 10, 10),
    list(r"(blah \mytag)", r"(\mytag)", 6, 11, 11),
    list(r"(blah\mytag{arg})", r"(\mytag)", 5, 10, 15),
    list(r"(\mytag hoohoo)", r"(\mytag)", 1, 6, 6),
    list(r"(\mytag)", r"(\mytag)", 1, 6, 6),
    list(r"(\mytag{arg})", r"(\mytag)", 1, 6, 11),
    list("blah \\mytag\nblah blah", r"(\mytag)", 6, 11, 11),

    ## Multiple tags
    list(
      r"(blah \tag1 \tag2{arg} blah)",
      c(r"(\tag1)", r"(\tag2)"),
      c(6, 12),
      c(10, 16),
      c(10, 21)
    ),
    list(
      r"(blah \tag1{ \tag2{arg} } blah)",
      c(r"(\tag1)", r"(\tag2)"),
      c(6, 13),
      c(10, 17),
      c(24, 22)
    ),
    list(
      "blah \\tag1{\n\\tag2{arg}\n} blah",
      c(r"(\tag1)", r"(\tag2)"),
      c(6, 13),
      c(10, 17),
      c(24, 22)
    )
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
  fragile <- c(r"(\frag)", r"(\frag1)", r"(\frag2)")

  cases <- list(
    list(r"(This is \frag{here}, \this{arg} not)", r"(\frag)"),
    list(r"(Embedded \frag{ into \frag1{arg} plus })", r"(\frag)"),
    list(
      r"(blah \cmd{ \frag{arg} \frag{arg} } \frag2 blah)",
      c(r"(\frag)", r"(\frag)", r"(\frag2)")
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
      data.frame(start = c(1, 6), end = c(2, 10), argend = c(2, 10)),
      "xxx"
    ),
    "xxx-1-345xxx-2-b"
  )

  expect_equal(
    str_sub_same(
      "123456789ab",
      data.frame(start = c(1, 8), end = c(7, 10), argend = c(7, 10)),
      "xxx"
    ),
    "xxx-1-xxx-2-b"
  )

  expect_equal(
    str_sub_same(
      "123456789ab",
      data.frame(start = numeric(), end = numeric(), argend = numeric()),
      "xxx"
    ),
    "123456789ab"
  )
})
