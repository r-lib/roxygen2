test_that("tags containing only whitespace generate warning", {
  expect_parse_failure <- function(code)  {
    (expect_warning(expect_null(code)))
  }

  expect_snapshot({
    tag <- roxy_test_tag(" ")
    expect_parse_failure(tag_value(tag))
    expect_parse_failure(tag_inherit(tag))
    expect_parse_failure(tag_name(tag))
    expect_parse_failure(tag_two_part(tag))
    expect_parse_failure(tag_name_description(tag))
    expect_parse_failure(tag_code(tag))
    expect_parse_failure(tag_examples(tag))
    expect_parse_failure(tag_markdown(tag))
    expect_parse_failure(tag_markdown_with_sections(tag))
  })
})

test_that("tags check for mismatched parents gives useful warnings", {
  expect_parse_failure <- function(code)  {
    (expect_warning(expect_null(code)))
  }

  expect_snapshot({
    tag <- roxy_test_tag("a {")
    expect_parse_failure(tag_value(tag))
    expect_parse_failure(tag_inherit(tag))
    expect_parse_failure(tag_name(tag))
    expect_parse_failure(tag_two_part(tag))
    expect_parse_failure(tag_words(tag))
    expect_parse_failure(tag_words_line(tag))
    expect_parse_failure(tag_examples(tag))

    "markdown tags return empty values"
    (expect_warning(tag_markdown(tag)))
    (expect_warning(tag_markdown_with_sections(tag)))
  })

  local_markdown()
  expect_snapshot({
    markdown_on()
    tag <- roxy_test_tag("{")
    (expect_warning(tag_markdown(tag)))
    tag <- roxy_test_tag("# one\ntwo\n# three\nfour {")
    (expect_warning(tag_markdown_with_sections(tag)))
  })
})

test_that("tag_inhert checks for valid inherits", {
  expect_snapshot({
    tag <- roxy_test_tag("foo params sction")
    . <- tag_inherit(tag)
  })
})
