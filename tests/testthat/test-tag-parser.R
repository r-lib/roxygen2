test_that("tags containing only whitespace generate warning", {
  tag <- roxy_tag("foo", " ")

  expect_parse_failure <- function(code)  {
    expect_warning(out <- code, "requires")
    expect_null(out)
  }

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
