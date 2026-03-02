test_that("tags containing only whitespace generate warning", {
  expect_snapshot({
    tag <- roxy_test_tag(" ")
    expect_parse_failure(tag_value(tag))
    expect_parse_failure(tag_inherit(tag))
    expect_parse_failure(tag_name(tag))
    expect_parse_failure(tag_name_description(tag))
    expect_parse_failure(tag_code(tag))
    expect_parse_failure(tag_examples(tag))
    expect_parse_failure(tag_markdown(tag))
    expect_parse_failure(tag_markdown_with_sections(tag))
  })
})

test_that("tags check for mismatched parents gives useful warnings", {
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
    tag_markdown(tag)
    tag_markdown_with_sections(tag)
  })

  local_markdown()
  markdown_on()
  expect_snapshot({
    tag <- roxy_test_tag("# one\ntwo\n# three\nfour {")
    tag_markdown_with_sections(tag)
  })
})

test_that("tag_inhert checks for valid inherits", {
  expect_snapshot({
    tag <- roxy_test_tag("foo params section")
    . <- tag_inherit(tag)
  })
})

test_that("tag_name() checks for valid names", {
  expect_snapshot({
    tag <- roxy_test_tag("a b c")
    expect_parse_failure(tag_name(tag))
  })
})

test_that("tag_two_part() gives useful warnings", {
  local_markdown()
  expect_snapshot({
    tag <- roxy_test_tag("")
    expect_parse_failure(tag_two_part(
      tag,
      "a name",
      "a value",
      required = FALSE
    ))
    expect_parse_failure(tag_two_part(tag, "a name", "a value"))
  })
})

test_that("tag_words() gives useful warnings", {
  expect_snapshot({
    tag <- roxy_test_tag("a b")
    expect_parse_failure(tag_words(tag, 3, 3))
    expect_parse_failure(tag_words(tag, 1, 1))
  })
})

test_that("tag_words_line() gives useful warnings", {
  expect_snapshot({
    tag <- roxy_test_tag("a\nb")
    expect_parse_failure(tag_words_line(tag))

    tag <- roxy_test_tag("a\nb\n2")
    expect_parse_failure(tag_words_line(tag))
  })
})

test_that("tag_toggle() gives useful warnings", {
  expect_snapshot({
    tag <- roxy_test_tag("x")
    tag_toggle(tag)
  })
})

test_that("tag_code() gives useful warnings", {
  expect_snapshot({
    tag <- roxy_test_tag("a + ")
    tag_code(tag)
  })
})
