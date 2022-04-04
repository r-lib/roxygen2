# tags containing only whitespace generate warning

    Code
      tag <- roxy_test_tag(" ")
      expect_parse_failure(tag_value(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test requires a value
    Code
      expect_parse_failure(tag_inherit(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test requires a value
    Code
      expect_parse_failure(tag_name(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test requires a name
    Code
      expect_parse_failure(tag_two_part(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test requires a value
    Code
      expect_parse_failure(tag_name_description(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test requires a value
    Code
      expect_parse_failure(tag_code(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test requires a value
    Code
      expect_parse_failure(tag_examples(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test requires a value
    Code
      expect_parse_failure(tag_markdown(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test requires a value
    Code
      expect_parse_failure(tag_markdown_with_sections(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test requires a value

# tags check for mismatched parents gives useful warnings

    Code
      tag <- roxy_test_tag("a {")
      expect_parse_failure(tag_value(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test has mismatched braces or quotes
    Code
      expect_parse_failure(tag_inherit(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test has mismatched braces or quotes
    Code
      expect_parse_failure(tag_name(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test has mismatched braces or quotes
    Code
      expect_parse_failure(tag_two_part(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test has mismatched braces or quotes
    Code
      expect_parse_failure(tag_words(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test has mismatched braces or quotes
    Code
      expect_parse_failure(tag_words_line(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test has mismatched braces or quotes
    Code
      expect_parse_failure(tag_examples(tag))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test has mismatched braces or quotes
    Code
      # markdown tags return empty values
      (expect_warning(tag_markdown(tag)))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test has mismatched braces or quotes
    Code
      (expect_warning(tag_markdown_with_sections(tag)))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test has mismatched braces or quotes

---

    Code
      markdown_on()
    Output
      [1] TRUE
    Code
      tag <- roxy_test_tag("{")
      (expect_warning(tag_markdown(tag)))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test has mismatched braces or quotes
    Code
      tag <- roxy_test_tag("# one\ntwo\n# three\nfour {")
      (expect_warning(tag_markdown_with_sections(tag)))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test has mismatched braces or quotes

