# tags containing only whitespace generate warning

    Code
      tag <- roxy_test_tag(" ")
      expect_parse_failure(tag_value(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test requires a value.
    Code
      expect_parse_failure(tag_inherit(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test requires a value.
    Code
      expect_parse_failure(tag_name(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test requires a value.
    Code
      expect_parse_failure(tag_name_description(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test requires two parts: a name and a description.
    Code
      expect_parse_failure(tag_code(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test requires a value.
    Code
      expect_parse_failure(tag_examples(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test requires a value.
    Code
      expect_parse_failure(tag_markdown(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test requires a value.
    Code
      expect_parse_failure(tag_markdown_with_sections(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test requires a value.

# tags check for mismatched parents gives useful warnings

    Code
      tag <- roxy_test_tag("a {")
      expect_parse_failure(tag_value(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test has mismatched braces or quotes.
    Code
      expect_parse_failure(tag_inherit(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test has mismatched braces or quotes.
    Code
      expect_parse_failure(tag_name(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test has mismatched braces or quotes.
    Code
      expect_parse_failure(tag_two_part(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test has mismatched braces or quotes.
    Code
      expect_parse_failure(tag_words(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test has mismatched braces or quotes.
    Code
      expect_parse_failure(tag_words_line(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test has mismatched braces or quotes.
    Code
      expect_parse_failure(tag_examples(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test has mismatched braces or quotes.
    Code
      # markdown tags return empty values
      tag_markdown(tag)
    Message
      x test.R:1: @test has mismatched braces or quotes.
    Output
      [test.R:  1] @test 'a {' {parsed}
    Code
      tag_markdown_with_sections(tag)
    Message
      x test.R:1: @test has mismatched braces or quotes.
    Output
      [test.R:  1] @test 'a {' {parsed}

---

    Code
      tag <- roxy_test_tag("# one\ntwo\n# three\nfour {")
      tag_markdown_with_sections(tag)
    Message
      x test.R:1: @test has mismatched braces or quotes.
    Output
      [test.R:  1] @test '# one...' {parsed}

# tag_inhert checks for valid inherits

    Code
      tag <- roxy_test_tag("foo params section")
      . <- tag_inherit(tag)
    Message
      x test.R:1: @test attempts to inherit from unknown type "section".

# tag_name() checks for valid names

    Code
      tag <- roxy_test_tag("a b c")
      expect_parse_failure(tag_name(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test must have only one argument, not 2.

# tag_two_part() gives useful warnings

    Code
      tag <- roxy_test_tag("")
      expect_parse_failure(tag_two_part(tag, "a name", "a value", required = FALSE))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test requires a name.
    Code
      expect_parse_failure(tag_two_part(tag, "a name", "a value"))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test requires two parts: a name and a value.

# tag_words() gives useful warnings

    Code
      tag <- roxy_test_tag("a b")
      expect_parse_failure(tag_words(tag, 3, 3))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test must have at least 3 words, not 2.
    Code
      expect_parse_failure(tag_words(tag, 1, 1))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test must have at most 1 word, not 2.

# tag_words_line() gives useful warnings

    Code
      tag <- roxy_test_tag("a\nb")
      expect_parse_failure(tag_words_line(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test must be a single line, not 2.
      i The first line is "a"
    Code
      tag <- roxy_test_tag("a\nb\n2")
      expect_parse_failure(tag_words_line(tag))
    Output
      <message/rlang_message>
      Message:
      x test.R:1: @test must be a single line, not 3.
      i The first line is "a"

# tag_toggle() gives useful warnings

    Code
      tag <- roxy_test_tag("x")
      tag_toggle(tag)
    Message
      x test.R:1: @test must not be followed by any text.
    Output
      NULL

# tag_code() gives useful warnings

    Code
      tag <- roxy_test_tag("a + ")
      tag_code(tag)
    Message
      x test.R:1: @test failed to parse.
      Caused by error in `parse()`:
      ! <text>:2:0: unexpected end of input
      1: a + 
         ^
    Output
      NULL

