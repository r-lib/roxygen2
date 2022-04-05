# error-ful evalRd generates warning

    Code
      expect_parse_failure(roxy_tag_eval(roxy_test_tag(val = 1)))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test must evaluate to a character vector
    Code
      expect_parse_failure(roxy_tag_eval(roxy_test_tag(val = NA_character_)))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test must not contain any missing values
    Code
      expect_parse_failure(roxy_tag_eval(roxy_test_tag(val = quote(stop("Uhoh")))))
    Output
      <warning/rlang_warning>
      Warning:
      [test.R:1] @test failed to evaluate
      Caused by error:
      ! Uhoh

