# non-text nodes in links fails

    Code
      (expect_error(markdown("[`foo` bar][x]"), "plain text"))
    Output
      <error/rlang_error>
      Error in `parse_link()`:
      ! Links must contain plain text.
      x Problematic node: `code`
      i Link target: `x`
    Code
      (expect_error(with_file(markdown("[`foo{}` bar __baz__][x]")), "plain text"))
    Output
      <error/rlang_error>
      Error in `parse_link()`:
      ! Links must contain plain text.
      x Problematic nodes: `code` and `strong`
      i Link target: `x`
      i Current file: 'test_file.R'

# short and sweet links work

    Code
      out1 <- roc_proc_text(rd_roclet(),
      "\n    #' Title\n    #'\n    #' See [11pkg::function()], [11pkg::object].\n    #' @md\n    foo <- function() {}")[[
        1]]
    Condition
      Warning:
      [<text>:4] @description Link to unavailable package: 11pkg::function.
      there is no package called '11pkg'
      Warning:
      [<text>:4] @description Link to unavailable package: 11pkg::object.
      there is no package called '11pkg'

---

    [<text>:4] @description Link to unknown topic: stringr::bar111

