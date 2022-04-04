# non-text nodes in links fails

    Code
      markdown("[`foo` bar][x]", tag = tag)
    Condition
      Warning:
      [foo.R:10] @title Links must contain plain text. Problematic link: x
    Output
      [1] ""
    Code
      markdown("[`foo{}` bar __baz__][x]", tag = tag)
    Condition
      Warning:
      [foo.R:10] @title Links must contain plain text. Problematic link: x
    Output
      [1] ""

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

