# short and sweet links work

    Code
      out1 <- roc_proc_text(rd_roclet(),
      "\n    #' Title\n    #'\n    #' See [11pkg::function()], [11pkg::object].\n    #' @md\n    foo <- function() {}")[[
        1]]
    Message
      x <text>:4: @description refers to un-installed package 11pkg.
      x <text>:4: @description refers to un-installed package 11pkg.

---

    Code
      out1 <- roc_proc_text(rd_roclet(), block)[[1]]
    Message
      x <text>:4: @description refers to unavailable topic cli::bar111.

# generates informative warnings

    Code
      tag <- roxy_test_tag()
      check_topic("11papaya", "foo", tag)
    Message
      x test.R:1: @test refers to un-installed package 11papaya.
    Code
      check_topic("cli", "foofofofoo", tag)
    Message
      x test.R:1: @test refers to unavailable topic cli::foofofofoo.

