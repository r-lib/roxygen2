# short and sweet links work

    Code
      out1 <- roc_proc_text(rd_roclet(),
      "\n    #' Title\n    #'\n    #' See [11pkg::function()], [11pkg::object].\n    #' @md\n    foo <- function() {}")[[
        1]]
    Message
      x <text>:4: @description refers to unavailable topic 11pkg::function.
      Caused by error in `find.package()`:
      ! there is no package called '11pkg'
      x <text>:4: @description refers to unavailable topic 11pkg::object.
      Caused by error in `find.package()`:
      ! there is no package called '11pkg'

---

    Code
      out1 <- roc_proc_text(rd_roclet(), block)[[1]]
    Message
      x <text>:4: @description refers to unavailable topic stringr::bar111.

