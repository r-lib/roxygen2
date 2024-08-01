# non-text nodes in links fails

    Code
      markdown("[`foo` bar][x]", tag = tag)
    Message
      x foo.R:10: @title (automatically generated) markdown links must contain plain text.
      i Problematic link: x
    Output
      [1] ""
    Code
      markdown("[__baz__][x]", tag = tag)
    Message
      x foo.R:10: @title (automatically generated) markdown links must contain plain text.
      i Problematic link: x
    Output
      [1] ""

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

# resolve_link_package

    Code
      resolve_link_package("roxygenize", "roxygen2", test_path("testMdLinks2"))
    Output
      [1] NA
    Code
      resolve_link_package("UseMethod", "roxygen2", test_path("testMdLinks2"))
    Output
      [1] NA
    Code
      resolve_link_package("cli_abort", "roxygen2", test_path("testMdLinks2"))
    Output
      [1] "cli"

---

    Code
      resolve_link_package("aa3bc042880aa3b64fef1a9", "roxygen2", test_path(
        "testMdLinks2"), list(tag = tag))
    Message
      x foo.R:10: @title (automatically generated) Could not resolve link to topic "aa3bc042880aa3b64fef1a9" in the dependencies or base packages.
      i If you haven't documented "aa3bc042880aa3b64fef1a9" yet, or just changed its name, this is normal. Once "aa3bc042880aa3b64fef1a9" is documented, this warning goes away.
      i Make sure that the name of the topic is spelled correctly.
      i Always list the linked package as a dependency.
      i Alternatively, you can fully qualify the link with a package name.
    Output
      [1] NA

# resolve_link_package name clash

    Code
      resolve_link_package("pkg_env", "roxygen2", test_path("testMdLinks2"), list(
        tag = tag))
    Message
      x foo.R:10: @title (automatically generated) Topic "pkg_env" is available in multiple packages: pkgload and rlang.
      i Qualify topic explicitly with a package name when linking to it.
    Output
      [1] NA

