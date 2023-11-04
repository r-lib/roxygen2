# @exportS3method generates fully automatically

    [<text>:2] @exportS3Method must be used with an known S3 method

# @exportS3method can extract class from generic

    [<text>:2] @exportS3Method must have form package::generic

---

    [<text>:2] @exportS3Method must be used with a function

---

    [<text>:2] @exportS3Method doesn't match function name
    x Expected to see "foo" to match "pkg::foo"
    i Function name is "foo1.bar"

# poorly formed importFrom throws error

    [<text>:2] @importFrom must have at least 2 words, not 1

# rawNamespace must be valid code

    [<text>:2] @rawNamespace failed to parse
    Caused by error in `parse()`:
    ! <text>:2:0: unexpected end of input
    1: a +
       ^

# evalNamespace warns for bad code

    [<text>:2] @evalNamespace failed to parse
    Caused by error in `parse()`:
    ! <text>:2:0: unexpected end of input
    1: a +
       ^

---

    [<text>:2] @evalNamespace failed to evaluate
    Caused by error:
    ! Uhoh

---

    [<text>:2] @evalNamespace must evaluate to a character vector

# Invalid imports throw a helpful error

    Code
      roc_proc_text(namespace_roclet(),
      "\n      #' @importFrom utils InvalidUtilsFunction\n      NULL\n    ")
    Condition
      Warning:
      [<text>:2] @importFrom Unknown export in `@importFrom utils`: `InvalidUtilsFunction`
    Output
      [1] "importFrom(utils,InvalidUtilsFunction)"

---

    Code
      roc_proc_text(namespace_roclet(),
      "\n      #' @importFrom utils InvalidUtilsFunction1 InvalidUtilsFunction2\n      NULL\n    ")
    Condition
      Warning:
      [<text>:2] @importFrom Unknown exports in `@importFrom utils`: `InvalidUtilsFunction1` and `InvalidUtilsFunction2`
    Output
      [1] "importFrom(utils,InvalidUtilsFunction1)"
      [2] "importFrom(utils,InvalidUtilsFunction2)"

