# @exportS3method generates fully automatically

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Condition
      Warning:
      [<text>:2] @exportS3Method must be used with an known S3 method

# @exportS3method can extract class from generic

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Condition
      Warning:
      [<text>:2] @exportS3Method must have form package::generic

---

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Condition
      Warning:
      [<text>:2] @exportS3Method must be used with a function

---

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Condition
      Warning:
      [<text>:2] @exportS3Method doesn't match function name
      x Expected to see "foo" to match "pkg::foo"
      i Function name is "foo1.bar"

# poorly formed importFrom throws error

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Condition
      Warning:
      [<text>:2] @importFrom must have at least 2 words, not 1

# rawNamespace must be valid code

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Condition
      Warning:
      [<text>:2] @rawNamespace failed to parse
      Caused by error in `parse()`:
      ! <text>:2:0: unexpected end of input
      1: a +
         ^

# evalNamespace warns for bad code

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Condition
      Warning:
      [<text>:2] @evalNamespace failed to parse
      Caused by error in `parse()`:
      ! <text>:2:0: unexpected end of input
      1: a +
         ^

---

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Condition
      Warning:
      [<text>:2] @evalNamespace failed to evaluate
      Caused by error:
      ! Uhoh

---

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Condition
      Warning:
      [<text>:2] @evalNamespace must evaluate to a character vector

