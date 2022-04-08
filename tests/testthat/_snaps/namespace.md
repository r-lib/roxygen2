# @exportS3method generatedsS3method()

    [<text>:1] @exportS3Method must be used with an S3 method when it has 0 or 1 values

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

