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

# warns if S3 method not documented

    Code
      roc_proc_text(namespace_roclet(),
      "\n      foo <- function(x) UseMethod('foo')\n      foo.numeric <- function(x) 1\n\n      mean.myclass <- function(x) 2\n    ")
    Message
      x [<text>:5] S3 method `mean.myclass` needs @export or @exportS3method tag.
      x [<text>:3] S3 method `foo.numeric` needs @export or @exportS3method tag.
    Output
      character(0)

