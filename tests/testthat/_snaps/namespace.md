# @exportS3method generates fully automatically

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Message
      x <text>:2: @exportS3Method must be used with an known S3 method.

# @exportS3method can extract class from generic

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Message
      x <text>:2: @exportS3Method must have form package::generic.

---

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Message
      x <text>:2: @exportS3Method must be used with a function.

---

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Message
      x <text>:2: @exportS3Method generic ("foo") doesn't match function ("foo1.bar").

# poorly formed importFrom throws error

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Message
      x <text>:2: @importFrom must have at least 2 words, not 1.

# can regenerate NAMESPACE even if its broken

    Code
      update_namespace_imports(path)
    Message
      Writing 'NAMESPACE'

# rawNamespace must be valid code

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Message
      x <text>:2: @rawNamespace failed to parse.
      Caused by error in `parse()`:
      ! <text>:2:0: unexpected end of input
      1: a +
         ^

# evalNamespace warns for bad code

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Message
      x <text>:2: @evalNamespace failed to parse.
      Caused by error in `parse()`:
      ! <text>:2:0: unexpected end of input
      1: a +
         ^

---

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Message
      x <text>:2: @evalNamespace failed to evaluate.
      Caused by error:
      ! Uhoh

---

    Code
      . <- roc_proc_text(namespace_roclet(), block)
    Message
      x <text>:2: @evalNamespace must evaluate to a character vector.

# Invalid imports throw a helpful error

    Code
      out <- roc_proc_text(namespace_roclet(), block)
    Message
      x <text>:2: @importFrom Excluding unknown export in from utils: `InvalidUtilsFunction`.

---

    Code
      out <- roc_proc_text(namespace_roclet(), block)
    Message
      x <text>:2: @importFrom Excluding unknown exports in from utils: `InvalidUtilsFunction1` and `InvalidUtilsFunction2`.

---

    Code
      out <- roc_proc_text(namespace_roclet(), block)

# warns if S3 method not documented

    Code
      roc_proc_text(namespace_roclet(),
      "\n      foo <- function(x) UseMethod('foo')\n      foo.numeric <- function(x) 1\n\n      mean.myclass <- function(x) 2\n    ")
    Message
      x <text>:5: S3 method `mean.myclass` needs @export or @exportS3method tag.
      x <text>:3: S3 method `foo.numeric` needs @export or @exportS3method tag.
    Output
      character(0)

