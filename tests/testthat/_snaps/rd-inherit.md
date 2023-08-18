# \links are transformed

    \arguments{
    \item{algo}{The hashing algoritm to be used by \code{\link[digest]{digest}}. Defaults to
    "sha1"}
    } 

# warns on unknown inherit type

    [<text>:2] @inherit attempts to inherit from unknown type "blah"

# warns if can't find section

    @inheritSection failed in topic "b".
    x Can't find section "A" in topic a.

# warned if no params need documentation

    @inheritParams failed in topic "x".
    x All parameters are already documented; none remain to be inherited.

# can inherit all from single function

    [1] "test-rd-inherit-dots.txt"

# does not produce multiple ... args

    [1] "test-rd-inherit-dots-inherit.txt"

# can inherit dots from several functions

    \arguments{
    \item{...}{
      Arguments passed on to \code{\link[=foo]{foo}}, \code{\link[=bar]{bar}}
      \describe{
        \item{\code{x}}{x}
        \item{\code{y}}{y1}
        \item{\code{z}}{z}
      }}
    } 

# useful error for bad inherits

    Code
      . <- roc_proc_text(rd_roclet(), text)
    Condition
      Warning:
      @inheritDotsParam failed in topic "bar".
      Caused by error in `FUN()`:
      ! object 'z' not found

# useful warnings if can't find topics

    Code
      get_rd("base2::attach", source = "source")
    Condition
      Warning:
      @inherits failed in topic "source".
      x Package base2 is not installed.
    Output
      NULL
    Code
      get_rd("base::function_not_found", source = "source")
    Condition
      Warning:
      @inherits failed in topic "source".
      x Can't find topic base::function_not_found.
    Output
      NULL
    Code
      get_rd("function", RoxyTopics$new(), source = "source")
    Condition
      Warning:
      @inherits failed in topic "source".
      x Can't find topic "function".
    Output
      NULL
    Code
      get_rd("foo::bar()", RoxyTopics$new(), source = "source")
    Condition
      Warning:
      @inherits failed in topic "source".
      x Can't find topic "foo::bar()".
    Output
      NULL

