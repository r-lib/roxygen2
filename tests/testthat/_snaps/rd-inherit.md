# \links are transformed

    \arguments{
    \item{algo}{The hashing algorithm to be used by \code{\link[digest]{digest}}. Defaults to
    "sha1"}
    } 

# invalid syntax gives useful warning

    Code
      . <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:2: @inheritDotParams requires a source.
      x <text>:3: @inheritSection requires two parts: a topic name and a section title.

# warns on unknown inherit type

    Code
      parse_text(text)
    Message
      x <text>:2: @inherit attempts to inherit from unknown type "blah".
    Output
      [[1]]
      <roxy_block> [<text>:3]
        $tag
          [line:  2] @inherit 'fun blah' {parsed}
          [line:  3] @backref '<generated>' {parsed}
        $call   NULL
        $object NULL
        
      

# warns if can't find section

    Code
      . <- roc_proc_text(rd_roclet(), code)
    Message
      x In topic 'b': @inheritSection failed to find section "A" in topic a.

# warned if no params need documentation

    Code
      . <- roc_proc_text(rd_roclet(), code)
    Message
      x In topic 'x': @inheritParams failed.
      i All parameters are already documented; none remain to be inherited.

# can inherit all from single function

    [1] "test-rd-inherit-dots.txt"

# does not produce multiple ... args

    [1] "test-rd-inherit-dots-inherit.txt"

# can inherit dots from several functions

    \arguments{
    \item{...}{
      Arguments passed on to \code{\link{foo}}, \code{\link{bar}}
      \describe{
        \item{\code{x}}{x}
        \item{\code{y}}{y1}
        \item{\code{z}}{z}
      }}
    } 

# useful error for bad inherits

    Code
      . <- roc_proc_text(rd_roclet(), text)
    Message
      x In topic 'bar': @inheritDotsParam failed.
      Caused by error in `FUN()`:
      ! object 'z' not found
      x In topic 'bar': @inheritDotParams failed.
      i No arguments inherited from `foo()`.

# warns when no params to inherit (#1671)

    Code
      out <- roc_proc_text(rd_roclet(), text)
    Message
      x In topic 'bar': @inheritDotParams failed.
      i No arguments inherited from `foo()`.

# inheritDotParams warns when source not found (#1602)

    Code
      . <- roc_proc_text(rd_roclet(), text)
    Message
      x In topic 'test': @inheritDotParams failed to find topic "format" in current package.
      x In topic 'test': @inheritDotParams failed.
      i No arguments inherited from `format()`.

# useful warnings if can't find topics

    Code
      get_rd("not_installed::pkg", source = "source")
    Message
      x In topic 'source': @inherits failed because not_installed is not installed.
    Output
      NULL
    Code
      get_rd("base::doesntexist", source = "source")
    Message
      x In topic 'source': @inherits failed to find topic base::doesntexist.
    Output
      NULL
    Code
      get_rd("doesntexist", RoxyTopics$new(), source = "source")
    Message
      x In topic 'source': @inherits failed to find topic "doesntexist" in current package.
    Output
      NULL

