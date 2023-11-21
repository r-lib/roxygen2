# has thoughtful print method

    Code
      parse_text(text)[[1]]
    Output
      <roxy_block> [<text>:5]
        $tag
          [line:  1] @title 'This is a title' {parsed}
          [line:  3] @param 'x,y A number' {parsed}
          [line:  4] @export '' {parsed}
          [line:  5] @usage '<generated>' {parsed}
          [line:  5] @.formals '<generated>' {parsed}
          [line:  5] @backref '<generated>' {parsed}
        $call   f <- function(x, y) x + y
        $object <function> 
          $topic f
          $alias f

# errors are propagated

    Code
      . <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:5: @eval failed to evaluate.
      Caused by error in `foo()`:
      ! Uhoh

# must return non-NA string

    Code
      . <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:3: @eval must evaluate to a character vector.

---

    Code
      . <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:3: @eval must not contain any missing values.

# warns about duplicate tags

    Code
      . <- roc_proc_text(rd_roclet(), block)
    Message
      x <text>:5: Block must contain only one @rdname.

