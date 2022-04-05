# has thoughtful print method

    <roxy_block> [<text>:5]
      $tag
        [line:  1] @title 'This is a title' {parsed}
        [line:  3] @param 'x,y A number' {parsed}
        [line:  4] @export '' {parsed}
        [????:???] @usage '<generated>' {parsed}
        [????:???] @.formals '<generated>' {parsed}
        [????:???] @backref '<text>' {parsed}
      $call   f <- function(x, y) x + y
      $object <function> 
        $topic f
        $alias f

# errors are propagated

    [<text>:5] @eval failed to evaluate
    Caused by error in `foo()`:
    ! Uhoh

# must return non-NA string

    [<text>:3] @eval must evaluate to a character vector

---

    [<text>:3] @eval must not contain any missing values

