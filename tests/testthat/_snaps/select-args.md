# warns on invalid input

    Code
      select_args_text(c("x", "na.rm"), "-xlab:", "test")
    Message
      x In topic 'test': argument selection failed.
      Caused by error in `parse()`:
      ! <text>:2:0: unexpected end of input
      1: -xlab:
         ^
    Output
      character(0)
    Code
      select_args_text(c("x", "na.rm"), "\"a\"", "test")
    Message
      x In topic 'test': argument selection failed.
      Caused by error in `select_check()`:
      ! Argument specification must evaluate to a numeric vector.
      Problem in `"a"`.
    Output
      character(0)
    Code
      select_args_text(c("x", "y", "z"), "-x:z", "test")
    Message
      x In topic 'test': argument selection failed.
      Caused by error:
      ! Argument specification must be all positive or all negative, not a mixture.
      i Problem in `-x:z`.
    Output
      character(0)

