# @examples and @example interleave

    \examples{
    example <- 'example1'
    a <- 2
    example <- 'example2'
    } 

# @example gives warning if used instead of @examples

    [<text>:4] @example must be a single line
    i Do you want @examples?

# warns if path doesn't exist

    [<text>:4] @example './this-path-doesnt-exist.R' doesn't exist

# @examplesIf

    \examples{
    \dontshow{if (foo::bar()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
    maybe-run-this-code
    \dontshow{\}) # examplesIf}
    \dontshow{if (foobar()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
    and-this
    \dontshow{\}) # examplesIf}
    } 

# @examplesIf warns about unparseable condition

    [<text>:4] @examplesIf condition failed to parse
    Caused by error in `parse()`:
    ! <text>:2:0: unexpected end of input
    1: 1 +
       ^

