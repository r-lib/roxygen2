# @examples and @example interleave

    \examples{
    example <- 'example1'
    a <- 2
    example <- 'example2'
    } 

# @examplesIf

    \examples{
    \dontshow{if (foo::bar()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
    maybe-run-this-code
    \dontshow{\}) # examplesIf}
    \dontshow{if (foobar()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
    and-this
    \dontshow{\}) # examplesIf}
    } 

