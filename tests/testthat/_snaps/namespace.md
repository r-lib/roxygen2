# poorly formed importFrom throws error

    [<text>:2] @importFrom must have at least 2 words

# rawNamespace must be valid code

    [<text>:2] @rawNamespace failed to parse
    Caused by error in `parse()`:
    ! <text>:2:0: unexpected end of input
    1: a +
       ^

# evalNamespace generates warning when code is invalid

    [<text>:2] @evalNamespace failed to parse
    Caused by error in `parse()`:
    ! <text>:2:0: unexpected end of input
    1: a +
       ^

# evalNamespace generates warning when code raises error

    [<text>:2] @evalNamespace failed to execute
    Caused by error:
    ! Uhoh

