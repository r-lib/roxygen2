# new wrapping style doesn't change unexpectedly

    f(
      a = "                                    a",
      b = "                                    b",
      c = "                                    c",
      d = "                                    d"
    ) 
    
    f(
      a = c("abcdef", "abcdef", "abcdef", "abcdef", "abcdef", "abcdef", "abcdef", "abcdef",
        "abcdef", "abcdef")
    ) 
    
    \method{mean}{reallyratherquitelongclassname}(
      reallyreatherquitelongargument = "reallyratherquitelongvalue_____________________"
    ) 
    
    long_replacement_fun(
      x,
      a = "aaaaaaaaaaaaaaaa",
      b = "aaaaaaaaaaaaaaaa",
      c = "aaaaaaaaaaaaaaaa"
    ) <- value 
    

# old wrapping style doesn't change unexpectedly

    f(a = "                                    a",
      b = "                                    b",
      c = "                                    c",
      d = "                                    d") 
    
    f(a = c("abcdef", "abcdef", "abcdef", "abcdef", "abcdef", "abcdef", "abcdef",
      "abcdef", "abcdef", "abcdef")) 
    
    
      \method{mean}{reallyratherquitelongclassname}(reallyreatherquitelongargument = "reallyratherquitelongvalue_____________________") 
    
    long_replacement_fun(x, a = "aaaaaaaaaaaaaaaa", b = "aaaaaaaaaaaaaaaa",
      c = "aaaaaaaaaaaaaaaa") <- value 
    
    f(xxxxxxxxxxxxxxxxxx1, xxxxxxxxxxxxxxxxxx2, xxxxxxxxxxxxxxxxxx3, x = "\\"'",
      xxxxxxxxxxxxxxxxxx4, xxxxxxxxxxxxxxxxxx5, xxxxxxxxxxxxxxxxxx6,
      xxxxxxxxxxxxxxxxxx7) 
    

