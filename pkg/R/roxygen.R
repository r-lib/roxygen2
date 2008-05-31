source("list.R")
pairwise(c(1,
           unlist(Map(function(srcref)
                      c(car(srcref) - 1,
                        caddr(srcref) + 1),
                      attributes(parse("example.R"))$srcref))))
