# Getting help with roxygen2

Thanks for using roxygen2. Before filing an issue, there are a few places
to explore and pieces to put together to make the process as smooth as possible.

## Making a reprex

Start by making a minimal **repr**oducible **ex**ample using the 
[reprex](https://reprex.tidyverse.org/) package. If you haven't heard of or used 
reprex before, you're in for a treat! Seriously, reprex will make all of your 
R-question-asking endeavors easier (which is a pretty insane ROI for the five to 
ten minutes it'll take you to learn what it's all about). For additional reprex
pointers, check out the [Get help!](https://www.tidyverse.org/help/) section of
the tidyverse site.

roxygen2 issues can be tricky to create a minimal reprex for. There are two general techniques that can help:

*   If you know the source of the problem, you can often recreate it using a 
    `roc_proc_text()` and text snippet:

    ```R
    library(roxygen2)
    roc_proc_text(rd_roclet(), "
      #' Title
      #' @param
      #'
      #' @export
      foo <- function() {}"
    )
    #> Error in FUN(X[[i]], ...): subscript out of bounds
    ```
    
    It's really helpful if you can spend a little time reducing the text
    to the absolute minimum. Make sure you delete the body of the function
    (since roxygen2 never looks at it), and try each roxygen2 tag in turn.
    Only leave a tag in if you've confirmed it's definitely part of the 
    problem.
    
*   Issues that involve multiple files or that don't give useful error messages
    are harder to track down. In these cases, sometimes your best option is
    to create a copy of your package, and progressively delete files until the
    problem goes away. Once you've done that, sometimes you'll be able to 
    reduce the problem further to a call to `roc_proc_text()` as above. If not,
    you'll need to make your minimal package available somewhere on the internet
    (preferably github) and link to it in the issue. The more you can do to
    make the package as small as possible, the less time it'll take me to
    find the bug, and the more time I'll have to work on it.

## Where to ask

Armed with your reprex, the next step is to figure out [where to ask](https://www.tidyverse.org/help/#where-to-ask). 

  * If it's a question: start with [community.rstudio.com](https://community.rstudio.com/), 
    and/or StackOverflow. There are more people there to answer questions.  
  * If it's a bug: you're in the right place, file an issue.  
  * If you're not sure: let the community help you figure it out! If your 
    problem _is_ a bug or a feature request, you can easily return here and 
    report it. 

Before opening a new issue, be sure to [search issues and pull requests](https://github.com/r-lib/roxygen2/issues) to make sure the 
bug hasn't been reported and/or already fixed in the development version. By 
default, the search will be pre-populated with `is:issue is:open`. You can 
[edit the qualifiers](https://help.github.com/articles/searching-issues-and-pull-requests/) 
(e.g. `is:pr`, `is:closed`) as needed. For example, you'd simply
remove `is:open` to search _all_ issues in the repo, open or closed.

## General advice

To be as efficient as possible, development of tidyverse packages tends to be very bursty. Nothing happens for a long time, until a sufficient quantity of issues accumulates, and then there’s a burst of intense activity as we focus our efforts. That makes development more efficient because it avoids expensive context switching between problems. This process makes a good reprex particularly important because it might be multiple months between your initial report and when we start working on it. If you can’t reproduce the bug, we can’t fix it!
