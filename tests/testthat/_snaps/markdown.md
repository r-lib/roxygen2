# code blocks work

    Before
    
    \if{html}{\out{<div class="sourceCode">}}\preformatted{x \%in\% 1:10
    }\if{html}{\out{</div>}}
    
    After

# code block with language creates HTML tag

    Before
    
    \if{html}{\out{<div class="sourceCode r">}}\preformatted{x \%in\% 1:10
    }\if{html}{\out{</div>}}
    
    After

# can convert table to Rd

    Code
      for (table in tables) {
        cat_line(table)
        cat_line(markdown(table))
        cat_line()
      }
    Output
      
      | x   | y   |
      | --- | --- |
      | 1   | 2   |
      \tabular{ll}{
         x \tab y \cr
         1 \tab 2 \cr
      }
      
      | x   | y   |
      | :-: | --: |
      | 1   | 2   |
      \tabular{cr}{
         x \tab y \cr
         1 \tab 2 \cr
      }
      
      | x     | y         |
      | ----- | --------- |
      | 1 _2_ | 3 *4* `5` |
        
      \tabular{ll}{
         x \tab y \cr
         1 \emph{2} \tab 3 \emph{4} \code{5} \cr
      }
      

# unhandled markdown generates warning

    [<text>:4] @description markdown translation failed
    x block quotes are not currently supported

