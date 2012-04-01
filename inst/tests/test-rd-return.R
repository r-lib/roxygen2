context("Rd - return")
roc <- rd_roclet()

test_that("@return overrides @returnList", {
        out <- roc_proc_text(roc, "
                #' @name a
                #' @return yada yada yada
                #' @returnList 
                NULL")[[1]]
        
        expect_identical(get_tag(out, "value")$values, "yada yada yada")
    })

test_that("@return overrides @returnClass", {
        out <- roc_proc_text(roc, "
                #' @name a
                #' @return yada yada yada
                #' @returnClass foo 
                NULL")[[1]]
        
        expect_identical(get_tag(out, "value")$values, "yada yada yada")
    })

test_that("@returnItem correctly adds item to \"value_tag\" object", {
        out <- roc_proc_text(roc, "
                #' @name a
                #' @return yada yada yada
                #' @returnItem foo bar
                NULL")[[1]]
        
        value <- get_tag(out, "value")$values[-1]  # remove general description
        expect_identical(value, c(foo="bar"))
    })

test_that("items generated with @returnItem are formatted correctly", {
        out <- roc_proc_text(roc, "
                #' @name a
                #' @return yada yada yada
                #' @returnItem foo bar
                NULL")[[1]]
        
        formatted <- format(get_tag(out, "value"))
        expect_match(formatted, "\\item\\{foo\\}\\{bar\\}")  # escape curly brackets in regex
    })
